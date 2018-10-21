;;; vc-hgcmd.el --- VC mercurial backend that uses hg command server -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: vc
;; URL: https://github.com/muffinmad/emacs-vc-hgcmd
;; Package-Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; VC backend to work with hg repositories through hg command server.
;; https://www.mercurial-scm.org/wiki/CommandServer
;;
;; The main advantage compared to vc-hg is speed.
;; Because communicating with hg over pipe is much faster than starting hg for each command.
;;
;; Also there are some other improvements and differences:
;;
;; - vc-hgcmd can't show file renames in `vc-dir' and doesn't have short log version yet
;;
;; - Unresolved conflict status for a file
;; Files with unresolved merge conflicts have appropriate status in `vc-dir'.
;; Also you can use `vc-find-conflicted-file' to find next file with unresolved merge conflict.
;;
;; - hg summary as `vc-dir' extra headers
;; hg cummary command gives useful information about commit, update and phase states.
;;
;; - Current branch is displayed on mode line.
;; It's not customizable yet.
;;
;; - Amend and close branch commits
;; While editing commit message you can togle --amend and --close-branch flags.
;;
;; - Merge branch
;; vc-hgcmd will ask for branch name to merge.
;;
;; - Default pull argements
;; You can customize default hg pulll command arguments.
;; By default it's --update. You can change it for particular pull by invoking `vc-pull' with prefix argument.
;;
;; - Branches and tags as revision completion table
;; Instead of list of all revisions of file vc-hgcmd provides list of named branches and tags.
;; It's very useful on `vc-retrieve-tag'.
;; You can specify -C to run hg update with -C flag and discard all uncommited changes.
;;
;; - Filenames in vc-annotate buffer are hidden
;; They are needed to annotate changes across renames but mostly useless in annotate buffer.
;; vc-hgcmd removes it from annotate buffer but keep it in text properties.
;;
;; - Create tag
;; vc-hgcmd creates tag on `vc-create-tag'
;; If `vc-create-tag' is invoked with prefix argument then named branch will be created.
;;
;; - Predefined commit message
;; While commiting merge changes commit message will be set to 'merged <branch>' if
;; different branch was merged or to 'merged <node>'

;;; Code:

(require 'bindat)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'vc)
(require 'vc-dir)


;;;; Customization


(defgroup vc-hgcmd nil
  "Settings for VC mercurial commandserver backend."
  :group 'vc
  :prefix "vc-hgcmd-")

(defcustom vc-hgcmd-hg-executable "hg"
  "Hg executable."
  :type '(string))

(defcustom vc-hgcmd-pull-args "--update"
  "Arguments for pull command.
This arguments will be used for each pull command.
You can edit this arguments for specific pull command by invoke `vc-pull' with prefix argument."
  :type '(string))

(defcustom vc-hgcmd-push-alternate-args "--new-branch"
  "Initial value for hg push arguments when asked."
  :type '(string))


;;;; Consts. Customizing this can lead to unexpected behaviour


(defconst vc-hgcmd-cmdserver-args
  '(
    ;; TODO: cmdserver clients must handle I and L channels
    ;; "--config" "ui.interactive=True"
    "--config" "ui.editor=emacsclient"
    "--config" "pager.pager="
    "serve"
    "--cmdserver" "pipe")
  "Args to start hg command server.")

(defconst vc-hgcmd-cmdserver-process-environment
  '("TERM=dumb"
    "HGPLAIN="
    "LANGUAGE=C"
    "HGENCODING=UTF-8"
    "ALTERNATE_EDITOR=emacs")
  "Environment variables for hg command server process.")


;;;; Modes


(define-derived-mode vc-hgcmd-process-mode fundamental-mode "Hgcmd process"
  "Major mode for hg cmdserver process"
  (hack-dir-local-variables-non-file-buffer)
  (set-buffer-multibyte nil)
  (setq
   buffer-undo-list t
   list-buffers-directory (abbreviate-file-name default-directory)
   buffer-read-only t))

(define-derived-mode vc-hgcmd-output-mode compilation-mode "Hgcmd output"
  "Major mode for hg output"
  (hack-dir-local-variables-non-file-buffer)
  (setq
   buffer-undo-list t
   list-buffers-directory (abbreviate-file-name default-directory)
   buffer-read-only t))


;;;; cmdserver communication


(defvar vc-hgcmd--process-buffers-by-dir (make-hash-table :test #'equal))

(cl-defstruct (vc-hgcmd--command (:copier nil)) command output-buffer result-code wait callback callback-args)

(defvar-local vc-hgcmd--current-command nil
  "Current running hgcmd command. Future commands will wait until the current command will finish.")
(put 'vc-hgcmd--current-command 'permanent-local t)

(defun vc-hgcmd--project-name (dir)
  "Get project name based on DIR."
  (file-name-nondirectory (directory-file-name dir)))

(defun vc-hgcmd--read-output ()
  "Parse process output in current buffer."
  (when (> (point-max) 5)
    (let* ((data (bindat-unpack '((c byte) (d u32)) (vconcat (buffer-substring-no-properties 1 6))))
           (channel (bindat-get-field data 'c))
           (size (bindat-get-field data 'd)))
      (when (> (point-max) (+ 5 size))
        (let ((data (vconcat (buffer-substring-no-properties 6 (+ 6 size))))
              (inhibit-read-only t))
          (delete-region 1 (+ 6 size))
          (cons channel data))))))

(defun vc-hgcmd--cmdserver-process-filter (process output)
  "Filter OUTPUT for hg cmdserver PROCESS.
Insert output to process buffer and check if amount of data is enought to parse it to output buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (let* ((current-command (with-current-buffer buffer vc-hgcmd--current-command))
             (output-buffer (vc-hgcmd--command-output-buffer current-command)))
        (when output-buffer
          (with-current-buffer buffer
            (goto-char (point-max))
            (let ((inhibit-read-only t)) (insert output))
            (while
                (let ((data (vc-hgcmd--read-output)))
                  (when data
                    (let ((channel (car data))
                          (data (cdr data)))
                      (with-current-buffer output-buffer
                        (let ((inhibit-read-only t))
                          (goto-char (point-max))
                          (cond ((or (eq channel ?e) (eq channel ?o))
                                 (insert (decode-coding-string (bindat-get-field (bindat-unpack `((f str ,(length data))) data) 'f) 'utf-8)))
                                ((eq channel ?r)
                                 (setf (vc-hgcmd--command-result-code current-command) (bindat-get-field (bindat-unpack `((f u32)) data) 'f))
                                 (with-current-buffer buffer (setq vc-hgcmd--current-command nil))
                                 (let ((callback (vc-hgcmd--command-callback current-command))
                                       (args (vc-hgcmd--command-callback-args current-command)))
                                   (when callback
                                     (if args (funcall callback args) (funcall callback)))))
                                ;; TODO: cmdserver clients must handle I and L channels
                                (t (error (format "unknown channel %c\n" channel)))))))
                    t)))))))))

(defun vc-hgcmd--check-buffer-process (buffer)
  "Create hg cmdserver process in BUFFER if needed."
  (unless (get-buffer-process buffer)
    (let ((process-environment (append vc-hgcmd-cmdserver-process-environment process-environment))
          (process-connection-type nil))
      (with-current-buffer buffer
        (let ((process (apply
                        #'start-file-process
                        (concat "vc-hgcmd process: " (vc-hgcmd--project-name default-directory))
                        buffer
                        vc-hgcmd-hg-executable
                        vc-hgcmd-cmdserver-args)))
          (set-process-sentinel process #'ignore)
          (set-process-query-on-exit-flag process nil)
          (set-process-coding-system process 'no-conversion 'no-conversion)
          ;; read hello message
          ;; TODO parse encoding
          (while (not (vc-hgcmd--read-output))
            (accept-process-output process 0.1 nil t))
          (set-process-filter process #'vc-hgcmd--cmdserver-process-filter)
          process)))))

(defun vc-hgcmd--repo-dir ()
  "Get repo dir."
  (abbreviate-file-name (or (vc-hgcmd-root default-directory) default-directory)))

(defun vc-hgcmd--create-process-buffer (dir)
  "Create hg cmdserver process buffer for repo in DIR."
  (let ((buffer (generate-new-buffer (concat "*hgcmd process: " (vc-hgcmd--project-name dir) "*"))))
    (with-current-buffer buffer
      (setq default-directory dir)
      (vc-hgcmd-process-mode))
    buffer))

(defun vc-hgcmd--get-process-buffer (dir)
  "Get hg cmdserver process buffer for repo in DIR."
  (let ((buffer (gethash dir vc-hgcmd--process-buffers-by-dir)))
    (when (buffer-live-p buffer) buffer)))

(defun vc-hgcmd--process-buffer ()
  "Get hg cmdserver process buffer for repo in `default-directory'."
  (let* ((dir (vc-hgcmd--repo-dir))
         (buffer (or (vc-hgcmd--get-process-buffer dir)
                     (puthash dir (vc-hgcmd--create-process-buffer dir) vc-hgcmd--process-buffers-by-dir))))
    (when buffer (vc-hgcmd--check-buffer-process buffer))
    buffer))

(defun vc-hgcmd--create-output-buffer (dir)
  "Create hg output buffer for repo in DIR."
  (let ((buffer (generate-new-buffer (concat "*hgcmd output: " (vc-hgcmd--project-name dir) "*"))))
    (with-current-buffer buffer
      (setq default-directory dir)
      (vc-hgcmd-output-mode))
    buffer))

(defun vc-hgcmd--get-output-buffer (&optional command)
  "Get hg output buffer for repo in `default-directory'.
Insert 'Running command' and display buffer text if COMMAND"
  (let* ((dir (vc-hgcmd--repo-dir))
         (buffer (or (seq-find (lambda (buffer)
                                 (with-current-buffer buffer
                                   (and (eq major-mode 'vc-hgcmd-output-mode)
                                        (equal (abbreviate-file-name default-directory) dir))))
                               (buffer-list))
                     (vc-hgcmd--create-output-buffer dir)))
         window-start)
    (when command
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (eq (point) (point-min)) (insert "\n"))
          (setq window-start (point))
          (insert (concat "Running \"" (mapconcat #'identity command " ") "%s\"...\n"))))
      (let ((window (display-buffer buffer)))
        (when window (set-window-start window window-start))))
    buffer))

(defun vc-hgcmd--prepare-command-to-send (command)
  "Prepare COMMAND to send to hg process."
  (let ((args (mapconcat #'identity command "\0")))
    (concat (bindat-pack '((l u32)) `((l . ,(length args)))) args)))

(defun vc-hgcmd--run-command (cmd)
  "Run hg CMD."
  (let* ((buffer (vc-hgcmd--process-buffer))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (while vc-hgcmd--current-command
        (accept-process-output process 0.1 nil t))
      (setq vc-hgcmd--current-command cmd)
      (process-send-string process (concat "runcommand\n" (vc-hgcmd--prepare-command-to-send (vc-hgcmd--command-command cmd))))
      (when (vc-hgcmd--command-wait cmd)
        (while vc-hgcmd--current-command
          (accept-process-output process 0.1 nil t))))))

(defun vc-hgcmd-command (&rest command)
  "Run hg COMMAND and return it's output."
  (with-temp-buffer
    (let ((cmd (make-vc-hgcmd--command :command command :output-buffer (current-buffer) :wait t)))
      (vc-hgcmd--run-command cmd)
      ;; TODO handle result codes
      (let ((result (string-trim-right (buffer-string))))
        (when (> (length result) 0)
          result)))))

(defun vc-hgcmd-command-output-buffer (buffer &rest command)
  "Send output of COMMAND to BUFFER and wait COMMAND to finish."
  (vc-setup-buffer buffer)
  (vc-hgcmd--run-command (make-vc-hgcmd--command :command command :output-buffer buffer :wait t)))

(defun vc-hgcmd--update-callback (buffer)
  "Update BUFFER where was command called from after command finished."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((derived-mode-p 'vc-dir-mode)
        (vc-dir-refresh))
       ((derived-mode-p 'dired-mode)
        (revert-buffer))))))

(defun vc-hgcmd-command-update-callback (command)
  "Run COMMAND and update current buffer afret command finished."
  (vc-hgcmd--run-command
   (make-vc-hgcmd--command
    :command command
    :output-buffer (vc-hgcmd--get-output-buffer command)
    :callback #'vc-hgcmd--update-callback
    :callback-args (current-buffer))))

(defconst vc-hgcmd--translation-status '((?C . up-to-date)
                                         (?= . up-to-date)
                                         (?A . added)
                                         (?I . ignored)
                                         (?R . removed)
                                         (?! . missing)
                                         (?? . unregistered)
                                         (?M . edited)
                                         (?  . origin))
  "Translation for status command output.")

(defconst vc-hgcmd--translation-resolve '((?U . conflict)
                                          (?R . edited))
  "Translation for resolve command output.")

(defun vc-hgcmd--branches ()
  "Return branches list."
  (split-string (vc-hgcmd-command "branches" "-T" "{branch}\n") "\n"))

(defun vc-hgcmd--tags ()
  "Return tags list."
  (split-string (vc-hgcmd-command "tags" "-q") "\n"))

(defconst vc-hgcmd--no-file-re
  ".+: No such file or directory$")

;;;; VC backend

(defun vc-hgcmd-revision-granularity ()
  "Per-repository revision number."
  'repository)

;;;###autoload (defun vc-hgcmd-registered (file)
;;;###autoload   (when (vc-find-root file ".hg")
;;;###autoload     (load "vc-hgcmd" nil t)
;;;###autoload     (vc-hgcmd-registered file)))

(defun vc-hgcmd-registered (file)
  "Is file FILE is registered."
  (when (vc-hgcmd-root file)
    (let ((state (vc-hgcmd-state file)))
      (and state (not (memq state '(ignored unregistered)))))))

(defun vc-hgcmd-state (file)
  "State for FILE."
  (let ((out (vc-hgcmd-command "status" "-A" file)))
    (when (and out (null (string-match-p vc-hgcmd--no-file-re out)))
      (let ((state (cdr (assoc (aref out 0) vc-hgcmd--translation-status))))
        (if (and (eq state 'edited) (vc-hgcmd--file-unresolved-p file))
            'conflict
          state)))))

(defun vc-hgcmd--dir-status-callback (update-function)
  "Call UPDATE-FUNCTION with result of status command."
  (let ((result nil)
        (conflicted (vc-hgcmd-conflicted-files)))
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at vc-hgcmd--no-file-re)
        (let* ((file (buffer-substring-no-properties (+ (point) 2) (line-end-position)))
               (state (if (member file conflicted)
                          'conflict
                        (cdr (assoc (char-after) vc-hgcmd--translation-status)))))
          (push (list file state nil) result)))
      (forward-line))
    (funcall update-function result)))

(defun vc-hgcmd-dir-status-files (dir files update-function)
  "Call UPDATE-FUNCTION with status for files in DIR or FILES."
    ;; TODO track file renames with -C option
    (let ((command (if files
                       (nconc (list "status" "-A") files)
                     (list "status" dir))))
    (vc-hgcmd--run-command
     (make-vc-hgcmd--command
      :command command
      :output-buffer (current-buffer)
      :callback #'vc-hgcmd--dir-status-callback
      :callback-args update-function))))

(defun vc-hgcmd--extra-header (name value)
  "Format NAME and VALUE as dir extra header."
  (concat (propertize name 'face 'font-lock-type-face)
          (propertize value 'face 'font-lock-variable-name-face)
          "\n"))

(defun vc-hgcmd--parent-info (data)
  "Parse and propertize parent log info from DATA."
  (when data
    (cl-multiple-value-bind (rev branch tags desc) (split-string data "\0")
      (apply #'concat
             (list
              (vc-hgcmd--extra-header "Parent     : " (concat rev " " branch " " tags))
              (vc-hgcmd--extra-header "           : " desc))))))

(defun vc-hgcmd--summary-info (search name)
  "Search for summary info prefixed by SEARCH and propertize with NAME."
  (goto-char (point-min))
  (when (search-forward-regexp (format "^%s: \\(.*\\)" search) nil t)
    (vc-hgcmd--extra-header name (match-string-no-properties 1))))

(defun vc-hgcmd-dir-extra-headers (_dir)
  "Return summary command for DIR output as dir extra headers."
  (let* ((parents (split-string (vc-hgcmd-command "log" "-r" "p1()+p2()" "--template" "{rev}:{node|short}\\0{branch}\\0{tags}\\0{desc|firstline}\n") "\n"))
         (result (apply #'concat (mapcar #'vc-hgcmd--parent-info parents))))
    (with-temp-buffer
      (vc-hgcmd-command-output-buffer (current-buffer) "summary")
      (concat result
              (vc-hgcmd--summary-info "branch" "Branch     : ")
              (vc-hgcmd--summary-info "commit" "Commit     : ")
              (vc-hgcmd--summary-info "update" "Update     : ")
              (vc-hgcmd--summary-info "phases" "Phases     : ")))))

;; TODO dir-printer
;; TODO status-fileinfo-extra

(defun vc-hgcmd-working-revision (file)
  "Working revision. Return repository working revision if FILE is commited."
  (if (and file (eq 'added (vc-state file)))
      "0"
    (or (vc-hgcmd-command "log" "-l" "1" "-f" "-T" "{rev}") "0")))

(defun vc-hgcmd-checkout-model (_files)
  "Files are always writable."
  'implicit)

(defun vc-hgcmd-mode-line-string (file)
  "Return a string for `vc-mode-line' to put in the mode line for FILE."
  (let* ((state (vc-state file))
	     (state-echo nil)
	     (face nil)
         ;; TODO allow to customize it.
	     (branch (vc-hgcmd-command "branch")))
    (propertize
     (concat
      "Hgcmd"
      (cond
       ((eq state 'up-to-date)
        (setq state-echo "Up to date file")
	    (setq face 'vc-up-to-date-state)
	    "-")
       ((eq state 'added)
        (setq state-echo "Locally added file")
	    (setq face 'vc-locally-added-state)
        "@")
       ((eq state 'conflict)
        (setq state-echo "File contains conflicts after the last merge")
	    (setq face 'vc-conflict-state)
        "!")
       ((eq state 'removed)
        (setq state-echo "File removed from the VC system")
	    (setq face 'vc-removed-state)
        "!")
       ((eq state 'missing)
        (setq state-echo "File tracked by the VC system, but missing from the file system")
	    (setq face 'vc-missing-state)
        "?")
	   (t
	    (setq state-echo "Locally modified file")
	    (setq face 'vc-edited-state)
	    ":"))
      branch)
     'face face
     'help-echo (concat state-echo " under the Hg version control system"))))

(defun vc-hgcmd-create-repo ()
  "Init Hg repository."
  (vc-hgcmd-command "init"))

;; TODO vc switches

(defun vc-hgcmd-register (files &optional _comment)
  "Register FILES."
  (apply #'vc-hgcmd-command (nconc (list "add") files)))

(defalias 'vc-hgcmd-responsible-p 'vc-hgcmd-root)

;; TODO receive-file

(defun vc-hgcmd-unregister (file)
  "Forget FILE."
  (vc-hgcmd-command "forget" file))

(declare-function log-edit-extract-headers "log-edit" (headers string))
(declare-function log-edit-toggle-header "log-edit" (header value))
(declare-function log-edit-set-header "log-edit" (header value &optional toggle))

(defun vc-hgcmd--arg-close-branch (value)
  "If VALUE is yes then --close-branch."
  (when (equal "yes" value) (list "--close-branch")))

(defun vc-hgcmd--arg-amend (value)
  "If VALUE is yes then --close-branch."
  (when (equal "yes" value) (list "--amend")))

(defun vc-hgcmd-checkin (files comment &optional _rev)
  "Commit FILES with COMMENT."
  (apply #'vc-hgcmd-command
         (nconc
          (list "commit" "-m")
          (log-edit-extract-headers `(("Author" . "--user")
                                      ("Date" . "--date")
                                      ("Amend" . vc-hgcmd--arg-amend)
                                      ("Close-branch" . vc-hgcmd--arg-close-branch))
                                    (encode-coding-string comment 'utf-8))
          files)))

(defun vc-hgcmd-find-revision (file rev buffer)
  "Put REV of FILE to BUFFER."
  (apply #'vc-hgcmd-command-output-buffer buffer (if rev (list "cat" "-r" rev file) (list "cat" file))))

(defun vc-hgcmd-checkout (file &optional rev)
  "Retrieve revision REV of FILE."
  (vc-hgcmd-find-revision file rev (or (get-file-buffer file) (current-buffer))))

(defun vc-hgcmd-revert (file &optional contents-done)
  "Refert FILE if not CONTENTS-DONE."
  (unless contents-done
    (vc-hgcmd-command "revert" file)))

(defun vc-hgcmd-merge-branch ()
  "Merge."
  (let* ((completion-fun (if (and (boundp 'ido-mode) ido-mode) #'ido-completing-read #'completing-read))
         (branch (funcall completion-fun "Merge from branch: " (nconc (list "") (vc-hgcmd--branches) (vc-hgcmd--tags)))))
    (vc-hgcmd-command-update-callback
     (if (> (length branch) 0)
         (list "merge" branch)
       (list "merge")))))

(defun vc-hgcmd-pull (prompt)
  "Pull. Prompt for args if PROMPT."
  (vc-hgcmd-command-update-callback
   (nconc
    (list "pull")
    (split-string-and-unquote (if prompt (read-from-minibuffer "Hg pull: " vc-hgcmd-pull-args) vc-hgcmd-pull-args)))))

(defun vc-hgcmd-push (prompt)
  "Pull. Prompt for args if PROMPT."
  (vc-hgcmd-command-update-callback
   (nconc
    (list "push")
    (when prompt (split-string-and-unquote (read-from-minibuffer "Hg push: " vc-hgcmd-push-alternate-args))))))

(defun vc-hgcmd-mark-resolved (files)
  "Mark FILES resolved."
  (apply #'vc-hgcmd-command (nconc (list "resolve" "-m") files)))

(defun vc-hgcmd-print-log (files buffer &optional shortlog start-revision limit)
  "Put maybe SHORTLOG log of FILES to BUFFER starting with START-REVISION limited by LIMIT."
  ;; TODO short log
  (let ((command
         (nconc
          (list "log")
          (when start-revision
            ;; start revision is used for branch log or specific revision log when limit is 1
            (list (if (eq limit 1) "-r" "-b") start-revision))
          (when limit (list "-l" (number-to-string limit)))
          (unless (or shortlog (eq limit 1)) (list "-f")) ; follow file renames
          files)))
    ;; If limit is 1 or vc-log-show-limit then it is initial diff and better move to working revision
    ;; otherwise remember point position and restore it later
    (let ((p (with-current-buffer buffer (unless (or (member limit (list 1 vc-log-show-limit))) (point)))))
      (apply #'vc-hgcmd-command-output-buffer buffer command)
      (with-current-buffer buffer
        (if p
            (goto-char p)
          (unless start-revision (vc-hgcmd-show-log-entry nil)))))))

(defun vc-hgcmd--log-in-or-out (type buffer remote-location)
  "Log TYPE changesets for REMOTE-LOCATION to BUFFER."
  (apply #'vc-hgcmd-command-output-buffer buffer type (unless (string= "" remote-location) remote-location)))


(defun vc-hgcmd-log-outgoing (buffer remote-location)
  "Log outgoing for REMOTE-LOCATION to BUFFER."
  (vc-hgcmd--log-in-or-out "outgoing" buffer remote-location))

(defun vc-hgcmd-log-incoming (buffer remote-location)
  "Log incoming from REMOTE-LOCATION to BUFFER."
  (vc-hgcmd--log-in-or-out "incoming" buffer remote-location))

(defconst vc-hgcmd--message-re "^changeset:\\s-*\\(%s\\):\\([[:xdigit:]]+\\)")
(defconst vc-hgcmd--log-view-message-re (format vc-hgcmd--message-re "[[:digit:]]+"))
(defvar log-view-per-file-logs)
(defvar log-view-message-re)
(defvar log-view-font-lock-keywords)

(define-derived-mode vc-hgcmd-log-view-mode log-view-mode "Log-View/Hgcmd"
  (require 'add-log)
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-message-re) vc-hgcmd--log-view-message-re)
  (set (make-local-variable 'log-view-font-lock-keywords)
  (append
   log-view-font-lock-keywords
   '(
     ("^user:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	  (1 'change-log-name)
	  (2 'change-log-email))
	 ("^user:[ \t]+\\([A-Za-z0-9_.+-]+\\(?:@[A-Za-z0-9_.-]+\\)?\\)"
	  (1 'change-log-email))
	 ("^date: \\(.+\\)" (1 'change-log-date))
     ("^parent:[ \t]+\\([[:digit:]]+:[[:xdigit:]]+\\)" (1 'change-log-acknowledgment))
	 ("^tag: +\\([^ ]+\\)$" (1 'highlight))
	 ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message))))))

(defun vc-hgcmd-show-log-entry (revision)
  "Show log entry positioning on REVISION."
  ;; REVISION might be branch name while print-branch-log
  ;; if 'changeset: revision' not found try move to working rev
  (goto-char (point-min))
  (if (search-forward-regexp (format vc-hgcmd--message-re revision) nil t)
      (goto-char (match-beginning 0))
    (when (search-forward-regexp (format vc-hgcmd--message-re (vc-hgcmd-working-revision nil)) nil t)
      (goto-char (match-beginning 0)))))

(defun vc-hgcmd-diff (files &optional rev1 rev2 buffer _async)
  "Place diff of FILES between REV1 and REV2 into BUFFER."
  (let ((command (nconc (list "diff") (when rev1 (list "-r" rev1)) (when rev2 (list "-r" rev2)) files)))
    (apply #'vc-hgcmd-command-output-buffer buffer command)))

(defun vc-hgcmd-revision-completion-table (_files)
  "Return branches and tags as they are more usefull than file revisions."
  (letrec ((table (lazy-completion-table table (lambda () (nconc (vc-hgcmd--branches) (vc-hgcmd--tags))))))))

(defconst vc-hgcmd-annotate-re
  (concat
   "^\\(?: *[^ ]+ +\\)?\\([0-9]+\\) "
   "\\([0-9]\\{4\\}-[0-1][0-9]-[0-3][0-9]\\): "
   ))

(defconst vc-hgcmd-annotate-filename-re
  "^\\(?: *[^ ]+ +\\)?\\([0-9]+\\) [0-9]\\{4\\}-[0-1][0-9]-[0-3][0-9]\\( +\\([^:]+\\)\\):"
  )

(defun vc-hgcmd-annotate-command (file buffer &optional revision)
  "Annotate REVISION of FILE to BUFFER."
  (apply #'vc-hgcmd-command-output-buffer buffer
         (nconc
          (list "annotate" "-qdnuf")
          (when revision (list "-r" revision))
          (list file)))
  ;; hide filenames but keep it in properties
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at vc-hgcmd-annotate-filename-re)
          (add-text-properties (line-beginning-position) (line-end-position)
                               (list 'vc-hgcmd-annotate-filename (match-string-no-properties 3)
                                     'vc-hgcmd-annotate-revision (match-string-no-properties 1)))
          (delete-region (match-beginning 2) (match-end 2)))
        (forward-line)))))

(declare-function vc-annotate-convert-time "vc-annotate" (&optional time))

(defun vc-hgcmd-annotate-time ()
  "Return the time of the next line of annotation at or after point, as a floating point fractional number of days."
  (when (looking-at vc-hgcmd-annotate-re)
    (goto-char (match-end 0))
    (vc-annotate-convert-time
     (let ((str (match-string-no-properties 2)))
       (encode-time 0 0 0
                    (string-to-number (substring str 6 8))
                    (string-to-number (substring str 4 6))
                    (string-to-number (substring str 0 4)))))))

(defun vc-hgcmd-annotate-extract-revision-at-line ()
  "Return revision at line."
  (cons (get-text-property (point) 'vc-hgcmd-annotate-revision)
        (expand-file-name (get-text-property (point) 'vc-hgcmd-annotate-filename) (vc-hgcmd-root default-directory))))

(defun vc-hgcmd-create-tag (_dir name branchp)
  "Create tag NAME. If BRANCHP create named branch."
  (vc-hgcmd-command (if branchp "branch" "tag") name))

(defun vc-hgcmd-retrieve-tag (_dir name _update)
  "Update to branch NAME."
  (if (string-empty-p name)
      (vc-hgcmd-command "update")
    (vc-hgcmd-command "update" name)))

(defun vc-hgcmd-root (file)
  "Return root folder of repository for FILE."
  (vc-find-root file ".hg"))

(defun vc-hgcmd-previous-revision (_file rev)
  "Revison prior to REV."
  (unless (string= rev "0")
    (vc-hgcmd-command "id" "-n" "-r" (concat rev "^"))))

(defun vc-hgcmd-next-revision (_file rev)
  "Revision after REV."
  (let ((newrev (1+ (string-to-number rev))))
    (when (<= newrev (string-to-number (vc-hgcmd-command "tip" "-T" "{rev}")))
      (number-to-string newrev))))

(declare-function log-edit-mode "log-edit" ())

(defun vc-hgcmd--set-log-edit-summary ()
  "Set summary of commit message to 'merged ...' if commiting after merge."
  (let* ((parents (split-string (vc-hgcmd-command "log" "-r" "p1()+p2()" "--template" "{node}\\0{branch}\n") "\n"))
         (p1 (car parents))
         (p2 (cadr parents)))
    (when p2
      (let ((p1 (split-string p1 "\0"))
            (p2 (split-string p2 "\0")))
        (save-excursion
          (insert (concat "merged " (if (string= (cadr p1) (cadr p2)) (car p2) (cadr p2)))))))))

(defun vc-hgcmd-log-edit-toggle-close-branch ()
  "Toggle --close-branch commit option."
  (interactive)
  (log-edit-toggle-header "Close-branch" "yes"))

(defun vc-hgcmd-log-edit-toggle-amend ()
  "Toggle --amend commit option. If on, insert commit message of the previous commit."
  (interactive)
  (when (log-edit-toggle-header "Amend" "yes")
    (log-edit-set-header "Summary" (vc-hgcmd-command "log" "-l" "1" "--template" "{desc}"))))

(defvar vc-hgcmd-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'vc-hgcmd-log-edit-toggle-amend)
    (define-key map (kbd "C-c C-l") 'vc-hgcmd-log-edit-toggle-close-branch)
    map)
  "Keymap for log edit mode.")

(define-derived-mode vc-hgcmd-log-edit-mode log-edit-mode "Log-Edit/Hgcmd"
  "Major mode for editing Hgcmd log messages.

\\{vc-hgcmd-log-edit-mode-map}"
  ;; if there are two parents create maybe helpful commit message
  ;; it must be done in log-edit-hook
  (add-hook 'log-edit-hook #'vc-hgcmd--set-log-edit-summary t t))

(defun vc-hgcmd-delete-file (file)
  "Delete FILE."
  (vc-hgcmd-command "remove" "--force" file))

(defun vc-hgcmd-rename-file (old new)
  "Rename file from OLD to NEW using `hg mv'."
  (vc-hgcmd-command "move" old new))

(defun vc-hgcmd--file-unresolved-p (file)
  "Return t if FILE is in conflict state."
  (let ((out (vc-hgcmd-command "resolve" "-l" file)))
    (and out (eq (aref out 0) ?U))))

(defun vc-hgcmd--after-save-hook ()
  "After save hook. Mark file as resolved if vc state eq conflict and no smerge mode."
  (when (and buffer-file-name
             (not (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^<<<<<<< " nil t)))
             (yes-or-no-p (format "Hgcmd: Mark %s as resolved? " buffer-file-name)))
    (vc-hgcmd-mark-resolved (list buffer-file-name))
    (remove-hook 'after-save-hook #'vc-hgcmd--after-save-hook t)))

;; TODO It's really handy to autostart smerge but additional hg command will be called on every find-file
(defun vc-hgcmd-find-file-hook ()
  "Find file hook. Start smerge session if vc state eq conflict."
  (when (vc-hgcmd--file-unresolved-p buffer-file-name)
    (smerge-start-session)
    (add-hook 'after-save-hook #'vc-hgcmd--after-save-hook nil t)
    (vc-message-unresolved-conflicts buffer-file-name)))

;; TODO extra menu

;; TODO extra-dir-menu. update -C for example or commit --close-branch or --amend without changes

(defun vc-hgcmd-conflicted-files (&optional _dir)
  "List of files where conflict resolution is needed."
  (let ((out (vc-hgcmd-command "files" "set:unresolved()")))
    (and out (split-string out "\n"))))

(defun vc-hgcmd-find-ignore-file (file)
  "Return the ignore file of the repository of FILE."
  (expand-file-name ".hgignore" (vc-hgcmd-root file)))

(provide 'vc-hgcmd)

;;; vc-hgcmd.el ends here

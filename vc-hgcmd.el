;;; vc-hgcmd.el --- VC mercurial backend that uses hg command server -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: vc
;; URL: https://github.com/muffinmad/emacs-vc-hgcmd
;; Package-Version: 1.3.13
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
;; - vc-hgcmd can't show file renames in `vc-dir' yet
;;
;; - graph log is used for branch or root log
;;
;; - Unresolved conflict status for a file
;; Files with unresolved merge conflicts have appropriate status in `vc-dir'.
;; Also you can use `vc-find-conflicted-file' to find next file with unresolved merge conflict.
;;
;; - hg summary as `vc-dir' extra headers
;; hg summary command gives useful information about commit, update and phase states.
;;
;; - Current branch is displayed on mode line.
;; It's not customizable yet.
;;
;; - Amend and close branch commits
;; While editing commit message you can toggle --amend and --close-branch flags.
;;
;; - Merge branch
;; vc-hgcmd will ask for branch name to merge.
;;
;; - Default pull arguments
;; You can customize default hg pull command arguments.
;; By default it's --update. You can change it for particular pull by invoking `vc-pull' with prefix argument.
;;
;; - Branches and tags as revision completion table
;; Instead of list of all revisions of file vc-hgcmd provides list of named branches and tags.
;; It's very useful on `vc-retrieve-tag'.
;; You can specify -C to run hg update with -C flag and discard all uncommitted changes.
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
;; While committing merge changes commit message will be set to 'merged <branch>' if
;; different branch was merged or to 'merged <node>'.
;;
;; Additionally predefined commit message passed to custom function
;; `vc-hgcmd-log-edit-message-function' so one can change it.
;; For example, to include current task in commit message:
;;
;;     (defun my/hg-commit-message (original-message)
;;       (if org-clock-current-task
;;           (concat org-clock-current-task " " original-message)
;;         original-message))
;;
;;     (custom-set-variables
;;      '(vc-hgcmd-log-edit-message-function 'my/hg-commit-message))
;;
;; - Interactive command `vc-hgcmd-runcommand' that allow to run custom hg commands


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

(defcustom vc-hgcmd-log-edit-message-function nil
  "Function to return string that will be used as initial value for log edit.
First param will be commit message computed by backend: 'merged <branch>' if
named branch was merged to current branch or 'merged <node>' if two heads on
same branch was merged."
  :type '(choice
          (function)
          (const :tag "Default commit message" nil)))


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

(cl-defstruct (vc-hgcmd--command (:copier nil)) command output-buffer skip-error result-code wait callback callback-args)

(defvar-local vc-hgcmd--current-command nil
  "Current running hgcmd command. Future commands will wait until the current command will finish.")
(put 'vc-hgcmd--current-command 'permanent-local t)

(defun vc-hgcmd--project-name (dir)
  "Get project name based on DIR."
  (file-name-nondirectory (directory-file-name dir)))

(defun vc-hgcmd--read-output ()
  "Parse process output in current buffer."
  ;; When some hg extension like hghooks 0.7.0 use 'print' then we will recieve output without channel.
  ;; So let's find '<channel><length>' pattern first and all output before it send to 'o' channel.
  ;; Suppose that length value will be less 2 ** 24 so there are always be \0 after channel.
  (goto-char 1)
  (when (search-forward-regexp "[oedrLI]\0\\(.\\|\n\\)\\{3\\}" nil t)
    (if (> (point) 6)
        (let ((data (decode-coding-string (buffer-substring-no-properties 1 (- (point) 5)) 'utf-8))
              (inhibit-read-only t))
          (delete-region 1 (- (point) 5))
          (cons ?o data))
      (let* ((data (bindat-unpack '((c byte) (d u32)) (vconcat (buffer-substring-no-properties 1 6))))
             (channel (bindat-get-field data 'c))
             (size (bindat-get-field data 'd)))
        (cond ((memq channel '(?o ?e ?d ?r))
               (when (> (point-max) (+ 5 size))
                 (let ((data (vconcat (buffer-substring-no-properties 6 (+ 6 size))))
                       (inhibit-read-only t))
                   (delete-region 1 (+ 6 size))
                   (cons channel (if (eq channel ?r)
                                     (bindat-get-field (bindat-unpack `((f u32)) data) 'f)
                                   (decode-coding-string
                                    (bindat-get-field (bindat-unpack `((f str ,(length data))) data) 'f)
                                    'utf-8))))))
              ((memq channel '(?I ?L))
               (let ((inhibit-read-only t))
                 (delete-region 1 6))
               (cons channel size)))))))

(defun vc-hgcmd--cmdserver-process-filter (process output)
  "Filter OUTPUT for hg cmdserver PROCESS.
Insert output to process buffer and check if amount of data is enought to parse it to output buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (let ((inhibit-read-only t)) (insert output))
        (let ((current-command (or vc-hgcmd--current-command
                                   (error "Hgcmd process output without command: %s" output))))
          (while
              (let ((data (vc-hgcmd--read-output)))
                (when data
                  (let ((channel (car data))
                        (data (cdr data)))
                    (cond ((memq channel '(?o ?e ?d))
                           (unless (and (eq channel ?e) (vc-hgcmd--command-skip-error current-command))
                             (let ((output-buffer (vc-hgcmd--command-output-buffer current-command)))
                               (when (or (stringp output-buffer) (buffer-live-p output-buffer))
                                 (with-current-buffer output-buffer
                                   (let ((inhibit-read-only t))
                                     (goto-char (point-max))
                                     (insert data)))))))
                          ((eq channel ?r)
                           (setf (vc-hgcmd--command-result-code current-command) data)
                           (setq vc-hgcmd--current-command nil)
                           (let ((output-buffer (vc-hgcmd--command-output-buffer current-command))
                                 (callback (vc-hgcmd--command-callback current-command))
                                 (args (vc-hgcmd--command-callback-args current-command)))
                             (when (or (stringp output-buffer) (buffer-live-p output-buffer))
                               (with-current-buffer output-buffer
                                 (setq mode-line-process nil)
                                 (when callback
                                   (if args (funcall callback args) (funcall callback)))))))
                          ;; TODO: cmdserver clients must handle I and L channels
                          (t (error (format "Hgcmd unhandled channel %c" channel)))))
                  t))))))))

(defun vc-hgcmd--cmdserver-process-sentinel (process _event)
  "Will listen for PROCESS events and kill process buffer if process killed."
  (unless (process-live-p process)
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when vc-hgcmd--current-command
            ;; process has died but command waits for output
            (vc-hgcmd--cmdserver-process-filter process (bindat-pack
                                                         '((c byte) (l u32) (v u32))
                                                         '((c . ?r) (l . 4) (v . 255))))))
        (kill-buffer buffer)))))

(defun vc-hgcmd--repo-dir ()
  "Get repo dir."
  (abbreviate-file-name (or (vc-hgcmd-root default-directory) default-directory)))

(defun vc-hgcmd--process-buffer ()
  "Get hg cmdserver process buffer for repo in `default-directory'."
  (let ((dir (vc-hgcmd--repo-dir)))
    (or
     (let ((buffer (gethash dir vc-hgcmd--process-buffers-by-dir)))
       (when (buffer-live-p buffer) buffer))
     (puthash
      dir
      (with-current-buffer (generate-new-buffer (concat " *hgcmd process: " (vc-hgcmd--project-name dir) "*"))
        (setq default-directory dir)
        (vc-hgcmd-process-mode)
        (let* ((process-environment (append vc-hgcmd-cmdserver-process-environment process-environment))
               (process-connection-type nil)
               (process
                (condition-case nil
                    (apply
                     #'start-file-process
                     (concat "vc-hgcmd process: " (vc-hgcmd--project-name default-directory))
                     (current-buffer)
                     vc-hgcmd-hg-executable
                     vc-hgcmd-cmdserver-args)
                  (error nil))))
          ;; process will be nil if hg executable not found
          (when (process-live-p process)
            (set-process-sentinel process #'ignore)
            (set-process-query-on-exit-flag process nil)
            (set-process-coding-system process 'no-conversion 'no-conversion)
            ;; read hello message
            ;; TODO parse encoding
            ;; check process again because it can be tramp sh process with output like "env: hg not found"
            (while (and (process-live-p process) (not (vc-hgcmd--read-output)))
              (accept-process-output process 0.1 nil t))
            (when (process-live-p process)
              (set-process-filter process #'vc-hgcmd--cmdserver-process-filter)
              (set-process-sentinel process #'vc-hgcmd--cmdserver-process-sentinel))))
        (current-buffer))
      vc-hgcmd--process-buffers-by-dir))))

(defun vc-hgcmd--output-buffer (command)
  "Get and display hg output buffer for COMMAND."
  (let* ((dir (vc-hgcmd--repo-dir))
         (buffer
          (or (seq-find (lambda (buffer)
                          (with-current-buffer buffer
                            (and (eq major-mode 'vc-hgcmd-output-mode)
                                 (equal (abbreviate-file-name default-directory) dir))))
                        (buffer-list))
              (let ((buffer (generate-new-buffer (concat "*hgcmd output: " (vc-hgcmd--project-name dir) "*"))))
                (with-current-buffer buffer
                  (setq default-directory dir)
                  (vc-hgcmd-output-mode))
                buffer))))
    (let ((window (display-buffer buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (eq (point) (point-min)) (insert "\n"))
          (set-window-start window (point))
          (insert (concat "Running \"" (mapconcat #'identity command " ") "\"...\n")))))
    buffer))

(defun vc-hgcmd--encode-command-arg (arg)
  "Encode command ARG."
  (encode-coding-string arg 'utf-8))

(defun vc-hgcmd--run-command (cmd)
  "Run hg CMD."
  (let* ((buffer (vc-hgcmd--process-buffer))
         (process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (when vc-hgcmd--current-command
        (user-error "Hg command \"%s\" is active" (car (vc-hgcmd--command-command vc-hgcmd--current-command))))
      (when (process-live-p process)
        (let* ((tty (process-tty-name process))
               (command (vc-hgcmd--command-command cmd))
               (output-buffer (or (vc-hgcmd--command-output-buffer cmd)
                                  (setf (vc-hgcmd--command-output-buffer cmd) (vc-hgcmd--output-buffer command)))))
          (setq vc-hgcmd--current-command cmd)
          (when (or (stringp output-buffer) (buffer-live-p output-buffer))
            (with-current-buffer output-buffer
              (setq mode-line-process
                    (propertize (format " [running %s...]" (car (vc-hgcmd--command-command cmd)))
                                'face 'mode-line-emphasis
                                'help-echo
                                "A command is in progress in this buffer"))))
          (process-send-string
           process
           (concat
            "runcommand\n"
            (let* ((args (mapconcat #'vc-hgcmd--encode-command-arg command "\0"))
                   (binary-data (bindat-pack '((l u32)) `((l . ,(length args))))))
              (concat (if tty
                          (mapconcat #'identity (mapcar (lambda (c) (concat "\x16" (char-to-string c))) binary-data) "")
                        binary-data)
                      args))))
          (when tty
            (process-send-eof process)))
        (when (vc-hgcmd--command-wait cmd)
          (while vc-hgcmd--current-command
            (accept-process-output process 0.1 nil t)))
        t))))

(defun vc-hgcmd--command (skip-error &rest command)
  "Run hg COMMAND and return it's output. If SKIP-ERROR is non nil data on error channel will be omited."
  (with-temp-buffer
    (let ((cmd (make-vc-hgcmd--command :command command :output-buffer (current-buffer) :wait t :skip-error skip-error)))
      (when (vc-hgcmd--run-command cmd)
        (let ((result (string-trim-right (buffer-string))))
          ;; TODO min result code for each command that is not error
          (if (= (vc-hgcmd--command-result-code cmd) 255)
              (with-current-buffer (vc-hgcmd--output-buffer command)
                (goto-char (point-max))
                (let ((inhibit-read-only t))
                  (insert (concat result "\n")))
                nil)
            (when (> (length result) 0)
              result)))))))

(defun vc-hgcmd-command (&rest command)
  "Run hg COMMAND and return it's output."
  (apply #'vc-hgcmd--command (nconc (list nil) command)))

(defun vc-hgcmd-command-to-buffer (buffer &rest command)
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
  "Run COMMAND and update current buffer after command finished."
  (vc-hgcmd--run-command
   (make-vc-hgcmd--command
    :command command
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
  (split-string (vc-hgcmd-command "branches" "-T" "{branch}\\n") "\n"))

(defun vc-hgcmd--tags ()
  "Return tags list."
  (split-string (vc-hgcmd-command "tags" "-q") "\n"))

(defun vc-hgcmd--file-relative-name (file)
  "Return FILE file name relative to vc root."
  (file-relative-name file (vc-hgcmd-root file)))

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
    (or (file-directory-p file)
        ;; vc-registered is called for buffer-file-name and
        ;; shortly then after for truename. Update default-dir so
        ;; 'hg state' will be called in right repo
        (let ((state
               (let ((default-directory (file-name-directory (expand-file-name file))))
                 (vc-hgcmd-state file))))
          (and state (not (memq state '(ignored unregistered))))))))

(defun vc-hgcmd-state (file)
  "State for FILE."
  (let ((out (vc-hgcmd--command t "status" "-A" (vc-hgcmd--file-relative-name file))))
    (when out
      (let ((state (cdr (assoc (aref out 0) vc-hgcmd--translation-status))))
        (if (and (eq state 'edited) (vc-hgcmd--file-unresolved-p file))
            'conflict
          state)))))

(defun vc-hgcmd--dir-status-callback (update-function)
  "Call UPDATE-FUNCTION with result of status command."
  (let* ((conflicted (vc-hgcmd-conflicted-files))
         (result (mapcar (lambda (file)
                           (list file 'conflict nil))
                         conflicted)))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((file (buffer-substring-no-properties (+ (point) 2) (line-end-position))))
        (unless (member file conflicted)
          (push (list file (cdr (assoc (char-after) vc-hgcmd--translation-status)) nil) result)))
      (forward-line))
    (funcall update-function result)))

(defun vc-hgcmd-dir-status-files (dir files update-function)
  "Call UPDATE-FUNCTION with status for files in DIR or FILES."
  ;; TODO track file renames with -C option
  (let ((command (if files
                     (nconc (list "status" "-A") (mapcar #'vc-hgcmd--file-relative-name files))
                   (list "status" (vc-hgcmd--file-relative-name dir)))))
    (vc-hgcmd--run-command
     (make-vc-hgcmd--command
      :command command
      :output-buffer (current-buffer)
      :callback #'vc-hgcmd--dir-status-callback
      :callback-args update-function
      :skip-error t))))

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
  (let* ((parents (vc-hgcmd-command "log" "-r" "p1()+p2()" "--template" "{rev}:{node|short}\\0{branch}\\0{tags}\\0{desc|firstline}\\n"))
         (result (when parents
                   (apply #'concat (mapcar #'vc-hgcmd--parent-info (split-string parents "\n"))))))
    (with-temp-buffer
      (when (vc-hgcmd--run-command (make-vc-hgcmd--command :command (list "summary") :output-buffer (current-buffer) :wait t))
        (concat result
                (unless parents
                  (vc-hgcmd--summary-info "parent" "Parent     : "))
                (vc-hgcmd--summary-info "branch" "Branch     : ")
                (vc-hgcmd--summary-info "commit" "Commit     : ")
                (vc-hgcmd--summary-info "update" "Update     : ")
                (vc-hgcmd--summary-info "phases" "Phases     : "))))))

;; TODO dir-printer
;; TODO status-fileinfo-extra

(defun vc-hgcmd-working-revision (file)
  "Working revision. Return repository working revision if FILE is committed."
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
  (apply #'vc-hgcmd-command (nconc (list "add") (mapcar #'vc-hgcmd--file-relative-name files))))

(defalias 'vc-hgcmd-responsible-p 'vc-hgcmd-root)

;; TODO receive-file

(defun vc-hgcmd-unregister (file)
  "Forget FILE."
  (vc-hgcmd-command "forget" (vc-hgcmd--file-relative-name file)))

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
                                    comment)
          (mapcar #'vc-hgcmd--file-relative-name files))))

(defun vc-hgcmd-find-revision (file rev buffer)
  "Put REV of FILE to BUFFER."
  (let ((file (vc-hgcmd--file-relative-name file)))
    (apply #'vc-hgcmd-command-to-buffer buffer (if rev (list "cat" "-r" rev file) (list "cat" file)))))

(defun vc-hgcmd-checkout (file &optional rev)
  "Retrieve revision REV of FILE."
  (vc-hgcmd-find-revision file rev (or (get-file-buffer file) (current-buffer))))

(defun vc-hgcmd-revert (file &optional contents-done)
  "Refert FILE if not CONTENTS-DONE."
  (unless contents-done
    (vc-hgcmd-command "revert" (vc-hgcmd--file-relative-name file))))

(defun vc-hgcmd-merge-branch ()
  "Merge."
  (let ((branch (completing-read "Merge from branch: " (vc-hgcmd-revision-completion-table))))
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
  (apply #'vc-hgcmd-command (nconc (list "resolve" "-m") (mapcar #'vc-hgcmd--file-relative-name files))))

(defun vc-hgcmd-print-log (files buffer &optional shortlog start-revision limit)
  "Put maybe SHORTLOG log of FILES to BUFFER starting with START-REVISION limited by LIMIT."
  ;; TODO short log
  (let ((command
         (nconc
          (list "log")
          (when shortlog (list "-G"))
          (when start-revision
            ;; start revision is used for branch log or specific revision log when limit is 1
            (list (if (eq limit 1) "-r" "-b") start-revision))
          (when limit (list "-l" (number-to-string limit)))
          (unless (or shortlog (eq limit 1)) (list "-f")) ; follow file renames
          (unless (equal files (list default-directory)) (mapcar #'vc-hgcmd--file-relative-name files)))))
    ;; If limit is 1 or vc-log-show-limit then it is initial diff and better move to working revision
    ;; otherwise remember point position and restore it later
    (let ((p (with-current-buffer buffer (unless (or (member limit (list 1 vc-log-show-limit))) (point)))))
      (apply #'vc-hgcmd-command-to-buffer buffer command)
      (with-current-buffer buffer
        (if p
            (goto-char p)
          (unless start-revision (vc-hgcmd-show-log-entry nil)))))))

(defun vc-hgcmd--log-in-or-out (type buffer remote-location)
  "Log TYPE changesets for REMOTE-LOCATION to BUFFER."
  (apply #'vc-hgcmd-command-to-buffer buffer type (unless (string= "" remote-location) remote-location)))


(defun vc-hgcmd-log-outgoing (buffer remote-location)
  "Log outgoing for REMOTE-LOCATION to BUFFER."
  (vc-hgcmd--log-in-or-out "outgoing" buffer remote-location))

(defun vc-hgcmd-log-incoming (buffer remote-location)
  "Log incoming from REMOTE-LOCATION to BUFFER."
  (vc-hgcmd--log-in-or-out "incoming" buffer remote-location))

(defun vc-hgcmd--graph-data-re (re)
  "Add graph data re to RE."
  (concat "^\\(?:[o@_x*+-|/: ]*\\)" re))

(defconst vc-hgcmd--message-re (vc-hgcmd--graph-data-re "changeset:\\s-*\\(%s\\):\\([[:xdigit:]]+\\)"))
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
        `(
          (,(vc-hgcmd--graph-data-re "user:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]")
	       (1 'change-log-name)
	       (2 'change-log-email))
	      (,(vc-hgcmd--graph-data-re "user:[ \t]+\\([A-Za-z0-9_.+-]+\\(?:@[A-Za-z0-9_.-]+\\)?\\)")
	       (1 'change-log-email))
	      (,(vc-hgcmd--graph-data-re "date: \\(.+\\)") (1 'change-log-date))
          (,(vc-hgcmd--graph-data-re "parent:[ \t]+\\([[:digit:]]+:[[:xdigit:]]+\\)") (1 'change-log-acknowledgment))
	      (,(vc-hgcmd--graph-data-re "tag: +\\([^ ]+\\)$") (1 'highlight))
	      (,(vc-hgcmd--graph-data-re "summary:[ \t]+\\(.+\\)") (1 'log-view-message))))))

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
  (let ((command (nconc
                  (list "diff")
                  (when rev1 (list "-r" rev1))
                  (when rev2 (list "-r" rev2))
                  (unless (equal files (list default-directory)) (mapcar #'vc-hgcmd--file-relative-name files)))))
    (apply #'vc-hgcmd-command-to-buffer buffer command)))

(defun vc-hgcmd-revision-completion-table (&optional _files)
  "Return branches and tags as they are more usefull than file revisions."
  (letrec ((table (lazy-completion-table table (lambda () (nconc (list "") (vc-hgcmd--branches) (vc-hgcmd--tags))))))))

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
  (apply #'vc-hgcmd-command-to-buffer buffer
         (nconc
          (list "annotate" "-qdnuf")
          (when revision (list "-r" revision))
          (list (vc-hgcmd--file-relative-name file))))
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

(defun vc-hgcmd--log-edit-default-message ()
  "Return 'merged ...' if there are two parents."
  (let* ((parents (split-string (vc-hgcmd-command "log" "-r" "p1()+p2()" "--template" "{node}\\0{branch}\\n") "\n"))
         (p1 (car parents))
         (p2 (cadr parents)))
    (when p2
      (let ((p1 (split-string p1 "\0"))
            (p2 (split-string p2 "\0")))
        (concat "merged " (if (string= (cadr p1) (cadr p2)) (car p2) (cadr p2)))))))

(defun vc-hgcmd--set-log-edit-summary ()
  "Set summary of commit message to 'merged ...' if committing after merge."
  (let* ((message (or (vc-hgcmd--log-edit-default-message) ""))
         (message (if vc-hgcmd-log-edit-message-function
                      (funcall vc-hgcmd-log-edit-message-function message)
                    message)))
    (when (and message (length message))
      (save-excursion
        (insert message)))))

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
  (vc-hgcmd-command "remove" "--force" (vc-hgcmd--file-relative-name file)))

(defun vc-hgcmd-rename-file (old new)
  "Rename file from OLD to NEW using `hg mv'."
  (vc-hgcmd-command "move" old new))

(defun vc-hgcmd--file-unresolved-p (file)
  "Return t if FILE is in conflict state."
  (let ((out (vc-hgcmd-command "resolve" "-l" (vc-hgcmd--file-relative-name file))))
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

(defun vc-hgcmd-runcommand (command)
  "Run custom hg COMMAND."
  (interactive "sRun hg: ")
  (vc-hgcmd-command-update-callback (split-string-and-unquote command)))

(provide 'vc-hgcmd)

;;; vc-hgcmd.el ends here

;;; straight-watcher.el --- filesystem watcher  -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'cl-lib)
(require 'filenotify)

;;; Code:
;;;; Declarations
(declare-function straight--determine-repo  "straight")
(declare-function straight--directory-files "straight")
(declare-function straight--file            "straight")
(declare-function straight--repos-dir       "straight")
(defvar straight-safe-mode)

;;;; Variables
(defvar straight-watcher-file "modified-repos.el" "Modified repos file name.")
(defvar straight-watchers nil "List of registered notification objects.")
(defvar straight-watcher-modified-repos nil "List of modified repositories.")
(defvar straight-watcher-timers (make-hash-table :test 'equal)
  "Hash of REPO -> timers.")

;;;; Customizations
(defcustom straight-watcher-debounce-interval 0.5
  "Length of time to wait before registering a change for a repo.
See `run-at-time' for acceptable values."
  :group 'straight
  :type (or 'string 'int 'float))

(defcustom straight-watcher-process-buffer "*straight-watcher*"
  "Name of buffer to use for the filesystem watcher."
  :group 'straight
  :type 'string)

;;;; Functions
(defun straight-watcher--load-repos ()
  "Read `straight-watcher-file' into `straight-modified-repos'."
  (with-current-buffer (find-file-noselect
                        (straight--file straight-watcher-file))
    (setq straight-watcher-modified-repos (read (buffer-string)))))
;;(straight-watcher--load-repos)

(defun straight-watcher--directories ()
  "Return a list of directories to register file notification watchers on."
  (cl-remove-if-not
   (lambda (dir) (and (file-directory-p dir)
                      (not (string-match-p "\\.github" dir))))
   (cl-reduce #'append
              (mapcar (lambda (dir)
                        (append (list dir)
                                (straight--directory-files dir nil 'full)))
                      (straight--directory-files (straight--repos-dir)
                                                 nil 'full)))))
;;(straight-watcher--directories)

(defun straight-watcher--write-changed ()
  "Write changed repos to `straight-watcher-file'."
  (let ((path (straight--file straight-watcher-file)))
    (with-current-buffer (find-file-noselect path 'nowarn nil)
      (let ((inhibit-read-only t)
            (print-level nil)
            (print-length nil)
            (print-quoted t)
            (coding-system-for-write 'utf-8)
            (standard-output (current-buffer)))
        (erase-buffer)
        (print straight-watcher-modified-repos))
      (write-file path))))

(defun straight-watcher--add-watches (files callback)
  "Add file system watchers to FILES.
CALLBACK is called with a `file-notify' event as its sole argument."
  (mapc (lambda (f)
          (setq straight-watchers
                (push (file-notify-add-watch f '(change) callback)
                      straight-watchers)))
        files))

(defun straight-watcher-register-change (repo)
  "Register REPO change event."
  (message "%s %s changed"
           (format-time-string "[%Y-%m-%d %H:%M:%S]")
           repo)
  (remhash repo straight-watcher-timers)
  (setq straight-watcher-modified-repos
        (delete-dups (push repo straight-watcher-modified-repos)))
  (straight-watcher--write-changed))

(defun straight-watcher--register-change-maybe (event)
  "Set up `straight-watcher-register-change' for proper EVENTs."
  (when (eq (nth 1 event) 'changed)
    (let* ((repo (straight--determine-repo (nth 2 event)))
           (timer (gethash repo straight-watcher-timers)))
      (when timer
        (cancel-timer timer)
        (remhash repo straight-watcher-timers))
      (puthash repo (run-at-time straight-watcher-debounce-interval
                                 nil
                                 #'straight-watcher-register-change
                                 repo)
               straight-watcher-timers))))

;;;; Commands
;;;###autoload
;;@INCOMPLETE:
;; - implement local vs child process
;; - kill previous instances
(defun straight-watcher-start (&optional _local)
  "Start the filesystem watcher, killing any previous instance.
If LOCAL is non-nil, the watcher is launched from the current Emacs process.
Else, it is launched in a persistent child process.
If the watcher fails to start, signal a warning and return nil."
  (interactive "P")
  (unless straight-safe-mode
    (straight-watcher--add-watches (straight-watcher--directories)
                                   #'straight-watcher--register-change-maybe)))

;;;###autoload
;;@INCOMPLETE:
;; - kill child process (once implemented)
(defun straight-watcher-stop ()
  "Kill the filesystem watcher, if it is running.
If there is an unexpected error, signal a warning and return nil."
  (interactive)
  (unless straight-safe-mode
    (while straight-watchers
      (file-notify-rm-watch (pop straight-watchers)))))

(provide 'straight-watcher)

;;; straight-watcher.el ends here

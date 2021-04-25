;;; straight-watcher.el --- filesystem watcher  -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'filenotify)
;;(require 'straight)
;;; Declarations
(defvar straight--build-cache)
(defvar straight-safe-mode)
(declare-function straight--determine-repo   "straight")
(declare-function straight--hash-repo-files  "straight")
(declare-function straight--repos-dir        "straight")
(declare-function straight--directory-files  "straight")
(declare-function straight--file             "straight")
(declare-function straight--load-build-cache "straight")

;;; Code:
;;;; Variables
(defvar straight-watcher-file "watcher-cache.el"
  "File name for the watcher's repo contents cache.")
(defvar straight-watchers nil "List of registered notification objects.")
(defvar straight-watcher-repos (make-hash-table :test 'equal)
  "Hash table of observed repositories.")
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
  "Read `straight-watcher-file' into `straight-watcher-repos'."
  (with-current-buffer (find-file-noselect
                        (straight--file straight-watcher-file))
    (setq straight-watcher-repos (read (buffer-string)))))

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
        (print straight-watcher-repos))
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
  (unless straight--build-cache (straight--load-build-cache))
  (when (straight-watcher-repo-modified-p repo)
    (puthash repo (straight--hash-repo-files repo) straight-watcher-repos))
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

(defun straight-watcher-repo-modified-p (package)
  "Return t if PACKAGE's repo hash does not match `straight--build-cache'."
  (unless straight-watcher-repos (straight-watcher--load-repos))
  ;; File may be missing.
  (when straight-watcher-repos
    (when-let ((build (nth 0 (gethash package straight--build-cache)))
               (hash (gethash package straight-watcher-repos)))
      (not (equal build hash)))))

(defun straight-watcher-modified-repos ()
  "Return a list of modified repos."
  (unless straight-watcher-repos (straight-watcher--load-repos))
  (let (repos)
    (maphash
     (lambda (repo hash)
       (unless (equal hash (nth 0 (gethash repo straight--build-cache)))
         (setq repos (push repo repos))))
     straight-watcher-repos)
    repos))

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
    (straight-watcher--add-watches
     (straight-watcher--directories)
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

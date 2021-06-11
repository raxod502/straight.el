;;; bootstrap.el --- Initialize straight.el -*- lexical-binding: t -*-

;; We need these libraries in this file, bootstrap.el. The libraries
;; for straight.el are required in that file.
(require 'bytecomp)
(require 'cl-lib)

(let* ((bootstrap.el
        ;; If this file is accessed through a symlink (this may happen
        ;; when an old version of the bootstrap snippet is used to
        ;; load straight.el), resolve it. We need to be looking at
        ;; the actual file, since the eventual target of the
        ;; symlink is the only way we can actually identify the
        ;; straight.el repository (which might be called something
        ;; else).
        (file-truename
         (or
          ;; If the file is being loaded from the init-file.
          load-file-name
          ;; If the file is being evaluated with something like
          ;; `eval-buffer'.
          buffer-file-name)))
       (straight.el
        (expand-file-name
         "straight.el" (file-name-directory bootstrap.el))))
  ;; This logic replicates that in `straight--build-compile',
  ;; and is used to silence byte-compile warnings and other cruft.
  (cl-letf (((symbol-function #'save-some-buffers) #'ignore)
            ((symbol-function #'byte-compile-log-1) #'ignore)
            ((symbol-function #'byte-compile-log-file) #'ignore)
            ((symbol-function #'byte-compile-log-warning) #'ignore))
    (let ((byte-compile-warnings nil)
          (byte-compile-verbose nil)
          (message-log-max nil)
          (inhibit-message t)
          (emacs-version-changed t))
      ;; Argument 0 means (for some reason) to byte-compile even if
      ;; the .elc file does not already exist (but not if the .elc
      ;; file is at least as new as the .el file).
      (byte-recompile-file straight.el nil 0)
      (catch 'emacs-version-changed
        ;; straight.el has a fun hack that throws
        ;; `emacs-version-changed' if the version of Emacs has changed
        ;; since the last time it was byte-compiled. This prevents us
        ;; from accidentally loading invalid byte-code, hopefully.
        (load (file-name-sans-extension
               (expand-file-name straight.el default-directory))
              nil 'nomessage)
        (setq emacs-version-changed nil))
      (when emacs-version-changed
        ;; In safe mode, sacrifice performance for safety.
        (if (bound-and-true-p straight-safe-mode)
            (load straight.el nil 'nomessage 'nosuffix)
          ;; Don't use the optional LOAD argument for
          ;; `byte-compile-file' because it emits a message.
          (byte-compile-file straight.el)
          (load (file-name-sans-extension
                 (expand-file-name straight.el default-directory))
                nil 'nomessage))))))

;; This assures the byte-compiler that we know what we are doing when
;; we reference functions and variables from straight.el below. It
;; does not actually do anything at runtime, since the `straight'
;; feature has already been provided by loading straight.elc above.
(require 'straight)

;; In case this is a reinit, and straight.el was already loaded, we
;; have to explicitly clear the caches.
(straight--reset-caches)

;; We start by registering the default recipe repositories. This is
;; done first so that any dependencies of straight.el can be looked up
;; correctly.

;; This is kind of aggressive but we really don't have a good
;; mechanism at present for customizing the default recipe
;; repositories anyway. So don't even try to cater to that use case.
(setq straight-recipe-repositories nil)

(straight-use-recipes '(org-elpa :local-repo nil))

(straight-use-recipes '(melpa :type git :host github
                              :repo "melpa/melpa"
                              :build nil))

(if straight-recipes-gnu-elpa-use-mirror
    (straight-use-recipes
     '(gnu-elpa-mirror :type git :host github
                       :repo "emacs-straight/gnu-elpa-mirror"
                       :build nil))
  (straight-use-recipes `(gnu-elpa :type git
                                   :repo ,straight-recipes-gnu-elpa-url
                                   :local-repo "elpa"
                                   :build nil)))

(straight-use-recipes '(el-get :type git :host github
                               :repo "dimitri/el-get"
                               :build nil))

(if straight-recipes-emacsmirror-use-mirror
    (straight-use-recipes
     '(emacsmirror-mirror :type git :host github
                          :repo "emacs-straight/emacsmirror-mirror"
                          :build nil))
  (straight-use-recipes '(emacsmirror :type git :host github
                                      :repo "emacsmirror/epkgs"
                                      :nonrecursive t
                                      :build nil)))

;; Then we register (and build) straight.el itself.
(straight-use-package `(straight :type git :host github
                                 :repo ,(format "%s/straight.el"
                                                straight-repository-user)
                                 :files ("straight*.el")
                                 :branch ,straight-repository-branch))

(if (straight--modifications 'check-on-save)
    (straight-live-modifications-mode +1)
  (straight-live-modifications-mode -1))

(when (straight--modifications 'watch-files)
  (declare-function straight-watcher-start "straight-watcher")
  (straight-watcher-start))

(if straight-use-symlinks
    (straight-symlink-emulation-mode -1)
  (straight-symlink-emulation-mode +1))

(if straight-enable-package-integration
    (straight-package-neutering-mode +1)
  (straight-package-neutering-mode -1))

(if straight-enable-use-package-integration
    (straight-use-package-mode +1)
  (straight-use-package-mode -1))

;;; bootstrap.el ends here

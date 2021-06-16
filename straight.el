;;; straight.el --- Next-generation package manager -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Radon Rosborough and contributors

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 1 Jan 2017
;; Homepage: https://github.com/raxod502/straight.el
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT
;; Version: prerelease

;;; Commentary:

;; straight.el is a next-generation package manager for Emacs. It
;; clones packages into your ~/.emacs.d and handles byte-compilation,
;; autoload generation, and load path management. Dependency
;; management, powerful tools for managing your packages in bulk, and
;; out-of-the-box compatibility with MELPA, GNU ELPA, and Emacsmirror
;; are also included.

;; straight.el improves on other package managers in several ways.
;; Most importantly, it offers first-class support for easily
;; modifying packages and contributing your changes upstream. It also
;; supports complete reproducibility for your configuration by writing
;; a lockfile with the versions of all your packages. Alternatively,
;; straight.el will work with manually managed packages, if you prefer
;; to merge in packages as subtrees.

;; Please see https://github.com/raxod502/straight.el for more
;; information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Detect change in Emacs version

;; This throws an error if you byte-compile it with one Emacs version
;; and then try to run the byte-compiled code in another Emacs
;; version. See bootstrap.el.
(eval
 `(unless (equal
           (emacs-version)
           ,(eval-when-compile (emacs-version)))
    (throw 'emacs-version-changed nil)))

(defun straight--executable-find (name)
  "`executable-find' NAME. If not found, throw an error."
  (or (executable-find name)
      (error "Straight unable to find required executable: %S" name)))

(straight--executable-find "git")

;;;; Libraries

(require 'cl-lib)
(require 'subr-x)

;;;; Backports

;; Note that we use `eval-and-compile' even for macros, because
;; otherwise libraries which load the byte-compiled version of this
;; file won't be able to use those macros.

;; Not defined before Emacs 25.3
(eval-and-compile
  (unless (boundp 'inhibit-message)
    (defvar inhibit-message nil
      "Non-nil means calls to ‘message’ are not displayed.
They are still logged to the *Messages* buffer.")))

;;;; Functions from other packages

;; `comp'
(declare-function native-compile-async "comp.el")
(defvar native-comp-deferred-compilation-deny-list)

;; `finder-inf'
(defvar package--builtins)

;; `flycheck'
(declare-function flycheck-checker-get "flycheck")
(declare-function flycheck-get-next-checker-for-buffer "flycheck")
(declare-function flycheck-start-current-syntax-check "flycheck")

;; `package'
(defvar package-selected-packages)
(declare-function package--ensure-init-file "package")
(declare-function package--save-selected-packages "package")

;; `use-package'
(defvar use-package-defaults)
(defvar use-package-ensure-function)
(defvar use-package-keywords)
(defvar use-package-pre-ensure-function)
(declare-function use-package-as-symbol "use-package")
(declare-function use-package-error "use-package")
(declare-function use-package-handler/:ensure "use-package")
(declare-function use-package-normalize/:ensure "use-package")
(declare-function use-package-only-one "use-package")
(declare-function use-package-process-keywords "use-package")

;;;; Customization variables

(defgroup straight nil
  "Next-generation, purely functional package manager for the Emacs hacker."
  :group 'applications
  :prefix "straight-")

(defgroup straight-faces nil
  "Faces used in straight.el."
  :group 'straight
  :group 'faces)

(defface straight-process-error
  '((t (:weight bold :foreground "red")))
  "Face for process errors in `straight-process-buffer'."
  :group 'straight-faces)

(defface straight-process-command
  '((t (:weight bold)))
  "Face for process commands in `straight-process-buffer'."
  :group 'straight-faces)

(defcustom straight-arrow
  (if (char-displayable-p ?→) " → " " -> ")
  "The string to use for an arrow in messages."
  :type 'string)

(defcustom straight-profiles
  '((nil . "default.el"))
  "Alist mapping package profile names to version lockfile names.
The profile names should be symbols, and the filenames may be
relative (to straight/versions/) or absolute."
  :type '(alist :key-type symbol :value-type string))

(defcustom straight-current-profile nil
  "Symbol identifying the current package profile.
This symbol should have an entry in `straight-profiles'. If you
wish to take advantage of the multiple-profile system, you should
bind this variable to different symbols using `let' over
different parts of your init-file."
  :type 'symbol)

(defcustom straight-repository-user "raxod502"
  "String identifying the GitHub user from which to clone straight.el.
You must set this variable before straight.el is bootstrapped for
it to have an effect. (It is used in the default recipe for
straight.el which is registered during bootstrap.)

If you have forked raxod502/straight.el to your-name/straight.el,
then to use your fork you should set `straight-repository-user'
to \"your-name\"."
  :type 'string)

(defcustom straight-repository-branch "master"
  "String identifying the branch of straight.el to clone.
You must set this variable before straight.el is bootstrapped for
it to have an effect. (It is used in the default recipe for
straight.el which is registered during bootstrap.)"
  :type '(choice
          (const :tag "Stable version (master)" "master")
          (const :tag "Development version (develop)" "develop")
          (string :tag "Use a custom branch")))

(defcustom straight-default-vc 'git
  "VC backend to use by default, if a recipe has no `:type'.
Functions named like `straight-vc-TYPE-clone', etc. should be
defined, where TYPE is the value of this variable."
  :type 'symbol)

(defcustom straight-recipe-repositories nil
  "List of recipe repositories to find recipes in.
These are used when you provide only a package name, rather than
a full recipe, to `straight-use-package' or
`straight-use-recipes'. The order in this list determines the
precedence. Functions named like `straight-recipes-NAME-list',
etc. should be defined, where NAME is any element of this list."
  :type '(list symbol))

(defcustom straight-recipe-overrides nil
  "Alist specifying recipes to override those provided explicitly.
The keys are symbols naming profiles, and the values are lists of
MELPA-style package recipes. Because the car of a MELPA-style
recipe is the package name as a symbol, this means the values can
also be interpreted as alists whose keys are symbols naming
packages.

If you have no need of the profile system, then using the default
profile (nil) will suffice without additional setup."
  :type '(alist :key-type symbol :value-type
                (alist :key-type symbol :value-type
                       (plist :key-type symbol :value-type sexp))))

(defcustom straight-allow-recipe-inheritance t
  "Non-nil allows partially overriding recipes.
If you override a recipe, every component that is not explicitly
overriden will be searched for in original recipe. If found, that
value will be added to the overriden recipe. This allows you to
only override the recipe components you are interested in,
instead of being required to override them all. The supported
components are the ones listed by `straight-vc-git-keywords' and
`:files'. Note that enabling this feature has the side effect
that all recipe repos (i.e. melpa, elpa) will always be cloned,
even if you explicitly specify all your recipes.

The `:fork' keyword is handled specially. If its value is a
string instead of a list, then it is assigned as the `:repo' of
the fork. Also the fork recipe will inherit its `:host' component
from the default recipe.

For example, the following are all equivalent with recipe
inheritance enabled.

\\='(package :host \\='gitlab :repo \"other-user/repo\"
          :fork (:host \\='gitlab :repo \"my-user/repo\"))

\\='(package :fork (:host \\='gitlab :repo \"my-user/repo\"))

\\='(package :fork (:repo \"my-user/repo\"))

\\='(package :fork \"my-user/repo\")

The `:inherit' keyword overrides this option on a per-recipe basis."
  :type 'boolean)

(defcustom straight-safe-mode nil
  "Non-nil means avoid doing anything that modifies the filesystem.
In safe mode, package modifications will still be detected
according to `straight-check-for-modifications'. However, if a
package needs to be cloned, built, or rebuilt, straight.el
instead generates an error. The build cache will not be written
back to disk, nor will the filesystem watcher be started (if
enabled).

As one example of a use case for safe mode, suppose you want to
byte-compile your Emacs configuration asynchronously in the
background. To avoid multiple Emacs processes modifying the
filesystem concurrently via straight.el, you might want to enable
safe mode for the background Emacs process.

Safe mode is not guaranteed to be as performant as normal
operation."
  :type 'boolean)

(defcustom straight-host-usernames nil
  "Alist mapping forge :host symbols to username strings.
The username associated with the host name is used to compute the :repo
when the :fork keyword is any of the following values:
  - t
  - a string prefixed with a forward slash
  - a plist which provides a :host and no :repo value
  - a plist which provides a :repo string prefixed with a forward slash

For example, with the following alist:

  (setq straight-host-usernames
        \\='((github . \"githubUser\")
          (gitlab . \"gitlabUser\")
          (bitbucket . \"bitbucketUser\")))

  (straight-use-package
   \\='( example :host github :type git :repo \"upstream/repo\"
      :fork t))

computes the fork as \"githubUser/repo\"

  (straight-use-package
   \\='( example :host github :type git :repo \"upstream/repo\"
      :fork \"/fork\"))

computes the fork as \"githubUser/fork\"."
  :type '(alist :key-type (choice
                           (const :tag "github" github)
                           (const :tag "gitlab" gitlab)
                           (const :tag "bitbucket" bitbucket))
                :value-type (string :tag "username")))

(defcustom straight-vc-git-post-clone-hook nil
  "Functions called after straight.el clones a git repository.

Each hook function is passed the following keyword arguments:

  - `:repo-dir' - the local directory to which the repository was cloned
  - `:remote' - the name of the remote from which the repository was cloned
  - `:url' - the URL from which the repository was cloned
  - `:branch' - the branch as specified by the recipe, if any,
    otherwise nil
  - `:depth' - the clone depth as specified by the recipe or
    `straight-vc-git-default-clone-depth'
  - `:commit' - the specific commit which was requested via the
    lockfile, if any, otherwise nil

Since keyword arguments are used, each function should be defined
via `cl-defun', with `&key' used at the front of the argument
list, and `&allow-other-keys' at the end to ensure forwards
compatibility."
  :type 'hook)

;;;; Utility functions
;;;;; Association lists

(defun straight--alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets
the cdr of the first matching association in the list. It does
not create duplicate associations. By default, key comparison is
done with `equal'. However, if SYMBOL is non-nil, then `eq' is
used instead.

This method may mutate the original alist, but you still need to
use the return value of this method instead of the original
alist, to ensure correct results."
  ;; See [1] for the genesis of this method, which should really be
  ;; built in.
  ;;
  ;; [1]: https://emacs.stackexchange.com/q/33892/12534
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

;;;;; Property lists

(defmacro straight--with-plist (plist props &rest body)
  "Binding from PLIST the given PROPS, eval and return BODY.
PROPS is a list of symbols. Each one is converted to a keyword
and then its value is looked up in the PLIST and bound to the
symbol for the duration of BODY."
  (declare (indent 2) (debug (form sexp body)))
  `(cl-destructuring-bind (&key ,@props &allow-other-keys) ,plist ,@body))

(defmacro straight--put (plist prop value)
  "Make copy of PLIST with key PROP mapped to VALUE, and re-set it.
PLIST must be a literal symbol naming a plist variable. PROP and
VALUE are evaluated."
  `(progn
     (setq ,plist (copy-sequence ,plist))
     (setq ,plist (plist-put ,plist ,prop ,value))))

(defmacro straight--remq (plist props)
  "Make copy of PLIST with keys PROPS removed, and re-set it.
PLIST must be a literal symbol naming a plist variable. PROPS is
evaluated and should result in a list. Key comparison is done
with `eq'."
  ;; The following subroutine is adapted from [1].
  ;;
  ;; [1]: https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00019.html
  (let ((props-sym (make-symbol "props")))
    `(let ((,props-sym ,props))
       (setq ,plist
             (cl-loop for (prop val) on ,plist by #'cddr
                      unless (memq prop ,props-sym)
                      collect prop and collect val)))))

(defun straight--plist-get (plist prop default)
  "Extract a value from a property list, or return a default.
PLIST is a property list, PROP is the key to search for, and
DEFAULT is the value to return if PROP is not in PLIST."
  (if-let ((result (plist-member plist prop)))
      (cadr result)
    default))

;;;;; Hash tables

(defun straight--insert (n key value table)
  "Associate index N in KEY with VALUE in hash table TABLE.
TABLE should be a hash whose values are lists. This function will
set the Nth entry of the list mapped to KEY in TABLE to VALUE. If
the list does not have an Nth entry, it will be padded with nils
so that it does, before the setting happens. The TABLE will be
modified and returned."
  (let ((list (gethash key table)))
    (if (>= n (length list))
        (puthash key
                 (append list
                         (make-list (- n (length list)) nil)
                         (list value))
                 table)
      (setcar (nthcdr n list) value))
    table))

(defvar straight--not-present 'straight--not-present
  "Value used as a default argument to `gethash'.")

(defvar straight--not-present-paranoid 'straight--not-present-paranoid
  "Value used as a default argument to `gethash'.
Why do we need this? Because whoever wrote the Elisp hash table
API didn't actually know how to write hash table APIs.")

(defun straight--checkhash (key table &optional paranoid)
  "Return non-nil if KEY is present in hash TABLE.
If PARANOID is non-nil, ensure correctness even for hash tables
that may contain `straight--not-present' as a value."
  (not
   (and (eq (gethash key table straight--not-present) straight--not-present)
        (or paranoid
            (eq (gethash key table straight--not-present-paranoid)
                straight--not-present-paranoid)))))

;;;;; Strings

(defun straight--split-and-trim (string &optional indent max-lines)
  "Split the STRING on newlines, returning a list.
Remove any blank lines at the beginning or end. If INDENT is
non-nil, then add that many spaces to the beginning of each line
and concatenate them with newlines, returning a string instead of
a list. If MAX-LINES is non-nil, then it should be a nonnegative
integer, and any lines past that many are discarded."
  (let ((parts (split-string string "\n")))
    ;; Remove blank lines from beginning.
    (while (equal (car parts) "")
      (setq parts (cdr parts)))
    (setq parts (nreverse parts))
    ;; Remove blank lines from end.
    (while (equal (car parts) "")
      (setq parts (cdr parts)))
    (setq parts (nreverse parts))
    ;; Remove tail.
    (when (and max-lines (< max-lines (length parts)))
      (setf (nthcdr max-lines parts) nil))
    ;; Add indentation.
    (if indent
        (let ((indent (make-string (or indent 0) ? )))
          (mapconcat (lambda (part) (concat indent part))
                     parts "\n"))
      parts)))

(cl-defun straight--uniquify (prefix taken)
  "Generate a string with PREFIX that is not in list TAKEN.
This is done by trying PREFIX-1, PREFIX-2, etc. if PREFIX is
already in TAKEN."
  (if (member prefix taken)
      (let ((n 1))
        (while t
          (let ((candidate (format "%s-%d" prefix n)))
            (if (member candidate taken)
                (cl-incf n)
              (cl-return-from straight--uniquify candidate)))))
    prefix))

;;;;; Functions

(defmacro straight--functionp (object)
  "Non-nil if OBJECT, an unquoted symbol, is bound to a function.
However, if OBJECT evaluates to its own symbol value or t, then
return nil. This is useful for allowing a function to be called
with nil, non-nil, or a function object, without worrying about
the non-nil value being interpreted as a function: just call the
function with the quoted name of the argument, or use t."
  (let ((object-sym (make-symbol "object")))
    `(let ((,object-sym ,object))
       (and (not (memq ,object-sym '(,object t)))
            (functionp ,object-sym)))))

;;;;; Messaging

(defun straight--output (string &rest objects)
  "Same as `message' (which see for STRING and OBJECTS) normally.
However, in batch mode, print to stdout instead of stderr."
  (if noninteractive
      (progn
        (princ (apply #'format string objects))
        (terpri))
    (apply #'message string objects)))

(defmacro straight--with-progress (task &rest body)
  "Displaying TASK as a progress indicator, eval and return BODY.
Display \"TASK...\", eval BODY, display \"TASK...done\", and
return the result of evaluating BODY. If TASK is nil, no messages
are displayed. TASK can also be a cons, whose car and cdr are
used as the TASK for the beginning and end messages
respectively. (Either the car or cdr, or both, can be nil.) See
also `straight--progress-begin' and `straight--progress-end'."
  (declare (indent 1) (debug t))
  (let ((task-sym (make-symbol "gensym--task"))
        (task-car-sym (make-symbol "gensym--task-car"))
        (task-cdr-sym (make-symbol "gensym--task-cdr")))
    `(let* ((,task-sym ,task)
            (,task-car-sym (if (listp ,task-sym)
                               (car ,task-sym)
                             ,task-sym))
            (,task-cdr-sym (if (listp ,task-sym)
                               (cdr ,task-sym)
                             ,task-sym)))
       (prog2
           (when ,task-car-sym
             (straight--output "%s..." ,task-car-sym))
           (progn
             ,@body)
         (when ,task-cdr-sym
           (straight--output "%s...done" ,task-cdr-sym))))))

(defun straight--progress-begin (message)
  "Display a MESSAGE indicating ongoing progress.
The MESSAGE is postpended with \"...\" and then passed to
`message'. See also `straight--with-progress' and
`straight--progress-end'."
  (straight--output "%s..." message))

(defun straight--progress-end (message)
  "Display a MESSAGE indicating completed progress.
The MESSAGE is postpended with \"...done\" and then passed to
`message'. See also `straight--with-progress' and
`straight--progress-begin'."
  (straight--output "%s...done" message))

(defvar straight--echo-area-dirty nil
  "Non-nil if a progress message has been wiped from the echo area.
This is used as an internal bookkeeping variable to determine if
a progress message has been bumped out of the echo area by
another message, and needs to be redisplayed.")

(defun straight--warn (message &rest args)
  "Display a warning from `straight'. Return nil.
The warning message is obtained by passing MESSAGE and ARGS to
`format'."
  (ignore
   (display-warning 'straight (apply #'format message args))))

;;;;; Buffers

(defun straight--ensure-blank-lines (n)
  "Ensure N newline characters preceding point, unless at beginning of buffer."
  (unless (= 1 (point))
    (let ((num-existing 0))
      (save-excursion
        (cl-dotimes (_ n)
          (when (or (= 1 (point))
                    (not (= ?\n (char-before))))
            (cl-return))
          (cl-incf num-existing)
          (backward-char)))
      (insert (make-string (- n num-existing) ?\n)))))

;;;;; Predicates

(defun straight--quoted-form-p (object)
  "Return t if OBJECT is quoted or backquoted, else nil."
  (when (member (car-safe object) '(quote \`)) t))

(defun straight--build-disabled-p (recipe)
  "Return t if RECIPE has an explicitly nil `:build` keyword."
  (let ((build (plist-member recipe :build)))
    (and build (not (cadr build)))))

(defun straight--buildable-p (recipe)
  "Return t if RECIPE has a non-nil, or vacuously nil `:build` keyword."
  (not (straight--build-disabled-p recipe)))

(defun straight--installed-p (recipe)
  "Return t if RECIPE's repository is available and `:local-repo` is non-nil."
  (and (plist-get recipe :local-repo)
       (straight--repository-is-available-p recipe)))

(defun straight--installed-and-buildable-p (recipe)
  "Return t if installed RECIPE has a non-nil or absent `:build'."
  (and (straight--buildable-p recipe)
       (straight--installed-p recipe)))

;;;;; Windows OS detection

;; THIS FUNCTION MUST BE MANUALLY SYNCED WITH
;; ./install.el straight--windows-os-p
(defun straight--windows-os-p ()
  "Check if the current operating system is Windows."
  (memq system-type '(ms-dos windows-nt)))

;;;;; Paths

(defcustom straight-base-dir user-emacs-directory
  "Directory in which the straight/ subdirectory is created.
Defaults to `user-emacs-directory'."
  :type 'string)

(defcustom straight-build-dir "build"
  "Name of the directory into which packages are built.
Relative to the straight/ subdirectory of `straight-base-dir'.
Defaults to \"build\".

By default, this variable also affects the name of the build
cache file, set the variable `straight-build-cache-fixed-name'
to override this."
  :type 'string)

(defcustom straight-build-cache-fixed-name nil
  "Name of the build cache file.
If it is nil, uses the default name, namely
\"`straight-build-dir'-cache.el\".

If it is not nil, it has to be a string which is used as the
name of the cache file.

In both cases, the path is relative to the \"straight/\"
subdirectory of `straight-base-dir'."
  :type '(choice (const :tag "Default location" nil)
                 (string :tag "Fixed location")))

(defvar straight--this-file
  (file-truename (or load-file-name buffer-file-name))
  "Absolute real path to this file, straight.el.")

(defun straight--path-prefix-p (prefix-path full-path)
  "Return non-nil if PREFIX-PATH is a prefix of FULL-PATH.
This takes into account case insensitivity on macOS."
  (string-prefix-p prefix-path full-path (eq system-type 'darwin)))

(defun straight--emacs-dir (&rest segments)
  "Get a subdirectory of the `user-emacs-directory'.
The SEGMENTS are path segments which are concatenated with
slashes and postpended to the straight directory. With no
SEGMENTS, return the `user-emacs-directory' itself.

\(straight--dir \"straight\" \"build\" \"esup\")
=> \"~/.emacs.d/straight/build/esup/\""
  (let ((dir straight-base-dir))
    (while segments
      (setq dir (expand-file-name
                 (file-name-as-directory (car segments)) dir))
      (setq segments (cdr segments)))
    dir))

(defun straight--emacs-file (&rest segments)
  "Get a file in the `user-emacs-directory'.
The SEGMENTS are path segments with are concatenated with slashes
and postpended to the straight directory.

\(straight--file \"straight\" \"build\" \"esup\" \"esup-autoloads.el\")
=> \"~/.emacs.d/straight/build/esup/esup-autoloads.el\""
  (expand-file-name
   (substring (apply 'straight--emacs-dir segments) 0 -1)))

(defun straight--dir (&rest segments)
  "Get a subdirectory of the straight/ directory.
SEGMENTS are passed to `straight--emacs-dir'. With no SEGMENTS,
return the straight/ directory itself."
  (apply #'straight--emacs-dir "straight" segments))

(defun straight--file (&rest segments)
  "Get a file in the straight/ directory.
SEGMENTS are passed to `straight--emacs-file'."
  (apply #'straight--emacs-file "straight" segments))

(defun straight--build-dir (&rest segments)
  "Get a subdirectory of the straight/build/ directory.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/build/ directory itself."
  (apply #'straight--dir straight-build-dir segments))

(defun straight--build-file (&rest segments)
  "Get a file in the straight/build/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file straight-build-dir segments))

(defun straight--autoloads-file (package)
  "Get the filename of the autoloads file for PACKAGE.
PACKAGE should be a string."
  (straight--build-file package (format "%s-autoloads.el" package)))

(defun straight--build-cache-file ()
  "Get the file containing straight.el's build cache."
  (straight--file
   (or straight-build-cache-fixed-name
       (concat straight-build-dir "-cache.el"))))

(defun straight--links-dir (&rest segments)
  "Get a subdirectory of straight/links/.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/links/ directory itself."
  (apply #'straight--dir "links" segments))

(defun straight--links-file (&rest segments)
  "Get a file in the straight/links/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file "links" segments))

(defun straight--modified-dir (&rest segments)
  "Get a subdirectory of straight/modified/.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/modified/ directory itself."
  (apply #'straight--dir "modified" segments))

(defun straight--modified-file (&rest segments)
  "Get a file in the straight/modified/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file "modified" segments))

(defun straight--mtimes-dir (&rest segments)
  "Get a subdirectory of straight/mtimes/.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/mtimes/ directory itself."
  (apply #'straight--dir "mtimes" segments))

(defun straight--mtimes-file (&rest segments)
  "Get a file in the straight/mtimes/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file "mtimes" segments))

(defun straight--repos-dir (&rest segments)
  "Get a subdirectory of the straight/repos/ directory.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/repos/ directory itself."
  (apply #'straight--dir "repos" segments))

(defun straight--repos-file (&rest segments)
  "Get a file in the straight/repos/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file "repos" segments))

(defun straight--versions-dir (&rest segments)
  "Get a subdirectory of the straight/versions/ directory.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/versions/ directory itself."
  (apply #'straight--dir "versions" segments))

(defun straight--versions-file (&rest segments)
  "Get a file in the straight/versions/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file "versions" segments))

(defun straight--watcher-dir (&rest segments)
  "Get a subdirectory of the straight/watcher/ directory.
SEGMENTS are passed to `straight--dir'. With no SEGMENTS, return
the straight/watcher/ directory itself."
  (apply #'straight--dir "watcher" segments))

(defun straight--watcher-file (&rest segments)
  "Get a file in the straight/watcher/ directory.
SEGMENTS are passed to `straight--file'."
  (apply #'straight--file "watcher" segments))

(defun straight--watcher-python ()
  "Get the path to the filesystem virtualenv's Python executable."
  (if (straight--windows-os-p)
      (straight--watcher-file "virtualenv" "Scripts" "python.exe")
    (straight--watcher-file "virtualenv" "bin" "python")))

(defun straight--versions-lockfile (profile)
  "Get the version lockfile for given PROFILE, a symbol."
  (if-let ((filename (alist-get profile straight-profiles)))
      (straight--versions-file filename)
    (error "Unknown profile: %S" profile)))

(defun straight--determine-repo (path)
  "Determine the local repository containing PATH, if any.
If PATH, a string, corresponds to a file or directory inside (or
equal to) any subfolder of `straight--repos-dir', then return the
name of the local repository (not a path), as a string.
Otherwise, return nil."
  (let ((repos-dir (straight--repos-dir)))
    (when (straight--path-prefix-p repos-dir path)
      ;; Remove the ~/.emacs.d/straight/repos/ part.
      (let ((relative-path (substring path (length repos-dir))))
        ;; Trim off any more path components after hte local
        ;; repository.
        (replace-regexp-in-string
         "/.*" "" relative-path 'fixedcase 'literal)))))

;;;;; Filesystem operations

(defun straight--symlinks-are-usable-p ()
  "Return non-nil if symlinks are well-supported by the OS.
This means that they are used to build packages rather than
copying files, which is slower and less space-efficient.

All operating systems support symlinks; however, on Microsoft
Windows you may need additional system configuration (see
variable `straight-use-symlinks')."
  (not (straight--windows-os-p)))

(defcustom straight-use-symlinks (straight--symlinks-are-usable-p)
  "Whether to use symlinks for building packages.
Using symlinks is always preferable.

On Microsoft Windows, this variable has to be set to non-nil
manually, if desired, as symlink-functionality is not always
available. On most versions of Windows 10, the user's account
needs to be assigned the right to \"Create symbolic links\" in
\"secpol.msc\". For more information about the symlink-setup on
MS Windows please refer to the section \"Customizing how packages
are built\" in the user manual.

Beware that copying is slower, less space-efficient, and
requiring of additional hacks."
  :type 'boolean)

(defun straight--directory-files (&optional directory match full sort)
  "Like `directory-files', but with better defaults.
DIRECTORY, MATCH, and FULL are as in `directory-files', but their
order has been changed. Also, DIRECTORY defaults to
`default-directory' if omitted. The meaning of the last argument
SORT has been inverted from `directory-files'. Finally, the . and
.. entries are never returned, and .git is removed from the
results if present."
  (cl-remove-if
   (lambda (file)
     (string-match-p "\\(?:\\(?:\\.\\(?:\\.\\|git\\)?\\)$\\)" file))
   (directory-files (or directory default-directory) full match (not sort))))

(defun straight--symlink-recursively (link-target link-name)
  "Make a symbolic link to LINK-TARGET, named LINK-NAME, recursively.
This means that if the link target is a directory, then a
corresponding directory is created (called LINK-NAME) and all
descendants of LINK-TARGET are linked separately into
LINK-NAME (except for directories, which are created directly).

If `straight-use-symlinks' is nil, then instead of creating a
symlink, the file is copied directly, and a corresponding entry
is created in the straight/links/ directory so that the file may
be interpreted later as a symlink."
  (if (and (file-directory-p link-target)
           (not (file-symlink-p link-target)))
      (progn
        (make-directory link-name 'parents)
        (dolist (entry (straight--directory-files link-target))
          (straight--symlink-recursively
           (expand-file-name entry link-target)
           (expand-file-name entry link-name))))
    (make-directory (file-name-directory link-name) 'parents)
    (condition-case _
        (if straight-use-symlinks
            (if (straight--windows-os-p)
                (straight--process-output
                 "cmd" "/c" "mklink"
                 (subst-char-in-string ?/ ?\\ link-name)
                 (subst-char-in-string ?/ ?\\ link-target))
              (make-symbolic-link link-target link-name))
          (copy-file link-target link-name)
          (let ((build-dir (straight--build-dir)))
            (when (straight--path-prefix-p build-dir link-name)
              (let* ((relative-path (substring link-name (length build-dir)))
                     (link-record (straight--links-file relative-path)))
                ;; This call may fail in the case that there was
                ;; previously a directory being symlinked, and now
                ;; there is a file by the same name being symlinked.
                ;; That edge case will need to be dealt with
                ;; eventually, but it's rather nontrivial so I'm not
                ;; doing it now.
                (make-directory (file-name-directory link-record) 'parents)
                (with-temp-file link-record
                  (insert link-target))))))
      (file-already-exists
       ;; We're OK with the recipe specifying to create the symlink
       ;; twice, as long as it's pointing to the same place both
       ;; times. Otherwise, signal a warning.
       (unless (string= link-target
                        (file-symlink-p link-name))
         (straight--warn "Attempted to link %S to both %S and %S"
                         link-name (file-symlink-p link-name) link-target))))))

;;;;; External processes
(defvar straight--process-log t
  "If non-nil, log process output to `straight-process-buffer'.")

(defvar straight--process-warn nil
  "If non-nil, warn for nonzero/failed processes.")

(defvar straight--default-directory nil
  "Overrides value of `default-directory'.
This is used because `default-directory' is buffer-local, which
means binding it for the duration of a recursive edit causes fun
side-effects like random buffers permanently forgetting which
directory they're in, and straight.el executing Git commands
against the wrong repositories.

If you set this globally to something other than nil, you may be
eaten by a grue.")

(defconst straight--process-stderr
  (expand-file-name (format "straight-stderr-%s" (emacs-pid))
                    temporary-file-directory)
  "File for storing proccesses' stderr.")

(defcustom straight-process-buffer "*straight-process*"
  "Name of buffer used for process output."
  :type 'string)

(defun straight--process-buffer ()
  "Return `straight-process-buffer' in `special-mode'."
  (with-current-buffer (get-buffer-create straight-process-buffer)
    (unless (derived-mode-p 'special-mode) (special-mode))
    (current-buffer)))

(defun straight--process-call (program &rest args)
  "Run PROGRAM syncrhonously with ARGS.
Return a list of form: (EXITCODE STDOUT STDERR).
If the process is unable to start, return an elisp error object."
  (let* ((program (if (string-match-p "/" program)
                      (expand-file-name program)
                    program)))
    (condition-case e
        (with-temp-buffer
          (list
           (apply #'call-process program nil
                  (list (current-buffer) straight--process-stderr)
                  nil args)
           (let ((s (buffer-string)))
             (unless (string-empty-p s) s))
           (with-current-buffer
               (find-file-noselect straight--process-stderr
                                   'nowarn 'raw)
             (prog1 (let ((s (buffer-string)))
                      (unless (string-empty-p s) s))
               (kill-buffer)))))
      (error e))))

(defmacro straight--process-with-result (result &rest body)
  "Provide anaphoric RESULT bindings for duration of BODY.
RESULT must be an expression which evaluates to a list of form:
  (EXITCODE STDOUT STDERR)
Anaphroic bindings provided:
  result: the raw process result list
  exit: the exit code of the process
  invoked: t if process executed without an elisp error
  success: t if process exited with exit code 0
  failure: t if process did not invoke or exited with a nonzero code
  stdout: output of stdout
  stderr: output of stderr"
  (declare (indent 1))
  `(let* ((result ,result)
          (exit (car result))
          (invoked (numberp exit))
          (success (and invoked (zerop exit)))
          (failure (not success))
          (stdout (nth 1 result))
          (stderr (nth 2 result)))
     ;; Stop the byte-compiler from complaining about unused bindings.
     (ignore result exit invoked success failure stdout stderr)
     ,@body))

(defun straight--process-log (entry &optional error)
  "Log formatted ENTRY in `straight-process-buffer'.
ENTRY is a list of the form: (PROGRAM (ARGS...) (RESULT) DIRECTORY)
If ERROR is non-nil, ENTRY's face is `straight-process-error'."
  (with-current-buffer (straight--process-buffer)
    (goto-char (point-max))
    (pcase-let ((`(,program ,args ,result ,directory) entry))
      (straight--process-with-result result
        (let* ((inhibit-read-only t)
               (entry
                (concat
                 (propertize
                  (format "$ cd %s\n$ %s\n\n"
                          (shell-quote-argument (expand-file-name directory))
                          (mapconcat #'shell-quote-argument
                                     (cons program args) " "))
                  'face 'straight-process-command)
                 (if (eq (car result) 'file-missing)
                     (propertize
                      (format "[File error while %s]\n" (cadr result))
                      'face 'straight-process-error)
                   (format "%s\n[Return code: %S]\n"
                           (concat (nth 1 result) (nth 2 result))
                           (car result))))))
          (straight--ensure-blank-lines 2)
          (insert (if error
                      (propertize entry 'face 'straight-process-error)
                    entry)))))))

(defun straight--process-warn (entry &optional display)
  "Emit a warning for ENTRY.
ENTRY is a list of the form: (PROGRAM (ARGS...) (RESULT) DIRECTORY).
If DISPLAY is non-nil, switch to `straight-process-buffer'."
  (pcase-let ((`(,program _args ,result _directory) entry))
    (straight--warn
     "Failed to run %S: %s"
     program
     (pcase (car result)
       ('file-missing "executable not found")
       ((pred numberp) ;first line of stderr up to 100 chars
        (let* ((stderr (replace-regexp-in-string "\n.*" "" (nth 2 result)))
               (length (length stderr))
               (limit 100))
          (concat (substring stderr 0 (min limit length))
                  (when (> length limit) "…"))))))
    (when display
      (switch-to-buffer-other-window straight-process-buffer)
      (goto-char (point-max)))))

(defun straight--process-run (program &rest args)
  "Run PROGRAM with ARGS via `straight--process-call'.
Output is logged in `straight-process-buffer' unless the
variable `straight--process-log' is nil.
Failed processes (nonzero exit code or elisp errors)
emit a warning unless the variable `straight--process-warn' is nil."
  (let ((default-directory (or straight--default-directory default-directory)))
    (straight--process-with-result
        (apply #'straight--process-call program args)
      (when (or straight--process-log straight--process-warn)
        (let ((entry (list program args result default-directory)))
          (when straight--process-log (straight--process-log entry failure))
          (unless (or success (not straight--process-warn))
            (straight--process-warn entry straight--process-log))))
      result)))

(defun straight--process-run-p (program &rest args)
  "Return t if PROGRAM ran with ARGS successfully."
  (straight--process-with-result (apply #'straight--process-run program args)
    success))

(defvar straight--process-trim t
  "If non-nil, trim `straight--process-output' results.")

(defun straight--process-output (program &rest args)
  "Return trimmed result of running PROGRAM with ARGS.
If `straight--process-trim' is nil, the output does not have its
leading/trailing whitespace trimmed.
If the command cannot be run or returns a nonzero exit code, throw an error."
  (straight--process-with-result
      (apply #'straight--process-run program args)
    (if success
        ;; Some programs may print to stderr even if they exit with 0.
        (let ((output (concat stdout stderr)))
          (if straight--process-trim (string-trim output) output))
      (error "Failed to run %S; see buffer %s"
             program straight-process-buffer))))

(defun straight--make-mtime (mtime)
  "Ensure that the `straight--mtimes-file' for MTIME (a string) exists.
This creates a file in the appropriate directory that has the
corresponding mtime. Return the name of the file.

This function may not work on all operating systems."
  (make-directory (straight--mtimes-dir) 'parents)
  (let ((file (straight--mtimes-file mtime)))
    (unless (file-exists-p file)
      (straight--process-run-p "touch" "-d" mtime file))
    file))

;;;;; Interactive popup windows

(defmacro straight--catching-quit (&rest body)
  "Exec BODY. If `quit' signaled, catch and return nil."
  (declare (indent defun))
  (let ((err (make-symbol "err")))
    `(condition-case ,err
         (progn ,@body)
       (quit))))

(defun straight--popup-raw (prompt actions)
  "Display PROMPT and allow user to choose between one of several ACTIONS.
PROMPT is a string, generally a complete sentence. ACTIONS is a
list of lists (KEY DESC FUNC ARGS...). KEY is a string
identifying the key that triggers this action; it is passed to
`kbd'. DESC is a description string to be displayed in the popup.
If it is nil, the action and its binding is not displayed in the
popup, although it still takes effect. If the user selects an
action, its FUNC is called with ARGS and the popup is dismissed.
The return value of `straight--popup-raw' is the return value of
FUNC.

ACTIONS later in the list take precedence over earlier ones with
regard to keybindings."
  (when noninteractive
    (error (format "Cannot display prompt in batch mode: %s" prompt)))
  (unless (assoc "C-g" actions)
    (setq actions (append actions '(("C-g" "Cancel" keyboard-quit)))))
  (let ((keymap (make-sparse-keymap))
        (func nil)
        (prompt (concat prompt "\n"))
        (max-length (apply #'max (mapcar #'length (mapcar #'car actions)))))
    (dolist (action actions)
      (cl-destructuring-bind (key desc func . args) action
        (when desc
          (setq prompt
                (format "%s\n %s%s %s" prompt
                        (make-string (- max-length (length key)) ? )
                        key desc)))
        (define-key keymap (kbd key)
          (lambda ()
            (interactive)
            (apply func args)))))
    (setq prompt (concat prompt "\n\n"))
    (let ((max-mini-window-height 1.0)
          (cursor-in-echo-area t))
      (when minibuffer-auto-raise
        (raise-frame (window-frame (minibuffer-window))))
      (while (not func)
        (setq func (lookup-key keymap (vector (read-key prompt))))))
    (funcall func)))

(defmacro straight--popup (prompt &rest actions)
  "Same as `straight--popup-raw', but with reduced need for quoting.
PROMPT is still evaluated at runtime. So are all elements of
ACTIONS, except for FUNC, which is wrapped in a `lambda'
automatically, and ARGS, which are superfluous and therefore
instead used as additional forms to place in the `lambda' after
FUNC."
  (declare (indent defun))
  `(straight--popup-raw
    ,prompt
    (list
     ,@(mapcar
        (lambda (action)
          (cl-destructuring-bind (key desc . args) action
            `(list ,key ,desc (lambda () ,@args))))
        actions))))

(defun straight-are-you-sure (&optional prompt)
  "Display a popup asking the user to confirm their questionable actions.
PROMPT has a sensible default; otherwise it is a string. Return
non-nil if the user confirms; nil if they abort."
  (straight--popup (or prompt "Are you sure?")
    ("y" "Yes, proceed" t)
    ("n" "No, abort" nil)))

;;;;; Transactions

(defvar straight--transaction-alist nil
  "Alist of actions being executed in the current transaction.
See `straight--transaction-exec'. The cars are their IDs, and the
cdrs are their END-FUNCs.

If nil, no transaction is not live.")

(defun straight--transaction-finalize-at-top-level ()
  "Schedule to finalize the current transaction when appropriate.
This means that `straight--transaction-finalize' will be invoked
on `post-command-hook', and it will wait until control is
returned to the top level before actually finalizing the
transaction and removing itself from the hook again. In batch
mode, the transaction is finalized using `kill-emacs-hook' rather
than `post-command-hook' (because the latter is not run in batch
mode)."
  (if noninteractive
      (add-hook 'kill-emacs-hook #'straight--transaction-finalize)
    (add-hook 'post-command-hook #'straight--transaction-finalize)))

(defun straight--transaction-finalize ()
  "Finalize the current transaction.
This means clearing `straight--transaction-alist' and executing
the functions recorded in it."
  ;; If we're inside a recursive edit, then don't finalize the
  ;; transaction yet. Instead, arrange to schedule another idle timer
  ;; once the user exits the recursive edit via one of the functions
  ;; listed below.
  (when (zerop (recursion-depth))
    (let ((alist straight--transaction-alist))
      (setq straight--transaction-alist nil)
      (remove-hook 'post-command-hook #'straight--transaction-finalize)
      (dolist (end-func (mapcar #'cdr alist))
        (when end-func
          (funcall end-func))))))

(cl-defun straight--transaction-exec (id &key now later manual)
  "Execute functions within a transaction.
ID is a symbol that acts as a unique identifier of the action
within the transaction. Only the first NOW and LATER functions
with a given ID are used within a transaction. NOW is invoked
immediately (with no arguments), and LATER is invoked at the end
of the transaction (with no arguments). These functions wrap the
transaction, so the LATER functions are invoked in reverse order
to the NOW functions. If either NOW or LATER is nil or omitted,
it acts as a no-op, but the ID is still registered to block
future `straight--transaction-exec' calls.

If MANUAL is non-nil, do not arrange for finalizing the
transaction. In this case, the caller must do this itself."
  ;; If `straight--transaction-alist' is non-nil, then we've already
  ;; started a transaction, but haven't yet finalized it. Don't
  ;; schedule more idle timers.
  (unless (or manual straight--transaction-alist)
    (straight--transaction-finalize-at-top-level))
  (unless (assq id straight--transaction-alist)
    ;; Push to start of list. At the end, we'll read forward, thus in
    ;; reverse order.
    (push (cons id later) straight--transaction-alist)
    (when now
      (funcall now))))

(defun straight-interactive-transaction ()
  "Start a recursive edit within a transaction."
  (interactive)
  (straight--transaction-exec 'interactive :manual t)
  (unwind-protect
      (recursive-edit)
    (straight--transaction-finalize)))

;;;; Feature detection

(defcustom straight-find-executable "find"
  "Executable path of find command used by straight.el."
  :type 'string)

(defun straight--determine-find-flavor ()
  "Determine the best default value of `straight-find-flavor'.
This uses -newermt if possible, and -newer otherwise."
  (if (straight--process-run-p
       straight-find-executable
       "/dev/null" "-newermt" "2018-01-01 12:00:00")
      `(newermt)
    nil))

(defcustom straight-find-flavor :guess
  "What options the available find(1) binary supports.
This is a list of symbols. If `newermt' is in the list, then
find(1) is given the `-newermt' option to check for files newer
than a particular timestamp. Otherwise, it is given the `-newer'
option instead (this requires creating temporary files with
particular mtimes, which is slower).

This variable can also be the symbol `:guess', meaning
straight.el will automatically assign an appropriate value when
the variable is next read.

For backwards compatibility, the value of this variable may also
be a symbol, which is translated into a corresponding list as
follows:

`gnu/bsd' => `(newermt)'
`busybox' => nil

This usage is deprecated and will be removed."
  :type '(choice
          (list
           (const :tag "Supports -newermt" newermt))
          (const :tag "Guess a value automatically" :guess)))

(defun straight--find-supports (symbol)
  "Check if `straight-find-flavor' contains SYMBOL.
However, if `straight-find-flavor' is itself one of the symbols
supported for backwards compatibility, account for that
appropriately."
  (when (eq straight-find-flavor :guess)
    (setq straight-find-flavor (straight--determine-find-flavor)))
  (memq symbol
        (pcase straight-find-flavor
          ('gnu/bsd '(newermt))
          ('busybox nil)
          (lst lst))))

;;;; Lockfile utility functions

(defun straight--lockfile-read (lockfile)
  "Read the given LOCKFILE and return an alist.
LOCKFILE is a filename relative to `straight--versions-dir', as
in `straight-profiles'. If the lockfile is missing, return nil.
If it is malformed, raise an error. (An informative error message
is not guaranteed, but at least there will be an error now
instead of later.)"
  (let ((lockfile-path (straight--versions-file lockfile)))

    (when (file-exists-p lockfile-path)
      (let ((alist (with-temp-buffer
                     (insert-file-contents-literally lockfile-path)
                     (read (current-buffer)))))
        (prog1 alist
          (unless (listp alist)
            (error "Malformed lockfile: not a list"))
          (mapc
           (lambda (cell)
             (unless (consp cell)
               (error "Malformed lockfile: not an alist"))
             (unless (stringp (car cell))
               (error "Malformed lockfile: nil local repository")))
           alist))))))

(defun straight--lockfile-read-all ()
  "Read version lockfiles and return merged alist of saved versions.
The alist maps repository names as strings to versions, whose
interpretations are defined by the relevant VC backend."
  (let ((versions nil))
    (dolist (spec straight-profiles)
      (cl-destructuring-bind (_profile . lockfile) spec
        (let ((versions-alist (straight--lockfile-read lockfile)))
          (dolist (spec versions-alist)
            (cl-destructuring-bind (local-repo . commit) spec
              (setq versions (straight--alist-set
                              local-repo commit versions)))))))
    versions))

;;;; Version control

(defun straight-vc (method type &rest args)
  "Call a VC backend method.
METHOD is a symbol naming a backend method, like symbol `clone'.
TYPE is a symbol naming a VC backend, like symbol `git'. ARGS are
passed to the method.

For example:
   (straight-vc \\='check-out-commit \\='git ...)
=> (straight-vc-git-check-out-commit ...)"
  (when (and straight-safe-mode
             (not (memq method '(local-repo-name keywords))))
    (error "VC operation `%S %S' not allowed in safe mode" type method))
  (let ((func (intern (format "straight-vc-%S-%S"
                              type method))))
    (unless (fboundp func)
      (let ((regexp (format "^straight-vc-%S-[a-z-]+$" type)))
        ;; Check if *any* methods are defined for this VC backend. If
        ;; not, there is probably no such backend.
        (if (cl-block nil
              (prog1 nil
                (mapatoms
                 (lambda (sym)
                   (when (and
                          (fboundp sym)
                          (string-match-p regexp (symbol-name sym)))
                     (cl-return t))))))
            (error "VC backend `%S' does not implement method `%S'"
                   type method)
          (error "No such VC backend `%S'" type))))
    (apply func args)))

(defun straight-vc-clone (recipe)
  "Clone the local repository specified by straight.el-style RECIPE.
If a commit is specified in one of the lockfiles, attempt to
check out that revision. If this fails, signal a warning.

This method sets `straight--default-directory' to the repos
directory and delegates to the relevant `straight-vc-TYPE-clone'
method, where TYPE is the `:type' specified in RECIPE. If the
repository already exists, throw an error."
  (straight--with-plist recipe
      (type local-repo)
    (let ((straight--default-directory (straight--repos-dir)))
      (when (file-exists-p (straight--repos-dir local-repo))
        (error "Repository already exists: %S" local-repo))
      ;; We're reading the lockfiles inline here, instead of caching
      ;; them like we do with the build cache. The reason is that
      ;; reading the lockfiles appears to be much faster than reading
      ;; the build cache (and we wouldn't want to engage in premature
      ;; optimization, now would we?), and also time is not really a
      ;; concern if we're already going to be cloning a repository.
      (let ((commit (cdr (assoc
                          local-repo (straight--lockfile-read-all)))))
        (straight-vc 'clone type recipe commit)))))

(defun straight-vc-normalize (recipe)
  "Normalize the local repository specified by straight.el-style RECIPE.
The meaning of normalization is backend-defined, but typically
involves validating repository configuration and cleaning the
working directory.

If the RECIPE does not specify a local repository, then no action
is taken.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-normalize' method, where TYPE is the `:type'
specified in RECIPE."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'normalize type recipe)))))

(defun straight-vc-fetch-from-remote (recipe)
  "Fetch from the primary remote for straight.el-style RECIPE.

If the RECIPE does not specify a local repository, then no action
is taken.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-fetch-from-remote' method, where TYPE is the
`:type' specified in RECIPE."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'fetch-from-remote type recipe)))))

(defun straight-vc-fetch-from-upstream (recipe)
  "Fetch from the upstream remote for straight.el-style RECIPE.
If RECIPE does not configure a fork, do nothing.

If the RECIPE does not specify a local repository, then no action
is taken.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-fetch-from-upstream' method, where TYPE is the
`:type' specified in RECIPE."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'fetch-from-upstream type recipe)))))

(defun straight-vc-merge-from-remote (recipe)
  "Merge from the primary remote for straight.el-style RECIPE.

If the RECIPE does not specify a local repository, then no action
is taken.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-merge-from-remote' method, where TYPE is the
`:type' specified in RECIPE."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'merge-from-remote type recipe)))))

(defun straight-vc-merge-from-upstream (recipe)
  "Merge from the upstream remote for straight.el-style RECIPE.
If RECIPE does not configure a fork, do nothing.

If the RECIPE does not specify a local repository, then no action
is taken.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-merge-from-upstream' method, where TYPE is the
`:type' specified in RECIPE."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'merge-from-upstream type recipe)))))

(defun straight-vc-push-to-remote (recipe)
  "Push to the primary remote for straight.el-style RECIPE, if necessary.

If the RECIPE does not specify a local repository, then no action
is taken.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-push-to-remote' method, where TYPE is the
`:type' specified in RECIPE."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'push-to-remote type recipe)))))

(defun straight-vc-check-out-commit (recipe commit)
  "Normalize the repo for RECIPE and check out COMMIT.

If RECIPE does not specify a local repository, then no action is
taken.

The interpretation of COMMIT is defined by the backend, but it
should be compatible with `straight-vc-get-commit'.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-check-out-commit'."
  (straight--with-plist recipe
      (local-repo type)
    (when local-repo
      (let ((straight--default-directory (straight--repos-dir local-repo)))
        (straight-vc 'check-out-commit type recipe commit)))))

(defun straight-vc-commit-present-p (recipe commit)
  "Check in RECIPE's repo if COMMIT can be checked out without fetching it.
Return non-nil if the commit is available."
  (straight--with-plist recipe
      (local-repo type)
    (let ((straight--default-directory (straight--repos-dir local-repo)))
      (straight-vc 'commit-present-p type local-repo commit))))

(defun straight-vc-get-commit (type local-repo)
  "Using VC backend TYPE, in LOCAL-REPO, return current commit.
TYPE is a symbol like symbol `git', etc. LOCAL-REPO is a string
naming a local package repository. The type of object returned is
defined by the backend, but it should be compatible with
`straight-vc-check-out-commit'.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-get-commit' method."
  (let ((straight--default-directory (straight--repos-dir local-repo)))
    (straight-vc 'get-commit type local-repo)))

(defun straight-vc-local-repo-name (recipe)
  "Generate a repository name from straight.el-style RECIPE.
If a repository name cannot be generated, return nil. This is
used for the default value of `:local-repo'. If nil is returned,
the package name is used instead.

This method sets `straight--default-directory' to the local
repository directory and delegates to the relevant
`straight-vc-TYPE-local-repo-name' method, where TYPE is the
`:type' specified in RECIPE."
  (straight--with-plist recipe
      (type)
    (straight-vc 'local-repo-name type recipe)))

(defun straight-vc-keywords (type)
  "Return a list of keywords used by the VC backend TYPE.
This does not include the `:type' keyword itself.

This method simply delegates to the relevant
`straight-vc-TYPE-keywords' method."
  (straight-vc 'keywords type))

;;;;; Built-in packages

(defun straight-vc-built-in-get-commit (_local-repo)
  "Get the currently checked-out commit object, given LOCAL-REPO name string.
For built-in packages, this is always nil because there cannot
actually be a local repository."
  nil)

(defun straight-vc-built-in-local-repo-name (_recipe)
  "Generate a repository name from straight.el-style RECIPE.
For built-in packages, this is always nil."
  nil)

;;;;; Git

(defcustom straight-vc-git-primary-remote "origin"
  "The remote name to use for the primary remote.
This variable is deprecated, and only applies to usage of the
deprecated `:upstream' keyword, except that if it is set to a
non-default value then it overrides the value of
`straight-vc-git-default-remote-name', for backwards
compatibility."
  :type 'string)
(make-obsolete-variable
 'straight-vc-git-primary-remote
 "see `straight-vc-git-default-remote-name' and
  `straight-vc-git-default-fork-name' instead,
  but note that the semantics are different."
 "2018-08-22")

(defcustom straight-vc-git-upstream-remote "upstream"
  "The remote name to use for the upstream remote.
This variable is deprecated, and only applies to usage of the
deprecated `:upstream' keyword."
  :type 'string)
(make-obsolete-variable
 'straight-vc-git-upstream-remote
 "see `straight-vc-git-default-remote-name' and
  `straight-vc-git-default-fork-name' instead,
  but note that the semantics are different."
 "2018-08-22")

(defcustom straight-vc-git-default-remote-name "origin"
  "The remote name to use for the primary remote.
For a forked package, this means the upstream remote.

You can override the value of this variable on a per-package
basis using the `:remote' keyword."
  :type 'string)

(defcustom straight-vc-git-default-fork-name "fork"
  "The remote name to use for the fork remote in a forked package.

You can override the value of this variable on a per-package
basis using the `:remote' keyword in the `:fork' sub-plist."
  :type 'string)

(defcustom straight-vc-git-default-protocol 'https
  "The default protocol to use for auto-generated URLs.
This affects the URLs used when `:host' is `github', `gitlab', or
`bitbucket'. It does not cause manually specified URLs to be
translated.

This may be either `https' or `ssh'."
  :type '(choice (const :tag "HTTPS" https)
                 (const :tag "SSH" ssh)))

(defcustom straight-vc-git-force-protocol nil
  "If non-nil, treat HTTPS and SSH URLs as incompatible.
This means that operations like `straight-normalize-package' will
re-set the remote URLs for packages whose recipes have non-nil
`:host' values, if they are using a different protocol than the
one specified in `straight-vc-git-default-protocol'."
  :type 'boolean)

(defcustom straight-vc-git-auto-fast-forward t
  "Whether to quietly fast-forward when pulling packages.
This suppresses popups for trivial remote changes (i.e. the
current HEAD is an ancestor to the remote HEAD).
Also re-attaches detached heads quietly when non-nil.
A nil value allows for inspection of all remote changes."
  :type 'boolean)

;;;;;; Utility functions

(defun straight-vc--host-username (host)
  "Return username associated with HOST in `straight-host-usernames'."
  (or (alist-get host straight-host-usernames)
      (error "No straight-host-usernames entry for host `%S'" host)))

(defvar straight-vc-repository-separator "/"
  "Symbol delimiting a VC host's username from the repository's name.")

(defun straight-vc--repo-substring (part string)
  "Return PART (either `usernmae' or `repo') of `:repo' STRING."
  (let ((separator straight-vc-repository-separator))
    (replace-regexp-in-string
     (cond ((eq part 'username) (concat separator ".*"))
           ((eq part 'repo) (concat ".*" separator))
           (t (error "Uknown `:repo' part `%S'" part)))
     "" string)))

(defun straight-vc-git--fork-string-type (string)
  "Return symbol indicating :fork STRING's type."
  (let ((separator straight-vc-repository-separator))
    (cond
     ((string-prefix-p separator string) 'repository)
     ((or (string-suffix-p separator string)
          (not (string-match-p separator string)))
      'username)
     (t 'full))))

(defun straight-vc-git--fork-repo (recipe)
  "Compute RECIPE's :fork repo value."
  (straight--with-plist recipe (fork host ((:repo upstream-repo)))
    ;; Override default :host and :repo if present on :fork.
    (straight--with-plist (if (listp fork) fork nil)
        ((host host) (repo upstream-repo))
      (pcase fork
        ('nil repo)
        ('t (concat (straight-vc--host-username host) "/"
                    (straight-vc--repo-substring 'repo repo)))
        ((pred listp)
         (straight-vc-git--fork-repo
          `(,@(when host `(:host ,host)) :repo ,upstream-repo
            ;; When the :fork plist provides a :host without
            ;; a :repo, we want the recursion to compute the
            ;; username from `straight-host-usernames'
            ;; and combine it with the inherited upstream
            ;; repo. Otherwise, we can recurse as if :fork
            ;; had been specified as a string.
            :fork ,(if (plist-member fork :repo) repo t))))
        ((pred stringp)
         (pcase (straight-vc-git--fork-string-type fork)
           ;; If no :host is given, the repository is assumed to exist
           ;; on the local file system.
           ('repository (concat
                         (when host (straight-vc--host-username host))
                         fork))
           ('username   (concat fork
                                (unless (string-suffix-p "/" fork) "/")
                                (straight-vc--repo-substring 'repo repo)))
           ('full fork)))
        (_ repo)))))

(defmacro straight-vc-git--destructure (recipe props &rest body)
  "Binding from RECIPE the given (virtual) PROPS, eval and return BODY.
Unlike `straight--with-plist', this macro has special support for
the `:fork' sub-plist. In particular, the REPO, HOST, BRANCH, and
REMOTE properties are taken from this sub-plist, while prepending
them with `upstream-' allows you to get the top-level values.
Prepending with `fork-' is the same as the default behavior,
except that if no `:fork' is configured then the values are bound
to nil. Furthermore, default values for BRANCH and REMOTE are
computed, based on the relevant user options, and the deprecated
`:upstream' property and accompanying user options are handled
appropriately."
  (declare
   (debug (form sexp body))
   (indent 2))
  (let ((recipe-sym (make-symbol "--recipe--"))
        (wrap-default
         (lambda (prop check value
                       &optional fork upstream-check upstream-value)
           (pcase prop
             ('host
              `(cond
                (,check ,value)
                (,upstream-check ,upstream-value)
                (t nil)))
             ('branch
              `(cond
                (,check ,value)
                (,upstream-check ,upstream-value)
                ;; We use nil here to signify there is no branch
                ;; specified, so it must be figured out by clients.
                (t nil)))
             ('remote
              `(cond
                (,check ,value)
                ,@(if fork
                      `((t straight-vc-git-default-fork-name))
                    `(((equal straight-vc-git-primary-remote "origin")
                       straight-vc-git-default-remote-name)
                      (t straight-vc-git-primary-remote)))))
             (_ value)))))
    `(let ((,recipe-sym ,recipe))
       (straight--with-plist ,recipe-sym
           (upstream repo host branch)
         (when upstream
           (setq ,recipe-sym (cl-copy-list ,recipe-sym))
           (setq ,recipe-sym
                 (plist-put
                  ,recipe-sym :fork
                  ;; Can't use a backquote here because we're already
                  ;; inside another backquote and we want to avoid
                  ;; resolving variables like
                  ;; `straight-vc-git-primary-remote' at
                  ;; macroexpansion time.
                  (list
                   :repo repo
                   :host host
                   :branch branch
                   :remote straight-vc-git-primary-remote)))
           (dolist (kw '(:host :repo))
             (setq ,recipe-sym
                   (plist-put ,recipe-sym kw (plist-get upstream kw))))
           (setq ,recipe-sym (plist-put ,recipe-sym :branch
                                        (plist-get upstream :branch)))
           (setq ,recipe-sym
                 (plist-put
                  ,recipe-sym :remote straight-vc-git-upstream-remote))))
       (let (,@(mapcar (lambda (prop)
                         `(,prop
                           ,(let ((require-fork nil))
                              (when (string-prefix-p
                                     "fork-" (symbol-name prop))
                                (setq prop (intern
                                            (string-remove-prefix
                                             "fork-" (symbol-name prop))))
                                (setq require-fork t))
                              (let ((kw (intern (format ":%S" prop))))
                                (if (memq prop
                                          '(repo host branch remote protocol))
                                    `(if-let ((fork (plist-get
                                                     ,recipe-sym :fork)))
                                         ,(funcall
                                           wrap-default
                                           prop
                                           `(plist-member fork ',kw)
                                           `(plist-get fork ',kw)
                                           'fork
                                           `(plist-member ,recipe-sym ',kw)
                                           `(plist-get ,recipe-sym ',kw))
                                       ,(unless require-fork
                                          (funcall
                                           wrap-default
                                           prop
                                           `(plist-member ,recipe-sym ',kw)
                                           `(plist-get ,recipe-sym ',kw))))
                                  (setq prop (intern
                                              (string-remove-prefix
                                               "upstream-"
                                               (symbol-name prop))))
                                  (setq kw (intern (format ":%S" prop)))
                                  (funcall
                                   wrap-default
                                   prop
                                   `(plist-member ,recipe-sym ',kw)
                                   `(plist-get ,recipe-sym ',kw)))))))
                       props))
         ,@body))))

(defun straight--ensure-magit-p (feature)
  "Load Magit FEATURE and return non-nil if it's avaliable.
If the feature isn't available, offer to install Magit with a
`y-or-n-p' prompt before loading it."
  (or (require feature nil 'noerror)
      (when (y-or-n-p "Install Magit? ")
        (straight-use-package 'magit)
        (require feature nil 'noerror))))

(defun straight--magit-status (directory)
  "Like `magit-status', but offer to install Magit if necessary.
Return non-nil if `magit-status-setup-buffer' is invoked successfully.
DIRECTORY is as in `magit-status'."
  (when (and (straight--ensure-magit-p 'magit-status)
             (fboundp 'magit-status-setup-buffer))
    (magit-status-setup-buffer directory)
    t))

(defun straight--recursive-edit ()
  "Start a new recursive edit session.
Make sure that other packages such as `server.el' don't cause us
to lose our session."
  ;; Don't mess up recursive straight.el operations. The wonderful
  ;; thing about using our own variable is that since it's not
  ;; buffer-local, a recursive binding to nil is actually able to
  ;; undo the effects of the ambient binding.
  (cl-letf (((symbol-function 'top-level) #'ignore)
            (straight--default-directory nil))
    (recursive-edit)))

(defun straight-vc-git--popup-raw (prompt actions)
  "Same as `straight--popup-raw', but specialized for vc-git methods.
Two additional actions are inserted at the end of the list: \"e\"
for Dired and recursive edit, and \"g\" for Magit and recursive
edit. Otherwise, PROMPT and ACTIONS are as for
`straight--popup-raw'."
  (straight--popup-raw
   prompt
   (append
    actions
    '(("e" "Dired and open recursive edit"
       (lambda ()
         (dired (or straight--default-directory default-directory))
         (straight--recursive-edit)))
      ("g" "Magit and open recursive edit"
       (lambda ()
         (when (straight--magit-status
                (or straight--default-directory default-directory))
           (straight--recursive-edit))))))))

(defmacro straight-vc-git--popup (prompt &rest actions)
  "Same as `straight--popup', but specialized for vc-git methods.
Two additional actions are inserted at the end of the list: \"e\"
for Dired and recursive edit, and \"g\" for Magit and recursive
edit. Otherwise, PROMPT and ACTIONS are as for
`straight--popup'."
  (declare (indent defun))
  `(straight--popup
     ,prompt
     ,@actions
     ("e" "Dired and open recursive edit"
      (dired (or straight--default-directory default-directory))
      (straight--recursive-edit))
     ("g" "Magit and open recursive edit"
      (when (straight--magit-status
             (or straight--default-directory default-directory))
        (straight--recursive-edit)))))

(defun straight-vc-git--encode-url (repo host &optional protocol)
  "Generate a URL from a REPO depending on the value of HOST and PROTOCOL.
REPO is a string which is either a URL or something of the form
\"username/repo\", like \"raxod502/straight.el\". If HOST is one
of the symbols `github', `gitlab', or `bitbucket', then REPO is
transformed into a standard SSH URL for the corresponding
service; otherwise, HOST should be nil, and in that case REPO is
returned unchanged. PROTOCOL must be either `https' or `ssh'; if
it is omitted, it defaults to `straight-vc-git-default-protocol'.
See also `straight-vc-git--decode-url'."
  (pcase host
    ('nil repo)
    ((or 'github 'gitlab 'bitbucket)
     (when (string-match-p ":" repo)
       (error "Malformed protocol detected: (:host %S :repo %S)"
              host repo))
     (let ((domain (pcase host
                     ('bitbucket "bitbucket.org")
                     (_ (format "%s.com" host)))))
       (pcase (or protocol straight-vc-git-default-protocol)
         ('https
          (format "https://%s/%s.git" domain repo))
         ('ssh
          (format "git@%s:%s.git" domain repo))
         (_ (error "Unknown protocol: %S" protocol)))))
    (_ (error "Unknown value for host: %S" host))))

(defun straight-vc-git--decode-url (url)
  "Separate a URL into a REPO, HOST, and PROTOCOL, returning a list of them.
All common forms of HTTPS and SSH URLs are accepted for GitHub,
GitLab, and Bitbucket. If one is recognized, then HOST is one of
the symbols `github', `gitlab', or `bitbucket', and REPO is a
string of the form \"username/repo\". Otherwise HOST is nil and
REPO is just URL. In any case, PROTOCOL is either `https', `ssh',
or nil (if the protocol cannot be determined, which happens when
HOST is nil). See also `straight-vc-git--encode-url'."
  (let ((protocol nil)
        (matched t))
    (or (and (string-match
              "^git@\\(.+?\\):\\(.+?\\)\\(?:\\.git\\)?$"
              url)
             (setq protocol 'ssh))
        (and (string-match
              "^ssh://git@\\(.+?\\)/\\(.+?\\)\\(?:\\.git\\)?$"
              url)
             (setq protocol 'ssh))
        (and (string-match
              "^https://\\(.+?\\)/\\(.+?\\)\\(?:\\.git\\)?$"
              url)
             (setq protocol 'https))
        ;; We have to take care of this case separately because if
        ;; `string-match' doesn't actually match anything, then
        ;; `match-string' has undefined behavior.
        (setq matched nil))
    (pcase (and matched (match-string 1 url))
      ("github.com" (list (match-string 2 url) 'github protocol))
      ("gitlab.com" (list (match-string 2 url) 'gitlab protocol))
      ("bitbucket.org" (list (match-string 2 url) 'bitbucket protocol))
      (_ (list url nil nil)))))

(defun straight-vc-git--urls-compatible-p (url1 url2)
  "Return non-nil if URL1 and URL2 can be treated as equivalent.
This means that `straight-vc-git--decode-url' returns the same
for both (but if `straight-vc-git-force-protocol' is nil, then
the returned protocol is allowed to differ). For example, HTTPS
and SSH URLs for the same repository are equivalent (unless
`straight-vc-git-force-protocol' is non-nil), and it does not
matter if a GitHub URL is suffixed with .git or not."
  (let ((spec1 (straight-vc-git--decode-url url1))
        (spec2 (straight-vc-git--decode-url url2)))
    (if straight-vc-git-force-protocol
        (equal spec1 spec2)
      ;; Only compare the first two elements; ignore the third, which
      ;; is the protocol.
      (equal (cl-subseq spec1 0 2)
             (cl-subseq spec2 0 2)))))

(defun straight-vc-git--list-remotes ()
  "Return a list of Git remotes as strings for the current directory.
Do not suppress unexpected errors."
  ;; Git remote names cannot have whitespace in them, thank goodness.
  (straight--split-and-trim (straight--process-output "git" "remote")))

;;;;;; Validation functions

(cl-defun straight-vc-git--ensure-remote (local-repo remote desired-url)
  "Ensure that LOCAL-REPO has REMOTE set to DESIRED-URL or equivalent.
All three arguments are strings. The URL of the REMOTE does not
necessarily need to match DESIRED-URL; it just has to satisfy
`straight-vc-git--urls-compatible-p'."
  ;; Always return nil unless we use `cl-return-from'.
  (ignore
   (if-let ((actual-url (condition-case nil
                            (straight--process-output
                             "git" "config" "--get"
                             (format "remote.%s.url" remote))
                          (error nil))))
       (if (straight-vc-git--urls-compatible-p
            actual-url desired-url)
           ;; This is the only case where we return non-nil.
           (cl-return-from straight-vc-git--ensure-remote t)
         (let ((new-remote (straight--uniquify
                            remote
                            (straight-vc-git--list-remotes))))
           (straight-vc-git--popup
             (format "In repository %S, remote %S has URL
  %S
but recipe specifies a URL of
  %S"
                     local-repo remote actual-url desired-url)
             ("r" (format (concat "Rename remote %S to %S, "
                                  "re-create %S with correct URL, and fetch")
                          remote new-remote remote)
              (straight--process-output
               "git" "remote" "rename" remote new-remote)
              (straight--process-output
               "git" "remote" "add" remote desired-url)
              (straight--process-output "git" "fetch" remote))
             ("R" (format (concat "Rename remote %S manually, re-create "
                                  "it with correct URL, and fetch")
                          remote)
              (straight--process-output
               "git" "remote" "rename" remote
               (read-string "Enter new remote name: "))
              (straight--process-output "git" "remote" "add"
                                        remote desired-url)
              (straight--process-output "git" "fetch" remote))
             ("d" (format (concat "Delete remote %S, re-create it "
                                  "with correct URL, and fetch")
                          remote)
              (when (straight-are-you-sure
                     (format "Really delete remote %S?" remote))
                (straight--process-output "git" "remote" "remove" remote)
                (straight--process-output
                 "git" "remote" "add" remote desired-url)
                (straight--process-output "git" "fetch" remote)))
             ("D" (format (concat "Delete remote %S, re-create it "
                                  "with manually set URL, and fetch")
                          remote)
              (when (straight-are-you-sure
                     (format "Really delete remote %S?" remote))
                (straight--process-output "git" "remote" "remove" remote)
                (straight--process-output
                 "git" "remote" "add" remote
                 (read-string "Enter new remote URL: "))
                (straight--process-output "git" "fetch" remote))))))
     ;; General policy is that if we make any modifications
     ;; whatsoever, then validation fails. You never know when you
     ;; might run into a weird edge case of Git and have an operation
     ;; unexpectedly violate a previously established assumption.
     (straight--process-output "git" "remote" "add" remote desired-url))))

(cl-defun straight-vc-git--ensure-remotes (recipe)
  "Ensure that repository for RECIPE has remotes set correctly.
RECIPE is a straight.el-style plist.

This means the primary and fork remotes, if configured, have
their URLs set to the same as what is specified in the RECIPE.
The URLs do not necessarily need to match exactly; they just have
to satisfy `straight-vc-git--urls-compatible-p'."
  (straight-vc-git--destructure recipe
      (local-repo
       upstream-repo upstream-host upstream-remote upstream-protocol
       fork-repo fork-host fork-remote fork-protocol)
    (and (or (null upstream-repo)
             (straight-vc-git--ensure-remote
              local-repo
              upstream-remote
              (straight-vc-git--encode-url
               upstream-repo upstream-host upstream-protocol)))
         (or (null fork-repo)
             (straight-vc-git--ensure-remote
              local-repo
              fork-remote
              (straight-vc-git--encode-url
               fork-repo fork-host fork-protocol))))))

(defun straight-vc-git--rebase-in-progress-p ()
  "Return t if rebase is in progress.
Delegate to magit's version of this function if available; it may
handle cases we don't try to here. For example, magit's version does
something more complicated to locate the git directory from Emacs
running under cygwin."
  (let ((default-directory straight--default-directory))
    (if (fboundp 'magit-rebase-in-progress-p)
        (magit-rebase-in-progress-p)
      (let ((git-dir (file-name-as-directory
                      (straight--process-output
                       "git" "rev-parse" "--git-dir"))))
        (or (file-exists-p (expand-file-name "rebase-merge" git-dir))
            (file-exists-p (expand-file-name
                            "rebase-apply/onto" git-dir)))))))

(defun straight-vc-git--ensure-nothing-in-progress (local-repo)
  "Ensure that no merge or rebase is unfinished in LOCAL-REPO.
LOCAL-REPO is a string."
  (cl-flet ((short-status () (straight--split-and-trim
                              (straight--process-output
                               "git" "status" "--short")
                              2)))
    (cond
     ((straight-vc-git--rebase-in-progress-p)
      (ignore
       (straight-vc-git--popup
         (format "Repository %S has an unfinished rebase:\n\n%s"
                 local-repo (short-status))
         ("a" "Abort rebase"
          (straight--process-output "git" "rebase" "--abort")))))
     ((not (string-empty-p
            (straight--process-output "git" "ls-files" "--unmerged")))
      (ignore
       (straight-vc-git--popup
         (format "Repository %S has a merge conflict:\n\n%s"
                 local-repo (short-status))
         ("a" "Abort merge"
          (straight--process-output "git" "merge" "--abort")))))
     (t t))))

(cl-defun straight-vc-git--ensure-worktree (local-repo)
  "Ensure that LOCAL-REPO has a clean worktree.
LOCAL-REPO is a string."
  (let ((status (let ((straight--process-trim nil))
                  (straight--process-output
                   "git" "-c" "status.branch=false"
                   "status" "--short"))))
    (if (string-empty-p status)
        (cl-return-from straight-vc-git--ensure-worktree t)
      (straight-vc-git--popup
        (format "Repository %S has a dirty worktree:\n\n%s"
                local-repo
                (straight--split-and-trim
                 status 2))
        ("z" "Stash changes"
         (let ((msg (read-string "Optional stash message: ")))
           (if (string-empty-p msg)
               (straight--process-output
                "git" "stash" "push" "--include-untracked")
             (straight--process-output
              "git" "stash" "save" "--include-untracked" msg))))
        ("d" "Discard changes"
         (when (straight-are-you-sure
                (format "Discard all local changes permanently?"))
           (and (straight--process-output "git" "reset" "--hard")
                (straight--process-output "git" "clean" "-ffd"))))))))

(defun straight-vc-git--abbrev-ref (ref)
  "Return the unambiguous, abbreviated name of REF."
  (straight--process-with-result
      (straight--process-run
       "git" "rev-parse" "--verify" "--abbrev-ref" ref)
    (cond
     ;; When ref is ambiguous, rev-parse exits successfully but prints
     ;; a warning to stderr.
     ((or failure stderr)
      (error "Failed to get unambiguous name for %S\n%s"
             ref (concat stdout stderr)))
     ((not stdout)
      (error "Failed to get short name for %S\n%s"
             ref (concat stdout stderr)))
     (t (string-trim stdout)))))

(defun straight-vc-git--local-branch (ref)
  "Return branch named by REF if REF is a local branch.
Otherwise, return nil. Returned ref may be ambiguous.
This is useful when dealing with ambiguous refs: If the short name of
a branch is 'xyz' and there's also a tag named 'xyz', the shortest
unambiguous branch name is at least 'heads/xyz'. If you attempt to
check out 'heads/xyz', you'll end up at the right commit, but in
detached head state. To check out the branch, you need to use the
short name as returned by this function. Git checkout will print a
warning about the ambiguous name, but succeed."
  (let ((full-ref (straight--process-output
                   "git" "rev-parse" "--verify" "--symbolic-full-name" ref)))
    (when (string-match-p "^refs/heads/.*$" full-ref)
      (string-remove-prefix "refs/heads/" full-ref))))

(defun straight-vc-git--compare-and-canonicalize (left right)
  "Return plist describing relationship between refs LEFT and RIGHT."
  (condition-case failure
      (let* ((left (straight-vc-git--abbrev-ref left))
             (right (straight-vc-git--abbrev-ref right))
             (head (straight-vc-git--abbrev-ref "HEAD"))
             (left-is-ancestor (straight--process-run-p
                                "git" "merge-base" "--is-ancestor"
                                left right))
             (right-is-ancestor (straight--process-run-p
                                 "git" "merge-base" "--is-ancestor"
                                 right left)))
        (list
         ;; Canonical refs
         :left-ref  left
         :right-ref right
         :head-ref  head
         ;; Flags
         :head-detached     (string= head "HEAD")
         :left-is-ancestor  left-is-ancestor
         :right-is-ancestor right-is-ancestor
         :same              (and left-is-ancestor right-is-ancestor)
         :diverged          (not (or left-is-ancestor right-is-ancestor))))
    (error
     (list :failure (cadr failure)))))

(defun straight-vc-git--reconcile-interactively (local-repo status)
  "Ask user how to update LOCAL-REPO.
Present options to reconcile default-branch in LOCAL-REPO with another
ref and HEAD. Refs are considered reconciled when HEAD is at
default-branch and default-branch is a descendant of the other ref,
called include-ref within this function.
STATUS is the result of comparing default-branch with include-ref
using `straight-vc-git--compare-and-canonicalize'.
HEAD must be at default-branch or include-ref before calling this
function, and default-branch should not already be pointing at
include-ref.
Return nil; this is a helper for
`straight-vc-git--ensure-head-at-branch' and
`straight-vc-git--ensure-default-branch-current', so if the chosen
action is successful, those will detect the refs are reconciled the
next time around."
  (ignore
   (straight--with-plist status
       (head-ref head-detached left-is-ancestor right-is-ancestor diverged)
     (let* ((default-branch (straight--plist-get status :left-ref nil))
            (include-ref (straight--plist-get status :right-ref nil))
            (head-at-default (string= head-ref default-branch))
            (head-at-include (string= head-ref include-ref))
            (other-ref (cond
                        (head-at-default include-ref)
                        (head-at-include default-branch)
                        (t (error (concat
                                   "Expected either default-branch %S or"
                                   " include-ref %S to be checked out, but"
                                   " HEAD is at %S")
                                  default-branch include-ref head-ref))))
            (head-ahead (or (and left-is-ancestor head-at-include)
                            (and right-is-ancestor head-at-default)))
            (prompt (concat
                     (format "In repository %S," local-repo)
                     (if head-detached
                         " detached HEAD"
                       (format " HEAD on %s%S"
                               (if head-at-default "default branch " "")
                               head-ref))
                     (cond
                      (diverged " has diverged from")
                      (head-ahead " is ahead of")
                      (t " is behind"))
                     (format " %s%S"
                             (if head-at-default "" "default branch ")
                             other-ref)))
            (range (cond
                    (diverged   (format "%s...%s" head-ref other-ref))
                    (head-ahead (format "%s..%s" other-ref head-ref))
                    (t          (format "%s..%s" head-ref other-ref))))
            ;; If we can rebase and include-ref is a local branch,
            ;; we'll offer to rebase it onto default-branch.
            ;; Otherwise, we'll fall back to rebasing the
            ;; default-branch onto include-ref, like when its a remote
            ;; ref.
            (include-branch (and diverged
                                 (straight-vc-git--local-branch include-ref))))
       (straight-vc-git--popup-raw
        prompt
        ;; Here be dragons! Watch the quoting very carefully in
        ;; order to get the lexical scoping to work right, and don't
        ;; confuse this syntax with the syntax of the
        ;; `straight--popup' macro.
        `(,@(unless head-at-default
              `(("c" ,(format "Checkout %S" default-branch)
                 ,(lambda ()
                    (straight--process-output
                     "git" "checkout" default-branch)))))
          ,@(when (and left-is-ancestor (not head-detached))
              `(("f" ,(format "Fast-forward %S to %S"
                              default-branch include-ref)
                 ,(lambda ()
                    (unless head-at-default
                      (straight--process-output
                       "git" "checkout" default-branch))
                    (straight--process-output
                     "git" "merge" "--ff-only" include-ref)))))
          ,@(when (and diverged (not head-detached))
              `(("m" ,(format "Merge %S into %S" include-ref default-branch)
                 ,(lambda ()
                    (unless head-at-default
                      (straight--process-output
                       "git" "checkout" default-branch))
                    ;; If merge fails due to conflict,
                    ;; `straight-vc-git--ensure-nothing-in-progress'
                    ;; will detect the failed merge and pause for us
                    ;; to clean it up. NOTE however, that if we then
                    ;; choose to abort the merge, HEAD will remain at
                    ;; default-branch.
                    (straight--process-run-p "git" "merge" include-ref)))
                ,@(if include-branch
                      `(("r" ,(format (concat "Rebase %S onto %S, then "
                                              "checkout and fast-forward %S")
                                      include-branch default-branch
                                      default-branch)
                         ,(lambda ()
                            ;; If the rebase encounters a conflict, no
                            ;; sweat: the possibility of a fast-forward
                            ;; will be detected elsewhere in this
                            ;; function the next time around. But we
                            ;; might as well finish the job if we can.
                            (when (straight--process-run-p
                                   "git" "rebase"
                                   default-branch include-branch)
                              (straight--process-output
                               "git" "checkout" default-branch)
                              ;; Fast-forward to include-branch, but
                              ;; specify its ref with include-ref
                              ;; because that is unambiguous.
                              (straight--process-output
                               "git" "merge" "--ff-only" include-ref)))))
                    `(("r" ,(format "Rebase %S onto %S"
                                    default-branch include-ref)
                       ,(lambda ()
                          ;; Again, rebase might fail, but don't throw
                          ;; on error.
                          (straight--process-run
                           "git" "rebase" include-ref default-branch)))))
                ("x" ,(format "Reset %S to %S" default-branch include-ref)
                 ,(lambda ()
                    (unless head-at-default
                      (straight--process-output
                       "git" "checkout" default-branch))
                    (straight--process-output
                     "git" "reset" "--hard" include-ref)))))
          ("l" ,(format "Magit log %S and open recursive edit" range)
           ,(lambda ()
              (when (and (straight--ensure-magit-p 'magit-log)
                         (fboundp 'magit-log-setup-buffer)
                         (fboundp 'magit-log-arguments))
                ;; `magit-log-arguments' and `magit-log-setup-buffer'
                ;; must run in the git repo's local directory.
                (let* ((default-directory straight--default-directory)
                       (args (car (magit-log-arguments))))
                  (when (and diverged
                             (not (member "--left-right" args)))
                    (setq args (cons "--left-right" args)))
                  (magit-log-setup-buffer (list range) args nil))
                ;; Be careful not to bind `default-directory' around
                ;; recursive edit, see docstring for
                ;; `straight--default-directory'.
                (straight--recursive-edit))))))))))

(defun straight-vc-git--ensure-head-at-branch (local-repo default-branch)
  "Ensure that LOCAL-REPO has its DEFAULT-BRANCH checked out.
Untracked files may cause conflicts on checkout or be deleted without
confirmation by reset, so this function should only be run after
`straight-vc-git--ensure-worktree' has passed."
  (let ((status (straight-vc-git--compare-and-canonicalize
                 default-branch "HEAD")))
    (straight--with-plist status
        (left-ref head-ref same head-detached failure)
      (cond
       (failure
        (straight-vc-git--popup
          (format "In repository %S, failed to get status:\n\n%s"
                  local-repo (straight--split-and-trim failure 2))))
       ((not same)
        (straight-vc-git--reconcile-interactively local-repo status))
       ;; Else if we're at the right commit but on the wrong branch:
       ((not (string= head-ref left-ref))
        (if straight-vc-git-auto-fast-forward
            (straight--process-output "git" "checkout" left-ref)
          (straight-vc-git--popup
            (concat (format "In repository %S, " local-repo)
                    (if head-detached
                        "detached HEAD"
                      (format "HEAD on %S" head-ref))
                    (format " is even with default branch %S."
                            left-ref))
            ("c" (if head-detached
                     (format "Attach HEAD to %S" left-ref)
                   (format "Checkout %S" left-ref))
             (straight--process-output "git" "checkout" left-ref)))))
       ;; Else done; default branch is checked out:
       (t t)))))

(defun straight-vc-git--ensure-default-branch-current
    (local-repo default-branch remote-branch)
  "Ensure LOCAL-REPO's DEFAULT-BRANCH is ahead of REMOTE-BRANCH.
DEFAULT-BRANCH must be checked out, so this function typically runs
after e.g. `straight-vc-git--ensure-local'.
Untracked files may cause conflicts on checkout or be deleted without
confirmation by reset, so this function should only be run after
`straight-vc-git--ensure-worktree' has passed."
  (let ((status (straight-vc-git--compare-and-canonicalize
                 default-branch remote-branch)))
    (straight--with-plist status
        (head-ref left-ref right-ref left-is-ancestor right-is-ancestor
                  failure)
      (cond
       (failure
        (straight-vc-git--popup
          (format "In repository %S, failed to get status:\n\n%s"
                  local-repo (straight--split-and-trim failure 2))))
       ((not (string= head-ref left-ref))
        (error (concat "Error: In repository %S, expected default branch"
                       " %S but %S is checked out instead")
               local-repo default-branch head-ref))
       ;; Upstream is at or behind tracking branch.
       (right-is-ancestor t)
       ;; Auto fast-forward when tracking branch is behind upstream.
       ((and left-is-ancestor straight-vc-git-auto-fast-forward)
        (straight--process-output "git" "merge" "--ff-only" right-ref)
        t)
       (t (straight-vc-git--reconcile-interactively local-repo status))))))

(cl-defun straight-vc-git--merge-from-remote-raw (recipe remote remote-branch)
  "Using straight.el-style RECIPE, merge from REMOTE.
REMOTE is a string. REMOTE-BRANCH is the branch in REMOTE that is
used; it should be a string that is not prefixed with a remote
name."
  (straight-vc-git--destructure recipe
      (local-repo branch)
    (let ((remote-branch (or remote-branch
                             branch
                             (straight-vc-git--default-remote-branch
                              remote local-repo))))
      (while t
        (and (straight-vc-git--ensure-local recipe)
             (or (straight-vc-git--ensure-default-branch-current
                  local-repo remote-branch
                  (format "%s/%s" remote remote-branch))
                 (straight-register-repo-modification local-repo))
             (cl-return-from straight-vc-git--merge-from-remote-raw t))))))

(cl-defun straight-vc-git--pull-from-remote-raw (recipe remote remote-branch)
  "Using straight.el-style RECIPE, pull from REMOTE.
REMOTE is a string. REMOTE-BRANCH is the branch in REMOTE that is
used; it should be a string that is not prefixed with a remote
name."
  (straight-vc-git-fetch-from-remote recipe)
  (straight-vc-git--merge-from-remote-raw recipe remote remote-branch))

(cl-defun straight-vc-git--ensure-head-pushed
    (recipe)
  "Ensure that in RECIPE's local repo, main branch is behind primary remote.
Return non-nil. If no local repository, do nothing and return non-nil."
  (cl-block nil
    (straight-vc-git--destructure recipe
        (local-repo repo branch remote)
      (unless repo
        (cl-return t))
      (let ((push-error-message nil)
            (branch
             (or branch
                 (straight-vc-git--default-remote-branch remote local-repo))))
        (while t
          (while (not (straight-vc-git--ensure-local recipe)))
          (let ((ref (format "%s/%s" remote branch)))
            (when (straight--process-run-p "git" "merge-base" "--is-ancestor"
                                           branch ref)
              (cl-return t))
            (let* ((log (straight--process-output
                         "git" "log" "--format=%h %s"
                         (concat ref ".." branch)))
                   (num-commits (length (straight--split-and-trim log))))
              (straight-vc-git--popup
                (format
                 (concat "In repository %S, branch %S has %d "
                         "commit%s unpushed to %S%s:\n\n%s")
                 local-repo branch num-commits (if (= num-commits 1) "" "s")
                 ref (if (> num-commits 5) ", including" "")
                 (concat
                  (straight--split-and-trim log 2 5)
                  (when push-error-message
                    (concat "\n\nPush failed:\n\n  "
                            push-error-message))))
                ("f" (format "Pull %S into branch %S" ref branch)
                 (straight-vc-git--pull-from-remote-raw recipe remote branch))
                ("p" (format "Push branch %S to %S" branch ref)
                 (when (straight-are-you-sure
                        (format "Really push to %S in %S?" ref local-repo))
                   (straight--catching-quit
                     (straight--process-with-result
                         (straight--process-run
                          "git" "push" remote
                          (format "refs/heads/%s:refs/heads/%s"
                                  branch branch))
                       (unless success
                         (setq push-error-message
                               (string-trim stderr)))))))))))))))

(defun straight-vc-git--ensure-local (recipe)
  "Ensure that local repository for RECIPE is as expected.
This means that the remote URLs are set correctly; there is no
merge currently in progress; the worktree is pristine; and the
primary :branch is checked out. The reason for \"local\" in the
name of this function is that for normal situations, no network
communication is done with the remotes."
  (straight-vc-git--destructure recipe
      (local-repo branch remote)
    (and (straight-vc-git--ensure-remotes recipe)
         (or (and (straight-vc-git--ensure-nothing-in-progress local-repo)
                  (straight-vc-git--ensure-worktree local-repo)
                  (straight-vc-git--ensure-head-at-branch
                   local-repo
                   (or branch (straight-vc-git--default-remote-branch
                               remote local-repo))))
             (straight-register-repo-modification local-repo)))))

(defcustom straight-vc-git-default-clone-depth 'full
  "The default value for `:depth' when `:type' is the symbol `git'.

The value should be the symbol `full' or an integer. If the value
is `full', clone the whole history of repositories. If the value
is an integer N, remote repositories are cloned with the options
--depth N --no-single-branch --no-tags.

The value may also be a list containing one of the above values and
the symbol `single-branch' to override the --no-single-branch option."
  :group 'straight
  :type '(choice integer (const full)))

;;@TODO: clean this function up. We can probably refactor to avoid
;; repetition.
(cl-defun straight-vc-git--clone-internal
    (&key depth remote url repo-dir branch commit)
  "Clone a remote repository from URL.

If DEPTH is the symbol `full', clone the whole history of the
repository. If DEPTH is an integer, pass it to the --depth option
of git-clone to perform a shallow clone. If this fails, try again
to clone without the option --depth and --branch, as a fallback.
If DEPTH is a list, it may specify whether or not to clone a single branch.
e.g. '(full single-branch) translates to --single-branch, whereas
\\='(full) translates to --no-single-branch.

REMOTE is the name of the remote to use \(e.g. \"origin\"; see
`straight-vc-git-default-remote-name'). URL and REPO-DIR are the
positional arguments passed to git-clone(1), and BRANCH is the
name of the default branch (although it won't be checked out as
per --no-checkout).

If COMMIT is non-nil and DEPTH is not `full', then try to clone
only that specific commit from the remote. Fall back to doing a
clone of everything."
  (let ((single-branch-p (when (listp depth)
                           (prog1
                               (eq (cadr depth) 'single-branch)
                             (setq depth (pop depth))))))
    (cond
     ((eq depth 'full)
      ;; Clone the whole history of the repository.
      ;; Binding default directory here to prevent process invocation
      ;; failure if we are recursing due to a failed shallow
      ;; fetch/clone. The previous call will have set the default
      ;; directory to the repo-dir, which no longer exists.
      (let ((default-directory (straight--repos-dir)))
        (apply #'straight--process-run
               "git" "clone" "--origin" remote
               "--no-checkout" url repo-dir
               (if single-branch-p "--single-branch" "--no-single-branch")
               (when branch `("--branch" ,branch)))))
     ((integerp depth)
      ;; Do a shallow clone.
      (if commit
          (let ((straight--default-directory nil)
                (default-directory repo-dir))
            (make-directory repo-dir)
            (straight--process-run "git" "init")
            (when branch (straight--process-run "git" "branch" "-m" branch))
            (apply #'straight--process-run
                   "git" "remote" "add" remote url
                   (when branch `("--master" ,branch)))
            (unless branch
              (straight--process-run
               "git" "branch" "-m"
               (straight-vc-git--default-remote-branch remote repo-dir)))
            (unless (straight--process-run-p "git" "fetch" remote commit
                                             "--depth" (number-to-string depth)
                                             "--no-tags")
              (when (file-exists-p repo-dir)
                (delete-directory repo-dir 'recursive))
              (straight-vc-git--clone-internal :depth 'full
                                               :remote remote
                                               :url url
                                               :repo-dir repo-dir
                                               :branch branch
                                               :commit commit)))
        (when (file-exists-p repo-dir) (delete-directory repo-dir 'recursive))
        (unless (apply #'straight--process-run-p
                       "git" "clone" "--origin" remote
                       "--no-checkout" url repo-dir
                       "--depth" (number-to-string depth)
                       (if single-branch-p "--single-branch"
                         "--no-single-branch")
                       "--no-tags"
                       (when branch `("--branch" ,branch)))
          ;; Fallback for dumb http protocol.
          (when (file-exists-p repo-dir)
            (delete-directory repo-dir 'recursive))
          (straight-vc-git--clone-internal :depth 'full
                                           :remote remote
                                           :url url
                                           :repo-dir repo-dir
                                           :branch branch))))
     (t (error "Invalid value %S of depth for %s" depth url)))))

;;;;;; API

(defun straight-vc-git-clone (recipe commit)
  "Clone local REPO for straight.el-style RECIPE, checking out COMMIT.
COMMIT is a 40-character SHA-1 Git hash. If it cannot be checked
out, signal a warning. If COMMIT is nil, check out the branch
specified in RECIPE instead. If that fails, signal a warning."
  (straight-vc-git--destructure recipe
      (package local-repo branch nonrecursive depth
               remote upstream-remote
               host upstream-host
               protocol upstream-protocol
               repo upstream-repo fork-repo)
    (unless upstream-repo
      (error "No `:repo' specified for package `%s'" package))
    (let ((success nil)
          (repo-dir (straight--repos-dir local-repo))
          (url (straight-vc-git--encode-url repo host protocol))
          (depth (or depth straight-vc-git-default-clone-depth)))
      (unwind-protect
          (progn
            (straight-vc-git--clone-internal :depth depth
                                             :remote remote
                                             :url url
                                             :repo-dir repo-dir
                                             :branch branch
                                             :commit commit)
            (let* ((straight--default-directory nil)
                   (default-directory repo-dir)
                   (branch (or branch
                               (straight-vc-git--default-remote-branch
                                remote local-repo))))
              (when fork-repo
                (let ((url (straight-vc-git--encode-url
                            upstream-repo upstream-host upstream-protocol)))
                  (straight--process-output "git" "remote" "add"
                                            upstream-remote url)
                  (straight--process-output "git" "fetch" upstream-remote)))
              (when commit
                (unless (straight--process-run-p "git" "reset" "--hard" commit)
                  (straight--warn
                   "Could not reset to commit %S in repository %S"
                   commit local-repo)
                  ;; We couldn't check out the commit, best to proceed
                  ;; as if we weren't given one.
                  (setq commit nil)))
              (unless commit
                (unless (straight--process-run-p
                         "git" "checkout" "-B" branch
                         (format "%s/%s" remote branch))
                  (straight--warn
                   "Could not check out branch %S of repository %S"
                   branch local-repo)
                  ;; Since we passed --no-checkout, we need to
                  ;; explicitly check out *something*, even if it's
                  ;; not the right thing.
                  (straight--process-output "git" "checkout" "HEAD")))
              (unless nonrecursive
                (straight--process-output
                 "git" "submodule" "update" "--init" "--recursive")))
            (setq success t))
        (if (not success)
            ;; Make cloning an atomic operation.
            (when (file-exists-p repo-dir)
              (delete-directory repo-dir 'recursive))
          (run-hook-with-args 'straight-vc-git-post-clone-hook
                              :repo-dir repo-dir :remote remote :url url
                              :branch branch :depth depth :commit commit))))))

(cl-defun straight-vc-git-normalize (recipe)
  "Using straight.el-style RECIPE, make the repository locally sane.
This means that its remote URLs are set correctly; there is no
merge currently in progress; its worktree is pristine; and the
primary :branch is checked out."
  (straight-vc-git--destructure recipe
      (local-repo)
    (while t
      (and (or (straight-vc-git--ensure-local recipe)
               (straight-register-repo-modification local-repo))
           (cl-return-from straight-vc-git-normalize t)))))

(cl-defun straight-vc-git-fetch-from-remote (recipe &optional from-upstream)
  "Using straight.el-style RECIPE, fetch from the primary remote.
If FROM-UPSTREAM is non-nil, fetch from the upstream remote
instead, if the recipe configures a fork. The FROM-UPSTREAM
argument is not part of the VC API."
  (cl-block nil
    (straight-vc-git--destructure recipe
        (upstream-repo upstream-remote repo remote fork)
      (when (and from-upstream (not fork))
        (cl-return t))
      (let ((repo (if from-upstream upstream-repo repo))
            (remote (if from-upstream upstream-remote remote)))
        (unless repo
          (cl-return t))
        (while t
          (and (straight-vc-git--ensure-remotes recipe)
               (straight--process-output "git" "fetch" remote)
               (cl-return t)))))))

(cl-defun straight-vc-git-fetch-from-upstream (recipe)
  "Using straight.el-style RECIPE, fetch from the upstream remote.
If RECIPE does not configure a fork, do nothing."
  (straight-vc-git-fetch-from-remote recipe 'from-upstream))

(cl-defun straight-vc-git--default-remote-branch (remote &optional local-repo)
  "Return the default remote branch of LOCAL-REPO, with remote name REMOTE.
If LOCAL-REPO is not specified, assume we are the correct
directory for the repository. If there is no remote repository,
return nil."
  (let* ((process-environment
          ;;force locale for git command
          (append (list "LC_ALL=C") process-environment))
         (default-directory (if local-repo
                                (let ((d (straight--repos-dir local-repo)))
                                  ;; New repositories do not yet
                                  ;; exist, so we don't want to switch
                                  ;; to them.
                                  (if (file-directory-p d) d
                                    default-directory))
                              default-directory))
         (branch-list (straight--process-output "git" "branch" "-r")))
    (if (string-match "^.*origin/HEAD -> origin/\\(.*$\\)" branch-list)
        (match-string 1 branch-list)
      ;; git doesn't always have the default remote branch name
      ;; available locally. For these cases, we have to look at the
      ;; remote. This is more reliable but also involves is slower, so
      ;; we do this later.
      (when branch-list
        (straight--process-with-result
            (straight--process-run "git" "remote" "show" remote)
          (when success
            (replace-regexp-in-string ".*: \\(.*\\)$" "\\1"
                                      (nth 3 (split-string stdout "\n")))))))))

(cl-defun straight-vc-git-merge-from-remote (recipe &optional from-upstream)
  "Using straight.el-style RECIPE, merge from the primary remote.
If FROM-UPSTREAM is non-nil, merge from the upstream remote
instead, if RECIPE configures a fork. The FROM-UPSTREAM argument
is not part of the VC API."
  (cl-block nil
    (straight-vc-git--destructure recipe
        (upstream-repo upstream-branch upstream-remote
                       repo branch remote fork)
      (when (and from-upstream (not fork))
        (cl-return t))
      (let* ((repo (if from-upstream upstream-repo repo))
             (remote (if from-upstream upstream-remote remote))
             (remote-branch
              (or (if from-upstream upstream-branch branch)
                  (straight-vc-git--default-remote-branch remote repo))))
        (unless repo
          (cl-return t))
        (straight-vc-git--merge-from-remote-raw
         recipe remote remote-branch)))))

(defun straight-vc-git-merge-from-upstream (recipe)
  "Using straight.el-style RECIPE, merge from upstream.
If RECIPE does not configure a fork, do nothing."
  (straight-vc-git-merge-from-remote recipe 'from-upstream))

(cl-defun straight-vc-git-push-to-remote (recipe)
  "Using straight.el-style RECIPE, push to primary remote, if necessary."
  (straight-vc-git--ensure-head-pushed recipe))

(cl-defun straight-vc-git-check-out-commit (recipe commit)
  "In RECIPE's repo, normalize and check out COMMIT.
RECIPE is a straight.el-style recipe. COMMIT is a 40-character
string identifying a Git commit."
  (straight-vc-git--destructure recipe
      (local-repo)
    (cl-block nil
      (while t
        (or (and (straight-vc-git--ensure-nothing-in-progress local-repo)
                 (straight-vc-git--ensure-worktree local-repo)
                 (straight-vc-git--ensure-local recipe)
                 (or (equal
                      commit (straight--process-output
                              "git" "rev-parse" "HEAD"))
                     (straight--process-output "git" "reset" "--hard" commit))
                 (cl-return))
            (straight-register-repo-modification local-repo))))))

(cl-defun straight-vc-git-commit-present-p (_local-repo commit)
  "Return non-nil if LOCAL-REPO has COMMIT present locally."
  (straight--process-run-p "git" "rev-parse" "-q" "--verify"
                           (format "%s^{commit}" commit)))

(defun straight-vc-git-get-commit (_local-repo)
  "Return the current commit for the current local repository.
This is a 40-character string identifying the current position of
HEAD in the Git repository."
  (straight--process-output "git" "rev-parse" "HEAD"))

(defun straight-vc-git-local-repo-name (recipe)
  "Generate a repository name from straight.el-style RECIPE.
For the GitHub, GitLab, and Bitbucket hosts, the repository name
is used as-is. Otherwise, an attempt is made to extract the
repository name from the URL. This may still fail, and nil is
then returned."
  (straight--with-plist recipe
      (repo host)
    (if host
        (replace-regexp-in-string
         "^.+/" "" repo)
      ;; The following is a half-hearted attempt to turn arbitrary
      ;; URLs into reasonable repository names.
      (let ((regexp "^.*/\\(.+\\)\\.git$"))
        ;; If this regexp does not match, just return nil.
        (when (string-match regexp repo)
          (match-string 1 repo))))))

(defun straight-vc-git-keywords ()
  "Return a list of keywords used by the VC backend for Git."
  '(:repo :host :branch :remote :nonrecursive
          :upstream :fork :depth :protocol))

;;;; Fetching repositories

(defun straight--repository-is-available-p (recipe)
  "Determine if the repository for the RECIPE exists locally."
  (straight--with-plist recipe
      (local-repo)
    (file-exists-p (straight--repos-dir local-repo))))

(defun straight--clone-repository (recipe &optional cause)
  "Clone the repository for the RECIPE, erroring if it already exists.
CAUSE is a string indicating the reason this repository is being
cloned."
  (straight--with-plist recipe
      (package local-repo)
    (make-directory (straight--repos-dir) 'parents)
    (straight--with-progress
        (concat cause (when cause straight-arrow)
                (format "Cloning %s" local-repo)
                ;; If this `member' check fails, then it means the
                ;; repository has a name that is substantially
                ;; different than the package name, and the user might
                ;; be confused about why we are cloning it.
                (unless (member local-repo
                                (list
                                 package
                                 (format "%s.el" package)
                                 (format "emacs-%s" package)))
                  (format " (for %s)" package)))
      (straight-vc-clone recipe))
    ;; We messed up the echo area.
    (setq straight--echo-area-dirty t)))

;;;; Recipe handling
;;;;; Built-in packages

(defvar straight--cached-built-in-packages nil
  "Hash table mapping package names to booleans.
All packages that are built in are mapped to non-nil. The value
of this variable is computed the first time
`straight--package-built-in-p' is called.")

(defun straight--package-built-in-p (package)
  "Given PACKAGE symbol, return non-nil if it's built in to Emacs.
The return value of this function might change between different
versions of Emacs for the same package.

If a package is built in, then the package won't be listed in GNU
ELPA (Mirror) and it won't be an error if no recipe can be found
for it."
  (unless straight--cached-built-in-packages
    (require 'finder-inf)
    (let ((table (make-hash-table)))
      (dolist (cell package--builtins)
        (puthash (car cell) t table))
      (setq straight--cached-built-in-packages table)))
  (gethash package straight--cached-built-in-packages))

;;;;; Declaration of caches

(defvar straight--recipe-cache (make-hash-table :test #'equal)
  "Hash table listing known recipes by package.
The keys are strings naming packages, and the values are the last
known recipe for that package. This is used for detecting
conflicting recipes for the same package; managing the build
cache and versions lockfile; and getting a list of all packages
in use.")

(defvar straight--repo-cache (make-hash-table :test #'equal)
  "Hash table listing known recipes by repository.
The keys are strings naming repositories, and the values are the
last known recipe that referenced the corresponding repository.
This is used for detecting conflicts (when multiple packages are
versioned in the same repository, but are specified with
incompatible recipes) and for silently adjusting recipes drawn
from recipe repositories so as to avoid conflicts.")

(defvar straight--profile-cache (make-hash-table :test #'equal)
  "Hash table mapping packages to lists of profiles.
The keys are strings naming packages, and the values are lists of
symbols identifying package profiles. These symbols are the
values that you bind `straight-current-profile' to, and they
should each have an entry in `straight-profiles'.")

(defvar straight--recipe-lookup-cache nil
  "Hash table keeping track of cached recipe lookups, or nil.
The keys are strings naming recipe repositories (i.e. specific
package names), and the values are hash tables recording cached
data for those recipe repositories. The value hash tables have
keys which are strings naming packages and values which are
MELPA-style recipes, or nil (meaning that the recipe repository
did not have a recipe for the package).")

(defvar straight--profile-cache-valid nil
  "Non-nil if `straight--profile-cache' accurately reflects the init-file.
The function `straight-freeze-versions' will be reluctant to
create a version lockfile if this variable is nil. This variable
is set to non-nil in the bootstrap code, and set back to nil when
`straight-use-package' is invoked outside of init.")

(defvar straight--functional-p nil
  "Non-nil if package operations are guaranteed to be functional.
This means they faithfully represent the contents of the
init-file. If package operations are performed when this variable
is nil, then `straight--profile-cache-valid' is set to nil.")

(defun straight--reset-caches ()
  "Reset caches tied to the init process.
This means `straight--recipe-cache', `straight--repo-cache', and
`straight--profile-cache'. (We don't ever want to reset the build
cache since it is a totally separate system from the caches
employed by `straight--convert-recipe', and we don't ever want to
reset the success cache since that would mean the user would
receive a duplicate message if they called `straight-use-package'
interactively, reloaded their init-file, and then called
`straight-use-package' on the same package again. The recipe
lookup cache is also part of the build cache.)

Also manage the internal variables
`straight--profile-cache-valid' and `straight--functional-p'
using the transaction system."
  (setq straight--recipe-cache (make-hash-table :test #'equal))
  (setq straight--repo-cache (make-hash-table :test #'equal))
  (setq straight--profile-cache (make-hash-table :test #'equal))
  (setq straight--profile-cache-valid t)
  (straight--transaction-exec
   'reset-caches
   :now
   (lambda ()
     (setq straight--functional-p t))
   :later
   (lambda ()
     (setq straight--functional-p nil))))

;;;;; Recipe repositories

(defvar straight--recipe-repository-stack nil
  "A list of recipe repositories that are currently being searched.
This is used to detect and prevent an infinite recursion when
searching for recipe repository recipes in other recipe
repositories.

If you set this globally to something other than nil, beware of
velociraptors.")

(defun straight-recipes (method name cause &rest args)
  "Call a recipe backend method.
METHOD is a symbol naming a backend method, like symbol
`retrieve'. NAME is a symbol naming the recipe repository, like
symbol `melpa'.

If the package repository is not available, clone it. If the
package cannot be found, return nil. CAUSE is a string explaining
why the recipe repository might need to be cloned.

ARGS are passed to the method.

This function sets `default-directory' appropriately, handles
cloning the repository if necessary, and then delegates to the
appropriate `straight-recipes-NAME-METHOD' function.

For example:
   (straight-recipes \\='retrieve \\='melpa ...)
=> (straight-recipes-melpa-retrieve ...)"
  (unless (memq name straight--recipe-repository-stack)
    (let ((straight--recipe-repository-stack
           (cons name straight--recipe-repository-stack)))
      ;; This is purely for cloning the recipe repository. It's
      ;; explicitly *not* designed to support the use case of looking
      ;; up the recipe for one recipe repository inside another recipe
      ;; repository without first running `straight-register-package'
      ;; for the first recipe repository ahead of time (doing so
      ;; produces undefined behavior).
      (straight-use-package name nil nil cause)
      (let ((recipe (straight--convert-recipe name cause)))
        (straight--with-plist recipe
            (local-repo)
          (let ((default-directory
                  ;; Only change directories if a local repository is
                  ;; specified. If one is not, then we assume the
                  ;; recipe repository code does not need to be in any
                  ;; particular directory.
                  (if local-repo
                      (straight--repos-dir local-repo)
                    default-directory))
                (func (intern (format "straight-recipes-%S-%S"
                                      name method))))
            (apply func args)))))))

(defun straight-recipes-retrieve (package &optional sources cause)
  "Look up a PACKAGE recipe in one or more SOURCES.
PACKAGE should be a symbol, and SOURCES should be a list that is
a subset of `straight-recipe-repositories'. (If it is omitted, it
defaults to allowing all sources in
`straight-recipe-repositories'.) If the recipe is not found in
any of the provided sources, return nil. CAUSE is a string
indicating the reason recipe repositories might need to be
cloned."
  ;; Oh god, I lost so much time debugging this because package names
  ;; are usually strings at this level of the code.
  (setq package (symbol-name package))
  (let* (;; If `sources' is omitted, allow all sources.
         (sources (or sources straight-recipe-repositories))
         ;; Update the `cause' to explain why repositories might be
         ;; getting cloned.
         (cause (concat cause (when cause straight-arrow)
                        (format "Looking for %s recipe" package))))
    (cl-dolist (source sources)
      (let ((table (gethash source straight--recipe-lookup-cache)))
        (if (and table (straight--checkhash package table))
            ;; Don't `cl-return' nil anywhere in this method. That will
            ;; prevent us from checking the other recipe repositories.
            (when-let ((recipe (gethash package table)))
              (cl-return recipe))
          (when-let
              ((recipe
                ;; NB: we use strings for the package names. This is
                ;; not just for convenience; it also allows us to
                ;; support having a package called `version' while
                ;; simultaneously using a symbol key called `version'
                ;; to keep track of the recipe repository lookup logic
                ;; version number.
                (puthash package
                         (straight-recipes 'retrieve source cause
                                           (intern package))
                         (or (gethash source straight--recipe-lookup-cache)
                             (let ((table (make-hash-table :test #'equal))
                                   (func (intern
                                          (format
                                           "straight-recipes-%S-version"
                                           source))))
                               (when-let ((version (and (fboundp func)
                                                        (funcall func))))
                                 (puthash 'version version table))
                               (puthash source table
                                        straight--recipe-lookup-cache)
                               table)))))
            (cl-return recipe)))))))

(defun straight-recipes-list (&optional sources cause)
  "List recipes available in one or more SOURCES.
PACKAGE should be a symbol, and SOURCES should be a list that is
a subset of `straight-recipe-repositories'. (If it is omitted, it
defaults to allowing all sources in
`straight-recipe-repositories'.)

CAUSE is a string indicating why recipe repositories might need
to be cloned.

Return a list of package names as strings."
  (let ((sources (or sources straight-recipe-repositories))
        (recipes nil))
    (dolist (source sources (sort (delete-dups recipes)
                                  #'string-lessp))
      (let ((cause (concat cause (when cause straight-arrow)
                           (format "Listing %S recipes" source))))
        (setq recipes (nconc recipes (straight-recipes
                                      'list source cause)))))))

(defcustom straight-built-in-pseudo-packages '(emacs nadvice python)
  "List of built-in packages that aren't real packages.
If any of these are specified as dependencies, straight.el will
just skip them instead of looking for a recipe.

Another application of this variable is to correctly handle the
situation where a package is built-in but Emacs incorrectly
claims that it's not (see
<https://github.com/raxod502/straight.el/issues/548>).

Note that straight.el can deal with built-in packages even if
this variable is set to nil. This just allows you to tell
straight.el to not even bother cloning recipe repositories to
look for recipes for these packages."
  :type '(repeat symbol))

(defun straight-recipe-source (package)
  "Return recipe respository used to obtain PACKAGE recipe.
If package is not found in any `straight-recipe-repositories', return nil."
  (unless (member (intern package)
                  (append straight-built-in-pseudo-packages '(straight)))
    (cl-some
     (lambda (repo)
       (let ((recipe-repo (gethash repo straight--recipe-lookup-cache)))
         (and (gethash package recipe-repo) repo)))
     straight-recipe-repositories)))

;;;;;; Org
(defcustom straight-byte-compilation-buffer "*straight-byte-compilation*"
  "Name of the byte compilation log buffer.
If nil, output is discarded."
  :type '(or (string :tag "Buffer name") (const :tag "Discard output" nil)))

(make-obsolete-variable
 'straight-fix-org
 "No longer necessary, as straight.el supports external build commands."
 "2020-07-28")

(defun straight-recipes-org-elpa--build ()
  "Generate `org-version.el`.
This is to avoid relying on `make` on Windows.
See: https://github.com/raxod502/straight.el/issues/707"
  (let* ((default-directory (straight--repos-dir "org" "lisp"))
         (orgversion
          (replace-regexp-in-string
           "release_" ""
           (straight--process-output "git" "describe" "--match" "release*"
                                     "--abbrev=0" "HEAD")))
         (gitversion
          (concat orgversion "-g" (straight--process-output
                                   "git" "rev-parse" "--short=6" "HEAD")))
         (emacs (concat invocation-directory invocation-name)))
    (call-process
     emacs nil straight-byte-compilation-buffer nil
     "-Q" "--batch"
     "--eval" "(setq vc-handled-backends nil org-startup-folded nil)"
     "--eval" "(add-to-list 'load-path \".\")"
     "--eval" "(load \"org-compat.el\")"
     "--eval" "(load \"../mk/org-fixup.el\")"
     ;; Do we want autoloads here, or should straight handle it?
     "--eval" "(org-make-org-loaddefs)"
     "--eval" (format "(org-make-org-version %S %S)"
                      orgversion gitversion))))

(defun straight-recipes-org-elpa-retrieve (package)
  "Look up a pseudo-PACKAGE recipe in Org ELPA.
PACKAGE must be either `org' or `org-contrib'.
Otherwise return nil."
  (pcase package
    ('org
     (list package
           :type 'git
           :repo "https://code.orgmode.org/bzg/org-mode.git"
           :local-repo "org"
           ;; `org-version' depends on repository tags.
           :depth 'full
           :pre-build '(straight-recipes-org-elpa--build)
           :build '(:not autoloads)
           :files '(:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))
    ('org-contrib
     (list package
           :type 'git
           :includes '(ob-arduino ; Intentionally short for indentation
                       ob-clojure-literate ob-csharp ob-eukleides
                       ob-fomus ob-julia ob-mathematica ob-mathomatic ob-oz
                       ob-php ob-redis ob-sclang ob-smiles ob-spice ob-stata
                       ob-tcl ob-vbnet ol-bookmark ol-elisp-symbol ol-git-link
                       ol-man ol-mew ol-notmuch ol-vm ol-wl org-annotate-file
                       org-attach-embedded-images org-bibtex-extras
                       org-checklist org-choose org-collector org-contacts
                       org-contribdir org-depend org-effectiveness org-eldoc
                       org-eval org-eval-light org-expiry
                       org-interactive-query org-invoice org-learn org-license
                       org-mac-iCal org-mac-link org-mairix org-notify
                       org-panel org-passwords org-registry org-screen
                       org-screenshot org-secretary org-static-mathjax
                       org-sudoku orgtbl-sqlinsert org-toc org-track
                       org-velocity org-wikinodes ox-bibtex ox-confluence
                       ox-deck ox-extra ox-freemind ox-groff ox-koma-letter
                       ox-rss ox-s5 ox-taskjuggler)
           :repo "https://git.sr.ht/~bzg/org-contrib"
           :files '(:defaults "lisp/*.el")))))

(defun straight-recipes-org-elpa-list ()
  "Return a list of Org ELPA pseudo-packages, as a list of strings."
  (list "org" "org-contrib"))

(defun straight-recipes-org-elpa-version ()
  "Return the current version of the Org ELPA retriever."
  9)

;;;;;; MELPA

(defun straight-recipes-melpa-retrieve (package)
  "Look up a PACKAGE recipe in MELPA.
PACKAGE should be a symbol. If the package has a recipe listed in
MELPA that uses one of the Git fetchers, return it; otherwise
return nil."
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally
           (expand-file-name (symbol-name package) "recipes/"))
          (let ((melpa-recipe (read (current-buffer)))
                (plist nil))
            (cl-destructuring-bind (name . melpa-plist) melpa-recipe
              (straight--put plist :type 'git)
              (straight--put plist :flavor 'melpa)
              (when-let ((files (plist-get melpa-plist :files)))
                ;; We must include a *-pkg.el entry in the recipe
                ;; because that file always needs to be linked over,
                ;; if it is present, but the `:files' directive might
                ;; not include it (and doesn't need to, because MELPA
                ;; always re-creates a *-pkg.el file regardless). See
                ;; https://github.com/raxod502/straight.el/issues/336.
                (straight--put
                 plist :files
                 (append files (list (format "%S-pkg.el" package)))))
              (when-let ((branch (plist-get melpa-plist :branch)))
                (straight--put plist :branch branch))
              (pcase (plist-get melpa-plist :fetcher)
                ('git (straight--put plist :repo (plist-get melpa-plist :url)))
                ((or 'github 'gitlab)
                 (straight--put plist :host (plist-get melpa-plist :fetcher))
                 (straight--put plist :repo (plist-get melpa-plist :repo)))
                ;; This error is caught by `condition-case', no need
                ;; for a message.
                (_ (error "")))
              (cons name plist))))
      (error nil))))

(defun straight-recipes-melpa-list ()
  "Return a list of recipes available in MELPA, as a list of strings."
  (straight--directory-files "recipes" "^[^.]"))

(defun straight-recipes-melpa-version ()
  "Return the current version of the MELPA retriever."
  2)

;;;;;; GNU ELPA

(defcustom straight-recipes-gnu-elpa-use-mirror t
  "Non-nil means to retrieve GNU ELPA packages via a mirror.
This means that all the packages work, even the `externals-list'
ones (e.g. `auctex'). However, you will not be able to contribute
changes back to GNU ELPA directly from the repository. This
should not be a major concern since the GNU ELPA build system
does such a good job of discouraging contributions anyway."
  :type 'boolean)

(defcustom straight-recipes-gnu-elpa-ignored-packages
  '(cl-generic
    cl-lib
    nadvice
    seq)
  "Packages from GNU ELPA that we should pretend don't exist.
Such packages would break things if they were installed. For
example, the `cl-lib' package from GNU ELPA is not the
development version but rather an obsolete forwards-compatibility
package designed for use with Emacs 24.2 and earlier. See
<https://github.com/raxod502/straight.el/issues/531> for some
discussion."
  :type '(list symbol))

;;;;;;; GNU ELPA mirror

(defun straight-recipes-gnu-elpa-mirror-retrieve (package)
  "Look up a PACKAGE recipe in the GNU ELPA mirror.
PACKAGE should be a symbol. If the package is maintained in GNU
ELPA (and should be retrieved from there, which isn't the case if
the package is built in to Emacs), return a MELPA-style recipe.
Otherwise, return nil."
  (unless (memq package straight-recipes-gnu-elpa-ignored-packages)
    (when (file-exists-p (symbol-name package))
      `(,package :type git
                 :host github
                 :repo ,(format "emacs-straight/%S" package)
                 ;; Kinda weird, but in fact this is how package.el
                 ;; works. So if we want to replicate the build
                 ;; process, we should trust that the gnu-elpa-mirror
                 ;; put the correct files into the repository, and
                 ;; then just link *everything*. As an FYI, if we
                 ;; don't do this, then AUCTeX suffers problems with
                 ;; style files, see
                 ;; <https://github.com/raxod502/straight.el/issues/423>.
                 :files ("*" (:exclude ".git"))))))

(defun straight-recipes-gnu-elpa-mirror-list ()
  "Return a list of recipe names available in the GNU ELPA mirror.
This is a list of strings."
  (cl-remove-if
   (lambda (package)
     (memq (intern package) straight-recipes-gnu-elpa-ignored-packages))
   (straight--directory-files)))

(defun straight-recipes-gnu-elpa-mirror-version ()
  "Return the current version of the GNU ELPA mirror retriever."
  3)

;;;;;;; GNU ELPA source

(defcustom straight-recipes-gnu-elpa-url
  "https://git.savannah.gnu.org/git/emacs/elpa.git"
  "URL of the Git repository for the GNU ELPA package repository."
  :type 'string)

(defun straight-recipes-gnu-elpa-retrieve (package)
  "Look up a PACKAGE recipe in GNU ELPA.
PACKAGE should be a symbol. If the package is maintained in GNU
ELPA (and should be retrieved from there, which isn't the case if
the package is built in to Emacs), return a MELPA-style recipe.
Otherwise, return nil."
  (unless (memq package straight-recipes-gnu-elpa-ignored-packages)
    (when (file-exists-p (expand-file-name (symbol-name package) "packages/"))
      ;; All the packages in GNU ELPA are just subdirectories of the
      ;; same repository.
      `(,package :type git
                 :repo ,straight-recipes-gnu-elpa-url
                 :files (,(format "packages/%s/*.el"
                                  (symbol-name package)))
                 :local-repo "elpa"))))

(defun straight-recipes-gnu-elpa-list ()
  "Return a list of recipe names available in GNU ELPA, as a list of strings."
  (cl-remove-if
   (lambda (package)
     (memq (intern package) straight-recipes-gnu-elpa-ignored-packages))
   (straight--directory-files "packages/")))

(defun straight-recipes-gnu-elpa-version ()
  "Return the current version of the GNU ELPA retriever."
  2)

;;;;;; Emacsmirror

(defcustom straight-recipes-emacsmirror-use-mirror t
  "Non-nil means to retrieve Emacsmirror packages via a mirror.
There is no disadvantage to doing this, and cloning the mirror is
much faster than cloning the official Emacsmirror."
  :type 'boolean)

;;;;;;; Emacsmirror mirror

(defun straight-recipes-emacsmirror-mirror-retrieve (package)
  "Look up a PACKAGE recipe in the Emacsmirror mirror.
PACKAGE should be a symbol. If the package is available from
Emacsmirror, return a MELPA-style recipe; otherwise return nil."
  (cl-block nil
    (let ((mirror-package (intern
                           (replace-regexp-in-string
                            "\\+" "-plus" (symbol-name package)
                            'fixedcase 'literal))))
      (dolist (org '("mirror" "attic"))
        (with-temp-buffer
          (insert-file-contents org)
          (when (re-search-forward
                 (format "^%S$" mirror-package) nil 'noerror)
            (cl-return
             `(,package :type git :host github
                        :repo ,(format "emacs%s/%S" org mirror-package)))))))))

(defun straight-recipes-emacsmirror-mirror-list ()
  "Return a list of recipes available in Emacsmirror, as a list of strings."
  (let ((packages nil))
    (dolist (org '("mirror" "attic"))
      (with-temp-buffer
        (insert-file-contents-literally org)
        (setq packages (nconc (mapcar
                               (lambda (package)
                                 (replace-regexp-in-string
                                  "-plus\\b" "+" package 'fixedcase 'literal))
                               (split-string
                                (buffer-string) "\r?\n" 'omit-nulls))
                              packages))))
    packages))

(defun straight-recipes-emacsmirror-mirror-version ()
  "Return the current version of the Emacsmirror mirror retriever."
  2)

;;;;;;; Emacsmirror source

(defun straight-recipes-emacsmirror-retrieve (package)
  "Look up a PACKAGE recipe in Emacsmirror.
PACKAGE should be a symbol. If the package is available from
Emacsmirror, return a MELPA-style recipe; otherwise return nil."
  ;; Try to get the URL for the submodule. If it doesn't exist,
  ;; return nil. This will work both for packages in the mirror
  ;; and packages in the attic.
  (when-let ((url (condition-case nil
                      (straight--process-output
                       "git" "config" "--file" ".gitmodules"
                       "--get" (format "submodule.%s.url"
                                       (symbol-name package)))
                    (error nil))))
    (and (not (string-empty-p url))
         ;; For the sake of elegance, we convert Github URLs to
         ;; use the `github' fetcher, if possible. At the time of
         ;; this writing, there are no Gitlab URLs (which makes
         ;; sense, since all the repositories should be hosted on
         ;; github.com/emacsmirror).
         (cl-destructuring-bind (repo host _protocol)
             (straight-vc-git--decode-url url)
           (if host
               `(,package :type git :host ,host
                          :repo ,repo)
             `(,package :type git :repo ,repo))))))

(defun straight-recipes-emacsmirror-list ()
  "Return a list of recipes available in Emacsmirror, as a list of strings."
  (append
   (straight--directory-files "mirror")
   (straight--directory-files "attic")))

(defun straight-recipes-emacsmirror-version ()
  "Return the current version of the Emacsmirror retriever."
  2)

;;;;;; el-get

;;;;;; shims for el-get variables/functions
;; If possible, we would like to avoid loading `el-get'
;; to make use of its recipes. Very few recipes make use of the
;; `:build` commands. A subset of those recipes use some
;; variables or functions to compute part of :build commands.
;; This is an incomplete list of shims, but it should handle most cases.

(defun straight--el-get-emacs ()
  "Return path to currently running Emacs.
Shim for `el-get-emacs' variable."
  (concat invocation-directory invocation-name))

(defun straight--el-get-install-info ()
  "Shim for `el-get-install-info' variable."
  (or (executable-find "ginstall-info")
      (executable-find "install-info")))

(defun straight--el-get-package-directory (package)
  "Return absolute path to PACKAGE repository.
Shim for `el-get-package-directory'."
  (straight--repos-dir (format "%S" package)))

(defmacro straight--recipes-el-get-build-commands (plist)
  "Convert `el-get' PLIST's :build/* commands to a programmatic elisp form.
Return nil if no :build/* commands are available."
  (let* ((names
          (cl-sort
           (delq nil
                 (mapcar (lambda (el)
                           (when (keywordp el)
                             (let ((name (symbol-name el)))
                               (and (string-prefix-p ":build" name) name))))
                         plist))
           ;; Sort so :build is the last case in the expanded pcase.
           (lambda (a b) (> (length a) (length b)))))
         (systems
          (mapcar (lambda (name)
                    (let ((system (replace-regexp-in-string "^:build/?"
                                                            "" name))
                          (value (plist-get plist (intern name))))
                      `(,(if (string-empty-p system)
                             '_
                           (list '\` (intern system)))
                        ,(if (symbolp (car value))
                             value
                           (list '\` (if (straight--quoted-form-p value)
                                         (cadr value)
                                       value))))))
                  names)))
    (list '\,
          `(cl-letf (((symbol-function #'el-get-package-directory)
                      (lambda (package)
                        (straight--repos-dir (format "%S" package))))
                     (el-get-install-info (straight--el-get-install-info))
                     (el-get-emacs (straight--el-get-emacs))
                     (el-get-dir (straight--repos-dir)))
             (pcase system-type ,@systems)))))

(defun straight--el-get-flatten (arg)
  "Return a version of ARG as a one-level list."
  (if (listp arg)
      (apply 'append (mapcar #'straight--el-get-flatten arg))
    (list arg)))

(defun straight-recipes-el-get-retrieve (package)
  "Look up a an `el-get' PACKAGE's recipe.
PACKAGE must be a symbol. If the package has an `el-get' recipe that
uses one of the Git fetchers, return it; otherwise return nil."
  (with-temp-buffer
    (ignore-errors
      (progn
        (insert-file-contents-literally
         (expand-file-name (concat (symbol-name package) ".rcp")
                           "recipes/"))
        (let ((recipe (read (current-buffer)))
              plist)
          (straight--with-plist recipe
              (type name url branch load-path ((:pkgname repo)))
            (when (stringp name) (setq name (intern name)))
            (when (member type '(git github))
              (straight--put plist :type 'git)
              (if (eq type 'git)
                  (straight--put plist :repo url)
                (straight--put plist :host 'github)
                (straight--put plist :repo repo))
              (when branch (straight--put plist :branch branch))
              ;; Differentiate between recipe explicitly declaring
              ;; :autoloads nil and recipe not declaring :autoloads.
              (when-let ((autoloads (plist-member recipe :autoloads)))
                ;; ignore    :autoloads t
                ;; translate :autoloads nil -> :build (:not autoloads)
                ;; el-get also allows a file or list of files which are
                ;; ignored for now.
                (unless (cadr autoloads)
                  (straight--put plist :build '(:not autoloads))))
              (when (plist-get recipe :build)
                (straight--put plist :pre-build
                               (macroexpand
                                `(straight--recipes-el-get-build-commands
                                  ,recipe))))
              (straight--put
               plist :files
               (append '(:defaults)
                       (cl-remove-if (lambda (file)
                                       (string= "." file))
                                     (straight--el-get-flatten load-path))))
              (list '\` (cons name plist)))))))))


(defun straight-recipes-el-get-list ()
  "Return a list of recipes available in `el-get', as a list of strings."
  (mapcar #'file-name-sans-extension
          (straight--directory-files "recipes")))

(defun straight-recipes-el-get-version ()
  "Return the current version of the `el-get' retriever."
  1)

;;;;; Recipe conversion

(defvar straight--build-keywords '(:build
                                   :files
                                   :flavor
                                   :local-repo
                                   :pre-build
                                   :post-build)
  "Keywords that affect how a package is built locally.
If the values for any of these keywords change, then package
needs to be rebuilt. See also `straight-vc-keywords'.")

(cl-defun straight--convert-recipe (melpa-style-recipe &optional cause)
  "Convert a MELPA-STYLE-RECIPE to a normalized straight.el recipe.
Recipe repositories specified in `straight-recipe-repositories'
may be cloned and searched for recipes if the MELPA-STYLE-RECIPE
is just a package name; otherwise, the MELPA-STYLE-RECIPE should
be a list and it is modified slightly to conform to the internal
straight.el recipe format. CAUSE is a string indicating the
reason recipe repositories might need to be cloned.

Return nil if MELPA-STYLE-RECIPE was just a symbol, and no recipe
could be found for it, and package.el indicates that the package
is built in to Emacs (e.g. the \"emacs\" package). This is used
for dependency resolution."
  ;; Special case for the `emacs' pseudo-package and similar, so that
  ;; by default we don't try to look up a recipe in recipe
  ;; repositories.
  (when (memq melpa-style-recipe straight-built-in-pseudo-packages)
    (cl-return-from straight--convert-recipe
      `(:type built-in :package ,(symbol-name melpa-style-recipe))))
  ;; Firstly, if the recipe is only provided as a package name, and
  ;; we've already converted it before, then we should just return the
  ;; previous result. This has nothing to do with efficiency; it's
  ;; actually to reduce conflicts. There are a couple of common cases:
  ;;
  ;; 1. I'm overriding the standard recipe for a package with a custom
  ;;    recipe, and then loading a second package that requires the
  ;;    first one as a dependency. In that case, loading the second
  ;;    package will cause the first package to be loaded again,
  ;;    without any special information provided about its
  ;;    recipe (i.e. the recipe is supposed to be looked up by
  ;;    `straight--convert-recipe' in the recipe repositories). But
  ;;    that will cause a conflict, since a different recipe was
  ;;    already provided. This problem is solved by the following two
  ;;    lines of code. (Note, though, that there is still a conflict
  ;;    if you load the second package before its custom-recipe
  ;;    dependency, as should be expected.)
  ;;
  ;; 2. I'm loading two different features from the same package with
  ;;    `use-package', and providing a non-standard recipe for the
  ;;    package. For example, this comes up when you need to load both
  ;;    `tex-site' and `tex' from your fork of `auctex'. It's
  ;;    inconvenient to specify your custom recipe in both
  ;;    `use-package' declarations, but with the following two lines
  ;;    of code, you can specify your custom recipe in the first
  ;;    `use-package' declaration and then specify only `auctex' as
  ;;    the recipe in the second `use-package' declaration.
  ;;
  ;; 3. I'm using `straight-rebuild-package' or
  ;;    `straight-rebuild-all', which both call `straight-use-package'
  ;;    with just the package name and expect this not to introduce
  ;;    conflicts.
  (or (and (symbolp melpa-style-recipe)
           (gethash (symbol-name melpa-style-recipe) straight--recipe-cache))
      (let* (;; It's important to remember whether the recipe was
             ;; provided explicitly, or if it was just given as a
             ;; package name (meaning that the recipe needs to be
             ;; looked up in a recipe repository, i.e. something in
             ;; `straight-recipe-repositories'). Why, you ask? It's so
             ;; that we can be a little more tolerant of conflicts in
             ;; certain cases -- see the comment below, before the
             ;; block of code that runs when `recipe-specified-p' is
             ;; nil.
             (recipe-specified-p (listp melpa-style-recipe))
             ;; Now we normalize the provided recipe so that it is
             ;; still a MELPA-style recipe, but it is guaranteed to be
             ;; a list. This is the part where the recipe repositories
             ;; are consulted, if necessary.
             (full-melpa-style-recipe
              (if recipe-specified-p
                  melpa-style-recipe
                (or (straight-recipes-retrieve
                     ;; Second argument is the sources list, defaults
                     ;; to all known sources.
                     melpa-style-recipe nil cause)
                    ;; Check if the package is considered as
                    ;; "built-in". If so, it's not an issue if we
                    ;; can't find it in any recipe repositories.
                    (if (straight--package-built-in-p melpa-style-recipe)
                        (cl-return-from straight--convert-recipe
                          `(:type built-in :package
                                  ,(symbol-name melpa-style-recipe)))
                      (error
                       (concat "Could not find package %S. "
                               "Updating recipe repositories: %S "
                               "with `straight-pull-recipe-repositories' "
                               "may fix this")
                       melpa-style-recipe
                       straight-recipe-repositories))))))
        ;; MELPA-style recipe format is a list whose car is the
        ;; package name as a symbol, and whose cdr is a plist.

        ;; Recipes retrieved from files may be backquoted.
        (when (straight--quoted-form-p full-melpa-style-recipe)
          (setq full-melpa-style-recipe (eval full-melpa-style-recipe)))
        (cl-destructuring-bind (package . plist) full-melpa-style-recipe
          ;; Recipes taken from recipe repositories would not normally
          ;; have `:local-repo' specified. But if the recipe was
          ;; specified manually, then you can specify `:local-repo' to
          ;; override the default value (which is determined according
          ;; to the selected VC backend).
          ;;
          (let* ((inherit (plist-member plist :inherit))
                 (inheritance (if inherit
                                  (cadr inherit)
                                straight-allow-recipe-inheritance)))
            (when inheritance
              ;; To keep overridden recipes simple, some keywords can be
              ;; inherited from the original recipe. This is done by
              ;; looking in original and finding all keywords that are
              ;; not present in the override and adding them there.
              (let* ((sources (plist-get plist :source))
                     (default
                       (or
                        (when-let ((retrieved (straight-recipes-retrieve
                                               package
                                               (if (listp sources)
                                                   sources
                                                 (list sources)))))
                          ;; Recipes retrieved from files may be backquoted.
                          (cdr (if (straight--quoted-form-p retrieved)
                                   (eval retrieved) retrieved)))
                        plist))
                     (type (or (plist-get default :type) 'git))
                     (keywords
                      (append straight--build-keywords
                              (unless (eq type 'built-in)
                                (straight-vc-keywords type)))))
                ;; Compute :fork repo name
                (when-let ((fork (plist-get plist :fork)))
                  (straight--put default :fork fork)
                  ;; Covers cases where :fork is a string or t
                  (unless (listp fork) (setq fork '()))
                  (straight--put fork :repo
                                 (straight-vc-git--fork-repo default))
                  (straight--put plist :fork fork))
                (dolist (keyword keywords)
                  (unless (plist-member plist keyword)
                    (when-let ((value (plist-get default keyword)))
                      (setq plist (plist-put plist keyword value))))))))
          ;; The normalized recipe format will have the package name
          ;; as a string, not a symbol.
          (let ((package (symbol-name package)))
            ;; Note that you can't override `:package'. That would
            ;; just be silly.
            (straight--put plist :package package)
            ;; If no `:type' is specified, use the default.
            (unless (plist-member plist :type)
              (straight--put plist :type straight-default-vc))
            ;; This `unless' allows overriding `:local-repo' in a
            ;; manual recipe specification, and also allows the
            ;; attribute to be set to nil to enforce that there is no
            ;; local repository (rather than a local repository name
            ;; being automatically generated).
            (unless (or (plist-member plist :local-repo)
                        (eq (plist-get plist :type) 'built-in))
              (straight--put
               plist :local-repo
               (or (straight-vc-local-repo-name plist)
                   ;; If no sane repository name can be generated,
                   ;; just use the package name.
                   package)))
            ;; This code is here to deal with complications that can
            ;; arise with manual recipe specifications when multiple
            ;; packages are versioned in the same repository.
            ;;
            ;; Specifically, let's suppose packages `swiper' and `ivy'
            ;; are both versioned in repository "swiper", and let's
            ;; suppose that I load both of them in my init-file (`ivy'
            ;; first and then `swiper'). Now suppose that I discover a
            ;; bug in `ivy' and fix it in my fork, so that (until my
            ;; fix is merged) I need to provide an explicit recipe in
            ;; my init-file's call to `straight-use-package' for
            ;; `ivy', in order to use my fork. That will cause a
            ;; conflict, because the recipe for `swiper' is
            ;; automatically taken from MELPA, and it does not point
            ;; at my fork, but instead at the official repository. To
            ;; fix the problem, I would have to specify my fork in the
            ;; recipe for `swiper' (and also `counsel', a third
            ;; package versioned in the same repository). That
            ;; violates DRY and is a pain.
            ;;
            ;; Instead, this code makes it so that if a recipe has
            ;; been automatically retrieved from a recipe repository
            ;; (for example, MELPA, GNU ELPA, or Emacsmirror), and the
            ;; `:local-repo' specified in that recipe has already been
            ;; used for another package, then the configuration for
            ;; that repository will silently be copied over, and
            ;; everything should "just work".
            ;;
            ;; Note that this weird edge case is totally unrelated to
            ;; the weird edge cases discussed earlier (in the first
            ;; comment of this function), and has to be handled in a
            ;; totally different way. It's surprising how complicated
            ;; recipe specification turns out to be.
            (unless recipe-specified-p
              (straight--with-plist plist
                  (local-repo)
                ;; Here we are checking to see if there is already a
                ;; formula with the same `:local-repo'. This is one of
                ;; the primary uses of `straight--repo-cache'.
                (when-let ((original-recipe (gethash local-repo
                                                     straight--repo-cache)))
                  ;; Remove all VC-specific attributes from the recipe
                  ;; we got from the recipe repositories.
                  (straight--remq
                   plist (cons :type
                               (straight-vc-keywords
                                ;; To determine which keywords to
                                ;; remove from `plist', we want to use
                                ;; the VC backend specified for that
                                ;; same recipe. This is important in
                                ;; case the recipe repository and the
                                ;; existing recipe specify different
                                ;; values for `:type'.
                                (plist-get plist :type))))
                  ;; Now copy over all the VC-specific attributes from
                  ;; the existing recipe.
                  (dolist (keyword
                           (cons :type
                                 (straight-vc-keywords
                                  ;; Same logic as above. This time
                                  ;; we're using the VC backend
                                  ;; specified by the original recipe.
                                  (plist-get original-recipe :type))))
                    (when-let ((value (plist-get original-recipe keyword)))
                      (straight--put plist keyword value))))))
            ;; Return the newly normalized recipe.
            plist)))))

(defun straight--get-overridden-recipe (package)
  "Given a PACKAGE symbol, check if it has an overridden recipe.
This means an entry in `straight-recipe-overrides'. If one is
found, return it as a MELPA-style recipe. Otherwise, return
nil."
  (let ((recipe nil))
    (cl-dolist (profile (mapcar #'car straight-profiles))
      (when-let ((recipes (alist-get profile straight-recipe-overrides)))
        (when-let ((overridden-recipe (assoc package recipes)))
          (setq recipe overridden-recipe))))
    recipe))

;;;;; Recipe registration

(defun straight--register-recipe (recipe)
  "Make the various caches aware of RECIPE.
RECIPE should be a straight.el-style recipe plist."
  (straight--with-plist recipe (package local-repo type)
    ;; Skip conflict detection for built-in packages.
    (unless (eq type 'built-in)
      ;; Step 1 is to check if the given recipe conflicts with an
      ;; existing recipe for a *different* package with the *same*
      ;; repository.
      (when-let ((existing-recipe (gethash local-repo straight--repo-cache)))
        ;; Avoid signalling two warnings when you change the recipe
        ;; for a single package. We already get a warning down below
        ;; in Step 2, no need to show another one here. Only signal a
        ;; warning here when the packages are actually *different*
        ;; packages that share the same repository.
        (unless (equal package (plist-get existing-recipe :package))
          ;; Only the VC-specific keywords are relevant for this.
          (cl-dolist (keyword (cons :type (straight-vc-keywords type)))
            ;; Note that it doesn't matter which recipe we get `:type'
            ;; from. If the two are different, then the first
            ;; iteration of this loop will terminate with a warning,
            ;; as desired.
            (unless (equal (plist-get recipe keyword)
                           (plist-get existing-recipe keyword))
              ;; We're using a warning rather than an error here,
              ;; because it's very frustrating if your package manager
              ;; simply refuses to install a package for no good
              ;; reason. Note that since we update
              ;; `straight--repo-cache' and `straight--recipe-cache'
              ;; at the end of this method, this warning will only be
              ;; displayed once per recipe modification.
              (straight--warn
               (concat
                "Packages %S and %S have incompatible "
                "recipes (%S cannot be both %S and %S)"
                (when (eq keyword :repo)
                  "\n(One of the recipes must specify a unique :local-repo)"))
               (plist-get existing-recipe :package)
               package
               keyword
               (plist-get existing-recipe keyword)
               (plist-get recipe keyword))
              (cl-return)))))
      ;; Step 2 is to check if the given recipe conflicts with an
      ;; existing recipe for the *same* package.
      (when-let ((existing-recipe (gethash package straight--recipe-cache)))
        (cl-dolist (keyword
                    (cons :type
                          (append straight--build-keywords
                                  ;; As in Step 1, it doesn't matter which
                                  ;; recipe we get `:type' from.
                                  (straight-vc-keywords type))))
          (unless (equal (plist-get recipe keyword)
                         (plist-get existing-recipe keyword))
            ;; Same reasoning as with the previous warning.
            (straight--warn
             (concat "Two different recipes given for %S "
                     "(%S cannot be both %S and %S)")
             package
             keyword
             (plist-get existing-recipe keyword)
             (plist-get recipe keyword))
            (cl-return)))))
    ;; Step 3, now that we've signaled any necessary warnings, is to
    ;; actually update the caches. Just FYI, `straight--build-cache'
    ;; is updated later (namely, at build time -- which may be quite a
    ;; while later, or never, depending on the values of NO-CLONE and
    ;; NO-BUILD that were passed to `straight-use-package'.
    (puthash package recipe straight--recipe-cache)
    ;; register recipes covered by :includes
    (when-let ((includes (plist-get recipe :includes)))
      (dolist (included (mapcar #'symbol-name
                                (if (listp includes)
                                    includes (list includes))))
        (puthash included
                 (list :package included
                       :type type
                       :build nil
                       :local-repo nil
                       :included-by package)
                 straight--recipe-cache)))
    ;; Don't record recipes which have no local repositories.
    (when local-repo
      (puthash local-repo recipe straight--repo-cache))
    (cl-pushnew straight-current-profile
                (gethash package straight--profile-cache)
                ;; Profiles are symbols and can be compared more
                ;; efficiently using `eq'.
                :test #'eq)
    ;; If we've registered a new package, then we no longer know that
    ;; the set of registered packages actually corresponds to the
    ;; packages requested in the init-file. (For instance, this could
    ;; be an interactive call.) But we're OK if this operation is
    ;; guaranteed to be functional (e.g. because we're currently
    ;; loading the init-file).
    (unless straight--functional-p
      (setq straight--profile-cache-valid nil))))

(defun straight--map-repos (func)
  "Call FUNC for each local repository referenced in the known recipes.
FUNC is passed one argument, the straight.el-style recipe plist.
It is called once for every local repository (i.e. each distinct
value of `:local-repo'). This means that if multiple packages are
versioned in the same local repository, then all but one of them
will be omitted."
  ;; Remember that `straight--repo-cache' only has the most recent
  ;; recipe that specified each `:local-repo'.
  (dolist (recipe (hash-table-values straight--repo-cache))
    (funcall func recipe)))

(defun straight--map-repo-packages (func)
  "Call FUNC for each local repository referenced in the known recipes.
The function FUNC is passed one argument, the name (as a string)
of one of the packages using the local repository."
  (straight--map-repos
   (lambda (recipe)
     (straight--with-plist recipe
         (package)
       (funcall func package)))))

;;;; Checking for package modifications

(defun straight--determine-best-modification-checking ()
  "Determine the best default value of `straight-check-for-modifications'.
This uses find(1) for all checking on most platforms, and
`before-save-hook' on Microsoft Windows."
  (if (straight--windows-os-p)
      (list 'check-on-save)
    (list 'find-at-startup 'find-when-checking)))

(defcustom straight-check-for-modifications
  (straight--determine-best-modification-checking)
  "When to check for package modifications.
This is a list of symbols. If `find-at-startup' is in the list,
then find(1) is used to detect modifications of all packages
before they are made available. If `find-when-checking' is in the
list, then find(1) is used to detect modifications in
\\[straight-check-package] and \\[straight-check-all]. If
`check-on-save' is in the list, then `before-save-hook' is used
to detect modifications of packages that you perform within
Emacs. If `watch-files' is in the list, then a filesystem watcher
is automatically started by straight.el to detect modifications.

Note that the functionality of `check-on-save' and `watch-files'
only covers modifications made within ~/.emacs.d/straight/repos,
so if you wish to use these features you should move all of your
local repositories into that directory.

PERFORMANCE IMPLICATIONS: `at-startup' means straight.el will run
a command during startup, which can be fairly slow, especially if
you do not have an SSD. Disable this to improve startup time.
However, you will still want to have package modifications
detected. Therefore add either `check-on-save', which has no
overhead but also does not catch modifications made outside of
Emacs, or `watch-files', which is more robust but has an external
dependency (watchexec) and takes up memory / file descriptors.

For backwards compatibility, the value of this variable may also
be a symbol, which is translated into a corresponding list as
follows:

`at-startup' => `(find-at-startup find-when-checking)'
`live' => `(check-on-save)'
`live-with-find' => `(check-on-save find-when-checking)'
`never' => nil

This usage is deprecated and will be removed."
  :type
  '(repeat
    (choice
     (const :tag "Use find(1) at startup" find-at-startup)
     (const :tag "Use find(1) in \\[straight-check-package]"
            find-when-checking)
     (const :tag "Use `before-save-hook' to detect changes" check-on-save)
     (const :tag "Use a filesystem watcher to detect changes" watch-files))))

(defun straight--modifications (symbol)
  "Check if `straight-check-for-modifications' contains SYMBOL.
However, if `straight-check-for-modifications' is itself one of
the symbols supported for backwards compatibility, account for
that appropriately."
  (memq symbol
        (pcase straight-check-for-modifications
          ('at-startup '(find-at-startup find-when-checking))
          ('live '(check-on-save))
          ('live-with-find '(check-on-save find-when-checking))
          ('never nil)
          (lst lst))))

(defcustom straight-cache-autoloads t
  "Non-nil means read autoloads in bulk to speed up startup.
The operation of this variable should be transparent to the user;
no changes in configuration are necessary."
  :type 'boolean)

;;;;; Build cache

(defvar straight--build-cache nil
  "Hash table keeping track of information about built packages, or nil.
The keys are strings naming packages, and the values are lists of
length three. The first entry is a timestamp identifying the last
time the package was successfully built; the second entry is a
list of the dependencies of the package, as strings; and the
third entry is the straight.el-normalized recipe plist for the
package. This information is used to determine whether or not a
package needs to be rebuilt.

The value of this variable is persisted in file pointed to in
`straight-build-cache-file'.")

(defvar straight--autoloads-cache nil
  "Hash table keeping track of autoloads extracted from packages, or nil.
The keys are strings naming packages, and the values are cons
cells. The car of each is a list of features that seem to be
provided by the package, and the cdr is the autoloads provided by
the package, as a list of forms to evaluate.")

(defvar straight--eagerly-checked-packages nil
  "List of packages that will be checked eagerly for modifications.
This list is read from the build cache, and is originally
generated at the end of an init from the keys of
`straight--profile-cache'.")

;; See http://stormlightarchive.wikia.com/wiki/Calendar for the
;; schema. After that we will switch to animal names starting with
;; sequential letters of the English alphabet.
(defvar straight--build-cache-version :tanat
  "The current version of the build cache format.
When the format on disk changes, this value is changed, so that
straight.el knows to regenerate the whole cache.")

(defvar straight--build-cache-text nil
  "Literal text of the build cache.
If this is unchanged between loading and saving the build cache,
then the saving step is skipped for efficiency.")

(defun straight--load-build-cache ()
  "Load data from the build cache into memory.
This sets the variables `straight--build-cache' and
`straight--eagerly-checked-packages'. If the build cache is
malformed, don't signal an error, but set these variables to
empty values (all packages will be rebuilt, with no caching)."
  ;; Start by clearing the build cache. If the one on disk is
  ;; malformed (or outdated), these values will be used.
  (setq straight--build-cache (make-hash-table :test #'equal))
  (setq straight--autoloads-cache (make-hash-table :test #'equal))
  (setq straight--recipe-lookup-cache (make-hash-table :test #'eq))
  (setq straight--eagerly-checked-packages nil)
  (setq straight--build-cache-text nil)
  (let ((needs-immediate-save nil))
    (ignore-errors
      (with-temp-buffer
        ;; Can't use `insert-file-contents-literally', see
        ;; https://github.com/raxod502/straight.el/issues/780.
        (insert-file-contents
         (straight--build-cache-file))
        (let ((version (read (current-buffer)))
              (last-emacs-version (read (current-buffer)))
              (build-cache (read (current-buffer)))
              (autoloads-cache (read (current-buffer)))
              (recipe-lookup-cache (read (current-buffer)))
              (eager-packages (read (current-buffer)))
              (use-symlinks (read (current-buffer)))
              ;; This gets set to nil if we detect a specific problem
              ;; with the build cache other than it being malformed,
              ;; so that we don't subsequently emit a second message
              ;; claiming that the cache is malformed.
              (malformed t))
          (unless (and
                   ;; Format version should be the symbol currently in
                   ;; use.
                   (symbolp version)
                   (or (eq version straight--build-cache-version)
                       (prog1 (setq malformed nil)
                         (straight--output
                          (concat
                           "Rebuilding all packages due to "
                           "build cache schema change"))))
                   ;; Emacs version should be the same as our current
                   ;; one.
                   (stringp last-emacs-version)
                   (or (string= last-emacs-version emacs-version)
                       (prog1 (setq malformed nil)
                         (straight--output
                          (concat
                           "Rebuilding all packages due to "
                           "change in Emacs version"))))
                   ;; Build cache should be a hash table.
                   (hash-table-p build-cache)
                   (eq (hash-table-test build-cache) #'equal)
                   ;; Autoloads cache should also be a hash table.
                   (hash-table-p autoloads-cache)
                   (eq (hash-table-test autoloads-cache) #'equal)
                   ;; Eagerly checked packages should be a list of
                   ;; strings.
                   (listp eager-packages)
                   (cl-every #'stringp eager-packages)
                   ;; Symlink setting should not have changed.
                   (or (eq use-symlinks straight-use-symlinks)
                       (prog1 (setq malformed nil)
                         (straight--output
                          (concat
                           "Rebuilding all packages due to "
                           "change in `straight-use-symlinks'")))))
            ;; If anything is wrong, abort and use the default values.
            (when malformed
              (straight--output
               "Rebuilding all packages due to malformed build cache"))
            (setq needs-immediate-save t)
            (error "Malformed or outdated build cache"))
          ;; Otherwise, we can load from disk.
          (setq straight--build-cache build-cache)
          (setq straight--autoloads-cache autoloads-cache)
          (setq straight--recipe-lookup-cache recipe-lookup-cache)
          (setq straight--eagerly-checked-packages eager-packages)
          (setq straight--build-cache-text (buffer-string))
          (when (or (straight--modifications 'check-on-save)
                    (straight--modifications 'watch-files))
            (when-let ((repos (condition-case _ (straight--directory-files
                                                 (straight--modified-dir))
                                (file-missing))))
              ;; Cause live-modified repos to have their packages
              ;; rebuilt when appropriate. Just in case init is
              ;; interrupted, however, we won't clear out the
              ;; `straight--modified-dir' until we write the build cache
              ;; back to disk.
              (dolist (package (hash-table-keys straight--build-cache))
                (ignore-errors
                  (when (member
                         (plist-get (nth 2 (gethash
                                            package straight--build-cache))
                                    :local-repo)
                         repos)
                    (remhash package straight--build-cache)))))))))
    ;; If we cleared out the build cache entirely due to a change in
    ;; Emacs version or similar, then we will be rebuilding all
    ;; packages during this init. However, the build cache will not be
    ;; written to disk until the end of init, so we can't rely on that
    ;; happening. If we rebuild a couple of packages and then init is
    ;; aborted before we can save the build cache, then it's possible
    ;; that the Emacs version (or similar) will have changed back to
    ;; its previous value. In this case, we should rebuild the
    ;; packages that we rebuilt during this init. However, since we
    ;; didn't write the build cache, the need for this can't be
    ;; detected. To solve the problem, we write the build cache
    ;; immediately in the case of needing to rebuild all packages.
    (when needs-immediate-save
      (straight--save-build-cache))))

(defun straight--save-build-cache ()
  "Write data from memory into the build cache file.
This uses the values of `straight--build-cache' and
`straight--eagerly-checked-packages'.

The name of the cache file is stored in
`straight-build-cache-file'."
  (unless straight-safe-mode
    (with-temp-buffer
      ;; Prevent mangling of the form being printed in the case that
      ;; this function was called by an `eval-expression' invocation
      ;; of `straight-use-package'.
      (let ((print-level nil)
            (print-length nil))
        ;; The version of the build cache.
        (print straight--build-cache-version (current-buffer))
        ;; Record the current Emacs version. If a different version of
        ;; Emacs is used, we have to rebuild all the packages (because
        ;; byte-compiled files cannot necessarily still be loaded).
        (print emacs-version (current-buffer))
        ;; The actual build cache.
        (print straight--build-cache (current-buffer))
        ;; The autoloads cache.
        (print straight--autoloads-cache (current-buffer))
        ;; The recipe lookup cache.
        (print straight--recipe-lookup-cache (current-buffer))
        ;; Which packages should be checked eagerly next init.
        (print (hash-table-keys straight--profile-cache) (current-buffer))
        ;; Whether packages were built using symlinks or copying.
        (print straight-use-symlinks (current-buffer)))
      (unless (and straight--build-cache-text
                   (string= (buffer-string) straight--build-cache-text))
        (write-region nil nil (straight--build-cache-file) nil 0))
      (when (or (straight--modifications 'check-on-save)
                (straight--modifications 'watch-files))
        ;; We've imported data from `straight--modified-dir' into the
        ;; build cache when loading it. Now that we've written the
        ;; build cache back to disk, there's no more need for that
        ;; data (and indeed, it would produce spurious package
        ;; rebuilds on subsequent inits).
        (condition-case _
            (delete-directory (straight--modified-dir) 'recursive)
          (file-error))))))

(cl-defun straight--make-build-cache-available (&key nosave)
  "Make the build cache available until the end of the current transaction.
If NOSAVE is non-nil, then don't bother saving it. (In this case,
you ought not to make any changes to it.)"
  (straight--transaction-exec
   'build-cache
   :now #'straight--load-build-cache
   :later (unless nosave #'straight--save-build-cache)))

;;;;; Live modification checking

(defun straight-register-repo-modification (local-repo)
  "Register a modification of the given LOCAL-REPO, a string.
Always return nil, for convenience of usage."
  (unless straight-safe-mode
    (prog1 nil
      (unless (string-match-p "/" local-repo)
        (make-directory (straight--modified-dir) 'parents)
        (with-temp-file (straight--modified-file local-repo))))))

(defun straight-register-file-modification ()
  "Register a modification of the current file.
This function is placed on `before-save-hook' by
`straight-live-modifications-mode'."
  (when buffer-file-name
    (when-let ((local-repo (straight--determine-repo buffer-file-name)))
      (straight-register-repo-modification local-repo))))

(define-minor-mode straight-live-modifications-mode
  "Mode that causes straight.el to check for modifications as you make them.
This mode is automatically enabled or disabled as you bootstrap
straight.el, according to the value of
`straight-check-for-modifications'."
  :global t
  :group 'straight
  (if straight-live-modifications-mode
      (add-hook 'before-save-hook #'straight-register-file-modification)
    (remove-hook 'before-save-hook #'straight-register-file-modification)))

;;;;; Filesystem watcher

(defcustom straight-watcher-process-buffer " *straight-watcher*"
  "Name of buffer to use for the filesystem watcher."
  :type 'string)

(defun straight-watcher--make-process-buffer ()
  "Kill and recreate `straight-watcher-process-buffer'. Return it."
  (ignore-errors
    (kill-buffer straight-watcher-process-buffer))
  (let ((buf (get-buffer-create straight-watcher-process-buffer)))
    (prog1 buf
      (with-current-buffer buf
        (special-mode)))))

(cl-defun straight-watcher--virtualenv-setup ()
  "Set up the virtualenv for the filesystem watcher.
If it fails, signal a warning and return nil."
  (let* ((virtualenv (straight--watcher-dir "virtualenv"))
         (python (straight--watcher-python))
         (straight-dir (file-name-directory straight--this-file))
         (watcher-dir (expand-file-name "watcher" straight-dir))
         (version-from (expand-file-name "version" watcher-dir))
         (version-to (straight--watcher-file "version")))
    (condition-case _
        (delete-directory virtualenv 'recursive)
      (file-missing))
    (make-directory
     (file-name-directory
      (directory-file-name virtualenv))
     'parents)
    (and (straight--process-run "python3" "-m" "venv" virtualenv)
         (straight--process-run python "-m" "pip" "install" "-e" watcher-dir)
         (prog1 t (copy-file version-from version-to
                             'ok-if-already-exists)))))

(defun straight-watcher--virtualenv-outdated ()
  "Return non-nil if the watcher virtualenv needs to be set up again.
This includes the case hwere it doesn't yet exist."
  (let* ((straight-dir (file-name-directory straight--this-file))
         (watcher-dir (expand-file-name "watcher" straight-dir))
         (version-from (expand-file-name "version" watcher-dir))
         (version-to (straight--watcher-file "version")))
    (not (straight--process-run-p "diff" "-q" version-from version-to))))

(cl-defun straight-watcher-start ()
  "Start the filesystem watcher, killing any previous instance.
If it fails, signal a warning and return nil."
  (interactive)
  (unless straight-safe-mode
    (unless (executable-find "python3")
      (straight--warn
       "Cannot start filesystem watcher without 'python3' installed")
      (cl-return-from straight-watcher-start))
    (unless (executable-find "watchexec")
      (straight--warn
       "Cannot start filesystem watcher without 'watchexec' installed")
      (cl-return-from straight-watcher-start))
    (when (straight-watcher--virtualenv-outdated)
      (straight--output "Setting up filesystem watcher...")
      (unless (straight-watcher--virtualenv-setup)
        (straight--output "Setting up filesystem watcher...failed")
        (cl-return-from straight-watcher-start))
      (straight--output "Setting up filesystem watcher...done"))
    (with-current-buffer (straight-watcher--make-process-buffer)
      (let* ((python (straight--watcher-python))
             (cmd (list
                   ;; Need to disable buffering, otherwise we don't
                   ;; get some important stuff printed.
                   python "-u" "-m" "straight_watch" "start"
                   (straight--watcher-file "process")
                   (straight--repos-dir)
                   (straight--modified-dir)))
             (sh (concat
                  "exec nohup "
                  (mapconcat #'shell-quote-argument cmd " "))))
        ;; Put the 'nohup.out' file in the ~/.emacs.d/straight/watcher/
        ;; directory.
        (setq default-directory (straight--watcher-dir))
        ;; Clear it out, since nohup(1) doesn't overwrite it.
        (condition-case _
            (delete-file (straight--watcher-file "nohup.out"))
          (file-missing))
        (let ((inhibit-read-only t))
          (insert "$ " sh "\n\n"))
        (start-file-process-shell-command
         "straight-watcher" straight-watcher-process-buffer sh)
        (set-process-query-on-exit-flag
         (get-buffer-process (current-buffer)) nil)))))

(defun straight-watcher-stop ()
  "Kill the filesystem watcher, if it is running.
If there is an unexpected error, signal a warning and return nil."
  (interactive)
  (unless straight-safe-mode
    (let ((python (straight--watcher-python)))
      (when (file-executable-p python)
        (straight--process-run python "-m" "straight_watch" "stop"
                               (straight--watcher-file "process"))))))

;;;;; Bulk checking

(defvar straight--cached-package-modifications (make-hash-table :test #'equal)
  "Hash table indicating the modification status of cached packages.
Valid for the duration of a single transaction. It is a hash
table whose keys are local repository names as strings and whose
values are booleans indicating whether the repositories have been
modified since their last builds.")

(cl-defun straight--cache-package-modifications ()
  "Compute `straight--cached-package-modifications'."
  (let (;; Keep track of which local repositories we've processed
        ;; already. This table maps repo names to booleans.
        (repos (make-hash-table :test #'equal))
        ;; The systematically generated arguments for find(1).
        (args-paths nil)
        (args-primaries nil)
        (args nil)
        ;; This list is used to make sure we don't try to search a
        ;; directory that doesn't exist, which would cause the find(1)
        ;; command to fail.
        (existing-repos (straight--directory-files (straight--repos-dir))))
    (dolist (package straight--eagerly-checked-packages)
      (when-let ((build-info (gethash package straight--build-cache)))
        ;; Don't use `cl-destructuring-bind', as that will
        ;; error out on a list of insufficient length. We
        ;; want to be robust in the face of a malformed build
        ;; cache.
        (let ((mtime (nth 0 build-info))
              (recipe (nth 2 build-info)))
          (straight--with-plist recipe
              (local-repo)
            (when (and local-repo
                       (not (gethash local-repo repos))
                       (member local-repo existing-repos))
              (if mtime
                  ;; The basic idea of the find(1) command here is
                  ;; that we search all the local repositories, and
                  ;; then the actual primaries evaluated are a
                  ;; disjunction that first prevents any .git
                  ;; directories from being traversed and then checks
                  ;; for any files that are in a given local
                  ;; repository *and* have a new enough mtime.
                  ;;
                  ;; See the following issue for an explanation about
                  ;; why an extra pair of single quotes is used on
                  ;; Windows:
                  ;; <https://github.com/raxod502/straight.el/issues/393>
                  (let ((newer-or-newermt nil)
                        (mtime-or-file nil))
                    (if (straight--find-supports 'newermt)
                        (progn
                          (setq newer-or-newermt "-newermt")
                          (setq mtime-or-file mtime))
                      (setq newer-or-newermt "-newer")
                      (setq mtime-or-file (straight--make-mtime mtime)))
                    (push (straight--repos-dir local-repo) args-paths)
                    (setq args-primaries
                          (append (list "-o"
                                        "-path"
                                        (expand-file-name
                                         (if (eq system-type 'windows-nt)
                                             "'*'"
                                           "*")
                                         (straight--repos-dir local-repo))
                                        newer-or-newermt
                                        mtime-or-file
                                        "-print")
                                  args-primaries)))
                ;; If no mtime is specified, it means the package
                ;; definitely needs to be (re)built. Probably there
                ;; was an error and we couldn't finish building the
                ;; package, but we wrote the build cache anyway.
                (puthash
                 local-repo t straight--cached-package-modifications))
              ;; Don't create duplicate entries in the find(1) command
              ;; for this local repository.
              (puthash local-repo t repos))))))
    ;; If no packages, abort. This shouldn't happen, but might in the
    ;; face of other errors/undefined behavior.
    (unless args-paths
      (cl-return-from straight--cache-package-modifications))
    ;; Construct the final find(1) command.
    (setq args (append
                args-paths
                (list "-name" ".git" "-prune")
                args-primaries))
    (let* ((default-directory (straight--repos-dir))
           (results (apply #'straight--process-output
                           straight-find-executable args)))
      (maphash (lambda (local-repo _)
                 (puthash
                  local-repo (string-match-p
                              (concat "^"
                                      (regexp-quote
                                       (file-name-as-directory
                                        (straight--repos-dir local-repo))))
                              results)
                  straight--cached-package-modifications))
               repos))))

(defun straight--uncache-package-modifications ()
  "Reset `straight--cached-package-modifications'."
  (setq straight--cached-package-modifications
        (make-hash-table :test #'equal)))

(defun straight--make-package-modifications-available ()
  "Make `straight--cached-package-modifications' available.
It is only available until the end of the current transaction."
  (straight--transaction-exec
   'bulk-find
   :now #'straight--cache-package-modifications
   :later #'straight--uncache-package-modifications))

;;;;; Individual checking

(defvar straight--allow-find nil
  "Bound to non-nil if find(1) can be used.
The value of this variable is only relevant when
`straight-check-for-modifications' contains `find-when-checking'.")

(cl-defun straight--package-might-be-modified-p (recipe no-build)
  "Check whether the package for the given RECIPE should be rebuilt.
If NO-BUILD is non-nil, then don't assume that the package should
have a build directory; only check for modifications since the
last time."
  (straight--with-plist recipe
      (package local-repo)
    (let* (;; `build-info' is a list of length three containing the
           ;; timestamp of the last build, the list of dependencies,
           ;; and the recipe plist, in that order.
           (build-info (gethash package straight--build-cache))
           (last-mtime (nth 0 build-info))
           (last-recipe (nth 2 build-info)))
      (or (null build-info)
          ;; Rebuild if relevant parts of the recipe have changed.
          (cl-dolist (keyword straight--build-keywords nil)
            (unless (equal (plist-get recipe keyword)
                           (plist-get last-recipe keyword))
              (cl-return t)))
          ;; Somebody deleted the build directory...
          (and (not no-build)
               (not (file-exists-p (straight--build-dir package))))
          (progn
            ;; No local repository means we certainly can't have
            ;; changes to the package on disk. Since there's nothing
            ;; on disk, you know.
            (unless local-repo
              (cl-return-from straight--package-might-be-modified-p))
            ;; Don't look at mtimes unless we're told to. Otherwise,
            ;; rely on live modification checking/user attention.
            (unless (or (straight--modifications 'find-at-startup)
                        (and (straight--modifications 'find-when-checking)
                             straight--allow-find))
              (cl-return-from straight--package-might-be-modified-p))
            (straight--make-package-modifications-available)
            (if (straight--checkhash
                 local-repo straight--cached-package-modifications)
                ;; Use the cached modification status if we've computed
                ;; one.
                (gethash local-repo straight--cached-package-modifications)
              ;; `last-mtime' should always be a string but you never
              ;; know.
              (or (not (stringp last-mtime))
                  (with-temp-buffer
                    (let ((newer-or-newermt nil)
                          (mtime-or-file nil))
                      (if (straight--find-supports 'newermt)
                          (progn
                            (setq newer-or-newermt "-newermt")
                            (setq mtime-or-file last-mtime))
                        (setq newer-or-newermt "-newer")
                        (setq mtime-or-file
                              (straight--make-mtime last-mtime)))
                      (let* ((default-directory
                               (straight--repos-dir local-repo))
                             ;; This find(1) command ignores the .git
                             ;; directory, and prints the names of any
                             ;; files or directories with a newer
                             ;; mtime than the one specified.
                             (results (straight--process-output
                                       straight-find-executable
                                       "." "-name" ".git" "-prune"
                                       "-o" newer-or-newermt mtime-or-file
                                       "-print")))
                        ;; If anything was printed, the package has
                        ;; (maybe) been modified.
                        (not (string-empty-p results))))))))))))

;;;; Building packages

(defvar straight--build-functions nil
  "Abnormal hook run to build a package.
This is lexically bound per-recipe during `straight--build-package'.
Each function recieves the current recipe as its argument.")


(defconst straight--build-default-steps '(autoloads
                                          compile
                                          native-compile
                                          info)
  "List of symbols representing the build steps.")

(defun straight--build-steps (recipe)
  "Compute RECIPE's :build functions for use in `straight--build-functions'."
  (unless (straight--build-disabled-p recipe)
    (let* ((build (plist-member recipe :build))
           (val (cadr build))
           (defaults (if (or (eq (car-safe val) :not) (not build))
                         (cl-remove-if
                          (lambda (step)
                            (symbol-value
                             (intern (concat "straight-disable-"
                                             (symbol-name step)))))
                          straight--build-default-steps)
                       straight--build-default-steps)))
      (mapcar (lambda (step)
                (intern (concat "straight--build-" (symbol-name step))))
              (pcase val
                ((or 'nil 't) defaults)
                (`(:not . ,steps) (cl-set-difference defaults steps))
                ((pred listp) val)
                (_ defaults))))))

;;;;; Main entry point
(defun straight--build-package (recipe &optional cause)
  "Build the package specified by the RECIPE.
This includes running RECIPE's `:pre-build` commands, symlinking the
package files into the build directory, building dependencies, running
RECIPE's `:build` and `:post-build` commands, and updating the build
cache. It is assumed that the package repository has already been
cloned.

RECIPE is a straight.el-style plist. CAUSE is a string indicating
the reason this package is being built."
  (straight--with-plist recipe
      (package)
    (when straight-safe-mode
      (error "Building %s not allowed in safe mode" package))
    (let ((task (concat cause (when cause straight-arrow)
                        (format "Building %s" package))))
      (straight--with-progress task
        (straight--run-build-commands recipe)
        (straight--symlink-package recipe)
        ;; The following function call causes the dependency list to
        ;; be written to the build cache. There is no need to save it
        ;; right away, as the transaction system ensures that in order
        ;; for the build cache to be loaded again, the current
        ;; transaction would first have to end, including saving the
        ;; build cache. (We know we're inside a transaction because
        ;; otherwise the build cache would not be available at all,
        ;; and hence this code would break immediately.)
        (straight--compute-dependencies package)
        ;; Before we (possibly) build the dependencies, we need to set
        ;; this flag so that we know if our progress message will need
        ;; to be redisplayed afterwards (before autoload generation
        ;; and byte-compilation).
        (setq straight--echo-area-dirty nil)
        ;; Yes, we do the following logic twice. Once here and again
        ;; in `straight-use-package'. Why? We need to do it here
        ;; because the dependencies need to be available before this
        ;; package can be byte-compiled. But the normal case is that
        ;; packages are already going to be built, so this code path
        ;; will not be hit and therefore autoloads will not be loaded
        ;; for the dependencies in that situation if we don't do it
        ;; again in `straight-use-package'.
        (when-let ((dependencies (straight--get-dependencies package)))
          (dolist (dependency dependencies)
            ;; The implicit meaning of the first argument to
            ;; `straight-use-package' here is that the default recipes
            ;; (taken from one of the recipe repositories) are used
            ;; for dependencies. (Well, maybe. See all the weird edge
            ;; cases and exceptions in `straight--convert-recipe'.)
            ;;
            ;; Note that the second and third arguments are always
            ;; nil. This means that dependencies will always be
            ;; eagerly cloned and built, if we got to building this
            ;; package.
            (straight-use-package (intern dependency) nil nil task))
          ;; We might need to redisplay the progress message from
          ;; `straight--with-progress' up above.
          (when straight--echo-area-dirty
            (straight--progress-begin task)))
        (let ((straight--build-functions
               (straight--build-steps recipe)))
          (run-hook-with-args 'straight--build-functions recipe))
        (straight--run-build-commands recipe 'post)
        (run-hook-with-args
         'straight-use-package-post-build-functions package))
      ;; We messed up the echo area.
      (setq straight--echo-area-dirty t))))

;;;;; Files directive processing

(defvar straight-default-files-directive
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for the `:files' directive in recipes.
It is also spliced in at any point where the `:defaults' keyword
is used in a `:files' directive.")

(defun straight--expand-files-directive-internal (files src-dir prefix flavor)
  "Expand FILES directive in SRC-DIR with path PREFIX.
FILES is a list that can be used for the `:files' directive in a
recipe. SRC-DIR is an absolute path to the directory relative to
which wildcards are to be expanded. PREFIX is a string, either
empty or ending with a slash, that should be prepended to all
target paths. FLAVOR is either the symbol `melpa' or nil; see
`straight-expand-files-directive'.

The return value is a cons cell of a list of mappings and a list
of exclusions. The mappings are of the same form that is returned
by `straight-expand-files-directive', while the exclusions are
analogous except that they are only cars, and do not include
destinations."
  (unless (listp files)
    (error "Invalid :files directive: %S" files))
  (let ((mappings nil)
        (exclusions nil))
    ;; We have to do some funny business to get `:defaults' splicing
    ;; and wildcard expansion to work, hence `while' instead of
    ;; `dolist'.
    (while files
      ;; Pop off the first spec. We might add some new specs back in
      ;; later on.
      (let ((spec (car files)))
        (setq files (cdr files))
        (cond
         ((eq spec :defaults)
          (setq files (append straight-default-files-directive files)))
         ;; Replace string-only specs with a bunch of conses that have
         ;; already been wildcard-expanded.
         ((stringp spec)
          (setq files
                ;; Function `nconc' doesn't mutate its last argument.
                ;; We use it for efficiency over `append'.
                (nconc
                 (mapcar
                  (lambda (file)
                    ;; Here we are using `file-name-nondirectory' to
                    ;; achieve a default of linking to the root
                    ;; directory of the target, but possibly with a
                    ;; prefix if one was created by an enclosing list.
                    (let ((filename (file-name-nondirectory file)))
                      (when (eq flavor 'melpa)
                        (setq filename
                              (replace-regexp-in-string
                               "\\.el\\.in\\'" ".el" filename 'fixedcase)))
                      (cons file (concat prefix filename))))
                  (file-expand-wildcards spec))
                 files)))
         ;; The only other possibilities were already taken care of.
         ((not (consp spec))
          (error "Invalid entry in :files directive: %S" spec))
         ((eq (car spec) :exclude)
          (cl-destructuring-bind
              (rec-mappings . rec-exclusions)
              (straight--expand-files-directive-internal
               (cdr spec) src-dir prefix flavor)
            ;; We still want to make previously established mappings
            ;; subject to removal, but this time we're inverting the
            ;; meaning of the sub-list so that its mappings become our
            ;; exclusions.
            (setq mappings (cl-remove-if
                            (lambda (mapping)
                              (assoc (car mapping) rec-mappings))
                            mappings))
            ;; Same as above. Mappings become exclusions. We drop the
            ;; actual exclusions of the `:exclude' sub-list, since
            ;; they are only supposed to apply to which elements
            ;; actually get excluded (a double exclusion does not make
            ;; an inclusion, at least here).
            (dolist (mapping rec-mappings)
              (push (car mapping) exclusions))))
         ;; Check if this is a proper list, rather than just a cons
         ;; cell.
         ((consp (cdr spec))
          ;; If so, the car should be a path prefix. We don't accept
          ;; `defaults' here obviously.
          (unless (stringp (car spec))
            (error "Invalid sub-list head in :files directive: %S" (car spec)))
          (cl-destructuring-bind
              ;; "rec" stands for "recursive".
              (rec-mappings . rec-exclusions)
              (straight--expand-files-directive-internal
               (cdr spec) src-dir (concat prefix (car spec) "/") flavor)
            ;; Any previously established mappings are subject to
            ;; removal from the `:exclude' clauses inside the
            ;; sub-list, if any.
            (setq mappings (cl-remove-if
                            (lambda (mapping)
                              (member (car mapping) rec-exclusions))
                            mappings))
            ;; We have to do this after the `cl-remove-if' above,
            ;; since otherwise the mappings established within the
            ;; sub-list after the `:exclude' clauses there would also
            ;; be subject to removal.
            (dolist (mapping rec-mappings)
              ;; This is the place where mappings generated further
              ;; down are propagated all the way up to the top (unless
              ;; they get hit by a `cl-remove-if').
              (push mapping mappings))
            ;; The exclusions might also apply to some more mappings
            ;; that were established in higher-level sub-lists.
            (dolist (exclusion rec-exclusions)
              (push exclusion exclusions))))
         ((or (not (stringp (car spec)))
              (not (stringp (cdr spec))))
          (error "Invalid entry in :files directive: %S" spec))
         (t
          ;; Filter out nonexistent files silently. This only matters
          ;; when mappings are specified explicitly with cons cells,
          ;; since `file-expand-wildcards' will only report extant
          ;; files, even if there are no wildcards to expand.
          (when (file-exists-p (car spec))
            ;; This is the only place where mappings are actually
            ;; generated in the first place.
            (push spec mappings))))))
    ;; We've been using `push' to stick stuff onto the fronts of our
    ;; lists, so we need to reverse them. Not that it should matter
    ;; too much.
    (cons (reverse mappings) (reverse exclusions))))

(defun straight-expand-files-directive
    (files src-dir dest-dir &optional flavor)
  "Expand FILES directive mapping from SRC-DIR to DEST-DIR.
SRC-DIR and DEST-DIR are absolute paths; the intention is that
symlinks are created in DEST-DIR pointing to SRC-DIR (but this
function does not do that). Return a list of cons cells
representing the mappings from SRC-DIR to DEST-DIR. The paths in
the cons cells are absolute.

FILES is a list, or nil. Each element of FILES can be a string, a
cons cell, a list, or the symbol `:defaults'.

If an entry is a string, then it is expanded into a (possibly
empty) list of extant files in SRC-DIR using
`file-expand-wildcards'. Each of these files corresponds to a
link from the file in SRC-DIR to a file with the same name (sans
directory) in DEST-DIR.

If an entry is a cons cell, then it is taken as a literal mapping
from a file in SRC-DIR to a file in DEST-DIR (the directory is
not removed). In this case, wildcard expansion does not take
place.

If an entry is a list, then it must begin with either a string or
the symbol `:exclude'.

If the list begins with a string, then the remainder of the list
is expanded as a top-level FILES directive, except that all
target paths have the first element of the list prepended to
then. In other words, this form specifies further links to be
placed within a particular subdirectory of DEST-DIR.

If the list begins with the symbol `:exclude', then the remainder
of the list is expanded as a top-level FILES directive, except
that all previously defined links pointing to any files in the
resulting list are removed. Note that this means any links
specified previously in the current list are subject to removal,
and also any links specified previously at any higher-level list,
but not any links specified afterwards in the current list, or
any higher-level list. Note also that `:exclude' can be nested:
in this case the inner `:exclude' results in some files being
excluded from the outer `:exclude', meaning that they will not
actually be excluded.

If the entry is the symbol `:defaults', then the value of
`straight-default-files-directive' is spliced into the enclosing
list to replace `:defaults'.

If FILES is nil, it defaults to
`straight-default-files-directive'.

If two links are specified that take the same source path to
different target paths, the one that is specified textually later
in FILES will win.

Note that this specification is quite similar to the one used by
the MELPA recipe repository, with some minor differences:

* MELPA recipes do not support cons cells to rename files or
  specify explicit subdirectories

* MELPA recipes do not support putting `:defaults' anywhere
  except as the first element of the top-level list

* When using `:exclude' in a MELPA recipe, the current DEST-DIR
  prefix created by enclosing lists is not respected.

* Whenever a *.el.in file is linked in a MELPA recipe, the target
  of the link is named as *.el.

* When using `:exclude' in a MELPA recipe, only links defined in
  the current list are subject to removal, and not links defined
  in higher-level lists.

If FLAVOR is nil or omitted, then expansion takes place as
described above. If FLAVOR is the symbol `melpa', then *.el.in
files will be linked as *.el files as in MELPA. If FLAVOR is any
other value, the behavior is not specified."
  ;; We bind `default-directory' here so we don't have to do it
  ;; repeatedly in the recursive section.
  (let* ((default-directory src-dir)
         (result (straight--expand-files-directive-internal
                  (or files straight-default-files-directive)
                  src-dir "" flavor))
         ;; We can safely discard the exclusions in the cdr of
         ;; `result', since any mappings that should have been
         ;; subject to removal have already had the exclusions
         ;; applied to them.
         (mappings (car result)))
    (let ((copy nil)
          (absolutes (mapcar (lambda (mapping)
                               (cl-destructuring-bind (src . dest) mapping
                                 ;; Make the paths absolute.
                                 (cons (expand-file-name src src-dir)
                                       (expand-file-name dest dest-dir))))
                             mappings)))
      ;; If there are duplicate keys, only keep the last declared cell.
      (dolist (el (nreverse absolutes) copy)
        (cl-pushnew el copy :test (lambda (a b) (equal (car a) (car b))))))))

;;;;; Running Build Commands

(defun straight--run-build-commands (recipe &optional post)
  "Run RECIPE's :pre-build or :post-build commands synchronously.
If POST is non-nil, RECIPE's :post-build commands are run.
Otherwise, the :pre-build commands are run.
RECIPE is a straight.el-style plist.

Each command is either an elisp form to be evaluated or a list of
strings to be executed in a shell context of the form:

  (\"executable\" \"arg\"...)

Commands are exectued in the repository directory.

The keyword's value is expected to be one of the following:

  - A single command
  - A list of commands
  - nil, in which case no commands are executed.
    Note if :build is nil, :pre/post-build commands are not executed."
  (straight--with-plist recipe (pre-build post-build package local-repo)
    (when-let ((commands (if post post-build pre-build))
               (repo (straight--repos-dir (or local-repo package))))
      (let ((default-directory repo)
            (commanderror nil))
        ;; Allow a single command or a list of commands.
        (dolist (command (if (cl-every #'listp commands)
                             commands
                           (list commands)))
          (condition-case err
              (if (cl-every #'stringp command)
                  (unless (apply #'straight--process-run-p command)
                    (setq inhibit-startup-screen t
                          commanderror t)
                    (switch-to-buffer-other-window straight-process-buffer)
                    (goto-char (point-max))
                    (error "%S" command))
                (eval command))
            ((error) (error (concat
                             (if post ":post" ":pre")
                             "-build command error in %S recipe"
                             (when commanderror " in command")
                             " %S")
                            package
                            (if commanderror (cadr err) err)))))))))

;;;;; Symlinking

(defun straight--symlink-package (recipe)
  "Symlink the package for the given RECIPE into the build directory.
This deletes any existing files in the relevant subdirectory of
the build directory, creating a pristine set of symlinks."
  (straight--with-plist recipe
      (package local-repo files flavor)
    ;; Remove the existing built package, if necessary.
    (let ((dir (straight--build-dir package)))
      (when (file-exists-p dir)
        (delete-directory dir 'recursive)))
    ;; Make a new directory for the built package.
    (make-directory (straight--build-dir package) 'parents)
    ;; Do the linking.
    (dolist (spec (straight-expand-files-directive
                   files
                   (straight--repos-dir local-repo)
                   (straight--build-dir package)
                   flavor))
      (cl-destructuring-bind (repo-file . build-file) spec
        (make-directory (file-name-directory build-file) 'parents)
        (straight--symlink-recursively repo-file build-file)))))

(defvar straight-symlink-emulation-mode)

(defun straight-chase-emulated-symlink (filename)
  "Check if FILENAME is an emulated symlink.
Return nil if it's not. Return the link target if it is. Return
`broken' if it seems like it should be, but it can't be resolved
to an existing file. See `straight-symlink-emulation-mode'."
  (when straight-symlink-emulation-mode
    (let ((build-dir (straight--build-dir)))
      (when (straight--path-prefix-p build-dir filename)
        ;; Remove the ~/.emacs.d/straight/build/ part, and get the
        ;; corresponding path under straight/links/.
        (let* ((relative-path (substring filename (length build-dir)))
               (link-record (straight--links-file relative-path)))
          (if (file-exists-p link-record)
              (let ((target
                     (with-temp-buffer
                       (insert-file-contents-literally link-record)
                       (buffer-string))))
                (if (or (string-empty-p target)
                        (not (file-exists-p target)))
                    'broken
                  target))
            'broken))))))

(defun straight-maybe-emulate-symlink ()
  "If visiting an emulated symlink, visit the link target instead.
See `straight-symlink-emulation-mode'."
  (when buffer-file-name
    (pcase (straight-chase-emulated-symlink buffer-file-name)
      ('nil)
      ('broken
       (straight--output
        "Broken symlink, you are not editing the real file"))
      (target (find-alternate-file target)))))

(define-minor-mode straight-symlink-emulation-mode
  "Minor mode for emulating symlinks in the software layer.
This means when a file in straight/build/ is visited, an advice
on `find-file-hook' causes straight.el to check if there is a
record in straight/links/ identifying it as a symlink. If so,
then the file referenced there is used instead. This mode is
automatically enabled or disabled when you load straight.el,
according to the value of `straight-use-symlinks'."
  :global t
  :group 'straight
  (if straight-symlink-emulation-mode
      (add-hook 'find-file-hook #'straight-maybe-emulate-symlink)
    (remove-hook 'find-file-hook #'straight-maybe-emulate-symlink)))

;;;;; Dependency management

(defun straight--process-dependencies (dependencies)
  "Normalize a package.el-style list of DEPENDENCIES.
Each dependency is a list of length two containing a symbol
naming a package and a string naming the minimum version
required (see the Package-Requires header in a
package.el-compliant Elisp package). The return value is a list
of strings naming the packages that are mentioned in the
dependency list."
  (mapcar
   (lambda (dep)
     (symbol-name
      (if (listp dep)
          (car dep)
        dep)))
   dependencies))

(defun straight--compute-dependencies (package)
  "Register the dependencies of PACKAGE in `straight--build-cache'.
PACKAGE should be a string naming a package. Note that this
function does *not* return the dependency list; see
`straight--get-dependencies' for that. (The reason these two
functions are separate is because dependencies are computed at
package build time, but they are retrieved later (when we are
activating autoloads, and may not have even built the package on
this run of straight.el)."
  (let ((dependencies
         ;; There are actually two ways of specifying a package in
         ;; Emacs. The first is to include a file called
         ;; <PACKAGE-NAME>-pkg.el which contains a data structure with
         ;; a bunch of information (including the dependency alist).
         ;; The second is to put the information as headers in the
         ;; preamble of the file <PACKAGE-NAME>.el. We account for
         ;; both of them here.
         (or (ignore-errors
               (with-temp-buffer
                 ;; Bypass `find-file-hook'.
                 (insert-file-contents-literally
                  (straight--build-file
                   package
                   (format "%s-pkg.el" package)))
                 (straight--process-dependencies
                  (eval (nth 4 (read (current-buffer)))))))
             (ignore-errors
               (with-temp-buffer
                 (insert-file-contents-literally
                  (straight--build-file
                   package
                   (format "%s.el" package)))
                 ;; Who cares if the rest of the header is
                 ;; well-formed? Maybe package.el does, but all we
                 ;; really need is the dependency alist. If it's
                 ;; missing or malformed, we just assume the package
                 ;; has no dependencies.
                 (let ((case-fold-search t))
                   (re-search-forward "^;* *Package-Requires *: *"))
                 (when (looking-at "(")
                   (straight--process-dependencies
                    (read (current-buffer)))))))))
    (straight--insert 1 package dependencies straight--build-cache)))

(defun straight--get-dependencies (package)
  "Get the dependencies of PACKAGE from `straight--build-cache'.
PACKAGE should be a string naming a package. This assumes that
they were previously registered in the build cache by
`straight--compute-dependencies'."
  (nth 1 (gethash package straight--build-cache)))

;;;;; Autoload generation

(defcustom straight-disable-autoloads nil
  "Non-nil means do not generate or activate autoloads by default.
This can be overridden by the `:build' property of an individual
package recipe."
  :type 'boolean)

(defun straight--build-autoloads (recipe)
  "Generate autoloads for the symlinked package specified by RECIPE.
RECIPE should be a straight.el-style plist. See
`straight--autoloads-file-name'. Note that this function only
modifies the build folder, not the original repository."
  ;; The `eval-and-compile' here is extremely important. If you take
  ;; it out, then straight.el will fail with a mysterious error and
  ;; then cause Emacs to segfault if you start it with --debug-init.
  ;; This happens because if you take out `eval-and-compile', then
  ;; `autoload' will not be loaded at byte-compile time, and therefore
  ;; `generated-autoload-file' is not defined as a variable. Thus
  ;; Emacs generates bytecode corresponding to a lexical binding of
  ;; `generated-autoload-file', and then chokes badly when
  ;; `generated-autoload-file' turns into a dynamic variable at
  ;; runtime.
  (eval-and-compile
    (require 'autoload))
  (straight--with-plist recipe
      (package)
    (let (;; The full path to the autoload file.
          (generated-autoload-file (straight--autoloads-file package))
          ;; The following bindings are in
          ;; `package-generate-autoloads'. Presumably this is for a
          ;; good reason, so I just copied them here. It's a shame
          ;; that Emacs activates so many random features even when
          ;; you are accessing files programmatically.
          ;;
          ;; Note: we used to bind `noninteractive', like package.el,
          ;; but apparently that code was a bug in package.el. Sigh.
          ;; See <https://github.com/raxod502/straight.el/issues/431>.
          (backup-inhibited t)
          (version-control 'never)
          ;; Tell Emacs to shut up.
          (message-log-max nil) ; no *Messages*
          (inhibit-message t)) ; no echo area
      ;; If the package provides an autoload file already, then don't
      ;; overwrite it (which would actually write into the source
      ;; repository through the symlink).
      (unless (file-exists-p generated-autoload-file)
        ;; Prevent `update-directory-autoloads' from running hooks
        ;; (for example, adding to `recentf') when visiting the
        ;; autoload file.
        (let ((find-file-hook nil)
              (write-file-functions nil)
              ;; Apparently fixes a bug in Emacs 27, see
              ;; <https://github.com/raxod502/straight.el/issues/434>.
              (debug-on-error nil))
          ;; Actually generate the autoload file.
          ;; Emacs > 28.1 replaces `update-directory-autoloads' with
          ;; `make-directory-autoloads'
          (if (fboundp 'make-directory-autoloads)
              (make-directory-autoloads (straight--build-dir package)
                                        generated-autoload-file)
            (and (fboundp 'update-directory-autoloads)
                 (update-directory-autoloads (straight--build-dir package)))))
        ;; And for some reason Emacs leaves a newly created buffer
        ;; lying around. Let's kill it.
        (when-let ((buf (find-buffer-visiting generated-autoload-file)))
          (kill-buffer buf))))))

;;;;; Byte-compilation

(define-obsolete-variable-alias 'straight-disable-byte-compilation
  'straight-disable-compile "2021-01-01")

(defcustom straight-disable-compile nil
  "Non-nil means do not byte-compile packages by default.
This can be overridden by the `:build' property of an
individual package recipe."
  :type 'boolean)

(defun straight--build-compile (recipe)
  "Byte-compile files for the symlinked package specified by RECIPE.
RECIPE should be a straight.el-style plist. Note that this
function only modifies the build folder, not the original
repository."
  (let* ((package (plist-get recipe :package))
         (dir (straight--build-dir package))
         (program (concat invocation-directory invocation-name))
         (args
          `("-Q" "-L" ,dir
            ,@(apply #'append
                     (mapcar (lambda (d)
                               (let ((d (straight--build-dir d)))
                                 (when (file-exists-p d) (list "-L" d))))
                             (straight--get-dependencies package)))
            "--batch" "--eval"
            ,(format "(byte-recompile-directory %S 0 'force)" dir))))
    (when straight-byte-compilation-buffer
      (with-current-buffer (get-buffer-create straight-byte-compilation-buffer)
        (insert "\n$ " (replace-regexp-in-string
                        "\\(-L [^z-a]*? \\)"
                        "\\1\\\\ \n  "
                        (string-join `(,program ,@args) " "))
                "\n")))
    (apply #'call-process
           `(,program nil ,straight-byte-compilation-buffer nil ,@args))))

;;;;; Native compilation

(define-obsolete-variable-alias 'straight-disable-native-compilation
  'straight-disable-native-compile "2021-01-01")

(defconst straight--native-comp-available
  (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  "Non-nil if Emacs was compiled with native compilation support.")

(defcustom straight-disable-native-compile
  (not straight--native-comp-available)
  "Non-nil means do not `native-compile' packages by default.
This can be overridden by the `:build' property of an
individual package recipe."
  :type 'boolean)

(defun straight--build-native-compile (recipe)
  "Queue native compilation for RECIPE's package.
RECIPE should be a straight.el-style plist.
Native compilation occurs asynchronously, and will continue in the
background after `straight-use-package' returns."
  (when (and straight--native-comp-available
             (member 'straight--build-compile straight--build-functions))
    (require 'comp)
    (straight--with-plist recipe (package)
      (let ((build-dir (straight--build-dir package)))
        (when (and straight--build-cache
                   (gethash package straight--build-cache))
          (let ((regexp (format "^%s" build-dir)))
            (setq native-comp-deferred-compilation-deny-list
                  (cl-remove-if (lambda (denied) (string= denied regexp))
                                native-comp-deferred-compilation-deny-list))))
        (let ((inhibit-message t)
              (message-log-max nil))
          (native-compile-async build-dir 'recursively))))))

;;;;; Info compilation

(defcustom straight-disable-info nil
  "Non-nil means do not generate or activate texinfo by default.
This can be overridden by the `:build' property of an individual
package recipe."
  :type 'boolean)

(defcustom straight-makeinfo-executable (executable-find "makeinfo")
  "Location of the makeinfo executable."
  :type 'string)

(defcustom straight-install-info-executable
  (executable-find "install-info")
  "Location of the install-info executable."
  :type 'string)

(defun straight--build-info (recipe)
  "Compile .texi files into .info files for package specified by RECIPE.
RECIPE should be a straight.el-style plist. Note that this
function only modifies the build directory, not the original
repository."
  (when (and straight-install-info-executable
             straight-makeinfo-executable)
    (straight--with-plist recipe
        (package local-repo files flavor)
      (let (infos)
        (pcase-dolist (`(,repo-file . ,build-file)
                       (straight-expand-files-directive
                        files
                        (straight--repos-dir local-repo)
                        (straight--build-dir package)
                        flavor))
          (cond
           ((string-match-p "\\.info$" build-file)
            (push build-file infos))
           ((string-match-p "\\.texi\\(nfo\\)?$" repo-file)
            (let ((texi repo-file)
                  (info
                   (concat (file-name-sans-extension build-file) ".info")))
              (push info infos)
              (unless (file-exists-p info)
                (let ((default-directory (file-name-directory texi)))
                  (straight--process-run "makeinfo" texi "-o" info)))))))
        (let ((dir (straight--build-file package "dir")))
          (unless (file-exists-p dir)
            (dolist (info infos)
              (when (file-exists-p info)
                (straight--process-run "install-info" info dir)))))))))

;;;;; Cache handling

(defun straight--declare-successful-build (recipe)
  "Update `straight--build-cache' to reflect a successful build of RECIPE.
RECIPE should be a straight.el-style plist. The build mtime and
recipe in `straight--build-cache' for the package are updated."
  (straight--with-plist recipe (package)
    ;; We've rebuilt the package, so its autoloads might have changed.
    (remhash package straight--autoloads-cache)
    ;; This time format is compatible with:
    ;;
    ;; * BSD find shipped with macOS >=10.11
    ;; * GNU find >=4.4.2
    ;;
    ;; Time is rounded up to the next second to avoid spurious
    ;; rebuilds. find(1) millisecond precision is not guaranteed
    ;; on all platforms (e.g. Apple).
    (straight--insert 0 package
                      (format-time-string "%F %T" (time-add nil 1))
                      straight--build-cache)
    (straight--insert 2 package recipe straight--build-cache)))

;;;; Loading packages

(defun straight--add-package-to-load-path (recipe)
  "Add the package specified by RECIPE to the `load-path'.
RECIPE is a straight.el-style plist. It is assumed that the
package has already been built."
  (straight--with-plist recipe
      (package)
    (add-to-list 'load-path (directory-file-name
                             (straight--build-dir package)))))

(defun straight--add-package-to-info-path (recipe)
  "Add the package specified by RECIPE to the `Info-directory-list'.
RECIPE is a straight.el-style plist. It is assumed that the
package has already been built. This function calls
`info-initialize'."
  (straight--with-plist recipe
      (package)
    ;; The `info-initialize' function is not autoloaded, for some
    ;; reason. Do `eval-and-compile' for the byte-compiler.
    (eval-and-compile
      (require 'info))
    ;; Initialize the `Info-directory-list' variable. We have to do
    ;; this before adding to it, since otherwise the default paths
    ;; won't get added later.
    (info-initialize)
    ;; Actually add the path. Only .info files at the top level will
    ;; be seen, which is fine. (It's the way MELPA works.)
    (add-to-list 'Info-directory-list (straight--build-dir package))))

(defun straight--load-package-autoloads (package)
  "Load autoloads provided by PACKAGE, a string, from disk."
  (let ((autoloads-file (straight--autoloads-file package)))
    ;; NB: autoloads file may not exist if no autoloads were provided,
    ;; in Emacs 26.
    (when (file-exists-p autoloads-file)
      (load autoloads-file nil 'nomessage))))

(defun straight--determine-package-features (package)
  "Determine what features are provided by PACKAGE, a string.
Inspect the build directory to find Emacs Lisp files that might
be loadable via `require'."
  (let ((files (straight--directory-files
                (straight--build-dir package)
                "^.+\\.el$")))
    (mapcar
     (lambda (fname)
       (intern (substring fname 0 -3)))
     files)))

(defun straight--read-package-autoloads (package)
  "Read and return autoloads provided by PACKAGE, a string, from disk.
The format is a list of Lisp forms to be evaluated."
  (let ((autoloads-file (straight--autoloads-file package)))
    ;; NB: autoloads file may not exist if no autoloads were provided,
    ;; in Emacs 26.
    (when (file-exists-p autoloads-file)
      (with-temp-buffer
        (insert-file-contents-literally autoloads-file)
        (let (;; Make the $# reader macro expand to the correct
              ;; filename.
              (load-file-name autoloads-file)
              (autoloads nil))
          (condition-case _
              (while t
                (push (read (current-buffer)) autoloads))
            (end-of-file))
          (nreverse autoloads))))))

(defun straight--activate-package-autoloads (recipe)
  "Evaluate the autoloads for the package specified by RECIPE.
This means that the functions with autoload cookies in the
package are now autoloaded and calling them will `require' the
package. It is assumed that the package has already been built.
If no autoload file exists (perhaps due to a non-nil
`:no-autoloads' attribute on the package recipe or due to the
global setting of `straight-disable-autoloads' or even because
Emacs 26 seems to not generate an autoload file when there are no
autoloads declared), then do nothing.

If `straight-cache-autoloads' is non-nil, read and write from the
global autoloads cache in order to speed up this process.

RECIPE is a straight.el-style plist."
  (straight--with-plist recipe
      (package)
    (if straight-cache-autoloads
        (progn
          (unless (straight--checkhash package straight--autoloads-cache)
            (let ((features (straight--determine-package-features package))
                  (autoloads (straight--read-package-autoloads package)))
              (puthash package (cons features autoloads)
                       straight--autoloads-cache)))
          ;; Some autoloads files expect to be loaded normally, rather
          ;; than read and evaluated separately. Fool them.
          (let ((load-file-name (straight--autoloads-file package))
                (load-in-progress t))
            ;; car is the feature list, cdr is the autoloads.
            (dolist (form (cdr (gethash package straight--autoloads-cache)))
              (eval form))))
      (straight--load-package-autoloads package))))

;;;; Interactive helpers
;;;;; Package selection

(defun straight--select-package (message &optional filter)
  "Use `completing-read' to select a package.
MESSAGE is displayed as the prompt; it should not end in punctuation
or whitespace.

FILTER is a function accepting one argument: a straight style recipe plist.
If it returns nil, the package is not considered a selection candidate."
  (completing-read
   (concat message ": ")
   (let ((packages nil))
     (maphash (lambda (package recipe)
                (when (or (null filter)
                          (funcall filter (plist-put recipe :package package)))
                  (push package packages)))
              straight--recipe-cache)
     (nreverse packages))
   (lambda (_) t)
   'require-match))

;;;;; Bookkeeping

(defvar straight--success-cache (make-hash-table :test #'equal)
  "Hash table containing successfully built packages as keys.
The keys are package names as strings; the values are
meaningless, and all non-nil.")

(defvar straight--packages-to-rebuild nil
  "Hash table of packages for which to force a rebuild.
The keys are package names as strings; the values are
meaningless, and all non-nil. When not let-bound, this variable
is nil. When `straight-use-package' is invoked for any of these
packages, they will be rebuilt even if they have not changed. The
special value `:all' is equivalent to a list of all possible
packages. See also `straight-rebuild-package'.")

(defvar straight--packages-not-to-rebuild nil
  "Hash table of packages for which rebuild forcing does not apply.
The keys are package names as strings; the values are
meaningless, and all non-nil. When not let-bound, this variable
is nil. Any packages in this list are immune to the effects of
`straight--packages-to-rebuild', even if it is set to `:all'.
This is used to prevent building dependencies twice when
`straight-rebuild-package' or `straight-rebuild-all' is
invoked.")

;;;;; Interactive mapping

(cl-defun straight--map-repos-interactively (func &optional predicate action)
  "Apply function FUNC for all local repositories, interactively.
FUNC is passed the name of one of the packages drawn from each
local repository, as a string. If FUNC throws an error or a quit
signal, the user is asked about what to do. They can choose to
skip the repository and come back to it later, cancel its
processing entirely, or halt the entire operation (skipping the
processing of all pending repositories). The return value of this
function is the list of recipes for repositories that were not
processed.

PREDICATE, if provided, is passed the package name as a string,
and should return a non-nil value to indicate that the package
should actually be processed.

ACTION is an optional string that describes the action being
performed on each repository, to be used for progress messages.
The default value is \"Processing\"."
  (let ((next-repos nil)
        (skipped-repos nil)
        (canceled-repos nil))
    (straight--map-repos
     (lambda (recipe)
       (push recipe next-repos)))
    (while t
      (cond
       (next-repos
        (let ((recipe (car next-repos)))
          (straight--with-plist recipe
              (package local-repo)
            (if (or (null predicate)
                    (funcall predicate package))
                (straight--with-progress
                    (format "%s repository %S"
                            (or action "Processing")
                            local-repo)
                  (cl-block loop
                    (while t
                      (straight--popup
                        (if-let ((err
                                  (condition-case-unless-debug e
                                      (progn
                                        (funcall func package)
                                        (setq next-repos (cdr next-repos))
                                        (cl-return-from loop))
                                    (error e)
                                    (quit))))
                            (format (concat "While processing repository %S, "
                                            "an error occurred:\n\n  %S")
                                    local-repo (error-message-string err))
                          (format (concat "Processing of repository %S paused "
                                          "at your request.")
                                  local-repo))
                        ("SPC" "Go back to processing this repository")
                        ("s" (concat "Skip this repository for now and "
                                     "come back to it later")
                         (push recipe skipped-repos)
                         (setq next-repos (cdr next-repos))
                         (cl-return-from loop))
                        ("c" (concat "Cancel processing of this "
                                     "repository; move on and do not "
                                     "come back to it later")
                         (push recipe canceled-repos)
                         (setq next-repos (cdr next-repos))
                         (cl-return-from loop))
                        ("e" "Dired and open recursive edit"
                         (dired (straight--repos-dir local-repo))
                         (straight--recursive-edit))
                        ("C-g" (concat "Stop immediately and do not process "
                                       "more repositories")
                         (keyboard-quit))))))
              (setq next-repos (cdr next-repos))))))
       (skipped-repos
        (setq next-repos skipped-repos)
        (setq skipped-repos nil))
       (t (cl-return-from straight--map-repos-interactively
            canceled-repos))))))

(defun straight--map-existing-repos-interactively
    (func &optional predicate action)
  "Apply function FUNC for all existing local repositories, interactively.
PREDICATE and ACTION are as in
`straight--map-repos-interactively'. The only difference is that
this function modifies PREDICATE to additionally require that the
local repository is already on disk."
  (straight--map-repos-interactively
   func
   (lambda (package)
     (let ((recipe (gethash package straight--recipe-cache)))
       (straight--with-plist recipe
           (local-repo)
         (and local-repo
              (straight--repository-is-available-p recipe)
              (or (null predicate) (funcall predicate package))))))
   action))

;;;; User-facing functions
;;;;; Removing unused repositories

;;;###autoload
(defun straight-remove-unused-repos (&optional force)
  "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos."
  (interactive "P")
  (let ((r '()))
    (dolist (repo (straight--directory-files
                   (straight--repos-dir) nil nil 'sort))
      (unless (straight--checkhash repo straight--repo-cache)
        (when (or force (y-or-n-p (format "Delete repository %S? " repo)))
          (delete-directory (straight--repos-dir repo) 'recursive 'trash)
          (push repo r))))
    (message (concat "%d " (if (eq (length r) 1) "repository" "repositories")
                     " deleted" (if r ": %s" "."))
             (length r) (nreverse r))))

;;;;; Recipe acquiry

;;;###autoload
(defun straight-get-recipe (&optional sources action)
  "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it)."
  (interactive (list (when current-prefix-arg 'interactive) 'copy))
  (when (eq sources 'interactive)
    (setq sources (list
                   (intern
                    (completing-read
                     "Which recipe repository? "
                     straight-recipe-repositories
                     nil
                     'require-match)))))
  (let ((sources (or sources straight-recipe-repositories)))
    (let* ((package (intern
                     (completing-read
                      "Which recipe? "
                      (straight-recipes-list sources)
                      (lambda (_) t)
                      'require-match)))
           ;; No need to provide a `cause' to
           ;; `straight-recipes-retrieve'; it should not be printing
           ;; any messages.
           (recipe (straight-recipes-retrieve package sources)))
      (unless recipe
        (user-error "Recipe for %S is malformed" package))
      (pcase action
        ('insert (insert (format "%S" recipe)))
        ('copy (kill-new (format "%S" recipe))
               (straight--output "Copied \"%S\" to kill ring" recipe))
        (_ recipe)))))

;;;;; Update recipe repositories
(defun straight-pull-recipe-repositories (&optional sources)
  "Update recipe repository SOURCES.
When called with `\\[universal-argument]', prompt for SOURCES.
If SOURCES is nil, update sources in `straight-recipe-repositories'."
  (interactive (list (if (equal current-prefix-arg '(4))
                         (completing-read-multiple
                          "Recipe Repositories (empty to select all): "
                          straight-recipe-repositories nil 'require-match)
                       straight-recipe-repositories)))
  (dolist (source (delete-dups
                   (mapcar (lambda (src) (if (stringp src) (intern src) src))
                           (or sources straight-recipe-repositories))))
    (unless (member source straight-recipe-repositories)
      (user-error
       "Package %S is not a recipe repository; see `straight-use-recipes'"
       source))
    (straight-pull-package-and-deps (symbol-name source) 'upstream)))

;;;;; Jump to package website

;;;###autoload
(defun straight-visit-package-website ()
  "Interactively select a recipe, and visit the package's website."
  (interactive)
  (let* ((melpa-recipe (straight-get-recipe))
         (recipe (straight--convert-recipe melpa-recipe)))
    (straight--with-plist recipe (host repo)
      (pcase host
        ('github (browse-url (format "https://github.com/%s" repo)))
        ('gitlab (browse-url (format "https://gitlab.com/%s" repo)))
        (_ (browse-url (format "%s" repo)))))))

;;;;; Package registration

(defcustom straight-use-package-prepare-functions nil
  "Abnormal hook run before a package is (maybe) built.
Unlike `straight-use-package-pre-build-functions', the functions
in this hook are called even if the package does not need to be
rebuilt. Each hook function is called with the name of the
package as a string. For forward compatibility, it should accept
and ignore additional arguments."
  :type 'hook)

(defcustom straight-use-package-pre-build-functions nil
  "Abnormal hook run before building a package.
Each hook function is called with the name of the package as a
string. For forward compatibility, it should accept and ignore
additional arguments."
  :type 'hook)

(defcustom straight-use-package-post-build-functions nil
  "Abnormal hook run after building a package.
Each hook function is called with the name of the package as a
string. For forward compatibility, it should accept and ignore
additional arguments."
  :type 'hook)

;;;###autoload
(cl-defun straight-use-package
    (melpa-style-recipe &optional no-clone no-build cause interactive)
  "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil)."
  (interactive
   (list (straight-get-recipe (when current-prefix-arg 'interactive))
         nil nil nil 'interactive))
  (let ((recipe (straight--convert-recipe
                 (or
                  (straight--get-overridden-recipe
                   (if (listp melpa-style-recipe)
                       (car melpa-style-recipe)
                     melpa-style-recipe))
                  melpa-style-recipe)
                 cause)))
    ;; We need to register the recipe before building the package,
    ;; since the ability of `straight--convert-recipe' to deal
    ;; properly with dependencies versioned in the same repository of
    ;; their parent package will break unless the caches are updated
    ;; before we recur to the dependencies.
    ;;
    ;; Furthermore, we need to register it before executing the
    ;; transaction block, since otherwise conflicts between recipes
    ;; cannot be detected (the transaction block will only be run once
    ;; for any given package in a transaction).
    (straight--register-recipe recipe)
    (straight--with-plist recipe
        (package local-repo type)
      ;; Now, don't bother to go any further if the package was
      ;; built-in. Return non-nil.
      (when (eq type 'built-in)
        (cl-return-from straight-use-package t))
      (straight--transaction-exec
       ;; Ignore when the same package is requested twice (this will
       ;; happen many times during dependency resolution). However,
       ;; allow the user to request a package twice with different
       ;; recipes or build settings, and in that case re-check
       ;; everything.
       (intern (format "use-package-%S-%S-%S"
                       recipe
                       ;; If the NO-CLONE and NO-BUILD functions
                       ;; compute their values dynamically, force
                       ;; package rebuilding so the changed values may
                       ;; be taken into account.
                       (if (straight--functionp no-clone)
                           (random)
                         no-clone)
                       (if (straight--functionp no-build)
                           (random)
                         no-build)))
       :now
       (lambda ()
         (let (;; Check if the package has been successfully built. If
               ;; not, and this is an interactive call, we'll want to
               ;; display a helpful hint message (see below). We have
               ;; to check this here, before the package is actually
               ;; built.
               (already-registered
                (gethash package straight--success-cache))
               (available
                ;; Package is vacuously available if nil local
                ;; repository was specified (if *no* local repository
                ;; was specified, which is different, then a default
                ;; name would have been generated).
                (or (null local-repo)
                    (straight--repository-is-available-p recipe))))
           ;; Possibly abort based on NO-CLONE.
           (when (if (straight--functionp no-clone)
                     (funcall no-clone package available)
                   no-clone)
             (cl-return-from straight-use-package nil))
           ;; If we didn't abort, ensure the repository is cloned.
           (unless available
             ;; We didn't decide to abort, and the repository still
             ;; isn't available. Make it available.
             (straight--clone-repository recipe cause))
           ;; Do this even for packages with `no-build' enabled, as we
           ;; still want to check for modifications and (if any)
           ;; invalidate the relevant entry in the recipe lookup
           ;; cache.
           (straight--make-build-cache-available)
           (let* ((no-build
                   (or
                    ;; Remember that `no-build' can come both from the
                    ;; arguments to `straight-use-package' and from
                    ;; the actual recipe. We also refrain from trying
                    ;; to build packages which have no local
                    ;; repositories.
                    (null local-repo)
                    (straight--build-disabled-p recipe)
                    (if (straight--functionp no-build)
                        (funcall no-build package)
                      no-build)))
                  (modified
                   (or
                    ;; This clause provides support for
                    ;; `straight-rebuild-package' and
                    ;; `straight-rebuild-all'.
                    (and
                     straight--packages-to-rebuild
                     (or (eq straight--packages-to-rebuild :all)
                         (gethash package straight--packages-to-rebuild))
                     (not (gethash
                           package straight--packages-not-to-rebuild))
                     ;; The following form returns non-nil, so it
                     ;; doesn't affect the `and' logic.
                     (puthash package t straight--packages-not-to-rebuild))
                    (straight--package-might-be-modified-p
                     recipe no-build))))
             (let ((func (intern (format "straight-recipes-%s-version"
                                         package)))
                   (table (gethash (intern package)
                                   straight--recipe-lookup-cache)))
               ;; Invalidate recipe lookup cache when the recipe
               ;; repository is modified, or caching is disabled
               ;; because there's no version function defined for the
               ;; recipe repository, or because the version has
               ;; changed since the last time it was cached.
               (when (or modified
                         (not (fboundp func))
                         (not (equal
                               ;; Avoid trying to look up a key in a
                               ;; nil table.
                               (and table (gethash 'version table))
                               (funcall func))))
                 (remhash (intern package) straight--recipe-lookup-cache)))
             (unless no-build
               ;; Multi-file packages will need to be on the
               ;; `load-path' in order to byte-compile properly. So we
               ;; do this before `straight--build-package'.
               (straight--add-package-to-load-path recipe))
             (run-hook-with-args
              'straight-use-package-prepare-functions package)
             ;; Prevent deferred native compilation of packages which
             ;; explicitly disable it.
             (when (boundp 'native-comp-deferred-compilation-deny-list)
               (when-let ((build (cadr (plist-member recipe :build))))
                 (when (and (eq (car-safe build) :not)
                            (member 'native-compile (cdr build)))
                   (cl-pushnew (format "^%s" (straight--build-dir package))
                               native-comp-deferred-compilation-deny-list
                               :test #'string=))))
             (when (and modified (not no-build))
               (run-hook-with-args
                'straight-use-package-pre-build-functions package)
               (straight--build-package recipe cause))
             ;; We need to do this even if the package wasn't built,
             ;; so we can keep track of modifications.
             (straight--declare-successful-build recipe)
             (unless no-build
               ;; Here we are not actually trying to build the
               ;; dependencies, but activate their autoloads. (See the
               ;; comment in `straight--build-package' about this
               ;; code.)
               (dolist (dependency (straight--get-dependencies package))
                 ;; There are three interesting things here. Firstly,
                 ;; the recipe used is just the name of the
                 ;; dependency. This causes the default recipe to be
                 ;; looked up, unless one of the special cases in
                 ;; `straight--convert-recipe' pops up. Secondly, the
                 ;; values of NO-BUILD and NO-CLONE are always nil. If
                 ;; the user has agreed to clone and build a package,
                 ;; we assume that they also want to clone and build
                 ;; all of its dependencies. Finally, we don't bother
                 ;; to update `cause', since we're not expecting any
                 ;; messages to be displayed here (all of the
                 ;; dependencies should have already been cloned [if
                 ;; necessary] and built back by
                 ;; `straight--build-package').
                 (straight-use-package (intern dependency) nil nil cause))
               ;; Only make the package available after everything is
               ;; kosher.
               (straight--add-package-to-info-path recipe)
               (straight--activate-package-autoloads recipe))
             ;; In interactive use, tell the user how to install
             ;; packages permanently.
             (when (and interactive (not already-registered))
               (straight--output
                (concat "If you want to keep %s, put "
                        "(straight-use-package %s%S) "
                        "in your init-file.")
                package "'" (intern package))))
           ;; The package was installed successfully.
           (puthash package t straight--success-cache)
           t))))))

;;;###autoload
(defun straight-register-package (melpa-style-recipe)
  "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'."
  (straight-use-package melpa-style-recipe 'no-clone))

;;;###autoload
(defun straight-use-package-no-build (melpa-style-recipe)
  "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'."
  (straight-use-package melpa-style-recipe nil 'no-build))

;;;###autoload
(defun straight-use-package-lazy (melpa-style-recipe)
  "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'."
  (straight-use-package
   melpa-style-recipe
   ;; Don't clone the package if it's not available.
   (lambda (_package available)
     (not available))))

;;;###autoload
(defun straight-use-recipes (melpa-style-recipe)
  "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list."
  (straight-use-package-lazy melpa-style-recipe)
  (add-to-list 'straight-recipe-repositories
               (if (listp melpa-style-recipe)
                   (car melpa-style-recipe)
                 melpa-style-recipe)
               'append))

;;;;; Recipe overrides

;;;###autoload
(defun straight-override-recipe (melpa-style-recipe)
  "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'."
  (setq straight-recipe-overrides
        (straight--alist-set
         straight-current-profile
         (straight--alist-set
          (car melpa-style-recipe)
          (cdr melpa-style-recipe)
          (alist-get straight-current-profile straight-recipe-overrides)
          'symbol)
         straight-recipe-overrides
         'symbol)))

;;;;; Rebuilding packages

;;;###autoload
(defun straight-check-package (package)
  "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'."
  (interactive (list (straight--select-package
                      "Check package"
                      #'straight--installed-and-buildable-p)))
  (let ((straight--allow-find t))
    (straight-use-package (intern package))))

;;;###autoload
(defun straight-check-all ()
  "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init."
  (interactive)
  (let ((straight--allow-find t))
    (dolist (package (hash-table-keys straight--recipe-cache))
      (straight-use-package (intern package)))))

;;;###autoload
(defun straight-rebuild-package (package &optional recursive)
  "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'."
  (interactive
   (list (straight--select-package "Rebuild package"
                                   #'straight--installed-and-buildable-p)
         current-prefix-arg))
  (let ((straight--packages-to-rebuild
         (if recursive
             :all
           (let ((table (make-hash-table :test #'equal)))
             (puthash package t table)
             table)))
        ;; Because we bind this here, the table will be deleted and
        ;; the variable reset to nil when we break out of the let. No
        ;; need to clear the hash explicitly.
        (straight--packages-not-to-rebuild
         (make-hash-table :test #'equal)))
    (straight-use-package (intern package))))

;;;###autoload
(defun straight-rebuild-all ()
  "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'."
  (interactive)
  (let ((straight--packages-to-rebuild :all)
        (straight--packages-not-to-rebuild
         (make-hash-table :test #'equal)))
    (dolist (package (hash-table-keys straight--recipe-cache))
      (straight-use-package (intern package)))))

;;;;; Cleanup

;;;###autoload
(defun straight-prune-build-cache ()
  "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded."
  (straight--make-build-cache-available)
  (dolist (package (hash-table-keys straight--build-cache))
    (unless (gethash package straight--profile-cache)
      (remhash package straight--build-cache)
      (remhash package straight--autoloads-cache)
      (message "Pruned %s from build cache" package)))
  (dolist (source (hash-table-keys straight--recipe-lookup-cache))
    (if (gethash (symbol-name source) straight--profile-cache)
        (let ((table (gethash source straight--recipe-lookup-cache)))
          (dolist (package (hash-table-keys table))
            (unless (or (equal package 'version)
                        (gethash package straight--profile-cache))
              (remhash package table))))
      (remhash source straight--recipe-lookup-cache))))

;;;###autoload
(defun straight-prune-build-directory ()
  "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted."
  (straight--make-build-cache-available)
  (dolist (package (straight--directory-files
                    (straight--build-dir)))
    ;; So, let me tell you a funny story. Once upon a time I didn't
    ;; have this `string-match-p' condition. But Emacs helpfully
    ;; returns . and .. from the call to `directory-files',
    ;; resulting in the entire build directory and its parent
    ;; directory also being deleted. Fun fun fun. (Now that I've
    ;; replaced `directory-files' with `straight--directory-files',
    ;; . and .. are no longer returned. But it's always good to be
    ;; paranoid with recursive deletes.)
    (unless (or (string-match-p "^\\.\\.?$" package)
                (gethash package straight--profile-cache))
      (delete-directory (straight--build-dir package) 'recursive)
      (message "Pruned %s from build directory" package))))

;;;###autoload
(defun straight-prune-build ()
  "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted."
  (interactive)
  (straight-prune-build-cache)
  (straight-prune-build-directory))

;;;;; Normalization, pushing, pulling

;;;###autoload
(defun straight-normalize-package (package)
  "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'."
  (interactive (list (straight--select-package "Normalize package"
                                               #'straight--installed-p)))
  (let ((recipe (gethash package straight--recipe-cache)))
    (straight-vc-normalize recipe)))

;;;###autoload
(defun straight-normalize-all (&optional predicate)
  "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized."
  (interactive)
  (straight--map-existing-repos-interactively #'straight-normalize-package
                                              predicate))

(defun straight--get-transitive-dependencies (package)
  "Get the (transitive) dependencies of PACKAGE.
PACKAGE should be a string naming a package. Return a list of
strings naming packages which are the dependencies, dependencies
of dependencies, etc. of PACKAGE. The list includes PACKAGE
itself. It is in no particular order."
  (straight--make-build-cache-available :nosave t)
  (let ((all-packages (make-hash-table :test #'equal))
        (unprocessed (make-hash-table :test #'equal)))
    (puthash package t unprocessed)
    (while (not (hash-table-empty-p unprocessed))
      (let* ((cur-package (car (hash-table-keys unprocessed)))
             (deps (nth 1 (gethash cur-package straight--build-cache))))
        (puthash cur-package t all-packages)
        (remhash cur-package unprocessed)
        (dolist (dep deps)
          (unless (gethash dep all-packages)
            (puthash dep t unprocessed)))))
    (hash-table-keys all-packages)))

;;;###autoload
(defun straight-fetch-package (package &optional from-upstream)
  "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages)."
  (interactive (list (straight--select-package "Fetch package"
                                               #'straight--installed-p)
                     current-prefix-arg))
  (let ((recipe (gethash package straight--recipe-cache)))
    (and (straight-vc-fetch-from-remote recipe)
         (when from-upstream
           (straight-vc-fetch-from-upstream recipe)))))

;;;###autoload
(defun straight-fetch-package-and-deps (package &optional from-upstream)
  "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages)."
  (interactive (list (straight--select-package "Fetch package and dependencies"
                                               #'straight--installed-p)
                     current-prefix-arg))
  (let ((deps (make-hash-table :test #'equal)))
    (dolist (dep (straight--get-transitive-dependencies package))
      (puthash dep t deps))
    (straight--map-existing-repos-interactively
     (lambda (cur-package)
       (straight-fetch-package cur-package from-upstream))
     (lambda (cur-package)
       (gethash cur-package deps)))))

;;;###autoload
(defun straight-fetch-all (&optional from-upstream predicate)
  "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched."
  (interactive "P")
  (straight--map-existing-repos-interactively
   (lambda (package)
     (straight-fetch-package package from-upstream))
   predicate))

;;;###autoload
(defun straight-merge-package (package &optional from-upstream)
  "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages)."
  (interactive (list (straight--select-package "Merge package"
                                               #'straight--installed-p)
                     current-prefix-arg))
  (let ((recipe (gethash package straight--recipe-cache)))
    (and (straight-vc-merge-from-remote recipe)
         (when from-upstream
           (straight-vc-merge-from-upstream recipe)))))

;;;###autoload
(defun straight-merge-package-and-deps (package &optional from-upstream)
  "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages)."
  (interactive (list (straight--select-package
                      "Merge package and dependencies" #'straight--installed-p)
                     current-prefix-arg))
  (let ((deps (make-hash-table :test #'equal)))
    (dolist (dep (straight--get-transitive-dependencies package))
      (puthash dep t deps))
    (straight--map-existing-repos-interactively
     (lambda (cur-package)
       (straight-merge-package cur-package from-upstream))
     (lambda (cur-package)
       (gethash cur-package deps)))))

;;;###autoload
(defun straight-merge-all (&optional from-upstream predicate)
  "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged."
  (interactive "P")
  (straight--map-existing-repos-interactively
   (lambda (package)
     (straight-merge-package package from-upstream))
   predicate))

;;;###autoload
(defun straight-pull-package (package &optional from-upstream)
  "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages)."
  (interactive (list (straight--select-package "Pull package"
                                               #'straight--installed-p)
                     current-prefix-arg))
  (let ((recipe (gethash package straight--recipe-cache)))
    (and (straight-vc-fetch-from-remote recipe)
         (straight-vc-merge-from-remote recipe)
         (when from-upstream
           (and (straight-vc-fetch-from-upstream recipe)
                (straight-vc-merge-from-upstream recipe))))))

;;;###autoload
(defun straight-pull-package-and-deps (package &optional from-upstream)
  "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages)."
  (interactive (list (straight--select-package
                      "Pull package and dependencies" #'straight--installed-p)
                     current-prefix-arg))
  (let ((deps (make-hash-table :test #'equal)))
    (dolist (dep (straight--get-transitive-dependencies package))
      (puthash dep t deps))
    (straight--map-existing-repos-interactively
     (lambda (cur-package)
       (straight-pull-package cur-package from-upstream))
     (lambda (cur-package)
       (gethash cur-package deps)))))

;;;###autoload
(defun straight-pull-all (&optional from-upstream predicate)
  "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled."
  (interactive "P")
  (straight-fetch-all from-upstream predicate)
  (straight-merge-all from-upstream predicate))

;;;###autoload
(defun straight-push-package (package)
  "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'."
  (interactive (list (straight--select-package "Push package"
                                               #'straight--installed-p)))
  (let ((recipe (gethash package straight--recipe-cache)))
    (straight-vc-push-to-remote recipe)))

;;;###autoload
(defun straight-push-all (&optional predicate)
  "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized."
  (interactive)
  (straight--map-existing-repos-interactively
   #'straight-push-package predicate))

;;;;; Lockfile management

(cl-defun straight--ensure-profile-cache-valid ()
  "Ensure that `straight--profile-cache' reflects the init-file correctly.
If not, prompt the user to reload the init-file."
  (when straight--profile-cache-valid
    (cl-return-from straight--ensure-profile-cache-valid t))
  (unless (y-or-n-p "Caches are outdated, reload init-file? ")
    (cl-return-from straight--ensure-profile-cache-valid nil))
  (straight--output "Reloading %S..." user-init-file)
  (load user-init-file nil 'nomessage)
  (straight--output "Reloading %S...done" user-init-file)
  (when straight--profile-cache-valid
    (cl-return-from straight--ensure-profile-cache-valid t))
  (error "Caches are still outdated; something is seriously wrong"))

;;;###autoload
(defun straight-freeze-versions (&optional force)
  "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'."
  (interactive "P")
  (when (or force
            (progn
              (straight--ensure-profile-cache-valid)
              (let ((unpushed-recipes
                     (straight-push-all
                      (lambda (package)
                        (let ((recipe
                               (gethash package straight--recipe-cache)))
                          (straight--with-plist recipe
                              (local-repo)
                            (and local-repo
                                 (straight--repository-is-available-p
                                  recipe))))))))
                (or
                 (null unpushed-recipes)
                 (straight-are-you-sure
                  (format (concat "The following packages were not pushed:"
                                  "\n\n  %s\n\nReally write lockfiles?")
                          (mapconcat
                           (lambda (recipe) (plist-get recipe :local-repo))
                           unpushed-recipes ", ")))))))
    (straight--map-repos
     (lambda (recipe)
       (straight--with-plist recipe
           (local-repo package)
         (unless (or (null local-repo)
                     (straight--repository-is-available-p recipe))
           (straight-use-package (intern package) nil 'no-build)))))
    (dolist (spec straight-profiles)
      (cl-destructuring-bind (profile . versions-lockfile) spec
        (let ((versions-alist nil)
              (lockfile-path (straight--versions-file versions-lockfile)))
          (straight--map-repos
           (lambda (recipe)
             (straight--with-plist recipe
                 (package local-repo type)
               (when (and local-repo
                          (memq profile
                                (gethash package straight--profile-cache))
                          (not (assoc local-repo versions-alist)))
                 (when-let ((commit (straight-vc-get-commit type local-repo)))
                   (push (cons local-repo commit) versions-alist))))))
          (setq versions-alist
                (cl-sort versions-alist #'string-lessp :key #'car))
          (make-directory (file-name-directory lockfile-path) 'parents)
          (with-temp-file lockfile-path
            (insert
             (format
              ;; When the recipe format is updated, this version
              ;; keyword will be updated. It tells install.el which
              ;; version of straight.el to use to interpret the recipe
              ;; that must be used to clone straight.el itself. I'm
              ;; using the Greek alphabet, for diversity (and
              ;; because using consecutive integers would make it
              ;; confusing when somebody else made a fork of the
              ;; project and needed to fork the version sequence as
              ;; well).
              ;;
              ;; The version keyword comes after the versions alist so
              ;; that you can ignore it if you don't need it.
              "(%s)\n:beta\n"
              (mapconcat
               (apply-partially #'format "%S")
               versions-alist
               "\n "))))
          (straight--output "Wrote %s" lockfile-path))))))

;;;###autoload
(defun straight-thaw-versions ()
  "Read version lockfiles and restore package versions to those listed."
  (interactive)
  (let ((versions-alist (straight--lockfile-read-all)))
    (straight--map-repos-interactively
     (lambda (package)
       (let ((recipe (gethash package straight--recipe-cache)))
         (when (straight--repository-is-available-p recipe)
           (straight--with-plist recipe
               (local-repo)
             ;; We can't use `alist-get' here because that uses
             ;; `eq', and our hash-table keys are strings.
             (when-let ((commit (cdr (assoc local-repo versions-alist))))
               (unless (straight-vc-commit-present-p recipe commit)
                 (straight-vc-fetch-from-remote recipe))
               (straight-vc-check-out-commit recipe commit)))))))))

;;;; Integration with other packages
;;;;; package.el "integration"

(defvaralias 'straight-ಠ_ಠ 'straight-enable-package-integration
  "The alias expresses how I feel about package.el sometimes.")

(defcustom straight-enable-package-integration t
  "Whether to enable \"integration\" with package.el.
This means that `package-enable-at-startup' is disabled, and
advices are put on `package--ensure-init-file' and
`package--save-selected-packages' to prevent package.el from
modifying the init-file."
  :type 'boolean)

;;;;;; Mode variables

(defvar straight-package--last-enable-at-startup t
  "Value of `package-enable-at-startup' at last mode toggle.")

;;;;;; Utility functions

(defun straight-package-advice-ensure-init-file ()
  "Prevent package.el from modifying the init-file.

This is an `:override' advice for `package--ensure-init-file'.")

(defun straight-package-advice-save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE.
But don't mess with the init-file.

This is an `:override' advice for `package--save-selected-packages'."
  (when value
    (setq package-selected-packages value)))

;;;;;; Mode definition

(define-minor-mode straight-package-neutering-mode
  "Minor mode to neuter package.el by inhibiting some offensive features.

This mode is enabled or disabled automatically when straight.el
is loaded, according to the value of
`straight-enable-package-integration'."
  :global t
  :group 'straight
  (with-eval-after-load 'package
    (if straight-package-neutering-mode
        (progn
          (when (boundp 'package-enable-at-startup)
            (setq straight-package--last-enable-at-startup
                  package-enable-at-startup))
          (setq package-enable-at-startup nil)
          (advice-add #'package--ensure-init-file :override
                      #'straight-package-advice-ensure-init-file)
          (advice-add #'package--save-selected-packages :override
                      #'straight-package-advice-save-selected-packages))
      (setq package-enable-at-startup
            straight-package--last-enable-at-startup)
      (advice-remove #'package--ensure-init-file
                     #'straight-package-advice-ensure-init-file)
      (advice-remove #'package--save-selected-packages
                     #'straight-package-advice-save-selected-packages))))

(defalias 'straight-ಠ_ಠ-mode #'straight-package-neutering-mode
  "The alias expresses how I feel about package.el sometimes.")

;;;;; use-package integration

(defcustom straight-enable-use-package-integration t
  "Whether to enable integration with `use-package'.
See `straight-use-package-version' for details."
  :type 'boolean)

;;;;;; Mode variables

(defcustom straight-use-package-version 'straight
  "Symbol identifying the version of `use-package' in use.

Value `ensure' is for older versions of `use-package' (before
commit 418e90c3 on Dec. 3, 2017). With this value, specifying a
non-nil value for `:ensure' in a `use-package' form causes the
package to be installed using straight.el. The value for
`:ensure' can be t, meaning use the feature name as the package
name; a symbol, meaning install that package; a plist, meaning
use the feature name as the package name and append the plist to
form a custom recipe; and a list whose cdr is a plist, meaning
use it as the recipe. If `:ensure' is t and you provide a non-nil
value for `:recipe', then that value is used instead. You can
cause `:ensure' to receive a value of t unless otherwise
specified by setting `use-package-always-ensure' to a non-nil
value.

Value `straight' is for the current version of `use-package'.
With this value, specifying a non-nil value for `:straight' in a
`use-package' form causes the package to be installed using
straight.el. The value for `:straight' can be t, which is
replaced with the feature name; a symbol, which is used as is; a
plist, which is prepended with the feature name; or a list whose
cdr is a plist, which is used as is."
  :type '(choice
          (const :tag "Classic (uses `:ensure' for all package managers)"
                 ensure)
          (const :tag "Modern (uses `:package', `:straight', etc.)" straight)))

(defvar straight-use-package--last-version nil
  "Value of `straight-use-package-version' at last mode toggle.")

(defcustom straight-use-package-by-default nil
  "Non-nil means install packages by default in `use-package' forms.
This only works when `straight-use-package-version' is
`straight'. When `straight-use-package-version' is `ensure', use
`use-package-always-ensure' instead."
  :type 'boolean)

;;;;;; Utility functions

(defun straight-use-package-ensure-function
    (name ensure state &optional context)
  "Value for `use-package-ensure-function' to use straight.el.
This is used for integration with `use-package' when
`straight-use-package-version' is `ensure'. NAME is a symbol
naming the feature for the `use-package' form in question; ENSURE
is the form passed to the `:ensure' keyword (a symbol or list);
STATE is the internal plist used during `use-package' expansion.
In older versions of `use-package' (before commit 93bf693b on
Dec. 1, 2017), an additional argument CONTEXT was passed. This
argument was used to identify whether package installation should
happen or not, and whether the user should be prompted before
doing it. When CONTEXT is not passed, straight.el has no way of
deciding and instead just installs the package unconditionally."
  (when ensure
    (straight-use-package
     (or (and (not (eq ensure t)) ensure)
         (plist-get state :recipe)
         name)
     (lambda (package available)
       (cond
        ;; If available, go ahead.
        (available nil)
        ;; When doing lazy installation, don't clone if not
        ;; available.
        ((eq context :pre-ensure) t)
        ;; In cases where installation should be automatic, do
        ;; it.
        ((memq context '(:byte-compile :ensure
                                       :config :pre-ensure
                                       :interactive nil))
         nil)
        ;; Otherwise, prompt the user.
        (t (not (y-or-n-p (format "Install package %S? " package)))))))))

(defun straight-use-package-pre-ensure-function
    (name ensure state)
  "Value for `use-package-pre-ensure-function' to use straight.el.
The meanings of NAME, ENSURE, and STATE are the same as in
`straight-use-package-ensure-function'."
  (straight-use-package-ensure-function
   name ensure state :pre-ensure))

(defvar straight-use-package--last-ensure-function nil
  "Value of `use-package-ensure-function' at last mode toggle.")

(defvar straight-use-package--last-pre-ensure-function nil
  "Value of `use-package-pre-ensure-function' at last mode toggle.")

(defun straight-use-package--ensure-normalizer
    (name-symbol keyword args)
  "Normalizer for `:ensure' and `:recipe' in `use-package' forms.
NAME-SYMBOL, KEYWORD, and ARGS are explained by the `use-package'
documentation."
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label arg)
      (if (keywordp (car-safe arg))
          (cons name-symbol arg)
        arg))))

(defun straight-use-package--recipe-handler
    (name _keyword recipe rest state)
  "Handler for `:recipe' in `use-package' forms.
NAME, KEYWORD, RECIPE, REST, and STATE are explained by the
`use-package' documentation."
  (use-package-process-keywords
    name rest (plist-put state :recipe recipe)))

(defun straight-use-package--straight-normalizer
    (name-symbol keyword args)
  "Normalizer for `:straight' in `use-package' forms.
NAME-SYMBOL, KEYWORD, and ARGS are explained by the `use-package'
documentation."
  (let ((parsed-args nil))
    (dolist (arg args (reverse parsed-args))
      (cond
       ((null arg) (setq parsed-args nil))
       ((eq arg t) (push name-symbol parsed-args))
       ((symbolp arg) (push arg parsed-args))
       ((not (listp arg))
        (use-package-error ":straight wants a symbol or list"))
       ;; lists
       (t (let ((c (car arg)))
            (cond
             ((keywordp c)
              ;; recipe without package name
              (push (cons name-symbol arg) parsed-args))
             ((cl-some #'keywordp arg)
              ;; assume it's a recipe
              (push arg parsed-args))
             ;; recipe with only a package name
             ((and (symbolp c) (not (member c '(\` quote))))
              (push arg parsed-args))
             ;; normalize backquoted/quoted arg and preserve quote
             (t (push (list c (car (straight-use-package--straight-normalizer
                                    name-symbol keyword (cdr arg))))
                      parsed-args)))))))))

(defun straight-use-package--straight-handler
    (name _keyword args rest state)
  "Handler for `:straight' in `use-package' forms.
NAME, KEYWORD, ARGS, REST, and STATE are explained by the
`use-package' documentation."
  ;; Disable `:ensure' when `:straight' is present. This part only
  ;; works when `:straight' is processed before `:ensure'. See below
  ;; for the other case.
  ;;
  ;; See <https://github.com/raxod502/straight.el/issues/425>.
  (when args (straight--remq rest '(:ensure)))
  (append
   (mapcar (lambda (arg)
             `(straight-use-package
               ,(if (member (car-safe arg) '(\` quote))
                    arg
                  ;; The following comparison against 't' is an
                  ;; unfortunate hack because `use-package-defaults'
                  ;; currently operates on the post-normalization
                  ;; values, rather than the pre-normalization ones.
                  (macroexpand `(quote ,(if (eq arg t) name arg))))))
           args)
   (use-package-process-keywords name rest state)))

(defun straight-use-package--ensure-handler-advice
    (handler name keyword args rest state)
  "Advice for `:ensure' handler in `use-package' forms.
HANDLER is the original handler function. NAME, KEYWORD, ARGS,
REST, and STATE are explained by the `use-package' documentation.

Disables `:ensure' when `:straight' is present. This part only
works when `:ensure' is processed before `:straight'. See above
for the other case."
  (if (plist-get rest :straight)
      (use-package-process-keywords name rest state)
    (funcall handler name keyword args rest state)))

;;;;;; Mode definition

(define-minor-mode straight-use-package-mode
  "Minor mode to enable `use-package' support in straight.el.
The behavior is controlled by variables
`straight-use-package-version' and
`straight-use-package-by-default'. If these variables are
changed, you must toggle the mode function to update the
integration.

This mode is enabled or disabled automatically when straight.el
is loaded, according to the value of
`straight-enable-use-package-integration'."
  :global t
  :group 'straight
  (pcase straight-use-package--last-version
    ('ensure
     (with-eval-after-load 'use-package
       (when (and (boundp 'use-package-ensure-function)
                  (eq use-package-ensure-function
                      #'straight-use-package-ensure-function)
                  straight-use-package--last-ensure-function)
         (setq use-package-ensure-function
               straight-use-package--last-ensure-function))
       (when (and (boundp 'use-package-pre-ensure-function)
                  (eq use-package-pre-ensure-function
                      #'straight-use-package-pre-ensure-function)
                  straight-use-package--last-pre-ensure-function)
         (setq use-package-pre-ensure-function
               straight-use-package--last-pre-ensure-function))
       (when (and (boundp 'use-package-keywords)
                  (listp use-package-keywords))
         (setq use-package-keywords (remq :recipe use-package-keywords)))
       (fmakunbound 'use-package-normalize/:recipe)
       (fmakunbound 'use-package-handler/:recipe)
       (advice-remove #'use-package-normalize/:ensure
                      #'straight-use-package--ensure-normalizer)))
    ('straight
     (with-eval-after-load 'use-package-core
       (when (and (boundp 'use-package-keywords)
                  (listp use-package-keywords))
         (setq use-package-keywords (remq :straight use-package-keywords)))
       (fmakunbound 'use-package-normalize/:straight)
       (fmakunbound 'use-package-handler/:straight)
       (advice-remove #'use-package-handler/:ensure
                      #'straight-use-package--ensure-handler-advice)
       (when (and (boundp 'use-package-defaults)
                  (listp use-package-defaults))
         (setq use-package-defaults
               (assq-delete-all :straight use-package-defaults))))))
  (setq straight-use-package--last-version nil)
  (when straight-use-package-mode
    (setq straight-use-package--last-version straight-use-package-version)
    (pcase straight-use-package-version
      ('ensure
       (with-eval-after-load 'use-package
         (when (boundp 'use-package-ensure-function)
           (setq straight-use-package--last-ensure-function
                 use-package-ensure-function))
         (setq use-package-ensure-function
               #'straight-use-package-ensure-function)
         (when (boundp 'use-package-pre-ensure-function)
           (setq straight-use-package--last-pre-ensure-function
                 use-package-pre-ensure-function))
         (setq use-package-pre-ensure-function
               #'straight-use-package-pre-ensure-function)
         (when (and (boundp 'use-package-keywords)
                    (listp use-package-keywords)
                    (memq :ensure use-package-keywords))
           (unless (memq :recipe use-package-keywords)
             (setq use-package-keywords
                   (let* ((pos (cl-position :ensure use-package-keywords))
                          (head (cl-subseq use-package-keywords 0 pos))
                          (tail (cl-subseq use-package-keywords pos)))
                     (append head (list :recipe) tail)))))
         (defalias 'use-package-normalize/:recipe
           #'straight-use-package--ensure-normalizer)
         (defalias 'use-package-handler/:recipe
           #'straight-use-package--recipe-handler)
         (advice-add #'use-package-normalize/:ensure :override
                     #'straight-use-package--ensure-normalizer)))
      ('straight
       (with-eval-after-load 'use-package-core
         (when (and (boundp 'use-package-keywords)
                    (listp use-package-keywords))
           (push :straight use-package-keywords))
         (defalias 'use-package-normalize/:straight
           #'straight-use-package--straight-normalizer)
         (defalias 'use-package-handler/:straight
           #'straight-use-package--straight-handler)
         (advice-add #'use-package-handler/:ensure :around
                     #'straight-use-package--ensure-handler-advice)
         (when (and (boundp 'use-package-defaults)
                    (listp use-package-defaults))
           (setq use-package-defaults (straight--alist-set
                                       :straight
                                       '('(t) straight-use-package-by-default)
                                       use-package-defaults
                                       'symbol))))))))

;;;;; Flycheck integration

(defcustom straight-fix-flycheck nil
  "If non-nil, install a workaround for a problem with Flycheck.
See <https://github.com/raxod502/straight.el/issues/508> for
discussion.

This variable must be set before straight.el is loaded (or
re-loaded) in order to take effect."
  :type 'boolean)

(defvar-local straight--flycheck-in-place-disabled t
  "If non-nil, inhibit in-place checkers in Flycheck.
This variable is toggled to nil after the first modification in a
buffer.")

(defun straight--flycheck-in-place-reenable ()
  "Re-enable in-place checkers in Flycheck."
  (setq straight--flycheck-in-place-disabled nil))

(defun straight--flycheck-in-place-inhibit (func checker)
  "Inhibit in-place Elisp checkers in Flycheck from running automatically.
You can still run them manually, and automatic checking will be
re-enabled after you modify the buffer.

This is an `:around' advice for
`flycheck-start-current-syntax-check'. FUNC and CHECKER are as in
any `:around' advice."
  (if (and
       straight--flycheck-in-place-disabled
       (memq
        'source-inplace
        (flycheck-checker-get checker 'command)))
      (when-let ((next-checker (flycheck-get-next-checker-for-buffer checker)))
        (flycheck-start-current-syntax-check next-checker))
    (funcall func checker)))

(if straight-fix-flycheck
    (progn
      (add-hook 'first-change-hook #'straight--flycheck-in-place-reenable)
      (advice-add 'flycheck-start-current-syntax-check :around
                  #'straight--flycheck-in-place-inhibit))
  (remove-hook 'first-change-hook #'straight--flycheck-in-place-reenable)
  (advice-remove 'flycheck-start-current-syntax-check
                 #'straight--flycheck-in-place-inhibit))

;;;;; Reporting Bugs

(defun straight-version (&optional message)
  "Return straight.el version.
Interactively, or when MESSAGE is non-nil, show in the echo area."
  (interactive)
  (let* ((library (locate-library "straight.el"))
         (default-directory (file-name-directory (file-truename library)))
         (declared (with-temp-buffer
                     (insert-file-contents-literally library)
                     (goto-char (point-min))
                     (save-match-data
                       (when (re-search-forward
                              "\\(?:;; Version: \\([^z-a]*?$\\)\\)"
                              nil 'no-error)
                         (substring-no-properties (match-string 1))))))
         (gitinfo
          (straight--process-with-result
              (straight--process-run "git" "show" "-s" "--format=%d %h %cs")
            (if success
                (string-trim (concat stdout stderr))
              (format "Uknown version. See %s" straight-process-buffer))))
         (version (format "%s %s" declared gitinfo)))
    (if (or message (called-interactively-p 'interactive))
        (message "%s" version)
      version)))

(defvar straight-bug-report--bootstrap
  '((defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el"
                             user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             (concat "https://raw.githubusercontent.com/"
                     "raxod502/straight.el/develop/install.el")
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    (condition-case nil
        (message "Test run with version: %s" (straight-version))
      (error nil)))
  "Static bootstrap portion of bug report metaprogram.")

(defvar straight-bug-report--default-args '("-Q" "--eval")
  "Args that are passed to the the Emacs executable when testing.")

(defvar straight-bug-report--process-buffer "*straight-bug-report-process*"
  "Name of the bug report subprocess buffer.")

(defvar straight-bug-report--setup
  '((setq straight-repository-branch "develop"))
  "Static setup portion of bug report metaprogram.")

(defun straight-bug-report--format (&rest preamble)
  "Format output of `straight-bug-report--process-buffer' as markdown.
If PREAMBLE is non-nil, it is inserted after the instructions."
  (with-current-buffer straight-bug-report--process-buffer
    (let ((output (buffer-string)))
      (erase-buffer)
      (when (fboundp 'markdown-mode) (markdown-mode))
      (insert
       (mapconcat
        (lambda (el) (apply #'format el))
        `(("<!-- copy entire buffer output and paste in an issue at:")
          ("https://github.com/raxod502/straight.el/issues/new/choose -->")
          ,@(when preamble
              `(("<details><summary>Test Case</summary>")
                ("\n```emacs-lisp")
                ("%s" ,@preamble)
                ("```")
                ("</details>\n")))
          ,(list (format-time-string "- Test run at: `%Y-%m-%d %H:%M:%S`"))
          ("- system-type: `%s`" ,system-type)
          ("- straight-version: `%s`" ,(straight-version))
          ("- emacs-version: `%s`" ,(emacs-version))
          ("\n<details><summary>Output</summary>")
          ("\n```emacs-lisp")
          ("%s" ,output)
          ("```")
          ("</details>"))
        "\n")))))

(defun straight-bug-report--report-form (form)
  "Convert elisp FORM into formatted string."
  (let* ((print-level nil)
         (print-length nil)
         (string (mapconcat
                  (lambda (el)
                    (concat (when (and el (listp el)) "\n")
                            (pp-to-string el)
                            (unless (keywordp el) "\n")))
                  form " ")))
    (with-temp-buffer
      (insert "(" string ")")
      (goto-char (point-min))
      ;; Replace dangling parens.
      (save-excursion
        (while (re-search-forward "\\(?:\n[[:space:]]*)\\)" nil 'no-error)
          (replace-match ")")))
      ;; Remove empty lines.
      (flush-lines "\\(?:^[[:space:]]*$\\)")
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun straight-bug-report-package-info ()
  "Return info for each built package.
Info is a plist of form:
  (:package PACKAGE :source SOURCE :version VERSION)"
  (mapcar
   (lambda (cell)
     (let* ((package (car cell))
            (repo (cdr cell))
            (source (straight-recipe-source package))
            (version
             (when repo
               (let ((default-directory (straight--repos-dir repo)))
                 (format "%s %s"
                         (straight-vc-git--local-branch "HEAD")
                         (straight--process-output
                          "git" "show" "-s" "--format=%h %cs"))))))
       (append
        (list :package package)
        (when source (list :source source))
        (when version (list :version version)))))
   (let ((cells '()))
     (maphash
      (lambda (key val)
        (setq cells (push (cons key (plist-get (nth 2 val) :local-repo))
                          cells)))
      straight--build-cache)
     (nreverse cells))))

(defun straight-bug-report--format-package-info (info)
  "Return Formatted `straight-bug-report-package-info' INFO."
  (mapconcat (lambda (info)
               (straight--with-plist info
                   (package (source "n/a") (version "n/a"))
                 (format "%-25S %-20s %s" package source version)))
             info "\n"))

;;;###autoload
(defmacro straight-bug-report (&rest args)
  "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args."
  (declare (indent 0))
  (unless lexical-binding
    (user-error "Lexical binding required for straight-bug-report"))
  (let* ((preserve-files    (make-symbol "preserve-files"))
         (temp-emacs-dir    (make-symbol "temp-emacs-dir"))
         (interactive       (make-symbol "interactive"))
         (emacs-args-symbol (make-symbol "emacs-args"))
         (emacs-executable  (make-symbol "emacs-executable"))
         (raw               (make-symbol "raw"))
         (report            (make-symbol "report"))
         (test              (make-symbol "test"))
         ;; Collect args in alist of the form: ((KEYWORD (BODY))...)
         ;; This allows us to have variadic keyword arguments.
         ;; Each keyword is associated with the args that follow it until
         ;; the next keyword.
         (keywords
          (let (forms target)
            (dolist (arg args (mapcar
                               (lambda (pair)
                                 ;; Return body forms in declared order
                                 (setcdr pair (nreverse (cdr pair))) pair)
                               forms))
              (if (not (keywordp arg))
                  (push arg (cdr (assq target forms)))
                (setq target arg)
                (unless (assq target forms)
                  (push (list target) forms))))))
         ;; Construct bug-report form
         (reportform (straight-bug-report--report-form
                      (append '(straight-bug-report) args)))
         (temp-dir (if-let ((dir (car (alist-get :user-dir keywords))))
                       (expand-file-name dir temporary-file-directory)
                     (make-temp-file "straight.el-test-" 'directory)))
         (executable (or (car (alist-get :executable keywords))
                         (concat invocation-directory invocation-name)))
         ;; Construct metaprogram to be evaled by subprocess
         ;; Convert the keywords and their args into a proper plist.
         (pargs (let ((plist '()))
                  (dolist (pair keywords plist)
                    (let* ((c (car pair))
                           (variadic
                            (member c '(:post-bootstrap :pre-bootstrap))))
                      (setq plist (plist-put plist c (if variadic
                                                         (append '(progn)
                                                                 (cdr pair))
                                                       (cadr pair))))))
                  ;; Add full path of user-dir.
                  (setq plist (plist-put plist :executable executable))
                  (setq plist (plist-put plist :user-dir temp-dir))))
         (program
          (let ((print-level nil)
                (print-length nil))
            (pp-to-string
             ;; The top-level `let' is an intentional local
             ;; variable binding. We want users of
             ;; `straight-bug-report' to have access to their
             ;; args within :pre/:post-bootstrap programs. Since
             ;; we are binding with the package namespace, this
             ;; should not overwrite other user bindings.
             (append
              '(with-demoted-errors "Error: %S")
              `(,(append
                  `(let ((straight-bug-report-args ',pargs)))
                  `((setq user-emacs-directory ,temp-dir))
                  straight-bug-report--setup
                  (alist-get :pre-bootstrap keywords)
                  straight-bug-report--bootstrap
                  `(,(append
                      '(unwind-protect)
                      `((progn ,@(alist-get :post-bootstrap keywords)))
                      '((message
                         "Packages:\n%s\n"
                         (straight-bug-report--format-package-info
                          (straight-bug-report-package-info)))))))))))))
    `(let* ((,preserve-files    ,(car (alist-get :preserve keywords)))
            (,interactive       ,(car (alist-get :interactive keywords)))
            (,emacs-executable  ,executable)
            (,emacs-args-symbol (append (unless ,interactive '("--batch"))
                                        ',straight-bug-report--default-args))
            (,raw               ,(car (alist-get :raw keywords)))
            (,test              ,program)
            (,report            ,reportform)
            (,temp-emacs-dir    ,temp-dir))
       ;; Reset process buffer.
       (with-current-buffer (get-buffer-create
                             straight-bug-report--process-buffer)
         (fundamental-mode)
         (erase-buffer))
       (make-process
        :name straight-bug-report--process-buffer
        :buffer straight-bug-report--process-buffer
        :command `(,,emacs-executable ,@,emacs-args-symbol ,,test)
        :sentinel (lambda (_process _event)
                    (unless ,interactive
                      (unless ,raw (straight-bug-report--format ,report))
                      (run-with-idle-timer
                       1 nil (lambda ()
                               (switch-to-buffer-other-window
                                straight-bug-report--process-buffer))))
                    (unless ,preserve-files
                      (when (file-exists-p ,temp-emacs-dir)
                        (delete-directory ,temp-emacs-dir 'recursive)))))
       (message "Testing straight.el in directory: %s"
                ,temp-emacs-dir))))

;;;; Dependency Info

;;;###autoload
(defun straight-dependencies (&optional package)
  "Return a list of PACKAGE's dependencies."
  (interactive (list
                (straight--select-package
                 "Dependencies of"
                 ;; Only offer candidates which have dependencies.
                 (lambda (recipe)
                   (and (straight--installed-p recipe)
                        (cl-remove-if (lambda (p) (string= p "emacs"))
                                      (nth 1 (gethash
                                              (plist-get recipe :package)
                                              straight--build-cache))))))))
  (let ((dependencies
         (mapcar (lambda (dependency)
                   (if-let ((transitive (straight-dependencies dependency)))
                       (append (list dependency) transitive)
                     dependency))
                 (cl-remove-if
                  (lambda (p) (string= p "emacs"))
                  (nth 1 (gethash package straight--build-cache))))))
    (if (called-interactively-p 'interactive)
        (message "Dependencies of %S: %S" package dependencies)
      dependencies)))

(defun straight--dependencies ()
  "Return a list of dependencies from `straight--build-cache'."
  (let ((dependencies))
    (maphash (lambda (_ val) (push (nth 1 val) dependencies))
             straight--build-cache)
    (cl-remove-if (lambda (p) (string= p "emacs"))
                  (delete-dups
                   (delq nil
                         (cl-reduce #'append dependencies))))))

;;;###autoload
(defun straight-dependents (&optional package)
  "Return a list PACKAGE's dependents."
  (interactive
   (list (straight--select-package
          "Dependents of"
          ;; Only offer cached dependencies.
          (lambda (recipe) (member (plist-get recipe :package)
                                   (straight--dependencies))))))
  (let (dependents)
    (maphash (lambda (key val)
               (when (member package (nth 1 val))
                 (push (if-let ((transitive (straight-dependents key)))
                           (append (list key) transitive)
                         key)
                       dependents)))
             straight--build-cache)
    (if (called-interactively-p 'interactive)
        (message "Dependents of %S: %S" package dependents)
      (nreverse dependents))))

;;;; Closing remarks

(provide 'straight)

;; Local Variables:
;; checkdoc-symbol-words: ("byte-compile" "top-level")
;; checkdoc-verb-check-experimental-flag: nil
;; indent-tabs-mode: nil
;; End:

;;; straight.el ends here

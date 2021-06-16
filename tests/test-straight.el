;;; straight.el --- Tests for straight.el -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'straight)

(defvar straight-test-default-recipe '( :package "package"
                                        :host github :type git
                                        :repo "upstream/repo"))

;;adapted from json.el's json-alist-p
(defun straight--alist-p (list &optional restriction)
  "Return non-nil if LIST is an alist.
RESTRICTION is an optionaly unary function which tests each element of
the list. If any element returns nil when passed to RESTRICTION,
return nil."
  (while (and (consp (car-safe list))
              (atom (caar list))
              (funcall (if restriction restriction #'identity)
                       (car-safe list))
              (setq list (cdr list))))
  (null list))

(defun straight--hash-equal (a b)
  "Return t if hash A and B are equal, else nil."
  (and (= (hash-table-count a)
          (hash-table-count b))
       (catch 'flag (maphash (lambda (x y)
                               (or (equal (gethash x b) y)
                                   (throw 'flag nil)))
                             a)
              t)))

(describe "defusctom :type specifications"
  (describe "straight-allow-recipe-inheritance"
    (it "defaults to a boolean"
      (expect (booleanp straight-allow-recipe-inheritance) :to-be t)))
  (describe "straight-arrow"
    (it "defaults to a string"
      (expect (stringp straight-arrow) :to-be t)))
  (describe "straight-base-dir"
    (it "defaults to a string"
      (expect (stringp straight-base-dir) :to-be t)))
  (describe "straight-build-cache-fixed-name"
    (it "defaults to nil or a string"
      (expect (or (null straight-build-cache-fixed-name)
                  (stringp straight-build-cache-fixed-name))
              :to-be t)))
  (describe "straight-build-dir"
    (it "defaults to a string"
      (expect (stringp straight-build-dir)) :to-be t))
  (describe "straight-built-in-pseudo-packages"
    (it "defaults to a list of symbols"
      (expect (and (listp straight-built-in-pseudo-packages)
                   (cl-every #'symbolp straight-built-in-pseudo-packages)
                   :to-be t))))
  (describe "straight-byte-compilation-buffer"
    (it "defaults to a string or nil"
      (expect (or (stringp straight-byte-compilation-buffer)
                  (null straight-byte-compilation-buffer))
              :to-be t)))
  (describe "straight-cache-autoloads"
    (it "defaults to a boolean"
      (expect (booleanp straight-cache-autoloads) :to-be t)))
  (describe "straight-check-for-modifications"
    (it "defaults to a list of any of the following symbols:
    - find-at-startup
    - find-when-checking
    - check-on-save
    - watch-files"
      (expect (and (listp straight-check-for-modifications)
                   (null (cl-remove-if (lambda (el)
                                         (member el '(find-at-startup
                                                      find-when-checking
                                                      check-on-save
                                                      watch-files)))
                                       straight-check-for-modifications)))
              :to-be t)))
  (describe "straight-current-profile"
    (it "defaults to a symbol"
      (expect (symbolp straight-current-profile) :to-be t)))
  (describe "straight-default-vc"
    (it "defaults to a symbol"
      (expect (symbolp straight-default-vc) :to-be t)))
  (describe "straight-disable-autoloads"
    (it "defaults to a boolean"
      (expect (booleanp straight-disable-autoloads) :to-be t)))
  (describe "straight-disable-compile"
    (it "defaults to a boolean"
      (expect (booleanp straight-disable-compile) :to-be t)))
  (describe "straight-disable-info"
    (it "defaults to a boolean"
      (expect (booleanp straight-disable-info) :to-be t)))
  (describe "straight-disable-native-compile"
    (it "defaults to a boolean"
      (expect (booleanp straight-disable-native-compile) :to-be t)))
  (describe "straight-enable-package-integration"
    (it "defaults to a boolean"
      (expect (booleanp straight-enable-package-integration) :to-be t)))
  (describe "straight-enable-use-package-integration"
    (it "defaults to a boolean"
      (expect (booleanp straight-enable-use-package-integration) :to-be t)))
  (describe "straight-find-executable"
    (it "defaults to a string"
      (expect (stringp straight-find-executable) :to-be t)))
  (describe "straight-find-flavor"
    (it "defaults to :guess or a list of symbols"
      (expect (or (equal straight-find-flavor :guess)
                  (listp straight-find-flavor))
              :to-be t)))
  (describe "straight-fix-flycheck"
    (it "defaults to a boolean"
      (expect (booleanp straight-fix-flycheck) :to-be t)))
  (describe "straight-host-usernames"
    (it "defaults to nil or an alist of form (SYMBOL . STRING)"
      (expect (or
               (straight--alist-p straight-host-usernames
                                  (lambda (cell)
                                    (and (symbolp (car cell))
                                         (stringp (cdr cell)))))
               (null straight-host-usernames))
              :to-be t)))
  (describe "straight-install-info-executable"
    (it "defaults to a string"
      (expect (stringp straight-install-info-executable) :to-be t)))
  (describe "straight-makeinfo-executable"
    (it "defaults to a string"
      (expect (stringp straight-makeinfo-executable) :to-be t)))
  (describe "straight-process-buffer"
    (it "defaults to a string"
      (expect (stringp straight-process-buffer) :to-be t)))
  (describe "straight-profiles"
    (it "defaults to an alist of form (SYMBOL . STRING)"
      (expect (straight--alist-p straight-profiles
                                 (lambda (cell) (and (symbolp (car cell))
                                                     (stringp (cdr cell)))))
              :to-be t)))
  (describe "straight-recipe-overrides"
    (it "defaults to an alist of form (SYMBOL . STRING)"
      (expect (or
               (straight--alist-p
                straight-recipe-overrides
                (lambda (cell)
                  (and (symbolp (car cell))
                       (straight--alist-p
                        (cdr cell)
                        (lambda (cell)
                          (and (symbolp (car cell))
                               ;;@INCOMPLETE: we could be more precise
                               ;; here if we had a `recipe-p` predicate.
                               (listp (cdr cell))))))))
               (nullp straight-recipe-overrides)))
      :to-be t))
  (describe "straight-recipe-repositories"
    (it "defaults to a list of symbols or nil"
      (expect (or (null straight-recipe-repositories)
                  (null (cl-remove-if #'symbolp straight-recipe-repositories)))
              :to-be t)))
  (describe "straight-recipes-emacsmirror-use-mirror"
    (it "defaults to a boolean"
      (expect (booleanp straight-recipes-emacsmirror-use-mirror) :to-be t)))
  (describe "straight-recipes-gnu-elpa-ignored-packages"
    (it "defaults to a list of symbols or nil"
      (expect
       (or (null straight-recipes-gnu-elpa-ignored-packages)
           (null (cl-remove-if #'symbolp
                               straight-recipes-gnu-elpa-ignored-packages)))
       :to-be t)))
  (describe "straight-recipes-gnu-elpa-url"
    (it "defaults to a string"
      (expect (stringp straight-recipes-gnu-elpa-url) :to-be t)))
  (describe "straight-recipes-gnu-elpa-use-mirror"
    (it "defaults to a boolean"
      (expect (booleanp straight-recipes-gnu-elpa-use-mirror) :to-be t)))
  (describe "straight-repository-branch"
    (it "defaults to a string"
      (expect (stringp straight-repository-branch) :to-be t)))
  (describe "straight-repository-user"
    (it "defaults to a string"
      (expect (stringp straight-repository-user) :to-be t)))
  (describe "straight-use-package-by-default"
    (it "defaults to a boolean"
      (expect (booleanp straight-use-package-by-default) :to-be t)))
  (describe "straight-use-package-version"
    (it "defaults to a symbol (straight or ensure)"
      (expect (member straight-use-package-version
                      '(straight ensure))
              :not :to-be nil)))
  (describe "straight-use-symlinks"
    (it "defaults to a boolean"
      (expect (booleanp straight-use-symlinks) :to-be t)))
  (describe "straight-vc-git-auto-fast-forward"
    (it "defaults to a boolean"
      (expect (booleanp straight-vc-git-auto-fast-forward) :to-be t)))
  (describe "straight-vc-git-default-clone-depth"
    (it "defaults to a symbol, interger, or list"
      (expect (or
               (eq 'full straight-vc-git-default-clone-depth)
               (integerp straight-vc-git-default-clone-depth)
               (and (listp straight-vc-git-default-clone-depth)
                    (member 'single-branch straight-vc-git-default-clone-depth)
                    (cl-some (lambda (el) (or (booleanp el) (integerp el)))
                             straight-vc-git-default-clone-depth))))
      :to-be t))
  (describe "straight-vc-git-default-fork-name"
    (it "defaults to a string"
      (expect (stringp straight-vc-git-default-fork-name) :to-be t)))
  (describe "straight-vc-git-default-protocol"
    (it "defaults to either the symbol `ssh` or `https`"
      (expect (member straight-vc-git-default-protocol '(ssh https))
              :not :to-be nil)))
  (describe "straight-vc-git-default-remote-name"
    (it "defaults to a string"
      (expect (stringp straight-vc-git-default-remote-name) :to-be t)))
  (describe "straight-vc-git-force-protocol"
    (it "defaults to a boolean"
      (expect (booleanp straight-vc-git-force-protocol) :to-be t)))
  (describe "straight-watcher-process-buffer"
    (it "defaults to a string"
      (expect (stringp straight-watcher-process-buffer) :to-be t))))

(describe "straight--alist-set"
  (describe "(key val alist)"
    (it "adds (KEY . VAL) to front of ALIST"
      (expect (straight--alist-set "a" 'b  '(("c" . d)))
              :to-equal '(("a" . b) ("c" . d))))
    (it "sets first KEY match cdr"
      (expect (straight--alist-set "a" 'd  '(("a" . b) ("a" . c)))
              :to-equal '(("a" . d) ("a" . c)))))
  (describe "(key val alist symbol)"
    (it "tests with eq when SYMBOL non-nil"
      (expect (straight--alist-set "a" 'a  '((a . b) (a . c)) 'symbol)
              :to-equal '(("a" . a) (a . b) (a . c))))))

(describe "straight--with-plist"
  (describe "(plist props body)"
    (it "correctly binds PROPS and executes BODY"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--with-plist plist (a b ((:c d)) (e 2))
                  (+ a b d e)))
              :to-equal 8))))

(describe "straight--put"
  (describe "(plist prop value)"
    (it "sets PROP to VALUE on PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--put plist :a 0)
                plist)
              :to-equal '(:a 0 :b 2 :c 3)))))

(describe "straight--remq"
  (describe "(plist props)"
    (it "removes PROPS from PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--remq plist '(:a :b))
                plist)
              :to-equal '(:c 3)))
    (it "compares PROPS with `eq'"
      (expect (let ((plist '("test" "PASS")))
                ;;this should not match the key
                (straight--remq plist '("test"))
                plist)
              :to-equal '("test" "PASS")))))

(describe "straight--plsit-get"
  (describe "(plist prop default)"
    (it "returns PROP from PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--plist-get plist :a nil))
              :to-equal 1))
    (it "uses DEFAULT if PROP not found on PLIST"
      (expect (let ((plist '(:a 1 :b 2 :c 3)))
                (straight--plist-get plist :d 2))
              :to-equal 2))))

(describe "straight--insert"
  (describe "(n key value table)"
    (it "associates Nth index in KEY with VALUE"
      (expect
       (let ((table
              #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--hash-equal
          (straight--insert 0 "p2" "d2" table)
          #s( hash-table size 2 test equal rehash-size 1.5
              rehash-threshold 0.8125 data
              ("p1" ("d1") "p2" ("d2")))))
       :to-be t))
    (it "pads with nil if Nth entry does not exist"
      (expect
       (let ((table #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--hash-equal
          (straight--insert 2 "p1" "d2" table)
          #s( hash-table size 2 test equal rehash-size 1.5
              rehash-threshold 0.8125 data
              ("p1" ("d1" nil "d2"))))))
      :to-be t)))

(describe "straight--checkhash"
  (describe "(key table)"
    (it "Retruns non-nil if KEY is present in hash TABLE"
      (expect
       (let ((table #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--checkhash "p1" table))
       :to-be t))
    (it "Retruns nil if KEY is not present in hash TABLE"
      (expect
       (let ((table #s(hash-table size 1 test equal data ("p1" ("d1")))))
         (straight--checkhash "p2" table))
       :to-be nil)))
  (describe "(key table paranoid)"
    (it "ensures correctness for TABLE containing straight--not-present")))

(describe "straight-vc-git--fork-repo"
  (before-each (setq straight-host-usernames
                     '((github . "githubUser")
                       (gitlab . "gitlabUser")
                       (bitbucket . "bitbucketUser"))))
  (describe ":fork nil"
    (it "returns inherited upstream :repo"
      ;; explicit
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork nil))
              :to-equal "upstream/repo")
      ;; implicit
      (expect (straight-vc-git--fork-repo straight-test-default-recipe)
              :to-equal "upstream/repo")))
  (describe ":fork t"
    (it "returns \"straight-host-username/upstream-repo-name\""
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork t))
              :to-equal "githubUser/repo")))
  (describe ":fork string"
    (it "returns \"STRING/upstream-repo\" with suffixed or absent \"/\""
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "fork/"))
              :to-equal "fork/repo")
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "fork"))
              :to-equal "fork/repo"))
    (it "returns \"straight-host-username/STRING\" when prefixed with \"/\""
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "/rename"))
              :to-equal "githubUser/rename"))
    (it "returns \"STRING\" when \"/\" that is not a prefix or suffix"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe :fork "fork/rename"))
              :to-equal "fork/rename")))
  (describe ":fork plist"
    (it "looks up :fork PLIST's :host"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host bitbucket)))
              :to-equal "bitbucketUser/repo"))
    (it "gives precedence to :fork PLIST's :repo"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:repo "/rename")))
              :to-equal "githubUser/rename")
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host github :repo "user/")))
              :to-equal "user/repo")
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host gitlab :repo "full/override")))
              :to-equal "full/override")
      ;; https://github.com/raxod502/straight.el/issues/592
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:host nil :repo "/local/repo")))
              :to-equal "/local/repo"))
    (it "looks up the user when :fork PLIST does not provide a :repo"
      (expect (straight-vc-git--fork-repo
               (plist-put straight-test-default-recipe
                          :fork '(:branch "feature")))
              :to-equal "githubUser/repo"))))

(describe "straight--build-steps"
  (describe ":build nil"
    (it "Returns an empty list"
      (expect (straight--build-steps '(:package "test" :build nil))
              :to-be nil)))
  (describe ":build t"
    (it "Returns the default list of build step functions"
      (expect (straight--build-steps '(:package "test" :build t))
              :to-have-same-items-as '(straight--build-autoloads
                                       straight--build-compile
                                       straight--build-native-compile
                                       straight--build-info)))
    (it "Ignores straight-disable-SYMBOL options"
      (expect (let ((straight-disable-autoloads t)
                    (striaght-disable-compile t)
                    (straight-disable-native-compile t)
                    (straight-disable-info t))
                (straight--build-steps '(:package "test" :build t))
                :to-have-same-items-as '(straight--build-autoloads
                                         straight--build-compile
                                         straight--build-native-compile
                                         straight--build-info)))))
  (describe ":build (steps...)"
    (it "Returns straight--build-SYMBOL functions in declared order"
      (expect (straight--build-steps '(:package "test" :build (compile)))
              :to-equal '(straight--build-compile))
      (expect (straight--build-steps
               '(:package "test" :build (autoloads info)))
              :to-equal '(straight--build-autoloads straight--build-info))))
  (describe ":build (:not steps...)"
    (it "Returns the default steps less each step in the list"
      (expect (straight--build-steps
               '(:package "test" :build (:not compile)))
              :to-equal '(straight--build-autoloads
                          straight--build-native-compile
                          straight--build-info)))
    (it "Respects straight-disable-SYMBOL options"
      (expect (let ((straight-disable-info t))
                (straight--build-steps
                 '(:package "test" :build (:not compile))))
              :to-equal '(straight--build-autoloads
                          straight--build-native-compile))))
  (describe "no :build declared"
    (it "Returns the default steps"
      (expect (straight--build-steps '(:package "test"))
              :to-equal '(straight--build-autoloads
                          straight--build-compile
                          straight--build-native-compile
                          straight--build-info)))
    (it "Respects straight-disable-SYMBOL options"
      (expect (let ((straight-disable-info t))
                (straight--build-steps '(:package "test")))
              :to-equal '(straight--build-autoloads
                          straight--build-compile
                          straight--build-native-compile)))))

(provide 'straight-test)

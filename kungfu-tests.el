(require 'ert)
(load-file "./kungfu.el")
(require 'kungfu)


;;;; for mock test ;;; 
(defun my-fixture (body)
  (unwind-protect
      (progn [set up]
             (funcall body))
    [tear down]))
(ert-deftest my-fixture ()
  (my-fixture
   (lambda ()
     [test code])))

;;;;
;; (pp-to-string '(quote quote))          ; expected: "'quote"
;; (pp-to-string '((quote a) (quote b)))  ; expected: "('a 'b)\n"
;; (pp-to-string '('a 'b))                ; same as above

(ert-deftest pp-test-quote ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n")))

;;;;;;;;

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

;;;; for kungfu.el test ;;;

;; mock the function
(defun get-mark-content (buffername) (unwind-protect "Test Abc"))
(ert-deftest downcase-str ()
  (should (equal (downcase-str) "test abc") )) 
;; must start drb server for test, add `gem 'drbcli' to Gemfile`
(ert-deftest rb-underscore ()
  (should (equal (rb-underscore "AdddTaaaGaaa") "addd_taaa_gaaa") ))

(defun relace-region-str (str) str)
(defun get-point-keyword () "BdasdasYdasds")
(ert-deftest rb-underscore-words ()
  (should (equal (rb-underscore-words) "bdasdas_ydasds") ))

(ert-deftest rb-camelize ()
  (should (equal (rb-camelize "sdadsa_jkkjtrlkj_fds") "SdadsaJkkjtrlkjFds")) )

(ert-deftest ruby-parser ()
  (should
   (equal
    (ruby-parser "def index a; a + 1; end")
    "s(:defn, :index, s(:args, :a), s(:call, s(:lvar, :a), :+, s(:lit, 1)))") ))

(ert-deftest ruby-parser-mark ()
  (defun get-mark-content (arg) "->a{a}") ;; mock method
  (should
   (equal
    (ruby-parser-mark)
    "s(:iter, s(:call, nil, :lambda), s(:args, :a), s(:lvar, :a))")
   ))

(ert-deftest rb-source ()
  (should
   (equal
    (last (split-string (car (rb-source "Object.drb_start")) "/"))
    (list "drbcli.rb"))) )

;; Base rb-source, and add `find-file` & `goto-line`
(ert-deftest rb-source-find ()
  (defun get-mark-content (arg) "Object.drb_start")
  (defun find-file (file) (message "+++++%s+++++" file) )
  (defun goto-line (line buffer) (progn (message "----%s-----%s" line buffer) (set 'find-buffer buffer) (set 'find-line line) ) )
  (rb-source-find)
  (should (equal find-buffer "drbcli.rb"))
  (should (equal find-line 16))
  (should (equal rb-obj-root "Object"))
  (makunbound 'find-buffer)
  (makunbound 'find-line)
  (makunbound 'rb-obj-root)
  )
;; Base rb-source-find's object: rb-obj-root "Post", find it's methods have the method "ccc"
(ert-deftest rb-source-find-next ()
  (rb-eval "class Post; \n def aaa;222;end; \n def self.bbb;ccc;end; \n def self.ccc; 111; end ; \n end; \n @post = Post.new")
  ;;(rb-source "Post.bbb") ;; 3
  (set 'rb-obj-root "Post")
  (defun get-mark-content (arg) "ccc")
  (defun find-file (file) (message "+++++%s+++++" file) )
  (defun goto-line (line buffer) (progn (message "----%s-----%s" line buffer) (set 'find-buffer buffer) (set 'find-line line) ) )
  (rb-source-find-next)
  (should (equal find-buffer "(eval)"))
  (should (equal find-line 4))
  (makunbound 'find-buffer)
  (makunbound 'find-line)
  )

;; when rb-source-find-next's rb-method-root "ccc" is not belongs to rb-obj-root "Post", the file name will as the new Object name
(ert-deftest rb-source-find-next-super ()
  (defun buffer-file-name () "abc/lib/post.rb")
  (setq rb-method-root "ccc")
  (rb-eval "class Post; \n def aaa;222;end; \n def self.bbb;ccc;end; \n def self.ccc; 111; end ; \n end; \n @post = Post.new")
  (defun find-file (file) (message "+++++%s+++++" file) )
  (defun goto-line (line buffer) (progn (message "----%s-----%s" line buffer) (set 'find-buffer buffer) (set 'find-line line) ) )
  (rb-source-find-next-super)
  (should (equal find-buffer "(eval)"))
  (should (equal find-line 4))
  (makunbound 'find-buffer)
  (makunbound 'find-line)
  )


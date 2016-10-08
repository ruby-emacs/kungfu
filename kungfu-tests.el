(require 'ert)
(load-file "./kungfu.el")
(require 'kungfu)

;;; for test ;;
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
(defun get-mark-content (arg) "Test Abc")
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

;;(defun get-mark-content (arg) "->a{a}")
;;(pop (split-string "dasdsa \n dasdsa \n " "\n")) , erro
;;(car (last (butlast '(1 2 5 8 1 3)))) ;;=> 1
;; ==== 在Emacs中运行是正确的,在命令运行是错误的
;; (ert-deftest ruby-parser-mark ()
;;   (should
;;    (equal
;;     (ruby-parser-mark)
;;     (get-messages-last-line))
;;    ))
;; 



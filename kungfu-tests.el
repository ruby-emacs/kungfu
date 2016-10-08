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
(defun get-mark-content (arg)
  "Test Abc")

(ert-deftest downcase-str ()
  (should (equal (downcase-str) "test abc") )) 


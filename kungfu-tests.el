(require 'ert)
(load-file "./kungfu.el")
(require 'kungfu)

;;; for test ;;
(ert-deftest my-test ()
  (my-fixture
   (lambda ()
     [test code])))
;;;;

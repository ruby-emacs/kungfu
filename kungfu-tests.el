(require 'ert)
(load-file "./kungfu.el")
(require 'kungfu)

;;; for test ;;
(ert-deftest my-fixture ()
  (my-fixture
   (lambda ()
     [test code])))
;;;;

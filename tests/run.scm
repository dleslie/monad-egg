(use utils)
(use test)

(test-group "CSC"
            (compile-file "tests.scm")
            (run-tests))

(test-group "CSI"
            (load "tests.scm")
            (run-tests))

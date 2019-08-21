(import test)
(import compile-file)

(test-group "CSC"
            (compile-file "tests.scm")
            (run-tests))

(test-group "CSI"
            (load "tests.scm")
            (run-tests))

(test-exit)

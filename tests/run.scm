(use utils)
(use test)

(load "tests.scm")

(test-group "CSI" (run-tests))

(define (run-tests) (error "Failed to compile/load tests"))
(compile-file "tests.scm")

(test-group "CSC" (run-tests))

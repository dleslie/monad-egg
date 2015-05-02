(use test)
(use monad)

(define (run-tests)

  (test-group "Identity"
              (test "Unit"
                    '()
                    (do <id>
                        (return '())))
              (test "Bind"
                    '(a)
                    (do <id>
                        (x <- 'a)
                      `(,x)))
              (test-error "Fail"
                          (do <id> (fail))))

  (test-group "Maybe"
              (test "Unit"
                    '(Just Second)
                    (do <maybe>
                        (if #t
                            (return 'First)
                            (fail))
                      (return 'Second)))
              (test "Bind"
                    'Nothing
                    (do <maybe>
                        (x <- (fail))
                      (if #t
                          x
                          (return 'First))
                      (return 'Second)))
              (test "Fail"
                    'Nothing
                    (do <maybe> (fail))))

  )

(use test)
(use monad)
(use srfi-1)

(define (run-tests)
  (test-group "Identity"
              (test "Unit"
                    '()
                    (do/m <id>
                          (return '())))
              (test "Bind"
                    '(a)
                    (do/m <id>
                          (x <- 'a)
                          (return `(,x))))
              (test-error "Fail"
                          (do/m <id> (fail))))

  (test-group "Maybe"
              (test "Unit"
                    '(Just Second)
                    (do/m <maybe>
                          (if #t
                              (return 'First)
                              (fail))
                          (return 'Second)))
              (test "Bind"
                    'Nothing
                    (do/m <maybe>
                          (x <- (fail))
                          (if #t
                              x
                              (return 'First))
                          (return 'Second)))
              (test "Fail"
                    'Nothing
                    (do/m <maybe> (fail))))

  (test-group "List"
              (test "Unit"
                    '(1)
                    (do/m <list>
                          (return 1)))
              (test "Bind"
                    '((1 a) (1 b) (2 a) (2 b))
                    (do/m <list>
                          (x <- '(1 2))
                          (y <- '(a  b))
                          (return `(,x ,y))))
              (test-error "Fail" (do/m <list> (fail))))

  (test-group "State"
              (test "Unit"
                    '(1 . #f)
                    ((do/m <state> (return 1)) #f))
              (test "Bind"
                    '(Positive . 1)
                    ((do/m <state>
                           (x <- (/m get))
                           (if (> x 0)
                               (return 'Positive)
                               (return 'Negative)))
                     1))
              (test "Gets"
                    '(Positive . -1)
                    ((do/m <state>
                           (x <- (/m! gets (lambda (value) (+ value 2))))
                           (if (> x 0)
                               (return 'Positive)
                               (return 'Negative)))
                     -1))
              (test "Modify"
                    '(Positive . 1)
                    ((do/m <state>
                           (/m! modify (lambda (value) (+ value 2)))
                           (x <- (/m get))
                           (if (> x 0)
                               (return 'Positive)
                               (return 'Negative)))
                     -1))
              (test "Put"
                    '(Positive . 99)
                    ((do/m <state>
                           (/m! put 99)
                           (x <- (/m get))
                           (if (> x 0)
                               (return 'Positive)
                               (return 'Negative)))
                     -1))
              (test-error "Fail" (do/m <state> (fail))))

  (test-group "Reader"
              (test "Unit"
                    1
                    ((do/m <reader> (return 1)) 2))
              (test "Bind"
                    1
                    ((do/m <reader>
                           (x <- (/m ask))
                           (return x))
                     1))
              (test-error "Fail"
                          ((do/m <reader> (fail)) 1))
              (test "Asks"
                    2
                    ((do/m <reader>
                           (x <- (/m! asks (lambda (v) (+ v 1))))
                           (return x))
                     1))
              (test "Local"
                    1
                    ((do/m <reader>
                           (/m! local
                                (lambda (v) (+ v 1))
                                (/m ask))
                           (x <- (/m ask))
                           (return x))
                     1)))
  (test-group "Writer"
              (test "Unit"
                    '(hello)
                    (do/m <writer>
                          (return 'hello)))
              (test "Bind"
                    '((hello world))
                    (do/m <writer>
                          (x <- (return 'hello))
                          (return (list x 'world))))
              (test-error "Fail"
                          (do/m <writer> (fail)))
              (test "Tell"
                    '(() hello world)
                    (do/m <writer>
                          (x <- (return 'hello))
                          (/m! tell x)
                          (/m! tell 'world)))
              (test "Listen"
                    '((() hello test world) hello test world)
                    (let ((other-writer
                           (do/m <writer>
                                 (/m! tell 'hello)
                                 (/m! tell 'test)
                                 (/m! tell 'world))))
                      (do/m <writer>
                            (x <- (/m! listen other-writer))
                            (return x))))
              (test "Listens"
                    '((() arf arf arf) hello test world)
                    (let ((other-writer
                           (do/m <writer>
                                 (/m! tell 'hello)
                                 (/m! tell 'test)
                                 (/m! tell 'world))))
                      (do/m <writer>
                            (x <- (/m! listens
                                       (lambda (l) (map (lambda (s) 'arf) l))
                                       other-writer))
                            (return x))))
              (test "Pass"
                    '(() "(1 2 3)")
                    (let ((other-writer
                           (do/m <writer>
                                 (/m! tell '(1 2 3))
                                 (return
                                  (/m! tell
                                       (lambda (l)
                                         (map ->string l)))))))
                      (do/m <writer>
                            (/m! pass other-writer))))
              (test "Censor"
                    '(() "(1 2 3)")
                    (let ((other-writer
                           (do/m <writer>
                                 (/m! tell '(1 2 3)))))
                      (do/m <writer>
                            (/m! censor
                                 (lambda (l) (map ->string l))
                                 other-writer)))))
  )

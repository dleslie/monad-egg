(use test)
(use monad)
(use srfi-1)

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
                      (return `(,x))))
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

  (test-group "List"
              (test "Unit"
                    '(1)
                    (do <list>
                      (return 1)))
              (test "Bind"
                    '((1 a) (1 b) (2 a) (2 b))
                    (do <list>
                      (x <- '(1 2))
                      (y <- '(a  b))
                      (return `(,x ,y))))
              (test-error "Fail" (do <list> (fail))))

  (test-group "State"
              (test "Unit"
                    '(1 . #f)
                    ((do <state> (return 1)) #f))
              (test "Bind"
                    '(Positive . 1)
                    ((do <state>
                       (x <- (/m get))
                       (if (> x 0)
                           (return 'Positive)
                           (return 'Negative)))
                     1))
              (test "Gets"
                    '(Positive . -1)
                    ((do <state>
                       (x <- (/m! gets (lambda (value) (+ value 2))))
                       (if (> x 0)
                           (return 'Positive)
                           (return 'Negative)))
                     -1))
              (test "Modify"
                    '(Positive . 1)
                    ((do <state>
                       (/m! modify (lambda (value) (+ value 2)))
                       (x <- (/m get))
                       (if (> x 0)
                           (return 'Positive)
                           (return 'Negative)))
                     -1))
              (test "Put"
                    '(Positive . 99)
                    ((do <state>
                       (/m! put 99)
                       (x <- (/m get))
                       (if (> x 0)
                           (return 'Positive)
                           (return 'Negative)))
                     -1))
              (test-error "Fail" (do <state> (fail))))

  (test-group "Reader"
              (test "Unit"
                    1
                    ((do <reader> (return 1)) 2))
              (test "Bind"
                    1
                    ((do <reader>
                       (x <- (/m ask))
                       (return x))
                     1))
              (test-error "Fail"
                          ((do <reader> (fail)) 1))
              (test "Asks"
                    2
                    ((do <reader>
                       (x <- (/m! asks (lambda (v) (+ v 1))))
                       (return x))
                     1))
              (test "Local"
                    1
                    ((do <reader>
                       (/m! local
                            (lambda (v) (+ v 1))
                            (/m ask))
                       (x <- (/m ask))
                       (return x))
                     1)))
  (test-group "Writer"
              (test "Unit"
                    '(hello)
                    (do <writer>
                      (return 'hello)))
              (test "Bind"
                    '((hello world))
                    (do <writer>
                      (x <- (return 'hello))
                      (return `(,x world))))
              (test-error "Fail"
                          (do <writer> (fail)))
              (test "Tell"
                    '(() hello world)
                    (do <writer>
                      (x <- (return 'hello))
                      (/m! tell `(,x))
                      (/m! tell '(world))))
              (test "Listen"
                    '((() hello test world) hello test world)
                    (let ((other-writer
                           (do <writer>
                             (/m! tell '(hello))
                             (/m! tell '(test))
                             (/m! tell '(world)))))
                      (do <writer>
                        (x <- (/m! listen other-writer))
                        (return x))))
              (test "Listens"
                    '((() arf arf arf) hello test world)
                    (let ((other-writer
                           (do <writer>
                             (/m! tell '(hello))
                             (/m! tell '(test))
                             (/m! tell '(world)))))
                      (do <writer>
                        (x <- (/m! listens
                                   (lambda (l) (map (lambda (s) 'arf) l))
                                   other-writer))
                        (return x)))
                    )
              )
  )

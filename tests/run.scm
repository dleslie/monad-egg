(use test)
(use monad)

(test-group "Identity"
  (test "Unit"
    '()
    (do <id>
      '()))
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
        '(Just First)
        'Nothing)
      '(Just Second)))
  (test "Bind"
    'Nothing
    (do <maybe>
      (x <- 'Nothing)
      (if #t
          x
	  '(Just First))
      '(Just Second)))
  (test "Fail"
    'Nothing
    (do <maybe> (fail))))


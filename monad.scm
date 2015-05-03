(module monad *
  (import scheme chicken extras srfi-1 monad-core)
  (reexport monad-core)
  
  (define-monad
    <id>
    (lambda (a) a)
    (lambda (a f) (f a)))

  (define-monad
    <maybe>
    (lambda (a)  `(Just ,a))
    (lambda (a f) (if (not (eq? 'Nothing a)) (f a) 'Nothing))
    (case-lambda (() 'Nothing)
		 ((_ . _) 'Nothing)))

  (define-monad
    <list>
    (lambda (a) (list a))
    (lambda (a f) (concatenate (map f a))))

  (define-monad
    <state>
    (lambda (a) (lambda (s) `(,a . ,s)))
    (lambda (a f)
      (lambda (s)
	(let* ((p (a s))
	       (a^ (car p))
	       (s^ (cdr p)))
	  ((f a^) s^)))))

  (define (<state>-get s)
    `(,s . ,s))

  (define (<state>-put new-state)
    (lambda (s)
      `(() . ,new-state)))

  (define (<state>-gets f)
    (do-using 
     <state>
     (s <- (/m get))
     (return (f s))))

  (define (<state>-modify f)
    (do-using
     <state>
     (s <- (/m get))
     (/m! put (f s))))

  (define-monad
    <reader>
    (lambda (a) (lambda (v) a))
    (lambda (a f) (lambda (v) ((f (a v)) v))))

  (define (<reader>-ask a) a)

  (define (<reader>-asks f)
    (do-using <reader>
	      (x <- (/m ask))
	      (return (f x))))

  (define (<reader>-local f r)
    (lambda (a)
      (r (f a))))

  (define-monad
    <cps>
    (lambda (a) (lambda (k) (k a)))
    (lambda (a f) (lambda (k) (a (lambda (a^) (let ((b (f a^))) (b k)))))))

  (define (<cps>-call/cc f)
    (lambda (c)
      ((f [lambda (a) (lambda () (c a))]) c)))

  (define-monad
    <exception>
    (lambda (a) `(success ,a))
    (lambda (a f) (if (eq? (car a) 'success) (f (cadr a)) a))
    (case-lambda (() `(failure))
		 ((a . b) `(failure ,a . ,b))))

  (define (<exception>-throw e)
    (do-using 
     <exception>
     (fail e)))

  (define (<exception>-catch m f)
    (if (eq? (car m) 'failure)
	(f m)
	m))

  (define-monad
    <writer>
    (lambda (a) `(,a . ()))
    (lambda (a f)
      (let* ((b (f (car a))))
	`(,(car b) . ,(append (cdr a) (cdr b))))))

  (define (<writer>-tell v)
    `(() . ,v))
  
  (define (<writer>-listen a)
    `(,a . ,(cdr a)))

  (define (<writer>-listens f m)
    (do <writer>
	(pair <- m)
      (return `(,(car pair) . ,(f (cdr pair))))))

  (define (<writer>-pass m) ; expects ((v . f) . w)
    (let* ((p (car m))
	   (a (car p))
	   (f (cdr p))
	   (w (cdr m)))
      `(,a . ,(f w))))

  (define (<writer>-censor f m)
    (<writer>-pass 
     (do-using <writer>
	       (a <- m)
	       (return `(,a . ,f)))))
  )

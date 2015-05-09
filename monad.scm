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
      (pair <- (/m! listen m))
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

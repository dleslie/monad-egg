(module monad *
  (import scheme chicken monad-core)
  (use extras srfi-1)
  (reexport monad-core)
  
  (define-monad
    <id>
    (lambda (a) a)
    (lambda (a f) (f a)))

  (define-monad
    <maybe>
    (lambda (a)  `(Just ,a))
    (lambda (a f) (if (not (eq? 'Nothing a)) (f (cadr a)) 'Nothing))
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
    (do/m 
     <state>
     (s <- (/m get))
     (return (f s))))

  (define (<state>-modify f)
    (do/m
     <state>
     (s <- (/m get))
     (/m! put (f s))))

  (define-monad
    <reader>
    (lambda (a) (lambda (v) a))
    (lambda (a f) (lambda (v) ((f (a v)) v))))

  (define (<reader>-ask a) a)

  (define (<reader>-asks f)
    (do/m <reader>
          (x <- (/m ask))
          (return (f x))))

  (define (<reader>-local f r)
    (lambda (a)
      (r (f a))))

  (define-monad
    <writer>
    (lambda (a) (cons a '()))
    (lambda (a f)
      (let* ((b (f (car a))))
	(cons (car b) (append (cdr a) (cdr b))))))

  (define (<writer>-tell . v)
    (cons '() v))
  
  (define (<writer>-listen a)
    (cons a (cdr a)))

  (define (<writer>-listens f m)
    (do/m <writer>
          (pair <- (/m! listen m))
          (return (cons (car pair) (f (cdr pair))))))

  (define (<writer>-pass m) ; expects ((a f) w)
    (let* ((p (car m))
	   (a (car p))
	   (f (cadr p))
	   (w (cdr m)))
      (cons a (f w))))

  (define (<writer>-censor f m)
    (<writer>-pass 
     (do/m <writer>
           (apply (/m tell) (cdr m))
           (return (/m! tell f)))))
  )

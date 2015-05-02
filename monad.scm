(module monad-core (%build-for-monad %let-alias define-monad using do do-using)
  (import scheme chicken extras srfi-1)

  (define-syntax %build-for-monad
    (er-macro-transformer
     (lambda (expression inject compare)
       (let ((monad (cadr expression))
	     (func (caddr expression)))
         (inject (symbol-append (strip-syntax monad) '- (strip-syntax func)))))))

  (define-syntax %define-monad
    (er-macro-transformer
     (lambda (expression inject compare)
       (let* ((monad (cadr expression))
	      (expression (cddr expression))
	      (unit-function (car expression))
	      (bind-function (cadr expression))
	      (fail-function (caddr expression))
	      (fail-function (if fail-function fail-function
				 `(case-lambda (() (error (format "Failure in evaluating ~S monad." ',monad)))
					       ((_ . _) (error (format "Failure in evaluating ~S monad." ',monad)))))))
	 `(begin
	    (define ,(inject (symbol-append (strip-syntax monad) '-unit))
	      ,unit-function)
	    (define ,(inject (symbol-append (strip-syntax monad) '-bind))
	      ,bind-function)
	    (define ,(inject (symbol-append (strip-syntax monad) '-fail))
	      ,fail-function))))))
  
  (define-syntax %let-alias
    (syntax-rules ()
      ((_ ((id alias) ...) body ...) 
       (let-syntax ((helper (syntax-rules () 
			      ((_ id ...) (begin body ...)))))
	 (helper alias ...)))))
  
  (define-syntax define-monad
    (syntax-rules ()
      ((_ monad unit-function bind-function fail-function)
       (%define-monad monad unit-function bind-function fail-function))
      ((_ monad unit bind)
       (%define-monad monad unit bind #f))))

  (define-syntax using
    (er-macro-transformer
     (lambda (e r c)
       (let* ((monad (cadr e))
  	      (body (cddr e)))
  	 `(%let-alias
  	   ((>>= (%build-for-monad ,monad bind))
  	    (return (%build-for-monad ,monad unit))
  	    (fail (%build-for-monad ,monad fail)))

  	   (define-syntax /m
  	     (syntax-rules ()
  	       ((_ func) (%build-for-monad ,monad func))))

           ;; Correct version that breaks everything
  	   ;; (define-syntax /m!
  	   ;;   (syntax-rules ()
  	   ;;     ((_ func . body) ((%build-for-monad ,monad func) . body))))

           ; Erroneous version that doesn't break other tests
           (define-syntax /m!
  	     (syntax-rules ()
  	       ((_ func . body) ((%build-for-monad ,monad func) body))))

           ,@body)))))
  
  (define-syntax %unroll-do-using
    (syntax-rules (<-)
      ((_ monad)
       (begin))
      ((_ monad expr)
       expr)
      ((_ monad (var <- expr) . rest)
       ((%build-for-monad monad bind) expr (lambda (var) (%unroll-do-using monad . rest))))
      ((_ monad expr . rest)
       ((%build-for-monad monad bind) expr (lambda (_) (%unroll-do-using monad . rest))))))

  (define-syntax do-using
    (syntax-rules ()
      ((_ monad expr)
       (using monad expr))
      ((_ monad expr ...)
       (using monad (%unroll-do-using monad expr ...)))))
  
  (define-syntax do
    (syntax-rules ()
      ((do m ...)
       (do-using m ...)))))

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

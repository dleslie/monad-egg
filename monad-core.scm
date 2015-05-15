(module monad-core (%build-for-monad %let-alias define-monad using do/m do-using)
  (import scheme chicken extras srfi-1)

  (define-syntax %build-for-monad
    (er-macro-transformer
     (lambda (expression rename compare)
       (let ((monad (cadr expression))
	     (func (caddr expression)))
         (symbol-append (strip-syntax monad) '- (strip-syntax func))))))

  (define-syntax %define-monad
    (er-macro-transformer
     (lambda (expression rename compare)
       (let* ((%begin (rename 'begin))
              (%define (rename 'define))
              (monad (cadr expression))
	      (expression (cddr expression))
	      (unit-function (car expression))
	      (bind-function (cadr expression))
	      (fail-function (caddr expression))
	      (fail-function (if fail-function fail-function
				 `(case-lambda (() (error (format "Failure in evaluating ~S monad." ',monad)))
					       ((_ . _) (error (format "Failure in evaluating ~S monad." ',monad)))))))
	 `(,%begin
           (,%define ,(symbol-append (strip-syntax monad) '-unit)
                     ,unit-function)
           (,%define ,(symbol-append (strip-syntax monad) '-bind)
                     ,bind-function)
           (,%define ,(symbol-append (strip-syntax monad) '-fail)
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
  	   (define-syntax /m!
  	     (syntax-rules ()
  	       ((_ func . body) ((%build-for-monad ,monad func) . body))))

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

  (define-syntax do/m
    (syntax-rules ()
      ((do/m m ...)
       (do-using m ...))))
  )

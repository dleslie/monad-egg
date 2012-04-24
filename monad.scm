(module 
 monad *
 (import scheme chicken extras srfi-1)

 (define-syntax define-monad
   (lambda (f r c)
     (##sys#check-syntax 'define-monad f '(_ _ _ . _))
     (let* ((name (cadr f))
            (unit-function (caddr f))
            (bind-function (cadddr f))
            (rest (cddddr f))
            (fail-function (if (null? rest)
                               `(case-lambda (() (error (format "Failure in evaluating ~S monad." ',name)))
                                             ((_ . _) (error (format "Failure in evaluating ~S monad." ',name))))
                               (car rest)))
            (bindf (symbol-append name '-bind))
            (unitf (symbol-append name '-unit))
            (failf (symbol-append name '-fail)))
       `(begin
          (,(r 'define) ,unitf ,unit-function)
          (,(r 'define) ,bindf ,bind-function)
          (,(r 'define) ,failf ,fail-function)))))

 (define-syntax using
   (lambda (f r c)
     (##sys#check-syntax 'using f '(_ _ . _))
     (let* ((name (cadr f))
            (body (cddr f)))
       `(,(r 'let) ((>>= ,(symbol-append name '-bind))
                    (return ,(symbol-append name '-unit))
                    (fail ,(symbol-append name '-fail)))
         ,@body))))

 (define-syntax fail
   (lambda (f r c)
     (##sys#check-syntax 'return f '(_ _ . _))
     (let* ((name (cadr f))
            (body (cddr f)))
       `(,(symbol-append name '-fail) ,@body))))

 (define-syntax return
   (lambda (f r c)
     (##sys#check-syntax 'return f '(_ _ . _))
     (let* ((name (cadr f))
            (body (cddr f)))
       `(,(symbol-append name '-unit) ,@body))))

 (define-syntax do-using
   (lambda (f r c)
     (##sys#check-syntax 'do-using f '(_ _ . _))
     (letrec ((name (cadr f))
              (body (cddr f))
              (bindf (symbol-append name '-bind))
              (unitf (symbol-append name '-unit))
              (failf (symbol-append name '-fail)))
       `((,(r 'lambda) ()
          (define-syntax bound-do
            (syntax-rules (<-)
              ((_ m) m)
              ((_ (var <- m) m* m** ...)
               (,bindf m (lambda (var) (bound-do m* m** ...))))
              ((_ m m* m** ...)
               (,bindf m (lambda (_) (bound-do m* m** ...))))))
          (bound-do ,@body))))))

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
   (lambda (a f) (concatenate! (map! f a))))

 (define-monad
   <state>
   (lambda (a) (lambda (s) `(,a . ,s)))
   (lambda (a f)
     (lambda (s)
       (let* ((p (a s))
              (a^ (car p))
              (s^ (cdr p)))
         ((f a^) s^)))))

 (define-monad
   <reader>
   (lambda (a) (lambda (v) a))
   (lambda (a f) (lambda (v) ((f (a v)) v))))

 (define-monad
   <cps>
   (lambda (a) (lambda (k) (k a)))
   (lambda (a f) (lambda (k) (a (lambda (a^) (let ((b (f a^))) (b k)))))))

 (define-monad
   <exception>
   (lambda (a) `(success ,a))
   (lambda (a f) (if (eq? (car a) 'success) (f (cadr a)) a))
   (case-lambda (() `(failure))
                ((a . b) `(failure ,a . ,b))))

 (define-monad
   <writer>
   (lambda (a) `(,a . ()))
   (lambda (a f)
     (let ((b (f (car a))))
       `(,(car b) . ,(append (cdr a) (cdr b)))))))

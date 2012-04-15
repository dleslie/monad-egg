(module 
 monad *
 (import scheme chicken extras srfi-1)

 ;; Want:
 ;; (define-monad name return bind)
 ;;
 ;; Which produces:
 ;; (name-unit value) : monad value
 ;; (name-bind value function) : (lambda (value-type)) : monad value2

 ;; Also have:
 ;; (using name monad) : monad value
 ;; Which defines, internally:
 ;; >>= return

 (define-record monad tag value)
 (define-record-printer (monad m out)
   (fprintf out "#~S ~S" (monad-tag m) (run m)))

 (define-syntax define-monad
   (lambda (f r c)
     (##sys#check-syntax 'define-monad f '(_ _ _ . _))
     (let* ((name (cadr f))
            (unit-function (caddr f))
            (bind-function (cadddr f))
            (pred (symbol-append name '?))
            (bindf (symbol-append name '-bind))
            (unit (symbol-append name '-unit))
            (run (symbol-append name '-run)))
       `(begin
          (,(r 'define) (,pred m)
           (and (monad? m)
                (eq? (monad-tag m) ',name)))
          (,(r 'define) (,unit val)
           (make-monad ',name (delay (,unit-function val))))
          (,(r 'define) (,bindf m1 f)
           (make-monad ',name (delay (,bind-function (force (monad-value m1)) f))))))))

 (define (run monad)
   (force (monad-value monad)))

 (define (run-chain init . monads)
   (fold (lambda (n p) (run (n p))) init monads))

 (define-syntax using
   (lambda (f r c)
     (##sys#check-syntax 'using f '(_ _ . _))
     (let* ((name (cadr f))
            (body (cddr f)))
       `(,(r 'let) ((>>= ,(symbol-append name '-bind))
                    (return ,(symbol-append name '-unit)))
         ,@body))))

                                        ; (>>= (>>= (first) (second)) (third))

 (define-syntax doto-using
   (lambda (f r c)
     (##sys#check-syntax 'do-using f '(_ _ _ . _))
     (letrec ((name (cadr f))
              (init (caddr f))
              (body (cdddr f))
              (bindf (symbol-append name '-bind))
              (unitf (symbol-append name '-unit))
              (bind-next 
               (lambda (previous-monad remaining)
                 (let* ((next (car remaining))
                        (rest (cdr remaining))
                        (current-monad `(,bindf ,previous-monad ,next)))
                   (if (eq? '() rest)
                       current-monad
                       (bind-next current-monad rest))))))
       (bind-next `(,unitf ,init) body))))

 (define-monad
   <id>
   (lambda (a) a)
   (lambda (a f) (f a)))

 (define-monad
   <maybe>
   (lambda (a) a)
   (lambda (a f) (if a (f a) #f)))

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
   (lambda (a f) (if (eq? (car a) 'success) (f (cadr a)) a)))

 (define-monad
   <writer>
   (lambda (a) `(,a . ()))
   (lambda (a f)
     (let ((b (f (car a))))
       `(,(car b) . ,(append (cdr a) (cdr b)))))))

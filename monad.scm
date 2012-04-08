(module 
 monad *
 (import scheme chicken extras)

 ;; Want:
 ;; (define-monad name return bind)
 ;;
 ;; Which produces:
 ;; (name-unit value) : monad value
 ;; (name-bind value function) : (lambda (value-type)) : monad value2
 ;; (name-run monad) : value

 ;; Also have:
 ;; (using name monad) : monad value
 ;; Which defines, internally:
 ;; >>= return

 (define-record monad tag value)
 (define-record-printer (monad m out)
   (fprintf out "#~S ~S" (monad-tag m) (force (monad-value m))))

 (define-syntax define-monad
   (syntax-rules ()
     ((define-monad name unit-function binding-function)
      (let* ((pred (symbol-append name '?))
             (bind (symbol-append name '-bind))
             (unit (symbol-append name '-unit))
             (run (symbol-append name '-run)))
        (eval 
         `(begin
            (define (,pred m)
              (and (monad? m)
                   (eq? (monad-tag m) name)))
            (define (,unit val)
              (make-monad name (delay (unit-function val))))
            (define (,bind m1 f)
              (make-monad name (delay (binding-function (force (monad-value m1)) f))))
            (define (,run m)
              (force (monad-value m)))))))))

 (define-syntax using
   (syntax-rules ()
     ((using name body)
      (eval
       (begin
         `(let ((>>= ,(symbol-append name '-bind))
                (return ,(symbol-append name '-unit)))
            body))))))

 (define-monad
   'idM
   (lambda (a) a)
   (lambda (a f) (f a)))

 (define-monad
   'maybeM
   (lambda (a) a)
   (lambda (a f) (if a (f a) #f)))

 (define-monad
   'listM
   (lambda (a) (list a))
   (lambda (a f) (concatenate! (map! f a)))))

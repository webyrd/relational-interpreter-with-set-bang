(load "mk.scm")

;;; This version of the interpreter uses curried lambda/application,
;;; and tries all interleavings of evaluation orders.

;;; The implementation is quite sloooow.

;;; adding 'cons' and/or 'quote', both of which are needed for quines,
;;; means that inferring an undefined program takes too long to come
;;; back (versus ~30 seconds otherwise).

;;; The coolest test for this code is this one, which returns after 30
;;; seconds.
#|
(time (test "infer-undefined-1"
  (run 1 (expr v1 v2)
    (=/= v1 v2)
    (evalo expr v1)
    (evalo expr v2))
  '(((((lambda (_.0)
         (_.0 (set! _.0 (lambda (_.1) _.0))))
       (lambda (_.2) _.2))
      void
      (closure (lambda (_.1) _.0) (ext-env _.0 z ())))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda)) ((_.0 set!)) ((_.0 void)) ((_.1 closure)) ((_.1 void)) ((_.2 closure)) ((_.2 void))) (sym _.0 _.1 _.2)))))
|#


(define printg
  (lambda (format-str . args)
    (lambda (c)
      (let ((args (walk* args (c->S c))))
        (apply printf format-str args))
      (newline)
      c)))


#|
(define-syntax extend-env
  (syntax-rules ()
    [(_ x addr env)
     `(ext-env ,x ,addr ,env)]))

(define-syntax extend-store
  (syntax-rules ()
    [(_ addr val store)
     `(ext-store ,addr ,val ,store)]))

(define lookup-varo
  (lambda (x env addr)
    (fresh (y a rest)
      (== `(ext-env ,y ,a ,rest) env)
      (conde
        ((== y x) (== a addr))
        ((=/= y x) (lookup-varo x rest addr))))))

(define lookup-addro
  (lambda (addr store val)
    (fresh (a v rest)
      (== `(ext-store ,a ,v ,rest) store)
      (conde
        ((== a addr) (== v val))
        ((=/= a addr) (lookup-addro addr rest val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `(ext-env ,y ,v ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))
|#




(define-syntax extend-env
  (syntax-rules ()
    [(_ x addr env)
     `((,x . ,addr) . ,env)]))

(define-syntax extend-store
  (syntax-rules ()
    [(_ addr val store)
     `((,addr . ,val) . ,store)]))

(define lookup-varo
  (lambda (x env addr)
    (fresh (y a rest)
      (== `((,y . ,a) . ,rest) env)
      (conde
        ((== y x) (== a addr))
        ((=/= y x) (lookup-varo x rest addr))))))

(define lookup-addro
  (lambda (addr store val)
    (fresh (a v rest)
      (== `((,a . ,v) . ,rest) store)
      (conde
        ((== a addr) (== v val))
        ((=/= a addr) (lookup-addro addr rest val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))




(define eval-listo
  (lambda (exp env store-in addr-in store-out addr-out val)
    (conde
      ((== '() exp)
       (== '() val)
       (== store-in store-out)
       (== addr-in addr-out))
      ((fresh (a d v-a v-d store^ addr^)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-expo a env store-in addr-in store^ addr^ v-a)
         (eval-listo d env store^ addr^ store-out addr-out v-d))))))


(define evalo
  (lambda (exp val)
    (fresh (store-out addr-out)
      (eval-expo exp '() '() 'z store-out addr-out val))))

(define eval-expo
  (lambda (exp env store-in addr-in store-out addr-out val)
    (fresh ()
      (absento 'closure exp)
      (absento 'void exp)
      (conde
        
        ((fresh (addr)
           (symbolo exp)
           (== store-in store-out)
           (== addr-in addr-out)
           (lookup-varo exp env addr)
           (lookup-addro addr store-in val)))
        
        ((fresh (x body)
           (== `(lambda (,x) ,body) exp)
           (== `(closure (lambda (,x) ,body) ,env) val)
           (== store-in store-out)
           (== addr-in addr-out)
           (symbolo x)
           (not-in-envo 'lambda env)))
        
        ((fresh (x e v addr store^ addr^)
           (== `(set! ,x ,e) exp)
           (== 'void val)                      
           (symbolo x)
           (== addr^ addr-out)
           (== (extend-store addr v store^) store-out)
           (not-in-envo 'set! env)
           (lookup-varo x env addr)           
           (eval-expo e env store-in addr-in store^ addr^ v)))
        
        #|
        ((fresh (a d v-a v-d store^ addr^)
           (== `(cons ,a ,d) exp)
           (== `(,v-a . ,v-d) val)
           (not-in-envo 'cons env)
           (eval-expo a env store-in addr-in store^ addr^ v-a)
           (eval-expo d env store^ addr^ store-out addr-out v-d)))        
        ((fresh (a d v-a v-d store^ addr^)
           (== `(cons ,a ,d) exp)
           (== `(,v-a . ,v-d) val)
           (not-in-envo 'cons env)
           (eval-expo d env store-in addr^ store^ addr-out v-d)
           (eval-expo a env store^ addr-in store-out addr^ v-a)))
        |#
        
        #|
        ((fresh (e*)
           (== `(list . ,e*) exp)
           (not-in-envo 'list env)
           (eval-listo e* env store-in addr-in store-out addr-out val)))
        |#               

        ((fresh (rator rand x body env^ v store^ store^^ addr^ addr^^ new-env new-store new-addr)
           (== `(,rator ,rand) exp)
           (== (extend-env x addr^^ env) new-env)
           (== (extend-store addr^^ v store^^) new-store)
           (== `(s ,addr^^) new-addr)
           (symbolo x)
           (eval-expo rator env store-in addr-in store^ addr^ `(closure (lambda (,x) ,body) ,env^))
           (eval-expo rand env store^ addr^ store^^ addr^^ v)
           (eval-expo body new-env new-store new-addr store-out addr-out val)))
        ((fresh (rator rand x body env^ v store^ store^^ addr^ addr^^ new-env new-store new-addr)
           (== `(,rator ,rand) exp)
           (== (extend-env x addr^^ env) new-env)
           (== (extend-store addr^^ v store^^) new-store)
           (== `(s ,addr^^) new-addr)
           (symbolo x)
           ;; we have to be careful here: want to feed the addr-in into the rator evaluation,
           ;; even though store-in is fed into the rand evaluation
           (eval-expo rand env store-in addr^ store^ addr^^ v)
           (eval-expo rator env store^ addr-in store^^ addr^ `(closure (lambda (,x) ,body) ,env^))
           (eval-expo body new-env new-store new-addr store-out addr-out val)))

        #|
        ((== `(quote ,val) exp)
         (== store-in store-out)
         (== addr-in addr-out)
         (not-in-envo 'quote env)
        )
        |#
        
        ))))

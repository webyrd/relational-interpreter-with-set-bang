(load "mk.scm")

;;; non-directional
(define lookupo
  (lambda (x env t)
    (fresh (y v rest)
      (== `(ext-env ,y ,v ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `(ext-env ,y ,v ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))

(define ext-envo
  (lambda (x a env out)
    (fresh ()
      (== `(ext-env ,x ,a ,env) out)
      (symbolo x))))







(define eval-list-left-to-righto
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d v-a v-d)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-exp-left-to-righto a env v-a)
         (eval-list-left-to-righto d env v-d))))))

(define eval-left-to-righto
  (lambda (exp val)
    (eval-exp-left-to-righto exp '() val)))

(define eval-exp-left-to-righto
  (lambda (exp env val)
    (conde
      ((== `(quote ,val) exp)
       (not-in-envo 'quote env)
       (absento 'closure exp))

      ((symbolo exp) (lookupo exp env val))

      #|
      ((fresh (a*)
      (== `(list . ,a*) exp)
      (not-in-envo 'list env)
      (eval-list-left-to-righto a* env val)))
      |#
      
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (== `(closure (lambda (,x) ,body) ,env) val)
         (symbolo x)
         (not-in-envo 'lambda env)))

      ((fresh (a d v-a v-d store^ addr^)
         (== `(cons ,a ,d) exp)
         (== `(,v-a . ,v-d) val)
         (not-in-envo 'cons env)
         (eval-exp-left-to-righto a env v-a)
         (eval-exp-left-to-righto d env v-d)))
        
      ((fresh (rator rand x body env^ a res)
         (== `(,rator ,rand) exp)
         (ext-envo x a env^ res)
         (symbolo x)
         (eval-exp-left-to-righto rator env `(closure (lambda (,x) ,body) ,env^))
         (eval-exp-left-to-righto rand env a)
         (eval-exp-left-to-righto body res val))))))






(define eval-list-right-to-lefto
  (lambda (exp env val)
    (fresh (acc)
      (eval-list-right-to-left-auxo exp val '() acc)
      (unrollo acc env))))

(define eval-list-right-to-left-auxo
  (lambda (exp val acc-in acc-out)
    (conde
      ((== '() exp)
       (== '() val)
       (== acc-in acc-out))
      ((fresh (a d v-a v-d)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-list-right-to-left-auxo d v-d `((,a . ,v-a) . ,acc-in) acc-out))))))

(define unrollo
  (lambda (acc env)
    (conde
      ((== '() acc))
      ((fresh (e v acc^)
         (== `((,e . ,v) . ,acc^) acc)
         (eval-exp-right-to-lefto e env v)
         (unrollo acc^ env))))))

(define eval-right-to-lefto
  (lambda (exp val)
    (eval-exp-right-to-lefto exp '() val)))

(define eval-exp-right-to-lefto
  (lambda (exp env val)
    (conde
      ((== `(quote ,val) exp)
       (not-in-envo 'quote env)
       (absento 'closure val))

      ((symbolo exp) (lookupo exp env val))

      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (eval-list-right-to-lefto a* env val)))
      
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (== `(closure (lambda (,x) ,body) ,env) val)
         (symbolo x)
         (not-in-envo 'lambda env)))

      #|
      ((fresh (a d v-a v-d store^ addr^)
         (== `(cons ,a ,d) exp)
         (== `(,v-a . ,v-d) val)
         (not-in-envo 'cons env)
         ;; this is a problem:
         ;;
         ;; evaluating the cdr first means we are cdr-ing down a list
         ;; of unknown length before having a chance to fail!
         ;;
         ;; swapping these two clauses is the difference between generating a quine
         ;; that uses 'cons' in 35ms vs not coming back...
         (eval-exp-right-to-lefto d env v-d)
         (eval-exp-right-to-lefto a env v-a)))
      |#
      
      ((fresh (rator rand x body env^ a res)
         (== `(,rator ,rand) exp)
         (ext-envo x a env^ res)
         (eval-exp-right-to-lefto rand env a)
         (eval-exp-right-to-lefto rator env `(closure (lambda (,x) ,body) ,env^))
         (eval-exp-right-to-lefto body res val))))))

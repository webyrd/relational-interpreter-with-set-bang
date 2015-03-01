(load "mk.scm")

;;; This version of the code has two interpreters: one that evaluates
;;; left-to-right, and one that evaluates right-to-left


(define printg
  (lambda (format-str . args)
    (lambda (c)
      (let ((args (walk* args (c->S c))))
        (apply printf format-str args))
      (newline)
      c)))



;;; these helpers work in both directions
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




(define eval-list-left-to-righto
  (lambda (exp env store-in addr-in store-out addr-out val)
    (conde
      ((== '() exp)
       (== '() val)
       (== store-in store-out)
       (== addr-in addr-out))
      ((fresh (a d v-a v-d store^ addr^)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-exp-left-to-righto a env store-in addr-in store^ addr^ v-a)
         (eval-list-left-to-righto d env store^ addr^ store-out addr-out v-d))))))

(define eval-left-to-righto
  (lambda (exp val)
    (fresh (store-out addr-out)
      (eval-exp-left-to-righto exp '() '() 'z store-out addr-out val))))

(define eval-exp-left-to-righto
  (lambda (exp env store-in addr-in store-out addr-out val)
    (fresh ()
      (absento 'closure exp)
      (absento 'void exp)
      (conde
        
        ((== `(quote ,val) exp)
         (== store-in store-out)
         (== addr-in addr-out)
         (not-in-envo 'quote env))

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

        #|
        ((fresh (a d v-a v-d store^ addr^)
           (== `(cons ,a ,d) exp)
           (== `(,v-a . ,v-d) val)
           (not-in-envo 'cons env)
           (eval-exp-left-to-righto a env store-in addr-in store^ addr^ v-a)
           (eval-exp-left-to-righto d env store^ addr^ store-out addr-out v-d)))
        |#
        
        ((fresh (e*)
           (== `(list . ,e*) exp)
           (not-in-envo 'list env)
           (eval-list-left-to-righto e* env store-in addr-in store-out addr-out val)))

        ((fresh (x e v addr store^ addr^)
           (== `(set! ,x ,e) exp)
           (== 'void val)       
           (symbolo x)
           (== addr^ addr-out)
           (== (extend-store addr v store^) store-out)
           (not-in-envo 'set! env)
           (lookup-varo x env addr)           
           (eval-exp-left-to-righto e env store-in addr-in store^ addr^ v)))        

        ((fresh (rator rand x body env^ v store^ store^^ addr^ addr^^ new-env new-store new-addr)
           (== `(,rator ,rand) exp)
           (== (extend-env x addr^^ env) new-env)
           (== (extend-store addr^^ v store^^) new-store)
           (== `(s ,addr-in) new-addr)
           (symbolo x)
           (eval-exp-left-to-righto rator env store-in addr-in store^ addr^ `(closure (lambda (,x) ,body) ,env^))
           (eval-exp-left-to-righto rand env store^ addr^ store^^ addr^^ v)
           (eval-exp-left-to-righto body new-env new-store new-addr store-out addr-out val)))

        ))))



;;; reverseo, using length of both ls and sl to cut off divergence
;;; when runningbackwards:
;;;
;;; (run* (q) (reverseo q  '(a b c))) => ((c b a))
(define reverseo
  (lambda (ls sl)
    (reverse-auxo ls '() sl sl)))

(define reverse-auxo
  (lambda (ls acc sl len-ls)
    (conde
      ((== '() ls) (== '() len-ls) (== acc sl))
      ((fresh (a d a-len d-len)
         (== `(,a . ,d) ls)
         (== `(,a-len . ,d-len) len-ls)
         (reverse-auxo d `(,a . ,acc) sl d-len))))))



(define dual-reverseo
  (lambda (ls1 sl1 ls2 sl2)
    (dual-reverse-auxo ls1 '() sl1
                       ls2 '() sl2
                       sl1
                       sl2)))

(define dual-reverse-auxo
  (lambda (ls1 acc1 sl1 ls2 acc2 sl2 len-ls1 len-ls2)
    (conde
      ((== '() ls1) (== acc1 sl1)
       (== '() ls2) (== acc2 sl2)
       (== '() len-ls1)
       (== '() len-ls2))
      ((fresh (a1 d1 a2 d2 a-len1 d-len1 a-len2 d-len2)
         (== `(,a1 . ,d1) ls1)
         (== `(,a2 . ,d2) ls2)
         (== `(,a-len1 . ,d-len1) len-ls1)
         (== `(,a-len2 . ,d-len2) len-ls2)
         (dual-reverse-auxo d1 `(,a1 . ,acc1) sl1
                            d2 `(,a2 . ,acc2) sl2
                            d-len1
                            d-len2))))))








(define eval-list-right-to-lefto
  (lambda (exp env store-in addr-in store-out addr-out val)
    (fresh (exp/val/addrin/addrout-acc)
      ;;; generate and test...  yuck
      (accumulate-exp/val/addro exp addr-in addr-out val '() exp/val/addrin/addrout-acc)
      (unrollo exp/val/addrin/addrout-acc env store-in store-out))))

(define accumulate-exp/val/addro
  (lambda (exp addr-in addr-out val exp/val/addrin/addrout-acc-in exp/val/addrin/addrout-acc-out)
    (conde
      ((== '() exp)
       (== '() val)
       (== addr-in addr-out)
       (== exp/val/addrin/addrout-acc-in exp/val/addrin/addrout-acc-out))
      ((fresh (a d v-a v-d addr^)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (accumulate-exp/val/addro d addr^ addr-out v-d `((,a ,v-a ,addr-in ,addr^) . ,exp/val/addrin/addrout-acc-in) exp/val/addrin/addrout-acc-out))))))

(define unrollo
  (lambda (exp/val/addrin/addrout-acc env store-in store-out)
    (conde
      ((== '() exp/val/addrin/addrout-acc)
       (== store-in store-out))
      ((fresh (e v addr-in addr-out exp/val/addrin/addrout-acc^ store^)
         (== `((,e ,v ,addr-in ,addr-out) . ,exp/val/addrin/addrout-acc^) exp/val/addrin/addrout-acc)
         (eval-exp-right-to-lefto e env store-in addr-in store^ addr-out v)
         (unrollo exp/val/addrin/addrout-acc^ env store^ store-out))))))





(define eval-right-to-lefto
  (lambda (exp val)
    (fresh (store-out addr-out)
      (eval-exp-right-to-lefto exp '() '() 'z store-out addr-out val))))

(define eval-exp-right-to-lefto
  (lambda (exp env store-in addr-in store-out addr-out val)
    (fresh ()
      (absento 'closure exp)
      (absento 'void exp)
      (conde
        
        ((== `(quote ,val) exp)
         (== store-in store-out)
         (== addr-in addr-out)
         (not-in-envo 'quote env))

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

        #|
        ((fresh (a d v-a v-d store^ addr^)
           (== `(cons ,a ,d) exp)
           (== `(,v-a . ,v-d) val)
           (not-in-envo 'cons env)
           (eval-exp-right-to-lefto d env store-in addr^ store^ addr-out v-d)
           (eval-exp-right-to-lefto a env store^ addr-in store-out addr^ v-a)))
        |#

        ((fresh (e* er* vr*)
           (== `(list . ,e*) exp)
           (not-in-envo 'list env)
           ;; reverse e*, then do left-to-right evaluation on the reversed list
           ;;
           ;; is there a better way to do this?
           ;;
           ;; Problem (other than performance): address gets fed in
           ;; the wrong direction.  Is there a way to fix this?
           ;;
           ;; wait--can I just swap addr-in and addr-out???  seems to cause divergence or failure
           ;;
           ;; could I somehow sub1 instead of add1 from the addr counter?

           ;; really do want these two tied together in terms of length
;           (reverseo e* er*)
;           (reverseo vr* val)

           (dual-reverseo e* er* vr* val)
           
           (eval-list-left-to-righto er* env store-in addr-in store-out addr-out vr*)
           
           ))

        #|
        ((fresh (e*)
           (== `(list . ,e*) exp)
           (not-in-envo 'list env)
           ;; tried accumulating a list of expr/val/addr-in/addr-out values,
           ;; to then be run in reverse order.
           ;;
           ;; Quine generation doesn't come back.  Why?  Seems like we enter generate-and-test
            (eval-list-right-to-lefto e* env store-in addr-in store-out addr-out val)))
        |#

        #|
        ((fresh (x e v addr store^ addr^)
           (== `(set! ,x ,e) exp)
           (== 'void val)       
           (symbolo x)
           (== addr^ addr-out)
           (== (extend-store addr v store^) store-out)
           (not-in-envo 'set! env)
           (lookup-varo x env addr)           
           (eval-exp-right-to-lefto e env store-in addr-in store^ addr^ v)))
        |#
        
        ((fresh (rator rand x body env^ v store^ store^^ addr^ addr^^ new-env new-store new-addr)
           (== `(,rator ,rand) exp)
           (== (extend-env x addr^^ env) new-env)
           (== (extend-store addr^^ v store^^) new-store)
           (== `(s ,addr-in) new-addr)
           (symbolo x)
           ;; we have to be careful here: want to feed the addr-in into the rator evaluation,
           ;; even though store-in is fed into the rand evaluation
           (eval-exp-right-to-lefto rand env store-in addr^ store^ addr^^ v)
           (eval-exp-right-to-lefto rator env store^ addr-in store^^ addr^ `(closure (lambda (,x) ,body) ,env^))
           (eval-exp-right-to-lefto body new-env new-store new-addr store-out addr-out val)))

        ))))

(load "mk.scm")

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

(define eval-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d v-a v-d)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-expo a env v-a)
         (eval-listo d env v-d))))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define list-of-symbolso
  (lambda (los)
    (conde
      ((== '() los))
      ((fresh (a d)
         (== `(,a . ,d) los)
         (symbolo a)
         (list-of-symbolso d))))))


(define evalo
  (lambda (exp val)
    (eval-expo exp '() val)))

(define eval-expo
  (lambda (exp env val)
    (conde
      ((== `(quote ,val) exp)
       (absento 'closure val)
       (not-in-envo 'quote env))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (eval-listo a* env val)))
      ((fresh (x* body)
         (== `(lambda ,x* ,body) exp)
         (== `(closure (lambda ,x* ,body) ,env) val)
         (list-of-symbolso x*)
         (not-in-envo 'lambda env)))
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a*)
         (ext-env*o x* a* env^ res)
         (eval-expo body res val)))
      ((prim-expo exp env val)))))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
      ((== '() x*) (== '() a*) (== env out))
      ((fresh (x a dx* da* env2)
         (== `(,x . ,dx*) x*)
         (== `(,a . ,da*) a*)
         (== `(ext-env ,x ,a ,env) env2)
         (symbolo x)
         (ext-env*o dx* da* env2 out))))))

(define prim-expo
  (lambda (exp env val)
    (conde
      ((boolean-primo exp env val))
      ((null?-primo exp env val))
      ((not-primo exp env val))
      ((car-primo exp env val))
      ((cdr-primo exp env val))
      ((cons-primo exp env val))
      ((if-primo exp env val)))))

(define boolean-primo
  (lambda (exp env val)
    (conde
      ((== #t exp) (== #t val))
      ((== #f exp) (== #f val)))))

(define cons-primo
  (lambda (exp env val)
    (fresh (a d v-a v-d)
      (== `(cons ,a ,d) exp)
      (== `(,v-a . ,v-d) val)
      (not-in-envo 'cons env)
      (eval-expo a env v-a)
      (eval-expo d env v-d))))

(define car-primo
  (lambda (exp env val)
    (fresh (p a d)
      (== `(car ,p) exp)
      (== a val)
      (=/= 'closure a)
      (not-in-envo 'car env)
      (eval-expo p env `(,a . ,d)))))

(define cdr-primo
  (lambda (exp env val)
    (fresh (p a d)
      (== `(cdr ,p) exp)
      (== d val)
      (=/= 'closure a)
      (not-in-envo 'cdr env)
      (eval-expo p env `(,a . ,d)))))

(define not-primo
  (lambda (exp env val)
    (fresh (e b)
      (== `(not ,e) exp)
      (conde
        ((=/= #f b) (== #f val))
        ((== #f b) (== #t val)))         
      (not-in-envo 'not env)
      (eval-expo e env b))))

(define null?-primo
  (lambda (exp env val)
    (fresh (e v)
      (== `(null? ,e) exp)
      (conde
        ((== '() v) (== #t val))
        ((=/= '() v) (== #f val)))
      (not-in-envo 'null? env)
      (eval-expo e env v))))

(define if-primo
  (lambda (exp env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) exp)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (conde
        ((=/= #f t) (eval-expo e2 env val))
        ((== #f t) (eval-expo e3 env val))))))

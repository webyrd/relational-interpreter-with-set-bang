(load "mk.scm")
(load "interp-curried-all-interleavings.scm")
(load "test-check.scm")



(test "defined-value"
  ;; this test should always return the same closure
  (run* (q)
    (evalo
     '(((lambda (_.0) (lambda (_.1) _.1)) (lambda (_.2) _.3))
       ((lambda (_.4) (lambda (_.5) _.6)) (lambda (_.7) _.8)))
     q))
  '((closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
    (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))))

;;; The tests that use quoted constants fail, unless quote is
;;; uncommented.  Alas, uncommenting quote results in
;;; 'infer-undefined-1' not coming back in any reasonable time (didn't
;;; wait for it to finish).
(test "set!-1"
  (run* (q) (evalo '((lambda (y) (set! y (quote 6))) (quote 3)) q))
  '(void void))

(test "set!-2"
  (run* (q) (evalo '((lambda (y) ((lambda (x) y) (set! y (quote 8)))) (quote 3)) q))
  '(8 8 8 8))

(test "set!-undefined-1"
  (run* (q) (evalo '((lambda (x)
                       (((lambda (y) (lambda (z) x)) (set! x (quote 4)))
                        (set! x (quote 5))))
                     (quote 3))
                   q))
  '(5 5 4 4 4 4 5 5))

(time
 (test "prove-undefined-1"
   (run* (x y)
     (fresh (expr)
       (== '((lambda (x)
               (((lambda (y) (lambda (z) x))
                 (set! x (quote 4)))
                (set! x (quote 5))))
             (quote 3))
           expr)
       (=/= x y)
       (evalo expr x)
       (evalo expr y)))
   '((5 4) (5 4) (5 4) (5 4) (5 4) (5 4) (5 4) (5 4) (4 5) (4 5)
     (4 5) (4 5) (4 5) (4 5) (4 5) (4 5) (4 5) (4 5) (4 5)
     (4 5) (4 5) (4 5) (4 5) (4 5) (5 4) (5 4) (5 4) (5 4)
     (5 4) (5 4) (5 4) (5 4))))

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

#|
((lambda (f)
   (f (set! f (lambda (x) f))))
 (lambda (y) y))
|#

#!eof




(test "cons-1"
  (run* (q) (evalo '(cons (quote 3) (quote 4)) q))
  '((3 . 4) (3 . 4)))

(test "quines-1"
  (run 1 (q) (evalo q q))
  '((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 quote)) ((_.0 void)))
     (sym _.0))))




(time (test "infer-lamp-1"
  (run 1 (expr)
    (evalo expr '(I love you))
    (evalo expr '(I love lamp)))
  '???))




;; try to infer an expression that evaluates to both (I love you) and
;; (I love lamp), depending upon evaluation order for application

;; might want to include a trace to track evaluation order for
;; cons/application



(time (run 30 (expr v1 v2)
        (=/= v1 v2)
        (evalo expr v1)
        (evalo expr v2)))
running stats for (run 30 (expr v1 v2) (=/= v1 v2) (evalo expr v1) (evalo expr v2)):
    258 collections
    60691 ms elapsed cpu time, including 135 ms collecting
    60796 ms elapsed real time, including 137 ms collecting
    2163440512 bytes allocated
(((((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
    (lambda (_.2) _.2))
   void (closure (lambda (_.1) _.0) (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
       ((_.0 set!)) ((_.0 void)) ((_.1 closure)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)))
  (sym _.0 _.1 _.2))
 ((((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
    (lambda (_.2) _.2))
   void (closure (lambda (_.1) _.0) (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
       ((_.0 set!)) ((_.0 void)) ((_.1 closure)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)))
  (sym _.0 _.1 _.2))
 ((((lambda (_.0)
      (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
    (lambda (_.4) _.4))
   void
   (closure (lambda (_.2) _.3)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.4 closure)) ((_.4 void)))
  (sym _.0 _.1 _.2 _.4) (absento (closure _.3) (void _.3)))
 ((((lambda (_.0)
      (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
    (lambda (_.4) _.4))
   void
   (closure (lambda (_.2) _.3)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.4 closure)) ((_.4 void)))
  (sym _.0 _.1 _.2 _.4) (absento (closure _.3) (void _.3)))
 ((((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
    (lambda (_.2) _.2))
   void (closure (lambda (_.1) _.0) (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
       ((_.0 set!)) ((_.0 void)) ((_.1 closure)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)))
  (sym _.0 _.1 _.2))
 ((((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
    (lambda (_.2) _.2))
   void (closure (lambda (_.1) _.0) (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
       ((_.0 set!)) ((_.0 void)) ((_.1 closure)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)))
  (sym _.0 _.1 _.2))
 ((((lambda (_.0)
      (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
    (lambda (_.4) _.4))
   void
   (closure (lambda (_.2) _.3)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.4 closure)) ((_.4 void)))
  (sym _.0 _.1 _.2 _.4) (absento (closure _.3) (void _.3)))
 ((((lambda (_.0)
      (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
    (lambda (_.4) _.4))
   void
   (closure (lambda (_.2) _.3)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.4 closure)) ((_.4 void)))
  (sym _.0 _.1 _.2 _.4) (absento (closure _.3) (void _.3)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
    (lambda (_.3) _.3))
   void
   (closure (lambda (_.1) ((lambda (_.2) _.0) _.1))
            (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)))
  (sym _.0 _.1 _.2 _.3))
 ((((lambda (_.0)
      (_.0
       (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
    (lambda (_.3) _.3))
   void
   (closure (lambda (_.1) ((lambda (_.2) _.0) _.1))
            (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)))
  (sym _.0 _.1 _.2 _.3))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
    (lambda (_.3) _.3))
   void
   (closure (lambda (_.1) ((lambda (_.2) _.0) _.1))
            (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)))
  (sym _.0 _.1 _.2 _.3))
 ((((lambda (_.0)
      (_.0
       (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
    (lambda (_.3) _.3))
   void
   (closure (lambda (_.1) ((lambda (_.2) _.0) _.1))
            (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)))
  (sym _.0 _.1 _.2 _.3))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.3 closure)) ((_.3 void)) ((_.5 closure))
       ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.3 closure)) ((_.3 void)) ((_.5 closure))
       ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
    (lambda (_.3) _.3))
   void
   (closure (lambda (_.1) ((lambda (_.2) _.0) _.1))
            (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)))
  (sym _.0 _.1 _.2 _.3))
 ((((lambda (_.0)
      (_.0
       (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
    (lambda (_.3) _.3))
   void
   (closure (lambda (_.1) ((lambda (_.2) _.0) _.1))
            (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)))
  (sym _.0 _.1 _.2 _.3))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4))
                (lambda (_.5) _.6))))))
    (lambda (_.7) _.7))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)) ((_.7 closure))
       ((_.7 void)))
  (sym _.0 _.1 _.2 _.3 _.5 _.7)
  (absento (closure _.4) (closure _.6) (void _.4)
           (void _.6)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) (lambda (_.3) _.4))
                (lambda (_.5) _.6))))))
    (lambda (_.7) _.7))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.2 (s (s z))
                     (ext-env _.1 (s z) (ext-env _.0 z ())))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 lambda))
       ((_.2 void)) ((_.3 closure)) ((_.3 void))
       ((_.5 closure)) ((_.5 void)) ((_.7 closure))
       ((_.7 void)))
  (sym _.0 _.1 _.2 _.3 _.5 _.7)
  (absento (closure _.4) (closure _.6) (void _.4)
           (void _.6)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.0) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure
    (lambda (_.1) ((lambda (_.2) _.0) (lambda (_.3) _.4)))
    (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)) ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.0) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure
    (lambda (_.1) ((lambda (_.2) _.0) (lambda (_.3) _.4)))
    (ext-env _.0 z ())))
  (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 closure))
       ((_.0 lambda)) ((_.0 set!)) ((_.0 void))
       ((_.1 closure)) ((_.1 lambda)) ((_.1 void))
       ((_.2 closure)) ((_.2 void)) ((_.3 closure))
       ((_.3 void)) ((_.5 closure)) ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.3 closure)) ((_.3 void)) ((_.5 closure))
       ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.3 closure)) ((_.3 void)) ((_.5 closure))
       ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.3 closure)) ((_.3 void)) ((_.5 closure))
       ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4)))
 ((((lambda (_.0)
      (_.0
       (set! _.0
             (lambda (_.1)
               ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
    (lambda (_.5) _.5))
   void
   (closure (lambda (_.3) _.4)
            (ext-env _.1 (s z) (ext-env _.0 z ()))))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 set!))
       ((_.0 void)) ((_.1 closure)) ((_.1 lambda))
       ((_.1 void)) ((_.2 closure)) ((_.2 void))
       ((_.3 closure)) ((_.3 void)) ((_.5 closure))
       ((_.5 void)))
  (sym _.0 _.1 _.2 _.3 _.5)
  (absento (closure _.4) (void _.4))))

(define rem-dups
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((member (car ls) (cdr ls)) (rem-dups (cdr ls)))
      (else (cons (car ls) (rem-dups (cdr ls)))))))

;;; expressions, including duplicates
(((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
  (lambda (_.2) _.2))
 ((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
  (lambda (_.2) _.2))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
  (lambda (_.4) _.4))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
  (lambda (_.4) _.4))
 ((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
  (lambda (_.2) _.2))
 ((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
  (lambda (_.2) _.2))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
  (lambda (_.4) _.4))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
  (lambda (_.4) _.4))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4))
              (lambda (_.5) _.6))))))
  (lambda (_.7) _.7))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4))
              (lambda (_.5) _.6))))))
  (lambda (_.7) _.7))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.0) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.0) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5)))


;;; dups removed:
(((lambda (_.0) (_.0 (set! _.0 (lambda (_.1) _.0))))
  (lambda (_.2) _.2))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) (lambda (_.2) _.3)))))
  (lambda (_.4) _.4))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4)) _.1)))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0 (set! _.0 (lambda (_.1) ((lambda (_.2) _.0) _.1)))))
  (lambda (_.3) _.3))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) (lambda (_.3) _.4))
              (lambda (_.5) _.6))))))
  (lambda (_.7) _.7))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.0) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5))
 ((lambda (_.0)
    (_.0
     (set! _.0
           (lambda (_.1)
             ((lambda (_.2) _.2) (lambda (_.3) _.4))))))
  (lambda (_.5) _.5)))

;; In Vicare (evaluating all 7 expressions):
(#<procedure _.0> #<procedure> #<procedure> #<procedure _.0>
  #<procedure> #<procedure _.0> #<procedure _.2>)

;; In Racket:
'(#<void>
  #<void>
  #<void>
  #<void>
  #<void>
  #<void>
  #<void>)









;;; Important: if we aren't consistent in how we thread through the
;;; address counter, we can get goofy answers.  Whether we evaluate
;;; 'rator' before 'rand', or 'rand' before 'rator', we should always
;;; feed through the address the same way (left-to-right, let's say).
;;; Otherwise, different evaluation order can result in the same
;;; closure, *other* than the addresses in the environment being
;;; closed over.  For example, here is the behavior when we *aren't*
;;; being consistent with our threading of the 'addr' counter.

(time
  (run 2 (expr v1 v2)
    (=/= v1 v2)
    (evalo expr v1)
    (evalo expr v2)))
running stats for (run 2 (expr v1 v2) (=/= v1 v2) (evalo expr v1) (evalo expr v2)):
    471 collections
    118566 ms elapsed cpu time, including 341 ms collecting
    118575 ms elapsed real time, including 343 ms collecting
    3946468512 bytes allocated

((((((lambda (_.0) (lambda (_.1) _.1)) (lambda (_.2) _.3))
    ((lambda (_.4) (lambda (_.5) _.6)) (lambda (_.7) _.8)))
   (closure (lambda (_.5) _.6) (ext-env _.4 z ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ())))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 void))
       ((_.1 closure)) ((_.1 void)) ((_.2 closure))
       ((_.2 void)) ((_.4 closure)) ((_.4 lambda))
       ((_.4 void)) ((_.5 closure)) ((_.5 void))
       ((_.7 closure)) ((_.7 void)))
  (sym _.0 _.1 _.2 _.4 _.5 _.7)
  (absento (closure _.3) (closure _.6) (closure _.8)
           (void _.3) (void _.6) (void _.8))) 
 (((((lambda (_.0) (lambda (_.1) _.1)) (lambda (_.2) _.3))
    ((lambda (_.4) (lambda (_.5) _.6)) (lambda (_.7) _.8)))
   (closure (lambda (_.5) _.6) (ext-env _.4 z ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ())))
  (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 void))
       ((_.1 closure)) ((_.1 void)) ((_.2 closure))
       ((_.2 void)) ((_.4 closure)) ((_.4 lambda))
       ((_.4 void)) ((_.5 closure)) ((_.5 void))
       ((_.7 closure)) ((_.7 void)))
  (sym _.0 _.1 _.2 _.4 _.5 _.7)
  (absento (closure _.3) (closure _.6) (closure _.8)
           (void _.3) (void _.6) (void _.8))))


(test "defined-value"
 ;; this test should always return the same closure
 (run* (q)
   (evalo
    '(((lambda (_.0) (lambda (_.1) _.1)) (lambda (_.2) _.3))
      ((lambda (_.4) (lambda (_.5) _.6)) (lambda (_.7) _.8)))
    q))
 '((closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 z ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 z ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 z ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 (s z) ()))
   (closure (lambda (_.5) _.6) (ext-env _.4 z ()))))

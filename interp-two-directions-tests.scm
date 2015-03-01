(load "mk.scm")
(load "interp-two-directions.scm")
(load "test-check.scm")

(time
 (test "quines-left-to-right"
   (run 1 (q)
     (eval-left-to-righto q q))
   '((((lambda (_.0) (list _.0 (list 'quote _.0)))
       '(lambda (_.0) (list _.0 (list 'quote _.0))))
      (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote))
           ((_.0 void)))
      (sym _.0)))))

#!eof

(time
 (test "quines-right-to-left"
   (run 1 (q)
     (eval-right-to-lefto q q))
   '((((lambda (_.0) (list _.0 (list 'quote _.0)))
       '(lambda (_.0) (list _.0 (list 'quote _.0))))
      (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote))
           ((_.0 void)))
      (sym _.0)))))

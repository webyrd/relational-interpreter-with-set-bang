# relational-interpreter-with-set-bang
Relational Scheme interpreter, written in miniKanren, with set!, and supporting different evaluation orders.

These files contain multiple interpreters from my research, in various states of completeness.  All of these interpreters all for subsets of Scheme, and are call-by-value.

Currently the "best" interpreters are in `interp-curried-two-directions-no-shadowing.scm`, which supports curriedn `lambda`/application, `cons`, `quote`, `set!`, and variables.  This file contains two interpreters, `eval-left-to-righto` and `eval-right-to-lefto`, which use left-to-right and right-to-left evaluation, respectively.  Racket uses left-to-right evaluation, while (Petite) Chez Scheme seems to use right-to-left evaluation, so it's easy to test the resulting Scheme programs using these two implementations.

Interesting queries, under Vicare Scheme:

Find a program that evaluates to different values under left-to-right and right-to-left evaluation:

```
> (load "interp-curried-two-directions-no-shadowing.scm")
> (time (run 1 (expr v1 v2)
          (=/= v1 v2)
          (eval-left-to-righto expr v1)
          (eval-right-to-lefto expr v2)))
running stats for (run 1 (expr v1 v2) (=/= v1 v2) (eval-left-to-righto expr v1) (eval-right-to-lefto expr v2)):
    4 collections
    230 ms elapsed cpu time, including 1 ms collecting
    231 ms elapsed real time, including 1 ms collecting
    33265328 bytes allocated
(((((lambda (_.0) (cons _.0 (set! _.0 '_.1))) '_.2)
    (_.2 . void) (_.1 . void))
   (=/= ((_.0 void)) ((_.1 _.2))) (sym _.0)
   (absento (closure _.1) (closure _.2) (void _.1)
     (void _.2))))	  
```

Find a program that evaluates to either `(you)` or `(lamp)`:

```
> (load "interp-curried-two-directions-no-shadowing.scm")
> (time (run 1 (expr)
          (eval-left-to-righto expr '(you))
          (eval-right-to-lefto expr '(lamp))))
running stats for (run 1 (expr) (eval-left-to-righto expr '(you)) (eval-right-to-lefto expr '(lamp))):
    8 collections
    898 ms elapsed cpu time, including 11 ms collecting
    898 ms elapsed real time, including 11 ms collecting
    67423920 bytes allocated
((((lambda (_.0)
     (cons _.0 ((lambda (_.1) '()) (set! _.0 'lamp))))
    'you)
   (=/= ((_.0 void)) ((_.1 void))) (sym _.0 _.1)))
```

Find a program that evaluates to either `(I love you)` or `(I love lamp)`:

```
> (time (run 1 (expr)
          (eval-left-to-righto expr '(I love you))
          (eval-right-to-lefto expr '(I love lamp))))
running stats for (run 1 (expr) (eval-left-to-righto expr '(I love you)) (eval-right-to-lefto expr '(I love lamp))):
    126 collections
    16624 ms elapsed cpu time, including 272 ms collecting
    16673 ms elapsed real time, including 273 ms collecting
    1061261424 bytes allocated
((((lambda (_.0)
     (cons 'I
       (cons 'love
         (cons _.0 ((lambda (_.1) '()) (set! _.0 'lamp))))))
    'you)
   (=/= ((_.0 void)) ((_.1 void))) (sym _.0 _.1)))
```

Or this version of the query, which re-uses inference of the "tricky" `(you)` vs. `(lamp)` subexpressions:

```
> (time (run 1 (full-expr)
          (fresh (tricky-expr left-tricky-value right-tricky-value context-expr full-value)
            (== '(you) left-tricky-value)
            (== '(lamp) right-tricky-value)
            (eval-left-to-righto tricky-expr left-tricky-value)
            (eval-right-to-lefto tricky-expr right-tricky-value)
            (== `((lambda (tricky-expr) ,context-expr) ,tricky-expr) full-expr)
            (eval-left-to-righto full-expr `(I love . ,left-tricky-value))
            (eval-right-to-lefto full-expr `(I love . ,right-tricky-value)))))
running stats for (run 1 (full-expr) (fresh (tricky-expr left-tricky-value right-tricky-value context-expr full-value) (== '(you) left-tricky-value) (== '(lamp) right-tricky-value) (eval-left-to-righto tricky-expr left-tricky-value) (eval-right-to-lefto tricky-expr right-tricky-value) (== `((lambda (tricky-expr) ,context-expr) ,tricky-expr) full-expr) (eval-left-to-righto full-expr `(I love unquote left-tricky-value)) (eval-right-to-lefto full-expr `(I love unquote right-tricky-value)))):
    10 collections
    1263 ms elapsed cpu time, including 6 ms collecting
    1264 ms elapsed real time, including 6 ms collecting
    76016400 bytes allocated
((((lambda (tricky-expr) (cons 'I (cons 'love tricky-expr)))
    ((lambda (_.0)
       (cons _.0 ((lambda (_.1) '()) (set! _.0 'lamp))))
      'you))
   (=/= ((_.0 void)) ((_.1 void))) (sym _.0 _.1)))
```

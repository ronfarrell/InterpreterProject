#lang racket

(require "../chez-init.rkt")
(provide bintree-to-list bintree-add leaf-node interior-node parse-exp unparse-exp)

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left-tree bintree?)
   (right-tree bintree?)))

; I've provide this one as a sample to you.
; It's used by the testcases though  so don't mess with it.
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
      [interior-node (value left right)
                (list value
                      (bintree-to-list left)
                      (bintree-to-list right))]
      [leaf-node (datum)
                 datum])))
                
; Here's the one you need to solve
(define bintree-add
  (lambda (bt num)
    (cond [(equal? (car bt) 'leaf-node) (leaf-node (+ num (cadr bt)))]
          [else (interior-node (cadr bt) (bintree-add (caddr bt) num) (bintree-add (cadddr bt) num))])))
; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (data number?)]
  [lambda-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand (list-of? expression?))]
  [let-exp
   (syms (list-of? symbol?))
   (vals (list-of? expression?))
   (body (list-of? expression?))]
  [let*-exp
   (syms (list-of? symbol?))
   (vals (list-of? expression?))
   (body (list-of? expression?))]
  [letrec-exp
   (syms (list-of? symbol?))
   (vals (list-of? expression?))
   (body expression?)]
  [if-else-exp
   (condition expression?)
   (true expression?)
   (false expression?)]
  [if-exp
   (condition expression?)
   (true expression?)]
  [set!-exp
   (val symbol?)
   (assign expression?)]
  )

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(number? datum) (lit-exp datum)]
      [(list? datum)
       (cond
         [(and (pair? datum) (not (list? datum))) (error 'parse-exp "prase-error")]
         [(eqv? (car datum) 'set!) (if (eqv? (length datum) 3) (set!-exp (2nd datum) (parse-exp (3rd datum))) (error 'parse-exp "parse-error"))]
         [(eqv? (car datum) 'lambda)
          (cond [(< (length datum) 3) (error 'parse-exp "parse-error")]
                [(symbol? (2nd datum)) (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
                [(valid-args? (2nd datum))
                 (lambda-exp (2nd datum)
                   (map parse-exp (cddr datum)))];parsing the rest of the lambda
                [else (error 'parse-exp "parse-error")])]
         [(eqv? (car datum) 'if)
          (cond [(= (length datum) 4)
                 (if-else-exp (parse-exp (car (2nd datum)))
                         (parse-exp (3rd datum))
                         (parse-exp (last datum)))]
                [(= (length datum) 3)
                 (if-exp (parse-exp (2nd datum))
                         (parse-exp (last datum)))])]

         ;lets
         [(eqv? (1st datum) 'let)
          (if (or (not(list? (2nd datum))) (not (= (length datum) 3)))
              (error 'parse-exp "parse-error") ;not valid
              (let-exp (parse-exp (2nd datum)) (parse-exp (3rd datum))));valid
          
          
          ];returning #f is just filler for now
         [(eqv? (1st datum) 'letrec) #f];returning #f is just filler for now
         [(eqv? (1st datum) 'let*) #f];returning #f is just filler for now
         [else (app-exp (parse-exp (1st datum))
                        (map parse-exp (cdr datum)))]
         )]
      [else (error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp ;; We won't get credit without using cases | provide the expression that you are looking for in EXP, that is what cases is searching for
      (var-exp (id) id)
      (if-exp (condition true) (list 'if condition true)))))




;helper procs
(define valid-args?
  (lambda (args)
  (cond
    [(null? args) #t]
    [(symbol? args) #t]
    [(and (list? args) (symbol? (1st args)))
     (valid-args? (cdr args))]
    [else #f])))

; An auxiliary procedure that could be helpful.
(define var-exp?
  (lambda (x)
    (cases expression x
      [var-exp (id) #t]
      [else #f])))

;;--------  Used by the testing mechanism   ------------------

(define-syntax nyi
  (syntax-rules ()
    ([_]
     [error "nyi"])))

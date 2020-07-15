(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.


(define (cons-all first rests)
  ;'replace-this-line
  (map 
    (lambda (arg)
                (cons first arg))
    rests))
 

;You may find that implementing a helper function, cons-all, 
;will be useful for this problem. To implement cons-all, 
;use the built-in map procedure. cons-all takes in an element first
; and a list of lists rests, and adds first to the beginning of each list in rests:
;(map <proc> <lst>)
;Returns a list constructed by calling proc (a one-argument procedure) on each item in lst.

(define (zip pairs)
  ;'replace-this-line
  (append
    (list (map car pairs))
    (list (map cadr pairs))
    ))
  
  ;(cons 
  ;(if (null? pairs)
  ;  nil
  ;  (cons (caar pairs) cons (zip (cdr pairs))) )
  ;(cons
  ;(if (null? pairs)
  ;  ()
  ;  (cons (cadr pairs) cons (zip (cdr pairs))) )
  ;nil))

  ;(cons (map (lambda (x) ())))
  

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17

  (define (helper s x)

    
      (cond
        ((null? s) nil)
        (else (cons(list x (car s)) (helper (cdr s) (+ 1 x) )))
        )
        
    )
  (helper s 0)   

  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
 
  (cond 
    ;((< total (car denoms)) )
    ((null? denoms) nil)
    ((< total 0) nil)
    ((= total 0) '(())  )
    (else 
      (append
        (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
        
        (list-change total (cdr denoms))
        )
      )))


  ; if test cases, if total is smaller than denom, then move on to cdr of list
    ;else two recursive calls:
      ;(total-denom, (denom) 
      ;(total, (cdr denom))
  ;if denom is null return 0 or 1
  ;
  
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
           expr
           ;(let-to-lambda   )

         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ;'replace-this-line
           ;'(lambda (params) (body))
           (cons 'lambda (cons params (let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ;'replace-this-line
           ;(zip values)

           (cons (cons 'lambda (cons (map let-to-lambda (car (zip values))) (let-to-lambda body)) ) (map let-to-lambda (cadr (zip values))))   

                    ;(car (zip values)))
                       ;(map let-to-lambda body)
                        
                            
            ;values (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         ;'replace-this-line
         (map let-to-lambda expr)

         ; END PROBLEM 19
         )))

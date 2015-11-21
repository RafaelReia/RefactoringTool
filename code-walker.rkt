#lang racket/base
(require syntax/parse)
(provide code-walker
         code-walker-non-expanded)
(provide test-visited) ;805 linhas inicio

;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;
(define syntax-list (list)) ;global, oh well 
(define start-line 0)
(define end-line 0)
(define result null)
(define visited #f)
(define (test-visited)
  (display "Code-walker visited ")
  (displayln visited))
;;;;;;;;;; END Definitions ;;;;;;;;;;;;;;;;

;;;;;;;;;; Clean up unwanted stuff
(define (walk-trought program current next previous)
  (if (null? next)
      (list)
      (begin
        (display "Current: ")
        (displayln current)
        (display "PAIR? ")
        (displayln (pair? next))
        (display "Syntax? ")
        (displayln (syntax? next))
        (cond
          [(syntax? next) ;Found syntax, go to there.
           (define aux (syntax-e current))
           (syntax-walker aux (car aux) (cdr aux) null)]
          [else 
           (begin
             (cons current
                   (walk-trought (cdr program)
                                 (cadr program)
                                 (cddr program) 
                                 null)))]))))
(define (go-to-syntax program-structure)
  (displayln "[go-to-syntax]")
  (displayln (pair? program-structure))
  (unless (null? program-structure)
    (walk-trought program-structure (car program-structure) (cdr program-structure) null ))
  (displayln "!!!!!!!!!!!! END OF FILE !!!!!!!!!!!!"))

(define (syntax-walker program current next previous)
  (display "Syntax-walker ")
  (displayln program)
  (define iteration 0)
  (define syntax-ret null)
  (displayln "Syntax-walker")
  (let loop ((program program)
             (current current)
             (next next)
             (previous previous))
    #|If #t must go and visit each syntax node of the program
      Have a way if it is not true to go back in the code.|#
    (cond
      [(pair? next) 
       (if (pair? (cdr program))
           (begin ;hack!
             (when (eq? iteration 1)
               (begin
                 #;(displayln "SET MADE!")
                 (set! syntax-ret program)))
             ;(set! syntax-list (cons syntax-list current))
             (set! iteration (add1 iteration))
             (loop (cdr program) (cadr program) (cddr program) current)
             )
           (begin
             (displayln "!!!!!!!!!!!!!!!! This should not happen !!!!!!!!!!!!!!!!")
             (display "!!!!!!!!!!!!!!!!")
             (displayln (car program)); do stuff
             (display "!!!!!!!!!!!!!!!!")
             (displayln (syntax? (car program)))
             )
           )]
      [(and (not (null? next)) (syntax? next) (not (null? (syntax-e next)))) (loop (syntax-e next) (car (syntax-e next)) (cdr (syntax-e next)) current)]
      [(not (or (pair? next) (syntax? next))) (displayln "$$$$$$$$$$$$$$$ End File $$$$$$$$$$$$$$$")]
      [else (displayln "!!!!!!!!!!!!!!!! Case not Supported !!!!!!!!!!!!!!!!")])) ;this happens on a black file. Find out why.
  (set! syntax-list syntax-ret)
  (explore-nodes syntax-ret))

(define (explore-nodes syntax-list)
  (set! syntax-list-aux syntax-list)
  (displayln "EXPLORING LIST")
  (displayln "[FIND-Everything]")
  (set! result (find-everything syntax-list-aux #'if)))

;;;;;;;;;; Definitions of Search ;;;;;;;;;;;;;
(define syntax-list-aux syntax-list)
(define stack (list))
(define level 0)
(define offset 0)
(define previous-node null)
(define node null)
(define next-node null)
(define (reset-offset)
  (set! offset 0))

;;;;;;;;;;;;;; Important Function ;;;;;;;;;;;;;
(define (find-everything source syntax-wanted)
  (define source-aux source)
  (define source-stack (list))
  (define check-line #t)
  (define result (list))
  (define aux-result null)
  (define aux-result? #t)
  ;; stores everything!!
  (define (selected-search source-aux line-begin line-end)
    (cond [(and (null? source-aux) (null? source-stack))
           (displayln "[Selected-search] End of file")]
          [(null? source-aux)
           ;; checks if it is null
           #;(display "#;Null Found: ")
           #;(displayln source-aux)
           (set! source-aux (car source-stack))
           (set! source-stack (cdr source-stack))
           (selected-search source-aux line-begin line-end)]
          [(pair? source-aux)
           (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
           (set! source-aux (car source-aux))
           (selected-search source-aux line-begin line-end)]
          [(and (syntax? source-aux) (not (pair? (syntax-e source-aux))))
           ;;Compare line numbers
           (display "[Selected-search] Special Line Number: ")
           (display (syntax-line source-aux))
           (display " Syntax: ")
           (displayln source-aux)
           (when aux-result?
             (begin
               (set! aux-result? #f)
               (set! aux-result source-stack)))
           (when (and (syntax-line source-aux) (not (null? source-aux)) (not (null? (syntax-e source-aux))))
             (begin
               (set! aux-result? #f)
               (set! result (cons (syntax-e source-aux) result))))
           (when aux-result?
             (set! aux-result source-stack))
           (set! check-line #f)
           (set! source-aux (car source-stack))
           (set! source-stack (cdr source-stack))
           (selected-search source-aux line-begin line-end)]
          [(syntax? source-aux) ;this shows first! that is good.
           (display "[Selected-search]  [Test]   Line Number: ")
           (displayln (syntax-line source-aux))
           (displayln source-aux)
           (define compare-aux (syntax-line source-aux))
           (if (and (real? compare-aux) (not (<= line-begin (syntax-line source-aux) line-end)))
               (begin
                 
                 (set! source-aux (car source-stack))
                 (set! source-stack (cdr source-stack)))
               (begin
                 (set! source-aux (syntax-e source-aux))
                 (set! check-line #t))) 
           (selected-search source-aux line-begin line-end)]
          [else
           (set! source-aux (car source-stack))
           (set! source-stack (cdr source-stack))
           (selected-search source-aux line-begin line-end)]))
  (displayln "Selected Search:")
  (displayln source)
  (display "start line" )
  (display start-line)
  (display " end line ")
  (displayln end-line)
  (selected-search source start-line end-line)
  (set! result (reverse result))
  (displayln aux-result)
  (displayln result)
  ;result
  aux-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define (code-walker code start end) ;;for the expanded version
  (set! start-line start)
  (set! end-line end)
  (go-to-syntax (syntax-e code))
  ;result
  (displayln (car result))
  ;(read)
  (define aux (get-syntax-aux code start-line end-line))
  (parameterize ((print-syntax-width 9000))
    (displayln "SEND RESULTS")
    (displayln (car result))
    (displayln "next")
    (displayln (cdr (syntax-e aux)))) ;;clean #%app
  (car result))

(define (code-walker-non-expanded code start end)
  (set! start-line start)
  (set! end-line end)
  (displayln (syntax-e code))
  (displayln " NON EXPANDED")
  (define result (get-syntax-aux code start-line end-line))
  #;(define program (cdr (syntax-e (car (syntax-e (cdr (cdr (cdr (syntax-e code))))))))) ;this is a pair
  #;(displayln program)
  #;(set! result (get-syntax-aux program start-line end-line))
  (displayln "result")
  (displayln result)
  #;(syntax-parse result
      #:literals ((not not #:phase -2)) ;; is lst a datum literal??
      [(not (> a b))
       (displayln #'(<= a b))])
  result)

(define (get-syntax-aux program start end)
  (define stop? #f)
  (define source-stack (list))
  (define aux-result null)
  (define (get-syntax program start end)
    (define source-aux program)
    (define check-line #t)
    (define result null)
    (define aux-result? #t)
    (displayln "source-aux is syntax? ")
    (displayln (syntax? source-aux))
    (parameterize ((print-syntax-width 9000))
      (displayln source-aux)
      (displayln source-stack))
    (define (get-next-compare source-aux source-stack)
      ;else says its bigger than the last part of the selection, could be the end of the program either. this happens when there is no next element.
      (displayln "NEXT COMPARE")
      (define aux (+ end 1))
      (parameterize ((print-syntax-width 9000))
        (displayln source-stack)
        (displayln (syntax? source-stack))
        #;(unless (null? source-stack)
            (displayln (car source-stack))))
      (displayln "is it true?")
      (displayln (and (pair? source-stack) (pair? (car source-stack)) (syntax? (car (car source-stack)))))
      (if (and (pair? source-stack) (pair? (car source-stack)) (syntax? (car (car source-stack))))
          (set! aux (syntax-line (car (car source-stack))))
          (when (or (null? source-stack) (and (syntax? (car source-stack)) (null? (syntax-e (car source-stack)))))
            (set! aux -1)))
      (display "Compare result: ")
      (display aux)
      (display " last line: ")
      (displayln end)
      aux)
    (cond [(null? source-aux)
           (displayln "It's null")]
          [stop? (displayln "evaluation stopped")]
          [(pair? source-aux)
           (displayln "It's pair")
           (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
           (set! source-aux (car source-aux))
           (get-syntax source-aux start end)]
          [(syntax? source-aux)
           (define compare-aux (syntax-line source-aux))
           (define next-compare null)
           (if (and (null? source-stack) (syntax? source-aux))
               (begin
                 (parameterize ((print-syntax-width 9000))
                   (displayln source-aux))
                 ;(read)
                 (set! source-aux (syntax-e source-aux)))
               ;(set! source-stack (cdr source-stack))
               ;(get-syntax (syntax-e source-aux) start end))
               
               (begin
                 (parameterize ((print-syntax-width 9000))
                   (displayln "begin source-aux")
                   (displayln source-aux))
                 (set! next-compare (get-next-compare source-aux source-stack))
                 (cond [(and (real? next-compare) (< next-compare 0))
                        ;go deeper
                        (displayln "-1")
                        (set! source-aux (syntax-e source-aux))] 
                       [(not (real? compare-aux)) 
                        ;next one
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       [(>= start next-compare)
                        ;next one
                        (set! source-aux (car source-stack))
                        (set! source-stack (cdr source-stack))]
                       [(> start compare-aux) ;; in the middle, enter
                        (set! source-aux (syntax-e source-aux))] 
                       [(<= start compare-aux end) ;; starts in the selected place, and it is not bigger then the next one.
                        ;(set! source-aux (syntax-e source-aux))
                        (display "FOUND IT! ")
                        (displayln source-aux)
                        (set! stop? #t)
                        (set! aux-result source-aux)]
                       [else
                        (displayln "little else")])))
           (parameterize ((print-syntax-width 9000))
             (displayln "In Syntax before leaving")
             (displayln source-aux)
             (displayln source-stack))
           (get-syntax source-aux start end)]
          [else
           (displayln "BIG ELSE REACHED")
           (set! source-aux (car source-stack))
           (set! source-stack (cdr source-stack))
           (get-syntax source-aux start end)]))
  (get-syntax program start end)
  aux-result)
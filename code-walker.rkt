#lang racket/base
(require syntax/parse)
(provide code-walker
         code-walker-non-expanded);805 linhas inicio

;;;;;;;;;;; Definitions ;;;;;;;;;;;;;;;;;;;
(define start-line 0)
(define end-line 0)
;;;;;;;;;; END Definitions ;;;;;;;;;;;;;;;;
(define (code-walker code start end) ;;for the expanded version
  (set! start-line start)
  (set! end-line end)
  (define aux (get-syntax-aux code start-line end-line))
  (parameterize ((print-syntax-width 9000))
    (displayln (cdr (syntax-e aux)))) ;;clean #%app
  (cdr (syntax-e aux)))

(define (code-walker-non-expanded code start end)
  (set! start-line start)
  (set! end-line end)
  (displayln (syntax-e code))
  (displayln " NON EXPANDED")
  (define result (get-syntax-aux code start-line end-line))
  (displayln "result")
  (displayln result)
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
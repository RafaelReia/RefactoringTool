#lang racket/base
(require racket/list)
(require syntax/parse)
(provide code-walker
         code-walker-non-expanded)
(provide test-visited)

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

;;;;;;;;;; Prepare ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Clean up AST
(define (walk-trought program current next previous)
  (if (null? next)
      (list)
      (begin
        (display "Current: ")
        (displayln current)
        (display "PAIR? ")
        (displayln (pair? next))
        ;(displayln (cdr program-aux))
        (display "Syntax? ")
        (displayln (syntax? next))
        (cond
          [(syntax? next) ;Found syntax, go to there.
           (begin
             (displayln " SYNTAX ")
             (display "DATUM ")
             (displayln (syntax->datum current))
             ;(go-to-syntax (syntax-e current))
             (define aux (syntax-e current))
             (displayln (syntax->datum current))
             (syntax-walker aux (car aux) (cdr aux) null )
             )]
          [else 
           (begin
             #;(displayln "NEXT PAIR")
             (cons current
                   (walk-trought (cdr program)
                                 (cadr program)
                                 (cddr program) 
                                 null)))]))))
(define (go-to-syntax program-structure)
  (displayln "[go-to-syntax]")
  #;(display "[go-to-syntax] program-structure null? ")
  #;(displayln (null? program-structure))
  (displayln (pair? program-structure))
  (unless (null? program-structure)
    (walk-trought program-structure (car program-structure) (cdr program-structure) null ))
  (displayln "!!!!!!!!!!!! END OF FILE !!!!!!!!!!!!")
  )

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
    #;(display "[Syntax-walker]  Current: ")
    #;(displayln current)
    #;(display "[Syntax-walker]  PAIR? ")
    #;(displayln (pair? next))
    #;(display "[Syntax-walker]  Syntax Next? ")
    #;(displayln (syntax? next))
    ;Need a test to know if it is a syntax node of the program.
    #;(display "[Syntax-walker]  Syntax Current ")
    #;(displayln (syntax? current))
    
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
  #;(display "[NEW TEST]  ")
  #;(displayln syntax-ret)
  #;(displayln (syntax->datum syntax-ret))
  (set! syntax-list syntax-ret)
  #;(display "[new test] Syntax-list  ")
  #;(displayln syntax-ret)
  (explore-nodes syntax-ret) ;TODO change this.
  #;(displayln (syntax->datum program)))

#|explore-nodes receives a syntax-list of the first level of the program, avoiding the need to explore every single node of the program.
  
 |#
(define (explore-nodes syntax-list)
  (set! syntax-list-aux syntax-list)
  #|(define (next-node)
    ;check this out
    (displayln "next node")
    (set! level (sub1 level))
    (set! offset (add1 offset)))
  (define (previous-node)
    ;figure out what to do with this.
    (displayln "previous node"))|#
  (displayln "EXPLORING LIST")
  ;(check-next-offset)  
  #;(displayln syntax-list)
  #;(display "[TEST] Car: ")
  #;(displayln (car syntax-list))
  #;(display "[TEST] car + syntax-e: ")
  #;(displayln (syntax-e (car syntax-list-aux)))
  
  (displayln "[FIND-Everything]")
  (set! result (find-everything syntax-list-aux #'if)) ;TODO change this.
  )

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
           (begin
             (displayln "[Selected-search] End of file"))]
          [(null? source-aux)
           (begin
             ;; checks if it is null
             #;(display "#;Null Found: ")
             #;(displayln source-aux)
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (selected-search source-aux line-begin line-end))]
          [(pair? source-aux)
           #;(displayln "Selected-search pair")
           (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
           (set! source-aux (car source-aux))
           (selected-search source-aux line-begin line-end)]
          [(and (syntax? source-aux) (not (pair? (syntax-e source-aux))))
           (begin
             ;;Compare line numbers
             (display "[Selected-search] Special Line Number: ")
             (display (syntax-line source-aux))
             (display " Syntax: ")
             (displayln source-aux)
             (when aux-result?
               (begin
                 (set! aux-result? #f)
                 (set! aux-result source-stack)))
             ;(display "[!!!!!!!!!!!!] Syntax-object")
             ;(displayln source-stack)
             (when (and (syntax-line source-aux) (not (null? source-aux)) (not (null? (syntax-e source-aux))))
               (begin
                 (set! aux-result? #f)
                 (set! result (cons (syntax-e source-aux) result))))
             (when aux-result?
               (set! aux-result source-stack))
             (set! check-line #f)
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (selected-search source-aux line-begin line-end)
             )]
          [(syntax? source-aux) ;this shows first! that is good.
           (begin
             ;(set! source-aux (syntax-e source-aux))
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
             (selected-search source-aux line-begin line-end))]
          [else
           (begin
             #;(displayln "[Find-everything] Selected-search Else reached")
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (selected-search source-aux line-begin line-end))])
    
    
    #;(deep-search source-aux))
  (define (deep-search source-aux)
    ;(display "source-aux ")
    ;(displayln source-aux)
    (cond [(and (null? source-aux) (null? source-stack))
           (begin 
             (display "[Find-everything] End of file "))] 
          [(null? source-aux)
           (begin
             ;; checks if it is null
             #;(display "[Find-everything] Null Found: ")
             (displayln source-aux)
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (deep-search source-aux))] ;;stop?
          [(pair? source-aux)
           (begin
             ;;will be evaluated after
             (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
             (set! source-aux (car source-aux))
             (deep-search source-aux))] ;;car, next evaluated
          [(and (syntax? source-aux) (not (pair? (syntax-e source-aux))))
           (begin
             ;; checks if it is null
             #;(display "[Find-everything] Identifier found Line: ")
             #;(display (syntax-line source-aux))
             #;(display "   Type:  ")
             #;(displayln source-aux)
             
             #;(when (compare-syntax source-aux syntax-wanted)
                 (begin
                   (display "!!!!!!!!!!!!!!!!!!!!!MATCH!!!!!!!!!!!!!!!!!!!!!!!!! ")
                   (displayln source-aux)))
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (deep-search source-aux))
           ]
          [(syntax? source-aux)
           (begin
             (set! source-aux (syntax-e source-aux))
             #;(display "[Syntax Found]")
             (displayln source-aux)
             (deep-search source-aux))]
          [else
           (begin
             (displayln "[Find-everything] [deep-search] Else reached")
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (deep-search source-aux))]))
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
  aux-result
  #;(deep-search source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-next-offset)
  (define aux (+ offset 1))
  (define aux-list syntax-list-aux) ;correct level
  
  (define (check-offset value)
    (if (= value 0)
        #t
        (if (pair? aux-list)
            (begin
              (set! aux-list (cdr aux-list))
              (check-offset (sub1 value))
              )
            #f)))
  (define (check-offset-aux next-offset)
    (if (pair? aux-list)
        #t
        #f))
  #;(displayln (check-offset 2))
  (display "[checkoffset] test")
  (displayln (check-offset-aux aux))
  (displayln (check-offset aux))
  (if (null? aux-list)
      #f
      (check-offset-aux aux))
  )
#|Level increases when entering a syntax object (syntax-e)
    If previous-node is null and level is > 0 you go down a level to the corresponding offset
    If next-node is null It may assume that you finish the syntax object, you can go
 to the next syntax object, by going up a level and increasing the offset. If that is not possible it ends the search.
    Selecting a syntax object increases the level.
    Going to the next syntax object increases the offset.
   |#
(define (go-to-place level offset syntax-list)
  ;this function does not have tests. all checks must be done before.
  (display "[GO-TO-PLACE] level: ")
  (displayln level)
  (display "[GO-TO-PLACE] offset: ")
  (displayln offset)
  (if (= 0 level offset)
      syntax-list
      (cond
        [(and 
          (not (pair? syntax-list)) (syntax? syntax-list)) 
         (begin
           (display "[GO-TO-PLACE] ")
           (displayln syntax-list)
           (go-to-place level offset (syntax-e syntax-list)))]
        [(> level 0) 
         (begin 
           (displayln "[GO-TO-PLACE] Level entered")
           (go-to-place (sub1 level) offset (car syntax-list)))]
        [(> offset 0) (go-to-place level (sub1 offset) (cdr syntax-list))]
        [else 
         (begin
           (displayln "!!!!!!!!!!!!!!!! Case not Supported !!!!!!!!!!!!!!!!")
           (syntax-list))]))) 
(define (select-syntax-object) ;down a level
  ;do checks
  (if  (null? syntax-list-aux) 
       null
       (if (syntax? (car syntax-list-aux)) ;selects the syntax-object to further inspection
           (begin
             (displayln "Selecting syntax")
             (set! stack (cons (car syntax-list-aux) stack ))
             (set! syntax-list-aux (syntax-e (car syntax-list-aux))) ;should it be done in go-to-place?
             (set! level (add1 level))
             (reset-offset)
             (go-to-place level offset syntax-list))
           (begin
             (displayln "[exit-syntax-object] error: It's not a syntax")
             null))))
(define (exit-syntax-object) ;up a level
  (if (> level 0)
      (begin
        (displayln "going up to Parent")
        (set! level (sub1 level))
        (reset-offset) ;reset offset Maybe create function
        (set! stack (first stack))
        (set! syntax-list-aux stack)
        (go-to-place level offset syntax-list))
      (begin
        (displayln "[exit-syntax-object] error: level is already 0")
        null)))

(define (next-syntax-object)
  (displayln "[Get-syntax-object]")
  (if (check-next-offset) 
      ;it only checks the next offset, should it go up one level? 
      (begin
        (displayln "Next Syntax")
        (set! offset (add1 offset))
        (go-to-place level offset syntax-list))
      (begin
        ;(displayln "[exit-syntax-object] error: There is no next syntax (can not increase offset)")
        (if (> level 0)
            (begin 
              (displayln "[Up-a-Level] can not increase offset") 
              (exit-syntax-object)
              (next-syntax-object))
            (begin
              (displayln "[exit-syntax-object] error: can not increase offset (end of the program) ")
              null)))))
(define (previous-syntax-object)
  ;or sub1 in the level and return void/error
  (if (> offset 0)
      (begin
        (displayln "Previous syntax")
        (set! offset (sub1 offset))
        (go-to-place level offset syntax-list))
      (begin
        (if (> level 0)
            (begin 
              (displayln "[Up-a-Level] can not increase offset") 
              (exit-syntax-object)
              (next-syntax-object))
            (displayln "[exit-syntax-object] error: can not decrease offset (start of the program)"))
        null)))
(define (go-deep)
  (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; Search Syntax ;;;;;;;;;;;;;;;;
(define (compare-syntax current syntax-wanted)
  ;Receives 2 syntax objects. create contract? 
  (when (identifier? current)
    (begin
      #|(display "Checking syntax: Current ")
      (display current)
      (display "  Wanted ")
      (displayln syntax-wanted)
      (displayln (free-identifier=? current syntax-wanted))|#
      (free-identifier=? current syntax-wanted))))
(define (find-syntax-object source syntax-wanted)
  (go-deep)
  (define (get-next-syntax-object source)
    (next-syntax-object)) ;FIX-ME ignoring source, using syntax-list
  (let loop ((source source)
             (syntax-wanted syntax-wanted)
             (syntax-tested (get-next-syntax-object source)))
    ;update source!!
    (when (syntax? syntax-tested) ;; Fix-me Never true...
      (begin
        (display "[Find-syntax] Syntax-tested: ")
        (displayln syntax-tested)))
    (cond [(and (identifier? syntax-tested) (compare-syntax syntax-tested syntax-wanted)) #t]
          [ (null? syntax-tested) (displayln "[Find-Syntax] Stopped it's null" )]
          [else (loop source syntax-wanted (get-next-syntax-object source))])))


;;;;;;;;;; Search Patterns ;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define (code-walker code start end)
  #;(define program-aux program-structure)
  (display "start-line ")
  (display start)
  (display " end-line ")
  (displayln end)
  (set! start-line start)
  (set! end-line end)
  (go-to-syntax (syntax-e code))
  (set! visited #t)
  (display "Code-walker visited ")
  (displayln visited)
  ; (save-expanded-program)
  ;(save-expanded-program)
  (displayln "END FILE")
  ;result
  (syntax-parse (car result)
    #:literals(if)
    [(call-with-values (lambda () (if test-expr then-expr else-expr)) print-values) 
     (when #t (equal? (syntax->datum #'(then-expr)) (not (syntax->datum #'else-expr)))
       (displayln (format "~.a" (syntax->datum #'(not test-expr)))))])
  )



(define (code-walker-non-expanded code start end)
  (display "start-line ")
  (display start)
  (display " end-line ")
  (displayln end)
  (set! start-line start)
  (set! end-line end)
  (displayln (syntax-e code))
  ;(go-to-syntax (syntax-e code))
  ;(syntax-walker (syntax-e code) (car (syntax-e code)) (cdr (syntax-e code)) null )
  (displayln " NON EXPANDED")
  (define test (cdr (syntax-e (car (syntax-e (cdr (cdr (cdr (syntax-e code))))))))) ;this is a pair
  (displayln test)
  (displayln (syntax? test))
  (displayln (pair? test))
  (set! visited #t)
  ;(find-everything test #'if)
  (displayln "get-syntax")
  (set! result (get-syntax-aux test start-line end-line))
  ; (save-expanded-program)
  ;(save-expanded-program)
  
  (displayln (car test))
  (displayln "END FILE")
  (displayln result)
  (displayln "$$$$$$$$$$$$$$$ BUG Literals TEST $$$$$$$$$$$$$$$$$$")
  #;(syntax-parse (car test)
      #:literals ((not not #:phase -2)) ;; is lst a datum literal??
      [(not (> a b))
       (displayln #'(<= a b))])
  (displayln (syntax-property-symbol-keys result))
  (displayln (syntax-original? result))
  (displayln (syntax-source result))
 #| (displayln (syntax-source-module result))
  (displayln (syntax-tainted? result))
  (displayln (syntax-tainted? (syntax-protect result)))
  (syntax-disarm result #f)
  (displayln (syntax-tainted? result))|#
  result
  )


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
    (define (get-next-compare source-aux source-stack)
      ;else says its bigger than the last part of the selection, could be the end of the program either. this happens when there is no next element.
      (displayln "NEXT COMPARE")
      (displayln source-stack)
      (displayln (syntax? source-stack))
      (displayln (car source-stack))
      ;(display "source-stack (syntax? (car source-stack) ")
      ;(displayln (syntax? (car source-stack)))
      ;(displayln (pair? (car source-stack)))
      ;(displayln (syntax? (car (car source-stack))))
      (if (and (pair? source-stack) (pair? (car source-stack)) (syntax? (car (car source-stack))))
          (syntax-line (car (car source-stack)))
          (+ end 1))
      
      
      ) 
    
    (cond [(null? source-aux)
           (displayln "It's null")]
          [stop? (displayln "evaluation stopped")]
          [(pair? source-aux)
           (displayln "It's pair")
           (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
           (set! source-aux (car source-aux))
           (displayln source-aux)
           (displayln source-stack)
           (get-syntax source-aux start end)]
          [(syntax? source-aux)
           (display "start ")
           (displayln start)
           (display "end ")
           (displayln end)
           
           (define compare-aux (syntax-line source-aux))
           (define next-compare (get-next-compare source-aux source-stack))
           ;(check-location compare-aux start end)
           (displayln "Test Loop")
           (displayln source-aux)
           (display "compare-aux ")
           (displayln compare-aux)
           (display "next-compare ")
           (displayln next-compare)
           (cond [(not (real? compare-aux)) 
                  (displayln "Not real found")
                  ;next one
                  (set! source-aux (car source-stack))
                  (set! source-stack (cdr source-stack))]
                 #;[(= next-compare end)
                    (set! stop? #t)
                    (set! aux-result source-aux)]
                 [(>= start next-compare)
                  ;next one
                  (displayln "Next Compare")
                  (set! source-aux (car source-stack))
                  (set! source-stack (cdr source-stack))]
                 [(> start compare-aux) ;; in the middle, enter
                  (set! source-aux (syntax-e source-aux))
                  ] 
                 [(<= start compare-aux end) ;; starts in the selected place, and it is not bigger then the next one.
                  (begin
                    ;(set! source-aux (syntax-e source-aux))
                    (display "FOUND IT! ")
                    (displayln source-aux)
                    (set! stop? #t)
                    (set! aux-result source-aux))]
                 [else
                  (displayln "else")])
           (get-syntax source-aux start end)]
          [else
           (displayln "Else reached")
           #;(displayln "[Find-everything] Selected-search Else reached")
           (set! source-aux (car source-stack))
           (set! source-stack (cdr source-stack))
           (get-syntax source-aux start end)]))
  (get-syntax program start end)
  aux-result)

#|
 (cond [(and (null? source-aux) (null? source-stack))
           (begin
             (displayln "[Selected-search] End of file"))]
          [(null? source-aux)
           (begin
             ;; checks if it is null
             #;(display "#;Null Found: ")
             #;(displayln source-aux)
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (selected-search source-aux line-begin line-end))]
          [(pair? source-aux)
           #;(displayln "Selected-search pair")
           (set! source-stack (cons (cdr source-aux) source-stack)) ;;add to stack
           (set! source-aux (car source-aux))
           (selected-search source-aux line-begin line-end)]
          [(and (syntax? source-aux) (not (pair? (syntax-e source-aux))))
           (begin
             ;;Compare line numbers
             (display "[Selected-search] Special Line Number: ")
             (display (syntax-line source-aux))
             (display " Syntax: ")
             (displayln source-aux)
             (when aux-result?
               (begin
                 (set! aux-result? #f)
               (set! aux-result source-stack)))
             ;(display "[!!!!!!!!!!!!] Syntax-object")
             ;(displayln source-stack)
             (when (and (syntax-line source-aux) (not (null? source-aux)) (not (null? (syntax-e source-aux))))
               (begin
                 (set! aux-result? #f)
                 (set! result (cons (syntax-e source-aux) result))))
             (when aux-result?
               (set! aux-result source-stack))
             (set! check-line #f)
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (selected-search source-aux line-begin line-end)
             )]
          [(syntax? source-aux) ;this shows first! that is good.
           (begin
             ;(set! source-aux (syntax-e source-aux))
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
             (selected-search source-aux line-begin line-end))]
          [else
           (begin
             #;(displayln "[Find-everything] Selected-search Else reached")
             (set! source-aux (car source-stack))
             (set! source-stack (cdr source-stack))
             (selected-search source-aux line-begin line-end))])|#



#|(displayln  program-structure)
  #;(for ((sexp (in-list  program-structure)))
      (displayln sexp))
  #;(displayln (syntax? code))
  #;(displayln (identifier? code))
  #;(displayln (syntax-source code))
  #;(displayln (syntax-line code))
  #;(displayln (syntax-column code))
  
  #;(displayln code) ;in theory this is the syntax object of the program
  ;test One layer
  #;(displayln (syntax-e code))
  ;test EVERYTHING
  #;(displayln (syntax->datum code)) |#


#|;midle steps!
  
  (display "[TEST-Middle] Syntax-e car Syntax-List")
  (displayln (syntax-e (car (syntax-list-aux))))
  (display "[TEST-Middle] Syntax-e car cdr + previous: ")
  (displayln (syntax-e (car (cdr (syntax-e (car (syntax-list-aux)))))))
  (display "[TEST-Middle] Syntax-e cdr cdr + previous: ")
  (displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (car (syntax-list-aux))))))))))
  (display "[TEST-Middle] Syntax-e car + previous: ")
  (displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (car (syntax-list-aux))))))))))))
  |#
;test if
#|(displayln "[Exploring nodes] [TEST IF]  ")
  (displayln (car 
              (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))))))))
  (displayln "[Exploring nodes] [TEST IF- NEXT]  ")
  (displayln (car (syntax-e (car 
                             (syntax-e 
                              (cdr 
                               (cdr 
                                (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))))))))
  #;(display "[TEST] FREE-IDENTIFIER=?: ")
  #;(displayln (free-identifier=? (car (syntax-e 
                                      (car 
                                       (syntax-e 
                                        (cdr 
                                         (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))))
                                #'if))
  (displayln "[Test Parts] Begin")
  (display "[Test Parts] first level (-4) ")
  (displayln (syntax-e (car syntax-list-aux)))
  (display "[Test Parts] second level (-3) ")
  (displayln (syntax-e (cdr (syntax-e (car syntax-list-aux)))))
  (display "[Test Parts] third level (-2) ")
  (displayln (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))
  (display "[Test Parts] fourth level (-1) ")
  (displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))
  (display "[Test Parts] last level (0) ")
  (displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))))
  (displayln "[Test Parts] END ")
  
  (displayln "[Test Parts -2] Begin")
  (display "[Test Parts] first level (-4) ")
  (displayln (syntax-e (car (cdr syntax-list-aux))))
  #;(display "[Test Parts] second level (-3) ")
  #;(displayln (syntax-e (cdr (syntax-e (car syntax-list-aux)))))
  #;(display "[Test Parts] third level (-2) ")
  #;(displayln (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))
  #;(display "[Test Parts] fourth level (-1) ")
  #;(displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))
  #;(display "[Test Parts] last level (0) ")
  #;(displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))))
  (displayln "[Test Parts] END ")
  
  (displayln "[Test Parts -3] Begin")
  (display "[Test Parts] first level (-4) ")
  (displayln (syntax-e (car (cdr (cdr syntax-list-aux)))))
  #;(display "[Test Parts] second level (-3) ")
  #;(displayln (syntax-e (cdr (syntax-e (car syntax-list-aux)))))
  #;(display "[Test Parts] third level (-2) ")
  #;(displayln (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))
  #;(display "[Test Parts] fourth level (-1) ")
  #;(displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))
  #;(display "[Test Parts] last level (0) ")
  #;(displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))))
  (displayln "[Test Parts] END ")
  
  (displayln "[Test Parts -4] Begin")
  (display "[Test Parts] first level (-4) ")
  (displayln (syntax-e (car (cdr (cdr (cdr syntax-list-aux))))))
  #;(display "[Test Parts] second level (-3) ")
  #;(displayln (syntax-e (cdr (syntax-e (car syntax-list-aux)))))
  #;(display "[Test Parts] third level (-2) ")
  #;(displayln (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))
  #;(display "[Test Parts] fourth level (-1) ")
  #;(displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))
  #;(display "[Test Parts] last level (0) ")
  #;(displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux)))))))))))))
  (displayln "[Test Parts] END ") 
  
  (displayln "[Test Parts SUPER TEST] Begin")
  (display "[Test Parts] first level (-4) ")
  (displayln (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux))))))))))
  (display "[Test Parts] second level (-3) ")
  (displayln (syntax-e (car (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux)))))))))))))))))
  (define test 
              (syntax-e 
               (car 
                (syntax-e 
                 (car 
                  (syntax-e (cdr (cdr (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux)))))))))))))))))
  (displayln (keyword? test))
  (displayln (syntax? test))
  (displayln (compare-syntax (syntax-e (car (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux)))))))))))))))) #'%app))
  #;(display "[Test Parts] third level (-2) ")
  #;(displayln (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux)))))))))))))))
  #;(display "[Test Parts] fourth level (-1) ")
  #;(displayln (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux))))))))))))))))))
  #;(display "[Test Parts] last level (0) ")
  #;(displayln (syntax-e (car (syntax-e (cdr (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car (cdr (cdr (syntax-e (car (cdr (cdr (cdr syntax-list-aux))))))))))))))))))))
  (displayln "[Test Parts] END ")
  
  #;(display "[TEST-Compare] ")
  #;(displayln (compare-syntax (car (syntax-e 
                                   (car 
                                    (syntax-e 
                                     (cdr 
                                      (cdr (syntax-e (car (cdr (syntax-e (cdr (syntax-e (car syntax-list-aux))))))))))))) #'if))
  
  (display "[TEST-Find-Syntax-Object]  ")
  #;(displayln (find-syntax-object syntax-list-aux #'if))
  (display "[TEST] pair? car+syntax-e: ")
  (displayln (pair? (syntax-e (car syntax-list-aux)))) 
  (select-syntax-object)
  (displayln syntax-list-aux)
  (exit-syntax-object)
  (displayln syntax-list-aux) 
  ;maybe going down to much, mixin between car and syntax-e. must check this.
  ;(displayln (go-to-place 0 2 syntax-list))  
  |#


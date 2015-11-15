#lang racket/base
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         framework
         syntax/parse
         racket/pretty
         "code-walker.rkt")
(provide tool@)
(define not-expanded-program null)
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define refactoring-tool-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-current-tab)
        (inherit register-toolbar-button)
        
        (let ((btn
               (new switchable-button%
                    (label "Refactoring If")
                    (callback (λ (button)
                                (refactoring-syntax
                                 (get-definitions-text)(get-current-tab))))
                    
                    (parent (get-button-panel))
                    (bitmap reverse-content-bitmap))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
    
    (define reverse-content-bitmap
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "blue" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "red" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))
    
    (define (refactoring-syntax text tab)
      ;(message-box "Plugin Test" "         Working       ")
      (define definitions-text-copy 
        (new (class text:basic%
               ;; overriding get-port-name like this ensures
               ;; that the resulting syntax objects are connected
               ;; to the actual definitions-text, not this copy
               (define/override (get-port-name)
                 (send definitions-text get-port-name))
               (super-new))))
      (define definitions-text text)
      (define drs-eventspace (current-eventspace))
      (define the-tab tab)
      (define user-custodian #f)
      (define normal-termination? #f)
      (define cleanup
        (void))
      (define init-proc
        (λ () 
          (set! user-custodian (current-custodian)) 
          (displayln "init done")))
      (define kill-termination
        (λ () 
          (set! user-custodian (current-custodian)) 
          (displayln "kill termination"))
        #;(λ ()
            (unless normal-termination?
              (parameterize ([current-eventspace drs-eventspace])
                (queue-callback
                 (λ ()
                   ;(send the-tab syncheck:clear-highlighting)
                   (cleanup)
                   (custodian-shutdown-all user-custodian)))))))
      (define settings (send definitions-text get-next-settings))
      (displayln "Help")
      ((λ ()
         ;(send the-tab clear-annotations)
         ;(send the-tab reset-offer-kill)
         ;(send the-tab syncheck:clear-highlighting)
         ;(send (send the-tab get-defs) syncheck:init-arrows)
         ((drracket:eval:traverse-program/multiple
           #:gui-modules? #f
           settings
           init-proc
           kill-termination)
          (drracket:language:make-text/pos text
                                           0
                                           (send text last-position))
          (λ (sexp loop) ;this is the "iter"
            ;(void) "syntax-parse-tests.rkt:1:0: read: #lang not enabled in the current context" + close-status-line: status line not open 'drracket:check-syntax:status
            (cond
              [(eof-object? sexp)
               (custodian-shutdown-all user-custodian)]
              ;(custodian-shutdown-all user-custodian)]
              [else
               ;(open-status-line 'drracket:check-syntax:status)
               (displayln sexp)
               (set! not-expanded-program sexp)
               (displayln not-expanded-program)
               (loop)])) 
          #t)))
      (define start-selection (send text get-start-position))
      (define end-selection (send text get-end-position))
      ;;find-line uses location, not position. must convert before!
      (define start-box (box 1))
      (define end-box (box 1))
      (display "Position-location ")
      (send text position-location start-selection #f start-box #t #f #f);Check this!
      (send text position-location end-selection #f end-box #t #f #f);Check this!
      
      (displayln (unbox start-box))
      
      (displayln (unbox end-box))
      
      (define start-line (send text find-line (unbox start-box)))
      (define end-line (send text find-line (unbox end-box)))
      
      (syntax-refactoring void text start-selection end-selection start-line end-line void)
      )
    (define (syntax-refactoring parent text start-selection end-selection start-line end-line binding-aux)
      (define arg null)
      ;; syntax says it's line 2 when here it says 0. adjustment is start-line +2 and end-line +2.
      (display "Start-Selection ")
      (display start-line)
      (display " end-selection ") 
      (displayln end-line)
      
      (define (write-back aux-stx)
        (displayln "WRINTING")
        (parameterize ((print-as-expression #f)
                       (pretty-print-columns 80))
          (displayln (pretty-format (syntax->datum aux-stx))))
        #;(let ([edit-sequence-txts null])
            ;(displayln call)
            ;;start editiing
            #;(begin-edit-sequence)
            ;(displayln "begin")
            #;(send text begin-edit-sequence)
            #;(set! edit-sequence-txts (cons text edit-sequence-txts))
            ;;Delete the text
            #;(send text delete start-selection end-selection)
            ;;write call
            #;(parameterize ((print-as-expression #f)
                             (pretty-print-columns 80))
                (send text insert (pretty-format (syntax->datum aux-stx)) start-selection 'same))
            ;; end Editing
            #;(for ([txt (in-list edit-sequence-txts)])
                (send txt end-edit-sequence))))
      
      #;(syntax-parse (car arg)
          #:literals(if)
          [(call-with-values (lambda () (if test-expr then-expr else-expr)) print-values) 
           (when #t (equal? (syntax->datum #'(then-expr)) (not (syntax->datum #'else-expr)))
             (write-back (format "~.a" (syntax->datum #'(not test-expr)))))])
      
      
      (print-syntax-width 2000)
      (displayln not-expanded-program)
      (displayln (syntax-e not-expanded-program))
      (set! arg (code-walker-non-expanded not-expanded-program (+ 1 start-line) (+ 1 end-line)))
      #;(code-walker expanded-program (+ 1 start-line) (+ 1 end-line))
      (displayln arg)
      ;;; require for template
      #;(syntax-parse arg
        ;#:literals ((if if #:phase 2))
        ;#:datum-literals (if)
        #:literals (if)
        [(if test-expr then-expr else-expr) (syntax->datum #'(not test-expr))])
      
      
      ;;Used format "~.a" to transform into a string, find a better way
      (syntax-parse arg
        #:datum-literals (cons if not > <= >= < and lambda map length list) ;; is lst a datum literal??
        ;#:literals ((>= >= #:phase 0 not #:phase 0iteral > #:phase 0) (~literal < #:phase 0) (~literal <= #:phase 0)  )
        [(not (> a b))
         (write-back #'(<= a b))]
        [(not (<= a b))
         (write-back #'(> a b))]
        [(not (< a b))
         (write-back #'(>= a b))]
        [(not (>= a b))
         (write-back #'(< a b))]
        [(if test-expr then-expr else-expr)
         (begin
           (when (and #t (not (syntax->datum #'then-expr)) (syntax->datum #'else-expr))
             (write-back #'(not test-expr)))
           (when (and #t (syntax->datum #'then-expr) (not (syntax->datum #'else-expr)))
             (write-back #'test-expr)))]
        [(and (< x y) (< v z))
         (when (equal? (syntax->datum #'y) (syntax->datum #'v))
           (write-back #'(< x y z)))]
        [(and (> x y) (> v z))
         (when (equal? (syntax->datum #'y) (syntax->datum #'v))
           (write-back #'(> x y z)))]
        [(cons x (list y v ...))
         (write-back #'(list x y v ...))]
        [(= (length l) 0) (write-back #'(null? l))]
        ;[(= (length l) 1) (write-back #'(singleton? l))] this does not exist?
        ;[(cons x (list y ... v)) (write-back #'(list x y ... v))]
        [(ft (lambda (arg-aux) (ftn arg-aux2)) arg)  #:when (eq? (syntax-e #'arg-aux) (syntax-e #'arg-aux2)) (write-back #'(ft ftn arg))]
        [((lambda (arg-aux) (function arg-aux2)) arg)  #:when (eq? (syntax-e #'arg-aux) (syntax-e #'arg-aux2)) (write-back #'(function art))]))
    (define (phase1) (void))
    (define (phase2) (void))
    (drracket:get/extend:extend-unit-frame refactoring-tool-mixin)))
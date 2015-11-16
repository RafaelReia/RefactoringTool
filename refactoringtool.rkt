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
                   (cleanup)
                   (custodian-shutdown-all user-custodian)))))))
      (define settings (send definitions-text get-next-settings))
      ((λ ()
         ((drracket:eval:traverse-program/multiple
           #:gui-modules? #f
           settings
           init-proc
           kill-termination)
          (drracket:language:make-text/pos text
                                           0
                                           (send text last-position))
          (λ (sexp loop) ;this is the "iter"
            (cond
              [(eof-object? sexp)
               (custodian-shutdown-all user-custodian)]
              [else
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
      (send text position-location start-selection #f start-box #t #f #f);Check this!
      (send text position-location end-selection #f end-box #t #f #f);Check this!
      (define start-line (send text find-line (unbox start-box)))
      (define end-line (send text find-line (unbox end-box)))
      (syntax-refactoring text start-selection end-selection start-line end-line))
    
    (define (syntax-refactoring text start-selection end-selection start-line end-line)
      (define arg null)
      ;; syntax says it's line 2 when here it says 0. adjustment is start-line +2 and end-line +2.
      (define (write-back aux-stx)
        (displayln "WRINTING")
        (parameterize ((print-as-expression #f)
                       (pretty-print-columns 80))
          (send text delete start-selection end-selection)
          (send text insert (pretty-format (syntax->datum aux-stx)) start-selection 'same)
          (displayln (pretty-format (syntax->datum aux-stx)))))
      
      #;(syntax-parse (car arg) ;used for the exapanded program
          #:literals(if)
          [(call-with-values (lambda () (if test-expr then-expr else-expr)) print-values) 
           (when (equal? (syntax->datum #'then-expr) (not (syntax->datum #'else-expr)))
             (write-back (format "~.a" (syntax->datum #'(not test-expr)))))])
      (set! arg (code-walker-non-expanded not-expanded-program (+ 1 start-line) (+ 1 end-line)))
      (displayln arg)
      (syntax-parse arg
        ;#:literals ((if if #:phase 2))
        ;#:datum-literals (if)
        #:datum-literals (if)
        [(if test-expr then-expr else-expr) (write-back #'(not test-expr))]))  
    (define (phase1) (void))
    (define (phase2) (void))
    (drracket:get/extend:extend-unit-frame refactoring-tool-mixin)))
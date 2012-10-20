(declare
 (uses chicken-syntax
       extras
       ports eval
       tcp
       srfi-1
       srfi-4
       srfi-18
       data-structures
       lolevel
       find-extension
       ))

#>
#include <stdio.h>
#include "cocos2d.h"
USING_NS_CC;

extern "C" void cs_init () {
                 printf ("HELLO HELLO HELLO HELLO HELLO HELLO\n");
                fflush (stdout);
                CHICKEN_run ((void*)C_toplevel) ;
                printf ("cocoscheme.scm: CHICKEN_run() done");
                fflush (stdout);
}
<#


(load-verbose #t)
(fprintf (current-error-port) "************************************ 1\n")


;; fix! 
(repository-path "/data/data/com.adellica.cocoscheme/lib/")
(use bind cplusplus-object coops)
;; make dummy bind module so (use bind) works in repl
(module bind *)
(define (bind-adapters#add-adapter . a) #f)


(fprintf (current-error-port) "cocoscheme: will bind\n")
(fprintf (current-error-port) "wish me luck\n")
(print  <c++-object>)

(include "cocoscheme-bind.scm")
(include "cocoscheme-bindhelpers.scm")
(include "cocoscheme-fibers.scm")
(include "cocoscheme-events.scm")
(include "cocoscheme-draw-shapes.scm")

(use tcp)
(define *repl-socket* (tcp-listen 1234))

(define (repl-prompt op)
  (display "@> " op)
  (flush-output op))

(define (repl-loop in-port out-port)
  (let loop ()
    (repl-prompt out-port)
    (handle-exceptions exn
      (begin (print-error-message exn out-port)
             (print-call-chain out-port 4)
             (loop))
      (let ([sexp (read in-port)])
        ;; eof, exit repl loop
        (if (eof-object? sexp)
            (fiber-yield!))
        (with-output-to-port out-port
          (lambda ()
            (with-error-output-to-port
             out-port
             (lambda ()
               (let ([result (eval sexp)])
                 (if (eq? (void) result)
                     (void) ;; don't print unspecified's
                     (begin
                       (write result out-port)
                       (display "\n" out-port)))))))))
      (loop))))


;; check if there are incoming repl connections
;; if so, make a handler and add to the the fiber-queue 
(define (repl-server-dispatch)
  (when (tcp-accept-ready? *repl-socket*)
    (tcp-read-timeout #f)
    (define-values (IN OUT) (tcp-accept *repl-socket*))
    (fiber-new  (string->symbol ;; name fiber with hostname&port
                 (conc "repl@"
                       (let-values (((local-adr remote-adr) (tcp-addresses IN)))
                         remote-adr) ":"
                       (let-values (((local-port remote-port)
                                     (tcp-port-numbers IN)))
                         remote-port)))
                (lambda ()
                  (repl-loop (make-yielding-input-port IN) OUT)))))


(define *update* (lambda () (void)))
(define *handle-events* (lambda () (void)))

(define *director* #f)
(define *scene* #f)
(define-external (c_foo ((instance "CCNode" <CCNode>) root_scene)
                        ((instance "CCDirector" <CCDirector>) director)) void
                        (flush-output)
                        
                        ;; first run! FIX: clean this mess up
                        (if (not *scene*)
                            (add-helper-labels root_scene))
                        (set! *scene* root_scene)
                        (set! *director* director)

                        ;; process any new incomming connections
                        (repl-server-dispatch)
                        ;; yield to other fibers; process all pending
                        ;; repl operations:
                        (fiber-yield!)

                        ;; handle events
                        (*handle-events*)
                        (consume-events!)
                        
                        ;; run user-defined game-loop
                        (handle-exceptions exn
                          (begin (print-error-message exn)
                                 (print-call-chain)
                                 (print "erasing *update* procedure")
                                 (set! *update* (lambda () (void))))
                          (*update*)))


(define *draw* (lambda () (void)))
(define-external (c_draw) void
  (handle-exceptions exn
    (begin (print-error-message exn)
           (print-call-chain)
           (print "erasing *draw* procedure")
           (set! *draw* (lambda () (void))))
    (*draw*)))

;; FIX: make safe (crashes with exceptions)
(define (*callback* sender) (void))
(define-external (c_callback ((instance "CCObject" <CCObject>) sender)) void
  (*callback* sender))


(include "scratch.scm")

(return-to-host)


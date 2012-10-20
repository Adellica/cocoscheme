
#>
// load chicken at load-time (when so file gets loaded)
// probably not very safe in terms of error-handling, but
// it allows us to not worry about any chicken-include-paths
// from the cocos2d project :
namespace {
  struct initializer {
    initializer() {
      CHICKEN_run ((void*)C_toplevel) ;
    }
  };
  static initializer i;
}
<#

(use bind coops cplusplus-object
     extras chicken chicken-syntax)
#>
#include "cocos2d.h"
USING_NS_CC;
<#
 
(use fibers srfi-4)
;; fix! 
(repository-path "/data/data/com.adellica.cocoscheme/lib/")

(include "cocoscheme-bind.scm")
(include "cocoscheme-bindhelpers.scm")


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


(define *scene* #f)

(define-method (getLocation (touch <CCTouch>))
  (let ([x ((foreign-lambda* float (((instance "CCTouch" <CCTouch>) touch))
                        "return(touch->getLocation().x);")
            touch)]
        [y ((foreign-lambda* float (((instance "CCTouch" <CCTouch>) touch))
                        "return(touch->getLocation().y);")
            touch)])
    (f32vector x y)))

(define-method (getDelta (touch <CCTouch>))
  (let ([x ((foreign-lambda* float (((instance "CCTouch" <CCTouch>) touch))
                        "return(touch->getDelta().x);")
            touch)]
        [y ((foreign-lambda* float (((instance "CCTouch" <CCTouch>) touch))
                        "return(touch->getDelta().y);")
            touch)])
    (f32vector x y)))


(define *update* (lambda () (void)))
(define-external (c_foo ((instance "CCNode" <CCNode>) root_scene)) void
  (set! *scene* root_scene)
  (repl-server-dispatch)
  (*update*)
  (fiber-yield!))


(define *draw* (lambda () (void)))
(define-external (c_draw) void
  (handle-exceptions exn
    (begin (print-error-message exn)
           (print-call-chain))
    (*draw*)))


;; TODO support multi-touch
(define *touch-begin* #f)
(define-external (c_touch_begin ((instance "CCTouch" <CCTouch>) touch)) void
  (if *touch-begin*
      (handle-exceptions exn
        (begin (print-error-message exn)
               (print-call-chain))
        (*touch-begin* touch))))

(define *touch-moved* #f)
(define-external (c_touch_moved ((instance "CCTouch" <CCTouch>) touch)) void
  (if *touch-moved*
      (handle-exceptions exn
        (begin (print-error-message exn)
               (print-call-chain))
        (*touch-moved* touch))))




(return-to-host)

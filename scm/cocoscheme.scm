
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

(include "cocoscheme-bind.scm")
(include "cocoscheme-bindhelpers.scm")


(define (repl-prompt)
  (display "@> ")
  (flush-output))

(repl-prompt)

(define (repl-loop port)
  (let loop ()
    (repl-prompt)
    (handle-exceptions
        exn
      (begin (print-error-message exn)
             (print-call-chain (current-output-port) 4)
             (repl-prompt)
             (loop))
      (print (eval (read port)))
      (loop))))

(fiber-new 'repl
           (lambda ()
             (repl-loop (make-yielding-input-port (current-input-port)))))

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

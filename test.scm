
#>
// load chicken at load-time (when so file gets loaded)
// probably not very safe in terms of error-handling, but
// it allows us to not worry about any chicken-include-paths
// from the cocos2d project :)
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
(bind-file "/home/klm/projects/cocos2d/HelloWorld/scm/CCNode.bind.hpp")

(bind* "extern CCNode* bar();")

(use fibers)



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

(define-external (c_foo ((instance "CCNode" <CCNode>) root_scene)) void
  (set! *scene* root_scene)
  (fiber-yield!))


(return-to-host)

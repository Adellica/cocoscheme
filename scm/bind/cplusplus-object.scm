;;;; cplusplus-object.scm


(module cplusplus-object (<c++-object> new delete constructor destructor)

(import scheme chicken)
(use coops)

(define-class <c++-object> () (this))

(define-method (constructor (x <c++-object>) initargs)
  (void))

(define-method (destructor (x <c++-object>))
  (void))

(define (new class . args)
  (let ((obj (make class)))
    (constructor obj args)
    obj))

(define (delete obj)
  (destructor obj))

)



;; cast coops class of cpp-inst to the
;; actual class of the underlying cpp instance
;; pointed to by cpp-inst.
;; if no cpp is found, an error is thrown
(define (dynamic-cast cpp-inst)
  (define coops-instance
    (letrec-syntax
        ([maybe-instance
          ;; return a coops instance, eg <CCNode> based on the
          ;; actual type of the pointer held in cpp-inst.
          ;; if none is found, coops-instance is #f.
          (ir-macro-transformer
           (lambda (x i t)
             (let ([class (cadr x)])
               `(if ((foreign-lambda* bool
                                 (((instance "CCObject" <CCObject>) obj))
                                 ,(conc "return (dynamic_cast<" (i class) "*>(obj) != 0);"))
                     cpp-inst)
                    ,(string->symbol (conc "<" (i class) ">"))
                    #f))))]
         [find-coops-instance
          (syntax-rules ()
            ((_ class1 class2 ...) (or (maybe-instance class1)
                                       (find-coops-instance class2 ...)))
            ((_ class) (maybe-instance class))
            ((_) #f))])
      (find-coops-instance CCSprite
                           CCMenu
                           CCMenuItemImage
                           #| ^ |# CCMenuItem
                           CCLayer
                           #| ^ |# CCNode
                           CCTouch
                           CCObject)))
  (if coops-instance
      ;; make a new class instance (of type coops-instance) and
      ;; copy pointer (much like a cast)
      (make coops-instance 'this (slot-value cpp-inst 'this))
      (error "could not find associated cpp class" cpp-inst)))

(define (CCArray->list array)
  (if array
   (let loop ([n (sub1 (count array))]
              [res '()])
     (if (< n 0)
         res
         (loop (sub1 n)
               (cons (objectAtIndex array n) res))))
   '()))

(define (children node)
  (map dynamic-cast (CCArray->list (getChildren node))))


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

;; TODO support multi-touch
(define-external (c_touch_begin ((instance "CCTouch" <CCTouch>) touch)) void
  (post-event 'touch-begin (getLocation touch)))

(define-external (c_touch_moved ((instance "CCTouch" <CCTouch>) touch)) void
  (post-event 'touch-moved (getLocation touch) (getDelta touch)))

(define-external (c_touch_ended ((instance "CCTouch" <CCTouch>) touch)) void
  (post-event 'touch-ended (getLocation touch)))


(define (fps)
 (/ 1 (getSecondsPerFrame *director*)))

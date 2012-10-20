(use cplusplus-object coops lolevel chickmunk srfi-13)

(include "scratch-truck.scm")
(include "scratch-labels.scm")
;; (define space (space-new))
;; (define (*draw*) #f)
(define (*draw*) #f
  (draw-shapes (space-shapes space))

  (if #f ;; draw cog?
      (map (lambda (body) #f
              (ccDrawCircle (v->CCPoint (vsub (body-get-pos body)
                                              (body-get-centroid body)) ) 3 0 6 #f))
           (space-bodies space ))))


;; #f if not touching, otherwise vector position
(define *touch-down* #f)
(add-handler 'touch-begin
             (lambda (t) (set! *touch-down*  t)))
(add-handler 'touch-ended
             (lambda (t) (set! *touch-down* #f)))

(define (*update*)
   (space-step space (/ 1 120))
   (space-step space (/ 1 120))


  (if truck
      (begin
        (if *touch-down*
            (let ([thrust (if (>= (v.x *touch-down*) 450) -10.0 10.0)])
              (for-each
               (lambda (wheel)
                 (body-set-ang-vel wheel (fp+ (body-get-ang-vel wheel) thrust)))
               (list wf wr))))
        ;; camera follows truck
        (let* ([cv (body-get-pos truck)]
               [camera.x (- 450 (v.x cv))]
               [camera.y (- 300 (v.y cv))])
          (setPosition *scene* camera.x camera.y))))
  
  ;; remove bodies that are off the screen
  (if #f (for-each
          (lambda (body)
            (for-each (lambda (shape) (space-remove-shape space shape)) (body-shapes body))
            (space-remove-body space body))
          (filter (lambda (body)
                    (<= (v.y (body-get-pos body)) 0))
                  (space-bodies space)))) )




;; (define *selection* '())
;; (define *bodies* #f)
;; (define *body* #f)
;; ;;(use chickmunk)
;; ;;(define space (space-new))
;; (define (*touch-begin* t)
;;   (let ([cv (getLocation t)])
;;     (set! *selection* (space-point-query space cv #xFFFF 0))
;;     (set! *bodies* (map shape-get-body *selection* ))
;;     (if (null? *bodies*) #f
;;         (set! *body* (car *bodies*)))
;;     (print "selection: " *selection* " --- " (map (cut shape-get-moment <> #f 0.0001) *selection*))
;; ; (print (body-pos2world) )
;;     ))

;;(define pin-joint (pin-joint-new w1 (space-get-static-body space) (v 0 0) (v 400 400)))
;;(pin-joint-set-dist pin-joint 100)

;; (begin
;;   (body-set-pos w1 (v 400 400))
;;   (body-set-pos w2 (v 400 400))
;;   (body-set-vel w1 (v 0 0))
;;   (body-set-vel w2 (v 0 0))
;;   (body-set-force w1 (v 0 0))
;;   (body-set-force w2 (v 0 0)))

;; (space-add-constraint space pin-joint)
;; (space-remove-constraint space pin-joint)

;; (pin-joint-get-dist pin-joint)
;; (pin-joint-get-anchr1 pin-joint)
;; (pin-joint-get-anchr2 pin-joint)


;; (define (*touch-moved* touch) #f

;;   (let ([delta (getDelta touch)]
;;         [cv (getLocation touch)])
;;    (map (lambda (b) (body-apply-impulse b (vmult delta 5.0) (vsub cv (body-get-pos b)))) *bodies* ))
  

;;   ;; (print "delta: " (getDelta touch))
;;   ;; (setPosition *scene*
;;   ;;              (f32vector-ref (getLocation touch) 0)
;;   ;;              (f32vector-ref (getLocation touch) 1))
  
;;   #|(runAction *scene* (CCMoveBy::create 0 (new <CCPoint>
;;   (f32vector-ref (getDelta touch) 0)
;;   (f32vector-ref (getDelta touch) 1))) )|#
;;   )






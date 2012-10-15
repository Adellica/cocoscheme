(use bind cplusplus-object coops lolevel chickmunk)

(begin
  (define sprite (CCSprite::create "./ground/G000M803.png"))
  (addChild *scene* sprite))

(begin
  (define sprite (list-ref (children *scene*) 0))
  (setOpacity sprite (integer->char 80)))


(runAction sprite (CCRotateBy::create 10 180))

(begin
  (define sprite2 (CCSprite::create "G000M800.png"))
  (addChild sprite sprite2))


(let ([x 3])
  (setPosition sprite (+ 64 (* x 128)) 300))

(runAction *scene* (CCRotateBy::create 2 90))

(setPosition *scene* 0 0)

(setPosition (makef <CCNode> 'this (pointer? (objectAtIndex (getChildren *scene*) 0)))
             100 500)

(map getRotation (children (list-ref (children *scene*) 2)))

(define sprite (list-ref (children *scene*) 1))

(runAction *scene* (CCMoveBy::create 1 (new <CCPoint> -5 -5)))

(define (*touch-moved* touch)

  (print "delta: " (getDelta touch))
  (setPosition *scene*
               (f32vector-ref (getLocation touch) 0)
               (f32vector-ref (getLocation touch) 1))
  
  #|(runAction *scene* (CCMoveBy::create 0 (new <CCPoint>
  (f32vector-ref (getDelta touch) 0)
  (f32vector-ref (getDelta touch) 1))) )|#
  )

(define space
  (nodes->space `(space ((gravity (0 -10)))
                        (body ()
                              (box (vertices ((230 530)
                                              (230 410)
                                              (310 530)))))
                        (body ((pos (350 350)))
                              (circle (radius 50)
                                      (friction 0.5)))
                        (body ((pos (420 230)))
                              (circle (radius 50)
                                      (friction 0.5)))
                        (body ((static 1))
                              (segment (endpoints ((0 150)
                                                   (800 150)))
                                       (friction 0.5))))))


(define poly (car (filter (lambda (sh)
                            (eq? 'poly (shape-get-type sh)))
                          (space-shapes space))))
(define circle (car (filter (lambda (sh)
                            (eq? 'circle (shape-get-type sh)))
                          (space-shapes space))))
(map shape-get-type (list poly circle))

(poly-shape-get-vert poly 0)
(define (poly-shape-get-verts-locative poly)
  (assert (eq? 'poly (shape-get-type poly)))
  
  (let loop ([n (sub1 (poly-shape-get-num-verts poly))]
            [r '()])
   (if (< n 0)
       r
       (loop (sub1 n)
             (cons (poly-shape-get-vert poly n)
                   r)))))

(define (poly-shape-get-world-vertices poly)
  (map vect->list
       (map (lambda (vert-loc)
          (body-local2world (shape-get-body poly)
                            vert-loc))
        (poly-shape-get-verts-locative poly))))

(poly-shape-get-world-vertices poly)
(poly-shape-get-vertices poly)

(define (draw-shape-poly poly)
  (assert (eq? (shape-get-type poly) 'poly))
  (ccDrawSolidPoly (apply f32vector (flatten (poly-shape-get-world-vertices poly)))
                   (poly-shape-get-num-verts poly)
                   1 0 1 0.5))



(define (draw-shape-circle circle)
  (assert (eq? (shape-get-type circle) 'circle))
  (ccDrawCircle (apply new (cons <CCPoint> (vect->list
                                            (vadd
                                             (circle-shape-get-offset circle)
                                             (body-get-pos (shape-get-body circle))))) )
                (circle-shape-get-radius circle)
                (body-get-angle (shape-get-body circle))
                16 #t))

(define (draw-shapes shapes)
  (for-each (lambda (shape)
              (case (shape-get-type shape)
                ((circle) (draw-shape-circle shape))
                ((poly) (draw-shape-poly shape))
                (else (void))))
            shapes))



(define (*draw*) #f)
(define (*draw*) 
  (space-step space (/ 1 60))
  (draw-shapes (space-shapes space)))

;; todo: write this as a space-manager instead

(use chickmunk coops)

(define (pos->CCPoint pos)
  (apply new (cons <CCPoint> pos)))

(define (v->CCPoint v)
  (new <CCPoint> (vect-x v) (vect-y v)))

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

;; draw shapes


(define (draw-shape-poly poly)
  (assert (eq? (shape-get-type poly) 'poly))
  (ccDrawSolidPoly (apply f32vector (flatten (poly-shape-get-world-vertices poly)))
                   (poly-shape-get-num-verts poly)
                   0.5 0.5 1 0.1))

;;(define (draw-shape-circle circle) #f)
(define (draw-shape-circle circle)
  (assert (eq? (shape-get-type circle) 'circle))
  (ccDrawColor4F 1 1 1 1)
  (ccDrawCircle (apply new (cons <CCPoint> (vect->list
                                            (vadd
                                             (circle-shape-get-offset circle)
                                             (body-get-pos (shape-get-body circle))))) )
                (circle-shape-get-radius circle)
                (body-get-angle (shape-get-body circle))
                16 #t))

(define (segment-shape-get-world-vertices segment)
  (map vect->list (map (cut body-local2world (shape-get-body segment) <>)
                       (list (segment-shape-get-a segment)
                             (segment-shape-get-b segment)))))

(define (draw-shape-segment segment)
  (assert (eq? (shape-get-type segment) 'segment))
  (ccDrawColor4F 0.7 0.7 0.7 1)
  (apply ccDrawLine (map pos->CCPoint (segment-shape-get-world-vertices segment))))

(define (draw-shapes shapes)
  (for-each (lambda (shape)
              (case (shape-get-type shape)
                ((circle) (draw-shape-circle shape))
                ((poly) (draw-shape-poly shape))
                ((segment) (draw-shape-segment shape))
                (else (void))))
            shapes))

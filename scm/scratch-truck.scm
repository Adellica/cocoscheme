(use chickmunk)
;; string is a svg path,
;; typically m x1,y2 x2,y2 ..... 
(define (string->path str)
  (map (lambda (str)
         (map string->number (string-split str ","))) 
       (string-split
        (string-filter
         (o not char-alphabetic?)
         str) " ")))

;; path coordinates are relative
;; make them absolute
(define (path->vertices path)
 (let loop ([curr (car path)]
            [result (list (car path))]
            [path (cdr path)])
   (if (null? path)
       (reverse result)
       (let ([pos (map + curr (car path))])
         (loop pos (cons pos result) (cdr path))))))

;; very untidy
;; paths coord 0,0 is top-left
(define (absolute-path path)
  (map (lambda (pos) (list (car pos) (- 100 (cadr pos)))) path))

(define (string->vertices str)
  (absolute-path (path->vertices (string->path str))))


(begin
  (define static-nodes
    (let ([ms (lambda (x1 y1 x2 y2)
                `(segment (friction 0.9)
                          (radius 1.0)
                          (endpoints ((,x1 ,y1)
                                      (,x2 ,y2)))))])
      `(space ((gravity (0 -400))
               (iterations 10))
              (body ((static 1))
                    ,(ms 2 150   760 150)
                    ,(ms 2 150     2 300)
                    ,(ms 2 500   200 500)
                    ,(ms 100 350 300 400)
                    ,(ms 300 400 500 350)
                    ,(ms 500 350 800 500)
                    ,(ms 800 500  1000 450)
                    ,(ms 1000 450 1050 480)
                    ,(ms 1050 480 1100 450)
                    ,(ms 1100 450  2000 500)
                    ,(ms 2000 500  2050 550)
                    ,(ms 2050 550  2100 500)
                    ,(ms 2100 500  2500 500)
                    ,(ms 2800 500  2200 100)
                    ,(ms 2200 100  1700 200)
                    ,(ms 1600 180  1500 180)
                    ,(ms 1500 180  1000 150)
                    ,(ms 1000 150   970 180)
                    ,(ms  970 180   940 150)
                    ,(ms  940 150   910 180)
                    ,(ms  910 180   880 150)
                    ,(ms  880 150   850 180)
                    ,(ms  850 180   820 150)
                    ,(ms  820 150   790 180)
                    ,(ms  790 180   760 150)
                    ;; wall behind starting-position
                    ,(ms  2 500 2 550)
                    ;; "secret" area, going left
                    ,(ms  -200 300 -500 300)
                    ,(ms -500 300 -1000 500)
                    
                    ;; steep hill:
                    ,(ms -1000 500 -2000 1800)
                    ;; jumpable platou 
                    ,(ms -2000 1800 -2500 1700)
                    ;; platou 'fence'
                    ,(ms -2500 1700 -2600 1800)
                    
                    ;; jump platform (you land here after the big jump)
                    ,(ms -1800 1700 -1200 1700)
                    ,(ms -1200 1700 -50 800)
                    ,(ms -50 800 2 550)
                    
                    ))))
  (define space (nodes->space static-nodes)))

(define truck-nodes
  (let ([bpos '(100 600)]
        [f 0.3] [fw 4])
    `((wheel-rear . (body ((pos ,bpos))
                          (circle (radius 20)
                                  (elasticity 0)
                                  (friction ,fw)
                                  (offset (18.6 15)))))
      (wheel-front . (body ((pos ,bpos))
                          (circle (radius 20)
                                  (elasticity 0)
                                  (friction ,fw)
                                  (offset (80 15)))))
      (truck . (body ((id truck)
                      (pos ,bpos))
                     (poly (id bottom) (friction ,f)
                           (vertices ,(string->vertices "m 28.469775,55.676233 6.432735,8.8445 30.252919,-0.2837 6.574601,-8.5608 z")))
                     (poly (id cabin) (friction ,f)
                           (vertices ,(string->vertices "m 72.328529,49.638133 -13.636867,-8.41884 -14.851595,0.28373 -0.993062,7.56761 z")))
                     (poly (id body) (friction ,f)
                           (vertices ,(string->vertices "m 13.760046,53.548333 14.709729,2.1279 43.260255,0 13.928338,-0.4628 0.0233,-4.866 -13.353131,-0.7093 -29.481523,-0.5675 -29.337497,0.8431 z")))
                     (poly (id front) (friction ,f)
                           (vertices ,(string->vertices "m 85.658368,55.213433 8.615049,6.705 -1.844257,-7.4879 -6.7475,-4.0831 z")))
                     (poly (id rear) (friction ,f)
                           (vertices ,(string->vertices "m 3.3550631,50.205533 0.3945653,9.8376 3.152084,0.5054 6.8583336,-7.0002 -0.250537,-3.6346 z"))))))))


(define truck #f)
(define wf #f)
(define wr #f)

(define (reset-space-with-truck)
  (define myspace (nodes->space static-nodes 0.01))
  (space-set-gravity myspace (v 0 -1800))
  (define bobo (map (lambda (spec) (cons (car spec) (caar (space-add myspace (cdr spec) 0.01)))) truck-nodes))

  (set! truck (alist-ref 'truck bobo))
  (set! wf (alist-ref 'wheel-front bobo))
  (set! wr (alist-ref 'wheel-rear bobo))
  
  (define pivot-joint1 (pivot-joint-new truck wr (vadd (v 18.6 15) (v 100 600))))
  (space-add-constraint myspace pivot-joint1)
  
  (define pivot-joint2 (pivot-joint-new truck wf (vadd (v 80 15) (v 100 600))))
  (space-add-constraint myspace pivot-joint2)

  (body-set-ang-vel-limit wr 60)
  (body-set-ang-vel-limit wf 60)
  
  (set! space myspace))

(reset-space-with-truck)

(define (*callback* sender)
  (reset-space-with-truck))

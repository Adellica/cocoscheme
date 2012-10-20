;;(use fibers srfi-4)

(define *Q* (make-queue))

(define-record fiber label proc)
(define-record-printer (fiber x op)
  (fprintf op "#<fiber ~A~A" (fiber-label x) ">"))

(define current-fiber (make-fiber 'main #f))


;; Add current continuation to end of Q-list
;; and execute next fiber
(define (fiber-yield!)
  (call/cc
   (lambda (k)
     (fiber-proc-set! current-fiber k)
     (queue-add! *Q* current-fiber)
     (set! current-fiber (queue-remove! *Q*))
     ((fiber-proc current-fiber) #f))))

;; don't add self to queue (like yield), then run next
(define (fiber-exit!)
  (if (queue-empty? *Q*)
      (signal "cannot exit fibre: no-one else currently running")
      (begin
        (set! current-fiber (queue-remove! *Q*))
        ((fiber-proc current-fiber) #f))))

;; add fiber to thread queue.
;; it should eventually get run after
;; enough fiber-yield!s
(define (fiber-new label proc)
  (queue-add! *Q*
              (make-fiber label
                          (lambda (ignore-return-value)
                            (proc)
                            (fiber-exit!)))))

(define (fiber-list)
  (cons current-fiber (queue->list *Q*)))
(define (fiber-current)
  (fiber-label current-fiber))


(define (make-yielding-input-port port)
  (let ([reader (lambda ()
                  (let loop ()
                    (if (char-ready? port)
                        (read-char port)
                        (begin (fiber-yield!)
                               (loop)))))] )
    (make-input-port reader
                     (lambda () (char-ready? port))
                     (lambda () (close-input-port port)))))

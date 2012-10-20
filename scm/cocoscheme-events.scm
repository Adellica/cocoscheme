
(use srfi-69 data-structures)

(define *events* (make-queue))
(define *event-handlers* (make-hash-table initial: '()))

(define (add-handler event proc)
  (if (and (symbol? event)
           (procedure? proc)
           (hash-table-update! *event-handlers* event (lambda (old-proc) (cons proc old-proc))))
      (void)
      (error "event must be symbol, handler must be procedure" event)))

(define (fire-event . evt-args)
  (if (symbol? (car evt-args))
      (queue-add! *events* evt-args)))

;; event is a list: (event-type arg1 arg2 ...)
;; args will be supplied to handlers
(define (dispatch-event event)
  (let* ([evt-type (car event)]
         [evt-args (cdr event)]
         [handlers (hash-table-ref/default *event-handlers* evt-type '())])
    (let loop ([hs handlers])
      (if (null? hs)
          (void)
          ;; propagate to next handler unless this handler returns #f:
          (if (apply (car hs) evt-args)
              (loop (cdr hs)))))))

(define (consume-events! events)
  (let loop ()
    (if (queue-empty? events)
        (void)
        (begin (dispatch-event (queue-remove! events))
               (loop)))))

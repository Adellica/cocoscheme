;;;; coops-primitive-objects.scm


(module coops-primitive-objects (<list>)

(import scheme chicken)
(use coops)

(define-syntax defprim
  (syntax-rules ()
    ((_ name pred)
     (defprim name pred <primitive-object>))
    ((_ name pred supers ...)
     (begin
       (export name)
       (define-primitive-class name (supers ...) pred)))))

(defprim <immediate> ##sys#immediate?)
 (defprim <boolean> boolean? <immediate>)
 (defprim <eof-object> eof-object? <immediate>)
 (defprim <char> char? <immediate>)

(defprim <record>
  (lambda (x) (and (not (##sys#immediate? x)) (##sys#generic-structure? x))))

(define (number-vector? x)
  (and (not (##sys#immediate? x))
       (##sys#generic-structure? x)
       (memq 
	(##sys#slot x 0)
	'(u8vector 
	  s8vector u16vector s16vector
	  u32vector s32vector
	  f32vector f64vector))))

(defprim <sequence> 
  (lambda (x)
    (or (null? x)
	(pair? x)
	(vector? x)
	(string? x)
	(and (not (##sys#immediate? x))
	     (##sys#generic-structure? x)
	     (memq (##sys#slot x 0)
		   '(u8vector s8vector u16vector s16vector u32vector s32vector
			      f32vector f64vector queue))))))

 (define-class <list> (<sequence>))

 (defprim <null> null? <immediate> <list>)
 (defprim <pair> pair? <list>)
 (defprim <vector> vector? <sequence>)
 (defprim <number-vector> number-vector? <sequence> <record>)
  (defprim <u8vector> (cut ##sys#structure? <> 'u8vector) <number-vector>)
  (defprim <s8vector> (cut ##sys#structure? <> 's8vector) <number-vector>)
  (defprim <u16vector> (cut ##sys#structure? <> 'u16vector) <number-vector>)
  (defprim <s16vector> (cut ##sys#structure? <> 's16vector) <number-vector>)
  (defprim <u32vector> (cut ##sys#structure? <> 'u32vector) <number-vector>)
  (defprim <s32vector> (cut ##sys#structure? <> 's32vector) <number-vector>)
  (defprim <f32vector> (cut ##sys#structure? <> 'f32vector) <number-vector>)
  (defprim <f64vector> (cut ##sys#structure? <> 'f64vector) <number-vector>)
 (defprim <string> string? <sequence>)
 (defprim <char-set> (cut ##sys#structure? <> 'char-set) <sequence> <record>)
 (defprim <symbol> symbol?)
 (defprim <keyword> keyword? <symbol>)

(defprim <number> number?)
 (defprim <integer> integer? <number>)
  (defprim <exact-number> (lambda (x) (and (integer? x) (exact? x))) <integer>)
 (defprim <inexact-number> (lambda (x) (and (number? x) (inexact? x))) <number>)
 (defprim <fixnum> fixnum? <exact-number> <immediate>)
 (defprim <flonum> flonum? <inexact-number>)

(defprim <thread> (cut ##sys#structure? <> 'thread) <record>)
(defprim <mutex> (cut ##sys#structure? <> 'mutex) <record>)
(defprim <condition-variable> (cut ##sys#structure? <> 'condition-variable) <record>)
(defprim <condition> (cut ##sys#structure? <> 'condition) <record>)
(defprim <tcp-listener> (cut ##sys#structure? <> 'tcp-listener) <record>)
(defprim <continuation> (cut ##sys#structure? <> 'continuation) <record>)
(defprim <regexp> (cut ##sys#structure? <> 'regexp) <record>)
(defprim <pointer> (lambda (x) (and (not (##sys#immediate? x)) (##sys#pointer? x))))
(defprim <locative> (lambda (x) (##core#inline "C_i_locativep" x)))
(defprim <promise> (cut ##sys#structure? <> 'promise) <record>)
(defprim <queue> (cut ##sys#structure? <> 'queue) <record> <sequence>)
(defprim <hash-table> (cut ##sys#structure? <> 'hash-table) <sequence> <record>)
(defprim <blob> (lambda (x) (and (not (##sys#immediate? x)) (##core#inline "C_bytevectorp" x))))

(defprim <port> port? <record>)
 (defprim <stream-port> (lambda (x) (and (port? x) (eq? 'stream (##sys#slot x 7)))) <port>)
 (defprim <custom-port> (lambda (x) (and (port? x) (eq? 'custom (##sys#slot x 7)))) <port>)
 (defprim <string-port> (lambda (x) (and (port? x) (eq? 'string (##sys#slot x 7)))) <port>)
 (defprim <tcp-port> (lambda (x) (and (port? x) (eq? 'tcp (##sys#slot x 7)))) <port>)

)

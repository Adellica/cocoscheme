;;;; coops.scm

;; based on:

;;scmobj.scm
;;Object system for Scheme
;;(c) Dorai Sitaram
;;April 13, 1996


(import scheme chicken matchable)
(begin-for-syntax
 (import chicken matchable)
 (use srfi-1))
(use srfi-1 data-structures extras)
(import record-variants)


(define-record-variant coops-instance (unsafe inline unchecked)
  class					; CLASS (instance)
  slots)				; #(SLOT-VALUE1 ...)

(define-constant +standard-class-base-slots+ 4)

(define-constant +standard-class-cpl-slot+ 0)
(define-constant +standard-class-slotnames-slot+ 1)
(define-constant +standard-class-classname-slot+ 2)
(define-constant +standard-class-initthunks-slot+ 3)

(define bootstrapping #t)
(define method-definition-counter 0)

(define (invalidate-caches)
  (set! method-definition-counter (fx+ method-definition-counter 1)))

(define-inline (check-instance x loc)
  (##sys#check-structure x 'coops-instance loc))

(define <standard-class>
  ;;all classes are instances of standard-class
  (make-coops-instance
   #f					; filled below
   '#(()				; #0: CPL
      (class-precedence-list slots classname initthunks) ; #1: slotnames
      <standard-class>			; #2: classname
      ())))				; #3: initthunks

(coops-instance-class-set! <standard-class> <standard-class>)

(define (fast-standard-class-slot c i)
  (let ((slots (coops-instance-slots c)))
    (##sys#slot slots (fx+ (fx- (##sys#size slots) +standard-class-base-slots+) i))))

(define (position x lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) #f)
	  ((eq? x (car lst)) i)
	  (else (loop (cdr lst) (fx+ i 1))))))

(define-syntax (build-cache x r c)
  ;; (build-cache N CLASS SLOTNAME FAIL) 
  (let* ((n (cadr x))
	 (n2 (* n 3))
	 (class (caddr x))
	 (slotname (cadddr x))
	 (fail (cadddr (cdr x)))
	 (%cache (r 'cache))
	 (%index (r 'index))
	 (%class (r 'class))
	 (%slotname (r 'slotname))
	 (%let (r 'let))
	 (%let* (r 'let*))
	 (%if (r 'if))
	 (%fx+ (r 'fx+))
	 (%fxmod (r 'fxmod))
	 (%eq? (r 'eq?))
	 (%quote (r 'quote))
	 (%tmp (r 'tmp))
	 (%begin (r 'begin))
	 (%lambda (r 'lambda))
	 (cache (make-vector (add1 n2) #f)))
    (vector-set! cache n2 0)		; last slot: current index
    `(,%let* ((,%cache (,%quote ,cache))
	      (,%class ,class)
	      (,%slotname ,slotname))
	     ,(let fold ((i 0))
		(if (>= i n)
		    ;; this should be thread-safe: a context-switch can only
		    ;; happen before this code and in the call to FAIL.
		    `(,%let ((,%tmp ,fail)
			     (,%index (##sys#slot ,%cache ,n2)))
			    (##sys#setslot ,%cache ,%index ,%class)
			    (##sys#setslot ,%cache (,%fx+ ,%index 1) ,%slotname)
			    (##sys#setslot ,%cache (,%fx+ ,%index 2) ,%tmp)
			    (##sys#setislot ; bump index
			     ,%cache ,n2
			     (cond-expand
			       (chicken-4.5
				(##core#inline "C_u_fixnum_modulo" (,%fx+ ,%index 3) ,n2))
			       (else
				(##core#inline "C_fixnum_modulo" (,%fx+ ,%index 3) ,n2))))
			    ,%tmp)
		    `(,%if (and (,%eq? (##sys#slot ,%cache ,(* i 3)) ,%class)
				(,%eq? (##sys#slot ,%cache ,(add1 (* i 3))) ,%slotname))
			   (##sys#slot ,%cache ,(+ (* i 3) 2))
			   ,(fold (add1 i))))))))

(define (slot-lookup obj slotname loc)
  (let ((class
	 (coops-instance-class 
	  (ensure coops-instance? obj loc "not an instance" obj))))
    (build-cache
     5 class slotname
     (position
      slotname  
      (fast-standard-class-slot class +standard-class-slotnames-slot+)))))

(define uninitialized (list 'uninitialized-slot))

(define (set-slot-value! obj f v)
  (let* ((obj 
	  (if (procedure? obj)
	      (generic-procedure-instance obj)
	      obj))
	 (i (slot-lookup obj f 'slot-value)))
    (if i
	(##sys#setslot (coops-instance-slots obj) i v)
	(error 'slot-value "slot not found" f obj))))

(define slot-value
  (getter-with-setter
   (lambda (obj f)
     (let* ((obj 
	     (if (procedure? obj)
		 (generic-procedure-instance obj)
		 obj))
	    (i (slot-lookup obj f 'slot-value)))
       (if i
	   (let ((val (##sys#slot (coops-instance-slots obj) i)))
	     (if (eq? val uninitialized)
		 (error 'slot-value "reference to uninitialized slot" f obj)
		 val))
	   (error 'slot-value "slot not found" f obj))))
   set-slot-value!))

(define slot-ref slot-value)		; OBSOLETE (for instance-of foreign type in older chickens)

(define (slot-initialized? obj slotname)
  (let ((i (slot-lookup obj slotname 'slot-initialized?)))
    (if i
	(not (eq? (##sys#slot (coops-instance-slots obj) i) uninitialized))
	(error "slot not found" slotname obj))))

(define (class-of o)
  (if (coops-instance? o)
      (coops-instance-class o)
      (primitive-class-hook o)))

(define-inline (fast-class-of o)
  (if (##sys#structure? o 'coops-instance)
      (##sys#slot o 1)
      (primitive-class-hook o)))

(define make
  (lambda (c . svsv)
    (check-instance c 'make)
    (let* ((slotnames 
	    (fast-standard-class-slot c +standard-class-slotnames-slot+))
	   (sv (make-vector (length slotnames) uninitialized))
	   (i (make-coops-instance c sv)))
      (define (slot-index name)
	(or (position name slotnames)
	    (error "no such slot in instances of given class" name c)))
      (let loop ((svsv svsv))
	(cond ((null? svsv) 
	       (unless bootstrapping (initialize-instance i))
	       i)
	      ((null? (cdr svsv))
	       (error 'make "missing slot value" (car svsv) i))
	      (else
	       (##sys#setslot sv (slot-index (car svsv)) (cadr svsv))
	       (loop (cddr svsv))))))))

(define-syntax make-class
  (syntax-rules ()
    ((make-class superclasses)
     (make-class superclasses ()))
    ((make-class (superclass ...) slots)
     (make-class #f (superclass ...) slots))
    ((make-class name (superclass ...) slots)
     (make-class name (superclass ...) slots <standard-class>))
    ((make-class name (superclass ...) (slot ...) metaclass)
     (let ((%superclasses (list superclass ...)))
       (make metaclass
         'class-precedence-list
	 (delete-duplicates
	  (append-map
	   (lambda (s)
	     (cons s (append (slot-value s 'class-precedence-list) '())))
	   %superclasses))
         'slots
	 (delete-duplicates
	  (append
	   (list 'slot ...)
	   (append-map
	    (lambda (s)
	      (append (slot-value s 'slots) '()))
	    %superclasses)))
	 'classname 'name)))))

(define <primitive-object>
  (make-class <primitive-object> () ()))

(define <procedure>
  (make-class <procedure> (<primitive-object>) ()))

(define <generic-procedure>
  (make-class 
   <generic-procedure>
   (<procedure>)
   (generic-procedure
    add-new-primary-method
    add-new-before-method
    add-new-after-method
    add-new-around-method)))

(define more-specific-method
  (lambda (m1 m2 cc)
    (let loop ((cc1 (car m1)) (cc2 (car m2)) (cc cc))
      (if (null? cc)
        (error "more-specific-method")
        (let ((c1 (car cc1)) (c2 (car cc2)))
          (cond ((eq? c1 c2)
                 (loop (cdr cc1) (cdr cc2) (cdr cc)))
            ((subclass? c1 c2) #t)
            ((subclass? c2 c1) #f)
            (else
              (let ((c (car cc)))
                (let ((cpl (if (eq? c #t)
			       '()
			       (slot-value c 'class-precedence-list))))
                  (let ((i1 (position c1 cpl))
                        (i2 (position c2 cpl)))
                    (if (and i1 i2)
                      (< i1 i2)
                      (error "more-specific-method" m1 m2))))))))))))

(define compute-applicable-methods
  (lambda (list-of-classes method-table)
    (let loop ((methods method-table)
               (the-applicable-methods '()))
      (if (null? methods)
        (map cdr
          (sort! the-applicable-methods
            (lambda (m1 m2)
              (more-specific-method m1 m2 list-of-classes))))
        (loop (cdr methods)
          (let ((method (car methods)))
            (if (every subclass? list-of-classes (car method))
              (cons method the-applicable-methods)
              the-applicable-methods)))))))

(cond-expand 
  (compiling
   (import foreign)
   (foreign-declare #<<EOF
static C_word C_fast_list_eq(C_word l1, C_word l2) {
  while(l1 != C_SCHEME_END_OF_LIST && l2 != C_SCHEME_END_OF_LIST) {
    if(C_u_i_car(l1) != C_u_i_car(l2)) return C_SCHEME_FALSE;
    
    l1 = C_u_i_cdr(l1);
    l2 = C_u_i_cdr(l2);
  }

  return C_SCHEME_TRUE;
}
EOF
) 
   (define-inline (fast-classlists-equal? l1 l2)
     (##core#inline "C_fast_list_eq" l1 l2)))
  (else 
   (define fast-classlists-equal? equal?)))

(define (fast-cache-check cache classlist make-effective-method)
  ;; Sections marked (*) must be atomic
  (let ((cached-classlist (##sys#slot cache 0))) ; *
    (cond ((and cached-classlist		 ; *
		(eq? method-definition-counter (##sys#slot cache 2)) ; *
		(fast-classlists-equal? cached-classlist classlist)) ; *
	   (##sys#slot cache 1))	; *
	  (else
	   (let ((effective-method (make-effective-method)))
	     (##sys#setslot cache 0 classlist)	      ; *
	     (##sys#setslot cache 1 effective-method) ; *
	     (##sys#setislot cache 2 method-definition-counter) ; *
	     effective-method)))))

(define (create-generic-procedure name specialized-args)
  (let ((%primary-method-table '())
	(%before-method-table '())
	(%after-method-table '())
	(%around-method-table '())
	(%cache (vector #f #f #f)))

    (make
     <generic-procedure>

     'generic-procedure
     (lambda %args
       (let ((%arg-classes
	      (let loop ((sargs specialized-args)
			 (args %args))
		(cond ((null? sargs) '())
		      ((null? args)
		       (error 
			name
			"missing specialized argument in generic procedure call"
			(strip-syntax (car sargs))))
		      (else
		       (cons (fast-class-of (car args)) 
			     (loop (cdr sargs) (cdr args))))))))
	 (define (make-effective-method)
	   (let ((%applicable-primary-methods
		  (compute-applicable-methods %arg-classes
					      %primary-method-table))
		 (%applicable-before-methods
		  (compute-applicable-methods %arg-classes
					      %before-method-table))
		 (%applicable-after-methods
		  (reverse
		   (compute-applicable-methods %arg-classes
					       %after-method-table)))
		 (%applicable-around-methods
		  (compute-applicable-methods %arg-classes
					      %around-method-table)))
	     (lambda (%args)
	       (let ((%primary-method-called #f)
		     (%applicable-primary-methods %applicable-primary-methods)
		     (%applicable-around-methods %applicable-around-methods))
		 (letrec
		     ((%next-method?
		       (lambda ()
			 (if %primary-method-called
			     (not (null? %applicable-primary-methods))
			     (or (not (null? %applicable-around-methods))
				 (not (null? %applicable-primary-methods))))))
		      (%call-next-method
		       (lambda ()
			 (cond (%primary-method-called
				(apply
				 (let ((m (car %applicable-primary-methods)))
				   (set! %applicable-primary-methods
				     (cdr %applicable-primary-methods))
				   m)
				 %next-method?
				 %call-next-method
				 %args))
			       ((not (null? %applicable-around-methods))
				(apply
				 (let ((m (car %applicable-around-methods)))
				   (set! %applicable-around-methods
				     (cdr %applicable-around-methods))
				   m)
				 %next-method?
				 %call-next-method
				 %args))
			       ((null? %applicable-primary-methods)
				(error name "no method defined for given argument classes" %arg-classes))
			       (else
				(set! %primary-method-called #t)
				(for-each
				 (lambda (%before-method)
				   (apply %before-method %args))
				 %applicable-before-methods)
				(call-with-values
				    (lambda ()
				      (apply
				       (let ((m (car %applicable-primary-methods)))
					 (set! %applicable-primary-methods
					   (cdr %applicable-primary-methods))
					 m)
				       %next-method?
				       %call-next-method
				       %args))
				  (lambda %res
				    (for-each
				     (lambda (%after-method)
				       (apply %after-method %args))
				     %applicable-after-methods)
				    (apply values %res))))))))
		   (%call-next-method))))))
	 (cond-expand
	   (no-cache
	    ((make-effective-method) %args))
	   (else
	    ((fast-cache-check %cache %arg-classes make-effective-method)
	     %args)))))

     'add-new-primary-method
     (lambda (specializer-classes m)
       (unless (method-defined? specializer-classes m %primary-method-table)
	 (set! %primary-method-table
	   (cons (cons specializer-classes m)
		 %primary-method-table))
	 (vector-set! %cache 0 #f))
       #t)

     'add-new-before-method
     (lambda (specializer-classes m)
       (unless (method-defined? specializer-classes m %before-method-table)
	 (invalidate-caches)
	 (set! %before-method-table
	   (cons (cons specializer-classes m)
		 %before-method-table))
	 (vector-set! %cache 0 #f))
       #t)

     'add-new-after-method
     (lambda (specializer-classes m)
       (unless (method-defined? specializer-classes m %after-method-table)
	 (invalidate-caches)
	 (set! %after-method-table
	   (cons (cons specializer-classes m)
		 %after-method-table))
	 (vector-set! %cache 0 #f))
       #t)

     'add-new-around-method
     (lambda (specializer-classes m)
       (unless (method-defined? specializer-classes m %around-method-table)
	 (invalidate-caches)
	 (set! %around-method-table
	   (cons (cons specializer-classes m)
		 %around-method-table))
	 (vector-set! %cache 0 #f))
       #t))))

(define (method-defined? specializer-classes m mtable)
  (any (lambda (c)
	 (and (every eq? specializer-classes (car c))
	      (begin
		(invalidate-caches)
		(set-cdr! c m)
		#t)))
       mtable))

(define subclass?
  (lambda (c1 c2)
    (cond ((eq? c1 c2) #t)
          ((eq? c1 #t) #f)
          ((eq? c2 #t) #t)
          ((memq c2 (slot-value c1 'class-precedence-list)) #t)
          (else #f))))

(define (primitive-class-hook x)
  (if (procedure? x)
      <procedure>
      #t))

(define (register-primitive-class pred class)
  (set! primitive-class-hook 
    (let ((old primitive-class-hook))
      (lambda (x) (if (pred x) class (old x))))))

(define-syntax define-primitive-class
  (syntax-rules ()
    ((_ name pred)
     (define-primitive-class name (<primitive-object>) pred))
    ((_ name supers pred)
     (begin
       (define-class name supers)
       (register-primitive-class pred name)))))

(define (class-name class)
  (slot-value class 'classname) )

(define (generic-procedure? x)
  (and (procedure? x)
       (##sys#lambda-decoration
	x
	generic-procedure-instance-slot?)
       #t))

(define (generic-procedure-instance-slot? x)
  (and (pair? x) (eq? '##coops#generic (car x))))

(define (funcallable-generic-procedure proc obj)
  (##sys#decorate-lambda
   proc
   generic-procedure-instance-slot?
   (lambda (p i)
     (##sys#setslot 
      p i
      (cons 
       '##coops#generic
       obj))
     p)))

(define (generic-procedure-instance proc)
  (let ((dec (##sys#lambda-decoration
	      proc
	      generic-procedure-instance-slot?)))
    (if dec
	(cdr dec)
	(error "procedure is not generic" proc))))

(define-syntax make-generic-procedure
  (syntax-rules ()
    ((_ x ...)
     (let* ((%gp (create-generic-procedure #f (list 'x ...)))
            (%gproc (funcallable-generic-procedure 
		     (slot-value %gp 'generic-procedure)
		     %gp)))
       %gproc))))

(define-syntax make-generic-procedure/name
  (syntax-rules ()
    ((_ name x ...)
     (let* ((%gp (create-generic-procedure 'name (list 'x ...)))
            (%gproc (funcallable-generic-procedure 
		     (slot-value %gp 'generic-procedure)
		     %gp)))
       %gproc))))

(define-syntax (call-next-method-lambda x r c)
  (let ((llist (cadr x))
	(body (cddr x)))
    `(,(r 'lambda)
      (next-method? call-next-method . ,llist)
      ,@body)))

(define-syntax define-method
  (syntax-rules (primary: before: after: around: #!optional #!rest #!key)

    ((define-method 1 gp qlfr args . body)
      (define-method 2 gp qlfr args
        () ;;specialising args
        () ;;non-specialising args
        . body))

    ((define-method 2 gp qlfr ((x c) . args) (sa ...) () . body)
      (define-method 2 gp qlfr args (sa ... (x c)) ()  . body))
    ((define-method 2 gp qlfr (x . args) sargs (nsa ...) . body)
      (define-method 2 gp qlfr args sargs (nsa ... x)  . body))
    ((define-method 2 gp qlfr () sargs nsargs . body)
      (define-method 3 gp qlfr sargs nsargs . body))
    ((define-method 2 gp qlfr (#!optional . r) sargs (nsa ...) . body)
      (define-method 3 gp qlfr sargs (nsa ... #!optional . r) body))
    ((define-method 2 gp qlfr (#!rest . r) sargs (nsa ...) . body)
      (define-method 3 gp qlfr sargs (nsa ... #!rest . r) body))
    ((define-method 2 gp qlfr (#!key . r) sargs (nsa ...) . body)
      (define-method 3 gp qlfr sargs (nsa ... #!key . r) body))
    ((define-method 2 gp qlfr r sargs (nsa ...) . body)
      (define-method 3 gp qlfr sargs (nsa ... . r) . body))

    ((define-method 3 gp primary: ((x c) ...) nsargs . body)
     (begin
       (ensure-generic-procedure (x ...) gp)
       ((slot-value gp 'add-new-primary-method)
	(list c ...)
	(call-next-method-lambda (x ... . nsargs) . body))))

    ((define-method 3 gp before: ((x c) ...) nsargs . body)
     (begin
       (ensure-generic-procedure (x ...) gp)
       ((slot-value gp 'add-new-before-method)
	(list c ...)
	(lambda (x ... . nsargs) . body))))

    ((define-method 3 gp after: ((x c) ...) nsargs . body)
     (begin
       (ensure-generic-procedure (x ...) gp)
       ((slot-value gp 'add-new-after-method)
	(list c ...)
	(lambda (x ... . nsargs) . body))))

    ((define-method 3 gp around: ((x c) ...) nsargs . body)
     (begin
       (ensure-generic-procedure (x ...) gp)
       ((slot-value gp 'add-new-around-method)
	(list c ...)
	(call-next-method-lambda (x ... . nsargs) . body))))

    ((define-method (gp primary: . args) . body)
      (define-method 1 gp primary: args . body))
    ((define-method (gp before: . args) . body)
      (define-method 1 gp before: args . body))
    ((define-method (gp after: . args) . body)
      (define-method 1 gp after: args . body))
    ((define-method (gp around: . args) . body)
      (define-method 1 gp around: args . body))
    ((define-method (gp . args) . body)
      (define-method 1 gp primary: args . body))))

(define-syntax define-generic
  (syntax-rules (setter)
    ((_ ((setter name) . args))
     (begin
       (ensure-generic-procedure args name)
       (set! (setter name) 
	 (make-generic-procedure/name (setter name) . args))))
    ((_ (name . args))
     (begin
       (register-generic-procedure name)
       (define name 
	 (getter-with-setter
	  (make-generic-procedure/name name . args)
	  (make-generic-procedure/name (setter name) . args)))))))

(define-syntax (ensure-generic-procedure x r c)
  (let* ((args (cadr x))
	 (name (caddr x))
	 (rname 
	  (if (and (pair? name)
		   (= (length name) 2)
		   (c 'setter (car name))) 
	      (cadr name)
	      name))
	 (m (##sys#current-module))
	 (exists
	  (and (symbol? rname)
	       (or (and-let* ((a (assq rname (##sys#current-environment))))
		     (symbol? (cdr a))) ; value binding?
		   (eq? (get rname '##coops#generic) 
			(or m #t))))))
    (cond (exists (r '(void)))
	  (else
	   (##sys#notice "implicitly defining generic-procedure" name)
	   `(,(r 'define-generic) (,rname ,@args))))))

(define-syntax (register-generic-procedure x r c)
  (let ((name (cadr x)))
    (when (symbol? name)
      (put! name '##coops#generic (or (##sys#current-module) #t)))
    (r '(void))))

(define-generic (print-object obj))

(define (default-print-method obj #!optional (out (current-output-port)))
  (let ((class (class-of obj)))
    (if (eq? class #t)
	(display obj out)
	(fprintf out "#<coops instance of `~a'>" (class-name class)))))

(define-method (print-object (obj #t) #!optional (out (current-output-port)))
  (default-print-method obj out))

(define-method (print-object (class <standard-class>) #!optional (out (current-output-port)))
  (fprintf out "#<coops standard-class `~a'>" (class-name class)))

(define-record-printer (coops-instance obj out)
  (handle-exceptions ex
      (begin
	(display "#<no print-method defined for: " out)
	(default-print-method obj out)
	(display ">" out))
    (print-object obj out) ) )

(define <standard-object>
  (make-class
   <standard-object>
   ()
   ()))

(define-generic (initialize-instance obj))

(define-method (initialize-instance (obj #t))
  (initialize-slots! obj))

(define (initialize-slots! obj)
  (let ((class (class-of obj)))
    (when (slot-initialized? class 'initthunks)
      (for-each
       (lambda (init)
	 (let ((slot (car init))
	       (proc (cdr init)))
	   (when (not (slot-initialized? obj slot))
	     (set-slot-value! obj slot (proc)))))
       (slot-value class 'initthunks)))))

(define (add-initthunks class inits)
  (set-slot-value!
   class
   'initthunks
   (append 
    inits
    (let walk ((cpl (slot-value class 'class-precedence-list)))
      (if (null? cpl)
	  '()
	  (let ((class (car cpl)))
	    (if (slot-initialized? class 'initthunks)
		(append 
		 (slot-value class 'initthunks)
		 (walk (cdr cpl)))
		(walk (cdr cpl)))))))))

(define-syntax (define-class x r c)
  (let ((slotnames '())
	(initforms '())
	(meta '())
	(%begin (r 'begin))
	(%define (r 'define))
	(%make-class (r 'make-class))
	(%void (r 'void))
	(%cons (r 'cons))
	(%quote (r 'quote))
	(%lambda (r 'lambda))
	(%list (r 'list))
	(%define-method (r 'define-method))
	(%set! (r 'set!))
	(%add-initthunks (r 'add-initthunks))
	(%setter (r 'setter))
	(%<standard-object> (r '<standard-object>))
	(%slot-value (r 'slot-value))
	(%set-slot-value! (r 'set-slot-value!)))
    (define (genclass name supers slotnames meta)
      `(,%define 
	,name
	(,%make-class ,name ,supers ,slotnames ,@meta)))
    (define (genaccessors name slots)
      (let loop ((slots slots))
	(if (null? slots)
	    `(,%void)
	    (let ((slot (car slots)))
	      `(,%begin
		,(genaccessors1 name slot)
		,(loop (cdr slots)))))))
    (define (genaccessors1 classname slot)
      (match slot
	((name init)
	 (genaccessors1 classname `(,name initform: ,init)))
	((name options ...)
	 (set! slotnames (cons name slotnames))
	 (genaccessors2 classname name options))
	((? symbol?)
	 (genaccessors1 classname (list slot)))
	(_ (syntax-error 'define-class "invalid slot specification" slot))))
    (define (genaccessors2 classname slotname options)
      (let loop ((options options))
	(match options
	  (() `(,%void))
	  (('reader: name . more)
	   `(,%begin
	     (,%define-method 
	      (,name (x ,classname))
	      (,%slot-value x ',slotname))
	     ,(loop more)))
	  (('writer: name . more)
	   `(,%begin
	     (,%define-method 
	      (,name (x ,classname) y)
	      (,%set-slot-value! x ',slotname y))
	     ,(loop more)))
	  (('accessor: name . more)
	   `(,%begin
	     (,%define-method 
	      (,name (x ,classname))
	      (,%slot-value x ',slotname))
	     (,%define-method 
	      ((,%setter ,name) (x ,classname) y)
	      (,%set-slot-value! x ',slotname y))
	     ,(loop more)))
	  (('initform: form . more)
	   (set! initforms (alist-cons slotname form initforms))
	   (loop more))
	  ((opt . _)
	   (syntax-error 'define-class "unknown slot option" opt classname)))))
    (define (process-options opts)
      (match opts
	(('metaclass: mc . more)
	 (set! meta (list mc))
	 (process-options more))
	((opt _)
	 (syntax-error 'define-class "unknown class option" opt classname))
	((opt)
	 (syntax-error 'define-class "slot-option without value" opt classname))
	(() #f)))
    (let loop ((x (cdr x)))
      (match x
	((name () slots options ...)
	 (loop `(,name (,%<standard-object>) ,slots ,@options)))
	((name supers slots options ...)
	 (let ((acc (genaccessors name slots)))
	   (process-options options)
	   `(,%begin
	     ,(genclass name supers (reverse slotnames) meta)
	     (,%add-initthunks
	      ,name
	      (,%list ,@(map (lambda (iform)
			       `(,%cons 
				 (,%quote ,(car iform))
				 (,%lambda () ,(cdr iform))))
			     initforms)))
	     ,acc)))
	((name supers)
	 (loop `(,name ,supers ())))
	((name)
	 (loop `(,name () ())))
	(_ (syntax-error 'define-class "invalid class definition" `(define-class ,@x)))))))

(set! bootstrapping #f)

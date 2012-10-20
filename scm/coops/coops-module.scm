;;;; coops-module.scm - module wrapper for coops


(declare
  (no-bound-checks)
  ;;XXX this appears to be broken: with chicken 4.5.1 coops 0.5 crashes for
  ;; "(mx +)" in tests/tests.scm.
  ;(no-procedure-checks-for-toplevel-bindings)
  (fixnum) )

(module coops (slot-value
	       slot-ref			; OBSOLETE
	       slot-initialized?
	       <standard-class>
	       <standard-object>
	       make
	       make-class
	       generic-procedure?
	       (make-generic-procedure
		create-generic-procedure
		funcallable-generic-procedure)
	       (make-generic-procedure/name
		create-generic-procedure
		funcallable-generic-procedure)
	       define-primitive-class
	       register-primitive-class
	       <primitive-object>
	       <procedure>
	       <generic-procedure>
	       subclass?
	       class-of
	       class-name
	       initialize-instance
	       (define-class set-slot-value! add-initthunks)
	       define-generic
	       define-method
	       print-object)

(import chicken)			; for `include'
(include "coops.scm")

)

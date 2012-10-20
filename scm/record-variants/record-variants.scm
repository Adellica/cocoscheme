;; Copyright (c) 2009 Jim Ursetto.  All rights reserved.
;; BSD license at end of file.

;;; (define-record-variant name-spec variant-spec slot1 slot2 ...)

;; name-spec := (variant-name original-name) | variant-name
;; variant-spec := (variant-type ...)
;; variant-type := unsafe | unchecked | inline

;; Defines alternate accessor procedures to the existing record
;; {{original-name}} according to {{variant-spec}}.  The accessors
;; will be defined using {{variant-name}}, as if
;; (define-record variant-name slot1 slot2 ...) had been invoked,
;; but they will operate on records of type {{original-name}}.

;; Variant type may be one of:
;; * inline, so procedure definitions use {{define-inline}};
;; * unchecked, so accessors do not check the record type;
;; * unsafe, so accessors use {{##sys#slot}} and {{##sys#setslot}}
;;   instead of the safe {{block-ref!}} and {{block-set!}}
;; and any combination of {{variant-type}} is allowed in {{variant-spec}}.

;; A constructor, {{make-VARIANT-NAME}}, is defined to create a record
;; of the original type.  If you are defining a variant on an existing
;; record, this is here essentially for completeness, as {{unsafe}}
;; and {{unchecked}} don't have any effect on the constructor --
;; though {{inline}} will inline it.  For new records the constructor
;; is naturally required.

;; Additionally, one new procedure over define-record is created:

;; (check-VARIANT-NAME x): Checks that x is of the corresponding record type
;; and returns x if so; otherwise throws an error.  When compiled
;; in unsafe mode no check is performed, regardless of variant-type.

;; Note that (define-record-variant foo () x y) is equivalent to
;; (define-record foo x y) except that a check-foo procedure
;; will be generated.

;;; (define-record-type-variant name-spec variant-spec pred-spec constructor field-spec)

;; name-spec := (variant-name original-name) | variant-name
;; variant-spec := (variant-type ...)
;; variant-type := unsafe | unchecked | inline
;; pred-spec := (predicate checker) | (predicate) | predicate
;; constructor, field-spec: as in SRFI 9

;; Defines alternate accessor procedures to the existing SRFI 9
;; record-type {{original-name}} according to {{variant-spec}}.

;; {{name-spec}} acts as it does in {{define-record-variant}},
;; including constructor generation behavior.

;; {{pred-spec}} may be a predicate identifier or a list containing
;; a predicate identifier and optionally a "checker" identifier.  The
;; checker identifier is used as the name of the generated
;; check-VARIANT-NAME procedure, which again behaves as in
;; {{define-record-variant}}.  If the checker identifier is
;; omitted, no check procedure is generated.

;; See {{define-record-variant}} and SRFI 9 for further details.

(module record-variants
  (define-record-variant define-record-type-variant)

  (import scheme)
  
  (define-syntax define-record-variant
    (lambda (x r c)
      (define (any p L)
        (and (pair? L)
             (or (p (car L))
                 (any p (cdr L)))))
      (##sys#check-syntax 'define-record-variant x
                          '(_ _ #(symbol 0) . #(symbol 0)))
      (let* ((name-spec (cadr x))
             (name (if (pair? name-spec) (car name-spec) name-spec))
             (original-name (if (pair? name-spec) (cadr name-spec) name-spec))
             (prefix (symbol->string name))
             (constructor? (or #t   ; force #t -- always generate constructor
                               (eq? name original-name)))

             (variant? (lambda (type) (any (lambda (x) (c x (r type)))
                                   (caddr x))))
             (unsafe? (variant? 'unsafe))
             (unchecked? (variant? 'unchecked))
             (inline? (variant? 'inline))

             (slots (cdddr x))
             (setters (memq #:record-setters ##sys#features))
             (%begin (r 'begin))
             (%define (if inline?
                          (r 'define-inline)
                          (r 'define)))
             (%getter-with-setter (r 'getter-with-setter))
             (%lambda (r 'lambda)))
        `(,%begin
           ,(if constructor?
                `(,%define
                   ,(string->symbol (string-append "make-" prefix))
                   (,%lambda ,slots
                     (##sys#make-structure ',original-name ,@slots)))
                `(,%begin))
           (,%define
              ,(string->symbol (string-append prefix "?"))
             (,%lambda (x) (##sys#structure? x ',original-name)))
           (,%define
             ,(string->symbol (string-append "check-" prefix))
             (,%lambda (x)
               (##core#check (##sys#check-structure x ',original-name))
               x))
           ,@(let loop ((slots slots) (i 1))
               (if (eq? slots '())
                   slots
                   (let* ((slotname (symbol->string (car slots)))
                          (setr (string->symbol (string-append
                                                 prefix "-" slotname "-set!")))
                          (getr (string->symbol (string-append
                                                 prefix "-" slotname))))
                     (cons
                      `(,%begin
                         (,%define
                           ,setr
                           (,%lambda (x val)
                             ,(if unchecked?
                                  `(,%begin)
                                  `(##core#check (##sys#check-structure
                                                  x ',original-name)))
                             ,(if unsafe?
                                  `(##sys#setslot x ,i val)
                                  `(##sys#block-set! x ,i val))))
                         (,%define
                           ,getr
                           ,(if setters
                                `(,%getter-with-setter
                                  (,%lambda (x)
                                    ,(if unchecked?
                                         `(,%begin)
                                         `(##core#check (##sys#check-structure
                                                         x ',original-name)))
                                    ,(if unsafe?
                                         `(##sys#slot x ,i)
                                         `(##sys#block-ref x ,i)))
                                  ,setr)
                                `(,%lambda (x)
                                   ,(if unchecked?
                                        `(,%begin)
                                        `(##core#check (##sys#check-structure
                                                        x ',original-name)))
                                   ,(if unsafe?
                                        `(##sys#slot x ,i)
                                        `(##sys#block-ref x ,i))))))
                      (loop (cdr slots) (add1 i))))))))))

  (define-syntax define-record-type-variant
    (lambda (form r c)
      (define (any p L)
        (and (pair? L)
             (or (p (car L))
                 (any p (cdr L)))))      
      (##sys#check-syntax 'define-record-type-variant form
                          '(_ _ #(variable 0)
                              #(variable 1) _ . _))
      (let* ((name-spec (cadr form))
             (name (if (pair? name-spec) (car name-spec) name-spec))
             (t (if (pair? name-spec) (cadr name-spec) name-spec))
             (variant? (lambda (type) (any (lambda (x) (c x (r type)))
                                      (caddr form))))
             (unsafe? (variant? 'unsafe))
             (unchecked? (variant? 'unchecked))
             (inline? (variant? 'inline))
             (constructor? (eq? name t))
             
             (conser (cadddr form))
             (predspec (car (cddddr form)))
             (pred (if (pair? predspec) (car predspec) predspec))
             (checker (if (and (pair? predspec)
                               (pair? (cdr predspec)))
                          (cadr predspec) #f))
             (slots (cdr (cddddr form)))
             (%begin (r 'begin))
             (%lambda (r 'lambda))
             (%define (if inline? (r 'define-inline) (r 'define)))
             (vars (cdr conser))
             (x (r 'x))
             (y (r 'y))
             (%getter-with-setter (r 'getter-with-setter))
             (slotnames (map car slots)))
        `(,%begin
           ,(if constructor?
                `(,%define ,conser
                   (##sys#make-structure 
                    ',t 
                    ,@(map (lambda (sname)
                             (if (memq sname vars)
                                 sname
                                 '(##core#undefined)))
                           slotnames)))
                `(,%begin))
           (,%define (,pred ,x) (##sys#structure? ,x ',t))
           ,(if checker
                `(,%define (,checker ,x)
                   (##core#check (##sys#check-structure ,x ',t)))
                `(,%begin))
           ,@(let loop ([slots slots] [i 1])
               (if (null? slots)
                   '()
                   (let* ([slot (car slots)]
                          (setters (memq #:record-setters ##sys#features))
                          (setr? (pair? (cddr slot))) 
                          (getr `(,%lambda (,x)
                                   ,(if unchecked?
                                        `(,%begin)
                                        `(##core#check
                                          (##sys#check-structure ,x ',t)))
                                   ,(if unsafe?
                                        `(##sys#slot ,x ,i)
                                        `(##sys#block-ref ,x ,i)))))
                     `(,@(if setr?
                             `((,%define (,(caddr slot) ,x ,y)
                                 ,(if unchecked?
                                       `(,%begin)
                                       `(##core#check
                                         (##sys#check-structure ,x ',t)))
                                 ,(if unsafe?
                                      `(##sys#setslot ,x ,i ,y)
                                      `(##sys#block-set! ,x ,i ,y))))
                             '())
                       (,%define ,(cadr slot) 
                         ,(if (and setr? setters)
                              `(,%getter-with-setter ,getr ,(caddr slot))
                              getr) )
                       ,@(loop (cdr slots) (add1 i))))))))))

  )


;; Copyright (c) 2009 Jim Ursetto.  All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 
;;  Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.
;;  Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;  Neither the name of the author nor the names of its contributors 
;;   may be used to endorse or promote products derived from this software 
;;   without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


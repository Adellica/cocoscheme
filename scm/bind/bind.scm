;;;; bind.scm


(module bind (bind
	      bind*
	      bind-type 
	      bind-file
	      bind-file*
	      bind-opaque-type
	      bind-rename
	      bind-rename/pattern
	      bind-options
	      bind-include-path)

(import scheme chicken foreign)

(import-for-syntax bind-translator srfi-1 srfi-13 utils)

(begin-for-syntax
 (require-library bind-translator))

(define-syntax bind
  (lambda (x r c)
    (let ((strs (append (cdr x) '("\n"))))
      `(,(r 'begin)
	,@(parse-easy-ffi (string-concatenate strs) r)))))
  
(define-syntax bind*
  (lambda (x r c)
    (let ((strs (append (cdr x) '("\n"))))
      `(,(r 'begin)
	(,(r 'declare) (foreign-declare ,@strs))
	(,(r 'bind) ,@strs)) ) ))

(define-for-syntax (bind:read-file f)
  (let ((fname 
	 (cond ((string? f) f)
	       ((symbol? f) (symbol->string (strip-syntax f)))
	       (else
		(syntax-error 'bind-file "invalid filename" f)))))
    (read-all fname)))

(define-syntax (bind-file x r c)
  `(,(r 'bind) ,@(map bind:read-file (cdr x))))

(define-syntax (bind-file* x r c)
  `(,(r 'bind*) ,@(map bind:read-file (cdr x))))

(define-syntax bind-include-path
  (lambda (x r c)
    (set! ffi-include-path-list (append (cdr x) ffi-include-path-list))
    `(,(r 'void) ) ) )

(define-syntax (bind-type x r c)
  (parse-type-declaration (cdr x) r))

(define-syntax (bind-opaque-type x r c)
  (parse-opaque-type-declaration (cdr x) r))

(define-syntax (bind-options x r c)
  (apply set-bind-options (strip-syntax (cdr x)))
  `(,(r 'void)))

(define-syntax (bind-rename x r c)
  (if (= 2 (length (cdr x)))
      (apply set-renaming (strip-syntax (cdr x)))
      (syntax-error 'bind-rename "bad number of arguments" x))
  `(,(r 'void)))

(define-syntax (bind-rename/pattern x r c)
  (if (= 2 (length (cdr x)))
      (apply set-renaming (append (strip-syntax (cdr x)) '(regex: #t)))
      (syntax-error 'bind-rename "bad number of arguments" x))
  `(,(r 'void)))

)

(in-package :quest)

(define-parser *bbcode-parser*
  (:start-symbol bbcode*)
  (:terminals (bare-word open-bracket close-bracket slash equals quote escape))

  (bbcode*
   bbcode
   ())

  (bbcode
   (bbcode expression #'append-bbcode)
   expression)

  (expression
   (open-tag bbcode close-tag #'tag-expression)
   (open-tag close-tag (lambda (a b) (tag-expression a nil b)))
   (unquotable-piece (curry 'make-bbcode nil nil)))

  (open-tag
   (open-bracket bare-word equals quotable-string close-bracket (lambda (a b c d e)
                                                                  (declare (ignore a c e))
                                                                  (list b d)))
   (open-bracket bare-word close-bracket (lambda (a b c)
                                           (declare (ignore a c))
                                           (list b nil))))

  (close-tag
   (open-bracket slash bare-word close-bracket (lambda (a b c d)
                                                 (declare (ignore a b d))
                                                 c)))

  (quotable-string
   string
   (quote quoted-string quote (lambda (a b c) (declare (ignore a c)) b)))

  (quoted-string
   (quoted-string quoted-piece (curry 'concatenate 'string))
   quoted-piece)

  (quoted-piece
   simple-string-piece
   quoted-literal-special
   quotable-special)

  (unquotable-piece
   string-piece
   quote)

  (string
   (string string-piece (curry 'concatenate 'string))
   string-piece)

  (string-piece
   simple-string-piece
   literal-special)

  (simple-string-piece
   bare-word
   equals
   slash)

  (quoted-literal-special
   (escape escape (lambda (a b) (declare (ignore a)) b))
   (escape quote (lambda (a b) (declare (ignore a)) b)))

  (literal-special
   (escape special (lambda (a b) (declare (ignore a)) b)))

  (special
   quotable-special escape quote)

  (quotable-special
   open-bracket close-bracket))

(defun terminal-description (terminal)
  (case terminal
    (bare-word "plain text")
    (open-bracket "[")
    (close-bracket "]")
    (slash "/")
    (equals "=")
    (quote "\"")
    (escape "\\")
    (t "end of input")))

(define-condition bbcode-syntax-error (bbcode-error)
  ((got :initarg :got :reader got)
   (expected :initarg :expected :reader expected))
  (:report (lambda (e s)
             (format s "Found ~A when we expected~:[~; one of:~] ~{~#[~;~a~;~a or ~a~:;~@{~a~#[~;, or ~:;, ~]~}~]~}" (or (and (got e) (format nil "\"~A\"" (got e))) "end of input") (> (length (expected e)) 1) (mapcar 'terminal-description (expected e))))))

(defun parse-bbcode (stream)
  (handler-case (parse-with-lexer (curry #'bbcode-lexer stream) *bbcode-parser*)
    (yacc-parse-error (e) (error 'bbcode-syntax-error :expected (yacc-parse-error-expected-terminals e) :got (yacc-parse-error-value e)))))

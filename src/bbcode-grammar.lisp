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
   string-piece
   quotable-special)

  (unquotable-piece
   string-piece
   quote)

  (string
   (string string-piece (curry 'concatenate 'string))
   string-piece)

  (string-piece
   bare-word
   equals
   slash
   literal-special)

  (literal-special
   (escape special (lambda (a b) (declare (ignore a)) b)))

  (special
   quotable-special escape quote)

  (quotable-special
   open-bracket close-bracket))

(defun parse-bbcode (stream)
  (parse-with-lexer (curry #'bbcode-lexer stream) *bbcode-parser*))

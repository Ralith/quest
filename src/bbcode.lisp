(in-package :quest)

(define-parser *bbcode-parser*
  (:start-symbol expression)
  (:terminals (bare-word open-bracket close-bracket slash equals))
  
  (expression
   (open-tag expression close-tag #'tag-expression)
   string)                              ; implicit action #'identity

  (open-tag
   (open-bracket bare-word equals string close-bracket (lambda (a b c d e)
                                                         (declare (ignore a c e))
                                                         (list b d)))
   (open-bracket bare-word close-bracket (lambda (a b c)
                                           (declare (ignore a c))
                                           (list b nil))))

  (close-tag
   (open-bracket slash bare-word close-bracket (lambda (a b c d)
                                                 (declare (ignore a b d))
                                                 c)))

  (string
   (string string-piece (curry 'concatenate 'string))
   string-piece)

  (string-piece
   bare-word
   equals
   slash))

(define-condition bbcode-error (yacc-runtime-error) ())

(define-condition tag-mismatch (bbcode-error)
  ((left :initarg :left :reader left)
   (right :initarg :right :reader right))
  (:report (lambda (e s)
             (format s "Mismatched tags: [~A] and [/~A]" (left e) (right e)))))

(defun tag-expression (open expr close)
  (destructuring-bind (name param) open
    (restart-case
        (progn (unless (string= name close)
                 (error 'tag-mismatch :left name :right close))
               (list name param expr))
      (use-tag (new-name)
        :report (lambda (s) (format s "Provide a tag name to use in place of the mismatched tags."))
        :interactive (lambda () (list (read-line)))
        (list new-name param expr)))))

(defun bbcode-lexer (&optional (stream *standard-input*))
  (let ((c (read-char stream nil nil)))
    (and c
         (case c
           (#\[ (values 'open-bracket "["))
           (#\] (values 'close-bracket "]"))
           (#\/ (values 'slash "/"))
           (#\= (values 'equals "="))
           (t (values 'bare-word
                      (coerce (cons c (loop for d = (read-char stream nil nil)
                                            until (or (null d)
                                                      (and (or (eql d #\[)
                                                               (eql d #\])
                                                               (eql d #\=))
                                                           (progn (unread-char d stream) t)))
                                            when d
                                              collect d))
                              'string)))))))
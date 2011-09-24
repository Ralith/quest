(in-package :quest)

(defun append-bbcode (a b)
  (cond
    ((first a) (list nil () a b))
    ((first b) (append a (list b)))
    (t (append a (cddr b)))))

(defun make-bbcode (tag param &rest body)
  (list* tag param body))

(define-condition bbcode-error (yacc-runtime-error) ())

(define-condition tag-mismatch (bbcode-error)
  ((left :initarg :left :reader left)
   (right :initarg :right :reader right))
  (:report (lambda (e s)
             (format s "Mismatched tags: [~A] and [/~A]" (left e) (right e)))))

(defclass bbcode ()
  ((body :initarg :body :reader body)))

(defclass tag (bbcode)
  ((name :initarg :name :initform (error "Tags must be named!") :reader name)
   (param :initarg :param :reader param)))

(defmethod print-object ((o tag) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A=~A (~A)" (name o) (param o) (body o))))

(defun tag-expression (open expr close)
  (destructuring-bind (name param) open
    (restart-case
        (progn (unless (string= name close)
                 (error 'tag-mismatch :left name :right close))
               (if (first expr)
                   (list name param expr)
                   (list* name param (cddr expr))))
      (use-tag (new-name)
        :report (lambda (s) (format s "Provide a tag name to use in place of the mismatched tags."))
        :interactive (lambda () (list
                                 #+swank (swank::read-from-minibuffer-in-emacs "Tag name: ")
                                 #-(or swank) (read-line)))
        (tag-expression (list new-name param) expr close)))))

(defun bbcode-lexer (&optional (stream *standard-input*))
  (let ((c (read-char stream nil nil)))
    (and c
         (case c
           (#\[ (values 'open-bracket "["))
           (#\] (values 'close-bracket "]"))
           (#\/ (values 'slash "/"))
           (#\= (values 'equals "="))
           (#\" (values 'quote "\""))
           (#\\ (values 'escape "\\"))
           (t (values 'bare-word
                      (coerce (cons c (loop for d = (read-char stream nil nil)
                                            until (or (null d)
                                                      (and (or (find d "[]/=\"\\"))
                                                           (progn (unread-char d stream) t)))
                                            when d
                                              collect d))
                              'string)))))))

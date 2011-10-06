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

(defvar *bbcode* (make-hash-table :test 'equal))

(defmacro defbbcode (name (stream body &optional param) &body actions)
  (with-gensyms (inner-tree)
    `(setf (gethash ,(string-downcase (symbol-name name)) *bbcode*)
           (list ,(and param t)
                 (lambda (,stream ,inner-tree ,@(when param (list param)))
                   (flet ((,body (stream)
                            (mapc (curry #'bbcode->html stream)
                                  ,inner-tree)))
                     ,@actions))))))

(defmacro defbbcode-trivial (name &optional left right)
  (setf left (or left (format nil "<~A>" (string-downcase (symbol-name name))))
        right (or right (format nil "</~A>" (string-downcase (symbol-name name)))))
  (with-gensyms (stream body)
    `(defbbcode ,name (,stream ,body)
       (write-string ,left ,stream)
       (,body ,stream)
       (write-string ,right ,stream))))

(defbbcode-trivial b)
(defbbcode-trivial i)
(defbbcode-trivial s)
(defbbcode-trivial u "<span style=\"text-decoration: underline\">" "</span>")
(defbbcode-trivial sup)
(defbbcode-trivial sub)
(defbbcode-trivial code "<pre>" "</pre>")
(defbbcode-trivial spoiler "<span class=\"spoiler\">" "</span>")
(defbbcode-trivial pre)
(defbbcode-trivial strong)
(defbbcode-trivial em)
(defbbcode color (s body color)
  (format s "<span style=\"color: ~A\">" (if color
                                             (escape-for-html color)
                                             ""))
  (body s)
  (format s "</span>"))
(defbbcode url (s body url)
  (if url
      (progn (format s "<a href=\"~A\">" (escape-for-html url))
             (body s)
             (format s "</a>"))
      (let ((body (with-output-to-string (o)
                    (body o))))
        (format s "<a href=\"~A\">~A</a>" body body))))
(defbbcode img (s body url)
  ;; TODO: Thumbnailing
  (if url
      (progn (format s "<img src=\"~A\" title=\"" (escape-for-html url))
             (body s)
             (format s "\">"))
      (progn (write-string "<img src=\"" s)
             (body s)
             (write-string "\">" s))))

(defun bbcode->html (stream bbcode)
  (etypecase bbcode
    (string (write-string (escape-for-html bbcode) stream))
    (list
     (destructuring-bind (tag param &rest body) bbcode
       (if-let (handler (gethash tag *bbcode*))
         (destructuring-bind (param? renderer) handler
           (if param?
               (funcall renderer stream body param)
               (funcall renderer stream body)))
         (mapc (curry #'bbcode->html stream) body)))))
  (values))

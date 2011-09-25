(in-package :quest)

(defun char-digit (c)
  (- (char-code c) (char-code #\0)))

(defun dice-lexer (&optional (stream *standard-input*)
                     &aux (c (read-char stream nil nil)))
  (cond
    ((null c) nil)
    ((digit-char-p c)
     (loop for d = (read-char stream nil nil)
           while (and d (digit-char-p d))
           with accum = (char-digit c)
           do (setf accum (+ (* 10 accum) (char-digit d)))
           finally (when d (unread-char d stream))
                   (return (values 'integer accum))))
    ((eql #\+ c) '+)
    ((eql #\- c) '-)
    ((eql #\d c) 'd)))

(defun pprint-roll (dice &optional (stream *standard-output*))
  (multiple-value-bind (value results) (eval-dice dice)
    (loop for (sign . rolls) in results
          for die in dice
          for paren? = (and (rest rolls) (> (length results) 1))
          with first-set = t
          do (if first-set
                 (progn (format stream "~:[~;-~]" (eq sign '-))
                        (setf first-set nil))
                 (format stream " ~A " sign))
             (when paren?
               (write-string "(" stream))
             (when (= 3 (length die))
               (format stream "~Ad~A: " (second die) (third die)))
             (format stream "~{~A~^ + ~}" rolls)
             (when paren?
               (write-string ")" stream)))
    (format stream " = ~A" value)
    value))

(defun eval-dice (dice &aux
                         (rolls (mapcar (lambda (x)
                                          (cons (first x) (eval-roll (rest x))))
                                        dice))
                         (accum 0))
  (loop for (sign . values) in rolls
        for sum = (apply '+ values)
        do (ecase sign
             (+ (incf accum sum))
             (- (decf accum sum))))
  (values accum rolls))

(defun eval-roll (roll)
  "Takes (count sides) or (value) and returns a list of values."
  (ecase (length roll)
    (1 roll)
    (2 (destructuring-bind (count sides) roll
         (loop for i below count
               collect (1+ (random sides)))))))

(define-parser *dice-parser*
  (:start-symbol dice)
  (:terminals (integer d + -))

  (dice
   (dice part (lambda (a b) (cons b a)))
   (roll (compose 'list (curry 'cons '+)))
   (integer (compose 'list (curry 'list '+)))
   (part))

  (part
   (sign integer)
   (sign roll (lambda (a b) (cons a b))))

  (roll
   (d integer (lambda (a b) (declare (ignore a)) (list 1 b)))
   (integer d integer (lambda (a b c) (declare (ignore b)) (list a c))))
  
  (sign
   (+ (constantly '+))
   (- (constantly '-))))

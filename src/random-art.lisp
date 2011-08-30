(defpackage #:random-art
  (:use #:cl #:sb-cga #:zpng)
  (:export #:render #:render-to-stream #:generate))
(in-package #:random-art)

(defvar *operators* ())

(defun operator (cons)
  (car cons))

(defun arity (cons)
  (cdr cons))

(defmacro with-op ((cons operator arity) &body body)
  (alexandria:once-only (cons)
    `(symbol-macrolet ((,operator (car ,cons))
                       (,arity (cdr ,cons)))
       ,@body)))

(defmacro defop (name children coords locals &body body)
  (let ((constr-name (alexandria:symbolicate "MAKE-" name)))
    `(progn
       (declaim (ftype (sb-cga::sfunction ,(loop repeat (length children)
                                                 collect 'function)
                                          (sb-cga::sfunction (single-float single-float) vec))))
       (defun ,constr-name ,children
         (let ,locals
           (the (sb-cga::sfunction (single-float single-float) vec)
                (lambda ,coords
                  (declare (type single-float ,@coords))
                  ,@(butlast body 1)
                  (the vec ,(car (last body)))))))
       (pushnew (cons ',constr-name ,(length children)) *operators* :key #'car))))

(defmacro with-vec ((v x y z) &body body)
  (alexandria:once-only (v)
   `(symbol-macrolet ((,x (aref ,v 0))
                      (,y (aref ,v 1))
                      (,z (aref ,v 2)))
      ,@body)))

(defop variable-x () (x y) ()
  (declare (ignore y))
  (vec x x x))

(defop variable-y () (x y) ()
  (declare (ignore x))
  (vec y y y))

(defop constant () (x y)
    ((c (vec (random 1.0) (random 1.0) (random 1.0))))
  (declare (ignore x y))
  c)

(defop sum (a b) (x y) ()
  (vec/ (vec+ (funcall a x y) (funcall b x y))
     2.0))

(defop product (a b) (x y) ()
  (hadamard-product (funcall a x y) (funcall b x y)))

(defun well (x)
  (declare (type single-float x))
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* x x)) 8.0))))

(defop well (a) (x y) ()
  (with-vec ((funcall a x y) r g b)
    (vec (well r) (well g) (well b))))

(defun tent (x)
  (declare (type single-float x))
  (- 1.0 (* 2.0 (abs x))))

(defop tent (a) (x y) ()
  (with-vec ((funcall a x y) r g b)
    (vec (tent r) (tent g) (tent b))))

(defop sin (a) (x y)
    ((phase (* (float pi 0.0) (random 1.0)))
     (freq (+ 1.0 (random 5.0))))
  (with-vec ((funcall a x y) r g b)
    (vec (sin (+ phase (* freq r)))
         (sin (+ phase (* freq g)))
         (sin (+ phase (* freq b))))))

(defop level (level a b) (x y)
    ((threshold (- 1.0 (random 2.0))))
  (with-vec ((funcall level x y) r1 g1 b1)
    (with-vec ((funcall a x y) r2 g2 b2)
      (with-vec ((funcall b x y) r3 g3 b3)
        (vec (if (< r1 threshold) r2 r3)
             (if (< g1 threshold) g2 g3)
             (if (< b1 threshold) b2 b3))))))

(defop mix (w a b) (x y) ()
  (let ((weight (* 0.5 (+ 1.0 (aref (funcall w x y) 0)))))
    (vec+ (vec* (funcall a x y)
                weight)
          (vec* (funcall b x y)
                (- 1.0 weight)))))

(defun ensure-nonzero (x)
  (if (= 0.0 x)
      0.42
      x))

(defop mod (a b) (x y) ()
  (with-vec ((funcall a x y) ar ag ab)
    (with-vec ((funcall b x y) br bg bb)
      (vec (mod ar (ensure-nonzero br))
           (mod ag (ensure-nonzero bg))
           (mod ab (ensure-nonzero bb))))))

(defun generate (&optional (depth 10))
  (let ((operators0 (remove-if (alexandria:curry #'/= 0)
                               *operators*
                               :key #'arity))
        (operators1 (remove-if (alexandria:curry #'= 0)
                               *operators*
                               :key #'arity)))
    (if (<= depth 0)
        (funcall (operator (alexandria:random-elt operators0)))
        (with-op ((alexandria:random-elt operators1) operator arity)
          ;; TODO: randomart.py-style selection
          (apply operator (loop repeat arity
                                collect (generate (1- depth))))))))

(defun vec->pixel (vec)
  (make-array 3 :element-type 'fixnum :initial-contents
              (list (max 0 (min 255 (truncate (* 128 (+ 1 (aref vec 0))))))
                    (max 0 (min 255 (truncate (* 128 (+ 1 (aref vec 1))))))
                    (max 0 (min 255 (truncate (* 128 (+ 1 (aref vec 2)))))))))

(defun do-render (function pixel-writer width height)
  (format t "|----------------------------------------|~%|")
  (loop for x from -1.0 to 1.0 by (/ 2 width)
        for i below width
        with progress = 0
        do (loop for y from -1.0 to 1.0 by (/ 2 height)
                 for j below height
                 do (funcall pixel-writer i j
                             (vec->pixel
                              (funcall function
                                       (float x 0.0)
                                       (float y 0.0)))))
           (when (> (/ i width) progress)
             (format t "=")
             (incf progress (/ 1 40))))
  (format t "|~%"))

(defun render (function &optional (width 256) (height 256))
  (let* ((png (make-instance 'png
                             :color-type :truecolor
                             :width width
                             :height height))
         (image (data-array png)))
    (do-render function
      (lambda (x y pixel)
        (with-vec (pixel r g b)
          (setf (aref image y x 0) r
                (aref image y x 1) g
                (aref image y x 2) b)))
      width height)
    png))

(defun render-to-file (function file &optional (width 256) (height 256))
  (let ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor
                             :width width
                             :height height)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (do-render function
        (lambda (x y pixel)
          (declare (ignore x y))
          (write-pixel pixel png))
        width height)
      (finish-png png))))

(defun render-to-stream (function stream &optional (width 256) (height 256))
  (let ((png (make-instance 'pixel-streamed-png
                            :color-type :truecolor
                            :width width
                            :height height)))
    (start-png png stream)
    (do-render function
      (lambda (x y pixel)
        (declare (ignore x y))
        (write-pixel pixel png))
      width height)
    (finish-png png)))
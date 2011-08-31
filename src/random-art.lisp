(defpackage #:random-art
  (:use #:cl #:zpng)
  (:export #:render #:render-to-stream #:generate))
(in-package #:random-art)

(defvar *operators* ())

(declaim (optimize (speed 3)))

(defun operator (cons)
  (car cons))

(defun arity (cons)
  (cdr cons))

(defstruct operator
  name arity children coords static-locals dynamic-locals red green blue)

(defmethod print-object ((o operator) s)
  (print-unreadable-object (o s :type t)
    (princ (operator-name o) s)))

(defmacro defop (name coords children static-locals dynamic-locals red green blue)
  `(progn
     (pushnew (make-operator :name ',name
                             :arity ,(length children)
                             :children ',children
                             :coords ',coords
                             :static-locals ',static-locals
                             :dynamic-locals ',dynamic-locals
                             :red ',red
                             :green ',green
                             :blue ',blue)
              *operators*
              :key #'operator-name)))

(defop variable-x (x y) () () ()
  x x x)

(defop variable-y (x y) () () ()
  y y y)

(defop constant (x y) ()
    ((r (random 1.0)) (g (random 1.0)) (b (random 1.0))) ()
  r g b)

(defop sum (x y) (((x y) (r1 g1 b1)) ((x y) (r2 g2 b2))) () ()
  (/ (+ r1 r2) 2.0)
  (/ (+ g1 g2) 2.0)
  (/ (+ b1 b2) 2.0))

(defop product (x y) (((x y) (r1 g1 b1)) ((x y) (r2 g2 b2))) () ()
  (* r1 r2)
  (* g1 g2)
  (* b1 b2))

(defop well (x y) (((x y) (r g b))) () ()
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* r r)) 8.0)))
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* g g)) 8.0)))
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* b b)) 8.0))))

(defop tent (x y) (((x y) (r g b))) () ()
  (- 1.0 (* 2.0 (abs r)))
  (- 1.0 (* 2.0 (abs g)))
  (- 1.0 (* 2.0 (abs b))))

(defop sin (x y) (((x y) (r g b)))
    ((phase (* (float pi 0.0) (random 1.0)))
     (freq (+ 1.0 (random 5.0)))) ()
  (sin (+ phase (* freq r)))
  (sin (+ phase (* freq g)))
  (sin (+ phase (* freq b))))

(defop level (x y) (((x y) (r1 g1 b1)) ((x y) (r2 g2 b2)) ((x y) (r3 g3 b3)))
    ((threshold (- 1.0 (random 2.0)))) ()
  (if (< r1 threshold) r2 r3)
  (if (< g1 threshold) g2 g3)
  (if (< b1 threshold) b2 b3))

(defop mix (x y) (((x y) (wr wg wb)) ((x y) (ar ag ab)) ((x y) (br bg bb))) ()
  ((weight (* 0.5 (+ 1.0 wr)))
   (compl (- 1.0 weight)))
  (+ (* ar weight)
     (* br compl))
  (+ (* ag weight)
     (* bg compl))
  (+ (* ab weight)
     (* bb compl)))

(defun ensure-nonzero (x)
  (declare (type single-float x))
  (if (= 0.0 x)
      single-float-epsilon
      x))
(declaim (inline ensure-nonzero))

(defop mod (x y) (((x y) (ar ag ab)) ((x y) (br bg bb))) () ()
  (mod ar (ensure-nonzero br))
  (mod ag (ensure-nonzero bg))
  (mod ab (ensure-nonzero bb)))

(defun generate-tree (&optional (depth 10))
  (let ((operators0 (remove-if (alexandria:curry #'/= 0)
                               *operators*
                               :key #'operator-arity))
        (operators1 (remove-if (alexandria:curry #'= 0)
                               *operators*
                               :key #'operator-arity)))
    (if (<= depth 0)
        (list (alexandria:random-elt operators0)) ; Leaf
        (let ((op (alexandria:random-elt operators1)))
         ;; TODO: randomart.py-style selection
         (list* op
                (loop repeat (operator-arity op)
                      collect (generate-tree (1- depth))))))))

(defun tree->code (tree &optional (x-form 'x) (y-form 'y))
  (labels ((build-mvb (bindings args body)
             (if (and bindings args)
                 `(multiple-value-bind ,(first bindings) ,(first args)
                    (declare (ignorable ,@(first bindings))
                             (type single-float ,@(first bindings)))
                    ,(build-mvb (rest bindings) (rest args) body))
                 body)))
    (destructuring-bind (op &rest args) tree
      `(let ((,(first (operator-coords op)) ,x-form)
             (,(second (operator-coords op)) ,y-form)
             ,@(loop for binding in (operator-static-locals op)
                     collect (list (first binding) (eval (second binding)))))
         (declare (ignorable ,(first (operator-coords op))
                             ,(second (operator-coords op))))
         ,(build-mvb (mapcar #'second (operator-children op))
                     (loop for arg in args
                           for child in (operator-children op)
                           for coord-forms = (first child)
                           collect (tree->code arg
                                               (first coord-forms)
                                               (second coord-forms)))
                     `(let* ,(operator-dynamic-locals op)
                        (values ,(operator-red op)
                                ,(operator-green op)
                                ,(operator-blue op))))))))

(defun code->func (code)
  (compile nil
           (eval `(lambda (x y)
                    (declare (optimize (speed 3)))
                    ,code))))

(defun generate (&optional (depth 10))
  (code->func (tree->code (generate-tree depth))))

(defun coord->color (x)
  (declare (type single-float x))
  (truncate (* 128 (+ 1 (min (/ 255.0 256.0) (max -1.0 x))))))
(declaim (inline coord->color))


(defun do-render (function pixel-writer width height)
  (declare (type fixnum width height))
  (format t "|----------------------------------------|~%|")
  (loop for x from -1.0 to 1.0 by (/ 2.0 width)
        for i below width
        with progress = 0.0
        do (loop for y from -1.0 to 1.0 by (/ 2.0 height)
                 for j below height
                 do (multiple-value-bind (r g b)
                        (funcall function x y)
                      (funcall pixel-writer i j
                               (coord->color r) (coord->color g) (coord->color b))))
           (when (> (/ i width) progress)
             (format t "=")
             (incf progress (/ 1.0 40.0))))
  (format t "|~%"))

(defun render (function &optional (width 256) (height 256))
  (let* ((png (make-instance 'png
                             :color-type :truecolor
                             :width width
                             :height height))
         (image (data-array png)))
    (do-render function
      (lambda (y x r g b)
        (setf (aref image y x 0) r
              (aref image y x 1) g
              (aref image y x 2) b))
      width height)
    png))

(defun render-to-file (function file &optional (width 256) (height 256))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (render-to-stream function stream width height)))

(defun render-to-stream (function stream &optional (width 256) (height 256))
  (let ((png (make-instance 'pixel-streamed-png
                            :color-type :truecolor
                            :width width
                            :height height)))
    (start-png png stream)
    (do-render function
      (lambda (x y r g b)
        (declare (ignore x y))
        (write-pixel (list r g b) png))
      width height)
    (finish-png png))
  (values))

(defpackage #:random-art
  (:use #:cl #:zpng)
  (:export #:render #:render-to-stream #:generate))
(in-package #:random-art)

(defparameter *operators* ())

;;; Initialized only for REPL convenience; all actual uses should use dynamic bindings for safety.
(defvar *randbuf* (cffi:foreign-alloc :char :count 24))

(declaim (optimize (speed 3)))

(cffi:defcfun (%drand48-r "drand48_r") :int
  (buffer :pointer)
  (result (:pointer :double)))

(cffi:defcfun (%lrand48-r "lrand48_r") :int
  (buffer :pointer)
  (result (:pointer :long)))

(cffi:defcfun (%mrand48-r "mrand48_r") :int
  (buffer :pointer)
  (result (:pointer :long)))

(cffi:defcfun (%seed48-r "seed48_r") :int
  (seed (:array :unsigned-short 3))
  (buffer :pointer))

(defun seed48 (seed)
  (declare (type (integer 0 281474976710655) seed))
  (%seed48-r (vector (ldb (byte 16 32) seed)
                     (ldb (byte 16 16) seed)
                     (ldb (byte 16 0)  seed))
             *randbuf*))

(defun drand48 ()
  (cffi:with-foreign-object (r :double)
    (%drand48-r *randbuf* r)
    (cffi:mem-aref r :double)))

(defun lrand48 ()
  (cffi:with-foreign-object (r :long)
    (%lrand48-r *randbuf* r)
    (cffi:mem-aref r :long)))

(defun mrand48 ()
  (cffi:with-foreign-object (r :long)
    (%mrand48-r *randbuf* r)
    (cffi:mem-aref r :long)))

(defun rand-elt (seq)
  (nth (rem (lrand48) (length seq)) seq))

(defun rand (limit)
  (typecase limit
    (integer (rem (lrand48) limit))
    (float (* limit (float (drand48) 0.0)))))

(defstruct operator
  name arity children coords static-locals dynamic-locals red green blue)

(defmethod print-object ((o operator) s)
  (print-unreadable-object (o s :type t)
    (princ (operator-name o) s)))

(defmacro defop (name coords static-locals children dynamic-locals red green blue)
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

(defop matrix (x y)
    ((a (- 1.0 (rand 2.0))) (b (- 1.0 (rand 2.0)))
     (c (- 1.0 (rand 2.0))) (d (- 1.0 (rand 2.0)))
     (e (- 1.0 (rand 2.0))) (f (- 1.0 (rand 2.0))))
    () ()
  (+ (* a x) (* b y))
  (+ (* c x) (* d y))
  (+ (* e x) (* f y)))

(defop coord-matrix (x y) 
    ((a (- 1.5 (rand 3.0))) (b (- 1.5 (rand 3.0))) (c (- 1.0 (rand 2.0)))
     (d (- 1.5 (rand 3.0))) (e (- 1.5 (rand 3.0))) (f (- 1.0 (rand 2.0))))
    ((((+ (* a x) (* b y) c)
       (+ (* d x) (* e y) f))
      (red green blue))) ()
  red green blue)

;; (defop color-matrix (x y)
;;     ((a (- 1.5 (rand 3.0))) (b (- 1.5 (rand 3.0))) (c (- 1.0 (rand 2.0)))
;;      (d (- 1.5 (rand 3.0))) (e (- 1.5 (rand 3.0))) (f (- 1.0 (rand 2.0)))
;;      (g (- 1.5 (rand 3.0))) (h (- 1.5 (rand 3.0))) (i (- 1.0 (rand 2.0))))
;;     )

(defop constant (x y)
    ((r (rand 1.0)) (g (rand 1.0)) (b (rand 1.0)))
    () ()
  r g b)

(defop sum (x y) () (((x y) (r1 g1 b1)) ((x y) (r2 g2 b2))) ()
  (/ (+ r1 r2) 2.0)
  (/ (+ g1 g2) 2.0)
  (/ (+ b1 b2) 2.0))

(defop product (x y) () (((x y) (r1 g1 b1)) ((x y) (r2 g2 b2))) ()
  (* r1 r2)
  (* g1 g2)
  (* b1 b2))

(defop well (x y) () (((x y) (r g b))) ()
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* r r)) 8)))
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* g g)) 8)))
  (- 1.0 (/ 2.0 (expt (+ 1.0 (* b b)) 8))))

(defop tent (x y) () (((x y) (r g b))) ()
  (- 1.0 (* 2.0 (abs r)))
  (- 1.0 (* 2.0 (abs g)))
  (- 1.0 (* 2.0 (abs b))))

(defop flat-sin (x y)
    ((phase (* (float pi 0.0) (rand 1.0)))
     (freq (+ 1.0 (rand 5.0))))
    (((x y) (r g b))) ()
  (sin (+ phase (* freq r)))
  (sin (+ phase (* freq g)))
  (sin (+ phase (* freq b))))

(defop bias-sin (x y)
    ((rphase (* (float pi 0.0) (rand 1.0)))
     (rfreq (+ 1.0 (rand 5.0)))
     (gphase (* (float pi 0.0) (rand 1.0)))
     (gfreq (+ 1.0 (rand 5.0)))
     (bphase (* (float pi 0.0) (rand 1.0)))
     (bfreq (+ 1.0 (rand 5.0))))
    (((x y) (r g b))) ()
  (sin (+ rphase (* rfreq r)))
  (sin (+ gphase (* gfreq g)))
  (sin (+ bphase (* bfreq b))))

(defop tan (x y)
    ((phase (* (float pi 0.0) (rand 1.0)))
     (freq (+ 1.0 (rand 5.0))))
    (((x y) (r g b))) ()
  (tan (+ phase (* freq r)))
  (tan (+ phase (* freq g)))
  (tan (+ phase (* freq b))))

(defop level (x y)
    ((threshold (- 1.0 (rand 2.0))))
    (((x y) (r1 g1 b1)) ((x y) (r2 g2 b2)) ((x y) (r3 g3 b3))) ()
  (if (< r1 threshold) r2 r3)
  (if (< g1 threshold) g2 g3)
  (if (< b1 threshold) b2 b3))

(defop mix (x y) ()
    (((x y) (wr wg wb)) ((x y) (ar ag ab)) ((x y) (br bg bb)))
  ((weight (* 0.5 (+ 1.0 wr)))
   (compl (- 1.0 weight)))
  (+ (* ar weight)
     (* br compl))
  (+ (* ag weight)
     (* bg compl))
  (+ (* ab weight)
     (* bb compl)))

(declaim (inline fmod))
(defun fmod (x y)
  "Return gibberish when the quotient exceeds single-float precision."
  (declare (type single-float x)
           (type (single-float -1.0 1.0) y))
  (if (= 0 y)
      (setf y single-float-epsilon))
  (- x (* y (truncate (#+sbcl sb-ext:truly-the
                       #-sbcl the
                       (single-float #.(float (1+ (ash -1 24)))
                                     #.(float (1- (ash 1 24))))
                                        (/ x y))))))

(defop mod (x y) () (((x y) (ar ag ab)) ((x y) (br bg bb))) ()
  (fmod ar br)
  (fmod ag bg)
  (fmod ab bb))

(defop complex-square (x y) ()
    ((((- (* x x) (* y y)) (* 2 x y)) (r g b))) ()
  r g b)

(defun save-tree (tree)
  (etypecase tree
    (operator (operator-name tree))
    (list (mapcar #'save-tree tree))))

(defun load-tree (tree)
  (etypecase tree
    (symbol (find tree *operators* :key #'operator-name))
    (list (mapcar #'load-tree tree))))

(defun generate-tree (min-depth max-depth &optional (current-depth 0))
  (let ((operators0 (remove-if (alexandria:curry #'/= 0)
                               *operators*
                               :key #'operator-arity))
        (operators1 (remove-if (alexandria:curry #'= 0)
                               *operators*
                               :key #'operator-arity)))
    (let ((op (rand-elt (cond
                          ((< current-depth min-depth) operators1)
                          ((>= current-depth max-depth) operators0)
                          (t *operators*)))))
      (list* op
             (loop repeat (operator-arity op)
                   collect (generate-tree min-depth max-depth (1+ current-depth)))))))

(defun tree->code (tree &optional (x-form 'x) (y-form 'y))
  (labels ((build-mvb (bindings args body)
             (if (and bindings args)
                 `(multiple-value-bind ,(first bindings) ,(first args)
                    (declare (ignorable ,@(first bindings))
                             (type (single-float -1.0 1.0) ,@(first bindings)))
                    ,(build-mvb (rest bindings) (rest args) body))
                 body)))
    (destructuring-bind (op &rest args) tree
      `(let ((,(first (operator-coords op)) ,x-form)
             (,(second (operator-coords op)) ,y-form))
         (declare (ignorable ,(first (operator-coords op))
                             ,(second (operator-coords op))))
         (let ,(loop for binding in (operator-static-locals op)
                     collect (list (first binding) (eval (second binding))))
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
                                  ,(operator-blue op)))))))))

(defun code->func (code)
  (compile nil `(lambda (x y)
                  (declare (optimize (speed 3))
                           (type single-float x y))
                  ,code)))

(defun generate (&key (min-depth 2) (max-depth 6) (seed (get-internal-real-time)))
  (cffi:with-foreign-object (*randbuf* :char 24)
    (seed48 seed)
    (code->func (tree->code (generate-tree min-depth max-depth)))))

(defun coord->color (x)
  (declare (type single-float x))
  (truncate (* 128 (+ 1 (min (/ 255.0 256.0) (max -1.0 x))))))
(declaim (inline coord->color))


(defun do-render (function pixel-writer width height &aux (ratio (float (/ width height) 0.0)))
  (declare (type fixnum width height)
           (optimize (speed 3)))
  (let ((xmin -1.0) (xmax 1.0) (ymin -1.0) (ymax 1.0) step)
    (if (> width height)
        (let ((recipio (/ 1.0 ratio)))
          (setf step (/ 2.0 width)
                ymin (- recipio)
                ymax recipio))
        (setf step (/ 2.0 height)
              xmin (- ratio)
              xmax ratio))
    (loop for y from ymin to ymax by step
          for j below height
          do (loop for x from xmin to xmax by step
                   for i below width
                   do (multiple-value-bind (r g b)
                          (funcall function x y)
                        (funcall pixel-writer i j
                                 (coord->color r) (coord->color g) (coord->color b)))))))

(defun render (function &optional (width 512) (height 512))
  (let* ((png (make-instance 'png
                             :color-type :truecolor
                             :width width
                             :height height))
         (image (data-array png)))
    (do-render function
      (lambda (x y r g b)
        (setf (aref image y x 0) r
              (aref image y x 1) g
              (aref image y x 2) b))
      width height)
    png))

(defun render-to-file (function file &optional (width 512) (height 512))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (render-to-stream function stream width height)))

(defun render-to-stream (function stream &optional (width 512) (height 512))
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

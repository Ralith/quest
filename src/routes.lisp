(in-package :quest)

(declaim (optimize (debug 3)))

(defclass route (routes:route)
  ((handler :accessor route-handler :initarg :handler)))

(defvar *route-map* (make-instance 'routes:mapper))

(defun add-route (template handler)
  (routes:connect *route-map*
                  (make-instance 'route
                                 :template (routes:parse-template template)
                                 :handler handler)))

(defmacro defroute (name template &body body)
  (let ((args (mapcar #'symbolicate (routes:template-variables (routes:parse-template template)))))
    `(progn (defun ,name ,(when args
                            `(&key ,@args))
              ,@body)
            (add-route ,template ',name))))

(defun create-route-dispatcher (mapper)
  (lambda (request)
    (multiple-value-bind (route bindings)
        (routes:match mapper (hunchentoot:request-uri* request))
      (when route
        (lambda ()
          (apply (route-handler route) (alexandria:alist-plist bindings)))))))

(setf (symbol-function 'routes-dispatcher)
      (create-route-dispatcher *route-map*))

(setf *dispatch-table* (list 'routes-dispatcher #'default-dispatcher))

(load "glib.lisp")

(defun user-init ()
  (defparameter *theta* 0.0)
  (defparameter *length* 100.0)
  (defparameter *n* 500)
  (defparameter *pi-part* (/ (* 2.0 PI) *n*))
  )

(defun user-idle ()
  (sleep (/ 1.0 60.0)))

(defun user-display ()
  (setf *theta* (+ *theta* 0.01))
  
  (mapcar
    (lambda (n)
      (let ((rand-len (random 100)))
        (g2linec (/ *width* 2.0)
                (/ *height* 2.0)
                (+ (/ *width* 2.0) (* (cos (+ *theta* (* n *pi-part*))) (+ *length* rand-len)))
                (+ (/ *height* 2.0) (* (sin (+ *theta* (* n *pi-part*))) (+ *length* rand-len)))
                (random 1.0)
                (random 1.0)
                (random 1.0))
        )
      )
    (iota *n*)
    )
  )

(defun g2line (x y x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x y 0.0)
    (gl:vertex x2 y2 0.0))
  )

(defun g2linec (x y x2 y2 r g b)
  (gl:with-primitives :lines
    (gl:color r g b)
    (gl:vertex x y 0.0)
    (gl::vertex x2 y2 0.0))
  (gl:color 1.0 1.0 1.0)
  )

(defun iota (m &optional (n 0) (step 1))
  (if (>= n m)
    nil
    (cons n (iota m (+ n step) step)))
  )

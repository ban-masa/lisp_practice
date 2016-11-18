(require 'asdf)
(require 'cl-opengl)
(require 'cl-glut)
(require 'cl-glu)

(defvar *width* 500)
(defvar *height* 500)

(defun user-display () ())
(defun user-init () ())
(defun user-idle () ())

(defclass main-window (glut:window) ()
  (:default-initargs :title "opengl test" :mode '(:double :rgb :depth) :width *width* :height *height*))

(defmethod glut:display ((window main-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:shade-model :flat)
  (gl:normal 0 0 1)
  (user-display)
  (glut:swap-buffers)
  )

(defmethod glut:idle ((window main-window))
  (user-idle)
  (glut:post-redisplay)
  )

(defmethod glut:reshape ((window main-window) width height)
  (gl:viewport 0 0 width height)
  (gl:load-identity)
  (glu:ortho-2d 0.0 *width* *height* 0.0)
  )

(defmethod glut:display-window :before ((window main-window))
  (gl:clear-color 1 1 1 0)
  )

(defun main ()
  (user-init)
  (glut:display-window (make-instance 'main-window))
  )

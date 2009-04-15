;;
;; Time-stamp: <2006-04-21 00:13:58 taro> higepon@users.sourceforge.jp
;; Picture language.
;; "Structure and Interpretation of Computer Programs"
;;
;; Requirements:
;;
;;   * Gauche
;;     <http://www.shiro.dreamhost.com/scheme/gauche/>
;;   * Gauche-gl OpenGL binding for Gauche
;;     <http://www.shiro.dreamhost.com/scheme/gauche/>
;;
(use gl)
(use gl.glut)
(load "./segment.scm")
(load "./frame.scm")
(load "./vector.scm")
(load "./painter.scm")
(load "./monar.scm")
;(load "./wave.scm")

(define frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode GLUT_RGBA)
  (glut-init-window-size 350 240)
  (glut-create-window "picture language sample")
  (glut-display-func display)
  (init)
  (glut-main-loop)
  )

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color 0.0 0.0 0.0)
  (gl-begin GL_LINES)
;;  ((corner-split monar 6) frame)
  ((square-limit monar 4) frame)
;  ((square-limit wave 4) frame)
;  ((square-limit wave-smile 4) frame)
  (gl-end)
  (gl-flush)
  )

(define (init)
  (gl-clear-color 1.0 1.0 1.0 1.0)
  )

(define (draw-line p1 p2)
  (define (t z)
    (- (* 2 z) 1))
  (gl-vertex (t (x-point p1)) (t (y-point p1)))
  (gl-vertex (t (x-point p2)) (t (y-point p2)))
  )




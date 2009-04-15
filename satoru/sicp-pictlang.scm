;;
;; Time-stamp: <2000-06-29 11:54:16 satoru>
;;
;; An implementation of the picture language presented in the section 
;; 2.2.4 of "Structure and Interpretation of Computer Programs 2n ed."
;;
;; Requirements:
;;
;;   * Structure and Interpretation of Computer Programs 
;;     <http://mitpress.mit.edu/sicp/>
;;     <http://www.ipl.t.u-tokyo.ac.jp/sicp/> (Japanese)
;;   * Guile - GNU Project - Free Software Foundation (FSF)
;;     <http://www.fsf.org/software/guile/guile.html>
;;   * (guile-gtk)
;;     <http://www.ping.de/sites/zagadka/guile-gtk/>
;;
;; Usage:
;;
;;   % guile -s sicp-pictlang.scm
;;
;; Author: (although most codes were extracted from SICP)
;;
;;   Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>
;;

(use-modules (gtk gtk)
	     (gtk gdk))

(define interesting-patterns
  (list "(x-mark a-frame)"
	"(diamond a-frame)"
	"(lambda-mark a-frame)"
	"(triangle a-frame)"
	"(border a-frame)"
	"(wave a-frame)"
	"(x-logo a-frame)"
	"((below lambda-mark x-mark) a-frame)"
	"((beside wave (flip-vert wave)) a-frame)"
	"((right-split wave 4) a-frame)"
	"((tiling x-logo 2) a-frame)"
	"((corner-split lambda-mark 5) a-frame)"
	"((square-limit (shrink-to-upper-right diamond) 3) a-frame)"
	"((squash-inwards (square-limit border 4)) a-frame)"
	"((cross-limit border 6) a-frame)"
	"((cross-limit (compose-painter border x-logo) 6) a-frame)"
	"((cross-mirror-limit x-mark 6) a-frame)"
	"((square-limit wave 4) a-frame)"))

(define (identity x) x)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; supplemented by satoru-t - EXERCISE 2.48
(define (make-segment start end)
  (cons start end))

;; supplemented by satoru-t - EXERCISE 2.48
(define (start-segment s)
  (car s))

;; supplemented by satoru-t - EXERCISE 2.48
(define (end-segment s)
  (cdr s))

;; supplemented by satoru-t - EXERCISE 2.46
(define (make-vect x y)
  (cons x y))

;; supplemented by satoru-t - EXERCISE 2.46
(define (xcor-vect v)
  (car v))

;; supplemented by satoru-t - EXERCISE 2.46
(define (ycor-vect v)
  (cdr v))

;; supplemented by satoru-t - EXERCISE 2.46
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

;; supplemented by satoru-t - EXERCISE 2.46
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

;; supplemented by satoru-t - EXERCISE 2.46
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;; supplemented by satoru-t - EXERCISE 2.47
(define (origin-frame frame)
  (car frame))

;; supplemented by satoru-t - EXERCISE 2.47
(define (edge1-frame frame)
  (cadr frame))

;; supplemented by satoru-t - EXERCISE 2.47
(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
(define a-frame
  (make-frame (make-vect 0 0)
	      (make-vect 1 0)
	      (make-vect 0 1)))

(define b-frame
  (make-frame (make-vect 0 0)
	      (make-vect 0.5 0)
	      (make-vect 0 0.5)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; supplemented by satoru-t - EXERCISE 2.50
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2

;; supplemented by satoru-t - EXERCISE 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)    ; new origin
                     (make-vect 0.0 0.0)    ; new end of edge1
                     (make-vect 1.0 1.0)))  ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; supplemented by satoru-t - EXERCISE 2.50
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

;; supplemented by satoru-t - EXERCISE 2.50
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; supplemented by satoru-t - EXERCISE 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-above
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

;; added by satoru-t
(define (cross-limit painter n)
  (if (= n 0)
      painter
      (let ((top    (beside painter (cross-limit painter (- n 1))))
	    (bottom (beside (cross-limit painter (- n 1)) painter)))
	(below bottom top))))

;; added by satoru-t
(define (cross-mirror-limit painter n)
  (let ((cross (cross-limit painter n)))
    (compose-painter cross (flip-vert cross))))

;; added by satoru-t
(define (tiling painter n)
  (if (= n 0)
      painter
      (let ((combine4 (square-of-four identity identity
				      identity identity)))
	(tiling (combine4 painter) (- n 1)))))

;; added by satoru-t
(define (compose-painter . painters)
  (lambda (frame)
    (for-each (lambda (painter) (painter frame)) painters)))

(define (read-text text)
  (define (read-all port)
    (let loop ((res '())
	       (val (read port)))
      (if (eof-object? val)
	  (reverse! res)
	  (loop (cons val res) (read port)))))
  (car (call-with-input-string text read-all)))

(define (draw-area-new width height sexp-combo
			eval-button clear-button demo-button)
  (let ((widget (gtk-drawing-area-new))
	(pixmap #f)
	(window #f)
	(fore-gc #f)
	(back-gc #f)
	(history '()))

    (define (realize)
      (set! window (gtk-widget-window widget))
      (let ((style (gtk-widget-style widget)))
	(set! fore-gc (gtk-style-fg-gc style 'normal))
	(set! back-gc (gtk-style-bg-gc style 'normal)))
      (configure #f))

    (define (configure ev)
      (cond (ev
	     (set! width (gdk-event-configure-width ev))
	     (set! height (gdk-event-configure-height ev))))
      (cond (window
	     (set! pixmap window))))

    (define (expose ev)
      (update))

    (define (update)
 	(for-each (lambda (proc) (proc))
		  (reverse history)))

    (define (clear)
      (gdk-draw-rectangle pixmap back-gc #t 0 0 width height)
      (clear-history))

    (define (add-history proc)
      (set! history (cons proc history)))

    (define (clear-history)
      (set! history '()))

    (define (scale-x x)
      (round (* width  x)))

    (define (scale-y y)
      (round (* height  (- 1 y))))

    (define (draw-line p1 p2)
      (let* ((x1 (xcor-vect p1))
	     (y1 (ycor-vect p1))
	     (x2 (xcor-vect p2))
	     (y2 (ycor-vect p2))
	     (proc (lambda ()
		     (gdk-draw-line pixmap fore-gc
				    (scale-x x1)
				    (scale-y y1)
				    (scale-x x2)
				    (scale-y y2)))))
	(proc)
	(add-history proc)))

    (define (draw-polygon poly)
      (let ((proc (lambda ()
		    (gdk-draw-polygon
		     pixmap fore-gc #t
		     (map (lambda (point)
			    (cons (scale-x (xcor-vect point))
				  (scale-y (ycor-vect point)))) poly)))))
	(proc)
	(add-history proc)))

    (define (segments->painter segment-list)
      (lambda (frame)
	(for-each
	 (lambda (segment)
	   (draw-line
	    ((frame-coord-map frame) (start-segment segment))
	    ((frame-coord-map frame) (end-segment segment))))
	 segment-list)))

    (define (polygon->painter polygon)
      (lambda (frame)
	(draw-polygon
	 (map (lambda (point)
		((frame-coord-map frame) point))
	      polygon))))

    ;; supplemented by satoru-t - EXERCISE 2.49
    (define (border frame)
      (let ((tl (make-vect 0 1))
	    (tr (make-vect 1 1))
	    (bl (make-vect 0 0))
	    (br (make-vect 1 0)))
	((segments->painter
	  (list (make-segment tl tr)
		(make-segment tr br)
		(make-segment br bl)
		(make-segment bl tl))) frame)))

    ;; supplemented by satoru-t - EXERCISE 2.49
    (define (x-mark frame)
      (let ((tl (make-vect 0 1))
	    (tr (make-vect 1 1))
	    (bl (make-vect 0 0))
	    (br (make-vect 1 0)))
	((segments->painter
	  (list (make-segment tl br)
		(make-segment tr bl))) frame)))

    ;; supplemented by satoru-t - EXERCISE 2.49
    (define (diamond frame)
      (let ((n (make-vect 0.5 1))
	    (w (make-vect 0 0.5))
	    (e (make-vect 1 0.5))
	    (s (make-vect 0.5 0)))
	((segments->painter
	  (list (make-segment n e)
		(make-segment e s)
		(make-segment s w)
		(make-segment w n))) frame)))

    ;; supplemented by satoru-t - EXERCISE 2.49
    (define (wave frame)
      (let ((p01 (make-vect 0.40 1.00))
	    (p02 (make-vect 0.60 1.00))
	    (p03 (make-vect 0.00 0.80))
	    (p04 (make-vect 0.35 0.80))
	    (p05 (make-vect 0.65 0.80))
	    (p06 (make-vect 0.00 0.60))
	    (p07 (make-vect 0.30 0.60))
	    (p08 (make-vect 0.40 0.60))
	    (p09 (make-vect 0.60 0.60))
	    (p10 (make-vect 0.70 0.60))
	    (p11 (make-vect 0.20 0.55))
	    (p12 (make-vect 0.30 0.55))
       	    (p13 (make-vect 0.35 0.50))
	    (p14 (make-vect 0.65 0.50))
	    (p15 (make-vect 0.20 0.45))
	    (p16 (make-vect 1.00 0.40))
	    (p17 (make-vect 0.50 0.20))
	    (p18 (make-vect 1.00 0.20))
       	    (p19 (make-vect 0.25 0.00))
	    (p20 (make-vect 0.40 0.00))
	    (p21 (make-vect 0.60 0.00))
	    (p22 (make-vect 0.75 0.00)))
      ((segments->painter
	(list (make-segment p01 p04)
	      (make-segment p04 p08)
	      (make-segment p08 p07)
	      (make-segment p07 p11)
	      (make-segment p11 p03)
	      (make-segment p06 p15)
	      (make-segment p15 p12)
	      (make-segment p12 p13)
	      (make-segment p13 p19)
	      (make-segment p20 p17)
	      (make-segment p17 p21)
	      (make-segment p22 p14)
	      (make-segment p14 p18)
	      (make-segment p16 p10)
	      (make-segment p10 p09)
	      (make-segment p09 p05)
	      (make-segment p05 p02))) frame)))

    ;; added by satoru-t
    (define (lambda-mark frame)
      (let ((tl (make-vect 0.0 1.0))
	    (c  (make-vect 0.5 0.5))
	    (bl (make-vect 0.0 0.0))
	    (br (make-vect 1.0 0.0)))
	((segments->painter
	  (list (make-segment tl br)
		(make-segment c  bl))) frame)))

    ;; added by satoru-t
    (define (triangle frame)
      (let ((t  (make-vect 0.5 1))
	    (bl (make-vect 0 0))
	    (br (make-vect 1 0)))
	((segments->painter
	  (list (make-segment t bl)
		(make-segment t br)
		(make-segment bl br))) frame)))

    ;; added by satoru-t
    ;; NOTE: This is not the official one.
    ;; See <ftp://ftp.x.org/pub/R6.4/xc/lib/Xmu/DrawLogo.c> for details.
    (define (x-logo frame)
      (let* ((p01 (make-vect 0.00 1.00))
	     (p02 (make-vect 0.25 1.00))
	     (p03 (make-vect 0.91 1.00))
	     (p04 (make-vect 1.00 1.00))
	     (p05 (make-vect (/ 347 620) (/ 273 465)))
	     (p06 (make-vect (/ 12 31) (/ 15 31)))
	     (p07 (make-vect (/ 19 31) (/ 16 31)))
	     (p08 (make-vect (/ 273 620) (/ 192 465)))
	     (p09 (make-vect 0.00 0.00))
	     (p10 (make-vect 0.09 0.00))
	     (p11 (make-vect 0.75 0.00))
	     (p12 (make-vect 1.00 0.00))
	     (poly1 (list p02 p05 p10 p09 p06 p01))
	     (poly2 (list p04 p07 p12 p11 p08 p03)))
	((polygon->painter poly1) frame)
	((polygon->painter poly2) frame)))

    (define (draw-pattern pat)
      (local-eval pat (the-environment)))

    (define (show-demo)
      (let* ((patterns-left interesting-patterns)
	     (dialog (gtk-dialog-new))
	     (label  (gtk-label-new "View the next picture?"))
	     (yes-button (gtk-button-new-with-label "Yes"))
	     (no-button  (gtk-button-new-with-label "No")))
	(define (show-one-demo)
	  (let ((pat (car patterns-left)))
	    (clear)
	    (gtk-entry-set-text (gtk-combo-entry sexp-combo) pat)
	    (draw-pattern (read-text pat))
	    (set! patterns-left (cdr patterns-left))
	    (if (null? patterns-left)
		 (gtk-widget-destroy dialog))))

	(gtk-window-set-title dialog "Demonstration")
	(gtk-container-border-width dialog 0)
	(gtk-widget-set-usize dialog 200 100)

	(gtk-widget-set-flags yes-button '(can-default))
	(gtk-widget-set-flags no-button  '(can-default))
	(gtk-widget-grab-default yes-button)

	(show-one-demo)

	(gtk-box-pack-start (gtk-dialog-vbox dialog)
			    label #t #t 0)
	(gtk-box-pack-start (gtk-dialog-action-area dialog)
			    yes-button #t #t 0)
	(gtk-box-pack-start (gtk-dialog-action-area dialog)
			    no-button #t #t 0)

	(gtk-signal-connect yes-button "clicked" show-one-demo)
	(gtk-signal-connect no-button  "clicked"
			    (lambda ()
			      (gtk-widget-destroy dialog)))
	(gtk-widget-show-all dialog)))


    (gtk-drawing-area-size widget width height)
    (gtk-signal-connect widget "realize" realize)
    (gtk-signal-connect widget "expose_event" expose)
    (gtk-signal-connect widget "configure_event" configure)

    (gtk-signal-connect eval-button "clicked"
			(lambda ()
			  (draw-pattern
			   (read-text (gtk-entry-get-text
				       (gtk-combo-entry sexp-combo))))))
    (gtk-signal-connect clear-button "clicked" clear)
    (gtk-signal-connect demo-button  "clicked" show-demo)

    widget))

(let* ((window (gtk-window-new 'toplevel))
       (vbox   (gtk-vbox-new #f 2))
       (hbox   (gtk-hbox-new #f 3))
       (combo  (gtk-combo-new))
       (eval-button  (gtk-button-new-with-label "Eval"))
       (clear-button (gtk-button-new-with-label "Clear"))
       (demo-button  (gtk-button-new-with-label "Demo"))
       (area   (draw-area-new 400 400
			       combo eval-button clear-button demo-button)))

  (gtk-combo-set-popdown-strings combo
				 (list->vector interesting-patterns))

  (gtk-window-set-title window "SICP: A Picture Language")

  (gtk-box-pack-start hbox combo #t #t 0)
  (gtk-box-pack-end hbox demo-button #f #f 0)
  (gtk-box-pack-end hbox clear-button #f #f 0)
  (gtk-box-pack-end hbox eval-button #f #f 0)

  (gtk-container-add window vbox)
  (gtk-box-pack-start vbox area  #t #t 0)
  (gtk-box-pack-end vbox hbox  #f #f 0)

  (gtk-container-border-width window 10)

  (gtk-widget-show-all window)
  (gtk-standalone-main window))


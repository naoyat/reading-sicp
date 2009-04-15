(use srfi-42) ; 先行評価的内包表記

;(define (!= x y) (not (= x y)))

(define (hit? x0 y0 x1 y1)
  (or ;(= x0 x1)
	  (= y0 y1)
	  (= (+ x0 y0) (+ x1 y1))
	  (= (- x0 y0) (- x1 y1)) ))

(list-ec (: ax 0 1) (: ay 0 8)

		 (: bx 1 2) (: by 0 8)
		 (not (hit? bx by ax ay))

		 (: cx 2 3) (: cy 0 8)
		 (not (hit? cx cy bx by))
		 (not (hit? cx cy ax ay))

		 (: dx 3 4) (: dy 0 8)
		 (not (hit? dx dy cx cy))
		 (not (hit? dx dy bx by))
		 (not (hit? dx dy ax ay))

		 (: ex 4 5) (: ey 0 8)
		 (not (hit? ex ey dx dy))
		 (not (hit? ex ey cx cy))
		 (not (hit? ex ey bx by))
		 (not (hit? ex ey ax ay))

		 (: fx 5 6) (: fy 0 8)
		 (not (hit? fx fy ex ey))
		 (not (hit? fx fy dx dy))
		 (not (hit? fx fy cx cy))
		 (not (hit? fx fy bx by))
		 (not (hit? fx fy ax ay))

		 (: gx 6 7) (: gy 0 8)
		 (not (hit? gx gy fx fy))
		 (not (hit? gx gy ex ey))
		 (not (hit? gx gy dx dy))
		 (not (hit? gx gy cx cy))
		 (not (hit? gx gy bx by))
		 (not (hit? gx gy ax ay))

		 (: hx 7 8) (: hy 0 8)
		 (not (hit? hx hy gx gy))
		 (not (hit? hx hy fx fy))
		 (not (hit? hx hy ex ey))
		 (not (hit? hx hy dx dy))
		 (not (hit? hx hy cx cy))
		 (not (hit? hx hy bx by))
		 (not (hit? hx hy ax ay))

		 (print (list (list ax ay)
					  (list bx by)
					  (list cx cy)
					  (list dx dy)
					  (list ex ey)
					  (list fx fy)
					  (list gx gy)
					  (list hx hy))
				))
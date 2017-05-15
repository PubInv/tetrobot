;; This is my attempt to implement a universal number
;; of the bc helix. Up until now, this file has
;; really had no conception of the geometry of the robot.
;; However, remaining at the concept of nets, this
;; code is dependent on the particular geometry of the tetrahelix.
;; If you build a tetrobot with a non-tetrahelix geometry,
;; you probably should not use any of the funciton without great care.

;; We're first creating a basic tetrahelix numbering scheme,
;; in which we number nodes of the tetrahelix in the order
;; they appear along the axis of the tetrahelix.
;; We then furthermore order the edges in the same way.

;; This is the "edge number" as a function two nodes.
;; return nil if they are not connected.

;; There is something wrong here, this is not giving
;; unique answers. (en 2 3) == (en 1 4), but this is WRONG!
(defun en (x y)
  (if (> (abs (- x y)) 3)
      nil
    (if (= x y)
	nil
      (if (< x y)
	  (enx x y)
	(enx y x)))))

(defun enx (x y)
  (cond ((= x 0)
	 (cond
	  ((= y 1)
	   0)
	  ((= y 2)
	   1)
	  ((= y 3)
	   3)))
	((= x 1)
	 (cond
	  ((= y 2)
	   2)
	  ((= y 3)
	   4)
	  ((= y 4)
	   6)))
	(t
	 (let ((d (- y x))
	       (n (neb (- y 1))))
	   (assert (and (>= d 1) (< d 4)))
	   (cond
	    ((= d 1)
	     (+ n 2))
	    ((= d 2)
	     (+ n 1))
	    ((= d 3)
	     (+ n 0)))))))

;; this is "number of edges behind"
(defun neb (x)
  (cond
   ((= x 0)
    0)
   ((= x 1)
    1)
   ((= x 2)
    3)
   ((>= x 3)
    (* 3 (- x 1)))))

(defun test-neb ()
  (let (value)      ; otherwise a value is a void variable
    (dotimes (number 30 value)
      (print (format "%d %d" number (neb number))))))

(defun test-en ()
  (let ((value)      ; otherwise a value is a void variable
	(numbers nil))
    (dotimes (x 10 value)
      (dotimes (y 10 value)
	(let ((v (en x y)))
	  (if (and (< x y) (numberp v))
	      (progn
		(setq numbers (cons v numbers))
		(print (format "(%d,%d) %d)" x y (en x y)))
		)
	    )
	  )))
    (print (sort numbers '<))
    )
  )

;; define color constants
(defvar RED 0)
(defvar YEL 1)
(defvar BLU 2)
(defvar ORA 3)
(defvar GRN 4)
(defvar PRP 5)

(defun nd-from-color (c n)
  (+ (* n 3) c)
  )


(defun num-red-edges (nds)
  (/ (- nds 1) 3))
(defun num-yel-edges (nds)
  (/ (- nds 2) 3))
(defun num-blu-edges (nds)
  (/ (- nds 3) 3))


;; This now seems suspect to me...
(defun num-red-nds (nds)
  (if (> nds 0)
      (+ (num-red-edges nds) 1)
    nil))
(defun num-yel-nds (nds)
  (if (> nds 1)
      (+ (num-yel-edges nds) 1)
    nil))
(defun num-blu-nds (nds)
  (if (> nds 2)
      (+ (num-blu-edges nds) 1)
    nil))

(defun num-ora-edges (nds)
  (if (> nds 1)
      (+ -1 (num-red-nds nds) (num-yel-nds nds))
  nil))
(defun num-grn-edges (nds)
  (if (> nds 2)
      (+ -1 (num-blu-nds nds) (num-yel-nds nds))
  nil))
(defun num-prp-edges (nds)
  (if (> nds 2)
      (+ -1 (num-red-nds nds) (num-blu-nds nds))
  nil))

(defun num-edges (color nds)
  (cond
   ((= color RED)
    (num-red-edges nds))
   ((= color YEL)
    (num-yel-edges nds))
   ((= color BLU)
    (num-blu-edges nds))
   ((= color ORA)
    (num-ora-edges nds))
   ((= color GRN)
    (num-grn-edges nds))
   ((= color PRP)
    (num-prp-edges nds))
  ))

(defun nine-list (st cnt)
  (if ( = cnt 0)
      nil
      (cons st
	    (nine-list (+ 9 st) (- cnt 1)))
      )
  )
(defun hopping (col1 col2 n)
  (let ((n2 (/ n 2)))
    (if (= (% n 2) 0)
	(en (nd-from-color col1 n2)
			      (nd-from-color col2 n2))
      (en (nd-from-color col1 (+ n2 1))
			    (nd-from-color col2 n2)))))

;; Get all edges of a color returned by
;; their universal edge number, based on the number of actuators
(defun all-edges (color nds)
  (let ((num (num-edges color nds))
	(rednds (num-red-nds nds))
	(yelnds (num-yel-nds nds))
	(blunds (num-blu-nds nds)))    
    (cond
     ((= color RED) 
      (nine-list 3 num))
     ((= color YEL)
      (nine-list 6 num))
     ((= color BLU)
      (nine-list 9 num))
     ((= color ORA)
      (let ((edges ()))
	(dotimes (nc num edges)
	  (setq edges (cons (hopping RED YEL nc) edges))
	  )))
     ((= color GRN)
      (let ((edges ()))
	(dotimes (nc num edges)
	  (setq edges (cons (hopping YEL BLU nc) edges))
	  )))
     ((= color PRP)
      (let ((edges ()))
	(dotimes (nc num edges)
	  (setq edges (cons (hopping RED BLU nc) edges))
	  )))
     )))


(defun test-num-edges ()
  (let ((value))
    (dotimes (i 10 value)
      (print (format "RED %d %s" i (num-edges RED i)))
      (print (format "YEL %d %s" i (num-edges YEL i)))
      (print (format "BLU %d %s" i (num-edges BLU i)))                 
      (print (format "ORA %d %s" i (num-edges ORA i)))
      (print (format "GRN %d %s" i (num-edges GRN i)))
      (print (format "PRP %d %s" i (num-edges PRP i)))                 
      )
    )
  )

(defun test-all-edges ()
  (assert (= 1 (length (all-edges RED 4))))
  (assert (= 2 (length (all-edges RED 7))))
  (assert (= 4 (length (all-edges ORA 7))))

  ;; Now we will try to run a circuit test by
  ;; looking up all edges for all covers, unioning,
  ;; and making sure each edge is found just once.
  (let* ((r (all-edges RED 10))
	(y (all-edges YEL 10))
	(b (all-edges BLU 10))
	(o (all-edges ORA 10))
	(g (all-edges GRN 10))
	(p (all-edges PRP 10))
	(all (append r y b o p g))
	)
    (assert (sort all '<) (numeric-sequence 0 23))
    )
  t  
)


;; produce a pose that arcs in the "red" direction
;; which is generally up
;; Arguments are "na -- number of actuators"

;; This is really just a test function to make sure I am understanding...
(defun test-arc-len (col len nds)
  (p (mapcar #'(lambda (x) (list x len))
	     (mapcar #'actuator-name-from-num (all-edges col nds)))))

(defun arc-up ()
  (test-arc-len RED 1000 10))


;; These are in meters..
(defvar MAX_EDGE_LENGTH 0.420)
(defvar MIN_EDGE_LENGTH 0.290)
(defvar MED_EDGE_LENGTH (/ (+ MAX_EDGE_LENGTH MIN_EDGE_LENGTH) 2))

(defun act-len-to-range (m)
  (* (- m MIN_EDGE_LENGTH) (/  1023.0 (- MAX_EDGE_LENGTH MIN_EDGE_LENGTH))))

(defun act-range-to-len (r)
  (+ MIN_EDGE_LENGTH (* (/ r 1023.0) (- MAX_EDGE_LENGTH MIN_EDGE_LENGTH))))


;; We should really define "inverts" as a macro so it could be used for any function...
(defun test-conversions ()
  (let ((inverts (lambda (x)
		      (assert (= (act-range-to-len (act-len-to-range x)) x)))))
    (funcall inverts 0.4)
    (funcall inverts MIN_EDGE_LENGTH)
  ))


(defun alt-app0 (a b c acc)
  (if (null a)
      acc
    (alt-app1 (cdr a) b c (cons (list (car a) b) acc))))

(defun alt-app1 (a b c acc)
  (if (null a)
      acc
    (alt-app0 (cdr a) b c (cons (list (car a) c) acc))))

(defun alternate-app (a b c)
  (alt-app0 a b c nil))

(defun append-to-all-aux (lst a acc)
  (if (null lst)
      acc
    (append-to-all-aux  (cdr lst) a (cons (list (car lst) a) acc))))

(defun append-to-all (lst a)
  (append-to-all-aux lst a '()))

(defun all-acts (col nds)
 (mapcar #'actuator-name-from-num (all-edges col nds)))
  
;; implementing the equitetrabeam math is one of the main points of this
;; file.  f = 1.0 is the normal pose.
(defun equitetrabeam-pose (nds f)
  (let* ((len  MED_EDGE_LENGTH)
	 (med-r (round (act-len-to-range len)))
	 (red med-r)
	 (yellow med-r)
	 (blue med-r)
	 (two-hop-r (round
		     (act-len-to-range (*
					f
					(/ 2 (sqrt 3))
					len))))
	 (one-hop med-r)
	 
	 (green_even med-r)
	 (green_odd two-hop-r)

	 (orange_even two-hop-r)	 
	 (orange_odd  med-r)
	 
	 (purple_even  med-r)	 
	 (purple_odd  two-hop-r)
	 
	 (r (all-acts RED nds))
	 (y (all-acts YEL nds))
	 (b (all-acts BLU nds))
	 ;; This should proabably be sorting from lowest to highest first.
	 (o (alternate-app (all-acts ORA nds) orange_even orange_odd))
	 (g (alternate-app (all-acts GRN nds) green_even green_odd))
	 (p (alternate-app (all-acts PRP nds) purple_even purple_odd)))
    (print p)
    (print o)
	  (append
	   o p g
	   (append-to-all
	    (append r y b)
	    med-r)
	   )
    )
  )

(defun test-equtb (f)
  (p (equitetrabeam-pose 10 f)))

(defun flat ()
  (test-equtb 1.0)
  )

;; Now exploring is it possible to do a "lean forward"
;; f > 1.0 is depth of lean
;; TODO: This doesn't seem to work at all---coming back after a break I need to relearn it.
(defun lean-pose (nds f)
  (let* ((len (/  (+ MIN_EDGE_LENGTH  MED_EDGE_LENGTH) 2))
	 (med-r (round (act-len-to-range len)))
	 (ab (shift-lengths
	      len
	      (* (/ 2 (sqrt 3)) len)
	      len
	      (* len f)))
	 (a (car ab))
	 (b (cdr ab))
	 (red med-r)
	 (yellow med-r)
	 (blue med-r)
	 (two-hop-r (round
		     (act-len-to-range (*
					(/ 2 (sqrt 3))
					len))))
	 (one-hop med-r)

	 (green_even med-r)
	 (green_odd two-hop-r)

	 (orange_odd  (round (act-len-to-range a)))
	 (purple_even (round (act-len-to-range a)))
	 
	 (orange_even (round (act-len-to-range b)))
	 (purple_odd  (round (act-len-to-range b)))
	 
	 (r (all-acts RED nds))
	 (y (all-acts YEL nds))
	 (b (all-acts BLU nds))
	 
	 (o (alternate-app (all-acts ORA nds) orange_even orange_odd))
	 (g (alternate-app (all-acts GRN nds) green_even green_odd))
	 (p (alternate-app (all-acts PRP nds) purple_even purple_odd)))
    (print orange_odd)
    (print orange_even)    
	  (append
	   o p g
	   (append-to-all
	    (append r y b)
	    med-r)
	   )
    )
  )

(defun test-lean (f)
  (p (lean-pose 10 f)))

(defun heron_a (a b c)
  (let ((s (/ (+ a b c) 2)))
    (/ (* 2 (sqrt (* s (- s a) (- s b) (- s c))))
       a)
    )
  )
;; Do we want to 
(defun shift-lengths (a b c d)
  (let* ((P (heron_a c a b))
	 (PP (* P P))
	 (XX (- (* a a) (* P P)))
	 (YY (- (* b b) (* P P)))
	 (x (sqrt XX))
	 (dd (* d d))
	 (ApAp (+ PP XX dd (- (* 2 x d))))
	 (BpBp (+ PP XX dd (* 2 x d))))
    (print (cons (sqrt ApAp) (sqrt BpBp)))
    (cons (sqrt ApAp) (sqrt BpBp))
    )
  )


;; TODO: I have reason to be leave the robot's physical
;; wiring doesn't match the current number scheme.  I need to test this.
;; Some of what I have done will be duplicating what i have done in javascript...sadly.

(defun walk1 ()
  (dance '(( (A0 250) (A1 634) (A2 250) (A3 250) (A4 634) (A5 250) (B0 250) (B1 634) (B2 250) (B3 250) (B4 634) (B5 250) (C0 250) (C1 634) (C2 250) (C3 250) (C4 634) (C5 250) (D0 250) (D1 634) (D2 250) (D3 250) (D4 634) (D5 250) )
( (A0 444) (A2 666) (A1 1019) (A5 743) (A4 647) (A3 559) (B2 1016) (B1 577) (B0 1013) (B5 513) (B4 1004) (B3 501) (C2 565) (C1 717) (C0 585) (C5 1016) (C4 544) (C3 512) (D2 0) (D1 982) (D0 422) (D5 454) (D4 858) (D3 0) )
( (A0 451) (A2 665) (A1 1019) (A5 743) (A4 671) (A3 559) (B2 1016) (B1 577) (B0 1021) (B5 513) (B4 1004) (B3 501) (C2 565) (C1 717) (C0 585) (C5 1016) (C4 544) (C3 512) (D2 0) (D1 982) (D0 422) (D5 454) (D4 858) (D3 0) )
( (A0 451) (A2 665) (A1 1019) (A5 743) (A4 671) (A3 559) (B2 1016) (B1 577) (B0 1021) (B5 671) (B4 633) (B3 333) (C2 550) (C1 717) (C0 585) (C5 1016) (C4 1010) (C3 512) (D2 0) (D1 982) (D0 775) (D5 454) (D4 858) (D3 0) )
( (A0 451) (A2 665) (A1 1019) (A5 743) (A4 671) (A3 559) (B2 1016) (B1 577) (B0 1021) (B5 539) (B4 709) (B3 5) (C2 816) (C1 717) (C0 585) (C5 1016) (C4 1023) (C3 512) (D2 0) (D1 982) (D0 996) (D5 454) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 743) (A4 1021) (A3 559) (B2 1016) (B1 577) (B0 940) (B5 533) (B4 718) (B3 0) (C2 824) (C1 717) (C0 585) (C5 1016) (C4 1020) (C3 512) (D2 0) (D1 982) (D0 996) (D5 454) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 743) (A4 1021) (A3 559) (B2 368) (B1 208) (B0 475) (B5 353) (B4 718) (B3 0) (C2 824) (C1 570) (C0 585) (C5 1016) (C4 1020) (C3 1018) (D2 0) (D1 982) (D0 996) (D5 454) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 743) (A4 1021) (A3 559) (B2 383) (B1 194) (B0 459) (B5 360) (B4 718) (B3 0) (C2 824) (C1 598) (C0 585) (C5 1016) (C4 1020) (C3 1023) (D2 0) (D1 982) (D0 996) (D5 454) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 743) (A4 1021) (A3 559) (B2 383) (B1 194) (B0 459) (B5 360) (B4 718) (B3 0) (C2 824) (C1 598) (C0 585) (C5 1016) (C4 1020) (C3 1023) (D2 736) (D1 348) (D0 321) (D5 606) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 743) (A4 1021) (A3 559) (B2 383) (B1 194) (B0 459) (B5 360) (B4 718) (B3 0) (C2 824) (C1 598) (C0 585) (C5 1016) (C4 1020) (C3 1023) (D2 521) (D1 368) (D0 13) (D5 764) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 743) (A4 1021) (A3 559) (B2 876) (B1 202) (B0 484) (B5 543) (B4 718) (B3 0) (C2 824) (C1 1023) (C0 585) (C5 1016) (C4 1020) (C3 949) (D2 483) (D1 381) (D0 0) (D5 773) (D4 858) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 753) (A4 1023) (A3 545) (B2 896) (B1 202) (B0 484) (B5 543) (B4 734) (B3 0) (C2 824) (C1 1023) (C0 602) (C5 403) (C4 384) (C3 502) (D2 1) (D1 381) (D0 0) (D5 773) (D4 671) (D3 0) )
( (A0 769) (A2 613) (A1 1019) (A5 753) (A4 1023) (A3 545) (B2 896) (B1 202) (B0 484) (B5 543) (B4 734) (B3 0) (C2 824) (C1 1023) (C0 602) (C5 651) (C4 341) (C3 310) (D2 234) (D1 381) (D0 0) (D5 773) (D4 1023) (D3 0) )
( (A0 769) (A2 602) (A1 12) (A5 434) (A4 1023) (A3 545) (B2 896) (B1 986) (B0 484) (B5 543) (B4 734) (B3 554) (C2 824) (C1 1023) (C0 602) (C5 651) (C4 341) (C3 310) (D2 234) (D1 381) (D0 0) (D5 773) (D4 1023) (D3 0) )
( (A0 769) (A2 570) (A1 64) (A5 610) (A4 1023) (A3 545) (B2 896) (B1 1023) (B0 484) (B5 543) (B4 734) (B3 670) (C2 824) (C1 1023) (C0 602) (C5 651) (C4 341) (C3 310) (D2 234) (D1 381) (D0 0) (D5 773) (D4 1023) (D3 0) )
( (A0 769) (A2 520) (A1 480) (A5 1020) (A4 1023) (A3 54(5) (B2 896) (B1 913) (B0 484) (B5 543) (B4 734) (B3 772) (C2 824) (C1 1023) (C0 602) (C5 651) (C4 341) (C3 310) (D2 234) (D1 381) (D0 0) (D5 773) (D4 1023) (D3 0) )
( (A0 1011) (A2 520) (A1 735) (A5 1020) (A4 1023) (A3 729) (B2 896) (B1 913) (B0 484) (B5 543) (B4 734) (B3 772) (C2 824) (C1 1023) (C0 602) (C5 651) (C4 341) (C3 310) (D2 234) (D1 381) (D0 0) (D5 773) (D4 1023) (D3 0) )
( (A0 250) (A1 634) (A2 250) (A3 250) (A4 634) (A5 250) (B0 250) (B1 634) (B2 250) (B3 250) (B4 634) (B5 250) (C0 250) (C1 634) (C2 250) (C3 250) (C4 634) (C5 250) (D0 250) (D1 634) (D2 250) (D3 250) (D4 634) (D5 250) )
))))

(defun walk0 ()
  (dance '(( (A0 250) (A1 634) (A2 250) (A3 250) (A4 634) (A5 250) (B0 250) (B1 634) (B2 250) (B3 250) (B4 634) (B5 250) (C0 250) (C1 634) (C2 250) (C3 250) (C4 634) (C5 250) (D0 250) (D1 634) (D2 250) (D3 250) (D4 634) (D5 250) )
( (A0 7) (A2 577) (A1 1009) (A5 445) (A4 709) (A3 370) (B2 857) (B1 712) (B0 994) (B5 290) (B4 1021) (B3 286) (C2 477) (C1 1024) (C0 263) (C5 797) (C4 640) (C3 241) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 69) (A2 590) (A1 1009) (A5 445) (A4 808) (A3 370) (B2 857) (B1 712) (B0 1023) (B5 290) (B4 1021) (B3 286) (C2 477) (C1 1024) (C0 263) (C5 797) (C4 640) (C3 241) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 359) (A2 551) (A1 1009) (A5 445) (A4 1026) (A3 370) (B2 857) (B1 712) (B0 951) (B5 290) (B4 1021) (B3 286) (C2 477) (C1 1024) (C0 263) (C5 797) (C4 640) (C3 241) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 359) (A2 387) (A1 520) (A5 271) (A4 1026) (A3 370) (B2 857) (B1 1016) (B0 951) (B5 290) (B4 1021) (B3 703) (C2 477) (C1 1024) (C0 263) (C5 797) (C4 640) (C3 241) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 359) (A2 371) (A1 537) (A5 321) (A4 1026) (A3 370) (B2 857) (B1 1023) (B0 951) (B5 290) (B4 1021) (B3 726) (C2 477) (C1 1024) (C0 263) (C5 797) (C4 640) (C3 241) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 321) (A4 1026) (A3 781) (B2 39) (B1 538) (B0 225) (B5 617) (B4 1021) (B3 726) (C2 477) (C1 891) (C0 263) (C5 797) (C4 640) (C3 1019) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 321) (A4 1026) (A3 781) (B2 39) (B1 538) (B0 225) (B5 617) (B4 1021) (B3 726) (C2 477) (C1 891) (C0 263) (C5 797) (C4 640) (C3 1019) (D2 254) (D1 1008) (D0 212) (D5 661) (D4 1016) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 321) (A4 1026) (A3 781) (B2 39) (B1 538) (B0 225) (B5 352) (B4 376) (B3 214) (C2 222) (C1 891) (C0 263) (C5 797) (C4 1023) (C3 1019) (D2 254) (D1 1008) (D0 752) (D5 661) (D4 1016) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 321) (A4 1026) (A3 781) (B2 39) (B1 538) (B0 225) (B5 352) (B4 376) (B3 214) (C2 222) (C1 891) (C0 263) (C5 797) (C4 1023) (C3 1019) (D2 254) (D1 1008) (D0 752) (D5 661) (D4 1016) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 327) (A4 1027) (A3 764) (B2 55) (B1 538) (B0 225) (B5 352) (B4 395) (B3 214) (C2 222) (C1 891) (C0 279) (C5 0) (C4 519) (C3 256) (D2 588) (D1 1008) (D0 752) (D5 661) (D4 791) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 327) (A4 1027) (A3 764) (B2 55) (B1 538) (B0 225) (B5 352) (B4 395) (B3 214) (C2 222) (C1 891) (C0 279) (C5 158) (C4 451) (C3 115) (D2 635) (D1 1008) (D0 752) (D5 661) (D4 1023) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 327) (A4 1027) (A3 764) (B2 55) (B1 538) (B0 225) (B5 352) (B4 395) (B3 214) (C2 222) (C1 891) (C0 279) (C5 158) (C4 451) (C3 115) (D2 324) (D1 95) (D0 6) (D5 247) (D4 1026) (D3 -4) )
( (A0 773) (A2 371) (A1 1025) (A5 327) (A4 1027) (A3 764) (B2 55) (B1 538) (B0 225) (B5 352) (B4 395) (B3 214) (C2 222) (C1 891) (C0 279) (C5 158) (C4 451) (C3 115) (D2 324) (D1 95) (D0 6) (D5 247) (D4 1026) (D3 -4) )
( (A0 250) (A1 634) (A2 250) (A3 250) (A4 634) (A5 250) (B0 250) (B1 634) (B2 250) (B3 250) (B4 634) (B5 250) (C0 250) (C1 634) (C2 250) (C3 250) (C4 634) (C5 250) (D0 250) (D1 634) (D2 250) (D3 250) (D4 634) (D5 250) )
)))

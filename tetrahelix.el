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
	   5)))
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
  (let (value)      ; otherwise a value is a void variable
    (dotimes (x 5 value)
      (dotimes (y 5 value)
	(let ((v (en x y)))
	  (if (numberp v)
	      (print (format "(%d,%d) %d)" x y (en x y)))
	    )
	)))))

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

;; We want this return the equitetrabeam pose.
;; This requires an understanding of our coloring
;; and numbering system as well as 
(defun equitetrabeam ()
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
;; 
(defun arc-red (nd)
  )

(require 'cl)

(setq TETA "/dev/cu.2TETBOT-RNI-SPP")
(setq TETB "/dev/cu.3TETBOTB-RNI-SPP")
(setq CONTROLLER-PORTS (list (cons 'A TETA)  (cons 'B TETB)))
(setq BAUD_RATE 19200)

;; This could be done automatically, but the benefit of that is small untill we have
;; many drivers.
;; This could be used to repair things if you plug things into the wrong slot,
;; for example.
(setq ACTUATOR-MAP '((A0 (A 0))
		     (A1 (A 1))
		     (A2 (A 2))
		     (A3 (A 3))
		     (A4 (A 4))
		     (A5 (A 5))
		     (B0 (B 0))
		     (B1 (B 1))
		     (B2 (B 2))
		     (B3 (B 3))
		     (B4 (B 4))
		     (B5 (B 5))))

(setq LEFT-RIGHT-SYMMETRY
      '((A1 . A2) (B1 . B2) (A4 . A5) (B4 . B5)))

(setq BACK-FRONT-SYMMETRY
      '((A3 . B3) (A1 . B1) (A2 . B2) (A4 . B4) (A5 . B5)))


;; sym is an optional identifying symbol to uniquely identify the call...
(defun driver-send-com (driver com &optional sym)
  ;; In theory this will work if the com is a string or an s-expr
  (progn
    (print (format "driver-send-com %s %s %s" driver com sym))
    (if (and (stringp com) sym)
	(print "Whoa, we can't add a symbol to a string!"))
    (let* ((driver-string (cdr (assoc driver CONTROLLER-PORTS)))
	   (process (get-process driver-string))
	   (com-with-sym (if (or (null sym) (stringp com))
			     com
			   (cons (list (car com) sym) (cdr com))))
	   )
      (let (
	    (command-str (format "%s" com-with-sym)))
	(print (format "about to call with str: %s" command-str))
	(process-send-string process command-str))
	)
      ))
		     
(defun try-building (k port)
  (let ((n 0)
	(process nil)
	)
    (while (and (< n k) (null process))
      (setq n (1+ n))
      (condition-case err
	  ;; Protected form.
	  (let (
		(p (make-serial-process :port port :speed BAUD_RATE))
		)
	    (if p
		(setq process p)))
	;; The handler.
	(error                        ; Condition.
	 ;; Display the usual message for this error.
	 (print err)
	 (message "%s" (error-message-string err))
	 ))
      )
    process
    )
  )

(defun build-procs (k ports)
  (close-procs)
  (mapcar (lambda (p) (try-building k (cdr p))) ports)
  (protected-mapcar (lambda (p) (set-process-filter (get-process (cdr p)) 'glusss-bot-response-filter)) ports)
  )


(defun protected-mapcar (f lst)
  (mapcar
   (lambda (p)
           (condition-case err
	       (funcall f p)
	     (error                        ; Condition.
	      ;; Display the usual message for this error.
	      (print err)
	      (message "%s" (error-message-string err))
	      )))
   lst))

;; rewrite this to use protected-mapcar
(defun close-procs ()
  (protected-mapcar
   (lambda (p)
	(delete-process (cdr p)))
   CONTROLLER-PORTS))



(defun send-all (command &optional sym)
  (protected-mapcar
   (lambda (p)
     (driver-send-com (car p) command sym))
   CONTROLLER-PORTS))

(defun replace-from-assoc (pose a)
  "Take a pose and replace all head symbols with results from the assoc list"
  (mapcar #'(lambda (x)
	      (let* ((a-assoc (assoc (car x) a))
		     (r-assoc (rassoc (car x) a)))
		(if (and a-assoc r-assoc)
		    (throw 'EGREGIOUS-SYMMETRY-DUPLICATION)
		  (if (and (null a-assoc) (null r-assoc))
		      x
		    (cons (if a-assoc (cdr a-assoc)
			    (car r-assoc)
			    )
			  (cdr x))))))
	  pose))

(defun mirror-left-right (p)
  "Take a pose produce a new pose that represents the left-to-right axis mirroring of the same pose"
  (replace-from-assoc p LEFT-RIGHT-SYMMETRY))

(defun mirror-back-front (p)
    "Take a pose produce a new pose that represents the back-to-front axis mirroring of the same pose"
  (replace-from-assoc p BACK-FRONT-SYMMETRY))

(defun init ()
    (build-procs 3 CONTROLLER-PORTS)
    (debug-level 'ERROR))

;; look back in the current buffer to find the most recent
;; status report
;; This could be accomplished with filter-function,
;; dynamically updating things.
;; I also need to consider the possibility that filter functions
;; could sound output from multiple controllers to the same buffer.
;; However, for now, I'm just playing around with reading.
(defun process-status (p)
  (bufferp (process-buffer (get-process p)))
  )

;; Driving commands
(defun get-symbol-for-com-use (&optional sym)
  (if sym
      sym
    (let* ((sym-name (gensym))
	   (sym (intern (format "%s" sym-name))))
      (put sym 'latch-value 0)
      sym)))

;; Poses!
;; Test poses
;; TODO: create "defpose" macro!!!
(defun big (&optional sym)
  (let ((msym (get-symbol-for-com-use sym)))
    (send-all '(big) msym)))

(defun small (&optional sym)
  (let ((msym (get-symbol-for-com-use sym)))
    (send-all '(small) msym)))

(defun relax (&optional sym)
  (let ((msym (get-symbol-for-com-use sym)))
    (send-all '(relax) msym)))


;; Movement poses -- a distinction should be made between a complete pose
;; and a partial pose. Note one could even go so far as to design
;; a move of one node keeping all other nodes in the same position.
(setq lo 0)
(setq hi 900)
(setq mid 450)

;; These are poses which are symmetric and have not mirror poses.
;; (But the could have opposites---are inversions a thing?
(setq long-pose
      `(
	(A0 ,mid) (A1 ,hi) (A2 ,hi) (A3 ,hi) (A4 ,hi) (A5 ,hi)
	(B0 ,lo) (B1 ,hi) (B2 ,hi) (B3 ,hi) (B4 ,hi) (B5 ,hi)
	))


(setq flat-pose
      `(
      	   (A0 ,lo) (A1 ,(+ 100 mid)) (A2 ,(+ 100 mid)) (A3 ,(+ 300 lo)) (A4 ,mid) (A5 ,mid)
	   (B0 ,mid) (B1 ,(+ 100 mid)) (B2 ,(+ 100 mid)) (B3 ,(+ 300 lo)) (B4 ,mid) (B5 ,mid)))

(setq hunker-pose
      `(
      	   (A0 ,hi) (A1 ,(+ 100 mid)) (A2 ,(+ 100 mid)) (A3 ,lo) (A4 ,hi) (A5 ,hi)
	   (B0 ,hi) (B1 ,(+ 100 mid)) (B2 ,(+ 100 mid)) (B3 ,lo) (B4 ,hi) (B5 ,hi)))


;; Here begin poses that may have mirrors.
(setq lean-forward-pose
      `(
      	   (A0 ,mid) (A1 ,hi) (A2 ,hi) (A3 ,lo)
	   (B0 ,mid) (B1 ,hi) (B2 ,hi) (B3 ,hi)
	   ))

(setq lean-back-pose
      (mirror-back-front lean-forward-pose))

(setq lean-right-right-f-ppose
      `(
      	   (A0 ,(+ lo 100)) (A2 ,hi) (A1 ,lo) (A3 ,lo)
	   (B2 ,hi) (B1 ,lo) (B3 ,hi)))

(setq lean-right-ppose
      `(
      	   (A0 ,(+ mid 100)) (A2 ,hi) (A1 ,lo) (A3 ,lo)
	   (B2 ,hi) (B1 ,lo) (A3 ,lo)))

(setq lean-left-ppose
      (mirror-left-right lean-right-ppose))

;; Some poses are used only on one side for walking
(setq lean-right-x-ppose
      `(
	(A3 ,mid)
	(B3 ,hi)
))


(setq raise-right-ppose
      `(
      	   (A1 ,lo) 
	   (B1 ,lo)))

(setq raise-left-ppose
      (mirror-left-right raise-right-ppose))

(setq right-f-ppose
      `(
      	   (A1 ,(+ mid 100)) (A4 ,lo) 
	   (B1 ,hi) (B4 ,hi)))

(setq left-f-ppose
      (mirror-left-right right-f-ppose))

(setq left-f-x-ppose
      `(
      	   (A2 ,lo) (A5 ,mid) 
	   (B2 ,hi) (B5 ,hi)))

(setq left-f-x-ppose
      `(
      	   (A2 ,lo) (A5 ,mid) 
	   (B1 ,mid) (B2 ,mid) (B3 ,mid) (B5 ,hi)))

(setq right-down-f-ppose
      `(
      	   (A0 ,lo) (A1 ,(+ mid 300)) (A2 ,mid) (A4 ,mid) 
	   (B1 ,hi) (B4 ,hi)))

(setq left-down-f-ppose
      `(
      	   (A0 ,lo) (A2 ,mid) (A3 ,lo) (A1 ,mid) (A5, lo)
	   (B1 ,mid) (B2 ,mid) (B3 ,mid) (B5 ,hi)
	   ))


(setq reach-f-ppose
      `(
      	   (A3 ,hi) (A4 ,hi) (A5 ,hi)
	   ))

(setq reach-b-ppose
	(mirror-back-front reach-f-ppose))

(setq front-up-ppose
      `(
	(A0 ,lo) (A3 ,lo) (A4 ,hi) (A5 ,hi)
	(B3 ,lo)
	))

(setq front-up-short-ppose
      `(
	(A0 ,lo) (A3 ,lo) (A4 ,mid) (A5 ,mid)
	(B3 ,lo)
	))


(setq back-up-ppose
      (mirror-back-front front-up-ppose))

(setq back-up-short-ppose
      (mirror-back-front front-up-short-ppose))



(setq front-left-ppose
      `(
	(A1 ,hi) (A2 ,lo) (A3 ,lo) (A4 ,hi) (A5 ,lo)
	(B3 ,mid)
	))

(setq front-left-down-ppose
      `(
	(A1 ,hi) (A2 ,lo) (A3 ,mid) (A4 ,hi) (A5 ,lo)
	(B3 ,mid)
	))

(setq back-right-down-ppose
      (mirror-back-front front-left-down-ppose))


(setq left-up-back-ppose
      `(
	(A0 ,lo) (A1 ,mid) (A2 ,mid) (A5 ,hi)
	(B0 ,hi) (B2, lo) (B5 ,lo)
	))

(setq left-dn-back-ppose
      `(
	(A0 ,lo) (A1 ,mid) (A2 ,mid) (A5 ,hi)
	(B0 ,hi) (B2, mid) (B3 ,lo) (B5 ,lo)
	))




(setq front-out-ppose
      `(
	(A0 ,mid) (A1 ,lo) (A2 ,lo) (A3 ,lo) (A4 ,hi) (A5 ,hi)
	(B3 ,mid)
	))

(setq front-out-short-ppose
      `(
	(A0 ,mid) (A1 ,lo) (A2 ,lo) (A3 ,lo) (A4 ,mid) (A5 ,mid)
	(B3 ,mid)
	))

(setq back-in-ppose
            ;; This mean moves the back foot forward.
      `(
	(A0 ,lo) (A3, lo)
	(B1 ,lo) (B2 ,lo) (B3 ,lo) (B4 ,lo) (B5 ,lo)
	))




(defun front-step-f (&optional sym)
  (dance '((flat) (lean-back) (front-up) (front-out) (lean-forward)))
  )

(defun back-step-f (&optional sym)
  (dance '((flat) (lean-forward) (back-up) (back-in) (lean-back)))
  )


;; Now I will try to move the right foot forward.

(defun right-step-f (&optional sym)
  (dance '((lean-left) (raise-right) (right-f) (right-down-f))
  ))

(defun left-step-f (&optional sym)
  (dance '((lean-right) (raise-left) (left-f) (left-down-f))
	 ))

;; Q: If we reverse these steps will we move backwards?
;; A: No, not unless we reverse and mirror each operation, but
;; that should be possible. My approach of having function
;; symbols here is a real problem. Can I change dance so that
;; it takes closures or something so that it doesn't require
;; function symbols everywhere?
;; Also, want a routine like "get-status" that captures
;; state.  Also want a "diff" for poses!!
;; Our goal is to be able to complete reverse any set of movements!!
(setq move-forward-steps
      '(
	   (flat) (lean-back) (front-up) (front-out) (lean-forward)
	   (lean-left)
	   (right-f)
	   (right-down-f)
	   (lean-right-right-f)
	   (lean-right)
	   (left-f-x)
	   (left-down-f)
	   (lean-forward)
	   (back-up)
	   (back-in)
	   (lean-back)
	   (flat)
	   ))

(setq move-forward-poses
      (append
       (list
	flat-pose)
      (list
       lean-right-ppose
       left-f-ppose
       left-down-f-ppose
       )
       
      (list
       lean-left-ppose
       right-f-ppose
       right-down-f-ppose
       )
      
      (list 
       lean-forward-pose
       back-up-ppose
       back-in-ppose
       lean-back-pose
       front-out-ppose
       lean-forward-pose
       )
      
      (list
       flat-pose)
      ))

;; I really need to create the steps not as functions but as poses...
;; Let me start that process....
;; This should be a reversible symbol...
(setq lean-forward-then-back-to-test
      (list flat-pose lean-forward-pose lean-back-pose flat-pose
	    ))

;; Note: This only works if you start and end with a complete pose,
;; otherwise it is undertermined.
;; Now let's try to rithgt a function that reverses and mirrors..
;; Perhaps I will call it "regress".
;; Basic attempt at an algorithm:
;; (A B C) => (C B A)
;; Maybe it is just "reverse, with no mirroring".

(defun regress (steps)
  (reverse steps)
  )

(defun test-regress ()
  (equal (regress (regress lean-forward-then-back-to-test)) lean-forward-then-back-to-test)
  )

;; this doesn't seem to work --- perhaps disproves the simple reversal notion, not sure.
(defun test-step-backward ()
  "Test by making the gluss bot step forward and then back"
  (dance
   (mapcar (lambda (s)
	     (mirror-back-front s)
	     )
	   move-forward-poses)))


;; TODO: It is really critical that we figure out how to mathematically
;; reverse stepping forward, and mathematically invert turning to the left.
;; it is not a simple reversal of steps --- must be reverse with invert.
;; Or maybe simply invert!! (Not really a reversal, but a symmetry.
      
(defun move-forward (&optional sym)
  (fdance move-forward-steps
  ))

(defun move-forward-3 (&optional sym)
  (let ((com move-forward-steps))
    (fdance (append com (append com com)))))

(defun turn-left (&optional sym)
  "turn to the left"
    (let ((msym (get-symbol-for-com-use sym)))
      (fdance '(
	       (flat)
	       (lean-back)
	       (front-up)
	       (front-left-down)
	       (lean-right)
	       (left-up-back)
	       (left-dn-back)	       
	       (lean-left)
	       (right-f)
	       (right-down-f)
	       (flat)
	       ))
      ))

(setq turn-left-poses
      (list
       flat-pose
       lean-right-ppose
       left-up-back-ppose
       left-dn-back-ppose
       lean-left-ppose
       right-f-ppose
       right-down-f-ppose

       lean-back-pose
       front-up-short-ppose
       front-left-down-ppose

       
       lean-forward-pose
       back-up-short-ppose
       (mirror-left-right (mirror-back-front front-left-down-ppose))
       )
      )


(defun test-turn-right ()
  "Test by making the gluss bot step forward and then back"
  (dance
   (mapcar #'mirror-left-right
	   turn-left-poses)))


(defun front-left-down (&optional sym)
  "Put feet down as flat as possible in a an otherwise relaxed pose"
    (let ((msym (get-symbol-for-com-use sym)))
     (p front-left-down-ppose msym)
  ))

(defun front-left (&optional sym)
  "Put feet down as flat as possible in a an otherwise relaxed pose"
    (let ((msym (get-symbol-for-com-use sym)))
     (p front-left-ppose msym)
  ))

(defun left-up-back (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
     (p left-up-back-ppose msym)
  ))

(defun left-dn-back (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
     (p left-dn-back-ppose msym)
  ))


(defun flat (&optional sym)
  "Put feet down as flat as possible in a an otherwise relaxed pose"
    (let ((msym (get-symbol-for-com-use sym)))
     (p flat-pose msym)
  ))

(defun long (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
     (p long-pose msym)
  ))

(defun hunker (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p hunker-pose msym)
      ))

(defun lean-back (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p lean-back-pose msym)
  ))

(defun lean-forward (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p lean-forward-pose msym)
  ))


(defun lean-left (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p lean-left-ppose msym)
  ))

(defun lean-right (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p lean-right-ppose msym)
  ))

(defun lean-right-x (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p lean-right-x-ppose msym)
  ))

(defun lean-right-right-f (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p lean-right-right-f-ppose msym)
  ))

(defun reach-f (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p reach-f-ppose msym)
      ))


(defun reach-b (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p reach-b-ppose msym)
  ))


(defun front-up (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p front-up-ppose msym)
      ))

(defun front-out (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p front-out-ppose msym)
      ))

(defun back-up (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p back-up-ppose msym)
      ))

(defun back-in (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p back-in-ppose msym)
      ))

(defun right-f (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p right-f-ppose msym)
      ))

(defun right-down-f (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p right-down-f-ppose msym)
      ))

(defun left-f (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p left-f-ppose msym)
      ))

(defun left-f-x (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p left-f-x-ppose msym)
      ))

(defun left-down-f (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p left-down-f-ppose msym)
      ))

(defun raise-right (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p raise-right-ppose msym)
      ))

(defun raise-left (&optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p raise-left-ppose msym)
      ))

(defun ex (pose &optional sym)
    (let ((msym (get-symbol-for-com-use sym)))
      (p pose msym)
      ))

;;


(defun get-status (&optional sym)
  (let ((msym (get-symbol-for-com-use sym)))
    (send-all '(get-status) msym)))

(setq DEBUG 5)
(setq INFORM 4)
(setq WARN 3)
(setq ERROR 2)
(setq PANIC 1)

(defun debug-level (level &optional sym)
  (let ((msym (get-symbol-for-com-use sym)))
    (send-all `(debug-level ,(symbol-value level)) msym)))


;; Note: This is only for testing --- there is
;; no good reason to every assume the same geometry
;; for the different drivers.
;; EX: (m '(100 200 300 400 500 600) SYM)
(defun m (args &optional sym)
  (let ((msym (get-symbol-for-com-use sym)))
    (send-all (append '(m) args) msym)))

(defun get-num-live-drivers (ports)
  (let ((cnt 0))
    (protected-mapcar
     (lambda (p)
       (if (get-process (cdr p))
	   (incf cnt)))
     ports)
    cnt))


;; This is used by the arduino code to set the current status in a buffer-local variable!
(defun status (args)
  ;; For now we just print
  (let* ((driver (car (rassoc (buffer-name (current-buffer)) CONTROLLER-PORTS)))
	 (num-drivers (get-num-live-drivers CONTROLLER-PORTS))
	 (sym (car args))
	 (then (if sym (get sym 'then-function) nil)))
    (print "processing:")
    (print sym)
    (incf (get sym 'latch-value))
    ;; This part of the function should be in the callback from the driver, with sym passed in.
    (print "QQQQ")
    (print (or (get sym 'num-controllers) num-drivers))
    ;; In reality the latch-value limit here should be the number of live actuators....
    (if (>= (get sym 'latch-value) (or (get sym 'num-controllers) num-drivers))
	(progn
	  (print "YES, WE WOULD TRIGGER THIS GOT SECOND CALL")
	  (print then)
	  (if then
	      (funcall then))
	  )
      )
    (let ((driver-string (cdr (assoc driver CONTROLLER-PORTS))))
      (let ((process (get-process driver-string)))
	(with-current-buffer (process-buffer process)
	  (setq gluss-status args)
	  )
	  )
    )
    ))

;; This needs some error protecting
(defun get-current-status (driver)
  (let* ((driver-string (cdr (assoc driver CONTROLLER-PORTS)))
	 (process (get-process driver-string)))
    (with-current-buffer (process-buffer process)
      gluss-status
      )
    )
  )

(defun test-major-moves ()
  (progn
    (build-procs 3 CONTROLLER-PORTS)
    (big)
    (small)
    (relax)
    ))


;; Got this from StackExchange....
(defun my-eval-string (driver str)
  "Read and evaluate all forms in str.
Return the results of all forms as a list."
  (let ((next 0)
	ret)
   (condition-case err
	(while t
	  (setq ret
		(cons
		 (funcall (lambda (ret) (setq next (cdr ret)) (eval (car ret)))
			  (read-from-string str next))
		 ret)))
      (end-of-file
       nil)
      (error
       ;; Display the usual message for this error.
       (print err)
       (message "%s" (error-message-string err))
       nil
       ))
    (nreverse ret)))

;; Possibly here I should set a buffer-local marker that points
;; to the currently unprocessed position.  Then we can "process"
;; from the marker to the end of the buffer (at each eoln),
;; assuming that there is no eoln within an s-expr.
(setq gluss-b-output nil)
(setq gluss-status nil)
(defun glusss-bot-response-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'gluss-b-output)
      (make-local-variable 'gluss-status)
      (if (null gluss-b-output)
	  (setq gluss-b-output (point-max-marker)))
      (let ((moving (= (point) (process-mark proc)))
	    (cpoint (point))
	    (driver (car (rassoc (buffer-name (current-buffer)) CONTROLLER-PORTS)))
	    )
	(end-of-buffer)
        (save-excursion
          ;; Insert the text, advancing the process marker.
	  (end-of-buffer)
          (insert string)
	  (progn
	    (goto-char (marker-position gluss-b-output))
	    (while (search-forward "\n" nil t)
      	      (my-eval-string driver (buffer-substring (marker-position gluss-b-output) (point)))
	      (set-marker gluss-b-output (point))
	      )
	    )
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))


;; Now we will make some datatypes...
;; A basic idea is to have a "posture", which
;; consists of assignments to various actuators (not necessarily all.)
;; In may be particularly valuable to not force all actuators
;; to move at the same time.  The driver does not
;; actually support this yet.

;; (setq SAMPLE-POSTURE
;;       '((A (0 400)
;; 	   (3 200))
;; 	(B (0 100)
;; 	   (1 200)
;; 	   (2 300)
;; 	   (3 400))))
;; This implies that we are naming the controllers "A" and "B" etc.
;; However, we want the driver code to be the same, so we actually
;; want to dynamically set the name from here, just as we would like
;; to be able to set the DEBUG Level.

;; A dance consists of moving through a series of postures.

;; Before implementing a gait function, I need to have
;; call-backs defined, numbers associated with each callback, and
;; a proper naming convention.
;; We would like to be able to assign names to each actuator here
;; in the LISP world.  In order to do this, we need several
;; naming conventions.
;; I choose the format XD, where X is a letter and D a digt,
;; as the basic way of naming an acuatar.  In the Arduino space,
;; the actuators are simply named 0-5.  However, we want to be a
;; able to associate the letter with the controller in our creation.

;; To do this, I am changing the "Controller-ports", which is input to build-procs,
;; to an association map of the for ((A . "driverfile") (B . "driverfile2").  Ideally
;; this would create the symbols A0 - A5 to be used in statemtns that span
;; several drivers, as in (p ((A0 400) (B3 500))).



(defun p-assignments (ps)
  (p-assignments-aux ps ())
  )

(defun p-assignments-aux (ps acc)
  (if (null ps)
      acc
    (let* ((p (car ps))
	   ;; p is the one we are processing....
	   (pa (cadr (assoc (car p) ACTUATOR-MAP))))
      ;; pa is now of the form (D NUM)
      (let ((pb (cdr (assoc (car pa) acc))))
	;; pb is a list if it exists in the accumulator already.
	(let ((newacc (assq-delete-all (car pa) acc)))
	  (p-assignments-aux
	   (cdr ps)
	   ;; now we have the additional item back to acc...
	   (let ((x (list (car pa) (list (cadr pa)  (cadr p)))))
	     (cons (append
		    x
		    pb
		    )
		   newacc)
	     )
	   )
	  )
	)
      )))

(defun test-p-assignments1 ()
  (assert
   (equal
    '(
      ;; we want All the B's here
      (B  (3 200) (5 500))
      (A (4 400))
      ;; 
      )
    (p-assignments '((B5 500) (A4 400 ) (B3 200)))
    )
  ))

(defun test-p-assignments2 ()
  (let ((pass (p-assignments '((B5 0) (B4 200) (B3 400) (B2 600) (B1 800) (B0 1000)))))
  (assert
   (equal
    6
    (length (cdar pass))
    )
  )))

(defun test-p-assignments3 ()
  (let ((pass (p-assignments '((B5 0) (B4 200) (B3 400)))))
  (assert
   (equal
    3
    (length (cdar pass))
    )
   )))
(defun test-all-assignments ()
  (test-p-assignments1)
  (test-p-assignments2)
  (test-p-assignments3)
  )

(defun send1 (driver command)
  (let* ((command-str (format "%s" command)))
    (driver-send-com driver command)))

;; TODO: Test that the args are properly handled this way.
;; EX: (p ((A0 100) (B3 200) (A2 300) (B2 400)) SYM)
;; (defun p (args &optional sym)
;;   (let ((msym (if sym
;; 		  sym
;; 		(get-symbol-for-com-use))))
;;     (send-all (append '(p) args) msym)))


;; Now the restuls of p-assignments can be more multiplexed to the drivers
;; in a pretty straightforward way....
;; TODO --- TEST THIS, and add the optional symbol argument, then put in
;; the dance.
(defun p-style (cmd args &optional sym)
  (let ((a (p-assignments args))
	(msym (get-symbol-for-com-use sym)))
    (protected-mapcar
     (lambda (arg)
       ;; TODO : BUG : this should probably be the same symbol (outside the mapcar!!)
       (let* ((driver (car arg))
	      (command (cons cmd (cdr arg))))
	 (driver-send-com driver command msym)
	 )
       )
     a)
    )
  )

(defun p (args &optional sym)
  (p-style 'p args sym)
  )

(defun set-activation (args &optional sym)
  (p-style 'set-activation args sym)
  )


;; Can I make a mock-driver for testing?  I think I probably can and that
;; would be very strong. Not sure how to do the process and buffer stuff...
(defun test-p ()
  (p '((B5 500) (A4 400 ) (B3 200)))
  )

(defun number-controllers-affected (ps)
  (let ((found))
    (mapcar (lambda (p)
		    (if (not (member (car (cadr (assoc (car p) ACTUATOR-MAP))) found))
			(setq found (cons (car (cadr (assoc (car p) ACTUATOR-MAP))) found))))
	    ps)
    (length found)))

(defun test-number-controllers-affected ()
  (equal 2 (number-controllers-affected '((A0 0) (A1 550) (A2 550) (A3 300) (A4 450) (A5 450) (B0 450) (B1 550) (B2 550) (B3 300) (B4 450) (B5 450))))
  )

;; Okay, now we need to create a "dance" function.
;; The key to dancing is not take the next step until the step is done.
;; Since we are in an asynchronous (and slow) environment, this means
;; we have to deal with call-backs in some way. But let me write the tests
;; first some how --- may drive the creation of a mock object.
;; A dance is therefore a series of "steps". A step can be
;; any command.




  



;; Okay now I am playing around with the promise concept, trying to understand it.
;; Our fundamental need is to wait on mulitple asynchronous processes.
;; If we had promise objects 

;; This is weird that I am not using limit or then.
(defun create-latch (procs then sym)
  ;; execute all of procs, passing the same variable to each. limit is reached,
  ;; execute the "then" function.)
  (let* ((limit (length procs)))
    (progn
      (put sym 'latch-value 0)
      (put sym 'then-function then)
      (print (get sym 'then-function))
      (print "MMMM")
      (mapcar (lambda (p)
		(progn
		  (print then)
		  (print "BBBB")
		  (funcall p sym limit then)
		  ))
	      procs))))



;; Now, how can we use create-latch to do a dance?



(defun create-procs-from-step (cmd)
  ;; We need to return a list of the individual commands to be executed on the drivers for this cmd
  ;; We are doing this so that we can defer execution to add the latch control code.
  (mapcar
   (lambda (p)
     ;; Note were are returning a thunk here...
     (let ((x (car p)))
       ;; This should be replace with a standard wrapping function or macro....
       `(lambda (symxxx limit then)
	  (print symxxx)
	  (print (get symxxx 'then-function))
	  (print "AAA")
	  (print (format " about to inoke: %s %s %s " (quote ,x) (quote ,cmd) symxxx ))
	  (driver-send-com (quote ,x) (quote ,cmd) symxxx)
	  )))
   CONTROLLER-PORTS))



(defun dance-orig (steps)
  ;; Execute the first step and set up the callbacks with the continuations.
  (if (null steps)
      t
    (let* ((sym-name (gensym))
	   (sym (intern (format "%s" sym-name)))
	   (procs (create-procs-from-step (car steps))))
      (progn
	(print sym)
	(print procs)
	(print "qqq")
	(put sym 'latch-value 0)
	(create-latch
	 procs
	 ;; This is supposed to be a lambda expression...
	 `(lambda ()
	   (print "XXXX")
	   (dance (quote ,(cdr steps))))
	 sym
	)))))

;; This is my attempt to create using just the "then" functionality
;; without calling create-latch so that we can execute more arbitrary functions.
;; Note this should probably take symbol as an optional argument
(defun fdance (steps)
  "take a list of zero-argument functions that can except a symbol as an optional argument"
  (if (null steps)
      t
    (let* (
	   (sym (get-symbol-for-com-use))
		  )
      (progn
	(put sym 'then-function 
	     `(lambda ()
		(fdance (quote ,(cdr steps)))))
	;; I need to make sure I can put a closure here.
	(funcall (caar steps) sym)
	))))

;; The advantage of this is that the list of poses are mathematical objects
;; which can be operated on (at least a little) via operations such as:
;; Mirror about axis
;; Invert
;; Mirror in time
;; I hypothesize that if we mirror about two axes and time, we can
;; make the robot reverse its steps.  So "move-forward" can become
;; "move-back".  But once we turn things into functions (as in closures) this
;; is opaque.
(defun dance (steps)
"Take a list of poses, and expect them to be executed in the controller with the 'p position function"
  (if (null steps)
      t
    (let* (
	   (sym (get-symbol-for-com-use))
		  )
      (progn
	(put sym 'then-function 
	     `(lambda ()
		(dance (quote ,(cdr steps)))))
	(let ((n (number-controllers-affected (car steps))))
	  (put sym 'num-controllers n)
	(funcall 'p (car steps) sym)
	)))))


;; This is important because it allows us to write and test a "trace-back-in-time function"
(defun test-dance0 ()
  (let ((s1 '((A0 400) (B0 400) (B1 400)))
	(s2 '((A0 400) (B0 0) (B1 0)))
	(s3 '((A0 400) (B0 600) (B1 600))))
    (dance (list s1 s2 s3))))

(defun test-dance-one-controller ()
  (let ((s1 '((A0 800)))
	(s2 '((A0 400) (B0 0) (B1 0)))
	(s3 '((B0 400) (B1 600))))
    (dance (list s1 s2 s3 s1))))



;; These test are currently RED, I don't know why.
(defun test-fdance1 ()
  ;; I really need to support better names in the driver, but until I do,
  ;; this will ahve to work.
  (let ((s1 '(relax))
	(s2 '(small))
	(s3 '(big))
	(s4 '(small))
	    )
    (fdance (list s1 s2 s3 s4))
    )
  )


(defun test-fdance2 ()
  (let ((s1 '(small))
	(s2 '(big))
	(s3 '(p (A0 0) (A1 0) (A2 0)))
	(s4 '(small)))
    (fdance (list s1 s2 s3 s4))
    )
  )



;; Okay now I am playing around with the promise concept, trying to understand it.
;; Our fundamental need is to wait on mulitple asynchronous processes.
;; If we had promise objects 

(defun test-create-latch ()
  (let* ((sym-name (gensym))
	 (sym (intern (format "%s" sym-name))))
    	(put sym 'latch-value 0)
  (create-latch (list (lambda (x limit then)
			(progn
			  (print "A")
			  (incf (get x 'latch-value))
			  (if (>= (get sym 'latch-value) limit)
			      (funcall then))
					  ))
		      (lambda (x limit then)
			(progn
			  (print "B")
			  (incf (get x 'latch-value))
			  (if (>= (get sym 'latch-value) limit)
			      (funcall then))
			  ))
		      )
		(lambda () (print x) (print "YES, THEN CALLED! UUUUUU"))
		sym)))

;; This is piggybacking on the "p" syntax, which allows you to potentially
;; address every actuator -- probably unneeded but good for consistentcy sake.
(defun test-set-actuator-unresponsive () 
  (let* ((actuator '((A2 0)))
	 (sym (get-symbol-for-com-use))
	 (then (lambda () (print "QQQQQ")
		 (print "We Really want to check the status here, but that is unimplemented."))))
    ;; The only really good way to test is to have "then" function
    ;; attached to the completion that screams if it goes wrong. This
    ;; function should be able to read the status (not yet fully materialized)
    ;; and observe that the actuator we desire is in fact inactive.
    ;; This also requires that we modify the status to report unresponsive
    ;; (currently being report as "nil").
    ;; So in fact this will be a bit of machinery!

    ;; Probably I will need to extend things to stack up multiple handlers...
    ;; at present this would not work in the "dance" function because it would overload the symbol
    ;; A realy lisp hero would like create a macro for this in some way...
    (put sym 'then-function then)
    (set-activation actuator sym)
    )
  )


;; Useful function snippets
;; (build-procs 3 CONTROLLER-PORTS)

;; This funtion is for the CURRENT glussbot with the broken actuator
(defun personalize-current-glussbot ()
  (let* ((actuator '((A2 0))))
    (set-activation actuator)
    )
  )


;; Here begins the part where I try to right a function to find good values to move two,
;; while keeping 4 still, or 1 with 5 still.  This can be done with a brute force algorithm.
;; the tricky part is the coordinate system.
;; I am currently planning a coordinate system:
;; Given nodes are named F,B,L,R,H,T, for Front, Back, Left, Right, Head, Tail
;; The origin is the median of FB
;; The Y axis goes through F and B
;; The shortest line connecting FB and RL defines the YZ plane.
;; If that FB line intersect RL, then FB defines the XY plane.
;; Input to the system is the collection of 4 or 5 nodes to remain fixed,
;; The pairs (NODE . POINT) where NODE is to be move to POINT or
;; to minimize distance. POINT is specified in the Cartesian coordinates
;; of the FB system described above.
;; This should be a useful reference:
;; https://en.wikipedia.org/wiki/Skew_lines
;; http://mathworld.wolfram.com/Line-LineDistance.html
;; Note we will also have to generate a physical model
;; of our joint/actuator complex to map from voltage of the potentiometer
;; to cartesian distance.
;; I think we will have to solve this as a system of equations.
;; In our case, the system is so simple we could probably build a map between
;; configurations that 3D models if we quantize positions pretty easily.

;; In order to test this, I need a way to draw the 3D positions
;; in three space from emacs, or at least from a file. I think
;; OpenGL is probably the standard way to do this, although
;; I am sure there are browser based mechanisms as well.
;; This could end up taking as much as a week of work---yuck.
;; I could probably get it working faster by simply tuning the
;; steps by hand.

;; This coordinate system would let us drive a graphical representation
;; as well if we had any need to do that.

;; An additional function that would be fun would be drive  4 nodes
;; to be coplanar as quickly as possible.  Input would be the 4 nodes, only.
;; This is good for making the feet lay flat.

;; Actually, our coordinate system probably gives us a good way to
;; do the coplanarity thing in a sneaky way: just put the RL line on the FB line.

;; Need to treat this as an rassoc as well, in which case we won't have to repeat

(defun test-mirror ()
  (progn
  (assert
   (equal (mirror-left-right raise-right-ppose)
	  raise-left-ppose))
  (assert
   (equal (mirror-back-front reach-f-ppose)
	  reach-b-ppose))
  ))


(setq TETA "/dev/cu.2TETBOT-RNI-SPP")
(setq TETB "/dev/cu.3TETBOTB-RNI-SPP")
(setq CONTROLLER-PORTS (list (cons 'A TETA)  (cons 'B TETB)))

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

(defun driver-send-com (driver com)
 ;; In theory this will work if the com is a string or an s-expr
  (let* ((driver-string (cdr (assoc driver CONTROLLER-PORTS)))
	(process (get-process driver-string))
	(command-str (format "%s" command)))
    (process-send-string process command-str)))

		     
(defun try-building (k port)
  (let ((n 0)
	(process nil)
	)
    (while (and (< n k) (null process))
      (setq n (1+ n))
      (condition-case err
	  ;; Protected form.
	  (let (
		(p (make-serial-process :port port :speed 9600))
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



(defun send-all (command)
  (protected-mapcar
   (lambda (p)
     (driver-send-com (car p) command))
   CONTROLLER-PORTS))


(defun test-one ()
  (progn
    (build-procs 3 CONTROLLER-PORTS)
    (send-all "x")))

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

(defun big () (send-all "k"))
(defun small () (send-all "l"))
(defun relax () (send-all "j"))
(defun s () (send-all "s"))

(defun m-all (k) (send-all (format "(m %d %d %d %d %d %d)" k k k k k k)))


;; This is used by the arduino code to set the current status in a buffer-local variable!
(defun status (args)
  ;; For now we just print
  (print
   (list "WE WOULD SAVE THIS:"
	 args
	 "YES WE WOULD."
   )))


(defun test-major-moves ()
  (progn
    (build-procs 3 CONTROLLER-PORTS)
    (big)
    (small)
    (relax)
    ))
	
(defun my-eval-string (str)
  "Read and evaluate all forms in str.
Return the results of all forms as a list."
  (let ((next 0)
	ret)
    (condition-case err
	(while t
	  (setq ret (cons (funcall (lambda (ret) (setq next (cdr ret)) (eval (car ret))) (read-from-string str next)) ret)))
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
(defun glusss-bot-response-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'gluss-b-output)
      (if (null gluss-b-output)
	  (setq gluss-b-output (point-max-marker)))
      (let ((moving (= (point) (process-mark proc)))
	    (cpoint (point)))
	(end-of-buffer)
        (save-excursion
          ;; Insert the text, advancing the process marker.
	  (end-of-buffer)
          (insert string)
	  (progn
	    (goto-char (marker-position gluss-b-output))
	    (while (search-forward "\n" nil t)
      	      (my-eval-string (buffer-substring (marker-position gluss-b-output) (point)))
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

;; A Gait consists of moving through a series of postures.


;; This function doesn't really make sense from a posturing point of view,
;; it is only for testing.
(defun p-all (ps) (send-all (format "%s" ps)))

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


(defun p (ps)
  ;; let's move through each of the items, finding the symbol, mapping into
  ;; a statement for each driver.
  (let ((ds (mapcar (lambda (c) (car c))) CONTROLLER-PORTS))
    ;; Now that we have the symbols, let's look through ps looking for each in turn
  (send-all (format "%s" ps))))

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
    (let ((pb (cadr (assoc (car pa) acc))))
      ;; pb is a list if it exists in the accumulator already.
      (let ((newacc (assq-delete-all (car pa) acc)))
	(p-assignments-aux
	 (cdr ps)
	 ;; now we have the additional item back to acc...
	 (cons (append
		(list (car pa) (list (cadr pa)  (cadr p)))
		(if (null pb)
		    nil
		(list pb)))
	       newacc)
	 )
       )
      )
    )
  ))

(defun test-p-assignments ()
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

(defun send1 (driver command)
  (let* ((command-str (format "%s" command)))
    (driver-send-com driver command)))



;; Now the restuls of p-assignments can be more multiplexed to the drivers
;; in a pretty straightforward way....
(defun p (cmd)
  (let ((a (p-assignments cmd)))
    (protected-mapcar
     (lambda (cmd-1)
       (let ((driver (car cmd-1))
	      (command (format "%s" (cons 'p (cdr cmd-1)))))
	 (driver-send-com driver command)
	 )
       )
     a)
    )
  )


;; Can I make a mock-driver for testing?  I think I probably can and that
;; would be very strong. Not sure how to do the process and buffer stuff...
(defun test-p ()
  (p '((B5 500) (A4 400 ) (B3 200)))
  )


;; Okay, now we need to create a "dance" function.
;; The key to dancing is not take the next step until the step is done.
;; Since we are in an asynchronous (and slow) environment, this means
;; we have to deal with call-backs in some way. But let me write the tests
;; first some how --- may drive the creation of a mock object.
;; A dance is therefore a series of "steps". A step can be
;; any command.

(defun dance (steps)
  ;; Execute the first step and set up the callbacks with the continuations.
  (
  )

(defun test-dance ()
  (let ((s1 '(small))
	(s2 '(big))
	(s3 '((A0 0) (A1 0) (A2 0)))
	(s4 '(small)))
    (dance (list s1 s2 s3 s4))
    )
  )






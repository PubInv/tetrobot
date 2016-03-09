(require 'cl)

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



(defun send-all (command &optional sym)
  (protected-mapcar
   (lambda (p)
     (driver-send-com (car p) command sym))
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
(defun get-symbol-for-com-use ()
  (let* ((sym-name (gensym))
	 (sym (intern (format "%s" sym-name))))
    (put sym 'latch-value 0)
    sym))

(defun big (&optional sym)
  (let ((msym (if sym
		  sym
		(get-symbol-for-com-use))))
    (send-all '(big) msym)))

(defun small (&optional sym)
  (let ((msym (if sym
		  sym
		(get-symbol-for-com-use))))
    (send-all '(small) msym)))

(defun relax (&optional sym)
  (let ((msym (if sym
		  sym
		(get-symbol-for-com-use))))
    (send-all '(relax) msym)))

(defun get-status (&optional sym)
  (let ((msym (if sym
		  sym
		(get-symbol-for-com-use))))
    (send-all '(get-status) msym)))

(setq DEBUG 5)
(setq INFORM 4)
(setq WARN 3)
(setq ERROR 2)
(setq PANIC 1)

(defun debug-level (level &optional sym)
  (let ((msym (if sym
		  sym
		(get-symbol-for-com-use))))
    (send-all `(debug-level ,(symbol-value level)) msym)))


;; Note: This is only for testing --- there is
;; no good reason to every assume the same geometry
;; for the different drivers.
;; EX: (m '(100 200 300 400 500 600) SYM)
(defun m (args &optional sym)
  (let ((msym (if sym
		  sym
		(get-symbol-for-com-use))))
    (send-all (append '(m) args) msym)))


;; This is used by the arduino code to set the current status in a buffer-local variable!
(defun status (args)
  ;; For now we just print
  (let ((driver (car (rassoc (buffer-name (current-buffer)) CONTROLLER-PORTS)))
	(sym (car args)))
    (print sym)
    (print (get sym 'latch-value))
    (print (get sym 'then-function))
    (print "in status")
    (incf (get sym 'latch-value))
    ;; This part of the function should be in the callback from the driver, with sym passed in.
    (if (>= (get sym 'latch-value) 2)
	(progn
	(print "YES, WE WOULD TRIGGER THIS GOT SECOND CALL")
	(print (get sym 'then-function))
	(funcall (get sym 'then-function))
	)
      )
    (print
     (list "WE WOULD SAVE THIS:"
	   args
	   "YES WE WOULD."
	   ))))


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
(defun glusss-bot-response-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (make-local-variable 'gluss-b-output)
      (if (null gluss-b-output)
	  (setq gluss-b-output (point-max-marker)))
      (let ((moving (= (point) (process-mark proc)))
	    (cpoint (point))
	    (driver 'A))
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
(defun p (cmd &optional sym)
  (let ((a (p-assignments cmd)))
    (protected-mapcar
     (lambda (cmd-1)
       (let* ((msym (if sym
		  sym
		(get-symbol-for-com-use)))
	      (driver (car cmd-1))
	      (command (cons 'p (cdr cmd-1))))
	 (driver-send-com driver command msym)
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

(defun test-dance1 ()
  (let ((s1 '(small))
	(s2 '(big))
	(s3 '((A0 0) (A1 0) (A2 0)))
	(s4 '(small)))
    (dance (list s1 s2 s3 s4))
    )
  )

(defun test-dance2 ()
  (let ((s1 '(p '((B0 400) (B1 400))))
	(s2 '(p '((B0 0) (B1 0))))
	(s3 '(p '((B0 600) (B1 600))))
	(s4 '(small)))
    (dance (list s1 s2 s3 s4))
    )
  )

(defun test-dance3 ()
  (let ((s1 '(p '((A0 400) (A1 400))))
	(s2 '(p '((A0 0) (A1 0))))
	(s3 '(p '((A0 600) (A1 600))))
	(s4 '(small)))
    (dance (list s1 s2 s3 s4))
    )
  )



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



(defun dance (steps)
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



(defun test-dance1 ()
  ;; I really need to support better names in the driver, but until I do,
  ;; this will ahve to work.
  (let ((s1 '(relax))
	(s2 '(small))
	(s3 '(big))
	(s4 '(small))
	    )
    (dance (list s1 s2 s3 s4))
    )
  )


(defun test-dance2 ()
  (let ((s1 '(small))
	(s2 '(big))
	(s3 '(p (A0 0) (A1 0) (A2 0)))
	(s4 '(small)))
    (dance (list s1 s2 s3 s4))
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

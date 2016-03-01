(setq TETA "/dev/cu.2TETBOT-RNI-SPP")
(setq TETB "/dev/cu.3TETBOTB-RNI-SPP")
(setq CONTROLLER-PORTS (list TETA TETB))

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
  (mapcar (lambda (p) (try-building k p)) ports)
  )

(defun close-procs ()
    (delete-process TETA)
    (delete-process TETB)
)

;; This needs to be error-protected, in addition to reading from buffers as output.
(defun send-both (command)
  (progn
    (process-send-string TETA command)
    (process-send-string TETB command)))

(defun send-both (command)
  (mapcar
   (lambda (p)
           (condition-case err
	       (process-send-string TETB command)
	     (error                        ; Condition.
	 ;; Display the usual message for this error.
	      (print err)
	      (message "%s" (error-message-string err))
	      )))
   CONTROLLER-PORTS))


(defun test-one ()
  (progn
    (build-procs 3 CONTROLLER-PORTS)
    (send-both "x")))








					; Driving commands

(defun big () (send-both "k"))
(defun small () (send-both "l"))
(defun relax () (send-both "j"))
(defun status () (send-both "s"))

;; send-both needs to be change to send-all
(defun test-major-moves ()
  (progn
    (build-procs 3 CONTROLLER-PORTS)
    (big)
    (small)
    (relax)
    ))
	

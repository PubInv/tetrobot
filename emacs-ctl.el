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
  (close-procs)
  (mapcar (lambda (p) (try-building k p)) ports)
  (protected-mapcar (lambda (p) (set-process-filter (get-process p) 'glusss-bot-response-filter)) ports)
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
	(delete-process p))
   CONTROLLER-PORTS))



(defun send-all (command)
  (protected-mapcar
   (lambda (p)
	(process-send-string p command))
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
	  ;;          (goto-char (process-mark proc))
	  (end-of-buffer)
          (insert string)
	  (progn
	    (goto-char (marker-position gluss-b-output))
	    (while (search-forward "\n" nil t)
;;	      (print (list "XX" (buffer-substring (marker-position gluss-b-output) (point)) "XX"))
      	      (my-eval-string (buffer-substring (marker-position gluss-b-output) (point)))
	      (set-marker gluss-b-output (point))
;;	      (print "OM:")
;;	      (print gluss-b-output)
	      )
	    )
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

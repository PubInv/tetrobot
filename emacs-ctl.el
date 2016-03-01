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
(defun status () (send-all "s"))

(defun test-major-moves ()
  (progn
    (build-procs 3 CONTROLLER-PORTS)
    (big)
    (small)
    (relax)
    ))
	
(defun ordinary-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;; Here I attempt to write a function that
;; listens for end-of-line, can treats each line as lisp-expression as it is encountered.
;; Other than that appends the process name to each line (hopefully valubable for processing
;; multiple buffers asyncrhonously.  
(defun treat-as-elisp-and-annotate-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((currentpoint (point))
	    (moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
	  (if (string-match "\n" string)
	      ;; Now we want to process from the current mark to the "Point", treating
	      ;; as potentially multiple s-sexpressions
	      (progn
		(print (list "|"
		 (buffer-substring (currentpoint) (point-max))
		 "|"))
		)
	    )
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

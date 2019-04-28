; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-io.lisp - Input, output data
; Function load and save to file, to add data to a stack
; -------------------------------------------------------------

(in-package :nn)

(defmethod make-result-entry ((network nnetwork))
  "Make a list-entry of network inputs, outputs 
and targets."
  (let* ((inputs (map 'list #'(lambda (input) (n-input input)) 
					  (nnetwork-inputs network)))
		 (outputs (map 'list #'(lambda (output) (n-output output)) 
					   (nnetwork-outputs network)))
		 (targets (map 'list #'(lambda (target) (n-target target)) 
					   (nnetwork-outputs network)))
		 (result-row (list :inputs inputs
						   :outputs outputs
						   :targets targets
						   :mset (nnetwork-mset network)
						   :errort (nnetwork-errort network))))
	result-row))

(defmethod add-to-store ((network nnetwork) store cycle epoch)
  "Add a result row of calulated data into store." 
  (let* ((epoch-size (getf (nnetwork-config network) :train-epoch-size))
		 (result-row (make-result-entry network))
		 (epoch-cycle-list (if cycle 						   
							   (list :cycle cycle 
									 :epoch (* cycle epoch-size))
							   (list :epoch epoch
									 :cycle (/ epoch epoch-size)))))
		 (push (append result-row epoch-cycle-list) store))
  store)

(defun load-data (path)
  "Load ascii file from path."
  (let ((store))
	(with-open-file (stream path 
				:direction :input)
	  (with-standard-io-syntax
		(setf store (read stream))))
	store))

(defun save-data (path store)
  "Save ascii data to path from store."
  (with-open-file (stream path 
			  :direction :output
			  :if-exists :supersede)
    (with-standard-io-syntax
	  (print store stream))))

(defun save-in-out-tgt-1D (path store)
  "Save inputs outputs and target for 1 dimension."
  (with-open-file (stream path 
						  :direction :output
						  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f~% ~f ~f~%" 
				(nth 0 (getf row :inputs))
				(nth 0 (getf row :outputs))
				(nth 0 (getf row :inputs))
				(nth 0 (getf row :targets)))))))

(defun save-in-out-tgt-2D (path store)
  "Save inputs outputs and target for 2 dimensions."
  (with-open-file (stream path 
						  :direction :output
						  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f~% ~f ~f~%" 
				(nth 0 (getf row :outputs))
				(nth 1 (getf row :outputs))
				(nth 0 (getf row :targets))
				(nth 1 (getf row :targets)))))))

(defun save-in-out-tgt-3D (path store)
  "Save inputs outputs and target for 3 dimensions."
  (with-open-file (stream path 
						  :direction :output
						  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f ~f~% ~f ~f ~f~%" 
				(nth 0 (getf row :outputs))
				(nth 1 (getf row :outputs))
				(nth 2 (getf row :outputs))
				(nth 0 (getf row :targets))
				(nth 1 (getf row :targets))
				(nth 2 (getf row :targets)))))))

(defun save-in-out (path store)
  (with-open-file (stream path 
						  :direction :output
						  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f~%" 
				(nth 0 (getf row :inputs))
				(nth 0 (getf row :outputs)))))))

 (defun save-in-out-1D-3D (path store)
   "Save input for x and output for y plot data."
  (with-open-file (stream path 
			  :direction :output
			  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f ~f~%" 
				(nth 0 (getf row :inputs))
				(nth 0 (getf row :outputs))
				(getf row :epoch))))))

 (defun save-in-out-2D-3D (path store)
  "Save output 1 for x and output 2 for y 
and epoch for z plot data."
  (with-open-file (stream path 
			  :direction :output
			  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f ~f~%" 
				(nth 0 (getf row :outputs))
				(nth 1 (getf row :outputs))
				(getf row :epoch))))))

(defun save-mset (path store)
  "Save epoch for x and mse for y plot data."
  (with-open-file (stream path 
			  :direction :output
			  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f~%" 
				(getf row :epoch)
				(getf row :mset))))))

(defun save-errort (path store)
  "Save epoch for x and mse for y plot data."
  (with-open-file (stream path 
			  :direction :output
			  :if-exists :supersede)
    (with-standard-io-syntax
	  (dolist (row store)
		(format stream "~f ~f~%" 
				(getf row :epoch)
				(getf row :errort))))))

;;
;; Marshalling - save network object
;;
(defun marshal-network (config network)
  "Serialize given network into data-file."
  (let ((folder (getf config :outfiles-dir))
		(objectfile (getf config :marshal-datafile))
		(dat-ext (getf config :dat-ext)))
    ; create object path
    (save-data 
		   (concatenate 'string folder objectfile dat-ext)
		   (ms:marshal network))))
		   
(defun unmarshal-network (config)
  "Unserialize network from data-file."
  (let ((folder (getf config :outfiles-dir))
		(objectfile (getf config :marshal-datafile))
		(dat-ext (getf config :dat-ext)))
    ; create object path
    (ms:unmarshal (load-data 
				   (concatenate 'string folder objectfile dat-ext)))))

(defun load-object (config-path)
  "Load network with given configuration-file."
  (let* ((config (load-configfile config-path))
		 (network (unmarshal-network config)))
	(setf (nnetwork-config network) config)
	network))

(defun save-object (network)
  "Serialize object to file."
  (let ((config (nnetwork-config network)))
	(marshal-network config network))
  network)

;
; send training or validation-file content to gnuplot
;
(defun send-to-gnuplot (gnuplot-path in-path out-path cmd width height type xlabel ylabel &optional zlabel)
  (let* ((terminal #+linux "wxt" #+windows type #+darwin type)
	 (command (concatenate 'string gnuplot-path " -p -e '" 
			       (format nil "set title \"Backpropagation Feedforward ANN (~A)\";"
				       (random 1024))
			       "set terminal " terminal
			       " size " (format nil "~f" width) 
			       "," (format nil "~f" height) ";"
			       "set xlabel \"" xlabel "\";set ylabel \"" ylabel "\";"
			       "set zlabel \"" zlabel "\";"
			       cmd " \"" in-path "\"' > " out-path)))
    (uiop:run-program command :output :string)))


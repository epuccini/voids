; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-logic.lisp - Application logic
; Training, validation and testing phases
; -------------------------------------------------------------

(in-package :nn)

(require 'alexandria)

(defmacro conc-options (config &body body)
  "Concatenate several keys in the body to create a filepath string"
 `(concatenate 'string
	       ,@(loop for f in body collect 
				  `(getf ,config ,f))))

 (defmethod train ((network nnetwork))
  "Train network 'epochs' training-set x epochs."
  (let* ((config (nnetwork-config network))
		 (plot (getf config :plot))
		 (collect (getf config :collect))
		 (epochs (getf config :epochs))
		 (error-data '())
		 (errfile-training-path
		  (conc-options config :outfiles-dir :training-errfile :dat-ext))
		 (infile-training-path
		  (conc-options config :infiles-dir :training-infile :dat-ext))
		 (training-set (load-data infile-training-path))
		 (epoch-size (list-length training-set))
		 (epochs-list (alexandria:iota epochs :start 0 :step 1)))
	;; size of epoch from set
	(setf (getf (nnetwork-config network) :train-epoch-size) epoch-size)
	;; begin training phase for 'epoch' 
	(mapcar (lambda (epoch)
			  (reset-mset network)
			  (reset-errort network)
			  (train-set network training-set epoch) ;; train a whole set of data
			  (if (or collect plot) ;; save data for error-graph
				  (setf error-data (add-to-store network error-data epoch 0))))
			epochs-list)
	;; decide to collect plotting data
	(if (or collect plot)
		(save-mset errfile-training-path error-data))
	(setf (getf (nnetwork-config network) :training-data) nil))
	; last print of total error
  network)

(defmethod train-set ((network nnetwork) training-set epoch)
  "Train network for all epochs of a set. (input target)."
  (let* ((epoch-size (list-length training-set))
		 (error-acc 0.0d0)
		 (mset-acc 0.0d0))
	(declare (type double-float error-acc mset-acc)
			 (type fixnum epoch-size))
    (mapcar (lambda (entry) 	;; begin training a whole set of training-data
			  (train-single-epoch network entry epoch)
			  (setq mset-acc (+ mset-acc (calc-mset-sum network)))
			  (setq error-acc (+ error-acc (calc-errort-sum network))))
			training-set)
	(calc-errort network error-acc epoch-size) ;; save total-errort
	(calc-mset network mset-acc epoch-size))) 	;; save total-mse of a complete set

(defun train-single-epoch (network entry epoch)
  "Train network for one single epoch. (input target)."
  (declare (ignore epoch))
  (let* ((inputs (getf entry :inputs))
		 (targets (getf entry :targets)))
    (declare (type list inputs targets)
	     (type number epoch))
	;; setup feed
	(feed-inputs network inputs)
	(feed-targets network targets)
	;; map feed forward
	(-> network
		calc-network-output
		calc-error
		calc-weights
		calc-bias)))

(defmethod training-plot ((network nnetwork))
  "Plot results stored in files into gnuplot."
  (let* ((config (nnetwork-config network))
	 (plot (getf config :plot))
	 (gnuplot-path (getf config :gnuplot-path))
	 (errfile-plot-path 
	  (conc-options config :imagefiles-dir :error-plot :img-ext))
	 (errfile-training-path 
	  (conc-options config :outfiles-dir :training-errfile :dat-ext)))
	;; send error~epoch -data to gnuplot
    (if plot
	(send-to-gnuplot gnuplot-path
			 errfile-training-path
			 errfile-plot-path
			 "plot"  1500 1500 (subseq (getf config :img-ext) 1 4)
			 "Epoch" "Network error")))
  network)

;;
;; Validation phase
;;
(defmethod validate ((network nnetwork))
  (let* ((config (nnetwork-config network))
		 (validation-set nil)
		 (infile-validation-path
		  (conc-options config
			:infiles-dir
			:validation-infile
			:dat-ext)))
    (progn
	  (setq validation-set (load-data infile-validation-path))
      (validate-set network validation-set)))
  network)

(defmethod validate-set ((network nnetwork) validation-set)
  "Validate network for all epochs of a set. (input target).
Count epochs."
  (let ((epoch 0))
	(declare (type fixnum epoch))
	(mapc (lambda (entry)
			(progn
			  (validate-single-epoch network entry epoch)
			  (setq epoch (1+ epoch))))
		  validation-set)))

(defmethod validate-single-epoch ((network nnetwork) entry epoch)
  "Validate network for one single epoch. (input target)."
  (declare (type fixnum epoch))
  (let* ((config (nnetwork-config network))
		 (plot (getf config :plot))
		 (collect (getf config :collect))
		 (inputs (getf entry :inputs))
		 (targets (getf entry :targets)))
	(declare (type boolean plot collect)
			 (type list inputs targets))
	(feed-inputs network inputs)
	(feed-targets network targets)
	(-> network
		calc-network-output)
	(if (or collect plot)
		(setf (getf config :validation-data) 
			  (add-to-store network (getf config :validation-data) 0 epoch)))))

(defmethod validation-plot ((network nnetwork))
  "Plot validaiton-data."
  (let* ((config (nnetwork-config network))
	 (plot (getf config :plot))
	 (gnuplot-path (getf config :gnuplot-path))
	 (collect (getf config :collect))
	 (dim (getf config :dim))
	 (validation-plot-path 
	  (conc-options config :imagefiles-dir :validation-plot :img-ext))
	 (outfile-validation-path 
	  (conc-options config :outfiles-dir :validation-outfile :dat-ext))
	 (img-ext (getf config :img-ext))
	 (img-type (subseq img-ext 1 (length img-ext)))
	 (plot-cmd "plot"))
    (if (or plot collect)
	(cond ((= dim 1)
	       (save-in-out-tgt-1D outfile-validation-path 
				   (getf config :validation-data)))
	      ((= dim 2)
	       (save-in-out-tgt-2D outfile-validation-path 
				   (getf config :validation-data)))
	      ((= dim 3)
		   (progn
		     (setq plot-cmd "splot")
		     (save-in-out-tgt-3D outfile-validation-path 
					 (getf config :validation-data))))))
    (if plot
	(send-to-gnuplot gnuplot-path
			 outfile-validation-path
			 validation-plot-path
			 plot-cmd 1500 1500 img-type 
			 "In" "Out"))
	(setf (getf config :validation-data) nil))
  network)

(defmethod get-input ((network nnetwork))
  "Get input and feed network."
  (let ((inputs (list-length (nnetwork-inputs network))))
	(setf cl:*read-default-float-format* 'double-float)
	(feed-inputs network
				 (loop for index from 0 to (1- inputs) collect
					  (progn
						(format t "Input(~D)?:> " index)
						(multiple-value-bind (num cnt)
							(read-from-string (read-line))
						  (declare (ignore cnt))
						  num))))
	(-> network
		calc-network-output
		print-in-out)))
  
(defmethod testing ((network nnetwork))
  "Ask for testing. Prompt for input
and deliver to the network and calculate output."
  (loop 
     (unless (yes-or-no-p)
       (return))
     (get-input network))
  network)

(defmethod optimise ((network nnetwork) &optional (test nil))
  "Train network, validate, plot and test if wanted"
  (with-network-info network
	'train
	'validate
	'save-object
	'training-plot 
	'validation-plot)
  (if test
	  (network-info network 'testing)))

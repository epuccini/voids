; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-network.lisp - Network functions
; Output calculation, error-functions, weight-adjustment.
; -------------------------------------------------------------

(in-package :nn)

;
; Optimize
;
(declaim (optimize (speed 3) (space 1) (debug 0)))
	   
;;
;; Class for creating neural networks
;;
(defclass nnetwork (model-serializable)
  ((inputs :accessor nnetwork-inputs
		   :initarg :inputs
		   :initform #()
		   :type simple-array)
   (hidden :accessor nnetwork-hidden
		   :initarg :hidden
		   :initform #()
		   :type simple-array)
   (outputs :accessor nnetwork-outputs
			:initarg :outputs
			:initform #()
			:type simple-array)   
   (bias :accessor nnetwork-bias
		 :initarg :bias
		 :initform #()
		 :type simple-array)
   (errort :accessor nnetwork-errort
		   :initform 0.0d0
		   :type double-float)
   (mset :accessor nnetwork-mset
		 :initform 0.0d0
		 :type double-float)
   (config :accessor nnetwork-config
		   :initform '()
		   :initarg :config
		   :type 'list)))

(defun make-nnetwork (config)
  "Neural nnetwork constructor."
  (let ((network
		 (make-instance 
		  'nnetwork 
		  :inputs (fn-array
				   #'make-ninput 
				   (getf config :n-input-layer))  ;; n: number of inputs
		  :hidden (make-hidden-layer 
				   (getf config :n-input-layer)
				   (getf config :hidden-layers)
				   (getf config :n-hidden-layer))
		  :outputs (fn-array 
					#'make-noutput 
				    (getf config :n-output-layer) ;; n: number of output-neurons
					(getf config :n-hidden-layer))  ;; parameter: number of weights per ouput
		  :bias (fn-array
				 #'make-nbias 
				 (getf config :n-total-count)) ; number of bias neurons
		  :config config)))
	network))

(defmethod ms:class-persistant-slots ((self nnetwork))
  '(inputs hidden outputs bias))

(defgeneric calc-output-error (network))
(defgeneric calc-hidden-error (network))
(defgeneric calc-output-weights (network))

(defun string-to-function (value)
  (symbol-function (find-symbol (string-upcase value))))

(defmacro doArray (sym array &body body)
  (dotimes (i (array-total-size array))
	 `(let ((sym (aref array i)))
		(setf (symbol-value ,sym) (aref array i))
		,@body)))

(defun test ()
  (doArray element #("Eddie" "Puccini" "ALAN")
	(print element)
	(terpri)))

(defmethod feed-inputs ((network nnetwork) inputs)
  "Feed input-neurons with input-values."
  (declare (type list inputs))
  (let ((in-fn (string-to-function (getf (nnetwork-config network) :IN-FN))))
	(dotimes (i (array-total-size (nnetwork-inputs network)))
      (setf (n-input (aref (nnetwork-inputs network) i)) (funcall in-fn (nth i inputs))))))
										;(+ input fluctations))

(defmethod feed-targets ((network nnetwork) targets)
  (declare (type list targets))
  (let ((out-fn (string-to-function (getf (nnetwork-config network) :TGT-FN))))
	(dotimes (k (array-total-size (nnetwork-outputs network)))
	  (setf (n-target (aref (nnetwork-outputs network) k)) (funcall out-fn (nth k targets))))))
						
(defmethod calc-input-output ((network nnetwork))
  "Calculate from output of input-layer to the inputs of hidden-layer 1"
  (let ((act-fn (string-to-function (getf (nnetwork-config network) :ACT-FN-HIDDEN)))
		(first 0))
	(dotimes (i (array-total-size (aref (nnetwork-hidden network) first)))
	  (let ((net 0.0d0) 
			(bias (n-weight (aref (nnetwork-bias network) i))) 
			(hidden-weights (n-weights (aref (aref (nnetwork-hidden network) first) i)))
			(output 0.0d0)
			(counter 0)
			(inputs (array-total-size (nnetwork-inputs network))))
		(declare (type double-float net bias output)
				 (type fixnum counter inputs))
		(dolist (weight hidden-weights)
		  (declare (type double-float weight))
		  (let ((input (n-input (aref (nnetwork-inputs network) (mod i inputs)))))
			(declare (type double-float input))
			(setq net (+ net (* weight input)))
			(setq counter  (1+ counter))))
		(setq net (+ net bias))
		(setf (n-net (aref (aref (nnetwork-hidden network) first) i)) net)
		(setq output (funcall act-fn net))
		(setf (n-output (aref (aref (nnetwork-hidden network) first) i)) 
			  output))))
  network)

(defmethod calc-hidden-output ((network nnetwork))
  "Calculate from output of input-layer to the inputs of hidden-layer 1"
  (loop for l from 1 to (- (getf (nnetwork-config network) :hidden-layers) 1) do
	(let ((act-fn (string-to-function (getf (nnetwork-config network) :ACT-FN-HIDDEN)))
		  (before (1- l)))
	  (dotimes (i (array-total-size (aref (nnetwork-hidden network) l)))
		(let ((net 0.0d0) 
			  (bias (n-weight (aref (nnetwork-bias network) i))) 
			  (hidden-weights (n-weights (aref (aref (nnetwork-hidden network) l) i)))
			  (inputs (array-total-size (aref (nnetwork-hidden network) before)))
			  (counter 0)
			  (output 0.0d0))
		  (declare (type double-float net bias output)
				   (type fixnum inputs counter))
		  (dolist (weight hidden-weights)
			(declare (type double-float weight))
			(let ((input (n-output (aref (aref (nnetwork-hidden network) before)
										 (mod counter inputs)))))
			  (declare (type double-float input))
			  (setq net (+ net (* weight input)))
			  (setq counter (1+ counter))))
		  (setq net (+ net bias))
		  (setf (n-net (aref (aref (nnetwork-hidden network) l) i)) net)
		  (setq output (funcall act-fn net))
		  (setf (n-output (aref (aref (nnetwork-hidden network) l) i)) output)))))
  network)
  
(defmethod calc-output-output ((network nnetwork))
  "Calculate output of last hidden-layer to the ouputs of the ouput-layer."
  (let ((act-fn (string-to-function (getf (nnetwork-config network) :ACT-FN-OUTPUT)))
		(last (1- (array-total-size (nnetwork-hidden network)))))
	(dotimes (i (array-total-size (nnetwork-outputs network)))
	  (let* ((net 0.0d0)
			 (index (+ i (getf (nnetwork-config network) :n-hidden-layer)))
			 (bias (n-weight (aref (nnetwork-bias network) index)))
			 (output-weights (n-weights (aref (nnetwork-outputs network) i)))
			 (counter 0)
			 (output 0.0d0))
		(declare (type double-float net bias output)
				 (type fixnum index counter))
		(dolist (weight output-weights)
		  (declare (type double-float weight))
		  (let ((input (n-output (aref (aref (nnetwork-hidden network) last) counter))))
			(declare (type double-float input))
			(setq net (+ net (* weight input)))
			(setq counter  (1+ counter))))
		(setq net (+ net bias))
		(setf (n-net (aref (nnetwork-outputs network) i)) net)
		(setq output (funcall act-fn net))
		(setf (n-output (aref (nnetwork-outputs network) i)) output))))
  network)

(defmethod calc-network-output ((network nnetwork))
  "Calculate output of all hidden-layers and output-layer."
  (calc-input-output network)
  (cond ((> (getf (nnetwork-config network) :hidden-layers) 1)
		 (calc-hidden-output network)))
  (calc-output-output network)
  network)

;(declaim (inline calc-delta-weight-sum))
(defmethod calc-delta-weight-sum ((network nnetwork) i)
  "Calculate the delta-sum of weights and error."
  (let ((sum 0.0d0))
	(declare (type double-float sum)
			 (type fixnum i))
    (dotimes (k (array-total-size (nnetwork-outputs network)))
      (let ((deltak (n-errord (aref (nnetwork-outputs network) k)))
	    (wkj (elt (n-weights (aref (nnetwork-outputs network) k)) i)))
	(declare (type double-float deltak wkj))
	(setq sum (+ sum (* deltak wkj)))))
    sum))

(defmethod calc-output-error ((network nnetwork))
  "Calculate the error-delta if all output neurons."
  (dotimes (k (array-total-size (nnetwork-outputs network)))
    (let* ((outputk (n-output (aref (nnetwork-outputs network) k)))
		   (target (n-target (aref (nnetwork-outputs network) k)))
		   (errord (* outputk (- 1.0d0 outputk) (- target outputk))))
	  (declare (type double-float outputk target errord))
      (setf (n-errord (aref (nnetwork-outputs network) k)) errord)))
  network)

(defmethod calc-hidden-layer-error ((network nnetwork) index)
  "Calculate the error-delta of the hiddenlayer neurons."
  (declare (type fixnum index))
  (dotimes (j (array-total-size (aref (nnetwork-hidden network) 0)))
    (let* ((sum (calc-delta-weight-sum network j))
		   (outputj (n-output (aref (aref (nnetwork-hidden network) index) j)))
		   (errord (* outputj (- 1.0d0 outputj) sum)))
	  (declare (type double-float sum outputj errord))
      (setf (n-errord (aref (aref (nnetwork-hidden network) index) j)) errord)))
  network)

(defmethod calc-hidden-error ((network nnetwork))
  "Calc errors of all hidden-layers"
  (loop for l from (1- (array-total-size (nnetwork-hidden network))) downto 0 do
	   (calc-hidden-layer-error network l)))

(defmethod calc-error ((network nnetwork))
  "Calculate the network error-delta of all hidden and 
output layer neurons."
  (calc-output-error network)
  (calc-hidden-error network)
  network)

(defmethod calc-output-weights ((network nnetwork))
  "Calculate the weights of the output-layer. Backpropagation
from output to the inputs"
  (dotimes (k (array-total-size (nnetwork-outputs network)))
    (let ((errord (n-errord (aref (nnetwork-outputs network) k)))
		  (last (1- (array-total-size (nnetwork-hidden network)))))
	  (declare (type double-float errord)
			   (type fixnum last))
      (dotimes (w (array-total-size (aref (nnetwork-hidden network) 
									(1- (array-total-size (nnetwork-hidden network))))))
		(let* ((outputi (n-output (aref (aref (nnetwork-hidden network) last) w)))
			   (wdelta (* outputi errord))
			   (wold (elt (n-weights (aref (nnetwork-outputs network) k)) w))
			   (last-change (n-delta (aref (nnetwork-outputs network) k)))
			   (current-change (* (getf (nnetwork-config network) :nn-learning-rate) wdelta))
			   (wnew (+ wold current-change 
						(* last-change 
						   (getf (nnetwork-config network) :nn-momentum-coeff)))))
		  (declare (type double-float outputi wdelta wold last-change current-change wnew))
		  (setf (n-delta (aref (nnetwork-outputs network) k)) current-change)
		  (setf (elt (n-weights (aref (nnetwork-outputs network) k)) w) wnew)))))
  network)

(defmethod calc-hidden-weights ((network nnetwork))
  "Calculate the weights of the output-layer. Backpropagation
from output to the inputs"
  (loop for l from (- (array-total-size (nnetwork-hidden network)) 2) downto 0 do
	   (dotimes (k (array-total-size (elt (nnetwork-hidden network) l)))
		 (let*  ((momentum (getf (nnetwork-config network) :nn-momentum-coeff))
				 (before (1+ l))
				 (errord (n-errord (aref (aref (nnetwork-hidden network) before) k))))
		   (declare (type double-float momentum errord)
					(type fixnum before))
		   (dotimes (w (array-total-size (aref (nnetwork-hidden network) l)))
			 (let* ((outputi (n-output (aref (aref (nnetwork-hidden network) l) w)))
					(wdelta (* outputi errord))
					(wold (elt (n-weights (aref (aref (nnetwork-hidden network) before) k)) w))
					(last-change (n-delta (aref (aref (nnetwork-hidden network) before) k)))
					(current-change (* (getf (nnetwork-config network) :nn-learning-rate)
									   wdelta))
					(wnew (+ wold current-change (* last-change momentum))))
			   (declare (type double-float outputi wdelta wold last-change 
							  current-change wnew))
			   (setf (n-delta (aref (aref (nnetwork-hidden network) before) k)) current-change)
			   (setf (elt (n-weights (aref (aref (nnetwork-hidden network) before) k)) w) wnew))))))
  network)

(defmethod calc-input-weights ((network nnetwork))
  "Calculate the weights of the hidden-layers. Backpropagation
from output to the inputs"
  (dotimes (j (getf (nnetwork-config network) :n-hidden-layer))
    (let* ((momentum (getf (nnetwork-config network) :nn-momentum-coeff))
		   (first 0)
		   (errord (n-errord (aref (aref (nnetwork-hidden network) first) j))))
	  (declare (type double-float momentum errord)
			   (type fixnum first))
      (dotimes (w (getf (nnetwork-config network) :n-input-layer))
		(let* ((outputi (n-input (aref (nnetwork-inputs network) w)))
			   (wdelta (* outputi errord))
			   (wold (elt (n-weights (aref (aref (nnetwork-hidden network) first) j)) w))
			   (last-change (n-delta (aref (aref (nnetwork-hidden network) first) j)))
			   (current-change (* (getf (nnetwork-config network) :nn-learning-rate) wdelta))
			   (wnew (+ wold current-change (* last-change momentum))))
		  (declare (type double-float outputi wdelta wold last-change current-change wnew))
		  (setf (n-delta (aref (aref (nnetwork-hidden network) first) j)) current-change)
		  (setf (elt (n-weights (aref (aref (nnetwork-hidden network) first) j)) w) wnew)))))
  network)

(defmethod calc-weights ((network nnetwork))
  "Backpropagation of the weights of all neurons from
output to input."
  (calc-output-weights network)
  (if (> (getf (nnetwork-config network) :hidden-layers) 1)
	  (calc-hidden-weights network))
  (calc-input-weights network)
  network)

(defmethod calc-bias-hidden ((network nnetwork))
  "Compute all hidden bias neurons."
  (dotimes (l (getf (nnetwork-config network) :hidden-layers))
	(dotimes (j (getf (nnetwork-config network) :n-hidden-layer))
	  (let ((bias (n-weight (aref (nnetwork-bias network) j)))
			(errord (n-errord (aref (aref (nnetwork-hidden network) l) j))))
		(declare (type double-float bias errord))
		(setf (n-weight (aref (nnetwork-bias network) j))
			  (+ bias (* errord (getf (nnetwork-config network) :nn-learning-rate)))))))
  network)

(defmethod calc-bias-output ((network nnetwork))
  "Compute all output bias neurons."
  (dotimes (k (getf (nnetwork-config network) :n-output-layer))
    (let* ((index (+ k (getf (nnetwork-config network) :n-hidden-layer)))
	   (bias (n-weight (aref (nnetwork-bias network) index)))
	   (errord (n-errord (aref (nnetwork-outputs network) k))))
	  (declare (type double-float bias errord)
			   (type fixnum index))
      (setf (n-weight (aref (nnetwork-bias network) index ))
			(+ bias (* errord (getf (nnetwork-config network) :nn-learning-rate))))))
  network)

(defmethod calc-bias ((network nnetwork))
  "Compute all bias neurons."
  (calc-bias-hidden network)
  (calc-bias-output network)
  network)

(defmethod calc-single-mse ((network nnetwork) output-index)
  "Mean square error function applied to one output and target."
  (declare (type fixnum output-index))
  (let ((target (n-target (aref (nnetwork-outputs network) output-index )))
		(output (n-output (aref (nnetwork-outputs network) output-index ))))
	(declare (type double-float target output))
	(expt (- target output) 2)))

(defmethod calc-mset-sum ((network nnetwork))
  "Calc mean square error from each output and
divide by number of outputs."
  (let ((mse-acc 0.0d0)
		(outputs (array-total-size (nnetwork-outputs network))))
	(declare (type double-float mse-acc))
	(dotimes (output-index outputs)
	  (let ((mse (calc-single-mse network output-index)))
		(declare (type double-float mse))
		(setf mse-acc (+ mse-acc mse))))
	(/ mse-acc outputs)))

(defmethod calc-mset ((network nnetwork) sum epochs)
  "Set mean square error with accumulated value divided
by epoch-size of one set."
  (declare (type double-float sum)
		   (type fixnum epochs))
	  (setf (nnetwork-mset network) (/ sum epochs)))

(defmethod calc-errort-sum ((network nnetwork))
  "Add all error-deltas and set network object."
  (let ((errort 0.0d0)
		(outputs (array-total-size (nnetwork-outputs network))))
	(declare (type double-float errort))
	(declare (type fixnum outputs))
	(loop for output-index from 0 to (1- outputs) do
		 (let ((errord (n-errord (aref (nnetwork-outputs network) output-index))))
		   (declare (type double-float errord))
		   (setf errort (+ errort (abs errord)))))
	errort))

(defmethod calc-errort ((network nnetwork) sum epochs)
  "Set mean square error with accumulated value divided
by epoch-size of one set."
  (declare (type double-float sum)
		   (type fixnum epochs))
  (setf (nnetwork-errort network) (/ sum epochs)))
	
(defmethod reset-mset ((network nnetwork))
  "Reset the mse-error value."
  (setf (nnetwork-mset network) 0.0d0))

(defmethod reset-errort ((network nnetwork))
  "Reset the total-error value."
  (setf (nnetwork-errort network) 0.0d0))

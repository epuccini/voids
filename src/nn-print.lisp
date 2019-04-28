; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-print.lisp - Application tools
; Pretty printing / logging with flood.
; -------------------------------------------------------------

(in-package :nn)

;
; Optimize
;
;(declaim (optimize (speed 3) (space 0) (debug 0)))

(defun network-info (network fn)
  "Create text-info from network-function."
  (let ((local-time (start-watch)))
	(inf (symbol-name fn) " phase start '" (getf (nnetwork-config network) :PRG-NAME) 
		 "' network: " (nnmodel-id network))
	(setq network (funcall fn network))
	(inf "Realtime: " (format nil "~,3f s" (t-real (stop-watch local-time))) 
		 " Runtime: " (format nil "~,3f s" (t-run (stop-watch local-time)))
		 " network: " (nnmodel-id network))
	(if (equal (symbol-name fn) "TRAINING")
		(inf (symbol-name fn) " MSE: " (nnetwork-mset network)))
	(inf (symbol-name fn) " phase end '" (getf (nnetwork-config network) :PRG-NAME)  
		 "' network: " (nnmodel-id network))))

(defmacro with-network-info (network &body forms)
  "List function and print info of network state."
  `(progn
	 ,@(loop for fn in forms collect	
			`(network-info ,network ,fn))))

(defun print-inputs (network)
  (terpri)
  (loop for i from 0 to (1- (array-total-size (nnetwork-inputs network))) do
	   (inf "Input neuron (input: " i ") - Inputs: " 
			(n-input (aref (nnetwork-inputs network) i)) "~%" )))

(defun print-hidden (network)
  (loop for l from 0 to (1- (array-total-size (nnetwork-hidden network))) do
	   (loop for i from 0 to (1- (array-total-size (aref (nnetwork-hidden network) l))) do
			(inf "Hidden neuron (hidden i/l: " i "/" l ") - Outputs: " 
				 (n-output (aref (aref (nnetwork-hidden network) l) i)) "~%")
			(loop for w from 0 to (1- (array-total-size (n-weights (aref (aref (nnetwork-hidden network) l) i)))) do
				 (inf "Weight (w: " w "): " 
					  (aref (n-weights (aref (aref (nnetwork-hidden network) l) i)) w) "~%")))))

(defun print-outputs (network)
  (loop for i from 0 to (1- (array-total-size (nnetwork-outputs network))) do
	   (inf "Ouput neuron (output: " i "): " 
			(n-output (aref (nnetwork-outputs network) i)) "~%")
	   (loop for w from 0 to (1- (array-total-size (n-weights (aref (nnetwork-outputs network) i)))) do
			(inf "Weight (" w "): "
				 (aref (n-weights (aref (nnetwork-outputs network) i)) w) "~%"))))

(defun print-network-and-break (network)
  (print-inputs network)
  (print-hidden network)
  (print-outputs network)
  (break)
  network)


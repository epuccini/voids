; -------------------------------------------------------------
; Edward Alan Puccini 19.06.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-config.lisp - Application globals in configuration files.
; -------------------------------------------------------------
(in-package :nn)

(defun default-config-list ()
"Default constants for global configuration."
  (list 
   :N-INPUT-LAYER 1 
   :N-HIDDEN-LAYER 8
   :HIDDEN-LAYERS 2
   :N-OUTPUT-LAYER 1
   :DIM 1
;   :ACT-FN-OUTPUT act-log
;   :ACT-FN-HIDDEN act-log
;   :IN-FN act-linear
;   :TGT-FN act-linear
   :N-OUTPUT-TH 0.5d0 
   :NN-LEARNING-RATE 0.9d0 
   :NN-MOMENTUM-COEFF 0.3d0 
   :NN-FINAL-TARGET-ERROR 1.0d0 
   :NN-ABORT-ERROR 0.389d0 
   :PRG-NAME "ann-ff-bp-positive-sine" 
   :DAT-EXT ".dat" 
   :IMG-EXT ".png" 
   :IMAGEFILES-DIR "../img/" 
   :OUTFILES-DIR "../output/" 
   :INFILES-DIR "../input/" 
   :TRAINING-INFILE "training-in-positive-sine" 
   :TRAINING-OUTFILE "training-out-positive-sine"
   :TRAINING-ERRFILE "error-out-positive-sine" ))

(defun dynamic-default-config ()
  (list 
   :train-epoch-size 0
   :training-data nil
   :validation-data nil
   :error-data nil
   :collect nil
   :plot nil))

(defun cleanup (network)
  (setf (getf (nnetwork-config network) :training-data) nil)
  (setf (getf (nnetwork-config network) :validation-data) nil)
  (setf (getf (nnetwork-config network) :error-data) nil)
  network)

(defun load-configfile (path)
"Load configuration file at path."
  (let ((config))
    (with-open-file (stream path)
      (with-standard-io-syntax
	(setf config (read stream))))
    ; append dynamic variables
    (setq config (append config (dynamic-default-config)))
	;; calc total neuron count
	(setq config (append config
					   (list :n-total-count 
							 (+ (getf config :n-input-layer)
								(* (getf config :n-hidden-layer)
								   (getf config :hidden-layers))
								(getf config :n-output-layer)))))
    config))

(defun save-configfile (config path)
"Save configuration as key-valued-list at path."
  (with-open-file (stream path
			  :direction :output
			  :if-exists :supersede)
    (with-standard-io-syntax
      (print config stream))))

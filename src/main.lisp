; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; main.lisp - Main Entry
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
; Requirements: trivial-shell, cl-marshal, fpp, flood,
; trivial-garbage, bordeaux-threads
; -------------------------------------------------------------

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :trivial-garbage)
  (use-package :flood)
  (use-package :nn))

(require 'argparse)

;; ---------------------------------------------------------------------
;; Init, run and cleanup
;; ---------------------------------------------------------------------
(defun prologue ()
  "Init application random-seed and kernel."
  (setf *random-state* (make-random-state t))
  (setf *gc-run-time* 1))

(defun run (configpath &optional (test nil))
  "Run creates the network and starts the training,
validation and tesing."
  (let* ((config (load-configfile configpath))
		 (network (make-nnetwork config)))
	(Prologue)
	(optimise network test)
	(if test
		(testing network))
	(epilogue config)))

(defun epilogue (config)
  "Cleanup and output memory usage and cycles/epochs."
  (let ((epochs (getf config :epochs))
	(epoch-size (getf config :train-epoch-size))
	(cycles 0))
  (declare (type fixnum cycles)
	   (type fixnum epoch-size)
	   (type fixnum epochs))
  (setq cycles (* epochs epoch-size))
  (trivial-garbage:gc)
  (room)
  (format *standard-output* "~%~D epochs / ~D bytes per training set. Ready after ~D cycles! ~%" epochs epoch-size cycles)))

 (defun main ()
  "Main entry point. Without param prints this help text.
With a specified configuration-file a optimisation-step is
beeing executed."
  (handler-case
      (let ((argument-data
             (argparse:with-arguments-hash-table
                 "ann-sim"
               "Backpropagation feedforward neural network simulator."
               "v1.0.4.0"
                '(:argument "--configuration"
                 :description "Configfile"
                 :group "Simulation"
                 :type 'string))))
        (argparse:handle-unknown-arguments argument-data) 
        (argparse:handle-missing-arguments argument-data) 
        (run (argparse:get-argument-value argument-data "--configuration")))
    (error (condition)
	  (wrn condition))))

  
(defun build ()
  "Save executable with necessary options."
  (save-lisp-and-die "ann-sim"
                     :executable t
                     :toplevel 'main
                     :save-runtime-options t))

;; ---------------------------------------------------------------------
;; Calling network training, validation and testing 
;; with sample network configurations 
;; ---------------------------------------------------------------------

(defun demo-small-sine ()
  "Demo with configuration from file. 
Epoch-size is cycles * number_of_input_data_lines"
  (run "../conf/small-sine-approximator.conf"))

(defun demo-circle ()
  "Demo with configuration from file. 
Epoch-size is cycles * number_of_input_data_lines"
  (run "../conf/circle2d-approximator.conf"))

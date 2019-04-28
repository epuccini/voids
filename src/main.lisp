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
  (use-package :fpp)
  (use-package :lparallel)
  (use-package :async-syntax)
  (use-package :flood)
  (use-package :nn))

(enable-async-syntax)

;; ---------------------------------------------------------------------
;; Init, run and cleanup
;; ---------------------------------------------------------------------
(defun prologue ()
  "Init application random-seed and kernel."
  (setf *random-state* (make-random-state t))
  (setf *kernel* (make-kernel (get-cores)))
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


(defparameter *help-message* 
  (format nil "Usage: ann-ff [options] [configfile]~% ~
Without given configuration file, the ~
program starts up in repl. ~% ~
Options:~%-c | --configuration    The configuration-file for the setup ~
of an artificial neural network. Topology and generated output files ~
and images are configured there. ~%~% ~
-h | --help     Prints this help message. ~
To start a fully configured training and ~
validation, just type:~%~%(run [configfile] [testing:t|nil])~%~% ~
Try these demos:~%(demo-small-sine) ;; Regression of sine-function ~
with less training-data.~%(demo-circle) ;; Regression of ~
sine- and cosine on 2 inputs and outputs~%(demo-double) ~
;; Regression of sine- and cosine 2 threads ~
in parallel with one input and one output~%(demo-four) ~
;; Regresson of four-threaded functions ~
in parallel with one input and one output~%~%"))

(defun command-line-args ()
  "Get implementation dependent commandline
arguments."
  (or
   ;;#+ECL si:*command-args*
   #+SBCL *posix-argv*
   #+GCL si::*command-args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP si:*command-args*
   nil))

 (defun main ()
  "Main entry point. Without param prints this help text.
With a specified configuration-file a optimisation-step is
beeing executed."
  (let ((arg1 (cadr (command-line-args)))
		(arg2 (caddr (command-line-args))))
  (declare (type simple-string arg1)
		   (type simple-string arg2)
		   (type simple-string *help-message*))
    (handler-case
     (cond ((or (equal arg1 "-c") (equal arg1 "--configuration"))
	    (run arg2))
	   ((or (equal arg1 "-h") (equal arg1 "--help"))
	    (format t *help-message*))
	   ((not arg1)
	    (format t *help-message*)))
     (error (condition)
	    (wrn condition)))))

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

(defun demo-double ()
  °(run "../conf/positive-sine-approximator.conf")
  °(run "../conf/positive-cosine-approximator.conf"))

(defun demo-four ()
  °(run "../conf/positive-sine-approximator.conf")
  °(run "../conf/positive-cosine-approximator.conf")
  °(run "../conf/circle-approximator.conf")
  °(run "../conf/circle3d-approximator.conf"))


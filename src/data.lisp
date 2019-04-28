; -------------------------------------------------------------
; Edward Alan Puccini 12.03.2016
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; data.lisp - Training and validation-data
; -------------------------------------------------------------

(in-package :nn)

;; ---------------------------------------------------------------------

(defun create-small-sine-training ()
  (save-data 
   "../input/training-in-small-sine.dat"
   (loop for x from 0.0d0 to (* 2 PI) by 0.05 collect 
		(list :INPUTS (list x) :TARGETS (list (sin x))))))

(defun create-small-sine-validation ()
  (save-data 
   "../input/validation-in-small-sine.dat"
   (loop for x from 0.025d0 to (* 2 PI) by 0.05 collect 
		(list :INPUTS (list x) :TARGETS (list (sin x))))))

(defun create-full-sine-training ()
  (save-data 
   "../input/training-in-full-sine.dat"
   (loop for x from 0.0d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (sin x))))))

(defun create-positive-sine-training ()
  (save-data 
   "../input/training-in-positive-sine.dat"
   (loop for x from 0.0d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (abs (sin x)))))))

(defun create-full-sine-validation ()
  (save-data 
   "../input/validation-in-full-sine.dat"
   (loop for x from 0.5d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (sin x))))))

(defun create-positive-sine-validation ()
  (save-data 
   "../input/validation-in-positive-sine.dat"
   (loop for x from 0.005d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (abs (sin x)))))))

(defun create-positive-cosine-training ()
  (save-data 
   "../input/training-in-positive-cosine.dat"
   (loop for x from 0.0d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (abs (cos x)))))))

(defun create-positive-cosine-validation ()
  (save-data 
   "../input/validation-in-positive-cosine.dat"
   (loop for x from 0.005d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (abs (cos x)))))))

(defun create-skipped-sine-training ()
  (save-data 
   "../input/training-in-skipped-sine.dat"
   (loop for x from 0.0d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (act-log (sin x)))))))

(defun create-skipped-sine-validation ()
  (save-data 
   "../input/validation-in-skipped-sine.dat"
   (loop for x from 0.005d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (act-log (sin x)))))))

(defun circle-training ()
  (save-data 
   "../input/training-in-circle.dat"
   (loop for x from 0.0d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x x) :TARGETS (list (act-log (cos x))
												(act-log (sin x)))))))
(defun circle-validation ()
  (save-data 
   "../input/validation-in-circle.dat"
   (loop for x from 0.005d0 to (* 2 PI) by 0.01 collect 
		(list :INPUTS (list x x) :TARGETS (list (act-log (cos x))
												(act-log (sin x)))))))
(defun circle3d-training ()
  (save-data 
   "../input/training-in-circle3d.dat"
   (loop for x from 0.0d0 to (* 4 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (act-log (cos x))
											  (act-log (sin x))
											  (act-log x))))))

(defun 3dcircle3d-training ()
  (save-data 
   "../input/training-in-3dcircle3d.dat"
   (loop for x from 0.01d0 to (* 8 PI) by 0.01 collect
		(progn
		  (list :INPUTS (list x) :TARGETS (list (act-log (sin x))
												  (act-log (cos x))
												  (act-log x)))))))

(defun circle3d-validation ()
  (save-data 
   "../input/validation-in-circle3d.dat"
   (loop for x from 0.005d0 to (* 4 PI) by 0.01 collect 
		(list :INPUTS (list x) :TARGETS (list (act-log (cos x))
											  (act-log (sin x))
											  (act-log x))))))


(defun 3dcircle3d-validation ()
  (save-data 
   "../input/validation-in-3dcircle3d.dat"
   (loop for x from 0.005d0 to (* 8 PI) by 0.01 collect
		(progn
		  (list :INPUTS (list x) :TARGETS (list (act-log (sin x))
												  (act-log (cos x))
												  (act-log x)))))))

(defun create-demo-data ()
  "Create demo-training and -validation-data."
  (create-small-sine-training)
  (create-small-sine-validation)
  (create-positive-sine-training)
  (create-positive-sine-validation)
  (create-positive-cosine-training)
  (create-positive-cosine-validation)
  (create-skipped-sine-training)
  (create-skipped-sine-validation)
  (circle-training)
  (circle-validation)
  (3dcircle3d-training)
  (3dcircle3d-validation)
  (circle3d-training)
  (circle3d-validation))

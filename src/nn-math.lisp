; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-math.lisp - Artificial neural network math
; Activation-, threshold-functions and random-number generators
; -------------------------------------------------------------

(in-package :nn)

;
; Optimize
;
(declaim (optimize (speed 3) (space 0) (debug 0)))

(defun act-th (output th)
  "Threshold activation function. Threshold specified
by global variable."
  (declare (type double-float output th))
  (if (> output th) 1 0))

(defun act-log (x)
  "Sigmoid activation function using logarithm.
Creates a value between 0.0 and 1.0."
  (declare (type double-float x))
  (act-log-t x 1.0d0))

(defun act-log-t (x temp)
  "Sigmoind activation function using logarithm.
Creates a value between 0.0 and 1.0.
temp-parameter defines how steep the slope is."
  (declare (type double-float x temp))
  (/ 1.0d0 (+ 1.0d0 (exp (/ (* -1.0d0 x) temp)))))

(defun act-tanh (x)
  "Sigmoid activation function using tangus hyperbolic.
Creates value between -1.0 and 1.0."
  (declare (type double-float x))
  (tanh x))

(defun act-atan (x)
  "Sigmoid activation function using tangus hyperbolic.
Creates value between -1.0 and 1.0."
  (declare (type double-float x))
  (atan x))

(defun act-linear (x)
  "Return as it is."
  (declare (type double-float x))
  x)

(defun gauss (x a b c)
  "Gaussion distribution function."
  (declare (type double-float x a b c))
  (* a (exp (* -1 (/ (sqrt (- x b)) (* 2 (sqrt c)))))))

(defun act-gauss (x)
  "Normal distribution."
  (declare (type double-float x))
  (gauss x 1 1 1))

;;
;; Dummy functions
;;
(defun act-fn-input (x)
  (declare (type double-float x))
  (act-linear x))

(defun act-fn-hidden (x)
  (declare (type double-float x))
  (act-log x))

(defun act-fn-output (x)
  (declare (type double-float x))
  (act-log x))

;;
;; Random numbers in different ranges
;;
(defun random-below-one ()
"Create a random number between 0.0 and 1.0."
  (/ (random 10.0d0) 10.0d0))
   
(defun random-plus-minus-one ()
  "Create a random number between -1.0 and 1.0"
  (let ((state (make-random-state t)))
    (+ -1.0d0 (/ (random 10.0d0 state) 5.0d0))))

(defun random-plus-minus-10th ()
  "Create a random number between -0.1 and 0.10"
  (let ((state (make-random-state t)))
    (+ -0.1d0 (/ (random 10.0d0 state) 50.0d0))))

(defun random-plus-minus-100th ()
  "Create a random number between -0.01 and 0.010"
    (let ((state (make-random-state t)))
      (+ -0.1d0 (/ (random 10.0d0 state) 500.0d0))))

(defun random-plus-minus-1000th ()
  "Create a random number between -0.001 and 0.0010"
  (let ((state (make-random-state t)))
    (+ -0.1d0 (/ (random 10.0d0 state) 5000.0d0))))

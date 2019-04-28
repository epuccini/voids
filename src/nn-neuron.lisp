; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-types.lisp - Neuron data structure
; Classes for artificial input-, hidden- and output-neurons
; -------------------------------------------------------------

(in-package :nn)

;; ------------------------------
;; We have no pointers at hand,
;; so we create neurons with id's
;; Now we can identify every 
;; single neuron
;;-------------------------------

(defvar *serial-id* 0)

(defun inc-serial-id ()
  (setq *serial-id* (1+ *serial-id*))
  *serial-id*)

(defclass neuron-serializable ()
  ((id :type 'integer 
	   :accessor n-id
	   :initform (inc-serial-id))))
	   
(defmethod ms:class-persistant-slots ((self neuron-serializable))
  '(id))

;;
;; Class for all kinds of neurons
;;
(defclass neuron (neuron-serializable)
 ((net :accessor n-net
	:initform 0.0d0
	:type 'double-float)
  (delta :accessor n-delta
	 :initform 0.0d0
	 :type 'double-float)
  (errord :accessor n-errord
	  :initform 0.0d0
	  :type 'double-float)
  (output :accessor n-output
	   :initarg :output
	   :initform 0.0d0
	   :type 'double-float)
  (weights :accessor n-weights
	   :initarg :weights
	   :type 'list)))

(defmethod ms:class-persistant-slots ((self neuron))
  '(id net delta errord output weights))

;;
;; Class for input-neurons
;;
(defclass ninput (neuron-serializable)
  ((input :accessor n-input
		  :initarg :input
		  :initform 0.0d0
		  :type 'double-float)))

(defmethod ms:class-persistant-slots ((self ninput))
  '(id input))

;
; Constructor
;
(defmethod make-ninput ()
  (make-instance 'ninput))

(defclass noutput (neuron)
  ((target :accessor n-target
		   :initform 0.0d0
		   :type 'bit)))

(defmethod ms:class-persistant-slots ((self noutput))
  '(id target weights))

(defun fn-collect (f n &optional (p nil))
  "Call given function n times and create a list 
of results"
  (if p 
	  (loop for i from 1 to n collect 
		   (funcall f p))
	  (loop for i from 1 to n collect 
		   (funcall f))))

;; create hidden-neuron with i-weights
(defmethod make-nhidden (i)
  (make-instance 'neuron :weights (fn-collect #'random-plus-minus-one i)))

; create output-neuron with i-weights
(defmethod make-noutput (i)
  (make-instance 'noutput :weights (fn-collect #'random-plus-minus-one i)))

(defclass nbias (neuron-serializable)
  ((output :accessor n-output
	   :initform 1
	   :type 'integer)
   (errord :accessor n-errord
	   :initform 0.0d0
	   :type 'double-float)
   (delta :accessor n-delta
	  :initform 0.0d0
	   :type 'double-float)
   (weight :accessor n-weight
	   :initarg :weight
	   :type 'double-float)))

(defmethod ms:class-persistant-slots ((self nbias))
  '(id output errord delta weight))

;;					
;; Constructor for nnetwork bias
;;
(defmethod make-nbias ()
  (make-instance 'nbias :weight (random-plus-minus-one)))

(defun fn-array (f n &optional (p nil))
  "Call given function n times and create an
array of results"
  (let ((a (make-array n)))
	(dotimes (i n)
	  (if p 
		  (setf (aref a i) (funcall f p))
		  (setf (aref a i) (funcall f))))
	a))

(defun make-hidden-layer (inputs layer neurons)
  "Make a list of neuron-layer. The number of weights
per neurons is the amount of neuron of the previous layer."
  (let ((hidden (make-array layer)))
	(loop for l from 1 to (1- layer) collect
		 (setf (aref hidden l)
			   (fn-array #'make-nhidden neurons neurons)))
	(setf (aref hidden 0) (fn-array #'make-nhidden neurons inputs))
	hidden))
	

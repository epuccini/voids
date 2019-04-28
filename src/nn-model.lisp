; -------------------------------------------------------------
; Edward Alan Puccini 28.03.2016
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-model.lisp - Network model
; Training, validation and testing phases
; -------------------------------------------------------------

(in-package :nn)

;;
;; Class for automatic network-id
;;
(defvar *model-id* 0)

(defun inc-model-id ()
  (setq *model-id* (1+ *model-id*))
  *model-id*)

(defclass model-serializable ()
  ((id :type 'integer 
	   :accessor nnmodel-id
	   :initform (inc-model-id))))


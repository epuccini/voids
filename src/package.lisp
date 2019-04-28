; -------------------------------------------------------------
; Edward Alan Puccini 24.05.2013
; -------------------------------------------------------------
; Feedforward Artificial Neural Network lib and main program.
; with backpropagation learning-, offline algorithm 
; -------------------------------------------------------------
; nn-package - Application package nn for Neural Network
; All public functions in the whole namepspace.
; -------------------------------------------------------------
(in-package :cl-user)

(require 'async-syntax)

;
; Package nn for neural networks
;
(defpackage :nn
  (:use 
   #:cl 
   #:fpp 
   #:async-syntax 
   #:flood 
   #:ga
   #:bordeaux-threads 
   #:lparallel)
  (:export
   #:*serial-id*
   #:*network-id*
   #:n-id
   #:nnetwork-id
   #:optimise
   #:network-info
   #:with-network-info
   #:create-demo-data
   #:default-config
   #:dynamic-default-config
   #:add-to-store-cycle
   #:add-to-store-si-epoch
   #:add-to-store-si-cycle
   #:load-configfile
   #:save-configfile
   #:marshal-network
   #:unmarshal-network
   #:make-ncons
   #:nconns-init-feedforward
   #:nconns-clear
   #:nconns-unset
   #:make-nnetwork
   #:make-ninput
   #:make-nhidden
   #:make-noutput
   #:nnetwork
   #:nnmodel-id
   #:nnetwork-inputs
   #:nnetwork-config
   #:nnetwork-errort
   #:nnetwork-mset
   #:print-keyval
   #:repeat-fn-to-list
   #:feed-inputs
   #:feed-targets
   #:training
   #:training-plot
   #:train-phase
   #:validation
   #:validation-plot
   #:testing
   #:train-set
   #:validate-set
   #:train-epoches
   #:validate-epoche
   #:train-single-epoch
   #:validate-single-epoch
   #:check-result
   #:get-input
   #:fn-collect
   #:save-data
   #:load-data
   #:load-db
   #:load-object
   #:save-object
   #:act-th
   #:act-log
   #:act-log-t
   #:act-tanh
   #:act-atan
   #:act-linear
   #:act-gauss
   #:act-fn-input
   #:act-fn-hidden
   #:act-fn-output
   #:output
   #:make-hidden-layer
   #:string-to-function
   #:partition
   #:random-below-one
   #:random-plus-minus-one
   #:calc-network-output
   #:calc-delta
   #:calc-output-delta
   #:calc-hidden-delta
   #:calc-delta-weight-sum
   #:calc-weights
   #:calc-bias
   #:print-total-error-plot
   #:calc-errort
   #:reset-errort
   #:save-mset
   #:save-errort
   #:calc-mset
   #:reset-mset
   #:cleanup
   #:set-to-store-entry-at
   #:add-to-store
   #:make-result-entry-at
   #:send-to-gnuplot
   ))

; -------------------------------------------------------------
; Edward Alan Puccini 13.02.2016
; -------------------------------------------------------------
; Voids library - backpropagation artificiel
; neural network simulation simulation application
; -------------------------------------------------------------
; file: voids-simulation.asd
; -------------------------------------------------------------

(require :asdf)

(defsystem "voids-simulation"
  :description "Voids - backpropagation artificial neural network simulation application."
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "trivial-shell" "marshal" "fpp" "lparallel"
				"async-syntax" "flood" "bordeaux-threads")
  :components (( :file "package" )
			   ( :file "nn-config" :depends-on ( "package" ))
			   ( :file "nn-neuron" :depends-on ("nn-config"))
			   ( :file "nn-model" :depends-on ("nn-neuron"))
			   ( :file "nn-network" :depends-on ("nn-model"))
			   ( :file "nn-print" :depends-on ("nn-network"))
			   ( :file "nn-math" :depends-on ("nn-network"))
			   ( :file "nn-logic" :depends-on ("nn-math"))
			   ( :file "nn-io" :depends-on ("nn-logic"))			   
			   ( :file "data" :depends-on ("nn-io"))
			   ( :file "main" :depends-on ("data"))))

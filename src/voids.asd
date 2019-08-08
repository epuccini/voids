; -------------------------------------------------------------
; Edward Alan Puccini 13.02.2016
; -------------------------------------------------------------
; Voids library - backpropagation artificiel
; neural network simulation library
; -------------------------------------------------------------
; file: voids.asd
; -------------------------------------------------------------

(require :asdf)

(defsystem "voids"
  :description "Voids - backpropagation artificial neural network simulation library."
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "trivial-shell" "marshal" "fpp" "lparallel"
				"argparse" "flood" "bordeaux-threads")
  :components (( :file "package" )
			   ( :file "nn-config" :depends-on ( "package" ))
			   ( :file "nn-neuron" :depends-on ("nn-config"))
			   ( :file "nn-model" :depends-on ("nn-neuron"))
			   ( :file "nn-network" :depends-on ("nn-model"))
			   ( :file "nn-print" :depends-on ("nn-network"))
			   ( :file "nn-math" :depends-on ("nn-print"))
			   ( :file "nn-logic" :depends-on ("nn-math"))
			   ( :file "nn-io" :depends-on ("nn-logic"))))



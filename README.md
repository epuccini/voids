# voids
Backpropagation feedforward network simulator with GUI<br/><br/>

# Change your current directory in your command line to the "src" source direcrory<br/><br/>
    cd ./src

# Start Steel Bank Common Lisp<br/><br/>
    sbcl

# Load system file<br/><br/>

* (asdf:load-system :voids-simulation)
WARNING:
   redefining EMACS-INSPECT (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>) in DEFMETHOD
WARNING: DEFIMPLEMENTATION of undefined interface (WAKE-THREAD)
WARNING: DEFIMPLEMENTATION of undefined interface (FLOAT-NAN-P)
WARNING: DEFIMPLEMENTATION of undefined interface (FLOAT-INFINITY-P)
WARNING: DEFIMPLEMENTATION of undefined interface (MAKE-AUTO-FLUSH-THREAD)
WARNING: System definition file #P"/Users/edward/quicklisp/dists/quicklisp/software/trivial-garbage-20150113-git/trivial-garbage.asd" contains definition for system "trivial-garbage-tests". Please only define "trivial-garbage" and secondary systems with a name starting with "trivial-garbage/" (e.g. "trivial-garbage/test") in that file.
T

# Buid a executable file with<br/><br/>
* (build)

# Start a demo "Small circle"<br/><br/>
* (demo-small-sine)
[17:07:48 / 15.08.2022]-[INF]-TRAIN phase start 'ann-ff-bp-small-sine' network: 1
[17:07:50 / 15.08.2022]-[INF]-Realtime: 1.921 s Runtime: 1.921 s network: 1
[17:07:50 / 15.08.2022]-[INF]-TRAIN phase end 'ann-ff-bp-small-sine' network: 1
[17:07:50 / 15.08.2022]-[INF]-VALIDATE phase start 'ann-ff-bp-small-sine' network: 1
[17:07:50 / 15.08.2022]-[INF]-Realtime: 0.006 s Runtime: 0.006 s network: 1
[17:07:50 / 15.08.2022]-[INF]-VALIDATE phase end 'ann-ff-bp-small-sine' network: 1
[17:07:50 / 15.08.2022]-[INF]-SAVE-OBJECT phase start 'ann-ff-bp-small-sine' network: 1
[17:07:50 / 15.08.2022]-[INF]-Realtime: 0.007 s Runtime: 0.007 s network: 1
[17:07:50 / 15.08.2022]-[INF]-SAVE-OBJECT phase end 'ann-ff-bp-small-sine' network: 1
[17:07:50 / 15.08.2022]-[INF]-TRAINING-PLOT phase start 'ann-ff-bp-small-sine' network: 1
[17:07:52 / 15.08.2022]-[INF]-Realtime: 1.598 s Runtime: 1.598 s network: 1
[17:07:52 / 15.08.2022]-[INF]-TRAINING-PLOT phase end 'ann-ff-bp-small-sine' network: 1
[17:07:52 / 15.08.2022]-[INF]-VALIDATION-PLOT phase start 'ann-ff-bp-small-sine' network: 1
[17:07:52 / 15.08.2022]-[INF]-Realtime: 0.254 s Runtime: 0.254 s network: 1
[17:07:52 / 15.08.2022]-[INF]-VALIDATION-PLOT phase end 'ann-ff-bp-small-sine' network: 1


  20,187,552 bytes for 94,316 immobile objects (space total)

1500 epochs / 126 bytes per training set. Ready after 189000 cycles!


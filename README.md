# voids
Backpropagation feedforward network simulator with GUI<br/><br/>

# Change your current directory in your command line to the "src" source direcrory<br/><br/>
    cd ./src

# Start Steel Bank Common Lisp<br/><br/>
    sbcl

# Load system file<br/><br/>

* (asdf:load-system :voids-simulation)<br/><br/>
WARNING:<br/>
   redefining EMACS-INSPECT (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>) in DEFMETHOD<br/>
WARNING: DEFIMPLEMENTATION of undefined interface (WAKE-THREAD)<br/>
WARNING: DEFIMPLEMENTATION of undefined interface (FLOAT-NAN-P)<br/>
WARNING: DEFIMPLEMENTATION of undefined interface (FLOAT-INFINITY-P)<br/>
WARNING: DEFIMPLEMENTATION of undefined interface (MAKE-AUTO-FLUSH-THREAD)<br/>
WARNING: System definition file #P"/Users/edward/quicklisp/dists/quicklisp/software/trivial-garbage-20150113-git/trivial-garbage.asd" contains definition for system "trivial-garbage-tests". Please only define "trivial-garbage" and secondary systems with a name starting with "trivial-garbage/" (e.g. "trivial-garbage/test") in that file.<br/><br/>
T

# Buid a executable file with<br/><br/>
* (build)<br/><br/>

# Start a demo "Small circle"<br/><br/>
* (demo-small-sine)<br/><br/>
[17:07:48 / 15.08.2022]-[INF]-TRAIN phase start 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-Realtime: 1.921 s Runtime: 1.921 s network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-TRAIN phase end 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-VALIDATE phase start 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-Realtime: 0.006 s Runtime: 0.006 s network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-VALIDATE phase end 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-SAVE-OBJECT phase start 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-Realtime: 0.007 s Runtime: 0.007 s network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-SAVE-OBJECT phase end 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:50 / 15.08.2022]-[INF]-TRAINING-PLOT phase start 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:52 / 15.08.2022]-[INF]-Realtime: 1.598 s Runtime: 1.598 s network: 1<br/>
[17:07:52 / 15.08.2022]-[INF]-TRAINING-PLOT phase end 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:52 / 15.08.2022]-[INF]-VALIDATION-PLOT phase start 'ann-ff-bp-small-sine' network: 1<br/>
[17:07:52 / 15.08.2022]-[INF]-Realtime: 0.254 s Runtime: 0.254 s network: 1<br/>
[17:07:52 / 15.08.2022]-[INF]-VALIDATION-PLOT phase end 'ann-ff-bp-small-sine' network: 1<br/><br/>


  20,187,552 bytes for 94,316 immobile objects (space total)<br/><br/>

1500 epochs / 126 bytes per training set. Ready after 189000 cycles!


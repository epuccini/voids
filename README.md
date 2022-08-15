# voids
Backpropagation feedforward network simulator with GUI

# Change your current directory in your command line to the "src" source direcrory
cd ./src

# Start Steel Bank Common Lisp
sbcl

# Load system file
* (asdf:load-system :voids-simulation)
WARNING:
   redefining EMACS-INSPECT (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>) in DEFMETHOD
WARNING: DEFIMPLEMENTATION of undefined interface (WAKE-THREAD)
WARNING: DEFIMPLEMENTATION of undefined interface (FLOAT-NAN-P)
WARNING: DEFIMPLEMENTATION of undefined interface (FLOAT-INFINITY-P)
WARNING: DEFIMPLEMENTATION of undefined interface (MAKE-AUTO-FLUSH-THREAD)
WARNING: System definition file #P"/Users/edward/quicklisp/dists/quicklisp/software/trivial-garbage-20150113-git/trivial-garbage.asd" contains definition for system "trivial-garbage-tests". Please only define "trivial-garbage" and secondary systems with a name starting with "trivial-garbage/" (e.g. "trivial-garbage/test") in that file.
T

# Buid a executable file with
* (build)

# Start a demo "Small circle"
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
Dynamic space usage is:   55,871,904 bytes.
Immobile space usage is:  20,515,120 bytes (104,256 bytes overhead).
Read-only space usage is:          0 bytes.
Static space usage is:         2,784 bytes.
Control stack usage is:        2,280 bytes.
Binding stack usage is:          656 bytes.
Control and binding stack usage is for the current thread only.
Garbage collection is currently enabled.

Breakdown for dynamic space:
  14,026,992 bytes for   119,142 simple-vector objects
  10,496,528 bytes for   181,947 instance objects
   9,727,264 bytes for   607,954 cons objects
   4,175,232 bytes for     4,295 simple-array-unsigned-byte-64 objects
   4,106,624 bytes for    68,983 simple-array-unsigned-byte-8 objects
   3,619,920 bytes for    74,294 simple-base-string objects
   3,024,464 bytes for    24,057 simple-character-string objects
   5,894,400 bytes for   148,389 other objects

  55,071,424 bytes for 1,229,061 dynamic objects (space total)

Breakdown for immobile space:
  17,458,432 bytes for 30,147 code objects
   1,550,352 bytes for 32,286 symbol objects
   1,178,768 bytes for 31,883 other objects

  20,187,552 bytes for 94,316 immobile objects (space total)

1500 epochs / 126 bytes per training set. Ready after 189000 cycles!
NIL

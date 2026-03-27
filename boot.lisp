;; boot.lisp
(format t "~%[BOOT] Initiating Bogu compilation sequence...~%")

(defpackage :bogu
  (:use :cl))

;; 0. Prepare the Environment (Load dependencies BEFORE compiling)
(format t "[BOOT] Loading external libraries...~%")
(ql:quickload "cl-ppcre")
;;(ql:quickload "osc")

;; 1. Automatically detect the EXACT directory this script is sitting in
(defparameter *bogu-dir*
  (make-pathname :name nil :type nil 
                 :defaults (or *compile-file-truename* *load-truename* *default-pathname-defaults*)))

;; 2. Define the strict dependency order
(defparameter *bogu-build-order*
  '("globals.lisp"
    "utilities.lisp"
    "audio-pipe.lisp"   ;; <-- The new bridge!
    "parser.lisp"
    "music-math.lisp"
    "commands.lisp"
    "help.lisp"
    "composition-repl.lisp"))

;; 3. Compile to machine code and load into RAM sequentially
(dolist (file *bogu-build-order*)
  (let ((full-path (merge-pathnames file *bogu-dir*)))
    (format t "~%[BOOT] Compiling ~A...~%" full-path)
    (let ((compiled-binary (compile-file full-path)))
      (if compiled-binary
          (load compiled-binary)
          (error "[BOOT FATAL ERROR] Lisp could not find or compile ~A" full-path)))))

(format t "~%[BOOT] All systems online. Starting engine...~%")

;; 4. Automatically launch the DSL
;;(bogu)

;; boot.lisp
(format t "~%[BOOT] Initiating Bogu compilation sequence...~%")

(defpackage :bogu
  (:use :cl))

(in-package :bogu)

;; 0. Prepare the Environment
(format t "[BOOT] Loading external libraries...~%")
(ql:quickload "cl-ppcre")

(defparameter *bogu-dir*
  (make-pathname :name nil :type nil 
                 :defaults (or *compile-file-truename* *load-truename* *default-pathname-defaults*)))

;; 2. Strict Dependency Order
(defparameter *bogu-build-order*
  '("globals.lisp"              
    "core/utilities.lisp"       
    "core/validator.lisp"
    "stdlib/music-math.lisp"    
    "audio/audio-pipe.lisp"     
    "audio/audio-engine.lisp"   
    "audio/synth-defs.lisp"     
    "core/parser.lisp"          
    "core/evaluator.lisp"       
    "stdlib/commands.lisp"           
    "interface/export.lisp"     
    "interface/project.lisp"    
    "interface/help.lisp"
    "interface/composition-repl.lisp"
    "core/l-system.lisp"
    
    "tests.lisp"))              

;; 3. Compile to machine code and load into RAM sequentially
(dolist (file *bogu-build-order*)
  (let ((full-path (merge-pathnames file *bogu-dir*)))
    (format t "~%[BOOT] Compiling ~A...~%" full-path)
    (let ((compiled-binary (compile-file full-path)))
      (if compiled-binary
          (load compiled-binary)
          (error "[BOOT FATAL ERROR] Lisp could not find or compile ~A" full-path)))))

(format t "~%[BOOT] All systems online. Starting engine...~%")

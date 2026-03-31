(in-package :bogu)

;; --- 1. SYSTEM STATE ---
(defvar *master-epoch* nil "The exact microsecond the Bogu universe began.")
(defparameter *bpm* '(60 0 "t"))
(defparameter *beats-per-bar* 4.0 "Standard 4/4 time.")
(defparameter *quantize-mode* :exact "Can be :exact, :free, or a decimal slop.")

;; --- 2. THE COMPOSITION MATRIX ---
(defparameter *score* '())
(defparameter *bogu-code* '())
;;(defparameter *playheads* (make-hash-table))
(defparameter *current-instrument* 1)
(defparameter *current-key* nil)
(defparameter *current-articulation* nil)
(defparameter *transpose-offset* 0)
(defparameter *velocity* 0.8)
(defparameter *current-project* nil)


;; --- 3. LIVE-LOOPING THREADS ---
(defparameter *live-loops* (make-hash-table :test 'equal))
(defparameter *loop-threads* (make-hash-table :test 'equal))
(defvar *play-thread* nil)

;; --- 4. LANGUAGE DICTIONARIES ---
(defparameter *command-dictionary* (make-hash-table :test 'eq))
(defparameter *vars* (make-hash-table :test 'equal))
(defparameter *stdlib-vars* (make-hash-table :test 'equal))

;; --- 5. HARDWARE RACK ---
(defparameter *synth-templates* (make-hash-table))
(defparameter *synth-rack* (make-hash-table))

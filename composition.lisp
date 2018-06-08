(defclass composition ()
  ((name :initarg :name
	 :initform "default"
	 :accessor name)
   (melodies :initarg :melodies
	     :initform '()
	     :accessor melodies)
   (header :initarg :header
	   :initform (make-instance 'header)
	   :accessor header)
   (bpm :initarg :bpm
	:initform 60
	:accessor bpm)
   (midi-key :initarg :midi-key
	     :initform 48
	     :accessor midi-key)))

(defclass header ()
  ((sr :initarg :sr
       :initform 44100
       :accessor sr)
   (kr :initarg :kr
       :initform 4410
       :accessor kr)
   (ksmps :initarg :ksmps
	  :initform 10
	  :accessor ksmps)
   (nchnls :initarg :nchnls
	   :initform 1
	   :accessor nchnls)))


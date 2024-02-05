(in-package bitsch)

; Author : Arno Geurts
; Date : 30-01-2024
; This file contains a temporary testing library

(print "Loading cp-params object...")

(om::defclass! cp-params ()
  ;attributes
  (
   ; ---------- Input cantus firmus ----------
   (cf-voice :accessor cf-voice :initarg :cf-voice :initform nil :documentation "")
   ; ---------- Output & Stop ----------
   ;(current-csp :accessor current-csp :initform nil :documentation "")
   ;(result-voice :accessor result-voice :initarg :result-voice :initform nil :documentation "")
  )
  (:icon 225)
  (:documentation "This class implements counterpoint following the rules of Marcel Bitsch")
  (
    (print "here")
    (print (chord-to-midi cf-voice))
    
  )
  
)

(om::defmethod! args-prep ((species number) (voice om::voice))
  :initvals '(1 nil)
  :indoc '("species" "midi values")
  :icon 176
  :doc "Take the output of a voice object and gets the needed information (midicent values) for the CSP"
  (setq chords (om::chords voice))
  (setq midic_list (loop :for n :from 0 :below (length chords) :by 1 collect (om::lmidic (nth n chords))))
  (setq midic_list_flatten (flatten midic_list))
  (setq midic_list_str (list_to_string midic_list_flatten))
  ;(setq midic_int (parse-integer midic_list_str))
)

; from https://www.lee-mac.com/flatten.html
(defun flatten ( l )
    (if (atom l)
        (list l)
        (append (flatten (car l)) (if (cdr l) (flatten (cdr l))))
    )
)

(defun list_to_String (lst)
    (format nil "~{~A~}" lst))




; <chords> a list of chord object
; Return the list of pitch contained in chords in midi format
(defun to-pitch-list (chords)
     (loop :for n :from 0 :below (length chords) :by 1 collect (to-midi (om::lmidic (nth n chords))))
)




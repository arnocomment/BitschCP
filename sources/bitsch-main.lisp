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
   (current-csp :accessor current-csp :initform nil :documentation "")
   (result-voice :accessor result-voice :initarg :result-voice :initform nil :documentation "")
  )
  (:icon 225)
  (:documentation "This class implements counterpoint following the rules of Marcel Bitsch")
)







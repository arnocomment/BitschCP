(in-package :om)

(defvar *Bitsch-path* (make-pathname  :directory (append (pathname-directory *LOAD-PATHNAME*) (list "Bitsch"))))

(require-library "GIL")

(defpackage :bitsch
(:use "COMMON-LISP" "OM" "CL-USER"))
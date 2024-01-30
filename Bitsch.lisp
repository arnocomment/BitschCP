(in-package :om)

(defvar *bitsch-sources-dir* nil)
(setf *bitsch-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))


(mapc 'om::compile&load 
      (list
        (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
        (make-pathname :directory (pathname-directory *bitsch-sources-dir*) :name "bitsch-main" :type "lisp")
       ))



(om::fill-library '(
    ("Solver" nil (bitsch::cp-params) nil)
   ))

(print "Bitsch loaded")
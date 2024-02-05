(in-package :om)

(defvar *bitsch-sources-dir* nil)
(setf *bitsch-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))

(defvar *libgecode* nil)

; trying to load mac library and then linux library if mac doesn't work
(handler-case
    (progn
        (setf *libgecode* (make-pathname :directory (pathname-directory *bitsch-sources-dir*) :name "libgecode.dylib")) 
        (if (equal (cffi:load-foreign-library  *libgecode*) nil)
            (print "There is a problem loading the Framework. Please double check that Gecode is correctly installed and you are using the appropriate version of GiL for your Operative System")))
    (t (c)
       (progn
           (setf *libgecode* (make-pathname :directory (pathname-directory *bitsch-sources-dir*) :name "libgecode.so")) 
           (if (equal (cffi:load-foreign-library  *libgecode*) nil)
                (print "There is a problem loading the Framework. Please double check that Gecode is correctly installed and you are using the appropriate version of GiL for your Operative System")))
    )
)

(mapc 'om::compile&load 
      (list
        (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
        (make-pathname :directory (pathname-directory *bitsch-sources-dir*) :name "bitsch-main" :type "lisp")
        (make-pathname :directory (pathname-directory *bitsch-sources-dir*) :name "problem-wrapper" :type "lisp")
       ))



(om::fill-library '(
    ("Solver" nil (bitsch::cp-params) nil)
   ))

(print "Bitsch loaded")
;;;; cl-fpgasm-device.lisp

(in-package #:cl-fpgasm-device)


(defparameter *raw* nil)   ;raw sexps after reading xdlrc
(defparameter *dev* nil)

(defun read-file (name)
  (with-open-file (in name)
    (setf *raw* (read in)))
)
;;==============================================================================
(defun load-device (&key (name (asdf:system-relative-pathname 'cl-fpgasm-device #p"data/xc3s200.tweaked")))
  (parse-xdlrc (read-file name))
  *dev*
)

(defstruct dev 
  name                  ;name of device
  tech                  ;technology
  tiles                 ;ARRAY of tiles
  prim                  ;HASHTABLE of primitive-defs
) 
(defstruct tile name type x y prim-sites)
(defstruct prim-def name pins elements)
(defstruct prim-site name prim-def bond)
(defstruct element name pins cfg conns)

;;==============================================================================
;; PUBLIC INTERFACE
;;==============================================================================
(defun get-tile-at (x y)
  "get a tile at x y location"
  (aref (dev-tiles *dev*) x y))
;;==============================================================================
(defun get-prim-def (name)
  "get a prim-def by name"
  (gethash name (dev-prim *dev*)))
;;==============================================================================
(defun get-element (name &key of) ;of is prim-def
  (gethash name (prim-def-elements of)))


;;==============================================================================
;; PARSING
;;==============================================================================
(defun dev-init () 
  (setf *dev* (make-dev :name nil :tech nil :tiles nil :prim nil)))

;;==============================================================================
;; Generic function. 
;; 
;; Expressions like (type rest...) get called here, and specialized by type.
;; everyone gets dev; some parsing functions deeper in also get container,
;; specifying the particular collection we are parsing into.
;;
(defgeneric parse-xdlrc-expr (type rest dev container))
;;==============================================================================
;; And if type is not supported, 
;;
(defmethod parse-xdlrc-expr (type rest dev container)
  (format t "Unhandled type ~S~%" type)
  (error "Unhandled type in parse-xdlrc-expr")
)
;;==============================================================================
;; XDL_RESOURCE_REPORT version device tech
;;
(defmethod parse-xdlrc-expr ((type (eql 'xdl_resource_report)) rest dev unused)
  (destructuring-bind (version device tech &rest expr) rest
    (declare (ignore version))
    (setf dev (dev-init)
	  (dev-name dev) device
	  (dev-tech dev) tech)
    ;;parsing the second part allows us to use prim-defs.
    (parse-xdlrc (cadr expr) dev)
    (parse-xdlrc (car expr) dev)))
;;==============================================================================
;; TILES w h (
;;
(defmethod parse-xdlrc-expr ((type (eql 'tiles)) rest dev unused)
    (destructuring-bind (x y &rest exprs) rest
      (setf (dev-tiles dev) (make-array (list x y)))
      (loop for expr in exprs
	 for i from 0
	 do (parse-xdlrc expr dev)) 
  	))

;;==============================================================================
;; TILE x y n1 n2 cnt (          of TILES
;;   --primitive_sites
(defmethod parse-xdlrc-expr ((type (eql 'tile)) rest dev unused)
    (destructuring-bind (x y n1 n2 cnt &rest exprs) rest
      ;;(format t "tile: ~S ~S ~S ~S ~d~%" x y n1 n2 cnt)
      (let ((tile (make-tile :name n1 :type n2 :x x :y y :prim-sites
			     ;;TODO: for now primdefs are just a list
			     (loop for expr in exprs
				for i from 0 to (1- cnt)
				for j = (parse-xdlrc expr dev)
				collect j ))))
	;store right into the array)
	(setf (aref (dev-tiles dev) x y) tile))))
;;==============================================================================
;; PRIMITIVE_SITE                 LEAF of TILE
;;
(defmethod parse-xdlrc-expr ((type (eql 'PRIMITIVE_SITE)) rest dev unused)
    (destructuring-bind (n1 n2 n3 cnt) rest
      ;;(format t " primitive-site: ~S ~S ~S ~d~%" n1 n2 n3 cnt)
      (declare (ignore cnt))
      (make-prim-site 
	  :name n1 
	  :prim-def (gethash n2 (dev-prim dev))  
	  :bond n3)
      ;;TODO: for now just returning data; tile makes a list...
))


;;==============================================================================
;; PRIMITIVE_DEFS cnt (    
;;
;; hashtable containing prim-def entries
(defmethod parse-xdlrc-expr ((type (eql 'PRIMITIVE_DEFS)) rest dev unused)
  (destructuring-bind (cnt &rest exprs) rest
    ;;create the dev's hashtable!
    (setf (dev-prim dev) (make-hash-table :size cnt))
    (loop for expr in exprs
       for i from 0
       do (parse-xdlrc expr dev)) ))

;;==============================================================================
;; PRIMITIVE_DEF name pins elements (...
;;  --primitive_def...
;;

(defmethod parse-xdlrc-expr ((type (eql 'PRIMITIVE_DEF)) rest dev unused)
  (destructuring-bind (name pin-cnt element-cnt &rest exprs) rest
    ;;(format t "PRIMITIVE_DEF ~S...~%" name)
    (let ((data (make-prim-def 
		 :name name
		 :pins (make-hash-table :size pin-cnt)
		 :elements (make-hash-table :size element-cnt))))
      (loop for expr in exprs
	 do (parse-xdlrc expr dev data))
      ;; now insert data into the prim-defs hasthable
      (setf (gethash name (dev-prim dev)) data))))



;;==============================================================================
;; PIN name n1 input/output    for PRIMITIVE_DEF
;; PIN name input/output       for ELEMENT
;; 
(defmethod parse-xdlrc-expr ((type (eql 'PIN)) rest dev container)
  (cond 
    ((eql (type-of container) 'prim-def)
     (destructuring-bind (name n1 dir) rest
       ;;(format t "PIN ~S...~%" name)
       ;;Why are there two names here? Is it simply to differentiate PIN
       ;;parsers for ELEMENT and PRIMITIVE_DEF???
       (if (not (eql n1 name))
	   (error "ELEMENT-PIN parser found a name mismatch"))
       (setf (gethash name (prim-def-pins container)) dir)))
    
    ((eql (type-of container) 'element )
     (destructuring-bind (name dir) rest
       (setf (gethash name (element-pins container)) dir)))))

;;==============================================================================
;; ELEMENT name pins (...       for PRIMITIVE_DEF
;;  --pin
;;  --conn
;;  --cfg
;;
;;
(defmethod parse-xdlrc-expr ((type (eql 'ELEMENT)) rest dev primdef)
  (destructuring-bind (name pin-cnt &rest exprs) rest
    ;;(format t "ELEMENT ~S...~%" name)
    (let ((data (make-element
		 :name name
		 :pins (make-hash-table :size pin-cnt)
		 :cfg nil
		 :conns nil)))
      (loop for expr in exprs
	 do (parse-xdlrc expr dev data ))
      ;;and store it into the primdef structure's elements hashtable
      (setf (gethash name (prim-def-elements primdef)) data)
      )))

;;==============================================================================
;; CONN element pin dir element pin   for ELEMENT
;;
(defmethod parse-xdlrc-expr ((type (eql 'CONN)) rest dev element)
  ;; TODO: unsure of how to store this for fast access...
  ;;for now, just store the list
  (setf (element-conns element) (cons rest (element-conns element))))

;;==============================================================================
;; cfg configuration   for ELEMENT
;;
;; TODO: for now, just store the entire list
(defmethod parse-xdlrc-expr ((type (eql 'CFG)) rest dev element )
  (setf (element-cfg element) rest)
)

(defun parse-xdlrc (list &optional dev container)
 ; (format t "parsing ~S ~S~%" (car list) (type-of (car list)))
  (if list
      (parse-xdlrc-expr (car list) (cdr list) dev container)))


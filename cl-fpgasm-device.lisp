
;;;; cl-fpgasm-device.lisp
#|******************************************************************************
 Copyright 2012 Victor Yurkovsky

    This file is part of cl-fpgasm

    FPGAsm is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    FPGAsm is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with cl-fpgasm.  If not, see <http://www.gnu.org/licenses/>.
******************************************************************************|#
(in-package #:cl-fpgasm-device)
(setf *print-circle* t)    ;we have circular structures!

(defparameter *raw* nil)   ;raw sexps after reading xdlrc
(defparameter *dev* nil)

(defun read-file (name)
  (with-open-file (in name)
    (let ((*package* (find-package "CL-FPGASM-DEVICE")))
      (setf *raw* (read in))))
)
;;==============================================================================
(defun load-device (&key (name (asdf:system-relative-pathname 'cl-fpgasm-device #p"data/xc3s200.tweaked")))
  (parse-xdlrc (read-file name))
  t
)

(defstruct dev 
  name                  ;name of device
  tech                  ;technology
  tiles                 ;ARRAY of tiles
  prim-defs             ;HASHTABLE of primitive-defs
) 
(defstruct tile name type x y prim-sites)
(defstruct prim-site name prim-def bond  #||# tile)
(defstruct prim-def name pins elements)
(defstruct element name pins cfg conns )

(defun keys (hashtable)
  (loop for key being the hash-keys of hashtable collect key)
 ;;(maphash #'(lambda (key val) (print key) ) hashtable)
)


;;==============================================================================
;; PARSING
;;
;; Parsing rules: every object parses children and stores them as it needs to.
;;==============================================================================
(defun dev-init () 
  (setf *dev* (make-dev :name nil :tech nil :tiles nil :prim-defs nil)))

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
  (error "Unhandled type in parse-xdlrc-expr - TYPE ~S package ~S" type *package* )
)
;;==============================================================================
;; XDL_RESOURCE_REPORT version device tech
;;
(defmethod parse-xdlrc-expr ((type (eql 'xdl_resource_report)) rest dev unused)
  (print "XDL RESOURCE parser")
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
      (let ((array (make-array (list x y))))
       	(loop for expr in exprs
	   for i from 0
	   do 
	     (let ((tile (parse-xdlrc expr))) ;parse a tile
	       (setf (aref array (tile-x tile) (tile-y tile) )
		     tile)))
	(setf (dev-tiles dev) array) ;???
)))

;;==============================================================================
;; TILE x y n1 n2 cnt (          of TILES
;;   --primitive_sites
(defmethod parse-xdlrc-expr ((type (eql 'tile)) rest dev unused)
    (destructuring-bind (x y n1 n2 cnt &rest exprs) rest
      ;;(format t "tile: ~S ~S ~S ~S ~d~%" x y n1 n2 cnt)
      (let ((tile (make-tile :name n1 :type n2 :x x :y y 
			     :prim-sites nil)))
	(setf (tile-prim-sites tile) 
	      (cons (loop for expr in exprs
		       for i from 0 to (1- cnt)
		       for j = (parse-xdlrc expr dev tile)
		       collect j)
		    (tile-prim-sites tile)))
	
	(set n1 tile) ;BIND symbol n1 to name
)))

;;==============================================================================
;; PRIMITIVE_SITE                 LEAF of TILE
;;
(defmethod parse-xdlrc-expr ((type (eql 'PRIMITIVE_SITE)) rest dev tile)
    (destructuring-bind (n1 n2 n3 cnt) rest
      ;;(format t " primitive-site: ~S ~S ~S ~d~%" n1 n2 n3 cnt)
      (declare (ignore cnt))
      (let ((data (make-prim-site 
		   :name n1 
		   :prim-def n2  
		   :bond n3
		   :tile tile)))
	(set n1 data)) ;BIND to name
      ;;TODO: for now just returning data; tile makes a list...
))


;;==============================================================================
;; PRIMITIVE_DEFS cnt (    
;;
;; hashtable containing prim-def entries
(defmethod parse-xdlrc-expr ((type (eql 'PRIMITIVE_DEFS)) rest dev unused)
  (destructuring-bind (cnt &rest exprs) rest
    (declare (ignore cnt))
    (setf (dev-prim-defs dev) (loop for expr in exprs
				 for i from 0
				 for j = (parse-xdlrc expr dev)
				 collect j)) ))

;;==============================================================================
;; PRIMITIVE_DEF name pins elements (...
;;  --primitive_def...
;;

(defmethod parse-xdlrc-expr ((type (eql 'PRIMITIVE_DEF)) rest dev unused)
  (destructuring-bind (name pin-cnt element-cnt &rest exprs) rest
    (declare (ignore pin-cnt) (ignore element-cnt))
    ;;(format t "PRIMITIVE_DEF ~S...~%" name)
    (let ((data (make-prim-def 
		 :name name
		 :pins  nil
		 :elements nil)))
      (loop for expr in exprs
	 do (parse-xdlrc expr dev data))
      (set name data) ;BIND to name

      

      (export (list name) *package* )
 (print "DONE")
      data)))



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
       (setf (prim-def-pins container)
	     (cons (cons  name dir) (prim-def-pins container) ))))
    
    ((eql (type-of container) 'element )
     (destructuring-bind (name dir) rest
       (setf (element-pins container)
	     (cons (cons name dir) (element-pins container)))))))

;;==============================================================================
;; ELEMENT name pins (...       for PRIMITIVE_DEF
;;  --pin
;;  --conn
;;  --cfg
;;
;;
(defmethod parse-xdlrc-expr ((type (eql 'ELEMENT)) rest dev primdef)
  (destructuring-bind (name pin-cnt &rest exprs) rest
    (declare (ignore pin-cnt))    ;;(format t "ELEMENT ~S...~%" name)
    (let ((data (make-element
		 :name name
		 :pins nil
		 :cfg nil
		 :conns nil)))
      (loop for expr in exprs
	 do (parse-xdlrc expr dev data ))
      (setf (prim-def-elements primdef)
	    (cons data (prim-def-elements primdef)))
      nil)))

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





;;==============================================================================
;; PUBLIC INTERFACE
;;==============================================================================
(defun get-tile-at (x y)
  "get a tile at x y location"
  (aref (dev-tiles *dev*) x y))
;;==============================================================================
(defun get-prim-def (name)
  "get a prim-def by name"
  (gethash name (dev-prim-defs *dev*)))
;;==============================================================================
(defun get-element (name &key of) ;of is prim-def
  (gethash name (prim-def-elements of)))


(defgeneric show (thing))

(defmethod show ((tile TILE))
  (format t "TILE (~d,~d) ~S  type ~S  contains prim-sites: "
	  (tile-x tile) (tile-y tile) (tile-name tile) (tile-type tile))
  (print (keys (tile-prim-sites tile)))
  nil
)

(defmethod show ((pd PRIM-DEF))
  (format t "PRIM-DEF ~S~%. Pins:" (prim-def-name pd))
  (print  (keys (prim-def-pins pd)))
  (format t "~%Elements:")
  (print (keys (prim-def-elements pd)))
  nil)

(defmethod show ((ps PRIM-SITE))
  (format t "PRIM-SITE ~S is a ~S (~S) at tile ~S"
	  (prim-site-name ps)
	  (prim-def-name (prim-site-prim-def ps))
	  (prim-site-bond ps)
	  (tile-name  (prim-site-tile ps)))
   nil)

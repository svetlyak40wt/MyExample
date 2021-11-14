(in-package #:spinneret)

(declaim (stream *html*))

(defparameter *html* (make-synonym-stream '*standard-output*)
  "Output stream for HTML generation.")

(declaim (string *html-lang* *html-charset*))

(defparameter *html-lang* "en")

(defparameter *html-charset* "UTF-8")

(declaim (type (integer -1 #.(1- most-positive-fixnum)) *depth*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defvar *indent*)

(defun get-indent ()
  (or (serapeum:bound-value '*indent*)
      *depth*))

(defvar *pre* nil)

(defparameter *fill-column* 80
  "Column at which to wrap text.
This is always measured from the start of the tag.")

(declaim (boolean *pending-space* *suppress-inserted-spaces*))

(defvar *pending-space* nil)

(defvar *suppress-inserted-spaces* nil
  "When set to non-nil, spaces will never be inserted automatically.")

(defvar *html-path* nil
  "List (in ascending order) of parent nodes.")
(assert (null *html-path*))

(defvar *html-style* :human
  "How should we pretty-print HTML?")
(declaim (type (member :human :tree) *html-style*))

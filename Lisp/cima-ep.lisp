;;; -*- Mode: LISP; Package: USER; Base: 10. -*-
;;;


#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

#+:ALLEGRO (setq excl:*tenured-bytes-limit* 120000000)
;;;
;;;;
(defparameter *ratio* .95);;changed  to median on 7/10/11
(defparameter *ratio-swing* .90);; changed from 1.2 on 10/24/09
(defparameter *posdev* .325)
(defparameter *posdev-swing* .47)
(defparameter *max-day-risk* 3000);;changed from 1350 6/23/08

(defparameter *max-swing-risk* 6000);;changed from 2000 6/23/08
(defparameter *max-position-risk* 9000)
(defparameter *reward-risk-ratio* .6)
(defparameter *reward-risk-ratio-swing* .6)
(defparameter *min-reward-risk-ratio-position* .20)
(defparameter *stop-loss-day* 1.0557) ;for day trades 01/18/14
(defparameter *stop-loss-swing* 1.382) ;for swing trades
(defparameter *stop-loss-position* 1.382) ;for position trades
(defparameter *objective-factor* 2.618);;;objective for day-trades 7/14/11
(defparameter *objective-factor-swing* 4.236);;;objective for swing-trades
(defparameter *objective-factor-position* 6.854);;;objective for position-trades
(defparameter *entry-factor* .764);; for day-trades
(defparameter *entry-factor-swing* .764);;for swing-trades
(defparameter *entry-factor-position* 2.618) ;;for position-trades

(defparameter *out-of-sample* nil)
(defparameter *position-trade-warehouse1* (make-hash-table :test #'equal))
(defparameter *swing-trade-warehouse1* (make-hash-table :test #'equal))
(defparameter *day-trade-warehouse1* (make-hash-table :test #'equal))
(defvar *stocks* nil)
(defparameter *day-commission* 75)
(defparameter *swing-commission* 100)

;;;the config file for the data is in the *database-upper-dir*  directory
;;;the file is          INDX-CFG.LSP

(defparameter *data-read-dir*    nil)


;;;THESE ARE USER OPERATIONAL CHOICES

(defparameter *data-name* 'DJIA)
(defvar *time-interval* nil	"USER INTENDED TIME INTERV BETW PRICE SAMPLES")
(defvar *n-filt* 1)
(defvar *debug* 9)
(defvar *degrees* 5)

;;;DATA PARAMETERS SET BY DATA FILE

(defparameter *trading-day-length* 6.5 "IN HOURS, USED IN PLOT")
(defparameter *num-per-day* 7.0 "HRS/TRADING-DAY FOR PLOTS - MUST BE INTEGER")

;;;INPUT-OUTPUT:DISPLAY
;;;
(defvar *in-stream*             *standard-input*)


;;;GLOBAL EWAVES VARIABLES:
;;;

(defvar *no-prims* nil       "NUMBER OF PRIMITIVES TO PROCESS OR THE END TIME")
(defvar *primitive* nil		"POINTER TO THE PROPERT'S OF A NEW PRIMITIVE")
(defvar *curr-ET* nil		"THE CURRENT TIME")
(defvar *latest-dtime* nil	"LATEST DATA AVAILABLE FOR CURRENT PRIMITIVE")
(defvar *statistics* nil)
(defvar *CAT-LIST*  nil)
(defvar *cat-indices* nil)
(defvar *USER-INTERFACE* T)

(defparameter *auxbuf* (make-array (+ 50000 100) :fill-pointer 0))
(defvar *data-format-32* nil)

(defvar *w* (make-array 24000))		; array for sub-arrays of wave arrays
(defvar *q* (make-array 1000))		; array that contains vectors of q's
(defconstant *wdim* 7)			; no of property slots in the w arrays
(defconstant *qdim* 15)			; no of shared data constants per wave
(defconstant *wdm* (+ *wdim* 2))	; dimension of the w arrays
(defconstant *qdm* (+ *qdim* 3))	; dimension of the q arrays
(defconstant *acc-val-dim* (+ *wdm* *qdim* 2)) ; dim of gen fncts
;(defconstant *w-subdim* 8192)		; dim. of *w* subarrays (slot of *w*)
(defconstant *w-subdim* 128)		; dim. of *w* subarrays (slot of *w*)
(defvar *maxg* (floor 101))		; max gensym number ever used
(defvar *maxw*)				; max no of waves ever attained
(defvar *maxq*)				; max no of q arrays ever attained
(defvar *q-stack* nil)			; list of slots in *q* not in use
(defvar *q-alist* nil)			; ST/ET vs. q-pointer assn list
(defvar *init-state-loaded-p* nil)	; init state & params loaded predicate
(defvar *init-w-arr-made-p* nil)	; setup first *w* sub-array at loading
(defconstant *iniwl*			; initialization of  w  arrays - list
  (append (list 1) (make-list (1- *wdm*) :initial-element nil)))
(defconstant *iniwv* (make-array *wdm* :initial-contents *iniwl*)) ; " - vect
(defconstant *iniql*			; initialization of  q  arrays - list
  (append (make-list *qdim* :initial-element nil) (list 1 nil nil)))
          ; note: last 3 elements of q-vctrs are the counter, the
          ; updated-for-HP .. -p slot and the updated-for-data-p .. slot.
(defconstant *iniqv* (make-array *qdm* :initial-contents *iniql*)) ; " - vect
(unless *init-w-arr-made-p*
    (setf (aref *w* 0) (make-array *w-subdim* :initial-element 0))
    (setq *init-w-arr-made-p* t))
(defvar *cidim* nil) ; set in test-set-up or in read counts
(defvar *waves-stack* nil	"WAVE NAMES THAT HAVE BEEN KILLED")
(defconstant *putvaluef*
  (make-array *acc-val-dim* :initial-contents
	      '(nil nil   pt1 pt1 pt1 pt1 pt11  pt21 pt21  pt2 pt2
		 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3 pt3)))

;;;AUXILLIARY PARAMETERS
(defparameter *base-year* 1900)
;(defparameter
;    *holidays* '(19870907 19871126 19871225 19890102 19890220
;	19890324 19890529 19890704 19890904 19891123 19891225
;			  19900101 19900219 19900413 19900528 19900704
;			  19900903 19901122 19901225 19910101))


;;;KEYS FOR THE ACCESS FUNCTIONS :
;;;
(defconstant PW 2 		"PREVIOUS-WAVE")
(defconstant WW 3 		"WITHIN-WAVE")
(defconstant LB 4 		"LABEL")
(defconstant DG 5 		"DEGREE")
(defconstant SB 6		"SUB-TYPE")
(defconstant C1 7		"PART 1 OF THE CONSISTENCY INDEX")
(defconstant C2 8		"PART 2 OF THE CONSISTENCY INDEX")
;;;COUNT DEPENDENT PROPERTIES
(defconstant NW 9 		"ROOT-LOWER")
(defconstant RL 10 		"NEXT-WAVE")
;;;DEFINE THE INDEX INTO THE SMALL Q ARRAYS
(defconstant ST 11 		"START-TIME")
(defconstant ET 12 		"END-TIME")
(defconstant TL 13		"TIME-LENGTH")
(defconstant SP 14 		"START-PRICE")
(defconstant EP 15 		"END-PRICE")
(defconstant HP 16 		"HIGHEST-PRICE")
(defconstant LP 17 		"LOWEST-PRICE")
(defconstant DR 18 		"DIRECTION")
(defconstant VP 19 		"UP-VOLUME-PEAK")
(defconstant VA 20 		"VOLUME-AVERAGE")
(defconstant AS 21 		"ADVANCE-DECLINE-SLOPE")
(defconstant AP 22 		"ADVANCE-DECLINE-PEAK")
(defconstant VD 23		"DOWN-VOLUME-PEAK")
(defconstant FL 24		"FILTER-SIZE")
(defconstant FT 25		"FIBONACCI TERM")

(defconstant *attr-names* '(PW WW LB DG SB C1 C2
	     NW RL ST ET TL SP EP HP LP DR VP VA AS AP VD FL FT))
;;;
;;;KLUDGE
;;;
(defconstant *pcsy* '%)

;;;******************************************************
;;;                    USEFUL MACROS
;;;******************************************************
;;;
(defmacro insert-nth (el indx lst)
  `(let ((nce (ncons ,el))
	 (lst ,lst))
     (rplacd nce (nthcdr (1+ ,indx) lst))
     (rplacd (nthcdr ,indx lst) nce)))

(defmacro el-index (elmnt lst &key (test '#'eql))
  `(position ,elmnt ,lst :test ,test))

;;;GCLISP EXTENSION MACROS:
;;;
(defmacro ifn (arg eval_nil &optional (eval_t nil))
  `(if ,arg ,eval_t ,eval_nil))

(defmacro array-length (arg)
  `(length ,arg))

(defmacro flatc (arg)
  `(length (format nil "~a" ,arg)))

(defmacro nequal (arg1 arg2)
  `(not (equal ,arg1 ,arg2)))

(defmacro neql (arg1 arg2)
  `(not (eql ,arg1 ,arg2)))

(defmacro neq (arg1 arg2)
  `(not (eq ,arg1 ,arg2)))

(defmacro ncons (arg)
  `(cons ,arg nil))

(defmacro copy-array-contents (arg1 arg2)
  `(replace ,arg2 ,arg1))

(defmacro rplacb (arg1 arg2)
  `(let ((a1 ,arg1)(a2 ,arg2))
     (rplaca a1 (car a2))
     (rplacd a1 (cdr a2))
     a1))

(defun string-append (&rest strings)
  (let ((string1 (car strings)))
    (dolist (stringn (cdr strings))
      (setq string1 (format nil "~a~a" string1 stringn)))
    string1))

(defmacro cd (arg)
  `(chdir ,arg))


#+:ALLEGRO (defmacro quit ()
	     `(exit))

(defmacro putprop (symb val key)
  `(setf (get ,symb ,key) ,val))


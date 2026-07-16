
;;; -*- Mode: LISP; Package: user; Base: 10. -*-

;;;;;;************************************************************************
;;;         GENERAL UTILTIES FOR E-WAVES & SOME LISP UTILITIES
;;;************************************************************************
;;;
;;; LAST CHANGED:
;;;   SUBSTITUTE DELETE-EQUIVALENTS FOR DELETE-DUPLICATES - LMF - 30-AUG-87
;;;
;;;************************************************************************
;;;
;;; THE PROCEDURES DEFINED HEREIN MANIPULATE THE BINDING
;;; OF THE GLOBAL SYMBOL  'TOTAL-COUNTS'  WHOSE STRUCTURE
;;; IS DEFINED AS FOLLOWS:
;;;
;;; TOTAL-COUNTS ==> A LIST OF COUNT NAMES, EACH OF THE FORM #:|ct???|
;;;   #:|ct???|  ==> A LIST OF WAVE NAMES, EACH AN INTEGER
;;;
;;;************************************************************************
;;;
;;; WAVE VALUES ARE ACCESSED WITH THE PROCEDURES: GETV, AND PUTV.  CALLING
;;; SEQUENCE IS AS FOLLOWS:
;;;
;;;  (GETV 'wave-name 'property ct) & (PUTV 'wave-name 'property value ct)
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
;;;************************************************************************
;;; Common Lisp Procedures that Gold Hill Forgot
;;;************************************************************************
;;;
;;; This program is distributed freely and without warranty
;;; to all who wish to use it for any purpose.
;;;
;;; Direct inquiries and comments to:
;;;
;;;              Tom Baker
;;;             LMSC/Austin
;;;
;;;***************************************************************************
;;;
;;; Copy the function definition and related properties of a symbol to another
;;; symbol.  The effect is to define a new function NEW-NAME that is a copy of
;;; function OLD-NAME.
;;;  NEW-NAME must be a symbol
;;;  OLD-NAME must be a symbol having a function definition.
;;;
(DEFMACRO DEFF (NEW-NAME OLD-NAME)
  `(COND ((NOT (SYMBOL-FUNCTION ,OLD-NAME))
          NIL)
         ((NOT (SYMBOLP ',NEW-NAME))
          (ERROR "ARG0 (~A) IS NOT A SYMBOL" ',NEW-NAME)
          NIL)
         (T
            (SETF (SYMBOL-FUNCTION ',NEW-NAME) (SYMBOL-FUNCTION ,OLD-NAME))
            (SETF (SYMBOL-PLIST ',NEW-NAME) (SYMBOL-PLIST ,OLD-NAME))
            ',NEW-NAME)))
;;;
;;; Define a same predicate:
;;; RETURNS T if both arguments have the same logical value.
(DEFUN SAMEP (ARG1 ARG2)
  (COND ((AND ARG1 ARG2) T)
        ((AND (NOT ARG1) (NOT ARG2)) T)
        (T NIL)))

#+gclisp
(DEFUN SORT (L LPRED)
  (COND ((CDR L)
         (DO ((I (1- (LENGTH L)) (1- I))
              (SWITCH NIL))
             ((OR (ZEROP I) SWITCH))
           (SETQ SWITCH T)
           (DO ((lwp L (CDR lwp)))
               ((NULL (CDR lwp)))
               (COND ((FUNCALL LPRED (CADR lwp) (CAR lwp))
                      (RPLACA lwp (PROG1 (CADR lwp)
                                         (RPLACA (CDR lwp) (CAR lwp))))
                      (SETQ SWITCH NIL)))))))
  L)

(defun vsort (lst1 predicate &optional key1)
  (let ((lst lst1) (key (or key1 #'identity)) lngth ptr)
     (setf (fill-pointer *auxbuf*) 0)
     (dotimes (i (length lst))
       (vector-push (pop lst) *auxbuf*))
     (setq lngth (length *auxbuf*))
     (qsort *auxbuf* predicate key)
     (setq ptr lst1)
     (dotimes (i lngth)
       (rplaca ptr (aref *auxbuf* i))
       (setq ptr (cdr ptr)))
     lst1))
;;;;
;;;this sort is for sorting lists of vectors
;;;based on an element of the vectos
(defun elt19 (s)
    (svref s 19))


(defmacro elt1 (seq indx)
  `(aref ,seq ,indx))

(defun qsort (sequence predicate &optional (key #'identity))
  (let (a c)
    (setq c (length sequence)
          a 0)
    (if (not (functionp predicate)) (error "Sort: Predicate Not a Function"))
    (case c
      (0 nil)
      (1 nil)
      (2 (exsort-int sequence a (1- c) predicate key))
      (t (qsort-int sequence a (1- c) predicate key)))))

(defun qsort-int (sequence a c predicate key)
  (let (va1 vb vc b imin imax temp)
    (setq b (truncate (/ (+ a c) 2.0))
          va1 (funcall key (elt1 sequence a))
          vb (funcall key (elt1 sequence b))
          vc (funcall key (elt1 sequence c)))
    (unless (funcall predicate va1 vb)
      (setq temp va1    va1 vb      vb temp)
      (setf temp (elt1 sequence a)
            (elt1 sequence a) (elt1 sequence b)
            (elt1 sequence b) temp))
    (unless (funcall predicate vb vc)
      (setq temp vc    vc vb      vb temp)
      (setf temp (elt1 sequence b)
            (elt1 sequence b) (elt1 sequence c)
            (elt1 sequence c) temp)
      (unless (funcall predicate va1 vb)
        (setq temp vb  vb va1      va1 temp)
        (setf temp (elt1 sequence a)
              (elt1 sequence a) (elt1 sequence b)
              (elt1 sequence b) temp)))
    ;;;    partition sequence
    (setq imax a  imin c)
    (loop
      (loop
        (when (not (funcall predicate (funcall key (elt1 sequence imax)) vb))
          (return nil))
        (incf imax)
        (when (> imax c)
          (setq imax c
                imin (1- c))
          (return nil)))
      (loop ;;;
        (when (funcall predicate (funcall key (elt1 sequence imin)) vb)
          (return nil))
        (decf imin)
        (when (< imin a)
          (setq imin a
                imax (1+ a))
          (return nil)))
      (cond ((< imax imin)  ;;; swap
             (setq temp (elt1 sequence imax))
             (setf (elt1 sequence imax) (elt1 sequence imin)
                   (elt1 sequence imin) temp) )
            (t (return nil))))
    (when (or (= imin c)
              (= imax a))
      (setq imin b
            imax (1+ b)))
    (cond ((< (- imin a) 11)
           (if (> imin a) (exsort-int sequence a imin predicate key)))
          (t
           (if (> imin a) (qsort-int sequence a imin predicate key))))
    (cond  ((< (- c imax) 11)   ;;;; Exchange Sort a small sequence
            (if (> c imax) (exsort-int sequence imax c predicate key)))
           (t
            (if (> c imax) (qsort-int sequence imax c predicate key))))))

(defun exsort-int (sequence a b predicate key)
  (do ((i a (1+ i))
       (si)
       (temp)
       (pos))
      ((> i b))
    (setq si (funcall key (elt1 sequence i)))
    (do ((j (1+ i) (1+ j)))
        ((> j b))
      (if (funcall predicate (funcall key (elt1 sequence j)) si)
          (setq si (funcall key (elt1 sequence j))
                pos j)))
    (when pos
      (setf temp (elt1 sequence pos)
            (elt1 sequence pos) (elt1 sequence i)
            (elt1 sequence i) temp
            pos nil))))



;;;;produces a randomized list of the elements in the argument list
;;;;this is sampling without replacement
(defun randomize-list (trades)
 (let ((dmy-list nil)(leng (length trades)) rdnum)
    
  (dotimes (jth leng)  
         ;  (format T "~%jth = ~A  length = ~A " jth (length trades))
           (setq rdnum (random (length trades)))
         ;  (format T "RDNUM = ~A nth = ~A " rdnum (nth rdnum trades))
           (push  (nth rdnum trades) dmy-list)
           (setq trades (remove (nth rdnum trades) trades :start rdnum :end (1+ rdnum))))
  dmy-list
))


;;;***************************************************************************
;;;***************************************************************************
;;;
;;; Generic print utilities
;;;
;;;***************************************************************************
;;;***************************************************************************
;;;
;;; Output N copies of CHAR to STREAM
;;;
(DEFUN PRINT-CHARS (STREAM N &OPTIONAL (CHAR " "))
  (IF (NOT (NUMBERP N)) (SETQ N 0))
  (DO ((INDEX 0 (1+ INDEX)))
      ((>= INDEX N))
    (FORMAT STREAM "~A" CHAR)))
;;;
;;; Output N copies of CODE to STREAM
;;;
(DEFUN PRINT-CODES (STREAM N &OPTIONAL (CODE (CODE-CHAR 32)))
  (IF (NOT (NUMBERP N)) (SETQ N 0))
  (DO ((INDEX 0 (1+ INDEX)))
      ((>= INDEX N))
    (FORMAT STREAM "~C" CODE)))
;;;
;;; Output STRING in a field of width FIELD with pad character FILL-CHAR
;;; If JUSTIFIED is T, STRING is right-justified in FIELD
;;;
(DEFUN PRINT-STRING (STREAM STRING &OPTIONAL FIELD (FILL-CHAR " ") JUSTIFIED)
  (LET ((LEN (LENGTH STRING)))
    (COND ((AND (NUMBERP FIELD) (> FIELD 0))
           (IF (AND JUSTIFIED (< LEN FIELD))
               (PRINT-CHARS STREAM (- FIELD LEN) FILL-CHAR))
           (DO ((INDEX 0 (1+ INDEX)))
               ((>= INDEX (MIN FIELD LEN)))
             (FORMAT STREAM "~C" (AREF STRING INDEX)))
           (IF (AND (NOT JUSTIFIED) (< LEN FIELD))
               (PRINT-CHARS STREAM (- FIELD LEN) FILL-CHAR)))
          (T (FORMAT STREAM "~A" STRING)))))
;;;
;;; Output OBJECT in a field of width FIELD with pad character FILL-CHAR
;;; If JUSTIFIED is T, STRING is right-justified in FIELD
;;;
#+gclisp
(DEFUN PRINT-OBJECT (STREAM OBJECT &OPTIONAL FIELD (FILL-CHAR " ")
                            JUSTIFIED (SPECIFIER "~A"))
  (IF (AND (NUMBERP FIELD) (> FIELD 0))
      (PRINT-STRING STREAM (FORMAT NIL SPECIFIER OBJECT)
                    FIELD FILL-CHAR JUSTIFIED)
      (FORMAT STREAM SPECIFIER OBJECT)))
#+gclisp
(DEFUN TB-PRINT-OBJECT (STREAM OBJECT &OPTIONAL FIELD (FILL-CHAR " ")
                            JUSTIFIED (SPECIFIER "~A"))
  (IF (AND (NUMBERP FIELD) (> FIELD 0))
      (PRINT-STRING STREAM (FORMAT NIL SPECIFIER OBJECT)
                    FIELD FILL-CHAR JUSTIFIED)
      (FORMAT STREAM SPECIFIER OBJECT)))
#-gclisp
(DEFUN TB-PRINT-OBJECT (STREAM OBJECT &OPTIONAL FIELD (FILL-CHAR " ")
                            JUSTIFIED (SPECIFIER "~s"))
  (IF (AND (NUMBERP FIELD) (> FIELD 0))
      (PRINT-STRING STREAM (FORMAT NIL SPECIFIER OBJECT)
                    FIELD FILL-CHAR JUSTIFIED)
      (FORMAT STREAM SPECIFIER OBJECT)))
;;;
;;; Expotentiation utility
;;;
(DEFUN ^ (X Y)
  (DO ((INDEX 0 (1+ INDEX))
       (RESULT 1))
      ((>= INDEX (ABS Y)) (IF (< Y 0) (/ 1.0 RESULT) RESULT))
    (SETQ RESULT (* RESULT X))))
;;;
;;; Output NUMBER in a field of width FIELD with pad character FILL-CHAR
;;; If JUSTIFIED is T, STRING is right-justified in FIELD
;;; The number of decimal places, PLACES, must be >= 1.
;;;
(DEFUN PRINT-NUMBER (STREAM NUMBER &OPTIONAL FIELD (PLACES 2) (FILL-CHAR " ")
                            JUSTIFIED (RADIX 10.))
  (if (not (numberp number)) (setq number 0.0))
  (SETQ NUMBER (+ NUMBER (/ 5.0 (FLOAT (^ 10 (+ PLACES 1)))))) ;temp (roundoff)
  (LET ((WHOLE (AND (NUMBERP FIELD)
                    (- FIELD (MAX 1 PLACES) (IF (MINUSP NUMBER) 2 1))))
        DIGITS (TEMP (ABS NUMBER)) TEMP2)
    (COND ((AND (NUMBERP WHOLE) (PLUSP WHOLE))
           (SETQ DIGITS (DO ((INDEX 1 (1+ INDEX)))
                            (NIL)
                          (IF (>= TEMP RADIX)
                              (SETQ TEMP (/ TEMP RADIX))
                              (RETURN (VALUES INDEX)))))
           (COND ((<= DIGITS WHOLE)
                  (IF JUSTIFIED
                      (PRINT-CHARS STREAM (- WHOLE DIGITS) FILL-CHAR))
                  (IF (MINUSP NUMBER) (FORMAT STREAM "~C" (code-char 45)))
                  (DO ((INDEX 0 (1+ INDEX)))
                      ((>= INDEX DIGITS))
                    (MULTIPLE-VALUE-SETQ (TEMP2 TEMP) (truncate TEMP))
                    (SETQ TEMP (* RADIX TEMP))
                    (FORMAT STREAM "~A" TEMP2))
                  (FORMAT STREAM "~C" (code-char 46))
                  (DO ((INDEX 0 (1+ INDEX)))
                      ((>= INDEX (MAX 1 PLACES)))
                    (MULTIPLE-VALUE-SETQ (TEMP2 TEMP) (truncate TEMP))
                    (SETQ TEMP (* RADIX TEMP))

;                   (WHEN (= INDEX (1- (MAX 1 PLACES)))
;                     (SETQ TTT (/ TEMP RADIX))
;                     (SETQ TTT (ROUND TTT))
;                     (SETQ TEMP2 (+ TEMP2 TTT)))

                    (FORMAT STREAM "~A" TEMP2))
                  (IF (NOT JUSTIFIED)
                      (PRINT-CHARS STREAM (- WHOLE DIGITS) FILL-CHAR)))
                 (T (PRINT-CHARS STREAM FIELD "*"))))
          (T (FORMAT STREAM "~A" NUMBER)))))


;;;  START




;;;********************************************************************
;;;LISP UTILITIES FOR GCLISP BETA RELEASE 2.2
;;;********************************************************************
;;;
;;;EXCLUSIVE OR
(defun ex-or (x y)
  (or (and x (not y)) (and (not x) y)))
;;;DELETES THE NTH ELEMENT OF A LIST WHERE LIST IS INDEXED 0,1,2,...
;;;(BEWARE: WORKS SAME AS DELETE AS FOR THE FIRST ELEMENT)
(defun delete-nth (ith inlist)
  (delete (nth ith inlist) inlist))
;;;DATE STAMP UTILITY
(defun getdate ()
  (let (d m y ignore)
   ; (declare (ignore ignore))
    (multiple-value-setq
	(ignore ignore ignore d m y) (get-decoded-time))
    (format nil "~:[~;0~]~D-~[JAN~;FEB~;MAR~;APR~;MAY~;JUN~;JUL~;AUG~;SEP~;OCT~;NOV~;DEC~]-~D"
	    (< d 10) d (1- m) (mod y 100))))

;;;SAME AS POSITION FOR GCLISP - MACRO IN CIMA FOR el-index IN LUCID VERS
#+gclisp
(defun el-index (elmnt lst &optional (ttt :test) (eqq 'eql))
  (let ((membp (member elmnt lst ttt eqq)))
    (if membp
        (- (length lst) (length membp)) nil)))
#+gclisp
(defun position (elmnt lst &optional (ttt :test) (eqq 'eql))
  (if (stringp lst) (setq lst (coerce lst 'list)))
  (let ((membp (member elmnt lst ttt eqq)))
    (if membp
        (- (length lst) (length membp)) nil)))
#+gclisp
(defun string-downcase (strg)
  (dotimes (i (length strg))
    (setf (aref strg i) (char-downcase (aref strg i))))
  strg)


;;;POINTER TO THE NEXT-TO-THE-LAST ELEMENT OF LIST
(defun penultimo(lst)
  (do* ((lst lst (cdr lst))
        (lst1 (cdr lst) (cdr lst)))
      ((null (cdr lst1)) (ifn lst1 lst1 lst))))
(defun antepenultimo(lst)
  (do ((lst lst (cdr lst))
       (lst2 (cdddr lst) (cdr lst2)))
      ((null lst2) (ifn (cddr lst) nil lst))))
;;;PAUSES FOR SPECIFIED NUMBER OF SECONDS
(defun pause (delt)
  (multiple-value-bind (secs mins) (get-decoded-time)
    (loop
      (multiple-value-bind (s1 m1) (get-decoded-time)
        (if (> (+ (* 60 (- m1 mins)) (- s1 secs)) delt) (return))))))
;;;INPUT RESULTS OF A PREVIOUS (GET-DECODED-TIME) EXCEPT YEAR AND COMPUTES
;;;ELAPSED TIME IN HOURS MINS AND SECS.
(defun elapsed-decoded-time (secs mins hrs dys mos)
  (multiple-value-bind (s1 m1 h1 d1 mo1)
      (get-decoded-time)
    (if (/= mo1 mos) (setq d1 (+ dys d1)))
    (multiple-value-bind (new-hrs rem-secs)
        (truncate (+ (* 86400 (- d1 dys))(* 3600 (- h1 hrs))
                      (* 60 (- m1 mins))(- s1 secs)) 3600)
      (multiple-value-bind (new-mins new-secs)(truncate rem-secs 60)
        (values new-secs new-mins new-hrs)))))
;;;NUMERICALLY ADDS THE ELEMENTS OF A LIST
(defun list-sum (lst)
  (let ((sum 0))(dolist (el lst) (setq sum (+ sum el))) sum))
;;;INSERT-NTH: MACRO THAT SURGICALLY INSERTS AN ELEMENT INTO A LIST
;;;(insert-nth el indx lst) -> Inserts el after nth position of lst - (in cima)

;;;****************************************************************************
;;;                 MANIPULATION OF TIMES UTILITIES FOR E-WAVES
;;;****************************************************************************
;;;
;;;****************************************************************************
;;;E-WAVES YEAR MONTH TIME DATE ACCESS AND CONVERSION FUNCTIONS
;;;  NOTE: THESE ARE TIED TO THE WAY TIME IS REPRESENTED IN THE WAVES PROGRAM:
;;;        IN THE FORMAT "YYMMDDHHMM" OR "YYMMDDAM" OR "YYMMDD"
;;;
;;;GIVEN A ELLIOTT TIME IN ONE OF VARIOUS FORMATS RETURN A DATE:
;;; NUMDATE OR GETNUMDATE -> THE NUMERIC DATE IN YYMMDD OR YYYYMMDD FORMAT
;;;                          DEPENDING ON THE INPUT,
;;; A YEAR A MONTH A DAY OR AN HOUR IN NUMERIC FORMAT (HOUR CAN BE A SYMBOL).
;;;
;;;
;;;
;;;FUNCTIONS THAT RETURN STRING DATES, YEARS, MONTHS, DAYS, HOURS.
;;;
;;;BASIC FUNCTIONS: (STRING) TIME-> (STRING) DATE,YEAR,LONG-YEAR,MONTH,DAY,HOUR
(defun getstrdate-from-string (str-time)
  (ifn str-time nil                                  ; "YYYYMMDDHHMM"
    (ifn (stringp  str-time) nil                     ;          -> "YYYYMMDD"
      (case (length str-time)                        ; "YYMMDDHHMM"
        ((12 9 8) (subseq str-time 0 8))             ;          -> "YYMMDD"
        ((10 7 6) (subseq str-time 0 6))))))
(defun getstryear-from-string (str-time)
  (let ((strdate (getstrdate-from-string str-time))) ; "YYYYMMDDHHMM" -> "YYYY"
    (case (length strdate)                           ; "YYMMDDHHMM"   -> "YY"
      (6 (subseq strdate 0 2))                       ;
      (8 (subseq strdate 0 4)))))
(defun getstryear-long-from-string (str-time)
  (let ((stryear (getstryear-from-string str-time))) ; "YYYYMMDDHHMM" -> "YYYY"
    (case (length stryear)                           ; "YYMMDDHHMM"   -> "YYYY"
      (2 (string-append (subseq
                          (format nil "~a" *base-year*) 0 2) stryear))
      (4 stryear))))
(defun getstrmonth-from-string (str-time)
  (let ((strdate (getstrdate-from-string str-time))) ;                -> "MM"
    (case (length strdate)
      (6 (subseq strdate 2 4))
      (8 (subseq strdate 4 6)))))
(defun getstrday-from-string (str-time)
  (let ((strdate (getstrdate-from-string str-time))) ;               -> "DD"
    (case (length strdate)
      (6 (subseq strdate 4 6))
      (8 (subseq strdate 6 8)))))
(defun getstrhour-from-string (str-time)
  (ifn str-time nil                                  ;              -> "HHMM"
    (ifn (stringp str-time) nil                      ; or           -> "A","P"
      (case (length str-time)                        ; or           -> "C"
        (12 (subseq str-time 8 12))
        (11 "C")
        (10 (let ((tem (read-from-string (subseq str-time 6 10))))
              (if (numberp tem) (subseq str-time 6 10)
                (subseq str-time 8 10))))
        (9 (let ((tem (read-from-string (subseq str-time 8))))
             (if (member tem '(A P)) (subseq str-time 8) "C")))
        (8 (let ((tem (read-from-string (subseq str-time 6 8))))
             (if (numberp tem) "C" (subseq str-time 6 8))))
        (7 (subseq str-time 6 7))
        (6 "C")))))

;;;INPUT:DATE (NUM OR STR), RETURN: A STRING TIME (DATE YEAR MONTH DAY OR HOUR)
(defun getstrdate (date)
  (if date (case (flatc date)
             (8 (format nil "~a" date))
             (7 (format nil "0~a" date))
             (6 (format nil "~a" date))
             (5 (format nil "0~a" date))
             (4 (format nil "00~a" date))
             (3 (format nil "000~a" date)))))
(defun getstryear (date)
  (let ((strdate (getstrdate date)))
    (case (length strdate)
      (6 (subseq strdate 0 2))
      (8 (subseq strdate 0 4)))))
(defun getstryear-long (date)
  (let ((year (getstryear date)))
    (case (length year)
      (2 (string-append (subseq (format nil "~a" *base-year*) 0 2) year))
      (t year))))
(defun getstrmonth (date)
  (let ((strdate (getstrdate date)))
    (case (length strdate)
      (6 (subseq strdate 2 4))
      (8 (subseq strdate 4 6)))))
(defun getstrday (date)
  (let ((strdate (getstrdate date)))
    (case (length strdate)
      (6 (subseq strdate 4 6))
      (8 (subseq strdate 6 8)))))
(defun getstrhour (hour)
  (cond ((stringp hour) hour)
        ((numberp hour)
         (case (flatc hour)
           (4 (format nil "~a" hour))
           (3 (format nil "0~a" hour))
           (2 (format nil "00~a" hour))
           (1 (format nil "000~a" hour))))
        ((not hour) "C")
        ((symbolp hour)
         (case hour
           ((A AM) "A")
           ((P PM) "P")
           ((C CLS) "C")))
        (t nil)))

;;;STRING RETURN IN E-WAVES TIME FORMAT
;;;                     (IE: "YYYYMMDD" "YYMMDD" "HHMM" "A" "P" "C" or "")
;;;PURPOSE: TO BUKLD E-WAVES TIMES TOGETHER FROM ITS COMPONENTS
;;;ALSO FOR DISPLAYS OF SOME TYPES
;;;(note: this returns "" for daily time formats sometime - not "C")
;;;(also see getnumhour-display)
;;;
;;;CONVERT A NUM (OR STR) DATE TO A STRING - CAN RETURN DOTS IF ASKED
(defun conv-date-to-string (ndte &optional (dots nil)); ndte -> numdate in
  (or (getstrdate ndte)
      (if dots (case dots (8 "........") (6 "......" ) (t "")))))
(defun conv-time-to-string (tme &optional (tdots nil))
  (let ((hour tme))
    (cond ((stringp hour) hour)
          ((numberp hour)
           (case (flatc hour)
             (4 (format nil "~a" hour))
             (3 (format nil "0~a" hour))
             (2 (format nil "00~a" hour))
             (1 (format nil "000~a" hour))))
          ((not hour)
           (if tdots
               (case tdots (4 "....") (3 "...") (2 "...") (1 ".") (0 ""))
             ""))
          ((symbolp hour)
           (case hour
             ((A AM) "A")
             ((P PM) "P")
             ((C CLS) "C"))))))
;;;CONVERT FROM NUMERIC DATE & HOUR TO *CURR-ET* TYPE OF TIME FORMAT
;;;             (YYMMDD & HHMM ---> "YYMMDDHHMM" OR "YYMMDD HHMM")
;spaced->t for displays: "yymmdd hhmm"  ->nil for: "yymmddhhmm"
; ddots tdots -> if non-nil number of dots string returned when tme->nil
(defun conv-to-string (dte tme &optional (spaced nil) (ddots nil) (tdots nil))
  (string-append (conv-date-to-string dte ddots)
                 (if dte (if spaced " " "") " ")
                 (conv-time-to-string tme tdots)))


;;;FUNCTIONS THAT RETURN NUMERIC DATES YEARS MONTHS DAYS HOURS
;;;
;;; INPUT: STRING-TIME "YYYYMMDDHHMM" -> RETS: NUMERIC YEAR, ETC...
(defmacro getnumdate-from-string (str-time)
  `(read-from-string (getstrdate-from-string ,str-time)))
(defmacro getnumyear-from-string (str-time)
  `(read-from-string (getstryear-from-string ,str-time)))
(defmacro getnumyear-long-from-string (str-time)
  `(read-from-string (getstryear-long-from-string ,str-time)))
(defmacro getnummonth-from-string (str-time)
  `(read-from-string (getstrmonth-from-string ,str-time)))
(defmacro getnumday-from-string (str-time)
  `(read-from-string (getstrday-from-string ,str-time)))
(defmacro getnumhour-from-string (str-time)
  `(read-from-string (getstrhour-from-string ,str-time)))
;;;
;;;INPUT: DATE (STRING OR NUMERIC) (YYYYMMDD OR "YYYYMMDD") -> NUMERIC
;;;
(defmacro numdate (date)
  `(getnumdate ,date))
(defmacro numyear (date)
  `(getnumyear ,date))
(defmacro numyear-long (date)
  `(getnumyear-long ,date))
(defmacro nummonth (date)
  `(getnummonth ,date))
(defmacro numday (date)
  `(getnumday ,date))
;;;INPUT STRING TIME "YYYYMMDDHHMM" -> NUMERIC HOUR (HHMM)
(defmacro numhour (time)
  `(getnumhour ,time))

;;;THESE TAKE AS INPUT A NUMERIC DATE OF TYPE: YYYYMMDD OR YYMMDD
;;;OR A STRING E-WAVES TIME FORMAT OF TYPE: "YYYYMMDDHHMM", ETC.
(defun getnumdate (date)
  (cond ((numberp date) date)
        ((stringp date) (getnumdate-from-string date))))
(defun getnumdate-long (date)
  (if (stringp date) (setq date (getnumdate date)))
  (let ((year (getnumyear date)))
    (case (flatc year)
      ((2 1)(+ (* *base-year* 10000) date))
      (t date))))
(defun getnumdate-short (date)
  (cond ((not date) nil)
        (t (if (stringp date) (setq date (getnumdate date)))
	   (let ((year (getnumyear date)))
             (case (flatc year)
               ((2 1) date)
               (t (if (= (truncate year 100) (truncate *base-year* 100))
                      (mod date 1000000) date)))))))
(defun short-stryear (numyear) ; XXYY->"YY"
  (let ((syr (mod numyear 100)))
    (format nil "~[00~;0~a~;~a~]" (if (zerop syr) 0 (flatc syr)) syr)))

(defun getnumyear (date)
  (cond ((numberp date) (truncate date 10000))
        ;((numberp date) (read-from-string (getstryear date)))
        ((stringp date) (getnumyear-from-string date))))
(defun getnumyear-long (date)
  (cond ((not date) nil)
        ((numberp date) (truncate date 10000))
        (t (let ((year (getnumyear date)))
             (case (flatc year) ((2 1) (+ year *base-year*)) (t year))))))
(defun getnummonth (date)
  (let (year monthday)
 ; (declare (ignore year))
  (cond ((numberp date)
        (multiple-value-setq (year monthday)(truncate date 10000))
        (truncate monthday 100))
       ; ((numberp date) (read-from-string (getstrmonth date)))
        ((stringp date) (getnummonth-from-string date))))
)
(defun getnumday (date)
   (let (yearmonth day)
  ; (declare (ignore yearmonth))
  (cond ((numberp date)
         (multiple-value-setq (yearmonth day) (truncate date 100))
         day)
        ;((numberp date) (read-from-string (getstrday date)))
        ((stringp date) (getnumday-from-string date))))
)

(defun getnumhour (str-time)
  (cond ((not str-time) nil)
        ((stringp str-time) (getnumhour-from-string str-time))
        (t "error in getnumhour parameter: a string is expected")))


;;;THIS IS AN ORPHAN THAT RETURNS TWO VALUES SINCE IT WOULD OVFLW OTHERWISE
;;;
;;;INP: STRING-TIME "YYYYMMDDHHMM" - OUPTUT:->YYYYMMDD AND HHMM
(defun getnumtime (str-time)                            ; date & hour ->
  (values (getnumdate str-time)                         ; as values:
          (getnumhour str-time)))                       ;    YYYYMMDD HHMM


;;;THESE ARE REDUNDANT WITH SOME OF THE ABOVE: NEED COMBINING (WITH MACROS)
;;;DEPENDING ON WHICH ARE MORE EFFICIENT.
;;;
;;;EXTRACT YEAR MO DAY IN YYYY MM DD FORMAT FROM A NUMERIC INPUT ONLY
;;;  INPUT: NUMERIC DATE (YYYYMMDD OR YYMMDD)
;;; OUTPUT: NUMERIC YEAR, MONTH OR DAY (YEAR->YYYY IS RETURNED).
(defun ngetnumyear (numdate)                    ; These take in YYMMDD or
  (case (flatc numdate)                         ; YYYYMMDD (numeric) and
    (8 (truncate numdate 10000))                ; return YYYY (complete date),
    (t (+ *base-year*                           ; MM or DD (numeric).
          (truncate numdate 10000)))))          ;
(defun ngetnummo (numdate)                      ; So far these are used only
  (truncate (mod numdate 10000) 100))           ; in encode-count-pathname
(defun ngetnumday (numdate)                     ; down below & one other.
  (mod numdate 100))                            ;



;;;CONVERT FROM STRING TO NUMERIC TIMES, RETS NIL FOR YYYY0000 dates
;;;  INPUT: STRING TIME "YYYYMMDDHHMM"
;;; OUTPUT: NUMERIC DATE, YEAR, MONTH, DAY, HOUR
(defun getnumdate-or-nil (str-time)               ; Used in fad-early-limit
  (let ((ndate (getnumdate str-time)))            ; where nil is needed if
    (if ndate                                     ; str-time is not supported
      (if (string= (getstrday ndate) "00") nil    ; by the data available.
        (values ndate)))))


;;;EXTRACT THE HOUR FOR DISPLAY FROM THE COMPLETE TIME REPRESENTATION
(defun getnumdate-display (str-time)
  (let ((date (getnumdate str-time)))
    (if date
        (format nil "~a-~a-~a"
                (getnumyear date)(getnummonth date)(getnumday date))
      "--")))
(defun getnumhour-display (str-time)
  (let ((date (getnumdate str-time)))                   ; Note: Need to diff
    (getdisphour (getnumhour str-time) date)))          ; betw no time "--" or
 (defun getdisphour (hour &optional (dailynil-p nil))   ; nil for daily:"CLOSE"
  (cond ((and (not hour)(not dailynil-p)) (format nil "--"))
        ((and (not hour) dailynil-p) "CLOSE")
        ((symbolp hour)
         (case hour (C "CLOSE")(A "AM")(P "PM")(t (format nil "~a" hour))))
        ((numberp hour)
         (let ((str-hour (format nil "~[~s~;0~a~;00~a~;000~a~]"
                                 (- 4 (flatc hour)) hour )))
           (format nil "~[~a:~a0~;~a:~a~]"(1- (length (subseq str-hour 2)))
                   (subseq str-hour 0 2)
                   (subseq str-hour 2))))
        (t (format nil "~a" hour))))


;;;EXTRACT THE NAME OF THE DAY OF THE WEEK (YYMMDD -> "FRIDAY")
(defun dayname (numdate)
  (case (flatc numdate)
    ((6 8) (day-of-week (format nil "~a" numdate)))
    (t nil)))

;;;CONVERTS A NUMERIC EWAVES DATE TO A MONTH/DAY DISPLAY FORMAT
;;;                   YYYYMMDD->JAN DD
(defun display-month-day (numdate)
  (cond ((not numdate) nil)
        (t
         (let ((mo (getnummonth numdate))
               (dy (getnumday numdate)))
           (format nil "~[JAN~;FEB~;MAR~;APR~;MAY~;JUN~;JUL~;AUG~;SEP~;OCT~;NOV~;DEC~] ~:[~;0~]~d"
                   (1- mo) (< dy 10) dy)))))

;;;PARSES A NUMERIC DATE ACCORDING TO THE NEW CONVENTION
;;;AND RETURNS (values YYYY MM & DD)
(defun parse-edate (dat)
  (multiple-value-bind (yy mmdd) (truncate dat 10000)
    (values (case (flatc yy)
              (4 yy)
              (3 (+ yy 1000))
              ((2 1 0) (+ yy *base-year*)))
            (truncate mmdd 100) (mod mmdd 100))))
;;;SEPARATE NUMERIC YY (or YYYY) MM DD -> (NUMERIC) YYMMDD (or YYYYMMDD)
(defun make-edate (yy mm dd)
  (+ (* 10000 yy) (* 100 mm) dd))
;;;PREDICATE TO COMPARE DATES ACCORDING TO THE NEW CONVENTION
(defun edate>= (dat1 dat2)
  (multiple-value-bind (yy1 mm1 dd1) (parse-edate dat1)
    (multiple-value-bind (yy2 mm2 dd2) (parse-edate dat2)
      (cond ((> yy1 yy2))
            ((= yy1 yy2) (cond ((> mm1 mm2))
                               ((= mm1 mm2) (>= dd1 dd2))))))))
(defun edate> (dat1 dat2)
  (multiple-value-bind (yy1 mm1 dd1) (parse-edate dat1)
    (multiple-value-bind (yy2 mm2 dd2) (parse-edate dat2)
      (cond ((> yy1 yy2))
            ((= yy1 yy2) (cond ((> mm1 mm2))
                               ((= mm1 mm2) (> dd1 dd2))))))))
(defun edate= (dat1 dat2)
  (multiple-value-bind (yy1 mm1 dd1) (parse-edate dat1)
    (multiple-value-bind (yy2 mm2 dd2) (parse-edate dat2)
      (and (= yy1 yy2)(= mm1 mm2)(= dd1 dd2)))))
(defun edate< (dat1 dat2)
  (not (edate>= dat1 dat2)))
(defun edate<= (dat1 dat2)
  (not (edate> dat1 dat2)))
;;;PREDICATE TO COMPARE ELLIOTT TIMES IE: YYYYMMDDHHMM & ET AL
(defun etime>= (tim1 tim2)
  (when (and tim1 tim2)
    (let ((date1 (getnumdate tim1))
          (hour1 (getnumhour tim1))
          (date2 (getnumdate tim2))
          (hour2 (getnumhour tim2)))
      (cond ((edate> date1 date2))
            ((edate= date1 date2)
             (cond ((and (numberp hour1) (numberp hour2)) (>= hour1 hour2))
                   ((and (eq hour1 'PM) (eq hour2 'AM)))
                   ((eq hour1 hour2))))))))
(defun etime= (tim1 tim2)
  (when (and tim1 tim2)
    (and (edate= (getnumdate tim1) (getnumdate tim2))
         (eq (getnumhour tim1) (getnumhour tim2)))))
(defun etime> (tim1 tim2)
  (when (and tim1 tim2)
    (and (etime>= tim1 tim2) (not (etime= tim1 tim2)))))
(defun etime< (tim1 tim2)
  (when (and tim1 tim2)
    (not (etime>= tim1 tim2))))
(defun etime<= (tim1 tim2)
  (when (and tim1 tim2)
    (not (etime> tim1 tim2))))
(defun later-datep (tim1 tim2)
  (etime<= tim1 tim2))
(defun ehour= (h1 h2)
  (cond ((and (numberp h1) (numberp h2)) (= h1 h2))
        ((and (member h1 '(A P))(member h2 '(A P))) (eql h1 h2))
        ((and (or (not h1)(eql h1 'C))(or (not h2)(eql h2 'C))))
        (t "ERROR IN COMPARING HOURS")))
(defun ehour> (h1 h2)
  (cond ((and (numberp h1) (numberp h2)) (> h1 h2))
        ((and (member h1 '(A P))(member h2 '(A P))) (eql h1 'P))
        ((and (or (not h1)(eql h1 'C))(or (not h2)(eql h2 'C))) nil)
        (t "ERROR IN COMPARING HOURS")))
(defun ehour< (h1 h2)
  (cond ((and (numberp h1) (numberp h2)) (< h1 h2))
        ((and (member h1 '(A P))(member h2 '(A P))) (eql h1 'A))
        ((and (or (not h1)(eql h1 'C))(or (not h2)(eql h2 'C))) nil)
        (t "ERROR IN COMPARING HOURS")))
(defun ehour<= (h1 h2) (or (ehour< h1 h2) (ehour= h1 h2)))
(defun olddate (numdate)
  (if (numberp numdate)
      (let ((numyear (getnumyear (newdate numdate))))
        (if (and (>= numyear *base-year*) (< numyear (+ *base-year* 100)))
            (mod numdate 1000000)
          numdate))))
(defun newdate (numdate)
  (if (numberp numdate)
      (case (flatc numdate)
        ((6 5 4 3) (+ numdate (* *base-year* 10000)))
        (t numdate))))

(defun end-of-prior-month (date)
 (getd (car (month-days  (+ (* 100 (getnumyear date)) (getnummonth date)))) 'ydate)
)

;;;
;;;****************************************************************************
;;;COUNT FILE NAME DECODER & ENCODER: CALENDAR TIME, HOURS, ETC.
;;;THE INPUT IS *CURR-ET* OR INPUT A TIME AS A STRING IN THE FORMAT:
;;;  YYMMDDHHMM ->NORMAL ; YYMMDDAM OR PM -> TWICE-DAILY ; YYMMDD -> DAILY
;;;  NOTE: WHEN THE CHANGE TO LONGER CURR-ET'S BECOME NECESSARY THEN JUST
;;;        MAKE THE CHANGE INT HTE GETNUMHOUR FUNCTION.
;;;
;;;RETURNS THE NUMERIC DATE YYMMDD
#|
(defun getfnumdate (count-pathname)
  (if count-pathname
      (multiple-value-bind (year month day) (decode-count-date count-pathname)
        (values (+ (* year 10000) (* month 100) day)))))
(defun getfnumhour (count-pathname)
  (if count-pathname (decode-count-hour count-pathname)))
                     ;(let ((hour (decode-count-hour count-pathname)))
                     ;  (cond ((and (symbolp hour) (eq hour 'CLS)) nil)
                     ;        (t hour)))))
(defun getfnumhour-display (count-pathname)
  (let* ((hour (decode-count-hour count-pathname)))
    (getdisphour hour t)))

;;;ENCODE COUNT PATHNAME
(defun encode-count-pathname (curr-et
                               &optional (vers 0) (hour (getnumhour curr-et))
                               (inhibit-vers nil))
  (let ((delt (if *time-interval*
                  (format nil "~a" (or (code-ti *time-interval*) 'N))
                (format nil "N")))
        (avcounts (ifn inhibit-vers (available-counts)))
        (filter (format nil "~[00~a~;0~a~;~a~]"
                        (1- (flatc (hextri *n-filt*))) (hextri *n-filt*)))
        (year (hextri (- (ngetnumyear (getnumdate curr-et)) 900)))
        (month (string-upcase
		(format nil "~x" (ngetnummo (getnumdate curr-et)))))
        (day (hextri1 (ngetnumday (getnumdate curr-et))))
        (hours (if (numberp hour) (hextri1 (truncate hour 100))
                 (case hour
                   ((A AM) (format nil "@"))
                   ((P PM) (format nil "%"))
                   (t (format nil "_")))))
        (mins (if (numberp hour) (hextri1 (floor (/ (mod hour 100) 5)))
                (format nil "_")))
        temp)

    (loop
      (setq temp (string-append delt filter year month day
                                (format nil ".")
                                hours mins (format nil "~a" (hextri1 vers))))
      (if inhibit-vers (return temp))
      (if (and (< vers 35) (member temp avcounts :test 'equal))
        (setq vers (+ vers 1))
        (return temp)))))
(defun decode-count-pathname (count-pathname)
  (let ((delt (get-lex-interval
                (decode-ti (read-from-string (subseq count-pathname 0 1)))))
        (filter (hext-dec (subseq count-pathname 1 4)))
        year month day hour
        (vers (read-from-string (subseq count-pathname 11 12))))
    (multiple-value-setq (year month day) (decode-count-date count-pathname))
    (setq hour (decode-count-hour count-pathname))
    (values delt filter year month day hour vers)))
(defun decode-count-date (count-pathname)
  (let ((year (+ 900 (hext-dec (subseq count-pathname 4 6))))
        (month (hext-dec (subseq count-pathname 6 7)))
        (day (hext-dec (subseq count-pathname 7 8))))
    (values year month day)))
(defun decode-count-hour (count-pathname)
  (case (read-from-string (subseq count-pathname 9 10))
    (@ 'A)
    ((% $) 'P)
    (_ 'C)
    (t (+ (* (hext-dec (subseq count-pathname 9 10)) 100)
          (* (hext-dec (subseq count-pathname 10 11)) 5)))))
|#
;;;CONVERT DECIMAL NUMBER TO HEXTRIGECIMAL STRING
(defun hextri (num)
  (multiple-value-bind (hi lo) (truncate num 36)
    (cond ((> hi 35) (string-append (hextri hi) (hextri1 lo)))
          (t (string-append (hextri1 hi) (hextri1 lo))))))
(defun hextri1 (num)
  (let ((rem (mod num 100)))
    (if (< rem 10) (format nil "~a" rem)
      (format nil "~c" (code-char (+ (char-code #\A) (- rem 10)))))))
;;;CONVERT HEXTRIGECIMAL STRING TO DECIMAL NUMBER
(defun hext-dec (strg-num)
  (let ((sum 0) (len (length strg-num)))
    (dotimes (i len)
      (setq sum (+ sum (* (hextdec1 (aref strg-num (- len (1+ i))))
                          (^ 36 i))))) sum))
#-gclisp
(defun hextdec1 (ascii-char)
  (let ((num (read-from-string (format nil "~c" ascii-char))))
    (cond ((numberp num) num)
          (t (+ 10 (- (char-code ascii-char) (char-code #\A)))))))
#+gclisp
(defun hextdec1 (ascii-num)
  (let ((num (read-from-string (format nil "~c" (code-char ascii-num)))))
    (cond ((numberp num) num)
          (t (+ 10 (- (char-code ascii-num) (char-code #\A)))))))
#|
(defun available-counts-list (directry)
  (let ((avctsfils (available-counts directry)) avcts)
    (setq avcts (mapcar #'(lambda (s) (list (getfnumdate s)
					    (getfnumhour s)
					    ;(decodecount-display s directry)
					    s)) avctsfils))
    (setq avcts (sort avcts #'s-pred)) ; note: sort screws up avcts
    (setq avcts (mapcar #'caddr avcts))
   ;;Strips the string legends and returns the counts only
   ;(setq avcts (mapcar #'(lambda (s) (cadddr s)) avcts))
    avcts))

(defun s-pred (ct1 ct2)
  (not (cond ((< (car ct1) (car ct2)))
	     ((= (car ct1) (car ct2))
	      (case (cadr ct1)
		(P nil)
		(A (if (eql (cadr ct2) 'AM) t nil))
		(C t)
		(t (if (numberp (cadr ct2))
		       (<= (cadr ct1) (cadr ct2))
		     (if (eql (cadr ct2) 'C) nil t))))))))

;;;RETURN A LIST OF AVAILABLE COUNTS IN *counts-save-dir* (string pathnames)
(defun available-counts (&optional (directry nil))
  (let ((avcnts (available-counts1 directry))
	(new-avcnts nil))
    (dolist (cnt avcnts)
      (if (> (length cnt) 10) (push cnt new-avcnts)))
    (nreverse new-avcnts)))
(defun available-counts1 (&optional directry)
  (mapcar #'(lambda (s)
              (string-append (pathname-name s) "." (pathname-type s)))

          (ifn directry (directory (string-append *counts-save-dir* "*.*"))
            (directory (string-append directry "*.*")))
          ))
|#
(defun string-del-blnks (string1)
  (let (k)
    (dotimes (i (length string1) (subseq string1 k i))
      (cond ((and (not k) (neql (aref string1 i) #\ )) (setq k i))
            ((and k (eql (aref string1 i) #\ ))
             (return (subseq string1 k i)))))))


(defun time-units-into-day (day tim)
  (1+ (position tim (getd day 'ptime))))
(defun time-units-into-day-ti (day tim)
  (let (timelist ignore)
  ;  (declare (ignore ignore))
    (multiple-value-setq (ignore ignore ignore timelist)
      (data-access day))
    (1+ (position tim timelist))))

;;;ETIME COMPARISSON FUNCTION - *time-interval* SENSITIVE
;;;NO PROVISION FOR TESTING FOR EXISTANCE OF THE DATA YET
;(defun f-etime<= (etim1 etim2)
;  (if (and (stringp etim1) (stringp etim2))
;      (let* ((edate1 (getnumdate etim1))
;	     (edate2 (getnumdate etim2))
;	     (ehour1 (getnumhour etim1))
;	     (ehour2 (getnumhour etim2))
;	     (ehour1-indx (time-units-into-day-ti edate1 ehour1))
;	     (ehour2-indx (time-units-into-day-ti edate2 ehour2)))
;	(if (and ehour1-indx ehour2-indx)
;	    (and (edate<= edate1 edate2) (<= ehour1-indx ehour2-indx))
;	  (etime<= etim1 etim2)))))

(defun f-etime<= (etim1 etim2) (or (equal etim1 etim2) (f-etime< etim1 etim2)))
(defun f-etime= (etim1 etim2) (equal etim1 etim2))
(defun f-etime>= (etim1 etim2) (not (f-etime< etim1 etim2)))
(defun f-etime< (etim1 etim2)
  (if (and (stringp etim1) (stringp etim2))
      (let* ((edate1 (read-from-string (subseq etim1 0 8)))
	     (edate2 (read-from-string (subseq etim2 0 8))))
	(cond ((< edate1 edate2))
	      ((= edate1 edate2)
	       (let* ((ehour1 (getnumhour etim1))
		      (ehour2 (getnumhour etim2))
		      ehour1-indx ehour2-indx timelist ignore)
	;	 (declare (ignore ignore))
		 (multiple-value-setq (ignore ignore ignore timelist)
		   (data-access edate1))
		 (setq ehour1-indx (position ehour1 timelist)
		       ehour2-indx (position ehour2 timelist))
		 (if (and ehour1-indx ehour2-indx)
		     (< ehour1-indx ehour2-indx)
		   (etime< etim1 etim2))))
	      (t nil)))))

;;;**********************************************************************
;;;;Gregorian calendar function:  Dave Register 7/4/88
;;;RETURNS THE DAY OF THE WEEK FOR ANY DAY AFTER 1582
;;;reference PC Magazine OCT 14,1986 p. 306-310
;;;january 1, 1583 was a SATURDAY the start of the Gregorian calendar
;;;365 days per year implies the day of the week normally advances by
;;; 1 per year.
;;;On leap years the day of the  week advances by two
;;;all centennial years are not leap years unless evenly divisible by 400
;;;
;;;expects the date to be a string in "YYMMDD" format
;;;or "YYYYMMDD" format if different century than "19XX".
;;;
(defun day-of-week (date)
  (let (year month  first-day-of-year (days 0)
        (weekdays '((0 . "SUNDAY") (1 . "MONDAY") (2 . "TUESDAY")
                   (3 . "WEDNESDAY") (4 . "THURSDAY") (5 . "FRIDAY")
                                   (6 . "SATURDAY")))
        (num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                       (4 . 30) (5 . 31) (6 . 30)
                                       (7 . 31) (8 . 31) (9 . 30)
                                       (10 . 31) (11 . 30) (12 . 31))))


    ;;;first get the day of the week for january 1 of the Year
    ;;;add  the number of years since 1583 to the weekday of 1/1/1583
    ;;;
    (setq year (numyear-long date))
        ;       (cond ((eql (length date) 6)
        ;             (+ (read-from-string (subseq date 0 2))
        ;                1900))
        ;            ((eql (length date) 8)
        ;             (+ (read-from-string (subseq date 0 4))))
        ;            (t (format t "~%TRY AGAIN with the date as \"YYYYMMDD\".")
        ;               )))
    (when year
      (setq first-day-of-year (mod (+ (- year 1583)  6
                                     (truncate (+ 2 (- year 1583)) 4);leap
                                     ;years
                                     (- (truncate (- year 1600) 100))
                                     (truncate (- year 1600) 400)) 7))
      ;;;next calculate the number of days since the start of the year
      ;;;number of days in completed months
      (setq month (nummonth date))
                ;    (cond ((eql (length date) 6)
                ;          (read-from-string (subseq date 2 4)))
                ;         ((eql (length date) 8)
                ;          (read-from-string (subseq date 4 6)))))
      (dotimes (ith month)
        (setq days  (+ (cdr (assoc ith num-days-per-month)) days)))
      ;;;on leap year add 1 day
      ;;;with adjustments for centennial years
      (if (and (> month 2)
               (eql (mod year 4) 0)
               (or (not (eql (mod year 100) 0)) (eql (mod year 400) 0)))
          (incf days))
      ;;;add the days in the current month
      (setq days (+ days (numday date) -1))
      ;;;calculate the day of the week
      (cdr (assoc (mod (+ first-day-of-year days) 7) weekdays))
      )))

#|
(defun num-day-of-week (date)
  (let ((weekdays '((0 . "SUNDAY") (1 . "MONDAY") (2 . "TUESDAY")
                    (3 . "WEDNESDAY") (4 . "THURSDAY") (5 . "FRIDAY")
                                    (6 . "SATURDAY"))))

    (car (rassoc (day-of-week date) weekdays :test 'equal))
      ))
|#

;;;period may be month or week
(defun num-day-of-week (tdate )
 (let ((date tdate) (ctr 1) date-1)
  
   (loop 
     (setq date-1 (getd date 'ydate))
     (if (> (subtract-dates date-1 date) 2) (return ctr) (incf ctr))
     (setq date date-1))
  ))

;;;;returns a list of dates
(defun calendar-dates-in-month (date)
 (let (dates (yr (numyear date))(mon (nummonth date))
       num-days-per-month)
        (setq num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                    (4 . 30) (5 . 31) (6 . 30)
                                    (7 . 31) (8 . 31) (9 . 30)
                                   (10 . 31) (11 . 30) (12 . 31)))

       (if (and (leap-yearp (numyear-long date))
                (= (nummonth date) 2))
                (setf (cdr (assoc 2 num-days-per-month)) 29))
  
       (if (and (not (leap-yearp (numyear-long date)))
                (= (nummonth date) 2))
                (setf (cdr (assoc 2 num-days-per-month)) 28))
        
         (dotimes (ith (cdr (assoc (nummonth date) num-days-per-month)))
            (push  (make-edate yr mon (1+ ith)) dates))
       
    dates
))

;;;;function to add and subtract days from a date
;;;;returns the new date as "YYMMDD"
;;;;the input date may be either "YYYYMMDD" or "YYMMDD"
;;;;delta-days may be positive or negative
(defun add-days-to-date2 (date delta-days)
  (getnumdate-long
    (read-from-string (add-days-to-date (getstrdate date) delta-days))))
(defun add-days-to-date1 (date delta-days)
  (read-from-string (add-days-to-date date delta-days)))

(defun add-days-to-date (date delta-days)
  (let ((num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                       (4 . 30) (5 . 31) (6 . 30)
                                       (7 . 31) (8 . 31) (9 . 30)
                                       (10 . 31) (11 . 30) (12 . 31)))
        year month (days 0))

    (setq year (numyear-long date))
;    (setq year (cond ((eql (length date) 6)
;                     (+ (read-from-string (subseq date 0 2))
;                        1900))
;                    ((eql (length date) 8)
;                     (+ (read-from-string (subseq date 0 4))))
;                    (t (format t "~%TRY AGAIN with the date as \"YYYYMMDD\".")
;                       )))

      ;;;first calculate the number of days since the start of the year
      ;;;number of days in completed months
    (setq month (nummonth date))
;    (setq month (cond ((eql (length date) 6)
;                      (read-from-string (subseq date 2 4)))
;                     ((eql (length date) 8)
;                      (read-from-string (subseq date 4 6)))))
    (dotimes (ith month)
      (setq days  (+ (cdr (assoc ith num-days-per-month)) days)))
      ;;;on leap year add 1 day
      ;;;with adjustments for centennial years
    (if (and (> month 2)
             (leap-yearp year))
        (incf days))
      ;;;add the days in the current month
    (setq days (+ days (numday date)
                ;  (cond ((eql (length date) 6)
                ;        (read-from-string (subseq date 4 6)))
                ;       ((eql (length date) 8)
                ;        (read-from-string (subseq date 6 8))))
                  ))

;;;add the change in days and adjust the year
;;;when the year boundary is crossed.
    (if (plusp delta-days)
        (progn
          (setq days (+ days delta-days))
          (loop
            (if (leap-yearp year)
                (cond ((> days 366) (setq days (- days 366)) (incf year))
                      (t (return)))
              (cond ((> days 365) (setq days (- days 365)) (incf year))
                    (t (return))))))
      (progn
        (setq days (+ days delta-days))
        (loop
          (if (leap-yearp (1- year))
              (cond ((not (plusp days)) (setq days (+ days 366)) (decf year))
                    (t (return)))
            (cond ((not (plusp days)) (setq days (+ days 365)) (decf year))
                    (t (return)))))))
;;;convert back to YYMMDD format
    (dotimes (ith 13)
      (setq month  ith)
      (setq days  (- days (cdr (assoc ith num-days-per-month))))
      ;;;on leap year take away 1 day
      ;;;with adjustments for centennial years
    (if (and (= month 2)
             (leap-yearp year))
        (decf days))
    (when (not (plusp days));if negative or 0 gone too far add back
      (setq days  (+ days (cdr (assoc ith num-days-per-month))))
      (if (and (= month 2)
               (leap-yearp year))
          (incf days))
          (return)))
      ;;;make the string
    (setq year (format nil "~S" year))
    (setq month (format nil "~S" month))
    (setq days (format nil "~S" days))
    (if (eql (flatc month) 1)
        (setq month (string-append "0" month)))
    (if (eql (flatc days) 1)
        (setq days (string-append "0" days)))
    (string-append
      year
;      (case (flatc year)
;       (4 (subseq year 2 4))
;       (2 year))
      month days)))


;;;Year is a string or a number , either 2 or 4 digits long
;;;if two digits assume 19 for first two
(defun leap-yearp (year &aux yr)
  (if (stringp year)
      (setq yr (read-from-string year))
    (setq yr year))
  (cond ((eql (flatc year) 4)
         (and (eql (mod yr 4) 0)
              (or (not (eql (mod  yr 100) 0))
                  (eql (mod yr 400) 0))))
        ((eql (flatc year) 2)
	 (format *debug-io* "Error: Internally year should have been with 4 digits")
         (setq yr (+ yr 1900))
         (and (eql (mod  yr 4) 0)
              (or (not (eql (mod  yr 100) 0))
                  (eql (mod yr 400) 0))))
        (t (format *debug-io* "~%TRY AGAIN with the year as \"YYYY\"."))
        ))

;;;;;THIS FUNCTION CALCULATES THE CALENDAR TIME INTERVAL BETWEEN TWO DATES
;;;IF DATE2 IS LATER THAN DATE1 THE RESULT IS POSITIVE
;;;IF DATE1 IS LATER THAN DATE2 THE RESULT IS NEGATIVE
(defun subtract-dates (date1 date2)
       (let ((num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                       (4 . 30) (5 . 31) (6 . 30)
                                       (7 . 31) (8 . 31) (9 . 30)
                                       (10 . 31) (11 . 30) (12 . 31)))
        yearE yearL month date (daysE 0) (daysL 0))
    (if (stringp date1) (setq date1 (read-from-string date1)))
    (if (stringp date2) (setq date2 (read-from-string date2)))
;;;find the earliest of date1 and date2
       (setq date (if (< date1 date2) date1 date2))
;;;GET the year
       (setq yearE (cond ((eql (flatc date) 6)
                          (+ (truncate date 10000) 1900))
                         ((eql (flatc date) 8)
                          (truncate date 10000))
                         (t (format
                              t "~%TRY AGAIN with the date as \"YYYYMMDD\".")
                            )))

;;;first calculate the number of daysE since the start of the year
;;;for the earliest date (date1 or date2)
;;;number of daysE in completed months
    (setq month (mod (truncate date 100) 100))
    (dotimes (ith month)
      (setq daysE  (+ (cdr (assoc ith num-days-per-month)) daysE)))
      ;;;on leap year add 1 day
      ;;;with adjustments for centennial years
    (if (and (> month 2)
             (leap-yearp yearE))
        (incf daysE))
      ;;;add the days in the current month
    (setq daysE (+ daysE (mod date 100)))


;;;NOW find the number of days from the beginning of the year
;;;for the later date
       (setq date (if (< date1 date2) date2 date1))
       (setq yearL (cond ((eql (flatc date) 6)
                          (+ (truncate date 10000) 1900))
                         ((eql (flatc date) 8)
                          (truncate date 10000))
                     (t (format t "~%TRY AGAIN with the date as \"YYYYMMDD\".")
                        )))

;;;first calculate the number of daysL since the start of the year
;;;for the earliest date (date1 or date2)
      ;;;number of daysL in completed months
    (setq month (mod (truncate date 100) 100))
    (dotimes (ith month)
      (setq daysL  (+ (cdr (assoc ith num-days-per-month)) daysL)))
;;on leap year add 1 day
;;;with adjustments for centennial years
    (if (and (> month 2)
             (leap-yearp yearL))
        (incf daysL))
;;;add the days in the current month
    (setq daysL (+ daysL (mod date 100)))
;;;add number of days for year differences
    (dotimes (ith (- yearL yearE))
      (if (leap-yearp (+ ith yearE))
          (setq daysL (+ 366 daysL))
        (setq daysL (+ 365 daysL))))

;;;calculate the difference
    (if (< date1 date2 ) (- daysL daysE) (- daysE daysL))))

;;;**************************************
;;;MARKET DAY FUNCTIONS - ADDED 28-DEC-89
;;;
;;;subtract market dates
;;;returns the number of market days between two dates
(defun sub-mkt-dates (startday endday)
  (block DAYS
   (if (not startday)(return-from DAYS nil))
   (if (>= startday endday) (return-from DAYS 0))
   (if (= (- endday startday) 1) (return-from DAYS 1))
  (let ((nextday (getd startday 'ndate)) (cntr 0))
    (loop
      (incf cntr)
      (if (stringp nextday) (return "ERROR! COULD NOT GET TO ENDDAY"))
      (if (and nextday (>= nextday endday)) (return cntr))
      (if (stringp (getd nextday 'ndate))(format T "problem date= ~A~%" nextday))
      (setq nextday (getd nextday 'ndate))
      
      ))));;;closes the loop let block and defun
      
;;;returns the market date of the market day "days" from the startdate
;;;days may be positive or negative.
(defun add-mkt-days (startday days)
  (block nil
    (if (stringp (data-access startday))
	(return "START DAY HAS NO DATA"))
    (let ((nextday startday) dmy)
      (if (plusp days)
	  (dotimes (ith days nextday)
	    (setq nextday
		  (cond ((not nextday) (return nextday))
                        ((numberp (setq dmy (getd nextday 'ndate)))
		           dmy)
			((stringp dmy)
			 (next-mkt-day nextday)))))
;             (return "ERROR! COULD NOT GO ALL THE DAYS")))
	(dotimes (ith (- days) nextday)
	  (setq nextday (getd nextday 'ydate))
	  (if (stringp nextday)
	      (return "ERROR! COULD NOT GO ALL THE DAYS")))))))
;;;;this is necessary for the pinpoint procedure
;;;so it does not go to the disk drive for days where data doesn't
;;;exist
(defun add-mkt-days1 (startday days)
  (let ((nextday startday))
    (if (plusp days)
        (dotimes (ith days nextday)
          (setq nextday (next-mkt-day nextday)))
      (dotimes (ith (- days) nextday)
        (setq nextday (getd nextday 'ydate))
        (if (stringp nextday)
            (return "ERROR! COULD NOT GO ALL THE DAYS"))))))

(defun my-add-mkt-days1 (startday days)
  (let ((nextday startday))
    (dotimes (ith days nextday)
      (setq nextday (next-mkt-day nextday)))))



(defun week-day-p (day)
  (let ((weekdays '("MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY")))
  (if (member (day-of-week day) weekdays :test #'equal)
      t nil)))


;;;given two consecutive market days that are weekdays
;;; how many weekdays inbetween? The second date is more recent than the first      
(defun num-holidays (second-date)
 (block nil
  (let* ((first-date (getd second-date 'ydate))
         first-dofw second-dofw) 
   (if (stringp first-date) (return 0))
   (if (= (subtract-dates first-date second-date) 1)  (return 0))
   (setq first-dofw (day-of-week first-date)
        second-dofw (day-of-week second-date))
 
   (cond ((and (equal second-dofw "MONDAY")
        
      (equal first-dofw "THURSDAY")) 1)
        
((and (equal second-dofw "TUESDAY")
        
      (equal first-dofw "FRIDAY")) 1)

        ((and (equal second-dofw "WEDNESDAY")
        
      (equal first-dofw "MONDAY")) 1)
        
((and (equal second-dofw "THURSDAY")
        
      (equal first-dofw "TUESDAY")) 1)
        
((and (equal second-dofw "FRIDAY")
        
      (equal first-dofw "WEDNESDAY")) 1)
        
      
        
((and (equal second-dofw "MONDAY")
        
      (equal first-dofw "WEDNESDAY")) 2)
        
((and (equal second-dofw "TUESDAY")
        
      (equal first-dofw "THURSDAY")) 2)

       
((and (equal second-dofw "WEDNESDAY")
        
      (equal first-dofw "FRIDAY")) 2)
        
((and (equal second-dofw "THURSDAY")
               (equal first-dofw "MONDAY")) 2)
        
((and (equal second-dofw "FRIDAY")
        
      (equal first-dofw "TUESDAY")) 2)    
        
(t 0)))) )     
;;;Take holidays into account

(defun next-mkt-day (mkt-day &aux tnxt-day)
  (setq tnxt-day
        (cond ((equal (day-of-week (format nil "~A" mkt-day)) "FRIDAY")
               (add-days-to-date1 mkt-day 3))
              ((week-day-p mkt-day)
               (add-days-to-date1 mkt-day 1))
              ((equal (day-of-week (format nil "~A" mkt-day)) "SATURDAY")
               nil)                        
   ))

  (cond ((mkt-holiday-p tnxt-day)
         (next-mkt-day tnxt-day))
        (t tnxt-day)))
;;;Ignore holidays
(defun next-mkt-day2 (mkt-day)
  (cond ((equal (day-of-week (format nil "~A" mkt-day)) "FRIDAY")
	 (add-days-to-date1 mkt-day 3))
	((week-day-p mkt-day)
	 (add-days-to-date1 mkt-day 1))))
(defun prior-mkt-day2 (mkt-day)
  (cond ((equal (day-of-week (format nil "~A" mkt-day)) "MONDAY")
	 (add-days-to-date1 mkt-day -3))
	((week-day-p mkt-day)
	 (add-days-to-date1 mkt-day -1))))


;;;holidays are only for US markets
;(defun mkt-holiday-p (day)
;  (let ((holiday-list (ifn (member *data-name* '(L-GOLD L-SILVER))
;                           *holidays*)))
;  (member (getnumdate-long day) holiday-list)))
  
(defun mkt-holiday-p (day)
 (block nil
  (if (not day)(return nil))
  (if (readt2 day) (return nil));;;if data then not a market holiday
  (if (week-day-p day)(return t));;;if not data and weekday then market holiday
   
))  
;;;does not check if data is available on both times
(defun sub-mkt-times (time1 time2)
  (block nil
  (let ((nextday (getnumdate time1)) (nexttime (getnumhour time1))
	(endday (getnumdate time2))(endtime (getnumhour time2))
        nextprice)
    (ifn (and time1 time2) (return nil))
    (ifn (f-etime<= time1 time2)
	(return (- (sub-mkt-times time2 time1))))
    (do ((cntr 0 (1+ cntr)))
	((and (= endday nextday) (or (not endtime) (eql endtime nexttime)))
	      cntr)
      (multiple-value-setq (nextday nexttime nextprice)
	(next-data-point nextday nexttime nextprice)))
      )))

;;;
;;; ADDITION LMF 07-DEC-89
(defun get-default-mdate (date addend)
  (let ((ndate (add-days-to-date1 date addend)))
    (dotimes (numb 3)
      (if (member (day-of-week (format nil "~a" ndate))
                  (list "SATURDAY" "SUNDAY") :test 'equal)
          (setq ndate (add-days-to-date1 ndate addend))
        (return)))
    ndate))

;;;ADDED ON 07-JUL-92 FROM DIO - USED IN DISPLAY OF HOURS IN DATA REVIEW (MEN1)
;;; (APPARENTLY NOT USED IN SUN)
;;;
(defun add-minutes-to-time (tim delt &optional (24p nil))

  ;;; tim as 1030 delt as 30 fres as 1100 sres as "11:00"

 (let (fterm sterm fres sres temp)
   (multiple-value-setq (fterm sterm)(truncate tim 100))
   (setq fres (+ (* fterm 60) sterm delt))
  (multiple-value-setq (temp sres) (truncate fres 60))
   (setq fres (+ (* temp 100) sres))
;   (if (> temp 12)(setq temp (- temp 12)))
   (cond (24p
	   (setq temp (mod temp 24))(if (zerop temp) (setq temp 24)))
	 (t
	  (setq temp (mod temp 12))(if (zerop temp) (setq temp 12))))

   (setq fres (mod fres 2400))(if (zerop fres) (setq fres 2400))
   (cond ((and (< temp 10)(> sres 0))
	   (setq sres (format nil "~A~A~A~A" "0" temp "\:" sres)))
	  ((and (< temp 10)(= sres 0))
	   (setq sres (format nil "~A~A~A~A" "0" temp "\:" "00")))
	  ((= sres 0)
	   (setq sres (format nil "~A~A~A"       temp "\:" "00")))
	  (T
	   (setq sres (format nil "~A~A~A"       temp "\:" sres))))
   (values fres sres)))

(defun first-available-date (market &optional (delta 300))
  (let (fdate)
   (set-market market)
   (cond ((eql market 'dj.d1b)
          (setq fdate (add-mkt-days 19971001 delta)))
         ((eql market 'sp.d1b)
           (setq fdate (add-mkt-days 19820420 delta)))
         ((eql market 'nk.d1b)
           (setq fdate (add-mkt-days 19900925 delta)))
         ((eql market 'cp.d1b)
           (setq fdate (add-mkt-days 19890103 delta)))
         ((eql market 'cc.d1b)
           (setq fdate (add-mkt-days 19950120 delta)))
         ((eql market 'pa.d1b)
           (setq fdate (add-mkt-days 20040924 delta)))
         ((eql market 'pl.d1b)
           (setq fdate (add-mkt-days 20030102 delta)))
         ((member market *forex-list*)
         ; (setq fdate (add-mkt-days 20030207 delta))) 
           (setq fdate (max (if (stringp (add-mkt-days 19900102 delta)) 0 (add-mkt-days 19900102 delta))
                            (add-mkt-days (car (month-days (get-first-index-date))) delta)))) 
         ((eql market 'lc.d1b)
           (setq fdate (add-mkt-days 19890804 delta)))
         (t  (setq fdate (add-mkt-days (car (month-days (get-first-index-date))) delta))))
  (and fdate  (max fdate 19800102))
))
(defun available-days (market tdate &optional (delta 250))
   (sub-mkt-dates (first-available-date market delta) tdate))

(defun total-available-days (tdate markets &optional (delta 300))
   (apply #'+ (mapcar #'(lambda (s) (available-days s tdate delta)) markets)))


(defun corrected-date (tdate)
   (let ((date tdate))
   (dotimes (ith 10)
      (if (numberp (getd date 'close)) (return date))
       (setq date (add-days-to-date1 date -1)))))


;;;;converts from 20161209 to "12/99/2016" format
(defun date-convert (date)
  (let (year day month)
    (setq day (getnumday date) year (getnumyear date)
	  month (getnummonth date))
    (format nil "~A/~A/~A" month day year)))

;;;;converts from 20161209 to "2016.12.09" format
(defun mt-date-convert (date)
  (let (year day month)
    (setq day (getnumday date) year (getnumyear date)
	  month (getnummonth date))
    (format nil "~4A.~2,,,'0@A.~2,,,'0@A" year month day)))


;;; converts a date from the format "MM/DD/YY" to YYYYMMDD

(defun conv-to-ewaves-date (data-str)
  (let (month day year  comma-loc)

    (ifn (stringp data-str) (setq data-str (format nil "~A" data-str)))

    (setq comma-loc (position #\/ data-str))

    (setq month (read-from-string data-str nil nil :start 0 :end comma-loc))

    (setq data-str (subseq data-str (1+ comma-loc)))

    (setq comma-loc (position #\/ data-str))
    (setq day (read-from-string data-str nil nil :start 0 :end comma-loc))

    (setq data-str (subseq data-str (1+ comma-loc)))
    (setq year (read-from-string data-str nil nil :start 0 :end 4))

    (+ (* 10000  year) (* 100 month) day)
    ))




;;;given a market and a date
;;;this function applies the rollover rules and
;;;the knowledge of the contracts traded for each futures market
;;;to find the contract month traded on that date*
;;;changes to the new contract on rollover date at the END of the day
;;;actually not trading the new contract on rollover date.
;;; example return value would be DEC13.

(defun contract-month (market date)
   (let (param1 rollover-dates (rod 0) ncmn yr rule)
   (set-market market)
   (setq param1
    (case market
     ((s.d1b bo.d1b sm.d1b c.d1b w.d1b pl.d1b lc.d1b ho.d1b hu.d1b)(setq rule 1)  9) ;rule1
     ((ct.d1b cc.d1b su.d1b cf.d1b)(setq rule 3)  14) ;rule 3
     ((cl.d1b lh.d1b pb.d1b oj.d1b)(setq rule 4) 11) ;rule 4
     ((dwti.d1b )(setq rule 4) 11) ;rule 4
    
     ((dj.d1b sp.d1b nd.d1b ru.d1b nk.d1b sxf.d1b) (setq rule 5) 2) ;rule 5
     ((dj1.d1b sp1.d1b nd1.d1b) (setq rule 5) 9)
     ((lb.d1b pa.d1b)(setq rule 6)  3) ;rule 6
   
     ((us.d1b ty.d1b gc.d1b si.d1b cp.d1b ddg.d1b dds.d1b)(setq rule 7) 5) ;rule 7
     (us.d1b (setq rule 7) 12)
     ((e1.d1b sf.d1b jy.d1b ad.d1b bp.d1b cd.d1b mx.d1b nz.d1b)(setq rule 8) 6) ;rule 8
    ; ((deu.d1b dbp.d1b dbsx.d1b)(setq rule 8) 7) ;rule 8
   
     (ng.d1b (setq rule 4) 11) ;rule 9 return 7; changed to 4 11
     ((dinr.d1b )(setq rule 10) 4);;;rule 10
     ((deu.d1b dbp.d1b djy.d1b) (setq rule 11) -5) ;;rule 11
     ((dbsx.d1b)(setq rule 12) -1);;; rule 12
     (cl1.d1b (setq rule 4) 18)
     ))
    (ifn param1 (return-from contract-month "CASH"))
 ;   (format t "~% rule = ~A param1 = ~A" rule param1)
    (setq rollover-dates
     (cond ((and (= param1 2)(eql rule 5)) ;;;for stock indexes
            (append (find-rollover-dates2 market date)
                    (find-rollover-dates2 market (+ date 10000))
                    (find-rollover-dates2 market (- date 10000))))

            ((and (= param1 6)(eql rule 8)) ;;for currencies
             (append (find-rollover-dates3 market date param1)
                     (find-rollover-dates3 market (+ date 10000) param1)
                     (find-rollover-dates3 market (- date 10000) param1)))
            ((and (= param1 4)(eql rule 10)) ;;;for Dubai Indian Rupee
             (append (find-rollover-dates4 market date param1)
                     (find-rollover-dates4 market (+ date 10000) param1)
                     (find-rollover-dates4 market (- date 10000) param1)))
            ((and (= param1 -5)(eql rule 11)) ;;;for Dubai Euro currency and Dubai British pound
             (append (find-rollover-dates5 market date param1)
                     (find-rollover-dates5 market (+ date 10000) param1)
                     (find-rollover-dates5 market (- date 10000) param1)))
            ((and (= param1 -1)(eql rule 12)) ;;;for Dubai Bombay Sensex
             (append (find-rollover-dates6 market date param1)
                     (find-rollover-dates6 market (+ date 10000) param1)
                     (find-rollover-dates6 market (- date 10000) param1)))
                      
 ;;;;for param1 days into the previous month from the expiry contract
            (t (append (find-rollover-dates market date param1)
                       (find-rollover-dates market (+ date 10000) param1)
                       (find-rollover-dates market (- date 10000) param1)))
     ))


    (dolist (ith rollover-dates)
      (if (>= date (cdr ith) rod) (setq rod (cdr ith))))
;;;cmn is the expiry month we need the following month
    (setq rollover-dates (vsort rollover-dates #'< #'cdr))
    (setq ncmn
     (dolist (jth rollover-dates)
      (if (< date (cdr jth)) (return (car jth)))))
;    (print date) (print ncmn)(print rollover-dates)
    (setq yr (getnumyear rod))
  ;  (print rod)
    (if (< ncmn (getnummonth rod))(incf yr))

;;;;The Dubai markets name the contract month for gold by the last trading day month 
;;;;not the delivery month 
   (if (eql market 'ddg.d1b) (decf ncmn));;kludge to fix contract month for Dubai gold

    (string-append (car (find-if #'(lambda(s) (= (third s) ncmn)) *month-codes*))
                       (subseq (format nil "~A" yr) 2 4))

;   (values rod mn yr)

) )



(defun rollovers-due (date)
   (let ((nmdate (get-default-mdate date 1)) chgs ccm ncm
          (path-out "~/exitpoints/daily-out/new-rollovers.csv"))
 (dolist (ith  *fore-list* )
   (setq ccm (contract-month ith date) ncm (contract-month ith nmdate))
   (unless (equal ccm ncm) (setq chgs (acons (get-ts-symbol ith) ncm chgs))))
  (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format str "For TOMORROW NIGHT the evening of ~A Rollover to:~%" (date-convert nmdate)) 
    (dolist (kth chgs)
     (format str "~A~A~%"  (car kth)(ts-contract-month (cdr kth)))))
  chgs
  ))

(defun rollovers-next-7days (date)
   (let ((nmdate (get-default-mdate date 7)) chgs ccm ncm
          (path-out "~/exitpoints/daily-out/new-rollovers.csv"))
     (dolist (ith  *aim-list* )
       
   (setq ccm (contract-month ith date) ncm (contract-month ith nmdate))
   (unless (equal ccm ncm) (setq chgs (acons (get-ts-symbol ith) ncm chgs))))
  (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format str "For next week there is a ~A Rollover to:~%" (date-convert nmdate)) 
    (dolist (kth chgs)
      (format str "~A   ~A~A~%" nmdate (car kth)(ts-contract-month (cdr kth)))
      (format T "~A  ~A~A~%"  nmdate (car kth)(ts-contract-month (cdr kth)))
      ))
  chgs
))


  


(defun most-recent-rollover (tdate)
  (let ((date tdate))
  (dotimes (ith 100)
    (if (getd date 'rollover) (return)
    (setq date (getd date 'ydate)))
    )
(values date (getd date 'rollover))
))

(defun rollover-between-dates (date1 date2)
  (let ((date date1)(roll 0) r-list ydate)
   (loop 
      (setq ydate date date (getd date 'ndate))
      (if (or (getd date 'rollover)
              (not (equal (contract-month *data-name* ydate)(contract-month *data-name* date))))
                             
          (setq roll (+ roll (or (getd date 'rollover) 0))
               r-list (acons date (contract-month *data-name* date) r-list)))
      (if (or (eql date date2) (not date)) (return (values roll r-list)))
      )))
;;convert contract-month from "MAY14" format to the Ninja format "05-14".

(defun ninja-contract-month (contract-month)
  (let ((months '((JAN . "01-")(FEB . "02-")(MAR . "03-")(APR . "04-")(MAY . "05-")
		  (JUN . "06-")(JUL . "07-")(AUG . "08-")(SEP . "09-")(OCT . "10-")
		  (NOV . "11-")(DEC . "12-"))))
  (if (equal contract-month "CASH") "CASH"
  (string-append (cdr (assoc
			(read-from-string (subseq contract-month 0 3)) months)) (subseq contract-month 3 5)))

))

(defun ts-contract-month (contract-month)
   (if (equal contract-month "CASH") ""
        (string-append (second (assoc (subseq contract-month 0 3) *month-codes* :test 'equal))
                      (subseq contract-month 3 5))))

(defun mt-contract-month (contract-month)
        
  (if (equal contract-month "CASH") ""
     (string-append  (subseq contract-month 0 3)
                   (if (> (read-from-string (subseq contract-month 3 5)) 50) "19" "20")
                   (subseq contract-month 3 5)))

)



;;;;idate is the initial date of entry and
;;;;cdate is the current date. 
;;;the value returned is the stop price for the next day after the current day
;;;tdir is the trade direction long or short.
(defun parabolic-stop (tdir idate cdate &optional (af .02)(afi .02))
   (let (sip sar date hprice lprice); (ndate (getd cdate 'ndate)))
   ;(setq ndate (or ndate cdate));;;if next day is nil use cdate
   (cond ((eql tdir 'long)
          (setq date idate sip (n-day-low date 10) sar sip hprice (getd date 'high))
          (loop 
              (if (eql cdate date) (return sar))
              (setq date (getd date 'ndate))
              (when (> (getd date 'high) hprice)
                    (if (< af .20)(setq af (+ afi af)))
                    (setq hprice (max (getd date 'high) hprice)))
               (setq sar 
                     (min (getd date 'low)(getd (getd date 'ydate) 'low)
                          (+ sar (* af (- hprice sar )))))
               
              ;  (if (>= date cdate) (return (+ sar (rollover-between-dates idate ndate)))))
               (if (>= date cdate) (return  sar)))      
              )

          ((eql tdir 'short)
           (setq date idate sip (n-day-high date 10) sar sip lprice (getd date 'low))
           (loop 
               (if (eql cdate date) (return sar))
               (setq date (getd date 'ndate))
               (when (< (getd date 'low) lprice)
               (if (< af .20)(setq af (+ afi af)))
               (setq lprice (min (getd date 'low) lprice)))
               (setq sar
                    (max (getd  date 'high)(getd (getd date 'ydate) 'high)
                          (- sar (* af (- sar lprice )))))
             ;  (if (>= date cdate) (return (+ sar (rollover-between-dates idate ndate)))))
               (if (>= date cdate) (return sar)))
           ))
    
))

;;;finds the date of the highest or lowest extreme price 
;;;that came first within a window of n days ending on the current date.
(defun sip-low (tdate size)
  (let ( slow  sdate (date tdate))
      (setq slow (getd tdate 'low) sdate tdate)
      (dotimes (ith size)
          (if (< (getd date 'low) (getd sdate 'low))
             (setq slow (getd date 'low) sdate date))
          (setq date (getd date 'ydate)))
      (values sdate slow) 
))

(defun sip-high (tdate size)
  (let ( shigh  sdate (date tdate))
      (setq shigh (getd tdate 'high) sdate tdate)
      (dotimes (ith size)
          (if (> (getd date 'high) (getd sdate 'high))
             (setq shigh (getd date 'high) sdate date))
          (setq date (getd date 'ydate)))
      (values sdate shigh) 
))
;;;;returns direction as long or short
;;;date of most recent flip
;;;stop loss price (target for counter trend
;;;date of penultimate flip
(defun parabolic-stops (tdate &optional (af .02)(afi .02))
 (ifn (readt2 tdate)(return-from parabolic-stops nil))
  (let (sdate sdir dir date stop stops
        (slow (sip-low tdate 252))(shigh (sip-high tdate 252)))
   
    (setq sdate (add-mkt-days (min shigh slow) 3))
    (if (> sdate tdate) (setq sdate (min shigh slow)))
    (setq  sdir  (if (< shigh slow) 'SHORT 'LONG)
         date sdate dir sdir)
    (setq stops (acons (min shigh slow) dir stops))
 ;  (format T "~%sdate= ~A slow= ~A shigh= ~A sdir= ~A" sdate slow shigh sdir)
   ;;;;;now build an association list of
    (loop 
    
     (if (>= date tdate)(return)) 
     (setq stop (parabolic-stop dir sdate date af afi))
  ;   (format T "~%sdate= ~A date= ~A stop= ~A dir= ~A" sdate date stop dir)
     (setq date (getd date 'ndate))
     (ifn date (return))
     (when
        (if (eql dir 'SHORT) (> (getd date 'high) stop)(< (getd date 'low) stop)) 
        (setq dir (if (eql dir 'SHORT) 'LONG 'SHORT) sdate date)
        (setq stops (acons date dir stops)))
   
      ) ;(setq stops1 stops)
(values (cdar stops)(caar stops)(parabolic-stop dir sdate tdate) (caadr stops) stops)
))

(defun flips-period (tdate days &optional (afi .02)(af .02))
  (let ((date tdate) dir flips) 
  (dotimes (ith days)
    (setq dir (parabolic-stops date af afi))
  ;  (setq dir (primitive-direction date af))
   ;  (setq dir (psycho-direction date af))
   ;  (setq dir (mvhl-trend date af))
  ; (setq dir (channel-direction date 3))
    (ifn (eql dir (car flips)) (push dir flips))
    (setq date (getd date 'ydate)))
(float (/ days (length flips)))
)) 


(defun parabolic-slope-index (tdate)
   (let (dirp psar startdate priorstartdate pdeltat pdeltal deltat deltal s0 s1)
  (multiple-value-setq (dirp startdate psar priorstartdate)(parabolic-stops tdate))
  (ifn priorstartdate  (setq  priorstartdate (nth-value 1 (parabolic-stops (getd tdate 'ydate)))))
   (setq deltat (1+ (sub-mkt-dates startdate tdate)))
   (setq deltal (- (getd tdate 'close)(getd startdate 'open)))

   (setq pdeltat (1+ (sub-mkt-dates priorstartdate startdate)))
   (setq pdeltal (- (getd startdate 'open)(getd priorstartdate 'open)))

   (setq s0 (/ deltal deltat) s1 (/ pdeltal pdeltat))
 ;  (format T "s0 = ~A  s1 = ~A~%" s0 s1)
   (cond ((and (plusp s0)(> (abs s0)(abs s1))) 2)
         ((and (plusp s0) (< s0 (abs s1))) -1)
         ((and (minusp s0)(> (abs s0)(abs s1))) -2)
         ((and (minusp s0)(<  (abs s0) (abs s1))) 1)
         (t 0))
))

(defun parabolic-stops-index (tdate)
  (let ((pbs  (/ (- (getd tdate 'close) (nth-value 2 (parabolic-stops tdate)))
                 (volatility tdate 21 1))))

       (cond ((> pbs 3.3) 5)
             ((> pbs 2.2) 4)
            ((> pbs 1.1) 3)
            ((> pbs .5) 2)
            ((> pbs 0) 1)
            ((> pbs -.5) -1)
            ((> pbs -1.1) -2)
            ((> pbs -2.2) -3)
            ((> pbs -3.3) -4)
            (t -5))

))

;;;;near 0 means far from target and 1 means near target for counter trend
(defun parabolic-range-index (tdate)
  (let (dir sdate stop-loss xprice pbs)
   
   (multiple-value-setq (dir sdate stop-loss)(parabolic-stops tdate))
   (setq xprice
        (if (eql dir 'long) 
             (n-day-high tdate (subtract-dates sdate tdate))
           (n-day-low tdate (subtract-dates sdate tdate))))
  
     (setq pbs  (abs (/ (- (getd tdate 'close) xprice)
                 (- stop-loss xprice))))
     
       (cond ((> pbs .9) 10)
             ((> pbs .8) 9)
            ((> pbs .7) 8)
            ((> pbs .6) 7)
            ((> pbs .5) 6)
            ((> pbs .4) 5)
            ((> pbs .3 ) 4)
            ((> pbs .2) 3)
            ((> pbs .1) 2)
            (t 1))

))



(defun timing-line (date &optional (size1 3)(size2 10))
   (- (ave date size1)(ave date size2)))
;returns positive if timing line is higher
;returns negative if timing line is lower
(defun timing-line-delta (date size1 size2)   
  (- (timing-line date size1 size2)(timing-line (getd date 'ydate) size1 size2)))

;;returns 'DN if positive today and negative yesterday
;;;return 'UP if negative today and positive yesterday
(defun timing-line-signal (date &optional (size1 3) (size2 11)) 
  (let ((tim1 (timing-line date size1 size2)))
          
  (cond ((plusp tim1) 1)
        ((minusp tim1) -1)
        (t 0))
        ))

(defun timing-line-signal1 (date &optional (size1 3) (size2 10)) 
  (let* (;(size2 (period-estimator date))(size1 (truncate size2 2)) 
     ;    (ss (slow-stochastic-index date size2)) 
        (tim1 (timing-line date size1 size2))
        (tim2 (timing-line (getd date 'ydate) size1 size2))
        (tim3 (- tim1 tim2))
         )
  
  (cond ((and (plusp tim1)(minusp tim2))  1)
        ((and (minusp tim1)(plusp tim2))  -1)

        ((and (plusp tim1)(plusp tim3)) 2)
        ((and (minusp tim1)(plusp tim3))  3)  

        ((and (plusp tim1)(minusp tim3)) -2)
        ((and (minusp tim1)(minusp tim3)) -3)

 ;       ((and (minusp tim1)(> tim1 tim2)) 4)
;        ((and (plusp tim1)(< tim1 tim2)) -4)
        (t 0))
    
        ))
;;returns 'DN if positive today and negative yesterday
;;;return 'UP if negative today and positive yesterday
(defun timing-line-signal2 (date &optional (size1 3) (size2 11)) 
  (let ((tim1 (timing-line date size1 size2))
        (tim2 (timing-line (getd date 'ydate) size1 size2)))  
  (cond ((and (plusp tim1)(minusp tim2)) 'DN)
        ((and (minusp tim1)(plusp tim2)) 'UP)
        (t  (timing-line-signal2 (getd date 'ydate) size1 size2)))
        ))


(defun timing-line-signal3 (tdate &optional (num1 5)(num2 10))
  (let (tim1 tim2 tim3 tim4 (ydate (getd tdate 'ydate)))
   (setq tim1 (timing-line tdate num1 num2)
         tim2 (timing-line ydate num1 num2)
         tim3 (timing-line (getd ydate 'ydate) num1 num2)
         tim4 (timing-line (getd (getd ydate 'ydate) 'ydate) num1 num2))
  (cond
        ((and (plusp tim1)(> tim1 tim2)
              (< tim1 (max tim2 tim3 tim4 ))) 1);;these are the fake signals
        ((and (minusp tim1)(< tim1 tim2)
              (> tim1 (min tim2 tim3 tim4 ))) -1)

       ((and (plusp tim1)(minusp tim2)
              (minusp tim3)) 2)
        ((and (minusp tim1)(plusp tim2)
              (plusp tim3)) -2)
        ((and (plusp tim2)(minusp tim3)
              (minusp tim4)) 3)
        ((and (minusp tim2)(plusp tim3)
              (plusp tim4)) -3)
        ((and (plusp tim1)(> tim1 tim2)) 4)
        ((and (plusp tim1)(< tim1 tim2)) 5)
        ((and (minusp tim1)(< tim1 tim2)) -4)
        ((and (minusp tim1)(> tim1 tim2)) -5)
        (t 0))

  ))





;;;;suggested values are size1= 20 size2= 50
(defun timing-line-index50 (date size1 size2)
(let* ((tim1 (timing-line date size1 size2)) rat
       (vol (volatility date size2 1)))

   (setq rat (/ tim1 vol))
   (cond ((< rat -4.0) -5)
         ((and (>= rat -4.0) (< rat -3.0)) -4)
         ((and (>= rat -3.00) (< rat -2.0)) -3)
         ((and (>= rat -2.0) (< rat -1.0)) -2)
         ((and (>= rat -1.0) (< rat 0)) -1)
         ((and (>= rat 0) (< rat 1.0)) 1)
        ((and (>= rat 1.0) (< rat 2.0)) 2)
         ((and (>= rat 2.0) (< rat 3.0)) 3)
         ((and (>= rat 3.0) (< rat 4.0)) 4)
         ((>= rat 4.0) 5))
        ))



;;;;suggested values are size1= 2 size2= 5
(defun timing-line-index5 (date size1 size2)
(let* ((tim1 (timing-line date size1 size2)) rat
       (vol (volatility date size2 1)))

   (setq rat (/ tim1 vol))
   (cond ((< rat -1.0) -5)
         ((and (>= rat -1.0) (< rat -.75)) -4)
         ((and (>= rat -.75) (< rat -.5)) -3)
         ((and (>= rat -.5) (< rat -.25)) -2)
         ((and (>= rat -.25) (< rat 0)) -1)
         ((and (>= rat 0) (< rat .25)) 1)
        ((and (>= rat .25) (< rat .5)) 2)
         ((and (>= rat .5) (< rat .75)) 3)
         ((and (>= rat .75) (< rat 1.0)) 4)
         ((>= rat 1.0) 5))
        ))

(defun ave-timing-line (date size1 size2 days)
  (let (timing-sum)
  (dotimes (ith days)
    (push (timing-line (add-mkt-days date (- ith)) size1 size2) timing-sum))
 (values  (/  (apply #'+ timing-sum) days)
          (car (last timing-sum))
          (- (car (last timing-sum)) (/  (apply #'+ timing-sum) days)))
))

(defun ave-timing-line-diff (date size1 size2 days &optional (diff 1))
  (- (nth-value 2 (ave-timing-line date size1 size2 days))
     (nth-value 2 (ave-timing-line (add-mkt-days date (- diff)) size1 size2 days)))
)


;;;this function returns value and direction
(defun confirming-line (tdate)
   (let ((date tdate) tim-list conf-line conf-line-1)
   (dotimes (ith 16)
     (push (timing-line date) tim-list)
     (setq date (getd date 'ydate)))
   (setq tim-list (reverse tim-list))
   (setq conf-line (/ (list-sum (butlast tim-list)) 15))
   (setq conf-line-1 (/ (list-sum (cdr tim-list)) 15))
   (values conf-line (- conf-line conf-line-1))
   ))

(defun average-range (date period)
 (let ((result 0)(tdate date))
   (dotimes (ith period (/ result period))
     (setq result (+ (- (getd tdate 'high)(getd tdate 'low)) result))
     (setq tdate (getd tdate 'ydate)))
))

(defun average-range% (date period &optional (len 1))
 (let ((result 0)
       (tdate (add-mkt-days date (1+ (- period)))))
    (dotimes (ith period (/ result period))
        (setq result 
          (+ (/ (- (n-day-high tdate len)(n-day-low tdate len))
                (getd tdate 'close))
             result))
         (setq tdate (getd tdate 'ndate)))
     
   
   ))
;;;;average daily range
(defun adr (date)
  (float (/ (+ (getd date 'high)(getd date 'low) (* 2 (getd date 'close))) 4)))

(defun raw%range (date period)
 (round (* 100.0 (/ (-  (/ (+ (adr date)(adr (getd date 'ydate))(adr (add-mkt-days date -2))) 3) (n-day-low date period))
            (n-day-range date 10)))))

(defun cycletrac (tdate)
 (let ((cycletrac 50) rr dir cct smcct)
 (dotimes (ith 10)
   (setq rr (raw%range (add-mkt-days tdate (- ith 10)) 10)) 
   (setq dir (- (adr (add-mkt-days tdate (- ith 10))) (adr (add-mkt-days tdate (- ith 11)))))
   (cond ((and (plusp dir) (> rr cycletrac)) (setq cycletrac rr))
         ((and (minusp dir)(< rr cycletrac))(setq cycletrac rr)))
    (push cycletrac cct))
   (setq smcct (round (+ (* .4 (nth 0 cct)) (* .3 (nth 1 cct)) (* .2 (nth 2 cct)) (* .1 (nth 3 cct)))))
  
 (values  (- cycletrac smcct) cct)

   ))

(defun cycletrac-signal (date)
 (let (dif cct (vol (ave-n-day-range date 10 252)) sig)
;;;the ave-n-day-range returns the 40 percentile
  (multiple-value-setq (dif cct) (cycletrac date))
(setq sig
   (cond ((and (plusp dif)
             (> (count-if #'(lambda (s)(< s 50)) cct) 2)
             (> (n-day-range date 10) vol)
             (> (/ (roc date 40) 40)(* 2 (gann-slope date 252)))
            ) 'BUY)
        ((and (minusp dif)
         (> (count-if #'(lambda (s)(> s 50)) cct) 2)
         (> (n-day-range date 10) vol)
         (minusp (roc date 40))
         (> (abs (/ (roc date 40) 40))(* 2 (gann-slope date 252)))
         ) 'SELL)))

 sig
))
(defun cycletrac-target (date cycle-signal)

  (cond ((eql cycle-signal 'BUY) (+ (getd date 'close) (ave-n-day-range date 10 252)))
       ((eql cycle-signal 'SELL) (- (getd date 'close) (ave-n-day-range date 10 252))))
 
)
(defun cycletrac-stop (date cycle-signal)

  (cond ((eql cycle-signal 'BUY)(float  (- (getd date 'close) (/ (ave-n-day-range date 10 252) 2))))
        ((eql cycle-signal 'SELL) (float (+ (getd date 'close) (/ (ave-n-day-range date 10 252) 2)))))
)
;;;;given a start date, it goes back the number of days and
;;;;finds the dates for the highest and lowest prices
;;;;returns them in order of most recent first

(defun n-day-extreme-dates (tdate days)
   (let ((date tdate) prices highs lows lowest-date highest-date)
    (dotimes (ith days)
      (push (list date (getd date 'high)(getd date 'low))  prices)
      (setq date (getd date 'ydate)))
 
 ;  (format t "~%Prices = ~A~%" prices)
   (setq highs (vsort (copy-list prices) #'> 'cadr));;;vsort is destructive
 ;  (format t "Highs = ~A~%" highs)
   (setq highest-date (caar highs))
   (setq lows (vsort prices #'< 'caddr))
 ;  (format t "Lows = ~A~%" lows)
   (setq lowest-date (caar lows))
   (values
       (if (> highest-date lowest-date) highest-date lowest-date)
       (if (> highest-date lowest-date) lowest-date highest-date)
      ; (if (> highest-date lowest-date) (second (car highs)) (third (car lows)))
      ; (if (> highest-date lowest-date) (third (car lows)) (second (car highs)))
       (second (car highs))(third (car lows))
       (cond ((> highest-date lowest-date) 'UP)
             ((= highest-date lowest-date)
              (if (> (- (getd highest-date 'close)(third (car lows)))
                     (* .5 (true-range highest-date))) 'UP 'DOWN))
             ((< highest-date lowest-date) 'DOWN)))
))
;;;returns num of days from recent extreme high or low
(defun n-day-extreme-dates1 (tdate days)
   (let ((date tdate) prices highs lows lowest-date highest-date
          extreme-date)
    (dotimes (ith days)
      (push (list date (getd date 'high)(getd date 'low))  prices)
      (setq date (getd date 'ydate)))

   (setq highs (vsort prices #'> 'cadr))
   (setq highest-date (caar highs))
   (setq lows (vsort prices #'< 'caddr))
   (setq lowest-date (caar lows))
   (setq extreme-date
       (if (> highest-date lowest-date) highest-date lowest-date)
       )
    (* -1 (sub-mkt-dates extreme-date tdate))
       ))

;;;the baseline is the stop
;;;returns UP DOWN 
(defun n-day-extreme-direction (tdate days)
    (let (extd1 extd2 deltat highp lowp dirp slope t0 baseline ;(tclose (getd tdate 'close))
         (speed-factor 1.0));.3333))
   
   (multiple-value-setq (extd1 extd2 highp lowp dirp)(n-day-extreme-dates tdate days))
     
   ; (ifn dirp (break "DIRP=~A" tdate))
    (setq deltat (1+ (sub-mkt-dates extd2 extd1)))
    (setq slope 
          (* speed-factor (if (eql dirp 'up) (/ (- highp lowp) deltat)
                                  (/ (- lowp highp) deltat))))
    (setq t0 (+ 2 (sub-mkt-dates extd2 tdate)));;;for the next day
    (setq baseline (if (eql dirp 'up)
                      (+ lowp (* slope t0))
                      (+ highp (* slope t0))))

 ;  (cond ((and (eql dirp 'UP) (< tclose (+ lowp (* slope (1- t0)))))
 ;          (setq dirp 'F2UP))
 ;        ((and (eql dirp 'UP) (< tclose (+ lowp (* 2 slope (1- t0)))))
 ;          (setq dirp 'F1UP))
 ;        ((and (eql dirp 'DOWN) (> tclose (+ highp (* slope (1- t0)))))
 ;         (setq dirp 'F2DOWN))
 ;         ((and (eql dirp 'DOWN) (> tclose (+ highp (* 2 slope (1- t0)))))
 ;         (setq dirp 'F1DOWN)))


   (values dirp baseline)
))



;;;returns num of days from recent extreme high or low
;;;after detrending using regression
(defun n-day-extreme-dates2 (tdate days)
   (let ((date tdate) prices highs lows lowest-date highest-date
          extreme-date forecast slope)
    (dotimes (ith days)
      (multiple-value-setq (forecast slope)(lregress date days 'hl))
      (push (list date (- (getd date 'high)(- forecast slope))
                       (- (getd date 'low) (- forecast slope)))  prices)
      (setq date (getd date 'ydate)))

   (setq highs (vsort prices #'> 'cadr))
   (setq highest-date (caar highs))
   (setq lows (vsort prices #'< 'caddr))
   (setq lowest-date (caar lows))
   (setq extreme-date
       (if (> highest-date lowest-date) highest-date lowest-date)
       )
    (values (if (eql extreme-date highest-date) 'DN 'UP)
      (* -1 (sub-mkt-dates extreme-date tdate)) days)
       ))
;;;;
(defun extreme-index (tdate period1 period2)
    (let (dirp1 dirp2)
   (setq dirp1 (nth-value 4 (n-day-extreme-dates tdate period1))
         dirp2 (nth-value 4 (n-day-extreme-dates tdate period2)))

   (cond ((and (eql dirp1 'UP)(eql dirp2 'UP)) 'UU)
         ((and (eql dirp1 'UP)(eql dirp2 'DOWN)) 'UD)
         ((and (eql dirp1 'DOWN)(eql dirp2 'UP)) 'DU)
         ((and (eql dirp1 'DOWN)(eql dirp2 'DOWN)) 'DD)
    )
))
;;;returns highest and lowest prices since
;;;;a starting date
(defun extreme-low-high (sdate tdate)
  (let ((days (sub-mkt-dates sdate tdate)))
     (values (n-day-low tdate (1+ days))(n-day-high tdate (1+ days)))
))

(defun reflect1 ( p0 p1 t0 p2 t1)
  (let ((s0 (/ (- p0 p1) t0)) (s1 (/ (- p1 p2) t1)))

;  (setq p2 (if (plusp speed0)(n-day-high (add-mkt-days date (- (round (/ t0 2)))) (round (/ size 2)))
 ;     (n-day-low (add-mkt-days date (- (round (/ t0 2)))) (round (/ size 2)))))
;  (format T "P0 = ~A P1= ~A P2= ~A T0= ~A T1= ~A s0= ~A s1= ~A" p0 p1 p2 t0 t1 s0 s1)
  (cond ((and (> s0 0) (> s0 (abs s1))) 2) ;;steep uptrend
        ((and (> s0 0) (<= s0 (abs s1))) 1);;slow uptrend
        ((and (< s0 0)(> (abs s0) s1)) -2);;steep downtrend
        ((and (< s0 0)(<= (abs s0) s1)) -1);;slow downtrend
        (t 0);;;balanced
        )
  ))
(defun reflect2 (tdate days)
  (let (extd1 extd2 highp lowp dirp deltat deltat1 p0 p1 p2 s0 s1 ldate wdelta gsi)
   
   (multiple-value-setq (extd1 extd2 highp lowp dirp)(n-day-extreme-dates tdate days))
   (setq deltat (1+  (sub-mkt-dates extd1 tdate)) wdelta (1+ (sub-mkt-dates extd2 extd1)))
   (setq ldate (add-mkt-days tdate (1+ (- days))))
   (setq deltat1 (1+ (sub-mkt-dates ldate extd2)))
   (setq gsi (gann-slope-index1 tdate wdelta))
 ;  (format T "extd1= ~A extd2= ~A ldate= ~A " extd1 extd2 ldate)
   (setq p2 (cond ((and (/= ldate extd2)(eql dirp 'DOWN))(n-day-low extd2 deltat1))
                  ((and (/= ldate extd2)(eql dirp 'UP)) (n-day-high  extd2  deltat1))
                  ((and (= ldate extd2)(eql dirp 'DOWN)) (n-day-high extd2 1))
                  ((and (= ldate extd2)(eql dirp 'UP)) (n-day-low extd2 1))
               ))

   (setq p0 (cond ((and (= extd1 tdate)(eql dirp 'UP)) nil)
                  ((and (= extd1 tdate)(eql dirp 'DOWN)) nil)
                  ((and (= extd1 extd2)(eql dirp 'UP))(n-day-low tdate 1))
                  ((and (= extd1 extd2)(eql dirp 'DOWN))(n-day-high tdate 1))
                  ((and (/= extd1 extd2)(eql dirp 'DOWN))(n-day-high tdate (1- deltat)))
                  ((and (/= extd1 extd2)(eql dirp 'UP))(n-day-low tdate (1- deltat)))
                  ((and (= extd1 extd2)(eql dirp 'DOWN))(n-day-low tdate deltat))
                  ((and (= extd1 extd2)(eql dirp 'UP))(n-day-high tdate deltat))
                  (t nil)))
   (setq p1 (cond ((and (/= extd1 tdate)(eql dirp 'DOWN)) lowp)
                  ((and (/= extd1 tdate)(eql dirp 'UP)) highp)
                  ((and (= extd1 tdate)(eql dirp 'DOWN)) highp)
                  ((and (= extd1 tdate)(eql dirp 'UP)) lowp)
                  ))

  ; (format T "P0 = ~A  p1= ~A  p2= ~A dirp= ~A deltat= ~A wdelta= ~A~%" p0 p1 p2 dirp deltat wdelta) 
   (if p0  (setq s0 (abs (/ (- p0 p1) deltat)) s1 (abs (/ (- p1 p2)  wdelta))))
  
    (cond ((and (not p0)(eql dirp 'UP)) (if (>= gsi 3) 2 1))
        ((and (not p0)(eql dirp 'DOWN)) (if (<= gsi -3) -2 -1))
        ((= wdelta days) 0)
        ((and (> p0 p1); (> p0 p2)
                       (> s0 s1)) 2) ;;steep uptrend
        ((and (> p0 p1) ;(< p0 p2)
                        (< s0 s1)) 1);;slow correction in uptrend
        ((and (< p0 p1);(< p0 p2)
                       (> s0 s1)) -2);;steep downtrend
        ((and (< p0 p1);(> p0 p2)
                       (< s0 s1)) -1);;slow correction in downtrend
        (t 0);;;balanced
        )

  ))
;;;;not working properly
(defun reflect2+ (tdate days)
  (let (rf)
        (setq rf (reflect2 tdate days))
        (if (zerop rf)(reflect2+ tdate (1+ days))
        (values rf days))
))

;;;size is a filter size
(defun reflect3 (date size)
 (let (bpl targ p0 p1 p2 p3 p4 t0 t1)

   (multiple-value-setq (bpl targ p0 p1 p2 p3 p4 t0 t1)
     (channel-trend date size))
    (reflect1  p0 p1 t0 p2 t1)))


(defun reflect4 (date days)

  (reflect3 date (max 5 (* 2 (1+ (abs (n-day-extreme-dates1 date days)))))))


(defun new-dominant-cycle (tdate period1 period2)
  (let (periods longest)
     (setq periods (reverse (cluster-cycles (nth-value 1 (dominant-cycle tdate period1 period2)))))

 ; (format T "~%~A" periods)
  (setq longest (first periods))
  (dolist (ith periods)
   (if (> (length ith) (length longest)) (setq longest ith)))
   (round (median longest)) 
  )) 
;;;find the lowst value in period days. the value could be lowest high
;;;lowest close or lowest low.  
(defun n-day-low (date period &optional (low 'low))
    (let ((tdate date) lows (roll 0))
     (dotimes (ith period (min* lows))       
        (push (+ roll (getd tdate low)) lows)
       ; (setq roll (+ roll (or (getd tdate 'rollover) 0)))
        (setq tdate (getd tdate 'ydate)))
))
(defun n-day-high (date period &optional (high 'high))
   (let ((tdate date) highs (roll 0))
      (dotimes (ith period (max* highs))
         (push (+ roll (getd tdate high)) highs)
        ; (setq roll (+ roll (or (getd tdate 'rollover) 0)))
         (setq tdate (getd tdate 'ydate)))
 ))
(defun n-day-lowp (date period &optional (low 'low))
   (= (getd date low) (n-day-low date period low)))
(defun n-day-highp (date period &optional (high 'high))
   (= (getd date high) (n-day-high date period high)))

(defun n-day-range (date period)
  (- (n-day-high date period) (n-day-low date period)))

;;;finds the average range of the range (period1) over a time of period2
(defun ave-n-day-range (date period1 period2)
  (let (ranges)
  (dotimes (ith period2)
     (push (n-day-range (add-mkt-days date (- ith period2)) period1) ranges)) 
 (percentile .50 ranges)
))
;;;;checks if recent 3 day high is higher than previous 3 day perod
;;;;checks if recent 3 day low is lower than the previous 3 day period
(defun high-low-index (tdate size)
   (let ((hg (n-day-high tdate size))(lw (n-day-low tdate size))
         (hgp (n-day-high (add-mkt-days tdate (- size)) size))
         (lwp (n-day-low (add-mkt-days tdate (- size)) size)))
   
  (cond ((and (>= hg hgp)(> lw lwp)) 1)
        ((and (<= lw lwp)(< hg hgp)) -1)
        (t 0))
))

(defun high-low-trend (date size)
  (let ((cl (getd date 'close))(lw (getd date 'low))
         (hg (getd date 'high))(anh (ave date size 'high))
         (anl (ave date size 'low)))

    (cond ((and (> cl anh)(> lw anl)) 'UP)
          ((and (< cl anl)(< hg anh)) 'DN)
          (t 'FT))
))

(defun short-term-trend (tdate)
  (let (ave-closes pivots (date tdate) ave-pivots)
  (setq ave-pivots
       (dotimes (ith 3 (/ (list-sum pivots) 3))
           (push (nth 2 (pivot-points date)) pivots)
           (setq date (getd date 'ydate))
           ))
  (setq ave-closes (ave tdate 5))
  (cond ((> (nth 2 (pivot-points tdate)) (max ave-pivots ave-closes)) 'UP)
        ((< (nth 2 (pivot-points tdate)) (min ave-pivots ave-closes)) 'DOWN)
        (t 'FLAT))
        )) 
        
(defun short-term-trend1 (tdate &optional (per .01))
  (let ((date tdate)(period 1) direction)
  
  (setq direction
  (loop
   (cond ((and (> (getd date 'open) (getd date 'close))
               (> (/ (- (n-day-high tdate period)(n-day-low tdate period)) (n-day-high tdate period)) per)) (return 'DN))
         ((and (< (getd date 'open)(getd date 'close))
               (> (/ (- (n-day-high tdate period)(n-day-low tdate period)) (n-day-low tdate period)) per)) (return 'UP)))
    (setq period (1+ period) date (add-mkt-days tdate (- period)))))
    
   (values direction (if (eql direction 'up) 
                        (* (- 1 per) (n-day-high tdate period))
                        (* (+ 1 per) (n-day-low tdate period)))) 
    

    ))

;;;;use size 11 for weekly trend
;;;;use size 3 for daily trend       
(defun dow-trend (tdate &optional (size 11))
  (let* ((filt *n-filt*)(time-interval *time-interval*) low high turns
        (days (* size -15))
        (starttime (conv-to-string (add-mkt-days tdate days) 'A)))

       (setq *n-filt* size *time-interval* 'DAILY-HIGH-LOW)
       (loop
          (setq turns (find-all-primitives starttime (conv-to-string tdate 'P))
                days (+ days -2))
          (if (< (length turns) 4)
            (setq starttime (conv-to-string (add-mkt-days tdate days) 'A))
            (return)))
       (setq low (getv 0 LP) high (getv 0 HP))
       (setq *n-filt* filt *time-interval* time-interval)  
       (cond ((and (> (first turns)(second turns))
                   (> (third turns)(first turns))
                   (< low (second turns))) 'DN)
             ((and (> (first turns)(second turns))
                   (> (third turns)(first turns))
                   (> (nth 3 turns)(second turns))) 'DN) 
             ((and (< (first turns)(second turns))
                   (< (first turns)(third turns))
                   (< (second turns)(nth 3 turns))
                   (<= high (second turns))) 'DN) 
            
             ((and (< (first turns)(second turns))
                   (< (third turns)(first turns))
                   (> high (second turns))) 'UP)    
             ((and (< (first turns) (second turns))
                   (< (third turns)(first turns))
                   (< (nth 3 turns)(second turns))) 'UP)
             ((and (> (first turns)(second turns))
                   (< (third turns)(first turns))
                   (< (nth 3 turns)(second turns))
                   (>= low (second turns))) 'UP)
                  
             ((and (< (first turns)(second turns))
                   (< (first turns)(third turns))
                   (< high (second turns))) 'EDN)
             ((and (> (first turns)(second turns))
                   (> (first turns)(third turns))
                   (> low (second turns))) 'EUP)  
             (t 'FT))
     
        )) 

(defun channel-trend (tdate size)
  (let* ((filt *n-filt*)(time-interval *time-interval*)(days (* size -5))
         p0 p1 p2 p3 p4 t0 t1 t2 t3 prices index-list ignore low high bpl turns
         target base-line (starttime (conv-to-string (add-mkt-days tdate days) 'A)))
        (declare (ignore ignore))
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv)
  ;      (format T "Start date= ~A~%" starttime)

       (loop
          (setq turns (find-all-primitives starttime (conv-to-string tdate 'P))
                days (+ days -2))
          (if (< (length turns) 4)
            (setq starttime (conv-to-string (add-mkt-days tdate days) 'A))
            (return)))

        (multiple-value-setq (prices ignore ignore ignore index-list)
            (find-all-primitives starttime  (conv-to-string tdate 'P)))
        (setq low (getv 0 LP) high (getv 0 HP))
;        (format T "low = ~A   high = ~A  PRICES = ~A~%" low high prices)

        (setq p1 (first prices) p2 (second prices) p3 (third prices) p4 (nth 3 prices)
            ; t0 (truncate (- *end-index* (first index-list)) 2) ;;;convert time to days
             t0  (- *end-index* (first index-list)) ;;;time is in half days
            ; t1 (truncate (- (first index-list)(second index-list)) 2)
             t1 (- (first index-list)(second index-list))
            ; t2 (truncate (- (second index-list)(third index-list)) 2)
             t2  (- (second index-list)(third index-list))
            ; t3 (truncate (- (third index-list)(nth 3 index-list)) 2)
             t3 (- (third index-list)(nth 3 index-list))
             )
        (if (zerop t0) (setq t0 1))(if (zerop t1)(setq t1 1))
        (if (zerop t2)(setq t2 1))(if (zerop t3)(setq t3 1))
       (setq bpl (+ (/ (+ p1 p2) 2)
                    (/ (* (- p1 p3)(+ (/ t1 2) t0 0)) ;;;need to add  1 for location of bpl tomorrow.
                       (+ t1 t2))))
       (setq target (+ p2 (/ (* (- p1 p3) (+ t0 t1 0)) (+ t1 t2)));;;add 1 for location of target tomorrow
             base-line (+ p1 (/ (* (- p1 p3)(+ t0 0)) (+ t1 t2))))
        (setq p0 (getd tdate 'close))
       ;(if (neql (getv 0 DR) 'UP) (setq p0 high)(setq p0 low))
        (setq *n-filt* filt *time-interval* time-interval)
    (values bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 base-line)
))


;;;these are the cycle projections
(defun cycle-proj (tdate &optional (days -252)(min-period 10)(max-period 30))
  (let ((n-filt *n-filt*) (starttime (conv-to-string (add-mkt-days tdate days) 'A)) prices index-list
	turns period gsi5 ignore)
    (declare (ignore ignore))
   (setq *n-filt*  (new-dominant-cycle tdate min-period max-period))
   (setq *time-interval* 'daily-high-low period *n-filt*)
   (setq gsi5 (gann-slope-index tdate 5))
    (multiple-value-setq (prices ignore ignore ignore index-list)
            (find-all-primitives starttime  (conv-to-string tdate 'P)))
    (dotimes (ith 5)
      (if (< (car prices)(cadr prices))
	  (progn
	    (push (truncate (- (+ (first index-list) (* 2 (1+ ith) *n-filt*)) *end-index*) 2) turns)
;	    (format t "~A1~%" turns)
	    (push (truncate (- (+ (third index-list) (* 2 (1+ ith) *n-filt*)) *end-index*) 2) turns)
					;	     (format t "~A2~%" turns)
	    )
	  (progn
	    (push (truncate (- (+ (second index-list) (* 2 (1+ ith) *n-filt*)) *end-index*) 2) turns)
	    ;(format t "~A3~%" turns)
	  (push (truncate (- (+ (fourth index-list) (* 2 (1+ ith) *n-filt*)) *end-index*) 2) turns)
         ; (format t "~A4~%" turns )
	  )
      )
     ; (format t "~% ~A ~A ~A ~A ~%"  index-list (* (1+ ith) *n-filt*) *end-index* turns)
    )
  (setq *n-filt* n-filt)
  (setq turns (remove-if #'(lambda(s)(or (< s -1)(> s 1))) turns))
 (values
  (cond ;((and (eql (car turns) -1)(minusp ave5d)) 'L1-)
	 ;((and (eql (car turns) 0)(minusp slope4)) 'L)
	 ((and (eql (car turns) 1)(minusp gsi5)) 'L)
	 ;((and (eql (car turns) -1) (plusp ave5d)) 'S1-)
	 ;((and (eql (car turns) 0)(plusp slope4)) 'S)
	 ((and (eql (car turns) 1)(plusp gsi5)) 'S)
	 (t 0))
   period) 
))
;;;the date entered is current day days is the number of days to look back for a candle signal
(defun cycle-proj1 (tdate &optional (days 3))
 (let ((date tdate) can dirp (cntr 0))
  (setq cntr         
	(dotimes (ith days)
	  (setq can (cycle-proj date -252 10 30)) ;(print can)
          (if (neql can 0) (return ith))
	  (if (= ith (1- days))(return ith))
          (setq date (getd date 'ydate))
    ))

;  (format t "~%CAN = ~A CNTR = ~A"  can cntr)
  ;;;check validity
 ; (if (and  cntr (> (- (getd (add-mkt-days tdate (- cntr)) 'close) (ave (add-mkt-days tdate (- cntr)) days 'close)) 0))
  ;    (setq dirp 'UP)(setq dirp 'DN))
  (if (plusp (nth-value 1 (lregress (add-mkt-days tdate (-  cntr)) 4 'close))) (setq dirp 'UP)(setq dirp 'DN))
  
;  (format t " ~A" dirp)
  
  (cond ((and cntr (eql dirp 'up) (> (n-day-high tdate (1+ cntr)) (getd (add-mkt-days tdate (- cntr)) 'high)))
        (setq can 0))
	((and cntr (eql dirp 'DN) (< (n-day-low tdate (1+ cntr)) (getd (add-mkt-days tdate (- cntr)) 'low)))
	 (setq can 0))
	(t can))
  
  (values  can dirp cntr (and cntr (add-mkt-days tdate (- cntr))))
))



;;;;
(defun channel-proj (tdate size)
 (let (bpl target baseline p0 p1 p2 p3 p4 t0 t1 t2 t3)
   (multiple-value-setq (bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline)(channel-trend tdate size))
 (sort (list bpl target baseline) #'<)
))

(defun channel-direction (tdate size)
   (let (bpl target base-line p0 p1 p2 p3 p4 t0 t1 t2 t3)
    (multiple-value-setq (bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 base-line)
               (channel-trend tdate size));;;size is a filter size
    (setq p0 (getd tdate 'close))
    (cond ((and (< p1 p3)(> p1 p2)(> p0 base-line)) 'AC);;;only happens if slope downward
          ((and (> p1 p3)(< p1 p2)(< p0 base-line)) 'BC);;;only happens if slope upwards

          ((and (> p1 p3)(< p1 p2)(> p0 target)) 'AT);;;steep trend upwards
          ((and (< p1 p3)(> p1 p2)(< p0 target)) 'BT);;;steep trend downwards

          ((and (< p1 p3)(< p1 p2)(> p0 p2)) 'US);;;up sharp steep trend upwards
          ((and (> p1 p3)(> p1 p2)(< p0 p2)) 'DS);;;down sharp steep trend downwards

          ((and (< p1 p3)(< p1 p2)(> p0 target)) 'AT2);;;steep trend upwards          
          ((and (> p1 p3)(> p1 p2)(< p0 target)) 'BT2);;;steep trend downwards

          ((and (> p1 p3)(< p1 p2)(> p0 bpl)) 'UC+);;upward channel
          ((and (< p1 p3)(> p1 p2)(< p0 bpl)) 'DC-);;;downward channel below bpl

          ((and (> p1 p3)(< p1 p2)(<= p0 bpl)) 'UC);;upward channel
          ((and (< p1 p3)(> p1 p2)(>= p0 bpl)) 'DC);;;downward channel above bpl

          ((and (> p1 p3)(> p1 p2)) 'UC-);;;upward channel price downward
          ((and (< p1 p3)(< p1 p2)) 'DC+);;;downward channel price upward
         
         
          ((and (= p1 p3)(> p1 p2)) 'IC-)
          ((and (= p1 p3)(< p1 p2)) 'IC+)
          (t 'IC))

))


(defun channel-direction-swing-index345 (tdate)
   (let ((chan3 (channel-direction tdate 3)) (chan4 (channel-direction tdate 4)) (chan5 (channel-direction tdate 5)) 
         (dir (mvhl-trend tdate))
          )
   (+
   (cond ((and (eql dir -1)(member chan3 '(BC UC- DC DS DC-))) 1)
         ((and (eql dir -1)(member chan3 '(BT2 BT IC-))) -0)
         ((and (eql dir 1)(member chan3 '(DC+ UC+ US))) -1)
         ((and (eql dir 1)(member chan3 '(AT2 DC IC- IC+))) 0) 
         (t 0)
     )    
   (cond ((and (eql dir -1)(member chan4 '(DC DS AC ))) 1)
         ((and (eql dir -1)(member chan4 '(IC- UC- UC BT))) 0)
         ((and (eql dir 1)(member chan4 '(AT BT2 US UC+))) -1)
         ((and (eql dir 1)(member chan4 '(DC+ IC+ IC-AT2))) 0)
         (t 0) 
         )
   (cond ((and (eql dir -1)(member chan5 '(AT2 DC DS))) 1)
         ((and (eql dir -1)(member chan5 '(DC+ IC- UC AC))) 0)
         ((and (eql dir 1)(member chan5 '(DC DC+ AT UC+))) -1)
         ((and (eql dir 1)(member chan5 '(AT2 IC+ BT2 DC+))) 0)
         (t 0) 
        )  
)


))

;;;for triumph
(defun channel-direction-index5 (tdate)
   (let ((can (channel-direction tdate 5)) 
         (bull '(AC DC IC- IC+ BC BT BT2 DC-))
         (bear '(BT DC AT AC UC- IC- US DS))
          )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )

))


;;;for Epic with no prefiltering
(defun channel-direction-index14 (tdate)
   (let ((can (channel-direction tdate 14)) 
;         (bull '(IC+ AC DC+ UC- UC+ AT2))(bear '(IC- AC BC IC+ BT2 DC- UC- UC+ AT2 ))
        (bull '(AC IC- BC UC- DC- IC+ AT2 DC+ UC+ ))(bear '(BC AC BT2 UC- DC+ DC- IC- DS UC+ )) 
         )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )

))

;;;;for counter swing
(defun channel-direction11 (tdate)
   (let ((chan (channel-direction tdate 11))
         (dir (parabolic-stops tdate))) 
         

   (cond ((and (eql dir 'LONG)(member chan '(BT2 IC+ UC- UC))) -1)
         ((and (eql dir 'LONG)(member chan '(AC DC BC UC+)))  0)
         
         ((and (eql dir 'SHORT)(member chan '(BC AT2 IC- BT AC))) 1)
         ((and (eql dir 'SHORT)(member chan '(DC+ DS DC- DC))) 0)
         (t 0))

))

(defun channel-direction-index21 (tdate )
   (let ((can (channel-direction tdate 21)) 
         (bull '(AC BT2 DC DC+ UC- IC- AT2 BT))
         (bear '(BC UC- UC DC+ AT BT2 DC AT2))
          )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )

))






;;;if the close price exceeds the target then the 
;;;bpl and target are recalculated like the P0 is the end of the wave       
(defun channel-trend1 (tdate size)
  (let ((filt *n-filt*)(time-interval *time-interval*)
         p0 p1 p2 p3 p4 t0 t1 t2 t3 prices index-list ignore low high bpl
         target (starttime (conv-to-string (add-mkt-days tdate (* size -20)) 'A)))    
    ;    (declare (ignore ignore))
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv) 
        (multiple-value-setq (prices ignore ignore ignore index-list)
            (find-all-primitives starttime  (conv-to-string tdate 'P)))        
        (setq low (getv 0 LP) high (getv 0 HP))  
                                   
        (setq p1 (first prices) p2 (second prices) p3 (third prices) p4 (nth 3 prices)
             t0 (- *end-index* (first index-list))
             t1 (- (first index-list)(second index-list)) 
             t2 (- (second index-list)(third index-list))
             t3 (- (third index-list)(nth 3 index-list))
             )
       (setq bpl (+ (/ (+ p1 p2) 2)
                    (/ (* (- p1 p3)(+ (/ t1 2) t0 2))
                       (+ t1 t2))))     
       (setq target (+ p2 (/ (* (- p1 p3) (+ t0 t1 2)) (+ t1 t2))))      
       (if (eql (getv 0 DR) 'UP) (setq p0 high)(setq p0 low))
       
       (if (or (and (> target bpl) (> (getd tdate 'close) target))
               (and (< target bpl) (< (getd tdate 'close) target)))
           (setq bpl (+ (/ (+ p0 p1) 2)
                        (/ (* (- p0 p2)(+ (/ t0 2) 2))
                           (+ t0 t1)))
              target (+ p1 (/ (* (- p0 p2)(+ t0 2))(+ t0 t1)))))      
       
       
        (setq *n-filt* filt *time-interval* time-interval)   
          
    (values bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3)
))

;;;;for a given filter size what price will change the primitive direction?
;;;first value is the price to change primitive direction
;;; second value is the initial stop loss
(defun price-to-go (tdate size)
   (let (prices (date tdate) bpl target p0 p1)
   (dotimes (ith size)
     (push (second (nth-value 4 (data-access date))) prices)
     (push (first (nth-value 4 (data-access date))) prices)
     (setq date (getd date 'ydate)))
  
   (setq prices (nreverse prices))
   (multiple-value-setq (bpl target p0 p1 )(channel-trend tdate size))
 ;   (print prices)
   (values
    (if (> p0 p1) (apply #'min (subseq prices 0 size))(apply #'max (subseq prices 0 size)))
    (ifn (> p0 p1) (apply #'min (subseq prices 0 size))(apply #'max (subseq prices 0 size)))
)))
    
   
     
;;;size refers to filter size  
(defun structure-trend (tdate size &optional (outfile nil))
  (let (bpl targ p0 p1 p2 p3 p4  trend rprice mtrend mrprice)
 ; (declare (ignore  bpl targ p4))
  (multiple-value-setq (bpl targ p0 p1 p2 p3 p4) (channel-trend tdate size))
   (if (> p0 p1) (setq mtrend 'UP) (setq mtrend 'DN))
   (if (eql mtrend 'UP) (setq mrprice (n-day-low tdate (ceiling size 2)))
      (setq mrprice (n-day-high tdate (ceiling size 2))))  
   (cond ((and (> p0 p1)(> p0 p2)) 
             (setq trend 'UP0 rprice (n-day-low tdate (ceiling size 2))))
         ((and (< p0 p1)(< p0 p2)) 
            (setq trend 'DN0 rprice (n-day-high tdate (ceiling size 2))))
               
         ((and (> p0 p1)(>= p1 p3))
            (setq trend 'UP1 rprice (n-day-low tdate (ceiling size 2))))
         ((and (< p0 p1)(<= p1 p3))
             (setq trend 'DN1 rprice (n-day-high tdate (ceiling size 2))))
         
         ((and (> p0 p1)(<= p1 p3)(< p0 p2))
              (setq trend 'UP2 rprice (n-day-low tdate (ceiling size 2))))
         ((and (< p0 p1)(>= p1 p3)(> p0 p2))
              (setq trend 'DN2 rprice (n-day-high tdate (ceiling size 2))))    
                       
         (t 'FT))
   (if outfile (format outfile "~%TREND= ~A  RPRICE=~7,,,F MTREND= ~A  MRPRICE= ~7,,,F" 
             trend rprice mtrend mrprice)) 
    
         
   (values trend rprice mtrend mrprice)      
))
      

(defun primitive-slope-trend (tdate size)
  (let (bpl targ p0 p1 p2 p3 p4 t0 t1 t2 t3 s0 s1 )

    (multiple-value-setq (bpl targ p0 p1 p2 p3 p4 t0 t1 t2 t3) (channel-trend tdate size))
     (setq s0 (/ (- p0 p1) t0)
           s1 (/ (- p1 p2) t1)
          )

    (cond 
          ((and (plusp s0)(> s0 (- s1))) 2)
          ((and (plusp s0)(<= s0 (- s1))) 1);;;up but slower momentum against downtrend
              
          ((and (minusp s0)(> (- s0) s1)) -2)
          ((and (minusp s0)(<= (- s0) s1)) -1);;;down but slower momentum against an uptrend
        
          (t 0))
   
))
      


(defun my-trend (tdate size)
  (let (bpl targ p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline)
  
   (multiple-value-setq (bpl targ p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline) (channel-trend tdate size))
    
   (cond  ((and (>= p0 (max bpl targ))(> p2 p1 p3))  4)
          ((and (>= p0 (min bpl targ))(> p2 p1 p3))  3)
          ((and (>= p0  baseline)(> p2 p1 p3))   2)
          ((and (< p0  baseline)(> p2 p1 p3))   1)
                 
          ((and (>= p0 (max bpl baseline))(> p1 p3 p2))  8)
          ((and (>= p0 (min bpl baseline))(> p1 p3 p2))  7)
          ((and (>= p0  targ)(> p1 p3 p2))   6)
          ((and (< p0  targ)(> p1 p3 p2))   5)
                 
          ((and (<= p0 (min bpl targ))(< p2 p1 p3))  -4)
          ((and (<= p0 (max bpl targ))(< p2 p1 p3))   -3)
          ((and (<= p0 baseline)(< p2 p1 p3))   -2)
          ((and (> p0 baseline)(< p2 p1 p3))   -1)

          ((and (<= p0 (min bpl baseline))(< p1 p3 p2))  -8)
          ((and (<= p0 (max bpl baseline))(< p1 p3 p2))   -7)
          ((and (<= p0 targ)(< p1 p3 p2))   -6)
          ((and (> p0 targ)(< p1 p3 p2))   -5)
   
          (t 0))         
))



(defun my-trend1 (tdate size)
  (let (bpl targ p0 p1 p2 p3 p4 trend )
  (declare (ignore  p3 p4))
  (loop
  (if (> size 20) (return))
  (multiple-value-setq (trend bpl targ p0 p1 p2) (my-trend tdate size))
  
  (if (member trend '(nil EUP1 EDOWN1)) (setq size (+ 2 size)) (return))) 
 
         
    (cond ((and (< targ bpl) (eql trend 'UP))
           (setq targ (+ bpl (- bpl targ))))
          ((and (> targ bpl) (eql trend 'DN))
           (setq targ (- bpl (- targ bpl)))))
            
   (values trend bpl targ p0 p1 p2)      
))
;;;this is the trend based on percentage move (expressed as ratio of the lower price)
;;;from Technical Analysis of Stocks and Commodities bonus issue 1993.

(defun merrill-trend (tdate &optional (per 1.05))
  (let ((days 1) ratio extd1 extd2 highp lowp dirp )
  (loop
     (multiple-value-setq (extd1 extd2 highp lowp dirp)(n-day-extreme-dates tdate days)) 
     (setq ratio (/ highp lowp))
      (if (> ratio per)(return)(incf days)))
(values dirp extd1 extd2 highp lowp days)
)) 


;;;returns a integer from 0 to 7
;;;indicates the pattern of the close price versus the pldot
;;;for the past three days
;;;Based on Drummond Geometry
(defun pldot-trend (date)
  (let* ((date-1 (getd date 'ydate))(date-2 (getd date-1 'ydate))(date-3 (getd date-2 'ydate))
         (close-0 (getd date 'close))(close-1 (getd date-1 'close))(close-2 (getd date-2 'close))
         (pldot-0 (ave date-1 3 'pivot)) (code 0)
         (pldot-1 (ave date-2 3 'pivot))
         (pldot-2 (ave date-3 3 'pivot)))

        (if (> close-2 pldot-2)(setq code (+ 4 code)))
        (if (> close-1 pldot-1)(setq code (+ 2 code)))
        (if (> close-0 pldot-0)(setq code (+ 1 code)))

       code ))
;;;;this tells where the current close is in relation to the
;;;pldot and the volatility envelope
 (defun pldot-index (date)
  (let ((pldot (ave (getd date 'ydate) 3 'pivot))
        (close0 (getd date 'close))
        (high3 (ave (getd date 'ydate) 3 'high))
        (low3 (ave (getd date 'ydate) 3 'low)))


    (cond ((> close0 high3) 2)
          ((>= close0 pldot) 1)
          ((< close0 low3) -2)
          ((< close0 pldot) -1)
          (t 0))

          ))
;;;compares two trends
(defun price-direction (tdate &optional (days1 2)(days2 10));(days3 21))
  (let ((dirp1 (n-day-extreme-direction tdate days1))
        (dirp2 (n-day-extreme-direction tdate days2))
       ; (dirp3 (n-day-extreme-direction tdate days3))
          )

  (cond ((and (eql dirp1 'UP)(eql dirp2 'UP)) 'UU);(eql dirp3 'UP)) 'UUU)
       ; ((and (eql dirp1 'UP)(eql dirp2 'UP)(eql dirp3 'DOWN)) 'UUD)
        
        ((and (eql dirp1 'UP)(eql dirp2 'DOWN)) 'UD); (eql dirp3 'UP)) 'UDU)
      ;  ((and (eql dirp1 'UP)(eql dirp2 'DOWN)(eql dirp3 'DOWN)) 'UDD)

        ((and (eql dirp1 'DOWN)(eql dirp2 'UP)) 'DU);(eql dirp3 'UP)) 'DUU)
      ;  ((and (eql dirp1 'DOWN)(eql dirp2 'UP)(eql dirp3 'DOWN)) 'DUD)
       
        ((and (eql dirp1 'DOWN)(eql dirp2 'DOWN)) 'DD));(eql dirp3 'UP)) 'DDU)
      ;  ((and (eql dirp1 'DOWN)(eql dirp2 'DOWN)(eql dirp3 'DOWN)) 'DDD))
))

;;;calculates the number of days the 4  day rate of change
;;;has stayed positive or negative

(defun tdsetup-index (tdate)
  (let ((roc4 (roc tdate 4)) date (ctr 0))

     (setq date tdate)
     (if (plusp roc4) (setq ctr 1) (setq ctr -1))

     (dotimes (ith 8)
        (setq date (getd date 'ydate))
        (setq roc4 (roc date 4))
        (cond ((and (>= ctr 1)(plusp roc4)) (incf ctr))
              ((and (<= ctr -1)(minusp roc4))(decf ctr))
              ((and (>= ctr 1)(minusp roc4))(return))
              ((and (<= ctr -1)(plusp roc4))(return))
              (t (return))))

      ctr
 ))
;;;ranks the most recent 3 bars of data and
;;;returns a ranking code
(defun range-index (date)
   (let ((bar1 (true-range date)) (bar2 (true-range (getd date 'ydate)))
         (bar3 (true-range (getd (getd date 'ydate) 'ydate))))

       (cond ((>= bar3 bar2 bar1) 0)
             ((>= bar3 bar1 bar2) 1)
             ((>= bar2 bar3 bar1) 2)
             ((>= bar2 bar1 bar3) 3)
             ((>= bar1 bar3 bar2) 4)
             ((>= bar1 bar2 bar3) 5))
))


;;;ranks the most recent size number of bars of data and
;;;returns an index of direction and number of the longest of the n (size) bars
(defun range-index1 (date size)
   (let  (dmy bar-range)
    (dotimes (ith size)
      (setq dmy  (acons (true-range (add-mkt-days date (- ith))) (- ith) dmy)))
 

    (vsort dmy #'> 'car) ; (print dmy)
     (setq bar-range (roc (add-mkt-days date (cdar dmy)) 1))
    (cond ((>= bar-range 0) (abs (1- (cdar dmy))))
          ((< bar-range 0) (1- (cdar dmy)))
         )
       
    ; (values (if (plusp bar-range) 'UP 'DN) (cdar dmy))  
))


;;;ranks the most recent size number of bars of data and
;;;returns an index of direction and number of the longest of the n (size) bars
;;;;finds the bar with largest daily change
(defun range-index2 (tdate size)
   (let  (dmy bar-range (date tdate))
    (dotimes (ith size)
      (setq dmy  (acons (abs (roc date 1)) (- (1+ ith)) dmy)
            date (getd date 'ydate)))
 

    (vsort dmy #'> 'car)  (print dmy)
    (setq bar-range (roc (add-mkt-days tdate (1- (cdar dmy))) 1))
    
    (values (if (plusp bar-range) 'UP 'DN) (cdar dmy))  
      
))


;;;compares the true range of today versus yesterday
(defun range-ratio-index (date)
     (let ((bar1 (true-range date)) (bar2 (true-range (getd date 'ydate)))(ctr -1) rat)
         (loop
             (if (and bar2 (zerop bar2))(setq bar2 (true-range (add-mkt-days date (decf ctr))))
               (return)))
          (setq rat (/ bar1 bar2))

          (cond ((< rat .4) -4)
                ((< rat .6) -3) 
                ((< rat .8) -2)
                ((< rat 1.0) -1)
                ((< rat 1.3) 1)
                ((< rat 1.6) 2)
                ((< rat 2.5) 3)
                ((>= rat 2.5) 4))
)) 


(defun body-range (tdate)
   (- (getd tdate 'close)(getd tdate 'open)))
;;;this is nothing more than the average true range
(defun ave-body-range (date &optional (period 5) (param 1.0))
  (let (ranges)
   (dotimes (ith period)
     (setq ranges (push (body-range (add-mkt-days date (- ith))) ranges)))
  (* param (/ (list-sum ranges) period))
    ))

(defun bodyrange-ratio-index (date period1 period2 &optional (param 1))
 (let (vol-ratio)
  (setq vol-ratio  (/ (ave-body-range date period1 param) (abs (ave-body-range date period2 param))))
  (cond  ((<= vol-ratio -1.50) 0)
         ((and (<= vol-ratio -1.3)(> vol-ratio -1.50)) 1)
         ((and (<= vol-ratio -1.15)(> vol-ratio -1.3)) 2) 
         ((and (<= vol-ratio -1.00)(> vol-ratio -1.15)) 3)
         ((and (<= vol-ratio -.85)(> vol-ratio -1.00)) 4)       
         ((and (<= vol-ratio -.70)(> vol-ratio -.85)) 5)
         ((and (<= vol-ratio -.50)(> vol-ratio -.7)) 6)
         
        ((and (<= vol-ratio 0)(> vol-ratio -.50)) 7)       
       
        ((and (<= vol-ratio .50)(> vol-ratio 0)) 8)       
        ((and (>= vol-ratio .50)(< vol-ratio .7)) 9)
        ((and (>= vol-ratio .70)(< vol-ratio .85)) 10)
        ((and (>= vol-ratio .85)(< vol-ratio 1.00)) 11)
        ((and (>= vol-ratio 1.00)(< vol-ratio 1.15)) 12)
        ((and (>= vol-ratio 1.15)(< vol-ratio 1.3)) 13)
        ((and (>= vol-ratio 1.3)(< vol-ratio 1.50)) 14)
        ((>= vol-ratio 1.50) 15))

 
))



(defun abs> (a b)
   (> (abs a)(abs b)))
;;;ranks the most recent size number of bars of data and
;;;returns and index of direction and number of the longest of the three bars
(defun body-index1 (date size)
   (let  (dmy)

    (dotimes (ith size)
      (setq dmy
         (acons (- (getd (add-mkt-days date (- ith)) 'close)
                   (getd (add-mkt-days date (- ith)) 'open))
                 (- ith) dmy)))
 

    (vsort dmy #'abs>  'car) ;(print dmy)
 
    (values 
     (cdar dmy)
   ; (cond ((>= (caar dmy) 0) (- (cdar dmy)))
   ;       ((<  (caar dmy) 0) (cdar dmy))
     ;    )
    (cond ((>= (caar dmy) 0) 1)
          ((<  (caar dmy) 0) -1)
         ))
       
))

(defun body-signal (date size)
  (let ((indx 0) dirp)
   (multiple-value-setq (indx dirp)(body-index1 date size))
   (cond ((and (= indx 0)(= dirp 1)) 1)
         ((and (= indx 0)(= dirp -1)) -1)
         (t 0))))
(defun body-range-ratio (tdate)
    (let ((tr (true-range tdate)))
   (if (zerop tr) 1
       (/ (- (getd tdate 'close)(getd tdate 'open)) (true-range tdate)))
))

(defun body-range$ (tdate)
 (round (* (calculate-point-value tdate) (- (getd tdate 'close)(getd tdate 'open)))))


(defun daily-change$ (tdate)
  (* (calculate-point-value tdate) (roc tdate 1 'close))
)
;;;calculates a points cahnge based on a dollar amt loss
;;;;used to calculate stop loss prices
(defun dollar-risk (tdate &optional (amt 1000))
   (/ amt (calculate-point-value tdate))
)
(defun risk-level (risk)
   (cond ((< risk 500) 0)
        ((< risk 1000) 1)
        ((< risk 1500) 2)
        ((< risk 2000) 3)
        ((< risk 2500) 4)
        ((< risk 3000) 5)
        ((< risk 3500) 6)
        ((< risk 4000) 7)
        ((>= risk 4000) 8))
)

#|
(defun body-range-index (tdate)
   (let ((bri (body-range-ratio tdate)))
   (cond ((> bri .667) 3)
         ((> bri .333) 2)
         ((> bri 0) 1)
         ((> bri -.333) -1)
         ((> bri -.667) -2)
         ((<= bri -.667) -3)
         )
))

(defun body-range-index (tdate)
   (let ((bri (body-range-ratio tdate)))
   (cond ((> bri .8) 5)
         ((> bri .6) 4)
         ((> bri .4) 3)
         ((> bri .2) 2)
         ((> bri 0) 1)
         ((> bri -.2) -1)
         ((> bri -.4) -2)
         ((> bri -.6) -3)
         ((> bri -.8) -4)
         ((<= bri -.8) -5)
         )
))
|#
 
(defun body-range-index (tdate)
   (let ((bri (body-range-ratio tdate)))
   (cond ((> bri .90) 5)
         ((> bri .75) 4)
         ((> bri .50) 3)
         ((> bri .25) 2)
         ((> bri .10) 1)
         ((> bri -.10) 0)
         ((> bri -.25) -1)
         ((> bri -.50) -2)
         ((> bri -.75) -3)
         ((> bri -.90) -4)
         ((<= bri -.90) -5))
))


;;;ranks the most recent 3 bars of data and
;;;returns the dirction of the longest of the three bars
(defun range-direction (date size)
   (let  (dmy bar-range)

    (dotimes (ith size)
      (setq dmy  (acons (true-range (add-mkt-days date (- ith))) (- ith) dmy)))
 

    (vsort dmy #'> 'car) ; (print dmy)
     (setq bar-range (roc (add-mkt-days date (cdar dmy)) 1))
   (cond ((>= bar-range 0) 1)
         ((< bar-range 0) -1)
         )
      
))
;;;checks Pat Aucoin's idea
;;;is there one day of the past four with a range twice as large as the others 
(defun big-range-day (tdate)
  (let ((date tdate) dmy)
   (dotimes (ith 4)
     (push (true-range date) dmy)
     (setq date (getd date 'ydate)))

    (vsort dmy #'>)
 (values  (if (> (first dmy) (* 1.5  (second dmy))) T nil) dmy)
)) 


;;;this is the 1.618 rule for reversal date
(defun williams-reversal1 (tdate &optional (size 9))
  (let* ((filt *n-filt*)(time-interval *time-interval*)(days (* size -9))
         t1 t2 t3 t4 prices index-list ignore turns
         targets  (starttime (conv-to-string (add-mkt-days tdate days) 'A)))
    ;    (declare (ignore ignore))
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv)
  ;      (format T "Start date= ~A~%" starttime)

       (loop
          (setq turns (find-all-primitives starttime (conv-to-string tdate 'P))
                days (+ days -5))
          (if (< (length turns) 5)
            (setq starttime (conv-to-string (add-mkt-days tdate days) 'A))
            (return)))

        (multiple-value-setq (prices ignore ignore ignore index-list)
            (find-all-primitives starttime  (conv-to-string tdate 'P)))
       
;        (format T "low = ~A   high = ~A  PRICES = ~A~%" low high prices)

        (setq            
             t1 (- (first index-list)(second index-list))
             t2 (- (second index-list)(third index-list))
             t3 (- (third index-list)(fourth index-list))
             t4 (- (fourth index-list)(fifth index-list))
           )
            
 ;      (push (- (+ (second index-list) (* 1.28 (+ t1 t2))) *end-index*) targets)
 ;      (push (- (+ (third index-list) (* 1.28 (+ t2 t3))) *end-index*) targets)
 ;      (push (- (+ (fourth index-list) (* 1.28 (+ t3 t4))) *end-index*) targets)
             
       (push (- (+ (first index-list) (* 1.618 (+ t1 t2 1))) *end-index*) targets)
       (push (- (+ (second index-list) (* 1.618 (+ t2 t3 1))) *end-index*) targets)
       (push (- (+ (third index-list) (* 1.618 (+ t3 t4 1))) *end-index*) targets)
        
       (push (- (+ (first index-list) (* 1.618 (+ t1 t2 t3 t4 1))) *end-index*) targets)

       (vsort targets #'<)    
       
       (setq *n-filt* filt *time-interval* time-interval)
       (setq targets (mapcar #'(lambda (s) (ceiling s 2)) targets))

       (cond  
              ((member 0 targets) 0)
              ((member -1 targets) -1)
              ((member 1 targets) 1)
              ((member -2 targets) -2)
              (t 9))

))

;;;this is the 1.28 rule for reversal date
(defun williams-reversal2 (tdate &optional (size 21))
  (let* ((filt *n-filt*)(time-interval *time-interval*)(days (* size -10))
         t1 t2 t3 t4 t5 prices index-list ignore turns
         targets  (starttime (conv-to-string (add-mkt-days tdate days) 'A)))
    ;    (declare (ignore ignore))
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv)
  ;      (format T "Start date= ~A~%" starttime)

       (loop
          (setq turns (find-all-primitives starttime (conv-to-string tdate 'P))
                days (+ days -5))
          (if (< (length turns) 6)
            (setq starttime (conv-to-string (add-mkt-days tdate days) 'A))
            (return)))

        (multiple-value-setq (prices ignore ignore ignore index-list)
            (find-all-primitives starttime  (conv-to-string tdate 'P)))
       
;        (format T "low = ~A   high = ~A  PRICES = ~A~%" low high prices)

        (setq            
             t1 (- (first index-list)(second index-list))
             t2 (- (second index-list)(third index-list))
             t3 (- (third index-list)(fourth index-list))
             t4 (- (fourth index-list)(fifth index-list))
             t5 (- (fifth index-list)(sixth index-list))
           )

       
       (push (- (+ (second index-list) (* 1.28 (+ t1 t2 1))) *end-index*) targets)
       (push (- (+ (third index-list) (* 1.28 (+ t2 t3 1))) *end-index*) targets)
       (push (- (+ (fourth index-list) (* 1.28 (+ t3 t4 1))) *end-index*) targets)
       (push (- (+ (fifth index-list) (* 1.28 (+ t4 t5 1))) *end-index*) targets)
        
;       (push (- (+ (first index-list) (* 1.618 (+ t1 t2))) *end-index*) targets)
;       (push (- (+ (second index-list) (* 1.618 (+ t2 t3))) *end-index*) targets)
;       (push (- (+ (third index-list) (* 1.618 (+ t3 t4))) *end-index*) targets)
        
       (vsort targets #'<)    
       

        (setq *n-filt* filt *time-interval* time-interval)
       (setq targets  (mapcar #'(lambda (s) (ceiling s 2)) targets))

       (cond  
              ((member 0 targets) 0)
              ((member -1 targets) -1)
              ((member 1 targets) 1)
              ((member -2 targets) -2)
              (t 9))

))
;;;no good for day trading predictions

(defun pinpoint (&optional date0 (size 8))
 (let ((filt *n-filt*)  prices times proj-list  p1 p2 p3 p4 p5 p6 p7 p8
        i0 i1 i2 i3 i4 i5 i6 i7 i8  proj-list1 (gsi5 (gann-slope-index date0 5))
        days indexes price-to-go time-to-go )
   
   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))
   (nil-tpv)
   (dotimes (ith 2)
     (setq size  (round (* 1.618 size))  proj-list nil)
     (setq days (* 4 size) *n-filt* size)

   (multiple-value-setq (prices times price-to-go time-to-go indexes )
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))

   (loop
      (setq days (+ 5 days))
     (if (< (length prices) 9)
         (multiple-value-setq (prices times price-to-go time-to-go indexes)
           (find-all-primitives (format nil "~sA" (add-mkt-days date0 (-  days)))
                                    (format nil "~sP" date0)))
         (return)))
    (setq prices (butlast prices) times (butlast times) indexes (butlast indexes))
  ; (setq times (mapcar #'getnumdate times))
 ;  (print times) (print prices) (print indexes);(print price-to-go)(print time-to-go)
      
       (setq p1 (first prices) p2 (second prices) p3 (third prices) p4 (fourth prices)
             p5 (fifth prices) p6 (sixth prices) p7 (seventh prices) p8 (eighth prices)
             i0 *end-index* i1 (first indexes) i2 (second indexes)  i3 (third indexes) i4 (fourth indexes)
             i5 (fifth indexes) i6 (sixth indexes) i7 (seventh indexes) i8 (eighth indexes))
     
       ; (format T "P1=~A P2=~A P3=~A i0=~A i1=~A i3=~A ~%" p1 p2 p3 i0 i1 i3)
         (if (or (and (> p1 p3)(> p2 p1)) (and (< p1 p3)(< p2 p1)))
             (push (ceiling  (- (/ (* (- p2 p1) (- i1 i3)) (- p1 p3)) (- i0 i1)) 2) proj-list))
         (if (or (and (> p2 p4)(> p3 p2)) (and (< p2 p4)(< p3 p2)))
             (push (ceiling  (- (/ (* (- p3 p2) (- i2 i4)) (- p2 p4)) (- i0 i2)) 2) proj-list))
         (if (or (and (> p3 p5)(> p4 p3)) (and (< p3 p5)(< p4 p3)))
             (push (ceiling  (- (/ (* (- p4 p3) (- i3 i5)) (- p3 p5)) (- i0 i3)) 2) proj-list))
         (if (or (and (> p4 p6)(> p5 p4)) (and (< p4 p6)(< p5 p4)))
             (push (ceiling  (- (/ (* (- p5 p4) (- i4 i6)) (- p4 p6)) (- i0 i4)) 2) proj-list))

         (if (or (and (> p5 p7)(> p6 p5)) (and (< p5 p7)(< p6 p5)))
             (push (ceiling  (- (/ (* (- p6 p5) (- i5 i7)) (- p5 p7)) (- i0 i5)) 2) proj-list))
        (if (or (and (> p6 p8)(> p7 p6)) (and (< p6 p8)(< p7 p6)))
             (push (ceiling  (- (/ (* (- p7 p6) (- i6 i8)) (- p6 p8)) (- i0 i6)) 2) proj-list))


     ;    (if (or (and (> p1 p5)(> p2 p1)(> p2 p4)(> p3 p5))(and (< p1 p5)(< p2 p1)(< p2 p4)(< p3 p5)))
      ;       (push (ceiling  (- (/ (* (- p2 p1) (- i1 i5)) (- p1 p5)) (- i0 i1)) 2) proj-list))
                    
        (setq *n-filt* filt) 
 
      (setq proj-list (sort proj-list '< ))
   ;  (print proj-list)
       (setq proj-list (remove-if #'(lambda (s) (or (< s -1)(> s 1))) proj-list))
       (setq proj-list1 (append proj-list1 proj-list))
      )
   
   (cond ;((and (eql (car proj-list1) -1) (plusp ave5d)) 'S-)
	 ;((and (eql (car proj-list1) -1) (minusp ave5d)) 'L-)
	 ((and (eql (car proj-list1) 0)) 'T)
	; ((and (eql (car proj-list1) 0) (minusp gsi5)) 'L)
	 ;((and (eql (car proj-list1) 1) (plusp ave5d)) 'S+)
	 ;((and (eql (car proj-list1) 1) (minusp ave5d)) 'L+)
	 (t '0))
))


;;;the date entered is current day days is the number of days to look back for a  signal
(defun pinpoint1 (tdate &optional (days 3))
 (let ((date tdate) can dirp (cntr 0))
  (setq cntr         
	(dotimes (ith days)
	  (setq can (pinpoint date 8)) ;(print can)
          (if (neql can 0) (return ith))
	  (if (= ith (1- days))(return ith))
          (setq date (getd date 'ydate))
    ))

;  (format t "~%CAN = ~A CNTR = ~A"  can cntr)
  ;;;check validity
 ; (if (and  cntr (> (- (getd (add-mkt-days tdate (- cntr)) 'close) (ave (add-mkt-days tdate (- cntr)) days 'close)) 0))
  ;    (setq dirp 'UP)(setq dirp 'DN))
;  (if (plusp (nth-value 1 (lregress (add-mkt-days tdate (-  cntr)) 4 'close))) (setq dirp 'UP)(setq dirp 'DN))
  
 ; (format t " ~A" dirp)
  
;  (cond ((and cntr (eql dirp 'up) (> (n-day-high tdate (1+ cntr)) (getd (add-mkt-days tdate (- cntr)) 'high)))
 ;       (setq can 0))
;	((and cntr (eql dirp 'DN) (< (n-day-low tdate (1+ cntr)) (getd (add-mkt-days tdate (- cntr)) 'low)))
;	 (setq can 0))
;	(t can))
  
  (values  can dirp cntr (and cntr (add-mkt-days tdate (- cntr))))
))

  

(defun reversal-dump (edate days func)
  (let ((date (add-mkt-days edate (- days))) dates)
  (dotimes (ith days)
    (if (zerop (funcall func date 9)) (push date dates))
    (setq date (getd date 'ndate))
    )
 (print dates)
))

;;;;;period may be 'day week or 'month
(defun pivot-points1 (date &optional (period 'day)) 
  (let (pivot pivot-1r pivot-2r pivot-1s pivot-2s highp (ydate (getd date 'ydate))
       lowp closep trade-days yymmdd dates)
     (cond ((eql period 'day) (setq highp (getd ydate 'high)
                                    lowp (getd ydate 'low)
                                    closep (getd ydate 'close)))
           ((eql period 'week) 
             (setq trade-days (last-weeks-trade-days date)
                   highp (max* (mapcar #'(lambda(s) (getd s 'high)) trade-days))
                    lowp (min* (mapcar #'(lambda(s) (getd s 'low)) trade-days))
                  closep (getd (car  (vsort trade-days #'> )) 'close))
              )
           ((eql period '2-bar)(setq highp (n-day-high ydate 2)
				     lowp (n-day-low ydate 2)
				     closep (getd ydate 'close)))
          ((eql period 'month) 
            (setq dates (calendar-dates-in-month date))
            (setq yymmdd 
             (dolist (ith dates)
               (if (and (week-day-p ith) (= ith date)) (return ith))
               (if (and (week-day-p ith) (/= ith date))
                         (return (add-days-to-date1 (car (last dates)) -1)))))
               (setq trade-days  (month-days (truncate yymmdd 100)) 
                highp (max* (mapcar #'(lambda(s) (getd s 'high)) trade-days))   
                lowp (min* (mapcar #'(lambda(s) (getd s 'low)) trade-days)) 
                closep (getd (car  (vsort trade-days #'> )) 'close))
            )
           )
  ; (print yymmdd)(print highp)(print lowp)(print closep)
    (setq pivot (/ (+ highp lowp closep) 3.0))
    (setq pivot-1r (- (* 2 pivot) lowp) pivot-2r (+ pivot (- highp lowp))
          pivot-1s (- (* 2 pivot) highp) pivot-2s (- pivot (- highp lowp))) 
   (values  (vsort (list pivot-2s pivot-1s pivot pivot-1r pivot-2r) #'<) lowp highp)
    ))   



;;;;;period may be 'day week or 'month
(defun pivot-points2 (date &optional (period 'day)) 
  (let (pivot pivot-1r pivot-2r pivot-1s pivot-2s highp (ydate (getd date 'ydate))
       lowp closep trade-days)
     (cond ((eql period 'day) (setq highp (getd ydate 'high)
                                    lowp (getd ydate 'low)
                                    closep (getd ydate 'close)))
           ((eql period 'week) 
             (setq trade-days (last-weeks-trade-days date)
                   highp (max* (mapcar #'(lambda(s) (getd s 'high)) trade-days))
                    lowp (min* (mapcar #'(lambda(s) (getd s 'low)) trade-days))
                  closep (getd (car  (vsort trade-days #'> )) 'close))
              )

          ((eql period 'month) 
               (setq trade-days  (last-months-trade-days date) 
                highp (max* (mapcar #'(lambda(s) (getd s 'high)) trade-days))   
                lowp (min* (mapcar #'(lambda(s) (getd s 'low)) trade-days)) 
                closep (getd (car  (vsort trade-days #'> )) 'close))
            
           ))
  ; (print yymmdd)(print highp)(print lowp)(print closep)
    (setq pivot (/ (+ highp lowp closep) 3.0))
    (setq pivot-1r (- (* 2 pivot) lowp) pivot-2r (+ pivot (- highp lowp))
          pivot-1s (- (* 2 pivot) highp) pivot-2s (- pivot (- highp lowp))) 
   (values  (vsort (list pivot-2s pivot-1s pivot pivot-1r pivot-2r) #'<) lowp highp)
    ))   


(defun last-weeks-trade-days (tdate)
   (let ((date tdate) dates last-day-week)
    
    (setq last-day-week
    (loop
       (if (equal (day-of-week date) "FRIDAY") (return date))
       (if (<= (subtract-dates (getd date 'ydate) date) 2)
           (setq date (getd date 'ydate)) (return (getd date 'ydate))))
    )
  ;  (print last-day-week)
    (setq dates (cons last-day-week dates) date last-day-week)
     (loop
       (if (<= (subtract-dates (getd date 'ydate) date) 2)
           (setq date (getd date 'ydate) dates (cons date dates))
           (return)))
   dates
))

(defun last-months-trade-days (tdate)
  (let (last-day-month)
    (setq last-day-month (getd (car (month-days (truncate tdate 100))) 'ydate))
   
    (month-days (truncate last-day-month 100))))


;;;target tolerance for weekly and monthly pivots
(defun pivot-tol (date)
   (volatility date 21 .2))  


;;;typ may be day week or month
(defun pivot-turn (date &optional (typ 'week))
  (let ((phigh (getd date 'high))(plow (getd date 'low))(pclose (getd date 'close))
         pivots s1 s2 s3 pp r1 r2 r3 tol whigh wlow)
   (setq pivots (pivot-points1 date typ) tol (pivot-tol date) 
         whigh (case typ
	        (2-bar (nth-value 2 (pivot-points1 date '2-bar)))
                (week  (n-day-high date 5));(second (current-week-high-low date)))
                (month (n-day-high date 20));(second (current-month-high-low date)))
                (day phigh))
         wlow (case typ
	        (2-bar (nth-value 1 (pivot-points1 date '2-bar)))
                (week (n-day-high date 5));(first (current-week-high-low date)))
                (month (n-day-low date 20));(first (current-month-high-low date)))
                (day plow))
        )
   (setq s2 (first pivots) s1 (second pivots) pp (third pivots) r1 (fourth pivots) r2 (fifth pivots)) 

   (setq s3 (- wlow (* 2 (- whigh pp))) r3 (+ whigh (* 2 (- pp wlow))))
  ; (format T "~% pp= ~A   wlow= ~A  plow= ~A tol= ~A~%" pp wlow plow tol)
   (cond ((and (< (abs (- phigh pp)) tol)
               (< whigh (+ pp tol))) 'R0)
    
         ((and (< (abs (- phigh r1)) tol);(< (abs (- pclose r1)) tol)
               (< whigh (+ r1 tol))) 'R1)
         ((and (< (abs (- phigh r2)) tol);(< (abs (- pclose r2)) tol)
               (< whigh (+ r2 tol))) 'R2)
         ((and (< (abs (- phigh r3)) tol);(< (abs (- pclose r2)) tol)
               (< whigh (+ r3 tol))) 'R3)
         
         ((and (< (abs (- plow pp)) tol)
               (> wlow (- pp tol)))  'S0)
        
         ((and (< (abs (- plow s1)) tol);(< (abs (- pclose s1)) tol)
               (> wlow (- s1 tol)))  'S1)
         ((and (< (abs (- plow s2)) tol);(< (abs (- pclose r2)) tol)
               (> wlow (- s2 tol))) 'S2)
         ((and (< (abs (- plow s3)) tol);(< (abs (- pclose r2)) tol)
               (> wlow (- s3 tol))) 'S3)         
         ((< (abs (- pclose (third pivots))) tol) 'PP) 
         ((and (> whigh (+ r3 tol))(> pclose (+ r3 tol))) 'H)
         ((and (< wlow (- s3 tol))(< pclose (- s3 tol))) 'L)
         (t 0))
))
;;;flawed logic
(defun pivot-turn-composite (tdate days)
  (let (piv (date tdate) (result 0) ave5d)
   
         (dotimes (ith days)
            (setq piv (pivot-turn date 'month) ave5d (- (getd date 'close)(ave date 5)))
            (cond ((and (member piv '(R3 H PP))(plusp ave5d))(return (setq result 'S)))
                  ((and (member piv '(S3 L PP))(minusp ave5d))(return (setq result 'L)))
	          (t (setq date (getd date 'ydate))))
            )
;	 (setq date tdate)
;          (dotimes (ith days)
;            (setq piv (pivot-turn date 'week))
;            (cond ((member piv '(R1 R2 R3))(return (setq result -1)))
;                  ((member piv '(S1 S2 S3))(return (setq result 1)))
;                  (t (setq date (getd date 'ydate))))
;              )
   result
))

(defun pivot-points (date)
  (let (pivot pivot-1r pivot-2r pivot-1s pivot-2s 
       (highp (getd date 'high))
       (lowp (getd date 'low)))
    (setq pivot (/ (+ highp lowp (getd date 'close)) 3))
    (setq pivot-1r (- (* 2 pivot) lowp) pivot-2r (+ pivot (- highp lowp))
          pivot-1s (- (* 2 pivot) highp) pivot-2s (- pivot (- highp lowp))) 
    (vsort (list pivot-2s pivot-1s pivot pivot-1r pivot-2r) #'<)
    ))      

(defun pivot-swing (date)
   (let ((pvt (pivot-turn date 'week)))
      (case pvt
        ((S1 S2 R0) 1)
        ((R1 R2 S0) -1)
        (9 9)
        (otherwise 0))
)) 
;;;
;;;returns the location of today's close relative to yesterday's pivot points
;;;typ is 'week 'day or 'month
(defun pivot-index (date typ)
 (let ((pivots (pivot-points2 date typ))(index (getd date 'close))(tol (pivot-tol date))
        s2 s1 pp r1 r2 )
   (setq s2 (first pivots) s1 (second pivots) pp (third pivots) r1 (fourth pivots) r2 (fifth pivots))
 (cond ((<= index (- s2 tol)) -5)
       ((<= index (+ s2 tol)) -4);;;within tol of s2
       ((<= index (- s1 tol)) -3)
       ((<= index (+ s1 tol)) -2);;within tol of s1
       ((<= index pp) -1)
     
       ((<= index (- r1 tol)) 1)
       ((<= index (+ r1 tol)) 2);;within tol of r1
       ((<= index (- r2 tol)) 3)
       ((<= index (+ r2 tol)) 4);;;within tol of r2
       (t 5))

))


(defun pivot-points-weekly (tdate) 
  (let (pivot pivot-1r pivot-2r pivot-1s pivot-2s highp lowp highs lows
        (date tdate) (weekday (day-of-week tdate)) weekly-close)
;;;first find the most recent Friday.
  (loop
   (if (equal weekday "FRIDAY") (return)
       (setq date (add-days-to-date1 date -1)  
             weekday (day-of-week date))
       ))

   (setq tdate date)
   
  (loop
    (when (readt2 date)
          (setq weekly-close (getd date 'close))
          (return))
    (setq date (add-days-to-date1 date -1)))
    
   (setq date tdate)

  (dotimes (ith 6) 
     (when (readt2 date) 
       (push (getd date 'high) highs)
       (push (getd date 'low) lows))
      (setq date (add-days-to-date1 date -1))) 
   

  (setq highp (max* highs) lowp (min* lows))
  
    (setq pivot (/ (+ highp lowp weekly-close) 3))
    (setq pivot-1r (- (* 2 pivot) lowp) pivot-2r (+ pivot (- highp lowp))
          pivot-1s (- (* 2 pivot) highp) pivot-2s (- pivot (- highp lowp))) 
    (list pivot-2s pivot-1s pivot pivot-1r pivot-2r)
        ))      

(defun pivot-points-monthly (tdate)
  (let (pivot pivot-1r pivot-2r pivot-1s pivot-2s highp lowp highs lows current-month
       (tmonth (ngetnummo tdate)) (date tdate) 
       current-year monthly-close)
;;;find end of the month
;;;Is Tdate the last market day of the month?
;;;if not use the prior month
;;;all I can do is test if subsequent dates are weekdays.
  (loop 
    (when (or (> (ngetnumday date) 30) 
              (neql (ngetnummo date) tmonth)) 
         (setq current-month tmonth)(return)) 
              
    (setq date (add-days-to-date1 date 1))
    
    (when (neql (ngetnummo date) tmonth) 
         (setq current-month tmonth)(return)) 
      
    (when (member (day-of-week date) '("MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY") :test #'equal)
      (setq current-month (- tmonth 1))
      (return)))
      
      
    (if (zerop current-month)
        (setq current-year (- (ngetnumyear date) 1) current-month 12)
        (setq current-year (ngetnumyear date)))
    
             
  (dolist (ith (month-days (+ (* 100 current-year) current-month)))
      (push (getd ith 'high) highs) (push (getd ith 'low) lows))
  
  (setq monthly-close (getd (car (last (month-days (+ (* 100 current-year) current-month)))) 'close))
  
  
  (setq highp (max* highs) lowp (min* lows))
   
    (setq pivot (/ (+ highp lowp monthly-close) 3))
    (setq pivot-1r (- (* 2 pivot) lowp) pivot-2r (+ pivot (- highp lowp))
          pivot-1s (- (* 2 pivot) highp) pivot-2s (- pivot (- highp lowp))) 
    (list pivot-2s pivot-1s pivot pivot-1r pivot-2r)

))

(defun reversal-low-compositep (tdate)
    (or (eql (reversal-dayp tdate) 'UP1)
        ;(eql (candle-swing tdate) 1)
        (member (wpp tdate) '(DDD DUU OUL DDU ODL))
       ; (member (pivot-turn tdate 'week) '(S0 S1 S2))
        )) 


(defun reversal-high-compositep (tdate)
    (or (eql (reversal-dayp tdate) 'DN1)
       ; (eql (candle-swing tdate) -1)
        (member (wpp tdate) '(UUU UDD ODH UUD OUH))
      ;  (member (pivot-turn tdate 'week) '(R0 R1 R2))
         )) 



;;This function returns the reward to risk ratio for daytrades
;;The dir argument is either 1 for long or -1 for short for the trade direction

(defun daytrade-reward-risk (tdate dir)
   (let (ave4 entry-short entry-long cover-short cover-long risk)

       (setq ave4 (ave tdate 4 'pivot)
             cover-short (exp (- (log ave4) (* *objective-factor* (volatility-log tdate 60 1))))
             cover-long (exp (+ (log ave4) (* *objective-factor* (volatility-log tdate 60 1))))
         )

   (multiple-value-setq (entry-short entry-long)(vprices tdate 4 *entry-factor* 1))

    (setq risk (abs (* *stop-loss-day* (/ (* .5 (- entry-long entry-short)) *entry-factor*))))
 ;  (format T "entry-short= ~A cover-short= ~A risk= ~A" entry-short cover-short risk)
   (case dir
    (-1 (my-round (/ (- entry-short cover-short) risk) 3))
    (1 (my-round (/ (- cover-long entry-long) risk) 3))
   )
))
;;;these function returns the high and low for the month up to the tdate
;;;if the tdate is the last day of the month then just the close of tdate is returned
;;;for the high and low.
(defun current-month-high-low (tdate)
  (let ((days (month-days (truncate tdate 100))) highs lows) 
  (setq days (member tdate (reverse days)))
  (dolist (ith days)
    (push (getd ith 'high) highs)
    (push (getd ith 'low) lows))
    
   (list (min* lows)(max* highs))
   ))
#|
(defun current-week-high-low (tdate)
  (let ((date tdate) highs lows)
    (cond ((equal (day-of-week date) "FRIDAY")
           (push (getd date 'close) highs)(push (getd date 'close) lows))
          (t (loop 
                  (when (readt2 date)
                        (push (getd date 'high) highs)
                        (push (getd date 'low) lows))
                  (setq date (add-days-to-date1 date -1))
                  (if (member (day-of-week date) '("SATURDAY" "SUNDAY") :test #'equal)(return)))))

           
    (list (min* lows)(max* highs))          

  ))
|#
(defun current-week-high-low (tdate)
 (let ((date tdate) highs lows)
   (loop 
    
     (when (readt2 date)
           (push (getd date 'high) highs)
           (push (getd date 'low) lows))
      (setq date (add-days-to-date1 date -1))
      (if (member (day-of-week date) '("SATURDAY" "SUNDAY") :test #'equal)(return)))
   
 (list (min* lows)(max* highs))
))


(defun two-bar (date)
  (let ((date-low (getd date 'low))(date-high (getd date 'high))
        (ydate-low (getd (getd date 'ydate) 'low)) ll hh
        (ydate-high (getd (getd date 'ydate) 'high)))

      (setq ll (- (* 2 date-low) ydate-low)
            hh (- (* 2 date-high) ydate-high))
           
    (values ll hh )
  ))


(defun 2-bar (date)
  (let ((date-low (getd date 'low))(date-high (getd date 'high))
        (ydate-low (getd (getd date 'ydate) 'low)) ll lh hl hh pp
        (ydate-high (getd (getd date 'ydate) 'high)))

      (setq ll (- (* 2 date-low) ydate-low)
            hh (- (* 2 date-high) ydate-high)
	    pp (/ (+ (max date-high ydate-high)(min date-low ydate-low)) 3.0)
            hl (- (* 2 date-low) ydate-high)
            lh (- (* 2 date-high) ydate-low))
    (vsort (list hl ll pp hh lh) #'<)
  ))
(defun 2-bar-index (date)
 (let (pivots (index (getd date 'close)))
 (setq pivots (2-bar (getd date 'ydate)))
 (cond ((<= index (first pivots)) -3)
       ((<= index (second pivots)) -2)
       ((<= index (third pivots)) -1)
       ((<= index (fourth pivots)) 1)
       ((<= index (fifth pivots)) 2)
       (t 3))

))
(defun 2-bar-weekly-index (date)
 (let (pivots (index (getd date 'close)))
 (setq pivots (2-bar-weekly (getd date 'ydate)))
 (cond ((<= index (first pivots)) -2)
       ((<= index (second pivots)) -1)
       ((<= index (third pivots)) 0)
       ((<= index (fourth pivots)) 1)
       (t 2))

))


;;;;
(defun 2-bar-weekly (date)
  (let (ll lh hl hh highp lowp yhighp ylowp (weekday (day-of-week date))
        (counter 0))
       
    (loop
       (if (and (not (equal weekday "FRIDAY"))(< counter 4))
           (setq date (getd date 'ydate) counter (+ 1 counter) 
                 weekday (day-of-week date))
       (return)))
    (setq highp (n-day-high date 5) lowp (n-day-low date 5))
    
    (setq date (getd date 'ydate) weekday (day-of-week date) counter 0)     
    (loop
       (if (and (not (equal weekday "FRIDAY"))(< counter 4))
           (setq date (getd date 'ydate) counter (+ 1 counter) 
                 weekday (day-of-week date))
       (return)))
    (setq yhighp (n-day-high date 5) ylowp (n-day-low date 5))  
   
     (setq ll (- (* 2 lowp) ylowp)
           hh (- (* 2 highp) yhighp)
           hl (- (* 2 lowp) yhighp)
           lh (- (* 2 highp) ylowp))
    (list hl ll hh lh)          
  ))  
  
  
(defun reversal-weekp (date)
  (let (highp lowp yhighp ylowp (weekday (day-of-week date))
        (counter 0) closep yclosep)
       
    (loop
       (if (equal weekday "FRIDAY")(return)
           (setq date (getd date 'ydate)  
                 weekday (day-of-week date)))
        (if (equal weekday "THURSDAY")(return))         
                 
                 )
    (if (equal weekday "FRIDAY") (setq counter 5) (setq counter 4))
    (setq highp (n-day-high date counter) lowp (n-day-low date counter) closep (getd date 'close))
 ;      (print date)(print closep)
    (setq date (getd date 'ydate) weekday (day-of-week date) counter 0)     
    (loop
       (if (equal weekday "FRIDAY")(return)
           (setq date (getd date 'ydate)  
                 weekday (day-of-week date)))
        (if (equal weekday "THURSDAY")(return))         
       )
    (if (equal weekday "FRIDAY") (setq counter 5) (setq counter 4))   
    (setq yhighp (n-day-high date counter) ylowp (n-day-low date counter) yclosep (getd date 'close))  
  ;  (print date)(print yclosep)
    (cond ((and (> highp yhighp)(< closep yclosep)) 'WS2)
          ((and (< lowp ylowp)(> closep yclosep)) 'WB2)) 
               
  ))
;;;measures how far the close is from a moving average in 
;;;relation to the volatility (average true range)
(defun range-dev (date size)
 (truncate  (/ (- (getd date 'close) (ave date size))
               (volatility date size 1)) .25))

#|
(defun roc (tdate size &optional (typ 'close))
   (float  (- (if (eql typ 'pivot)(third (pivot-points tdate))
                                 (getd tdate typ))
         (if (eql typ 'pivot)(third (pivot-points (add-mkt-days tdate (- size))))
                    (getd (add-mkt-days tdate (- size)) typ))))
)
|#
;;;;rate of change takes date number of days and typ
;;;typ may be pivot for pivot point
;;;or ss for slow stochastic or cci for commodity channel index
;;;or expma is for exponential moving average
(defun roc (tdate size &optional (typ 'close)(param 21))
   (float  (cond ((eql typ 'pivot)
                  (- (third (pivot-points tdate))(third (pivot-points (add-mkt-days tdate (- size))))) 
                  )
		 ((eql typ 'adx)(- (adx tdate param)(adx (add-mkt-days tdate (- size)) param)))
		 ((eql typ 'di)(- (nth-value 1 (adx tdate param))
				  (nth-value 1 (adx (add-mkt-days tdate (- size)) param))) )
                 ((eql typ 'ss)
                  (- (slow-stochastic tdate param)(slow-stochastic (add-mkt-days tdate (- size)) param)))
                 ((eql typ 'fs) ;;fast stochastic %D
                  (- (fast-stochastic tdate param)(fast-stochastic (add-mkt-days tdate (- size)) param))) 
                 ((eql typ 'fstd) ;;fast stochastic %K
                  (- (nth-value 2 (fast-stochastic tdate param))(nth-value 2 (fast-stochastic (add-mkt-days tdate (- size)) param)))) 
              
                 ((eql typ 'ma)
                  (- (ave tdate param )
                     (ave (add-mkt-days tdate (- size)) param ))) 
                 ((eql typ 'expma)
                  (- (ave-exp tdate param 'close)
                     (ave-exp (add-mkt-days tdate (- size)) param 'close))) 
                
                  ((eql typ 'cci)
                  (- (commodity-channel-index tdate param )
                     (commodity-channel-index (add-mkt-days tdate (- size)) param ))) 
                              
                  ((eql typ 'rsi)
                  (- (rsi tdate param)(rsi (add-mkt-days tdate (- size)) param)))
                 
                 (t (- (getd tdate typ)(getd (add-mkt-days tdate (- size)) typ)))))
)



(defun roc-div-index (tdate size)
  (let ((rocp (roc tdate size 'close))(roca (roc tdate size 'rsi 21)))
  
  (cond  ((and (plusp rocp)(plusp roca)) 2)
          ((and (minusp rocp)(minusp roca)) -2)
          ((and (plusp rocp)(minusp roca)) 1)
          ((and (minusp rocp)(plusp roca)) -1)
          (t 0))
    
   ))

(defun roc% (date &optional (size 13) (type 'close))

     (* 100.0 (/ (roc date size type)(getd date 'close))))

(defun ep-roc% (date &optional (size 13) (type 'close))

  (* 100.0 (/ (ep-roc date size 108 type)(getd date 'close))))

(defun roc%-percentile (tdate size)
  (let ((date tdate) list)
    
  (dotimes (ith 200)
    (push (ep-roc% date size) list)
    (setq date (getd date 'ydate)))
 ; (values (min* list)(percentile low list)(mean list)(median list)(percentile high list)
;	  (max* list))
  (truncate (* 100 (percentile1 (ep-roc% tdate size) list)) 10)
  ))
(defun accel-percentile (tdate size &optional (low .1) (high .9))
  (let ((date tdate) list)
    
  (dotimes (ith 200)
    (push (accel date size) list)
    (setq date (getd date 'ydate)))
;  (values (min* list)(percentile low list)(mean list)(median list)(percentile high list)
;	  (max* list))
 (truncate (* 100 (percentile1 (accel tdate size) list)) 10)
  ))

(defun bpl-index (date size)
  (let (bpl targ slope p0 p1 p2 p3 p4 t0 t1 t2)
   
    (multiple-value-setq (bpl targ p0 p1 p2 p3 p4 t0 t1 t2 )(channel-trend date size))
    (setq slope (plusp (/ (* 2 (- p1 p3)) (+ t1 t2)))) ;;;factor of 2 is to change from prices to days
    (cond ((and slope (> targ bpl)(> p0 targ)) 'U3)
	  ((and (not slope) (> targ bpl)(> p0 targ)) 'D3)
          ((and slope (> targ bpl)(> p0 bpl)) 'U2)
	  ((and (not slope) (> targ bpl)(> p0 bpl)) 'D2)
          ((and slope (> targ bpl)) 'U1)
	  ((and (not slope) (> targ bpl)) 'D1)
          ((and slope (< targ bpl)(< p0 targ)) '-U3)
	  ((and (not slope) (< targ bpl)(< p0 targ)) '-D3)
          ((and slope (< targ bpl)(< p0 bpl)) '-U2)
	  ((and (not slope) (< targ bpl)(< p0 bpl)) '-D2)
          ((and slope (< targ bpl)) '-U1)
	  ((and (not slope) (< targ bpl)) '-D1)
         )
))
;;;size is a filter size
(defun bpl-slope (date size)
 (let (bpl targ p0 p1 p2 p3 p4 t0 t1 t2)
  (multiple-value-setq (bpl targ p0 p1 p2 p3 p4 t0 t1 t2 )(channel-trend date size))
 (/ (* 2 (- p1 p3)) (+ t1 t2)) ;;;factor of 2 is to change from prices to days
))

(defun accel (date size1 &optional (size2 1) (typ 'pivot))
  (- (ep-roc date size1 108 typ) (ep-roc (add-mkt-days date (- size2)) size1 108 typ)))

(defun accel1 (date size1 &optional (size2 1) (typ 'close))
  (- (nth-value 1 (lregress date size1 typ))
     (nth-value 1 (lregress  (add-mkt-days date (- size2)) size1 typ))))
     
(defun accel-context (date size1 &optional (size2 1))
  (let ((trend (roc date size1 'close))
        (acc (accel date size1 size2 'close)))
  (cond ((and (plusp trend) (plusp acc)) 2)
        ((and (plusp trend)(minusp acc)) 1)
        ((and (minusp trend)(plusp acc)) -1)
        ((and (minusp trend)(minusp acc)) -2)
        (t 0))))
     
(defun accel-context1 (date size1 &optional (size2 1))
  (let ((trend (vsignals1 date))
        (acc (accel date size1 size2 'close)))
  (cond ((and (eql trend 'LONG) (plusp acc)) 2)
        ((and (eql trend 'LONG)(minusp acc)) 1)
        ((and (eql trend 'SHORT)(plusp acc)) -1)
        ((and (eql trend 'SHORT)(minusp acc)) -2)
        (t 0))))

;;;num4 can be 1 or more.(days includes the current day.
(defun accel-direction (tdate &optional (num1 5)(num4 2)(typ 'roc))
  (let ((date tdate) ccis )
   
  (dotimes (ith (1+ num4))
   (cond ((eql typ 'roc)
          (push (ep-roc date num1 (* 10 num1)) ccis))
         ((eql typ 'cci)
          (push (commodity-channel-index date num1) ccis)))
   (setq date (getd date 'ydate))
  )
  (setq ccis (reverse ccis))

  (cond
        ((< (car ccis) (min* (cdr ccis))) 'DN)
        ((> (car ccis) (max* (cdr ccis))) 'UP)
        (t (accel-direction (getd tdate 'ydate) num1 num4)))

 ))
 ;;;;
(defun ave (date size &optional (typ 'close))
 (block nil
  (let ((tdate date) (result 0) (roll 0)) 
   
    (dotimes (ith size (float (/ result size)))  
      (if (and (eql typ 'volume)(not (getd tdate 'volume))) (return 0))   
      (setq result (+ result roll (if (eql typ 'pivot)(third (pivot-points tdate))
                                 (getd tdate typ))))
   ;   (if (and (getd tdate 'rollover)(neql typ 'volume))
    ;       (setq roll (+ roll (getd tdate 'rollover))))                           
      (setq tdate (getd tdate 'ydate)))   
  )))
(defun ave-index (tdate period)
  (let ((av (ave tdate period 'close)))
       (cond ((> (getd tdate 'close) av) 1)
             ((< (getd tdate 'close) av) -1)
             (t 0)))) 

;;;;period1 is the moving average parameter
;;;period2 is the rate of change parameter
;;;;used as a componnent of KST (KNOW SURE THING) by Martin Pring
(defun ave-roc (tdate period1 period2)
   (let ((sum 0)(date (add-mkt-days tdate (1+ (- period1)))))
   (dotimes (ith period1)
      (setq sum (+ (roc date period2) sum)
        date (getd date 'ndate)))
    (float (/ sum period1))
))

(defun kst (tdate)

 (+ (* 1 (ave-roc tdate 10 10))
    (* 2 (ave-roc tdate 10 15))
    (* 3 (ave-roc tdate 10 20))
    (* 4 (ave-roc tdate 15 30)))
)
;;;;1 means uoward -1 downward
(defun kst-trend (tdate days)
  (if (plusp (- (kst tdate)(kst (add-mkt-days tdate (- days))))) 1 -1))

;;;moving average high/low trend
(defun mvhl-trend (date &optional (days 3))
  (let* ((date-1 (getd date 'ydate))
        (avel (ave date-1 days 'low))(aveh (ave date-1 days 'high))
        ;(avel (n-day-low date-1 days))(aveh (n-day-high date-1 days))
        (thigh (getd date 'high))(tlow (getd date 'low)))

  (cond ;((and (>= thigh (+ aveh (* 2 (- aveh avel))))
        ;;      (<= tlow (- avel (* 2 (- aveh avel))))) 'FT)

       ; ((>= thigh (+ aveh (* 2 (- aveh avel)))) 'XUP)
       ; ((<= tlow (- avel (* 2 (- aveh avel)))) 'XDN)


       ; ((and (>= thigh (+ aveh (- aveh avel)))
       ;       (<= tlow (- avel (- aveh avel)))) 'FT)

       ; ((>= thigh (+ aveh (- aveh avel))) 'SUP)
       ; ((<= tlow (- avel (- aveh avel))) 'SDN)

        ((and (> thigh aveh)(>= tlow avel)) 1) 
        ((and (<= thigh aveh)(< tlow avel)) -1)

        ((and (> thigh aveh)(< tlow avel)
        ; (if (> (getd date 'open)(getd date 'close)) -1 1))
          (>= (getd date 'close) aveh)) 1)
        ((and (> thigh aveh)(< tlow avel)
           (<= (getd date 'close) avel)) -1)
        (t (mvhl-trend date-1)))))

;;;moving average high/low trend
(defun keltner-trend (date)
  (let* ((date-1 (getd date 'ydate))(kave (ave date-1 3 'pivot))
         (kvol (volatility date-1 21 1))
         (aveh (+ kave kvol))(avel (- kave kvol))
        (thigh (getd date 'high))(tlow (getd date 'low)))

  (cond ((and (>= thigh (+ aveh (* 2 kvol)))
              (<= tlow (- avel (* 2 kvol)))) 'FT)

        ((>= thigh (+ aveh (* 2 kvol))) 'XUP)
        ((<= tlow (- avel (* 2 kvol))) 'XDN)


        ((and (>= thigh (+ aveh kvol))
              (<= tlow (- avel kvol))) 'FT)

        ((>= thigh (+ aveh kvol)) 'SUP)
        ((<= tlow (- avel kvol)) 'SDN)

        ((and (> thigh aveh)(>= tlow avel)) 'UP) 
        ((and (<= thigh aveh)(< tlow avel)) 'DN)

        ((and (> thigh aveh)(< tlow avel)) 'FT)
        (t (keltner-trend date-1)))))


(defun keltner-trend1 (date)
 (let ((date-1 (getd date 'ydate))(size 3) (diff 0))
  (setq diff (- (ave date size 'pivot)(ave date-1 size 'pivot)))
  (cond ((plusp diff) 'UP)
        ((minusp diff) 'DN)
        (t (keltner-trend1 date-1)))))



;;;high and low target are returned
(defun vtargets (date)
 (let* ((vol63 (volatility date 63 1.618))(close0 (getd date 'close)))
  (values (my-pretty-price (+  close0 vol63))
          (my-pretty-price (-  close0 vol63)))))
;;;
(defun volume-check (date markets &optional (size 21))
  (let (vols )
   (maind-x)(set-cat-list)
 (dolist (ith markets)
   (set-market ith)
;   (format T "~%~A   ~D" ith (round (ave date size 'volume)))
   (setq vols (acons (round (ave date size 'volume)) ith vols))
     
    )
   (vsort vols #'> 'car)
   (dolist (kth vols)
     (format T "~%~A ~A" (cdr kth)(car kth)))
   (subseq (mapcar #'cdr vols) 0 20)  
))


;;;;This is the special case because of the
;;; mistake in storing openint in the database
(defun ave1 (date size &optional (typ 'close))
  (let ((tdate date) (result 0) (roll 0))  
    (dotimes (ith size (float (/ result size)))      
      (setq result (+ result roll (car (getd tdate typ))))
      (if (getd tdate 'rollover) (setq roll (+ roll (getd tdate 'rollover))))                           
      (setq tdate (getd tdate 'ydate)))   
  ))


;;;defines an exponential moving average

(defun ave-exp (tdate size &optional (typ 'close))
  (let* ((date (add-mkt-days tdate (* -10 size))) (factor (/ 2 (+ size 1)))
         (result (ave date size 'close)) (roll 0))  
    (dotimes (ith (* 3 size) result)    
      (setq date (getd date 'ndate)) 
    ;  (when (getd date 'rollover)
    ;        (setq roll (+ roll (getd date 'rollover))
    ;              result (+ roll result))
    ;        )
      (setq result (+ (* (- 1 factor) result) 
                      (* factor (cond ((eql typ 'pivot)(third (pivot-points date)))
                                      ((eql typ '+DM)(/ (nth-value 0 (dm date))
							(volatility date size 1)))
				       ((eql typ '-DM)(/ (nth-value 1 (dm date))
						         (volatility date size 1)))
				      ((eql typ 'DI)(di date size))
				      (t (getd date typ))))))                      
      )
  ))
;;;given a list it returns the exponential moving average


(defun rsi (stopdate size)
  (block nil
  (let ((changes nil)(up-col nil)(dn-col nil)(up-col-ave nil)(dn-col-ave nil)
         (tdate stopdate))

    (dotimes (ith (* 7 size))
      (push (- (getd tdate 'close)(getd (getd tdate 'ydate) 'close)) changes)
      (setq tdate (getd tdate 'ydate)))
    
;;;need to keep the positive and negative sums for size observations
;;;need first size number of changes
    (dotimes (jth size)
      (cond ((plusp (car changes))
	     (push (car changes) up-col)
	     (push 0 dn-col))
	    ((minusp (car changes))
	     (push (car changes) dn-col)
	     (push 0 up-col))
	     ((zerop (car changes))
	      (push 0 dn-col)(push 0 up-col)))
       (pop changes))
    (setq up-col-ave (/ (list-sum up-col) size)
	  dn-col-ave (/ (list-sum dn-col) size))
	  
  
   (dolist (jth changes)
     (setq up-col-ave (/ (+ (* up-col-ave (1- size)) (if (plusp jth) jth 0)) size)
           dn-col-ave (/ (+ (* dn-col-ave (1- size)) (if (minusp jth) jth 0)) size)))
           
   (my-round (* 100 (/ up-col-ave (+ up-col-ave (- dn-col-ave)))) 1) 
   
      ))) 


(defun rsi-signal (tdate size days )
 (let (rsio)
   
   (dotimes (ith days 0)
     (setq rsio (rsi (add-mkt-days tdate (- ith)) size))
     (cond ((>= rsio 70) (return -1))
           ((<= rsio 30) (return 1)) 
           (t 0)))

))


;;;;the size is for computing the rsi
;;;the days is for the number of days to cumulate      
(defun rsi-high-low (stopdate size days)
  (let (rsis)
   (dotimes (ith days)
     (push (rsi (add-mkt-days stopdate (- ith)) size) rsis))
     (values
     (apply #'max rsis) (apply #'min rsis))
))

(defun rsi5-index (tdate)
  (let (rsi5 rsi10)
  (setq rsi5 (rsi tdate 5) rsi10 (rsi tdate 10))
  (cond ((and (>= rsi10 70) (>= rsi5 70)) 4)
        ((and (>= rsi10 70) (<= rsi5 30)) 2)
        ((and (>= rsi10 70) ) 3)
        ((and (<= rsi10 30) (>= rsi5 70)) -2)
        ((and (<= rsi10 30) (<= rsi5 30)) -4)
        ((and (<= rsi10 30) ) -3)
        ((and (< rsi10 70)(>= rsi10 30)(>= rsi5 70)) 1)
        ((and (< rsi10 70)(>= rsi10 30)(<= rsi5 30)) -1)
        ((and (< rsi10 70)(>= rsi10 30)) 0)

        (t 0))
))

(defun rollover-window (tdate days1 days2)
  (let ((startday (add-mkt-days tdate (- days1)))
        (endday (add-mkt-days tdate days2))(counter days1))
      (loop  
          (if (numberp (getd startday 'rollover)) (return counter))
          (if (stringp (getd startday 'rollover)) (return nil))
          (ifn startday (return nil))
          (if (> startday endday) (return nil))          
          (setq counter (1- counter) startday (getd startday 'ndate))))) 


;;;the rate is the parameter for the momentum in days
;;;the size is the filter size (half days)

(defun momentum-divergence (tdate &optional (size 9)(rate 14))
 (let ((filt *n-filt*)(time-interval *time-interval*) times mo1 mo2 
         p0 p2 prices low high dirp  p1 p3 ;p4 
         (stopdate (conv-to-string tdate 'P))
        (starttime (conv-to-string (add-mkt-days tdate (* size -50)) 'A)))      
         
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv) 
          
        (multiple-value-setq (prices times)
                (find-all-primitives starttime stopdate))
 ;   (format T "~%PRICES= ~A ~%  TIMES= ~A~%" prices times)
        (setq dirp (getv 0 DR))                
        (setq low (getv 0 LP) high (getv 0 HP)) 
        (if (eql dirp 'UP) (setq p0 high)(setq p0 low))
        (setq p2 (second prices) p1 (first prices) p3 (third prices)); p4 (nth 3 prices))
  
        
        (setq *time-interval* time-interval *n-filt* filt)  
        (setq mo1 (- (roc (getnumdate (first times)) rate)(roc (getnumdate (third times)) rate)))
       (setq mo2 (- (roc tdate rate)(roc  (getnumdate (second times))  rate)))
  ;      (format t "p0= ~A p1= ~A p2= ~A p3= ~A mo1= ~A 1st= ~A 3rd=~A~%" p0 p1 p2 p3 mo1 (first times)(third times) )
     ;;;;dirp is the direction of the primitive
  
        (cond ((and (minusp mo2)(> p0 p2)) 'DN)
              ((and (plusp mo2)(< p0 p2)) 'UP)
              ;((and (or (minusp mo1)(minusp mo2))(<= p1 p3)) 'DOWN)
              ;((and (or (plusp mo1)(plusp mo2))(>= p1 p3)) 'UP)
              
              ((and (eql dirp 'DOWN)(minusp mo1)(> p1 p3)) 'DN1)
              ((and (eql dirp 'UP)(plusp mo1)(< p1 p3)) 'UP1)
              ((and (eql dirp 'DOWN)(plusp mo1)(< p1 p3)) 'UP2)
              ((and (eql dirp 'UP)(minusp mo1)(> p1 p3)) 'DN2)
	      (t nil))
	      ))   
	      	      

(defun momentum-divergence1 (tdate &optional (size 6)(rate 15) (size2 3))
 (let ((filt *n-filt*)(time-interval *time-interval*) times mo1 mo2
         p0 p2 prices low high dirp   p4 price-to-go (ctr 0)
         (stopdate (conv-to-string tdate 'P))
        (starttime (conv-to-string (add-mkt-days tdate (* size -50)) 'A)))       
         
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv) 
            
        (loop
           (multiple-value-setq (prices times price-to-go)
                    (find-all-primitives starttime stopdate))
           (incf ctr)
           (ifn (nth 3 times) 
                (setq starttime (conv-to-string (add-mkt-days tdate (- (* size -50) ctr)) 'A)) (return)))

        (setq dirp (getv 0 DR))                
        (setq low (getv 0 LP) high (getv 0 HP)) 
        (if (eql dirp 'UP) (setq p0 high)(setq p0 low))
        (setq p2 (second prices)  p4 (nth 3 prices))
          
        
        (setq *time-interval* time-interval *n-filt* filt)  
        (setq mo1 (- (macd tdate size rate size2)(macd (getnumdate (second times)) size rate size2)))
        (setq mo2 (- (macd tdate size rate size2)(macd (getnumdate (nth 3 times)) size rate size2)))
        ; (format t "p0= ~A p1= ~A p2= ~A p3= ~A mo1= ~A mo2= ~A" p0 p1 p2 p3 mo1 mo2)
       
       (values (cond ((and (eql dirp 'UP) (minusp mo1)(minusp mo2)(> p0 (max p2 p4))) 'DN)
                     ((and (eql dirp 'DOWN) (plusp mo1)(plusp mo2)(< p0 (min p2 p4))) 'UP)
              
                     ((and (eql dirp 'UP) (minusp mo1)(> p0 p2)(> p2 p4)) 'DN)
                     ((and (eql dirp 'DOWN) (plusp mo1)(< p0 p2)(< p2 p4)) 'UP)
              
                     ((and (eql dirp 'UP)(minusp mo1)(> p0 p2)(< p0 p4)) 'DN)
                     ((and (eql dirp 'DOWN)(plusp mo1)(< p0 p2)(> p0 p4)) 'UP)             
              
                     ((and (eql dirp 'UP) (minusp mo2)(> p0 p2)(> p0 p4)(< p2 p4)) 'DN)
                     ((and (eql dirp 'DOWN) (plusp mo2)(< p0 p2)(< p0 p4)(> p2 p4)) 'UP)
              
	             (t 'FT))
	      price-to-go mo1 mo2 dirp p0 p2 p4)
	      ))   	            



(defun momentum-divergence2 (tdate size3 &optional (size 14))
 (let ((filt *n-filt*)(time-interval *time-interval*) times mo1 mo2 
         p0 p1 p2 p3 prices low high dirp    price-to-go (ctr 0)
         (stopdate (conv-to-string tdate 'P))
        (starttime (conv-to-string (add-mkt-days tdate (* size3 -50)) 'A)))       
         
        (setq *n-filt* size3 *time-interval* 'daily-high-low)(nil-tpv) 
            
        (loop
           (multiple-value-setq (prices times price-to-go)
                    (find-all-primitives starttime stopdate))
           (incf ctr)
           (ifn (nth 3 times) 
                (setq starttime (conv-to-string (add-mkt-days tdate (- (* size3 -50) ctr)) 'A)) (return)))

        (setq dirp (getv 0 DR))                
        (setq low (getv 0 LP) high (getv 0 HP)) 
        (if (eql dirp 'UP) (setq p0 high)(setq p0 low))
        (setq p2 (second prices) p3 (third prices) )
       (setq p1 (first prices))   
        
        (setq *time-interval* time-interval *n-filt* filt)  
        (setq mo1 (- (slow-stochastic tdate size )(slow-stochastic (getnumdate (second times)) size)))
        (setq mo2 (- (slow-stochastic tdate size )(slow-stochastic (getnumdate (first times)) size)))
;         (format t "p0= ~A p1= ~A p2= ~A p3= ~A mo1= ~A " p0 p1 p2 p3 mo1 )
 ;;;type 1 divergence      
        (cond ((and (eql dirp 'DOWN) (> p1 p3)
                    (minusp mo1)(> p0  p2)) 'DN1)
              ((and (eql dirp 'UP)(< p1 p3)
                    (plusp mo1)(< p0  p2)) 'UP1)
              ((and (eql dirp 'UP);(> p1 p3)
                    (minusp mo2)(< p0  p2)) 'DN2);;;mo goes to new low price does not
              ((and (eql dirp 'DOWN); (< p1 p3)
                    (plusp mo2)(> p0  p2)) 'UP2);;;mo does to new high price does not
                     (t 'FT))
;;;the principle is that momentum leads price.	     
	      ))       

;;;looks over a fixed number of days for a divergence of 
;;;price slope and momentum slope   num days must be > 2
;;;uses cci21 to represent the momentum
(defun momentum-divergence3 (tdate days &optional (size 21))
 (let (pslope mslope mo) 
       
       (setq pslope  (roc tdate days 'pivot))
       (setq mslope  (roc tdate days 'cci size))
        
;    (format t "p0= ~A p1= ~A p2= ~A p3= ~A mo1= ~A " p0 p1 p2 p3 mo1 )
 ;;;type 1 divergence  
   (setq mo    
        (cond ((and (minusp pslope)(minusp mslope)) 'CDN)
              ((and (minusp pslope)(plusp mslope)) 'DDN)
              ((and (plusp pslope)(minusp mslope)) 'DUP)
              ((and (plusp pslope)(plusp mslope)) 'CUP)
              (t 'FLT)))
;;;the principle is that momentum leads price.	     
;        (case mo
;          (DUP -1)
;          (DDN 1)
;          (otherwise 0))
  ))

;;;looks over a fixed number of days for a divergence of 
;;;price slope and momentum slope
;;;uses cci21 to represent the momentum
(defun momentum-divergence345 (tdate &optional (size 21))
 (let (pslope mslope (date tdate)(days 3) (md345 nil)) 
       (dotimes (ith 3)

       (setq pslope (nth-value 1 (lregress date (+ ith days) 'hl)))
       (setq mslope (nth-value 1 (lregress date (+ ith days) 'cci size)))
       
;    (format t "p0= ~A p1= ~A p2= ~A p3= ~A mo1= ~A " p0 p1 p2 p3 mo1 )
 ;;;type 1 divergence
       (setq md345
          
        (cond ((and (minusp pslope)(minusp mslope)) 'CD)
              ((and (minusp pslope)(plusp mslope)) 'DD)
              ((and (plusp pslope)(minusp mslope)) 'DU)
              ((and (plusp pslope)(plusp mslope)) 'CU)
              (t nil))
          )
        (if md345 (return)))
;;;the principle is that momentum leads price.	     
	  md345))       

(defun modivpx (tdate days &optional (size 21))
   (let (md (date tdate))
  (dotimes (ith days)
    (setq md (momentum-divergence3 date days size))
    (if (eql md  'DDN) (return md))
    (if (eql md 'DUP) (return md))
    (setq date (getd date 'ydate)))
))
;;;returns the trend direction if confirmed
(defun momentum-convergence (tdate &optional (size 5))
 (let ((filt *n-filt*)(time-interval *time-interval*) times mo2
 	p0 p1 p2 p3 prices low high dirp mo1
        (starttime (conv-to-string (add-mkt-days tdate (* size -25)) 'A)))      
        
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv) 
        (multiple-value-setq (prices times)
            (find-all-primitives starttime  (conv-to-string tdate 'P)))        
        (setq low (getv 0 LP) high (getv 0 HP)) 
        (setq *time-interval* time-interval *n-filt* filt)  
        (setq mo1 (- (roc tdate 10)(roc (getnumdate (second times)) 10)))
        (setq mo2 (- (roc (getnumdate (first times)) 10)(roc (getnumdate (third times)) 10)))
        (setq dirp (getv 0 DR))
        (if (eql dirp 'UP) (setq p0 high)(setq p0 low))
        (setq p1 (first prices) p2 (second prices) p3 (third prices))
        (cond ((and (eql dirp 'UP)(< p1 p3)(< p0 p2)(minusp mo2)) 'DOWN)
	      ((and (eql dirp 'DOWN)(> p1 p3)(> p0 p2)(plusp mo2)) 'UP)
	      
	      ((and (eql dirp 'UP)(<= p1 p3)(> p0 p2)(plusp mo1)) 'UP) 
	      ((and (eql dirp 'UP)(> p1 p3)(plusp mo2)) 'UP) 
	      
	      ((and (eql dirp 'DOWN)(>= p1 p3)(< p0 p2)(minusp mo1)) 'DOWN)
	      ((and (eql dirp 'DOWN)(< p1 p3)(minusp mo2)) 'DOWN)
	                      
	      (t nil))
	      
	      ))


(defun volume-divergence (tdate size3)
 (let ((filt *n-filt*)(time-interval *time-interval*) times vs1 ps1 
         p0 p1 p2 p3 prices low high dirp    price-to-go (ctr 0)
         (stopdate (conv-to-string tdate 'P)) index-list time-to-go
        (starttime (conv-to-string (add-mkt-days tdate (* size3 -10)) 'A)))       
         
        (setq *n-filt* size3 *time-interval* 'daily-high-low)(nil-tpv) 
            
        (loop
           (multiple-value-setq (prices times price-to-go time-to-go index-list)
                    (find-all-primitives starttime stopdate))
           (incf ctr)
           (ifn (nth 3 times) 
                (setq starttime (conv-to-string (add-mkt-days tdate (- (* size3 -10) ctr)) 'A)) (return)))

        (setq dirp (getv 0 DR))                
        (setq low (getv 0 LP) high (getv 0 HP)) 
        (if (eql dirp 'UP) (setq p0 high)(setq p0 low))
        (setq p2 (second prices) p3 (third prices) )
       (setq p1 (first prices))   
        
        (setq *time-interval* time-interval *n-filt* filt)  
        (setq vs1 (nth-value 1 (lregress tdate  (ceiling (/ (- *end-index* (car index-list)) 2)) 'volume)))
        (setq ps1 (nth-value 1 (lregress tdate (ceiling (/ (- *end-index* (car index-list)) 2)) 'close)))
;         (format t "p0= ~A p1= ~A p2= ~A p3= ~A vs1= ~A ps1= ~A" p0 p1 p2 p3 vs1 ps1 )
 ;;;type 1 divergence      
        (cond ((and (minusp vs1)(minusp ps1)) 'DN2)
              ((and (plusp vs1)(minusp ps1)) 'DN1)
              ((and (minusp vs1)(plusp ps1)) 'UP2)
              ((and (plusp vs1)(plusp ps1)) 'UP1)
              (t 'FT))
;;;the principle is that momentum leads price.	     
	      ))       


;;;returns the number of waves in the previous price move at filter 11
(defun num-waves (tdate &optional (sml 3)(lrg 9))
  (let ((fdate (add-mkt-days tdate -60)) prices times price-to-go time-to-go
         index-list1 index-list2 aa bb)
;  (declare (ignore prices times price-to-go time-to-go))
     (setq *n-filt* lrg)
     (multiple-value-setq (prices times price-to-go time-to-go index-list1)
         (find-all-primitives (format nil "~SA" fdate) (format nil "~SP" tdate)))
     (setq *n-filt* sml)
     (multiple-value-setq (prices times price-to-go time-to-go index-list2)
         (find-all-primitives (format nil "~sA" fdate) (format nil "~SP" tdate)))

   (setq aa (first index-list1) bb (second index-list1))
    (1- (length (member aa (reverse (member bb (reverse index-list2))))))

))
;;;returns the slope of the 1:1 line
;;;period is in trading days
;;;use 21 for monthly chart
;;;use 63 for quarterly chart and 252 for yearly
;;;compares current slope of price movement to Gann angles
(defun gann-slope (tdate period)
  (float (/ (- (n-day-high tdate period) (n-day-low tdate period)) period)))

;;;;the filter is in 1/2 days
(defun gann-slope-index (date0 &optional (filter 21) (period 84))
   (let ((filt *n-filt*)  p2  p1  P0  prices  ignore1 indx1 indx2
         slope t0  s0 t1 s1 index-list (days (* 4 filter)))
   ;(declare (ignore p2)); ignore1))
    (nil-tpv)(setq *n-filt* filter)
    (loop
      (multiple-value-setq (prices ignore1 ignore1 ignore1 index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
      (if (<= (length prices) 4)(setq days (+ 20 days))(return)))


    (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt);;restore
   (setq 
          ; (if (eql (getv 0 DR) 'UP)(getv 0 HP)(getv 0 LP))
         P1 (first prices) p2 (second prices)
         t0 (/ (- *end-index* (first index-list)) 2.0);;;need to divide by 2 to convert from prices to days
	 t1 (/ (- (first index-list)(second index-list)) 2.0)
        ; P0 (if (< p2 p1) (getv 0 LP)(getv 0 HP))
       ;  P0 (getd date0 'close)
	 p0 (if (eql (getv 0 'DR) 'UP) (getv 0 'HP)(getv 0 'LP))
           )
   (setq s0  (/ (-  P0 P1) t0) s1 (/ (- p1 p2) t1))
   (setq slope (gann-slope date0 period))
   (setq indx1 (cond ((> (/ s0 slope) 12.0) 7)
         ((> (/ s0 slope) 9.0) 6)
         ((> (/ s0 slope) 6.0) 5)
         ((> (/ s0 slope) 4.0) 4)
         ((> (/ s0 slope) 3.0) 3)
         ((> (/ s0 slope) 2.0) 2)
         ((> (/ s0 slope) 1.0) 1)
         
         ((< (/ s0 slope) -12.0) -7)
         ((< (/ s0 slope) -9.0) -6)
         ((< (/ s0 slope) -6.0) -5)
         ((< (/ s0 slope) -4.0) -4)
         ((< (/ s0 slope) -3.0) -3)
         ((< (/ s0 slope) -2.0) -2)
         ((< (/ s0 slope) -1.0) -1)
         (t 0)))
   (setq indx2 (cond ((> (/ s1 slope) 12.0) 7)
         ((> (/ s1 slope) 9.0) 6)
         ((> (/ s1 slope) 6.0) 5)
         ((> (/ s1 slope) 4.0) 4)
         ((> (/ s1 slope) 3.0) 3)
         ((> (/ s1 slope) 2.0) 2)
         ((> (/ s1 slope) 1.0) 1)
         
         ((< (/ s1 slope) -12.0) -7)
         ((< (/ s1 slope) -9.0) -6)
         ((< (/ s1 slope) -6.0) -5)
         ((< (/ s1 slope) -4.0) -4)
         ((< (/ s1 slope) -3.0) -3)
         ((< (/ s1 slope) -2.0) -2)
         ((< (/ s1 slope) -1.0) -1)
         (t 0)))

    (values indx1 indx2) 
   ))

(defun gann-slope-index1 (tdate &optional (period 3))
 (let  ((slope (/ (roc tdate period 'close) period)) (s0 (gann-slope tdate 63)) r1)
   (setq r1  (/  slope s0))

   (cond 
         ((> r1 8.0) 7)
         ((> r1 5.0) 6)
         ((> r1 3.0) 5)
         ((> r1 2.0) 4)
         ((> r1 1.0) 3)
         ((> r1 .618) 2)
         ((> r1 .382) 1)
         ((< r1 -8.0) -7)
         ((< r1 -5.0) -6)
         ((< r1 -3.0) -5)
         ((< r1 -2.0) -4)
         ((< r1 -1.0) -3)
         ((< r1 -.618) -2)
         ((< r1 -.382) -1)
         (t 0))
       
))



;;;computes the slope from the start of the parabolic signal
(defun parabolic-slope (tdate)
  (let (dir start-date start-date-1 stop begin-date)
    (multiple-value-setq (dir start-date stop begin-date)(parabolic-stops tdate))
     (setq start-date-1 (getd start-date 'ydate)) (ifn begin-date (setq begin-date start-date))
     (float (/ (- (getd tdate 'close) (if (eql dir 'short)(n-day-high start-date-1 
                                                             (1+ (sub-mkt-dates begin-date start-date)))
                                           (n-day-low start-date-1 
                                                     (1+ (sub-mkt-dates begin-date start-date)))))
        (sub-mkt-dates 
             (if (eql dir 'short) (sip-high start-date-1 (sub-mkt-dates begin-date start-date))
                         (sip-low start-date-1 (sub-mkt-dates begin-date start-date)))
                tdate)))
))
;;;the 126 represents 6 months
(defun parabolic-gann-slope-ratio (tdate)
   (abs (/ (parabolic-slope tdate)(gann-slope tdate 126))))

(defun gann-slope-ratio (date0 &optional (filter 9) (period 84))
   (let ((filt *n-filt*)  p2  p1  p0  prices  ignore1
         slope t0  s0  index-list (days (* 4 filter)))
   ;(declare (ignore ignore1))
    (nil-tpv)(setq *n-filt* filter)
    (loop
      (multiple-value-setq (prices ignore1 ignore1 ignore1 index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
      (if (<= (length prices) 4)(setq days (+ 20 days))(return)))


    (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt);;restore
   (setq 
          ; (if (eql (getv 0 DR) 'UP)(getv 0 HP)(getv 0 LP))
         p1 (first prices) p2 (second prices)
         t0 (/ (- *end-index* (first index-list)) 2.0);;;need to divide by 2 to convert from prices to days
         P0 (if (<  p2 p1) (getv 0 LP)(getv 0 HP))         
          )
   (setq s0 (abs (/ (-  P0 P1) t0)))
   (setq slope (gann-slope date0 period))
   (/ s0 slope)
    ))


;;;;checks if near support or resistance
(defun sr-index (date0 &optional (filter 9))
  (let (p0 p1 p2 p4  prices  (filt *n-filt*) (ch (getd date0 'high)) (cl (getd date0 'low))
        (days (* 4 filter)))
    (nil-tpv) (setq *n-filt* filter)
 ;  (format T "Date0= ~A ch= ~A   cl= ~A~%" date0 ch cl)
    (loop
      (setq prices 
         (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
  ;   (format T "prices= ~A~%" prices) 
     (if (<= (length prices) 4) (setq days (+ 20 days))(return)))

    (setq *n-filt* filt)
  (setq p0 (if (eql (getv 0 DR) 'UP) (getv 0 HP)(getv 0 LP))
       p1 (first prices) p2 (second prices)  p4 (fourth prices))
;  (format T "p0= ~A  p1= ~A p2= ~A~%" p0 p1 p2)
  (values
   (cond ((and (> p0 p1)(>= p0 p2)(<= p2 ch)(>= p2 cl)) 1)
         ((and (> p0 p1)(>= p0 p4)(<= p4 ch)(>= p4 cl)) 2)
         ((and (< p0 p1)(<= p0 p2)(<= p2 ch)(>= p2 cl)) -1)
         ((and (<= p0 p1)(<= p0 p4) (<= p4 ch)(>= p4 cl)) -2)
         (t 0))
    p2 p1)
))
 
(defun support-resistance (date0 &optional (size 10))
   (let ((days (* 10 size)) sr p0 p1 p2 p4 (filt *n-filt*)
         (ch (getd date0 'high))(cl (getd date0 'low)) prices)
     
      
      
        (multiple-value-setq (prices ) 
         (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
  ;   (format T "prices= ~A~%" prices) 
      
 
       (setq p0 (if (eql (getv 0 DR) 'UP) (getv 0 HP)(getv 0 LP))
             p1 (first prices) p2 (second prices)  p4 (fourth prices))
       (when (not sr) (pushnew p1 sr) (pushnew p2 sr))
       (if (and sr (or (< p1 (min* sr))(> p1 (max* sr))))(pushnew p1 sr))
       (if (and sr (or (< p2 (min* sr))(> p2 (max* sr))))(pushnew p2 sr))
       (if (and (> p0 p1)(> p4 p2)(> p4 (max* sr)))(pushnew p4 sr))
       (if (and (< p0 p1)(< p4 p2)(< p4 (min* sr)))(pushnew p4 sr))
         
    (setq *n-filt* filt)
    (values  (count-if #'(lambda(s) (and (<= s ch)(>= s cl))) sr) sr)
   ))



(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
              `(,(incf key) ,expr))
              exprs))))

(defun standard-deviation ( prices)
  (let ((m1 (/ (list-sum prices)(length prices))))
     
   ;  (setq m2  (mapcar #'(lambda (s1) (expt (- s1 m1) 2)) prices))
   ;  (print m2)
     (sqrt (/ (list-sum (mapcar #'(lambda (s1) (expt (- s1 m1) 2)) prices)) (1- (length prices))))
  

 ))

;;;returns the lower and upper  Bollinger band levels
(defun bb (tdate &optional (period 10) (factor 1.5))
 (let (std (mean (ave tdate period))(date tdate) prices)
  (dotimes (ith period)
    (push (getd date 'close) prices)
    (setq date (getd date 'ydate)))
;;;

 ;  (setq prices1 prices)
  (setq std (standard-deviation prices)) 


   (values (- mean (* factor std))(+ mean (* factor std)))
))

(defun bb-index (tdate period factor)
 (let (bb-lower bb-higher (pclose (getd tdate 'close)))
   (multiple-value-setq (bb-lower bb-higher) (bb tdate period factor))
  (cond ((>= pclose bb-higher) 1)
        ((<= pclose bb-lower) -1)
        (t 0))
))

(defun three-moving-ave-index (date &optional (parm1 3)(parm2 8)(parm3 13))
  (let ((ave3 (ave-exp date parm1))(ave8 (ave-exp date parm2))
	(ave13 (ave-exp date parm3)))
    (cond ((<= ave3 ave8 ave13) -3)
	  ((<= ave3 ave13 ave8) -2)
	  ((<= ave8 ave3 ave13) -1)
	  ((<= ave8 ave13 ave3) 1)
	  ((<= ave13 ave3 ave8) 2)
	  ((<= ave13 ave8 ave3) 3)
          (t 0))))




;;;END OF MARKET DAY FUNCTIONS
;;;***************************


;;;*******************************************************************
;;; GENERAL LISP UTILITIES FOR COMPARING ARGUMENTS - TCB - FALL '86
;;;*******************************************************************
;;;
;;; compare numbers BUT return NIL for ANY NIL args
(DEFUN ABN (ARG)
  (AND (NUMBERP ARG) (ABS ARG)))
(DEFUN GTR (ARG1 ARG2)
  (AND (NUMBERP ARG1) (NUMBERP ARG2) (> ARG1 ARG2)))
(DEFUN GTE (ARG1 ARG2)
  (AND (NUMBERP ARG1) (NUMBERP ARG2) (NOT (< ARG1 ARG2))))
(DEFUN LTN (ARG1 ARG2)
  (AND (NUMBERP ARG1) (NUMBERP ARG2) (< ARG1 ARG2)))
(DEFUN LTE (ARG1 ARG2)
  (AND (NUMBERP ARG1) (NUMBERP ARG2) (NOT (> ARG1 ARG2))))
(DEFUN ADD (&REST ARGS)
  (AND (EVERY #'NUMBERP ARGS) (APPLY #'+ ARGS)))
(DEFUN SUB (&REST ARGS)
  (AND (EVERY #'NUMBERP ARGS) (APPLY #'- ARGS)))
(DEFUN MUL (&REST ARGS)
  (AND (EVERY #'NUMBERP ARGS) (APPLY #'* ARGS)))
(DEFUN DIV (&REST ARGS)
  (AND (EVERY #'NUMBERP ARGS)
       (NOT (SOME #'ZEROP (CDR ARGS)))
       (APPLY #'/ ARGS)))
(DEFUN LOGE (ARG)
  (AND (NUMBERP ARG) (LOG ARG)))

  
(defun add1 (arg1 arg2)
   (cond ((and (numberp arg1)(numberp arg2)) (+ arg1 arg2))
         ((numberp arg1) arg1)
         ((numberp arg2) arg2)
         (t nil)))

(defun sub1 (arg1 arg2)
   (cond ((and (numberp arg1)(numberp arg2)) (- arg1 arg2))
         ((numberp arg1) arg1)
         ((numberp arg2) arg2)
         (t nil)))


(defun max* (l)
  (let ((highest (car l)))
  (dolist (jth l highest)
    (setq highest  (max jth highest)))))

(defun min* (l)
  (let ((lowest (car l)))
  (dolist (jth l lowest)
    (setq lowest  (min jth lowest)))))
(defun median (l)
  (when (and l (listp l))
    (setq l (vsort l #'<))
    (if (oddp (length l))
	(nth (truncate (length l) 2) l)
    (/ (+ (nth (truncate (length l) 2) l)
	  (nth (1- (truncate (length l) 2)) l)) 2))))

;;; compare things BUT treat NIL as INCOMPARABLE to anything
(DEFUN EQN (ARG1 ARG2)
  (COND ((OR ARG1 ARG2) (EQL ARG1 ARG2))
        (T NIL)))

;;; compare numbers BUT treat NIL args as INCOMPARABLE to numbers
;;; ex: (fgtr 3 4)   => nil, (fgtr 3 nil) => t, (fgtr nil 4) => nil
(DEFUN FGTR (ARG1 ARG2)
  (COND ((AND (NUMBERP ARG1) (NUMBERP ARG2)) (GTR ARG1 ARG2))
        ((NUMBERP ARG1) T)
        (T NIL)))
(DEFUN FGTE (ARG1 ARG2)
  (COND ((AND (NUMBERP ARG1) (NUMBERP ARG2)) (NOT (LTN ARG1 ARG2)))
        ((NUMBERP ARG1) T)
        (T NIL)))
(DEFUN FLTN (ARG1 ARG2)
  (COND ((AND (NUMBERP ARG1) (NUMBERP ARG2)) (LTN ARG1 ARG2))
        ((NUMBERP ARG2) T)
        (T NIL)))
(DEFUN FLTE (ARG1 ARG2)
  (COND ((AND (NUMBERP ARG1) (NUMBERP ARG2)) (NOT (LTN ARG1 ARG2)))
        ((NUMBERP ARG2) T)
        (T NIL)))

(DEFUN FMAX (&REST ARGS)
  (LET ((NUMS (REMOVE-IF #'(lambda (s) (not (numberp s))) ARGS)))
    (COND ((NULL NUMS) NIL)
          (T (APPLY #'MAX NUMS)))))
(DEFUN FMIN (&REST ARGS)
  (LET ((NUMS (REMOVE-IF  #'(lambda (s) (not (numberp s))) args)))
    (COND ((NULL NUMS) NIL)
          (T (APPLY #'MIN NUMS)))))

;;;**************************************************************************
;;; UTILITIES TO SUPPORT THE E-WAVES DEVELOPMENT & DIAGNOSIS ACTIVITIES
;;;**************************************************************************
;;;

;(defun prnt-code (code )
;  (if *print-spro;uts*
;      (FORMAT *debug-io* "*>~S " CODE)))


;;;PRINT ITEM AND RETURN CURSOR TO ORIGINAL LOCATION, NOTE: ITEM SHOULD BE
;;;THE LAST OBJECT IN THE LINE
#+gclisp
(defun in-place (i &optional (term nil) (stream *standard-output*))
  (multiple-value-bind (x y)
      (send stream :cursorpos)
    (send stream :clear-eol)
    (format stream " ~s~@[~s~]" i term)
    (send stream :set-cursorpos x y)))
#-gclisp
(defun in-place (i &optional (term nil) (stream *standard-output*))
  (format stream " ~s~@[~s~]" i term))

;;;PRINT ITEM AT GIVEN OFFSET FROM CURRENT POSITION AND RETURN CURSOR
;;;TO ORIGINAL POSITION, NOTE ITEM SHOULD BE THE LAST OBJECT IN THE LINE
#+gclisp
(defun in-place2 (i &optional (x-offset 0) (y-offset -1))
  (multiple-value-bind (x y)
      (send *terminal-io* :cursorpos)
    (send *terminal-io* :set-cursorpos (+ x x-offset) (+ y y-offset))
    (send *terminal-io* :clear-eol)
    (format t " ~s" i)
    (send *terminal-io* :set-cursorpos x y)))
#-gclisp
(defun in-place2 (i &optional (x-offset 0) (y-offset -1))
  (setq x-offset x-offset y-offset y-offset)
  (format t " ~s" i))

;;;MENU SYSTEM UTILITIES
;;;
(defun datep (num)
  (if (and num (numberp num)(>= num 100))
      (let ((yer (getnumyear num))
            (mon (getnummonth num))
            (day (getnumday num)))
        (and (> yer 0)(>= mon 1)(<= mon 12)(>= day 1)(<= day 31)))))
(defun hourp (tim)
  (cond ((eql *time-interval* 'monthly-high-low) nil)
	((eql *time-interval* 1440)
	 (or (not tim) (eql tim 'c) (eql tim 'cls)))
	((eql *time-interval* 720) (member tim '(A P AM PM)))
	((or (numberp *time-interval*) (eql *time-interval* 'daily-high-low))
	 (or (and (numberp tim) (>= tim 0) (<= tim 2400))
	     (member tim '(A P AM PM))))
	(t nil)))


(defun combine-simulation-trades (markets time-frame)
  (let (trades paths (path-out (string-append *output-upper-dir* "day-trades.csv")) code)
    (setq code (case time-frame
                  (day 3)
                  (day2 2)
                  (forexday 2)))

   (dolist (market markets)
          (push (string-append *output-upper-dir*
            (format nil "~Aday-simulation~A.dat" market code)) paths));;;closes dolist over markets

   (dolist (ith paths)
    (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
         (push record trades)))
    )
   (vsort trades #'> #'(lambda(s) (read-from-string (subseq s 0 9))))

  (with-open-file (stream2 path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (ith (reverse trades))
          (format stream2 "~A~%" ith)))
  ))
;;;takes a list of paths and combines them into path-out
(defun my-append-files (path-out &rest paths)
  (let (records)

    (dolist (ith paths)
     (when (probe-file ith)
    ;;;if ".csv" file use the read-line
     (with-open-file (str ith :direction :input)
      ;(do ((record (read str nil 'eof) (read str nil 'eof)))
      ;    ((eql record 'eof))
      ;   (push record records)));;closes the if and the do
     
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
          (push record records)));;closes the if and the do
     


  )) ;;closes with-open-file and the dolist over paths

 (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith (reverse records))
         (format stream "~A~%" ith)));;;closes the with-open-file
));;closes the let and defun

(defun combine-ts-files ()
 (let (paths)
 (setq paths (append (directory  "~/exitpoints/ninja/ts-fore-201707*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201708*.*")
                    ; (directory  "~/exitpoints/testfiles20200601/ts-fore-2019*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201709*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201710*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201711*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201712*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201801*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201802*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201803*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201804*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201805*.*")
                     (directory  "~/exitpoints/ninja/ts-fore-201806*.*")
                    ; (directory  "~/exitpoints/ninja/ts-fore-201807*.*")
                    ; (directory  "~/exitpoints/ninja/ts-fore-201908*.*")
    ))
  (mapcar #'(lambda(s) (remove-duplicate-lines s)) paths)
 (apply #'my-append-files "~/mk-data/111-dropbox/ts-fore.csv" paths) 

))

(defun combine-ts-database-files ()
 (let (paths)
 (setq paths (directory "~/mk-data/111-dropbox/edgeai/psarbasic/*_database.txt"));;
 ;(print paths)
 (apply #'my-append-files "~/mk-data/111-dropbox/ts-database.csv" paths) 
 (remove-duplicate-lines "~/mk-data/111-dropbox/ts-database.csv")

; (convert-ts-database "~/mk-data/111-dropbox/ts-database.csv")
; (ts-brochure "~/exitpoints/ts-performance.csv" 15 40000  300)
))

;;;convert the ts-database trades to date, mkt, P&L
(defun convert-ts-database (path1)
 (let (records record1 records1 records2 dmy (path-out "~/exitpoints/ts-performance.csv"))

   (maind-x)(set-cat-list)(set-market 'dj.d1b)
   (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        ;(format t "~%~S" record)
        ;(setq record1 (mapcar #'(lambda(s) (read-from-string s nil nil)) (my-split-sequence #\, record) ))
        (setq record1 (my-split-sequence #\, record) )
      ;  (if (and (numberp (second record1))(car record1)(nth 4 record1))
 
              (push record1 records)
      ;  (format t "~%~S" record1)
       )) ;;closes with-open-file and the dolist over paths


  (setq records (mapcar #'(lambda(s) (list (nth 0 s)(nth 1 s)(nth 2 s) (nth 3 s)
                                           (if (or (equal (nth 3 s) "Buy Cover")
                                                 (equal (nth 3 s) "Buy Long")) 
                                 (- (read-from-string (nth 4 s) nil nil)) (read-from-string (nth 4 s) nil nil)))) records))                                        
 
 ; (format t "~% Records = ~S ~%" records)
  ;(setq records (mapcar #'(lambda(s) (if (eql (nth 3 s) 'Buy) (- (nth 4 s))(nth 4 s))) records))

   (setq records (mapcar #'(lambda(s) (list (list (conv-to-ewaves-date (car s)) (nth 1 s)
                                         (if (or (equal (nth 3 s) "Sell Cover")
                                                 (equal (nth 3 s) "Buy Long")) "Long" "Short"))
                                              (nth 4 s))) records))
                                            
                                          
   
   ;(setq records (remove-if #'(lambda(S) (/= (car s) 20200416)) records))
   (setq records  (vsort records #'< #'caar))
  ;(format t "~% Records = ~A ~%" records)
   ;  (format t "~%P&L Before deductions = ~A~%"  (list-sum (mapcar #'(lambda(s)(fourth s)) records)))
    ; (setq num-trades  (list-sum (mapcar #'(lambda(s)(second s)) records)))
      

   (dolist (ith records)
       (if (equal (first (assoc (car ith) records1 :test #'equal))(first ith))
               
           (setf (second (assoc (car ith) records1 :test #'equal)) (+ (second ith)(second (assoc (car ith) records1 :test #'equal))))
         (push ith records1)))

    (setq records1  (vsort records1 #'< #'caar))

;     (format t "~% Records1 = ~A ~%" records1)
;;get rid of inner list
    (dolist (ith records1)
     (push (list (first (first ith))(second (first ith))(third (first ith))(second ith)) records2))
  
; (format t "~% Records2 = ~A ~%" records2)

    (dolist (ith records2)
   
       (setq dmy (second ith))
     
       (setq dmy (subseq dmy 0 (- (length dmy) 3)))
       (setq dmy (read-from-string dmy nil nil))
     
       (setq dmy (car (rassoc dmy *ts-symbol*)))
       (set-market dmy)
       (setf (fourth ith)(* (fourth ith)
                           (if (eql dmy 'hu.d1b)(* 100 (calculate-point-value (first ith)))
                               (calculate-point-value (first ith))))))
 


;;;convert secomnd field to ninjatrader symbols

       (dolist (kth records2)
        
         (setq dmy  (second kth))
         (setq dmy (subseq dmy 0 (- (length dmy) 3)))
         
         (setq dmy (read-from-string dmy nil nil) )
         (setq dmy (car (rassoc dmy *ts-symbol*)));;;this is exitpoints symbol
        
         (setq dmy (cdr (assoc dmy *ninja-symbol*)))
         (setf (second kth) dmy))

    

      (setq records1  (vsort records2 #'> #'car))

      (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)    
          (dolist (jth (reverse records2))
             (format str "~A, ~A, ~A, ~A~%" (car jth)
                (second jth);(read-from-string (subseq (format nil "~S" (second jth)) 0 (- (length (format nil "~S" (second jth))) 3)))
                 (round (fourth jth)) (third jth))))

  
;records
))


;;;compiles a list of markets/contracts that are in the ts-e12.csv file
;;;use fore for the month to test files
(defun trade-action-markets (month)
 (let ((path (string-append "~/mk-data/111-dropbox/ts-" (format nil "~S" month) ".csv")) 
       (path-out (string-append "~/mk-data/111-dropbox/ts-" (format nil "~S" month) ".txt"))
        records record1)

     (with-open-file (str path :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
          (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
          (push (list (nth 1 record1)(nth 2 record1)) records)));;closes the open and the do
   
     (setq records (remove-duplicates records :test #'equal))
  (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (ith records)
       (format str "~A~%" ith)))
    

 ))
   
;;;removes duplicate lines from a file
(defun remove-duplicate-lines (path1)

 (let (records)

     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
          
          (push record records)));;closes the open and the do
   
     (setq records (remove-duplicates records :test #'equal))

     (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith (reverse records))
        (format str "~A~%" ith)))
  ))

(defun remove-do-not-trade-days (path1)
  (let (records record1)

    (with-open-file (str path1 :direction :input)
    (do ((record (read-line str nil 'eof)(read-line str nil 'eof)))
       ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
       ; (print record1)
        (if (not (member (car record1) *do-not-trade*))
            (push record records)))
       
 (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith (reverse records))
        (format str "~A~%" ith)))
  ))
)
;;;****************************************************************************
;;;                              END OF FILE UTL1
;;;****************************************************************************

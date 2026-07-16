
#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


;;;Note: this is the one that seems to be the one being used:
(defparameter *counts-save-dir* nil) 
(defparameter *proj-save-dir* "/home/silver/flores/projs/")

(defparameter *plots-ci-calc-dir* "/home/silver/ewaves/cicalc/")

(defparameter *counts-upper-dir* "/home/silver/flores/ewaves/counts/")
(defvar *indx-cfg* nil)
;(defvar *ewaves-home* nil)
;(defvar *ewaves-local* nil)
(defvar *all-markets* nil)
(defvar *analyst-mode* nil)


(defun read-markets-config ()
  (let ((lpath (string-append *database-upper-dir* "index.cfg"))
	(bpath (string-append *database-upper-dir* "bar.cfg"))
	(epath (string-append *database-upper-dir* "index.cfg"))
	unknowns indx-cfg temp)

    (unless (probe-file lpath)
     (format t "~%Could not find index.cfg in ~a" lpath)
     (return-from read-markets-config))
    (dotimes (i 5)
      (cond ((probe-file bpath)
	     (sleep 9)
	     (if (> *debug* 19)
		 (format t "~%Attempt ~a reading local index.cfg nogo" i))
	     (when (>= i 4)
	       (format t "~%Attempts at reading local index.cfg failed 5 times")
	       (return-from read-markets-config)))
	    (t (with-open-file (strm lpath :direction :input)
		 (setq *indx-cfg* (read strm nil 'eof)))
	       (ifn (eql *indx-cfg* 'eof) (return)))))
    (when (not (probe-file epath))
      (format t "~%Could not find index.cfg in ~a" epath)
      (return-from read-markets-config))
    (dotimes (i 5)
      (cond((probe-file bpath)
	    (if (> *debug* 19)
		(format t "~%Attempt ~a reading dbase index.cfg nogo" i))
	    (sleep 9)
	    (when (>= i 4)
	      (format t "~%Attempts at reading dbase index.cfg failed 5 times")
	      (return-from read-markets-config)))
	   (t (with-open-file (strm epath :direction :input)
		(setq *all-markets* (read strm nil 'eof)))
	      (ifn (equal *all-markets* 'eof) (return)))))

    ;; It does nothing. Here from the first time ever?
    (dolist (idx *all-markets*)
      (setq temp (assoc 'ddelt (cdr idx)))
      (when temp
	(setf (cdr idx)
	      (remove (assoc 'ddelt (cdr idx)) (cdr idx)))))
    
    (dolist (idx *indx-cfg*)
      (setq temp (assoc (car idx) *all-markets*))
      (cond (temp
	     (setf (cdr idx) (append (cdr temp) (cdr idx)))
	     (push idx indx-cfg))
	    (t
	     (push (car idx) unknowns))))
    
    (setq *indx-cfg* (reverse indx-cfg))

    ;; Not needed for batch, since I want to call this after setting
    ;; the data name and time interval from the input data.  This would
    ;; override both, the data name and the time interval.
    ;;(setq *data-name* (caar *indx-cfg*)
    ;;      *time-interval* (index-timeint))

    (setq *counts-save-dir* (format nil "~a~(~a~)/"
				    *counts-upper-dir* *data-name*)
	  *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir*
				  *data-name*))

;    (setq *upper-dir* *ewaves-local*)
    (setq *cat-list* '())
    (setq *cat-indices* nil)
    (dolist (nm *indx-cfg*)
      (push (cons (car nm) nil) *cat-list*))
    unknowns
    nil))


(defun add-market-to-index-cfg (data-name)
  (let (index)
    (dolist (mark *all-markets*)
      (when (eql data-name (car mark))
	(setq index (copy-list mark))
	(return nil)))
    
    ;(car (rassoc pdi *int-periods* ; From iewaves. pdi->
    ;             :test #'string=))       ; (string) time-intrv.
    (setf (cdr index)(append (list '(DDELT . DAILY-HIGH-LOW))
			     (cdr index)))
    (setq *indx-cfg* (append *indx-cfg* (list index)))
    (write-markets-config t)
    (shell (format nil "mkdir ~acounts/~(~a~)" *ewaves-local*
		   (car index)))
    (setq *cat-list* '())
    (setq *cat-indices* nil)
    (dolist (nm *indx-cfg*)
      (push (cons (car nm) nil) *cat-list*))
    t))

(defun write-markets-config (localp)
  (cond (localp
	 (with-open-file (strm (format nil "~(~a~)index.cfg" *ewaves-local*)
			       :direction :output :if-exists :supersede)
	   (format strm "(")
	   (dolist (x *indx-cfg*)
	     (format strm "(~a (DDELT . ~s))~%"
		     (index-sname (car x))
		     (index-timeint (car x))))
	   (format strm ")"))
	 )
	(t
	 (with-open-file (strm (format nil "~(~a~)index.cfg" 
				       *database-upper-dir*)
			       :direction :output :if-exists :supersede)
	   (prin1 *all-markets* strm)))))





(defun find-group (count) (setq count count) nil)
;(defun find-group (count &aux wv)
;  (setq wv (wv-for-degree count *gdegree*))
;  (if (and wv (> (getv wv 'dg) (getv (find-parent count) 'dg)))
;      (setq wv (find-parent count)))
;  (ifn wv (list count)
;    (nreverse (group-rcounts2 total-counts (top-atlist wv (getv wv 'dg))))))


;; Fucntion from mainc (originally from w8z-men0)
;;; Input->"199301031200_199292931200_1993..._  etc,  "
;;;Output->(199301031200 199292931200 1993...   etc.  )
(defun parse-string-list21 (strg)
  (ifn strg (return-from parse-string-list21))
  (let ((wrd-on nil) (strgl (length strg)) strglm slist temp-strg ib ie)
    ;;Note: The inp file was delimited by "_", so subst it instead of blanks
    ;;Start of PC program
    (setq strglm (1- strgl))
    (dotimes (i strgl)
      (cond 
       ;;Take care of leading blanks - added here
       ((and (eql (aref strg i) #\_) (not wrd-on)))
       ;;Look for delimiter
       ((and (or (eql (aref strg i) #\_) (= i strglm)) wrd-on)
	(setq ie (if (= i strglm) strgl i) wrd-on nil)
	(setq temp-strg (subseq strg ib ie))
	;(push (read-from-string temp-strg) slist)
	(push temp-strg slist))
       ;;Look for new word
       ((and (not (equal (aref strg i) #\_)) (not wrd-on))
	(setq ib i wrd-on t))))
    (nreverse slist)))








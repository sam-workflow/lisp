;;; -*- Mode: LISP; Package: user; Base: 10. -*-
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
(defvar *end-index* nil "index of the last data point ")

(defvar *TP* (make-array 2048)) ;subarrays time-price-vector
(defparameter *TP-subdim* 256)

;;;finds 3 waves per degree local variable "num-waves"
;;;changed to 4
(defun degree-finder (start-index start-time prev-start-index prev-start-time
				 degree hww &optional (end-time *curr-et*)
				 stop-time (filter-large (* 2 *n-filt*))
				 num-waves)
  (let (DJIA-LIST TIMES-LIST index-list PRED-LOW PRED-HIGH PRED-NEXT
		  PRED-PREV PRED-PREV-1 PRED-PREV-2 UP-DN ENDT-PTI
		  PDATA-PTI (filter-small 0) price-to-go time-to-go
		  (endday (getnumdate end-time))
		  ;(read-from-string (subseq end-time 0 6)))
		  )
    (setq *n-filt* (round (/ *n-filt* 2)));initial value to try
    ;;;step 2 apply filter(s) to the data to get num-waves +1 points
    (loop
      (when (< (- filter-large filter-small) 2)
	(setq *n-filt* filter-small)
	(if (zerop *n-filt*) (return))
	(if (> degree 2)
	    (setq prev-start-index
		  (cdr (assoc 'start-index (cdr (assoc (- degree 1)
						       *degrees*))))
		  prev-start-time
		  (cdr (assoc 'start-time
			      (cdr (assoc (- degree 1)
					  *degrees*))))))
	(return
	  (multiple-value-setq
	      (djia-list times-list index-list PRED-LOW PRED-HIGH PRED-NEXT
			 PRED-PREV PRED-PREV-1 PRED-PREV-2 UP-DN 
			 ENDT-PTI PDATA-PTI price-to-go time-to-go)
	    (find-prims prev-start-index prev-start-time end-time stop-time))))
 ;     (if *print-sprouts* (format hww "~%trying *n-filt*=~A" *n-filt*))
      (multiple-value-setq
	  (djia-list times-list index-list PRED-LOW PRED-HIGH PRED-NEXT
		     PRED-PREV PRED-PREV-1 PRED-PREV-2 UP-DN 
		     ENDT-PTI PDATA-PTI price-to-go time-to-go)
	(find-prims start-index start-time end-time stop-time))
      (if *print-sprouts*
	  (FORMAT hww "~% DJIA-LIST=~S TIMES-list=~S INDEX-LIST=~A"
	      DJIA-LIST times-list index-list))
      (cond ((= (length djia-list) (1+ num-waves))
	     (if (> degree 2)
		 (setq prev-start-index
		       (cdr (assoc 'start-index (cdr (assoc (- degree 1)
						      *degrees*))))
		       prev-start-time
		       (cdr (assoc 'start-time
					  (cdr (assoc (- degree 1)
						      *degrees*))))))
	     (return (multiple-value-setq
			 (djia-list times-list index-list PRED-LOW PRED-HIGH
				    PRED-NEXT PRED-PREV PRED-PREV-1
				    PRED-PREV-2 UP-DN
				    ENDT-PTI PDATA-PTI price-to-go time-to-go)
		       (find-prims PREV-start-index PREV-start-time
				   end-time stop-time))))
	    ((< (length djia-list) (1+ num-waves))
	     (setq filter-large *n-filt*
		   *n-filt* (truncate (+ filter-small *n-filt*) 2)))
	    ((> (length djia-list) (1+ num-waves))
	     (setq filter-small *n-filt*
		   *n-filt* (truncate (+ filter-large *n-filt*) 2))))
      
      );;closes loop
    (ifn (zerop *n-filt*)
	 (values (cond ((evenp (length (ldiff index-list
					      (member start-index
						 index-list))))
			(nth 3 index-list));changed to 3 11/19/89
		       (t (nth 3 index-list))) ;index of the new starttime
		 (cond ((evenp (length
				 (ldiff times-list
					(member start-time times-list
						:test 'equal))))
			(nth 3 times-list))
		       (t (nth 3 times-list))) ; this is new start-time
		 START-INDEX START-TIME
		 `((*n-filt* . ,*n-filt*)
		   (HL-list . ,djia-list)
		   (TIMES-list . ,times-list)
		   (INDEX-LIST . ,INDEX-LIST)
		   (END-PRICE . ,(getd endday 'pdata
				       (getnumh end-time)))
		   (PRED-LOW . ,PRED-LOW)
		   (PRED-HIGH . ,PRED-HIGH)
		   (PRED-NEXT . ,PRED-NEXT)
		   (PRED-PREV . ,PRED-PREV)
		   (PRED-PREV-1 . ,PRED-PREV-1)
		   (PRED-PREV-2 . ,PRED-PREV-2)
		   (UP-DN . ,UP-DN)
		   (ENDT-KNOWN . ,ENDT-PTI)
		   (PRICE-KNOWN . ,PDATA-PTI)
		   (start-index . ,start-index)
		   (start-time . ,start-time)
		   (price-to-go . ,price-to-go)
		   (time-to-go . ,time-to-go))))
    ))
(defun find-prims (start-index start-time end-time &optional stop-time)
  (block nil
    (let ((DJIA-LIST 
	     (list (svref (get-TPV start-index) 3)))
	  (times-list (list start-time))
	  (index-list (list start-index))
	  (endday (getnumdate end-time))(endtime (getnumh end-time))
	  UP-DN PRED-HIGH PRED-LOW PRED-NEXT PRED-PREV pred-prev-1
	  pred-prev-2 ENDT-PTI PDATA-PTI aa bb curr-ET cc dd ignore)
      (declare (ignore ignore))
      (setq curr-ET start-time) ; acc::*maxq* 0)
      (setf (car djia-list) (float (car djia-list)))
;using slot 'dg to store the index of the time for the last primitive
;;;hope this isn't too confusing
      (do ((cur-start-time start-time (getv 0 ET))
	   (cur-start-index start-index (getv 0 DG))
	   (i 0 (1+ i)))
	  ((or (string= END-TIME curr-ET)
	       (and (=  endday (getnumdate curr-et))
		  (or (not endtime)
		      (case endtime
			(A t)
			(P (eql 'P (getnumh curr-et)))
			(t (<= (time-units-into-day endday endtime)
			       (time-units-into-day (getnumdate curr-et)
						    (getnumh curr-et)))))))
	       ))
	(setq endt-pti aa pdata-pti bb)
;THE cur-start-index sets prices-only true
	(multiple-value-setq (aa bb ignore ignore ignore cc dd)
	  (provide-primitive cur-start-time stop-time
			     cur-start-index))
	(if *primitive* (setq curr-ET (getv 0 ET)))
	(unless *PRIMITIVE*
	  ;(PRINT "YOU HAVE REACHED THE END OF YOUR DATA")
	  (RETURN))
	;;; INFORMATION ABOUT CURRENT POINT PTI
	(push (float (getv 0 EP)) DJIA-LIST)
	(push (getv 0 et) times-list)
	(push (getv 0 dg) index-list);not really 'dg index of start-time
	(case  (getv 0 DR)
	  (DOWN (setq  UP-DN 'LOW))
	  (UP (setq UP-DN 'HIGH)))
	(when (>= i 3)	  
	  (cond ((EQL (getv 0 DR) 'DOWN )
		 (setq PRED-LOW
		       (my-round (* (nth 0 djia-list)
				    (/ (nth 1 djia-list) (nth 3 djia-list))) 2)
		       PRED-HIGH
		       (my-round (* (nth 1 djia-list)
				    (/ (nth 2 djia-list) (nth 4 djia-list))) 2)
		       
		      ))
		(t (setq PRED-HIGH
			 (my-round (* (nth 0 djia-list)
				      (/ (nth 1 djia-list)
					 (nth 3 djia-list))) 2)
			 PRED-LOW
			 (my-round (* (nth 1 djia-list)
				      (/ (nth 2 djia-list)
					 (nth 4 djia-list))) 2)
			 
			 ))))
	);closes the DO over primitives
      (if (>= (length djia-list) 3)
	  (setq pred-next
		(my-round (* (if (eql up-dn 'high)
				  (getv 0 lp)
				  (getv 0 hp))
			      (/ (nth 0 djia-list) (nth 2 djia-list))) 2)
		))
      (if (>= (length djia-list) 6)
	  (setq pred-prev
		(my-round (* (nth 2 djia-list)
				    (/ (nth 3 djia-list) (nth 5 djia-list)))
			  2)))
      (if (>= (length djia-list) 7)
	  (setq pred-prev-1
		(my-round (* (nth 3 djia-list)
				    (/ (nth 4 djia-list) (nth 6 djia-list)))
			  2)))
      (if (>= (length djia-list) 8)
	  (setq pred-prev-2
		(my-round (* (nth 4 djia-list)
				    (/ (nth 5 djia-list) (nth 7 djia-list)))
			  2)))
      (values djia-list  TIMES-LIST index-list
	      PRED-LOW PRED-HIGH PRED-NEXT PRED-PREV PRED-PREV-1
	      PRED-PREV-2 UP-DN
	      ENDT-PTI PDATA-PTI cc dd))))


;;;this creats the current code
(defun encode1 (trends)
  (dolist (ith trends)
    (let* ((dir  (case  (cdr (assoc 'UP-DN (cdr ith)))
		   (HIGH "D")
		   (LOW  "U")))
	   (spec (cond ((equal dir "U")
			(COND ((or (not (cdr (assoc 'PRED-LOW (cdr ith))))
				    (not (cdr (assoc 'PRED-HIGH (cdr ith)))))
			       nil)
			      ((and (ltn (cdr (assoc 'pred-prev-1 (cdr ith)))
					 (cdr (assoc 'pred-prev-2 (cdr ith))))
				    (<= (second (cdr (assoc 'hl-list
							   (cdr ith))))
				       (nth 3 (cdr (assoc 'hl-list
							   (cdr ith)))))
				    (< (cdr (assoc 'pred-prev (cdr ith)))
				       (car (cdr (assoc 'HL-list (cdr ith))))))
			       3)
			      ((and (ltn (cdr (assoc 'PRED-HIGH (cdr ith)))
					 (cdr (assoc 'PRED-PREV (cdr ith))))
				    (> (cdr (assoc 'end-price (cdr ith)))
				       (cdr (assoc 'PRED-HIGH (cdr ith))))) 1)
			      ((and (gtr (cdr (assoc 'pred-prev (cdr ith)))
					 (cdr (assoc 'pred-prev-1 (cdr ith))))
				    (or (> (cdr (assoc 'PRED-HIGH (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith))))
					(> (cdr (assoc 'PRED-LOW (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith)))))
				    (>= (first (cdr (assoc 'hl-list
							   (cdr ith))))
				       (third (cdr (assoc 'hl-list
							  (cdr ith)))))) 2)
			      ((and (> (cdr (assoc 'PRED-LOW (cdr ith)))
				       (cdr (assoc 'PRED-HIGH (cdr ith))))
				    (or (> (cdr (assoc 'PRED-HIGH (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith))))
					(> (cdr (assoc 'PRED-LOW (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith)))))
				    )  0)))
		       ((equal dir "D")
			(cond ((or (not (cdr (assoc 'PRED-LOW (cdr ith))))
				   (not (cdr (assoc 'PRED-HIGH (cdr ith)))))
			       nil)
			      ((and (gtr (cdr (assoc 'pred-prev-1 (cdr ith)))
					 (cdr (assoc 'pred-prev-2 (cdr ith))))
				    (>= (second (cdr (assoc 'hl-list
							    (cdr ith))))
					(nth 3 (cdr (assoc 'hl-list
							   (cdr ith)))))
				    
				    (> (cdr (assoc 'pred-prev (cdr ith)))
				       (car (cdr (assoc 'hl-list (cdr ith))))))
			       3)
			      ((and (gtr (cdr (assoc 'PRED-LOW (cdr ith)))
					 (cdr (assoc 'PRED-PREV (cdr ith))))
				    (< (cdr (assoc 'end-price (cdr ith)))
				       (cdr (assoc 'PRED-LOW (cdr ith))))) 1)
			      ((and (ltn (cdr (assoc 'pred-prev (cdr ith)))
					 (cdr (assoc 'pred-prev-1 (cdr ith))))
				    (or (< (cdr (assoc 'PRED-HIGH (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith))))
					(< (cdr (assoc 'PRED-LOW (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith)))))
				    (<= (first (cdr (assoc 'hl-list
							   (cdr ith))))
					(third (cdr (assoc 'hl-list
							   (cdr ith)))))) 2)
			      ((and (< (cdr (assoc 'PRED-HIGH (cdr ith)))
				       (cdr (assoc 'PRED-LOW (cdr ith))))
				    (or (< (cdr (assoc 'PRED-HIGH (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith))))
					(< (cdr (assoc 'PRED-LOW (cdr ith)))
					   (cdr (assoc 'end-price (cdr ith)))))
				    ) 0)))))
	   (pos (cond ((or (not (cdr (assoc 'PRED-LOW (cdr ith))))
			   (not (cdr (assoc 'PRED-HIGH (cdr ith))))) "N")
		      ((>  (cdr (assoc 'end-price (cdr ith)))
			   (max (cdr (assoc 'PRED-HIGH (cdr ith)))
				(cdr (assoc 'PRED-LOW (cdr ith))))) "O")
		      ((<  (cdr (assoc 'end-price (cdr ith)))
			   (min (cdr (assoc 'PRED-HIGH (cdr ith)))
				(cdr (assoc 'PRED-LOW (cdr ith))))) "U")
		      ((< (cdr (assoc 'PRED-HIGH (cdr ith)))
			  (cdr (assoc 'PRED-LOW (cdr ith)))) "A")
		      (t "B"))))
      (if (assoc 'MCODE ith)
	  (setf (cdr (assoc 'MCODE ith)) (string-append dir pos))
	(nconc ith (list (cons 'MCODE (string-append dir pos)))))
      (if (assoc 'SCODE ith)
	  (setf (cdr (assoc 'SCODE ith)) spec)
	(nconc ith (list (cons 'SCODE spec)))))))

;;;;this creats the "next" code
(defun encode2 (trends)
  (dolist (ith trends)
    (let* ((dir  (case  (cdr (assoc 'UP-DN (cdr ith)))
		   (HIGH "U")
		   (LOW  "D")))
	   (pos (cond ((or (not (cdr (assoc 'PRED-LOW (cdr ith))))
			   (not (cdr (assoc 'PRED-HIGH (cdr ith))))) "N")
		      ((>  (cdr (assoc 'end-price (cdr ith)))
;		      ((>  (cdr (or (assoc 'price-to-go (cdr ith))
;				    (assoc 'end-price (cdr ith))))
			   (max (cdr (assoc 'PRED-NEXT (cdr ith)))
				(cdr (assoc (if (equal dir "U") 'PRED-HIGH
					      'PRED-LOW)
					    (cdr ith))))) "O")
		      ((<  (cdr (assoc 'end-price (cdr ith)))		      
;		      ((<  (cdr (or (assoc 'price-to-go (cdr ith))
;				    (assoc 'end-price (cdr ith))))
			   (min (cdr (assoc 'PRED-NEXT (cdr ith)))
				(cdr (assoc (if (equal dir "U") 'PRED-HIGH
					      'PRED-LOW)
					    (cdr ith))))) "U")
		      ((and (equal dir "U")
			    (> (cdr (assoc 'pred-next (cdr ith)))
			       (cdr (assoc  'PRED-HIGH (cdr ith))))) "A")
		      ((and (equal dir "D")
			    (< (cdr (assoc 'pred-next (cdr ith)))
			       (cdr (assoc  'PRED-LOW (cdr ith))))) "A")
		      (t "B"))))
      (if (assoc 'NCODE ith)
	  (setf (cdr (assoc 'NCODE ith)) (string-append dir pos))
	(nconc ith (list (cons 'NCODE (string-append dir pos))))))))
;;;creates the previous code
(defun encode3 (trends)
  (dolist (ith trends)
    (let* ((dir  (case  (cdr (assoc 'UP-DN (cdr ith)))
		   (HIGH "U")
		   (LOW  "D")))
	   (pos (cond ((not (cdr (assoc 'PRED-PREV (cdr ith)))) "N")
		      ((>  (cadr (assoc 'HL-LIST (cdr ith)))
			   (cdr (assoc 'PRED-PREV (cdr ith)))) "O")
		      ((<  (cadr (assoc 'HL-LIST (cdr ith)))		      
			   (cdr (assoc 'PRED-PREV (cdr ith)))) "U")
		      (t "B"))))
      (if (assoc 'PCODE ith)
	  (setf (cdr (assoc 'PCODE ith)) (string-append dir pos))
	(nconc ith (list (cons 'PCODE (string-append dir pos))))))))


(DEFUN ENCODE-AGE (trends)
  (dolist (ith trends)
    (if (assoc 'AGE ith)
	(setf (cdr (assoc 'AGE ith))
	      (my-round
			 (/ (- *end-index*
			       (car (cdr (assoc 'index-list (cdr ith)))))
			    (cdr (assoc  '*n-filt* (cdr ith)))) 1))
    (NCONC ITH
	   (LIST (CONS 'AGE
		       (my-round
			 (/ (- *end-index*
			       (car (cdr (assoc 'index-list (cdr ith)))))
			    (cdr (assoc  '*n-filt* (cdr ith)))) 1)))))))
;;;used in TRND file
(defun CODE-meaning (ith code ncode AGE scode)
 (let ((phigh (cdr (assoc 'pred-high ith)))
       (plow (cdr (assoc 'pred-low ith)))
       (pcode (cdr (assoc 'pcode ith))))
   (if (stringp pcode) (setq pcode (read-from-string pcode)))
  (if (stringp code) (setq code (read-from-string code)))
  (if (stringp ncode) (setq ncode (read-from-string ncode)))
  (case code
    (DB (case ncode
	  (UU (case scode
		(0 "NO COMPELLING MESSAGE")
		(2 (IF (< age 2) "WEAK BEARISH" "NO COMPELLING MESSAGE"))
		(1 (cond ((< age 1) "BEARISH" )
			 ((< age 1.5) "WEAK BEARISH")
			 (t "NO COMPELLING MESSAGE")))
		(3 (cond ((< age 1) "VERY BEARISH" )
			 ((< age 1.5) "BEARISH")
			 (t "NO COMPELLING MESSAGE")))
		(otherwise
		 (case pcode
		   (UO (format nil "likely BOTTOM over ~A "
			(if (index-32sp)
			    (my-round (convert-to-32nds
					(cdr (assoc 'pred-low ith))) 2)
			  (cdr (assoc 'pred-low ith)))))
		   (UU (format nil "likely stay under ~A on next rise"
			  (if (index-32sp)
			      (my-round (convert-to-32nds
					  (cdr (assoc 'pred-high ith))) 2)
			    (cdr (assoc 'pred-high ith)))))
		   (otherwise (format nil "likely stay under ~A on next rise"
			   (if (index-32sp)
			       (my-round (convert-to-32nds
					   (cdr (assoc 'pred-high ith))) 2)
			     (cdr (assoc 'pred-high ith)))))))))
	  (otherwise (case scode
		       ((0 2) "NO COMPELLING MESSAGE")
		       (1 (cond ((< age 1) "BEARISH")
				((< age 2) "NO COMPELLING MESSAGE")
				(t "EXPECT LAST DITCH UPTURN")))
		       (3 (cond ((< age 1) "BEARISH")
				((< age 2) "NO COMPELLING MESSAGE")
				(t "EXPECT UPTURN SOON")))
		       (otherwise "NO COMPELLING MESSAGE")))))
    (UB (case ncode
	  (DO (case scode
		(0 "NO COMPELLING MESSAGE")
		(2 (if (< age 2) "WEAK BULLISH" "NO COMPELLING MESSAGE"))
		(1 (cond ((< age 1) "BULLISH")
			 ((< age 1.5) "WEAK BULLISH")
			 (t "NO COMPELLING MESSAGE")))
		(3 (cond ((< age 1) "VERY BULLISH")
			 ((< age 1.5) "BULLISH")
			 (t "NO COMPELLING MESSAGE")))
		(otherwise
		 (case pcode
		   (DO (format nil "likely stay over ~A on next fall"
				   (if (index-32sp)
				       (my-round (convert-to-32nds
						   (cdr (assoc
							  'pred-low ith))) 2)
				     (cdr (assoc 'pred-low ith)))))
		   (DU (format nil "likely TOP under ~A "
			       (if (index-32sp)
				   (my-round (convert-to-32nds
					       (cdr (assoc 'pred-high ith))) 2)
				 (cdr (assoc 'pred-high ith)))))
		   (otherwise (format nil "likely stay over ~A on next fall"
			(if (index-32sp)
			    (my-round (convert-to-32nds
					(cdr (assoc 'pred-low ith))) 2)
			  (cdr (assoc 'pred-low ith)))))))))
	  (otherwise (case scode
		       ((0 2) "NO COMPELLING MESSAGE")
		       (1 (cond ((< age 1) "BULLISH")
				((< age 2) "NO COMPELLING MESSAGE")
				(t "EXPECT LAST DITCH DOWNTURN")))
		       (3 (cond ((< age 1) "BULLISH")
				((< age 2) "NO COMPELLING MESSAGE")
				(t "EXPECT DOWNTURN SOON")))
		       (otherwise "NO COMPELLING MESSAGE")))))
    (UO (case ncode
	  (DO (cond ((< age 2) "VERY BULLISH")
		    ((< age 3) "BULLISH")
		    (t "AGED BULLISH")))
	  (DB (if (gtr plow phigh)
		  (cond ((< age 1) "EXTREME BULLISH")
			((< age 3) "EXTREME BULLISH")
			((< age 6) "EXTREME BULLISH\; EXTENDED WAVE")
			(t "LIKELY A HIGH HAS FORMED OR WILL SOON"))
		(case scode
		  (0 (if (eql pcode 'DO) "BULLISH"
			 (cond ((< age 1) "BULLISH")
			   ((< age 2) "BULLISH")
			   ((< age 3) "AGED BULLISH")
			   (t "LIKELY A HIGH HAS FORMED OR WILL SOON"))))
		  (2 (if (eql pcode 'DO) "BULLISH" "NO COMPELLING MESSAGE"))
		  (1 (if (< age 3) "BULLISH" "NO COMPELLING MESSAGE"))
		  (3 (if (< age 3) "BULLISH" "NO COMPELLING MESSAGE"))
		  (otherwise (if (or (< age 3)(eql pcode 'DO)) "BULLISH"
			       "NO COMPELLING MESSAGE")))))))
    (DU (case ncode
	  (UB (if (gtr plow phigh)
		  (cond ((< age 1) "EXTREME BEARISH")
			((< age 3) "EXTREME BEARISH")
			((< age 6) "EXTREME BEARISH\; EXTENDED WAVE")
			(t "LIKELY A LOW HAS FORMED OR WILL SOON"))
		  (case scode
		(0 (if (eql pcode 'UU) "BEARISH"
		       (cond ((< age 1) "BEARISH")
			 ((< age 2) "BEARISH")
			 ((< age 3) "AGED; SOON TO BOTTOM")
			 (t "LIKELY A LOW HAS FORMED OR WILL SOON"))))
		(2 (if (eql pcode 'UU) "BEARISH" "NO COMPELLING MESSAGE"))
		(1 (if (< age 3) "BEARISH" "NO COMPELLING MESSAGE"))
		(3 (if (< age 3) "BEARISH" "NO COMPELLING MESSAGE"))
		(otherwise (if (or (< age 3)(eql pcode 'UU)) "BEARISH"
			     "NO COMPELLING MESSAGE")))))
	  (UU (cond ((< age 2) "VERY BEARISH")
		    ((< age 3) "BEARISH")
		    (t "AGED BEARISH")))))
    (DO (case pcode
	  (UO (case ncode
		(UB "NO COMPELLING MESSAGE")
		(UO (case scode
		      ((0 2) (format nil "likely BOTTOM above ~A"
			       (if (index-32sp)
				   (my-round (convert-to-32nds
					       (cdr (assoc 'pred-low ith))) 2)
				 (cdr (assoc 'pred-low ith)))))
		      (otherwise (if (< age 2) "NO COMPELLING MESSAGE"
				   "LIKELY TO RALLY SOON"))))))
	  (UU (case ncode
		(UB  (format nil "target ~A "
			     (if (index-32sp)
				 (my-round (convert-to-32nds
					     (cdr (assoc 'pred-low ith))) 2)
			       (cdr (assoc 'pred-low ith)))))
		(UO (case scode
		      ((0 2)
		       (format nil "target ~A "
			       (if (index-32sp)
				   (my-round (convert-to-32nds
					       (cdr (assoc 'pred-low ith))) 2)
				 (cdr (assoc 'pred-low ith)))))
		      (otherwise
		       (format nil  "target ~A "
			       (if (index-32sp)
				   (my-round
				     (convert-to-32nds
				       (cdr (assoc 'pred-low ith))) 2)
				 (cdr (assoc 'pred-low ith)))))))))
	  (otherwise
	   (case ncode
	     (UB "NO COMPELLING MESSAGE")
	     (UO (case scode
		   ((0 2) (format nil "likely BOTTOM above ~A"
			       (if (index-32sp)
				  (my-round (convert-to-32nds
					      (cdr (assoc 'pred-low ith))) 2)
				 (cdr (assoc 'pred-low ith)))))
		   (otherwise (if (< age 3) "NO COMPELLING MESSAGE"
				"LIKELY TO RALLY SOON"))))))))
    (UU (case pcode
	  (DO (case ncode
		(DB (format nil "target ~A "
			    (if (index-32sp)
			      (my-round (convert-to-32nds
					  (cdr (assoc 'pred-high ith))) 2)
			      (cdr (assoc 'pred-high ith)))))
		
		(DU (case scode
		      ((0 2) 
		       (format nil "target ~A "
			       (if (index-32sp)
			     (my-round (convert-to-32nds
					 (cdr (assoc 'pred-high ith))) 2)
				 (cdr (assoc 'pred-high ith)))))
		      (otherwise
		       (format nil "target ~A "
			       (if (index-32sp)
				   (my-round (convert-to-32nds
					       (cdr (assoc 'pred-high ith))) 2)
				 (cdr (assoc 'pred-high ith)))))))))
	 (DU (case ncode
	       (DB "NO COMPELLING MESSAGE")
	       (DU (case scode
		     ((0 2) (format nil "likely TOP under ~A "
				    (if (index-32sp)
				    (my-round
				      (convert-to-32nds
					(cdr (assoc 'pred-high ith))) 2)
				      (cdr (assoc 'pred-high ith)))))
		     (otherwise (if (< age 2) "NO COMPELLING MESSAGE"
				  "LIKELY TO FALL SOON"))))))
	 (otherwise
	  (case ncode
	    (DB "NO COMPELLING MESSAGE")
	    (DU (case scode
		  ((0 2) (format nil "likely TOP under ~A "
				    (if (index-32sp)
				    (my-round
				      (convert-to-32nds
					(cdr (assoc 'pred-high ith))) 2)
				      (cdr (assoc 'pred-high ith)))))
		  (otherwise (if (< age 3) "NO COMPELLING MESSAGE"
			       "LIKELY TO FALL SOON"))))))))
    (otherwise "NONE"))))


;;;access functions
(defun GET-TPV (index)
  (multiple-value-bind (subarray subindex)(truncate index *TP-subdim*)
    (if (vectorp (aref *TP* subarray))
	(aref (aref *TP* subarray) subindex))))

    
;;;access function for price and momentum values from the data vectors
;;;item is either 'price or a filter size (integer)
(defun get-tpv-val (index item)
  (cond ((not (numberp index)))
	((eql item 'price) (and (vectorp (get-tpv index))(svref (get-TPV index) 3)))
	((and (numberp item) (>= (- index item) 0))
	 (sub (and (vectorp (get-tpv index))(svref (get-TPV index) 3))
	    (and (vectorp (get-tpv (- index item)))(svref (get-TPV (- index item)) 3))))))


(defun PUT-TPV (index item)
  (multiple-value-bind (subarray subindex)(truncate index *TP-subdim*)
    (ifn (vectorp (aref *TP* subarray))
	 (setf (aref *TP* subarray)
	       (make-array *TP-subdim* :initial-element nil)))
    (setf (aref (aref *TP* subarray) subindex) item)))
	 
(defun NIL-TPV ( &aux dmy (cntr 0) (dmy2 (length *TP*)))
  (loop
    (if (>= cntr dmy2) (return))
    (setq dmy (aref *TP* cntr))
    (if (vectorp dmy)
	(dotimes (ith *TP-subdim*)
	  (setf (aref dmy ith) nil)))
    (incf cntr)))
	   
(defun find-new-indices (times-list last-index)
  (let (nindex-list day tim)
    (setq times-list (reverse times-list)
	  day (getnumdate (car times-list))
	  tim (getnumh (car times-list)))
    (dotimes (ith (1+ last-index))
      (cond ((not day) (return))
	    ((and (eql day (and (vectorp (get-tpv ith))(svref (get-tpv ith) 1)))
		  (eql tim (and (vectorp (get-tpv ith))(svref (get-tpv ith) 2))))
	     (push ith nindex-list) (pop times-list)
	     (setq day (getnumdate (car times-list))
		   tim (getnumh (car times-list))))
	    (t nil))) nindex-list))


;;;here's the test if need to add data since last primitive
(defun add-tpv-data (cur-start-index cur-startday cur-starttime endday endtime
				     hww   &aux (last-index 0))
  (setq hww hww)
  (when (and (eql (and (vectorp (get-tpv cur-start-index))(svref (get-TPV cur-start-index) 1))
		  cur-startday)
	     (eql (and (vectorp (get-tpv cur-start-index))(svref (get-TPV cur-start-index) 2))
		  cur-starttime))
;LUCID-PC DIFFERENCE
;    (format hww "~% ADDING DATA SINCE LAST PRIMITIVE")
;;;find index of first nil value      
    (setq last-index
	  (if (get-TPV 0)
	      (do ((ith 1 (1+ ith)))
		  ((or (stringp (and (vectorp (get-tpv ith))(svref (get-TPV ith) 1)))
		       (null (get-TPV ith))
		       (and (<= endday (and (vectorp (get-tpv ith))(svref (get-TPV ith) 1)))
			    (or (not endtime)
				(case endtime
				  (A t)
				  (P (eql 'P (and (vectorp (get-tpv last-index))(svref (get-TPV last-index) 2))))
				  (t (<= (time-units-into-day endday endtime)
					 (time-units-into-day
					   (and (vectorp (get-tpv last-index))(svref (get-TPV last-index) 1))
					 (and (vectorp (get-tpv last-index))(svref (get-TPV last-index) 2)))))))))
		  (and (vectorp (get-tpv (1- ith))) (svref (get-TPV (1- ith)) 0)))) 0))
;;;add the new data	
    (do ((index (1+ last-index) (1+ index))
	 (nxtday (svref (get-TPV last-index) 1))
	 (nxttime (svref (get-TPV last-index) 2))
	 (nxtprice (svref (get-TPV last-index) 3)))
	((and (<= endday nxtday)
	      (or (not endtime)
		  (case endtime
		    (A t)
		    (P (eql 'P nxttime))
		    (t (<= (time-units-into-day endday endtime)
			   (time-units-into-day nxtday nxttime))))))
	 (SETQ *END-INDEX* (1- INDEX))
	 ;LUCID-PC DIFFERENCE
	 ;(format hww "  ~A PRICES" (- *end-index* last-index))
	 )
      ;	     (format t "~%*END-INDEX*= ~A" *end-index*))
      (multiple-value-setq (nxtday nxttime nxtprice)
	(next-data-point nxtday nxttime nxtprice))
      (put-TPV index (vector index nxtday nxttime nxtprice))))
  last-index)

	  
(defun getnumh (tim)
  (let ((xx (getnumhour tim)) aa ignore)
    (declare (ignore ignore))
    (cond ((and (eql xx 'C)
     		(eql *time-interval* 'daily-high-low))
	   (multiple-value-setq (ignore ignore ignore aa)
	     (data-access (getnumdate tim)))
	   (second aa))
	  ((neql xx 'C) xx))))


(defun tpv-vector-data (startday starttime startprice endday endtime
				 priorday priortime priorprice
				 nextday nexttime nextprice last-index hww)
  (setq hww hww)
;;;test if need to add new data to the array
      (when (and last-index
		 (eql startday (and (vectorp (get-tpv 1))(svref (get-TPV 1) 1)))
		 (eql starttime (and (vectorp (get-tpv 1))(svref (get-TPV 1) 2)))
		 (eql startprice (and (vectorp (get-tpv 1))(svref (get-TPV 1) 3)))
		 (eql priorday (and (vectorp (get-tpv 0))(svref (get-TPV 0) 1)))
		 (eql priortime (and (vectorp (get-tpv 0))(svref (get-TPV 0) 2)))
		 (eql nextday (and (vectorp (get-tpv 2))(svref (get-TPV 2) 1)))
		 (eql nexttime (and (vectorp (get-tpv 2))(svref (get-TPV 2) 2))))
	
	(do ((index (1+ last-index) (1+ index))
	     (nxtday (and (vectorp (get-tpv last-index))(svref (get-TPV last-index) 1)))
	     (nxttime (and (vectorp (get-tpv last-index))(svref (get-TPV last-index) 2)))
	     (nxtprice (and (vectorp (get-tpv last-index))(svref (get-TPV last-index) 3))))
	    ((or (< endday nxtday)
		 (and (= endday nxtday)
		  (or (not endtime)
		      (case endtime
			(A t)
			(P (eql 'P nxttime))
			(t (<= (time-units-into-day endday endtime)
			       (time-units-into-day nxtday nxttime)))))))
	     (SETQ *END-INDEX* (1- INDEX)))
;	     (format t "~%*END-INDEX*= ~A" *end-index*))
	  (multiple-value-setq (nxtday nxttime nxtprice)
	    (next-data-point nxtday nxttime nxtprice))
	  (put-TPV index (vector index nxtday nxttime nxtprice))))

;;;test if need to fill a new array		   
      (unless (and (get-TPV 0)
		   (eql startday (and (vectorp (get-tpv 1))(svref (get-TPV 1) 1)))
		   (eql starttime (and (vectorp (get-tpv 1))(svref (get-TPV 1) 2)))
		   (eql priorday (and (vectorp (get-tpv 0))(svref (get-TPV 0) 1)))
		   (eql priortime (and (vectorp (get-tpv 0))(svref (get-TPV 0) 2)))
		   (eql nextday (and (vectorp (get-tpv 2))(svref (get-TPV 2) 1)))
		   (eql nexttime (and (vectorp (get-tpv 2))(svref (get-TPV 2) 2)))
		   (and (get-TPV *end-index*)
			(<= endday
			    (and (vectorp (get-tpv *end-index*))(svref (get-TPV *end-index*) 1))))
		   (if endtime
		       (case endtime
			 (A t)
			 (P (eql 'P
				  (svref (get-TPV
					       *end-index*) 2)))
			 (t (<= (time-units-into-day endday endtime)
				(time-units-into-day
				  (and (vectorp (get-tpv *end-index*))(svref (get-TPV *end-index*) 1))
				(and (vectorp (get-tpv *end-index*))
				(svref (get-TPV *end-index*) 2))))))
;		     (not (svref (get-TPV *end-index*) 2))
		     t))

	;LUCID-PC DIFFERENCE
	;(format hww "~% INITIALIZING THE DATA VECTOR AT ~A ~A"
	;	startday starttime)

	(nil-TPV);sets all values to nil
	;;;now fill the data vectors	
	;;;start with the startday at index 1
	(put-TPV 1 (vector 1 startday starttime startprice))
	;;;fill the 1st point prior to the startday

	(put-TPV 0 (vector 0 priorday priortime priorprice))
	;;;fill the next points until the end-time      
	(put-TPV 2 (vector 2 nextday nexttime nextprice))
	(do ((index 3 (1+ index)))
	    ((and (= endday nextday) (or (not endtime) (eql endtime nexttime)))
	     (SETQ *END-INDEX* (1- INDEX)))
	    
	  (multiple-value-setq (nextday nexttime nextprice)
	    (next-data-point nextday nexttime nextprice))
	  (put-TPV index (vector index nextday nexttime nextprice)))
	(setq last-index *end-index*))
      )

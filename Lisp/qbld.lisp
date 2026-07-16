;;; -*- Mode: LISP; Package: USER; Base: 10. -*-
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
;;; USE THIS IN BUILDING A COUNT
;;;;this is a function that builds q vectors
;;;it depends on having wave 0 and *time-interval* set properly
;;;startday is 6 digits integer starttime is 4 digit integer HHMM
;;;direction may be given or calculated (start price less end price)
;;;
(defun FILL-Qvector (startday starttime endday endtime
			&optional (dir nil) ;(primw *standard-output*)
			&AUX TDATE  NDATE TIME-LIST PRICE-LIST ADHOC-LIST
			VOL-LIST f-o extra-vol ignore)
  (DECLARE (IGNORE IGNORE))
  (block QBD
    (if endday (putv 0 et (conv-to-string ENDDAY ENDTIME))
      (PUTV 0 ET NIL))
    (putv 0 st (conv-to-string STARTDAY STARTTIME))
    (putv 0 sp (getd startday 'pdata starttime))
    (ifn (getv 0 sp) (return-from QBD NIL))
    (if endday (putv 0 ep (getd endday 'pdata endtime))
      (putv 0 ep nil))
    (or dir endday 
;	(format primw "USER ERROR! QBUILD NEEDS EITHER ENDTIME OR DIRECTION!")
	(return-from QBD nil))
    (if dir (putv 0 dr dir)
      (if (> (getv 0 sp) (getv 0 ep))
	  (putv 0 dr 'down) (putv 0 dr 'up)))
    ;;;the next six data values 'tl 'va 'vp 'vd 'as 'ap
    ;;;must be obtained by looping thru the data start-time to end-time
    ;;;first let's do the startday from the starttime to the end of the day
    (let ((volume-total 0)  first-index last-index (a-dadu 0) (admax 0) dmy)
	   (multiple-value-setq (startday ignore ndate time-list price-list
					  adhoc-list
					  vol-list f-o IGNORE extra-vol)
	     (data-access startday))	   
	   ;;;stop at endtime if same day
	   (putv 0 tl (if (and (car time-list)
				(not (member *time-interval*
						'(1440 720 daily-high-low))))
				;(numberp starttime))
			 (if (minusp (setq dmy
					   (or (sub-times-in-hours starttime
						(or (and (eql startday endday)
						    endtime)
					       (car (last time-list)))) 0)))
			     (+ 24 dmy) dmy)
			 (case starttime
			   (A 12)
			   (P 0)
			   (otherwise 0))))
	   (putv 0 hp (if (and (car time-list) (not (member *time-interval*
						'(1440 720 daily-high-low))))
	    (apply #'max (subseq price-list
				 (setq first-index
				       (position starttime
						 time-list))
				 (setq last-index
				       (1+ (position   
					     (or (and (eql startday endday)
						      endtime)
						 (car (last time-list)))
					     time-list)))))
		    (case starttime
		      (A (apply #'max price-list))
		      (P (cadr price-list))
		      (otherwise (car price-list)))))

	   (putv 0 lp (if (and (car time-list)
				(not (member *time-interval*
						'(1440 720 daily-high-low))))
		      (apply #'min (subseq price-list
					   first-index last-index))
		    (case starttime
		      (A (apply #'min price-list))
		      (P (cadr price-list))
		      (otherwise (car price-list)))))

	   (when vol-list
	     (if (and (car time-list)(not (member *time-interval*
						'(1440 720 daily-high-low))))
		 (putv 0 va
		       (ifn (zerop (getv 0 tl))
			    (/ (apply #'+
				      (subseq vol-list
					      (setq first-index
						    (1+ (position starttime
								  time-list)))
					      (setq last-index
						    (1+
						      (position   
							  (or
							    (and
							      (eql startday
								   endday)
							      endtime)
							    (car (last
								   time-list)))
							  time-list)))))
			       (float (getv 0 tl)))
			 0))
	       (putv 0 va
		     (case starttime
		       (A (/ (car vol-list) (float (getv 0 tl))))
		       (P 0)
		       (otherwise 0))))
	     (setq volume-total (* (getv 0 va) (getv 0 tl)))

	     (ifn (zerop (getv 0 tl))
		  (if (and (car time-list)(not (member *time-interval*
						'(1440 720 daily-high-low))))
		      (do* ((vlist (subseq vol-list first-index last-index)
				   (cdr vlist))
			    (vol (car vlist) (car vlist))
			    (plist (subseq price-list (1- first-index)
					   last-index)
				   (cdr plist))
			    (price-1 (car plist) (car plist))
			    (price (second plist) (second plist))
			    (volp 0)
			    (vold 0))
			   ((null vlist) (putv 0 vp volp) (putv 0 vd vold))
			(cond ((> price price-1)
			       (if (> vol volp)(setq volp vol)))
			      ((< price price-1)
			       (if (> vol vold)(setq vold vol)))))
		    (case starttime
		      (A (cond ((< (car price-list) (cadr price-list))
				 (putv 0 vp (cadr vol-list))
				 (putv 0 vd 0))
				((> (car price-list) (cadr price-list))
				 (putv 0 vd (cadr vol-list))
				 (putv 0 vp 0))))
		      (P (putv 0 vp 0) (putv 0 vd 0))
		      (otherwise (putv 0 vp 0) (putv 0 vd 0))))
	       (progn (putv 0 vp 0) (putv 0 vd 0))
	       );;closes the ifn
	     );;closes the when
	   (putv 0 ap 0)(putv 0 as 0)
	   (when (or (and (assoc 'advances adhoc-list)
			  (neql starttime (car (last time-list)))
		      (neql startday endday))
		     (and (cdr (assoc 'advances adhoc-list)) (eql endtime
						(car (last time-list)))
			  (eql startday endday)))
	     (setq admax (/ (cdr (assoc 'advances adhoc-list))
			    (float (cdr (assoc 'declines adhoc-list)))))
	     (setq a-dadu (/ (float (- (cdr (assoc 'advances adhoc-list))
				       (cdr (assoc 'declines adhoc-list))))
			     (apply #'+  (list
					(cdr (assoc 'advances adhoc-list))
					(cdr (assoc 'declines adhoc-list))
					(cdr (assoc 'unchanged adhoc-list))))))
	     (putv 0 as  (ifn (zerop (getv 0 tl))
			       (/ (* 100 A-DADU) (float (getv 0 tl))) 0))
	     (putv 0 ap  admax))

	   
 ;;;read another day of data
 ;;;next let's do the full days
	   (multiple-value-setq (tdate ignore ndate time-list price-list
				       adhoc-list vol-list volume-total a-dadu
				       admax extra-vol f-o)
	     (add-more-days-data-qbld startday endday ndate volume-total a-dadu admax
				      extra-vol f-o))

	   (if (or (stringp tdate) (not ndate))
	       (return-from QBD nil))
 ;;;and last let's do the beginning of the end or stop day to the end or
 ;;; stop time Note this is not used if endday is complete.
	   (unless (eql startday endday)
	     (multiple-value-setq
		 (tdate ignore ndate time-list price-list adhoc-list vol-list 
			volume-total a-dadu admax extra-vol f-o)
	       (last-day-data-qbld
		 tdate endday endtime ndate volume-total a-dadu admax
		 extra-vol f-o))	     
	     (if (or (stringp tdate) (not tdate))
		 (return-from QBD nil)))
	);;closes the let
    (putv 0 FT (fibo-term (max-lgth% 0)))   t))


;;;;;;
(defun add-more-days-data-qbld (tdate endday ndate volume-total a-dadu admax
				       extra-vol f-o)
  (let (ydate time-list price-list adhoc-list vol-list ignore dmy
	      close time-to-close)
    (DECLARE (IGNORE IGNORE))
  (loop
    ;;;check if next day is a full day
    (cond ((or (not endday) (> endday ndate))
	   (setq close (getd tdate 'close-time)
		 time-list (getd tdate 'ptime))
	   (if close
	       (setq time-to-close
		     (if (minusp
			   (setq dmy
				 (or (sub-times-in-hours
				       (car (last time-list)) close) 0)))
			 (+ dmy 24) dmy))
	     (setq time-to-close 0))

	   (multiple-value-setq (tdate ydate ndate time-list
				       price-list adhoc-list
				       vol-list f-o IGNORE extra-vol)
	     (data-access ndate))
	   (if (stringp tdate) (return nil))
	   (putv 0 tl
		 (if (and (car time-list)
			  (not (member *time-interval*
			         '(1440 720 daily-high-low))))
			  ;(numberp (car time-list)))
		     (+ (getv 0 tl) time-to-close
			(if (minusp
			      (setq dmy (or (sub-times-in-hours (car time-list)
					    (car (last time-list))) 0)))
			    (+ 24 dmy) dmy)
			f-o)
		   (+ (getv 0 tl) 24)))
	   (putv 0 hp
		 (apply #'max (cons (getv 0 hp) price-list)))
	   (putv 0 lp
		 (apply #'min (cons (getv 0 lp) price-list)))

	   (when vol-list 
	     (if (and (car time-list)(not (member *time-interval*
						  '(1440 720 daily-high-low))))
		 (progn

		   (putv 0 va (/ (+ volume-total
				     (apply #'+ vol-list))
				  (float (getv 0 tl))))
		   (setq volume-total (* (getv 0 va) (getv 0 tl)))

		   (do* ((vlist vol-list (cdr vlist))
			 (vol (+ (car vlist) extra-vol) (car vlist))
			 (plist (cons (car (last (getd ydate 'pdata)))
				      price-list)
				(cdr plist))
			 (price-1 (car plist) (car plist))
			 (price (second plist) (second plist))
			 (volp (getv 0 vp))
			 (vold (getv 0 vd)))
			((null vlist) (putv 0 vp volp) (putv 0 vd vold))
		     (cond ((> price price-1)
			    (if (> vol volp)(setq volp vol)))
			   ((< price price-1)
			    (if (> vol vold)(setq vold vol))))))
	       (case (length vol-list)
		 (1  
		   (putv 0 va (/ (+ volume-total (car vol-list))
				  (float (getv 0 tl))))
		   (setq volume-total (* (getv 0 va) (getv 0 tl)))
		   (cond ((> (car price-list)
			     (car (getd ydate 'pdata)))
			  (putv 0 vp (max (getv 0 vp)
					   (car vol-list))))
			 ((< (car price-list)
			     (car (getd ydate 'pdata)))
			  (putv 0 vd (max (getv 0 vd)
					   (car vol-list))))))
		 (2 (putv 0 va (/ (+ volume-total (car vol-list)
				      (cadr vol-list))
				   (float (getv 0 tl))))
		    (setq volume-total (* (getv 0 va) (getv 0 tl)))
		    (cond ((> (car price-list)
			      (car (getd ydate 'pdata)))
			   (putv 0 vp (max (getv 0 vp)
					    (car vol-list))))
			  ((< (car price-list)
			      (car (getd ydate 'pdata)))
			   (putv 0 vd (max (getv 0 vd)
					    (car vol-list)))))
		    (cond ((> (cadr price-list)
			      (car price-list))
			   (putv 0 vp (max (getv 0 vp)
					    (cadr vol-list))))
			  ((< (cadr price-list)
			      (car price-list))
			   (putv 0 vd (max (getv 0 vd)
					    (cadr vol-list)))))))))
	   
	   (when (cdr (assoc 'advances adhoc-list))
	     (setq a-dadu (+ a-dadu
			     (/ (float (- (cdr (assoc 'advances adhoc-list))
					  (cdr (assoc 'declines adhoc-list))))
				(apply #'+
				       (list
					 (cdr (assoc 'advances
						     adhoc-list))
					 (cdr (assoc 'declines
						     adhoc-list))
					 (cdr (assoc 'unchanged
						     adhoc-list)))))))
	     (if (> (/ (cdr (assoc 'advances adhoc-list))
		       (float (cdr (assoc 'declines adhoc-list)))) admax)
		 (setq admax (/ (cdr (assoc 'advances adhoc-list))
				(float (cdr (assoc 'declines adhoc-list))))))
	     ));closes the first clause of cond
	  (t (return))))
  (values tdate ydate ndate time-list price-list adhoc-list vol-list 
	       volume-total a-dadu admax extra-vol f-o)))
;;;;;
(defun last-day-data-qbld (tdate endday endtime ndate volume-total a-dadu admax
				  extra-vol f-o)
  (block nil
  (let (ydate time-list price-list adhoc-list vol-list IGNORE dmy
	      close time-to-close)
    (DECLARE (IGNORE IGNORE))
	   (when (and ndate (or (not endday) (= endday ndate)))
	     (setq close (getd tdate 'close-time)
		   time-list (getd tdate 'ptime))
	     (if close
		 (setq time-to-close
		       (if (minusp
			     (setq dmy
				(or (sub-times-in-hours
				     (car (last time-list)) close) 0)))
			   (+ dmy 24) dmy))
	       (setq time-to-close 0))
	     (multiple-value-setq (tdate ydate ndate time-list
					 price-list adhoc-list
					 vol-list f-o IGNORE extra-vol)
	       (data-access ndate))
	     (if (stringp tdate) (return nil))
	     (putv 0 tl
		   (if (and (car time-list)
			    (not (member *time-interval*
					 '(1440 720 daily-high-low))))
			    ;(numberp (car time-list)))
		       (+ (getv 0 tl) time-to-close
			  (if (minusp
				(setq dmy
				   (or (sub-times-in-hours (car time-list)
					(or endtime (car (last time-list))))
				       0)))
			      (+ 24 dmy) dmy)
			    f-o)
		     (case endtime
		       (A (+ (getv 0 tl) 12.0))
		       (P (+ (getv 0 tl) 24.0))
		       (otherwise  (+ (getv 0 tl) 24.0)))))
	     (putv 0 hp
		   (if (and (car time-list)(not (member *time-interval*
						'(1440 720 daily-high-low))))
		       (apply #'max
			      (cons (getv 0 hp)
				    (subseq price-list 0
					    (1+ (position
						  (or endtime
						      (car (last
							     time-list)))
							  time-list)))))
		     (case endtime
		       (A (apply #'max (cons (getv 0 hp)
					      (butlast price-list))))
		       (P (apply #'max (cons (getv 0 HP) price-list)))
		       (otherwise (max (getv 0 hp) (car price-list))))))
	     (putv 0 lp
		   (if (and (car time-list) (not (member *time-interval*
						'(1440 720 daily-high-low))))
		       (apply #'min
			      (cons (getv 0 lp)
				    (subseq price-list 0
					    (1+ (position
						  (or endtime
						      (car (last
							     time-list)))
						  time-list)))))
		     (case endtime
		       (A (apply #'min (cons (getv 0 lp)
					      (butlast price-list))))
		       (P (apply #'min (cons (getv 0 lp) price-list)))
		       (otherwise (min (getv 0 lp) (car price-list))))))
	     
	     (when vol-list 
	       (if (and (car time-list) (not (member *time-interval*
						'(1440 720 daily-high-low))))
	       (progn
		 (putv 0 va
		     (/ (+ volume-total 
			   (apply #'+
				  (subseq vol-list
					  0
					  (1+ (position        ;1+ stop index
						(or endtime
						    (car (last time-list)))
						time-list)))))
			(float (getv 0 tl))))
	       (do* ((vlist (subseq vol-list 0 (1+ (position endtime
							     time-list)))
			    (cdr vlist))
		     (vol (+ (car vlist) extra-vol) (car vlist))
		     (plist (cons (car (last (getd ydate 'pdata)))
				  (subseq price-list 0
					  (1+ (position endtime time-list))))
			    (cdr plist))
		     (price-1 (car plist) (car plist))
		     (price (second plist) (second plist))
		     (volp (getv 0 vp))
		     (vold (getv 0 vd)))
		    ((null vlist) (putv 0 vp volp) (putv 0 vd vold))
		 (cond ((> price price-1)
			(if (> vol volp)(setq volp vol)))
		       ((< price price-1)
			(if (> vol vold)(setq vold vol)))))
	       );closes the progn
		 (case endtime
		   (A (putv 0 va (/ (+ volume-total (car vol-list))
				      (float (getv 0 tl))))
		       (cond ((> (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 vp (max (getv 0 vp) (car vol-list))))
			     ((< (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 vd (max (getv 0 vd) (car vol-list))))))
		   (P (putv 0 va (/ (+ volume-total (car vol-list)
					 (cadr vol-list))
				     (float (getv 0 tl))))
		       (cond ((> (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 vp (max (getv 0 vp) (car vol-list))))
			     ((< (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 vd (max (getv 0 vd) (car vol-list)))))
		       (cond ((> (cadr price-list)
				 (car price-list))
			      (putv 0 vp (max (getv 0 vp) (cadr vol-list))))
			     ((< (cadr price-list)
				 (car price-list))
			      (putv 0 vd (max (getv 0 vd)
					       (cadr vol-list))))))
		   (otherwise (putv 0 va (/ (+ volume-total
						(car vol-list))
					     (float (getv 0 tl))))
			      (cond ((> (car price-list)
					(car (last (getd ydate 'pdata))))
				     (putv 0 vp (max (getv 0 vp)
						      (car vol-list))))
				    ((< (car price-list)
					(car (last (getd ydate 'pdata))))
				     (putv 0 vd (max (getv 0 vd)
						      (car vol-list)))))))
		 );closes the if time-list
	       );closes the when vol-list
	     (when (cdr (assoc 'advances adhoc-list))
	       (if (and (eql endtime (car (last time-list)))
			(> (/ (cdr (assoc 'advances adhoc-list))
			      (float (cdr (assoc 'declines adhoc-list))))
			   admax))
		   (setq admax (/ (cdr (assoc 'advances adhoc-list))
				  (float (cdr (assoc 'declines adhoc-list))))
			 a-dadu (+ a-dadu
				   (/ (- (cdr (assoc 'advances adhoc-list))
					 (float (cdr (assoc
						       'declines adhoc-list))))
				      (apply #'+
					     (list
					       (cdr (assoc 'advances
							   adhoc-list))
					       (cdr (assoc 'declines
							   adhoc-list))
					       (cdr (assoc 'unchanged
							   adhoc-list))))))))
	       (putv 0 ap admax)
	       (putv 0 as (/ (* 100 a-dadu) (float (getv 0 tl))))
	       );closes the when
	     )
	   (values
	     tdate ydate ndate time-list price-list adhoc-list vol-list 
	      volume-total a-dadu admax extra-vol f-o))))
;closes the last day test

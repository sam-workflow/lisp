;;; -*- Mode: LISP; Package: user; Base: 10. -*-
;;;
;;;builds wave 0 final results in wave 1 (not done here)
;;;returns 5 values
;;; the known time, the known price, the index, time and price of the
;;;end of the new primitive

;;;nextday refers to the day of the next price point it will in many cases
;;;not change from point to point
;;;priorday is similarly defined
;;;nexttime is the time of the next point (4 digits) HHMM.
;;;the stop-time is 10 digit string which simulates end of data at that time
;;;i.e. the computer does not see data later than stop-time
;;;PRICES-ONLY is the value of the start-index if called from
;;;find-all-trends
;;;fsize is the filter size when momentum primitives are being created
;;;instead of price primitives 
;;;
;;;*latest-dtime* is the time a primitive is known
;;;or its the stoptime sent as an argument
;;;or its the end of the data if no primitive and no stoptime given

;******************************************************************************
;;;;start-time and stop-time are strings

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


(defun provide-primitive (start-time &optional stop-time PRICES-ONLY
				     ;(primw *standard-output*)
				     (fsize 'price))
  (block prov
  (let ((startday (getnumdate start-time))
	(starttime (getnumhour start-time))
	startprice
	stopday stoptime nextindex nextday nexttime nextprice
	priorindex  ignore priorprice)
    (declare (ignore ignore))
    (setq *primitive* nil) (ifn *n-filt* (return-from prov "*n-filt*= nil"))
    (if (and (eql *time-interval* 1440) (neql starttime 'C)
	     (neql (getd startday 'close-time) starttime))
	(return-from prov "ERROR daily data not started at close of day"))
    (when stop-time
      (setq stopday  (getnumdate stop-time)
	    stoptime (getnumhour stop-time)))

;;;price is the 4th element of the list
    (if prices-only
	(setq startprice
	      (get-tpv-val prices-only fsize))
      (setq startprice (getd startday 'pdata starttime)))

;;;is the start-time a high or low?
;;;if high check if new low over *n-filt* points has occurred
;;;if low check if new high over *n-filt* points has occurred
    (MULTIPLE-VALUE-SETQ (nextday nexttime nextprice)
      (cond (prices-only
	      (setq nextindex (1+ prices-only))
	      (values (svref (get-TPV nextindex) 1)
		      (svref (get-tpv nextindex) 2)
		      (get-tpv-val nextindex fsize)))
	    (t (NEXT-DATA-POINT STARTDAY STARTTIME STARTPRICE))))
    
    (MULTIPLE-VALUE-SETQ (IGNORE IGNORE priorprice)
      (cond (prices-only
	      (setq priorindex (1- prices-only))
	      (values (svref (get-TPV priorindex) 1)
		      nil (get-tpv-val priorindex fsize)))
;	      (values-list (cdr (get-TPV priorindex))))
      (t (PRIOR-DATA-POINT STARTDAY STARTTIME startprice))))

    (when (and priorprice startprice nextprice
	       (= priorprice startprice nextprice))
      (loop
	(MULTIPLE-VALUE-SETQ (nextday nexttime nextprice)
	  (cond (prices-only
		  (setq nextindex (1+ nextindex))
		  (values (svref (get-TPV nextindex) 1)
			  (svref (get-tpv nextindex) 2)
			  (get-tpv-val nextindex fsize)))
		(t (NEXT-DATA-POINT nextDAY nextTIME nextPRICE))))
	(if (neql startprice nextprice)(return))))

	
    (cond ((< nextprice startprice)
	   (test-for-low startday starttime startprice stopday stoptime
			 PRICES-ONLY fsize)) 
	  ((> nextprice startprice)
	   (test-for-high startday starttime startprice stopday stoptime
			  PRICES-ONLY fsize))
	  ((< priorprice startprice)
	   (test-for-low startday starttime startprice stopday stoptime
			 PRICES-ONLY fsize))
	  ((> priorprice startprice)
	   (test-for-high startday starttime startprice stopday stoptime
			  PRICES-ONLY fsize))
	  (t nil)))))

(defun test-for-low (startday starttime startprice stopday stoptime PRICES-ONLY
			      fsize
			      &aux priorday priortime priorprice
			      priorindex dayindex test-index  minindex
			      PRICE DAY TIM findex
			      minprice minday mintime TEST-DAY TEST-TIME
			      lowindex lowday lowtime lowprice pri nofail
			      (age 0) fday ftim fpri highest-price)

  (multiple-value-setq (test-day test-time price)
    (cond (prices-only
	    (setq test-index (1+ prices-only))
	    (values (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 1))
		    (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 2))
		    (get-tpv-val test-index fsize)))
	  (t (setq test-index 1 findex 1)
	     (next-data-point startday starttime startprice))))
  (setq minindex test-index minprice price minday test-day mintime test-time)
  (loop  
    
    ;;;test-day and test-time defines the test point 
    ;;;check if price exceeds the startprice
;;;or if *n-filt* points have passed since the last low
    (if (or (> price startprice)
	    (and (> price minprice)
		 (>= (- test-index minindex) *n-filt*)))
	(setq lowindex minindex lowday minday lowtime mintime
	      lowprice minprice)
      ;;;compare a point (price) against the prior *n-filt* points to see
      ;;;if its higher than all of them
      (progn
	(block TL
;;;this part finds a fail day and its age 
;;;quick test if will fail must start full checking in successful
	(when (and age (not (zerop age)) (plusp (- *n-filt* age))
		  (minusp (- test-index findex *n-filt*)))
;	  (format t "age=~A fday=~A fpri=~A~%" age fday fpri)
	  (setq dayindex findex day fday tim ftim pri fpri)
	  (multiple-value-setq (nofail lowtime lowprice age findex
				       fday ftim fpri)
	    (dotimes (ith (- *n-filt* age) (values t nil minprice))
	      (multiple-value-setq (priorday priortime priorprice)
		(cond (prices-only
			(setq priorindex (1- dayindex))
			(values (and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 1))
				(and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 2))
				(get-tpv-val priorindex fsize)))
		      (t (setq priorindex (1- dayindex))
			 (prior-data-point day tim pri))))
	      (if (< priorday startday) (return (values t nil minprice)))
	      (cond ((> price priorprice)
		     (if (< priorprice minprice)
			 (setq minprice priorprice minday priorday
			       mintime priortime minindex priorindex))
		     (setq day priorday tim priortime pri priorprice
			   dayindex priorindex))
		    (t (if (< price minprice)
			   (setq minprice price minday test-day
				 mintime test-time minindex test-index))
;		       (format t "day=~A tim=~A pri=~A~%" day tim pri)
		       (return (values nil nil minprice
				       (+ ith age 1) dayindex day tim pri))))))
	  (ifn nofail (return-from TL))
	  ));closes the block
;;;this is where the full checking starts
	(when (or nofail (zerop age)
		  (not (minusp (- test-index findex *n-filt*))))
	  (setq dayindex test-index day test-day tim test-time pri price)
	  (multiple-value-setq (lowindex lowday lowtime lowprice
					 age findex fday ftim fpri)
	    (dotimes (ith *n-filt* (values minindex minday mintime minprice))
	      (multiple-value-setq (priorday priortime priorprice)
		(cond (prices-only
			(setq priorindex (1- dayindex))
			(values (and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 1))
				(and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 2))
				(get-tpv-val priorindex fsize)))
		      (t (setq priorindex (1- dayindex))
			 (prior-data-point day tim pri))))
	      
	      (cond ((> price priorprice)
		     (if (< priorprice minprice)
			 (setq minprice priorprice minday priorday
			       mintime priortime minindex priorindex))
		     (setq dayindex priorindex day priorday tim priortime
			   pri priorprice))
		    (t (if (< price minprice)
			   (setq minprice price minday test-day
				 minindex test-index
				 mintime test-time))
		       (return (values nil nil nil minprice
				       ith dayindex day tim pri)))))
	      ))));;;closes the if

    (when lowday
      (setq *primitive* t)
      (qbuild startday starttime lowday lowtime
	      stopday stoptime 'down lowprice
	      (if prices-only lowindex) prices-only fsize)
      (setq *latest-dtime*
	    (conv-to-string TEST-DAY TEST-TIME))
      (return (VALUES (conv-to-string TEST-DAY TEST-TIME)
		      price
		      lowindex
		      (conv-to-string lowday lowtime)
		      lowprice)))
;;;reset the point to one later if last point unsucessful
;;;THEN CHECK THE *N-FILT* POINTS AGAIN
;    (format t "TEST_DAY=~A price=~A COUNTER=~A priorday=~A priorprice=~A~%"
;	    test-day price counter priorday priorprice) (incf counter)
    (multiple-value-setq (test-day test-time price)
      (cond (prices-only
	      (setq test-index (1+ test-index))
	      (values (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 1))
		      (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 2))
				(get-tpv-val test-index fsize)))
;	      (values-list (cdr (get-TPV test-index))))
	    (t (incf test-index)
	       (next-data-point test-day test-time price))))

;;; check if actually end of data
;;;if so then build incomplete q vector and exit loop
    (when (or (and (stringp test-day) (not test-time))
	      (not price)
	      (not test-day))
      (qbuild startday starttime nil nil stopday stoptime 'down
	      minprice PRICES-ONLY prices-only fsize)
;;;find the time and/or price change it would take to make a new primitive
;;;find the highest point over the last *n-filt* -1 days
;;;and find the number of price points since the lowest price
;;;after the starttime 
     (setq highest-price
	   (if prices-only (get-tpv-val (1- test-index) fsize))
	   )
     (if prices-only
	 (dotimes (ith (min (1- test-index)(1- *n-filt*)))
;	   (if (> (1+ ith)(1- test-index))(return))
	   (if (< highest-price
		  (get-tpv-val (- (1- test-index) (1+ ith)) fsize))
	       (setq highest-price
		     (get-tpv-val (- (1- test-index) (1+ ith)) fsize)))))
     (return (values nil nil minprice nil nil
		      highest-price
		      (- *n-filt* (- (1- test-index) minindex)))))
      
;;; check if the new point is later than the stopday and stoptime
    (when (or (and stopday stoptime
	       (or (> test-day stopday)
		   (and (= test-day stopday)
			(cond ((integerp test-time)
			       (> (time-units-into-day test-day test-time)
				  (time-units-into-day stopday stoptime)))
;			       (> test-time stoptime))
			      ((eql test-time 'P)
			       (eql stoptime 'A))))))
	      (and stopday (or (not stoptime)(eql stoptime 'C))
			       (> test-day stopday)))
      (qbuild startday starttime nil nil stopday stoptime 'down
	      minprice PRICES-ONLY prices-only fsize)
;;;find the time and/or price change it would take to make a new primitive
;;;find the highest point over the last *n-filt* -1 days
;;;and find the number of price points since the lowest price
;;;after the starttime 
     (setq highest-price
	   (if prices-only (get-tpv-val (1- test-index) fsize)))
     (if prices-only
	 (dotimes (ith (min (1- *n-filt*)(1- test-index)))
	   (if (< highest-price
		  (get-tpv-val (- (1- test-index) (1+ ith)) fsize))
	       (setq highest-price
		     (get-tpv-val (- (1- test-index) (1+ ith)) fsize)))))
      (return (values nil nil minprice nil nil
		      highest-price
		      (- *n-filt* (- (1- test-index) minindex)))))

    );closes the loop over test days
  );;closes the test-for-low

(defun test-for-high (STARTDAY STARTTIME startprice stopday
			       stoptime prices-only fsize
			      &aux priorindex priorday priortime priorprice
			      price DAY TIM
			      dayindex maxindex test-index highindex
			      maxprice maxday maxtime test-day test-time
			      (highday NIL) hightime highprice pri nofail
			      (age 0) findex fday ftim fpri lowest-price)
;;;set the initial value of testday and test-time (next data point from start)
  (multiple-value-setq (test-day test-time price)
    (cond (prices-only
	    (setq test-index (1+ prices-only))
	    (values (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 1))
		    (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 2))
				(get-tpv-val test-index fsize)))
	  (t (setq test-index 1)
	     (next-data-point STARTDAY STARTTIME startprice))))

;;;initialize the max price day and time as the first point after start
  (setq maxprice price maxday test-day maxtime test-time maxindex test-index)
  (loop 
;    (format t "~%TEST-DAY= ~A TEST-TIME= ~A test-index= ~A price= ~A" 
;	    test-day test-time test-index price)
;;;test-day and time defines the test point 
;;;check if the price goes below the start
;;;or if *n-filt* points have passed since the high occurred
    (if (or (< price startprice)
	    (and (< price maxprice)
		 (>= (- test-index maxindex) *n-filt*)))
	(setq highindex maxindex highday maxday hightime maxtime
	      highprice maxprice)
    ;;;compare a point (price) against the prior *n-filt* points to see
    ;;;if its lowerer than all of them
      (progn
	(block TL
 	;;;this part finds a fail day and its age 
	;;;quick test if will fail must start full checking in successful
;;;nofail is T if successful

	(when (and age (not (zerop age)) (plusp (- *n-filt* age))
		   (minusp (- test-index findex *n-filt*)))
	  (setq dayindex findex day fday tim ftim pri fpri)
	  (multiple-value-setq (nofail hightime highprice age findex fday
				       ftim fpri)
	    (dotimes (ith (- *n-filt* age) (values t nil maxprice))
	      (multiple-value-setq (priorday priortime priorprice)
		(cond (prices-only
			(setq priorindex (1- dayindex))
			(values (and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 1))
				(and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 2))
				(get-tpv-val priorindex fsize)))
		(t (setq priorindex (1- dayindex))
		   (prior-data-point day tim pri))))
	      (if (< priorday startday)
		  (return (values t nil maxprice)))
	      (cond ((< price priorprice)
		     (if (> priorprice maxprice)
			 (setq maxprice priorprice maxday priorday
			       maxtime priortime maxindex priorindex))
		     (setq dayindex priorindex day priorday tim priortime
			   pri priorprice))
		    (t (if (< maxprice price)
			   (setq maxprice price maxday test-day
				 maxtime test-time maxindex test-index))
		       (return (values nil nil maxprice
				       (+ ith age 1) dayindex day tim pri))))))
	  (ifn nofail (return-from TL))
	  ));closes the block
;;;this is the full test over *n-filt* values
	(when (or nofail (zerop age)
		  (not (minusp (- test-index findex *n-filt*))))
	  (setq dayindex test-index day test-day tim test-time pri price)
	  
	  (multiple-value-setq (highindex highday hightime highprice
					  age findex fday ftim fpri)
	    (dotimes (ith *n-filt* (values maxindex maxday maxtime maxprice))
	      (multiple-value-setq (priorday priortime priorprice)
		(cond (prices-only
			(setq priorindex (1- dayindex))
			(values (and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 1))
				(and (vectorp (get-tpv priorindex))(svref (get-TPV priorindex) 2))
				(get-tpv-val priorindex fsize)))
		      (t (setq priorindex (1- dayindex))
			 (prior-data-point day tim pri))))
	      (cond ((< price priorprice)
		     (if (> priorprice maxprice) 
			 (setq maxprice priorprice maxday priorday
			       maxtime priortime maxindex priorindex))
		     (setq dayindex priorindex day priorday tim priortime
			   pri priorprice))
		    (t (if (< maxprice price)
			   (setq maxprice price maxday test-day
				 maxtime test-time maxindex test-index))
		       (return (values nil nil nil maxprice
				       ith dayindex day tim pri))))))
	  )));closes the if
				     

    (when highday
      (setq *primitive* t)
      (qbuild startday starttime highday hightime
	      stopday stoptime 'up highprice
	      (if prices-only highindex) prices-only fsize)
      (setq *latest-dtime*
	    (conv-to-string TEST-DAY TEST-TIME))
      (return (values (conv-to-string test-day test-time)
		      price
		      highindex
		      (conv-to-string highday  hightime)
		      highprice)))
;;;reset the point to one later if last point unsucessful

    (multiple-value-setq (test-day test-time price)
      (cond (prices-only
	      (setq test-index (1+ test-index))
	      (values (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 1))
		      (and (vectorp (get-tpv test-index))(svref (get-TPV test-index) 2))
				(get-tpv-val test-index fsize)))

	    (t (incf test-index) (next-data-point test-day test-time price))))

;;;check if end of data,if so, build incomplete q vector and exit loop
    (when (or (and (stringp test-day) (not test-time))
	      (not price)
	      (not test-day))
      (qbuild startday starttime nil nil stopday stoptime 'UP
	       maxprice  PRICES-ONLY prices-only fsize)

;;;find the time and/or price change it would take to make a new primitive
;;;find the lowest point over the last *n-filt* -1 days
;;;and find the number of price points since the highest price
;;;after the starttime 
     (setq lowest-price
	   (if prices-only (get-tpv-val (1- test-index) fsize)))
     (if prices-only
	 (dotimes (ith (min (1- test-index) (1- *n-filt*)))
	   (if (> lowest-price
		  (get-tpv-val (- (1- test-index) (1+ ith)) fsize))
	       (setq lowest-price
		     (get-tpv-val (- (1- test-index) (1+ ith)) fsize)))))
      (return (values nil nil maxprice nil nil
		      lowest-price (- *n-filt*
				      (sub (1- test-index) maxindex)))))
;;;but first check if the new point is later than the stopday and stoptime
    (when (or (and stopday stoptime
		(or (> test-day stopday)
		    (and (= test-day stopday)
			 (cond ((integerp test-time)
				(> (time-units-into-day test-day test-time)
				   (time-units-into-day stopday stoptime)))
;				    (> test-time stoptime))
			       ((eql test-time 'P)
				(eql stoptime 'A))))))
	      (and stopday (or (not stoptime)(eql stoptime 'C))
		   (> test-day stopday)))
      (qbuild startday starttime nil nil stopday stoptime 'up
	      maxprice PRICES-ONLY prices-only fsize)

;;;find the time and/or price change it would take to make a new primitive
;;;find the lowest point over the last *n-filt* -1 days
;;;and find the number of price points since the highest price
;;;after the starttime 
     (setq lowest-price
	   (if prices-only (get-tpv-val (1- test-index) fsize)))
     (if prices-only
	 (dotimes (ith (min (1- test-index)(1- *n-filt*)))
	   (if (> lowest-price
		  (get-tpv-val (- (1- test-index) (1+ ith)) fsize))
	   (setq lowest-price
		 (get-tpv-val (- (1- test-index) (1+ ith)) fsize)))))
      (return (values nil nil maxprice nil nil
		      lowest-price (- *n-filt*
				      (sub (1- test-index) maxindex)))))
    );closes the loop over points
  )
;;;moved to w8z-utl1
;(defun time-units-into-day (day tim)
 ; (1+ (position tim (getd day 'ptime))))

(defun next-data-point (startday starttime startprice &aux next-times nexttime
				 ndate tdate time-list price-list
				 next-prices nextprice ignore next)

  (declare (ignore ignore))
  (block nil
    (multiple-value-setq (tdate ignore ndate time-list price-list)
      (data-access startday))
    (ifn (and (car time-list)
	      (not (member *time-interval*
			   '(1440 720 daily-high-low ohlc))))
	 (case (length price-list)
	   (1 (if (listp (getd ndate 'pdata))
		  (return (values ndate 'C (car (last (getd ndate 'pdata)))))
		(return (values ndate nil nil))))
	   (2 (case starttime
		(A (return (values startday 'P (cadr price-list))))
		(P (multiple-value-setq
		       (next ignore ignore time-list price-list)
		     (data-access ndate))
		    (if (listp price-list)
			(return (values ndate (car time-list)
					(car price-list)))
		      (return (values next nil nil))))));next is a string
	      ))
    (setq next-times (member starttime time-list)
	  next-prices (nthcdr (- (length time-list) (length next-times))
			      price-list)) 
    (if next-times (setq nexttime (cadr next-times)
			 nextprice (cadr next-prices))
      (setq nexttime (if starttime (car time-list) (car (last time-list)))
	    nextprice (if starttime (car price-list))))
    (unless (and nexttime nextprice) 
      (multiple-value-setq (tdate ignore ndate time-list price-list)
	(data-access ndate))
	 (return (values tdate (car time-list)
			 (car price-list))))
       (values tdate nexttime nextprice)))


(defun prior-data-point (startday starttime &optional startprice 
				  &aux prior-times priortime tdate ydate
				  prior-prices priorprice 
				  time-list price-list t-l ignore)

  (declare (ignore ignore))
  (block nil    
    (multiple-value-setq (tdate ydate ignore time-list price-list)
	(data-access startday))
    (ifn (and (car time-list)(not (member *time-interval*
						'(1440 720 daily-high-low ohlc))))
	 (case (length price-list)
	   (1 (return (values ydate 'C (car (last (getd ydate 'pdata))))))
	   (2 (case starttime
		(A (multiple-value-setq (ignore ignore ignore t-l)
		     (data-access ydate))
		  (return (values ydate (second t-l)
				  (getd ydate 'pdata (second t-l)))))
		(P (return (values startday 'A (car price-list))))))
	      ))
    (setq prior-times (ldiff time-list (member starttime time-list))
	  prior-prices (butlast price-list
				(- (length time-list)
				   (length prior-times))))
    (if prior-times (setq priortime (car (last prior-times))
			  priorprice (car (last prior-prices)))
      (setq priortime nil))
    (unless priortime 
      (multiple-value-setq (tdate ydate ignore time-list price-list)
	(data-access ydate))
      (return (values tdate (car (last time-list))
		      (car (last price-list)))))
    (values tdate priortime priorprice)))

;;;BUILDS A Q-VECTOR ACTUALLY PLACES THE DATA VALUES IN THE Q VECTOR FOR WAVE 0
;;;THIS FUNCTION IS FOR PRIMITIVES (COMPLETE OR INCOMPLETE)
;;;'HP AND 'EP ARE ASSUMED THE SAME FOR COMPLETE 'UP WAVES AND VICE-VERSA
;;;ext-price is the 'lp for 'up waves 'hp for 'down waves
;;;this assumes that wave 0 exists
(defun qbuild (startday starttime endday endtime
			&optional (stopday nil) (stoptime nil)
			(dir nil) (ext-price nil) endindex PRICES-ONLY fsize
			;(primw *standard-output*)
			&AUX TDATE YDATE NDATE TIME-LIST PRICE-LIST ADHOC-LIST
			VOL-LIST F-O EXTRA-VOL ignore nextday nexttime
			nextprice)
  (declare (ignore ignore))
  (block QBD
    (if endday (putv 0 'et (conv-to-string ENDDAY ENDTIME))
      (PUTV 0 'ET NIL))
    (putv 0 'dg endindex);this is the index of end-time
    (putv 0 'st (conv-to-string STARTDAY STARTTIME))
    (putv 0 'sp (float (if prices-only 
			   (get-tpv-val prices-only fsize)
			 (getd startday 'pdata starttime))))
    (if endday (putv 0 'ep (float (if prices-only
			       (get-tpv-val endindex fsize)
			       (getd endday 'pdata endtime))))
      (putv 0 'ep nil))
    (or dir endday 
	;(format primw "USER ERROR! QBUILD NEEDS EITHER ENDTIME OR DIRECTION!")
	(return-from QBD nil))
    (if dir (putv 0 'dr dir)
      (if (> (getv 0 'sp) (getv 0 'ep))
	  (putv 0 'dr 'down) (putv 0 'dr 'up)))
    
    (cond ((eql (getv 0 'dr) 'up)
	   (putv 0 'hp (float ext-price))
	   (putv 0 'lp (getv 0 'sp)))
	  ((eql dir 'down)
	   (putv 0 'lp (float ext-price))
	   (putv 0 'hp (getv 0 'sp))))
;    (putv 0 'FT (fibo-term (max-lgth% 0)))
    ;;;the next six data values 'tl 'va 'vp 'vd 'as 'ap
    ;;;also 'hp and 'lp if ext-price is nil
    ;;;must be obtained by looping thru the data start-time to end-time
    ;;;or stop-time
    (if stopday
	(setq *latest-dtime*
	    (conv-to-string STOPDAY STOPTIME)))
;;;if no endtime or endday and no stoptime or stopday
;;;then the end of data is the stopday and stoptime
    (unless (or prices-only endday stopday)
      (setq nextday startday nexttime starttime)
      (loop
	(setq stopday nextday stoptime nexttime)
	(multiple-value-setq (nextday nexttime nextprice)
	  (next-data-point nextday nexttime nextprice))
	(if (or (not nextday) (stringp nextday))(return))
	(if (stringp (data-access nextday)) (return)))
      (setq *latest-dtime*
	    (conv-to-string STOPDAY STOPTIME)))
	
    (ifn (or endtime endday) (setq endday stopday endtime stoptime))
    
    ;;;you may not have either endtime or stoptime
    
    ;;;first let's do the startday from the starttime to the end of the day
    (IFN PRICES-ONLY
	 (let ((volume-total 0) first-index last-index (a-dadu 0)
				(admax 0) dmy close time-to-close)
	   (multiple-value-setq (startday ydate ndate time-list price-list
					  adhoc-list
					  vol-list f-o IGNORE extra-vol)
	     (data-access startday))
	   ;;;stop at endtime if same day
	   (putv 0 'tl (float
			 (if (and (car time-list)
				  (not (member *time-interval*
				         '(1440 720 daily-high-low))))
				  ;(numberp starttime))
			    (if (minusp (setq dmy (sub-times-in-hours starttime
						 (or (and (eql startday endday)
							  endtime)
						     (car (last time-list))))))
				(+ 24 dmy) dmy)
			   (case starttime
			     (A 12)
			     (P 0)
			     (C 0)
                             (otherwise (if (eql starttime (car time-list))
                                            12 0))))))
	   
	   (when vol-list
	     (if (and starttime (car time-list))
		 (putv 0 'va
		       (float (ifn (zerop (getv 0 'tl))
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
			       (getv 0 'tl))
			 0)))
	       (putv 0 'va
		     (case starttime
		       (A (/ (car vol-list) (getv 0 'tl)))
		       (P 0)
		       (otherwise 0))))
	     (setq volume-total (* (getv 0 'va) (getv 0 'tl)))
	   
	     (ifn (zerop (getv 0 'tl))
		  (if (and starttime (car time-list))
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
			   ((null vlist) (putv 0 'vp (float volp))
					 (putv 0 'vd (float vold)))
			(cond ((> price price-1)
			       (if (> vol volp)(setq volp vol)))
			      ((< price price-1)
			       (if (> vol vold)(setq vold vol)))))
		    (case starttime
		      (A (cond ((< (car price-list) (cadr price-list))
				 (putv 0 'vp (float (cadr vol-list)))
				 (putv 0 'vd 0))
				((> (car price-list) (cadr price-list))
				 (putv 0 'vd (float (cadr vol-list)))
				 (putv 0 'vp 0))))
		      (P (putv 0 'vp 0) (putv 0 'vd 0))
		      (otherwise (putv 0 'vp 0) (putv 0 'vd 0))))
	       (progn (putv 0 'vp 0) (putv 0 'vd 0))
	       );;closes the ifn
	     );;closes the when
	   (putv 0 'ap 0)(putv 0 'as 0)
	   (when (or (and (cdr (assoc 'advances adhoc-list))
			  (neql starttime (car (last time-list)))
		      (neql startday endday))
		     (and (cdr (assoc 'advances adhoc-list)) (eql endtime
						(car (last time-list)))
			  (eql startday endday)))
	     (setq admax (/ (cdr (assoc 'advances adhoc-list))
			    (cdr (assoc 'declines adhoc-list))))
	     (setq a-dadu (/ (- (cdr (assoc 'advances adhoc-list))
				(cdr (assoc 'declines adhoc-list)))
			     (apply #'+
				(list (cdr (assoc 'advances adhoc-list))
				      (cdr (assoc 'declines adhoc-list))
				      (cdr (assoc 'unchanged adhoc-list))))))
	     (putv 0 'as  (ifn (zerop (getv 0 'tl))
			       (/ (* 100 A-DADU) (getv 0 'tl)) 0))
	     (putv 0 'ap  (float admax)))
	   
 ;;;read another day of data
 ;;;next let's do the full days
	   (setq tdate startday)
	   (loop
	     ;;;check if next day is a full day
	     (cond ((or (not endday) (> endday ndate))
		    (setq close (getd tdate 'close-time))
		    (if close
			(setq time-to-close
			      (if (minusp
				    (setq dmy
					  (or (sub-times-in-hours
						(car (last time-list))
						close) 0)))
				  (+ dmy 24) dmy))
		      (setq time-to-close 0))
		    (multiple-value-setq (tdate ydate ndate time-list
						price-list adhoc-list
						vol-list f-o IGNORE extra-vol)
		      (data-access ndate))
		    (if (stringp tdate) (return))

		    (putv 0 'tl
			  (float (if (and (car time-list)
				       (not (member *time-interval*
				            '(1440 720 daily-high-low))))
				;	  (numberp (car time-list)))
			      (+ (getv 0 'tl)
				 time-to-close
				 (if (minusp
				       (setq dmy (or (sub-times-in-hours
						   (car time-list)
						   (car (last time-list))) 0)))
				     (+ 24 dmy) dmy) f-o)
			    (+ (getv 0 'tl) 24))))
		    (when vol-list
		      (if (and starttime (car time-list))
			  (progn
			    (putv 0 'va (float (/ (+ volume-total
					      (apply #'+ vol-list))
				     (getv 0 'tl))))
			    (setq volume-total (* (getv 0 'va) (getv 0 'tl)))
			    (do* ((vlist vol-list (cdr vlist))
				  (vol (+ (car vlist) extra-vol) (car vlist))
				  (plist (cons (car (last (getd ydate 'pdata)))
					       price-list)
					 (cdr plist))
				  (price-1 (car plist) (car plist))
				  (price (second plist) (second plist))
				  (volp (or (getv 0 'vp) 0))
				  (vold (or (getv 0 'vd) 0)))
				 ((null vlist) (putv 0 'vp (float volp))
					       (putv 0 'vd (float vold)))
			      (cond ((> price price-1)
				     (if (> vol volp)(setq volp vol)))
				    ((< price price-1)
				     (if (> vol vold)(setq vold vol))))))
			(case (length vol-list)
			  (1  
			      (putv 0 'va (/ (+ volume-total (car vol-list))
					     (getv 0 'tl)))
			      (setq volume-total (* (getv 0 'va) (getv 0 'tl)))
			      (cond ((> (car price-list)
					(car (getd ydate 'pdata)))
				     (putv 0 'vp
					   (float (max (or (getv 0 'vp) 0)
						       (car vol-list)))))
				    ((< (car price-list)
					(car (getd ydate 'pdata)))
				     (putv 0 'vd
					   (float (max (or (getv 0 'vd) 0)
						       (car vol-list)))))))
			  (2 (putv 0 'va (/ (+ volume-total (car vol-list)
					       (cadr vol-list))
					     (getv 0 'tl)))
			     (setq volume-total (* (getv 0 'va) (getv 0 'tl)))
			     (cond ((> (car price-list)
					(car (getd ydate 'pdata)))
				     (putv 0 'vp
					   (float (max (or (getv 0 'vp) 0)
						       (car vol-list)))))
				    ((< (car price-list)
					(car (getd ydate 'pdata)))
				     (putv 0 'vd
					   (float (max (or (getv 0 'vd) 0)
						       (car vol-list))))))
			     (cond ((> (cadr price-list)
					(car price-list))
				     (putv 0 'vp
					   (float (max (or (getv 0 'vp) 0)
						       (cadr vol-list)))))
				    ((< (cadr price-list)
					(car price-list))
				     (putv 0 'vd
					   (float (max (or (getv 0 'vd) 0)
						     (cadr vol-list))))))))))
			     
		    (when (cdr (assoc 'advances adhoc-list))
		      (setq a-dadu
			    (+ a-dadu
			       (/ (- (cdr (assoc 'advances adhoc-list))
				     (cdr (assoc 'declines adhoc-list)))
				  (apply #'+
					 (list (cdr (assoc 'advances
							   adhoc-list))
					       (cdr (assoc 'declines
							   adhoc-list))
					       (cdr (assoc 'unchanged
							   adhoc-list)))))))
		      (if (> (/ (cdr (assoc 'advances adhoc-list))
				(cdr (assoc 'declines adhoc-list))) admax)
			  (setq admax
				(/ (cdr (assoc 'advances adhoc-list))
				   (cdr (assoc 'declines adhoc-list)))))))
		   (t (return))))
	   
 ;;;and last let's do the beginning of the end or stop day to the end or
 ;;; stop time
	   (when (and ndate (or (not endday) (= endday ndate)))
	     	     (setq close (getd tdate 'close-time))
;;;this calculates the time length from the last price on the previous
;;;day to the close on that day. usually zero except for daily high low	     
	     (if close
		 (setq time-to-close
		       (if (minusp (setq dmy
					 (or (sub-times-in-hours
					   (car (last time-list)) close) 0)))
			   (+ dmy 24) dmy))
	       (setq time-to-close 0))

	     (multiple-value-setq (tdate ydate ndate time-list
					 price-list adhoc-list
					 vol-list f-o IGNORE extra-vol)
	       (data-access ndate))
	     (putv 0 'tl
		   (float (if (and (car time-list)
				   (not (member *time-interval*
				   	'(1440 720 daily-high-low))))
				   ;(numberp (car time-list)))
		       (+ (getv 0 'tl) time-to-close
			  (if (minusp (setq dmy (or (sub-times-in-hours
			      (car time-list)
			      (or  endtime (car (last time-list)))) 0)))
			      (+ 24 dmy) dmy)
			    f-o)
		     (case endtime
		       (A (+ (getv 0 'tl) 12))
		       (P (+ (getv 0 'tl) 24))
		       (otherwise  (+ (getv 0 'tl) 24))))))
	     (when vol-list 
	       (if (and starttime (car time-list))
	       (progn (putv 0 'va
		     (float (/ (+ volume-total 
			   (apply #'+
				  (subseq vol-list
					  0
					  (1+ (position        ;1+ stop index
						(or endtime
						    (car (last time-list)))
						time-list)))))
			(getv 0 'tl))))
		      (do* ((vlist (subseq vol-list 0
					   (1+ (position endtime time-list)))
				   (cdr vlist))
			    (vol (+ (car vlist) extra-vol) (car vlist))
			    (plist (cons (car (last (getd ydate 'pdata)))
					 (subseq price-list 0
						 (1+ (position endtime
							       time-list))))
				   (cdr plist))
			    (price-1 (car plist) (car plist))
			    (price (second plist) (second plist))
			    (volp (getv 0 'vp))
			    (vold (getv 0 'vd)))
			   ((null vlist) (putv 0 'vp (float volp))
					 (putv 0 'vd (float vold)))
			(cond ((> price price-1)
			       (if (> vol volp)(setq volp vol)))
			      ((< price price-1)
			       (if (> vol vold)(setq vold vol)))))
	       );closes the progn
		 (case endtime
		   (A (putv 0 'va (/ (+ volume-total (car vol-list))
				      (getv 0 'tl)))
		       (cond ((> (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 'vp (float (max (or (getv 0 'vp) 0)
					       (car vol-list)))))
			     ((< (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 'vd (float (max (or (getv 0 'vd) 0)
					       (car vol-list)))))))
		   (P (putv 0 'va (/ (+ volume-total (car vol-list)
					 (cadr vol-list)) (getv 0 'tl)))
		       (cond ((> (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 'vp (float (max (or (getv 0 'vp) 0)
					       (car vol-list)))))
			     ((< (car price-list)
			      (car (last (getd ydate 'pdata))))
			      (putv 0 'vd (float (max (or (getv 0 'vd) 0)
					       (car vol-list))))))
		       (cond ((> (cadr price-list)
				 (car price-list))
			      (putv 0 'vp (float (max (or (getv 0 'vp) 0)
					       (cadr vol-list)))))
			     ((< (cadr price-list)
				 (car price-list))
			      (putv 0 'vd (float (max (or (getv 0 'vd) 0)
					       (cadr vol-list)))))))
		   (otherwise (putv 0 'va (/ (+ volume-total
						(car vol-list))
					     (getv 0 'tl)))
			      (cond ((> (car price-list)
					(car (last (getd ydate 'pdata))))
				     (putv 0 'vp
					   (float (max (or (getv 0 'vp) 0)
						       (car vol-list)))))
				    ((< (car price-list)
					(car (last (getd ydate 'pdata))))
				     (putv 0 'vd
					   (float (max (or (getv 0 'vd) 0)
						       (car vol-list))))))))
		 );closes the if time-list
	       );closes the when vol-list
	     (when (cdr (assoc 'advances adhoc-list))
	       (if (and (eql endtime (car (last time-list)))
			(> (/ (cdr (assoc 'advances adhoc-list))
			      (cdr (assoc 'declines adhoc-list))) admax))
		   (setq admax (/ (cdr (assoc 'advances adhoc-list))
				  (cdr (assoc 'declines adhoc-list)))
			 a-dadu
			 (+ a-dadu
			    (/ (- (cdr (assoc 'advances adhoc-list))
				  (cdr (assoc 'declines adhoc-list)))
			       (apply #'+
				      (list
					(cdr (assoc 'advances adhoc-list))
					(cdr (assoc 'declines adhoc-list))
					(cdr (assoc 'unchanged adhoc-list))))))
			 ))
	       (putv 0 'ap (float admax))
	       (putv 0 'as (/ (* 100 a-dadu) (getv 0 'tl)))
	       )
	     );closes the last day test
	   );closes the let
      )      ))

;for testing
;(qbuild 880826 1130 880826 1200 nil nil 'up 2017.78)
;(qbuild 880818 1600 880819 1100 nil nil 'up 2042.14)
;(data-access 880826)
; (showd 0)
;;not needed 
;(defun find-index (day tim)
 ; (dotimes (ith (length *time-price-vector*))
  ;   (and (eql (second (get-TPV ith)) day)
;	  (eql (third (get-TPV ith)) tim)
;	  (return (first (get-TPV ith))))))


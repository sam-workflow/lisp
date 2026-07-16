;;; -*- Mode: LISP; Package: user; Base: 10. -*-
;;;
;;; THIS PROGRAM RETURNS 10 VALUES:
;;;      the date of the data                    
;;;      the previous market day                 
;;;      the next market date                    
;;;      the list of times requested             
;;;      the requested list of prices            
;;;      the list of scalar (daily data) values  
;;;      the list of VOLUMES for the requested times  
;;;      the time interval between the open and first time
;;;      the time between samples
;;;      second volume sample or 0 for provide-primitive
;;;the macro scalar-data is defined in men0.
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
(defparameter *dio-parms*
	'((ydate . 0 ) (ndate . 1) (index . 2) (index-sample-period . 3)
	  (open-time . 4) (close-time . 5) (first-time . 6) (aux-data . 7)
	  (aux-data-sample-period . 8) (nsamp . 9) (ptime . 10) (pdata . 11)
	  (stime . 12) (sdata . 13) (time-base . 14) (comment . 15)))
(defparameter *cat-list-ceiling* 2000)
(defun data-access (tdate)
  (let* ((svals (readt2 tdate))
	 (p-period (get-val 'INDEX-SAMPLE-PERIOD svals)))
    (cond ((and (not svals) (neql *time-interval* 'MONTHLY-HIGH-LOW))
	   "DATA NOT AVAILABLE")
;;;   *time-interval* nil requests data "as is"

	  ((not *time-interval*) (stored-data-return svals))
;;;
	  ((eql *time-interval* 'DAILY-HIGH-LOW) (daily-high-low svals))
	  
	  ((eql *time-interval* 'OHLC) (daily-open-high-low-close svals))
	  
	  ((eql *time-interval* 'MONTHLY-HIGH-LOW) (monthly-high-low tdate))
	  ((or (not (integerp *time-interval*))
	       (< *time-interval* 1)
	       (> *time-interval* 1440)) "BAD TIME INTERVAL")
;;;

	  ((and p-period (< *time-interval* p-period))
	   "UNABLE TO SUPPORT TIME INTERVAL REQUESTED")
;;;
	  ((= *time-interval* 1440) (daily-data svals))
	  ((and (= *time-interval* 720)
                (eql (cdr (assoc 'nsamp (cdr svals))) 1)) (daily-open-close svals))
	
	  ((eql (cdr (assoc 'nsamp (cdr svals))) 1)
	   "UNABLE TO SUPPORT TIME INTERVAL REQUESTED")
;;;
	  ((and (= *time-interval* 720)
		(eql (cdr (assoc 'nsamp (cdr svals))) 2)) (twice-daily svals))
        
;;;checks if the requested time interval is the same as the price and volume
;;;	  
	  ((= *time-interval* p-period
	      (or (cdr (assoc 'AUX-DATA-SAMPLE-PERIOD (cdr svals)))
		  *time-interval*)) (STORED-DATA-return svals))
;;;volume has different interval than time interval requested
	  ((= *time-interval* p-period) (TIMED-SDATA svals))
;;;p-period should be true by now and requested time interval should be
;;;a multiple of p-period (the price sample period)
	  ((or (not p-period) (/= (mod *time-interval* p-period) 0))
	   "UNABLE TO SUPPORT REQUESTED TIME INTERVAL")
	  ((and (= *time-interval* 720)(> *time-interval* p-period))
	   "UNABLE TO SUPPORT REQUESTED TIME INTERVAL")
;;;standard case where the data is stored at higher freqency than requested
	  ((> *time-interval* p-period)
	   (TIMED-DATA svals))
	  (t "ERROR"))))
	  
(defun stored-data-return (svals &aux dmy)
  (values (car svals) (get-val 'YDATE svals) (get-val 'NDATE svals)
	  (butlast (get-val 'PTIME svals)
		   (length (member nil (get-val 'PDATA svals))))
	  (if (index-32sp)
	      (mapcar #'convert-to-decimal (remove nil (get-val 'pdata svals)))
	    (copy-list (remove nil (get-val 'PDATA svals))))
	  (mapcar #'(lambda (s) (assoc s (cdr svals)))  (scalar-data))
	  (copy-list (remove nil (get-val 'SDATA svals)))
	  (if (minusp (setq dmy
			    (or (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
			      (get-val 'first-time svals)) 0)))
	      (+ 24 dmy) dmy)
	  (ifn (get-val 'nsamp svals)
	       (get-val 'index-sample-period svals)
	    (/ 1440 (get-val 'nsamp svals)))
	  0.0))

(defun daily-high-low (svals)
  (block nil
    (cond ((get-val 'high svals)
	  (let ((valp-list (list (float (get-val 'high svals))
				 (float (get-val 'low svals))))
		(close (car (last (get-val 'pdata svals))))
		(open (get-val 'open svals))
		(prev-close (car
			      (last
				(get-val 'pdata
					 (readt2 (get-val 'ydate svals)))))))
	    (if (and (not (car valp-list)) (not (cadr valp-list)))
		(setq valp-list (list close close)))
	    (if (gtr close (or open prev-close))
		   (setq valp-list (nreverse valp-list)))
	    (values (car svals)
		    (get-val 'YDATE svals)
		    (get-val 'NDATE svals)
		    '(A P)
		    (if (index-32sp)
			(mapcar #'convert-to-decimal valp-list)
		      valp-list)
		    (mapcar #'(lambda (s) (assoc s (cdr svals)))
			    (scalar-data))
		    (if (and (get-val 'aux-data svals)(get-val 'sdata svals))
			(list 0 (apply #'+ (remove nil
						   (get-val 'sdata svals)))))
		    0
		    720
		    0.0)))
	  ((eql (get-val 'nsamp svals) 1)
	   (let* ((close (car (last (get-val 'pdata svals))))
		  (valp-list (list close close)))	    	    
	    (values (car svals)
		    (get-val 'YDATE svals)
		    (get-val 'NDATE svals)
		    '(A P)
		    (if (index-32sp)
			(mapcar #'convert-to-decimal valp-list)
		      valp-list)
		    (mapcar #'(lambda (s) (assoc s (cdr svals)))
			    (scalar-data))
		    (if (and (get-val 'aux-data svals)(get-val 'sdata svals))
			(list 0 (apply #'+ (remove nil
						   (get-val 'sdata svals)))))
		    0
		    720
		    0.0)))
	  ((eql (get-val 'nsamp svals) 2)
	   (let ((valp-list (remove nil (get-val 'PDATA svals)))
		 (pt-list (get-val 'PTIME svals)))
	     (if (and (not (car valp-list)) (not (cadr valp-list)))
		 (return "UNABLE TO SUPPORT TIME INTERVAL REQUESTED"))
	     (values (car svals)
		     (get-val 'YDATE svals)
		     (get-val 'NDATE svals)
		     '(A P)
		     (if (index-32sp)
			 (mapcar #'convert-to-decimal valp-list)
		       valp-list)
		     (mapcar #'(lambda (s) (assoc s (cdr svals)))
			     (scalar-data))
		     (if (and (get-val 'aux-data svals)
			      (get-val 'sdata svals))
			 (list (apply #'+
				      (remove nil
					      (get-val 'sdata svals)))))
		     (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
					 (car pt-list))
		     (ifn (get-val 'nsamp svals)
			  (get-val 'index-sample-period svals)
		       (/ 1440 (get-val 'nsamp svals)))
		     0.0)))
	  (t (let (low low-index low-time high  high-index high-time
		       (valp-list (get-val 'PDATA svals))
		       (pt-list (get-val 'PTIME svals)) dmy)
	       (if ;(and (not (car valp-list)) (not (cadr valp-list)))
		   (some #'not valp-list)
		   (return "UNABLE TO SUPPORT TIME INTERVAL REQUESTED"))
	       (setq low (apply #'min valp-list)
		     high (apply #'max valp-list))
	       (if (neql low high)                         ; 4 new lines here
		   (setq low-index (position low valp-list); 92/03/03
			 high-index (position high valp-list))
		 (setq low-index 0 high-index (1- (length valp-list))))
	       (setq low-time (nth low-index pt-list)
		     high-time (nth high-index pt-list))
	       (if (> high-index low-index)
		   (setq pt-list (list low-time high-time)
			 valp-list (list low high))
		 (setq pt-list (list high-time low-time)
		       valp-list (list high low)))
	       (values (car svals)
		       (get-val 'YDATE svals)
		       (get-val 'NDATE svals)
		       pt-list
		       (if (index-32sp)
			   (mapcar #'convert-to-decimal valp-list)
			 valp-list)
		       (mapcar #'(lambda (s) (assoc s (cdr svals)))
			       (scalar-data))
		       (if (and (get-val 'aux-data svals)
				(get-val 'sdata svals))
			   (list 0 (apply #'+
					  (remove nil
						  (get-val 'sdata svals)))))
		       (if (minusp
			     (setq dmy (or (SUB-TIMES-IN-HOURS
					 (get-val 'open-time svals)
					   (car pt-list)) 0)))
			   (+ dmy 24) dmy)
		       (ifn (get-val 'nsamp svals)
			    (get-val 'index-sample-period svals)
			 (/ 1440 (get-val 'nsamp svals)))
		       0.0))))))


(defun daily-open-high-low-close (svals)
  (block nil
    (cond ((get-val 'high svals)
	  (let* ((close (car (last (get-val 'pdata svals))))
	         (valp-list (list (float (get-val 'high svals))
				 (float (get-val 'low svals))))
		
		(open (get-val 'open svals))
		(prev-close (car
			      (last
				(get-val 'pdata
					 (readt2 (get-val 'ydate svals)))))))
	    (if (and (not (car valp-list)) (not (cadr valp-list)))
		(setq valp-list (list close close)))
	    (if (gtr close (or open prev-close))
		   (setq valp-list (nreverse valp-list)))
	    
	    (setq valp-list (append (list open) valp-list (list close)))
	    (values (car svals)
		    (get-val 'YDATE svals)
		    (get-val 'NDATE svals)
		    '(O A P C)
		    (if (index-32sp)
			(mapcar #'convert-to-decimal valp-list)
		      valp-list)
		    nil
			   
		    (if (and (get-val 'aux-data svals)(get-val 'sdata svals))
			(list 0 (apply #'+ (remove nil
						   (get-val 'sdata svals)))))
		    0
		    360
		    0.0)))
	

	  (t  (return "UNABLE TO SUPPORT TIME INTERVAL REQUESTED"))
     )))

;;;this function is called only if *time-interval* is set to 'monthly-high-low
(defun monthly-high-low (tdate)
  (block monthly

;;;find first market day of month
  (let* ((year (getnumyear-long tdate))
	 (month (getnummonth tdate))
	 (nmonth (next-month (add (mul 100 year) month)))
	 svals1 first-market-day volumes next-day times
	 lowest-date lowest-time highest-date highest-time
	 thighest-time highest-price thighest-price ignore lowest-price
	 tlowest-price tlowest-time prices ldate ndate ydate
	 (local-time-int *time-interval*))
    (declare (ignore ignore))
    (unwind-protect
	(progn
    (setq *time-interval* 'daily-high-low) 
    (setq first-market-day (car (month-days (truncate tdate 100))))
    (setq svals1 (readt2 first-market-day))
    	  
    (ifn first-market-day (return-from monthly
			    "DATA NOT AVAILABLE"))
    (if (assoc 'aux-data (cdr svals1))
	(setq volumes (apply #'add (cdr (assoc 'sdata (cdr svals1)))))
      (setq volumes nil))
    (setq ydate (cdr (assoc 'ydate (cdr svals1))))
;;;need to find the highest and lowest price for the first day
    (multiple-value-setq (ignore ignore next-day times prices)
      (data-access first-market-day))
    (setq highest-price (max* prices) lowest-price (min* prices))
    (setq highest-date first-market-day
	  highest-time (nth (position highest-price prices) times)
	  lowest-date first-market-day
	  lowest-time (nth (position lowest-price prices) times))

    (setq ldate first-market-day)          
;;;;;go through the days of the month
    (loop
      (multiple-value-setq (tdate ignore next-day times prices ignore svals1)
	(data-access next-day))
      (ifn (and prices (< (add (mul (getnumyear tdate) 100)
			       (getnummonth tdate))
			  nmonth)) (return))
      (setq ldate tdate
	    volumes (add volumes (apply #'add svals1))
	    thighest-price (max* prices) tlowest-price (min* prices)
	    thighest-time (nth (position thighest-price prices) times)
	    tlowest-time (nth (position tlowest-price prices) times))
      (if (> thighest-price highest-price)
	  (setq highest-date tdate highest-price thighest-price
		highest-time thighest-time))
      (if (< tlowest-price lowest-price)
	  (setq lowest-date tdate lowest-price tlowest-price lowest-time
		tlowest-time)))
;;;;
    (setq times
	  (sort (list (conv-to-string highest-date highest-time)
		      (conv-to-string lowest-date lowest-time)) #'string<)
	  prices (if (string= (car times)
			     (conv-to-string highest-date highest-time))
		     (list highest-price lowest-price)
		   (list lowest-price highest-price))
	  ndate (getd ldate 'ndate)
	  *time-interval* 'monthly-high-low)
    (values  first-market-day ydate ndate times prices nil (list 0 volumes)
	     nil 1 0)
    )
      (setq *time-interval* local-time-int))
    )))


(defun daily-data (svals &aux dmy)
  (block nil
    (ifn (car (last (get-val 'pdata svals)))
	 (return "UNABLE TO SUPPORT TIME INTERVAL REQUESTED"))
    (values (car svals) (get-val 'YDATE svals) (get-val 'NDATE svals)
	    ;(case (get-val 'nsamp svals)
	     ; (1 (list 'C))
	     ; (2 (list 'C))
	     ; (otherwise (get-val 'close-time svals)))
	    (list 'C)
	    (if (index-32sp)
		(mapcar #'convert-to-decimal
			(last (get-val 'pdata svals)))
	      (last (get-val 'pdata svals)))
	    (mapcar #'(lambda (s) (assoc s (cdr svals))) (scalar-data))
	    (if (and (get-val 'aux-data svals)(get-val 'sdata svals))
		(list (apply #'+ (remove nil (get-val 'sdata svals)))))
	    (if (minusp (setq dmy (or (SUB-TIMES-IN-HOURS
				    (get-val 'open-time svals)
				    (get-val 'close-time svals)) 0)))
		(+ dmy 24) dmy)
	    (ifn (get-val 'nsamp svals)
		 (get-val 'index-sample-period svals)
	      (/ 1440 (get-val 'nsamp svals)))
	    0.0)))

;;;nsamp is 2
(defun twice-daily (svals)
  (values (car svals) (get-val 'YDATE svals) (get-val 'NDATE svals)
	  (butlast '(A P)
		   (length (member nil (get-val 'PDATA svals))))
	  (if (index-32sp)
	      (mapcar #'convert-to-decimal (remove nil (get-val 'PDATA svals)))
	    (copy-list (remove nil (get-val 'PDATA svals))))
	  (mapcar #'(lambda (s) (assoc s (cdr svals))) (scalar-data))
	  (if (and (get-val 'aux-data svals)(get-val 'SDATA svals))
	      (list 0 (apply #'+ (remove nil (get-val 'sdata svals)))))
	  (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
			      (get-val 'first-time svals))
	  720 0.0))

;;;nsamp is 1
(defun daily-open-close (svals)
   (values (car svals) (get-val 'ydate svals) (get-val 'ndate svals)
           '(O C)
           (cons (get-val 'open svals)(last (get-val 'pdata svals)))
           nil
           nil
           0
           720
           0))
;;;volume is sampled more frequently than price
;;;need to match volumes to prices
(defun timed-sdata (svals)
  (let (vol-incf (volumes (get-val 'SDATA svals))
	 (pdata-list (get-val 'pdata svals))
	 (pt-list (get-val 'ptime svals)) (st-list (get-val 'stime svals))
		      matched-vol first-period-index)
    (setq pt-list (butlast pt-list (length (member nil pdata-list)))
	  st-list (butlast st-list (length (member nil volumes))))

    (loop  (setq vol-incf 0)
	  (dotimes (ith (1+ (position (pop pt-list) st-list)))
	    (setq vol-incf (+ (pop volumes) vol-incf))
		  (pop st-list))
	  (push vol-incf matched-vol) 
	 (ifn pt-list (return)))

    (setq vol-incf 0 volumes (and (get-val 'aux-data svals)
				  (get-val 'SDATA svals))
	  st-list (get-val 'STIME svals))
;;;computes vol-incf the correct volume for first price interval
    (setq first-period-index
	  (1+ (position (add-to-time (get-val 'open-time svals)
				     (get-val 'index-sample-period svals))
			st-list)))
    (if (>= (length (remove nil volumes))  first-period-index)
	(dotimes (ith first-period-index)
	    (setq vol-incf (+ (pop volumes) vol-incf))
		  (pop st-list))
      (setq vol-incf (* (apply #'+ (remove nil volumes))
			(/ (get-val 'index-sample-period svals)
			   (get-val 'aux-data-sample-period svals)))))
	  
  (values (car svals) (get-val 'YDATE svals) (get-val 'NDATE svals)
	  (butlast (get-val 'PTIME svals)
		   (length (member nil pdata-list)))
	  (if (index-32sp)
	      (mapcar #'convert-to-decimal (remove nil pdata-list))
	    (copy-list (remove nil pdata-list)))
	  (mapcar #'(lambda (s) (assoc s (cdr svals))) (scalar-data))
	  (reverse matched-vol)
	  (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
			      (get-val 'first-time svals))
	  (ifn (get-val 'nsamp svals)
		       (get-val 'index-sample-period svals)
		    (/ 1440 (get-val 'nsamp svals)))
	   (- vol-incf (car (last matched-vol))))))
#| Changed 12/26/92
;;;;if both prices and volume are higher frequency than requested
(defun timed-data (svals)
  (let ((pt-list (get-val 'PTIME svals)) npt-list nP-D wnpt-list
       	(P-D (get-val 'PDATA svals)) vol-incf (volumes (get-val 'SDATA svals))
	(st-list (get-val 'stime svals)) matched-vol first-period-index)
    (setq pt-list (butlast pt-list (length (member nil P-D)))
	  st-list (butlast st-list (length (member nil volumes))))
;;;creat new list of times for price data
    (setq npt-list (last pt-list))
    (do ((n-time (add-to-time (car npt-list) (- *time-interval*))
		 (add-to-time (car npt-list) (- *time-interval*))))
     ;((<= n-time o-time))
      ((or (not (member n-time pt-list)) (member n-time npt-list)))
      (push n-time npt-list)) 
;;;creat new list of corresponding prices    
    (setq nP-D (mapcar #'(lambda (s) (nth (position s pt-list) P-D)) npt-list))
;;;creats list of proper volumes
    (when st-list
      (setq wnpt-list npt-list)
      (loop  (setq vol-incf 0)
	 (dotimes (ith (1+ (position (pop wnpt-list) st-list)))
	   (setq vol-incf (+ (pop volumes) vol-incf))
	   (pop st-list))
	 (push vol-incf matched-vol)
	 (ifn wnpt-list (return)))
;;;figures proper return for item 10
    (setq vol-incf 0 volumes (and (get-val 'aux-data svals)
				  (get-val 'SDATA svals))
	  st-list (get-val 'STIME svals))
    (setq first-period-index (1+ (position (add-to-time
				  (get-val 'open-time svals)
				  *time-interval*)
				st-list)))
    (if (>= (length (remove nil volumes))  first-period-index)
	(dotimes (ith first-period-index)
	  (setq vol-incf (+ (pop volumes) vol-incf))
	  (pop st-list))
      (setq vol-incf (* (apply #'+ (remove nil volumes))
			(/ *time-interval*
			   (* (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
						  (get-val 'first-time svals))
			      60))))))
	  
  (values (car svals) (get-val 'YDATE svals) (get-val 'NDATE svals)
	  npt-list
	  (if (index-32sp)
	      (mapcar #'convert-to-decimal nP-D) nP-D)
	  (mapcar #'(lambda (s) (assoc s (cdr svals))) (scalar-data))
	  (reverse matched-vol)

	  (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
			      (get-val 'first-time svals))
	  (ifn (get-val 'nsamp svals)
		       (get-val 'index-sample-period svals)
		    (/ 1440 (get-val 'nsamp svals)))
	   (if matched-vol (- vol-incf (car (last matched-vol)))))))
					 
|#
;; ftime as 1000 stime as 1230 function returns as 2.5 ;;;
;;;returns nil if ftime or stime is nil	or not integers  
(DEFUN SUB-TIMES-IN-HOURS (ftime stime)
  (and (integerp ftime) (integerp stime)
       (let (ftemp1 ftemp2 stemp1 stemp2)
	 (multiple-value-setq (ftemp1 ftemp2) (truncate ftime 100))
	 (multiple-value-setq (stemp1 stemp2) (truncate stime 100))
	 (/ (- (+ stemp2 (* 60 stemp1))
	       (+ ftemp2 (* 60 ftemp1))) 60))))
	   
;;; tim as 1030 delt as 30 return 1000
(DEFUN ADD-TO-TIME (tim delt)
 (let (fterm sterm fres sres temp)
    (multiple-value-setq (fterm sterm)(truncate tim 100))
    (setq fres (+ (* fterm 60) sterm delt))
    (ifn (plusp fres) (setq fres (+ fres 1440)))
    (multiple-value-setq (temp sres) (truncate fres 60))
    (setq fres (+ (* temp 100) sres))))



;;;tests if the date is the last market day of the week
(defun end-of-market-weekp (yr-mn-dy)
  (let ((nextday (getd yr-mn-dy 'NDATE)))
    (if (numberp nextday)
	(> (subtract-dates yr-mn-dy nextday) 2))))

;;; GETD IS ANALOGOUS IN ROLE TO GETV
;;; 
(defun getd (str-date item &optional (time 'all))
  (BLOCK G-A  
    (LET  ((svals (READT2 str-date)) vals time-list ind dmy)
      (IF (not svals) (RETURN-FROM G-A "DATA NOT AVAILABLE"))
      (setq vals (cdr (assoc item (cdr svals))))
      (if (and (eql item 'pdata) (index-32sp))
	  (setq vals
		(mapcar #'(lambda (s) (and s (convert-to-decimal s))) vals)))
;;;all prices should be floating point numbers
      (if (eql item 'pdata)
	  (setq vals (mapcar #'(lambda (s) (and s (float s))) vals)))
      (if (eql item 'close)
	  (return-from G-A
	    (if (index-32sp)
		(convert-to-decimal
		  (car (last (assoc 'pdata (cdr svals)))))
	      (car (last (assoc 'pdata (cdr svals)))))))
      (cond ((and vals (eql item 'high))
	     (return-from G-A (if (and vals (index-32sp))
				  (convert-to-decimal vals) vals)))
	    ((and (not vals)(eql item 'high)(cdr (assoc 'pdata (cdr svals))))
	     (return-from G-A
	       (if (index-32sp)
		   (convert-to-decimal
		     (apply #'max (cdr (assoc 'pdata (cdr svals)))))
		 (apply #'max (cdr (assoc 'pdata (cdr svals))))))))
      (cond ((and vals (eql item 'low))
		  (return-from G-A (if (and vals (index-32sp))
				       (convert-to-decimal vals) vals)))
	    ((and (not vals)(eql item 'low)(cdr (assoc 'pdata (cdr svals))))
	     (return-from G-A
	       (if (index-32sp)
		   (convert-to-decimal
		     (apply #'min (cdr (assoc 'pdata (cdr svals)))))
		 (apply #'min (cdr (assoc 'pdata (cdr svals))))))))
      (if (eql item 'open)
	  (return-from G-A (if (and vals (index-32sp))
			       (convert-to-decimal vals) vals)))
      (if (eql item 'rollover)
	  (return-from G-A (if (and vals (index-32sp))
			       (convert-to-decimal vals) vals)))
      (if (and (eql item 'ptime)(eql (cdr (assoc 'nsamp (cdr svals))) 1)
	       (eql *time-interval* 'daily-high-low)
	       (cdr (assoc 'high (cdr svals)))) (return-from G-A '(A P)))
      (if (and (eql item 'ptime)(eql (cdr (assoc 'nsamp (cdr svals))) 2)
	       ;(eql *time-interval* 720)
	       ) (return-from G-A '(A P)))
      (if (and (eql item 'ptime)
	       (eql (cdr (assoc 'nsamp (cdr svals))) 1))
	  (return-from G-A '(C)))      
      (when (and (eql item 'pdata)(eql time 'all)
		 (eql (cdr (assoc 'nsamp (cdr svals))) 1)
		 (eql *time-interval* 'daily-high-low)
		 (setq dmy (list (cdr (assoc 'high (cdr svals)))
				 (cdr (assoc 'low (cdr svals))))))
	
	(ifn (car dmy)(setq dmy (list (car (last (get-val 'pdata svals)))
				      (car (last (get-val 'pdata svals))))))
	(if (gtr (car (last vals))
		 (or (get-val 'open svals)
		     (car
		       (last
			 (get-val 'pdata
				  (readt2
				    (get-val 'ydate svals)))))))
		     (setq dmy (nreverse dmy)))
	(return-from G-A
	  (if (index-32sp)
	      (mapcar #'(lambda (s) (and s (convert-to-decimal s))) dmy) dmy)))
      (if (and (eql item 'pdata)(eql time 'all)
	       (eql (cdr (assoc 'nsamp (cdr svals))) 1))
	  (return-from G-A (last vals)))
      (if (eql time 'all) 
	(return-from G-A vals))
      (case (cdr (assoc 'nsamp (cdr svals)))
	(1 (if (eql *time-interval* 'daily-high-low)
	       (let ((valp-list (if (index-32sp)
				    (mapcar #'convert-to-decimal
					    (list (get-val 'high svals)
						  (get-val 'low svals)))
				  (list (get-val 'high svals)
					(get-val 'low svals))))
		     (close (car (last (get-val 'pdata svals))))
		     (open (get-val 'open svals))
		     (prev-close
		       (car
			 (last
			   (get-val 'pdata
				    (readt2
				      (get-val 'ydate svals)))))))
		 
		 (if (gtr close (or open prev-close))
		     (setq valp-list (nreverse valp-list)))
		 (ifn (car valp-list) (setq valp-list (list close close)))
		 (case time
		   (A (car valp-list))
		   (P (cadr valp-list))
		   (all valp-list)
		   (otherwise (cadr valp-list))))
	     (car vals)))
	(2 (case time
	     (A (car vals))
	     (P (cadr vals))
	     (otherwise (cadr vals))))
	(otherwise
	 (CASE item
	   (PDATA
	     (cond ((numberp time)
		    (setq time-list
			  (member time (cdr (assoc 'PTIME (cdr svals))))
			  ind (- (length vals) (length time-list)))
		    (nth ind vals))
		   ((not time)(car (last vals)))
		   ((member time '(A P))
		    (let ((valp-list (if (index-32sp)
					(mapcar #'convert-to-decimal
						(list (get-val 'high svals)
						      (get-val 'low svals)))
				      (list (get-val 'high svals)
					    (get-val 'low svals))))
			  (open (get-val 'open svals))
			 (close (car (last (get-val 'pdata svals))))
			 (prev-close
			   (car
			     (last
			       (get-val 'pdata
					(readt2
					  (get-val 'ydate svals)))))))
		     (if (gtr close (or open prev-close))
			 (setq valp-list (nreverse valp-list)))
		     (case time
		       (A (car valp-list))
		       (P (cadr valp-list))
		       (otherwise (cadr valp-list)))))
		   ((eql time 'C)(car (last vals)))
		   ((eql time 'high)(max* (remove-if #'not vals)))
		   ((eql time 'low)(min* (remove-if #'not vals)))))
	   (SDATA 
	     (setq time-list (member time (cdr (assoc 'STIME (cdr svals))))
		   ind (- (length vals) (length time-list)))
	     (nth ind vals))))))))


;;;;if both prices and volume are higher frequency than requested
(defun timed-data (svals)
  (let ((pt-list (get-val 'PTIME svals)) npt-list nP-D wnpt-list
        (close-time (get-val 'close-time svals))
	(P-D (get-val 'PDATA svals)) vol-incf (volumes (get-val 'SDATA svals))
	(st-list (get-val 'stime svals)) matched-vol first-period-index)
    
;;;creat new list of times for price data
    (setq npt-list (list close-time))
    (do ((n-time (add-to-time close-time (- *time-interval*))
		 (add-to-time (car npt-list) (- *time-interval*))))
      ((or (not (member n-time pt-list)) (member n-time npt-list)))
      (push n-time npt-list))
;;;creat new list of corresponding prices    
    (setq nP-D (mapcar #'(lambda (s) (nth (position s pt-list) P-D)) npt-list))
    (setq npt-list (butlast npt-list (length (member nil nP-D)))
	  st-list (butlast st-list (length (member nil volumes))))
    (setq nP-D (butlast nP-D (length (member nil nP-D))))
;;;creats list of proper volumes
    (when st-list
      (setq wnpt-list npt-list)
      (loop  (setq vol-incf 0)
	 (dotimes (ith (1+ (position (pop wnpt-list) st-list)))
	   (setq vol-incf (+ (pop volumes) vol-incf))
	   (pop st-list))
	 (push vol-incf matched-vol)
	 (ifn wnpt-list (return)))
;;;figures proper return for item 10
    (setq vol-incf 0 volumes (and (get-val 'aux-data svals)
				  (get-val 'SDATA svals))
	  st-list (get-val 'STIME svals))
    (setq first-period-index (1+ (position (add-to-time
				  (get-val 'open-time svals)
				  *time-interval*)
				st-list)))
    (if (>= (length (remove nil volumes))  first-period-index)
	(dotimes (ith first-period-index)
	  (setq vol-incf (+ (pop volumes) vol-incf))
	  (pop st-list))
      (setq vol-incf (* (apply #'+ (remove nil volumes))
			(/ *time-interval*
			   (* (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
						  (get-val 'first-time svals))
			      60))))))
	  
  (values (car svals) (get-val 'YDATE svals) (get-val 'NDATE svals)
	  npt-list
	  (if (index-32sp)
	      (mapcar #'convert-to-decimal nP-D) nP-D)
	  (mapcar #'(lambda (s) (assoc s (cdr svals))) (scalar-data))
	  (reverse matched-vol)

	  (SUB-TIMES-IN-HOURS (get-val 'open-time svals)
			      (get-val 'first-time svals))
	  (ifn (get-val 'nsamp svals)
		       (get-val 'index-sample-period svals)
		    (/ 1440 (get-val 'nsamp svals)))
	   (if matched-vol (- vol-incf (car (last matched-vol)))))))
					 
;;;this function is for testing with daily high low data
;;;it returns the open high low and close prices

(defun trader-access (date
		       &aux svals open-price high-price
		       rollover low-price close-price)
  (block nil
  (setq svals (cdr (readt2 date)))
  (ifn svals (return "DATA NOT AVAILABLE"))
  (setq open-price (cdr (assoc 'open svals))
	high-price (cdr (assoc 'high svals))
	low-price (cdr (assoc 'low svals ))
	close-price (car (last (assoc 'pdata svals)))
	rollover (cdr (assoc 'rollover svals)))
  ;;;if open price is not available use yesterday's close  
  (ifn open-price
       (setq open-price
	     (car (last (assoc 'pdata
			       (cdr (readt2 (cdr (assoc 'ydate svals)))))))))
  (ifn high-price (setq high-price (max (max* (cdr (assoc 'pdata svals)))
					open-price)))
  (ifn low-price (setq low-price (min (min* (cdr (assoc 'pdata svals)))
				      open-price)))
  (when (index-32sp)
    (setq open-price (convert-to-decimal open-price)
	  high-price (convert-to-decimal high-price)
	  low-price (convert-to-decimal low-price)
	  close-price (convert-to-decimal close-price)
	  rollover (convert-to-decimal rollover)))
  (if (> open-price high-price) (setq open-price high-price))
  (if (< open-price low-price) (setq open-price low-price))
      
  (values open-price high-price low-price close-price rollover)))


(defun complete-qvectors (ct)
  (let ((do-qvct t) wv)
    (dolist (wv-pair (symbol-value ct))
      (setq wv (car wv-pair))
      (if do-qvct
	  (ifn (getv wv TL)
	       (multiple-value-bind (dte1 tme1 dte2 tme2)
		   (get-numeric-times (getv wv ST)(getv wv ET))
		 (cond ((or (and (getv wv RL ct)(getv (getv wv RL ct) TL))
			    (not (stringp (getd dte1 'pdata))))
			(ifn (complete-qvector ct wv dte1 tme1 dte2 tme2)
			     (setq do-qvct nil)))
		       (t (setq do-qvct nil))))))
      (cond ((getv wv SB))
	    (t (ifn (or (getv wv RL ct)(getv wv SB)(getv wv ET)(getv wv ST))
		    (putv wv SB 'UNK ct)(putv wv SB nil ct)))))))




(defun complete-qvector (ct wv dte1 tme1 dte2 tme2)
  (let ((incpp dte2) (rootl (getv wv RL ct)) dmy) ; ret)
    (ifn incpp (setq dte2 (getnumdate *curr-et*) tme2 (getnumhour *curr-et*)))
    (cond ((and rootl (getv rootl TL))
	   (setq dmy t)
	   (putv wv TL (time-lengthf ct wv))
	   (putv wv HP (highest-pricef ct wv))
	   (putv wv LP (lowest-pricef ct wv))
	   (putv wv FT (fibo-term (max-lgth% wv)))
	   (when (getv rootl VA)
	     (putv wv VA (volume-avgef ct wv))
	     (putv wv VP (up-volume-peakf ct wv))
	     (putv wv VD (dn-volume-peakf ct wv)))
	   (when (getv rootl AS)
	     (putv wv AS (adv-dec-slopef ct wv))
	     (putv wv AP (adv-dec-peak-ratiof ct wv))))
	  ((not dte2))
	  (t (ifn rootl (putv wv SB 'UNK))
	     (if (setq dmy (fill-qvector dte1 tme1 dte2 tme2))
		 (copy-q-array 0 wv)))) ; (setq ret nil))))
    dmy))
    
 
(defun copy-q-array (wave1 wave2)
  (multiple-value-bind (wsub-indx1 swave1) (truncate wave1 *w-subdim*)
    (multiple-value-bind (wsub-indx2 swave2) (truncate wave2 *w-subdim*)
      (let ((vctr2 (aref *q* (aref (aref (aref *w* wsub-indx2) swave2) 1)))
	    (vctr1 (aref *q* (aref (aref (aref *w* wsub-indx1) swave1) 1))))
	(dotimes (i *qdim*)
	  (setf (aref vctr2 i) (aref vctr1 i)))))))
   
    
    
(defun get-numeric-times (startt endt)
  (let* ((dt1 startt) (dt2 endt) 
	 (dte1 (if dt1 (getnumdate startt)))
	 (tme1 (if dt1 (getnumhour startt)))
	 (dte2 (if dt2 (getnumdate endt)))
	 (tme2 (if dt2 (getnumhour endt))))
    (values dte1 tme1 dte2 tme2
	    (if (and (not dte2) dte1) (flatc dte1) 6)
	    (if (and dte1 tme1) (case tme1
				     ((A P) 2)
				     (C 0)
				     (t 4)) 0))))



(defun maind-x ()
;;;
;;;      Set Paths
;;;
;  (setf *ewaves-home* (environment-variable "EWAVESHOME")
;	*ewaves-local* (environment-variable "EWAVESLOCAL")
;	*counts-upper-dir* (format nil "~acounts/" *ewaves-local*)
;	*upper-dir* *ewaves-local*
;	*plots-ci-calc-dir* (format nil "~acicalc/" *ewaves-local*)
;	*proj-save-dir* (format nil "~aprojs/" *ewaves-local*)
;	)
;  (with-open-file (str 
;		   ;(lcl:string-append *ewaves-home* "markets.path")
;		   (format nil "~a~a" *ewaves-home* "markets.path")
;		   )
;    (setf *database-upper-dir* (read str)))
;;;
;;;      Test Key
;;;
#|
  (setf *public* #(1 2 3 4 5 6 7 8 9 10 11 12 13 14))

  (let (keys file)
    (setf file (format nil "~aewaves.key" (environment-variable "EWAVESHOME")))
    (with-open-file (str file)
      (setf keys (read str))
      (setf keys (cdr (assoc 
		       (format nil "~X" (user::gethostid))
		       keys :test 'string=)))
      (dotimes (i (length keys))
	(setf (aref *public* i) (pop keys)))
      ))
  (setf *public* #(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
  (user::make-privates (gethostid))
  (setf *public* #(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
|#
;;;
;;;       Init System Globals
;;;
  (setq *analyst-mode* t)
  (setq *all-markets* nil
	*indx-cfg* nil)

	
  
  )


;;;
;;;
;;;**************************************************************************
;;;				END OF FILE - DIO1
;;;**************************************************************************


;;; -*- Mode: LISP; Package: USER; Base: 10. -*-

;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
;;;************************************************************************
;;; UTILITIES FOR THE E-WAVES PROGRAM -  EXTRACT WAVE STRUCTURE INFORMATION
;;;************************************************************************
;;;

(defun swing-rating (tdate)
 (let ((dir (parabolic-stops tdate)) rating)
 
       (truncate (+
              (progn
               (setq rating
                     (* 2 (+ (/ (channel-direction-swing-index345 tdate) 3)
                           (/ (zero-strength-swing345 tdate) 3))))
               (if (minusp rating)(floor rating)(ceiling rating)))

               (formation-signal tdate 3)
               (candle-composite tdate 3)
               (* (if (eql dir 'SHORT) 1 -1)
                  (truncate (+ (time-cycle-ratios tdate) (pinpoint tdate)) 3))
              (* 2 (channel-direction11 tdate))
              (* 2 (zero-strength-swing11 tdate))
              (case (ccix-ob-os tdate 21 3)
                 (1 1)
                 (-1 -1)
                 (otherwise 0))
              (* (if (eql dir 'SHORT) 1 -1)
                 (case (cci-range-index tdate 5 5)
                  ((0 1) 1)
                  (otherwise 0)))
               (cci-range-index-swing tdate)) 3)  ;;;feature 14
))


(defun true-range (date)
  (let (true-high true-low yclose)
  (setq yclose (+ (getd (getd date 'ydate) 'close) (or (getd date 'rollover) 0)))
  (setq true-high
       (max (getd date 'high) yclose))
   (setq true-low
       (min (getd date 'low) yclose))
   (- true-high true-low)
   ))
;;;parm1 is 'low or 'high
;;;factor is usually .5 or .618
(defun range-hl (tdate size parm1 factor)
  (let ((date tdate) data1 range lowv highv)
    (dotimes (ith size)
      (push (getd date parm1) data1)
      (setq date (getd date 'ydate)))
    (setq range (- (apply 'max data1)(apply 'min data1)))
    (if (eql parm1 'low) (setq lowv (- (apply 'max data1)(* factor range))))
    (if (eql parm1 'high) (setq highv (+ (apply 'min data1)(* factor range))))
    (cond ((eql parm1 'low) lowv)
          ((eql parm1 'high) highv))))
#|
;;;this is nothing more than the average true range
(defun volatility (date &optional (period 5) (param 1.0))
  (let (ranges)
   (dotimes (ith period)
     (setq ranges (push (true-range (add-mkt-days date (- ith))) ranges)))
  (* param (/ (list-sum ranges) period))
    ))
|#

;;;this is nothing more than the average true range
(defun volatility (tdate &optional (period 14) (param 1.0) (typ 'mean))
  (let (ranges (date (add-mkt-days tdate (-  period))))
 ;  (format t "~% ~A ~A" period date)
   (dotimes (ith period)
     (setq date (getd date 'ndate)) 
     (setq ranges (push (true-range date) ranges))
   ;  (format t "~% ~A  ~A" date ranges)  
    
     )
    
  (cond ((eql typ 'mean) (* param (/ (list-sum ranges) period))) 
	((eql typ 'median)(* param (median ranges)))
	((eql typ 'mode) (* param (mode ranges)))
	((numberp typ) (* param (percentile typ ranges))))
    ))


(defun volatility-ratio-index (date period1 period2 &optional (param 1)(typ 'mean))
 (let (vol-ratio)
  (setq vol-ratio  (/ (volatility date period1 param typ) (volatility date period2 param typ)))
  (cond ((< vol-ratio .50) 6)       
        ((and (>= vol-ratio .50)(< vol-ratio .60)) 5)
        ((and (>= vol-ratio .60)(< vol-ratio .70)) 4)
        ((and (>= vol-ratio .70)(< vol-ratio .80)) 3)
        ((and (>= vol-ratio .80)(< vol-ratio .90)) 2)
        ((and (>= vol-ratio .90)(< vol-ratio 1.00)) 1)
        ((= vol-ratio 1.00) 0)
        ((and (>= vol-ratio 1.00)(< vol-ratio 1.11)) -1)
        ((and (>= vol-ratio 1.11)(< vol-ratio 1.25)) -2)
        ((and (>= vol-ratio 1.25)(< vol-ratio 1.43)) -3)
        ((and (>= vol-ratio 1.43)(< vol-ratio 1.67)) -4)
        ((and (>= vol-ratio 1.67)(< vol-ratio 2.00)) -5)
        ((>= vol-ratio 2.00) -6))
 
))

;;;this is the change in the volatility ratio
(defun volatility-change (tdate period1 period2 &optional (diff 1))
  (- (/ (volatility tdate period1 1.0)(volatility tdate period2 1.0))
    (/ (volatility (add-mkt-days tdate (- diff)) period1 1.0)
       (volatility (add-mkt-days tdate (- diff)) period2 1.0))))

(defun volatility-change-index (tdate period1 period2 &optional (diff 1))
   (let ((vc (volatility-change tdate period1 period2 diff)))
    (cond ((< vc -.375) -4)         
          ((< vc -.25) -3)         
          ((< vc -.125) -2)         
          ((< vc 0) -1)
          ((< vc .125) 1)
          ((< vc .25) 2)
          ((< vc .375) 3)
          ((>= vc .375) 4))))

;;;slope of the volatilty over past days
(defun volatility-direction (date days)
 (if (plusp (nth-value 1 (lregress date days 'tr))) 1 -1)
)
;;;;performs a regression
(defun volatility-trend (tdate days period1)
    (let ((slope (nth-value 1 (lregress tdate days 'tr period1))))
      (cond ((< slope -.40) -5)
            ((< slope -.30) -4)
            ((< slope -.20) -3)
            ((< slope -.10) -2)
            ((< slope 0) -1)
            ((< slope .10) 1)
            ((< slope .20) 2)
            ((< slope .30) 3)
            ((< slope .40) 4)
            ((>= slope .40) 5)
)))
;;;measures from the n day closes
(defun vprices (date &optional (period *duration*)(param *factor*) (days 1) (typ *type*))
  (let ((epsignal (volatility date period param typ)))
    (values (my-pretty-price (- (n-day-high date days 'close) epsignal))
	    (my-pretty-price (+ (n-day-low date days 'close) epsignal))
	   (my-pretty-price  epsignal))))

;;;measures from the exp ave pivots
(defun vprices1 (date &optional (period 4)(param .85) (days 4))
  (let ((epsignal (volatility date period param))
        (ave-days (ave-exp date days 'pivot)))
    (values (my-pretty-price (- ave-days epsignal))
       (my-pretty-price (+ ave-days epsignal)))))

;;;;measures from n day high and low
(defun vprices2 (date &optional (period 5)(param .85) (days 1)(typ 'mean))
  (let ((epsignal (volatility date period param typ)))
    (values (my-pretty-price (- (n-day-high date days ) epsignal))
       (my-pretty-price (+ (n-day-low date days ) epsignal)))))


;;;;measures the volatility from the average of pivots
(defun vprices4 (date &optional (period1 3)(param 1.0)(period2 3))
  (let ((epsignal (volatility date period1 param))
        (ave4 (ave date period2 'pivot)))
    (values (my-pretty-price (- ave4 epsignal))
       (my-pretty-price (+ ave4 epsignal)))))

;;;measures from the simple average of closes
(defun vprices5 (date &optional (period 4)(param .85) (days 4))
  (let ((epsignal (volatility date period param))
        (ave-days (ave date days 'close)) (ave-days-1 (ave (getd date 'ydate) days 'close)))
    (values (my-pretty-price (- ave-days epsignal))
            (my-pretty-price (+ ave-days epsignal))
        (if (plusp (- ave-days ave-days-1)) 'UP 'DN)) 
))
;;;measures from the simple average of closes
(defun vprices6 (date &optional (period 4)(param .85) (days 4))
  (let ((epsignal (volatility date period param))
        (ave-days (ave date days 'close)) (ave-days-1 (ave (getd date 'ydate) days 'close)))
    (values (my-pretty-price (min (- ave-days epsignal) (getd date 'low)))
            (my-pretty-price (max (+ ave-days epsignal) (getd date 'high)))
        (if (plusp (- ave-days ave-days-1)) 'UP 'DN)) 
))
;;;measures from the simple average of closes or current low/high
(defun vprices7 (date &optional (period 3))
  (let ((avel3 (ave date period 'low))(aveh3 (ave date period 'high)))
       
    (values (my-pretty-price (min avel3 (getd date 'low)))
            (my-pretty-price (max aveh3 (getd date 'high))))
   
))


;;This function tests to find the current volatility signal
;;;for swing and position trading systems
;;;days needs to be 1; period is the parameter for the volatility calculation
(defun vhighextremes (tdate &optional (period *duration*)(param *factor*)(days 1)(typ *type*))
  (let (vlow vhigh vlows vhighs (date tdate) highs)
    
  (loop
   
  (multiple-value-setq (vlow vhigh)(vprices date period param days typ))
   (push vlow vlows)(push vhigh vhighs)
   (push (getd date 'high) highs)
  ; (format t "~A ~A ~A~%" date vhighs highs)
   (if (and (> vhigh (min* vhighs))(>= (max* highs) (min* vhighs)))
	    (return (min* vhighs)))
   (setq date (getd date 'ydate)))

  ))
(defun vlowextremes (tdate &optional (period *duration*)(param *factor*)(days 1)(typ *type*))
  (let (vlow vhigh vlows vhighs (date tdate) lows)
    
  (loop
   
  (multiple-value-setq (vlow vhigh)(vprices date period param days typ))
   (push vlow vlows)(push vhigh vhighs);(format t "~A ~A~%" date vlows)
   (push (getd date 'low) lows)
   (if (and (< vlow (max* vlows))(<= (min* lows) (max* vlows)))
       (return (max* vlows)))
   (setq date (getd date 'ydate)))

  ))

(defun vsignals (tdate &optional (period *duration*)(param *factor*)(days 1)(typ *type*))
  (let (vhigh vlow hprice lprice  vsig (counter 0)(date tdate) lows highs
              vhighs vlows)
    
  
  (loop
   (setq hprice (getd date 'high) lprice (getd date 'low)
        )(push lprice lows)(push hprice highs)
   (setq  date (getd date 'ydate))

   (multiple-value-setq (vlow vhigh)(vprices date period param days typ))
   (push vlow vlows)(push vhigh vhighs)

 ;  (format t "~A ~A ~A ~A ~A ~A ~A ~A ~A~%" date (getd date 'close)
;	   hprice vhigh lprice vlow vlows vhighs vsig) 
   (cond ((and (>= hprice ;(max* highs)  ;hprice
		   (min* vhighs));vhigh)
	       ;(vhighextremes date))
	       (<= lprice ;(min* lows);  lprice
		   (max* vlows)));vlow)
	          ; (vlowextremes date)))
	;  (format t "~A  both ~A  ~A~%" date vlows vhighs)
          (setq vsig 'both)    
          (return))
   ;
         ((>= hprice ;(max* highs) ;hprice
	      (min* vhighs));vhigh);      
	      ;(vhighextremes date))
	 ; (format t "~A  buy ~A  ~A~%" date highs vhighs)   	    
          (setq vsig 'buy) (return))
  
         ((<= lprice ;(min* lows) ;lprice
	      (max* vlows));vlow);
	     ; (vlowextremes date))
	 ; (format t "~A sell  ~A  ~A~%" date lows vlows)   	    	      
          (setq vsig 'sell) (return))
	 )
   
     (decf counter)
  )
;;;;now find the entry-long price if prev signal was sell
;;; or entry-short price of prev signal was buy
 ; (format t "~A ~A ~A ~A~%" tdate vsig (getd tdate 'close)(getd (getd tdate 'ydate) 'close))
  (if (and (eql vsig 'both)
           (<= (getd tdate 'close)(getd (getd tdate 'ydate) 'close)))
      (setq vsig 'sell))
  (if (and (eql vsig 'both)
	   (>= (getd tdate 'close)(getd (getd tdate 'ydate) 'close)))
      (setq vsig 'buy))
  
  (setq vlows (reverse vlows) vhighs (reverse vhighs))
  (multiple-value-setq (vlow vhigh)(vprices tdate period param days typ))       
  (push vlow vlows)(push vhigh vhighs); (format t "~A ~A~%" vlows vhighs)
 
  (values vsig counter (if (eql vsig 'buy)(max* (subseq vlows 0 (1+ (- counter))))
			   (min* (subseq vhighs 0 (1+ (- counter)))))) 
		       
   ))

(defun vsignals1 (tdate)
  (let (sdate rdate sdir dir date date-1 vlow vhigh vlows vhighs sldate slprice shdate shprice risk)
  
    (multiple-value-setq (sldate slprice)(sip-low tdate 21))
    (multiple-value-setq (shdate shprice)(sip-high tdate 21))
    
    (setq sdate (min shdate sldate));;;sdate is earliest
    (setq date-1 (getd sdate 'ydate))
   ; (format T "~%sldate= ~A shdate= ~A" sldate shdate)
    (setq  sdir (if (< shdate sldate) 'SHORT 'LONG)
         date sdate dir sdir) 
    (cond ((and (eql shdate sldate) (< (getd date-1 'close)(getd sdate 'close)))
	   (setq dir 'LONG))
	  ((and (eql shdate sldate) (>= (getd date-1 'close)(getd sdate 'close)))
	   (setq dir 'SHORT)))
    
   ;(format T "~%sdate= ~A sdir= ~A" sdate  sdir)
  
    (loop 
     (multiple-value-setq (vlow vhigh risk)(vprices date))
     (push vlow vlows) (push vhigh vhighs)
     (if (>= date tdate)(return)) 
      
   
     (setq date-1 date date (getd date 'ndate))
     (ifn date (return))
   ;  (format T "~%date= ~A vlows= ~A vhighs= ~A  dir= ~A" date vlows vhighs dir) 
     (cond ((and (>= (getd date 'high) (min* vhighs))
		 (<= (getd date 'low) (max* vlows)))
	   ; (format t "~%1 ~A ~A" date dir)
	    (if (>= (getd date 'close) (getd date-1 'close))
		(setq dir 'LONG rdate date vhighs nil vlows nil)
		(setq dir 'SHORT rdate date vhighs nil vlows nil)))
  	   
	   ((and (eql dir 'SHORT)
		 (eql (which-first date) 'low-first)
		 (<= (getd date 'low) vlow);(max* vlows))
		 (>= (getd date 'close) (getd date-1 'close))
		; (>= (getd date 'high) (min* vhighs))
		 );(+ vlow risk)))
	    (if (eql dir 'SHORT) (setq vhighs nil vlows nil))
	   ; (format t "~%2 ~A ~A" date dir)
	    (setq dir 'LONG rdate date))
           ((and (eql dir 'LONG)
	         (eql (which-first date) 'high-first)
		 (>= (getd date 'high) vhigh);(min* vhighs))
		 (<= (getd date 'close)(getd date-1 'close))
	        ; (<= (getd date 'low) (max* vlows))
		 );(- vhigh risk)))
	    ;   (format t "~%3 ~A ~A" date dir)
	    (if (eql dir 'LONG) (setq vhighs nil vlows nil))
	    (setq dir 'SHORT rdate date))

	   
	   ((and (eql dir 'SHORT) (>= (getd date 'high) (min* vhighs)))
	  ;  (format t "~%4 ~A ~A" date dir)
            (setq dir 'LONG rdate date vhighs nil vlows nil))
	   
	   ((and (eql dir 'LONG) (<= (getd date 'low) (max* vlows)))
	   ; (format t "~%5 ~A ~A" date dir)
	    (setq dir 'SHORT rdate date vhighs nil vlows nil))
	 
	 )
     
     )
    
    (values dir rdate (if (eql dir 'SHORT) (min* vhighs) (max* vlows)))
))

;;This function tests to find the current volatility signal
;;;days is the exp moving average size
;;;period is the time length for volatility calculation
(defun vsignals2 (tdate &optional (period 4)(param 1.0)(days 4))
  (let (vhigh vlow hprice lprice epsignal (counter 0)
        (date tdate) vhighs vlows)
  
  (loop
   (setq hprice (getd date 'high) lprice (getd date 'low)
       )
   (setq date (getd date 'ydate))

   (multiple-value-setq (vlow vhigh)(vprices2 date period param days))
  
   (when (>= hprice vhigh) (setq epsignal 'sell) (return))
   (when (<= lprice vlow) (setq epsignal 'buy) (return))
    (push vlow vlows)(push vhigh vhighs)  (decf counter)
   )
   (multiple-value-setq (vlow vhigh)(vprices2 tdate period param days))
   (push vlow vlows)(push vhigh vhighs)
 ;  (values epsignal counter (getd date 'ndate)(if (eql epsignal 'buy) vhigh vlow))
    (values epsignal (if (eql epsignal 'buy) (max* vlows)(min* vhighs)) (add-mkt-days tdate counter))
   ))



(defun objprices1 (date &optional (period1 8) (param1 2.0)(param2 1.0))
  (let ((a8 (ave-exp date period1)) (vol (volatility date (* 10 period1) 1))
        (a8-2 (ave-exp (add-mkt-days date -2) period1)))
 
     (values (+ (* param1 (- vol))
                a8 (* param2 (- a8 a8-2)))
         (+ (* param1 vol)
            a8 (* param2 (- a8 a8-2))))
))
        

;;;;this gives %d
;;;;an exponextial moving average is used to smooth the result
;(defun slow-stocastic (date size)
; (let ((tdate date) (result-num 0)(result-den 0))
; (dotimes (ith 3)
;   (setq result-num (+ result-num (- (getd tdate 'close)(n-day-low tdate size))))
;   (setq result-den (+ result-den (- (n-day-high tdate size)(n-day-low tdate size))))
;   (setq tdate (getd tdate 'ydate)));;

;  (* 100 (/ result-num result-den))
; ))
;;;gsv = greatest swing value
;;;on up days (close less open) take open less low
;;;for aid in placing stop-long or entry-short
(defun failure-selling (tdate &optional(period 21) (param 1.80))
  (let ((date tdate) gsv)
   (loop 
    (if (plusp (- (getd date 'close) (getd date 'open)))
         (push (- (getd date 'open)(getd date 'low)) gsv)
       )
   ;  (print gsv)
     (if (>= (length gsv) period) (return) (setq date (getd date 'ydate))))
    (float (* param (/ (list-sum gsv) (length gsv))))))
   
;;;on down days (close less open) take high less open
;;;;for aid in placing stop-short or entry-long
(defun failure-buying (tdate &optional (period 21)(param 1.80))
  (let ((date tdate) gsv)
   (loop 
    (if (minusp (- (getd date 'close) (getd date 'open)))
         (push (- (getd date 'high)(getd date 'open)) gsv)
       )
     (if (>= (length gsv) period) (return) (setq date (getd date 'ydate))))
       (float (* param (/ (list-sum gsv) (length gsv))))))

(defun fprices (tdate &optional (period 21) (param 1))
   (let ((nopen (getd (getd tdate 'ndate) 'open)) fb fs)
    (setq fs (failure-selling tdate period param)
          fb (failure-buying tdate period param))
   (values (- nopen fs)(+ nopen fb))
))

;;;open to close
(defun adverse-excursion (tdate &optional (period 21))
    (let ((date tdate) gsvup gsvdn vol)
   (loop 
     (setq vol (volatility date 21 1))
    (if (plusp (- (getd date 'close) (getd date 'open)))
        (push (/ (- (getd date 'open)(getd date 'low)) vol) gsvdn);;;adverse selling on up days
       (push (/ (- (getd date 'high)(getd date 'open)) vol) gsvup);;;adverse buying on down days
        )
    (if (and (>= (length gsvdn) period)(>= (length gsvup) period))
        (return))
    (setq date (getd date 'ydate))
   )
    (values (/ (list-sum gsvdn)(length gsvdn))(/ (list-sum gsvup)(length gsvup)))
    
))


;;;;close to close
(defun adverse-excursion1 (tdate &optional (period 21))
    (let ((date tdate) gsvup gsvdn vol)
   
   (loop 
     (setq vol (volatility date period 1))
    (if (plusp (- (getd date 'close) (getd (getd date 'ydate) 'close)))
        (push (/ (- (getd (getd date 'ydate) 'close)(getd date 'low)) vol) gsvdn);;;adverse selling on up days
       (push (/ (- (getd date 'high)(getd (getd date 'ydate) 'close)) vol) gsvup);;;adverse buying on down days
        )
    (if (and (>= (length gsvdn) period)(>= (length gsvup) period))
        (return))
    (setq date (getd date 'ydate))
   )
    (values (/ (list-sum gsvdn)(length gsvdn))(/ (list-sum gsvup)(length gsvup)))
    
))
;;;;creates a list of adverse excurions (as multiples of the volatility)
;;;for all the unfiltered trades
;;;;only appropriate for swing trades
(defun adverse-excursion2 (swings)
  (let (extlow exthigh excursions vol excursion-dollars dates worst
        (path1 (string-append *output-upper-dir* "excursion-percentiles.dat"))
         (path2 (string-append *output-upper-dir* "adverse-excursions.csv")))
 (labels ((nineteen (s)(svref s 19)))
   (dolist (ith swings)
     (set-market (svref ith 0))
     (multiple-value-setq (extlow exthigh)(extreme-low-high (svref ith 1)(svref ith 17)))
    ; (format T "sdate = ~A tdate = ~A extlow = ~A exthigh ~A~%" (svref ith 1) (svref ith 17) extlow exthigh)
     (setq vol (volatility (svref ith 1) 21 1.0)) 
     
     (case (svref ith 2)
         (1  (push (/ (- extlow (svref ith 3)) vol) excursions)
             (push (* (- extlow (svref ith 3))(calculate-point-value (svref ith 1))) excursion-dollars)
             (setq dates (cons (list (* (- extlow (svref ith 3))(calculate-point-value (svref ith 1)))
                                         (svref ith 1) (svref ith 0)) dates))
             )
        (-1 (push (/ (- (svref ith 3) exthigh) vol) excursions)
            (push (* (- (svref ith 3) exthigh)(calculate-point-value (svref ith 1))) excursion-dollars)
             (setq dates (cons (list (* (- (svref ith 3) exthigh)(calculate-point-value (svref ith 1)))
                              (svref ith 1) (svref ith 0))  dates))
           )
    )
   ;  (format T "market = ~A vol = ~A  direction = ~A excursion = ~A~%" (svref ith 0) vol (svref ith 2) (car excursions))
 );;;closes the dolist
   
  
  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "ADVERSE EXCURSION FOR SWING TRADES~%~%")

    (format stream "per%  per volatility ~%~%")
    (format stream "~D  ~D~%" 99 (my-round (percentile .99 excursions) 3))
    (format stream "~D  ~D~%" 95 (my-round (percentile .95 excursions) 3))
    (format stream "~D  ~D~%" 90 (my-round (percentile .90 excursions) 3))
    (format stream "~D  ~D~%" 80 (my-round (percentile .80 excursions) 3))
    (format stream "~D  ~D~%" 70 (my-round (percentile .70 excursions) 3))
    (format stream "~D  ~D~%" 60 (my-round (percentile .60 excursions) 3))
    (format stream "~D  ~D~%" 50 (my-round (percentile .50 excursions) 3))
    (format stream "~D  ~D~%" 40 (my-round (percentile .40 excursions) 3))
    (format stream "~D  ~D~%" 30 (my-round (percentile .30 excursions) 3))
    (format stream "~D  ~D~%" 20 (my-round (percentile .20 excursions) 3))
    (format stream "~D  ~D~%" 10 (my-round (percentile .10 excursions) 3))
    (format stream "~D  ~D~%" 05 (my-round (percentile .05 excursions) 3))
    (format stream "~D  ~D~%~%" 01 (my-round (percentile .01 excursions) 3))

   (format stream "HIGHEST = ~D  LOWEST = ~D~%" (my-round (max* excursions) 3) (my-round (min* excursions) 3))
   (format stream "MEAN = ~D~%" (my-round (mean excursions) 3))
   (format stream "NUM TRADES = ~A~%~%" (length excursions))
 )

 
  (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "~%per%   $ ~%~%")
    (format stream "~D  ~D~%" 99 (round (percentile .99 excursion-dollars)))
    (format stream "~D  ~D~%" 95 (round (percentile .95 excursion-dollars)))
    (format stream "~D  ~D~%" 90 (round (percentile .90 excursion-dollars)))
    (format stream "~D  ~D~%" 80 (round (percentile .80 excursion-dollars)))
    (format stream "~D  ~D~%" 70 (round (percentile .70 excursion-dollars)))
    (format stream "~D  ~D~%" 60 (round (percentile .60 excursion-dollars)))
    (format stream "~D  ~D~%" 50 (round (percentile .50 excursion-dollars)))
    (format stream "~D  ~D~%" 40 (round (percentile .40 excursion-dollars)))
    (format stream "~D  ~D~%" 30 (round (percentile .30 excursion-dollars)))
    (format stream "~D  ~D~%" 20 (round (percentile .20 excursion-dollars)))
    (format stream "~D  ~D~%" 10 (round (percentile .10 excursion-dollars)))
    (format stream "~D  ~D~%" 05 (round (percentile .05 excursion-dollars)))
    (format stream "~D  ~D~%~%" 01 (round (percentile .01 excursion-dollars)))

   (format stream "HIGHEST = ~D  LOWEST = ~D~%" (round (max* excursion-dollars))
            (round (min* excursion-dollars)))
   (format stream "MEAN = ~D~%" (round (mean excursion-dollars)))
   (format stream "NUM TRADES = ~A~%" (length excursion-dollars))
 )
   (setq swings (vsort swings #'< #'nineteen))
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (format stream "DATE,MARKET,DIRECTION,P&L,ADVERSE EXCURSION~%")
   (dolist (jth swings)
     (set-market (svref jth 0))
     (multiple-value-setq (extlow exthigh)(extreme-low-high (svref jth 1)(svref jth 17)))
    
     (format stream "~A,~A,~A,~A,~A~%" (svref jth 1) (svref jth 0) (svref jth 2)(svref jth 19)
           (if (plusp (svref jth 2))
               (round (* (- extlow (svref jth 3))(calculate-point-value (svref jth 1))))
           (round (* (- (svref jth 3) exthigh)(calculate-point-value (svref jth 1))))
          )))
)

  (setq worst (assoc (min* excursion-dollars) dates))
 (values (car worst)
  (car  (member-if #'(lambda(s)(and (eql (second worst) (svref s 1))(eql (third worst) (svref s 0)))) swings))
)))) 


;;;;close to close
(defun propitious-excursion1 (tdate &optional (period 21))
    (let ((date tdate) gsvup gsvdn vol (param 0))
   (loop 
     (setq vol (volatility date period 1))
    (if (>= (getd date 'high) (+ (getd (getd date 'ydate) 'close)) (* param vol))
      (push (/ (- (getd date 'close)(+ (* param vol)(getd (getd date 'ydate) 'close))) vol) gsvdn));;;adverse selling on up days 
   
    (if (<= (getd date 'low)(- (getd (getd date 'ydate) 'close) (* param vol))) 
       (push (/ (- (getd (getd date 'ydate) 'close)(* param vol) (getd date 'close)) vol) gsvup);;;adverse buying on down days
        )
    (if (and (>= (length gsvdn) period)(>= (length gsvup) period))
        (return))
    (setq date (getd date 'ydate))
   )
    (values (/ (list-sum gsvdn)(length gsvdn))(/ (list-sum gsvup)(length gsvup)))
    
))




(defun macd-signal (tdate &optional (days 2))
  (let (mac0 mac1 num1 num2 num3)
    (setq num2 (* 2 (new-dominant-cycle tdate 10 30)))
    (setq num1 (truncate num2 2) num3 (truncate num2 3))
    (setq mac0 (macd tdate num1 num2 num3) mac1 (macd (add-mkt-days tdate (- days)) num1 num2 num3)) 
     
   
    (cond ((and (>  mac0 mac1)) 1)
          ((and (<  mac0 mac1)) -1)
          (t 0))

  ))

(defun swing-entry (date period)
   (let (bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline entry-short entry-long
         stop-short stop-long risk prev-direction)

    (multiple-value-setq (bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline)(channel-trend date period))
 
    (if (> p0 p1) (setq entry-short (- (price-to-go date period) (index-tick-size))
                        entry-long nil stop-long entry-short)
         (setq entry-long (+ (price-to-go date period) (index-tick-size))
               entry-short nil stop-short entry-long)
          )
     (multiple-value-bind (e1 e2)(price-to-go date period)
          (setq risk (abs (- e1 e2))))

     (if (> p0 p1)(setq prev-direction 'up)(setq prev-direction 'dn))
     (values entry-short entry-long stop-short stop-long risk prev-direction)
))


(defun ave-buy-ratio (tdate period)
  (let ((date tdate) ratios)
  (dotimes (ith period)
       (push (if (= (getd date 'high)(getd date 'low)) .5
               (/ (- (getd date 'close) (getd date 'low))(- (getd date 'high)(getd date 'low)))) ratios)
       (setq date (getd date 'ydate)))
   (/ (list-sum ratios) (length ratios))))

(defun ave-buy-ratio-index (tdate period)
  (let ((ratio (ave-buy-ratio tdate period)))
  (cond ((< ratio .1) 0)
        ((< ratio .2) 1)
        ((< ratio .4) 2)
        ((< ratio .6) 3)
        ((< ratio .8) 4)
        ((< ratio .9) 5)
        ((< ratio 1.0) 6))
))
(defun bar-pattern-high (tdate)
   (float (/ (- (getd tdate 'high) (getd tdate 'close))
      (volatility tdate 14 1))))
(defun bar-pattern-low (tdate)
   (float (/ (- (getd tdate 'close) (getd tdate 'low))
      (volatility tdate 14 1))))

(defun bar-pattern-high-index (tdate)
   (let ((bph (bar-pattern-high tdate)))
    (cond ((> bph 1.0) 3)
          ((> bph .33) 2)
          ((> bph .0825) 1)
          (t 0))) )


(defun bar-pattern-low-index (tdate)
   (let ((bpl (bar-pattern-low tdate)))
    (cond ((> bpl 1.0) 3)
          ((> bpl .33) 2)
          ((> bpl .0825) 1)
          (t 0))) )


(defun bar-pattern-high-1 (tdate)
   (float (/ (- (getd (getd tdate 'ydate) 'high) (getd tdate 'close))
      (volatility tdate 14 1))))
(defun bar-pattern-low-1 (tdate)
   (float (/ (- (getd tdate 'close) (getd (getd tdate 'ydate) 'low))
      (volatility tdate 14 1))))

(defun bar-pattern-high-index-1 (tdate)
   (let ((bph (bar-pattern-high-1 tdate)))
    (cond ((> bph 1.5) 3)
          ((> bph .5) 2)
          ((> bph -.5) 1)
          (t 0))) )


(defun bar-pattern-low-index-1 (tdate)
   (let ((bpl (bar-pattern-low-1 tdate)))
    (cond ((> bpl 1.5) 3)
          ((> bpl .5) 2)
          ((> bpl -.5) 1)
          (t 0))) )


;;;;adjusts for degree by dividing by expt to the 2/3
(defun ep-roc (date period1 &optional (period2 105) (typ 'close))
   (/ (roc date period1 typ) (volatility date period2 1 'median)
      (expt (/ period1 10) (/ 2 3)))
 )

#|
(defun ep-roce (date period1 period2 &optional (typ 'close))
   (* period1 (/ (- (ave-exp date period1 typ)(ave-exp (getd date 'ydate) period1 typ))
                 (volatility date period2 1) ))) 

|#

(defun ep-roc-high-low (tdate period days)
 (let (eprocs (date tdate))
    (dotimes (ith days)
       (push (ep-roc date period (* 10 period) 'close) eprocs)
       (setq date (getd date 'ydate)))
   (values (max* eprocs)(min* eprocs))
))
     

;;;;days must be a natural number 1 or more
(defun ep-roc-trend (tdate period1 days)
   (let (asis (date tdate))
     
     (dotimes (ith (1+ days))
        (push (ep-roc date period1 (* 10 period1)) asis)
        (setq date (getd date 'ydate)))
     (setq asis (reverse asis))
      (cond ((= (car asis) (apply #'max asis)) 1)
           ((= (car asis)(apply #'min asis)) -1)
           (t (ep-roc-trend (getd tdate 'ydate) period1 days)))
      
))



;;;relative rate of change
;;; rate of change divided by the average true range
;;; and devided by degree adjustment (2/3 power)
(defun ep-roc-index (date period)
  (let (roch)
   (setq roch (ep-roc date period (* 10 period)))
     
   (cond ((<= roch -4.0) -7)
         ((and (> roch -4.0)(<= roch -2.5)) -6)
         ((and (> roch -2.5)(<= roch -2.0)) -5)
         ((and (> roch -2.0)(<= roch -1.5)) -4)
         ((and (> roch -1.5)(<= roch -1.0)) -3)
         ((and (> roch -1.0)(<= roch -.5)) -2)
         ((and (> roch -.5)(<= roch 0)) -1)
         ((and (> roch 0)(<= roch .5)) 1)
         ((and (> roch .5)(<= roch 1.0)) 2)
         ((and (> roch 1.0)(<= roch 1.5)) 3)
         ((and (> roch 1.5)(<= roch 2.0)) 4)
         ((and (> roch 2.0)(<= roch 2.5)) 5)
         ((and (> roch 2.5)(<= roch 4.0)) 6)
         ((> roch 4.0) 7))  

))



;;;relative rate of change
;;; rate of change divided by the average true range
;;; and divided by degree adjustment
(defun ep-roc-index10 (date period1)
  (let (roch (period2 (* 10 period1)))
   (setq roch (ep-roc date period1 period2))
       
   (cond ((<= roch -6.854) -6)
         ((and (> roch -6.854)(<= roch -4.236)) -5)
         ((and (> roch -4.236)(<= roch -2.618)) -4)
         ((and (> roch -2.618)(<= roch -1.618)) -3)
         ((and (> roch -1.618)(<= roch -1.00)) -2)
         ((and (> roch -1.00)(<= roch 0)) -1)
         ((and (> roch 0)(<= roch 1.00)) 1)
         ((and (> roch 1.00)(<= roch 1.618)) 2)
         ((and (> roch 1.618)(<= roch 2.618)) 3)
         ((and (> roch 2.618)(<= roch 4.236)) 4)
         ((and (> roch 4.236)(< roch 6.854)) 5)
         ((>= roch 6.854) 6))  

))


#|
;;;relative rate of change
;;; rate of change divided by the average true range
;;; values set for period1 = 5 and period2 = 45
(defun roce-rel-index5 (date)
  (let (roch (period1 5)(period2 5))
   (setq roch (ep-roc-range date period1 period2))
   (if (minusp (ep-roce date period1 (* 9 period1) 'close))
       (setq roch (- roch)))
   (cond ((<= roch -3.5) -7)
         ((and (> roch -3)(<= roch -2.5)) -6)
         ((and (> roch -2.5)(<= roch -2)) -5)
         ((and (> roch -2)(<= roch -1.5)) -4)
         ((and (> roch -1.5)(<= roch -1)) -3)
         ((and (> roch -1)(<= roch -.5)) -2)
         ((and (> roch -.5)(<= roch 0)) -1)
         ((and (> roch 0)(<= roch 1)) 1)
         ((and (> roch 1)(<= roch 2)) 2)
         ((and (> roch 2)(<= roch 3)) 3)
        
         ((> roch 3) 4))  

))

;;;relative rate of change
;;; rate of change divided by the average true range
;;; values set for period1 = 21 and period2 = 21

(defun roc-rel-index21 (date)
  (let (roch (period1 21)(period2 21))
   (setq roch (ep-roc-range date period1 period2))
   (if (minusp (ep-roce date period1 (* 9 period1) 'close))
       (setq roch (- roch)))
     
   (cond ((<= roch -5.5) -5)
         ((and (> roch -5.5)(<= roch -4.0)) -4)
         ((and (> roch -4.0)(<= roch -2.5)) -3)
         ((and (> roch -2.5)(<= roch -1.25)) -2)
         ((and (> roch -1.25)(<= roch 0)) -1)
         ((and (> roch 0)(<= roch 1.25)) 1)
         ((and (> roch 1.25)(<= roch 2.5)) 2)
         ((and (> roch 2.5)(<= roch 4.0)) 3)
         ((and (> roch 4.0)(<= roch 5.5)) 4)
        
         ((> roch 5.5) 5))  

))

;;;relative rate of change
;;; rate of change divided by the average true range
;;; values set for period1 = 20 and period2 = 180
(defun roce-rel-index21 (date)
  (let (roch (period1 21)(period2 189))
   (setq roch (ep-roce date period1 period2))
       
   (cond ((<= roch -5.5) -5)
         ((and (> roch -5.5)(<= roch -4.0)) -4)
         ((and (> roch -4.0)(<= roch -2.5)) -3)
         ((and (> roch -2.5)(<= roch -1.25)) -2)
         ((and (> roch -1.25)(<= roch 0)) -1)
         ((and (> roch 0)(<= roch 1.25)) 1)
         ((and (> roch 1.25)(<= roch 2.5)) 2)
         ((and (> roch 2.5)(<= roch 4.0)) 3)
         ((and (> roch 4.0)(<= roch 5.5)) 4)
        
         ((> roch 5.5) 5))  

))
|#
;;;for perod1 = 2 and period2 = 20
(defun roc-rel-index1 (date period1 period2)
  (let (roch)
   (setq roch (ep-roc date period1 period2))
   (cond ((<= roch -1.6) -5)
         ((and (> roch -1.6)(<= roch -1.2)) -4)
         ((and (> roch -1.2)(<= roch -.8)) -3)
         ((and (> roch -.8)(<= roch -.4)) -2)
         ((and (> roch -.4)(<= roch 0)) -1)
         ((and (> roch 0)(<= roch .4)) 1)
         ((and (> roch .4)(<= roch .8)) 2)
         ((and (> roch .8)(<= roch 1.2)) 3)
         ((and (> roch 1.2)(<= roch 1.6)) 4)
         ((> roch 1.6) 5))  

))
;;;period1 is 5 and days is 2
(defun ep-roc-change-index (date period1 days)
   (let ((eproc (- (ep-roc date period1 (* 10 period1))
                  (ep-roc (add-mkt-days date (- days)) period1 (* 10 period1)))))

;;;it appears that large values are bad and small change is good.
;;;; the direction does not seem to matter.
     (cond ((< eproc -3.0) -4)
               ((< eproc -2.0) -3)
               ((< eproc -1.0) -2)
               ((< eproc 0.0) -1)
               ((< eproc 1.0) 1)
               ((< eproc 2.0) 2)
               ((< eproc 3.0) 3)
               ((>= eproc 3.0) 4))
))

;;;period1 is 3 and period2 is 10
(defun ep-roc-change-index2 (date period1 period2)
   (let ((eproc (- (ep-roc date period1 (* 10 period1))
                  (ep-roc date period2 (* 10 period2)))))

       (cond  ((< eproc 0.0) -1)
              ((>= eproc -.0) 1))
))


;;;period1 is 10 and period2 is 20
(defun ep-roc-change-index1 (date period1 period2)
   (let ((eproc (- (ep-roc date period1 (* 10 period1))
                  (ep-roc date period2 (* 10 period2)))))
        (cond ((< eproc -3.6) -5)
              ((< eproc -2.7) -4)
              ((< eproc -1.8) -3)
              ((< eproc -.9) -2)
              ((< eproc 0.0) -1)
              ((< eproc .9) 1)
              ((< eproc 1.8) 2)
              ((< eproc 2.7) 3)
              ((< eproc 3.6) 4)
              ((>= eproc 3.6) 5))
))
;;;;volume oscillator slope compared to price slope 10 25 2
(defun volume-div (tdate days)
   (let ((voslope (nth-value 1 (lregress tdate days 'volume)))
                   
         (pslope (nth-value 1 (lregress tdate days 'close))))
    
     (cond ((not (numberp voslope)) 0)
	   ((not (numberp pslope)) 0) 
	   ((and (plusp voslope)(plusp pslope)) 1);;confirms up
           ((and (plusp voslope)(minusp pslope)) -1);;confirms down
           ((and (minusp voslope)(plusp pslope)) -2)
           ((and (minusp voslope)(minusp pslope)) 2)
           (t 0))
))
;;;;volume oscillator slope compared to price slope 10 25 2
(defun openint-div (tdate days)
   (let ((oslope (nth-value 1 (lregress tdate days 'openint)))
                   
         (pslope (nth-value 1 (lregress tdate days 'close))))
    
     (cond ((not (numberp oslope)) 0)
	   ((not (numberp pslope)) 0)
	   ((and (plusp oslope)(plusp pslope)) 1);;confirms up
           ((and (plusp oslope)(minusp pslope)) -1);;confirms down
           ((and (minusp oslope)(plusp pslope)) -2)
           ((and (minusp oslope)(minusp pslope)) 2)
           (t 0))
))

;;;;volume oscillator slope compared to openint slope 
(defun volume-openint-div (tdate days)
   (let (voslope oslope)
       (dotimes (ith days)
        (ifn (getd (add-mkt-days tdate (- ith)) 'openint) (return 0))
        (ifn (getd (add-mkt-days tdate (- ith)) 'volume) (return 0))
          )
      (setq voslope (nth-value 1 (lregress tdate days 'volume))
            oslope (nth-value 1 (lregress tdate days 'openint)))
    
    (cond ((and voslope oslope (plusp voslope)(plusp oslope)) 'UU);;confirms current direction up or down
           ((and voslope oslope (plusp voslope)(minusp oslope)) 'UD);;closing positions capitulation
           ((and voslope oslope (minusp voslope)(plusp oslope)) 'DU);;;adding to positions at a slowing rate
           ((and voslope oslope (minusp voslope)(minusp oslope)) 'DD);;;trend is ending
           (t 0))
))


;;;most typical are 5/20 and 10/25
;;;;this is the ratio of the two moving averages on the current day

;;;;adjusts for missing volume on current day for futures
(defun volume-index (tdate period1 period2)
  (let (rat vol1 vol2 (date tdate))
   ;;;for futures contracts replace current day volume of zero with previous day's volume.
   
    (if (and (index-futurep)(zerop (getd tdate 'volume))) (setq vol1 (getd (getd tdate 'ydate) 'volume))
                 (setq vol1 (getd tdate 'volume)))
        (setq vol1 (or vol1 0))
       (setq vol2 vol1)
      (setq vol1
            (dotimes (ith (1- period1) (/ vol1 period1))
    ; (format t "date= ~A vol = ~A vol1 = ~A~%" date (getd date 'volume) vol1)
                (setq date (getd date 'ydate))
                 (setq vol1 (+ vol1 (or (getd date 'volume) 0)))))
     ; (format t "2date= ~A vol = ~A vol1 = ~A~%~%" date (getd date 'volume) vol1)
       (setq date tdate)
       (setq vol2
            (dotimes (ith (1- period2) (/ vol2 period2))
    ; (format t "date= ~A vol = ~A vol2 = ~A~%" date (getd date 'volume) vol2)
                (setq date (getd date 'ydate))
                 (setq vol2 (+ vol2 (or (getd date 'volume) 0)))))

  ; (format T "date = ~A vol2 =~A vol1 = ~A " date vol2 vol1)
  (setq rat (/ vol1 (if (zerop vol2) 1 vol2)))
  (cond  ((= rat 0) 0)
         ((> rat 2.100) 5)
         ((> rat 1.75) 4)
         ((> rat 1.45) 3)
         ((> rat 1.2) 2)
         ((> rat 1.0) 1)
         ((> rat .80) -1)
         ((> rat .60) -2)
         ((> rat .4) -3)
         ((> rat .25) -4)
         ((<= rat .25) -5))
))
;;;;expressed as a percentage
;;;assumes most recent day is a repeat of the previous day volume
;;;for futures most recent day has zero volume
(defun volume-ave-ratios (tdate period1 period2)
   (let (vol2 vol1 (date tdate))
 
      (if (index-futurep) (setq vol1 (getd (getd tdate 'ydate) 'volume))
                 (setq vol1 (getd tdate 'volume)))
       (setq vol1 (or vol1 0))
       (setq vol2 vol1)
      (setq vol1
            (dotimes (ith (1- period1) (float (/ vol1 period1)))
  ;   (format t "date= ~A vol = ~A vol1 = ~A~%" date (getd date 'volume) vol1)
                (setq date (getd date 'ydate))
                 (setq vol1 (+ vol1 (or (getd date 'volume) 0)))))
 ;     (format t "2date= ~A vol = ~A vol1 = ~A~%~%" date (getd date 'volume) vol1)
       (setq date tdate)
     ; (format t "date= ~A vol = ~A vol2 = ~A~%" date (getd date 'volume) vol2)
       (setq vol2
            (dotimes (ith (1- period2) (float (/ vol2 period2)))
                 (setq date (getd date 'ydate))
                 (setq vol2 (+ vol2 (or (getd date 'volume) 0)))
     ;      (format t "date= ~A vol = ~A vol2 = ~A~%" date (getd date 'volume) vol2)
               ))
      ;  (format t "3date= ~A vol1 = ~A vol2 = ~A~%" date vol1 vol2)

    (my-round (* 100 (1- (/ (if (zerop vol1) 1 vol1)
                    (if (zerop vol2) 1 vol2)))) 1)
))


;;;this uses volume-ave-ratios for the regression
;;;so it already compensates for futures
(defun volume-ratio-index (tdate period3 &optional (typ 'vi5-20))
   (let ((rslope (nth-value 1 (lregress tdate period3 typ))))
       (cond ((< rslope -8) -5)
             ((< rslope -4) -4)
             ((< rslope -1) -3)
             ((< rslope -.5) -2)
             ((< rslope 0) -1)
             ((= rslope 0) 0) 
             ((< rslope .5) 1)
             ((< rslope 1) 2)
             ((< rslope 4) 3)
             ((< rslope 8) 4)
             ((>= rslope 8) 5)
             )
))
;;;finds the minumum volume-ratio-index over the past days
;;;period3 is the regression length
(defun vril (tdate period3 days)
  (let (vris (date tdate))
  (dotimes (ith days)
    (push (volume-ratio-index date period3 'vi5-20) vris)
    (setq date (getd date 'ydate)))
  
  (min* vris)
))

;;;finds the change in the volume oscillator
;;;;this is the delta from today versus period3 days ago.
(defun volume-ratio-index1 (tdate period1 period2 period3)
   (let ((rslope (- (volume-ave-ratios tdate period1 period2)
                    (volume-ave-ratios (add-mkt-days tdate (- period3)) period1 period2))))
    
       (cond ((< rslope -24) -5)
             ((< rslope -12) -4)
             ((< rslope -6) -3)
             ((< rslope -3) -2)
             ((< rslope 0) -1) 
             ((= rslope 0) 0)
             ((< rslope 3) 1)
             ((< rslope 6) 2)
             ((< rslope 12) 3)
             ((< rslope 24) 4)
             ((>= rslope 24) 5)
             )
  
))


;;;returns 1 if slope of on balance volume is upward for a period of days. returns -1 if downward sloping
(defun obv (tdate days)
  (let ((date (add-mkt-days tdate (- (1- days)))) (cumvol (list 0)))
   (when (getd date 'volume)
    (if (zerop (getd tdate 'volume))(setq date (getd date 'ydate)))
   (dotimes (ith days)
   ;  (format T "date= ~A dir= ~A vol= ~A~%" date (if (plusp (roc date 1)) 1 -1)(getd date 'volume))
     (push (+ (car cumvol)(* (if (plusp (roc date 1)) 1 -1)(or (getd date 'volume) 0))) cumvol)
     (setq date (getd date 'ndate))
     ))
   (values (cond ((plusp (car cumvol)) 1)
                 ((minusp (car cumvol)) -1)
                  (t 0))
             (butlast cumvol))
))


;;;;improved OBV 
;;;for futures on current day add the previous day volume instead of zero
;;;Williams million dollar formula
(defun adobv (tdate days)
  (let ((date (add-mkt-days tdate (- (1- days)))) (cumvol (list 0)))
   (dotimes (ith days)
   ;  (format T "date= ~A dir= ~A vol= ~A~%" date (if (plusp (roc date 1)) 1 -1)(getd date 'volume))
     (push (round (+ (car cumvol)
                     (* (if (zerop (true-range date)) 1 (/  (body-range date) (true-range date)))
                         (if (and (= date tdate) (index-futurep))
                             (or (getd (getd date 'ydate) 'volume) 0)
                           (or (getd date 'volume) 0))))) cumvol)
     (setq date (getd date 'ndate))
     )
  (values (cond ((plusp (car cumvol)) 1)
         ((minusp (car cumvol)) -1)
         (t 0))
     (butlast  cumvol))
))

(defun obv-div (tdate days)
   (let ((obvslope (obv tdate days)) (pslope (nth-value 1 (lregress tdate days 'hl))))
     (cond ((and (plusp obvslope)(plusp pslope)) 2);;confirmed up
           ((and (plusp obvslope)(minusp pslope)) 1)
           ((and (minusp obvslope)(plusp pslope)) -1)
           ((and (minusp obvslope)(minusp pslope)) -2);;confirmed dow
           (t 0))
))

;;;cummulatice change openint times volume time change in price
;;;returns 1 for up and -1 for down. 
(defun OIXDP (tdate &optional (days 11))
   (let ((date (add-mkt-days tdate (- (1- days)))) intercept slope correlation r-squared stderr xs (cum 0))
    (labels ((get-openint (xdate)
                (cond ((not (getd xdate 'openint)) 0)
                      ((listp (getd xdate 'openint))(car (getd xdate 'openint)))
                      ((zerop (getd xdate 'openint))
                       (or (getd (getd xdate 'ydate) 'openint) 0))
                     
                      (t (getd xdate 'openint))))
              (get-volume (xdate)
                (cond ((not (getd xdate 'volume)) 0)
                      ((listp (getd xdate 'volume))(car (getd xdate 'volume)))
                      ((zerop (getd xdate 'volume))
                       (or (getd (getd xdate 'ydate) 'volume) 0))
                     
                      (t (getd xdate 'volume))))
              )
      
       (dotimes (ith days)
     ;    (format T "~%date = ~A roc= ~A Volume= ~A OPENINT = ~A  CUM= ~A~%"
     ;                date (roc date 1) (get-volume date)(get-openint date) cum)
          (setq cum  (+ cum (* (roc date 1);(get-volume date)
                               (- (get-openint date)(get-openint (getd date 'ydate))))))
          (push (list (- days ith) cum)  xs)
           (setq date (getd date 'ndate)))

       
 ;;;linear regression requires a list of pairs in a list (point coordinates x and y)
;;; example ((x1 y1)(x2 y2)(x3 y3) ...
  (if (apply #'= (mapcar #'second xs));;;only if all prices are equal
     (setq intercept (second (car xs)) slope 0 correlation 1 r-squared 1)   
  (multiple-value-setq (intercept slope correlation r-squared stderr) (linear-regression xs)))
    ;(print xs)
 ;  (dolist (ith xs)
 ;        (push (- (second ith)(+ intercept (* (first ith) slope))) deviations));;

;    (setq lower-dev (min* deviations) upper-dev (max* deviations))

     (if (plusp slope) 1 -1)
   )))

(defun oixdp1 (tdate)
    (- (oixdp tdate)(oixdp (getd tdate 'ydate))))

;;;;expressed as a percentage
(defun openint-ave-ratios (tdate period1 period2)
   (let (vol2 vol1 (date tdate))
   (labels ((getdo (odate typ) (cond ((not (getd odate typ)) 0)
                                     ((listp (getd odate typ))
                                          (if (zerop (car (getd odate typ)))
                                            (if (listp (getd (getd odate 'ydate) typ))
                                                 (car (getd (getd odate 'ydate) typ))
                                                (getd (getd odate 'ydate) typ))
                                               (car (getd odate typ))))
                                       
                                     ((zerop (getd odate typ)) (getd (getd odate 'ydate) typ))
                                     (t (getd odate typ)))))
 
      (if (and (index-futurep)(zerop (getd tdate 'openint))) (setq vol1 (getdo (getd tdate 'ydate) 'openint))
                 (setq vol1 (getdo tdate 'openint)))
      ;    (format t "1date= ~A vol = ~A vol1 = ~A~%" date (getdo date 'openint) vol1)
       (setq vol1 (or vol1 0))
       (setq vol2 vol1)
     ; (format t "2date= ~A vol = ~A vol1 = ~A~%" date (getdo date 'openint) vol1)
      (setq vol1
            (dotimes (ith (1- period1) (float (/ vol1 period1)))
        ;    (format t "3date= ~A vol = ~A vol1 = ~A~%" date (getdo date 'openint) vol1)
                (setq date (getd date 'ydate))
                 (setq vol1 (+ vol1 (or (getdo date 'openint) 0)))))
     ; (format t "4date= ~A vol = ~A vol1 = ~A~%~%" date (getdo date 'openint) vol1)
       (setq date tdate)
     ; (format t "5date= ~A vol = ~A vol2 = ~A~%" date (getdo date 'openint) vol2)
       (setq vol2
            (dotimes (ith (1- period2) (float (/ vol2 period2)))
                 (setq date (getd date 'ydate))
                 (setq vol2 (+ vol2 (or (getdo date 'openint) 0)))
         ;  (format t "6date= ~A vol = ~A vol2 = ~A~%" date (getdo date 'openint) vol2)
               ))
     ;   (format t "5date= ~A vol1 = ~A vol2 = ~A~%" date vol1 vol2)

    (my-round (* 100 (1- (/ (if (zerop vol1) 1 vol1)
                    (if (zerop vol2) 1 vol2)))) 1)
)))

;;;;volume oscillator slope compared to price slope 10 25 2
(defun openint-div-ratio (tdate period1 period2 days)
   (let ((voslope (- (openint-ave-ratios tdate period1 period2)
                    (openint-ave-ratios (add-mkt-days tdate (- days)) period1 period2)))
         (pslope (roc tdate days 'close)))
    
    (cond ((and (plusp voslope)(plusp pslope)) 2);;confirmed up
           ((and (plusp voslope)(minusp pslope)) 1)
           ((and (minusp voslope)(plusp pslope)) -1)
           ((and (minusp voslope)(minusp pslope)) -2);;confirmed dow
           (t 0))
))

;;;this uses openint-ave-ratios for the regression
;;;so it already compensates for futures
(defun openint-ratio-index (tdate period3 &optional (typ 'op1-20))
   (let ((rslope (nth-value 1 (lregress tdate period3 typ))))
       (cond ((< rslope -8) -5)
             ((< rslope -4) -4)
             ((< rslope -1) -3)
             ((< rslope -.5) -2)
             ((< rslope 0) -1)
             ((= rslope 0) 0) 
             ((< rslope .5) 1)
             ((< rslope 1) 2)
             ((< rslope 4) 3)
             ((< rslope 8) 4)
             ((>= rslope 8) 5)
             )
))

;;;
(defun reversion (date period1)
   (let* ((ave1 (ave date period1))(ave2 (nth-value 2 (parabolic-stops date)))
         (dev (/ (- (getd date 'close) ave1)(volatility date period1 1))))

     (cond ((and (> ave1 ave2) (< dev -1.618)) 5)
           ((and (> ave1 ave2) (< dev -1)) 4)
           ((and (> ave1 ave2) (< dev -.618)) 3)
           ((and (> ave1 ave2) (< dev 0)) 2)
           ((> ave1 ave2) 1)
           ((and (< ave1 ave2) (> dev 1.618)) -5)
           ((and (< ave1 ave2) (> dev 1)) -4)
           ((and (< ave1 ave2) (> dev .618)) -3)
           ((and (< ave1 ave2) (> dev 0)) -2)
           ((< ave1 ave2) -1)
           (t 0))
))

;;;Welles Wilder swing index page 90
(defun ww-swing-index (d2)
  (let* ((h2 (getd d2 'high))(l2 (getd d2 'low))(o2 (getd d2 'open))
        (cs2 (getd d2 'close))(d1 (getd d2 'ydate))
        (o1 (getd d1 'open))(cs1 (getd d1 'close))
        (kay (max (abs (- h2 cs1))(abs (- l2 cs1))))
        (elle (volatility d2 252 4.0));;;uses 4 times volatility to stand in for the limit move
        argh si ;;;argh is the "R" in Wilders formaula for SI      
   )
    (setq argh
          (cond ((>= (abs (- h2 cs1)) (max (abs (- l2 cs1))(abs (- h2 l2))))
                 (+ (abs (- h2 cs1)) (* -.50 (abs (- l2 cs1)))(* .25 (abs (- cs1 o1)))))
  
                ((>= (abs (- l2 cs1)) (max (abs (- h2 cs1))(abs (- h2 l2))))
                 (+ (abs (- l2 cs1))(* -.50 (abs (- h2 cs1)))(* .25 (abs (- cs1 o1)))))

                ((>= (abs (- h2 l2))(max (abs (- h2 cs1))(abs (- l2 cs1))))
                 (+ (abs (- h2 l2))(* .25 (abs (- cs1 o1)))))
                ))
     (if (zerop argh) (setq si 0)
         (setq si (/ (* 50 (+ (- cs2 cs1) (* .50 (- cs2 o2)) (* .25 (- cs1 o1))) kay)
                     (* argh elle))))
     (round si)

))

(defun ww-swing-index1 (tdate)
  (let ((si (ww-swing-index tdate)))
   
      (cond ((<= si -20) -3)
           
            ((< si -10) -2)
            ((< si 0) -1)
            ((< si 10) 1)
            ((< si 20) 2)
            ((>= si 20) 3)
           )
))

;;;;this is the accumulated swing index
(defun ww-asi (tdate days)
  (let ((sis (list 0))(date (add-mkt-days tdate (- days))))
   (dotimes (ith days)
       (setq date (getd date 'ndate))     
       (push (+ (ww-swing-index date)(car sis)) sis))
     

sis
))
;;;;days must be and natural number larger than 1
;;; 3 means higher or lowere than the previous two days.
(defun ww-asi-trend (tdate days)
   (let (asis)
     (setq asis (ww-asi tdate days))
     (cond ((> (car asis) (apply #'max (subseq asis 1 days))) 1)
           ((< (car asis) (apply #'min (subseq asis 1  days))) -1)
           (t (ww-asi-trend (getd tdate 'ydate) days)))
))
;;;find asi highs and lows

;;;need at least three most recent inflection points
(defun asi-extremes (tdate)
  (let* (xtremes trend
         (sis (ww-asi tdate 40)) (csis (car sis)))
       (loop
         (if (> (first sis) (second sis)) (return (setq trend 1)))
         (if (< (first sis)(second sis)) (return (setq trend -1)))
         (setq sis (cdr sis)))
      ;  (format t "trend = ~A  sis = ~A~%" trend sis)
;;;it takes three prices to test for a high or low in asi
   (dotimes (ith 40)   
     
;;;is the second si  a high or low?
     (cond ((and (> (first sis)(second sis)) (> (third sis)(second sis)))
            (setq trend -1)(push (second sis) xtremes))
           ((and (= (first sis)(second sis))(= trend -1)(< (third sis)(second sis)))
                 (setq trend 1)(push (second sis) xtremes))

           ((and (< (first sis)(second sis))
                 (< (third sis)(second sis)))(setq trend 1) (push (second sis) xtremes))
           ((and (= (first sis)(second sis))(= trend 1)(> (third sis)(second sis)))
                (setq trend -1)(push (second sis) xtremes)))

          ; (format T "~%trend = ~A sis = ~A~% extremes= ~A~%" trend sis xtremes)
           (if (>= (length xtremes) 3)(return))
           (setq sis (cdr sis)))

   (values (reverse xtremes) csis)

))
     
(defun asi-direction (tdate)
  (let (xtremes csi)
   (multiple-value-setq (xtremes csi)(asi-extremes tdate))

   (cond ((and (> csi (first xtremes))
               (> csi (second xtremes))) 1)
         ((and (< csi (first xtremes))
               (< csi (second xtremes))) -1)

         ((> (- (first xtremes) csi) 60) -1)
         ((> (- csi (first xtremes)) 60) 1)

        ; ((and (> csi (first xtremes))
        ;       (< csi (second xtremes))
        ;       (< (first xtremes)(third xtremes))) -1)
        ; ((and (> csi (first xtremes))
        ;       (< csi (second xtremes))
        ;       (> (first xtremes)(third xtremes))) 1)

        ; ((and (< csi (first xtremes))
        ;       (> csi (second xtremes))
        ;       (< (first xtremes)(third xtremes))) -1)
        ; ((and (< csi (first xtremes))
        ;       (> csi (second xtremes))
        ;       (> (first xtremes)(third xtremes))) 1)
         (t (asi-direction (getd tdate 'ydate))))

   
))   




#|
;;;Welles Wilder swing index page 90
(defun ww-swing-index-test ()
  (let* ((h2 52.00)(l2 51.00 )(o2 52.0)
        (cs2 51.0)
        (o1 53.50)(cs1 52.50)
        (kay (max (abs (- h2 cs1))(abs (- l2 cs1))))
        (elle 3.0);;;uses 4 times volatility to stand in for the limit move
        argh si ;;;argh is the "R" in Wilders formaula for SI      
   )
    (setq argh
          (cond ((>= (abs (- h2 cs1)) (max (abs (- l2 cs1))(abs (- h2 l2))))
                 (+ (abs (- h2 cs1)) (* -.50 (abs (- l2 cs1)))(* .25 (abs (- cs1 o1)))))
  
                ((>= (abs (- l2 cs1)) (max (abs (- h2 cs1))(abs (- h2 l2))))
                 (+ (abs (- l2 cs1))(* -.50 (abs (- h2 cs1)))(* .25 (abs (- cs1 o1)))))

                ((>= (abs (- h2 l2))(max (abs (- h2 cs1))(abs (- l2 cs1))))
                 (+ (abs (- h2 l2))(* .25 (abs (- cs1 o1)))))
                ))
 
     (setq SI (/ (* 50 (+ (- cs2 cs1) (* .50 (- cs2 o2)) (* .25 (- cs1 o1))) kay)
                 (* argh elle)))

))
|#

(defun primitive-direction (tdate filt)
  (let (sdate turns turn-dates ptg)
     (setq *n-filt* filt)
  (setq sdate (add-mkt-days tdate (- (* 10 filt))))
   (multiple-value-setq (turns turn-dates ptg)
            (find-all-primitives (format nil "~A" sdate)(conv-to-string tdate 'P)))  
 (values (if (> (car turns)(cadr turns)) -1 1) ptg)

))

;;;returns direction and stoploss
(defun psycho-direction (date days)
 (let ((direction 0)(5high (n-day-high date days))(5low (n-day-low date days)))
;;;;start with the penultimate date
;;;;go backwards one day at a time testing for low or high based on psycho difinition parameter(days)
 (setq direction 
       (cond ((and (= (getd date 'high) 5high)(= (getd date 'low) 5low))
              (if (> (getd date 'close)(getd date 'open)) 1 -1))
             ((= (getd date 'high) 5high) 1)
             ((= (getd date 'low) 5low) -1)
             (t (psycho-direction (getd date 'ydate) days))
                 ))
   (values direction (if (plusp direction) 5low 5high))
))

;;;;this gives %k (of slow stochastic) 
;;;;this is %D for first value and difference of %K and %D for the second value
(defun fast-stochastic (date size &optional (smooth 3))
 (let ((tdate date) fsto fstos)
 (dotimes (ith smooth)
   (if (/= (n-day-high tdate size)(n-day-low tdate size))
       (setq fsto (/ (- (getd tdate 'close)(n-day-low tdate size))
                 (- (n-day-high tdate size)(n-day-low tdate size))))
      (setq fsto 0))
   (push fsto fstos)
   (setq tdate (getd tdate 'ydate)))

  (values (* 100 (/ (list-sum fstos) smooth)) ;;;this is %D
          (- (* 100 (car (last fstos))) (* 100 (/ (list-sum fstos) smooth)))
          (* 100 (car (last fstos))))
 ))

(defun fast-stochastic-signal (date size &optional (smooth 3))
  (let ((fst0 (fast-stochastic date size smooth))
        (fst-1 (fast-stochastic (getd date 'ydate) size smooth)))
  (cond ((and (> fst0 20)(< fst-1 20)) 1)
        ((and (< fst0 80)(> fst-1 80)) -1)
        (t 0))
))

(defun slow-stochastic (tdate size)
 (let ((result-total 0)(date tdate))
  (dotimes (jth 3 (/ result-total 3.0))
     (setq result-total (+ result-total (fast-stochastic date size)))
     (setq date (getd date 'ydate)))
 
  ))

(defun stochastic-delta (tdate period smooth &optional (num 1))
   (- (nth-value 0 (fast-stochastic tdate period smooth))(nth-value 0 (fast-stochastic (add-mkt-days tdate (- num)) period smooth))))

;;;;
(defun stochastic-delta-index (date size &optional (num 1))
   (let (ss)
     (setq ss (stochastic-delta date size num)) 
     (cond ((>= ss 15) 4)
           ((> ss 10) 3)
           ((> ss 5) 2)
           ((> ss 0) 1)
           ((> ss -5) -1)
           ( (> ss -10) -2)
           ((> ss -15) -3)
           ((<= ss -15) -4))
))


;;;;has 8 levels
(defun slow-stochastic-index (date size)
   (let (ss)
     (setq ss (slow-stochastic date size)) 
     (cond ((<= ss 10) -4)
           ((and (> ss 10)(<= ss 20)) -3)
           ((and (> ss 20)(<= ss 30)) -2)
           ((and (> ss 30)(<= ss 50)) -1)
           ((and (> ss 50)(<= ss 70)) 1)
           ((and (> ss 70)(<= ss 80)) 2)
           ((and (> ss 80)(<= ss 90))  3)
           ((>= ss 90) 4))
))


;;;;has 6 levels
(defun fast-stochastic-index (date size &optional (smooth 3))
   (let (ss)
     (setq ss (nth-value 2 (fast-stochastic date size smooth))) 
     (cond ((< ss 3) -4)
           ((<= ss 10) -3)
           ((and (> ss 10)(<= ss 20)) -2)
           ((and (> ss 20)(<= ss 40)) -1)
	   ((and (> ss 40)(<= ss 60)) 0)
           ((and (> ss 60)(<= ss 80))  1)
           ((and (> ss 80)(<= ss 90))  2)
           ((and (>= ss 90)(<= ss 97)) 3)
           ((> ss 97) 4))
))


;;;
(defun stochastic-rsi (tdate size)
  (let ((date tdate) rsis rsi-high rsi-low (rsi-tdate (rsi tdate size)))

   (dotimes (ith (1+ size))
     (push (rsi date size) rsis)
     (setq date (getd date 'ydate)))
  
    (setq rsi-high (apply #'max rsis)
          rsi-low (apply #'min rsis)) 
    (/ (- rsi-tdate rsi-low)(- rsi-high rsi-low))
))
(defun stochastic-rsi-index (date size)
 (let (rs)
   (setq rs (stochastic-rsi date size))
    
   (cond ((<= rs 20) -2)
           
         ((and (> rs 20)(<= rs 50)) -1)
         ((and (> rs 50)(<= rs 80)) 1)
        
         ((>= rs 80) 2))
))

(defun cycle-crossings (tdate min-period max-period)
  (let (period diffs (date tdate) diffs2 (counter1 0)(counter2 0) dates dates2)
    (setq period (new-dominant-cycle tdate min-period max-period))

    (dotimes (ith (truncate (* period 20)))
      (push  (- (ave date (truncate period 2) 'pivot) (ave date period  'pivot)) diffs)
      (push date dates)
     (setq date (getd date 'ydate)))

    (setq diffs (nreverse diffs) dates (nreverse dates))
    (do ((diffs diffs (cdr diffs))
	(ith (car diffs) (car diffs))
	 (ith-1 (second diffs) (second diffs))
	 (dates dates (cdr dates))
	 (date (car dates) (car dates)))
	((not ith-1) 0)
      (push
      (cond ((and (>= ith 0)(>= ith-1 0)) 0)
	    ((and (<= ith 0)(<= ith-1 0)) 0)
	    ((and (>= ith 0) (< ith-1 0)) 1)
            ((and (<= ith 0)(> ith-1 0)) -1))
      diffs2)
       (push date dates2))
    (setq diffs2 (nreverse diffs2) dates2 (nreverse dates2))

    (format t "~A~%" (length (member-if #'(lambda(s) (\= s 0)) diffs)))

    (dolist (kth diffs2)
      (if (= kth 1) (incf counter1))
      (if (= kth -1)(incf counter2))
      )
    (values (float (/ (* period 20) (+ counter1 counter2))) counter1 counter2 diffs2 dates2)
    
  ))
    
(defun cycle-signal (tdate &optional (min-period 10)(max-period 30))
  (let* (period diffs (date tdate) csignal ave5d)
  (setq period (new-dominant-cycle tdate min-period max-period))
  (setq ave5d (- (getd tdate 'close)(ave tdate 5)))
  (dotimes (ith (truncate (/ period 3)))
     (push  (- (ave date (truncate period 2) 'close) (ave date period  'close)) diffs)
     (setq date (getd date 'ydate)))
  (setq diffs (nreverse diffs))
  (do ((ith (first diffs) (first diffs1))
       (jth (second diffs) (second diffs1))
       (kth (third diffs) (third diffs1))
       (diffs1 (cdr diffs)(cdr diffs1)))
       ((or csignal (not kth)))
    
       (setq csignal
             (cond ((and (> ith 0)(< jth 0)) -1)
                   ((and (< ith 0)(> jth 0)) 1)
                   ((and (> ith 0)(>= jth 0)(< kth 0)) -2)
                   ((and (< ith 0)(<= jth 0)(> kth 0)) 2)
                   (t 0))))
  (values
  (cond ((and (= csignal -1) (plusp ave5d)) 'S1)
	((and (= csignal -1) (minusp ave5d)) 'S2)
	((and (= csignal 1) (minusp ave5d)) 'L1)
	((and (= csignal 1) (plusp ave5d)) 'L2)
	(t 0))
     period)
))

(defun cycle-index (tdate &optional (min-period 10)(max-period 30))
  (let (ci period)
     (setq period (new-dominant-cycle tdate min-period max-period))
     (setq ci (/ (- (ave tdate (truncate period 2) 'pivot)(ave tdate period 'pivot))
              (volatility tdate period 1)))
    (cond ((> ci 1.5) 4)
          ((> ci 1.0) 3)
          ((> ci .5) 2)
          ((> ci 0) 1)
          ((> ci -.5) -1)
          ((> ci -1.0) -2)
          ((> ci -1.5) -3)
          ((<= ci -1.5) -4)
          )
))

(defun cycle-index10 (tdate &optional (min-period 10)(max-period 30))
  (let (ci period)
     (setq period (new-dominant-cycle tdate min-period max-period))
     (setq ci (/ (- (ave tdate (truncate period 2) 'pivot)(ave tdate period 'pivot))
              (volatility tdate period 1)(expt (/ period 10) (/ 2 3))))
    (cond ((> ci 1.382) 5)
          ((> ci 1.0) 4)
          ((> ci .618) 3)
          ((> ci .382) 2)
          ((> ci 0) 1)
          ((> ci -.382) -1)
          ((> ci -.618) -2)
          ((> ci -1.0) -3)
          ((> ci -1.382) -4)
          ((<= ci -1.382) -5)
          )
))




(defun rsi-signal (tdate size days )
 (let (cci)
   
   (dotimes (ith days 0)
     (setq cci (rsi (add-mkt-days tdate (- ith)) size))
     (cond ((>= cci 70) (return 1))
           ((<= cci -70) (return -1)) 
           (t 0)))

))

;;;size is the parameter for the RSI
;;;days is the average length for measuring change
;;;returns if the current average rsi is above or below previous number of day's average rsi

(defun rsi2-diff (tdate days)
      (- (rsi2x tdate 2) ;;2 day mean of the RSI2 (2-period rsi)
         (rsi2x (add-mkt-days tdate (- days)) 2))
  )

;;;
(defun rsi-index (date size)
 (let (rs)
   (setq rs (rsi date size))
   (cond ((<= rs 10) -5)
         ((<= rs 20) -4)
         ((<= rs 30) -3)
         ((<= rs 40) -2)
         ((<= rs 50) -1)
         ((<= rs 60) 1)
         ((<= rs 70) 2)
         ((<= rs 80) 3)
         ((<= rs 90) 4)
         ((<= rs 100) 5)
        )
         
))

;;;
(defun rsi-index1 (date size)
 (let (rs)
   (setq rs (rsi date size))
   (cond ((<= rs 20) -2)
         ((<= rs 40) -1)
	 ((<= rs 60) 0)
         ((<= rs 80) 1)
         ((<= rs 100) 2)
          )
         
))
;;;Larry Conners "Short term trading Strategies that work"
(defun rsi2-index (date)
 (let (rs)
   (setq rs (rsi date 2));;;2-period RSI
   (cond ((<= rs 1) -4)
         ((<= rs 2) -3)
         ((<= rs 5) -2)
         ((<= rs 10) -1)
        
         ((<= rs 90) 0)
         ((<= rs 95) 1)
         ((<= rs 98) 2)
         ((<= rs 99) 3)
         ((<= rs 100) 4)
        )         
))
;;;finds the individually high and low over past days
(defun rsi2x-direction (tdate &optional (days 2))
  (let (rsis (date tdate))
  (dotimes (ith  days)
   (push (rsi2x date 2) rsis)
   (setq date (getd date 'ydate)))

  (setq rsis (reverse rsis))
 (values
  (cond ((> (car rsis) (max* (cdr rsis))) 1)
        ((< (car rsis)(min* (cdr rsis))) -1)
        (t (rsi2x-direction (getd tdate 'ydate) days))) 
  (min* rsis)(max* rsis)) 
))
;;;finds the individually high and low over past days
(defun rsi5x-direction (tdate &optional (days 3))
  (let (rsis (date tdate))
  (dotimes (ith  days)
   (push (rsi5x date 1) rsis)
   (setq date (getd date 'ydate)))

  (setq rsis (reverse rsis))
 (values
  (cond ((> (car rsis) (max* (cdr rsis))) 1)
        ((< (car rsis)(min* (cdr rsis))) -1)
        (t (rsi5x-direction (getd tdate 'ydate) days))) 
  (min* rsis)(max* rsis)) 
))

(defun rsi2x-range (tdate days)
 (let (dirp rsi2l rsi2h)
  (multiple-value-setq (dirp rsi2l rsi2h)(rsi2x-direction tdate days))
 (* dirp (- rsi2h rsi2l))
))

;;;averages the rsi2 for X days (by using mean the rsi2 is still on a scale of 0 to 100.
(defun rsi2x (tdate &optional (days 2))
 (let ((rs 0)(date tdate))
   (dotimes (ith days)
     (setq rs (+ rs (rsi date 2)))
     (setq date (getd date 'ydate)))
  (my-round (/ rs days) 1)
))

(defun rsi5x (tdate &optional (days 2))
 (let ((rs 0)(date tdate))
   (dotimes (ith days)
     (setq rs (+ rs (rsi date 5)))
     (setq date (getd date 'ydate)))
  (my-round (/ rs days) 1)
))


;;;;indexes the ave of the rsi2 over days
;;;Larry Conners "Short term trading Strategies that work"
(defun rsi2x-index (tdate &optional (days 2))
 (let ((rsi2 0)(date tdate))
    (setq rsi2 (rsi2x date days))
  
  (cond ((>= rsi2 99) -4)
        ((>= rsi2 95) -3)
        ((>= rsi2 90) -2)
        ((>= rsi2 85) -1)
        ((<= rsi2 1) 4)
        ((<= rsi2 5) 3)
        ((<= rsi2 10) 2)
        ((<= rsi2 15) 1)
        (t 0))
    
))

(defun rsi2x-ob-os (date &optional (days 2))
  (let (rsi2xh rsi2xl rsi2 dir rsi2c rsi2i)
  (multiple-value-setq (dir rsi2xl rsi2xh)(rsi2x-direction date days))
  (setq rsi2 (if (> (abs (- rsi2xh 50))(abs (- 50 rsi2xl)))
                 rsi2xh  rsi2xl));;;determines the most extreme from 50
  (setq rsi2c (rsi date 2))
  (setq rsi2i
  (cond ((>= rsi2 99) -4)
        ((>= rsi2 95) -3)
        ((>= rsi2 90) -2)
        ((>= rsi2 85) -1)
        ((<= rsi2 1) 4)
        ((<= rsi2 5) 3)
        ((<= rsi2 10) 2)
        ((<= rsi2 15) 1)
        (t 0)))

 (if (and (plusp rsi2i) (> rsi2c 50))(setq rsi2i 0))
 (if (and (minusp rsi2i) (< rsi2c 50))(setq rsi2i 0))
rsi2i
))

(defun rsi2xcci5 (date num1 num2)
 (let (dirp rsi2xl rsi2xh cci5h3 cci5l3) 
     (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction date num1))
     (multiple-value-setq (cci5h3 cci5l3) (cci-high-low date num2 num1))
  (cond ((and (> rsi2xh 85)(> cci5h3 100)) -1)
        ((and (< rsi2xl 15)(< cci5l3 -100)) 1)
        (t 0))))


(defun rsi-con-index (date &optional (period 9)(delta 2))
   (let ((rsv (rsi date period)) (rsv1 (rsi (add-mkt-days date (- delta)) period))
         rsvd rsil code rsid)
    (setq rsvd (- rsv rsv1))
    (setq rsil (rsi-index1 date period))    

     (setq rsid
          (cond ;((>= rsvd 12) 3)
                ((> rsvd 8) 1)
               ; ((> rsvd 0) 1)
                ((> rsvd -8) 0)
               ; ((> rsvd -12) -2)
                ((<= rsvd -8) -1)))

    (setq code (+ (* 10 rsil) rsid))

        
))

;;;; 21 63 11 
(defun ep-macd-signal (date period1 period2 period3)
  (let* (macd1 macd2 macd3  (date-1 (add-mkt-days date -1)) indx
	       (date-2 (add-mkt-days date -2)))
       (setq indx (ep-macd-index date-1 period1 period2 period3))
       (setq macd1 (ep-macd date period1 period2 period3) 
             macd2 (ep-macd date-1 period1 period2 period3)
	     macd3 (ep-macd date-2 period1 period2 period3)
       )
     
       (cond ((and (> macd1 macd2)(> macd3 macd2)(<= indx 0)) 'B)
	
	     ((and (< macd1 macd2)(< macd3 macd2)(>= indx 0)) 'S)
     
	     ((and (> macd1 macd2)(>= macd2 macd3) (<= indx 0)) 'UB)
	     ((and (> macd1 macd2)(>= macd2 macd3) (>= indx 0)) 'UA)
	     ((and (< macd1 macd2)(<= macd2 macd3) (>= indx 0)) 'DA)
	     ((and (< macd1 macd2)(<= macd2 macd3) (<= indx 0)) 'DB)
	     (t (ep-macd-signal date-1 period1 period2 period3)))
       
         
))
;;;this is for 6 15 3
(defun macd-index1 (date period1 period2 period3)
  (let (rat (mac (macd date period1 period2 period3))
        (vol (volatility date period2 1)))

   (setq rat (/ mac vol))
   (cond ((< rat -.16) 5)
         ((and (>= rat -.16) (< rat -.12)) 4)
         ((and (>= rat -.12) (< rat -.08)) 3)
         ((and (>= rat -.08) (< rat -.04)) 2)
         ((and (>= rat -.04) (< rat   0)) 1)
         ((and (>= rat 0) (< rat .04)) -1)
         ((and (>= rat .04) (< rat .08)) -2)
         ((and (>= rat .08) (< rat .12)) -3)
         ((and (>= rat .12) (< rat .16)) -4)
         ((>= rat .16) -5))


))
;;;this is for 12 26 9
(defun ep-macd-index (date &optional (period1 21)(period2 63)(period3 11))
  (let (rat (mac (ep-macd date period1 period2 period3))
        ) 

   (setq rat mac)
   (cond ((< rat -.50) -5)
         ((and (>= rat -.50) (< rat -.30)) -4)
         ((and (>= rat -.30) (< rat -.20)) -3)
         ((and (>= rat -.20) (< rat  -.10)) -2)
         ((and (>= rat -.10) (< rat .0)) -1)
         ((and (>= rat .0) (< rat .10)) 1)
         ((and (>= rat .10) (< rat .20)) 2)
         ((and (>= rat .20)(< rat .30)) 3)
         ((and (>= rat .30)(< rat .50)) 4)
         ((>= rat .50) 5))

))

(defun ep-macd (tdate &optional (period1 12) (period2 26) (period3 9))
  (let (macd0 macd1 macd2 vol0 vol1 vol2)
    (multiple-value-setq (macd0 macd1 macd2) (macd tdate period1 period2 period3))
    (setq vol0 (volatility tdate period2 1 'median) vol1 (volatility (add-mkt-days tdate -1) period2 1 'median)
	  vol2 (volatility (add-mkt-days tdate -2) period2 1 'median))

 (values (/ macd0 vol0)(/ macd1 vol1)(/ macd2 vol2))
))
(defun ep-macd-diff (tdate period1 period2 period3)
  (- (ep-macd tdate period1 period2 period3)
     (ep-macd (getd tdate 'ydate) period1 period2 period3)))

(defun ep-macd-diff-index (date &optional (period1 21) (period2 63) (period3 11))
  (let (mac macd0 macd1)
    (multiple-value-setq (macd0 macd1)(ep-macd date period1 period2 period3))
    (setq mac (- macd0 macd1))
    (cond 
          ((> mac .09) 4)
          ((> mac .05) 3)
          ((> mac .02) 2)
          ((> mac 0.00) 1)
          ((= mac 0.00) 0)
          ((> mac -.02) -1)
          ((> mac -.05) -2)
          ((> mac -.09) -3)
          (t -4)
       )
))
#|
;;;;returns the EP-MACD histogram and EP-MACD values
(defun ep-macd (tdate &optional (num1 12)(num2 26)(num3 9))
  (let ((date tdate) osc result diff0 )
  (dotimes (ith (+ 4 num3))
     (setq osc (/ (- (ave-exp date num1 'close) (ave-exp date num2 'close)) (volatility date num2 1)))
     (push osc result)
     (setq date (getd date 'ydate)))
   (setq result (reverse result)); (setq result1 result)
   (setq diff0 (- (first result) (/ (list-sum (subseq result 0 num3)) num3)))
;   (setq diff1 (- (second result)(/ (list-sum (subseq result 1 (1+ num3))) num3)))
;   (setq diff2 (- (third result)(/ (list-sum (subseq result 2 (+ num3 2))) num3)))
;   (setq diff3 (- (fourth result)(/ (list-sum (subseq result 3 (+ num3 3))) num3)))
;   (setq diff4 (- (fifth result)(/ (list-sum (subseq result 4 (+ num3 4))) num3)))
(values diff0 (first result))
))
|#
;;;;the size is for computing the rsi
;;;the days is for the number of days to cumulate      
(defun ep-macd-high-low (stopdate period1 period2 period3 days)
  (let (rsis)
   (dotimes (ith (1+ days))
     (push (ep-macd (add-mkt-days stopdate (- ith)) period1 period2 period3) rsis))
     (values
     (apply #'max rsis) (apply #'min rsis))
))

(defun ep-macd-range (tdate period1 period2 period3 days)
  (let (cci-h cci-l)
   (multiple-value-setq (cci-h cci-l)(ep-macd-high-low tdate period1 period2 period3 days))
  (- cci-h cci-l)
))
(defun ep-macd-range-index (tdate period1 period2 period3 days)
  (let ((mcd (ep-macd-range tdate period1 period2 period3 days)))
    (cond ((> mcd .9) 6)
          ((> mcd .8) 5)
          ((> mcd .6) 4)
          ((> mcd .4) 3)
          ((> mcd .2) 2)
          ((> mcd .1) 1)
          (t 0))
))


(defun macd1 (tdate &optional (num1 21)(num2 63))
    (- (ave-exp tdate num1)(ave-exp tdate num2))
)

(defun ave-macd1 (tdate period1 period2 period3)
   (let ((sum 0)(date (add-mkt-days tdate (1+  (- period3)))))
   (dotimes (ith period3)
      (setq sum (+ (macd1 date period1 period2) sum)
        date (getd date 'ndate)))

   (- (macd1 tdate period1 period2)(float (/ sum period3)))
))

(defun macd1-diff (tdate period1 period2 period3)
  (- (ave-macd1 tdate period1 period2 period3)
     (ave-macd1 (getd tdate 'ydate) period1 period2 period3)))


(defun macd1-index (tdate &optional (num1 11)(num2 22)(num3 2))
  (let ((mcd (macd1 tdate num1 num2))(mcd-1 (macd1 (add-mkt-days tdate (- num3)) num1 num2)))
 (cond ((and (> mcd 0)(> mcd mcd-1)) 3)
       ((and (> mcd 0)(<= mcd mcd-1)) 2)
       ((and (= mcd 0)(> mcd mcd-1)) 1)
       ((and (= mcd 0)(= mcd mcd-1)) 0)
       ((and (= mcd 0)(< mcd mcd-1)) -1)
       ((and (< mcd 0)(>= mcd mcd-1)) -2)
       ((and (< mcd 0)(< mcd mcd-1)) -3)
)))

(defun macd-signal1 (tdate &optional (num1 12)(num2 26)(num3 9))
  (let (mac0 mac1 mac2 mac3 )
  (multiple-value-setq (mac0 mac1 mac2 mac3 ) (macd tdate num1 num2 num3))
  (cond
        ((and (plusp mac0)(> mac0 mac1)
              (< mac0 (max mac1 mac2 mac3 ))) -1);;these are the fake signals
        ((and (minusp mac0)(< mac0 mac1)
              (> mac0 (min mac1 mac2 mac3 ))) 1)

       ((and (plusp mac0)(minusp mac1)
              (minusp mac2)) 2)
        ((and (minusp mac0)(plusp mac1)
              (plusp mac2)) -2)
        ((and (plusp mac1)(minusp mac2)
              (minusp mac3)) 3)
        ((and (minusp mac1)(plusp mac2)
              (plusp mac3)) -3)
        ((> mac0 mac1) 4)
        ((< mac0 mac1) -4)
        (t 0))

  ))

(defun ep-macd-direction (tdate period1 period2 period3 days)
  (let ((epmdc (ep-macd tdate period1 period2 period3))(date tdate) epmdl epmdh)
  
     (multiple-value-setq (epmdh epmdl)(ep-macd-high-low tdate period1 period2 period3 days))
     (cond ((= epmdc epmdl) -1)
           ((= epmdc epmdh) 1)
           (t (ep-macd-direction (getd date 'ydate) period1 period2 period3 days))) 
))

;;;num4 can be 1 to 4
(defun macd-direction (tdate &optional (num1 12)(num2 26)(num3 9)(num4 2))
  (let (mac0 mac1 mac2 mac3 mac4 macs)
  (multiple-value-setq (mac0 mac1 mac2 mac3 mac4) (macd tdate num1 num2 num3))
  (setq macs (list mac1 mac2 mac3 mac4))
  (cond
        ((and (<= mac0 0)(< mac0 (min* (subseq macs 0 num4)))) -2)
        ((and (plusp mac0)(< mac0 (min* (subseq macs 0 num4)))) -1)
        ((and (>= mac0 0)(> mac0 (max* (subseq macs 0 num4)))) 2)
        ((and (minusp mac0)(> mac0 (max* (subseq macs 0 num4)))) 1)
        (t (macd-direction (getd tdate 'ydate) num1 num2 num3 num4)))

   
 ))


(defun aroon-up (tdate days)
   (let ((date tdate) prices highs index )
    (dotimes (ith (1+ days))
      (push (list date (getd date 'high) ith)  prices)
      (setq date (getd date 'ydate)))
 ;  (format T "highs = ~A" prices)
   (setq highs (vsort prices #'> 'cadr))
   (setq index (third (first highs)))
 ;  (format T "sorted = ~A index = ~A" highs index)
  (float (* (/ (- days index) days) 100))
       ))


(defun aroon-down (tdate days)
   (let ((date tdate) prices lows index )
    (dotimes (ith (1+ days))
      (push (list date (getd date 'low) ith)  prices)
      (setq date (getd date 'ydate)))

   (setq lows (vsort prices #'< 'cadr))
   (setq index (third (first lows)))
   (float  (* (/ (- days index) days) 100))
       ))

(defun aroon-index (tdate days)
  (let ((ari (- (aroon-up tdate days)(aroon-down tdate days))))
   (cond ((> ari 90) 3)
         ((> ari 50) 2)
         ((> ari 0) 1)
         ((> ari -50) -1)
         ((> ari -90) -2)
         (t -3))
))

;;;
(defun indicator-percentile-report (tdate markets)
  (let (rocs path1 num date ); r-squared );forecast )
       (maind-x)(set-cat-list)
      (dolist (market markets)
       (set-market market)(setq date tdate rocs nil)
       (setq path1 (string-append *output-upper-dir* (format nil "~A-" market) "percentiles.dat"))
      ; (setq num (cdr (assoc market *market-days-available*)))  
       (setq num (available-days market tdate 500))
     ;   (setq num 1000)

       (dotimes (ith num)
         
   ;      (setq date-1 (add-mkt-days date -1))
       ;  (setq r-squared (nth-value 2 (lregress date 5)))
          (push 
          ;  (- (float (/ (macd date 12 26 9)(volatility date 26))))
   
          (* 100 (- (/ (getd date 'close) (getd (add-mkt-days date -14) 'close)) 1))
          ;  (/ (volatility date 4 1)(volatility date 63 1));;feature 2   
          
            ; (slow-stochastic date 20)    
              rocs)
         (setq date (getd date 'ydate)))
   
  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "per%  value ~%~%")
    (format stream "~D  ~D~%" 99 (my-round (percentile .99 rocs) 3))
    (format stream "~D  ~D~%" 95 (my-round (percentile .95 rocs) 3))
    (format stream "~D  ~D~%" 90 (my-round (percentile .90 rocs) 3))
    (format stream "~D  ~D~%" 80 (my-round (percentile .80 rocs) 3))
    (format stream "~D  ~D~%" 70 (my-round (percentile .70 rocs) 3))
    (format stream "~D  ~D~%" 60 (my-round (percentile .60 rocs) 3))
    (format stream "~D  ~D~%" 50 (my-round (percentile .50 rocs) 3))
    (format stream "~D  ~D~%" 40 (my-round (percentile .40 rocs) 3))
    (format stream "~D  ~D~%" 30 (my-round (percentile .30 rocs) 3))
    (format stream "~D  ~D~%" 20 (my-round (percentile .20 rocs) 3))
    (format stream "~D  ~D~%" 10 (my-round (percentile .10 rocs) 3))
    (format stream "~D  ~D~%" 05 (my-round (percentile .05 rocs) 3))
    (format stream "~D  ~D~%~%" 01 (my-round (percentile .01 rocs) 3))

   (format stream "HIGHEST = ~D  LOWEST = ~D~%" (my-round (max* rocs) 3) (my-round (min* rocs) 3))
 ;  (format stream "AVERAGE = ~D~%" (round (/ (list-sum rocs)(length rocs))))
   (format stream "NUM DAYS = ~A~%" (length rocs))
  
))))

#|
;;;
(defun indicator-percent-report ()
  (let (rocs path1 results date-1);  r-squared forecast )
       (maind-x)(set-cat-list)
      
       (setq path1 (string-append *output-upper-dir* (format nil "~A-" market) "percent.dat"))
    
      (dolist (ith daytrades)
          (set-market (svref ith 0))
          (setq date-1 (getd (svref ith 1) 'ydate))
   
        ; (multiple-value-setq (forecast r-squared)(lregress date 5))
          (setq rocs        
               (acons
                      (gann-slope-index (getd (svref ith 1) 'ydate) 5 84)
                      (svref ith 19)           
              rocs))
         )
    (vsort rocs #'< 'car)
    (setq results (my-bin-and-count rocs))


  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "Percentile upper-per  Ave P&L ~%~%")
    (dolist (kth results)   

        (format stream "~@D     ~@D  ~4,2F~%" (car kth) (second kth)(third kth))
  
 ;  (format stream "HIGHEST = ~D  LOWEST = ~D~%" (my-round (max* rocs) 3) (my-round (min* rocs) 3))
 ;  (format stream "AVERAGE = ~D~%" (round (/ (list-sum rocs)(length rocs))))
          (format stream "NUM TRADES = ~A~%" (length rocs)))
  
)))

;;;rocs already sorted based on 'car and is a a-list
(defun my-bin-and-count (rocs)
   (let (results bin lower-per upper-per (ind-list (mapcar #'lambda(s) (car s) rocs)))
    
   (dotimes (kth 10)
     (setq lower-per (percentile2 ind-list (* kth 10))
          upper-per (percentile2 ind-list (* (1+ kth) 10)))
     
    (setq bin (list-sum (mapcar #'(lambda(y) (cdr y))
                (remove-if #'(lambda(s) (or (< (cdr s) lower-per)(> (cdr s) upper-per))) rocs))))
    (setq results (list kth upper-per bin))
    )
    results
))

|#
#|
;;;this is to test alternation and is unfinished
(defun ob-os-order (tdate markets)
  (let (rocs  path1 num date mac obos prev-mac); date-2)
       (maind-x)(set-cat-list)
      (dolist (market markets)
       (set-market market)(setq date tdate rocs nil)
       (setq path1 (string-append *output-upper-dir* (format nil "~A-" market) "ob-os.dat"))
       (setq num (available-days market tdate))   
    
        (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
       (dotimes (ith num)
         (setq mac (macd-index date 12 26 9))
          (when (and (member mac '(5 -5))(/= mac prev-mac))
           (push  mac rocs)
           (format str "~A ~A~%" date mac)
            )
         
         (setq prev-mac mac date (getd date 'ydate)))))
   
        (do ((jth (car rocs) (car macds))
            (kth (cadr rocs)(cadr macds))
            (macds  rocs (cdr macds))) 
           ((not kth))
          (if (= jth kth) (push 0 obos)(push 1 obos))) 

        (float (/ (apply #'+ obos) (length obos)))
))
|#

 (defun day-bar-type (date)
   (let* ((open (getd date 'open))(low (getd date 'low))(close (getd date 'close))
          (range (true-range date)) (vol14 (volatility date 14 1)))
     
    (* (if (> range vol14) 1 -1)
     (cond ((and (< open (+ low (* .3334 range)))
                 (< close (+ low (* .3334 range)))) 11)
           ((and (< open (+ low (* .3334 range)))
                 (< close (+ low (* .6667 range)))
                 (>= close (+ low (* .3334 range)))) 12)
           ((and (< open (+ low (* .3334 range)))
                 (>= close (+ low (* .6667 range)))) 13)
           ((and (< open (+ low (* .6667 range)))
                 (>= open (+ low (* .3334 range)))
                 (< close (+ low (* .3334 range)))) 21)
           ((and (< open (+ low (* .6667 range)))
                 (>= open (+ low (* .3334 range)))
                 (< close (+ low (* .6667 range)))
                 (>= close (+ low (* .3334 range)))) 22)
           ((and (< open (+ low (* .6667 range)))
                 (>= open (+ low (* .3334 range)))
                 (>= close (+ low (* .6667 range)))) 23)
           ((and (>= open (+ low (* .6667 range)))
                 (< close (+ low (* .3334 range)))) 31)
           ((and (>= open (+ low (* .6667 range)))
                 (< close (+ low (* .6667 range)))
                 (>= close (+ low (* .3334 range)))) 32)
           ((and (>= open (+ low (* .6667 range)))
                 (>= close (+ low (* .6667 range)))) 33)
	   (t 0)
                 )))             )


(defun day-bar-type-index (date)
 (let ((can (day-bar-type date))
       (bull '(31 23 32 13 33 22));; 
       (bear '(21 12 13 11 31 22));; 
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))



 (defun day-bar-type2 (date)
   (let* ((high (getd date 'high))(low (getd date 'low))(close (getd date 'close))
         (range (- high low)))

   (cond  ((< close (+ low (* .1 range))) -5)
          ((< close (+ low (* .2 range))) -4)
          ((< close (+ low (* .3 range))) -3)
          ((< close (+ low (* .4 range))) -2)
          ((< close (+ low (* .5 range))) -1)
          ((< close (+ low (* .6 range))) 1)
          ((< close (+ low (* .7 range))) 2)
          ((< close (+ low (* .8 range))) 3)
          ((< close (+ low (* .9 range))) 4)
          (t 5)
                 )
  ))

(defun buying-pressure (tdate)
  (my-round (if (zerop (true-range tdate)) 0
                (* 100 (/ (- (getd tdate 'close)
                             (min (getd tdate 'low)(getd (getd tdate 'ydate) 'close)))
                           (true-range tdate)))) 1)
)
;;;for buying long
(defun buying-pressure1 (tdate)
   (let ((close0 (getd tdate 'close)) (open0 (getd tdate 'open))(high0 (getd tdate 'high))
       (low0 (getd tdate 'low)) pressure0 entry0 exit0)
  (if (> close0 open0);;;close above open
      (setq entry0 low0 exit0 high0 pressure0 (- exit0 entry0))
   ;;;close below open
      (if (> (- high0 open0)(- close0 low0))
       (setq entry0 open0 exit0 high0 pressure0 (- exit0 entry0))
       (setq entry0 low0 exit0 close0 pressure0 (- exit0 entry0))))
(values pressure0 entry0 exit0)
))

(defun selling-pressure (tdate)
   (my-round (if (zerop (true-range tdate)) 0
                 (* 100  (/ (- (max (getd tdate 'high)(getd (getd tdate 'ydate) 'close))
                               (getd tdate 'close))
                         (true-range tdate)))) 1)
)
;;;For short selling
(defun selling-pressure1 (tdate)
   (let ((close0 (getd tdate 'close)) (open0 (getd tdate 'open))(high0 (getd tdate 'high))
       (low0 (getd tdate 'low)) pressure0 entry0 exit0)
  (if (< close0 open0);;;close below open
      (setq entry0 high0 exit0 low0 pressure0 (- entry0 exit0))
   ;;;close above open
      (if (> (- open0 low0)(- high0 close0))
       (setq entry0 open0 exit0 low0 pressure0 (- entry0 exit0))
       (setq entry0 high0 exit0 close0 pressure0 (- entry0 exit0))))
(values pressure0 entry0 exit0)
))

(defun selling-pressure-extreme (tdate &optional (days 3))
  (let (sps (date tdate))
    (dotimes (ith days)
      (push (selling-pressure date) sps)
      (setq date (getd date 'ydate)))
    (min* sps)
))

(defun buying-pressure-extreme (tdate &optional (days 3))
  (let (sps (date tdate))
    (dotimes (ith days)
      (push (buying-pressure date) sps)
      (setq date (getd date 'ydate)))
    (min* sps)
))
    

 (defun day-bar-type2n (date days)
   (let* ((high (n-day-high date days))(low (n-day-low date days))(close (getd date 'close))
         (range (- high low)))

   (cond  ((< close (+ low (* .10 range))) 0)
          ((< close (+ low (* .20 range))) 1)
          ((< close (+ low (* .40 range))) 2)
          ((< close (+ low (* .60 range))) 3)
          ((< close (+ low (* .80 range))) 4)
          ((< close (+ low (* .90 range))) 5)
          ((>= close (+ low (* .90 range))) 6)
                 )
  ))


 (defun day-bar-type2o (date)
   (let* ((high (getd date 'high))(low (getd date 'low))(open (getd date 'open))
         (range (- high low)))

   (cond ((< open (+ low (* .20 range))) 1)
           ((< open (+ low (* .40 range))) 2)
           ((< open (+ low (* .60 range))) 3)
           ((< open (+ low (* .80 range))) 4)
           ((>= open (+ low (* .800 range))) 5)
                 )
  ))


;;;a bar may be characterised as outside, inside, or trending

(defun day-bar-type1 (date)
    (let* ((high0 (getd date 'high))(low0 (getd date 'low))(close0 (getd date 'close))
           (ydate (getd date 'ydate)) (low1 (getd ydate 'low))(high1 (getd ydate 'high))
           (open0 (getd date 'open))(rg0 (true-range date))(rg1 (true-range (getd date 'ydate))))

     (cond ((and (<= high0 high1)(>= low0 low1)) 'IN)
           ((and (> high0 high1)(< low0 low1)(> close0 open0)) 'OU)
           ((and (> high0 high1) (< low0 low1)(<= close0 open0)) 'OD)
           ((and (> high0 high1)(> low0 low1)(> rg0 rg1)) 'UL)
           ((and (> high0 high1)(> low0 low1)(<= rg0 rg1)) 'US)

           ((and (< high0 high1)(< low0 low1)(> rg0 rg1)) 'DL)
           ((and (< high0 high1)(< low0 low1)(<= rg0 rg1)) 'DS)
          (t 'FT))
))


;;;returns either 'UP 'DN or nil
(defun reversal-dayp (date)
 (let ((high0 (getd date 'high))(low0 (getd date 'low))(lvlh 180)(lvll -180)
       (close0 (getd date 'close))(low1 (getd (getd date 'ydate) 'low))
       (close1 (getd (getd date 'ydate) 'close))(high1 (getd (getd date 'ydate) 'high))
       (cci21 (commodity-channel-index (getd date 'ydate) 21))
       )
       (when (getd date 'rollover)
             (setq close1 (+ close1 (getd date 'rollover)) high1 (+ high1 (getd date 'rollover))
                   low1 (+ low1 (getd date 'rollover))))

       (cond ((and (>= cci21 lvlh)(> high0 high1) (< close0 close1)) -1)
             ((and (<= cci21 lvll)(< low0 low1)(> close0 close1))  1)
             ((and (<= cci21 lvll)
                   (<= close1 (+ low1 (* .1 (- high1 low1))));;;closes in lower 10% of range on day 1
                   (>= close0 (+ low0 (* .9 (- high0 low0))))) 2);;;closes in upper 10% of range on day 0
             ((and (>= cci21 lvlh)
                   (>= close1 (+ low1 (* .9 (- high1 low1))))
                   (<= close0 (+ low0 (* .1 (- high0 low0))))) -2)
             (t 0))

        ))

(defun reversal-dayp-composite (tdate days)
  (let (rev (date tdate))
  (dotimes (ith days)
   (setq rev (reversal-dayp date))
   (if (/= rev 0)(return)(setq date (getd date 'ydate)))
)
  rev
))




;;;if market closes near high of day and is at 11 day high then probably a top the next day
(defun wpp1 (tdate)
   (let ((date-1 (getd tdate 'ydate))(volave (volatility tdate 63 1))
          (asi (ww-asi-trend tdate 3)))
        
   (cond ((and (>= (buying-pressure date-1) 76.4)
               (or (n-day-highp date-1 5)(n-day-highp tdate 5))
               (or (> (true-range date-1)(true-range (getd date-1 'ydate)))
                   (> (true-range date-1) volave))
               (< (getd tdate 'close) (getd date-1 'high))
               (> (body-range date-1) 0)
               (<= (body-range tdate) 0)
               (plusp asi)) -1)
         ((and (>= (selling-pressure date-1) 76.4)
               (or (n-day-lowp date-1 5)(n-day-lowp tdate 5))
               (or (> (true-range date-1)(true-range (getd date-1 'ydate)))
                   (> (true-range date-1) volave))
               (> (getd tdate 'close) (getd date-1 'low))
               (< (body-range date-1) 0)
               (>= (body-range tdate) 0)
               (minusp asi)) 1)
          ((and (>= (buying-pressure date-1) 76.4)
               (or (n-day-highp date-1 5)(n-day-highp tdate 5))
               (minusp (reversal-dayp tdate) )) -1)
          ((and (>= (selling-pressure date-1) 76.4)
               (or (n-day-lowp date-1 5)(n-day-lowp tdate 5))
               (plusp (reversal-dayp tdate) )) 1)
         
         (t 0))
))

(defun wpp1-composite (tdate days)
   (let ((wp 0)(date tdate))
  (dotimes (ith days)
    (setq wp (wpp1 date))
     (cond ((and (< date tdate)(= wp -1)
                (> (getd tdate 'high)(n-day-high (getd tdate 'ydate) days))
                )
           (setq wp 0))
          ((and (< date tdate)(= wp 1)
                (< (getd tdate 'low)(n-day-low (getd tdate 'ydate) days))
                )
            (setq wp 0)))

    (if (not (zerop wp)) (return wp) (setq date (getd date 'ydate))))
   wp 
))

(defun wpp2 (tdate)
   (let ((date-1 (getd tdate 'ydate)))
        
   (cond ((and (>= (buying-pressure date-1) 76.4)
               (or (n-day-highp date-1 5)(n-day-highp tdate 5))
               (minusp (reversal-dayp tdate))) -1)
         ((and (>= (selling-pressure date-1) 76.4)
               (or (n-day-lowp date-1 5)(n-day-lowp tdate 5))
               (plusp (reversal-dayp tdate))) 1)
         (t 0))
))
                        

;;;characterizes the current day
;;;First letter = by inside, outside, upward or downward compared to prior day
;;;Second letter = and by direction of close to prior close
;;;third letter = close compared to open or compared to 7 day high or low
(defun wpp (tdate)
  (let* ((high0 (getd tdate 'high))(low0 (getd tdate 'low))(close0 (getd tdate 'close))
         (open0 (getd tdate 'open))(ndhigh0 (n-day-highp tdate 7))(ndlow0 (n-day-lowp tdate 7))
         (body0 (plusp (- close0 open0)))
         (ydate (getd tdate 'ydate))
         (low1 (getd ydate 'low))(high1 (getd ydate 'high))(close1 (getd ydate 'close))
         (ndhigh1 (n-day-highp ydate 7))(ndlow1 (n-day-lowp ydate 7))(odc (plusp (- close0 close1)))
         (inside (and (<= high0 high1)(>= low0 low1)))
         (outside (and (> high0 high1)(< low0 low1)))
         (upward (and (> high0 high1)(>= low0 low1)))
         (downward (and (<= high0 high1)(< low0 low1)))
         )
      (cond ((and inside odc ndhigh1) 'IUH);;;inside day following 7 day high
            ((and inside odc ndlow1) 'IUL);;;inside day following 7 day low
            ((and inside odc) 'IUF)
            
            ((and inside (not odc) ndhigh1) 'IDH)
            ((and inside (not odc) ndlow1) 'IDL)
            ((and inside (not odc)) 'IDF)


            ((and outside odc ndhigh0) 'OUH)
            ((and outside odc ndlow0) 'OUL)
            ((and outside odc) 'OUF)
            
            ((and outside (not odc) ndhigh0) 'ODH)
            ((and outside (not odc) ndlow0) 'ODL)
            ((and outside (not odc)) 'ODF)

            ((and upward odc body0 ndhigh0) 'UUU7)
            ((and upward odc body0) 'UUU)
            ((and upward odc (not body0)) 'UUD)

            ((and upward (not odc) body0) 'UDU)
            ((and upward (not odc) (not body0)) 'UDD)

            ((and downward (not odc) (not body0) ndlow0) 'DDD7)
            ((and downward (not odc) body0) 'DDU)
            ((and downward (not odc) (not body0)) 'DDD)

            ((and downward odc body0) 'DUU)
            ((and downward odc (not body0)) 'DUD)

;;;;these are the Williams smash days
            ((and (> close0 high1) (eql high0 (n-day-high tdate 4))) 'UP1)
            ((and (< close0 low1) (eql low0 (n-day-low tdate 4))) 'DN1)
            ((and (< close0 close1) (> close0 open0) ;;Down day white body
              (>= close0 (- high0 (* .25 (true-range tdate))))) 'UP2);;;closes in the top 75% of range
            ((and (> close0 close1)(< close0 open0) ;;;Up day with black body
              (<= close0 (+ low0 (* .25 (true-range tdate))))) 'DN2);;;closes in the bottem 25% of range
            (t (candle1 tdate)))
          

))

;;;;this is for FORE
;;;direction is either 'long or 'short
(defun wpp-index0 (date)
 (let ((wp (wpp date)))
       
      (cond ((member wp '(DDU UDD  IDL OUH IDH IDF ODH OUF IUF)) 1)
           
            ((member wp '(DUD ODF OUL DUU UUU IUH UUD ODL DDD7)) -1)
          
            (t 0))
))


(defun wpp-composite (tdate days)
  (let (wp (date tdate) rtn)
    (setq rtn
    (dotimes (ith days)
      (setq wp (wpp date))
      (cond ((member wp '(DDD DUU OUL DDU ODL))(return '1))
            ((member wp '(UUU UDD ODH UUD OUH))(return '-1))
            (t (setq date (getd date 'ydate) rtn  0)))
     ))
 (if rtn rtn 0)
   ))
 
;;;;this is for Triumph21 and Triumph11
(defun wpp-index (date)
 (let ((can (wpp date))
       (bull '(DDU UDD OUH IDL OUF IUF DDD7))
       (bear '(UDU IUH ODL ODF IUL ODH IDH ))
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))
;;;;this is for Triumph21 and Triumph11
(defun wpp-index-nk (date)
 (let ((can (wpp date))
       (bull '(OUL ODL IUL IDF UDD ODF ODH))
       (bear '(IUL UDU OUL IUH DUU DDD DDD7))
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))
;;;;this is for Triumph21 and Triumph11
(defun wpp-index-nd (date)
 (let ((can (wpp date))
       (bull '(DDU DUU IDF IUH ODF ODH IDH))
       (bear '(IDH DDU DUD UUU7 UDU OUF ODH))
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))
(defun wpp-index-indexes (date)
 (let ((can (wpp date))
       (bull '(OUL IUL ODL DUU ODF IUH IDL))
       (bear '(OUL OUF IUL ODH UUU7 DUU UDU))
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))



(defun wpp-index1 (date)
 (let ( (can (wpp date))
       
         (bull '(UDU ODH IDF IUF DUU UDD UUD IDH IDL IUL))
     
      ;  (bear '(UPMS UPMB DNHM UPDJ IDST UPTW ));; for 2c
        (bear '(DUD IDF IUH ODF OUF IDL IDH IUL IUF UUD OUL DDD DUU DDU))
      
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))


(defun wpp-index2 (date)
 (let ( (can (wpp date))
       
         (bull '(OUL OUH ODL OUF IUL IDF IUF UDU ODF ODH DUU IDL))
     
      ;  (bear '(UPMS UPMB DNHM UPDJ IDST UPTW ));; for 2c
        (bear '(IDH IDF IUH UUD IDL IUF ODF UDD DDU ))
      
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))

;;;for current day
(defun wppfx-index0 (date)
 (let ((can (wpp date))
       (bull '(IDF IUH IDL IUF IDH UDD ODL DDD OUH))
       (bear '(ODF OUL IUL DUU IUH UUD DDU UDU UUU))
        )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))

;;;for 1 day back
(defun wppfx-index1 (date)
 (let ((can (wpp date))
       (bull '(ODF IDF IDL DDU OUH UUD IDH IUF UDU ))
       (bear '(ODH UDU DUD UDD ODL DUU IUF IDH IDL))
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))

;;;for 2 days back
(defun wppfx-index2 (date)
 (let ((can (wpp date))
       (bull '(UUD OUL UDU DUD IUF IUH IDL IDF DUU ODH UUU))
       (bear '(IDL IDF IUH DUU OUF OUL IUF IUL UDU UDD DDU ODF))
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))
;;; for swing forex
(defun wpp-swing (date)
 (let ((can (wpp date))
       (bull '(DDD DUU OUL))
       (bear '(UUU UDD ODH))
       )
;;;bull is associated with bottoms
;;;bear is associated with tops
   (cond ;((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))
;;;if an island 
;;;returns days ( 1 2 or 3 ) at top or bottom
;;;reuturns negative for island tops and positive for bottoms

(defun island (tdate)
  (let ((gap 0) (island-signal 0))

   (if (< (getd tdate 'high)(getd (getd tdate 'ydate) 'low)) (setq gap -1))
   (if (> (getd tdate 'low)(getd (getd tdate 'ydate) 'high))(setq gap 1) )
  (if (zerop gap) (setq island-signal 0))

  
   (when (= gap -1)
     (setq island-signal
     (dotimes (ith 3 0)
       (if (> (getd (add-mkt-days tdate (- (+ ith 1))) 'low)
              (getd (add-mkt-days tdate (- (+ ith 2))) 'high))
        (return (- (+ ith 1)))))))

   (when (= gap 1)
      (setq island-signal
      (dotimes (ith 3 0)
        (if (< (getd (add-mkt-days tdate (- (+ ith 1))) 'high)
              (getd (add-mkt-days tdate (- (+ ith 2))) 'low))
        (return (+ ith 1))))))

island-signal
))

(defun find-island (market date days)
  (set-market market)
  (dotimes (ith days)
    (if (/= (island date) 0)
        (format T "~%date = ~A Size= ~A" date (island date)))
    (setq date (getd date 'ydate))))

(defun ave-range% (tdate days)
   (let ((date tdate) ranges)
   (dotimes (ith days)   
       (push (/ (true-range date) (getd date 'close)) ranges)
     (setq date (getd date 'ydate)))
     (float (/ (apply #'+ ranges) days))
))

;;;this is the midpoint of the 9 day range (aka conversion line)
(defun tenkan-sen (date &optional (days 9))
   (/ (+ (n-day-high date days) (n-day-low date days)) 2.0))

;;;this is the midpoint of the 26 day range (aka baseline)
(defun kijun-sen (date)
  (/ (+ (n-day-high date 26)(n-day-low date 26)) 2.0))

(defun tkcd (date)
   (/ (- (tenkan-sen date)(kijun-sen date)) (volatility date 52 1)))

(defun tkcd-index (date)
   (let ((rat (tkcd date)))
    (cond ((< rat -2.0) -5)
         ((and (>= rat -2.0) (< rat -1.5)) -4)
         ((and (>= rat -1.5) (< rat -1.0)) -3)
         ((and (>= rat -1.0) (< rat  -.5)) -2)
         ((and (>= rat -.5) (< rat .0)) -1)
         ((and (>= rat .00) (< rat .5)) 1)
         ((and (>= rat .5) (< rat 1.0)) 2)
         ((and (>= rat 1.0)(< rat 1.5)) 3)
         ((and (>= rat 1.5)(< rat 2.0)) 4)
         ((>= rat 2.0) 5))
))

(defun tk (tdate)
  (let ((cl (getd tdate 'close))(ten (tenkan-sen tdate))(kij (kijun-sen tdate)))
   (cond ((and (> cl ten)(> cl kij)) 2)
         ((and (> cl ten)(<= cl kij)) 1)
         ((and (<= cl ten)(> cl kij)) -1)
         ((and (<= cl ten)(<= cl kij)) -2)
         (t 0))
))
(defun swing-signal (tdate)
   (let ((tk-sen (tk tdate))(pb (parabolic-stops tdate)))
    (cond ((and (= tk-sen 2)(eql pb 'long)) 4)
          ((and (= tk-sen 1)(eql pb 'long)) 3)
          ((and (= tk-sen -1)(eql pb 'long)) 2)
          ((and (= tk-sen -2)(eql pb 'long)) 1)
          ((and (= tk-sen 2)(eql pb 'short)) -1)
          ((and (= tk-sen 1)(eql pb 'short)) -2)
          ((and (= tk-sen -1)(eql pb 'short)) -3)
          ((and (= tk-sen -2)(eql pb 'short)) -4)
          (t 0))
))

;;;;this is the faster cloud boundary (aka leading span A)
;;;;it is plotted 26 days ahead
(defun senkou-span-a (date)
    (/ (+ (tenkan-sen (add-mkt-days date -26)) (kijun-sen (add-mkt-days date -26))) 2.0))

;;;this is the slower cloud boundary (aka leading span B)
;;;;it is plotted 26 days ahead
(defun senkou-span-b (date)
    (/ (+ (n-day-high (add-mkt-days date -26)  52) (n-day-low (add-mkt-days date -26) 52)) 2.0))

(defun ichimoku-bias (tdate)
  (let ((ssa (senkou-span-a tdate))(ssb (senkou-span-b tdate)) (tclose (getd tdate 'close)))
   (cond ((> tclose (max ssa ssb)) 'bullish)
         ((< tclose (min ssa ssb)) 'bearish)
         (t 'neutral))
))
 ;;;the cloud width may be negative or positive
(defun cloud-width (tdate)
   (/ (- (senkou-span-a tdate)(senkou-span-b tdate)) (volatility tdate 52 1)))

(defun cloud-index (tdate)
 (let ((ca (cloud-width tdate)))
   (cond ((> ca 3.0) 4)
         ((> ca 2.0) 3)
         ((> ca 1.0) 2)
         ((> ca 0) 1)
         ((> ca -1) -1)
         ((> ca -2) -2)
         ((> ca -3) -3)
         ((<= ca -3) -4))))

(defun ichimoku-signal1 (tdate)
    (let (condition (date tdate) (fdate (first-available-date *data-name*))
         (tclose (getd tdate 'close)))
 
     (when (and (eql (ichimoku-bias tdate) 'bullish) ;;;trading bias bullish   
               (> tclose (tenkan-sen tdate)))

        (setq condition
         (loop
           (if (< date fdate)(return nil))
           (if (eql (ichimoku-bias date) 'bearish)    
               (return nil))
           (if (< (getd date 'close) (kijun-sen date))(return 'buy))
           (setq date (getd date 'ydate)))))
      
     
      (setq date tdate)
      (when (and (eql (ichimoku-bias date) 'bearish) ;;;trading bias bearish   
               (< tclose (tenkan-sen tdate)))

        (setq condition
          (loop
             (if (< date fdate)(return nil))
             (if (eql (ichimoku-bias date) 'bullish) ;;;trading bias bullish
               (return nil))   
             (if (>  (getd date 'close) (kijun-sen date))(return 'sell))
             (setq date (getd date 'ydate)))))
      
condition
))

(defun latest-ichimoku-signal1 (tdate)
  (let (condition (date tdate) (counter 0)(fdate (first-available-date *data-name*)))
   (loop
     (if (< date fdate)(return (setq condition (ichimoku-signal2 tdate))))
     (setq condition (ichimoku-signal1 date))
     (if condition (return condition) (setq date (getd date 'ydate) counter (1+ counter))))
   (values condition counter (add-mkt-days tdate (- counter)))
))

(defun ichimoku-signal2 (tdate)
  (let (condition); (tclose (getd tdate 'close)))
      (when (and (eql (ichimoku-bias tdate) 'bullish) ;;;trading bias bullish   
                ; (> tclose (tenkan-sen tdate));;; closes above blue line
                 (> (tenkan-sen tdate)(kijun-sen tdate)));;;blue above red
           (setq condition 'buy))
      (when (and (eql (ichimoku-bias tdate) 'bearish) ;;;trading bias bearish   
                ; (< tclose (tenkan-sen tdate));;;closes below the blue line
                 (< (tenkan-sen tdate)(kijun-sen tdate)));;;blue below red           
              (setq condition 'sell))
condition
))  

(defun tsen (tdate)
  (let* ((cl (getd tdate 'close)) (fast9 (tenkan-sen tdate))
         (delta-tsen (- fast9 (tenkan-sen (getd tdate 'ydate))))
         (delta-close (- cl fast9)))
      (cond ((plusp delta-tsen) (setq delta-tsen 1))
            ((minusp delta-tsen)(setq delta-tsen -1))
            ((zerop delta-tsen)(setq delta-tsen 0)))
      (cond ((plusp delta-close) (setq delta-close 1))
            ((minusp delta-close)(setq delta-close -1))
            ((zerop delta-close)(setq delta-close 0)))

 
       (cond ((and (= delta-close 1) (= delta-tsen 1)) 4)
             ((and (= delta-close 0)(= delta-tsen 1)) 3)
             ((and (= delta-close -1)(= delta-tsen 1)) 2)
             ((and (= delta-close 1)(= delta-tsen 0)) 1)
             ((and (= delta-close 0)(= delta-tsen 0)) 0)
             ((and (= delta-close -1)(= delta-tsen 0)) -1)
             ((and (= delta-close 1)(= delta-tsen -1)) -2)
             ((and (= delta-close 0)(= delta-tsen -1)) -3)
             ((and (= delta-close -1)(= delta-tsen -1)) -4)
             )
   ))

(defun commodity-channel-index (tdate &optional (size 20))
  (let* ((md 0)(av (ave tdate size 'pivot)))

;;;compute the mean deviation
  (dotimes (ith size)
;   (format T "~%~A ~A ~A ~A ~A" md av  (ave (add-mkt-days tdate (- ith)) 1 'pivot) ith 
 ;          (add-mkt-days tdate (- ith)))   
    (setq md (+ (abs (- (ave (add-mkt-days tdate (- ith)) 1 'pivot)  av)) md)))
  (setq md (/ md size))
  
 (if (zerop md) 0 (my-round (/ (- (ave tdate 1 'pivot) av)  (* .015 md)) 2))

 ))

;;;;the size is for computing the cci
;;;the days is for the number of days to cumulate      
(defun cci-high-low (stopdate size days)
  (let (rsis)
   (dotimes (ith days)
     (push (commodity-channel-index (add-mkt-days stopdate (- ith)) size) rsis))
;;;which came most recent the low or the high?
   

     (values
     (apply #'max rsis) (apply #'min rsis))
))

(defun ccix-ob-os (date size days)
  (let ( cci5h3 cci5l3 cci ccii ccic)
  (multiple-value-setq (cci5h3 cci5l3)(cci-high-low date size days))
  (if (> (abs cci5h3)(abs cci5l3)) (setq cci cci5h3 )(setq cci cci5l3))
  (setq ccic (commodity-channel-index date size))
  (setq ccii 
        (cond ((< cci -300) -7)
              ((< cci -250) -6)
              ((< cci -200) -5)
              ((< cci -150) -4)
              ((< cci -100) -3)
              ((< cci -50) -2)
              ((< cci 0) -1)
              ((< cci 50) 1)
              ((< cci 100) 2)
              ((< cci 150) 3)
              ((< cci 200) 4)
              ((< cci 250) 5)
              ((< cci 300) 6)
              ((>= cci 300) 7)))

 (if (and (< ccii 0)(> ccic 50)) (setq ccii 0))
 (if (and (> ccii 0)(< ccic -50)) (setq ccii 0))
ccii

))

(defun cci-range (tdate size days)
   (let (cci-h cci-l)
   (multiple-value-setq (cci-h cci-l)(cci-high-low tdate size days))
  (- cci-h cci-l)
 ))


(defun cci-range-index (tdate size days)
   (let ((cci (cci-range tdate size days)))
    ;  (truncate cci 100) 
     (cond ((> cci 350) 7)
              ((> cci 275) 6)
              ((> cci 200) 5)
              ((> cci 150) 4)
              ((> cci 100) 3)
              ((> cci 75) 2)
              ((> cci 50) 1)
              (t 0))
            
))
;;;;1 means reversal 
(defun cci-range-index-swing (tdate)
  (let ((cci (cci-range-index tdate 21 11))(dir (parabolic-stops tdate)))
  (* (if (eql dir 'SHORT) 1 -1)
     (case cci
        ((0 1 2) 1)
        ((3 4 5 6) 0)
        (7 -1)
        (otherwise 0)))
))


(defun cci-high-index (tdate size days)
   (let ((cci (cci-high-low tdate size days)))
        (cond ((> cci 400) 5)
              ((> cci 300) 4)
              ((> cci 200) 3)
              ((> cci 100) 2)
              ((> cci 0) 1)
              ((> cci -100) -1)
              ((> cci -200) -2)
              ((> cci -300) -3)
              (t -4))
            
))

(defun cci-low-index (tdate size days)
   (let ((cci (nth-value 1 (cci-high-low tdate size days))))
        (cond ((< cci -400) -5)
              ((< cci -300) -4)
              ((< cci -200) -3)
              ((< cci -100) -2)
              ((< cci 0) -1)
              ((< cci 100) 1)
              ((< cci 200) 2)
              ((< cci 300) 3)
              (t 4))
            
))

(defun cci-extreme-index (tdate size days)
  (let ((dir (parabolic-stops tdate)))
    (if (eql dir 'LONG)(cci-high-index tdate size days)(cci-low-index tdate size days))))

;;;num4 can be 1 or more.(days includes the current day.
(defun cci-direction (tdate &optional (num1 20)(num4 2))
  (let ((date tdate) ccis )
   
  (dotimes (ith (1+ num4))
   
   (push (commodity-channel-index date num1) ccis)
   (setq date (getd date 'ydate))
  )
  (setq ccis (reverse ccis))
 ;(format T "ccis = ~A~%" ccis)
  (cond
        ((and (<= (car ccis) -250)(< (car ccis) (min* (cdr ccis)))) -7)
        ((and (<= (car ccis) -180)(< (car ccis) (min* (cdr ccis)))) -6)
        ((and (<= (car ccis) -135)(< (car ccis) (min* (cdr ccis)))) -5)
        ((and (<= (car ccis) -90)(< (car ccis) (min* (cdr ccis)))) -4)
        ((and (<= (car ccis) -45)(< (car ccis) (min* (cdr ccis)))) -3)
        ((and (<= (car ccis) 0)(< (car ccis) (min* (cdr ccis)))) -2)
        ((and (plusp (car ccis))(< (car ccis) (min* (cdr ccis)))) -1)
        ((and (>= (car ccis) 250)(> (car ccis) (max* (cdr ccis)))) 7)
        ((and (>= (car ccis) 180)(> (car ccis) (max* (cdr ccis)))) 6)
        ((and (>= (car ccis) 135)(> (car ccis) (max* (cdr ccis)))) 5)
        ((and (>= (car ccis) 90)(> (car ccis) (max* (cdr ccis)))) 4)
        ((and (>= (car ccis) 45)(> (car ccis) (max* (cdr ccis)))) 3)
        ((and (>= (car ccis) 0)(> (car ccis) (max* (cdr ccis)))) 2)
        ((and (minusp (car ccis))(> (car ccis) (max* (cdr ccis)))) 1)
        (t (cci-direction (getd tdate 'ydate) num1 num4)))

   
 ))

(defun cci-slope-index (tdate days param)  ;;;number of days = number of data points for regression
 (let ((cci (nth-value 1 (lregress tdate days 'cci param))));;; this is one more than the equivalent ROC number of days 
       
    (cond ((< cci -75) -6)
          ((< cci -60) -5)
          ((< cci -45) -4)
          ((< cci -30) -3)
          ((< cci -15) -2)
          ((< cci 0) -1)
          ((< cci 15) 1)
          ((< cci 30) 2)
          ((< cci 45) 3)
          ((< cci 60) 4)
          ((< cci 75) 5)
          ((>= cci 75) 6))
   
))

(defun cci-slope (tdate days param)
  (/ (roc tdate days 'cci param) days))

(defun cci-speed-index (tdate param1 &optional (param2 1))
 (let (;(cci (nth-value 1 (lregress tdate days 'cci param))))
       (cci (/ (roc tdate param2 'cci param1) 1.0)))
    (cond ((<= cci -90) -7)
           ((< cci -75) -6)
          ((< cci -60) -5)
          ((< cci -45) -4)
          ((< cci -30) -3)
          ((< cci -15) -2)
          ((< cci 0) -1)
          ((< cci 15) 1)
          ((< cci 30) 2)
          ((< cci 45) 3)
          ((< cci 60) 4)
          ((< cci 75) 5)
          ((< cci 90) 6)
          ((>= cci 90) 7))
   
))


;;;this is the entry context
(defun cci-slope-hl (tdate days param)
  (let (cci-slopes (date tdate) current-slope)

    (dotimes (ith 2)
     (push (cci-slope-index date days param) cci-slopes)
     (setq date (getd date 'ydate)))

    (setq current-slope (car (last cci-slopes))) 
    (cond ;((and (<= (min* cci-slopes) -2)(> current-slope -3)) 1)
          ((and (>= current-slope -2)(> current-slope (car cci-slopes))) 1)
         ; ((and (>= (max* cci-slopes) 2)(< current-slope 3)) -1)
          ((and (<= current-slope 2)(< current-slope (car cci-slopes))) -1)
          (t 0))))


;;;this is the entry context which is basically acceleration (one day change in slope of cci)
(defun cci-accel-index (tdate param)
  (let (delta)

    (setq delta (- (cci-speed-index tdate param)(cci-speed-index (getd tdate 'ydate) param))) 
    
     ))



;;;this is the exits context
(defun cci-slope-hlex (tdate days param)
  (let (cci-slopes (date tdate) current-slope)

    (dotimes (ith 2)
     (push (cci-slope-index date days param) cci-slopes)
     (setq date (getd date 'ydate)))

    (setq current-slope (car (last cci-slopes))) 
    (cond ((< (car cci-slopes) current-slope) 1)
          ((> (car cci-slopes) current-slope) -1)
          (t 0))))


(defun cci-level-index (tdate size)
   (let ((cci (commodity-channel-index tdate size)))
        (cond ((< cci -270) -7)
              ((< cci -225) -6)
              ((< cci -180) -5)
              ((< cci -135) -4)
              ((< cci -90) -3)
              ((< cci -45) -2)
              ((< cci 0) -1)
              ((< cci 45) 1)
              ((< cci 90) 2)
              ((< cci 135) 3)
              ((< cci 180) 4)
              ((< cci 225) 5)
              ((< cci 270) 6)
              ((>= cci 270) 7))
))

(defun cci-index1 (tdate size)
   (let ((cci (commodity-channel-index tdate size)))
        (cond ((< cci -150) -4)
              ((< cci -100) -3)
              ((< cci -50) -2)
              ((< cci 0) -1)
              ((< cci 50) 1)
              ((< cci 100) 2)
              ((< cci 150) 3)
              ((>= cci 150) 4))
))


(defun cci-signal (tdate size days )
 (let (cci)
   
   (dotimes (ith days 0)
     (setq cci (commodity-channel-index (add-mkt-days tdate (- ith)) size))
     (cond ((>= cci 100) (return 1))
           ((<= cci -100) (return -1)) 
           (t 0)))

   ))

(defun cci-signal2 (tdate param)
  (let ((cci1 (commodity-channel-index tdate param))
	(cci2 (commodity-channel-index (getd tdate 'ydate) param)))
	
	(cond ((and (>= cci2 100)(< cci1 100)) -1)
              ((and (<= cci2 -100)(> cci1 -100)) 1)
	      (t 0))))
	      
;;;this is the entry trigger
(defun cci-signal1 (tdate param &optional (days 5)(lmt 100))
   (let (cci5h3 cci5l3 dir cci5h5 cci5l5 cci5 trig); ccis trig mcci tcci)
   (multiple-value-setq (cci5h3 cci5l3)(cci-high-low tdate param days))
   (multiple-value-setq (cci5h5 cci5l5)(cci-high-low tdate param (+ days 2)))
   (setq dir (roc tdate 1 'cci param) cci5 (commodity-channel-index tdate param))
  (setq trig
   (cond 
        ; ((and (> cci5h3 100)(= cci5h3 cci5h5)) -1)
        ; ((and (< cci5l3 -100)(= cci5l3 cci5l5)) 1)
       
         ((and (<= cci5l3 (- lmt))(< cci5h3 0)(> cci5h3 (- lmt))(<= dir 0)(> cci5 (- lmt))) -1);(> cci5 -50)) -1)
         ((and (>= cci5h3 lmt)(> cci5l3 0)(< cci5l3 lmt)(>= dir 0)(< cci5 lmt)) 1);(< cci5 50)) 1)
         (t 0)))
#|
   (dotimes (ith 5)
      (push (commodity-channel-index (add-mkt-days tdate (- ith)) 5) ccis))

   (setq mcci (cond ((= trig 1)(max* ccis))
                    ((=  trig -1)(min* ccis))
                    (t 0)))
      
   (setq tcci (second (member mcci (reverse ccis))))
   (cond ((and tcci (= trig 1)(> tcci 100)) (setq trig 0))
         ((and tcci (= trig -1)(< tcci -100)) (setq trig 0))
         (t trig))
|#
))

(defun cci-context (tdate &optional (period 21))
  (let (;(cci21 (commodity-channel-index tdate period))
	(cci21d1 (cci-direction tdate period 1))
        (dir (cci-signal tdate period 5))cci21h5 cci21l5)

    (multiple-value-setq (cci21h5 cci21l5)(cci-high-low tdate period 5))

    (cond 
          ((and (eql dir -1)(>= cci21h5 -100)(<= cci21h5 0)(minusp cci21d1)) -1)    
          ((and (eql dir 1)(<= cci21l5 100)(>= cci21l5 0)(plusp cci21d1)) 1)       
          (t 0))

))


(defun cci-context1 (tdate)
  (let ((cci21 (commodity-channel-index tdate 21))(cci21d1 (cci-direction tdate 21 1))
         (cci21d2 (cci-direction tdate 21 2)) cci21h11 cci21l11 )

    (multiple-value-setq (cci21h11 cci21l11)(cci-high-low tdate 21 11))

    (cond ((and (< cci21 -50)(>= cci21l11 -250)(plusp cci21d2)) 3)
	  ((and (> cci21 50)(< cci21h11 250)(minusp cci21d2)) -3)
	 
          ((and (>= cci21 -50)(<= cci21h11 50)(minusp cci21d1)) -2)
	  ((and (<= cci21 50)(>= cci21l11 -50)(plusp cci21d1)) 2)
          ((and (< cci21 -50)) -1)
          ((and (> cci21 50)) 1)
          
          
          (t 0))

))



;;;;;Linear regression
;;;;returns the forecast for the next day, slope, r-squared, and stderr
;;;slope is normalized to average true range
(defun lregress (tdate days &optional (att 'close) (param 21))
  (let ((tm-int *time-interval*)
       xs (date tdate) intercept slope correlation r-squared data (stderr 0)); deviations lower-dev upper-dev)
   
      (dotimes (ith days)
        (cond ((eql att 'pivot)
               (push (list (- days ith) (nth 2 (pivot-points date))) xs))
	      ((eql att 'adobv)
	       (push (list (- days ith) (car (nth-value 1 (adobv date days)))) xs))
              ((eql att 'obv)
	       (push (list (- days ith) (car (nth-value 1 (obv date days)))) xs))
              ((eql att 'rsi)
               (push (list (- days ith) (rsi date param)) xs))
              ((eql att 'tr);;;true range
               (push (list (- days ith) (/ (volatility date param)(volatility date (* 7 param) 1))) xs))            
               ((eql att 'roc)
               (push (list (- days ith) (roc date param)) xs))
             ((eql att 'cci)
               (push (list (- days ith) (commodity-channel-index date param)) xs))
               
             ((eql att 'bodyrange)
               (push (list (- days ith) (body-range date)) xs))

              ((eql att 'vi1-20);;;volume index
               (push (list (- days ith) (volume-ave-ratios date 1 20)) xs))
              ((eql att 'vi5-20);;;volume index'
               (push (list (- days ith) (volume-ave-ratios date 5 20)) xs))
              ((eql att 'vi10-25);;;volume index
             
               (push (list (- days ith) (volume-ave-ratios date 10 25)) xs))

              ((eql att 'op1-20);;;openint index
               (push (list (- days ith) (openint-ave-ratios date 1 20)) xs))

              ((eql att 'ss)
               (push (list (- days ith) (slow-stochastic date param)) xs))             
              ((eql att 'fs)
               (push (list (- days ith) (fast-stochastic date 14)) xs))             
              ((eql att 'hl)(setq *time-interval* 'daily-high-low)
               (setq data (nth-value 4 (data-access date)))
                 (push (list (* 3 (- days ith)) (getd date 'close)) xs)
                (push (list (1- (* 3 (- days ith))) (second data)) xs)
                (push (list (- (* 3 (- days ith)) 2) (first data)) xs))  
              (t (push (list (- days ith)(or (getd date att) 0)) xs)))
             ; (format t "date = ~A  xs = ~A~%" date xs) 
          (setq date (getd date 'ydate)))

  ;(print xs);(setq xs1 xs)
  (setq *time-interval* tm-int)
 ;;;linear regression requires a list of pairs in a list (point coordinates x and y)
;;; example ((x1 y1)(x2 y2)(x3 y3) ...
  (if (apply #'= (mapcar #'second xs));;;only if all prices are equal
     (setq intercept (second (car xs)) slope 0 correlation 1 r-squared 1 stderr 0)   
     (multiple-value-setq (intercept slope correlation r-squared stderr) (linear-regression xs)))
  
 ;   (format t "2date = ~A xs = ~A intercept = ~A" date xs intercept)
; (format t "~%Intercept = ~A slope = ~A correlation = ~A r-squared = ~A" intercept slope correlation r-squared)
 ;  (dolist (ith xs)
 ;        (push (- (second ith)(+ intercept (* (first ith) slope))) deviations));;

;    (setq lower-dev (min* deviations) upper-dev (max* deviations))

   (values  (my-pretty-price (+ (* slope (+ (length xs) (/ (length xs) days))) intercept))
           (float  slope)  (float r-squared) stderr
         (-  (my-pretty-price (+ (* slope (+ (length xs) (/ (length xs) days))) intercept))(* 2 stderr));;;lower deviation
         (+  (my-pretty-price (+ (* slope (+ (length xs) (/ (length xs) days))) intercept))(* 2 stderr));;;upper deviation
            )
          
))

(defun bodyrange-proj (tdate &optional (period 5))
 (let (forecast slope rsquared stderr)
  (multiple-value-setq (forecast slope rsquared stderr)(lregress tdate period 'bodyrange))

 (cond ((> forecast 3.0 stderr) 3)
       ((> forecast 2.0 stderr) 2)
       ((> forecast 1.0 stderr) 1)
       (t 0))))


;;; bin regression used in display bin expected values3
;;;;takes as input the longs or shorts
;;;computes correlation of indicator index value to ave bin P&L
#|
(defun bin-regression (trades)
  (let (xs intercept slope correlation r-squared)
    (setq xs (mapcar #'(lambda(s) (list (second (second s)) (car s))) trades))
     ;;;linear regression requires a list of pairs in a list (point coordinates x and y)
;;; example ((x1 y1)(x2 y2)(x3 y3) ...
  (when (every #'numberp (mapcar #'first xs))
  (if (apply #'= (mapcar #'second xs));;;only if all prices are equal
     (setq intercept (second (car xs)) slope 0 correlation 1 r-squared 1)   
  (multiple-value-setq (intercept slope correlation r-squared) (linear-regression xs))))
  (values correlation slope)
))
|#

;;;returns value of the line on tdate
(defun regression-line (tdate days &optional (att 'close))
  (let (forecast slope)

   (multiple-value-setq (forecast slope)(lregress tdate days att))

   (- forecast slope)
))

(defun r-squared-index (tdate days)
   (let ((indx (nth-value 2 (lregress tdate days 'hl))))
        (truncate indx .1)
))

(defun r-squared-change-index (tdate days &optional (delta 1))
   (let  ((indx (nth-value 2 (lregress tdate days 'hl ))) cng
          (indx-1 (nth-value 2 (lregress (add-mkt-days tdate (- delta)) days 'hl ))))
  ;(round  (- indx indx-1) .1)
    (setq cng (- indx indx-1)) 
    (cond ((> cng .32) 5)
          ((> cng .21) 4)
          ((> cng .12) 3)
          ((> cng .05) 2)
          ((> cng 0) 1)
          ((> cng -.05) -1)
          ((> cng -.12) -2)
          ((> cng -.21) -3)
          ((> cng -.32) -4)
          ((<= cng -.32) -5))
#| 
   (cond ((> cng .35) 3)
        ;  ((> cng .21) 4)
          ((> cng .15) 2)
         ; ((> cng .05) 2)
          ((> cng 0) 1)
          ;((> cng -.05) -1)
          ((> cng -.15) -1)
         ; ((> cng -.21) -3)
          ((> cng -.35) -2)
          ((<= cng -.35) -3))
 |#
  
))
#|
;;;days is the length of the regression time
(defun regression-dump (market sdate tdate days)
   (let ( dir (date tdate) date-1 can chan
         (path1 (string-append *output-upper-dir* "lregress-dump.dat")))
   (set-market market)(setq date-1 (getd date 'ydate))
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (loop
      
      (setq dir (ldev-index date days) can (candle date 2) chan (channel-direction date 3)) 
               
      (format stream "~8@A  ~8@A ~6@A ~6@A  ~8@A  ~8@A ~8@A~%" 
            date (getd date 'close) dir can chan
            (getd date 'low) (getd date 'high))
      (if (<= date sdate )(return))
      (setq date date-1 date-1 (getd date-1 'ydate))

    ))
))
|#

(defun l-proj (tdate &optional (period 5))
  (let (lforecast sforecast cforecast  hforecast lslope hslope lr2 hr2 lstderr hstderr)
      (multiple-value-setq (cforecast sforecast) (lregress tdate period 'close))
      (multiple-value-setq (lforecast lslope lr2 lstderr) (lregress  tdate period 'low))
      (multiple-value-setq (hforecast hslope hr2 hstderr)  (lregress tdate period 'high))
  (values (- lforecast (* 1.618 lstderr)) lforecast hforecast (+ hforecast (* 1.618 hstderr)) sforecast)))

(defun ldev-index (tdate &optional (period 5))
  (let (lforecast lslope lr2 cforecast cslope cr2 hstderr lstderr
         hforecast hslope hr2  (tclose (getd tdate 'close))
       ; (thigh (getd tdate 'high))(tlow (getd tdate 'low))
        (date-1 (getd tdate 'ydate)))
      (multiple-value-setq (cforecast cslope cr2  ) (lregress date-1 period 'close))
      (multiple-value-setq (lforecast lslope lr2 lstderr) (lregress  date-1 period 'low))
      (multiple-value-setq (hforecast hslope hr2 hstderr)  (lregress date-1 period 'high))
                    
 ;;;reversal lows indicated by 3 1
;;;reversal highs indicated by -1 -3     
       (cond ((> tclose (+ hforecast (or hstderr 0))) 3)
             ((> tclose hforecast) 2)
             ((> tclose cforecast) 1)
                  
              ((= tclose cforecast) 0)
           
             ((< tclose (- lforecast (or lstderr 0))) -3)
             ((< tclose  lforecast) -2)
             ((< tclose cforecast) -1)     
            
             (t 0))

))
(defun ldev-index1 (tdate period)
   (let (forecast slope r2 stderr (vol (volatility tdate period 1))
         (tclose (getd tdate 'close))(date-1 (getd tdate 'ydate)))
    (multiple-value-setq (forecast slope r2 stderr) (lregress date-1 period 'hl))
     (cond ((> forecast (+ tclose (* 1.618 vol))) 4)
           ((> forecast (+ tclose (* 1.0 vol))) 3)
           ((> forecast (+ tclose (* .618 vol))) 2)
           ((> forecast (+ tclose (* 0 vol))) 1) 
           ((> forecast (- tclose (* .618 vol))) -1)
           ((> forecast (- tclose (* 1.00 vol))) -2)
           ((> forecast (- tclose (* 1.618 vol))) -3)
            (t -4))
  ))         
    

(defun lprojdelta (tdate period)
  (- (lproj-index tdate period)(lproj-index (getd tdate 'ydate) period)))

;;;compares tomorrow's projection with todays projection.
;;;period is in days
(defun lproj-index (tdate &optional (period 10))
  (let ((lproj (lregress tdate period 'hl)) (vol (volatility tdate period 1))
        (lproj-1 (lregress (getd tdate 'ydate) period 'hl)) lpj)
     
    (setq lpj (/ (- lproj lproj-1) vol) )
   (cond ((< lpj -1.40) -5)
         ((< lpj -1.05) -4)
         ((< lpj -.70) -3)
         ((< lpj -.35) -2)
         ((< lpj 0) -1)
         ((< lpj .35) 1)
         ((< lpj .70) 2)
         ((< lpj 1.05) 3)
         ((< lpj 1.40) 4)
         ((>= lpj 1.40) 5))
))

;;;;
;;;default is 2 standard errors above and below the forecast
(defun lbounds (tdate &optional (num1 5) (width 2))
  (let (code forecast slope r2 stderr (h1 (getd tdate 'high))(l1 (getd tdate 'low))
        (cl1 (getd tdate 'close)))

   (multiple-value-setq (forecast slope r2 stderr) (lregress (getd tdate 'ydate) num1 'hl))
  
    (setq code             
        (cond ((and (< l1 (- forecast (* width stderr)))
                    (> h1 (+ forecast (* width stderr)))) 9)
              ((< l1 (- forecast (* width stderr))) -1)
              ((> h1 (+ forecast (* width stderr))) 1)  
              (t 0)))
        (if (< cl1 (- forecast (* width stderr))) (decf code))
        (if (> cl1 (+ forecast (* width stderr))) (incf code))
   code
))




(defun mo-trend-index (tdate)
   (let ((mo (macd-direction tdate 12 26 9)) forecast slope r-squared)
        (multiple-value-setq (forecast slope r-squared)(lregress tdate 5 'close))
     (cond ((and (plusp mo)(plusp slope)) 'UP0)
           ((and (minusp mo) (plusp slope)) 'UP1)
           ((and (plusp mo)(minusp slope)) 'DN1)
           ((and (minusp mo)(minusp slope)) 'DN0)
           (t 'FT))
))
;;;****************************************************************************
;;;FIRST-WAVE, END-WAVE PREDICATES (Transfered from MAKE 09-JUL-89)
;;;

#|
;;;;converts to a string for printing
(defun convert-to-32nds (price)
  (if price
  (multiple-value-bind (whole fraction) (truncate price)
    (when (= (round (* fraction 32)) 32)
	(incf whole)(setq fraction 0))
    ;(my-round (+ whole (* fraction .32)) 2))
    (setq fraction (round (* fraction 32)))
    
    (format nil "~A\`~2,,,'0@A" whole fraction))
    
    ))

;;;;converts to a string for printing
(defun convert-to-32nds (price)
  (if price
  (multiple-value-bind (whole fraction) (truncate price)
    (when (= (round (* fraction 64)) 64)
	(incf whole)(setq fraction 0))
    
    (setq fraction (* .5 (round (* fraction 32) .5)))
    
    (format nil "~A\`~4,,,'0@A" whole fraction))
    
    ))
|#
;;;;converts to a string for printing
(defun convert-to-32nds (price)

   (cond ((and price (member *data-name* '(ty.d1b us.d1b)))
          (multiple-value-bind (whole fraction) (truncate price)
           (when (= (round (* fraction 64)) 64)
	     (incf whole)(setq fraction 0))

           (setq fraction (* .5 (round (* fraction 32) .5)))
           (format nil "~A\`~4,,,'0@A" whole fraction)))
         (price
           (multiple-value-bind (whole fraction) (truncate price)
           (when (= (round (* fraction 32)) 32)
	     (incf whole)(setq fraction 0))

           (setq fraction (* 1 (round (* fraction 32) 1)))
           (format nil "~A\`~2,,,'0@A" whole fraction))))

     )

;;;the price is in decimal
;;;this is for the oec .csv file
(defun convert-to-oec-32nds (price)
  (if price
  (multiple-value-bind (whole fraction) (truncate price)
    (when (= (round (* fraction 32)) 32)
	(incf whole)(setq fraction 0))

    (setq fraction (* 1 (round (* fraction 32) 1)))

    (format nil "~A ~2,,,'0@A" whole fraction))

    ))


#|
(defun convert-to-32 (price)
  (if price
  (multiple-value-bind (whole fraction) (truncate price)
    (when (= (round (* fraction 32)) 32)
	(incf whole)(setq fraction 0))
    (my-round (+ whole (* fraction .32)) 2))))
|#

(defun convert-to-32 (price)
  (if price
  (multiple-value-bind (whole fraction) (truncate price)
    (when (= (round (* fraction 32)) 32)
	(incf whole)(setq fraction 0))
    (my-round (+ whole (* .005 (round (* fraction .32) .005))) 3))))


;;;converts from fractions to decimals
;;;changes data like bonds from 32nds to 100ths
;;;enter the data-format with a decimal in front for 64 use .64
(defun convert-to-decimal (price &OPTIONAL (data-format .32))
  (if price
      (multiple-value-bind (whole fraction) (truncate price)
	(+ whole (/ (my-round fraction 5) data-format)))))

;;;rounds off a number to a number of decimal places

(defun convert-to-60 (price)
  (if price
  (multiple-value-bind (whole fraction) (truncate price)
    (when (= (round (* fraction 60)) 60)
	(incf whole)(setq fraction 0))
    (my-round (+ whole (* fraction .60)) 2))))
 

(defun my-round (number places)  ;; 7/00 lmf
  (cond ((not number) nil)
	((> (abs number) 99999) number)
	(t (let ((shift1 (^ 10 places)))
	     (/ (float (round (* number shift1))) shift1)))))


;;;UTILITY FUNCTIONS FOR THE CHECKING PROGRAMS;;;

(defun c (gl p1 p2 &optional (tol nil))
 (let (dum dif)
  (setq dum (and p1 p2))
  (if dum (setq dif (- p1 p2))) 
  (cond ((not dum) t)
        ((not tol) (if (EQL gl '>) (>= dif 0) (<= dif 0)))
        (t (if (EQL gl '>) (or (>= dif 0) (> tol (abs dif)))
                             (or (<= dif 0) (> tol (abs dif))))))))
   
   
 	      
(defun price-string (price &optional width (convert-32ndsp t))
  (let (whole rem fmt digits)
    (ifn price (return-from price-string "nil"))
    (if (= price 0) (return-from price-string "0"))
    (ifn width (setq width (- 5 (ceiling (log (abs price) 10)))))
    (cond ((index-32sp)
	   (setq fmt (format nil "~~~d,' d'~~2,'0d" width))
	   (if convert-32ndsp (setq price (convert-to-32nds price)))
	   (multiple-value-setq (whole rem) (truncate price 1.0))
	   (setq rem (abs (round (* 100 rem))))
	   (format nil fmt whole rem)
	   )
	  (t
	   (setq digits (index-digits *data-name* *all-markets*))
	   (ifn digits (setq digits 2))
	   (if (> digits 0)
	       (setf fmt (format nil "~~~d,~df" width digits))
	       (setf fmt (format nil "~~~dd" width)))
	   (format nil fmt price)))))

;;;;;determines the time length of incomplete waves
;;;does not include wave 0 the partial primitive
;;;added back 1/17/2015
(defun max-lgth% (wave)
   (- (log (getv wave HP)) (log (getv wave LP))))


;;;;defines market sector    
(defun sector-index (market)
  (cond ((member market *indexes-list*) 0)
        ((member market *interest-rates-list*) 1)
        ((member market *metals-list*) 2)
        ((member market *energies-list*) 3)
        ((member market *currencies-list*) 4)
        ((member market *softs-list*) 5)
        ((member market *grains-list*) 6)
        ((member market *meats-list*) 7)))

(defun ilan-exit1 (date)
  (let ((date-1 (getd date 'ydate)))
  (cond ((and (< (getd date 'low)(getd date-1 'low))
              (> (getd date 'close)(getd date-1 'close))
              (<= (commodity-channel-index date-1 18) -180)) 1)
        ((and (> (getd date 'high)(getd date-1 'high))
              (< (getd date 'close)(getd date-1 'close))
              (>= (commodity-channel-index date-1 18) 180)) -1)
        (t 0))
))

(defun warehouse (market)
   (case (sector-index market)

  (0  (build-swingtrade-warehouse *indexes-list*))
  (2 (build-swingtrade-warehouse *metals-list*))
  (4 (build-swingtrade-warehouse (remove 'jy.d1b *currencies-list*)))
  (6 (build-swingtrade-warehouse *grains-list*)))
)

(defun DM-init (tdate size)
  (let (date upmove downmove thigh tlow yhigh ylow +DM -DM (+DM14 0) (-DM14 0) (tr14 0))
    (setq date (add-mkt-days tdate (- size)))
    (dotimes (ith size)
      (setq date (getd date 'ndate))
    (setq thigh (getd date 'high) tlow (getd date 'low) yhigh (getd (getd date 'ydate) 'high)
	  ylow (getd (getd date 'ydate) 'low))
    (setq upmove (- thigh yhigh) downmove (- ylow tlow)) 

    (if (and (> upmove downmove)(plusp upmove)) (setq +DM upmove) (setq +DM 0))
    (if (and (> downmove upmove) (plusp downmove)) (setq -DM downmove)(setq -DM 0))
    (setq +DM14 (+ +DM14 +DM) -DM14 (+ -DM14 -DM) tr14 (+ (true-range date) tr14))
    )
  
  (values +DM14 -DM14 tr14 )))

(defun adx (tdate size)
  (let (date thigh tlow yhigh ylow (+DM 0.0) (-DM 0.0) (+DM14 0.0) (-DM14 0.0) (+DI14 0.0)
	     (-DI14 0.0)(tr14 0.0) (tr 0.0) (adxv 0.0) (dx 0.0) (upmove 0.0) (downmove 0.0))

    (setq date (add-mkt-days tdate (- 150)))
    
    (multiple-value-setq (+DM14 -DM14 tr14)(dm-init date size))
    
   ; (format T "~%~A  ~A   ~A  ~A  ~%" date +DM14 -DM14 tr14)
    ;(setq +DM14 (my-round +DM14 2) -DM14 (my-round -DM14 2) tr14 (my-round tr14 2))
   ;(break "123")
   (setq +DI14 (/ +DM14 tr14) -DI14 (/ -DM14 tr14))
   (setq dx (* 100 (/ (abs (- +DI14 -DI14))(+ +DI14 -DI14))))
   (setq adxv dx)
    
   (dotimes (ith 150)
     (setq date (getd date 'ndate))
    (setq thigh (getd date 'high) tlow (getd date 'low) yhigh (getd (getd date 'ydate) 'high)
	  ylow (getd (getd date 'ydate) 'low))
    (setq upmove (- thigh yhigh) downmove (- ylow tlow) tr (true-range date)) 
   
    (if (and (> upmove downmove)(plusp upmove)) (setq +DM upmove) (setq +DM 0.0))
    (if (and (> downmove upmove) (plusp downmove)) (setq -DM downmove)(setq -DM 0.0))
   ; (setq +DM14 (my-round +DM14 2) -DM14 (my-round -DM14 2) tr14 (my-round tr14 2))
   ; (format T "~%~A ~A  ~A ~A ~A ~%" Date +DM -DM tr tr14 )
   ; (break "234")
    (setq +DM14 (+ (- +DM14 (/ +DM14 size)) +DM)
	  -DM14 (+ (- -DM14 (/ -DM14 size)) -DM)
	  tr14 (+ (- tr14 (/ tr14 size)) tr))
   ; (format T "~%~A ~A  ~A ~A ~A ~%" Date +DM -DM tr tr14 )
    ;(break "345")
   (setq +DI14 (/ +DM14 tr14) -DI14 (/ -DM14 tr14))
   (setq dx (* 100 (/ (abs (- +DI14 -DI14))(+ +DI14 -DI14))))
   (setq adxv  (/ (+ (* adxv (1- size)) dx) size))
  ; (format T "~%~A ~A ~A  ~A   ~A   ~A     ~A    ~A    ~A~%" date +DM -DM +DM14 -DM14  +DI14 -DI14 DX ADXV)
  ; (break "456")
   )
   (values adxv (- +DI14 -DI14))
    ))

(defun adx-index (date size)
 (let ((adxv (adx date size)))
   (cond ((<= adxv 15) 15);;weak or absent trend
	 ((<= adxv 25) 25)
	 ((<= adxv 35) 35)
	 ((<= adxv 45) 45);;strong trend
         ((<= adxv 60) 60);;;very strong trend
   	 (t 100))));;;extremely strong trend

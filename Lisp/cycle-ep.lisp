
					; -*- Mode: LISP; Package: user; Base: 10. -*-
; Functions that calculates the cycles

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

         ;;;THIS CYCLE-EP1.LISP is for TESTING NOT PRODUCTION 


;;;;;;;;;
;;;the minimum cycle size is in days i.e. there are 2 prices per day
;;;the maximum cycle size is in days
;;;data-length is 125 days
;;;must go 1 full cycle length of prices beyond data-length to compute moving average
(defun cycle-dif-oc (stop-time  &optional (min-cycle-size 10)(max-cycle-size 30))
  (block FAT
    (let* ((data-length 126)
           (startday (add-mkt-days (getnumdate stop-time) -126));-244))
	   (starttime nil) startprice

	   (endday (getnumdate stop-time))
           (time-interval *time-interval*)
	   (endtime nil) (summa nil)  cycle-2
	   priorday priortime priorprice  nextday nexttime nextprice
	   zz (result 0) )
     
           (if (or (stringp startday)(stringp (data-access startday))
              (stringp (getd startday 'ydate))(stringp (data-access (getd startday 'ydate))))
          (setq data-length 126 startday (add-mkt-days (getnumdate stop-time) -126)));-122)))

      (setq *time-interval* 1440)
      ;;;change cycle sizes to prices
      (setq min-cycle-size (* 2 min-cycle-size) max-cycle-size (* 2 max-cycle-size))

      (setq starttime 'O startprice (getd startday 'open) endtime  'C)
      (if (stringp (data-access startday))
	  (return-from fat (data-access startday)))

      (multiple-value-setq (priorday priortime priorprice)
	  (prior-data-point startday starttime startprice))
      (setq priorday (getd startday 'ydate) priortime 'C priorprice (getd priorday 'close))
      (setq nextday startday  nexttime 'C nextprice (getd startday 'close))
      (nil-tpv)
;;now fill the data vector
;;;start with the startday at index 1
	(put-TPV 1 (vector 1 startday starttime startprice 0 0 0))
	;;;fill the 1st point prior to the startday
	(put-TPV 0 (vector 0 priorday priortime priorprice 0 0 0))
	;;;fill the next points until the end-time
	(put-TPV 2 (vector 2 nextday nexttime nextprice 0 0 0))
	(do ((index 3 (1+ index)))
	    ((and (= endday nextday) (or (not endtime) (eql endtime nexttime)))
	     (SETQ *END-INDEX* (1- INDEX))
	     (put-TPV  (1+ *end-index*) nil))
	  (if (oddp index) (setq nextday (getd nextday 'ndate) nexttime 'O nextprice (getd nextday 'open))
	     (setq nexttime 'C nextprice (getd nextday 'close)))
	  (put-TPV index (vector index nextday nexttime nextprice 0 0 0)));;;closes the do

;;;must loop from 20 to 60 (10 to 30 days)
      (do ((cycle-length  min-cycle-size (+ cycle-length 2))
	   (max-days 0))
	  ((> cycle-length  max-cycle-size) )
;;;calculate maxdays for the cycle-length
	(setq max-days
	      	   (1+ (* cycle-length
			  (max (if (oddp
				      (setq zz
				           (truncate (/ (- data-length 1) cycle-length))))
			               (decf zz) zz)
			        1))))
;;detrend for a given cycle-length

        (setq cycle-2 (truncate cycle-length 2))
;;;calculates the full cycle moving average for each data point
;;;this is a  moving average
        (dotimes (ith   max-days)
	  (setq result 0)
	  (setf (svref  (get-tpv (- *end-index* ith)) 4)
	      (dotimes (jth cycle-length (float (/ result cycle-length)))
		(setq result
		      (+ result
			 (svref  (get-tpv (- *end-index* ith jth)) 3))))))
;	(dotimes (ith cycle-2)
;	   (setf (svref (get-tpv (- *end-index* ith)) 4)
;	         (svref (get-tpv (- *end-index* cycle-2)) 4)))
;;;;calculates the deviation from trend
;;;these deviations are from the average
	(dotimes (ith   max-days)
	  (setf (svref (get-tpv (- *end-index* ith)) 5)
		(- (svref (get-tpv (- *end-index* ith)) 3)
		   (svref (get-tpv (- *end-index* ith)) 4))))
;;;;difference in assoc list of summa for a given cycle-length
	(setq summa (acons cycle-length 0 summa))
	(setq result 0)
	(setf (cdr (assoc cycle-length summa))
	      (dotimes (kth (- max-days cycle-2) result)
	         (setq result
		    (+ result
                       (- (svref (get-tpv (- *end-index* kth)) 5)
		                 (svref (get-tpv (- *end-index* kth cycle-2)) 5))))
	       ))
;;;de-weight sum
         (setf (cdr (assoc cycle-length summa))
	       (/ (cdr (assoc cycle-length summa))
	          (* cycle-2
                             (- max-days cycle-2))))
		  );;;closes
      (dolist (jth summa)
	(setf (cdr jth) (abs (cdr jth))))
      (vsort summa #'< 'cdr)
 ;;compute full cycle average at the current time

(setq *time-interval* time-interval)
    (values (truncate (caar summa) 2) summa)
      )))
;;;;;FOr highs and lows
(defun cycle-dif-hl (stop-time  &optional (min-cycle-size 10)(max-cycle-size 30))
  (block FAT
    (let* ((data-length 126)
           (startday (add-mkt-days (getnumdate stop-time) -252));-244))
	   (starttime nil) startprice t-list p-list
	   (endday (getnumdate stop-time))
           (time-interval *time-interval*)
	   (endtime 'P) (summa nil)  cycle-2
	   priorday priortime priorprice  nextday nexttime nextprice
	   zz (result 0) ignore)
    
      (setq *time-interval* 'daily-high-low)
      (if (or (stringp startday)(stringp (data-access startday))
              (stringp (getd startday 'ydate))
              (stringp (data-access (getd startday 'ydate))))
          (setq data-length 122 startday (add-mkt-days (getnumdate stop-time) -244)));-122)))

      ;;;change cycle sizes to prices
      (setq min-cycle-size (* 2 min-cycle-size) max-cycle-size (* 2 max-cycle-size))

      (if (stringp (data-access startday))
	  (return-from fat (data-access startday)))

      (multiple-value-setq (ignore priorday nextday t-list p-list)
        (data-access startday))
      (setq starttime (car t-list) startprice (car p-list) endtime (cadr t-list))
      (multiple-value-setq (priorday priortime priorprice)
	  (prior-data-point startday starttime startprice))
      (multiple-value-setq (nextday nexttime nextprice)
          (next-data-point startday  starttime startprice))
      (nil-tpv)
;;now fill the data vector
;;;start with the startday at index 1
	(put-TPV 1 (vector 1 startday starttime startprice 0 0 0))
	;;;fill the 1st point prior to the startday
	(put-TPV 0 (vector 0 priorday priortime priorprice 0 0 0))
	;;;fill the next points until the end-time
	(put-TPV 2 (vector 2 nextday nexttime nextprice 0 0 0))
	(do ((index 3 (1+ index)))
	    ((and (= endday nextday) (or (not endtime) (eql endtime nexttime)))
	     (SETQ *END-INDEX* (1- INDEX))
	     (put-TPV  (1+ *end-index*) nil))
	  (multiple-value-setq (nextday nexttime nextprice)
	                       (next-data-point nextday nexttime nextprice))
	  (put-TPV index (vector index nextday nexttime nextprice 0 0 0)));;;closes the do

;;;must loop from 20 to 60 (10 to 30 days)
      (do ((cycle-length  min-cycle-size (+ cycle-length 2))
	   (max-days 0))
	  ((> cycle-length  max-cycle-size) )
;;;calculate maxdays for the cycle-length
	(setq max-days
	      	   (1+ (* cycle-length
			  (max (if (oddp
				      (setq zz
				           (truncate (/ (- data-length 1) cycle-length))))
			               (decf zz) zz)
			        1))))
;;detrend for a given cycle-length

        (setq cycle-2 (truncate cycle-length 2))
;;;calculates the full cycle moving average for each data point
;;;this is a centered moving average
        (dotimes (ith  (+  cycle-2 max-days))
	  (setq result 0)
	  (setf (svref  (get-tpv (- *end-index* ith cycle-2)) 4)
	      (dotimes (jth cycle-length (/ result cycle-length))
		(setq result
		      (+ result
			 (svref  (get-tpv (- *end-index* ith jth)) 3))))))
	(dotimes (ith cycle-2)
	   (setf (svref (get-tpv (- *end-index* ith)) 4)
	         (svref (get-tpv (- *end-index* cycle-2)) 4)))
;;;;calculates the deviation from trend
;;;these deviations are from the centered average
	(dotimes (ith  (+ cycle-2 max-days))
	  (setf (svref (get-tpv (- *end-index* ith)) 5)
		(- (svref (get-tpv (- *end-index* ith)) 3)
		   (svref (get-tpv (- *end-index* ith)) 4))))
;;;;difference in assoc list of summa for a given cycle-length
	(setq summa (acons cycle-length 0 summa))
	(setq result 0)
	(setf (cdr (assoc cycle-length summa))
	      (dotimes (kth (- max-days cycle-2) result)
	         (setq result
		    (+ result
(- (svref (get-tpv (- *end-index* kth)) 5)
		                 (svref (get-tpv (- *end-index* kth cycle-2)) 5))))
	       ))
;;;de-weight sum
         (setf (cdr (assoc cycle-length summa))
	       (/ (cdr (assoc cycle-length summa))
	          (* cycle-2
          (- max-days cycle-2))))
		  );;;closes
      (dolist (jth summa)
	(setf (cdr jth) (abs (cdr jth))))
      (vsort summa #'< 'cdr)
 ;;compute full cycle average at the current time

(setq *time-interval* time-interval)
    (values (truncate (caar summa) 2) summa)
      )))

;;;;;;;;;size is in days
(defun rsi-ohlc (stopdate size &optional prices)
  (block nil
  (let ((changes nil)(up-col nil)(dn-col nil)(up-col-ave nil)(dn-col-ave nil)
         tdate (time-interval *time-interval*) p-list  ignore aver)
    
;;;use only the open and close prices
;;;need to subtract the average of twice the size
;;;need a list of changes from start to finish
   (unless prices
    (setq tdate stopdate *time-interval* 720)
    (dotimes (ith (1+ size))
      (multiple-value-setq (ignore ignore ignore ignore p-list)
        (data-access tdate))

      (setq aver (ave tdate (* 2 size)))
      (setq prices (cons (- (nth 1 p-list) aver) prices))
      (setq prices (cons (- (car p-list) aver) prices))
      (setq tdate (getd tdate 'ydate)))

    (setq prices (reverse prices))
      );;;closes the unless
    (dotimes (ith (* 2 size))
    (push (- (nth ith prices)(nth (1+ ith) prices)) changes))

    (setq changes (reverse changes))
;;;need to keep the positive and negative sums for size observations
;;;need first size number of changes
    (dotimes (jth (* 2 size))
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
    (setq *time-interval* time-interval)
    (round (* 100 (/ up-col-ave (+ up-col-ave (- dn-col-ave)))))

      )))

(defun slow-stochasticp (tdate size)
  (let ((stoc (slow-stochastic tdate size)))
  (cond ((>= stoc 80) 'DN)
        ((<= stoc 20) 'UP))

   ))
(defun stochastic-signal (tdate size)
  (let* ((slow (slow-stochastic tdate size))(fast (fast-stochastic tdate size))
         (epsignal (- fast slow)))
       (cond ((plusp epsignal) 'UP)
             ((minusp epsignal) 'DN))
     ))

;;;this is the Williams percent R
;;;ranges from 0 to -100.
(defun %R (tdate size)
   (let ((nhigh (n-day-high tdate size))(nlow (n-day-low tdate size))(closet (getd tdate 'close)))

    (* -100 (/ (- nhigh closet)(- nhigh nlow)))))


;;;given a sorted list of cycle periods this function clusters them

;;;example returns a list ((10)(12 12 13)(16)(20 20 20 20 20 20 21 21)

;;;                        (23 23 23 23 23 23 23 23 24 24 24)(26 26 27 27)

;;;                        (29 29 29 29))
(defun cluster-cycles (period-list)
  (let (temp cluster)
    (do* ((periods period-list (cdr periods))
         (ith (car period-list) (car periods))
         (jth (cadr period-list) (cadr periods)))
        ((null ith) cluster)
      (cond  ((null jth)(push ith temp)(push temp cluster))
             ((= ith jth) (push ith temp))
             ((= ith (1- jth))(push ith temp))
             (t (push ith temp)(push temp cluster) (setq temp nil))))

))

 ;;;;;;;;;THis is for daily closes only

;;;the minimum cycle size is 30 i.e. 15 mkt days for daily-high-low
;;;the maximum cycle size is 90 i.e. 45 mkt days for daily-high-low
(defun cycle-dif-c (stop-time  &optional (min-cycle-size 10)(max-cycle-size 30))

    (let* ((data-length 122)
           (startday (add-mkt-days (getnumdate stop-time) -488));-244))
	   (starttime nil) startprice
 cycle-2
	   (endday (getnumdate stop-time))

	   (endtime nil) summa    (time-interval *time-interval*)
	   priorday priortime priorprice  nextday nexttime nextprice
	   zz (result 0))
      (setq *time-interval* 1440)
      (if (or (stringp startday)(stringp (data-access startday))
              (stringp (getd startday 'ydate))(stringp (data-access (getd startday 'ydate))))
          (setq data-length 61 startday (add-mkt-days (getnumdate stop-time) -244)));-122)))

      (setq startprice (getd startday 'close))

      (multiple-value-setq (priorday priortime priorprice)
	  (prior-data-point startday 'C startprice))
      (multiple-value-setq (nextday nexttime nextprice)
	  (next-data-point startday 'C startprice))

	;;;start with the startday at index 1
	(put-TPV 1 (vector 1 startday starttime startprice 0 0))
	;;;fill the 1st point prior to the startday
	(put-TPV 0 (vector 0 priorday priortime priorprice 0 0))
	;;;fill the next points until the end-time
	(put-TPV 2 (vector 2 nextday nexttime nextprice 0 0))
	(do ((index 3 (1+ index)))
	    ((and (= endday nextday) (or (not endtime) (eql endtime nexttime)))
	     (SETQ *END-INDEX* (1- INDEX))
	     (put-TPV  (1+ *end-index*) nil))

	  (multiple-value-setq (nextday nexttime nextprice)
	    (next-data-point nextday nexttime nextprice))
	  (put-TPV index (vector index nextday nexttime nextprice 0 0)))


;;;must loop from 10 to 50 days
;;;min cycle-size must be an even number
      (do ((cycle-length  min-cycle-size (+ cycle-length 2))
	   (max-days 0))
	  ((> cycle-length  max-cycle-size) )
;;;calculate maxdays for the cycle-length

	(setq max-days
	      	   (1+ (* cycle-length
			  (max (if (oddp
				(setq zz
				      (truncate (/ (- data-length 1) cycle-length))))
			      (decf zz) zz)
			      1))))
	  (setq cycle-2 (truncate cycle-length 2))
;;;detrend for a given cycle-length

;;;calculates the full cycle moving average for each data point
;;;this is a centered moving average
        (dotimes (ith  (- *end-index* cycle-length))
	  (setq result 0)
	  (setf (svref (get-tpv (- *end-index* ith cycle-2)) 4)
	        (dotimes (jth cycle-length (/ result cycle-length))
		   (setq result
		      (+ result
			 (svref (get-tpv (- *end-index* ith jth)) 3))))))

      (dotimes (ith cycle-2)

        (setf (svref (get-tpv (- *end-index* ith)) 4)

              (svref (get-tpv (- *end-index* cycle-2)) 4)))
;;;;calculates the deviation from trend

	(dotimes (ith  (- *end-index* cycle-length))
	  (setf (svref (get-tpv (- *end-index* ith)) 5)
		(- (svref (get-tpv (- *end-index* ith)) 3)
		   (svref (get-tpv (- *end-index* ith)) 4))))

;;;;difference in assoc list of summa for a given cycle-length
	(setq summa (acons cycle-length 0 summa))
	(dotimes (kth (- max-days cycle-2))
	  (setf (cdr (assoc cycle-length summa))
		(+ (cdr (assoc cycle-length summa))
		   (- (svref (get-tpv (- *end-index* kth)) 5)
		      (svref (get-tpv (- *end-index* kth cycle-2)) 5)))))
;;;de-weight sum
      (setf (cdr (assoc cycle-length summa))
	    (/ (cdr (assoc cycle-length summa))
	       (* cycle-2  (- max-days cycle-2))))
		  );;;closes

      (dolist (jth summa)
	(setf (cdr jth) (abs (cdr jth))))
      (vsort summa #'< 'cdr)

      (setq *time-interval* time-interval)

       (caar summa)



      ))



;;;;summa is expected to be 2 prices per day
;;;min-size is in days max-size is in days
 (defun harmonics (seed min-size max-size summa)
    (let* ((har (* 2 seed)) (harms (list har)))

     (loop
        (setq har (harmonic-down har min-size summa))
        (if har (setq harms (cons har harms)) (return)))

     (setq har (* 2 seed))
     (loop
        (setq har (harmonic-up har max-size summa))
        (if har (setq harms (cons har harms)) (return)))

      (vsort harms #'<)

      (mapcar #'(lambda (s1) (round (/ s1 2))) harms)
      ))


 (defun harmonic-down (seed min-size summa)
    (block nil
    (let ((har-2 (round (/ seed 2))))
       (if (oddp har-2) (decf har-2))

       (loop (ifn (assoc har-2 summa) (setq har-2 (- har-2 1)) (return))
             (if (< har-2 (* 2 min-size)) (return)))
       (if (< har-2 (* 2 min-size)) (return nil))


       (setq har-2 (best-harmonic har-2 summa))

        har-2)))


(defun harmonic-up (seed max-size summa)
    (let ((2har (* seed 2)))
         (if (> 2har (* 2 max-size)) nil
             (setq 2har (best-harmonic 2har summa)))))

(defun best-harmonic  (seed summa)
   (let (summa1 (lo (round (* seed .75)))(hi (round (* seed 1.25))))
    (dolist (kth summa)
      (if (and (>= (car kth) lo)(<= (car kth) hi))
          (push kth summa1)))

     (vsort summa1 #'< 'cdr)
     (caar summa1)))

(defun cycles (&optional tdate (min-period 10)(max-period 30)
                          (market-list (append *day-list* *dubai-list*)) (outfile t))
   (let ((counter 0) date P0  chan cr can  vri csignal cy2 trend bri ldev10
          ldev rel-s path1  mac-dir date-1 date-2)
        (maind-x)(set-cat-list)
	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
       
        (setq path1 (string-append "exitpoints/" (format nil "cy~S-~S.dat" min-period max-period)))
        (if (and outfile (probe-file path1))
            (delete-file path1))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%MKT      PR     CY1   BRI  VRI   CHAN   CR  CSIG   CAN  GANN  RSQR  MCD  LDEV L10~%" (date-convert date)))
        (dolist (market market-list)
          (incf counter)  (set-market market)
          (setq *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir* *data-name*))
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
         (setq date-1 (getd date 'ydate) date-2 (getd date-1 'ydate))
       
      
        
        ;(setq cy2 (cy-dir market date))
       
        (setq bri (body-range-index date ))

        (setq chan  (channel-direction date 13))    

        (setq cr (wpp date))

        (multiple-value-setq (csignal p0)(cycle-signal date min-period max-period))
        (setq can (candle-composite date 3))
        
        (setq rel-s (gann-slope-index date 5 ))
        (setq trend (r-squared-change-index date 5 1))
      
        
        (setq vri (volatility-ratio-index date 1 14 1))
       ; (multiple-value-setq (mac-dir mac-dir1) (macd-direction date 5 34 5))
         (setq mac-dir (macd-direction date 12 26 9 2))
         (setq ldev (ldev-index date 5))
         (setq ldev10 (ldev-index date 10))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 4)
            (format stream " ~9@A  " (my-pretty-price (getd date 'close)))
            (format stream " ~3@A "  (if P0 P0 " "))
           ; (format stream "  ~3@A " cy2) 
          
            (format stream " ~3@A  " bri) 
            (format stream " ~3@A "  vri)
         ;   (format stream " ~3@A "  (if dfh  dfh  " "))
             (format stream "  ~4@A " chan)
             (format stream " ~4@A" cr)
           
            (format stream "  ~3@A " csignal)

            (format stream "  ~3@A " can)
            (format stream "  ~3@A" rel-s)
            (format stream " ~3@A " trend)
          ;  (format stream " ~3@A " rev)
            (format stream " ~2D  " mac-dir)
            (format stream " ~2D  " ldev)
            (format stream " ~2D  " ldev10)
      
            (if (zerop (mod counter 5)) (terpri stream))
          );;closes with open file


           )))
;;;;
#|
(defun osc1 (&optional tdate)
    (let  ((conv nil)
           (market-list  '(ad.d1b bp.d1b cd.d1b jy.d1b sf.d1b sp.d1b ty.d1b gc.d1b cf.d1b ct.d1b s.d1b w.d1b ho.d1b ng.d1b mx.d1b
                           hu.d1b lc.d1b lh.d1b pb.d1b bo.d1b  sm.d1b  c.d1b djia
                           oj.d1b su.d1b  cc.d1b si.d1b cp.d1b cl.d1b us.d1b)) date (counter 0))
            (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate)) (set-cat-list)
            (if  (probe-file "~/exitpoints/osc1.txt")
                  (delete-file "~/exitpoints/osc1.txt"))

            (with-open-file (stream "/home/register/cycles/osc1.txt"
                              :direction :output :if-exists :append :if-does-not-exist :create)
              (format stream "~A~%MKT    CLOSE   4DIR  9DIR    ENTRY6    ENTRY5       OBJ       AVE4    ENTRY7~%~%" (date-convert date))
             (dolist (market market-list)
              (incf counter)
              (setq *data-name* market  *time-interval* 'daily-high-low)
              (setq *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir* *data-name*))
              (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
              (let* ((obj (dev-objective .333 date 4 'pivot))(ave4 (ave date 4 'pivot))(ave5 (ave date 5 'pivot))
                     (ave9 (ave date 9 'pivot))(dev0 (dev-objective .667 date 4 'pivot))
                     (dev1 (dev-objective .667 date 9 'pivot))
                     (dirp4 (cond ((> (getd date 'close) ave5) 'UP)
                                ((< (getd date 'close) ave5) 'DOWN)
                                (t  'FLAT)))
                    (dirp9 (cond ((> (getd date 'close) ave9) 'UP)
                                ((< (getd date 'close) ave9) 'DOWN)
                                (t  'FLAT)))
                     (xentry (if (eql dirp4 'UP)(max (+ ave9 dev1)(+ ave4 dev0))
                                    (min (- ave9 dev1)(- ave4 dev0))))
                                )
                 (if (member *data-name* '(TY.D1B US.D1B)) (setq conv t)(setq conv nil))

                (print-string stream (format nil "~A" *data-name*) 3)
                (if conv (format stream " ~9,2,0,'*,' F" (convert-to-32nds (getd date 'close)))
                    (format stream " ~9,4,0,'*,' F" (getd date 'close)))
                (format stream "  ~4@A ~4@A"   dirp4 dirp9)
                (if conv (format stream " ~9,2,0,'*,' F" (convert-to-32nds (ave date 3 'pivot)))
                     (format stream "  ~9,4,0,'*,' F" (my-round (ave date 3 'pivot) 4)));;entry
                (if conv   (format stream "  ~9,2,0,'*,' F"
                                (convert-to-32nds (cond ((eql dirp9 'UP)(- ave4 obj));;;entry5
                                                        ((eql dirp9 'DOWN)(+ ave4 obj))
                                                        (t nil))))
                      (format stream "  ~9,4,0,'*,' F"
                              (my-round (cond ((eql dirp9 'UP)(- ave4 obj))
                                              ((eql dirp9 'DOWN)(+ ave4 obj))
                                              (t nil)) 4)))
                (if conv (format stream "  ~9,2,0,'*,' F"
                                (convert-to-32nds (cond ((eql dirp4 'UP) (+ ave4 obj));;objective
                                                        ((eql dirp4 'DOWN) (- ave4 obj))
                                                        (t nil))))
                     (format stream "  ~9,4,0,'*,' F"
                                (my-round (cond ((eql dirp4 'UP) (+ ave4 obj));;objective
                                                ((eql dirp4 'DOWN) (- ave4 obj))
                                                (t nil)) 4)))
                (if conv (format stream "  ~9,2,0,'*,' F" (convert-to-32nds ave4))
                     (format stream "  ~9,4,0,'*,' F"  (my-round ave4 4)))
                (if conv (format stream " ~9,2,0,'*,' F~%" (convert-to-32nds xentry))
                     (format stream " ~9,4,0,'*,' F~%" (my-round xentry 4)))
                (if (zerop (mod counter 5)) (terpri stream))))

                 )))


|#
#|
(defun osc2 (&optional tdate)
    (let  ((conv nil)
           (market-list '(ad.d1b bp.d1b cd.d1b jy.d1b sf.d1b sp.d1b ty.d1b gc.d1b cf.d1b ct.d1b s.d1b w.d1b ho.d1b ng.d1b mx.d1b
                                          hu.d1b lc.d1b lh.d1b pb.d1b bo.d1b  sm.d1b  c.d1b djia
                                          oj.d1b su.d1b  cc.d1b  si.d1b cp.d1b  cl.d1b us.d1b )) date (counter 0))
            (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate)) (set-cat-list)
            (if  (probe-file "/home/register/cycles/osc2.txt")
                  (delete-file "/home/register/cycles/osc2.txt"))

            (with-open-file (stream "/home/register/cycles/osc2.txt"
                              :direction :output :if-exists :append :if-does-not-exist :create)
              (format stream "~A~%MKT    CLOSE   4DIR    ENTRY15     OBJ  ~%~%" (date-convert date))
             (dolist (market market-list)
              (incf counter)
              (setq *data-name* market  *time-interval* 'daily-high-low)
              (setq *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir* *data-name*))
              (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
              (let* ((obj (dev-objective .25 date 4 'pivot))(ave5 (ave date 5 'pivot))
                     (ave3 (ave date 3 'pivot))
                     (dirp4 (cond ((> (getd date 'close) ave5) 'UP)
                                ((< (getd date 'close) ave5) 'DOWN)
                                (t  'FLAT)))
                                                   )
                 (if (member *data-name* '(TY.D1B US.D1B)) (setq conv t)(setq conv nil))

                (print-string stream (format nil "~A" *data-name*) 3)
                (if conv (format stream " ~9,2,0,'*,' F" (convert-to-32nds (getd date 'close)))
                    (format stream " ~9,4,0,'*,' F" (getd date 'close)))
                (format stream "  ~4@A"   dirp4 )

                (if conv (format stream "  ~9,2,0,'*,' F"
                                (convert-to-32nds (cond ((eql dirp4 'UP) (- ave3 obj));;entry
                                                        ((eql dirp4 'DOWN) (+ ave3 obj))
                                                        (t nil))))
                     (format stream "  ~9,4,0,'*,' F"
                                (my-round (cond ((eql dirp4 'UP) (- ave3 obj));;entry
                                                ((eql dirp4 'DOWN) (+ ave3 obj))
                                                (t nil)) 4)))
                (if conv (format stream "~9,2,0,'*,' F~%" (convert-to-32nds ave3))
                     (format stream " ~9,4,0,'*,' F~%" (my-round ave3 4)));;objective


                (if (zerop (mod counter 5)) (terpri stream))))

                 )))
|#
(defun cycles-d (date market &optional (min-period 10) (max-period 30) (outfile nil))
  (let (period0 dir0 dfl dfh )
         (setq *data-name* market)
         (setq *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir* *data-name*))
               (setq period0  (dominant-cycle date  min-period max-period))
         (if period0 (multiple-value-setq (dir0 dfl dfh ) (amplitude date period0)))

      (when outfile
         (with-open-file (stream (string-append "/users/ep-da/exitpoints/"
                                   (format nil "~A" market) ".dat") :direction :output :if-exists :append :if-does-not-exist :create)
            (format stream "~A~A" (date-convert date) #\tab)
            (format stream "~9,4,0,'*,' F~A" (- (getd (getd date 'ndate) 'close)(getd (getd date 'ndate) 'open)) #\tab)



;(format stream "~9,4,0,'*,' F~A" (if period0 (/  dfl period0) 0) #\tab)


            )
            );closes the when outfile

        (values dir0 dfl dfh period0)
   ))



(defun macd (tdate &optional (num1 12)(num2 26)(num3 9))
  (let ((date tdate) prices expave12 expave26 oscs macd-values signal-lines)
;;;first make the list of closing prices
    (dotimes (ith (* num2 8))
       (push (getd date 'close) prices)
       (setq date (getd date 'ydate)))
;;;prices ordered most recent last
;;;;compute the size1 exponential moving average
;;; 2/(n +1) is relationship of simple to exponential average
;;;;for n =12 the value is .15385
    (push (car prices) expave12)
    (dolist (ith (cdr prices))
      (push (+ (car expave12)(* (/ 2 (1+ num1)) (- ith (car expave12)))) expave12))

    (push (car prices) expave26)
    (dolist (ith (cdr prices))
      (push (+ (car expave26)(* (/ 2 (1+ num2)) (- ith (car expave26)))) expave26))
;;;expave12 and expave26 ordered most recent first
;;;;calulate the list of differences in exp averages
    (setq oscs (mapcar #'(lambda (s1 s2) (- s1 s2)) expave12 expave26))

;;;;calculate 9 day exp average of the oscs
    (setq oscs (reverse oscs));;;order most recent last;
    (push (car oscs) signal-lines)
    (dolist (ith (cdr oscs))
      (push (float (+ (car signal-lines)(* (/ 2 (1+ num3)) (- ith (car signal-lines))))) signal-lines))

;;;create macd values
    (setq oscs (reverse oscs));;;order of most recent first
    (setq macd-values (mapcar #'(lambda (s1 s2) (float (- s1 s2))) oscs signal-lines))
; (print macd-values)
 (values (first macd-values)
         (second macd-values)
         (third macd-values)
         (nth 3 macd-values)
         (nth 4 macd-values)
         (car signal-lines))
 ));;;closes the let and defun


;;;requires that the calling function have a special hashtable called MT
(defun fill-hash-macd (tdate num &optional (num1 5)(num2 34)(num3 5))
  (let ((sdate (add-mkt-days tdate (- num))) date prices expave12 expave26 oscs macd-values signal-lines)
  (declare (special MT))
     (setq date sdate)
;;;first make the list of closing prices
    (dotimes (ith (* num2 5))
       (push (getd date 'close) prices)
       (setq date (getd date 'ydate)))
;;;prices ordered most recent last
;;;;compute the size1 exponential moving average
;;; 2/(n +1) is relationship of simple to exponential average
;;;;for n =12 the value is .15385
    (push (car prices) expave12)
    (dolist (ith (cdr prices))
      (push (float (+ (car expave12)(* (/ 2 (1+ num1)) (- ith (car expave12))))) expave12))

    (push (car prices) expave26)
    (dolist (ith (cdr prices))
      (push (float (+ (car expave26)(* (/ 2 (1+ num2)) (- ith (car expave26))))) expave26))
;;;expave12 and expave26 ordered most recent first
;;;;calulate the list of differences in exp averages
    (setq oscs (mapcar #'(lambda (s1 s2) (- s1 s2)) expave12 expave26))

;;;;calculate 9 day exp average of the oscs
    (setq oscs (reverse oscs));;;order most recent last;
    (push (car oscs) signal-lines)
    (dolist (ith (cdr oscs))
      (push (float (+ (car signal-lines)(* (/ 2 (1+ num3)) (- ith (car signal-lines))))) signal-lines))

;;;create macd values
    (setq oscs (reverse oscs));;;order of most recent first
    (setq macd-values (mapcar #'(lambda (s1 s2) (- s1 s2)) oscs signal-lines))

 ;   (setf (gethash sdate MT)(- (first macd-values)(second macd-values)))
    (setf (gethash sdate MT) (first macd-values))
 ;;;;At this point we have the first macd-direction for the sdate
    (setq date sdate)
    (loop
;;;;need to add a day
    (setq date (getd date 'ndate))
;;;need to update expave12
    (push (float (+ (car expave12)(* (/ 2 (1+ num1)) (- (getd date 'close) (car expave12))))) expave12)
    (push (float (+ (car expave26)(* (/ 2 (1+ num2)) (- (getd date 'close) (car expave26))))) expave26)
    (push (float (+ (car signal-lines)(* (/ 2 (1+ num3)) (- (- (car expave12)(car expave26)) (car signal-lines))))) signal-lines)
    (push (float (- (- (car expave12)(car expave26)) (car signal-lines))) macd-values)
    (setf (gethash date MT)(first macd-values))
    (if (equal date tdate) (return)))

  ));;;closes the let and defun

;;;requires that the calling function have a special hashtable called MT
(defun fill-hash-macd-signal-line (tdate num &optional (num1 5)(num2 34)(num3 5))
  (let ((sdate (add-mkt-days tdate (- num))) date prices expave12 expave26 oscs signal-lines)
  (declare (special MT))
     (setq date sdate)
;;;first make the list of closing prices
    (dotimes (ith (* num2 5))
       (push (getd date 'close) prices)
       (setq date (getd date 'ydate)))
;;;prices ordered most recent last
;;;;compute the size1 exponential moving average
;;; 2/(n +1) is relationship of simple to exponential average
;;;;for n =12 the value is .15385
    (push (car prices) expave12)
    (dolist (ith (cdr prices))
      (push (float (+ (car expave12)(* (/ 2 (1+ num1)) (- ith (car expave12))))) expave12))

    (push (car prices) expave26)
    (dolist (ith (cdr prices))
      (push (float (+ (car expave26)(* (/ 2 (1+ num2)) (- ith (car expave26))))) expave26))
;;;expave12 and expave26 ordered most recent first
;;;;calulate the list of differences in exp averages
    (setq oscs (mapcar #'(lambda (s1 s2) (- s1 s2)) expave12 expave26))

;;;;calculate 9 day exp average of the oscs
    (setq oscs (reverse oscs));;;order most recent last;
    (push (car oscs) signal-lines)
    (dolist (ith (cdr oscs))
      (push (float (+ (car signal-lines)(* (/ 2 (1+ num3)) (- ith (car signal-lines))))) signal-lines))


    (setf (gethash sdate MT)(first signal-lines))
 ;;;;At this point we have the first signal line for the sdate
    (setq date sdate)
    (loop
;;;;need to add a day
    (setq date (getd date 'ndate))
;;;need to update expave12
    (push (float (+ (car expave12)(* (/ 2 (1+ num1)) (- (getd date 'close) (car expave12))))) expave12)
    (push (float (+ (car expave26)(* (/ 2 (1+ num2)) (- (getd date 'close) (car expave26))))) expave26)
    (push (float (+ (car signal-lines)(* (/ 2 (1+ num3)) (- (- (car expave12)(car expave26)) (car signal-lines))))) signal-lines)

    (setf (gethash date MT)(first signal-lines))
    (if (equal date tdate) (return)))

  ));;;closes the let and defun

;;;requires that the calling function have a special hashtable called MT
(defun fill-hash-macd-direction (tdate num &optional (num1 5)(num2 34)(num3 5))
  (let ((sdate (add-mkt-days tdate (- num))) date prices expave12 expave26 oscs macd-values signal-lines)
  (declare (special MT))
     (setq date sdate)
;;;first make the list of closing prices
    (dotimes (ith (* num2 5))
       (push (getd date 'close) prices)
       (setq date (getd date 'ydate)))
;;;prices ordered most recent last
;;;;compute the size1 exponential moving average
;;; 2/(n +1) is relationship of simple to exponential average
;;;;for n =12 the value is .15385
    (push (car prices) expave12)
    (dolist (ith (cdr prices))
      (push (+ (car expave12)(* (/ 2 (1+ num1)) (- ith (car expave12)))) expave12))

    (push (car prices) expave26)
    (dolist (ith (cdr prices))
      (push (+ (car expave26)(* (/ 2 (1+ num2)) (- ith (car expave26)))) expave26))
;;;expave12 and expave26 ordered most recent first
;;;;calulate the list of differences in exp averages
    (setq oscs (mapcar #'(lambda (s1 s2) (- s1 s2)) expave12 expave26))

;;;;calculate 9 day exp average of the oscs
    (setq oscs (reverse oscs));;;order most recent last;
    (push (car oscs) signal-lines)
    (dolist (ith (cdr oscs))
      (push (+ (car signal-lines)(* (/ 2 (1+ num3)) (- ith (car signal-lines)))) signal-lines))

;;;create macd values
    (setq oscs (reverse oscs));;;order of most recent first
    (setq macd-values (mapcar #'(lambda (s1 s2) (- s1 s2)) oscs signal-lines))

    (setf (gethash sdate MT)(- (first macd-values)(second macd-values)))
 ;;;;At this point we have the first macd-direction for the sdate
    (setq date sdate)
    (loop
;;;;need to add a day
    (setq date (getd date 'ndate))
;;;need to update expave12
    (push (+ (car expave12)(* (/ 2 (1+ num1)) (- (getd date 'close) (car expave12)))) expave12)
    (push (+ (car expave26)(* (/ 2 (1+ num2)) (- (getd date 'close) (car expave26)))) expave26)
    (push (+ (car signal-lines)(* (/ 2 (1+ num3)) (- (- (car expave12)(car expave26)) (car signal-lines)))) signal-lines)
    (push (- (- (car expave12)(car expave26)) (car signal-lines)) macd-values)
    (setf (gethash date MT)(- (first macd-values)(second macd-values)))
    (if (equal date tdate) (return)))

  ));;;closes the let and defun


(defun fill-hash-macd-signal (tdate num &optional (num1 5)(num2 34)(num3 5))
  (let ((sdate (add-mkt-days tdate (- num))) date prices expave12 expave26 oscs macd-values signal-lines
          mac0 mac1 mac2 mac3 mac-s)
  (declare (special MT))
     (setq date sdate)
;;;first make the list of closing prices
    (dotimes (ith (* num2 5))
       (push (getd date 'close) prices)
       (setq date (getd date 'ydate)))
;;;prices ordered most recent last
;;;;compute the size1 exponential moving average
;;; 2/(n +1) is relationship of simple to exponential average
;;;;for n =12 the value is .15385
    (push (car prices) expave12)
    (dolist (ith (cdr prices))
      (push (+ (car expave12)(* (/ 2 (1+ num1)) (- ith (car expave12)))) expave12))

    (push (car prices) expave26)
    (dolist (ith (cdr prices))
      (push (+ (car expave26)(* (/ 2 (1+ num2)) (- ith (car expave26)))) expave26))
;;;expave12 and expave26 ordered most recent first
;;;;calulate the list of differences in exp averages
    (setq oscs (mapcar #'(lambda (s1 s2) (- s1 s2)) expave12 expave26))

;;;;calculate 9 day exp average of the oscs
    (setq oscs (reverse oscs));;;order most recent last;
    (push (car oscs) signal-lines)
    (dolist (ith (cdr oscs))
      (push (+ (car signal-lines)(* (/ 2 (1+ num3)) (- ith (car signal-lines)))) signal-lines))

;;;create macd values
    (setq oscs (reverse oscs));;;order of most recent first
    (setq macd-values (mapcar #'(lambda (s1 s2) (- s1 s2)) oscs signal-lines))
    (setq mac0 (car macd-values) mac1 (second macd-values) mac2 (third macd-values) mac3 (nth 3 macd-values))

    (setq mac-s
          (cond ((and (plusp mac0)(minusp mac1)
                    (minusp mac3)) 1)
               ((and (minusp mac0)(plusp mac1)
                    (plusp mac3)) -1)
               ((and (plusp mac1)(minusp mac2)
                     (minusp mac3)) 2)
               ((and (minusp mac1)(plusp mac2)
                     (plusp mac3)) -2)
               (t 0)))

      (setf (gethash sdate MT)
            (cond ((member mac-s '(1 2)) 'DN)
                  ((member mac-s '(-1 -2)) 'UP)
                  (t 'FT)))

 ;;;;At this point we have the first macd-signal for the sdate
    (setq date sdate)
    (loop
;;;;need to add a day
    (setq date (getd date 'ndate))
;;;need to update expave12
    (push (+ (car expave12)(* (/ 2 (1+ num1)) (- (getd date 'close) (car expave12)))) expave12)
    (push (+ (car expave26)(* (/ 2 (1+ num2)) (- (getd date 'close) (car expave26)))) expave26)
    (push (+ (car signal-lines)(* (/ 2 (1+ num3)) (- (- (car expave12)(car expave26)) (car signal-lines)))) signal-lines)
    (push (- (- (car expave12)(car expave26)) (car signal-lines)) macd-values)
    (setq mac0 (car macd-values) mac1 (second macd-values) mac2 (third macd-values) mac3 (nth 3 macd-values))
    (setq mac-s
        (cond ((and (plusp mac0)(minusp mac1)
                    (minusp mac3)) 1)
               ((and (minusp mac0)(plusp mac1)
                    (plusp mac3)) -1)
               ((and (plusp mac1)(minusp mac2)
                     (minusp mac3)) 2)
               ((and (minusp mac1)(plusp mac2)
                     (plusp mac3)) -2)
               (t 0)))

     (setf (gethash date MT)
           (cond ((member mac-s '(1 2)) 'DN)
                 ((member mac-s '(-1 -2)) 'UP)
                 (t 'FT)))

    (if (equal date tdate) (return)))

  ));;;closes the let and defun
#|
(defun commodity-channel-index (tdate &optional (size 20))
  (let ((date (add-mkt-days tdate (- 1 size))) (md 0))

;;;compute the mean deviation
  (dotimes (ith size)
    (setq md (+ (abs (- (ave (add-mkt-days date ith) 1 'pivot)(ave (add-mkt-days date ith) size 'pivot))) md)))
  (setq md (/ md size))

 (round (/ (- (ave tdate 1 'pivot)(ave tdate size 'pivot)) (* .015 md)))

 ))
|#

;;;this is the trend balance point system in Welles Wilder book
;;;I generalized it to apply to longer time frames than just 2 day rate of change
(defun tbps-signal (date &optional (size 2) (func #'roc))
  (let (rocs epsignal target stop ydate midpoint)

  (dotimes (ith (1+ size))
     (push (funcall func (add-mkt-days date (- ith size)) size) rocs))


   (setq epsignal
     (cond ((>= (car rocs) (apply #'max rocs)) 'UP)
           ((<= (car rocs) (apply #'min rocs)) 'DN)
           (t nil)))
   (setq  ydate date)
   (loop
     (setq ydate (getd ydate 'ydate))
     (unless epsignal (setq epsignal (tbps-signal ydate size)))
     (if epsignal (return)))
   (when epsignal  (setq midpoint (/ (+ (getd date 'high)(getd date 'low)(getd date 'close)) 3)))
   (if (eql epsignal 'UP)(setq target (- (* 2 midpoint) (getd date 'low))))
   (if (eql epsignal 'DN)(setq target (- (* 2 midpoint) (getd date 'high))))

   (if (eql epsignal 'UP) (setq stop (- midpoint (* 1 (true-range date)))))
   (if (eql epsignal 'DN) (setq stop (+ midpoint (* 1 (true-range date)))))

  (values epsignal target stop)))

(defun back-count (date0 filter)
  (let ((filt *n-filt*)  prices times turns matches matches1
        (days (* 4 filter))
        (fibo-nums '(5 8 13 21 34 55 89)))

   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))
   (setq *n-filt* filter)
   (nil-tpv)

   (loop
    (multiple-value-setq (prices times)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
    (setq prices (butlast prices) times (butlast times))
    (if (< (length prices) 3)
      (setq days (* 2 days))(return))
         )

   (setq *n-filt* filt)

;;;;calculate the fibonacci backward count for trend change
;;;first convert the times for the highs and lows to a list of days from today
      (setq turns (mapcar #'(lambda (s) (sub-mkt-dates (getnumdate s) date0)) times))
      (setq matches (intersection turns fibo-nums :test #'=))
  ;;;need to remove duplicate days
     (dolist (ith matches)
       (pushnew ith matches1))
      (setq matches matches1)

      matches
                 ))

;;;ratio of the net price length of the most recent completed three waves
;;;divided by the sum of the price lengths of the three waves
(defun zigzag-index (date0 filter)
 (let ((filt *n-filt*) prices times  (days (* 5 filter)))
  (setq *n-filt* filter)
   (nil-tpv)
   (loop
    (multiple-value-setq (prices times)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
    (setq prices (butlast prices) times (butlast times))
    (if (< (length prices) 4)
      (setq days (+ 2 days))(return))
         )
    (setq *n-filt* filt)

    (float (/ (- (first prices)(fourth prices))
             (+ (- (first prices)(second prices))
                (- (- (second prices)(third prices)));;reverse sign second wave
                (- (third prices)(fourth prices)))
      ))


 ))

(defun matches (tdate)
 (let (match-list matches1)
 (dolist (ith '(13 21 34 55 89 144))
    (setq match-list (append (back-count tdate ith) match-list)))
 (dolist (ith match-list)
     (pushnew ith matches1))
 (setq match-list matches1)
 (length match-list)
 ))

(defun turnpoint-index (date)
   (let ((pin (pinpoint date))(period (cycle-dif-oc date)) period-ave half-ave
          period-ave-1 half-ave-1 (date-1 (getd date 'ydate)))
   
   (setq period-ave (ave date period 'close) half-ave (ave date (truncate period 2) 'close)
         period-ave-1 (ave date-1 period 'close) half-ave-1 (ave date-1 (truncate period 2) 'close))
 
  (cond ((and (< half-ave-1 period-ave-1)(> half-ave period-ave) (zerop pin)) -2) 
        ((and (< half-ave-1 period-ave-1)(> half-ave period-ave) pin) -3)
        ((and (> half-ave-1 period-ave-1)(< half-ave period-ave) (zerop pin)) 2)
        ((and (> half-ave-1 period-ave-1)(< half-ave period-ave) pin) 3) 
       
        ((and (>= half-ave-1 period-ave-1)(>= half-ave period-ave) pin) -1)
        ((and (<= half-ave-1 period-ave-1)(<= half-ave period-ave)  pin) 1)  

        ((zerop pin) 0))
        
))


(defun time-cycle-ratios (&optional date0)
 (let ((filt *n-filt*) prices times proj-list t1 t2 t3 t4 t5
       (fibonacci-numbers '(5 8 13 21 ))
       ;(fibonacci-numbers '(21 55 ))
        used (counter 0)
       date0-t1 date0-t3 date0-t2  date0-t5 p1 p2 p3 p4 p5
       t2-t1 t3-t2 t4-t3 t5-t4 days)

   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))
   (nil-tpv)


   (dolist (kth fibonacci-numbers)

    (setq days (* 5 kth) *n-filt* kth)

    (multiple-value-setq (prices times)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))

   (loop
      (setq days (+ 5 days))
     (if (< (length times) 7)
         (multiple-value-setq (prices times)
           (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
         (return)))

   (setq times (butlast (mapcar #'getnumdate times)) prices (butlast prices))

;;;;now we need to iterate

    (loop
        (if (< (length times) 6) (return))
        (if (> counter 4)(return));;;limits to four loops
        (setq t1 (first times) t2 (second times) t3 (third times)
              date0-t1 (sub-mkt-dates t1 date0) t4 (nth 3 times)
              t5 (nth 4 times)
              t3-t2 (sub-mkt-dates t3 t2) t2-t1 (sub-mkt-dates t2 t1)
              t4-t3 (sub-mkt-dates t4 t3) t5-t4 (sub-mkt-dates t5 t4)
              date0-t2 (+ date0-t1  t2-t1) date0-t3 (+ date0-t1 t2-t1 t3-t2)
              date0-t5 (+ date0-t1 t2-t1 t3-t2 t4-t3 t5-t4)
              p1 (first prices) p2 (second prices) p3 (third prices)
              p4 (nth 3 prices) p5 (nth 4 prices)
        )

;;;;technique II
        (ifn (member (sxhash (list t3 t2)) used)
        (dolist (ith '(1.309 1.618 2 2.618 4.236))
            (if (or (and (> p2 p1)(> p1 p3)) (and (< p2 p1)(< p1 p3)))
            (push (round (- (* ith t3-t2) date0-t3)) proj-list))))
;;;;technique IIa
          (ifn (member (sxhash (list t4 t3 t2 t1)) used)
        (dolist (ith '(1.309 1.618 2 2.618 4.236))
            (if (or (and (> p1 p3)(> p2 p4)(> p3 p5)) (and (< p1 p3)(< p2 p4)(< p3 p5)))
            (push (round (- (* ith (+ t5-t4 t4-t3 t3-t2)) date0-t5)) proj-list))))

;;;technique III
        (ifn (member (sxhash (list t2 t1)) used)
          (if (or (and (> p1 p3)(< p1 p2))(and (< p1 p3)(> p1 p2)))
        (dolist (ith '(1.309 1.618 2 2.618 4.236))
            (push (round (- (* ith t2-t1) date0-t2)) proj-list))))
;;;technique IIIa
        (ifn (member (sxhash (list t4 t3 t2 t1)) used)
        (dolist (ith '(1.309 1.618 2 2.618 4.236))
            (if (or (and (< p1 p2)(< p1 p3)(< p2 p4))(and (> p1 p2)(> p1 p3)(> p2 p4)))
            (push (round (- (* ith (+ t4-t3 t3-t2 t2-t1)) date0-t2)) proj-list))))

;;;technique IV  (includes Williams 1.618 propation)
        (ifn (member (sxhash (list t3 t2 t1)) used)
        (dolist (ith '(1.309 1.618 2 2.618 4.236))
            (if (or (and (> p1 p3)(> p1 p2)) (and (< p1 p3)(< p1 p2)))
            (push (round (- (* ith (+ t3-t2 t2-t1)) date0-t3)) proj-list))))
;;;technique IVa
        (ifn (member (sxhash (list t5 t4 t3 t2 t1)) used)
        (dolist (ith '(1.309 1.618 2 2.618 4.236))
        (if (or (and (> p1 p3)(> p1 p2)(> p3 p5))(and (< p1 p3)(< p1 p2)(< p3 p5)))
            (push (round (- (* ith (+ t5-t4 t4-t3 t3-t2 t2-t1)) date0-t5)) proj-list))))

;;;technique V
         (ifn (member (sxhash (list t3 t2 t1 )) used)
        (dolist (jth '(.382 .50 .618 1 1.618 2.618 4.236))
            (if (or (and (> p2 p1)(> p1 p3))(and (< p2 p1)(< p1 p3)))
            (push (round (- (* jth t3-t2) date0-t1)) proj-list))))
;;;technique Va
         (ifn (member (sxhash (list t5 t4 t3 t2 t1 )) used)
        (dolist (jth '(.382 .50 .618 1 1.618 2.618 4.236))
            (if (or (and (> p1 p3)(> p2 p4)(> p3 p5)) (and (< p1 p3)(< p2 p4)(< p3 p5)))
            (push (round (- (* jth (+ t5-t4 t4-t3 t3-t2)) date0-t1)) proj-list))))


;;;Williams 1.28 propagation From high between low-to-low and low between high-to-high
        (ifn (member (sxhash (list t3 t2 t1)) used)
        (dolist (ith '(1.28))
            (push (round (- (* ith (+ t3-t2 t2-t1)) date0-t2)) proj-list)))


         (push (sxhash (list t3 t2)) used)
         (push (sxhash (list t2 t1)) used)
         (push (sxhash (list t3 t2 t1)) used)
         (push (sxhash (list t4 t3 t2 t1)) used)
         (push (sxhash (list t5 t4 t3 t2 t1)) used)

        (setq times (cdr times) counter (1+ counter))

       );;;closes the loop

  );;;closes the dolist

    (setq *n-filt* filt)
    (setq proj-list (sort proj-list '< ))
    (setq proj-list (remove-if #'(lambda (s) (or (< s -1)(> s 0))) proj-list))
    (length proj-list)

     ))

;;;returns the number of target zones the extreme price is within.

(defun fibonacci-target-zones (&optional date0 )
 (let ((filt *n-filt*)  prices proj-list ignore p1 p2 p3 p4 p5 (counter 0)
       (fibonacci-numbers '(8 13 21 34   )) used 2high 2low days proj-list1)
   
   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))
   (nil-tpv)
   (dolist (kth fibonacci-numbers)
    (setq days (* 10 kth) *n-filt* kth)
    (multiple-value-setq (prices ignore)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
   (loop
     (setq days (* 2 days))
     (if (< (length prices) 7)
         (multiple-value-setq (prices ignore)
           (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
         (return))
         )
    (setq prices (butlast prices))
   (setq *n-filt* filt)

;;;;now we need to iterate

    (loop
        (if (< (length prices) 6) (return))
        (if (> counter 2)(return));;;limits to three loops
        (setq p1 (first prices) p2 (second prices) p3 (third prices)
              p4 (nth 3 prices) p5 (nth 4 prices)
                 )
;;;;Robert Fischer's relationship of 5 to 0->3
        (ifn (member (sxhash (list p5 p4 p3 p2 p1)) used)
        (dolist (jth '(.618 1.618))
            (cond ((and (> p2 p1)(> p1 p3)(> p4 p3) (> p3 p5))
                   (push (list (+ p2 (* .97 jth (- p2 p5))) (+ p2 (* 1.03 jth (- p2 p5)))) proj-list))
                  ((and (< p2 p1)(< p1 p3)(< p4 p3)(< p3 p5))
                   (push (list (- p2 (* 1.03 jth (- p5 p2))) (- p2 (* .97 jth (- p5 p2)))) proj-list))
                  (t nil)))
       )
;;;;Technique I of Turn Points by Joseph Duffy
        (ifn (member (sxhash (list p3 p2 p1)) used)
        (dolist (jth '(.382 .500 .618))
            (cond ((and (> p1 p2)(> p1 p3))
                   (push (list (- p1 (* 1.03 jth (- p1 p2))) (- p1 (* .97 jth (- p1 p2)))) proj-list))
                  ((and (< p1 p2)(< p1 p3))
                   (push (list (+ p1 (* .97 jth (- p2 p1))) (+ p1 (* 1.03 jth (- p2 p1)))) proj-list))
                  (t nil)))
           )
;;;;;Technique Ia   retracement of 0-3
          (ifn (member (sxhash (list p4 p3 p2 p1)) used)
              (dolist (jth '(382 .500 .618))
              (cond ((and (> p1 p2)(> p1 p3)(> p2 p4))
                    (push (list (- p1 (* .97 jth (- p1 p4))) (- p1 (* 1.03 jth (- p1 p4)))) proj-list))
                  ((and (< p1 p2)(< p1 p3)(< p2 p4))
                   (push (list (+ p1 (* 1.03 jth (- p4 p1))) (+ p1 (* .97 jth (- p4 p1)))) proj-list))
                  (t nil)))
       )
;;;;Technique II
        (ifn (member (sxhash (list p3 p2 p1)) used)
        (dolist (ith '(.618 1.309 1.618 2 4.236))
            (cond ((and (> p2 p1)(> p1 p3))
                   (push (list (+ p3 (* .97 ith (- p2 p3))) (+ p3 (* 1.03 ith (- p2 p3)))) proj-list))
                  ((and (< p2 p1)(< p1 p3))
                    (push (list (- p3 (* 1.03 ith (- p3 p2))) (- p3 (* .97 ith (- p3 p2)))) proj-list))
                  (t nil)))
       )
;;;Technique III
        (ifn (member (sxhash (list p3 p2 p1)) used)
        (dolist (jth '(.618 1.309 1.618 2 4.236))
            (cond ((and (> p1 p2)(> p1 p3))
                   (push (list (- p1 (* 1.03 jth (- p1 p2))) (- p1 (* .97 jth (- p1 p2)))) proj-list))
                  ((and (< p1 p2)(< p1 p3))
                   (push (list (+ p1 (* .97 jth (- p2 p1))) (+ p1 (* 1.03 jth (- p2 p1)))) proj-list))
                  (t nil)))
           )
;;;;Technique IV  low-to-low and high-to-high
        (ifn (member (sxhash (list p3 p2 p1)) used)
        (dolist (ith '(.618 1.309 1.618 2 4.236))
            (cond ((and (> p2 p1)(> p1 p3))
                   (push (list (+ p3 (* .97 ith (- p1 p3))) (+ p3 (* 1.03 ith (- p1 p3)))) proj-list))
                  ((and (< p2 p1)(< p1 p3))
                    (push (list (- p3 (* 1.03 ith (- p3 p1))) (- p3 (* .97 ith (- p3 p1)))) proj-list))
                  (t nil)))
            )
;;;Technique V

          (ifn (member (sxhash (list p3 p2 p1)) used)
        (dolist (ith '(.382 .5 .618 1.0 1.618  2.618 4.236))
            (cond ((and (> p2 p1)(> p1 p3))
                   (push (list (+ p1 (* .97 ith (- p2 p3))) (+ p1 (* 1.03 ith (- p2 p3)))) proj-list))
                  ((and (< p2 p1)(< p1 p3))
                    (push (list (- p1 (* 1.03 ith (- p3 p2))) (- p1 (* .97 ith (- p3 p2)))) proj-list))
                  (t nil)))
            )

         (push (sxhash (list p5 p4 p3 p2 p1)) used)
         (push (sxhash (list p4 p3 p2 p1)) used)
         (push (sxhash (list p3 p2 p1)) used)
        (setq prices (cdr prices) counter (1+ counter))
       );;;closes the loop
    ;  (format T "~% ~A~%" proj-list)

       (setq 2high (getv 0 HP) 2low (getv 0 LP))
    (if (> p2 p1)
       (setq proj-list1 (append proj-list1 (mapcar #'(lambda(s) (and (>= 2high (car s))(<= 2high (cadr s)))) proj-list)))
       (setq proj-list1 (append proj-list1 (mapcar #'(lambda(s) (and (>= 2low (car s))(<= 2low (cadr s)))) proj-list))))
    (setq proj-list1 (remove nil proj-list1) proj-list nil)
    );;;closes the dolist
     (length proj-list1) 

     ))

(defun fibonacci-retracement (date0 size)
  (let ((filt *n-filt*) prices proj-list days  p0 p1 p2)
    (nil-tpv)
  (setq days (* 10 size)) (setq *n-filt* size)
  (multiple-value-setq (prices)(find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
  (setq p0 (if (eql (getv 0 'dr) 'up) (getv 0 'hp)(getv 0 'lp)))
  (setq p1 (car prices) p2 (second prices)) (setq *n-filt* filt)
  (my-round  (/ (- p1 p0) (- p1 p2)) 3)
  ))
  
    
(defun fibonacci-turns (&optional date0 )
 (let ((filt *n-filt*)  prices proj-list ignore p0 p1 p2 p3 p4 p5 (counter 0)
       (fibonacci-numbers '(10)) used  days ;proj-list1 ext
       tol)
   
   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))
   (nil-tpv) (setq tol (pivot-tol date0))

   (dolist (kth fibonacci-numbers)
    (setq days (* 10 kth) *n-filt* kth)
    (multiple-value-setq (prices ignore)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
;   (loop
;     (setq days (* 2 days))
;     (if (< (length prices) 7)
;         (multiple-value-setq (prices ignore)
;           (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
;         (return))
;         )
    (setq prices (butlast prices))
   (setq *n-filt* filt)
  ; (format T "~%Prices= ~A~%" prices)
;;;;now we need to iterate

    (loop
        (if (< (length prices) 6) (return))
        (if (> counter 0)(return));;;limits to three loops
        (setq p1 (first prices) p2 (second prices) p3 (third prices)
              p4 (nth 3 prices) p5 (nth 4 prices) p0 (if (eql (getv 0 DR) 'up) (getv 0 HP)(getv 0 LP)) 
                 )
;;;;Robert Fischer's relationship of 5 to 0->3
 ;       (ifn (member (sxhash (list p5 p4 p3 p2 p1)) used)
 ;       (dolist (jth '(.618 1.618))
 ;           (cond ((and (> p2 p1)(> p1 p3)(> p4 p3) (> p3 p5))
 ;                  (push (list (+ p2 (* .97 jth (- p2 p5))) (+ p2 (* 1.03 jth (- p2 p5)))) proj-list))
 ;                 ((and (< p2 p1)(< p1 p3)(< p4 p3)(< p3 p5))
 ;                  (push (list (- p2 (* 1.03 jth (- p5 p2))) (- p2 (* .97 jth (- p5 p2)))) proj-list))
 ;                 (t nil)))
 ;      )
;;;;Technique I of Turn Points by Joseph Duffy
;;;;these are retracements of the most recent move.
        (ifn (member (list p1 p2 p3) used :test #'equal)
        (dolist (jth '(.382 .500 .618)) ;1.0 1.382 1.618))
            (cond ((and (> p1 p2)(> p0 (- p2 tol)))
                   (push  (- p1 (* jth (- p1 p2))) proj-list))
                  ((and (< p1 p2) (< p0 (+ p2 tol)))
                   (push (+ p1 (* jth (- p2 p1))) proj-list))
                  (t nil)))
          
           )
       
;;;;;Technique Ia   retracement of 0-3
;          (ifn (member (sxhash (list p4 p3 p2 p1)) used)
;              (dolist (jth '(.382 .500 .618))
;              (cond ((and (> p1 p2)(> p1 p3)(> p2 p4))
;                    (push (list (- p1 (* .97 jth (- p1 p4))) (- p1 (* 1.03 jth (- p1 p4)))) proj-list))
;                  ((and (< p1 p2)(< p1 p3)(< p2 p4))
;                   (push (list (+ p1 (* 1.03 jth (- p4 p1))) (+ p1 (* .97 jth (- p4 p1)))) proj-list))
;                  (t nil)))
;       )
;;;;Technique II
;        (ifn (member (sxhash (list p3 p2 p1)) used)
;        (dolist (ith '(.618 1.309 1.618 2 4.236))
;            (cond ((and (> p2 p1)(> p1 p3))
;                   (push (list (+ p3 (* .97 ith (- p2 p3))) (+ p3 (* 1.03 ith (- p2 p3)))) proj-list))
;                  ((and (< p2 p1)(< p1 p3))
;                    (push (list (- p3 (* 1.03 ith (- p3 p2))) (- p3 (* .97 ith (- p3 p2)))) proj-list))
;                  (t nil)))
;       )
;;;Technique III
;;;these are the extensions
#|
        (unless (member (list p1 p2 p3) used :test #'equal)
        (dolist (jth '(.618 1.0 1.382 1.618 2 2.618 4.236))
            (cond ((and (> p1 p2)(< p1 p3))
                   (push (- p1 (* jth (- p3 p2))) proj-list))
                  ((and (< p1 p2)(> p1 p3))
                   (push (+ p1 (* jth (- p2 p3))) proj-list))
                  (t nil)))
          
           )
  |#     
;;;;Technique IV  low-to-low and high-to-high
;        (ifn (member (sxhash (list p3 p2 p1)) used)
;        (dolist (ith '(.618 1.309 1.618 2 4.236))
;            (cond ((and (> p2 p1)(> p1 p3))
;                   (push (list (+ p3 (* .97 ith (- p1 p3))) (+ p3 (* 1.03 ith (- p1 p3)))) proj-list))
;                  ((and (< p2 p1)(< p1 p3))
;                    (push (list (- p3 (* 1.03 ith (- p3 p1))) (- p3 (* .97 ith (- p3 p1)))) proj-list))
;                  (t nil)))
;            )
;;;Technique V

 ;         (ifn (member (sxhash (list p3 p2 p1)) used)
 ;       (dolist (ith '(.382 .5 .618 1.0 1.618  2.618 4.236))
 ;           (cond ((and (> p2 p1)(> p1 p3))
 ;                  (push (list (+ p1 (* .97 ith (- p2 p3))) (+ p1 (* 1.03 ith (- p2 p3)))) proj-list))
 ;                 ((and (< p2 p1)(< p1 p3))
 ;                   (push (list (- p1 (* 1.03 ith (- p3 p2))) (- p1 (* .97 ith (- p3 p2)))) proj-list))
 ;                 (t nil)))
 ;           )

        ; (push  (list p5 p4 p3 p2 p1) used)
       ;  (push  (list p4 p3 p2 p1) used)
         (push  (list p1 p2 p3) used)
     ;  (format T "~%used= ~A proj= ~A~%" (car used) proj-list)
        (setq prices (cdr prices) counter (1+ counter))
       );;;closes the loop
      (vsort proj-list #'< #'identity)
    

     ;  (if (eql (getv 0 DR) 'UP)(setq ext (getv 0 HP)) (setq ext (getv 0 LP)))
   
     ;  (setq proj-list1 (append proj-list1 (mapcar #'(lambda(s)
     ;                                         (and (<= ext (+ tol s))(>= ext (- s tol)))) proj-list)))
       
    ;(setq proj-list1 (remove nil proj-list1) proj-list nil)
    );;;closes the dolist
    (values  proj-list tol)

     ))
;;;;works with parabolic stop to determine how many 
;;;;times has there been a fibonnacci projection/pivot target hit at the extreme price
;;;positive for lows and negative for highs
(defun paraturns (tdate )
   (let (proj-list tol lw dir sdate ct)
       (multiple-value-setq (dir sdate )(parabolic-stops tdate))
       (multiple-value-setq (proj-list tol)(fibonacci-turns tdate))
       (setq proj-list (append (pivot-points1 tdate 'month)
                               (pivot-points1 tdate 'week)
                                proj-list))
       (setq lw (if (eql dir 'SHORT)(n-day-low tdate (1+ (sub-mkt-dates sdate tdate)))
                      (n-day-high tdate (1+ (sub-mkt-dates sdate tdate)))))
       (setq ct (count-if #'(lambda(s) (and (> lw (- s tol))(< lw (+ s tol)))) proj-list))
       (if (eql dir 'SHORT) ct (- ct))
))
;;;returns plus number if low
;;;returns negative number if high
;;;0 if not a turn       
(defun price-turns (tdate)
  (let (proj-list (tol (pivot-tol tdate)) (ct 0) (phigh (getd tdate 'high))(plow (getd tdate 'low)))
    ;(multiple-value-setq (proj-list tol)(fibonacci-turns tdate))
       (setq proj-list (append ;(pivot-points1 tdate 'month); (zero-proj tdate 4)
                               ;(pivot-points1 tdate 'week)
			       (channel-proj tdate 10)(channel-proj tdate 20)
			      ; (2-bar tdate)
                                proj-list))
       (vsort proj-list #'< )
       (if (= phigh (n-day-high tdate 9))
           (setq ct (- (count-if #'(lambda(s) (and (> phigh (- s tol))(< phigh (+ s tol)))) proj-list))))
       (if (= plow (n-day-low tdate 9))
          (setq ct (+ ct (count-if #'(lambda(s) (and (> plow (- s tol))(< plow (+ s tol)))) proj-list))))
   (values ct proj-list)
))    

(defun cycle-lgths (market stop-time)
  (let (lgths prices min-low max-high conv (filt *n-filt*) periods longest (data-lgth 252))
  (setq *n-filt* 9 *time-interval* 'daily-high-low )
  (set-cat-list)(nil-tpv)(set-market market)
  ;(setq *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir* *data-name*))
  (setq prices (find-all-primitives
     (format nil "~sA" (add-mkt-days (getnumdate stop-time) (- data-lgth))) (format nil "~sP" stop-time)))
  (setq *n-filt* filt)
  (do ((pr1 (car prices) (car prs))
       (pr2 (second prices) (second prs))
       (prs (cdr prices) (cdr prs)))
       ((not pr2) lgths)
       (push (abs (- pr2 pr1)) lgths))
  (setq lgths (vsort lgths #'<))

  (setq max-high (n-day-high (getnumdate stop-time) data-lgth))
  (setq min-low (n-day-low (getnumdate stop-time) data-lgth))
  (setq conv (/ data-lgth  (- max-high min-low)));;;inverse of gann-slope 

  (setq periods (reverse (cluster-cycles (mapcar #'(lambda (s) (round (* s conv))) lgths))))
 ; (format T "~%~A" periods)
  (setq longest (first periods))
  (dolist (ith periods)
   (if (> (length ith) (length longest)) (setq longest ith)))
  (round (* 2 (median longest)) );;;the lgths represent the high to low prices converted to days
  ))                              ;;;so the period is twice the time

(defun dominant-cycle (date &optional (min-size 10)(max-size 30))
    (let ((tdate (add-mkt-days date -30)) period  periods)
    (dotimes (ith 30) ;61)
      (multiple-value-setq (period)(cycle-dif-oc tdate min-size max-size))
      (push period periods)
      (multiple-value-setq (period)(cycle-dif-hl tdate min-size max-size))
      (push period periods)
     ; (multiple-value-setq (period)(cycle-dif-c tdate min-size max-size))
      ;(push period periods)
      (setq  tdate (getd tdate 'ndate))
  );;;closes the dotimes
  ; (setq periods (mapcar #'(lambda (s) (if (oddp s) (1- s) s)) periods))
  ;(print periods)
    (values (mode periods) (vsort periods #'<))
       ))

(defun mode (data)
  (let (size dummy data-mode num-data-mode el)
    (loop
        (setq size (length data))
        (setq el (car data)) (setq data (remove el data))
        (push (cons (- size (length data)) el) dummy)
        (ifn data (return)))

    (setq num-data-mode (caar dummy) data-mode (cdar dummy))
    (dolist (ith dummy data-mode)
      (if (> (car ith) num-data-mode)
          (setq data-mode (cdr ith) num-data-mode (car ith))))
  ))



(defun amplitude (date period )
   (let ( averages  averages-2 direction  ave 
            (dfl 0) (dfh 0)  (days1 0) deviations2 (tdate date)
           )
 (setq deviations2 nil  averages nil averages-2 nil direction nil  ave nil days1 0)
;;;;prepare list of prices get 2 prices per day
;    (dotimes (ith   (* 22 period))
 ;      ;  (format T "tdate = ~A~%" tdate)
  ;       (setq price-list (if (> (getd tdate 'close) (getd tdate 'open))
   ;                          (list (getd tdate 'high)(getd tdate 'low))
    ;                         (list (getd tdate 'low)(getd tdate 'high))))
     ;;    (setq prices (cons (nth 1 price-list) prices) prices (cons (car price-list) prices))
       ;  (setq tdate (getd tdate 'ydate)))
;    (setq prices (reverse prices))

;;;compute full cycle moving average of pivots
;;;there are one prices per day
   (dotimes (ith (* 10 period))
    (setq ave (ave tdate period 'pivot))
     (push ave averages)
     (setq tdate (getd tdate 'ydate))
  )
  (setq averages (reverse averages)) 
;    (push (ave (getd date 'ydate) period 'pivot) averages)
 ;   (push (ave date period 'pivot) averages)
;  (print averages)

  (setq tdate date)
;;;compute half cycle moving average
    (dotimes (ith  (* 10 period))
        (setq ave
           (ave tdate (truncate period 2) 'pivot))
        (push ave averages-2)
        (setq tdate (getd tdate 'ydate)))
   
    (setq averages-2 (reverse averages-2))   
;    (push (ave (getd date 'ydate) (truncate period 2) 'pivot) averages-2)
 ;   (push (ave date (truncate period 2) 'pivot) averages-2)
 ;   (print averages-2)

  (dotimes (ith (length averages))
     (push (- (nth ith averages-2)(nth ith averages)) deviations2))

  (setq deviations2 (reverse deviations2))
  
;  (setq direction (cond ((plusp (first deviations2)) 'DOWN)
;                        ((minusp (first deviations2)) 'UP)
;                        ((> (first deviations2)(second deviations2)) 'DOWN)
;                        ((< (first deviations2)(second deviations2)) 'UP)
;                        (t 'FT)))


;;;calculates days from the crossover of cycle and half cycle
 ; (format t "~%~A ~%~A  ~%~A" averages-2 averages deviations2)
  
     (loop

        (cond ((and (>= (first deviations2) 0)(<= (second deviations2) 0))
              (incf days1) (setq direction 'DOWN)(return))
              ((and (<= (first deviations2) 0)(>= (second deviations2) 0))
               (incf days1)(setq direction 'UP)(return))
              ((and (>= (first deviations2) 0)(>= (second deviations2) 0))
                (incf days1)(setq deviations2 (cdr deviations2)))
              ((and (<= (first deviations2) 0)(<= (second deviations2) 0))
                (incf days1)(setq deviations2 (cdr deviations2)))
                 
               (t (setq direction nil))
            )
         )

      (cond ((eql direction 'UP)(setq dfl days1 dfh nil))
            ((eql direction 'DOWN)(setq dfh days1 dfl nil))
            (t (setq dfh nil dfl nil)))


;;;dfl means days from low
;;;dfh means days from high

    (values direction dfl dfh)

  ))

;;;estimates the period from a list of lows
;;;then can use the 1/2 period moving average minus the full period moving average crossover
;;;to find expected reversal times.

(defun period-estimator (date0 &optional (filter 18))
  (let ((filt *n-filt*)  prices times days lows time-lengths
        indexes price-to-go time-to-go )
  
   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))
   (nil-tpv)
     (setq *n-filt* filter days (* 15 filter))
   (multiple-value-setq (prices times price-to-go time-to-go indexes )
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))

   ; (print prices) (print indexes)
;;;need to find list of lows
    (do* ((prs prices (cdr prs))
         (inds indexes (cdr inds))
         (kth (first prices) (first prs))
         (kth+1 (second prices) (second prs))
         (ith (first indexes) (first inds)))

        ((null kth+1))
        (if (< kth kth+1)(push ith lows))

      )
   (reverse lows )
   ; (format T "~%lows= ~A" lows)
 ;;;convert to time lengths
   (do* ((inds lows (cdr inds))
        (kth (first lows) (first inds))
        (kth+1 (second lows) (second inds)))

        ((null kth+1))
        (push (abs (- kth+1 kth)) time-lengths))
  (setq *n-filt* filt)
  ; (format T "~%time-lengths= ~A" time-lengths)
 ; (setq time-lengths (subseq time-lengths 0 4))
 (values (truncate (median time-lengths) 2)(mapcar #'(lambda(s) (truncate s 2))(vsort time-lengths #'< )))

))

;;for two trends returns the signal as 'UP 'DN or 'FT
;;'FT means not 'UP and not 'DN
;;
(defun trend-signal (date0 &optional (filter 5))
   (let ((filt *n-filt*)   p1 p2 P0 p3 prices  ignore trend1
         slope t0 t1 s0 s1  index-list (days (* 4 filter)))
 
    (nil-tpv)(setq *n-filt* filter)
    (loop
      (multiple-value-setq (prices ignore ignore ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
      (if (<= (length prices) 4)(setq days (+ 20 days))(return)))


    (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt)
   (setq P0 ;(getd date0 'close)
           (if (eql (getv 0 DR) 'UP)(getv 0 HP)(getv 0 LP))
         P1 (first prices) P2 (second prices) P3 (third prices)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))

    )
   (setq s0 (/ (- (log P0)(log P1)) t0)
         s1 (/ (- (log p1)(log p2)) t1)
            )

   (setq slope
   (if (< (abs s0)(abs s1))
       (if (plusp s0) 'DN 'UP)
     (if (plusp s0) 'UP 'DN)
      ))
   (setq trend1
     (cond ((and (> p0 p1)(> p1 p3)) 'UP)
           ((and (> p0 p1)(< p1 p3)(< p0 p2)) 'DN)
           ((and (< p0 p1)(< p1 p3)) 'DN)
           ((and (< p0 p1)(> p1 p3)(> p0 p2)) 'UP)
           ((> p0 p2) 'UP)
           ((< p0 p2) 'DN)
           (t nil)))

       (cond ((and (eql trend1 'UP)(eql slope 'UP)) 'CU)
           ((and (eql trend1 'DN)(eql slope 'DN)) 'CD)
           ((and (eql trend1 'UP)(eql slope 'DN)) 'DN)
           ((and (eql trend1 'DN)(eql slope 'UP)) 'UP)
           (t 'FT))


             ))


;;;returns 9 levels -4 -3 -2 -1 0 1 2 3 4

(defun trend-signal1 (date0 &optional (filter 5))
   (let ((filt *n-filt*)   p1 p2 P0 p3 prices  ignore trend1
          t0 t1 t2 s0 s1 s2  index-list (days (* 4 filter)))
 
    (nil-tpv)(setq *n-filt* filter)
    (loop
      (multiple-value-setq (prices ignore ignore ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
      (if (<= (length prices) 4)(setq days (+ 20 days))(return)))


    (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt)
   (setq P0 (getd date0 'close)
         P1 (first prices) P2 (second prices) P3 (third prices)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list))
    )
   (setq s0 (abs (/ (- (log P0)(log P1)) t0))
         s1 (abs (/ (- (log p1)(log p2)) t1))
         s2 (abs (/ (- (log p2)(log p3)) t2))
            )


   (setq trend1
     (cond ((and (> p0 p1)(> p1 p3)) 'UP)
           ((and (> p0 p1)(< p1 p3)(< p0 p2)) 'DN)
           ((and (< p0 p1)(< p1 p3)) 'DN)
           ((and (< p0 p1)(> p1 p3)(> p0 p2)) 'UP)
           ((> p0 p2) 'UP)
           ((< p0 p2) 'DN)
           (t nil)))

       (cond ((and (eql trend1 'UP)(> s0 s1)(> s0 s2)) 4)
             ((and (eql trend1 'UP)(> s0 s1)(<= s0 s2)) 3)
             ((and (eql trend1 'UP)(<= s0 s1)(> s0 s2)) 2)
             ((and (eql trend1 'UP)(<= s0 s1)(<= s0 s2)) 1)

             ((and (eql trend1 'DN)(> s0 s1)(> s0 s2)) -4)
             ((and (eql trend1 'DN)(> s0 s1)(<= s0 s2)) -3)
             ((and (eql trend1 'DN)(<= s0 s1)(> s0 s2)) -2)
             ((and (eql trend1 'DN)(<= s0 s1)(<= s0 s2)) -1)

             (t 0))


             ));;closes the let and the defun




(defun high-or-low (date)
  (block nil
  (let ((tdate (getnumdate date)) thigh tlow )
      (setq thigh (getd tdate 'high) tlow (getd tdate 'low))
       (if (>= thigh (max (getd (getd tdate 'ydate) 'high) (getd (getd tdate 'ndate) 'high)))
           (return 'high))
       (if (<= tlow (min (getd (getd tdate 'ydate) 'low) (getd (getd tdate 'ndate) 'low)))
           (return 'low))

 )))

;;;;need to get the distribution of the maximum deviations from a 4 day moving average
;;;;every time it turns up there is a maximum positive deviation. every time it turns
;;;;down there is a negative deviation. I need the 67 percentile.
(defun dev-up-distribution  (tdate &optional (size 4)(typ 'pivot))
  (let ((date  tdate) dmy-hi dmy-lo cluster-up cluster-down hp0 lp0 ave-value)
    (dotimes (ith (* 125 size))
       (setq hp0 (getd date 'high) lp0 (getd date 'low))
       (setq date (getd date 'ydate) ave-value (ave date size typ))
       (if (plusp (- (getd date 'close) ave-value)) 
           (setq cluster-up (cons (if (plusp (- hp0 ave-value))(/ (- hp0 ave-value) ave-value) 0) cluster-up) 
                  dmy-lo (cons (max* cluster-down) dmy-lo) cluster-down nil) 
        (setq cluster-down (cons (if (plusp (- ave-value lp0))(/ (- ave-value lp0) ave-value) 0) cluster-down)
              dmy-hi (cons (max* cluster-up) dmy-hi) cluster-up nil)))
   (setq dmy-hi (remove nil dmy-hi) dmy-lo (remove nil dmy-lo))          
   (values dmy-hi dmy-lo)          
             ))
(defun dev-down-distribution  (tdate &optional (size 4)(typ 'close))
  (let ((date  tdate) dmy-hi dmy-lo cluster-up cluster-down hp0 lp0)
    (dotimes (ith (* 125 size))
       (setq hp0 (getd date 'high) lp0 (getd date 'low))
       (setq date (getd date 'ydate))
       (if (and (plusp (- (getd date 'close)(ave date (1+ size) typ)))
                (plusp (- (getd (getd date 'ndate) 'close)(ave (getd date 'ndate) (1+ size) typ)))) 
           (setq cluster-up (cons (if (plusp (- (ave date (1- size) typ) lp0))(- (ave date (1- size) typ) lp0) 0) cluster-up) 
                  dmy-lo (cons (max* cluster-down) dmy-lo) cluster-down nil))
       (if (and (minusp (- (getd date 'close)(ave date (1+ size) typ)))
                (minusp (- (getd (getd date 'ndate) 'close)(ave (getd date 'ndate) (1+ size) typ))))            
        (setq cluster-down (cons (if (plusp (- hp0 (ave date (1- size) typ)))(- hp0 (ave date (1- size) typ)) 0) cluster-down)
              dmy-hi (cons (max* cluster-up) dmy-hi) cluster-up nil)))
   (setq dmy-hi (remove nil dmy-hi) dmy-lo (remove nil dmy-lo))          
   (values dmy-hi dmy-lo)          
             ))             
 
 (defun dev-objective (per tdate size &optional (typ 'close))
  (let (dev-hi dev-lo targ1 targ2 )
   (multiple-value-setq (dev-hi dev-lo)
   ; (if (eql dirp 'UP)
        (dev-up-distribution tdate size typ))
     ;   (dev-down-distribution tdate size typ)))
   (setq dev-hi (reverse dev-hi) dev-lo (reverse dev-lo) )
     
     
   (setq targ1 (* (ave tdate size)(+ 1 (percentile per dev-hi)))
         targ2 (* (ave tdate size) (- 1 (percentile per dev-lo))))
  
   (values (float targ1)(float targ2))))
   


;;;;need to get the distribution of the maximum deviations from a 4 day moving average
;;;;every time it turns up there is a maximum positive deviation. every time it turns
;;;;down there is a negative deviation. I need the 67 percentile.
(defun dev-distribution  (tdate &optional (size 4)(typ 'pivot)(per .95))
  (let ((date  tdate) cluster cl0  ave-value
         (ave0 (ave tdate size typ)))
    (dotimes (ith (* 125 size))
       (setq cl0 (getd date 'close) )
       (setq date (getd date 'ydate) ave-value (ave date size typ))
       (setq cluster (cons (/ (- cl0 ave-value) ave-value)  cluster))
     
       
         )
    
    (setq per (percentile1 (/ (- (getd tdate 'close) ave0) ave0)  cluster))
   ; (values (* (- 1 targ2) (ave tdate size typ))  (* (+ 1 targ1) (ave tdate size typ)))
             ))
(defun dev-down-distribution  (tdate &optional (size 4)(typ 'close))
  (let ((date  tdate) dmy-hi dmy-lo cluster-up cluster-down hp0 lp0)
    (dotimes (ith (* 252 size))
       (setq hp0 (getd date 'high) lp0 (getd date 'low))
       (setq date (getd date 'ydate))
       (if (and (plusp (- (getd date 'close)(ave date (1+ size) typ)))
                (plusp (- (getd (getd date 'ndate) 'close)(ave (getd date 'ndate) (1+ size) typ))))
           (setq cluster-up (cons (if (plusp (- (ave date (1- size) typ) lp0))(- (ave date (1- size) typ) lp0) 0) cluster-up)
                  dmy-lo (cons (max* cluster-down) dmy-lo) cluster-down nil))
       (if (and (minusp (- (getd date 'close)(ave date (1+ size) typ)))
                (minusp (- (getd (getd date 'ndate) 'close)(ave (getd date 'ndate) (1+ size) typ))))
        (setq cluster-down (cons (if (plusp (- hp0 (ave date (1- size) typ)))(- hp0 (ave date (1- size) typ)) 0) cluster-down)
              dmy-hi (cons (max* cluster-up) dmy-hi) cluster-up nil)))
   (setq dmy-hi (remove nil dmy-hi) dmy-lo (remove nil dmy-lo))
   (values dmy-hi dmy-lo)
             ))
#|
 (defun dev-objective (per tdate size &optional (typ 'pivot))
  (let (dev-hi dev-lo targ1 targ2 stop)
   (multiple-value-setq (dev-hi dev-lo)
     (dev-up-distribution tdate size typ))
   (setq dev-hi (reverse dev-hi) dev-lo (reverse dev-lo) stop; (percentile .95 (append dev-lo dev-hi)))
     (max* (append dev-lo dev-hi)))

   (setq targ1 (percentile per dev-hi) targ2 (percentile per dev-lo))

;   (values (float (/ (+ targ1 targ2) 2)) stop)))
    (values (* (+ 1 targ1) (ave tdate size typ))(* (- 1 targ2) (ave tdate size typ)))
))
;;;;need to test how often stopped out if stop at p4 and the
;;;;trend doesn't change direction
|#
(defun dev-stop-test  (tdate)
  (let ((date  tdate) losses  hp0 lp0 cl0 ave4 p4 (counter1 0)(counter2 0))
    (dotimes (ith 500)
       (setq hp0 (getd date 'high) lp0 (getd date 'low) cl0 (getd date 'close))
       (setq date (getd date 'ydate) p4 (getd (add-mkt-days date -3) 'close))
       (setq ave4 (ave date 4))
       (if (or (and (plusp (roc date 4))(< lp0  p4) (> cl0 p4))
               (and (minusp (roc date 4))(> hp0 p4)(< cl0 p4)))
           (progn (push (abs (- ave4 p4)) losses)(incf counter1)))
       (if (or (and (plusp (roc date 4))(< lp0 p4)(< cl0 p4))
               (and (minusp (roc date 4))(> hp0 p4)(> cl0 p4)))
           (progn (push (abs (- ave4 p4)) losses)(incf counter2))))

   (values (float (/ counter1 (+ counter1 counter2))) (float (/ (list-sum losses) (length losses))))
             ))


;;;;need to get the distribution of the maximum deviations of closes from a n-day moving average
;;;;every time it turns up there is a maximum positive deviation. every time it turns
;;;;down there is a negative deviation. I need the .875 percentile.
;;;this function returns Sell if should exit longs; Buy if exit shorts
(defun dev-distribution-close  (tdate &optional (size 4)(per .95)(typ 'close))
  (let ((date  tdate) dmy-hi dmy-lo cluster-up cluster-down cl0 ave-value obj-dev)
    (dotimes (ith (* 250 size))
       (setq date (getd date 'ydate) ave-value (ave date size typ) cl0 (getd date 'close))
       (if (plusp (- cl0 ave-value))
           (setq cluster-up (cons (/ (- cl0 ave-value) ave-value) cluster-up)
                  dmy-lo (cons (max* cluster-down) dmy-lo) cluster-down nil)
        (setq cluster-down (cons (/ (- ave-value cl0) ave-value) cluster-down)
              dmy-hi (cons (max* cluster-up) dmy-hi) cluster-up nil)));;;closes the dotimes

   (setq dmy-hi (remove nil dmy-hi) dmy-lo (remove nil dmy-lo))

  (setq obj-dev (* (percentile per (append dmy-lo dmy-hi)) (ave tdate size typ)))

  (cond ((< (+ obj-dev (ave tdate size typ)) (getd tdate 'close)) 'SELL)
        ((> (- (ave tdate size typ) obj-dev) (getd tdate 'close)) 'BUY)
        (t 'FLAT))
             ))




(defun set-market (market)
 (setq *data-name*  market  *time-interval* 'daily-high-low)
 (setq *data-read-dir* (format nil "~a~(~a~)/" *database-upper-dir* *data-name*))
 (nil-tpv)
)


(defun roc-test (tdate &optional (period2 144))
   (let ((date tdate) devs)
     (ifn period2 (setq period2 (- (sub-mkt-dates (first (month-days (get-first-index-date))) tdate) 13 1)))
     (dotimes (ith period2)
          (push (+ (/ (roc date 13) 13)(/ (roc date 8) 8)(/ (roc date 5) 5)) devs)
          (setq date (getd date 'ydate)))
    (values (/ (list-sum devs) period2) (max* devs) (min* devs))))

(defun roct (tdate)
    (+ (/ (roc tdate 34) 34)(/ (roc tdate 21) 21)(/ (roc tdate 13) 13)(/ (roc tdate 8) 8)(/ (roc tdate 5) 5)))


(defun roca (tdate)
   (- (+ (/ (roc tdate 13) 13)(/ (roc tdate 8) 8)(/ (roc tdate 5) 5))
      (+ (/ (roc (add-mkt-days tdate -13) 13) 13)
         (/ (roc (add-mkt-days tdate -8) 8) 8)
         (/ (roc (add-mkt-days tdate -5) 5) 5))))
(defun rocae (tdate)
   (- (+ (/ (roc tdate 34) 34)(/ (roc tdate 21) 21)(/ (roc tdate 13) 13)(/ (roc tdate 8) 8)(/ (roc tdate 5) 5))
      (+ (/ (roc (add-mkt-days tdate -34) 34) 34)
         (/ (roc (add-mkt-days tdate -21) 21) 21)
         (/ (roc (add-mkt-days tdate -13) 13) 13)
         (/ (roc (add-mkt-days tdate -8) 8) 8)
         (/ (roc (add-mkt-days tdate -5) 5) 5))))



(defun set-cat-list ()
  (let ((path (format nil "~Aindex.cfg" *database-upper-dir*)))
    (setq *cat-list* nil)
    (with-open-file (stream path :direction :input)
       (setq *indx-cfg* (read stream)))
    (dolist (ith *indx-cfg*)
      (push (cons (car ith) nil) *cat-list*))))

;;; symbol factor accuracy return/trade largest-loss digits symbol Print-name P&L #trades years
(defparameter *DEV-LIST*
  '((sp.d1b .618 64 1116 -19851 2 spm.d1b "JUNE S&P 500" 108253 97 13)
    (nd.d1b 1.382 68 1043 -77426 2 ndm.d1b "JUNE NASDAQ 100" 97020 93 7)

    (ty.d1b .618 67 153 -1948 nil tym.d1b "JUNE TEN YEAR NOTES" 6410 66 7)
    (gc.d1b 1.382 70 48 -1056 1 gcm.d1b "JUNE COMEX GOLD" 4258 88 7)
    (si.d1b 1.382 61 75 -2326 1 sik.d1b "MAY COMEX SILVER" 4042 54 7)

    (ad.d1b .618 68 118 -2976 4 adm.d1b "JUNE AUSTRALIAN DOLLAR" 9222 78 13)
    (bp.d1b 1.382 68 150 -3464 4 bpm.d1b "JUNE BRITISH POUND" 22336 149 17)
    (cd.d1b .618 67 101 -616 4 cdm.d1b "JUNE CANADIAN DOLLAR" 4560 45 7)
    (jy.d1b .618 60 87 -4926 4 jym.d1b "JUNE JAPANESE YEN" 13692 157 25)
    (sf.d1b .618 65 123 -5251 4 sfm.d1b "JUNE SWISS FRANC" 18015 147 25)

    (nk.d1b .618 67 665 -6176 0 nkm.d1b "JUNE CME NIKKEI 225" 29405 94 7)

    (s.d1b 1.382 64 150 -3314 2 sk.d1b "MAY SOYBEANS" 26036 174 20)
    (w.d1b .618 68 99 -2464 2 wk.d1b "MAY CBOT WHEAT" 4059 41 7)

    (cl.d1b .618 65 218 -5176 2 clk.d1b "MAY CRUDE OIL" 13488 62 15)
    (ng.d1b .618 84 850 -19806 3 ngk.d1b "MAY NATURAL GAS" 37394 44 7)

    (cf.d1b 1.382 65 361 -8839 2 cfk.d1b "MAY COFFEE" 26745 74 7)
    (ct.d1b 1.382 71 97 -2486 2 ctk.d1b "MAY COTTON" 13534 139 15)


    ))


#|
;;market is the continuation contract name
;;;market1 is the specific contract being traded
(defun exitpoints (date market market1 &optional (param .618))
 (let (ave4 dev adj cover-long cover-short ave3 ave20 per buy sell deltat deltaa tim1)
   (set-cat-list)(set-market market)
  (setq tim1 (momentum-divergence1 date 5 13))

 (multiple-value-setq (per)(dev-objective .8 date 4 'pivot))
 (set-market market1)

 (setq ave4 (ave date 4 'pivot) dev (* per ave4) ave3 (ave date 3 'pivot)
       ave20 (ave date 20 'pivot) deltat (* param (roct date)) deltaa (* param (rocae date))
             buy (- ave4 dev (- deltat) (if (minusp deltaa) (- deltaa) 0))
             sell (+ ave4 dev deltat (if (plusp deltaa) deltaa 0)))

             (setq adj (/ dev 4))

             (setq cover-long (if (eql tim1 'UP)(min (+ ave4 dev)(max ave20 (+ ave3 adj)))(+ ave3 adj)))
             (setq cover-short (if (eql tim1 'DOWN)(max (- ave4 dev)(min ave20 (- ave3 adj))) (- ave3 adj)))

             (if (> cover-long sell) (setq sell cover-long))
             (if (< cover-short buy) (setq buy cover-short))


 (format T "~A  SELL=  ~A                BUY=  ~A~%" market1 sell buy)
 (format T "SELL OBJECTIVE= ~A       BUY OBJECTIVE= ~A" cover-short cover-long)
  ))
|#
#|
(defun exitpoints-obj (date dur &optional (param .618))
 (let (ave4 dev  per buy sell deltat deltaa )

 (multiple-value-setq (per)(dev-objective .80 date dur 'pivot))

 (setq ave4 (ave date dur 'pivot) dev (* per ave4)
       deltat (* param (roct date)) deltaa (* param (rocae date))
       buy (- ave4 dev (- deltat) (if (minusp deltaa) (- deltaa) 0))
       sell (+ ave4 dev deltat (if (plusp deltaa) deltaa 0)))
 (values sell buy)
   ))
|#
#|
(defun check-x-treme (tdate)
    (let  ((market-list *X-list*)
            date date1 )

           (set-cat-list) (set-market (first (first market-list)))

          (setq date1 tdate date (getd tdate 'ydate))


           (dolist (market market-list)
              (set-market (first market))

              (setq date (getd tdate 'ydate))
              (when (numberp date)
              (let* (ave3 ave4 ave20 dev per buy sell adj cover-long cover-short deltat deltaa (tim1 (momentum-divergence1 date 5 13)))

              (multiple-value-setq (per)(dev-objective .8 date 4 'pivot))
             ; (if (nth 3 market)(set-market (nth 3 market)))

              (setq ave4 (ave date 4 'pivot) ave3 (ave date 3 'pivot) dev (* per ave4)
                    ave20 (ave date 20 'pivot) deltat (* (second market)(roct date)) deltaa (* (second market)(rocae date))
                    buy (- ave4 dev (- deltat) (if (minusp deltaa) (- deltaa) 0))
                    sell (+ ave4 dev deltat (if (plusp deltaa) deltaa 0)))
              (setq  adj (/ dev 4))


             (setq cover-long (if (eql tim1 'UP)(min (+ ave4 dev)(max ave20 (+ ave3 adj)))(+ ave3 adj)))
             (setq cover-short (if (eql tim1 'DOWN)(max (- ave4 dev)(min ave20 (- ave3 adj))) (- ave3 adj)))

              (when (getd tdate 'rollover)
                (setq buy (+ buy (getd tdate 'rollover))
                      sell (+ sell (getd tdate 'rollover))
                      cover-long (+ cover-long (getd tdate 'rollover))
                      cover-short (+ cover-short (getd tdate 'rollover))
                      ))
             (if (> cover-long sell) (setq sell cover-long))
             (if (< cover-short buy) (setq buy cover-short))

               (when (> (getd date1 'high) sell)

                  (format T "~%~A exceeded sell @ ~A" (nth 5 market) sell))
               (when (< (getd date1 'low) buy)

                  (format T "~%~A exceeded buy @ ~A" (nth 5 market) buy))


               );closes the let*
               ); closes the when
               );closes the dolist

               )) ;close the let and defun

|#
#|
(defun x-treme1 (&optional tdate (holidayp nil))
    (let  ((conv nil)(market-list *dev-list*)(rand (rem (get-universal-time) 1000))
            date (counter 0) date1 month day year path1)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate)) (set-cat-list)
          (setq date1 (if holidayp (if (or (string-equal (day-of-week date) "friday")
                                           (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)(add-days-to-date date 1))))
          (setq month (getstrmonth date1) rand (format nil "~S" rand))
          (setq day (getstrday date1))
          (setq year (getstryear date1))
          (setq path1 (string-append "/home/register/cycles/" month day year rand ".asp"))

          (if  (probe-file path1)
              (delete-file path1))

          (with-open-file (stream path1
                              :direction :output :if-exists :append :if-does-not-exist :create)

             (format stream  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">~%~%")
             (format stream "<html>~%<head>~%
       <title>Exit Points for ~A</title>~%</head>~%~%" (date-convert date1))

             (format stream "<body bgcolor=#264989>~%<div align=\"center\">~%<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" width=\"760\">~%")
             (format stream "<tr><td colspan=\"5\" bgcolor=\"#4766A6\"><br><h1>&nbsp;&nbsp;<font color=\"white\"~%")
             (format stream "     ><FONT~%")

             (format stream "      face=\"Bookman Old Style\">ExitPoints.com</FONT>  </font></h1></td></tr>~%")
             (format stream "<tr><td bgcolor=\"#4766A6\" width=\"1%\" rowspan=\"2\"></td>~%")
             (format stream "<td rowspan=\"2\" valign=\"top\" bgcolor=\"#4766A6\" nowrap>~%")
             (format stream "<p><font color=#FDFE96 size=2 face=\"Bookman Old Style\">~%")
            ;;;;

             (format stream "<!-- Vertical Menu Bar Starts Here -->~%")
             (format stream "<%~%")
             (format stream "	strPage=\"ExitPoints Archive\"~%")
             (format stream "	~%")
             (format stream "	~%")
             (format stream "	Set Conn = Server.CreateObject(\"ADODB.Connection\")~%")
             (format stream "	Set rsTop = Server.createobject(\"ADODB.Recordset\")~%")
             (format stream "	Set rsBottom = Server.createobject(\"ADODB.Recordset\")~%")
             (format stream "	Set rsPage = Server.createobject(\"ADODB.Recordset\")~%")
             (format stream "	~%")
             (format stream "	Conn.Open \"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=\" & \"c:\\inetpub\\ExitPoints.mdb\"~%")
             (format stream "	rsTop.open \"SELECT * FROM tblSideBar WHERE ListGroup=\"\"Top\"\" ORDER BY ListOrder;\"\,conn~%")
             (format stream "	rsBottom.open \"SELECT * FROM tblSideBar WHERE ListGroup=\"\"Bottom\"\" ORDER BY ListOrder\"\,conn~%")
             (format stream "	if not strGroup=\"\" then~%")
             (format stream "		rsPage.open \"SELECT * FROM tblSideBar WHERE ListGroup=\"\"\" & strGroup & \"\"\" ORDER BY ListOrder;\"\,conn~%")
             (format stream "	else~%")
             (format stream "		rsPage.open \"SELECT * FROM tblSideBar WHERE ListGroup=\"\"\" & strPage & \"\"\" ORDER BY ListOrder;\"\,conn~%")
             (format stream "	end if~%")
             (format stream "	do until rsTop.eof~%")
             (format stream "		if rsTop(\"Page\")=strPage then~%")
             (format stream "		%>~%")
             (format stream "			<STRONG><font color=\"#FFFFFF\"><% =rsTop(\"Page\") %></font></STRONG><br>~%")
             (format stream "		<%  do until rsPage.eof %>~%")
             (format stream "				<strong><A onmouseover=\"this.style.color=\'#FFFFFF\';\" style=\"MARGIN: 0px 0px 0px 15px; COLOR: #fdfe96; TEXT-DECORATION: none\" onmouseout=\"this.style.color=\'#FDFE96\';\" href=\"<% =rsPage(\"Link\") %>\" ~%")
             (format stream "				><% =rsPage(\"Page\") %></a></strong><br>~%")
             (format stream "			<%	rsPage.movenext~%")
             (format stream "			loop~%")
             (format stream "		else	%>~%")
             (format stream "			<STRONG><A onmouseover=\"this.style.color =\'#FFFFFF\';\" style=\"COLOR: #fdfe96; TEXT-DECORATION: none\" onmouseout=\"this.style.color=\'#FDFE96\';\" href=\"<% =rsTop(\"Link\") %>\" ~%")
             (format stream "			><% =rsTop(\"Page\") %></a></STRONG><BR>~%")
             (format stream "		<%  if rsTop(\"Page\") = strGroup then~%")
             (format stream "				do until rsPage.eof ~%")
             (format stream "					if rsPage(\"Page\")=strPage then %>~%")
             (format stream "						<strong><FONT color=\"#ffffff\" style=\"MARGIN: 0px 0px 0px 15px\"~%")
             (format stream "						><% =rsPage(\"Page\") %></FONT></strong><br>~%")
             (format stream "				 <% else %>~%")
             (format stream "						<strong><A onmouseover=\"this.style.color=\'#FFFFFF\';\" style=\"MARGIN: 0px 0px 0px 15px; COLOR: #fdfe96; TEXT-DECORATION: none\" onmouseout=\"this.style.color='#FDFE96';\" href=\"<% =rsPage(\"Link\") %>\" ~%")
             (format stream "						><% =rsPage(\"Page\") %></a></strong><br>~%")
             (format stream "			<%		end if~%")
             (format stream "					rsPage.movenext~%")
             (format stream "				loop ~%")
             (format stream "			end if~%")
             (format stream "		end if	~%")
             (format stream "		rsTop.MoveNext~%")
             (format stream "	loop %>")
             (format stream "	<hr width=\"100\">~%")
             (format stream "	<%~%")
             (format stream "	do until rsBottom.eof~%")
             (format stream "		if rsBottom(\"Page\")=strPage then~%")
             (format stream "		%>~%")
             (format stream "			<STRONG><font color=\"#FFFFFF\"><% =rsBottom(\"Page\") %></font></STRONG><br>~%")
             (format stream "		<%  do until rsPage.eof %>~%")
             (format stream "				<strong><A onmouseover=\"this.style.color=\'#FFFFFF\';\" style=\"MARGIN: 0px 0px 0px 15px; COLOR: #fdfe96; TEXT-DECORATION: none\" onmouseout=\"this.style.color=\'#FDFE96\';\" href=\"<% =rsPage(\"Link\") %>\" ~%")
             (format stream "				><% =rsPage(\"Page\") %></a></strong><br>~%")
             (format stream "		<%		rsPage.movenext~%")
             (format stream "			loop~%")
             (format stream "		else	%>~%")
             (format stream "			<STRONG><A onmouseover=\"this.style.color =\'#FFFFFF\';\" style=\"COLOR: #fdfe96; TEXT-DECORATION: none\" onmouseout=\"this.style.color=\'#FDFE96\';\" href=\"<% =rsBottom(\"Link\") %>\" ~%")
             (format stream "			><% =rsBottom(\"Page\") %></a></STRONG><BR>~%")
             (format stream "		<%  if rsBottom(\"Page\") = strGroup then~%")
             (format stream "				do until rsPage.eof ~%")
             (format stream "					if rsPage(\"Page\")=strPage then %>~%")
             (format stream "						<strong><FONT color=\"#ffffff\" style=\"MARGIN: 0px 0px 0px 15px\"~%")
             (format stream "						><% =rsPage(\"Page\") %></FONT></strong><br>~%")
             (format stream "				 <% else %>~%")
             (format stream "						<strong><A onmouseover=\"this.style.color=\'#FFFFFF\';\" style=\"MARGIN: 0px 0px 0px 15px; COLOR: #fdfe96; TEXT-DECORATION: none\" onmouseout=\"this.style.color=\'#FDFE96\';\" href=\"<% =rsPage(\"Link\") %>\" ~%")
             (format stream "						><% =rsPage(\"Page\") %></a></strong><br>~%")
             (format stream "			<%		end if~%")
             (format stream "					rsPage.movenext~%")
             (format stream "				loop ~%")
             (format stream "			end if~%")
             (format stream "		end if	~%")
             (format stream "		rsBottom.movenext~%")
             (format stream "	loop~%")
             (format stream "%>~%")
             (format stream "<br><a href=\"http://www.exitpoints.com/join.asp\"><img src=\"http://www.exitpoints.com/images/signupbutton.gif\" border=\"0\" alt=\"\"></a></FONT></P>~%")
             (format stream "<!-- End Menu Bar -->~%")

;;;;;;;;;;;;;
             (format stream "</td><td width=\"2%\" bgcolor=\"#9dcc68\">&nbsp;</td><td width=\"*\" height=\"20\" ")
             (format stream "valign=\"top\" bgcolor=\"#9DCC68\"><FONT~%")
             (format stream "      face=\"Bookman Old Style\" color=#ffffff size=2><strong>~%")
             (format stream "<%~%")

             (format stream "        Set rsHorizontal = Server.createobject(\"ADODB.Recordset\")~%")
             (format stream "        rsHorizontal.open \"SELECT * FROM tblHorizontalBar ORDER BY ListOrder;\"\,conn~%~%")
             (format stream "        do until rsHorizontal.eof~%")
             (format stream "            If rsHorizontal(\"ListOrder\")>1 then %> \| <% end if %>~%")
             (format stream "            <a href=\"<% =rsHorizontal(\"Link\") %>\" style=\"color:\#FFFFFF; margin: ")
             (format stream "20;\"><% =rsHorizontal(\"Page\") %></a>~%")
             (format stream "<%           rsHorizontal.movenext~%")
             (format stream "       loop~%")
             (format stream "%>~%")
             (format stream "</strong></font></td><td width=\"30\" bgcolor=\"#4766a6\" rowspan=\"2\"></td></tr>~%")

             (format stream "<tr><td bgcolor=\"white\">&nbsp;</td><td bgcolor=\"white\">~%")
             (format stream "<p><p>~%")

             (format stream "<div align=\"center\"><H1>ExitPoints for ~A</H1></div>~%" (date-convert date1))

             (format stream "<div align=\"center\" style=\"MARGIN: 20px\">~%")

             (dolist (market market-list)
              (incf counter)
              (set-market (first market))
              (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
              (let* (ave4 ave3 ave20 dev (places (nth 5 market))
                     cover-long cover-short per pers adj buy-stop sell-stop
                     (tim1 (momentum-divergence1 date 5 13)) buy sell deltat deltaa)

              (if (member *data-name* '(TY.D1B US.D1B)) (setq conv t)(setq conv nil))
              (multiple-value-setq (per pers)(dev-objective .8 date 4 'pivot))


              (if (nth 6 market)(set-market (nth 6 market)))

              (setq ave4 (ave date 4 'pivot) ave3 (ave date 3 'pivot) dev (* per ave4)
                    ave20 (ave date 20 'pivot) deltat (roct date) deltaa (rocae date)
                    buy (- ave4 dev (- deltat) (if (minusp deltaa) (- deltaa) 0))
                    sell (+ ave4 dev deltat (if (plusp deltaa) deltaa 0)))

             (setq  adj (/ dev 4) buy-stop (* (- 1 (* 1.2 pers)) ave4) sell-stop (* (+ 1 (* 1.2 pers)) ave4))
             (if (nth 6 market)(set-market (first market)))

             (setq cover-long (if (eql tim1 'UP)(min (+ ave4 dev)(max ave20 (+ ave3 adj)))(+ ave3 adj)))
             (setq cover-short (if (eql tim1 'DOWN)(max (- ave4 dev)(min ave20 (- ave3 adj))) (- ave3 adj)))



            (format stream "<p><table bgcolor=\"black\"><tr bgcolor=\"#f1f0ff\"><td colspan=\"2\" align=\"center\">")
            (format stream "<font style=\"color: #993399; font: bold;\">~A</font></td></tr>~%"(nth 7 market))
            (format stream "<tr valign=\"top\" bgcolor=\"white\">~%")
            (format stream "<td><table>~%")
            (format stream "<tr><td align=\"center\" valign=\"bottom\"><strong><font size=2>Action</font></strong></td><td>&nbsp;</td><td align=\"center\" valign=\"bottom\"><strong><font size=2>Price</font>")
            (format stream "</strong></td><td>&nbsp;</td><td align=\"center\" valign=\"bottom\"><strong><font size=2>Objective</font></strong></td><td>&nbsp;</td><td align=\"center\" valign=\"bottom\">")
            (format stream "<strong><font size=2>Stop</font></strong></td></tr>~%")

            (if conv (setq sell (format nil "~,2,,,F" (convert-to-32nds sell))))
            (if conv (setf (aref sell (position #\. sell)) #\'))


            (format stream "<tr><td align=\"center\" valign=\"middle\"><font color=#ff0000 size=2>SELL</font></td><td>&nbsp;</td><td align=\"center\" valign=\middle\"><font size=2>")
              (if conv (format stream " ~A" sell)
                      (format stream "~7,,0,'*,' F" (my-round sell places)))

              (if conv (setq cover-short (format nil "~,2,,,F" (convert-to-32nds cover-short))))
              (if conv (setf (aref cover-short (position #\. cover-short)) #\'))


            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"middle\"><font size=2>")
              (if conv (format stream "~A" cover-short)
                     (format stream "~7,,0,'*,' F" (my-round cover-short places)))

              (if conv (setq sell-stop (format nil "~,2,,,F" (convert-to-32nds sell-stop))))
              (if conv (setf (aref sell-stop (position #\. sell-stop)) #\'))


            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"middle\"><font size=2>")
             (if conv (format stream "~A" sell-stop)
                  (format stream "~7,,0,'*,' F" (my-round sell-stop places)))
            (format stream "</font></td></tr>~%")

              (if conv (setq buy (format nil "~,2,,,F" (convert-to-32nds buy))))
              (if conv (setf (aref buy (position #\. buy)) #\'))


            (format stream "<tr><td align=\"center\" valign=\"middle\"><font color=#009900 size=2>BUY</td><td>&nbsp;</td><td align=\"center\" valign=\"middle\"><font size=2>")
             (if conv (format stream " ~A" buy)
                      (format stream "~7,,0,'*,' F" (my-round buy places)))

            (if conv (setq cover-long (format nil "~,2,,,F" (convert-to-32nds cover-long))))
              (if conv (setf (aref cover-long (position #\. cover-long)) #\'))


            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"middle\"><font size=2>")
            (if conv (format stream "~A" cover-long)
                     (format stream "~7,,0,'*,' F" (my-round cover-long places)))

              (if conv (setq buy-stop (format nil "~,2,,,F" (convert-to-32nds buy-stop))))
              (if conv (setf (aref buy-stop (position #\. buy-stop)) #\'))

            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"middle\"><font size=2>")
              (if conv (format stream "~A" buy-stop)
                  (format stream "~7,,0,'*,' F" (my-round buy-stop places)))
            (format stream "</font></td></tr>~%")
            (format stream "</table></td>~%")
            (format stream "<td><table>~%")
            (format stream "<tr><td align=\"center\" valign=\"bottom\"><strong><font size=2>Net P/L</font> </strong></td><td>&nbsp;")
            (format stream "</td><td align=\"center\" valign=\"bottom\"><strong><font size=2># Trades</font> </strong></td><td>&nbsp;")
            (format stream "</td><td align=\"center\" valign=\"bottom\"><strong><font size=2>Time</font></strong></td></tr>~%")
            (format stream "<tr><td align=\"center\" valign=\"top\"><font size=2>$ ~A" (nth 8 market))
            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"top\"><font size=2>~A" (nth 9 market))
            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"top\"><font size=2>~A yrs" (nth 10 market))
            (format stream "</font></td></tr>~%")
            (format stream "<tr><td align=\"center\" valign=\"bottom\"><strong><font size=2>Accuracy</font></strong></td><td>&nbsp;")
            (format stream "</td><td align=\"center\" valign=\"bottom\"><strong><font size=2>Avg Return</font></strong></td><td>&nbsp;")
            (format stream "</td><td align=\"center\" valign=\"bottom\"><strong><font size=2>Largest Loss</font> </strong></td></tr>~%")
            (format stream "<tr><td align=\"center\" valign=\"top\"><font size=2>~A" (nth 2 market))
            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"top\"><font size=2>$ ~A" (nth 3 market))
            (format stream "</font></td><td>&nbsp;</td><td align=\"center\" valign=\"top\"><font size=2>$ ~A" (nth 4 market))
            (format stream "</font></td></tr>~%")
            (format stream "</table></td>~%")
            (format stream "</tr>~%")
            (format stream "</table></p>~%")


               );closes the dolist
               );closes the let*
            (format stream "</div>~%</td></tr>~%<tr><td bgcolor=\"#4766a6\" colspan=\"5\" height=\"50\"></td></tr>~%")




  (format stream "</table>~%</div>~%</body>~%</html>~%")



                )
           ;      (shell (string-append "cp " path1 " /home/mk-data/luis"))
                ) );close the with-open-file let and defun
|#
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
(defun x-treme2 (&optional tdate (market-list *X-list*) date1)
    (let  ((conv nil) date (counter 0) path1 path2)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate)) (set-cat-list)

          ;(setq date1 (if holidayp (if (or (string-equal (day-of-week date) "friday")
          ;                                 (string-equal (day-of-week date) "thursday"))
          ;                               (add-days-to-date date 4)(add-days-to-date date 2))
          ;           (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)(add-days-to-date date 1))))


          (setq path1 (string-append *daily-output-dir* "exitpoints.dat") path2 (string-append *daily-output-dir* "dcr.xml"))

          (if  (probe-file path1)
              (delete-file path1))

          (with-open-file (stream path1
                              :direction :output :if-exists :append :if-does-not-exist :create)

          (with-open-file (stream1 path2
                              :direction :output :if-exists :append :if-does-not-exist :create)

            (dolist (market market-list)
              (incf counter)
              (set-market (first market))

              (format stream "~A\,~A\,~A\," (nth 5 market)(nth 4 market) date1)
              (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
              (let* (ave4 ave3 ave20 dev (places (nth 2 market))
                     cover-long cover-short per adj directive buy1 sell1 cover-short1 cover-long1
                     (tim1 (if (nth 7 market)(momentum-divergence1 date 5 13))) buy sell deltat deltaa)

              (if (member *data-name* '(TY.D1B US.D1B)) (setq conv t)(setq conv nil))
              (setq per (dev-objective .8 date 4 'pivot))


             ; (if (nth 3 market)(set-market (nth 3 market)))

              (setq ave4 (ave date 4 'pivot) ave3 (ave date 3 'pivot) dev (* per ave4)
                    ave20 (ave date 20 'pivot) deltat (* (second market)(roct date)) deltaa (* (second market) (rocae date))
                    buy (- ave4 dev (- deltat) (if (minusp deltaa) (- deltaa) 0))
                    sell (+ ave4 dev deltat (if (plusp deltaa) deltaa 0)))

             (setq  adj (/ dev 4))
             ;(if (nth 3 market)(set-market (first market)))

             (setq cover-long (if (eql tim1 'UP)(min (+ ave4 dev)(max ave20 (+ ave3 adj)))(+ ave3 adj)))
             (setq cover-short (if (eql tim1 'DOWN)(max (- ave4 dev)(min ave20 (- ave3 adj))) (- ave3 adj)))

             (if (> cover-long sell) (setq sell cover-long))
             (if (< cover-short buy) (setq buy cover-short))

           (setq directive (string-append "~7," (format nil "~A" places) ",0,'*,' F"))

          ;;;;;;write out the buy price

            ;(if conv (setq buy (format nil "~,2,,,F" (convert-to-32nds buy))))
            ;(if conv (setf (aref buy (position #\. buy)) #\'))
             (if conv (setq buy1 (convert-to-32nds buy)))


            (cond (conv (format stream " ~A" buy1))
                  ((zerop places)(format stream "~D" (* (nth 6 market)(round buy (nth 6 market)))))
                  (t (format stream directive (* (nth 6 market)(round buy (nth 6 market))))))

            (format stream "\,")
          ;;;;;;;write out the buy objective

            ;(if conv (setq cover-long (format nil "~,2,,,F" (convert-to-32nds cover-long))))
            ;(if conv (setf (aref cover-long (position #\. cover-long)) #\'))
             (if conv (setq cover-long1 (convert-to-32nds cover-long)))

             (cond (conv (format stream "~A" cover-long1))
                   ((zerop places)(format stream "~D" (* (nth 6 market)(round cover-long (nth 6 market)))))
                   (t (format stream directive (* (nth 6 market)(round cover-long (nth 6 market))))))

           (format stream "\,1\,")
           ;;;;;;;;write out the sell price

            ;(if conv (setq sell (format nil "~,2,,,F" (convert-to-32nds sell))))
            ;(if conv (setf (aref sell (position #\. sell)) #\'))
            (if conv (setq sell1 (convert-to-32nds sell)))

            (cond (conv (format stream " ~A" sell1))
                  ((zerop places)(format stream "~D" (* (nth 6 market)(round sell (nth 6 market)))))
                  (t (format stream directive (* (nth 6 market)(round sell (nth 6 market))))))

           (format stream "\,")
           ;;;;;;;;;;write out the sell objective

           ; (if conv (setq cover-short (format nil "~,2,,,F" (convert-to-32nds cover-short))))
           ; (if conv (setf (aref cover-short (position #\. cover-short)) #\'))
            (if conv (setq cover-short1 (convert-to-32nds cover-short)))

            (cond (conv (format stream "~A" cover-short1))
                  ((zerop places)(format stream "~D" (* (nth 6 market)(round cover-short (nth 6 market)))))
                  (t (format stream directive (* (nth 6 market)(round cover-short (nth 6 market))))))

            (format stream "\,1\,")

            (format stream "~%")
            ;(format T "sell=~A cover-short=~A" sell cover-short)

            (write-xml-record1 date1 buy sell cover-long cover-short stream1)


               );closes the dolist
                            );closes the let*



));close the with-open-file
         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)
           (format stream1 "</trades>~%"))
       ;  (shell (string-append "cp " path1 " /home/mk-data/luis"))
                ));close the let and defun


|#

(defun speed-alert (&optional (date0 NIL)(days 125))
  (let ((filt *n-filt*)  p3 p1 p2 P0  prices  ignore
        month day year t0 t1 t2 s0 s1 s2 index-list trend path1)
  
   (ifn date0 (setq date0 (car (last (month-days (get-latest-index-date))))))(set-cat-list)
   (setq month (getstrmonth date0))
   (setq day (getstrday date0))
   (setq year (getstryear date0))
   (setq path1 (string-append "/home/register/cycles/mo" month day year ".dat"))

   (if  (probe-file path1)
        (delete-file path1))

   (with-open-file (stream path1
                              :direction :output :if-exists :append :if-does-not-exist :create)
   (dolist (filter '(5 9))
   (setq *n-filt* filter)
   (format stream "FOR FILTER = ~A~%" filter)
   (format T "~%FOR FILTER = ~A~%" filter)
   (dolist (market *day-list*)
   (set-market market)(nil-tpv)
   (multiple-value-setq (prices ignore ignore ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
   (setq prices (butlast prices) index-list (butlast index-list))

   (setq P0 (if (eql (getv 0 DR) 'UP) (getv 0 HP)(getv 0 LP))
         P1 (first prices) P2 (second prices) P3 (third prices)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list)))
   (setq s0 (/ (- (log P0)(log P1)) t0)
           s1 (/ (- (log p1)(log p2)) t1)
           s2 (/ (- (log p2)(log p3)) t2)

           )

   (setq trend (my-trend date0 filter))
    (when (and (< (abs s0)(abs s2))
             (> (abs s1)(abs s2))
                         )
      (print-string stream (format nil "~A" *data-name*) 2)
      (print-string T (format nil "~A" *data-name*) 2)
      (format T "   ~A    ~A~%" (if (plusp s0) "BEARISH" "BULLISH") trend)
      (format stream "   ~A    ~A~%" (if (plusp s0) "BEARISH" "BULLISH") trend)
     ;  (format T "~A ~A ~A ~A ~A ~A ~%" p0 p1 p2 t0 t1 t2)
      ; (format T "~A ~A ~A ~A ~A ~A ~%" *end-index* (first index-list) (second index-list) s0 s1 s2)
       )
    ));;closes the dolists
     (setq *n-filt* filt)


    );close the with-open-file

   ;  (shell (string-append "cp " path1 " /home/mk-data/luis"))
                 ))

 ;;;;returns the value that exceeded means the trend has been broken.
 ;;;;the days is the length of the trend not including the current day.
 ;;;;offset is the number of days into the future to extend the trend line
 (defun trend-change (date days &optional (offset 0))
     (let (slope center devs datex date3)

     (setq date3 (add-mkt-days date (- days)))
     (setq slope
            (/ (- (/ (+ (getd date 'low)(getd date 'high)) 2)
                  (/ (+ (getd date3 'low)(getd date3 'high)) 2)) days))
      ;(/ (roc date days) days))

     (setq center (/ (+ (ave date (1+ days) 'high)(ave date (1+ days) 'low)) 2))
     ;;;;compute deviations
     (setq datex date)
     (dotimes (ith (1+ days))
       (push (- (getd datex (if (plusp slope) 'low 'high))(+ center (* slope (- (/ days 2) ith)))) devs)
       (setq datex (add-mkt-days datex -1)))

    ;;;;
     (setq datex date)
     (dotimes (ith (1+ days))
       (push (- (getd datex (ifn (plusp slope) 'low 'high))(+ center (* slope (- (/ days 2) ith)))) devs)
       (setq datex (add-mkt-days datex -1)))

      (values (+ center (* (+ offset (* (1+ days) .5)) slope) (if (plusp slope) (min* devs) (max* devs)) (* 2 (index-tick-size)))
         slope
         (+ center (* (+ offset (* (1+ days) .5)) slope) (ifn (plusp slope) (min* devs) (max* devs)) (* -2 (index-tick-size)))

         )
      ))

(defun trend-change1 (date filt)
  (let (sdate prices times days (filt1 *n-filt*)  entry stop slope)

    (setq *n-filt* filt)
   (setq sdate (add-mkt-days date (- (* 10 filt))))
   (multiple-value-setq (prices times)(find-all-primitives (conv-to-string sdate 'P) (conv-to-string date 'P)))
   (setq days (sub-mkt-dates (getnumdate (car times)) date)) (setq *n-filt* filt1)
   (multiple-value-setq (entry slope stop)(trend-change2 date days 0))
;;;now calculate phase as in reaction trend system by Welles Wilder.
  ;;A low is a "B" so phase =0 means B
;   (setq phase-num (mod (+ 1 days) 3))
;   (setq phase
;         (cond ((and (plusp slope) (= phase-num 0)) 'B)
 ;              ((and (plusp slope)(= phase-num 1)) 'O)
  ;             ((and (plusp slope)(= phase-num 2)) 'S)
   ;            ((and (minusp slope)(= phase-num 0)) 'S)
   ;            ((and (minusp slope)(= phase-num 1)) 'B)
  ;;             ((and (minusp slope)(= phase-num 2)) 'O)))
  ;;
;     (setq buy (- (* (/ (+ (getd date 'high)(getd date 'close)(getd date 'low)) 3) 2)
 ;                 (getd date 'high)))
;     (setq sell (- (* (/ (+ (getd date 'high)(getd date 'close)(getd date 'low)) 3) 2)
;                  (getd date 'low)))

    (values (float slope) entry stop days)
   ))

 ;;;;returns the value that exceeded means the trend has been broken.
 ;;;;the days is the length of the trend not including the current day.
 ;;;;offset is the number of days into the future to extend the trend line
(defun trend-change2 (date days &optional (offset 0))
     (let (slope center devs datex date3)

     (setq date3 (add-mkt-days date (- days)))
 ;    (setq slope
 ;           (/ (- (/ (+ (getd date 'low)(getd date 'high)) 2)
  ;                (/ (+ (getd date3 'low)(getd date3 'high)) 2)) days))

     (setq slope
           (/ (- (max (getd date 'high)(getd date3 'high))
                 (min (getd date 'low)(getd date3 'low))) days))
     (if (> (ave date 1 'pivot)(ave date3 1 'pivot))(setq slope slope)(setq slope (- slope)))

     (setq center (/ (+ (ave date (1+ days) 'high)(ave date (1+ days) 'low)) 2))
     ;;;;compute deviations
     (setq datex date)
     (dotimes (ith (1+ days))
       (push (abs (- (getd datex (if (plusp slope) 'low 'high))(+ center (* slope (- (/ days 2) ith))))) devs)
       (setq datex (add-mkt-days datex -1)))

    ;;;;
     (setq datex date)
     (dotimes (ith (1+ days))
       (push (abs (- (getd datex (ifn (plusp slope) 'low 'high))(+ center (* slope (- (/ days 2) ith))))) devs)
       (setq datex (add-mkt-days datex -1)))

      (values (+ center (* (+ offset (* (1+ days) .5)) slope) (if (plusp slope)(- (max* devs)) (max* devs)) (* 2 (index-tick-size)))
         slope
         (+ center (* (+ offset (* (1+ days) .5)) slope) (ifn (plusp slope)(- (max* devs))(max* devs)) (* -2 (index-tick-size)))

         )
      ))


(defun entry-price (date)
   (let (path1 entry trend)
   (set-cat-list)
   (setq path1 (string-append *upper-dir* "entry.dat"))

   (if  (probe-file path1)
        (delete-file path1))

   (with-open-file (stream path1
                              :direction :output :if-exists :append :if-does-not-exist :create)


   (dolist (market *day-list*)
   (set-market market)
   (multiple-value-setq (entry trend) (trend-change date 3))
   (print-string stream (format nil "~A" *data-name*) 2)
   (format stream "   ~A    ~A~%" entry trend)
 ))))

;;;;this procedure is to test trading strategies that enter the market on stops.

(defun trend-test-stop-entry (date2 num &optional (param .85)(output T))
 (let (date stop-long stop-short trades long short
       vlow vhigh  long-entry short-entry
       ave-win ave-loss losers winners extended-trades trade-long trade
        (path1 (string-append *output-upper-dir* "trend.dat")))
  
   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
   (setq vhigh (n-day-high date 3) vlow (n-day-low date 3))

   (setq long-entry (min vhigh (volatility date 4 param)) short-entry (- vlow (volatility date 4 param)))

   (setq date (add-mkt-days date 1))

    (cond ((and (getd date 'rollover) long)
           (setq long (+ long (getd date 'rollover)))
           (setf (nth 2 trade-long) long))
          ((and (getd date 'rollover) short)
           (setq short (+ short (getd date 'rollover)))
           (setf (nth 2 trade) short)
           ))


;   (format T "date=~A  long-entry= ~A  short-entry=~A  long=~A short=~A stop=~A~%" date long-entry short-entry long short stop)
 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long short-entry)))
   (when short (setq stop-short (min stop-short long-entry)))


   (when (and long (<= (getd date 'low) stop-long))
           (push (- (min stop-long (getd date 'open)) long) trades)
           (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
   (when (and short (>= (getd date 'high) stop-short))
           (push (- short (max stop-short (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))

;;;;check if exit signal

;    (cond ((and long (>= (getd (getd date 'ydate) 'high) (n-day-high date 5)))
;           (push (- (getd date 'open) long) trades)
;           (setq trade (append trade (list date 'exit (getd date 'open) (- (getd date 'open) long))))
;           (push trade extended-trades)
;           (setq trade nil long nil stop-long nil))
;         ((and short (<= (getd (getd date 'ydate) 'low) (n-day-low date 5)))
;           (push (- short (getd date 'open)) trades)
;           (setq trade (append trade (list date 'exit (getd date 'open) (- short (getd date 'open)))))
;           (push trade extended-trades)
;           (setq trade nil short nil stop-short nil)))


;;;check if new entry
   (when (and (not short)
               ;(not (or (and (eql trend 'UP)
               ;         (<= vlow rprice))
               ;        (eql trend 'DOWN)))
               ;(and (neql trend 'DOWN)
              ;;  ;    (or (not rprice)(>= vlow rprice)))

              (<= (getd date 'low) short-entry)
               )
          (setq short (min (getd date 'open) short-entry)
                trade (list date 'short short)
                stop-short long-entry))

    (when (and (not long)
               ;(not (or (and (eql trend 'DOWN)
               ;          (>= vhigh rprice))
                ;       (eql trend 'UP)))
               ;(and (neql trend 'UP)
                ;    (or (not rprice)(<= vhigh rprice)))
           (>= (getd date 'high) long-entry)
                                          )
           (setq long (max (getd date 'open) long-entry)
                 trade-long (list date 'long long)
                 stop-long short-entry))

 ;;;check if stopped out on same day of entry
   (cond ((and long (> (getd date 'open)(getd date 'close))
                   (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
         ((and short (< (getd date 'open)(getd date 'close))
                     (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil)))




   );;;closes the dotimes


   (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )

   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
       P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
       (round (* (list-sum trades) (index-point-value)))
       (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss))))

       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
    );;closes the when
    (list-sum trades)

   ));;;closes the let and the defun

;;;returns four values. The first is the price that would break the current trend
;;;based on the last high and low of the most recently completed three primitives and
;;;the extreme of the current working primitive.
;;;second is  the price that will be the next trend channel break price
;;;third is the most recent extreme price
;;;fourth is the direction of the current channel lines.

(defun trend-entry-price (date0 &optional (filter 5))
   (let ((filt *n-filt*) sl state p1 p2 P3 P0  prices  ignore
         t0 t1 t2 index-list price-to-go (days (* 10 filter)))
 
    (nil-tpv)(setq *n-filt* filter)
   (multiple-value-setq (prices ignore price-to-go ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
    (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt)
   (setq
         P1 (first prices) P2 (second prices)  P3 (third prices)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list))
         P0 (if (eql (getv 0 DR) 'UP) (getv 0 HP)(getv 0 LP))
         sl (/ (- P1 P3)(+ t1 t2))
         state (cond ((and (plusp sl)(eql (getv 0 DR) 'UP)) 'UPUP)
                  ((and (plusp sl)(eql (getv 0 DR) 'DOWN)) 'DNUP)
                  ((and (not (plusp sl))(eql (getv 0 DR) 'UP)) 'UPDN)
                  ((and (not (plusp sl))(eql (getv 0 DR) 'DOWN)) 'DNDN))
    )

    (values (if (member state '(UPDN DNUP))(+ P2 (* (+ 1 t0 t1) sl))
                     (+ p1 (* (+ 1 t0) (/ (- P0 P2)(+ t0 t1))))
                         )
            (if (member state '(UPUP DNDN))(+ P2 (* (+ 1 t0 t1) sl))
                         (+ p1 (* (+ 1 t0) (/ (- P0 P2)(+ t0 t1)))))
             state P0 P1 P2 P3 price-to-go
  )

    ))


(defun momentum (date0 &optional (filter 9))
   (let ((filt *n-filt*)   p1 p2 p3 P0  prices  ignore dirp low high price-to-go
         t0 t1 t2 s0 s1 s2 index-list (days (* 20 filter)))
 
    (nil-tpv)(setq *n-filt* filter)
   (multiple-value-setq (prices ignore price-to-go ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
       (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt)


   (setq dirp (getv 0 DR) low (getv 0 LP) high (getv 0 HP))
   (if (eql dirp 'UP) (setq P0 high)(setq P0 low))


   (setq
         P1 (first prices) P2 (second prices) P3 (third prices); P4 (nth 3 prices)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list))
        ; t3 (- (third index-list)(nth 3 index-list))

    )
   (setq s0 (/ (- (log P0)(log P1)) t0)
         s1 (/ (- (log p1)(log p2)) t1)
         s2 (/ (- (log p2)(log p3)) t2)
        ; s3 (/ (- (log p3)(log p4)) t3)

            )
   (values
   (cond ((and (> p0 p1)(> p1 p3)(> (abs s0)(abs s2))) 'UP)
         ((and (> p0 p1)(> p1 p3)(< (abs s0)(abs s2))) 'DN)
         ((and (> p0 p1)(< p1 p3)(> (abs s0)(abs s1))) 'UP)
         ((and (> p0 p1)(< p1 p3)(< (abs s0)(abs s1))) 'DN)

         ((and (< p0 p1)(< p1 p3)(> (abs s0)(abs s2))) 'DN)
         ((and (< p0 p1)(< p1 p3)(< (abs s0)(abs s2))) 'UP)
         ((and (< p0 p1)(> p1 p3)(> (abs s0)(abs s1))) 'DN)
         ((and (< p0 p1)(> p1 p3)(< (abs s0)(abs s1))) 'UP)
         (t nil))
 ;  (values (if (and (> (abs s1) (abs s2))(< (abs s0)(abs s1)))
  ;             (if (plusp s0) 'DN 'UP))

                price-to-go s0)


             ))


;;for day trades

(defun momentum1 (date0 &optional (filter 9))
   (let ((filt *n-filt*)  p3 p1 p2 P0  prices  ignore
         t0 t1 t2 s0 s1 s2 index-list (days (* 10 filter)))
  
    (nil-tpv)(setq *n-filt* filter)
   (multiple-value-setq (prices ignore ignore ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days date0 (- days))) (format nil "~sP" date0)))
    (setq prices (butlast prices) index-list (butlast index-list))
    (setq *n-filt* filt)
   ; (format T "index-list ~A ~%prices=~A" index-list prices)
   (setq P0 (getd date0 'close)
         P1 (first prices) P2 (second prices) p3 (third prices)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list))
    )
   (setq s0 (/ (- (log P0)(log P1)) t0)
         s1 (/ (- (log p1)(log p2)) t1)
         s2 (/ (- (log p2)(log p3)) t2)
            )


   (cond  ((and (plusp s0)
                (> (abs s0)(abs s1))
               ; (> (abs s0)(abs s2))
                (> p1 p3)
                ) 'UP)
          ((and (minusp s0)
                (> (abs s0)(abs s1))
               ; (> (abs s0)(abs s2))
                (< p1 p3)
                ) 'DN)

          ((and (minusp s0)
                (< (abs s0)(abs s1))
                (< (abs s0)(abs s2))
               (> p1 p3)

                ) 'UP)
          ((and (plusp s0)
                (< (abs s0)(abs s1))
                (< (abs s0)(abs s2))
                (< p1 p3)

                ) 'DN)

          (t nil))

    ; (format t "s0=~A s1=~A p0=~A p1=~A p2=~A p3=~A" s0 s1 p0 p1 p2 p3)

             ))


;;;size is the filter size
(defun mo-diver (tdate size)
  (let ((filt *n-filt*) r0 r1 r2 r3 ignore prices times
        trend1 slope index-list (days (* 10 size)) s0 s1 s2 t0 t1 t2)
 
    (nil-tpv)(setq *n-filt* size)
   (loop
      (multiple-value-setq (prices times ignore ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days tdate (- days))) (format nil "~sP" tdate)))
      (if (<= (length prices) 4)(setq days (+ 20 days))(return)))

     (setq prices (butlast prices) times (butlast times) index-list (butlast index-list))
    (setq *n-filt* filt)


   (setq prices (butlast prices) index-list (butlast index-list))
   (setq *n-filt* filt)
   (setq r0 (rsi tdate 9)

         r1 (rsi (getnumdate (first times)) 9) r2 (rsi (getnumdate (second times)) 9) r3 (rsi (getnumdate (third times)) 9)
         t0 (- *end-index* (first index-list))
         t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list))
    )

    (setq s0 (/ (- r0 r1) t0)
         s1 (/ (- r1 r2) t1)
         s2 (/ (-  r2 r1) t2)
            )

   (setq slope
   (if (< (abs s0)(abs s1))
       (if (plusp s0) 'DN 'UP)
     (if (plusp s0) 'UP 'DN)
      ))
   (setq trend1
     (cond ((and (> r0 r1)(> r1 r3)) 'UP)
           ((and (> r0 r1)(< r1 r3)(< r0 r2)) 'DN)
           ((and (< r0 r1)(< r1 r3)) 'DN)
           ((and (< r0 r1)(> r1 r3)(> r0 r2)) 'UP)
           ((> r0 r2) 'UP)
           ((< r0 r2) 'DN)
           (t nil)))
   (values
    (cond ((and (eql trend1 'UP)(eql slope 'UP)) 'CU)
          ((and (eql trend1 'DN)(eql slope 'DN)) 'CD)
          ((and (eql trend1 'UP)(eql slope 'DN)) 'DN)
          ((and (eql trend1 'DN)(eql slope 'UP)) 'UP)
          (t 'FT))
   r0 r1 r2 r3 (float s0) (float s1) (float s2) t0 t1 t2)

     ))


;;;size is the filter size
(defun mo-diver1 (tdate size)
  (let ((filt *n-filt*) P1  P3 P0 ignore prices times dirp low high
   index-list (days (* 10 size)) s0 t0 t2 r0 r2 divers price-to-go)
 
    (nil-tpv)(setq *n-filt* size)
   (multiple-value-setq (prices times price-to-go ignore index-list)
       (find-all-primitives (format nil "~sA" (add-mkt-days tdate (- days))) (format nil "~sP" tdate)))
       (setq prices (butlast prices) times (butlast times) index-list (butlast index-list))
    (setq *n-filt* filt)

     (setq dirp (getv 0 DR) low (getv 0 LP) high (getv 0 HP))
     (if (eql dirp 'UP) (setq P0 high)(setq P0 low))

   (setq
         P1 (first prices)
          P3 (third prices)
         t0 (- *end-index* (first index-list))
        ; t1 (- (first index-list)(second index-list))
         t2 (- (second index-list)(third index-list))
    )

    (setq s0 (/ (- (log P0)(log P1)) t0))
     (if (plusp s0) (setq r0 (rsi-highest tdate 9 (truncate t0 2))
                          r2 (rsi-highest (getnumdate (second times)) 9 (truncate t2 2)))
            (setq r0 (rsi-lowest tdate 9 (truncate t0 2)) r2 (rsi-lowest (getnumdate (second times)) 9 (truncate t2 2))))

    (setq divers
        (cond ((and (plusp s0)(> p0 p1)(> p1 p3)(< r0 r2)
                  ) 'DN)
              ((and (minusp s0)(< p0 p1)(< p1 p3)(> r0 r2)
                   ) 'UP)
              (t 'FT)))

     (values divers price-to-go s0)
     ))
;;;this finds the highest and lowest rsi over a number of days ending
;;;but not including tdate

(defun rsi-highest (tdate size days)
   (let ((date tdate) rsi0 (rsi-max 0))

  (dotimes (ith (1+ days))
     (setq rsi0 (rsi date size))
     (if (> rsi0 rsi-max) (setq rsi-max rsi0))
     (setq date (getd date 'ydate)))
 rsi-max

 ))


(defun rsi-lowest (tdate size days)
   (let ((date tdate) rsi0 (rsi-min 100))

  (dotimes (ith (1+ days))
     (setq rsi0 (rsi date size))
     (if (< rsi0 rsi-min) (setq rsi-min rsi0))
     (setq date (getd date 'ydate)))
 rsi-min

 ))

(defun rsi-cross (tdate size days)
  (let ((date tdate) rsi0 rsi1  rsis rsi-ave-0 rsi-ave-1)

   (dotimes (ith (1+ days))
     (push (rsi date size) rsis)
     (setq date (getd date 'ydate)))

  (setq rsi-ave-0 (/ (list-sum (cdr rsis)) days)
        rsi-ave-1 (/ (list-sum (butlast rsis)) days)
        rsi0 (car (last rsis))
        rsi1 (car (last (butlast rsis))))
  (cond ((and (plusp (- rsi0 rsi-ave-0))
              (minusp (- rsi1 rsi-ave-1))) 'UP)
        ((and (minusp (- rsi0 rsi-ave-0))
              (plusp (- rsi1 rsi-ave-1))) 'DN)
        ((plusp (- rsi0 rsi-ave-0)) 'CU)
        ((minusp (- rsi0 rsi-ave-0)) 'CD)
        (t nil))

))

(defun smash-day (date)
  (let ((ydate (getd date 'ydate))(tclose (getd date 'close)))
  (cond ((and (> tclose (getd ydate 'high))
              (eql (getd date 'high)(n-day-high date 4))) 'UP1)
        ((and (< tclose (getd ydate 'low))
              (eql (getd date 'low)(n-day-low date 4))) 'DN1)
        ((and (< tclose (getd ydate 'close)) (> tclose (getd date 'open)) ;;Down day white body
              (>= tclose (- (getd date 'high)(* .25 (true-range date))))) 'UP2);;;closes in the top 75% of range
        ((and (> tclose (getd ydate 'close))(< tclose (getd date 'open)) ;;;Up day with black body
              (<= tclose (+ (getd date 'low)(* .25 (true-range date))))) 'DN2);;;closes in the bottem 25% of range
       
         (t nil))))
#|
(defun smash-day-type2 (date)
  (let ((ydate (getd date 'ydate))(tclose (getd date 'close)))
  (cond ((and (< tclose (getd ydate 'close))
              (>= tclose (- (getd date 'high)(* .25 (true-range date))))
              ;(> tclose (getd date 'open))
              ) 'UP)
        ((and (> tclose (getd ydate 'close))
              (<= tclose (+ (getd date 'low)(* .25 (true-range date))))
              ;(< tclose (getd date 'open))
              ) 'DN)
         (t nil))))
|#

;;;this procedure tests counter trend strategies and enters with limit orders.
;;;also trails the price with a limit objective order.
#|
(defun counter-trend-test (date2 num &optional (param .618) (mom t))
 (let (date trades long short  per deltat deltaa tim1
       ave3 ave4 ave20 dev adj cover-long cover-short entry-long entry-short
       winner-durations loser-durations winner-pers loser-pers ave-win ave-loss losers winners extended-trades trade
       (path1 (string-append *output-upper-dir* "trend.dat")))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
   (if mom (setq tim1 (momentum-divergence1 date)))
   (multiple-value-setq (per) (dev-objective .8 date 4 'pivot))

   (setq ave4 (ave date 4 'pivot) ave3 (ave date 3 'pivot) dev (* per ave4)
                   ave20 (ave date 20 'pivot) deltat (* param (roct date)) deltaa (* param (rocae date))
                   entry-long (- ave4 dev (- deltat) (if (minusp deltaa) (- deltaa) 0))
                   entry-short (+ ave4 dev deltat (if (plusp deltaa) deltaa 0)))
   (setq  adj (/ dev 4))
   (setq cover-long (if (eql tim1 'UP)(min (+ ave4 dev)(max ave20 (+ ave3 adj)))(+ ave3 adj)))

   (setq cover-short (if (eql tim1 'DOWN)(max (- ave4 dev)(min ave20 (- ave3 adj))) (- ave3 adj)))


    (if (> cover-long entry-short) (setq entry-short cover-long))
    (if (< cover-short entry-long) (setq entry-long cover-short))


   (setq date (add-mkt-days date 1))

    (cond ((and (getd date 'rollover) long)
           (setq long (+ long (getd date 'rollover)))
           (setf (third trade) long)
           )

          ((and (getd date 'rollover) short)
           (setq short (+ short (getd date 'rollover)))
           (setf (third trade) short)
           ))


 ;;;;check if met objective for prior position
;;;;if open is beyond the objective the open is slipped one tick
   (cond ((and long (> (getd date 'high) cover-long))
           (push (- (max cover-long (- (getd date 'open) (index-tick-size))) long) trades)
           (setq trade (append trade (list date 'exit (max cover-long (- (getd date 'open)(index-tick-size)))
                                           (- (max cover-long (- (getd date 'open) (index-tick-size))) long))))
           (push trade extended-trades)
           (setq trade nil long nil ))
         ((and short (< (getd date 'low) cover-short))
           (push (- short (min cover-short (+ (getd date 'open)(index-tick-size)))) trades)
           (setq trade (append trade (list date 'exit (min cover-short (+ (getd date 'open)(index-tick-size)))
                                (- short (min cover-short (+ (getd date 'open)(index-tick-size)))))))
           (push trade extended-trades)
           (setq trade nil short nil)))


 ;;;check if new entry
   (cond ((and (not short)
               (> (getd date 'high) entry-short)
                  )
          (setq short (max (getd date 'open) entry-short)
                trade (list date 'short short)
                ))


         ((and (not long)
               (< (getd date 'low) entry-long)
               )
           (setq long (min (getd date 'open) entry-long)
                 trade (list date 'long long)
               ))

              )
 ;;;check if stopped out
;   (cond ((and long (<= (getd date 'low) stop))
;           (push (- stop long) trades)
;           (setq trade (append trade (list date 'exit stop (- stop long))))
;           (push trade extended-trades)
;           (setq trade nil long nil stop nil))
;         ((and short (>= (getd date 'high) stop))
;           (push (- short stop) trades)
;           (setq trade (append trade (list date 'exit stop (- short stop))))
;           (push trade extended-trades)
;           (setq trade nil short nil stop nil)))

;;;check if met objective on day of entry
     (cond ((and long (< (getd date 'open)(getd date 'close))
            (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil ))
           ((and short (> (getd date 'open)(getd date 'close))
            (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil)))



   );;;closes the dotimes

   (dolist (ith extended-trades)
      (cond ((plusp (nth 6 ith))
             (push (subtract-dates (nth 0 ith)(nth 3 ith)) winner-durations)
             (push (* 100 (/ (abs (- (nth 5 ith) (nth 2 ith))) (nth 2 ith))) winner-pers))


            (t (push (subtract-dates (nth 0 ith)(nth 3 ith)) loser-durations)
               (push (* 100 (/ (abs (- (nth 5 ith)(nth 2 ith))) (nth 2 ith))) loser-pers))

             ))
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F"
       (round (* (list-sum trades) (index-point-value))) (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       )

   (format T "~%AVE-WIN = ~,2,,F%  AVE-WIN DUR= ~,1,,F DAYS  75% DUR= ~,1,,F DAYS MAX DUR= ~,1,,F DAYS ~%~
             AVE-LOSS = ~,2,,F%  AVE-LOSS DUR= ~,1,,F DAYS 75% DUR= ~,1,,F DAYS  MAX DUR= ~,1,,F DAYS"
        (/ (list-sum winner-pers)(length winner-pers))
        (/ (list-sum winner-durations)(length winner-durations))
        (percentile .75 winner-durations) (max* winner-durations)
        (/ (list-sum loser-pers)(length loser-pers))
        (/ (list-sum loser-durations)(length loser-durations))
        (percentile .75 loser-durations)(max* loser-durations)
        )


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (ith extended-trades)
    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))

   ));;;closes the let and the defun
|#
;;;;this one enters and exits on the open price.


(defun trend-test-open-entry (date2 num)
 (let (date trades long short trend3 cover-long cover-short  macd0 macd1 vhigh vlow
       ave-win ave-loss losers winners extended-trades trade stop-long stop-short
       (path1 (string-append *output-upper-dir* "trend.dat")))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
;    (setq ratio (/ (volatility date dur 1)(volatility date (* 5 dur) 1)))
 ;   (multiple-value-setq (vlow vhigh) (vprices date dur (* ratio param)))
    (setq vlow (n-day-low date 2) vhigh (n-day-high date 2))
    (multiple-value-setq (macd0 macd1)(macd date 12 26 9))
    (cond ((and (plusp macd0)(> macd0 macd1))(setq trend3 'up))
          ((and (minusp macd0)(< macd0 macd1))(setq trend3 'down))
          (t (setq trend3 nil)))
    (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max vlow stop-long)

               ) )
   (when short (setq stop-short (min vhigh stop-short)

               ) )

     (cond ((and long stop-long (<= (getd date 'low) stop-long))
           (push (- (min stop-long (getd date 'open)) long) trades)
           (setq trade (append trade (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
         ((and short stop-short (>= (getd date 'high) stop-short))
           (push (- short (max stop-short (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           )))



;;;check if met objective or exit criteria

     (when (and long (eql trend3 'down))

           (setq cover-long (getd date 'open))
           (push (- cover-long long) trades)
           (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil))

      (when (and short (eql trend3 'up))

            (setq cover-short (getd date 'open))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))



;;;check if new entry
   (cond ((and (not short) (eql trend3 'down)
               (> (getd date 'open) (getd (getd date 'ydate) 'low))

                          )
          (setq short (getd date 'open)
                trade (list date 'short short)
                stop-short vhigh
                 ))

         ((and (not long)
               (< (getd date 'open) (getd (getd date 'ydate) 'high))

                )
           (setq long (getd date 'open)
                 trade (list date 'long long)
                 stop-long vlow
                 ))

              )
 ;;;check if stopped out

   (cond ((and long stop-long (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
         ((and short stop-short (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           )))


;;;check if met objective or exit criteria on day of entry
;     (when long
;
;            (setq cover-long (getd date 'close))
;            (push (- cover-long long) trades)
;            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
;            (push trade extended-trades)
;            (setq trade nil long nil stop-long nil))
;
;      (when short
;
;            (setq cover-short (getd date 'close))
;
;            (push (- short cover-short) trades)
;            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
;            (push trade extended-trades)
;            (setq trade nil short nil stop-short nil))






   );;;closes the dotimes


   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F"
       (round (* (list-sum trades) (index-point-value))) (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       )



   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))


   ));


#|
(defun trends (&optional tdate (min-period 4)(max-period 9)
                          (market-list *day-list*) (outfile t))
   (let ((counter 0) date  trend trend1 path1  dir0 z-1 z0 z1 z2 zerost 3step-signal)
    
	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1 (string-append *output-upper-dir* (format nil "tr~S-~S.dat" min-period max-period)))
        (if (and outfile (probe-file path1))
            (delete-file path1))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%MKT   CLOSE   FSTO    Z0      Z1     Z2     3-S   BPLSIG~%" (date-convert date)))
        (dolist (market market-list)
          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

          (setq trend1 (fast-stochastic date 5))
          (setq trend (bpl-signal date 5))
      ;    (setq 3step-signal (zero-signal1 date 3))
      ;    (setq zerost (zero-strength date 2))
          (multiple-value-setq (dir0 z-1 z0 z1 z2)(zero-proj date 3))
           (setq dir0 (macd date 4 9 4))


        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 4)
            (if (member market '(TY.D1B US.D1B))
                  (format stream " ~7@A " (convert-to-32nds (getd date 'close)))
                  (format stream " ~7F "
                  (* (index-tick-size)(round (getd date 'close) (index-tick-size))) ))



             (format stream "~5@A" (cond ((<= trend1 20) 'OS)
                                         ((>= trend1 80) 'OB)
                                         (t "    ")))

             (if (member market '(TY.D1B US.D1B))
             (format stream " ~7@A " (convert-to-32nds z0))
            (format stream " ~7F " (* (index-tick-size)(round z0 (index-tick-size)))))

            (if (member market '(TY.D1B US.D1B))
             (format stream " ~7@A " (convert-to-32nds z1))
            (format stream " ~7F " (* (index-tick-size)(round z1 (index-tick-size)))))

            (if (member market '(TY.D1B US.D1B))
              (format stream " ~7@A " (convert-to-32nds z2))
            (format stream " ~7F "  (* (index-tick-size)(round z2 (index-tick-size)))))

            (format stream "~A ~A" (cond ((eql 3step-signal 'UP) 'UP)
                                         ((eql 3step-signal 'DN) 'DN)
                                         (t "  "))
                                   (cond ((eql zerost 'UP) 'SG)
                                         ((eql zerost 'DN) 'WK)
                                         (t "  ")))

           ; (format stream " ~6@A " (cond ((and (member trend3 '(EUP1 UP))(member trend5 '(EDOWN1 DOWN))) 'SELL)
           ;                               ((and (member trend3 '(EDOWN1 DOWN))(member trend5 '(EUP1 UP))) 'BUY)
           ;                               ((and (member trend3 '(EUP1 UP))
;				                (>= (getd date 'high)(n-day-high date 5))) 'OUT)
;                                          ((and (member trend3 '(EDOWN1 DOWN))
 ;                                               (<= (getd date 'low)(n-day-low date 5))) 'OUT)
  ;                                        (t "    ")))


            (format stream " ~6@A " (cond ((eql trend 'UP) 'UP)
                                          ((eql trend 'DN) 'DN)
                                          (t "    ")))




          (if (zerop (mod counter 5)) (terpri stream))
          );;closes with open file

)))

|#
;;;this procedure tests swing trend strategies and enters with limit orders.
;;;also exits with a market order on the close if stop price exceeded
;;;uses objectives.

(defun swing-trend-test (date2 num)
 (let (date trades long short  entry-long entry-short
       trend  cover-long cover-short avel3 aveh3
       losers winners extended-trades trade  ave-win ave-loss 
       (path1 (string-append *output-upper-dir* "trend.dat")))
  
   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))


;;;;from date1 to date2
 (dotimes (ith num)

  (setq trend (macd date 5 34 5)
        avel3 (ave date 3 'low) aveh3 (ave date 3 'high))

  (setq trend (cond ((minusp trend) 'DN)
                    ((plusp trend) 'UP)
                    (t nil)))


   (setq entry-long avel3
         entry-short aveh3
         cover-long aveh3
         cover-short avel3)


   (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))


;   (when long (setq stop-long (max stop-long vlow)))
;   (when short (setq stop-short (min stop-short vhigh)))

#|
     (cond ((and long stop-long (<= (getd date 'low) stop-long))
           (push (- (min stop-long (getd date 'open)) long) trades)
           (setq trade (append trade (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
         ((and short stop-short (>= (getd date 'high) stop-short))
           (push (- short (max stop-short (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           )))

|#
 ;;;check if met exit criteria on next day
     (when (and long (> (getd date 'high) cover-long)
                         )
            (setq cover-long (max (getd date 'open) cover-long))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil ))

      (when (and short (< (getd date 'low) cover-short)
                            )
            (setq cover-short (min (getd date 'open) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil ))



 ;;;check if new entry
   (cond ((and (not short)

               (eql trend 'DN)
               (> (getd date 'high) entry-short)
                  )
          (setq short (max (getd date 'open) entry-short)
                trade (list date 'short short)
                ;stop-short vhigh)
                ))

         ((and (not long)
               (eql trend 'UP)
               (< (getd date 'low) entry-long)
                  )
           (setq long (min (getd date 'open) entry-long)
                 trade (list date 'long long)
                 ;stop-long vlow))
              )))


 ;;;check if stopped out on same day
 ;;;;we do not place stop loss orders
 ;;;instead we get out at the close of the day that the stop loss price is exceeded
#|
   (when (and long stop-long
               (<= (getd date 'low) stop-long))
           (push (- (getd date 'close) long) trades)
           (setq trade (append trade (list date 'exit (getd date 'close) (- (getd date 'close) long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
    (when (and short stop-short
               (>= (getd date 'high) stop-short))
           (push (- short (getd date 'close)) trades)
           (setq trade (append trade (list date 'exit (getd date 'close) (- short (getd date 'close)))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           ))

|#
;;;check if met objective or exit criteria on same day
     (when (and long (> (getd date 'high) entry-short) (< (getd date 'open)(getd date 'close))
                         )
            (setq cover-long (max (getd date 'open) entry-short))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil ))

      (when (and short (< (getd date 'low) entry-long) (> (getd date 'open)(getd date 'close))
                                      )
            (setq cover-short (min (getd date 'open) entry-long))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil ))



    );;;closes the dotimes


   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
       P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
       (round (* (list-sum trades) (index-point-value)))
       (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss))))

       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
    (dolist (ith extended-trades)
    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))

   ));;;closes the let and the defun



(defun cycle1212 (date &optional (highs nil))
 (let ((filter *n-filt*)(filt 9) index-highs index-lows result ignore prices index-list fits ideals ideal-fit period)
 
  (setq *n-filt* filt)(nil-tpv)
  (multiple-value-setq (prices ignore ignore ignore index-list)
     (find-all-primitives (format nil "~sA" (add-mkt-days date (- 250))) (format nil "~sP" date)))
     (setq prices (butlast prices) index-list (butlast index-list))
  (setq index-list (butlast index-list 3))
  (dotimes (jth (length index-list))
   (cond ((and (evenp jth)(> (first prices)(second prices)))
           (push (nth jth index-list) index-highs))
         ((and (oddp jth)(> (first prices)(second prices)))
           (push (nth jth index-list) index-lows))
         ((and (evenp jth)(<= (first prices)(second prices)))
           (push (nth jth index-list) index-lows))
         ((and (oddp jth)(<= (first prices)(second prices)))
           (push (nth jth index-list) index-highs))))


;;;;index-list is the list of times of the actual highs and lows with filter 40.
;;;generate ideal turn times
;;;times vary from *end-index* to 0)

;;;;;lets look for cycle of 10 to 30 days . size equal 20 to 60
;;;offset varies from 0 to period
   (setq period 0)
   (dotimes (kth (- 60 10))
     (setq period (+ kth 10))
     (dotimes (offset period)
  ;;;;for a given period and offset find the ideal list of times
       (setq ideals nil)
       (dotimes (ith (1+ (truncate *end-index* period)))
          (if (< (+ offset (* ith period)) *end-index*)
          (push (+ offset (* ith period)) ideals))) ;;closes the dotimes

 ;;;calculate the goodness of fit
       (setq ideal-fit 0)
       (dolist (jth ideals)
       (setq ideal-fit
          (+ (expt (apply #'min (mapcar #'(lambda (s) (abs (- jth s))) (if highs index-highs index-lows))) 2)  ideal-fit))
        	);;;closes the dolist
       (setq ideal-fit (/ ideal-fit (length ideals)))
;;;;ideal-fit should now be a measure of how well a period/offset combination fits the actual highs and lows.
     (push (list period offset ideal-fit) fits)

     );closes the offset dotimes
     );closes the dotimes over period

  (setq result (first fits))

   (dolist (item fits)
     (if  (< (third item) (third result))
          (setq result item)))


    (setq *n-filt* filter ideals nil)
   (dotimes (ith (1+ (truncate *end-index* (first result))))
          (if (< (+ (second result) (* ith (first result))) *end-index*)
          (push (+ (second result) (* ith (first result))) ideals)))

   (format T "Period=~A   offset=~A   fit=~A" (first result)(second result)(third result))
   (format T "~%next turn= ~A" (- (+ (first result) (car ideals)) *end-index*))
   (format T "~%ideal list=~A" ideals)

     ));;;closes the let and defun


(defun big-movers (date &optional (len 1))
  (let ((market-list *day-list*) ranges)
  (set-cat-list)
  (dolist (market market-list)
    (set-market market)
     (push (cons market
      ;(/ (- (getd date 'high)(getd date 'low)) (getd date 'close)))
       (- (log (n-day-high date len))(log (n-day-low date len))))
       ranges)
  )
  (vsort ranges #'> 'cdr)
  (dotimes (ith 5)
     (format T "~A   ~A ~%" (car (nth ith ranges))(cdr (nth ith ranges))))
  ))

(defun ave-roc (tdate period1 period2)
 (let ((date tdate) (result 0))
   (dotimes (ith period2)
     (setq result (+ result (roc date period1)))
     (setq date (getd date 'ydate))

     )
  (/ result period2)))

 (defun ave-roc-delta (tdate period1 period2)
   (- (ave-roc tdate period1 period2)(ave-roc (getd tdate 'ydate) period1 period2)))


(defun test-open (date days)
  (dotimes (ith days)
     (ifn (getd date 'open) (return date))
     (if (or (> (getd date 'open)(getd date 'high))
             (< (getd date 'open)(getd date 'low)))
             (return date))
     (setq date (getd date 'ydate))))

(defun test-same (date days)
  (dotimes (ith days)
    (ifn date (return))
    (if (= (getd date 'open)(getd date 'high)(getd date 'low)(getd date 'close)) (break "~A" date))
    (setq date (getd date 'ydate))))

(defun trend-line (tdate days)
  (let (near-extreme far-extreme trend-price slope)
  (multiple-value-setq (near-extreme far-extreme)
  (n-day-extreme-dates tdate days))
  (multiple-value-setq (trend-price slope)
   (trend-change near-extreme (sub-mkt-dates far-extreme near-extreme)))
  (cond ((and (minusp slope)
              (> (n-day-high tdate (sub-mkt-dates near-extreme tdate)) trend-price))
          'UPB)
        ((and (plusp slope)
         (< (n-day-low tdate (sub-mkt-dates near-extreme tdate)) trend-price))
         'DOWNB)
         ((minusp slope) 'DOWN)
         ((plusp slope) 'UP))

  ))

(defun num-contracts (account-size estimated-risk initial-margin)
  (let ((num1c 0)(num1a 0)(num2 0))
    (setq num1c (floor (/ (* .01 account-size) estimated-risk)))
    (setq num1a (floor (/ (* .02 account-size) estimated-risk)))
    (setq num2 (floor (/ (* .1 account-size) initial-margin)))
    (values (min num1c num2)(min num1a num2))))


(defun true-range-log (date)
    (let (true-high true-low yclose)
        (setq yclose (+ (getd (getd date 'ydate) 'close) (or (getd date 'rollover) 0)))
        (setq true-high
                 (max (getd date 'high) yclose))
        (setq true-low
                 (min (getd date 'low) yclose))
       (- (log true-high) (log true-low))
   ))

(defun volatility-log (tdate &optional (period 5) (param .85))
  (let ((total 0) (date tdate))
  (dotimes (ith period)
    (setq total (+ (true-range-log date) total))
    (setq date (getd date 'ydate)))

  (* param (/ total period))
    ))


(defun volatility-log-exp (tdate &optional (period 5) (param .85))
  (let* ((date (add-mkt-days tdate (* -3 period)))(total (true-range-log date))
        (factor (/ 2 (+ period 1))))

  (dotimes (ith (1+ (* 3 period)))
    (setq total (+ (* factor (true-range-log date)) (* (- 1 factor) total)))
    (setq date (getd date 'ndate)))

  (* param total)
    ))


(defun vprices-log-exp (date &optional (period 5)(param .85))
  (let ((epsignal (volatility-log-exp date period param)))
 (values (exp (- (log (getd date 'close)) epsignal))
         (exp (+ (log (getd date 'close)) epsignal)))))

(defun volatility-log-gsv (tdate &optional (period 4) (param 1.8))
  (let ((total 0) (date tdate))

  (dotimes (ith period)
    (if (< (getd date 'close)(getd (getd date 'ydate) 'close))
        (setq total (+ (- (log (max (getd date 'high)(getd (getd date 'ydate) 'close)))
                          (log (getd (getd date 'ydate) 'close))) total))
        (setq total (+ (- (log (getd (getd date 'ydate) 'close))
                          (log (min (getd date 'low)(getd (getd date 'ydate) 'close)))) total)))
    (setq date (getd date 'ydate)))

  (* param (/ total period))
    ))


(defun vprices-log-gsv (date &optional (period 5)(param .85))
  (let ((epsignal (volatility-log-gsv date period param)))
 (values (exp (- (log (getd date 'close)) epsignal))
         (exp (+ (log (getd date 'close)) epsignal)))))


(defun vprices-log (date &optional (period 4)(param .85)(days 1))
  (let ((epsignal (volatility-log date period param)))
   (values (exp (- (log (n-day-high date days 'close)) epsignal))
         (exp (+ (log (n-day-low date days 'close)) epsignal)))))

;;body range ratio varies from -1 to +1
(defun day-body-range-ratio (date)
    (let ((thigh (getd date 'high)) (tlow (getd date 'low))(topen (getd date 'open))(tclose (getd date 'close)))

    (my-round (if (> thigh tlow) (/ (- tclose topen) (- thigh tlow)) 1) 3)))


(defun percentile-body-range (tdate per &optional (days 100))
   (let ((date tdate) ranges)
   (dotimes (ith days)
      (push (body-range date) ranges)
      (setq date (getd date 'ydate)))
   (percentile per ranges)))

;;;relative rate of change
;;; rate of change divided by the average true range
(defun rocrel (date period1 period2)

  (my-round (float (/ (roc date period1) (volatility date period2 1))) 3))


;;;;returns the dollar value
(defun max-true-range (date1 date2)
  (let (range (date date1) (max-range 0))
   (loop
     (if (> date date2) (return))
     (setq range (true-range date))
     (if (> range max-range) (setq max-range range))
     (setq date (getd date 'ndate)))
  (round (* max-range (index-point-value)))
    ))
;;;vix is a volatility index. Compares current volatility to 10 day average
;;;returns ratio of current volatility to 10 day average volatility
(defun my-vix (date &optional (period 10))
  (let (vixs)
    (dotimes (jth period)
      (setq vixs (push (volatility (add-mkt-days date (- jth)) 7 1) vixs)))
    (float (/ (car (last vixs)) (/ (list-sum vixs) period)))
    ))

#|
(defun vprices1 (date &optional (period 5)(param .85))
  (let ((epsignal (volatility date period param)))
   (values (- (getd date 'high) epsignal) (+ (getd date 'low) epsignal))))
|#
;;;;for day trades OB and OS
(defun standard-reward-risk (tdate);;;designed for swing trade time frame
  (let (vt ave4 entry-short entry-long cover-short cover-long
        stop-short stop-long risk-short risk-long )

   (setq ave4 (ave tdate 4 'pivot) vt (volatility-log tdate 60 1))

   (setq cover-long (exp (+ (log ave4) (* *objective-factor* vt)))
         cover-short (exp (- (log ave4) (* *objective-factor* vt))))

   (multiple-value-setq (entry-short entry-long)
          (vprices tdate 4 *entry-factor* 1))

    (setq entry-short (min entry-short (getd tdate 'close))
          entry-long  (max entry-long (getd tdate 'close)))

    (setq risk-short (* *stop-loss-day* (abs (- entry-short (n-day-high tdate 1 'close)))))
    (setq risk-long (* *stop-loss-day* (abs (- entry-long (n-day-low tdate 1 'close)))))

     (setq stop-long (- entry-long risk-long)
           stop-short (+ entry-short risk-short))

   (values (/ (- entry-short cover-short) (- stop-short entry-short))
           (/ (- cover-long entry-long) (- entry-long stop-long))
           )

 ))
;;;;

(defun standard-reward-risk1 (tdate)
  (let (rr-short rr-long)
   (multiple-value-setq (rr-short rr-long)(standard-reward-risk tdate))
   (if (< rr-short rr-long)
       (if (minusp rr-short) 'OS)
     (if (minusp rr-long) 'OB))
    ))

(defun weekly-vsignal-prices (tdate &optional (param 5) (param1 .85) (friday-holiday nil))
  (let ((date tdate) vdev dmy weeks ranges pclose highs lows)

;;;;organize into weeks
;;;does not include the first incomplete week
;;;
   (dotimes (ith (1- (* 6 param)))
     (push date dmy)
     (when (>= (subtract-dates (getd date 'ydate) date) 3)
         (push dmy weeks)
         (setq dmy nil))
      (setq date (getd date 'ydate))

         )

   (setq weeks (reverse weeks))
   (cond ((and (not friday-holiday)
               (< (length (car weeks)) 5))
           (setq weeks (cdr weeks)))
         ((and friday-holiday
               (< (length (car weeks)) 4))
            (setq weeks (cdr weeks))))

;;;now find true ranges for the weeks
    (dolist (ith weeks)
       (setq highs (mapcar #'(lambda (s)(getd s 'high)) ith)
             lows (mapcar #'(lambda (s)(getd s 'low)) ith))

;;;need the close of the prior week
      (setq pclose (getd (getd (car ith) 'ydate) 'close))
      (setq highs (push pclose highs))
      (setq lows (push pclose lows))
      (setq ranges (push (- (max* highs)(min* lows)) ranges)))

     (setq vdev (* param1 (/ (list-sum ranges)(length ranges))))
     (setq pclose (getd (car (last (car weeks))) 'close))
     (values (- pclose vdev)(+ pclose vdev))


))

(defun formation-signal (tdate &optional (days 1))
  (let ((date tdate) epsignal (signals 0))
   (dotimes (ith days)
     (setq epsignal (car (formation date)))
     (if epsignal (return))
     (setq date (getd date 'ydate)))
    
      (setq signals (+ signals
                     (cond ((member epsignal '(DB1 DB2 DB3 DB4 DB5 DB6 DB7 DB8)) 1)
                          ((member epsignal '(DS1 DS2 DS3 DS4 DS5 DS6 DS7 DS8)) -1)
                          (t 0))))
     signals ))


 ;;;trading formations
(defun formation (tdate)
   (let ((date tdate) high0 low0 close0 high1 low1 close1 high2 low2 close2 epsignal
          lows highs day0 day1 (aa 0)(bb 0) step1 step2 step3 reversals)

        (setq high0 (getd date 'high) low0 (getd date 'low) close0 (getd date 'close))
        (setq date (getd date 'ydate))
        (setq high1 (getd date 'high) low1 (getd date 'low) close1 (getd date 'close))
        (setq date (getd date 'ydate))
        (setq high2 (getd date 'high) low2 (getd date 'low) close2 (getd date 'close))

 ;;;test if prices close in the upper half of the day's range

        (cond ((and (>= close0 (+ low0 (* .75 (- high0 low0))))
                 (> close1 (/ (+ high1 low1) 2))
                 (> close2 (/ (+ high2 low2) 2)))   (push 'DB1 epsignal))
              ((and (<= close0 (+ low0 (* .25 (- high0 low0))))
                 (< close1 (/ (+ high1 low1) 2))
                 (< close2 (/ (+ high2 low2) 2))) (push 'DS1 epsignal))
                 );;;closes the cond

;;;check if the second day is a reversal day
          (setq day1 (reversal-dayp (getd tdate 'ydate)))
;;;check if the first most recent day is a reversal day
          (setq day0 (reversal-dayp tdate))

;;;;test if buy or sell epsignal
         (cond ((and (eql day1 'UP)(>= close0 (+ low0 (* .75 (- high0 low0)))))
                 (push 'DB2 epsignal))
               ((and (eql day1 'DN)(<= close0 (+ low0 (* .25 (- high0 low0)))))
                 (push 'DS2 epsignal))
               ((and (eql day0 'UP)(> close1 (+ low1 (* .50 (- high1 low1))))
                      (>= close0 (+ low0 (* .75 (- high0 low0)))))
                 (push 'DB2 epsignal))
               ((and (eql day0 'DN)(< close1 (+ low1 (* .50 (- high1 low1))))
                  (<= close0 (+ low0 (* .25 (- high0 low0)))))
                  (push 'DS2 epsignal)))

;;;;check for gaps
         (cond ((> low1 high2) (setq day1 'GAPUP))
               ((< high1 low2) (setq day1 'GAPDN))
               ((> low0 high1) (setq day0 'GAPUP))
               ((< high0 low1) (setq day0 'GAPDN)))
;;;;test if buy or sell
          (cond ((and (eql day1 'GAPUP)(> close1 (+ low1 (* .50 (- high1 low1))))
                      (>= close0 (+ low0 (* .75 (- high0 low0)))))(push 'DB3 epsignal))
                ((and (eql day0 'GAPUP)(> close0 (+ low0 (* .75 (- high0 low0))))
                      (> close1 (+ low1 (* .50 (- high1 low1))))) (push 'DB3 epsignal))
                ((and (eql day1 'GAPDN)(< close1 (+ low1 (* .50 (- high1 low1))))
                      (<= close0 (+ low0 (* .25 (- high0 low0))))) (push 'DS3 epsignal))
                ((and (eql day0 'GAPDN)(< close0 (+ low0 (* .25 (- high0 low0))))
                      (< close1 (+ low1 (* .50 (- high1 low1)))))(push 'DS3 epsignal)))

;;;;test for island
            (cond ((and (eql day1 'GAPDN)(eql day0 'GAPUP)
                         (> close0 (+ low0 (* .75 (- high0 low0)))))(push 'DB4 epsignal))
                  ((and (eql day1 'GAPUP)(eql day0 'GAPDN)
                        (< close0 (+ low0 (* .25 (- high0 low0)))))(push 'DS4 epsignal)))

          (setq date tdate step1 nil step2 nil step3 nil)
;;;;test for Lindahl buy
           (dotimes (ith 8)
             (push (getd date 'low) lows)(push (getd date 'high) highs)
             (setq date (getd date 'ydate)))
;;;step 1 find the lowest price in the last 8 market days
           (setq aa (position (min* lows) lows))
           (dotimes (jth (- 7 aa))
              (if (> (nth (+ jth aa 1) highs) (nth aa highs)) (setq bb (+ jth aa 1) step1 T)))
;;;bb is the index to the day of the high after index aa  (the low of the formation)
;;;step 2 find if a day following the bb high goes lower than the preceding day
           (dotimes (ith (- 7 bb))
              (if (< (nth (+ ith bb 1) lows)(nth (+ ith bb) lows)) (setq step2 T)))
;;;step 3
            (if (and (> high0 high1) (> close0 close1)(> close0 (getd tdate 'open))
                     (>= close0 (+ low0 (* .75 (- high0 low0))))) (setq step3 T))

          (if (and step1 step2 step3) (push 'DB5 epsignal))

;;;;test for Lindahl sell

          (setq date tdate step1 nil step2 nil step3 nil lows nil highs nil)
          (dotimes (ith 7)
             (push (getd date 'low) lows)(push (getd date 'high) highs)
             (setq date (getd date 'ydate)))
;;;step 1 find the highest price in the last 7 market days
           (setq aa (position (max* highs) highs))
           (dotimes (jth (- 6 aa))
              (if (< (nth (+ jth aa 1) lows) (nth aa lows)) (setq bb (+ jth aa 1) step1 T)))
;;;bb is the index to the day of the high after index aa  (the low of the formation)
;;;step 2 find if a day following the bb high goes lower than the preceding day
           (dotimes (ith (- 6 bb))
              (if (> (nth (+ ith bb 1) highs)(nth (+ ith bb) highs)) (setq step2 T)))
;;;step 3
            (if (and (< low0 low1) (< close0 close1)(< close0 (getd tdate 'open))
                     (<= close0 (+ low0 (* .25 (- high0 low0))))) (setq step3 T))

          (if (and step1 step2 step3) (push 'DS5 epsignal))
;;;test for trend continuation rule
          (cond ((and (>= close0 (+ low0 (* .75 (- high0 low0))))
                      (plusp (reversal-dayp tdate)) (member (dow-trend tdate 3) '(EUP UP)))
                  (push 'DB6 epsignal))
                ((and (<= close0 (+ low0 (* .25 (- high0 low0))))
                      (minusp (reversal-dayp tdate))(member (dow-trend tdate 3) '(EDOWN DOWN)))
                      (push 'DS6 epsignal)))

;;;test for Spike high or low
          (cond ((and (< close0 close1)(<= close0 (+ low0 (* .25 (- high0 low0))))
                      (> high0 (n-day-high (getd tdate 'ydate) 5))
                      (> (- high0 low0)  (* 2 (average-range (getd tdate 'ydate) 5)))) (push 'DS7 epsignal))
                ((and (> close0 close1)(>= close0 (+ low0 (* .75 (- high0 low0))))
                      (< low0 (n-day-low (getd tdate 'ydate) 5))
                      (> (- high0 low0) (* 2 (average-range (getd tdate 'ydate) 5)))) (push 'DB7 epsignal)))


;;;test for double reversal
           (setq date tdate reversals nil)
           (dotimes (ith 5)
              (push (reversal-dayp date) reversals)
              (setq date (getd date 'ydate)))
           (if (and (> (+ (count 1 reversals)(count 2 reversals)) 1)
                    (>= close0 (+ low0 (* .75 (- high0 low0))))) (push 'DB8 epsignal))
           (if (and (> (+ (count -1 reversals)(count -2 reversals)) 1)
                    (<= close0 (+ low0 (* .25 (- high0 low0))))) (push 'DS8 epsignal))
     ; (print reversals)
      epsignal
            ))

(defun structures (&optional tdate (min-period 3)(market-list *day-list*) (outfile t))
   (let ((counter 0) date  path1 (P&L 0)(num-trades 0) trades profit)

	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        ;(set-cat-list)
        (setq path1 (string-append *output-upper-dir* "structures.dat"))
        (if (and outfile (probe-file path1))
            (delete-file path1))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert date)))
        (dolist (market market-list)
          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))


        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 4)
            (format stream " ~7@A ~%" (if (member market '(TY.D1B US.D1B))(convert-to-32nds (getd date 'close))(getd date 'close)))

         (multiple-value-setq (profit trades)(structure-test date 2500 min-period stream))
         (setq P&L (+ P&L profit) num-trades (+ num-trades trades))

          (terpri stream)
           );;;closes the stream
           );;;closes the dolist
        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~%P&L= ~D   ~A~%" (round P&L) num-trades))

))


#|
(defun position-trades (&optional tdate (market-list *position-list*) (outfile t) date1)
   (let ((counter 0) date  path1 path2  profit trades new-profit new-trades trade-list
          directive path3)
  
	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        ;(set-cat-list)
        (setq path1 (string-append *daily-output-dir* "position-trades.dat") path2 (string-append *daily-output-dir* "dcr.xml")
                       path3 (string-append *daily-output-dir* "position-orders.csv"))

        (if (and outfile (probe-file path1))
            (delete-file path1))
      ; (if (and outfile (probe-file path3))
       ;     (delete-file path3))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert date)))
       ; (with-open-file (stream3 path3 :direction :output :if-exists :append :if-does-not-exist :create)
       ;  (format stream3 "EXITPOINTS, POSITION, SYSTEM, ORDERS, FOR, ~A~%" (date-convert date1))
       ;   (format stream3 "Market,              Month,   Direction,      Entry,    Stop,  Objective~%~%"))

         (apply #'position-trade-bins1 *position-features*) ;;;need just once for all markets

        (dolist (market market-list)
          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D~%"
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F~%")))


        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~A" market) 6)
            ; (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
              (format stream "  ~A  " (contract-month market tdate))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A ~%" (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))
                     ))

         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

          (with-open-file (stream3 path3 :direction :output :if-exists :append :if-does-not-exist :create)


           (multiple-value-setq (profit trades new-profit new-trades trade-list)
             (find-best-position-trade1 date stream stream1 stream3 date1))

           ));;closes stream3 and stream1


         (terpri stream)

          );;;closes the stream
          );;;closes the dolist
       ;   (setq optimal (optimal-f sum-trade-list))
       ; (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
       ;    (format stream "~%P&L= ~D  ~A     New P&L= ~D New Num Trades= ~A~%" (round P&L) num-trades (round new-P&L) new-num-trades)
       ;    (format stream "~%$/Contract= ~D~%" optimal))


))

|#

(defun day-trades (&optional tdate (market-list *day-list*) (holidayp nil))
   (let (date date1 path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  bin features 
     ;     (path-in (string-append *config-dir* "day-features3.dat"))
          )
         (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc  counter))
	
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades.dat")
              path2 (string-append *daily-output-dir* "dcr.xml"))
      ;  (setq path3 (string-append *output-upper-dir* "oec.csv"))

         (setq features *day-features3*)
         (apply #'day-trade-bins3b features)

        (dolist (market market-list)
            (set-market market)
   ;        (format T "~%Market = ~A   Features = ~A ~%" market features)          
           (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
           (if (and tdate (readt2 tdate)) (setq date tdate))
           (if (and tdate (not (readt2 tdate)))
               (setq date (car (last (month-days (get-latest-index-date))))))
           (setq date1 (if (or holidayp (not (readt2 tdate)))
                           (if (or (string-equal (day-of-week date) "friday")
                                   (string-equal (day-of-week date) "thursday"))
                                (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))



          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))



         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
         ;   (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
             (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))


          (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)
        ;  (with-open-file (output-oec path3 :direction :output :if-exists :append :if-does-not-exist :create)


         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-daytrades3b date features))

       
       ; (format T "~%BIN = ~A twr-short=~A~%" bin twr-short)
  
        (find-best-daytrade3 date stream stream1 date1)
       ;   );;;closes output-oec
          );;closes stream1



          );closes the stream
          );closes the dolist

  )) ;;closes the let and defun


(defun day-trades2 (&optional tdate (market-list *epic-list*)(holidayp nil))
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
   ;      (path-in (string-append *config-dir* "day-features2.dat"))
          )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
    
    (ifn (boundp 'counter)(setq counter 0))
    
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades2.dat")
              path2 (string-append *daily-output-dir* "dcr2.xml"))
       
       (setq features *day-features2*)
       (apply #'day-trade-bins2b features)

        (dolist (market market-list)
            (set-market market)
    ;       (format T "~%Market = ~A   Features = ~A ~%" market features)          
     
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))

          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))

         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
           
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))

           (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

                (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-daytrades2b date features))

                (find-best-daytrade2 date stream stream1 date1)

          );;closes stream1



          );closes the stream
          );closes the dolist
     (if (probe-file (string-append *daily-output-dir* "sol-epic.csv"))
       (rename-file (string-append *daily-output-dir* "sol-epic.csv")
                    (string-append "~/mk-data/111-dropbox/sol-epic.csv")))
 
  )) ;;closes the let and defun

(defun day-trades2c (&optional tdate (market-list *epicc-list*)(holidayp nil))
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
   ;      (path-in (string-append *config-dir* "day-features2.dat"))
          )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
    
    (ifn (boundp 'counter)(setq counter 0))
    
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades.dat")
              path2 (string-append *daily-output-dir* "dcr.xml"))
       
       (setq features *day-features2c*)
       (apply #'currency-trade-bins2b features)

        (dolist (market market-list)
            (set-market market)
    ;       (format T "~%Market = ~A   Features = ~A ~%" market features)          
     
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))


          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))


         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
           ; (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))

         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-currencytrades2b date features))

       
         (find-best-currencytrade2 date stream stream1 date1)

          );;closes stream1



          );closes the stream
          );closes the dolist

  )) ;;closes the let and defun


(defun dubai-trades2 (&optional tdate (market-list *dubai-list*)(holidayp nil))
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
   ;      (path-in (string-append *config-dir* "day-features2.dat"))
          )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
    
    (ifn (boundp 'counter)(setq counter 0))
    
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades2.dat")
              path2 (string-append *daily-output-dir* "dcr2.xml"))
    
       (setq features *dubai-features2*)
       (apply #'dubai-trade-bins2b features)

        (dolist (market market-list)
            (set-market market)
        ;   (format T "~%Market = ~A   Features = ~A ~%" market features)          
     
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))


          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))

        ; (format T "Directive = ~A" directive) 
         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
            (format stream "~%~A " (cdr (assoc market *mt-symbol*)))
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))
         ;  (format T "~%path2 = ~A" (contract-month market date))
         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-dubaitrades2b date features))

         
   ; (format T "~%111 date = ~A  date1 = ~A" date date1)
         (find-best-dubaitrade2 date stream stream1 date1)
    ; (format T "~%222")
          );;closes stream1

          );closes the stream
          );closes the dolist
;   (if (probe-file (string-append "~/exitpoints/ninja/mt-epic-" (format nil "~A" date1) ".csv"))
;       (rename-file (string-append "~/exitpoints/ninja/mt-epic-" (format nil "~A" date1) ".csv")
;                    (string-append "~/mk-data/111-dropbox/mt-epic-" (format nil "~A" date1) ".csv")))
  )) ;;closes the let and defun


(defun equity-test (tdate market-list)
  (let ((date (add-mkt-days tdate -40)))
   (dotimes (ith 40)
     (equity-trades2 date market-list nil)
     (setq date (getd date 'ndate)))

)) 
(defun equity-trades2 (&optional tdate (market-list *equity-list*)(holidayp nil))
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
   ;      (path-in (string-append *config-dir* "day-features2.dat"))
          )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
    
    (ifn (boundp 'counter)(setq counter 0))
    
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades2.dat")
              path2 (string-append *daily-output-dir* "dcr2.xml"))

;;    (dolist (ith (directory (string-append *ninja-output-dir* "ts-epic-*.csv")))
  ;           (delete-file ith))
     
       (setq features *equity-features2*)
       (apply #'equity-trade-bins2b features)

        (dolist (market market-list)
            (set-market market)
        ;   (format T "~%Market = ~A   Features = ~A ~%" market features)          
     
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))


          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))

        ; (format T "Directive = ~A" directive) 
         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 9)
           ; (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
           ; (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))
         ;  (format T "~%path2 = ~A" (contract-month market date))
         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-equitytrades2b date features))

        
   ; (format T "~%111 date = ~A  date1 = ~A" date date1)
         (find-best-equitytrade2 date stream stream1 date1)
    ; (format T "~%222")
          );;closes stream1

          );closes the stream
          );closes the dolist
 ;  (if (probe-file (string-append "~/exitpoints/ninja/eq-epic-" (format nil "~A" date1) ".csv"))
 ;      (rename-file (string-append "~/exitpoints/ninja/eq-epic-" (format nil "~A" date1) ".csv")
 ;                   (string-append "~/mk-data/111-dropbox/eq-epic-" (format nil "~A" date1) ".csv")))
  )) ;;closes the let and defun



(defun forex-trades2 (&optional tdate (market-list *forex-list*) (holidayp nil))
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin date1  
          )
         (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
        (set-cat-list)
    	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
     
        (setq path1  (string-append *daily-output-dir* "day-trades2.dat")
              path2 (string-append *daily-output-dir* "dcr2.xml"))
       
        (apply #'forex-trade-bins2b *forex-features2*)
        (dolist (market market-list)
          (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))

          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))

         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
           ; (print-string stream (format nil "~%~A" market) 7)
           (format stream "~%~A " (cdr (assoc market *mt-symbol*)))
            (format stream "  ~A  " (contract-month market tdate))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))))

         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-forextrades2b date *forex-features2*))

     
;  (format T "~%111 ~A~%" *data-name*)
         (find-best-forextrade2 date stream stream1 date1)
                 );;closes stream1
          );closes the stream
          );closes the dolist
   (if (probe-file (string-append *ninja-output-dir* "mt-epic.csv"))
       (rename-file (string-append *ninja-output-dir* "mt-epic.csv")
                    (string-append "~/mk-data/111-dropbox/mt-epic.csv")))
 
  )) ;;closes the let and defun

#|
(defun currency-daytrades (&optional tdate (market-list *currencies-list*)  date1)
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  bin twr-long twr-short
          )
         (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))

	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades.dat") path2 (string-append *daily-output-dir* "dcr.xml"))
        ;(setq path3 (string-append *output-upper-dir* "oec.csv"))

         (apply #'currencies-bins3 *currencies-features1*) ;;;need just once for all markets in list

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert date)))


        (dolist (market market-list)
           (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))


         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
           ; (print-string stream (format nil "~%~A" market) 7)
             (format stream "~A " (get-ninja-symbol market))
        ;    (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))))


         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)
       ; (with-open-file (output-oec path3 :direction :output :if-exists :append :if-does-not-exist :create)


         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-daytrades3b date *currencies-features1*))
       
   ;       (cond ((and (> longs 0)(>= (/ long-gains longs) 100)(> twr-long 1)
   ;                   (member epsignal '(UP OK)))
   ;              (find-best-daytrade3 date stream stream1  date1))
   ;             ((and (> shorts 0)(>= (/ short-gains shorts) 100) (> twr-short 1)
   ;                   (member epsignal '(DOWN OK)))
   ;              (find-best-daytrade3 date stream stream1  date1))
               ; ((and (> longs 0)(<= (/ long-gains longs) -200) (< twr-long 1))
                ; (find-best-contraday-trade1 date stream stream1 date1))
               ; ((and (> shorts 0)(<= (/ short-gains shorts) -200) (< twr-short 1))
                ; (find-best-contraday-trade1 date stream stream1 date1))
      (find-best-daytrade3 date stream stream1  date1)
         ;);;;closes the output-oec
          );;closes stream1



          );closes the stream
          );closes the dolist

  )) ;;closes the let and defun
|#
(defun day-trades4 (&optional tdate (market-list *micro-list*) (holidayp nil))
   (let (date path1  edge-path sample-edge-path directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1 ninja-fore-path3 pf-longs pf-shorts
;          (path-in (string-append *config-dir* "day-features4.dat"))
          (num-markets 3)
          )
         (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long
                   counter bin pf-longs pf-shorts))
        (ifn (boundp 'counter)(setq counter 0))
    ;	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)(setq *nt-pf-list* nil *ts-pf-list* nil)
        (setq path1  (string-append *daily-output-dir* "day-trades4.dat")
             edge-path (string-append *daily-output-dir* "edgemailtemplate2.html")
             sample-edge-path (string-append *daily-output-dir* "sampleedgetemplate.txt")
             ninja-fore-path3 (string-append *ninja-output-dir* "ninja-fore-" (format nil "~A" date1) ".csv")
            )
        (if (probe-file edge-path)(delete-file edge-path))
        (if (probe-file sample-edge-path)(delete-file sample-edge-path))
        (if (probe-file ninja-fore-path3)(delete-file ninja-fore-path3))
 
     ; (setq features *day-features4*) 
      (build-daytrade-warehouse4 *micro-list* tdate)        
      (setq features (find-best-indicator-set4a nil))
      (apply #'day-trade-bins4b T features)
      
        (dolist (market market-list)
           (set-market market)
     ;     (build-daytrade-warehouse4 (list market) tdate);*fore-list* tdate)
     ;     (format T "~%Market = ~A   Features = ~A ~%" market features)               
      
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))

;          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D   "
                          (string-append " ~8," (format nil "~A" (index-digits)) ",0,'*,' F   ")))

         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
          ;  (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))

   ;      (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin pf-longs pf-shorts)
                 (bin-classifier-daytrades4b date features))
                ;  (bin-classifier-daytrades4a date features))

        (format stream "~%BIN = ~A~%" bin)
        (find-best-daytrade4 date stream  date1)
  
  ;        );;closes stream1
   


          );closes the stream
          );closes the dolist

;;;write out Edge trades with only top six based on profit factor
;         (write-nt-edge-best-six)(write-edge-template-best-six edge-path)

;;;write out Edge trades with only top five based on profit factor
        (when *ts-pf-list*
          (write-nt-edge-best-pf num-markets)(write-edge-template-best-pf edge-path num-markets))


         (when (probe-file edge-path) (combine-edge)            
              ; (sample-edge sample-edge-path)
             ); 7/9/2018
  )) ;;closes the let and defun


(defun day-trades4s (&optional tdate (market-list *stocks-list*) (holidayp nil))
   (let (date path1  edge-path directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
;          (path-in (string-append *config-dir* "day-features4.dat"))
          )
         (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
        (ifn (boundp 'counter)(setq counter 0))
 ;   	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades4s.dat")
             edge-path (string-append *daily-output-dir* "edgemailtemplate2s.html"))
        (if (probe-file edge-path)(delete-file edge-path))

          
           (setq features *day-features4s*) 
           (apply #'day-trade-bins4bs features)

        (dolist (market market-list)
            (set-market market)
         
   ;        (format T "~%Market = ~A   Features = ~A ~%" market features)               
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))




;          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and  (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))


         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
           ; (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))


   ;      (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-daytrades4b date features))

       
         (find-best-daytrade4s date stream  date1)

  ;        );;closes stream1
   


          );closes the stream
          );closes the dolist
         (combine-equities-edge)
  )) ;;closes the let and defun


(defun day-trades4x (&optional tdate (market-list *forex-list*)(holidayp nil) )
   (let (date path1  directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
;          (path-in (string-append *config-dir* "day-features4.dat"))
          )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
        (ifn (boundp 'counter)(setq counter 0))
    	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades4x.dat")) ; path2 (string-append *daily-output-dir* "dcr4.xml"))

   ;    (with-open-file (str path-in :direction :input)
   ;       (do ((record (read str nil 'eof) (read str nil 'eof)))
   ;          ((eql record 'eof))
    ;         (push record forefeatures)))    
        
            (setq features *day-features4x*) 
           (apply #'day-trade-bins4bx features)

        (dolist (market market-list)
            (set-market market)
         
   ;        (format T "~%Market = ~A   Features = ~A ~%" market features)               


          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))


          (if (and tdate (readt2 tdate))(setq date tdate))
          (if (and tdate (not (readt2 tdate)))
              (setq date (car (last (month-days (get-latest-index-date)))))) 
          (setq date1 (if (or holidayp (not (readt2 tdate)))
                         (if (or (string-equal (day-of-week date) "friday")
                                 (string-equal (day-of-week date) "thursday"))
                                         (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))




          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))


         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
          ;  (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
            (format stream "  ~A  " (contract-month market date))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))))


   ;      (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-daytrades4bx date features))
       
         (find-best-daytrade4x date stream  date1)

  ;        );;closes stream1



          );closes the stream
          );closes the dolist

  )) ;;closes the let and defun


(defun day-tradesx (&optional tdate (market-list *charger-list*) (holidayp nil))
   (let (date date1 )
     
        (set-cat-list)
     
        (dolist (market market-list)
            (set-market market)
   
           (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
           (if (and tdate (readt2 tdate)) (setq date tdate))
           (if (and tdate (not (readt2 tdate)))
               (setq date (car (last (month-days (get-latest-index-date))))))
           (setq date1 (if (or holidayp (not (readt2 tdate)))
                           (if (or (string-equal (day-of-week date) "friday")
                                   (string-equal (day-of-week date) "thursday"))
                                (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))
  
        (find-best-daytradex date T date1)
     
                );closes the dolist

  )) ;;closes the let and defun

#|
(defun day-trades7 (&optional tdate (market-list *mini-list*)  date1)
   (let (date path1 path2 directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features minifeatures
          (path-in (string-append *config-dir* "day-features7.dat"))
          )
         (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
        (ifn (boundp 'counter)(setq counter 0))
    	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1  (string-append *daily-output-dir* "day-trades7.dat") path2 (string-append *daily-output-dir* "dcr7.xml"))

       (with-open-file (str path-in :direction :input)
          (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record minifeatures)))    

        (dolist (market market-list)
            (set-market market)
            (setq features (cdr (assoc market minifeatures)))
     
           (apply #'day-trade-bins features)

          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))


         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
            ;(format stream "  ~A  " (nth 2 (assoc market *c-list*)))
            (format stream "  ~A  " (contract-month market tdate))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A    " (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))))


         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-daytrades7a date features))

      
         (find-best-daytrade7 date stream stream1 date1)

          );;closes stream1



         );closes the stream
          );closes the dolist

  )) ;;closes the let and defun
|#



;;;;builds trade action files for testing TS automation

(defun ts-test (tdate)
 (let (date days); paths)
    (maind-x)  (read-markets-config)
    (set-market 'dj.d1b)(setq days (length (month-days (round (/ tdate 100)))))
    (setq date (second (reverse (month-days (round (/ tdate 100))))))
   (build-daytrade-warehouse4 *fore-list*)
   (apply #'day-trade-bins4b T *day-features4*)   
  (dotimes (ith days)
   ;(setq *fore-list* *day-list*)
   (if (= (next-mkt-day2 date) (getd date 'ndate))(day-swing-trades date)(day-swing-trades date T))
   (setq date (getd date 'ydate))
  )
 ; (setq paths (directory (string-append "~/exitpoints/ninja/ts-fore-" "*.*")))
 ; (apply #'my-append-files "~/mk-data/111-dropbox/ts-fore.csv" paths)

)) 



(defun day-swing-trades (&optional tdate (holidayp nil))
  (let ((outfile t) date1 (counter 0) ) ;counter is to increment the oec order id number
   (declare (special counter))

    (maind-x)  (read-markets-config)
   (setq date1 (if holidayp (if (or (string-equal (day-of-week tdate) "friday")
                                           (string-equal (day-of-week tdate) "thursday")) 
                                         (add-days-to-date tdate 4)(add-days-to-date tdate 2)) 
                     (if (string-equal (day-of-week tdate) "friday")
                         (add-days-to-date tdate 3)(add-days-to-date tdate 1))))

	(ifn tdate (setq tdate (car (last (month-days (get-latest-index-date))))))
    (setq *fore-list* *micro-list*)
;   (swing-open-equity-report tdate)
;   (position-open-equity-report tdate)

     ;   (dolist (ith (directory (string-append *daily-output-dir* "*charger.csv")))
      ;       (delete-file ith))
     ;   (dolist (ith (directory (string-append *daily-output-dir* "*starter.csv")))
      ;       (delete-file ith))

     ;   (dolist (ith (directory (string-append *daily-output-dir* "*starter2.csv")))
      ;       (delete-file ith))

     ;   (dolist (ith (directory (string-append *daily-output-dir* "*score.csv")))
      ;       (delete-file ith))
    ;    (dolist (ith (directory (string-append *daily-output-dir* "*entry.csv")))
     ;        (delete-file ith))

    
        (if (and outfile (probe-file (string-append *daily-output-dir* "day-trades.dat")))
            (delete-file (string-append *daily-output-dir* "day-trades.dat")))
        (if (and outfile (probe-file (string-append *daily-output-dir* "dcr.xml")))
            (delete-file (string-append *daily-output-dir* "dcr.xml")))
       

    ;     (dolist (ith (directory (string-append *daily-output-dir* "ts-triump21*.*")))
     ;        (delete-file ith))


      ;   (dolist (ith (directory (string-append *daily-output-dir* "ts-triumph11*.*")))
       ;      (delete-file ith))

        (if (probe-file (string-append *ninja-output-dir* "ninja-fore-" (format nil "~A\.csv" date1)))
            (delete-file (string-append *ninja-output-dir* "ninja-fore-" (format nil "~A\.csv" date1))))

        (if (probe-file (string-append *ninja-output-dir* "ts-fore-" (format nil "~A\.csv" date1)))
            (delete-file (string-append *ninja-output-dir* "ts-fore-" (format nil "~A\.csv" date1))))



;	 (if (and outfile (probe-file (string-append *daily-output-dir* "swing-trades4.dat")))
;            (delete-file (string-append *daily-output-dir* "swing-trades4.dat")))
; 	 (if (and outfile (probe-file (string-append *daily-output-dir* "swing-trades.dat")))
;            (delete-file (string-append *daily-output-dir* "swing-trades.dat")))

  ;   (with-open-file (stream (string-append *daily-output-dir* "day-trades.dat")
  ;                           :direction :output :if-exists :append :if-does-not-exist :create)
  ;       (format stream "~A~%" (date-convert date1)))

     (with-open-file (stream1 (string-append *daily-output-dir* "dcr.xml")
                               :direction :output :if-exists :append :if-does-not-exist :create)
          (format stream1 "<?xml version=\"1.0\" ?>~% <trades>~%"))
     (build-daytrade-warehouse4 *micro-list*)
     (setq *day-features4* (find-best-indicator-set4a nil))
       
   #|
      (setq *day-features3* *features-us-bonds3* )
      (build-daytrade-warehouse3 '(us.d1b))
      (day-trades tdate (intersection '(us.d1b) *triumph21-list*) holidayp)
 
      (setq *day-features3* *features-sectors31*)
      (build-daytrade-warehouse3
          (remove 'cl.d1b (append (remove 'cp.d1b (remove 'si.d1b *metals-list*))
                                  (remove 'hu.d1b *energies-list*))))
      (day-trades tdate (intersection
                        (remove 'cl.d1b (append (remove 'cp.d1b (remove 'si.d1b *metals-list*))
                                                (remove 'hu.d1b *energies-list*) 
                                                ))
                                  *triumph21-list*) holidayp)
      
       
             (setq *day-features3* *features-crude-oil3*)
             (build-daytrade-warehouse3 '(cl.d1b))
             (day-trades tdate (intersection *triumph21-list* '(cl.d1b)) holidayp)
        
             (setq *day-features3* *features-rbob3*)
             (build-daytrade-warehouse3 '(hu.d1b))
             (day-trades tdate (intersection *triumph21-list* '(hu.d1b)) holidayp)
           
             (setq *day-features3* *features-silver3*)
             (build-daytrade-warehouse3 '(si.d1b))
             (day-trades tdate (intersection *triumph21-list* '(si.d1b)) holidayp)
           
             (setq *day-features3* *features-copper3*)
             (build-daytrade-warehouse3 '(cp.d1b))
             (day-trades tdate (intersection *triumph21-list* '(cp.d1b)) holidayp)
           
            (setq *day-features3* *features-dowjones3*)
             (build-daytrade-warehouse3 '(dj.d1b))
             (day-trades tdate (intersection *triumph21-list* '(dj.d1b)) holidayp)
  
            (setq *day-features3* *features-s&p3*)
             (build-daytrade-warehouse3 '(sp.d1b))
             (day-trades tdate (intersection *triumph21-list* '(sp.d1b)) holidayp)
 
             (setq *day-features3* *features-nikkei3*)
             (build-daytrade-warehouse3 '(nk.d1b))
             (day-trades tdate (intersection *triumph21-list* '(nk.d1b)) holidayp)

         ;    (setq *day-features3* *features-nasdaq3*)
         ;    (build-daytrade-warehouse3 '(nd.d1b))
         ;    (day-trades tdate (intersection *triumph21-list* '(nd.d1b)) holidayp)
 
          ;   (setq *day-features3* *features-indexes3* )
          ;   (build-daytrade-warehouse3 '(ru.d1b))
          ;   (day-trades tdate (intersection '(ru.d1b) *triumph21-list*) holidayp)
 
            
             (setq *day-features3* *features-soybeans3*)
             (build-daytrade-warehouse3 *grains-list*)
             (day-trades tdate (intersection *triumph21-list* '(s.d1b)) holidayp)
             
             (setq *day-features3* *features-wheat3*)
             (build-daytrade-warehouse3 '(w.d1b))
             (day-trades tdate (intersection *triumph21-list* '(w.d1b)) holidayp)
        
             (setq *day-features3* *features-soymeal3*)
             (build-daytrade-warehouse3 '(sm.d1b))
             (day-trades tdate (intersection *triumph21-list* '(sm.d1b)) holidayp)
          
             (setq *day-features3* *features-soyoil3*)
             (build-daytrade-warehouse3 '(bo.d1b))
             (day-trades tdate (intersection *triumph21-list* '(bo.d1b)) holidayp)
            
             (setq *day-features3* *features-cocoa3*)
             (build-daytrade-warehouse3 *softs-list*)
             (day-trades tdate (intersection *triumph21-list* '(cc.d1b)) holidayp)
            
             (setq *day-features3* *features-cotton3*)
             (build-daytrade-warehouse3 *softs-list*)
             (day-trades tdate (intersection *triumph21-list* '(ct.d1b)) holidayp)
            
             (setq *day-features3* *features-sugar3*)
             (build-daytrade-warehouse3 *softs-list*)
             (day-trades tdate (intersection *triumph21-list* '(su.d1b)) holidayp)
     
             (setq *day-features3* *features-coffee3*)
             (build-daytrade-warehouse3 '(cf.d1b))
             (day-trades tdate (intersection *triumph21-list* '(cf.d1b)) holidayp)
     
             (setq *day-features3* *features-livecattle3*)
             (build-daytrade-warehouse3 '(lc.d1b))
             (day-trades tdate (intersection *triumph21-list* '(lc.d1b)) holidayp)
                 
     |# 
     
;  (swing-trades4 tdate  *swing-list* outfile date1)
 ; (position-trades tdate  *EP-position-list* outfile date1)
 ; (forex-swing-trades tdate  *forex-list* outfile date1)
;  (x-treme2 tdate *X-list* date1)
;  (exitpoints-open-equity-report tdate )

;  (with-open-file (stream (string-append *daily-output-dir* "day-trades2.dat")
;                            :direction :output :if-exists :append :if-does-not-exist :create)
;     (format stream "~A~%" (date-convert date1)))

;  (with-open-file (stream1 (string-append *daily-output-dir* "dcr2.xml") :direction :output :if-exists :append :if-does-not-exist :create)
 ;    (format stream1 "<?xml version=\"1.0\" ?>~% <trades>~%"))
;   (day-trades2c tdate *epicc-list* holidayp);;;only for Score
 ;  (forexday-trades2 tdate *forex-list* date1)

;    (if (probe-file (string-append *daily-output-dir* "sol-score.csv"))
 ;      (rename-file (string-append *daily-output-dir* "sol-score.csv")
 ;                   (string-append "~/mk-data/111-dropbox/sol-score.csv")))
 ;   (if (probe-file (string-append *daily-output-dir* "sol-starter.csv"))
 ;      (rename-file (string-append *daily-output-dir* "sol-starter.csv")
 ;                   (string-append "~/mk-data/111-dropbox/sol-starter.csv")))
 
;    (dolist (ith (directory (string-append *daily-output-dir* "ts-triumph21*.*")))
;             (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))

;    (dolist (ith (directory (string-append *daily-output-dir* "ts-triumph11*.*")))
 ;            (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))

    (fore-trades tdate holidayp)
    (build-swingtrade-warehouse aa);*micro-list*)
    (swing-trades tdate aa holidayp) ;(append '(c.d1b w.d1b) *core-list*))
  ; (day-trades tdate holidayp  *micro-list9*)


 ;  (my-copy-file (string-append "~/mk-data/111-dropbox/ts/ts-triumph21-" (format nil "~A.csv" date1))
 ;                "~/mk-data/111-dropbox/ts-t21.csv")

   (if (probe-file (string-append "~/exitpoints/ninja/ninja-fore-" (format nil "~A.csv" date1)))
         (my-copy-file (string-append "~/exitpoints/ninja/ninja-fore-" (format nil "~A.csv" date1))
                 "~/mk-data/111-dropbox/nt-e12.csv")
       (if (probe-file "~/mk-data/111-dropbox/nt-e12.csv")
           (delete-file "~/mk-data/111-dropbox/nt-e12.csv"))
        )

  (if (probe-file (string-append "~/exitpoints/ninja/ts-fore-" (format nil "~A.csv" date1)))
         (my-copy-file (string-append "~/exitpoints/ninja/ts-fore-" (format nil "~A.csv" date1))
                 "~/mk-data/111-dropbox/ts-e12.csv")
       (if (probe-file "~/mk-data/111-dropbox/ts-e12.csv")
           (delete-file "~/mk-data/111-dropbox/ts-e12.csv"))
        )

        


   ));;closes the defun


(defun epic-trades (&optional tdate (holidayp nil))
  (let ((outfile t)  (counter 0)  ) ;counter is to increment the oec order id number
   (declare (special counter))
   (maind-x)  (read-markets-config)
        (dolist (ith (directory (string-append *daily-output-dir* "*epic.csv")))
             (delete-file ith))
          (dolist (ith (directory (string-append *daily-output-dir* "*ts-epic*.csv")))
             (delete-file ith))
         (dolist (ith (directory (string-append *daily-output-dir* "*ts-forex*.csv")))
             (delete-file ith))

        (if (and outfile (probe-file (string-append *daily-output-dir* "futures-view.txt")))
            (delete-file (string-append *daily-output-dir* "futures-view.txt")))
        (if (and outfile (probe-file (string-append *daily-output-dir* "equity-view.txt")))
            (delete-file (string-append *daily-output-dir* "equity-view.txt")))
        (if (and outfile (probe-file (string-append *daily-output-dir* "forex-view.txt")))
            (delete-file (string-append *daily-output-dir* "forex-view.txt")))
     
        (if (and outfile (probe-file (string-append *daily-output-dir* "day-trades2.dat")))
            (delete-file (string-append *daily-output-dir* "day-trades2.dat")))
          
        (if (and outfile (probe-file (string-append *daily-output-dir* "dcr2.xml")))
            (delete-file (string-append *daily-output-dir* "dcr2.xml")))
       
 ;      (if (probe-file (string-append *ninja-output-dir* "mt-epic-" (format nil "~A\.csv" date1)))
  ;          (delete-file (string-append *ninja-output-dir* "mt-epic-" (format nil "~A\.csv" date1))))
;         (dolist (ith (directory (string-append "~/mk-data/111-dropbox/" "mt-epic-*.csv")))
;                  (delete-file ith))
 
         (dolist (ith (directory (string-append *ninja-output-dir* "mt-epic*.csv")))
                  (delete-file ith))
 
;   (with-open-file (stream (string-append *daily-output-dir* "day-trades2.dat")
;                            :direction :output :if-exists :append :if-does-not-exist :create)
;      (format stream "~A~%" (date-convert date1)))
   (with-open-file (stream1 (string-append *daily-output-dir* "dcr2.xml")
                              :direction :output :if-exists :append :if-does-not-exist :create)
     (format stream1 "<?xml version=\"1.0\" ?>~% <trades>~%"))
  
    (day-trades2 tdate  *epic-list* holidayp);;Epic and futures View
    (equity-trades2 tdate *equity-list* holidayp);;Equity View and TS-epic
 
    (dubai-trades2 tdate *dubai-list* holidayp);;;Dubai MT-epic
    (forex-trades2 tdate *forex-list*  holidayp);;;Forex View and MT-epic 


    (dolist (ith (directory (string-append *daily-output-dir* "ts-epic*.*")))
             (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))

    (dolist (ith (directory (string-append *daily-output-dir* "ts-forex*.*")))
             (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))

    
  ));;closes the defun



(defun fore-trades (&optional tdate (holidayp nil))
  (let ((outfile t)  (counter 0) ) ;counter is to increment the oec order id number
   (declare (special counter))

    (maind-x)  (read-markets-config)
  ;  (ifn tdate (setq tdate (car (last (month-days (get-latest-index-date))))))

 ;  (setq date1 (if holidayp (if (or (string-equal (day-of-week tdate) "friday")
 ;                                          (string-equal (day-of-week tdate) "thursday"))
 ;                                        (add-days-to-date tdate 4)(add-days-to-date tdate 2))
 ;                    (if (string-equal (day-of-week tdate) "friday")(add-days-to-date tdate 3)
 ;                         (add-days-to-date tdate 1))))

        (dolist (ith (directory (string-append *daily-output-dir* "*fore.txt")))
             (delete-file ith))

        (dolist (ith (directory (string-append *daily-output-dir* "*cfd.txt")))
             (delete-file ith))
         (dolist (ith (directory (string-append *daily-output-dir* "edge-for-*.csv")))
          (delete-file ith))
           (dolist (ith (directory (string-append *daily-output-dir* "*fore.txt")))
              (delete-file ith))

 
  ;      (if (probe-file (string-append *ninja-output-dir* "ninja-fore-" (format nil "~A\.csv" date1)))
  ;        (delete-file (string-append *ninja-output-dir* "ninja-fore-" (format nil "~A\.csv" date1))))
 
        (if (and outfile (probe-file (string-append *daily-output-dir* "day-trades4.dat")))
            (delete-file (string-append *daily-output-dir* "day-trades4.dat")))

     ;   (if (and outfile (probe-file (string-append *daily-output-dir* "day-trades4x.dat")))
      ;      (delete-file (string-append *daily-output-dir* "day-trades4x.dat")))

;   (with-open-file (stream (string-append *daily-output-dir* "day-trades4.dat")
;                            :direction :output :if-exists :append :if-does-not-exist :create)
;     (format stream "~A~%" (date-convert date1)))
    
    (day-trades4 tdate *micro-list* holidayp)
  
   ; (day-trades4x tdate *forex-list* holidayp )


 (dolist (ith (directory (string-append *daily-output-dir* "ts-fore*.*")))
             (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))

 ;   (day-trades4s tdate *stocks-list* holidayp)
    ));;closes the defun

#|
(defun forex-trades (&optional tdate (holidayp nil))
  (let ((outfile t) date1 (counter 0) ) ;counter is to increment the oec order id number
   (declare (special counter))

    (maind-x)  (read-markets-config)(set-market 'dj.d1b)
    (ifn tdate (setq tdate (car (last (month-days (get-latest-index-date))))))

   (setq date1 (if holidayp (if (or (string-equal (day-of-week tdate) "friday")
                                           (string-equal (day-of-week tdate) "thursday"))
                                         (add-days-to-date tdate 4)(add-days-to-date tdate 2))
                     (if (string-equal (day-of-week tdate) "friday")(add-days-to-date tdate 3)
                          (add-days-to-date tdate 1))))

        (dolist (ith (directory (string-append *daily-output-dir* "*forex.txt")))
             (delete-file ith))

        (if (and outfile (probe-file (string-append *daily-output-dir* "day-trades4x.dat")))
            (delete-file (string-append *daily-output-dir* "day-trades4x.dat")))

   (with-open-file (stream (string-append *daily-output-dir* "day-trades4x.dat")
                            :direction :output :if-exists :append :if-does-not-exist :create)
     (format stream "~A~%" (date-convert date1)))

     (day-trades4x tdate *forex-list* date1)
    ));;closes the defun



(defun currency-trades (&optional tdate (holidayp nil))
  (let ((market-list *currencies-list*) (outfile t) date1)
  (setq date1 (if holidayp (if (or (string-equal (day-of-week tdate) "friday")
                                            (string-equal (day-of-week tdate) "thursday"))
                                         (add-days-to-date tdate 4)(add-days-to-date tdate 2))
                     (if (string-equal (day-of-week tdate) "friday")(add-days-to-date tdate 3)(add-days-to-date tdate 1))))

;  (forex-scott tdate market-list)
  (currency-daytrades tdate market-list  date1)
  (forex-swing-trades tdate  market-list outfile date1)
;  (shell "cp /home/mk-date/exitpoints/forex.dat /home/mk-data/xml/forex.xml")

  ))
|#
#|
(defun currency-daytrades (&optional tdate (period 4)(market-list *currencies-list*) (outfile t) date1)
   (let ((counter 0) date path1 path2 directive profit (num-trades 0) trades
         (P&L 0) new-profit new-trades (new-P&L 0) (new-num-trades 0) sum-trade-list optimal trade-list)

	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1  (string-append *output-upper-dir* "currency-daytrades.dat") path2 (string-append *output-upper-dir* "currency.dat")
              )

        (if (and outfile (probe-file path1))
            (delete-file path1))
        (if (and outfile (probe-file path2))
            (delete-file path2))


        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert date)))

        (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)
          (format stream1 "<?xml version=\"1.0\" ?>~% <trades>~%"))


        (dolist (market market-list)
          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D~%"
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F~%")))





        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 7)
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~7@A ~%" (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))))


        (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

           (multiple-value-setq  (Profit trades new-profit new-trades trade-list) (find-best-daytrade3 date stream stream1 date1))
           (setq P&L (+ P&L profit) num-trades (+ num-trades trades)  new-P&L (add new-P&L (or new-profit 0))
           new-num-trades (+ new-num-trades (or new-trades 0))  sum-trade-list (append sum-trade-list trade-list))
           )



          );closes the stream
          );closes the dolist
      (setq optimal (optimal-f sum-trade-list))
      (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
          (format stream "~%P&L= ~D  ~A     New P&L= ~D New Num Trades= ~A~%" (round P&L) num-trades (round new-P&L) new-num-trades)
          (format stream "~%$/Contract= ~D~%" optimal)
           (format stream "~%"))

))

|#
#|
(defun forex-swing-trades (&optional tdate (market-list *forex-list*) (outfile t) date1)
   (let ((counter 0) date  path1 path2 directive  trades profit (twr-long 1) (twr-short 1) bin
          new-profit new-trades   trade-list epsignal longs long-gains long-acc shorts short-gains short-acc )
      
        (declare (special epsignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin))
	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

        (setq path1 (string-append *daily-output-dir* "forex-swing-trades.dat") path2 (string-append *daily-output-dir* "dcr.xml")
              )
        (if (and outfile (probe-file path1))
            (delete-file path1))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert date)))


        (dolist (market market-list)
          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (index-digits)
                                   (zerop (index-digits)))  " ~D~%"
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F~%")))


        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~A" market) 7)
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~7@A ~%" (convert-to-32nds (getd tdate 'close)))
               (format stream directive
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))
                     ))

         (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-swings1 date *swing-features1*))
         (setq twr-short (swing-bin-twr1 bin))(setf (nth 0 bin) 1)
         (setq twr-long (swing-bin-twr1 bin))
         (multiple-value-setq (profit trades new-profit new-trades trade-list)
             (find-best-swing-trade1 date stream stream1 date1))

            );;;closes stream1


         (terpri stream)

          );;;closes the stream
          );;;closes the dolist

))
|#
(defun trend-trigger-factor (date &optional (param 15))
  (let (high0-14 high15-29 low0-14 low15-29 sell-power buy-power)

   (setq high0-14 (n-day-high date param)
         high15-29 (n-day-high (add-mkt-days date (- param)) param)
         low0-14 (n-day-low date param)
         low15-29 (n-day-low (add-mkt-days date (- param)) param))

   (setq sell-power (- high15-29 low0-14)
         buy-power (- high0-14 low15-29))

   (* 100 (/ (- buy-power sell-power)
             (* .5 (+ buy-power sell-power))))
   ))

(defun ave-diff-days (end-date period mkt-days cal-days)
  (let* ((start-day (add-mkt-days end-date (- period))) (date start-day) aves)
   (loop
     (when (equal (add-mkt-days date (- mkt-days))
                  (add-days-to-date1 date (- cal-days)))
      (push
       (abs (- (/ (getd date 'close)
                  (add1 (getd (add-mkt-days date (- mkt-days)) 'close)
                        (getd date 'rollover))) 1)) aves)
         ); closes the when
     (setq date (getd date 'ndate))
     (if (or (not date)(>= date end-date)) (return))
     );;closes the loop

     (values (/ (list-sum aves) (length aves)) (length aves))
     ))



(defun find-best-trend-test-stop-entry (tdate num &optional (output T))
  (let ((best-param 0) (result 0) (prev-result -10000))

  (do ((param .50 (+ param .01)))
      ((> param 2) best-param)
      ;(format T "~A~%" param)
      (setq result
          (trend-test-stop-entry tdate num param nil))

      (if (> result prev-result) (setq prev-result result best-param param)))
  ;(when output
    (format output "~7,2,,,F" best-param)
    ;(format output "    Current Signal= ~A~%" (vsignals tdate dur best-param))
    ;(volatility-test tdate num best-param dur output)
    ;(multiple-value-setq (vlow vhigh)(vprices tdate dur (* (gethash tdate HT) best-param)))

   ;   (if (member *data-name* '(US.D1B TY.D1B))
    ;      (format output "~%SELL= ~7@A BUY= ~7@A~%" (convert-to-32nds vlow)(convert-to-32nds vhigh))
     ;    (format output "~%SELL= ~7,4,,,F BUY= ~7,4,,,F~%" vlow vhigh))
      ;(if (member *data-name* '(US.D1B TY.D1B))
       ;   (format output "today's LOW= ~7@A today's HIGH= ~7@A" (convert-to-32nds (getd tdate 'low))(convert-to-32nds (getd tdate 'high)))
        ; (format output "today's LOW= ~7,4,,,F today's HIGH= ~7,4,,,F~%" (getd tdate 'low) (getd tdate 'high))))

    (trend-test-stop-entry tdate num best-param T)
      ))

(defun pivots (&optional tdate
                          (market-list *day-list*) (outfile t))
  (let (ppd ppw ppm cwhl cmhl fibs4 fibs9 fibs4R fibs4S fibs9R fibs9S date path1)


	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
        (set-cat-list)
        (setq path1 (string-append *output-upper-dir* "pivots.dat"))
        (if (and outfile (probe-file path1))
            (delete-file path1))

        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%MKT   CLOSE  ~%" (date-convert date)))
        (dolist (market market-list)
            (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq ppd (pivot-points date)
                ppw (pivot-points-weekly date)
                ppm (pivot-points-monthly date)
                cwhl (current-week-high-low date)
                cmhl (current-month-high-low date)
                fibs4 (fibonacci-proj date 4)
                fibs9 (fibonacci-proj date 9))
           (setq fibs4S (butlast fibs4 5)
                 fibs4R (last fibs4 5)
                 fibs9S (butlast fibs9 5)
                 fibs9R (last fibs9 5))



        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (print-string stream (format nil "~%~A" market) 4)

            (if (member market '(TY.D1B US.D1B))
                  (format stream " ~7@A " (convert-to-32nds (getd date 'close)))
                  (format stream " ~7F "
                  (* (index-tick-size)(round (getd date 'close) (index-tick-size))) ))
             (dolist (ith (append cwhl cmhl))
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
             (terpri stream)
             (dolist (ith ppd)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
            (terpri stream)
             (dolist (ith ppw)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
            (terpri stream)
             (dolist (ith ppm)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
              (terpri stream) (terpri stream)

             (dolist (ith fibs4S)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
              (terpri stream)
             (dolist (ith fibs4R)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
              (terpri stream)(terpri stream)
               (dolist (ith fibs9S)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
              (terpri stream)
                (dolist (ith fibs9R)
                 (if (member market '(TY.D1B US.D1B))
                     (format stream " ~7@A " (convert-to-32nds ith))
                   (format stream " ~7F " (* (index-tick-size)(round ith (index-tick-size)))))
                   )
              (terpri stream)
           ))))


(defun fibonacci-proj (tdate size)
  (let (targ bpl p0 p1 p2 S1 S2 S3 S4 S5 S6 S7 R1 R2 R3 R4 R5 R6 R7)
  

  (multiple-value-setq (bpl targ p0 p1 p2) (channel-trend tdate size))

  (when (< p0 p1)
     (setq R1 (+ p0 (* .618 (- p1 p2))) R2 (+ p0 (* 1.0 (- p1 p2)))
           R3 (+ p0 (* 1.618 (- p1 p2))) R4 (+ p0 (* 2.618 (- p1 p2)))
           S1 (- p1 (* .382 (- p1 p2)))
           S2 (- p1 (* .50 (- p1 p2))) S3 (- p1 (* .618 (- p1 p2)))
           S4 (- p1 (* .786 (- p1 p2)))
           S5 (- p1 (* 1.272 (- p1 p2))) S6 (- p1 (* 1.618 (- p1 p2)))
           S7 (- p1 (* 2.618 (- p1 p2)))))

   (WHEN (> P0 P1)
      (setq S1 (- p0 (* .618 (- p2 p1))) S2 (- p0 (* 1.0 (- p2 p1)))
            S3 (- p0 (* 1.618 (- p2 p1))) S4 (- p0 (* 2.618 (- p2 p1)))
            R1 (+ p1 (* .382 (- p2 p1)))
            R2 (+ p1 (* .50 (- p2 p1))) R3 (+ p1 (* .618 (- p2 p1)))
            R4 (+ p1 (* .786 (- p2 p1)))
            R5 (+ p1 (* 1.272 (- p2 p1))) R6 (+ p1 (* 1.618 (- p2 p1)))
            R7 (+ p1 (* 2.618 (- p2 p1)))))



    (sort (if (< p0 p1)(list S7 S6 S5 S4 S3 S2 S1 R1 R2 R3)
                (list S3 S2 S1 R1 R2 R3 R4 R5 R6 R7)) #'<)




      ))

(defun zero-proj (tdate size)
 (let (p0 p1 p2 p3 p4 p5 p6 p7)

 (multiple-value-setq (p0 p1 p2 p3 p4 p5 p6 p7) (zero-trend tdate size))

 (sort (list
         (exp (+ (log p4)(- (log p5)(log p7))))
         (exp (+ (log p3)(- (log p4)(log p6))))
         (exp (+ (log p2)(- (log p3)(log p5))))
         (exp (+ (log p1)(- (log p2)(log p4))))) #'<)

  ))

(defun zero-proj1 (tdate size)
 (let (p0 p1 p2 p3 p4 p5 p6 p7)

 (multiple-value-setq (p0 p1 p2 p3 p4 p5 p6 p7) (zero-trend tdate size))

 (values (if (> p1 p2) 'DN 'UP)
        
         (exp (+ (log p4)(- (log p5)(log p7))));;;z-1
         (exp (+ (log p3)(- (log p4)(log p6))));;z0
         (exp (+ (log p2)(- (log p3)(log p5)))) ;;z1
         (exp (+ (log p1)(- (log p2)(log p4))));;z2
         p0
)  
))

#|
(defun zero-signal (tdate size)
 (let (dir0 z-1 z0 z1 z2)

 (multiple-value-setq (dir0 z-1 z0 z1 z2) (zero-proj tdate size))

 (cond ((and (eql dir0 'UP)(> z2 z1)) 'DN)
       ((and (eql dir0 'DN)(< z2 z1)) 'UP)

  ))

(defun zero-signal1 (tdate size)
 (let (dir0 z-1 z0 z1 z2)

 (multiple-value-setq (dir0 z-1 z0 z1 z2) (zero-proj tdate size))

 (cond ((and (eql dir0 'UP)(< z0 z1 z2)) 'DN) ; 3-step
       ((and (eql dir0 'DN)(> z0 z1 z2)) 'UP) ; 3-step
       ((and (eql dir0 'UP)(< z-1 z0 z1)) 'DN) ; last ditch
       ((and (eql dir0 'DN)(> z-1 z0 z1)) 'UP) ; last ditch
       (t 'FT))
  ))
|#
(defun zero-strength (tdate size)
 (let (dir0 z-1 z0 z1 z2 p0)

 (multiple-value-setq (dir0 z-1 z0 z1 z2 p0) (zero-proj1 tdate size))

 (cond ((and (<= p0 z1)(< z0 z1 z2)) '3SU) ; 3-step BEARISH
       ((and (>= p0 z1)(> z0 z1 z2)) '3SD) ; 3-step BULLISH
       ((and (<= p0 z1)(< z-1 z0 z1)) 'LDU) ; last ditch BEARISH
       ((and (>= p0 z1)(> z-1 z0 z1)) 'LDD) ; last ditch BULLISH
       ((and (eql dir0 'UP)(< p0 z2)(< p0 z1)) 'WUP);;WEAK UP
       ((and (eql dir0 'DN)(> p0 z2)(> p0 z1)) 'WDN)
       ((and (eql dir0 'UP)(> p0 z2)(> p0 z1)) 'SUP);;STRONG UP
       ((and (eql dir0 'DN)(< p0 z2)(< p0 z1)) 'SDN)
       ((and (eql dir0 'UP)(<= p0 z1)) 'UPT);;MOVING UP TOWARDS TARGET
       ((and (eql dir0 'DN)(>= p0 z1)) 'DNT)
       ((and (eql dir0 'UP)(> p0 z1)) 'AZT);;;MOVING UP ABOVE TARGET
       ((and (eql dir0 'DN)(< p0 z1)) 'BZT)
       (t 'FT))
  ))


(defun zero-strength1 (tdate size)
 (let (dir0 z-1 z0 z1 z2 (tdatec (getd tdate 'close)))
 
 (multiple-value-setq (dir0 z-1 z0 z1 z2) (zero-proj1 tdate size))

 (cond ((and (< tdatec z0)(< tdatec z1)) 'DN)
       ((and (> tdatec z0)(> tdatec z1)) 'UP)
       (t 'FT))
  ))

(defun zero-strength2 (tdate size)
 (let (dir0 z-1 z0 z1 z2 (tdatec (getd tdate 'close)))

 (multiple-value-setq (dir0 z-1 z0 z1 z2) (zero-proj tdate size))

 (cond ((and (< tdatec z0)(< tdatec z1)) 'DN)
       ((and (> tdatec z0)(> tdatec z1)) 'UP)
       (t 'FT))
  ))

;;;;this is for FORE trades (day4)
(defun zero-strength345 (date)
  (let (zb3 zb4 zb5)
  (setq zb3 (zero-strength date 3) zb4 (zero-strength date 4) zb5 (zero-strength date 5))

  (+ 
     (cond ((member zb3 '(3SD 3SU SUP)) 1)
           ((member zb3 '(WUP BZT UPT)) -1)
           (t 0))
     (cond ((member zb4 '(LDD AZT 3SD)) 1)
           ((member zb4 '(WUP WDN SDN)) -1)
           (t 0))
     (cond ((member zb5 '(AZT SUP BZT)) 1)
           ((member zb5 '(SDN 3SD WDN)) -1)
           (t 0))
 )))

(defun zero-strength-swing11 (date)
  (let ((dir (parabolic-stops date)) (zb (zero-strength date 11)))
       
  (cond ((and (eql dir 'SHORT) (member zb '(LDU SDN SUP DNT))) 1)
        ((and (eql dir 'SHORT)(member zb '(LDU UPT 3SD BZT))) 0)
        ((and (eql dir 'LONG)(member zb '(WUP SDN 3SD AZT))) -1)
        ((and (eql dir 'LONG) (member zb '(3SU LDU LDD WDN))) 0)
        (t 0))
))


(defun zero-strength-swing345 (date)
  (let ((dir (mvhl-trend date)) (zb3 (zero-strength date 3))
       (zb4 (zero-strength date 4)) (zb5 (zero-strength date 5)))
(+
  (cond ((and (eql dir -1) (member zb3 '(WDN LDD BZT SDN))) 1)
        ((and (eql dir -1)(member zb3 '(3SD 3SU WUP))) 0)
        ((and (eql dir 1)(member zb3 '(LDD LDU DNT))) -1)
        ((and (eql dir 1) (member zb3 '(3SU WDN 3SD))) 0)
        (t 0))
 (cond ((and (eql dir -1) (member zb4 '(WDN LDD BZT SDN))) 1)
        ((and (eql dir -1)(member zb4 '(UPT LDU 3SU))) 0)
        ((and (eql dir 1)(member zb4 '(LDU UPT))) -1)
        ((and (eql dir 1) (member zb4 '(WDN SDN 3SD AZT))) 0)
        (t 0))
 (cond ((and (eql dir -1) (member zb5 '(WND BZT DNT SDN))) 1)
        ((and (eql dir -1)(member zb5 '(SUP 3SD WUP LDU))) 0)
        ((and (eql dir 1)(member zb5 '(WUP WDN DNT LDU))) -1)
        ((and (eql dir 1) (member zb5 '(SUP UPT SDN))) 0)
        (t 0))
)



))



(defun zero-trend (tdate size)
  (let ((filt *n-filt*)(time-interval *time-interval*)
         p0 p1 p2 p3 p4 p5 p6 p7 prices ignore low high
        (starttime (conv-to-string (add-mkt-days tdate (* size -8)) 'A)))
     
        (setq *n-filt* size *time-interval* 'daily-high-low)(nil-tpv)
        (loop
          (multiple-value-setq (prices ignore ignore ignore ignore)
             (find-all-primitives starttime  (conv-to-string tdate 'P)))
          (if (> (length prices) 6) (return)
             (setq starttime (conv-to-string (add-mkt-days tdate (1- (* size -25))) 'A))))

        (setq low (getv 0 LP) high (getv 0 HP))

        (setq p1 (first prices) p2 (second prices) p3 (third prices) p4 (nth 3 prices)
           p5 (nth 4 prices) p6 (nth 5 prices) p7 (nth 6 prices)
             )

       (if (> P2 P1) (setq p0 high)(setq p0 low))

      (setq *n-filt* filt *time-interval* time-interval)
    (values p0 p1 p2 p3 p4 p5 p6 p7)
))

;;;for swing trades
(defun bpl-signal (tdate size)
  (let ((bpl (channel-trend tdate size)) (mo (momentum tdate size)))
   (cond ((and (> (getd tdate 'close) bpl) (eql mo 'up)) 'UP)
         ((and (< (getd tdate 'close) bpl) (eql mo 'DN)) 'DN)
         (t nil))
         ))
;;for day trades
(defun bpl-signal1 (tdate size)
  (let (bpl targ )
   (multiple-value-setq (bpl targ) (channel-trend tdate size))
   (cond ((and (> (getd tdate 'close) targ)(> targ bpl)) 2)
         ((and (< (getd tdate 'close) targ)(< targ bpl)) -2)
         ((and (> (getd tdate 'close) bpl)(< targ bpl)) 1)
         ((and (< (getd tdate 'close) bpl)(> targ bpl)) -1)
         (t 0))
         ))

;;;ftp = first trade price

;;;PC-ADD-FACTOR aim says to add 50% of purchases to portfolio control
;;; 1.1010259 is safe
;;; abp is aim buy price
;;; abs is aim sell price
;;;pc is portfolio control

(defun aim-action (date pc num-shares cash &optional (pc-add-factor .50)(safe .10))
  (let* ((stock-price (getd date 'open)) advice
      	  sell-resist buy-resist market-order stock-value
	)
    (setq stock-value (* num-shares stock-price))    

    (setq sell-resist (round (* stock-value safe))
	  buy-resist (round (* stock-value safe)))

    (setq advice (- pc stock-value))
;;;market order is positive for a buy and negative for a sell order.
    
    (setq market-order
          (cond ((and (> advice (* .1 stock-value))
		       (> advice (+  buy-resist (* .1 stock-value)))) (- advice buy-resist))
		((and (< advice (* -.1 stock-value))
		      (< advice (+ (- sell-resist) (* -.1 stock-value))))(+ advice sell-resist))
		(t 0)
		))
    ;;; good number to use is minp = safe * pc.
   (if (>= market-order cash) (setq market-order cash))
   (if (plusp market-order)(setq pc (+ pc (* pc-add-factor market-order))))
   (setq num-shares (+ num-shares (/ market-order stock-price)))
   (setq cash (- cash market-order))
;   (when stream
;     (format stream "~%     BUY ~A SHARES AT A PRICE OF ~A "
;		    (ceiling abq) (my-round abp 2))
;     (format stream "~% OR SELL ~A SHARES AT A PRICE OF ~A "
;		    (ceiling asq) (my-round asp 2))
;     (format stream "~&     NEW PC = ~A IF BUY. "
;	    (round (+ pc (* 100 pc-add-factor (ceiling (/ abq 100))
;			    (my-round abp 2)))))
			    

    (values stock-price (round market-order)
	   (round pc) num-shares (round cash) (round (* num-shares stock-price)))
    ))

(defun aim-sim (start-date end-date Initial-capital &optional (pc-add-factor .6)(safe .10))
  (let (pc num-shares cash date stock-value stock-price buy&hold
	   market-order)
      (setq end-date (truncate end-date 100))
      (setq date (car (month-days (truncate start-date 100))))
      (setq stock-price (getd date 'open))
      (setq pc (round (* pc-add-factor initial-capital)))
      (setq cash (round (- initial-capital pc)))
      (setq num-shares (float (/ pc stock-price)))
        (if market-order (format T "~A ~A ~A ~A ~A ~A ~A~%" date stock-price market-order pc stock-value cash num-shares))
      (loop
       (if (= (truncate date 100) end-date) (return))
        (multiple-value-setq (stock-price market-order pc num-shares cash stock-value) 
          (aim-action date pc num-shares cash pc-add-factor safe))
        (if market-order (format T "~A ~A ~A ~A ~A ~A ~A~%" date stock-price market-order pc stock-value cash num-shares))
        (setq date (car (month-days (next-month (truncate date 100)))))
       )
      (setq buy&hold (* initial-capital
			(/ stock-price (getd (car (month-days(truncate  start-date 100))) 'open))))
      (values (round stock-value) (round cash) (round (+ stock-value cash)) (round buy&hold)) 

      ))
(defun elmoturn-dates (enddate num)
   (let ((date enddate) elmo-dates)
     (dotimes (ith num)
       (format T "~A~%" date)
   (if (member 0
          (get-elmo date 252)) (push date elmo-dates))
   (setq date (getd  date 'ydate)))
  elmo-dates
   ))

#|
(defun exitpoint-signal (tdate)
  (let* ((date-1 (getd tdate 'ydate))(date-2 (getd date-1 'ydate))
        (ti (timing-index tdate))(ti1 (timing-index date-1))(ti2 (timing-index date-2))
        (rsd (round (rsi-ave-diff tdate 14 2)))(rsd1 (round (rsi-ave-diff date-1 14 2)))
        (sst (round (slow-stochastic tdate 21)))(sst1 (round (slow-stochastic date-1 21)))
       ; (srr (or (standard-reward-risk1 tdate) 'FT))(srr1 (or (standard-reward-risk1 date-1) 'FT))
       ; (srr2 (or (standard-reward-risk1 date-2) 'FT))
        (ts5 (trend-signal tdate 5))
        (ts15 (trend-signal tdate 15))(ts45 (trend-signal tdate 45)))

   (cond ((and (>= rsd 0)
               (or (<= sst 20)(<= sst1 20))
               (or (>= ti 4)(>= ti1 4)(>= ti2 4))
               ;(neql srr 'OB)
               (member ts5 '(UP CU))
               (or (member ts15 '(UP CU))
                   (member ts45 '(UP CU)))
               )
                            'L1)
         ((and  (<= rsd 0)
                (or (>= sst 80)(>= sst1 80))
                (or (>= ti 4)(>= ti1 4)(>= ti2 4))
               ; (neql srr 'OS)
                (member ts5 '(DN CD))
                (or (member ts15 '(DN CD))
                    (member ts45 '(DN CD)))
                   )
                             'H1)
         ((and (>= rsd rsd1)
               (< sst 80)
               (member ts5 '(UP CU))
               (member ts15 '(UP CU))
               (member ts15 '(UP CU))
              ; (or (eql srr 'OS)
              ;     (eql srr1 'OS)
              ;     (eql srr2 'OS))
                 )
                            'L2)
         ((and (<= rsd rsd1)
               (> sst 20)
               (member ts5 '(DN CD))
               (member ts15 '(DN CD))
               (member ts45 '(DN CD))
              ; (or (eql srr 'OB)
              ;     (eql srr1 'OB)
              ;     (eql srr2 'OB))
                 )
                             'H2)
          ((and (>= rsd 0)
                (< sst 80)
                (member ts5 '(UP CU))
                (member ts15 '(UP CU))
                (member ts45 '(UP CU))
                ;(neql srr 'OB)
                ;(neql srr1 'OB)
                  )
                           'L3)
          ((and (<= rsd 0)
                (> sst 20)
                (member ts5 '(DN CD))
                (member ts15 '(DN CD))
                (member ts45 '(DN CD))
               ; (neql srr 'OS)
               ; (neql srr1 'OS)
                   )
                          'H3)
           ((and (>= rsd rsd1)
                 (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                 (member ts5 '(UP CU))
                ; (or (eql srr 'OS)
                ;     (eql srr1 'OS)
                ;     (eql srr2 'OS))
                     )
                             'L4)
           ((and (<= rsd rsd1)
                 (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                 (member ts5 '(DN CD))
                ; (or (eql srr 'OB)
                ;     (eql srr1 'OB)
                ;     (eql srr2 'OB))
                   )
                          'H4)
           ((and (>= rsd rsd1)
                 (<= sst 20)
                 (member ts5 '(UP CU))
                 (or (member ts15 '(UP CU))
                     (member ts45 '(UP CU)))
                ; (or (eql srr 'OS)
                ;     (eql srr1 'OS)
                ;     (eql srr2 'OS))
                     )
                           'L5)
           ((and (<= rsd rsd1)
                 (>= sst 80)
                 (member ts5 '(DN CD))
                 (or (member ts15 '(DN CD))
                     (member ts45 '(DN CD)))
                ; (or (eql srr 'OB)
                ;     (eql srr1 'OB)
                ;     (eql srr2 'OB))
                     )
                          'H5)





              (t nil))

  )) ;;;closes the let* and defun of expoint-signal

|#
(defun my-pretty-price (price)
 (block nil
  (ifn price (return))
  (if (member *data-name* '(US.D1B TY.D1B))
       (my-round (convert-to-decimal (convert-to-32 price)) 5)
     (my-round (* (index-tick-size)(round price (index-tick-size)))
      (index-digits))
     )))



;;;converts a file of day trades p&ls to a file of daily p&ls.
;;;path1 is a file of trades. path2 is a file of corresspnding dates
;;;;
(defun trades-to-daily-gains-losses (path2 path1)
  (let (trades dates dates-list outfile temp-list)
    (setq outfile (string-append *output-upper-dir* "daily-gains-losses.dat"))


   (with-open-file (str path1 :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))


   (with-open-file (str path2 :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push (conv-to-ewaves-date record) dates)))


   (setq temp-list (pairlis dates trades))

   (dolist (ith temp-list)
      (if (assoc (car ith) dates-list)
          (setf (cdr (assoc (car ith) dates-list)) (+ (cdr (assoc (car ith) dates-list))(cdr ith)))
          (pushnew ith dates-list)))

   (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith (reverse dates-list))
      (format str "~D~%" (cdr ith))))

   (summary-gains-losses outfile 'day (car dates-list) (car (last dates-list)))

 ))


;;;;num is the number of the field in the line
;;;0 id the first field value
(defun n-field-csv (str num)
   (let ((p1 -1))
    (dotimes (ith num)
      (setq p1 (position #\, str :start (+ 1 p1))))
    (subseq str (+ 1 p1) (position #\, str :start (+ 1 p1)))))



(defun merge-diary-files (&rest paths)
  (let (data pos gain-loss date-list date
       (path1 (string-append *output-upper-dir* "merged-diaries.csv")))

   (dolist (ith paths)
   (setq pos (position ith paths :test 'equal))

   (with-open-file (stream ith :direction :input )
        (do* ((record (read-line stream nil 'eof)(read-line stream nil 'eof)))
	     ((eql record 'eof))
            (setq date (read-from-string (n-field-csv record 0))
              gain-loss (read-from-string (n-field-csv record 3)))
            (unless (assoc date data)
              (setq date-list (make-list (+ 2 (length paths)) :initial-element 0))
              (setf (nth 0 date-list) date)
              (push date-list data))
            (when (assoc date data)
               (setf (nth (1+ pos)(assoc date data)) gain-loss)))
     );;;closes the with-open-file
     );;;closes the dolist

   (setq data (vsort data #'> 'car))
;;;;now add across total the gains and losses per day.
   (dolist (kth data)
      (setf (nth (+ (length paths) 1) kth)
           (apply #'+ (cdr kth))))



  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (ith data)
      (dolist (jth ith)
       (if (eql (car ith) jth)(format stream "~A" jth)
          (format stream "\,~D" jth)))
       (format stream "~%"))

   );;;closes the with-open-file

 ));;;closes the let and defun


;;;converts a swing trade  csv file from ExitPoints.
(defun exitpoints-diary-file (path)
  (let (trades dates (path1 (string-append *output-upper-dir* "exitpoints-diary.csv"))
        entry-date exit-date market-symbol direction entry-price exit-price market
        gain-loss date)

   (with-open-file (stream path :direction :input )
        (do* ((record (read-line stream nil 'eof)(read-line stream nil 'eof)))
	     ((eql record 'eof))
	     ;(print (read-from-string (read-from-string (n-field-csv record 0))))
	    (when (numberp (read-from-string (read-from-string (n-field-csv record 0))))
            (setq entry-date (conv-to-ewaves-date (read-from-string (n-field-csv record 1)))
                  exit-date (conv-to-ewaves-date (read-from-string (n-field-csv record 2)))
                  market-symbol (read-from-string (n-field-csv record 3))
                  direction (read-from-string (read-from-string (n-field-csv record 7)))
              entry-price (read-from-string (read-from-string (n-field-csv record 8)))
              exit-price (read-from-string (read-from-string (n-field-csv record 9))))
            (setq market
             (cdr (assoc (read-from-string (subseq market-symbol 0 2)) *market-symbol-conversion*)))
            (push (list entry-date exit-date market direction entry-price exit-price) trades))
           );;;closes the do
     );;;closes the with-open-file
    (set-cat-list)
   ; (setq trades1 trades)
    (dolist (ith trades)
       (set-market (nth 2 ith)) (print ith)
       (cond ((eql (car ith) (cadr ith));;;in and out on the same day
              (setq entry-price (nth 4 ith))
            ;  (if (getd (car ith) 'rollover)
            ;       (setq entry-price (+ (nth 4 ith) (getd (car ith) 'rollover))))
              (setq gain-loss
               (if (eql (nth 3 ith) 1) (* (- (nth 5 ith) entry-price)(index-point-value))
                   (* (- entry-price (nth 5 ith))(index-point-value))))
              (if (assoc (car ith) dates)
                  (setf (cdr (assoc (car ith) dates)) (+ (cdr (assoc (car ith) dates)) (round gain-loss)))
                 (setq dates (acons (car ith) (round gain-loss) dates)))

              )
             ((neql (car ith) (cadr ith))
              (setq date (car ith) entry-price (nth 4 ith))
             ; (if (getd (car ith) 'rollover)
             ;     (setq entry-price (+ (nth 4 ith) (getd (car ith) 'rollover))))
              (setq gain-loss (if (eql (nth 3 ith) 1)
                                 (* (- (getd (car ith) 'close) entry-price) (index-point-value))
                                (* (- entry-price (getd (car ith) 'close)) (index-point-value))))
               (if (assoc date dates)
                  (setf (cdr (assoc date dates)) (+ (cdr (assoc date dates)) (round gain-loss)))
              (setq dates (acons date (round gain-loss) dates)))

              (loop ;;;this is for the days within the trade
                (setq date (getd date 'ndate))
                (if (eql date (cadr ith)) (return))
               ; (if (getd date 'rollover)
               ;    (setq entry-price (+ (nth 4 ith) (getd date 'rollover))))
                (setq gain-loss
                 (if (eql (nth 3 ith) 1)
                     (* (- (getd date 'close)(getd (getd date 'ydate) 'close))(index-point-value))
                     (* (- (getd (getd date 'ydate) 'close) (getd date 'close)) (index-point-value))))
                  (if (assoc date dates)
                      (setf (cdr (assoc date dates)) (+ (cdr (assoc date dates)) (round gain-loss)))
                   (setq dates (acons date (round gain-loss) dates)))
                 );;;closes the loop
            ;  (if (getd (cadr ith) 'rollover)
            ;      (setq entry-price (+ (nth 4 ith) (getd (cadr ith) 'rollover))))
              (setq gain-loss
                 (if (eql (nth 3 ith) 1)
                     (* (- (nth 5 ith)(getd (getd date 'ydate) 'close))(index-point-value))
                     (* (- (getd (getd date 'ydate) 'close) (nth 5 ith)) (index-point-value))))
                (if (assoc (cadr ith) dates)
                    (setf (cdr (assoc (cadr ith) dates)) (+ (cdr (assoc (cadr ith) dates)) (round gain-loss)))
                  (setq dates (acons (cadr ith) (round gain-loss) dates)))
                 ; (if (< (cdr (assoc (cadr ith) dates)) -20000) (break "123"))
                         ));;;closes the cond

     );;;closes the dolist

     (setq dates (vsort dates #'> #'car))
  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (ith dates)
     (format stream "~A\,~D~%" (car ith) (round (cdr ith))))
   );;;closes the with-open-file
  ; (setq dates1 dates)

    ));;;closes the defun


 (defparameter *market-symbol-conversion*
  '((ey . dj.d1b)
    (es . sp.d1b)
    (en . nd.d1b)
    (er . ru.d1b)
    (nk . nk.d1b)
    (ty . ty.d1b)
    (us . us.d1b)
    (gc . gc.d1b)
    (si . si.d1b)
    (hg . cp.d1b)
    (pl . pl.d1b)
    (pa . pa.d1b)
    (ad . ad.d1b)
    (bp . bp.d1b)
    (cd . cd.d1b)
    (ec . e1.d1b)
    (jy . jy.d1b)
    (sf . sf.d1b)
    (mx . mx.d1b)
    (cl . cl.d1b)
    (ho . ho.d1b)
    (hu . hu.d1b)
    (qg . ng.d1b)
    (ng . ng.d1b)
    (co . cc.d1b)
    (kc . cf.d1b)
    (sb . su.d1b)
    (oj . oj.d1b)
    (ct . ct.d1b)
    (c- . c.d1b)
    (s- . s.d1b)
    (w- . w.d1b)
    (sm . sm.d1b)
    (bo . bo.d1b)
    (lc . lc.d1b)
    (lh . lh.d1b)
    ))


#|
;;;;assumes the path is the name of a file with a series of trade gains and losses (one per line)
;;;; the gains and losses are already in $.
;;;; deducts the 2/20 for maintenance and performance fees. Assumes 63 market days is a quarter
(defun summary-gains-losses1 (path &optional (date1 '?) (date2 '?)(limit 1000))
 (let (trades (outfile nil) (outfile1 nil) winners losers ave-win ave-loss temp
         zeros-list collection max-inactive-time inactive-time (day-ctr 0)
        (qty 1)(qty2 1) (qty3 1)(dollar-contract 0)(conser-dollar-contract 0)
        (running-sum-list '(0)) (running-sum2-list '(0)) (running-sum3-list '(0))
        (initial-capital 0) maintenance-fee maintenance-fee2 maintenance-fee3
        performance-fee performance-fee2 performance-fee3
        (running-sum 0)(running-sum2 0)(running-sum3 0))
   (setq outfile (string-append *output-upper-dir* "summary.dat") outfile1 (string-append *output-upper-dir* "optimal-f1.csv"))

   (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))
   (setq trades (reverse trades));;;orders so most recent is first
     (dolist (ith trades)
         (if (zerop ith) (push ith zeros-list))
         (when (and (not (zerop ith)) zeros-list)
               (push (length zeros-list) collection)(setq zeros-list nil)))
     (if zeros-list (push (length zeros-list) collection))

      (setq max-inactive-time (apply #'max collection) inactive-time (count-if #'zerop trades))

    (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
     (format str "Traded from ~A to ~A~%" date1 date2)
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  LARGEST GAIN= ~D PROFIT FACTOR= ~,2,0,'*,' F  ~
         ~%DRAWDOWN= ~D  $/contract= ~D LONGEST INACTIVE TIME= ~D   %TIME-IN-MARKET= ~F"
       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round  (min* losers))(round (max* winners))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (round (drawdown trades))
      (if (plusp (list-sum trades))(round (optimal-f trades)) 0)
      max-inactive-time (my-round (- 100 (* 100 (/ inactive-time (length trades)))) 1)


     ));close the format and with-open-file

     (with-open-file (stream outfile1 :direction :output :if-exists :supersede :if-does-not-exist :create)

     (dolist (ith (reverse trades))
        (push ith temp) (incf day-ctr)
        (setq qty (min qty limit) qty2 (min qty2 limit) qty3 (min qty3 limit))
        (setq running-sum (round (+ running-sum (* qty ith)))
              running-sum2 (round (+ running-sum2 (* qty2 ith)))
              running-sum3 (round (+ running-sum3 (* qty3 ith))))
        (when (zerop (mod day-ctr 63))
              (push running-sum running-sum-list)
              (push running-sum2 running-sum2-list)(push running-sum3 running-sum3-list)
              (setq initial-capital (* 2 (abs (drawdown temp))))
              (setq maintenance-fee (* .005 (+ running-sum initial-capital)))
              (setq performance-fee (* .20 (- running-sum (max* (cdr running-sum-list )))))
              (setq running-sum (round (- running-sum maintenance-fee (if (plusp performance-fee) performance-fee 0))))
             ; (format T "~%~D ~D ~D ~A~%" maintenance-fee performance-fee running-sum running-sum-list)
              (setq maintenance-fee2 (* .005 (+ running-sum2 initial-capital)))
              (setq performance-fee2 (* .20 (- running-sum2 (max* (cdr running-sum2-list )))))
              (setq running-sum2 (round (- running-sum2 maintenance-fee2 (if (plusp performance-fee2) performance-fee2 0))))

              (setq maintenance-fee3 (* .005 (+ running-sum3 initial-capital)))
              (setq performance-fee3 (* .20 (- running-sum3 (max* (cdr running-sum3-list )))))
              (setq running-sum3 (round (- running-sum3 maintenance-fee3 (if (plusp performance-fee3) performance-fee3 0))))
              );;;closes the when
        (format stream "~D\,~D\,~D\,~D\,~D\,~D\,~D\,~D\,~D~%" day-ctr ith dollar-contract qty running-sum qty2 running-sum2 qty3 running-sum3)

        (setq dollar-contract (if (> (length temp) 30)(optimal-f temp) 0)
              conser-dollar-contract (if (> (length temp) 30) (round (abs (drawdown temp))) 0)
              qty (if (and (plusp running-sum)(plusp dollar-contract))
                      (+ 1 (floor (/ running-sum dollar-contract))) 1)
              qty2 (if (and (plusp running-sum2)(plusp dollar-contract))
                      (+ 1 (floor (/ running-sum2 (* 2 dollar-contract)))) 1)
              qty3 (if (and (plusp running-sum3)(plusp conser-dollar-contract))
                      (+ 1 (floor (/ running-sum3 (* 2 conser-dollar-contract)))) 1))

        ));;closes the dolist and with-open-file
))
|#

(defun exitpoints-open-equity-report (tdate )
  (let ((path1 (string-append *output-upper-dir* "swing-trades.dat")) stop-long stop-short (equity 0)(total-equity 0))
    (vsort *open-swings* #'< #'fourth)
   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream "~%---------------------------------------------------------------------------------~%")
    (format stream "~%~%EXITPOINTS FUTURES OPEN EQUITY AS OF ~A~%~%" (date-convert tdate))
    (format stream "Month     Market        Direction Open Date  Entry Price Open Equity Stop Loss~%")
    (dolist (ith *open-swings*)
      (when (member (car ith) *swing-list*) ;;*ewaves-list*)
           (set-market (car ith))
           (multiple-value-setq (stop-long stop-short) (vprices tdate 4 1.66 3))
           (if (eql (nth 1 ith) 'long) (setq stop-long (max stop-long (nth 2 ith)))
           (setq stop-short (min stop-short (nth 2 ith))))
           (setq equity (round (* (if (eql (nth 1 ith) 'long) 1 -1)(- (getd tdate 'close) (nth 4 ith))(index-point-value))))
           (setq total-equity (+ equity total-equity))
          ; (format stream "~5A " (nth 2 (assoc (car ith) *c-list*)))
           (format stream "~5A " (contract-month *data-name* tdate))
           (if (eql (car ith) 'sp.d1b)
               (format stream "~20A " (string-append (subseq (index-lname) 0 2)
                  (subseq (index-lname) 6 (length (index-lname)))))
            (format stream "~20A " (index-lname)))
           (format stream "~6A " (nth 1 ith))
           (format stream "~13A " (date-convert (nth 3 ith)))
           (if (member (car ith) '(us.d1b ty.d1b))
               (format stream "~7A     " (convert-to-32nds (nth 4 ith)))
              (format stream "~7F     " (my-pretty-price (nth 4 ith))))
           (format stream "~6@D    " equity)
           (if (member (car ith) '(us.d1b ty.d1b))
           (format stream "~7A     ~%" (convert-to-32nds (if (eql (nth 1 ith) 'long) stop-long stop-short)))
           (format stream "~7F~%" (my-pretty-price (if (eql (nth 1 ith) 'long) stop-long stop-short))))
      ));closes the when and dolist

      (format stream "~%Total Open Equity = $~D" total-equity)
      (format stream "~%~%Always remember to adjust the stop loss~%and objective prices daily as necessary for your open positions.~%")

      )))
;;;the input file is the simulation
(defun percentile-report (path)
  (let (rocs (path1 (string-append *output-upper-dir* "percentiles.dat"))
        winners losers)
   (with-open-file (str path :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push (read-from-string (n-field-csv record 9)) rocs)))
   
   (setq rocs (mapcar #'(lambda (s) (- s  *day-commission*)) rocs))
   (setq winners (remove 0 (mapcar #'(lambda (s) (if (> s 0) s 0)) rocs)))
   (setq losers (remove 0 (mapcar #'(lambda (s) (if (<= s 0) s 0)) rocs)))
  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "per%  value ~%~%")
    (format stream "~D  ~D~%" 90 (percentile .90 rocs))
    (format stream "~D  ~D~%" 80 (percentile .80 rocs))
    (format stream "~D  ~D~%" 70 (percentile .70 rocs))
    (format stream "~D  ~D~%" 60 (percentile .60 rocs))
    (format stream "~D  ~D~%" 50 (percentile .50 rocs))
     (format stream "~D  ~D~%" 40 (percentile .40 rocs))
     (format stream "~D  ~D~%" 30 (percentile .30 rocs))
     (format stream "~D  ~D~%" 20 (percentile .30 rocs))
     (format stream "~D  ~D~%~%" 10 (percentile .10 rocs))

 (format stream "HIGHEST = ~D  LOWEST = ~D~%" (max* rocs) (min* rocs))
 (format stream "PROB OF GAIN = ~D~%" (round (* 100 (/ (count-if #'(lambda (s) (> s 0)) rocs) (length rocs)))))
 (format stream "AVERAGE = ~D~%" (round (/ (list-sum rocs)(length rocs))))
 (format stream "AVERAGE WINNING TRADE = ~D~%"
    (round (/ (list-sum winners) (length winners))))
 (format stream "AVERAGE LOSING TRADE = ~D~%"
    (round (/ (list-sum losers) (length losers))))
  (format stream "~%NUM TRADES LARGER THAN $2,000 = ~A~%" (count-if #'(lambda (s) (>= s 2000)) rocs))
  (format stream "NUM TRADES = ~A~%" (length rocs))
  (format stream "TOTAL P&L = ~A" (round (list-sum rocs)))
)))


(defun swing-open-equity-report (tdate)
  (let ((path1 (string-append *output-upper-dir* "swing-open-equity.txt")) stop-long stop-short (equity 0)(total-equity 0))
    (vsort *open-swings* #'< #'fourth)
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "SWING TRADE OPEN EQUITY AS OF ~A~%~%" (date-convert tdate))
    (format stream "Month     Market        Direction Open Date  Entry Price Open Equity Stop Loss~%")
    (dolist (ith *open-swings*)
      (set-market (car ith))
      (multiple-value-setq (stop-long stop-short) (vprices tdate 4 1.66 3))
      (if (eql (nth 1 ith) 'long) (setq stop-long (max stop-long (nth 2 ith)))
         (setq stop-short (min stop-short (nth 2 ith))))
      (setq equity (round (* (if (eql (nth 1 ith) 'long) 1 -1)(- (getd tdate 'close) (nth 4 ith))(index-point-value))))
      (setq total-equity (+ equity total-equity))
     ; (format stream "~5A " (nth 2 (assoc (car ith) *c-list*)))
      (format stream "~5A " (contract-month *data-name* tdate))
      (if (eql (car ith) 'sp.d1b)
          (format stream "~20A " (string-append (subseq (index-lname) 0 2)
                  (subseq (index-lname) 6 (length (index-lname)))))
       (format stream "~20A " (index-lname)))
      (format stream "~6A " (nth 1 ith))
      (format stream "~13A " (date-convert (nth 3 ith)))
      (if (member (car ith) '(us.d1b ty.d1b))
          (format stream "~7A     " (convert-to-32nds (nth 4 ith)))
        (format stream "~7F     " (my-pretty-price (nth 4 ith))))
      (format stream "~6@D    " equity)
      (if (member (car ith) '(us.d1b ty.d1b))
          (format stream "~7A     ~%" (convert-to-32nds (if (eql (nth 1 ith) 'long) stop-long stop-short)))
      (format stream "~7F~%" (my-pretty-price (if (eql (nth 1 ith) 'long) stop-long stop-short))))
      )

      (format stream "~%Total Open Equity = $~D" total-equity)

      )))


(defun position-open-equity-report (tdate)
  (let ((path1 (string-append *output-upper-dir* "position-open-equity.txt")) stop-long stop-short (equity 0)(total-equity 0))
   (vsort *open-positions* #'< #'fourth)
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "POSITION TRADE OPEN EQUITY AS OF ~A~%~%" (date-convert tdate))
    (format stream "Month     Market        Direction Open Date  Entry Price Open Equity Stop Loss~%")
    (dolist (ith *open-positions*)
      (set-market (car ith))
      (multiple-value-setq (stop-long stop-short) (vprices tdate 18 2.53 7))
      (if (eql (nth 1 ith) 'long) (setq stop-long (max stop-long (nth 2 ith)))
         (setq stop-short (min stop-short (nth 2 ith))))
      (setq equity (round (* (if (eql (nth 1 ith) 'long) 1 -1)(- (getd tdate 'close) (nth 4 ith))(index-point-value))))
      (setq total-equity (+ equity total-equity))
     ; (format stream "~5A " (nth 2 (assoc (car ith) *c-list*)))
      (format stream "~5A " (contract-month *data-name* tdate))
      (if (eql (car ith) 'sp.d1b)
          (format stream "~20A " (string-append (subseq (index-lname) 0 2)
                  (subseq (index-lname) 6 (length (index-lname)))))
        (format stream "~20A " (index-lname)))
      (format stream "~6A " (nth 1 ith))
      (format stream "~13A " (date-convert (nth 3 ith)))
      (if (member (car ith) '(us.d1b ty.d1b))
          (format stream "~7A     " (convert-to-32nds (nth 4 ith)))
        (format stream "~7F     " (my-pretty-price (nth 4 ith))))
      (format stream "~6@D    " equity)
      (if (member (car ith) '(us.d1b ty.d1b))
          (format stream "~7A     ~%" (convert-to-32nds (if (eql (nth 1 ith) 'long) stop-long stop-short)))
      (format stream "~7F~%" (my-pretty-price (if (eql (nth 1 ith) 'long) stop-long stop-short))))
      )

      (format stream "~%Total Open Equity = $~D" total-equity)

      )))


(defun find-first-day-volume (market)
  (let (date)
  (maind-x)(set-cat-list)(set-market market)

  (setq date (car (month-days (get-first-index-date))))
  (loop
   (if (numberp (getd date 'volume)) (return date) (Setq date (getd date 'ndate)))
   (ifn date (return nil)))

   ))

(defun find-first-day-openint (market)
  (let (date)
  (maind-x)(set-cat-list)(set-market market)

  (setq date (car (month-days (get-first-index-date))))
  (loop
   (if (numberp (getd date 'openint)) (return date) (Setq date (getd date 'ndate)))
   (ifn date (return nil)))

   ))


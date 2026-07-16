;;; -*- Mode: LISP; Package: USER; Base: 10. -*-
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
;;;this procedure will find the special primitives for fast analysis mode
;;; 
;;;first get 2 waves at degree 1 (maybe 3)
;;;then find filter that will give you *nsp* number of waves.
(defparameter *nsp* 34)
(defun find-special-primitives
       (start-time stop-time)
  (block FAT
    (let* (end-time
	    (startday (getnumdate start-time))
	    (starttime (getnumh start-time))
	    (startprice (getd startday 'pdata starttime))
	    (endday (if (stringp end-time)
			(getnumdate end-time)
		      (getnumdate stop-time)))
	    (endtime (if (stringp end-time)
			 (getnumh end-time)
		       (getnumh stop-time)))
	    (degree 1) (filt *n-filt*) priorday priortime priorprice
	    nextday nexttime nextprice  tdate time-list
	    num-waves degrs
	    trend-results last-index ignore)
    (declare (ignore ignore))
      (multiple-value-setq (tdate ignore ignore time-list)
	  (data-access endday))
    (if (stringp tdate)
	  (return-from FAT tdate))
      (ifn (member (getnumhour stop-time) time-list)
	   (return-from FAT "STOP TIME DOES NOT MATCH DATA TIMES"))
      (ifn end-time (setq end-time stop-time))
      (if (eql *time-interval* 1440)(setq endtime nil))
      (cd *data-read-dir*)
      (setf (get 'degrs 'index) *data-name*)
 ;;step1 find the time in hours between start-time and end-time  
      (multiple-value-setq (priorday priortime priorprice)
	(prior-data-point startday starttime startprice))
      (multiple-value-setq (nextday nexttime nextprice)
	(next-data-point startday starttime startprice))
      ;;;find index of first nil value
      (setq last-index
	    (if (get-TPV 0)
		(do ((ith 1 (1+ ith)))
		    ((or (stringp (and (vectorp (get-tpv ith))(svref (get-TPV ith) 1)))
			 (null (get-TPV ith)))
		     (and (vectorp (get-tpv (1- ith)))(svref (get-TPV (1- ith)) 0)))) 0))
      ;;;;fills or updates the tpv-vector as needed
      (tpv-vector-data startday starttime startprice endday endtime
		       priorday priortime priorprice
		       nextday nexttime nextprice last-index nil)

      ;;;initially set the filter size to half the time length.(approximate)
      (setq *n-filt* (truncate (1+ *end-index*) 2))
      
 ;;;for deg number 1
 ;; set the num-waves equal 2 (i.e. tenth arg to degree-finder)
      (multiple-value-setq
	  (ignore ignore ignore START-TIME
			    trend-results)
	(degree-finder 1 start-time 1 start-time degree nil
		       end-time stop-time *n-filt* 2))

      (and trend-results
	   (setq degrs (acons degree trend-results degrs)))
;      (format hww "    DEGREE= ")
 ;;;sets the number of waves 
      (setq num-waves  *nsp*)
;      (in-place degree nil hww)
      (incf degree)

      (multiple-value-setq
	  (ignore ignore ignore start-time
			    trend-results)
	(degree-finder 1 start-time
		       1 start-time degree nil
		       stop-time
		        stop-time *n-filt* num-waves))

      (setq degrs
	    (acons degree trend-results degrs))

 ;;;find index of stop-time
      (if (cadr (assoc 'index-list (cdar degrs)))
	  (do ((ith (cadr (assoc 'index-list (cdar degrs))) (1+ ith)))
	      ((and (= (svref (get-TPV ith) 1) endday)
		    (or (not endtime)
			(eql (svref (get-TPV ith) 2) endtime)))
	       (setq *end-index* ith))))

      (setq degrs (reverse degrs))
 ;returns globals to original value
      (setq *n-filt* filt)
      (cd *UPPER-DIR*)
;;;;returns the stop time and the filter size for this stage
      (values
	nil
	(cdr (assoc '*N-filt* (cdr (assoc '2 degrs))))
	(1- (length (cdr (assoc 'times-list (cdr (assoc '2 degrs)))))))
     )))

;;;
;;;;this procedure will find the primitives for graphics
;;;;given a start time and end time and a filter size it returns 
;;;a list of the highs and lows and a list of the corresponding times.

;;; NOTE: This program is the same as the one you sent me dated 2-05-90
;;; except as indicated.  It is the one that I am using.
;;;						Luis
;;;;note provide-primitive changes *latest-dtime*

(defun find-all-primitives
       (start-time stop-time)

  (unless (wave-p 0)			       ; \  NOTE: The bracketed part is
    (setq *curr-ET* start-time *maxq* -1) ;  | not part of Dave's version.
    (spring-time nil 0 nil nil t nil)	       ;  | wave-p is defined in the
    )  ; used  for working storage	       ; /  next version of w8y-acce.

  (block FAT
    (let* (end-time
	    (startday (getnumdate start-time))
	    (starttime (getnumh start-time))
	    (startprice (getd startday 'pdata starttime))
	    (endday (if (stringp end-time)
			(getnumdate end-time)
		      (getnumdate stop-time)))
	    (endtime (if (stringp end-time)
			 (getnumh end-time)
		       (getnumh stop-time)))
	    (ldt *latest-dtime*)
	    priorday priortime priorprice
	    nextday nexttime nextprice djia-list times-list last-index ignore
	    price-to-go time-to-go index-list)
      (declare (ignore ignore))
 
      (ifn end-time (setq end-time stop-time))
      (if (eql *time-interval* 1440)(setq endtime nil))
      (if (and (eql *time-interval* 'daily-high-low)
	       (numberp endtime))
	  (multiple-value-setq (endday endtime)
	    (nearest-daily-high-low-time endday endtime)))
		   
     ; (cd *data-read-dir*)
      
 ;;step1 find the time in hours between start-time and end-time  
      (multiple-value-setq (priorday priortime priorprice)
	(prior-data-point startday starttime startprice))
      (multiple-value-setq (nextday nexttime nextprice)
	(next-data-point startday starttime startprice))
      ;;;find index of first nil value
      (setq last-index
	    (if (get-TPV 0)
		(do ((ith 1 (1+ ith)))
		    ((or (stringp (and (vectorp (get-tpv ith))(svref (get-TPV ith) 1)))
			 (null (get-TPV ith)))
		     (and (vectorp (get-tpv (1- ith)))(svref (get-TPV (1- ith)) 0)))) 0))
      ;;;;fills or updates the tpv-vector as needed
      (tpv-vector-data startday starttime startprice endday endtime
		       priorday priortime priorprice
		       nextday nexttime nextprice last-index nil)

      (multiple-value-setq
	  (djia-list times-list index-list ignore ignore ignore ignore
		     ignore ignore ignore ignore ignore price-to-go time-to-go)
	(find-prims 1 start-time  end-time stop-time))
 ;returns globals to original value
      (setq *latest-dtime* ldt)
     ; (cd *UPPER-DIR*)
      (values djia-list times-list price-to-go time-to-go index-list
	      last-index))))

(defun median-prim-lgth (start-time stop-time &optional (factor .618))
  (let (lgths prices)
  (setq prices (find-all-primitives start-time stop-time))
  (do ((pr1 (car prices) (car prs))
       (pr2 (second prices) (second prs))
       (prs (cdr prices) (cdr prs)))
       ((not pr2) lgths)
       (push (abs (- pr2 pr1)) lgths))
  (setq lgths (vsort lgths #'<))
  (values (*  factor (nth (truncate (length lgths) 2) lgths))
	  (length lgths))
  ))


;;;;the goal of this procedure is to find 3 valid starting points
;;;for an E-WAVES analysis ranked in order to try.
(defparameter *auto-window-size* 13)

#|
(defun auto-start-times (lastest-time &optional
				      (window-size *auto-window-size*)
				      (min-filt 4))

  (block auto
  (let* ((lastest-day (getnumdate lastest-time))
	 (lastest-day-8
	   (checkoutdate
	     (add-days-to-date2 lastest-day
			       (- (round (* 365.25 window-size))))))
	 lastest-time-8  (nsp *nsp*)(filt *n-filt*) ignore
	 pair-list low-list high-list price-list times-list
	 (num-start-times 5) ndate ydate  second-cutoff)
      (declare (ignore ignore))


    (unless lastest-day-8
      (setq lastest-day-8
	    (car (month-days (get-first-index-date)))))
    (when lastest-day-8
      (multiple-value-setq (ignore ydate ndate)(data-access lastest-day-8))
      (if (or (not ydate)(stringp (data-access ydate)))
	  (setq lastest-day-8 ndate)))
    
    (unless (wave-p 0)			       
      (setq acc::*maxq* -1) 
      (spring-time nil 0 nil nil t nil)	       
      )  ; used  for working storage	       

    (multiple-value-setq (ignore ignore ignore times-list)
				(data-access lastest-day-8))
    (ifn times-list (return-from auto "NOT ENOUGH DATA"))
    (setq lastest-time-8
	  (conv-to-string lastest-day-8 (car times-list)))
    (setq *nsp* num-start-times)(nil-tpv)

    (loop

  ;  (print (list 'nsp *nsp*))
  ;  (print lastest-time-8)
  ;  (print lastest-time)
      (multiple-value-setq (ignore *n-filt*)
	(find-special-primitives lastest-time-8 lastest-time))

      (multiple-value-setq (price-list times-list)
	(find-all-primitives lastest-time-8 lastest-time))


      (setq price-list (butlast price-list) times-list (butlast times-list))
;;;remove extremes that are less than 1 filter size from either end of the data
;;;

    (setq   second-cutoff
	    (conv-to-string (svref (get-tpv (- *end-index* *n-filt*)) 1)
			    (svref (get-tpv (- *end-index* *n-filt*)) 2)))
    (dotimes (kth (length times-list))
      (if (or (etime> (nth kth times-list) second-cutoff))
	  (setf (nth kth times-list) nil
		(nth kth price-list) nil)))
    (setq times-list (remove nil times-list)
	  price-list (remove nil price-list))      
 ;<   (format t "~%index 0 =~A" (get-tpv 0))
    (setq pair-list (pairlis times-list price-list)
	    high-list nil low-list nil)
    #+:ALLEGRO (setq pair-list (nreverse pair-list))
    (when (second price-list)
      (if (< (first price-list)(second price-list))
	  (dotimes (ith (length times-list))
	    (if (evenp ith)(push (nth ith times-list) low-list)
	      (push (nth ith times-list) high-list)))
	(dotimes (ith (length times-list))
	  (if (oddp ith)(push (nth ith times-list) low-list)
	    (push (nth ith times-list) high-list)))))
      
;    (format t "~%high-list=~A ~%low-list=~A" high-list low-list)      
;;;if there's a time on the low-list that is later in time and lower in price
;;;remove from the times-list
      (setq times-list
	    (remove-if
	      #'(lambda (s) 
		  (and (member s low-list :test #'equal)
		       (some
			 #'(lambda (kth)
			     (or (and (f-etime< s kth)
				      (< (cdr (assoc kth pair-list
						     :test #'equal))
					 (cdr (assoc s pair-list
						     :test #'equal))))
				 (< (getv 0 lp)
				    (cdr (assoc s pair-list
						:test #'equal)))))
			 low-list))) times-list))
      
      (setq times-list
	    (remove-if
	      #'(lambda (s) 
		  (and (member s high-list :test #'equal)
		       (some
			 #'(lambda (kth)
			     (or (and (f-etime< s kth)
				      (> (cdr (assoc kth pair-list
						     :test #'equal))
					 (cdr (assoc s pair-list
						     :test #'equal))))
				 (> (getv 0 hp)
				    (cdr (assoc s pair-list
						:test #'equal)))))
			 high-list))) times-list))
      
      (setq low-list
	    (remove-if #'(lambda (s)
			   (not (member s times-list :test #'equal)))
		       low-list))
      (setq high-list
	    (remove-if #'(lambda (s)
			   (not (member s times-list :test #'equal)))
		       high-list))
      (if (>= (+ (length low-list)(length high-list)) num-start-times)
	  (setq times-list (screen-extremes low-list high-list)))
      (if (and (< (length times-list) num-start-times)
	       (> *n-filt* min-filt))
	  (setq *nsp* (ceiling (* 1.25 *nsp*)))

	  (return))

      ) ; loop

     (setq times-list
	   (remove-if #'(lambda (s)
			  (not (auto-start-time-valid-p s lastest-time)))
		      times-list))
;;clean up the lisp world
   (setq *n-filt* filt *nsp* nsp) (nil-tpv)
      times-list)))
|#

;;;;this function assumes that the tpv vector is full.
(defun screen-extremes (low-list high-list)
  (let (low-sizes high-sizes low-index-list high-index-list new-pair-list
        left-low-price left-high-price pair-list high-price low-price
	kth-day kth-hour last-retrace)
;;;low-list and high-list are in order earliest to latest
;;;upon entering this function    
;;;need to find indicies of times-list
    (if low-list
  (setq low-index-list
	(dolist (kth low-list (reverse low-index-list))
	  (setq kth-day (getnumdate kth) kth-hour (getnumhour kth))
	  (dotimes (ith *end-index*)
	    (when (and (eql kth-day (svref (get-tpv ith) 1))
		       (eql kth-hour (svref (get-tpv ith) 2)))
	      (push ith low-index-list)(return))))))
    (if  high-list
  (setq high-index-list
	(dolist (kth high-list (reverse high-index-list))
	  (setq kth-day (getnumdate kth) kth-hour (getnumhour kth))
	  (dotimes (ith *end-index*)
	    (when (and (eql kth-day (svref (get-tpv ith) 1))
		       (eql kth-hour (svref (get-tpv ith) 2)))
	      (push ith high-index-list)(return))))))
;;;need highest price to the left of earliest low
;;;then compute size
    (if low-list
	(dotimes (jth (length low-index-list))
	  (setq high-price (svref (get-tpv (if (= jth 0) 1
					     (nth  (1- jth) low-index-list))) 3))
	  (dotimes (kth (- (nth jth low-index-list)
			   (if (= jth 0) 0 (nth (1- jth) low-index-list))))
	    (if (> (svref (get-tpv (+ kth
				      (if (= jth 0) 1
					(nth (1- jth)
					     low-index-list)))) 3) high-price)
		(setq high-price
		      (svref (get-tpv (+ kth
					 (if (= jth 0) 1
					   (nth (1- jth) low-index-list)))) 3))))
	  (setq left-high-price high-price)
	  (push (/  left-high-price (svref (get-tpv (nth jth low-index-list)) 3))
		low-sizes)))
    (setq low-sizes (reverse low-sizes))
;;;compute size of move from start of data to first high
    (if  high-list
	 (dotimes (jth (length high-index-list))
	   (setq low-price (svref (get-tpv (if (= jth 0) 1
					     (nth (1- jth) high-index-list))) 3))
	   (dotimes (kth (- (nth jth high-index-list)
			    (if (= jth 0) 0 (nth (1- jth) high-index-list))))
	     (if (< (svref (get-tpv (+ kth
				       (if (= jth 0) 1
					 (nth (1- jth) high-index-list)))) 3)
		    low-price)
		 (setq low-price
		       (svref (get-tpv (+ kth (if (= jth 0) 1
						(nth (1- jth)
						     high-index-list))))  3))))
	   (setq left-low-price low-price)
;	   (format t "left-low-price=~A~%" left-low-price)
;	   (format t "high-price=~A~%"
;		   (nth 3 (get-tpv (nth jth high-index-list))))
	   (push (/ (svref (get-tpv (nth jth high-index-list)) 3) left-low-price)
		 high-sizes)))
  (setq high-sizes (reverse high-sizes))
;;;must find the largest drop off the extreme after the last low on
;;;index-list
   (when low-list
    (setq low-price (svref (get-tpv (car (last low-index-list))) 3)
	  high-price low-price)
    (setq last-retrace 1)
    (dotimes (kth (- *end-index* (car (last low-index-list)) -1))
      (if (> (svref (get-tpv (+ kth (car (last low-index-list)))) 3)
	     high-price)
          (setq high-price
		(svref (get-tpv (+ kth (car (last low-index-list)))) 3)
		low-price high-price)
	(if (< (svref (get-tpv (+ kth (car (last low-index-list)))) 3)
	       low-price)
	    (setq low-price
		  (svref (get-tpv (+ kth (car (last low-index-list)))) 3)
                  last-retrace (max last-retrace (/ high-price low-price))))
        ))
;;;order of pair list is earliest to latest    
    (setq pair-list (pairlis low-list low-sizes))
    #+:ALLEGRO (setq pair-list (nreverse pair-list))    
    (setq pair-list
	  (remove-if #'(lambda (s) (> last-retrace (cdr s))) pair-list)))

;;;assumes the pair-list is in chronological order
    (if (> (length pair-list) 1)
    (do* ((plist pair-list (cdr plist))
	  (kth (car plist) (car plist)))
	 ((not kth))
       (when (>= (cdr kth)(max* (mapcar #'cdr plist)))
	 (push kth new-pair-list)))
      (if pair-list (push (car pair-list) new-pair-list)))
;;;;
    (setq pair-list nil)
    (when high-list
      (setq high-price (svref (get-tpv (car (last high-index-list))) 3)
	    low-price high-price)
      (setq last-retrace 1)
      (dotimes (kth (- *end-index* (car (last high-index-list)) -1))
	(if (< (svref (get-tpv (+ kth (car (last high-index-list)))) 3)
	       low-price)
	    (setq low-price
		  (svref (get-tpv (+ kth (car (last high-index-list)))) 3)
		  high-price low-price)
	  (if (> (svref (get-tpv (+ kth (car (last high-index-list)))) 3)
		 high-price)
	      (setq high-price
		    (svref (get-tpv (+ kth (car (last high-index-list)))) 3)
             last-retrace (max last-retrace (/ high-price low-price))  ))))
      (setq last-retrace (/ high-price low-price))
;      (break "high-price=~A low-price=~A last-retrace=~A~%" high-price
 ;      low-price last-retrace)
      ;;;order of pair list is earliest to latest    
      (setq pair-list (pairlis high-list high-sizes))
      #+:ALLEGRO (setq pair-list (nreverse pair-list))
      (setq pair-list
	    (remove-if #'(lambda (s) (> last-retrace (cdr s))) pair-list)))

;;;assumes the pair-list is in chronological order
    (if (> (length pair-list) 1)
	(do* ((plist  pair-list (cdr plist))
	      (kth (car plist) (car plist)))
	     ((not kth))
	  (when (>= (cdr kth)(max* (mapcar #'cdr plist)))
	    (push kth new-pair-list)))
      (if pair-list (push (car pair-list) new-pair-list)))
;;;new pair list should have both highs and lows      
  (setq new-pair-list (vsort new-pair-list #'etime< #'car))
 (mapcar #'car new-pair-list)))


;;;;checks to see if the auto-start-time is still valid
;;;this is used at the time curr-time
;;;;returns nil if auto-start-time is not ok
;;;T means the auto-start-time is still valid nil means a new one is needed
(defun auto-start-time-valid-p (auto-start-time curr-time)
  (block auto
    (let* ((auto-start-date (getnumdate auto-start-time))
	   (auto-start-hour (getnumhour auto-start-time))
	   (curr-date (getnumdate curr-time))(result t) prices
	   start-price start-extreme ignore (window-size *auto-window-size*)
	   (data-window-start	     
	     (checkoutdate
	        (add-days-to-date2 curr-date
				   (- (round (* 365.25 window-size))))))
	   tdate ttime tprice low-price high-price priorday priortime
	   priorprice nextday nexttime nextprice
	   auto-start-size last-retrace
	   yprice nprice ydate ndate)
      (declare (ignore ignore))

      (unless auto-start-date (return-from auto nil))
      (unless data-window-start
	(setq data-window-start
	    (car (month-days (get-first-index-date)))))
      
      (when data-window-start
	(setq ydate (getd data-window-start 'ydate)
	      ndate (getd data-window-start 'ndate))
	(if (or (not ydate)(stringp (data-access ydate)))
	    (setq data-window-start ndate)))
;;;first the time length test i.e. 5 years usually
      (if (edate< auto-start-date data-window-start)
	  (return-from auto nil))
;;;;if auto-start-time is a high is the curr-price higher?
;;;if auto-start-time is a low is the curr-price lower?
      (setq start-price (getd auto-start-date 'pdata auto-start-hour))
      (multiple-value-setq (ignore ignore yprice)
	(prior-data-point  auto-start-date auto-start-hour nil))
      (multiple-value-setq (ignore ignore nprice)
	(next-data-point auto-start-date auto-start-hour nil))
      (setq start-extreme
	    (if (or (> start-price yprice)
		    (> start-price nprice)) 'high 'low))
      (multiple-value-setq (ignore ignore ignore ignore prices)
	(data-access curr-date))
      (setq result
	    (case start-extreme
	      (high (< (max* prices) start-price))
	      (low  (> (min* prices) start-price))))
      (ifn result (return-from auto nil))
;;;need to check if a larger retracement has occurred than the
;;;auto-start-time extreme
     (when (eql start-extreme 'low)
      (setq low-price start-price high-price start-price
	    tdate auto-start-date ttime auto-start-hour tprice start-price)
      (setq auto-start-size
	    (loop
	      (multiple-value-setq (priorday priortime priorprice)
		(prior-data-point tdate ttime tprice))
	      (if (or (stringp priorday)
		      (etime< priorday data-window-start))
		  (return (/ high-price low-price)))
	      (if (> priorprice high-price) (setq high-price priorprice)
		(if (< priorprice low-price)(return (/ high-price low-price))))
	      (setq tdate priorday ttime priortime tprice priorprice)
	      ))
;;;;size retracement after the low to curr-date      
      (setq low-price start-price high-price start-price tprice start-price
	    tdate auto-start-date ttime auto-start-hour tprice start-price)
      (setq last-retrace 1)
      (loop 
	(multiple-value-setq (nextday nexttime nextprice)
	  (next-data-point tdate ttime tprice))
	(if (or (stringp nextday)
		(etime> nextday curr-date))
	    (return))
	(if (> nextprice high-price)
	    (setq high-price nextprice low-price high-price)
	  (if (< nextprice low-price)
	      (setq low-price nextprice 
		    last-retrace (max last-retrace (/ high-price low-price)))))
	(setq tdate nextday ttime nexttime tprice nextprice))
      (if (or (> last-retrace auto-start-size)
	      (< low-price start-price))
	  (return-from auto nil)))
;;;;;
     (when (eql start-extreme 'high)
      (setq low-price start-price high-price start-price
	    tdate auto-start-date ttime auto-start-hour tprice start-price)
      (setq auto-start-size
	    (loop
	      (multiple-value-setq (priorday priortime priorprice)
		(prior-data-point tdate ttime tprice))
	      (if (or (stringp priorday)
		      (etime< priorday data-window-start))
		  (return (/ high-price low-price)))
	      (if (< priorprice low-price) (setq low-price priorprice)
		(if (> priorprice high-price)
		    (return (/ high-price low-price))))
	      (setq tdate priorday ttime priortime tprice priorprice)
	      ))
;;;;size retracement after the high to curr-date      
      (setq low-price start-price high-price start-price tprice start-price
	    tdate auto-start-date ttime auto-start-hour tprice start-price)
      (setq last-retrace 1)
      (loop 
	(multiple-value-setq (nextday nexttime nextprice)
	  (next-data-point tdate ttime tprice))
	(if (or (stringp nextday)
		(etime> nextday curr-date))
	    (return))
	(if (< nextprice low-price)
	    (setq low-price nextprice high-price nextprice)
	  (if (> nextprice high-price)
	      (setq high-price nextprice
		    last-retrace (max last-retrace (/ high-price low-price)))))
;	(format t
;		"last-retrace=~A  high-price=~A low-price=~A start-price=~A~%"
;		last-retrace high-price low-price start-price)
	(setq tdate nextday ttime nexttime tprice nextprice))
	   
      (if (or (> last-retrace auto-start-size)
	      (> high-price start-price))
	  (return-from auto nil)))
     
      result)))

(defun nearest-daily-high-low-time (date tim)
  (let (cand-time ydate times-list ignore)
    (declare (ignore ignore))
    (multiple-value-setq (ignore ydate ignore times-list)
      (data-access date))
    (setq cand-time (time-units-into-day date tim))
    (cond ((>= cand-time
	       (time-units-into-day date (second times-list)))
	   (setq tim (second times-list)))
	  ((>= cand-time
	       (time-units-into-day  date (first times-list)))
	   (setq tim (first times-list)))
	  (t (multiple-value-setq (date ignore ignore times-list)
	       (data-access ydate))
	     (setq tim (second times-list))))
      (values date tim)))

(defun wave-p (wv-no)
  (and (vectorp (multiple-value-bind (wsub-indx swave)
		    (truncate wv-no *w-subdim*)
		  (aref (aref *w* wsub-indx) swave)))
       (not (member wv-no *waves-stack*))))
;;;
(defconstant *ws-l* (byte 7 0))
(defconstant *ws-u* (byte 10 7))


(defun wave-st (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				  (ldb *ws-l* wave)) 1)) 0)))
				  
(defun wave-et (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7))
				  (ldb *ws-l* wave)) 1)) 1)))
				  
				
				  
(defun wave-tl (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				  (ldb *ws-l* wave)) 1)) 2)))
(defun wave-sp (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
	(aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				    (ldb *ws-l* wave)) 1)) 3)))
(defun wave-ep (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				  (ldb *ws-l* wave)) 1)) 4)))
(defun wave-hp (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				  (ldb *ws-l* wave)) 1)) 5)))
(defun wave-lp (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				  (ldb *ws-l* wave)) 1)) 6)))
(defun wave-dr (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w* *q*))
  (if wave
      (aref (aref *q* (aref (aref (aref *w* (ash wave -7)) 
				  (ldb *ws-l* wave)) 1)) 7)))				  

;;;WAVE & Q ARRAY GENERATION AND MANAGEMENT
;;;
;;;CONTROLS THE CREATION OF A NEW WAVE: wave. GENERATES A W VECTOR IF NECESS.
;;;GENS A Q-ARRAY IF NEC. (NEWLY SPROUTED WAVES DO NOT ACQUIRE A Q-ARRAY YET).
;;;IF oldw ->non-nil IT SUBSTS A WPAIR (OF wave) FOR THE WPAIR OF oldw.
;;;IF spl-p ->non-nil IT SPLICES WAVE AFTER spl-p. ELSE IT ADDS WPAIR AT HEAD.
(defun spring-time (ct-symb wave property-list &optional (indep-P t)
			    (indep-Q t) (oldw-Q nil) (oldw nil) (spl-p nil))
  (multiple-value-bind (wsub-indx swave wvvctr) (truncate wave *w-subdim*)
    (setq wvvctr (aref (aref *w* wsub-indx) swave))
    (if (vectorp wvvctr)
	(copy-array-contents *iniwv* wvvctr)
	(setf wvvctr (make-array *wdm* :initial-contents *iniwl*)))
    (setf (aref (aref *w* wsub-indx) swave) wvvctr))
  (if indep-Q (arrange-for-a-q-array wave nil nil property-list)
    (if oldw-Q (point-q-array wave oldw-Q))); <--it may or mayn't point to one.
  (if indep-P (foliate-branch ct-symb wave oldw spl-p))
  (if property-list (addv wave property-list ct-symb)))

;;;BOUNCES WAVE'S Q-ARRAY AGAINST THE *Q-ALIST* AND IF THERE IT REPOINTS WAVE'S
;;;Q-ARRAY TO THE EXISTING ONE, ELSE IT MAKES A NEW Q, IE: A COPY OF WAVE'S Q
(defun arrange-for-a-q-array (wave start endt &optional (prop-lst nil))
  (if prop-lst (multiple-value-setq (start endt) (get-inp-tprop prop-lst)))
  (ifn start (define-new-q-array wave)
    (multiple-value-bind (q-array-exists q-pointer)
	(q-array-exists-p start endt)
      (cond ((and q-array-exists		; Note: This case appears to be
	          (get-q-pointer wave)		; there is nothing to do, this
		  (eql (getv wave 'ET) endt)))	; wave has q-array and its al-
						; ready pointing to it
	    (q-array-exists
	      (change-q-pointer1 wave q-pointer) nil)
	    ((not (get-q-pointer wave))		; New wave that doesnt have a
	     (define-new-q-array wave)		; q pointer yet?
	     (push (cons (list start endt) (get-q-pointer wave)) *q-alist*))
	    (t (make-indep-q wave start endt) t)))))



;;;DEFINES Q ARRAY THE OLD FASHIONED WAY
(defun define-new-q-array (wave)
  (let ((q1 (getqno)))
    (multiple-value-bind (wsub-indx swave) (truncate wave *w-subdim*)
      (if (vectorp (aref *q* q1))
	  (copy-array-contents *iniqv* (aref *q* q1))
	(progn
	  (setf (aref *q* q1)
	       ;(make-array *qdm* :initial-contents *iniql* :leader-length 2)))
		(make-array *qdm* :initial-contents *iniql*))))
      (setf (aref (aref (aref *w* wsub-indx) swave) 1) q1)) q1))

(defun getqno ()
  (cond ((null *q-stack*) (setq *maxq* (1+ *maxq*)))
	(t (pop *q-stack*))))


(defun putv (wave key value &optional (ct-symb nil))
  (setq key (eval key))
  (and wave (ifn ct-symb (funcall (aref *putvaluef* key) wave key value)
	      (funcall (aref *putvaluef* key) ct-symb wave key value))))

(defun pt1 (wave key value)
  (multiple-value-bind (wsub-indx swave) (truncate wave *w-subdim*)
    (setf (aref (aref (aref *w* wsub-indx) swave) key) value)))
	   
(defun pt3 (wave key value)
  (multiple-value-bind (wsub-indx swave) (truncate wave *w-subdim*)
    (setf (aref (aref *q* (aref (aref (aref *w* wsub-indx) swave) 1))
		(- key 11)) value)))
   
(defun wave-dg (wave)
  (declare (fixnum wave))
  (declare (type simple-vector *w*))
  (if wave
      (aref (aref (aref *w* (ash wave -7)) (ldb *ws-l* wave)) 5)))	      	
					

;;; -*- Mode: LISP; Package: USER; Base: 10. -*-
#+:SBCL (in-package :common-lisp-user)

;;;These functions are for research and development

;;;;shows one month of swing trades unfiltered

(defun show-swings (market tdate)
  (let (mtrades dates)
   (set-market market)
     (setq  dates (month-days (truncate tdate 100)))
   
   (apply #'swing-trade-binsb *swing-features*);;;fills swings
  (dolist (kth dates)
    (push (find-if #'(lambda(s1) (and (eql (svref s1  0) market)(eql (svref s1 1) kth))) swings)
       mtrades))
   (setq mtrades (remove nil mtrades))
   (ifn mtrades (format T "~%No Trades" ))
   (dolist (kth mtrades)
     (format T "~A~%" kth))(terpri)
))
;;;computes the distance of the largest move from the previous day's close
;;;
(defun voltest1 (tdate days markets)
  (let (date date-1 rocs vol63 (path1 "~/exitpoints/percentile.txt"))
(dolist (ith markets)

 (set-market ith)
 (setq date (add-mkt-days tdate (- days)))
  (dotimes (ith days)
  (setq date (getd date 'ndate) date-1 (getd date 'ydate))
   (setq vol63 (volatility date-1 63 2))
    (cond ((> (- (getd date 'high)(getd date-1 'close)) vol63)
              (push (- (+ (getd date-1 'close) vol63) (getd date 'close)) rocs))
          ((> (- (getd date-1 'close)(getd date 'low)) vol63)
              (push (- (getd date 'close)(- (getd date-1 'close) vol63)) rocs))
          (t nil))
  ;  (format T "~% vol63= ~A high= ~A low= ~A CLOSE= ~A  ROC= ~A" vol63 (getd date 'high)(getd date 'low)(getd date 'close) (car rocs))  
   ))
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
   (format stream "MEAN = ~D~%" (round (/ (list-sum rocs)(length rocs))))
   (format stream "NUM TRADES = ~A  NUM DAYS = ~A RATIO = ~D% ~%" (length rocs) days (Float (* 100 (/ (length rocs) days))))

)))

;;;computes the distance of the largest move from the previous day's close
;;;
(defun voltest (tdate days markets)
  (let (date rocs (path1 "~/exitpoints/percentile.txt"))
(dolist (ith markets)

 (set-market ith)
 (setq date (add-mkt-days tdate (- days)))
  (dotimes (ith days)
  (setq date (getd date 'ndate))
   (push 
    (/ (max (- (getd date 'high)(getd date 'open))
            (- (getd date 'open)(getd date 'low)))
       (volatility date 63 1))
       rocs))
   )
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
)))



(defun extract-unfiltered-trades (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "unfilteredtrades.csv"))
        new-trades-path trades)
         

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "swingtrades.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~A,~A,~A~%" (svref ith 1)(svref ith 17)(svref ith 19)))
       ) ;;;closes the with-open-file
));;closes the let and defun

(defun is3 (&optional (typ 'all))
  (indicator-study3 (set-difference *day-warehouse-list* *currencies-list*) typ)
)

(defun indicator+market+study (markets)
  (let (bf (path1 "~/exitpoints/ims.lisp"))
  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
  (dolist (ith markets)
    (build-daytrade-warehouse4 (list ith))
    (setq bf (find-best-indicator-set4a))
    (format str "~A  ~A~%" ith bf)
  ;  (daytrade-simulation-test4 ith 20190215 5000 bf)
    )
)
))

(defun indicator-value-list (market tdate days)
  (let (codes (date tdate))
     (set-market market)
     (dotimes (ith days)
       (if  (/= (ilan-exit1 date) 0)
           (setq codes (cons (list date (ilan-exit1 date)) codes)))
       (setq date (getd date 'ydate)))

    (format T "~A" codes)

))

;;;tests if ndate of data matches ydate of tomorrow's data
;;;tdate is intentionally global
(defun test-data (&optional startday stopdate)
  (let (ndt ydt tdate times prices)
   (ifn stopdate (setq stopdate (car (month-days (get-latest-index-date)))))
   (ifn startday (setq startday (car (month-days (get-first-index-date)))))
    (setq tdate startday)  
    (loop
     (when (<= tdate stopdate)
       (if (equal (day-of-week (format nil "~A" tdate)) "SUNDAY") 
	   (return (format nil "ERROR ~A is a SUNDAY" tdate)))
       (if (equal (day-of-week (format nil "~A" tdate)) "SATURDAY") 
	   (return (format nil "ERROR ~A is a SATURDAY" tdate)))
       (setq ndt (getd tdate 'ndate))
       (ifn (or ndt (= tdate stopdate)) (return (format nil "ERROR NO NEXT DATE ~A" tdate)))
       (if (stringp ndt) (return (format nil "ERROR BAD NEXT DAY ~A" ydt)))
       (setq ydt (getd ndt 'ydate))
       (ifn (or (eql tdate ydt)(= tdate stopdate))
	    (return (format nil "ERROR MISMATCHED DATES ~A ~A" tdate ndt)))
       (setq times (getd tdate 'ptime) prices (getd tdate 'pdata))
       (setq prices (remove nil prices))
       (when (member 0 prices) (return (format nil "ERROR ZERO PRICE ON ~A" tdate)))
       (when (eql (getd tdate 'high) 0)(return (format nil "ERROR ZERO HIGH ON ~A" tdate)))
       (when (eql (getd tdate 'low) 0)(return (format nil "ERROR ZERO LOW ON ~A" tdate)))
       (when (eql tdate (getd tdate 'ydate))(return
(format nil "ERROR YDATE BAD ON ~A" tdate)))
       (ifn prices
	    (return (format nil "ERROR NO DATA ON ~A" tdate)))
       (if (neql (length times)(length prices))
	   (return (format nil "ERROR MISSING DATA ~A" tdate)))
     
       (when (assoc 'high (cdr (readt2 tdate)))
	 (ifn (getd tdate 'high) (return (format nil "ERROR MISSING HIGH PRICE ON ~A" tdate)))  
	 (ifn (getd tdate 'low) (return (format nil "ERROR MISSING LOW PRICE ON ~A" tdate)))  
	 (ifn (getd tdate 'open) (return (format nil "ERROR MISSING OPEN PRICE ON ~A" tdate)))
	 (if (> (getd tdate 'open)(getd tdate 'high))(return (format nil "ERROR OPEN PRICE ABOVE HIGH PRICE on ~A" tdate)))
	 (if (> (getd tdate 'low)(getd tdate 'high))(return (format nil "ERROR LOW PRICE ABOVE HIGH PRICE on ~A" tdate)))
	 (if (> (getd tdate 'close)(getd tdate 'high))(return (format nil "ERROR CLOSE PRICE ABOVE HIGH PRICE on ~A" tdate)))
	 (if (< (getd tdate 'open)(getd tdate 'low))(return (format nil "ERROR OPEN PRICE BELOW LOW PRICE on ~A" tdate)))	  
	 (if (< (getd tdate 'high)(getd tdate 'low))(return (format nil "ERROR HIGH PRICE BELOW LOW PRICE on ~A" tdate)))
	 (if (< (getd tdate 'close)(getd tdate 'low))(return (format nil "ERROR CLOSE PRICE BELOW LOW PRICE on ~A" tdate)))
     (if (= (getd tdate 'open)(getd tdate 'high)(getd tdate 'low)(getd tdate 'close)) 
          (format T "~A OPEN HIGH LOW CLOSE ARE EQUAL on ~A~%" *data-name* tdate))
	 (if (and (not (stringp (getd (getd tdate 'ydate) 'close)))(> (/ (getd tdate 'high)(getd (getd tdate 'ydate) 'close)) 1.5))
	     (return (format nil "HIGH PRICE ON ~A MORE THAN 25% ABOVE PREVIOUS CLOSE" tdate)))
	 (if (and (not (stringp (getd (getd tdate 'ydate) 'close)))(< (/ (getd tdate 'low)(getd (getd tdate 'ydate) 'close)) .5))
	     (return (format nil "LOW PRICE ON ~A MORE THAN 25% BELOW PREVIOUS CLOSE" tdate)))
	 
	 ));closes the two whens
     
     
     (setq tdate ndt)
     (if (> tdate stopdate) (return "DATA OK"))
     )));;;closes the loop the let and the defun

(defun data-check (markets)
  (dolist (ith markets)
     (terpri)
   (set-market ith)
   (test-data)))
;;;GIVEN A LIST OF ATTRIBUTES FOR EACH OF SEVERAL WAVES IT RETURNS THE COUNTS
;;;THAT HAVE WAVES WITH THESE ATTRIBUTES 
;;;e.g. '( (lb A et "8605071200") (lb D SB DBT) )

;;;this function is rarely used because it simply removes any volume that
;;;is in the data base for a given market. You must "select" the market
;;;before using this function!
(defun remove-market-volume (startdate stopdate)
  (let (tdate svals)
  (setq tdate (getnumdate startdate))
  (loop
    (setq svals (readt2 tdate))
    (ifn svals (return))
    (if (cdr (assoc 'aux-data svals))
	(setf (cdr (assoc 'aux-data svals)) nil))
    (if (cdr (assoc 'sdata svals))(setf (cdr (assoc 'sdata svals)) nil))
    (if (cdr (assoc 'stime svals))(setf (cdr (assoc 'stime svals)) nil))
    (update-file svals tdate)
    (if (eql tdate (getnumdate stopdate))(return))
    (setq tdate (cdr (assoc 'ndate svals)))
    )))

;;; This is all the new stuff from utils2, that used to be in subt????? - lmf 11/17/99


;;;this function is meant to test all the data in the user's market list
;;;The output is a file with all the errors found.
(defun global-test-data ()
  (let (ndt ydt tdate times prices startdate stopdate markets indx-cfg data-folders
        error errors (path "~/ewaves/index.cfg")(path2 "index.cfg")
        (path1 "~/ewaves/data-errors.dat") upper-dir (path3 "/home/ewaves/markets.path"))

        (with-open-file (stream path3)
            (setq upper-dir (read stream)))
        (setq path2 (string-append upper-dir path2))  
      
       (maind-x) (setq data-folders (mapcar #'file-namestring (directory upper-dir))) 
       (with-open-file (stream path)
            (setq indx-cfg (read stream)))
      (setq markets (mapcar 'car indx-cfg))
       (with-open-file (stream path2)
            (setq indx-cfg (read stream)))

      (dolist (market markets)
       (unless (member (string-downcase (format nil "~S" market)) data-folders :test #'equal)
              (format T "~%NO DATA FOLDER FOR ~A~%" market) (setq markets (remove market markets))))

       (dolist (market markets)
       (unless (assoc market indx-cfg) (format T "~%PERSONAL MARKET ~A NOT IN DATABASE~%" market)
              (setq markets (remove market markets))))

     (dolist (market markets)
    
     (set-market market)(push (cons market nil) *cat-list*)
     (setq startdate (car (month-days (get-first-index-date)))
           stopdate (car (last (month-days (get-latest-index-date)))))

    (setq tdate startdate)  
    (setq error
    (loop
     (when (<= tdate stopdate)
       (if (equal (day-of-week (format nil "~A" tdate)) "SUNDAY") 
	   (return (format nil "~A ~A is a SUNDAY" market tdate)))
       (if (equal (day-of-week (format nil "~A" tdate)) "SATURDAY") 
	   (return (format nil "~A ~A is a SATURDAY" market tdate)))
       (setq ndt (getd tdate 'ndate))
       (ifn (or ndt (= tdate stopdate)) (return (format nil "~A NO NEXT DATE ~A" market tdate)))
       (if (stringp ndt) (return (format nil "~A BAD NEXT DAY ~A" market ydt)))
       (setq ydt (getd ndt 'ydate))
       (ifn (or (eql tdate ydt)(= tdate stopdate))
	    (return (format nil "~A MISMATCHED DATES ~A ~A" market tdate ndt)))
       (setq times (getd tdate 'ptime) prices (getd tdate 'pdata))
       (setq prices (remove nil prices))
       (when (member 0 prices) (return (format nil "~A ZERO PRICE ON ~A" market tdate)))
       (when (eql (getd tdate 'high) 0)(return (format nil "~A ZERO HIGH ON ~A" market tdate)))
       (when (eql (getd tdate 'low) 0)(return (format nil "~A ZERO LOW ON ~A" market tdate)))
       (ifn prices
	    (return (format nil "~A NO DATA ON ~A" market tdate)))
       (if (neql (length times)(length prices))
	   (return (format nil "~A MISSING DATA ~A" market tdate)))
     
       (when (assoc 'high (cdr (readt2 tdate)))
	 (ifn (getd tdate 'high) (return (format nil "~A MISSING HIGH PRICE ON ~A" market tdate)))  
	 (ifn (getd tdate 'low) (return (format nil "~A MISSING LOW PRICE ON ~A" market tdate)))	 
         (ifn (getd tdate 'open)(return (format nil "~A  OPEN PRICE IS MISSING on ~A" market tdate)))

	 (if (> (getd tdate 'open)(getd tdate 'high))(return (format nil "~A OPEN PRICE ABOVE HIGH PRICE on ~A" market tdate)))
	 (if (> (getd tdate 'low)(getd tdate 'high))(return (format nil "~A LOW PRICE ABOVE HIGH PRICE on ~A" market tdate)))
	 (if (> (getd tdate 'close)(getd tdate 'high))(return (format nil "~A CLOSE PRICE ABOVE HIGH PRICE on ~A" market tdate)))
	 (if (< (getd tdate 'open)(getd tdate 'low))(return (format nil "~A OPEN PRICE BELOW LOW PRICE on ~A" market tdate)))	  
	 (if (< (getd tdate 'high)(getd tdate 'low))(return (format nil "~A HIGH PRICE BELOW LOW PRICE on ~A" market tdate)))
	 (if (< (getd tdate 'close)(getd tdate 'low))(return (format nil "~A CLOSE PRICE BELOW LOW PRICE on ~A" market tdate)))
	 (if (and (not (stringp (getd (getd tdate 'ydate) 'close)))(> (/ (getd tdate 'high)(getd (getd tdate 'ydate) 'close)) 1.25))
	     (return (format nil "~A HIGH PRICE ON ~A MORE THAN 25% ABOVE PREVIOUS CLOSE" market tdate)))
	 (if (and (not (stringp (getd (getd tdate 'ydate) 'close)))(< (/ (getd tdate 'low)(getd (getd tdate 'ydate) 'close)) .75))
	     (return (format nil "~A LOW PRICE ON ~A MORE THAN 25% BELOW PREVIOUS CLOSE" market tdate)))
	 
	 ));closes the two whens
     
     
     (setq tdate ndt)
     (if (> tdate stopdate) (return)) ;(format nil "~A DATA OK" market)))
     ));;;closes the loop and setq

     (if error (push error errors))

);;;closes the dolist

    (with-open-file (stream path1 :direction :output :if-exists :supersede)
      (dolist (ith errors)
        (format stream "~%~A" ith)))

));;;closes the let and the defun



;;;days is the length of the regression time
(defun roc-dump (market sdate tdate )
   (let ( (date tdate) date-1 atr tr
         (path1 (string-append *output-upper-dir* "eproc-dump.dat")))
   (set-market market)(setq date-1 (getd date 'ydate))
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (loop
      
      (setq  atr (volatility date 90 1) tr (true-range date))
               
      (format stream "~8@A  ~8@A ~6@A ~6@A  ~8@A  ~8@A ~8A ~%" 
            date (getd date 'open) (getd date 'high) (getd date 'low) (getd date 'close) tr atr )
             
      (if (<= date sdate )(return))
      (setq date date-1 date-1 (getd date-1 'ydate))

    ))
))

(defun indicator-study3 (markets &optional (screen 'all))
  (let (new-trades-path bin) 
  (labels ((nineteen (s)(svref s 19)))
  (maind-x)(set-cat-list)(setq daytrades nil)
  
  
    (dolist (market markets)
    (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "daytrades3.dat")) 
    (with-open-file (str new-trades-path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))
    )
;;;;filter out the top 5% of trades
   (case screen
     (all )
     (best  (vsort daytrades #'> #'nineteen)
            (setq daytrades (subseq daytrades 0 (truncate (length daytrades) 20))))
     (worst  (vsort daytrades #'< #'nineteen)
             (setq daytrades (subseq daytrades 0 (truncate (length daytrades) 20))))
    )

   (dotimes (ith 14)
;     (day-trade-bins3a 1 (1+ ith))
    (setq  day-bin-codes nil)(clrhash *day-trade-warehouse3*)
   (dolist (record daytrades);;;record is a vector and bin is a list
     (setq bin (encode-day-trades3 record (list 1 (1+ ith))))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
         );;;closes the dolist

    (format T  "~%FEATURES = ~A~%" (1+ ith))
    (rank-daytrade-bins-by-profit3 daytrades)


     (display-daytrade-bins-by-expected-value3 (+ ith 1))
    )
)))

(defun indicator-study2c ()

 (dotimes (ith 14)
   (currency-trade-bins2a 1 (1+ ith))
  (display-daytrade-bins-by-expected-value3 (1+ ith))

))


(defun indicator-study2x ()

 (dotimes (ith 14)
   (forex-trade-bins2a 1 (1+ ith))
  (display-daytrade-bins-by-expected-value3 (1+ ith))

))


(defun indicator-study4a ()

 (dotimes (ith 14)
   (day-trade-bins4a T 1 (1+ ith))
  (display-daytrade-bins-by-expected-value3 (1+ ith))

))


(defun indicator-study4x ()

 (dotimes (ith 14)
   (day-trade-bins4ax 1 (1+ ith))
  (display-daytrade-bins-by-expected-value3 (1+ ith))

))

(defun indicator-study2a ()

 (dotimes (ith 14)
   (day-trade-bins2a 1  (+ ith 1))
  (display-daytrade-bins-by-expected-value3 (+ ith 1))

))


(defun indicator-study ()

 (dotimes (ith 14)
   (swing-trade-bins nil 1  (+ ith 1))
  (display-swingtrade-bins-by-expected-value (+ ith 1))

))


(defun indicator-studyq ()

 (dotimes (ith 14)
   (qswing-trade-bins 1  (+ ith 1))
  (display-swingtrade-bins-by-expected-value (+ ith 1))

))



(defun indicator-study1 ()
  (let (plot (path1 "~/exitpoints/indicator-test.csv"))
  (maind-x)(set-cat-list)
  (day-trade-bins3a 1 2)

  (dolist (ith daytrades)
   (set-market (svref ith 0))
   ;(format T "date = ~A  ydate= ~A mkt = ~A~%" (svref ith 1) (getd (svref ith 1) 'ydate) (svref ith 0))
   (setq plot (acons (svref ith 19) (gann-slope-ratio (getd (svref ith 1) 'ydate) 11 84) plot))
   )

  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (kth plot)
      (format str "~A\, ~A~%" (car kth)(cdr kth))))
 ))
;;;displays the trades for a market month of trading
(defun trade-study (market edate )
  (let ( dates mtrades)
     (set-market market)
     (setq  dates (month-days (truncate edate 100)))
   
   (apply #'day-trade-bins3b *day-features3*);;;fills daytrades
;;;;these are the market dates
  (dolist (kth dates)
    (push (find-if #'(lambda(s1) (and (eql (svref s1  0) market)(eql (svref s1 1) kth))) daytrades)
       mtrades))
   (setq mtrades (remove nil mtrades))
   (dolist (kth mtrades)
     (format T "~A~%" kth))(terpri)
   (bstat mtrades)

))  

;;;used with the above function trade-study
(defun bstat (mtrades)
  (let (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
  (dolist (jth mtrades)
    
    (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
           (bin-classifier-daytrades3b (getd (svref jth 1) 'ydate) *day-features3*))
    (format T"~A  ~A~%" (svref  jth 0) (svref jth 1))
    (format T "~A  ~A  ~A  ~A  ~A  ~A  ~A ~A~%"
            epsignal longs long-gains long-acc shorts short-gains short-acc bin)
    (format T "Direction = ~A  P$L =  ~A~%~%" (svref jth 2) (svref jth 19))
    )
    (format T "ToTAL P&L = ~A~%~%" (apply #'+ (mapcar #'(lambda(s) (svref s  19)) mtrades)))
))


;; PERCENTILE
;; Rosner 19
;; NB: Aref is 0 based!
;;; percent is like 50 not .5 for 50%.
(defun percentile2 (percent sequence )
;  (test-variables (sequence :numseq) (percent :percentage))
  (let* ((sorted-vect (coerce (sort (copy-seq sequence) #'<) 'simple-vector))
         (n (length sorted-vect))
         (k (* n (/ percent 100)))
         (floor-k (floor k)))
    (if (= k floor-k)
        (/ (+ (aref sorted-vect k)
              (aref sorted-vect (1- k)))
           2.0)
        (aref sorted-vect floor-k))))
      
;;;given a value it returns the percentile within a list
;;;percentile is a decimal value.    	  
(defun percentile1 (value lst)
   (let ((lgth (length lst))(tlist (copy-list lst)))
      (setq tlist (remove-if #'(lambda (s) (>= s value)) tlist))
      (my-round (/ (length tlist) lgth) 2)
      ))
      
;;;;returns a value that is above
;;; the per percentile of the lst of numbers
;;;enter per as a decimal fraction
(defun percentile (per lst)
 (let ((lgth (length lst))(tlist (copy-list lst)))
   (setq tlist (sort tlist #'<))
   (nth (1- (ceiling (* lgth per))) tlist)

))


(defun show-worst-unfiltered-trades (swings)

  (vsort swings #'< #'(lambda(s) (svref s 19)))
  (dotimes (ith 10)
   (print (nth ith swings))))


(defun show-best-unfiltered-trades (swings)

  (vsort swings #'> #'(lambda(s) (svref s 19)))
  (dotimes (ith 10)
   (print (nth ith swings))))

(defun show-worst-trades (path1)
    (let (records record1 )
    (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
 ;       (if (assoc (car record1) records)
 ;           (setf (third (assoc (car record1) records))
 ;                 (+ (abs (third (assoc (car record1) records))) (abs (third record1)))
 ;             (fourth (assoc (car record1) records))
 ;             (+ (fourth (assoc (car record1) records)) (fourth record1))
           ;   (fifth (assoc (car record1) records))
           ;   (+ (fifth (assoc (car record1) records)) (fifth record1))
 ;             )
         (push record1 records));;closes the do
   )
     (vsort records #'< #'(lambda(s) (ninth s)))
     (dotimes (ith 10)
          (print (nth ith records)))

))


(defun show-best-trades (path1)
    (let (records record1 )
    (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
 ;       (if (assoc (car record1) records)
 ;           (setf (third (assoc (car record1) records))
 ;                 (+ (abs (third (assoc (car record1) records))) (abs (third record1)))
 ;             (fourth (assoc (car record1) records))
 ;             (+ (fourth (assoc (car record1) records)) (fourth record1))
           ;   (fifth (assoc (car record1) records))
           ;   (+ (fifth (assoc (car record1) records)) (fifth record1))
 ;             )
         (push record1 records));;closes the do
   )
     (vsort records #'> #'(lambda(s) (ninth s)))
     (dotimes (ith 10)
          (print (nth ith records)))

))



;;;;reads diary file and sorts by quantity
;;;;each line of the diary file has four fields
;;;separated by commas:   date, quantiy, daily gain/loss. cumulative P&L
(defun distribution-qty (path1)
  (let (records results record1)  

   (when (probe-file path1)
     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (push record1 records));;closes the do
       ))

    ;;sort by quantity
   (dolist (ith records)
     (cond ((assoc (second ith) results)
            (setf (cdr (assoc (second ith) results))
                  (cons (third ith) (cdr (assoc (second ith) results)))))
           (t (setq results (cons (list  (second ith) (third ith)) results)))
                  ))

     (vsort results #'< 'car) 

   (dolist (jth results)
     (format T "~%~A  ~A  ~A" (car jth)(length (cdr jth))(list-sum (cdr jth)))
      )

))



;;;path1 is the location of the ts test file.
(defvar path1 "~/mk-data/111-dropbox/edgeai/results.csv")
(defun ts-brochure1 (path1 &optional (commission 15);(min-acct 40000)
                                         (vendor 300))
  (let (record1 records draw flat ;num-days ave-loss-day ave-win-day 
        dates trades trades1 dates1 records2
         num-months draw-low-date draw-start-date records1 record num-trades)
   (maind-x)(set-cat-list)(set-market 'dj.d1b)
   (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
    ;    (format t "~%~S" record)
        (setq record1 (mapcar #'(lambda(s) (read-from-string s nil nil)) (my-split-sequence #\, record) ))
        (if (and (numberp (second record1))(car record1)(nth 4 record1))
    ;  (if (member (second record1) '(YM NK ES US TY HG PL NG S SM))
            (push record1 records))
     ;   (format t "~%~S" record1)
       )) ;;closes with-open-file and the dolist over paths

;     (format t "~% Records = ~A ~%" records)

   ;  (format t "~%P&L Before deductions = ~A~%"  (list-sum (mapcar #'(lambda(s)(fourth s)) records)))
     (setq num-trades  (list-sum (mapcar #'(lambda(s)(second s)) records)))
      

   (dolist (ith records)
    (setq record  (cons (read-from-string (string-append (format nil "~A" (nth 5 ith))
                                             (if (< (nth 4 ith) 10)
                                             (format nil "0~A" (nth 4 ith))
                                             (format nil "~A" (nth 4 ith)))))
            ith))
       (setq record (butlast record 2)) (push record records1))
  
      (format t "~%Records1 = ~A~%"  records1)  

  ;  (format t "~%P&L records1 = ~A~%" (list-sum (mapcar #'(lambda(s) (fifth s)) records1)))
  ;  (format t "~%Num trades records1 = ~A~%" (list-sum (mapcar #'(lambda(s) (third s)) records1)))

   (setq records1 (vsort records1 #'> 'car)) (setq records2 nil)
;;;;combine trades into months
   (dolist (jth records1)
      (if (assoc (car jth) records2)
          (setf (nth 1 (assoc (car jth) records2))(+ (nth 1 (assoc (car jth) records2)) (nth 2 jth))
                (nth 2 (assoc (car jth) records2))(+ (nth 2 (assoc (car jth) records2)) (nth 4 jth)))
          (push (list (car jth)(third jth)(fifth jth)) records2))) 
  
   (format t "~%Before fees = ~A~%" (list-sum (mapcar #'(lambda(s) (third s)) records2)))               
   (format t "~%Records2 = ~A~%" records2)
  (setq num-months (length records2)) ;(nth-value 1 (ts-monthly-gains-losses records nil)))
 
  ;;;remove the commission
    (dolist (ith records2)
   ;  (print ith)
    (setf (third ith)(- (third ith)(* (second ith) commission) vendor)))     
   
   (setq num-trades (list-sum (mapcar #'(lambda(s) (nth 1 s)) records2))) 

   (setq trades (mapcar #'(lambda (s)(third s)) records2)
         dates (mapcar #'(lambda (s)(first s)) records2))


  ;(format t "~%~A ~A" dates trades) 
   ; (setq tdates dates)
  
    (format t "~%After fees = ~S" records2) 
  (setq trades1 trades dates1 dates)
 
   (multiple-value-setq (draw flat draw-low-date draw-start-date)(drawdown1 trades dates))
  ; (format T "~% ~A ~A" (car (last dates)) (first dates))
  ; (setq num-days (1+ (sub-mkt-dates (car (last dates))(first dates))))
  ; (print "12345")
 ;  (setq pls (build-ts-diary records))
   (format T "~%Total Period Profit($) = ~D~%" (round (list-sum trades)))
   
   (format T "Number of months = ~A~%Number of Trades = ~A~%" num-months (length records))

   (format T "Average profit per month($) = ~D~%" (round (/ (list-sum trades) num-months))) 
   (format T "Average profit per trade($) = ~D~%" (round (/ (list-sum trades) (length records)))) 
   (format T "maximum drawdown($) = ~D~%" (round draw))
 ;   (format T "Flat time = ~A~%" flat)
   (format T "Max-drawdown start = ~A~%~%" draw-start-date)
   (format T "Max-drawdown Low date = ~A~%~%" draw-low-date)
   (format T "months with a gain(%)= ~3,1F~%" (* 100.0 (/ (count-if #'(lambda(s) (plusp s)) trades)
                                                       (length trades))))
   
   (format T "months with a breakeven(%)= ~3,1F~%" (* 100.0 (/ (count-if #'(lambda(s) (zerop s)) trades)
                                                          (length trades))))
   (format T "months with a loss(%)= ~3,1F~%~%" (* 100.0 (/ (count-if #'(lambda(s) (minusp s)) trades)
                                                       (length trades))))


 ;  (format T "Days with a gain(%) = ~A~%" 
 ;       (my-round (* 100 (/ (count-if #'(lambda(s) (plusp (cdr s))) pls) num-days)) 1))

 ;  (format T "Days with no trades(%) = ~A~%" 
 ;       (my-round (* 100 (/ (- num-days (length pls)) num-days)) 1))
 ;  (format T "Days with a loss(%) = ~A~%~%" 
 ;       (my-round (* 100 (/ (count-if #'(lambda(s) (minusp (cdr s))) (build-ts-diary records)) num-days)) 1))
 ;  (format T "Round Turn Deduction for Commission and Vendor fee($) = ~D~%" (round commission))

;   (setq ave-win-day  (/  (list-sum (remove-if #'(lambda (s)(not (plusp s))) trades))
;           (length (remove-if #'(lambda (s)(not (plusp s))) trades))))
   
 ;  (format t "Average winning trade($) = ~A~%" (round ave-win-day))
   
 ;  (setq ave-loss-day   (/  (list-sum (remove-if #'(lambda (s)(plusp s)) trades))
 ;          (length (remove-if #'(lambda (s)(plusp s)) trades))))
    
 ;  (format T "Average losing trade ($) = ~A~%" (round ave-loss-day))
      
;   (format T "Payoff Ratio = ~4,2F~%~%" (abs (/ ave-win-day ave-loss-day)))
;   (format T "Max consecutive Days with no Trades = ~A~%" (max-inactive-time dates))
;   (format T "Time in the market(%) = ~A~%" (my-round (* 100 (/ (length pls) num-days)) 1))

;   (format T "Largest single losing month($) = ~D~%" (min* trades) 
;          ; (round  (apply #'min (mapcar #'cdr pls))))
;   (format T "Largest single winning day($) = ~D~%" (max* trades)) ;(round (apply #'max (mapcar #'cdr pls))))
;;;sum of the winning days divided by te sum of the losung days.

 ;  (format T "Profit factor = ~4,2F ~%"
 ;      (abs  (/  (list-sum (mapcar #'(lambda (s)(if (plusp (cdr s)) (cdr s) 0)) pls))
 ;                    (list-sum (mapcar #'(lambda (s)(if (plusp (cdr s)) 0 (cdr s))) pls)))))
   (format T "Largest single losing month = ~D~%" (round (apply #'min trades)))
   (format T "Largest single winning month = ~D~%" (round (apply #'max trades)))

   (setq num-months (ts-monthly-gains-losses records2 t))
 ;  (format T "~%Recommended Minimum Equity($) = ~D~%" min-acct) 
 ;  (format T "~%Num Trades Per Month = ~A~%" (round (/ (length records) (length num-months))))
 ;  (format T "Annual Rate of Return(%) = ~4,1F~%"
 ;     (* 100  (/ (/ (list-sum trades)(/ (length num-months) 12)) min-acct)))
 ;  (format T "Max drawdown(%) = ~4,1F~%" (* 100 (/ draw min-acct)))

 ;  (format T "~%1/2 Optimal-F = ~A~%" (round (* 2 (optimal-f trades))))
 ;  (exit-statistics1 records)

))




;;;function to create fake ts-e12.csv files
(defun ts-fake-file (tdate days)
    (let ((date tdate)(ts-fore-path2 "~/mk-data/111-dropbox/ts-fake-es-short.csv")(time-zone 'CT)
           date1 (direction 'SELL))

    (dotimes (ith days)
     ; (setq direction (mvhl-trend date 3)
      (setq date1 (getd date 'ndate))
      ;(if (member direction '(UP SUP XUP)) (setq direction 'BUY))
     ; (if (member direction '(DN SDN XDN))(setq direction 'SELL))
      
      (with-open-file (ninja-output ts-fore-path2 :direction :output :if-exists :append :if-does-not-exist :create)
                  (write-ts-record ninja-output  time-zone date date1 direction 0.0 0.0 ))
      (setq date (getd date 'ydate))

    )
))

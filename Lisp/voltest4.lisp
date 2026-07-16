;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


(defparameter *day-features2* '(1 2 3 6 11 13 14)) 
;;;enters with limit orders and exits end of the day or with stop loss orders
;;;Plain  with 4 .764 and 1 parameter values.
;;;;For DAY TRADING
;;;Designed to write out the file for the DAY Trades Warehouse.  

;;;;the file has one record per trade each record is a list. The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1 
;;;;;;;
;;;
;;; (*data-name* entry-date direction entry-price HT BT BR T5 T15 DO BR-1 CAN SST T45 ZBL BPL TIM)
;;; This is the GRABBER system
;;; enters on a stop if the market opens under the previous day's close for a long otherwise enters with limit order
;;;enters on a stop if the market opens above the previous day's close for a short otherwise enters with limit order
;;;
;;; exits at the end of the day with no objective
;;; the stop loss price is at .382 of the average true range over past 4 days
   
(defun populate-day-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 cover-long cover-short entry-long entry-short risk
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short 
       (path1 (string-append "~/exitpoints/" (format nil "~S" market) "daytrades2.dat")))
    
   (maind-x)(set-cat-list)
   (set-market market) 
   
   (unless date2 
       (setq date2 (car (last (month-days (get-latest-index-date))))))       
   
   (setq date (add-mkt-days date2 (- num)))
 
   (setq date-1 (getd date 'ydate))
 
  
 ;;;;from date1 to date2
 (dotimes (ith num)
  
 
   (multiple-value-setq (stop-long stop-short)(vprices date 4 .382 1))   
  
   (setq entry-short (getd date 'close) entry-long (getd date 'close))
   
   
   (setq risk (min (abs (- stop-long (getd date 'close)))
                   (/ *max-day-risk* (index-point-value))))       
  
   (setq   date-1 date date (add-mkt-days date 1))
    
 
      
;;;check if new entry  with limit orders at previous day's close
;;;if price opens below entry long
      
   (when (and (< (getd date 'open) stop-short) 
              (> (getd date 'high) entry-short)                                                       
              )
          (setq short  entry-short                   
                short-trade (create-daytrade-entry-record-list1 date-1 -1 short)
                stop-short (+ short risk)
                 ))
 
                 
    (when (and (> (getd date 'open) stop-long) 
               (< (getd date 'low) entry-long)                                                  
             )                                         
           (setq long entry-long                    
                 long-trade (create-daytrade-entry-record-list1 date-1 1 long)
                 stop-long (- long risk)
                 ))
              
            
 ;;;check if stopped out on same day 

   (when (and long stop-long                                   
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq long-trade (append long-trade (list date 'exit (my-pretty-price stop-long)
                                        (round (* (index-point-value) (- stop-long long))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))
     
    (when (and short stop-short                                
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date 'exit (my-pretty-price stop-short)
                                           (round (* (index-point-value) (- short stop-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
   
           
;;;check if met exit criteria to exit at end of day of entry for day trade     
     (when long                
            (setq cover-long (getd date 'close))                   
            (push (- cover-long long) trades)
            (setq long-trade (append long-trade (list date 'exit (my-pretty-price cover-long)
                                           (round (* (index-point-value) (- cover-long long))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))
            
      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit (my-pretty-price cover-short)
                                                (round (* (index-point-value) (- short cover-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))  
           
   );;;closes the dotimes
  
  
     
   ;;;;apply commission of $25 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ *day-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))
    
  (when outfile   
 
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
  
   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round (* (list-sum trades) (or (index-point-value) 1))) (length trades)
       (/ (- (length trades) (length losers)) 
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (* (list-sum trades) (or (index-point-value) 1)) (if (zerop (length trades)) 1 (length trades)))) 
       (round ave-win)
       (round ave-loss)
       (round (* (or (min* losers) 0) (or (index-point-value) 1))) 
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers)) 
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (* (drawdown trades)(or (index-point-value) 1)))
      ; (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
       )
   
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)    
       (format stream "~S~%" ith) 
    ));;closes the dolist and with-open-file
 
  );;;closes the when outfile  
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
       (length trades))
 ));     




(defun daytrade-simulation-test2 (market date2 num &optional (stocks nil)(features *day-features2*)) 
 (let (date trades long short cover-long cover-short  record trading-dates 
        date-1 signal longs long-gains shorts short-gains (running-sum 0) risk
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short 
       long-acc short-acc  (twr-long 1) (twr-short 1) bin draw
       (outfile (string-append "~/exitpoints/" (format nil "~S" market) "day-summary2.dat"))
       (path1 (string-append "~/exitpoints/" (format nil "~S" market) "day-simulation2.dat"))
       (path2 (string-append "~/exitpoints/" (format nil "~S" market) "day-diary2.dat")))    
   (declare (ignore long-acc short-acc))

   (if stocks (setq *stocks* T)(setq *stocks* nil))
  
 
   (apply #'day-trade-bins2 features)
   (set-market market)
   
   (setq date (add-mkt-days date2 (- num)))
   (setq record (list date (getd date 'close) 0 0 0))
   (push record trading-dates)
 
 ;;;;from date1 to date2
 (dotimes (ith num)
 
   (multiple-value-setq (stop-long stop-short)(vprices date 4 .382 1)) 
   
   (setq risk (abs (- stop-short stop-long)))
     
   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (list date (getd date 'close) 0 0 0)) 
           
;   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
;    			             ; cover-long (+ cover-long (getd date 'rollover))
;    				      
;    ))
;   (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))
;    	                             ; cover-short (+ cover-short (getd date 'rollover))
;    						 
;    ))

;;;;calculate bin-classifier only as needed
 (when (or (< (getd date 'open) stop-short)
           (> (getd date 'open) stop-long))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades1 date-1 features))
       (setq twr-short (day-bin-twr1 bin))(setf (nth 0 bin) 1)
       (setq twr-long (day-bin-twr1 bin))
       )
      
;;;check if new entry   
      
   (when (and              
              (<= (* risk (index-point-value)) *max-day-risk*) 
              (< (getd date 'open) stop-short) 
              (> (getd date 'high) stop-long)              
              (member signal '(OK DOWN))(> shorts 0.0)
              (>= (/ short-gains shorts) 125.0) (> twr-short 1.0) 
              (>= (/ short-gains shorts) (if (> longs 0.0) (/ long-gains longs) 0))                        
                 )
          (setq short  (min (getd date 'open) stop-long)                    
                short-trade (list date 'short short)
             
                ) 
           (setf (nth 2 record) -1)      
           (setf (nth 3 record) (- (nth 3 record) (/ *day-commission* (index-point-value))))
                 )
 
                 
    (when (and  
               (<= (* risk (index-point-value)) *max-day-risk*) 
               (> (getd date 'open) stop-long) 
               (< (getd date 'low) stop-short)              
               (member signal '(OK UP))(> longs 0.0)
               (>= (/ long-gains longs) 125.0) (> twr-long 1.0) 
               (> (/ long-gains longs) (if (> shorts 0.0)(/ short-gains shorts) 0))                     
             )
                                         
           (setq long  (max (getd date 'open) stop-short)                      
                 long-trade (list date 'long long) 
              
                 ) 
           (setf (nth 2 record) 1)      
           (setf (nth 3 record) (- (nth 3 record) (/ *day-commission* (index-point-value))))
                 )
              
            
 ;;;check if stopped out on same day 

   (when (and long stop-long                                  
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq long-trade (append long-trade (list date 'exit stop-long (- stop-long long))))
           (push long-trade extended-trades)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setq long-trade nil long nil stop-long nil
           ))
       
    (when (and short stop-short                                 
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date 'exit stop-short (- short stop-short))))
           (push short-trade extended-trades)
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setq short-trade nil short nil stop-short nil
           ))
   

           
;;;check if met exit criteria on day of entry (exit at end of day)     
     (when long                
            (setq cover-long (getd date 'close))                   
            (push (- cover-long long) trades)
            (setq long-trade (append long-trade (list date 'exit cover-long (- cover-long long))))
            (push long-trade extended-trades)
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- (getd date 'close) long)))
            (setq long-trade nil long nil stop-long nil))
            
      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short (getd date 'close))))
           (setq short-trade nil short nil stop-short nil))    
       
   
      (setf (nth 4 record) (+ (nth 3 record) running-sum))
      (setq running-sum (nth 4 record))
      (push record trading-dates)
      (setq record nil)  
         
           
   );;;closes the dotimes
  
     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%" 
           (first ith) (second ith) (third ith) (round (* (index-point-value)(nth 3 ith)))
           (round (* (index-point-value) (nth 4 ith))))  
     )) 
     
   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ *day-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))
    
  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
 
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
;   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2) 
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A~%" (length daytrades))       
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D  "    
       
       (round (* (list-sum trades) (or (index-point-value) 1))) (length trades)
       (/ (- (length trades) (length losers)) 
          (if (zerop (length trades)) 1 (length trades)))
      
       (round (/  (* (list-sum trades) (or (index-point-value) 1)) (if (zerop (length trades)) 1 (length trades)))) 
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (* (or (min* losers) 0) (or (index-point-value) 1))) 
       
       (round (* (or (max* winners) 0)(or (index-point-value) 1))) 
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers)) 
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (* (drawdown trades)(or (index-point-value) 1))))
       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
         )     
    
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/ (* (list-sum trades) (or (index-point-value) 1))
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))
   
   
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
        ));;;closes the with-open-file
  );;;closes the when outfile  
   
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)  
   ))     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun day-trade-bins2 ( &rest features)
  (let (bin path)
  (if *stocks* (setq path "~/exitpoints/stocksdaywarehouse2.dat")
    (setq path "~/exitpoints/daytradewarehouse2.dat"))
  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse1*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record daytrades)))
 
     
;;;;now all the trades are in the list swings.  
;;;we now assign/sort the trades into bins   
;;;basically process a record list into a code or bin
;;;and create a code
   
   (dolist (record daytrades)
     (setq bin (encode-day-trades1 record features))
     (pushnew bin day-bin-codes :test #'equal)
     (cond ((gethash bin *day-trade-warehouse1*)
            (ifn (member record (gethash bin *day-trade-warehouse1*) :test #'equal)
                 (setf (gethash bin *day-trade-warehouse1*)
                       (cons record (gethash bin *day-trade-warehouse1*)))))
           ((not (gethash bin *day-trade-warehouse1*))
             (setf (gethash bin *day-trade-warehouse1*)
              (list record)))))
  
    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit1 daytrades) 
     
         
  ))     
  
  
   
;;;requires a candidate list to add to the base features         
(defun daytrades-add-one-in2 (base-features candidate-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq *stocks* T)(setq *stocks* nil))
  (apply #'day-trade-bins2 base-features)
  
  (dolist (ith candidate-features)
    (setq result (apply #'day-trade-bins2 (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))    

;;;requires a base features list       
(defun daytrades-leave-one-out2 (base-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq *stocks* T)(setq *stocks* nil))
  (apply #'day-trade-bins2 base-features)
  
  (dolist (ith base-features)
    (setq result (apply #'day-trade-bins2 (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 )) 
   
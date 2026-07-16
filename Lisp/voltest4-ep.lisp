;;; -*- Mode: LISP; Package: user; Base: 10. -*-
#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;enters with limit orders and exits end of the day or with stop loss orders
;;;Plain  with 4 .236 and .944 parameter values.
;;;;For DAY TRADING.944
;;Designed to write out the file for the DAY Trades Warehouse.

;;;;the file has one record per trade each record is a list. 
;;;The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1
;;;;;;;
;;;
;;; (*data-name* entry-date direction entry-price HT BT BR T5 T15 DO BR-1 CAN SST T45 ZBL BPL TIM)
;;; This is the EPIC system
;;; enters on a stop if the market opens under the previous day's close for a long otherwise enters with limit order
;;;enters on a stop if the market opens above the previous day's close for a short otherwise enters with limit order
;;;
;;; exits at the end of the day with no objective
;;; the stop loss price is at .65 of the average true range over past 4 days

(defun update-daytrade-warehouse2 (date &optional (markets *epic-warehouse-list*))

   (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-day-trades2 ith date (available-days ith date 500)))
 
   (build-daytrade-warehouse2 markets) 
   (setq *day-features2* (find-best-indicator-set2a))
    (portfolio-simulation3 '(epic) date 4040 (list *epic-list*))
 ;  (portfolio-simulation3 '(epic) date 5040 (list markets))
)

(defun populate-day-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 cover-long cover-short entry-long entry-short risk  vri1 vri2
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades2.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

   (format outfile "~%~A~%" market)

 ; (setq *entry-factor* .3333 *stop-loss-day* .6667 *max-day-risk* 3000) ;.944) ;;;;1.0557)
   (setq *entry-factor* .3333 *stop-loss-day* .944
         *max-day-risk* 2000 
        *commission* 40 *pips-slippage* 0)
 
   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

 ;  (format T "~A ~A~%" date (getd date 'close))
  (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
 ;  (let ((two-bars (2-bar date)))
 ;      (setq entry-short (nth 1 two-bars) entry-long (nth 2 two-bars)
 ;            ))

   
   (setq risk (volatility date 4 *stop-loss-day*))
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1))

   (setq  date-1 date date (add-mkt-days date 1))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover))
    ))

    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk));;;needs to be after the rollover
;;;
;;;
;;; entry short
   (when (and (<= (getd date 'low) entry-short)
         
              (>= (* risk (calculate-point-value date))
                  (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
	      (<= (* risk (calculate-point-value date)) *max-day-risk*)
           ;   (plusp vri1)(plusp vri2)
              )
          (setq short  (min entry-short (getd date 'open))
                short-trade (create-daytrade-entry-record-list2 date-1 -1 short)
                 ))

    (when (and (>= (getd date 'high) entry-long)
         
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
               (<= (* risk (calculate-point-value date)) *max-day-risk*)
      ;         (plusp vri1)(plusp vri2)
                )
           (setq long (max entry-long (getd date 'open))
                 long-trade (create-daytrade-entry-record-list2 date-1 1 long)
                 ))

;;;check if stopped out on same day

   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
         
                  (<= (getd date 'close) stop-long))
              ; (<= (getd date 'low) stop-long)
               )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date-1))
                           (comm+slip date-1))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                    (round (* (calculate-point-value date) (my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
             
                    (>= (getd date 'close) stop-short))
               ; (>= (getd date 'high) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date-1))
                           (comm+slip date-1))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                   (round (* (calculate-point-value date)(my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1))
                            (comm+slip date-1))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                   (round (* (calculate-point-value date)(my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1))
                              (comm+slip date-1))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                 (round (* (calculate-point-value date) (my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes

  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners) 
                            (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) 
                            (if (zerop (length losers)) 1 (length losers)))
        
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (drawdown trades))
      ; (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
       )

     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
       (format stream "~S~%" ith)
    ));;closes the dolist and with-open-file

  );;;closes the when outfile
  (values (round (list-sum trades))
       (length trades))
 ));

;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-daytrade-entry-record-list2 (date direction entry)
   (let* ((date-1 (getd date 'ydate)) )
;       (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                             (sector-index *data-name* );;feature 2
                             (roc-rel-index5 date ) ;feature 3  
                             (ep-roc-index date 10) ;;features 4   
                             (volatility-ratio-index date 4 28 1) ;;;feature 5             
                             (slow-stochastic-index date 30) ;; feature 6 
                   
                             (reflect3  date 14);;;feature 7

                             (volatility-change-index date 4 28 2) ;;;feature 8
                           
                             (wpp-index0 date);;  feature 9                                    
                             (wpp-index0 date-1);;feature 10                                               
                             (wpp-index0 (getd date-1 'ydate)) ;;feature 11
                     
                             (channel-direction-index21 date);;feature 12
                             (channel-direction-index9 date);;feature 13 

                             (channel-direction-index14 date) ;;feature 14 
                                       
                       
                              );;;closes the list

 ); closes the let
)

;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-daytrade-indicators2 ()
   (let ((path (string-append *upper-dir-warehouse* "daytradewarehouse2.dat"))
           date-1)
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))
     ;(setq date-2 (getd date-1 'ydate))

      ;  (setf (svref ith 4) (sector-index *data-name* ));;feature 2
      ;  (setf (svref ith 5) (roc-rel-index date 10 90)) ;;features 3              
      ;  (setf (svref ith 6) (reflect3  date 14));;;feature 4

      
       ; (setf (svref ith 7) (volatility-ratio-index date-1 4 28 1));;;feature5       
      ;  (setf (svref ith 9) (ldev-index date 9));;  feature 7
                        
      ;  (setf (svref ith 10) (roc-rel-index21 date)) ;;;feature 8  
        (setf (svref ith 14) (channel-direction date-1 21));;feature 12 
        (setf (svref ith 15) (channel-direction  date-1 9)) ;feature 13  5 levels
        (setf (svref ith 16) (channel-direction  date-1 14)) ;feature 14
      ;  (setf (svref ith 14) (my-trend date 14)) ;;feature 12                     
                            
      ;  (setf (svref ith 15) (slow-stochastic-index date 30)) ;; feature 13 6 levels

     ;  (setf (svref ith 16) (ldev-index date 14)) ;;feature 14 
                                       
                       
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))

))


(defun encode-day-trades2 (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction
;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
     (2 (push (svref record 4) bin-list))

;;;;Feature 3 with 2 levels range-direction date 3
      (3 (push (svref record 5) bin-list))
;;;;Feature 4 with 6 levels is the stochastic with parameter 21
      (4 (push (svref record 6) bin-list)) 
;;;;Feature 5 has 6 levels is range-index1 3
       (5 (push (svref record 7) bin-list))
;;;Feature 6 is mo-diver date 5
       (6 (push (svref record 8) bin-list))
;;;Feature 7 is Gann-slope-index 5 63
       (7 (push (svref record 9) bin-list))
;;;Feature 8 with 6 levels pivot-index
       (8 (push (svref record 10) bin-list))
 ;;Feature 9 is range-index
       (9 (push (svref record 11) bin-list))
;;;Feature 10 with 6 levels 2-bar pivotindex 
       (10  (push (svref record 12) bin-list))
;;; feature 11 with 6 levels is the 3-bar pivot-index 
       (11 (push (svref record 13) bin-list))
;;Feature 12 reflect3 date 3
       (12  (push (svref record 14)  bin-list))
 ;;;;Feature 13 Gann-slope-index 9 63
       (13 (push (svref record 15) bin-list))
;;;;Feature 14 with 5 levels (pivot-index for the previous day)
       (14  (push (svref record 16) bin-list))
                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))
;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-daytrades2b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list2 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-day-trades2 record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
 
    (setf (nth 0 bin) -1) 
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (eql nxdate (svref s1 1))) contents))
 ;        (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
    
    (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
        (incf num-winners-long) (setq gains-long (+ gains-long (svref kth 19))))
    (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short)(setq gains-short (+ gains-short (svref kth 19))))
      (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

        );;;closes dolist over contents
   
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

    (cond ;((and (plusp results-long)(plusp results-short)
          ;    (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
          ;    (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
          ;    (>= (float (/ num-winners-long longs)) crit-acc)
          ;    (>= (float (/ num-winners-short shorts)) crit-acc)
          ; 
          ;     )
          ;    (setq epsignal 'OK))
        ((and (plusp results-long)
               (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
                 (>= (float (/ num-winners-long longs)) crit-acc)
          
                 )
              (setq epsignal 'UP))
        ((and (plusp results-short)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc)
        
                )
              (setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))

#|
;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-daytrades2 (date features)
  (let (record bin (result 0) (counter 0) contents epsignal
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list2 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

    (setq bin (encode-day-trades2 record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
;    (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))

   (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
       (incf num-winners-long))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short))
        );;;closes dolist over contents
  (setq result 0)
  (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

  (cond ((and (plusp results-long)(plusp results-short)) (setq epsignal 'OK))
        ((plusp results-long) (setq epsignal 'UP))
        ((plusp results-short)(setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE)))

  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))
|#

(defun daytrade-simulation-test2 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
       date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk singles vri1 vri2
       (ave-win 0) (ave-loss 0) (losers 0) (winners 0) extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw ignore ;draw-ave draw-90
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary2.dat")))
    ; (declare (ignore long-acc short-acc))
 
    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

     (set-market market) (format T "~%~A~%" *data-name*)
  
   (setq *entry-factor* .3333 *stop-loss-day* 1.0557 
         *max-day-risk* 2000  *min-epic-expected-value* 80
          *commission* 40 *pips-slippage* 0)
   
   (multiple-value-setq (ignore ignore ignore singles) (apply #'day-trade-bins2b features))

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

   

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))

   (setq risk (volatility date 4 *stop-loss-day*))
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1))

   (setq  date-1 date date (add-mkt-days date 1))
  (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover))
    ))



   (setq record (vector date (getd date 'close) 0 0 0))

      (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))
;    (format T "~%~A~%" date)
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades2b date-1 features))
      
       )
;   (format T "~%longs= ~A long-gains = ~A epsignal = ~A" longs long-gains epsignal)
;;;check if new entry

   (when (and 
              (<= (getd date 'low) entry-short)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
               (plusp vri1)(plusp vri2)
                 )
          (setq short  (min entry-short (getd date 'open))
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)

                 )

 ;    (format T "~%shorts = ~A short-gains = ~A risk= ~A entry-long = ~A" shorts short-gains risk entry-long)
    (when (and 
               (>= (getd date 'high) entry-long)
	        (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
               (plusp vri1)(plusp vri2)
                 )
           (setq long  (max entry-long (getd date 'open))
                 long-trade (list date 'long long))
           (setf (svref record 2) 1)

                 )

;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
;                   (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5))    
                   (<= (getd date 'close) stop-long))
;          (<= (getd date 'low) stop-long)
            )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                   (contract-month *data-name* date) 'S stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
        ;   (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- stop-long long)) (calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
 ;                  (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))   
                 (>= (getd date 'close) stop-short))
 ;          (> (getd date 'high) stop-short)    
             )
           (push (round (- (* (my-pretty-price (- short stop-short)) (calculate-point-value date))(comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                   (contract-month *data-name* date) 'S stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                    (round (- (* (my-pretty-price (- short stop-short)) (calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long)) (calculate-point-value date))(comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
         ;   (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                    (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
               (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date)) (comm+slip date)))))
           (setq short-trade nil short nil stop-short nil))

;;;convert (svref record 3) to US dollars
       
      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes

;;;writes the diary file
     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith trading-dates)
         (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))(round (svref ith 4))))
     );;;closes the with-open-file

;;;
;;;


  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win  (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
 ;  (format T "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)

    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
 ;   (format T "~%trades = ~A winners = ~A~%" (list-sum trades) (length winners))
  
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLE RATIO = ~A~%" *stop-loss-day* singles)

      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED-OUT=    ~,1,2,'*,' F% "

       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/ (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
      (/ (count 'S (mapcar #'(Lambda(s1) (svref s1 6)) extended-trades)) 
         (if (zerop (length extended-trades)) 1 (length extended-trades)));;percentage of trades stopped out
         );;;closes format
  ;    (setq ext extended-trades)
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 
 ;   (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
   
       (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/ (list-sum trades) 
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))

;   (multiple-value-setq (draw-ave draw-90)(monte-carlo-drawdown trades))
;       (setq draw-ave (* draw-ave (index-point-value)) draw-90 (* draw-90 (index-point-value)))
;      (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A" draw-ave draw-90) 


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
      (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3)) (svref ith 8))))
        ));;;closes the with-open-file

  );;;closes the when outfile

  (values (round (list-sum trades))
          (length trades) trades)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun day-trade-bins2b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
       (setq bin (encode-day-trades2 record (butlast features ith)))
      
        (cond ((gethash bin *day-trade-warehouse3*)
               (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
               (setf (gethash bin *day-trade-warehouse3*)
                     (cons record (gethash bin *day-trade-warehouse3*)))))
              ((not (gethash bin *day-trade-warehouse3*))
               (setf (gethash bin *day-trade-warehouse3*) (list record))
               (push bin day-bin-codes)))
        ))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)

  ))



(defun day-trade-bins2a ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades2 record features))
    
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
    );;closes the dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))

#|
(defun day-trade-bins2 ( &rest features)
  (let (bin path)

  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades2 record features))
 
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
            (setf (gethash bin *day-trade-warehouse3*)(list record))
            (push bin day-bin-codes)))
)

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))

|#
(defun build-daytrade-warehouse2 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse2.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse2.backup"))
           (delete-file (string-append *upper-dir-warehouse* "daytradewarehouse2.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse2.dat"))
          (rename-file (string-append *upper-dir-warehouse* "daytradewarehouse2.dat")
                            "daytradewarehouse2.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "daytrades2.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


(defun add-day-trades2 (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse2.dat")) trades)

       (if (probe-file path-out)
        (with-open-file (str path-out :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (push record trades))
          ))

     (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (pushnew record trades :test #'equalp)
          ))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


;;;requires a base features list
(defun daytrades-leave-one-out2a (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp)
 

  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp)
                    (apply #'day-trade-bins2a (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))

(defun find-best-indicator-set2a ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)

    (setq base-list '(1 2) candidate-list '(3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (daytrades-add-one-in2a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
     (if (or (not candidate-list)(> result .85)
             (< (fifth (car winners-list)) 1.9)
             (> (fourth (car winners-list)) .20))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      
      (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 3.5)(return))
        (if (> (fifth (second winners-list)) 3.5)(return)))
 
     (setq winners-list (daytrades-leave-one-out2a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    
     (format T "~%Best indicator set = ~A~%" base-list)
   base-list
    ))
 

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun daytrades-add-one-in2a (base-features candidate-features)
  (let (winners-list (result 0) average-profit  single-bins rtb rp csgof)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'day-trade-bins2a (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
    (vsort winners-list #'> 'seventh)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

#|
;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun daytrades-add-one-in2 (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'day-trade-bins2 (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

;;;requires a base features list
(defun daytrades-leave-one-out2 (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'day-trade-bins2a (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;  (setq result (daytrade-chi-squared-gof))
   (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))
|#


(defun find-best-daytrade2 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk  stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long
        action directive1 striker-epic-path2 sol-epic-path2 
         ninja-epic-path2 kingsview-epic-path2 ;rjobrien-epic-path2 
         ts-epic-path2  pacific-exit-time
         pacific-entry-time retail-path2
          pacific-cancel-time vri1 vri2
           pacific-end-session-time oec-symbol oco-code
	  central-exit-time central-entry-time central-cancel-time central-end-session-time
	  (time-zone "CT") offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))

    (setq *entry-factor* .3333  *stop-loss-day* .764 ;1.0557
          *max-day-risk* 2000
           *min-epic-expected-value* 100)
 
    (setq sol-epic-path2 (string-append *daily-output-dir* "sol-epic.csv")
       ;   rjobrien-epic-path2 (string-append *daily-output-dir* "rjobrien-epic.csv")
          retail-path2 (string-append *daily-output-dir* "futures-view.txt")
          striker-epic-path2 (string-append *daily-output-dir* "striker-epic.csv")
          kingsview-epic-path2 (string-append *daily-output-dir* "kingsview-epic.csv")
          ninja-epic-path2 (string-append *ninja-output-dir* "ninja-epic-" (format nil "~A" date1) ".csv")
          ts-epic-path2 (string-append *daily-output-dir* "ts-epic-" (format nil "~A" date1) ".csv")
	  )

   
  (format T "~%EPSIGNAL = ~A longs = ~A  long-gains = ~A ~%"
             epsignal longs long-gains)
  (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains)

   (setq risk  (volatility tdate 4 *stop-loss-day*))
    (setq vri2 (volatility-ratio-index tdate 4 28 1) vri1 (volatility-ratio-index tdate 1 7 1))

   (if (and (member epsignal '(OK UP))(>= (/ long-gains longs) *min-epic-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
            (plusp vri1)(plusp vri2)
            )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))(>= (/ short-gains shorts) *min-epic-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
            (plusp vri1)(plusp vri2)
            )
    (push 'DN trade-direction)(push 'FT trade-direction))

   (multiple-value-setq (entry-short entry-long)(vprices tdate 4 *entry-factor* 1))

   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

 ;           (when (if (member *data-name* '(US.D1B TY.D1B))
 ;                 (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
 ;                                  (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-day-risk*)
 ;                (> (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
 ;                                 (* (index-tick-size) (round entry-short (index-tick-size))))
 ;                                 (index-point-value))) *max-day-risk*)) (push "NOT TODAY" action))

             (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)))
                   ((member 'UP trade-direction) (push "NOT SHORT" action))
                   ((member 'DN trade-direction) (push "NOT LONG" action))
                   (t (push "NOT TODAY" action)))


         (setq action (cond ((member "NOT TODAY" action :test #'equalp) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equalp)
                                  (member "NOT SHORT" action :test #'equalp)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equalp)
                             (incf counter) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equalp)
                              (incf counter) "NOT SHORT")
                            (t (incf counter) "OK      ")))

      (format output " ~A ~%" action )
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~3F~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))(convert-to-decimal (convert-to-32 entry-short)))(index-point-value)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (index-point-value))))
         (format output

            (string-append "~%SELL= " directive1 " STOP= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
            (* (index-tick-size) (round entry-short (index-tick-size)))
            (* (index-tick-size) (round stop-short (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
                          (* (index-tick-size) (round entry-short (index-tick-size)))
                             )  (index-point-value)))
            (* (index-tick-size) (round entry-long (index-tick-size)))
            (* (index-tick-size) (round stop-long (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round entry-long (index-tick-size)))
                          (* (index-tick-size) (round stop-long (index-tick-size)))
                       )  (index-point-value)))
            ))

     (setq
           oec-symbol (make-oec-symbol *data-name* tdate)
           pacific-entry-time "default" ;(second (assoc *data-name* *cannon-market-times-list*)) ;;this is the release time
	   pacific-end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *cannon-market-times-list*)))
           pacific-exit-time  (string-append (date-convert date1) " "
                                     (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) 0))
           pacific-cancel-time (string-append (date-convert date1) " "
                                     (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*))  -26))
	   oco-code (format nil "OSO~A" counter))

      (setq
          ; central-entry-time (string-append (date-convert tdate) " " (second (assoc *data-name* *foremost-market-times-list*))) ;;this is the release time "default"
           central-entry-time "default" ;;this is the release time "default"
	   central-end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *foremost-market-times-list*)))
           central-exit-time (string-append (date-convert date1) " "
                                    (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) 0))
           central-cancel-time (string-append (date-convert date1) " "
                                    (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
     (format T "~%~A  ~A~%" *data-name* action)

     (cond
             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'LONG date1 entry-long stop-long cover-long output1)
              (when (member *data-name* *epic-list*)
                 (setq offset (random-choice -1 0)
                     pacific-exit-time (string-append (date-convert date1) " " 
                                            (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*))
                                            offset T)))
 ;                 (write-oec-long  cannon-epic-path2 oec-symbol *cannon-epic-block-acct* *cannon-epic-qty* entry-long stop-long
 ;                                  pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
  
              (with-open-file (ninja-output ts-epic-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-epic-qty*))
             
              (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " " 
                                             (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

               (write-oec-long kingsview-epic-path2 oec-symbol *kingsview-epic-block-acct* *kingsview-epic-qty*
                               entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)


              (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " "
                                             (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

     ;         (write-oec-long rjobrien-epic-path2 oec-symbol *rjobrien-epic-block-acct* *rjobrien-epic-qty*
     ;                          entry-long stop-long central-entry-time
      ;                         central-cancel-time central-exit-time central-end-session-time oco-code)

              (setq offset (random-choice -1 0)
               central-exit-time  (string-append (date-convert date1) " "
                                         (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

               (write-oec-long striker-epic-path2 oec-symbol *striker-epic-block-acct* *striker-epic-qty* 
                               entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)
               (when (member *data-name* (set-difference *epic-list* *ice-list*))
                     (write-oec-long sol-epic-path2 oec-symbol *sol-epic-block-acct* *sol-epic-qty* 
                                     entry-long stop-long central-entry-time
                                     central-cancel-time central-exit-time central-end-session-time oco-code)
                      )

               (with-open-file (ninja-output ninja-epic-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ninja-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long))
               
                (write-futures-view retail-path2 tdate 'BUY entry-long stop-long)
             );closes the when
               )


             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'SHORT date1 entry-short stop-short cover-short output1)

              (when (member *data-name* *epic-list*)
                (setq offset (random-choice -1 0)
                      pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*))
                                  offset T)))
   ;              (write-oec-short  cannon-epic-path2 oec-symbol *cannon-epic-block-acct* *cannon-epic-qty* entry-short stop-short
   ;                                pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

                  (setq offset (random-choice -1 0)
                       central-exit-time  (string-append (date-convert date1) " "
                      (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*))
                                  offset T)))

               (with-open-file (ninja-output ts-epic-path2 :direction :output :if-exists :append :if-does-not-exist :create)
                 	     (write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short *striker-epic-qty*))
            
                (write-oec-short kingsview-epic-path2 oec-symbol *kingsview-epic-block-acct* *kingsview-epic-qty*
                               entry-short stop-short central-entry-time 
                               central-cancel-time central-exit-time central-end-session-time oco-code)


               (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " "
                                             (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
          
    ;          (write-oec-short rjobrien-epic-path2 oec-symbol *rjobrien-epic-block-acct* *rjobrien-epic-qty*
    ;                           entry-short stop-short central-entry-time 
    ;                           central-cancel-time central-exit-time central-end-session-time oco-code)

               (setq offset (random-choice -1 0)
                     central-exit-time (string-append (date-convert date1) " "
                                                     (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*))
                                   offset T)))
                (write-oec-short  striker-epic-path2 oec-symbol *striker-epic-block-acct* *striker-epic-qty* entry-short stop-short
                                   central-entry-time central-cancel-time central-exit-time central-end-session-time oco-code)
               (when (member *data-name* (set-difference *epic-list* *ice-list*))
                 (write-oec-short  sol-epic-path2 oec-symbol *sol-epic-block-acct* *sol-epic-qty* entry-short stop-short
                                   central-entry-time central-cancel-time central-exit-time central-end-session-time oco-code)
                  )
               (with-open-file (ninja-output ninja-epic-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	                	 (write-ninja-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
               (write-futures-view retail-path2 tdate 'SELL entry-short stop-short)
              );;closes the when
               ));;closes clause the cond

      )) ;;;closes the let and the defun


;;;
(defun display-num-trades-by-market2 (daytrades)
   (let (training-data markets-list num-trades first-date last-date
         (path (string-append *output-upper-dir* "market-trade-data2.dat"))
      )
    (setq training-data (num-markets-in-warehouse3 daytrades))
    (dolist (ith training-data)
      (multiple-value-setq (num-trades first-date last-date )(num-trades-in-warehouse3 ith daytrades))
      (setq markets-list
           (cons (list ith (gain-loss-trades-in-warehouse3 ith daytrades) num-trades first-date last-date) markets-list))
       );;closes the dolist
    (vsort markets-list #'> 'second)
   ; (vsort markets-list #'< #'(lambda(s)(first-char-code (car s))))
    (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "Market    #trades  Sum $P&L Ave $P&L    FIRST     LAST~%")
    (dolist (jth markets-list)
     (format str"~%~10A ~4D ~10D   ~4D    ~A  ~A"
     (car jth) (third jth)(second jth) (round (/ (second jth)(third jth)))(fourth jth)(fifth jth))))
  ));;closes the let and defun.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun update-currency-warehouse2 (date &optional (markets  *epicc-warehouse-list* ))

  (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-currency-trades2 ith date (available-days ith date 500)))
 
   (build-currency-warehouse2 markets) 
   (setq *day-features2c* (find-best-indicator-set2c))
   (portfolio-simulation3 '(epicc) date 4050 (list *epicc-warehouse-list*));(list *epicc-list*))

  ; (portfolio-simulation4 '(forexday) date 3500 (list markets))
)


(defun populate-currency-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short date-1 cover-long cover-short entry-long entry-short risk vri1 vri2
        eprc310 rri5 rri10 rri21 can0 can1 can2
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "currencytrades2.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

   (format outfile "~%~A~%" market)
   (setq *entry-factor* .3333 *stop-loss-day* .994 ;1.0557
        *max-day-risk* 3000) 
;   (setq *entry-factor* .764 *stop-loss-day* .944) ;;;;1.0557)
   (setq *commission* 40 *pips-slippage* 0)

    (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
 
  (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
 
;   (setq risk (min (volatility date 4 *stop-loss-day*)(/ *max-day-risk* (index-point-value))))
   (setq risk (volatility date 4 *stop-loss-day*))
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1)
         rri5 (roc-rel-index5 date) rri10 (ep-roc-index date 10) rri21 (roc-rel-index21 date)
         can0 (candle-index date) can1 (candle-index-1 date) can2 (candle-index-2 date)
         eprc310 (ep-roc-change-index date 3 10))
   (setq  date-1 date date (add-mkt-days date 1))
   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover))
    ))
    
    (setq stop-long (- (max entry-long (getd date 'open)) risk)
         stop-short (+ (min entry-short (getd date 'open)) risk));;;needs to be after the rollover

   (when (and (<= (getd date 'low) entry-short)
              (>= (* risk (calculate-point-value date))
                  (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))

               (>= vri1 -3) (>= vri2 0)
               (/= rri5 4)(/= rri5 -4)(/= rri5 3)(/= rri5 -3)
               (/= rri10 4)(/= rri10 -4)(/= rri10 3)
               (<= rri21 4)(>= eprc310 -2)
               (not (member can0 '(3 1)))(not (member can1 '(3 1)))(not (member can2 '(3 1)))
              )
          (setq short  (min entry-short (getd date 'open))
                short-trade (create-currency-entry-record-list2 date-1 -1 short)
                 ))

    (when (and (>= (getd date 'high) entry-long)
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))

               (>= vri1 -3) (>= vri2 0)
               (/= rri5 4)(/= rri5 -4)(/= rri5 3)(/= rri5 -3)
               (/= rri10 4)(/= rri10 -4)(/= rri10 3)
               (<= rri21 4)(>= eprc310 -2)
               (not (member can0 '(-3 -1)))(not (member can1 '(-3 -1)))(not (member can2 '(-3 -1)))
                )
           (setq long (max entry-long (getd date 'open))
                 long-trade (create-currency-entry-record-list2 date-1 1 long)
                 ))
;;;check if stopped out on same day

   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
    ;          (<= (getd date 'low) stop-long)     
          )
           (push (round (- (* (my-pretty-price (- stop-long long)) (calculate-point-value date))
                              (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                 (round (* (calculate-point-value date)(my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil)
           )

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
          ;     (>= (getd date 'high) stop-short)
                    )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                              (round (* (calculate-point-value date)(my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil)
           )

;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                            (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                              (round (* (calculate-point-value date)(my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                              (round (* (calculate-point-value date)(my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes

 
  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win (/  (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))  (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (drawdown trades))
      ; (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
       )

     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
       (format stream "~S~%" ith)
    ));;closes the dolist and with-open-file

  );;;closes the when outfile
  (values (round (list-sum trades))
       (length trades))
 ));


;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-currency-entry-record-list2 (date direction entry)
   (let* ( );(date-1 (getd date 'ydate)) )
    
     ; (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                             (sector-index *data-name*);;;feature 2
                             (volatility-ratio-index date 4 28 1);;feature 3  
                             (macd-index1 date 6 15 3 );;;feature 4
                             (candle-index date) ;;;feature 5
                             (roc-rel-index5  date) ;feature 6
                             (timing-line-index5 date 2 5)      ;;  feature 7             
                             (candle-index-1 date) ;; feature 8      
                            
                             (slow-stochastic-index date 21) ;feature 9
                             (candle-index-2 date) ;;;feature 10
 
                             (ldev-index date 9) ;;;feature 11
                             (reflect3 date 14);;;feature 12
                           
                             (ldev-index date 5) ;feature 13
                             (ep-macd-index date 12 26 9 )  ;feature 14
                                                    
                              );;;closes the list

))

;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-currencytrade-indicators2 ()
   (let ((path (string-append *upper-dir-warehouse* "currencywarehouse2.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))

      (setf (svref ith 6) (macd-index1 date-1 6 15 3));;feature 4
      (setf (svref ith 16)(ep-macd-index date-1 12 26 9)) ;;feature 14
     ; (setf (svref ith 12) (candle-index-2 date-1 ));;feature 10
     ; (setf (svref ith 11)(slow-stochastic-index date-1 21)) ;;feature 9
     ; (setf (svref ith 11) (ldev-index date-1 9)) ;;;feature 11
     ; (setf (svref ith 14)(reflect3 date-1 14));;feature 12
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))

))


(defun currency-trade-bins2a ( &rest features)
  (let (bin path)

  ; (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "currencytrades2.dat"))
   (setq path (string-append *upper-dir-warehouse*  "currencywarehouse2.dat"))
     (maind-x)(set-cat-list)
   (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-currency-trades2 record features))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
      );;closes the dolist
    
    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))

(defun currency-trade-bins2b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "currencywarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
       (setq bin (encode-currency-trades2 record (butlast features ith)))
      
        (cond ((gethash bin *day-trade-warehouse3*)
               (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
               (setf (gethash bin *day-trade-warehouse3*)
                     (cons record (gethash bin *day-trade-warehouse3*)))))
              ((not (gethash bin *day-trade-warehouse3*))
               (setf (gethash bin *day-trade-warehouse3*) (list record))
               (push bin day-bin-codes)))
        ))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)

  ))



(defun encode-currency-trades2 (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction
;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
     (2 (push (svref record 4) bin-list))

;;;;Feature 3 with 2 levels range-direction date 3
      (3 (push (svref record 5) bin-list))
;;;;Feature 4 with 6 levels is the stochastic with parameter 21
      (4 (push (svref record 6) bin-list)) 
;;;;Feature 5 has 6 levels is range-index1 3
       (5 (push (svref record 7) bin-list))
;;;Feature 6 is mo-diver date 5
       (6 (push (svref record 8) bin-list))
;;;Feature 7 is Gann-slope-index 5 63
       (7 (push (svref record 9) bin-list))
;;;Feature 8 with 6 levels pivot-index
       (8 (push (svref record 10) bin-list))
;;;Feature 9 is range-index
       (9 (push (svref record 11) bin-list))
;;;Feature 10 with 6 levels 2-bar pivotindex 
       (10  (push (svref record 12) bin-list))
;;; feature 11 with 6 levels is the 3-bar pivot-index 
       (11 (push (svref record 13) bin-list))
;;Feature 12 reflect3 date 3
       (12  (push (svref record 14)  bin-list))
 ;;;;Feature 13 Gann-slope-index 9 63
       (13 (push (svref record 15) bin-list))
;;;;Feature 14 with 5 levels (pivot-index for the previous day)
       (14  (push (svref record 16) bin-list))
                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))


(defun find-best-indicator-set2c ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1 2) candidate-list '(3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (currency-add-one-in2a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)(> result .85)
             (< (fifth (car winners-list)) 1.9)
             (> (fourth (car winners-list)) .20))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list);

   (loop     
      
       (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 2.9)(return))
        (if (> (fifth (second winners-list)) 2.9)(return)));

       (setq winners-list (currency-leave-one-out2a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    
     (format T "~%Best indicator set = ~A~%" base-list)
    base-list
    ))
 

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun currency-add-one-in2a (base-features candidate-features)
  (let (winners-list (result 0) average-profit rtb single-bins rp csgof)

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'currency-trade-bins2a (append base-features (list ith))))
     (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
   (vsort winners-list #'> 'seventh)
;   (if (zerop (sixth (first winners-list)))
 ;     (vsort winners-list #'> 'third))

   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))



;;;requires a base features list
(defun currency-leave-one-out2a (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp)
                    (apply #'currency-trade-bins2a (remove ith base-features)))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))



;;;
(defun currency-simulation-test2 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk vri1 vri2
        eprc310 rri5 rri10 rri21 can0 can1 can2
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw  unfiltered-trades
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "epicc-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "epicc-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "epicc-diary2.dat")))
;   (declare (ignore long-acc short-acc))

     (if (and num (> num (available-days market date2)))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

   (set-market market) (format T "~%~A~%" *data-name*)
   
   (setq *entry-factor* .3333 *stop-loss-day* .994 
          *max-day-risk* 2500 *min-epic-expected-value* 80 
          *commission* 40 *pips-slippage* 0)
 
     (apply #'currency-trade-bins2b features) 
   
   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))

   (setq risk (volatility date 4 *stop-loss-day*))
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1)
         rri5 (roc-rel-index5 date) rri10 (ep-roc-index date 10) rri21 (roc-rel-index21 date)
         can0 (candle-index date) can1 (candle-index-1 date) can2 (candle-index-2 date)
         eprc310 (ep-roc-change-index date 3 10))

   (setq  date-1 date date (add-mkt-days date 1))
   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover))
    ))


   (setq record (vector date (getd date 'close) 0 0 0))

    (setq stop-long (- (max entry-long (getd date 'open)) risk)
          stop-short (+ (min entry-short (getd date 'open)) risk))

;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-currencytrades2b date-1 features))
              )

;;;check if new entry

   (when (and (<= (getd date 'low) entry-short)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
              (>= vri1 -3) (>= vri2 0)
              (/= rri5 4)(/= rri5 -4)(/= rri5 3)(/= rri5 -3)
              (/= rri10 4)(/= rri10 -4)(/= rri10 3)
              (<= rri21 4)(>= eprc310 -2)
              (not (member can0 '(3 1)))(not (member can1 '(3 1)))(not (member can2 '(3 1)))
               )
          (setq short  (min entry-short (getd date 'open))
                short-trade (list date 'short short)

                )
           (setf (svref record 2) -1)

                 )


    (when (and (>= (getd date 'high) entry-long)
	       (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
               (>= vri1 -3) (>= vri2 0)
               (/= rri5 4)(/= rri5 -4)(/= rri5 3)(/= rri5 -3)
               (/= rri10 4)(/= rri10 -4)(/= rri10 3)
               (<= rri21 4) (>= eprc310 -2)
               (not (member can0 '(-3 -1)))(not (member can1 '(-3 -1)))(not (member can2 '(-3 -1)))
               )

           (setq long (max entry-long (getd date 'open))
                 long-trade (list date 'long long)

                 )
           (setf (svref record 2) 1)
          
            )

;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
;               (<= (getd date 'low) stop-long)
           )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'S
                                                          stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
     ;      (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                              (comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
;               (>= (getd date 'high) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                           stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades) 
            (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
         ;   (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                   (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                             (comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                                              (comm+slip date)))))
           (setq short-trade nil short nil stop-short nil))


      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes


     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
           (round (svref ith 4)))
     ))


  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
;   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F ~%" *stop-loss-day* )

    (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D  "

       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/  (list-sum trades)
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))


    (setq unfiltered-trades (trim-trades daytrades *data-name* (add-mkt-days date2 (- num)) date2))
     (display-unfiltered-trades unfiltered-trades str) 

   ;  (multiple-value-setq (draw-ave draw-90 draw-99)(monte-carlo-drawdown trades))
     
   ;  (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A~%99 PERCENTILE DRAWDOWN= ~A~%" 
   ;      (round draw-ave) (round draw-90) (round draw-99)) 


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
      (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 0)) (svref ith 8))))
        ));;;closes the with-open-file

  );;;closes the when outfile

  (values (round (list-sum trades))
          (length trades) trades)
   ))

(defun build-currency-warehouse2 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "currencywarehouse2.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "currencywarehouse2.backup"))
           (delete-file (string-append *upper-dir-warehouse* "currencywarehouse2.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "currencywarehouse2.dat"))
          (rename-file (string-append *upper-dir-warehouse* "currencywarehouse2.dat")
                            "currencywarehouse2.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "currencytrades2.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun



(defun find-best-currencytrade2 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk vri1 vri2 rri5 rri10 rri21 stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long eprc310 can0 can1 can2
        action directive1 offset (time-zone "CT")
       ; ninja-forex-path2
        pacific-exit-time pacific-entry-time pacific-cancel-time pacific-end-session-time oec-symbol
        oco-code retail-path2 sol-score-path3 
        striker-score-path3   striker-starter-path3   kingsview-starter-path3 ts-score-path3
        central-exit-time central-entry-time central-cancel-time central-end-session-time
	;  (time-zone "CT")
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))
    (format T "~%~A~%" *data-name*)
    (setq *entry-factor* .3333 *stop-loss-day* 1.0557 *max-day-risk* 2500
          *min-epic-expected-value* 80
          *commission* 40 *pips-slippage* 0)

    (setq sol-score-path3 (string-append *daily-output-dir* "sol-score.csv")
          
          kingsview-starter-path3 (string-append *daily-output-dir* "kingsview-starter.csv")
          striker-score-path3 (string-append *daily-output-dir* "striker-score.csv")
          striker-starter-path3 (string-append *daily-output-dir* "striker-starter.csv")
         ; cannon-score-path3 (string-append *daily-output-dir* "cannon-score.csv")
         ; cannon-starter-path3 (string-append *daily-output-dir* "cannon-starter.csv")
          retail-path2 (string-append *daily-output-dir* "futures-view.txt")
          ts-score-path3 (string-append *daily-output-dir* "ts-triumph21-" (format nil "~A" date1) ".csv")
         )
  
     
     (setq vri2 (volatility-ratio-index tdate 4 28 1) vri1 (volatility-ratio-index tdate 1 7 1)
         rri5 (roc-rel-index5 tdate) rri10 (ep-roc-index tdate 10) rri21 (roc-rel-index21 tdate)
            can0 (candle-index tdate) can1 (candle-index-1 tdate) can2 (candle-index-2 tdate)
         eprc310 (ep-roc-change-index tdate 3 10))

   (format T "~%EPSIGNAL = ~A  longs = ~A  long-gains = ~A~%"
            epsignal  longs long-gains)
  (format T "  shorts = ~A  short-gains = ~A~%" shorts short-gains )

   (setq risk (volatility tdate 4 *stop-loss-day*))
   
   (if (and (member epsignal '(OK UP))
            (>= (/ long-gains longs) *min-epic-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
            (>= vri1 -3) (>= vri2 0)
            (/= rri5 4)(/= rri5 -4)(/= rri5 3)(/= rri5 -3)
            (/= rri10 4)(/= rri10 -4)(/= rri10 3)
            (<= rri21 4)(>= eprc310 -2)
            (not (member can0 '(3 1)))(not (member can1 '(3 1)))(not (member can2 '(3 1)))
            )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))(> shorts 0)
            (>= (/ short-gains shorts) *min-epic-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
            (>= vri1 -3) (>= vri2 0)
            (/= rri5 4)(/= rri5 -4)(/= rri5 3)(/= rri5 -3)
            (/= rri10 4)(/= rri10 -4)(/= rri10 3)
            (<= rri21 4)(>= eprc310 -2)
            (not (member can0 '(-3 -1)))(not (member can1 '(-3 -1)))(not (member can2 '(-3 -1)))
             )
    (push 'DN trade-direction)(push 'FT trade-direction))
 ; (format T "~%~A 222 ~%" *data-name*)

   (multiple-value-setq (entry-short entry-long)(vprices tdate 4 *entry-factor* 1))

   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

      ;      (when (if (member *data-name* '(US.D1B TY.D1B))
      ;            (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
      ;                             (convert-to-decimal (convert-to-32 stop-long))) (calculate-point-value tdate)))
      ;                  *max-day-risk*)
      ;           (> (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
      ;                            (* (index-tick-size) (round entry-short (index-tick-size))))
      ;                            (calculate-point-value tdate))) *max-day-risk*)) (push "NOT TODAY" action))

             (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)))
                   ((member 'UP trade-direction) (push "NOT SHORT" action))
                   ((member 'DN trade-direction) (push "NOT LONG" action))
                   (t (push "NOT TODAY" action)))



         (setq action (cond ((member "NOT TODAY" action :test #'equalp) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equalp)
                                  (member "NOT SHORT" action :test #'equalp)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equalp)
                             (incf counter) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equalp)
                              (incf counter) "NOT SHORT")
                            (t (incf counter) "OK      ")))

      (format output " ~A " action )
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~3F~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))(convert-to-decimal (convert-to-32 entry-short)))(index-point-value)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (index-point-value))))
         (format output

            (string-append "~%SELL= " directive1 " STOP= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
            (* (index-tick-size) (round entry-short (index-tick-size)))
            (* (index-tick-size) (round stop-short (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
                          (* (index-tick-size) (round entry-short (index-tick-size)))
                             )  (calculate-point-value tdate)))
            (* (index-tick-size) (round entry-long (index-tick-size)))
            (* (index-tick-size) (round stop-long (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round entry-long (index-tick-size)))
                          (* (index-tick-size) (round stop-long (index-tick-size)))
                       )  (calculate-point-value tdate)))
            ))
  (setq
           oec-symbol (make-oec-symbol *data-name* tdate)
           pacific-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
	   pacific-end-session-time "default" ;  (string-append (date-convert date1) " " (third (assoc *data-name* *cannon-market-times-list*)))
   ;        pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) 0))
           pacific-cancel-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
   
   ;  (setq
   ;        oec-symbol (make-oec-symbol *data-name* tdate)
   ;        entry-time "default" ;(second (assoc *data-name* *cannon-market-times-list*)) ;;this is the release time
   ;        end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *cannon-market-times-list*)))
   ;        exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) 0))
   ;        cancel-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*))  -28));
;	   oco-code (format nil "OSO~A" counter))

  ; (format T "~% ~A 333~%" *data-name*)
      (setq

        ;   central-entry-time (string-append (date-convert tdate) " " (second (assoc *data-name* *foremost-market-times-list*))) ;;this is the release time "default"
            central-entry-time "default" ;;this is the release time "default"
    	   central-end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *foremost-market-times-list*)))
           central-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) 0))
           central-cancel-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -28))
	   oco-code (format nil "OSO~A" counter))
      (format T "~%~A  ~A~%" *data-name* action)

     (cond  ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'LONG date1 entry-long stop-long cover-long output1)
              (setq offset (random-choice -1 0) pacific-exit-time (string-append (date-convert date1) " "
                                                          (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
;               (write-oec-long  cannon-score-path3 oec-symbol *cannon-score-block-acct* *cannon-score-qty*
;                                 entry-long stop-long pacific-entry-time
;                                pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
               (setq offset (random-choice -1 0)
                   central-exit-time  (string-append (date-convert date1) " " 
                                                (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
               (write-oec-long striker-score-path3 oec-symbol *striker-score-block-acct* *striker-score-qty*
                               entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)
               (with-open-file (ninja-output ts-score-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-score-qty*))
         
               (when (member *data-name* (set-difference *epicc-list* *ice-list*)) 
                      (write-oec-long sol-score-path3 oec-symbol *sol-score-block-acct* *sol-score-qty*
                               entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)
                  )
               (setq offset (random-choice -1 0) pacific-exit-time (string-append (date-convert date1) " "
                                                          (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
             
     ;          (write-oec-long  cannon-starter-path3 oec-symbol *cannon-starter-block-acct* *cannon-starter-qty*
     ;                            entry-long stop-long pacific-entry-time
     ;                           pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
     ;          (write-oec-long  kingsview-starter-path3 oec-symbol *kingsview-starter-block-acct* *kingsview-starter-qty*
     ;                            entry-long stop-long central-entry-time
     ;                           central-cancel-time central-exit-time central-end-session-time oco-code)
     ;          (setq offset (random-choice -1 0)
     ;              central-exit-time  (string-append (date-convert date1) " " 
     ;                                           (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
     ;          (write-oec-long striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty*
     ;                          entry-long stop-long central-entry-time
     ;                          central-cancel-time central-exit-time central-end-session-time oco-code);
                
               (write-futures-view retail-path2 tdate 'BUY entry-long stop-long)

               )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'SHORT date1 entry-short stop-short cover-short output1)
              (setq offset (random-choice -1 0)
                    pacific-exit-time (string-append (date-convert date1) " "
                                                 (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
  ;            (write-oec-short cannon-score-path3 oec-symbol *cannon-score-block-acct* *cannon-score-qty* 
  ;                             entry-short stop-short pacific-entry-time
  ;                             pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
    
               (setq offset (random-choice -1 0)
                     central-exit-time  (string-append (date-convert date1) " "
                                           (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
               (write-oec-short striker-score-path3 oec-symbol *striker-score-block-acct* *striker-score-qty*
                                entry-short stop-short central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)
               (with-open-file (ninja-output ts-score-path3 :direction :output :if-exists :append :if-does-not-exist :create)
                 	     (write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short *striker-score-qty*))
            
               (when (member *data-name* (set-difference *epicc-list* *ice-list*)) 
                     (write-oec-short sol-score-path3 oec-symbol *sol-score-block-acct* *sol-score-qty*
                                entry-short stop-short central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)
                   )
       ;         (write-oec-short striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty*
       ;                         entry-short stop-short central-entry-time
       ;                         central-cancel-time central-exit-time central-end-session-time oco-code)
       ;         (write-oec-short kingsview-starter-path3 oec-symbol *kingsview-starter-block-acct* *kingsview-starter-qty*
       ;                         entry-short stop-short central-entry-time
       ;                         central-cancel-time central-exit-time central-end-session-time oco-code)
                (write-futures-view retail-path2 tdate 'SELL entry-short stop-short)

               ));;closes clause the cond


      )) ;;;closes the let and the defun


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-currencytrades2b (date-1 features)
  (let (record bin (result 0) (counter 0) contents epsignal ;(crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date-1 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-currency-entry-record-list2 date-1 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
      (setq bin (encode-currency-trades2 record features))
      (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
   
      (setf (nth 0 bin) -1) 
      (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
      (setq contents
           (remove-if #'(lambda(s1) (eql nxdate (svref s1 1))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
       (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
    
    (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
        (incf num-winners-long) (setq gains-long (+ gains-long (svref kth 19))))
    (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short)(setq gains-short (+ gains-short (svref kth 19))))
      (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

        );;;closes dolist over contents
   
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

    (cond ((and (plusp results-long)(plusp results-short)
           ;   (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
           ;   (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
           ;   (>= (float (/ num-winners-long longs)) crit-acc)
          
           
             )  
            (setq epsignal 'OK))
        ((and (plusp results-long)
          ;     (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
          ;     (>= (float (/ num-winners-long longs)) crit-acc)
        
                 )
               (setq epsignal 'UP))
        ((and (plusp results-short)
           ;     (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
           ;   (>= (float (/ num-winners-short shorts)) crit-acc)
         
                )
                (setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Retail product;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;this simulates the retail product
;;;uses the same warehouse as the epic
(defun daytrade-simulation-test2r (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk 
       (ave-win 0) (ave-loss 0) (losers 0) (winners 0) extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc   bin draw ;draw-ave draw-90

       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary2.dat")))
 

    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

    
     (set-market market) (format T "~%~A~%" *data-name*)

   (setq *entry-factor* .3333 *stop-loss-day* 1.0557
          *max-day-risk* 2500
          *commission* 40 *pips-slippage* 0)
   
   (apply #'day-trade-bins2b features)

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

   (if (member market *forex-list*)
       (setq *min-epic-expected-value* (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
      (setq *min-epic-expected-value* 80))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))

    (setq entry-long (+ entry-long (* (1+ (random 6)) (index-tick-size)))
          entry-short  (- entry-short (* (1+ (random 6)) (index-tick-size))))  

   (setq risk (volatility date 4 *stop-loss-day*))  
   

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
    			              entry-short (+ entry-short (getd date 'rollover))))

    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

    (setq stop-long (- stop-long (* (1+ (random 6)) (index-tick-size)))
          stop-short  (+ stop-short (* (1+ (random 6)) (index-tick-size))))

;    (format T "~%~A~%" date)
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades2b date-1 features))
      
       )
;   (format T "~%longs= ~A long-gains = ~A epsignal = ~A" longs  long-gains epsignal)
;;;check if new entry

   (when (and 
              (<= (getd date 'low) entry-short)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
              
                 )
          (setq short  (min entry-short (getd date 'open))
                short-trade (list date 'short short)

                )
           (setf (svref record 2) -1)
   
                 )

 ;    (format T "~%shorts = ~A short-gains = ~A risk= ~A entry-long = ~A" shorts short-gains risk entry-long)
    (when (and 
               (>= (getd date 'high) entry-long)
	        (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
              
               )

           (setq long  (max entry-long (getd date 'open))
                 long-trade (list date 'long long)

                 )
           (setf (svref record 2) 1)
   
                 )

;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
;                   (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5))    
                   (<= (getd date 'close) stop-long))
;          (<= (getd date 'low) stop-long)
            )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'S stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
              (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
 ;                  (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))   
                 (>= (getd date 'close) stop-short))
 ;        (> (getd date 'high) stop-short)    
             )
           (push (round (- (* (my-pretty-price (- short stop-short)) (calculate-point-value date))
                           (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                           (contract-month *data-name* date) 'S stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                (round  (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))



;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                            (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                         (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil))


      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes


     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith trading-dates)
          (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
           (round (svref ith 4))))
     );;;closes the with-open-file

;;;
;;;


  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win  (/ (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
 ;  (format T "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)

    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
 ;   (format T "~%trades = ~A winners = ~A~%" (list-sum trades) (length winners))
  
      (format str "NUM TRADES IN WAREHOUSE= ~A~%" (length daytrades))
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED-OUT=    ~,1,2,'*,' F% "

       (round  (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
      (/ (count 'S (mapcar #'(Lambda(s1) (svref s1 6)) extended-trades)) 
         (if (zerop (length extended-trades)) 1 (length extended-trades)));;percentage of trades stopped out
         )
  ;    (setq ext extended-trades)
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 
 ;   (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
   
       (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/  (list-sum trades) 
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))

;   (multiple-value-setq (draw-ave draw-90)(monte-carlo-drawdown trades))
;       (setq draw-ave (* draw-ave (index-point-value)) draw-90 (* draw-90 (index-point-value)))
;      (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A" draw-ave draw-90) 


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
      (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3)) (svref ith 8))))
         ));;;closes the with-open-file

  );;;closes the when outfile

  (values (round (list-sum trades))
          (length trades) trades)
   ))

;;;--------------------------------------------------------------------------------------------------------
;;;Epic for Dubai futures markets


(defun update-dubai-warehouse2 (date &optional (markets *dubai-warehouse-list*))

   (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-dubai-trades2 ith date (available-days ith date 400)))
 
   (build-dubaitrade-warehouse2 markets) 
   (setq *dubai-features2* (find-best-dubai-indicator-set2a))
   (portfolio-simulation3 '(dubai) date 756 (list *dubai-list*))
)



(defun build-dubaitrade-warehouse2 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "dubaitradewarehouse2.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "dubaitradewarehouse2.backup"))
           (delete-file (string-append *upper-dir-warehouse* "dubaitradewarehouse2.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "dubaitradewarehouse2.dat"))
          (rename-file (string-append *upper-dir-warehouse* "dubaitradewarehouse2.dat")
                            "dubaitradewarehouse2.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "dubaitrades2.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


(defun populate-dubai-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 cover-long cover-short entry-long entry-short risk vri1 vri2
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "dubaitrades2.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

  (format outfile "~%~A~%" market)


   (setq *entry-factor* .3333 *stop-loss-day* 1.0557
         *max-day-risk* 2500 *commission* 10 *pips-slippage* 0)
 
   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

;   (format T "~A ~A~%" date (getd date 'close))
  (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
 
  (setq entry-short (- entry-short (* 3.0 (index-tick-size))) entry-long (+ entry-long (* 3.0 (index-tick-size))))
   
   (setq risk (volatility date 4 *stop-loss-day*) risk (+ risk (* 3.0 (index-tick-size))))
   
    (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1)) 

   (setq  date-1 date date (add-mkt-days date 1))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover))
   ))

    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk));;;needs to be after the rollover
;;;
;;;

   (when (and (<= (- (getd date 'low) (* 3 (index-tick-size))) entry-short)
             
              (>= (* risk (calculate-point-value date))
                  (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
	      (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (plusp vri2)(plusp vri1)
              )
          (setq short  (min entry-short (getd date 'open))
                short-trade (create-dubaitrade-entry-record-list2 date-1 -1 short)
                 ))

    (when (and (>= (+ (getd date 'high) (* 3 (index-tick-size))) entry-long)
             
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size)))
               (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (plusp vri2)(plusp vri1)
                )
           (setq long (max entry-long (getd date 'open))
                 long-trade (create-dubaitrade-entry-record-list2 date-1 1 long)
                 ))

;;;check if stopped out on same day

   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
              ;     (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5)) 
                  (<= (getd date 'close) stop-long))
             ;  (<= (- (getd date 'low) (* 3 (index-tick-size))) stop-long)
               )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                 (round (* (calculate-point-value date)(my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short) 
                        (<= (getd date 'open) (getd date 'close)))
              ;     (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))      
                    (>= (getd date 'close) stop-short))
         ;          (>= (+ (getd date 'high)(* 3 (index-tick-size))) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                  (round (* (calculate-point-value date)(my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (- (max (getd date 'close) stop-long)(* 3 (index-tick-size))))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                   (round (* (calculate-point-value date) (my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (+ (min (getd date 'close) stop-short)(* 3 (index-tick-size))))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                 (round (* (calculate-point-value date)(my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes

  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (list-sum winners)
                            (if (zerop (length winners)) 1 (length winners)))
         ave-loss   (/ (list-sum losers) 
                            (if (zerop (length losers)) 1 (length losers)))
         )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (drawdown trades))
      ; (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
       )

     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
       (format stream "~S~%" ith)
    ));;closes the dolist and with-open-file

  );;;closes the when outfile
  (values (round (list-sum trades))
       (length trades))
 ));


(defun dubai-trade-bins2a ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "dubaitradewarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades2 record features))
    
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
    );;closes the dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))


(defun dubai-trade-bins2b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "dubaitradewarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
       (setq bin (encode-day-trades2 record (butlast features ith)))
      
        (cond ((gethash bin *day-trade-warehouse3*)
               (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
               (setf (gethash bin *day-trade-warehouse3*)
                     (cons record (gethash bin *day-trade-warehouse3*)))))
              ((not (gethash bin *day-trade-warehouse3*))
               (setf (gethash bin *day-trade-warehouse3*) (list record))
               (push bin day-bin-codes)))
        ))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)

  ))
;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-dubaitrade-entry-record-list2 (date direction entry)
   (let* ((date-1 (getd date 'ydate)))
     ;  (multiple-value-setq (csignal period) (cycle-signal date 10 20))
      ; (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))

                              (body-range-index date);;feature 2
                              (body-range-index date-1);;feature 3
                              (body-range-index (getd date-1 'ydate)) ;;features 4

                              (r-squared-change-index date 5 1);;feature 5  
                              (r-squared-change-index date-1 5 1);;;feature 6  10 levels
                              (r-squared-change-index (getd date-1 'ydate) 5 1);;;feature 7
                          
                              (r-squared-change-index date 10 2) ;;;feature 8
                              (r-squared-change-index date 20 4) ;;;feature 9
                              (r-squared-change-index date 40 8) ;;;feature 10
                              (r-squared-change-index date 80 16) ;;;feature 11
                              (r-squared-change-index date 160 32) ;;;feature 12
                 
                              (volatility-ratio-index date 1 5 1) ;; feature 13
                              (tkcd-index date) ;;;feature 8
                              );;;closes the list

))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-dubaitrades2b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-dubaitrade-entry-record-list2 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-day-trades2 record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
  
    (setf (nth 0 bin) -1) 
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades with the same next day
   (setq contents
    (remove-if #'(lambda (s1) (eql nxdate (svref s1 1))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
    
    (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
        (incf num-winners-long) (setq gains-long (+ gains-long (svref kth 19))))
    (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short)(setq gains-short (+ gains-short (svref kth 19))))
      (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

        );;;closes dolist over contents
   
      (setq result 0)
     (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

    (cond ((and (plusp results-long)(plusp results-short)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc)
         
             ) 
            (setq epsignal 'OK))
        ((and (plusp results-long)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)
            
                )
              (setq epsignal 'UP))
        ((and (plusp results-short)
               (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
               (>= (float (/ num-winners-short shorts)) crit-acc)
             
                ) 
             (setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


(defun find-best-dubaitrade2 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk vri1 vri2  stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long
        action directive1  
         mt-dubai-path2  
         oco-code
         central-exit-time central-entry-time central-cancel-time central-end-session-time
	  (time-zone "UT") offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc  counter))

    (setq *entry-factor* .3333  *stop-loss-day* 1.0557 *max-day-risk* 2500 *min-dubai-expected-value* 40) ; 1.0557)

    (setq ; cannon-epic-path2 (string-append *daily-output-dir* "cannon-epic.csv")
;          striker-epic-path2 (string-append *daily-output-dir* "striker-epic.csv")
;          kingsview-epic-path2 (string-append *daily-output-dir* "kingsview-epic.csv")
          mt-dubai-path2 (string-append *ninja-output-dir* "mt-epic.csv")
	  )

    
    (setq vri2 (volatility-ratio-index tdate 4 28 1) vri1 (volatility-ratio-index tdate 1 7 1))
    (format T "~%EPSIGNAL = ~A longs = ~A  long-gains = ~A ~%"
            epsignal longs long-gains )
     (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains  )
     (format T " vri2= ~4,2F  vri1= ~4,2F~%" vri2 vri1)
    (setq risk  (volatility tdate 4 *stop-loss-day*) risk (+ risk (* 3.0 (index-tick-size))))

   (if (and (member epsignal '(OK UP))
            (>= (/ long-gains longs) *min-dubai-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
            (plusp vri2)(plusp vri1)
            )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))
            (>= (/ short-gains shorts) *min-dubai-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
            (plusp vri2)(plusp vri1)      
             )
    (push 'DN trade-direction)(push 'FT trade-direction))
;   (format T "~%risk = ~A trade-direction= ~A" (* risk (calculate-point-value tdate)) trade-direction)
   (multiple-value-setq (entry-short entry-long)(vprices tdate 4 *entry-factor* 1))


   (setq entry-long (+ entry-long (* 3.0 (index-tick-size))) entry-short (- entry-short (* 3.0 (index-tick-size))))
   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))


             (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)))
                   ((member 'UP trade-direction) (push "NOT SHORT" action))
                   ((member 'DN trade-direction) (push "NOT LONG" action))
                   (t (push "NOT TODAY" action)))


         (setq action (cond ((member "NOT TODAY" action :test #'equalp) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equalp)
                                  (member "NOT SHORT" action :test #'equalp)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equalp)
                             (incf counter) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equalp)
                              (incf counter) "NOT SHORT")
                            (t (incf counter) "OK      ")))

      (format output " ~A " action )
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~3F~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
                         (convert-to-decimal (convert-to-32 entry-short)))(index-point-value)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (index-point-value))))
         (format output

            (string-append "~%SELL= " directive1 " STOP= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
            (* (index-tick-size) (round entry-short (index-tick-size)))
            (* (index-tick-size) (round stop-short (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
                          (* (index-tick-size) (round entry-short (index-tick-size)))
                             )  (index-point-value)))
            (* (index-tick-size) (round entry-long (index-tick-size)))
            (* (index-tick-size) (round stop-long (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round entry-long (index-tick-size)))
                          (* (index-tick-size) (round stop-long (index-tick-size)))
                       )  (index-point-value)))
            ))



      (setq
           central-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
	   central-end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *foremost-market-times-list*)))
           central-exit-time (string-append (date-convert date1) " "
                                    (add-minutes1 (third (assoc *data-name* *mt-market-times-list*)) 0))
           central-cancel-time (string-append (date-convert date1) " "
                                    (add-minutes1 (third (assoc *data-name* *mt-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
     (format T "~%~A  ~A~%" *data-name* action)

     (cond
             ((equal action "NOT SHORT")
             (write-xml-record tdate "tblTradeRecs" 'EPIC 'LONG date1 entry-long stop-long cover-long output1)

              (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " " 
                                             (add-minutes1 (third (assoc *data-name* *mt-market-times-list*)) offset T)))
             ;   (format T "~%~A ~A ~A ~A" mt-dubai-path2 time-zone tdate date1)
               (with-open-file (ninja-output mt-dubai-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-mt-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long))
               

               )


             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'SHORT date1 entry-short stop-short cover-short output1)


               (setq offset (random-choice -1 0)
                     central-exit-time (string-append (date-convert date1) " "
                                              (add-minutes1 (third (assoc *data-name* *mt-market-times-list*))
                                   offset T)))
             ;  (format T "~%~A ~A ~A ~A" mt-dubai-path2 time-zone tdate date1)

               (with-open-file (ninja-output mt-dubai-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	                	 (write-mt-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
               
               ));;closes clause the cond

      )) ;;;closes the let and the defun


(defun find-best-dubai-indicator-set2a ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (dubaitrades-add-one-in2a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)
             (> result .85)
             (< (fifth (car winners-list)) 1.9)
             (> (fourth (car winners-list)) .20)
            )
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list);

   (loop     
      
       (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 2.4)(return))
        (if (> (fifth (second winners-list)) 2.4)(return)))

       (setq winners-list (dubaitrades-leave-one-out2a base-list))
       (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    
     (format T "~%Best indicator set = ~A~%" base-list)
    base-list
    ))
 

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun dubaitrades-add-one-in2a (base-features candidate-features)
  (let (winners-list (result 0) average-profit rtb rp single-bins csgof)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'dubai-trade-bins2a (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
    (vsort winners-list #'> 'seventh)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

;;;requires a base features list
(defun dubaitrades-leave-one-out2a (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp)
 

  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp)
                    (apply #'dubai-trade-bins2a (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))


(defun dubaitrade-simulation-test2 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk singles vri2 vri1
       (ave-win 0) (ave-loss 0) (losers 0) (winners 0) extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw ignore ;draw-ave draw-90
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "dubai-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "dubai-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "dubai-diary2.dat")))
    
 
    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

     (set-market market) (format T "~%~A~%" *data-name*)
  
   (setq *entry-factor* .3333 *stop-loss-day* .764 ;1.0557
         *max-day-risk* 1500 *commission* 10 *pips-slippage* 0
          *min-dubai-expected-value* 40)  

   (multiple-value-setq (ignore ignore ignore singles) (apply #'dubai-trade-bins2b features))

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
   (setq entry-short (- entry-short (* 3 (index-tick-size))) entry-long (+ entry-long (* 3 (index-tick-size))))

   (setq risk (volatility date 4 *stop-loss-day*) risk (+ risk (* 3 (index-tick-size))))
   
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1))

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
    			              entry-short (+ entry-short (getd date 'rollover))))

    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))
;    (format T "~%~A~%" date)
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-dubaitrades2b date-1 features))
            )
;   (format T "~%longs= ~A long-gains = ~A epsignal = ~A" longs long-gains epsignal)
;;;check if new entry

   (when (and 
              (<= (- (getd date 'low)(* 3 (index-tick-size))) entry-short)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-dubai-expected-value*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
              (plusp vri2)(plusp vri1)
                 )
          (setq short  (min entry-short (getd date 'open))
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)
                 )

 ;    (format T "~%shorts = ~A short-gains = ~A risk= ~A entry-long = ~A" shorts short-gains risk entry-long)
    (when (and 
               (>= (+ (getd date 'high)(* 3 (index-tick-size))) entry-long)
	        (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-dubai-expected-value*)
            
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
               (plusp vri2)(plusp vri1)
             )
           (setq long  (max entry-long (getd date 'open))
                 long-trade (list date 'long long))
           (setf (svref record 2) 1)
                 )

;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
;                   (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5))    
                   (<= (getd date 'close) stop-long))
;          (<= (- (getd date 'low)(* 3 (index-tick-size))) stop-long)
            )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (get-ninja-symbol *data-name*)
                             (contract-month *data-name* date) 'S stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
               (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
 ;                  (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))   
                 (>= (getd date 'close) stop-short))
 ;          (> (+ (getd date 'high)(* 3 (index-tick-size))) stop-short)    
             )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (get-ninja-symbol *data-name*)
                           (contract-month *data-name* date) 'S stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (- (max (getd date 'close) stop-long)(* 3 (index-tick-size))))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                            (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (get-ninja-symbol *data-name*)
                            (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (+ (min (getd date 'close) stop-short)(* 3 (index-tick-size))))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (get-ninja-symbol *data-name*)
                         (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil))

      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)

   );;;closes the dotimes

     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
           (round  (svref ith 4)))
     ))

;;;

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win  (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
 ;  (format T "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)

    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
 ;   (format T "~%trades = ~A winners = ~A~%" (list-sum trades) (length winners))
  
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLE RATIO = ~A~%" *stop-loss-day* singles)

      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED-OUT=    ~,1,2,'*,' F% "

       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
      (/ (count 'S (mapcar #'(Lambda(s1) (svref s1 6)) extended-trades)) 
         (if (zerop (length extended-trades)) 1 (length extended-trades)));;percentage of trades stopped out
         )
  ;    (setq ext extended-trades)
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 
 ;   (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
   
       (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/ (list-sum trades)
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))

;   (multiple-value-setq (draw-ave draw-90)(monte-carlo-drawdown trades))
;       (setq draw-ave (* draw-ave (index-point-value)) draw-90 (* draw-90 (index-point-value)))
;      (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A" draw-ave draw-90) 


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
      (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3)) (svref ith 8))))
        ));;;closes the with-open-file

  );;;closes the when outfile

  (values (round (list-sum trades))
          (length trades) trades)
   ))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-dubaitrade-indicators2 ()
   (let ((path (string-append *upper-dir-warehouse* "dubaitradewarehouse2.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))

    ;  (setf (svref ith 6) (candle date-1 (1+ (- (n-day-extreme-dates1 date-1 9)))));;feature 4
 ;    (setf (svref ith 7)(pldot-index date-1)) ;;feature 5
  ;   (setf (svref ith 9) (r-squared-change-index date-1 5 1));;feature 7
     ; (setf (svref ith 14)(reversal-index date-1)) ;;feature 12
 ;    (setf (svref ith 14) (reflect3 date-1 3)) ;;;feature 12
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))

))

;---------------------------------------------------------------------------------------------------
;;;Epic for Stock equity markets


(defun update-equity-warehouse2 (date &optional (markets *equity-warehouse-list*))

   (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-equity-trades2 ith date (min 5040 (available-days ith date 400))))
 
   (build-equitytrade-warehouse2 markets) 
   (setq *equity-features2* (find-best-equity-indicator-set2a))
   (portfolio-simulation3 '(equity) date 5040 (list *equity-warehouse-list*))
)



(defun build-equitytrade-warehouse2 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "equitytradewarehouse2.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "equitytradewarehouse2.backup"))
           (delete-file (string-append *upper-dir-warehouse* "equitytradewarehouse2.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "equitytradewarehouse2.dat"))
          (rename-file (string-append *upper-dir-warehouse* "equitytradewarehouse2.dat")
                            "equitytradewarehouse2.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "equitytrades2.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


(defun populate-equity-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 cover-long cover-short entry-long entry-short risk vri1 vri2
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "equitytrades2.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

   (format outfile "~%~A~%" market)
  
   (setq *entry-factor* .3333 *stop-loss-day* 1.0557 *max-day-risk* 2500
            *commission* 35 *pips-slippage* 0)
 
   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

 ;  (format T "~A ~A~%" date (getd date 'close))
  (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
  
   (setq risk (volatility date 4 *stop-loss-day*)) 
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1))

   (setq  date-1 date date (add-mkt-days date 1))


    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk));;;needs to be after the rollover
;;;

   (when (and (<= (getd date 'low)  entry-short)
              (>= (* risk (calculate-point-value date))
                  (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
	      (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (plusp vri2)(plusp vri1) (> (getd date-1 'close) 5.0) 
              )
          (setq short  (min entry-short (getd date 'open))
                short-trade (create-equitytrade-entry-record-list2 date-1 -1 short)
                 ))

    (when (and (>= (getd date 'high)  entry-long)
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
               (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (plusp vri2)(plusp vri1)(> (getd date-1 'close) 5.0)
               )
           (setq long (max entry-long (getd date 'open))
                 long-trade (create-equitytrade-entry-record-list2 date-1 1 long)
                 ))

;;;check if stopped out on same day

   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
              ;     (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5)) 
                  (<= (getd date 'close) stop-long))
              ; (<=  (getd date 'low)  stop-long)
               )
           (push (round (- (* (my-pretty-price (- stop-long long)) (calculate-point-value date))
                              (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                   (round (* (calculate-point-value date) (my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and 
                        (<= (getd date 'open) (getd date 'close)))
              ;     (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))      
                    (>= (getd date 'close) stop-short))
         ;          (>=  (getd date 'high) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                   (round (* (calculate-point-value date) (- short stop-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                   (round (* (calculate-point-value date)(my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                (round (* (calculate-point-value date) (my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes

  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners) 
                            (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/  (list-sum losers) 
                            (if (zerop (length losers)) 1 (length losers)))
       
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round  (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (drawdown trades))
      ; (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
       )

     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
       (format stream "~S~%" ith)
    ));;closes the dolist and with-open-file

  );;;closes the when outfile
  (values (round (list-sum trades))
       (length trades))
 ));


(defun equity-trade-bins2a ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "equitytradewarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades2 record features))
    
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
    );;closes the dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))


(defun equity-trade-bins2b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "equitytradewarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
       (setq bin (encode-day-trades2 record (butlast features ith)))
      
        (cond ((gethash bin *day-trade-warehouse3*)
               (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
               (setf (gethash bin *day-trade-warehouse3*)
                     (cons record (gethash bin *day-trade-warehouse3*)))))
              ((not (gethash bin *day-trade-warehouse3*))
               (setf (gethash bin *day-trade-warehouse3*) (list record))
               (push bin day-bin-codes)))
        ))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)

  ))
;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-equitytrade-entry-record-list2 (date direction entry)
   (let* ((date-1 (getd date 'ydate)))
    
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))

                              (body-range-index date);;feature 2
                              (body-range-index date-1);;feature 3
                              (body-range-index (getd date-1 'ydate)) ;;features 4

                              (r-squared-change-index date 5 1);;feature 5  
                              (r-squared-change-index date-1 5 1);;;feature 6  
                              (r-squared-change-index (getd date-1 'ydate) 5 1) ;;;feature 7
           
                              (r-squared-change-index date 10 2) ;;  feature 8 
                              (r-squared-change-index date 20 4);;;feature 9
                              (r-squared-change-index date 40 8) ;feature 10                         
                              (r-squared-change-index date 80 16);; feature  11
                              (r-squared-change-index date 160 32);; feature  12
                              
                              (range-index1 date 5)  ;feature 13 
                              
                              (tkcd-index date) ;;;feature 14
                                                   
                          
                              );;;closes the list

))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-equitytrades2b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-equitytrade-entry-record-list2 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-day-trades2 record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
  
    (setf (nth 0 bin) -1) 
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda (s1) (eql nxdate (svref s1 1))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
    
    (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
        (incf num-winners-long) (setq gains-long (+ gains-long (svref kth 19))))
    (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short)(setq gains-short (+ gains-short (svref kth 19))))
      (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

        );;;closes dolist over contents
   
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

    (cond ((and (plusp results-long)(plusp results-short)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc)
        
             ) 
            (setq epsignal 'OK))
        ((and (plusp results-long)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)
                      ) 
             (setq epsignal 'UP))
        ((and (plusp results-short)
               (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc)
            
               ) 
             (setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


(defun find-best-equitytrade2 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk vri1 vri2  stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long
        action directive1  retail-path2
         ts-equity-path2  
         oco-code
         central-exit-time central-entry-time central-cancel-time central-end-session-time
	 ; (time-zone "UT")
          offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))

    (setq *entry-factor* .3333  *stop-loss-day* 1.0557 *max-day-risk* 2500 *min-equity-expected-value* 80) 

    (setq   retail-path2 (string-append *daily-output-dir* "equity-view.txt")
           ; cannon-epic-path2 (string-append *daily-output-dir* "cannon-epic.csv")
;          striker-epic-path2 (string-append *daily-output-dir* "striker-epic.csv")
;          kingsview-epic-path2 (string-append *daily-output-dir* "kingsview-epic.csv")
           ts-equity-path2 (string-append *daily-output-dir* "ts-epic-" (format nil "~A" date1) ".csv")
	  )
   
   (setq vri2 (volatility-ratio-index tdate 4 28 1) vri1 (volatility-ratio-index tdate 1 7 1))
  (format T "~%EPSIGNAL = ~A longs = ~A  long-gains = ~A~%" 
            epsignal longs long-gains)
  (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains )

   (setq risk  (volatility tdate 4 *stop-loss-day*) risk (+ risk (* 3.0 (index-tick-size))))

   (if (and (member epsignal '(OK UP))
            (>= (/ long-gains longs) *min-equity-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
            (plusp vri2)(plusp vri1)(> (getd tdate 'close) 5.0)
             )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))
            (>= (/ short-gains shorts) *min-equity-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
            (plusp vri2)(plusp vri1)(> (getd tdate 'close) 5.0)
            )
    (push 'DN trade-direction)(push 'FT trade-direction))
  (format T "~%risk = ~A trade-direction= ~A" (round (* risk (calculate-point-value tdate))) trade-direction)
   (multiple-value-setq (entry-short entry-long)(vprices tdate 4 *entry-factor* 1))

   (setq entry-long (+ entry-long (* 3.0 (index-tick-size))) entry-short (- entry-short (* 3.0 (index-tick-size))))
   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))


             (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)))
                   ((member 'UP trade-direction) (push "NOT SHORT" action))
                   ((member 'DN trade-direction) (push "NOT LONG" action))
                   (t (push "NOT TODAY" action)))


         (setq action (cond ((member "NOT TODAY" action :test #'equalp) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equalp)
                                  (member "NOT SHORT" action :test #'equalp)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equalp)
                             (incf counter) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equalp)
                              (incf counter) "NOT SHORT")
                            (t (incf counter) "OK      ")))

      (format output " ~A ~%" action )
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~3F~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
                         (convert-to-decimal (convert-to-32 entry-short)))(calculate-point-value tdate)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (calculate-point-value tdate))))
         (format output

            (string-append "~%SELL= " directive1 " STOP= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
            (* (index-tick-size) (round entry-short (index-tick-size)))
            (* (index-tick-size) (round stop-short (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
                          (* (index-tick-size) (round entry-short (index-tick-size)))
                             )  (calculate-point-value tdate)))
            (* (index-tick-size) (round entry-long (index-tick-size)))
            (* (index-tick-size) (round stop-long (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round entry-long (index-tick-size)))
                          (* (index-tick-size) (round stop-long (index-tick-size)))
                       )  (calculate-point-value tdate)))
            ))

      (setq
           central-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
	   central-end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *foremost-market-times-list*)))
           central-exit-time (string-append (date-convert date1) " "
                                 ;   (add-minutes1 (third (assoc *data-name* *equity-market-times-list*)) 0))
                                    (add-minutes1 "15:58:00" 0))
           central-cancel-time (string-append (date-convert date1) " "
;                                    (add-minutes1 (third (assoc *data-name* *equity-market-times-list*)) -26))
                                    (add-minutes1 "15:58:00" -26))
	   oco-code (format nil "OSO~A" counter))
     (format T "~%~A  ~A~%" (get-ninja-symbol *data-name*) action)

     (cond
             ((equal action "NOT SHORT")
             (write-xml-record tdate "tblTradeRecs" 'EPIC 'LONG date1 entry-long stop-long cover-long output1)

              (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " " 
             ;                                (add-minutes1 (third (assoc *data-name* *equity-market-times-list*)) offset T)))
                                             (add-minutes1 "15:58:00" offset T)))
             ;   (format T "~%~A ~A ~A ~A" mt-equity-path2 time-zone tdate date1)
           ;    (with-open-file (ninja-output mt-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	   ;      	     (write-mt-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long))
               (write-equity-view retail-path2  'BUY entry-long stop-long)
             ;  (with-open-file (ninja-output ts-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	     ;    	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long 100))
               

               )


             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'SHORT date1 entry-short stop-short cover-short output1)


               (setq offset (random-choice -1 0)
                     central-exit-time (string-append (date-convert date1) " "
;                                              (add-minutes1 (third (assoc *data-name* *equity-market-times-list*))
                     (add-minutes1 "15:58:00" offset T)))
             ;  (format T "~%~A ~A ~A ~A" mt-equity-path2 time-zone tdate date1)

            ;   (with-open-file (ninja-output mt-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	    ;            	 (write-mt-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
                (write-equity-view retail-path2  'SELL entry-short stop-short)
             ;  (with-open-file (ninja-output ts-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	     ;    	     (write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short 100))
               


               )

            ((equal action "OK      ")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'SHORT date1 entry-short stop-short cover-short output1)


               (setq offset (random-choice -1 0)
                     central-exit-time (string-append (date-convert date1) " "
;                                              (add-minutes1 (third (assoc *data-name* *equity-market-times-list*))
                     (add-minutes1 "15:58:00" offset T)))
             ;  (format T "~%~A ~A ~A ~A" mt-equity-path2 time-zone tdate date1)

            ;   (with-open-file (ninja-output mt-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	    ;            	 (write-mt-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
                (write-equity-view retail-path2  'SELL entry-short stop-short)
            ;   (with-open-file (ninja-output ts-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	    ;     	     (write-ts-record ninja-output time-zone tdate date1 'SELL entry-short stop-short 100))
               
            
             (write-xml-record tdate "tblTradeRecs" 'EPIC 'LONG date1 entry-long stop-long cover-long output1)

              (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " " 
             ;                                (add-minutes1 (third (assoc *data-name* *equity-market-times-list*)) offset T)))
                                             (add-minutes1 "15:58:00" offset T)))
             ;   (format T "~%~A ~A ~A ~A" mt-equity-path2 time-zone tdate date1)
           ;    (with-open-file (ninja-output mt-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	   ;      	     (write-mt-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long))
               (write-equity-view retail-path2  'BUY entry-long stop-long)
            ;   (with-open-file (ninja-output ts-equity-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	    ;     	     (write-ts-record ninja-output time-zone tdate date1 'BUY entry-long stop-long 100))
               

               )



);;closes clause the cond

      )) ;;;closes the let and the defun


(defun find-best-equity-indicator-set2a ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (equitytrades-add-one-in2a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)
             (> result .85)
             (< (fifth (car winners-list)) 1.9)
             (> (fourth (car winners-list)) .20)
          )
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      
       (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 3.50)(return))
        (if (> (fifth (second winners-list)) 3.50)(return)))

      (setq winners-list (equitytrades-leave-one-out2a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    
     (format T "~%Best indicator set = ~A~%" base-list)
   base-list
    ))
 

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun equitytrades-add-one-in2a (base-features candidate-features)
  (let (winners-list (result 0) average-profit rtb single-bins rp csgof)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'equity-trade-bins2a (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
    (vsort winners-list #'> 'seventh)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

;;;requires a base features list
(defun equitytrades-leave-one-out2a (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp)
 

  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp)
                    (apply #'equity-trade-bins2a (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))


(defun equitytrade-simulation-test2 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
       date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk singles vri2 vri1
       (ave-win 0) (ave-loss 0) (losers 0) (winners 0) extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc   bin draw ignore ;draw-ave draw-90
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "equity-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "equity-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "equity-diary2.dat")))
     
    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

     (set-market market) (format T "~%~A~%" *data-name*)
  
   (setq *entry-factor* .3333 *stop-loss-day* 1.0557 
         *max-day-risk* 2500 *commission* 35 *pips-slippage* 0
         *min-epic-expected-value* 80)
 
   (multiple-value-setq (ignore ignore ignore singles) (apply #'equity-trade-bins2b features))

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
   (setq entry-short (- entry-short (* 3 (index-tick-size))) entry-long (+ entry-long (* 3 (index-tick-size))))

   (setq risk (volatility date 4 *stop-loss-day*) risk (+ risk (* 3 (index-tick-size))))
   (setq vri2 (volatility-ratio-index date 4 28 1) vri1 (volatility-ratio-index date 1 7 1))

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))
;;;;the record has the date the closing price and 0 0 0 at the start of each day.
;;;the third slot of record is 1 or -1 for direction of trade
;;;
    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))
;    (format T "~%~A~%" date)
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-equitytrades2b date-1 features))
      
       )
;   (format T "~%longs= ~A  long-gains = ~A epsignal = ~A" longs long-gains epsignal)
;;;check if new entry

   (when (and 
              (<=  (getd date 'low) entry-short)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
              (plusp vri2)(plusp vri1)(> (getd date-1 'close) 5.0) ;;stocks over 5 dollars  
              )
          (setq short (min entry-short (getd date 'open))
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)

                   )

 ;    (format T "~%shorts = ~A short-gains = ~A risk= ~A entry-long = ~A" shorts short-gains risk entry-long)
    (when (and 
               (>= (getd date 'high) entry-long)
	       (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
               (plusp vri2)(plusp vri1)(> (getd date-1 'close) 5.0)
               )

           (setq long (max entry-long (getd date 'open))
                 long-trade (list date 'long long))
           (setf (svref record 2) 1)

                 )

;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
;                   (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5))    
                   (<= (getd date 'close) stop-long))
;          (<= (getd date 'low) stop-long)
           )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                                (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (get-ninja-symbol *data-name*)
                            (contract-month *data-name* date) 'S stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
       ;    (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
                   (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
 ;                  (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))   
                 (>= (getd date 'close) stop-short))
 ;          (>=  (getd date 'high) stop-short)    
             )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                           (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (get-ninja-symbol *data-name*)
                          (contract-month *data-name* date) 'S stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
        ;   (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long)) (calculate-point-value date))
                               (comm+slip date))) trades)

            (setq long-trade (apply #'vector (append long-trade (list date (get-ninja-symbol *data-name*)
                          (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
         ;   (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                  (round (- (*  (my-pretty-price (- cover-long long))(calculate-point-value date))
                            (comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (get-ninja-symbol *data-name*)
                        (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
               (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil))

      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)

    ; (print trades)
   );;;closes the dotimes

;;;;writes out the diary
     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith trading-dates)
        (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
           (round (svref ith 4)))
     ));;;closes the with-open-file

;;;
      ;  (format T "Trades = ~A" trades)
   
  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win  (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) 
          )
 ;  (format T "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)

    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
 ;   (format T "~%trades = ~A winners = ~A~%" (list-sum trades) (length winners))
  
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLE RATIO = ~A~%" *stop-loss-day* singles)

      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED-OUT=    ~,1,2,'*,' F% "

       (round  (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round  (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f  trades)) 0)
      (/ (count 'S (mapcar #'(Lambda(s1) (svref s1 6)) extended-trades)) 
         (if (zerop (length extended-trades)) 1 (length extended-trades)));;percentage of trades stopped out
         )
  ;    (setq ext extended-trades)
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 
 ;   (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
   
       (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/ (list-sum trades) 
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))

;   (multiple-value-setq (draw-ave draw-90)(monte-carlo-drawdown trades))
;       (setq draw-ave (* draw-ave (index-point-value)) draw-90 (* draw-90 (index-point-value)))
;      (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A" draw-ave draw-90) 


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
      (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3)) (svref ith 8))))
        ));;;closes the with-open-file

  );;;closes the when outfile

  (values (round  (list-sum trades))
          (length trades) trades)
   ))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-equitytrade-indicators2 ()
   (let ((path (string-append *upper-dir-warehouse* "equitytradewarehouse2.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))

    ;  (setf (svref ith 6) (candle date-1 (1+ (- (n-day-extreme-dates1 date-1 9)))));;feature 4
 ;    (setf (svref ith 7)(pldot-index date-1)) ;;feature 5
  ;   (setf (svref ith 9) (r-squared-change-index date-1 5 1));;feature 7
  ;    (setf (svref ith 14)(reversal-index date-1)) ;;feature 12
 ;    (setf (svref ith 14) (reflect3 date-1 3)) ;;;feature 12
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))

))


;;;;;;;;;;;FOREX startes here;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun update-forex-warehouse2 (date &optional (markets  *forex-warehouse-list* ))

  (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-forex-trades2 ith date (available-days ith date 600)))
 
   (build-forex-warehouse2 markets) 
   (setq *forex-features2* (find-best-indicator-set2x))
   (portfolio-simulation3 '(forex) date 3700 (list markets))

  ; (portfolio-simulation4 '(forexday) date 3500 (list markets))
)


(defun populate-forex-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 entry-long entry-short 
       dir sdate stop-loss xtr cover-long cover-short
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "forextrades2.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

   (format outfile "~%~A~%" market)
   (setq *entry-factor* .764
         *stop-loss-day* 1.0557  ;.6667;.944 ;1.0557  
         *commission* 0 *pips-slippage* 6 *max-day-risk* 500 )
 
   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
 

   (multiple-value-setq (dir sdate stop-loss)(parabolic-stops date))
    (multiple-value-setq (entry-short entry-long)(vprices date 21 *entry-factor* 1))
    (multiple-value-setq (stop-long stop-short)(vprices date 21 *stop-loss-day* 1))

        (setq ;macdd (macd-direction date 12 26 9 2)
            xtr (volatility date 21 *entry-factor*)
            )
           ;  eproc (ep-roc-change-index date 3 10))           
   
    (if (eql dir 'long) (setq cover-short  (my-pretty-price stop-loss)
                             stop-short (+ (getd date 'close) (volatility date 21 *stop-loss-day*))
                             stop-long nil cover-long nil               
                  
                                )
                       (setq cover-long (my-pretty-price stop-loss)
                             stop-long (- (getd date 'close)(volatility date 21 *stop-loss-day*))
                             stop-short nil cover-short nil      
                                 ))
    

  ; (setq vri428 (volatility-ratio-index date 4 28 1) vci428 (volatility-change-index date 4 28 2)
  ;       cdi5 (channel-direction-index5 date)
  ;       rri5 (roc-rel-index5 date)       )
   (setq  date-1 date date (add-mkt-days date 1))

  

   (when (and (<= (getd date 'low) entry-short)
                (eql dir 'LONG)
        ;          (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
            ;  (<= (* risk (calculate-point-value date)) *max-day-risk*)
             ;  (plusp vri428)
            ;   (member cdi5 '(-1 2));(member wpp '(-1 2))
              ; (/= vci428 -2)(/= rri5 4)
               
                      )
          (setq short (min  entry-short (getd date 'open))
                short-trade (create-forex-entry-record-list2 date-1 -1 short)
               cover-short stop-loss
                 ))

    (when (and (>= (getd date 'high) entry-long)
                (eql dir 'SHORT)
          ;         (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
	   ;    (<= (* risk (calculate-point-value date)) *max-day-risk*)
              ;  (plusp vri428)
             ;   (member cdi5 '(1 2));(member wpp '(1 2))
              ;  (/= vci428 2)(/= rri5 -4)
                
             )
           (setq long (max entry-long (getd date 'open))
                 cover-long stop-loss
                 long-trade (create-forex-entry-record-list2 date-1 1 long)
                 ))


;;;check if met exit criteria to exit at target
     (when (and long cover-long (> (getd date 'high) cover-long))
             (setq cover-long (max (getd date 'open) cover-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                            (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                 (round (* (calculate-point-value date) (my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil cover-long nil))

      (when (and short cover-short (< (getd date 'low)  cover-short))
            (setq cover-short (min (getd date 'open) cover-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                  (round (* (calculate-point-value date)(my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil cover-short nil))


;;;check if stopped out on same day
   (when (and long stop-long
;               (or (and (<= (getd date 'low) stop-long)
;                        (>= (getd date 'open) (getd date 'close)))
;                   (<= (getd date 'close) stop-long))
              (<= (getd date 'low) stop-long)     
          )
           (push (round (- (* (my-pretty-price (- stop-long long)) (calculate-point-value date))
                              (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                   (round (* (calculate-point-value date) (my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
   ;            (or (and (>= (getd date 'high) stop-short)
   ;                     (<= (getd date 'open) (getd date 'close)))
   ;                 (>= (getd date 'close) stop-short))
               (>= (getd date 'high) stop-short)
                    )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                 (round (* (calculate-point-value date)(my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                               (round (* (calculate-point-value date)(my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                            (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                (round (* (calculate-point-value date)(my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes

 
  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))  (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (drawdown trades))
      ; (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
       )

     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
       (format stream "~S~%" ith)
    ));;closes the dolist and with-open-file

  );;;closes the when outfile
  (values (round (list-sum trades))
       (length trades))
 ));


;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-forex-entry-record-list2 (date direction entry)
   (let* (  (date-1 (getd date 'ydate)) ; period)
         )
     ; (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                           

                             (channel-fx10 date );;feature 2* 
                             (wpp-index0 date) ;;;feature 3*
                             (channel-direction-index21  (getd date-1 'ydate)) ;feature 4*
                            
                             (n-day-extreme-direction date 34) ;;  feature 5*   
                             (candle-fx date ) ;;;feature 6*                          
   
                             (pivot-turn-fx-month date );;7
                             (channel-direction-index date-1 5);;8*
                             (reversal-day-index date);;9
                            
                             (wpp-fx-composite date ) ;;feature 10*
                             (pivot-turn-fx-week date) ;;feature 11
                            
                             (channel-fx5 date );;feature 12            
                             (roc-rel-index5 date);;feature 13
                           

                             (n-day-extreme-direction date 8);;feature 14* 

                           ;  (body-range-index  date) ;;features 13 
                           ;  (reflect3 date 21);;;feature 14                          
                                   
                             );;;closes the list

))

;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-forextrade-indicators2 ()
   (let ((path (string-append *upper-dir-warehouse* "forexwarehouse2.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))

    ;  (setf (svref ith 9) (n-day-extreme-direction date-1 21));;feature 7
    ;  (setf (svref ith 6)(n-day-extreme-direction date-1 13)) ;;feature 4
    ;  (setf (svref ith 11) (n-day-extreme-direction date-1 5));;feature 9
    ;  (setf (svref ith 14)(n-day-extreme-direction date-1 4)) ;;feature 12
      (setf (svref ith 15) (channel-direction-index9  date-1)) ;;;feature 13
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))

))


(defun forex-trade-bins2a ( &rest features)
  (let (bin path)

  ; (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "forextrades2.dat"))
   (setq path (string-append *upper-dir-warehouse*  "forexwarehouse2.dat"))
     (maind-x)(set-cat-list)
   (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-forex-trades2 record features))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
      );;closes the dolist
    
    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))

(defun forex-trade-bins2b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "forexwarehouse2.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
       (setq bin (encode-forex-trades2 record (butlast features ith)))
      
        (cond ((gethash bin *day-trade-warehouse3*)
               (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
               (setf (gethash bin *day-trade-warehouse3*)
                     (cons record (gethash bin *day-trade-warehouse3*)))))
              ((not (gethash bin *day-trade-warehouse3*))
               (setf (gethash bin *day-trade-warehouse3*) (list record))
               (push bin day-bin-codes)))
        ))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)

  ))



(defun encode-forex-trades2 (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction
;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
     (2 (push (svref record 4) bin-list))

;;;;Feature 3 with 2 levels range-direction date 3
      (3 (push (svref record 5) bin-list))
;;;;Feature 4 with 6 levels is the stochastic with parameter 21
      (4 (push (svref record 6) bin-list)) 
;;;;Feature 5 has 6 levels is range-index1 3
       (5 (push (svref record 7) bin-list))
;;;Feature 6 is mo-diver date 5
       (6 (push (svref record 8) bin-list))
;;;Feature 7 is Gann-slope-index 5 63
       (7 (push (svref record 9) bin-list))
;;;Feature 8 with 6 levels pivot-index
       (8 (push (svref record 10) bin-list))
;;;Feature 9 is range-index
       (9 (push (svref record 11) bin-list))
;;;Feature 10 with 6 levels 2-bar pivotindex 
       (10  (push (svref record 12) bin-list))
;;; feature 11 with 6 levels is the 3-bar pivot-index 
       (11 (push (svref record 13) bin-list))
;;Feature 12 reflect3 date 3
       (12  (push (svref record 14)  bin-list))
 ;;;;Feature 13 Gann-slope-index 9 63
       (13 (push (svref record 15) bin-list))
;;;;Feature 14 with 5 levels (pivot-index for the previous day)
       (14  (push (svref record 16) bin-list))
                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))


(defun find-best-indicator-set2x ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (forex-add-one-in2a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)
              (> result .85)
              (< (fifth (car winners-list)) 1.9)
              (> (fourth (car winners-list)) .20)
            )
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      
       (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 2.9)(return))
        (if (> (fifth (second winners-list)) 2.9)(return)));

       (setq winners-list (forex-leave-one-out2a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    
     (format T "~%Best indicator set = ~A~%" base-list)
    base-list
    ))
 

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun forex-add-one-in2a (base-features candidate-features)
  (let (winners-list (result 0) average-profit rtb rp single-bins csgof)

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'forex-trade-bins2a (append base-features (list ith))))
     (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
    (vsort winners-list #'> 'seventh)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))



;;;requires a base features list
(defun forex-leave-one-out2a (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp)
                    (apply #'forex-trade-bins2a (remove ith base-features)))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))



;;;
(defun forex-simulation-test2 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0)  reward
      ;  vri428 cdi5 vci428 rri5 
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw draw-ave draw-90 draw-99 
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "forex-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "forex-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "forex-diary2.dat")))

     (if (and num (> num (available-days market date2)))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

   (set-market market) (format T "~%~A~%" *data-name*)
   
   (setq *entry-factor* 0 ;.3333
         *stop-loss-day* .382 *objective-reward* 1.618 ;.6667 ;.944 ;1.0557 
          *max-day-risk* 500 
          *commission* 0 *pips-slippage* 6 )
    
      (apply #'forex-trade-bins2b features) 
   
   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 21 *entry-factor* 1))
   (multiple-value-setq (stop-long stop-short)(vprices date 21 *stop-loss-day* 1))
   (setq *min-epic-expected-value* (* 2  (comm+slip date)))

   (setq 
         reward (volatility date 21 *objective-reward*))
 
 ; (setq vri428 (volatility-ratio-index date 4 28 1) cdi5 (channel-direction-index5 date)
  ;       rri5 (roc-rel-index5 date) vci428 (volatility-change-index date 4 28)
  ;      )
   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

  
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-forextrades2b date-1 features))
       
       )

;;;check if new entry

   (when (and 
              (<= (getd date 'low) entry-short)
      ;        (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
          ;    (>= (/ short-gains shorts)
           ;       (if (> longs 0.0) (/ long-gains longs) 0))
              ; (plusp vri428 )
         ;      (member cdi5 '(-1 2));(member wpp '(-1 2))
              ; (/= vci428 -2)(/= rri5 4)
               
                  )
          (setq short  (min entry-short (getd date 'open))
                short-trade (list date 'short short)
                cover-short (my-pretty-price (- short reward))
                )
           (setf (svref record 2) -1)

                 )

    (when (and 
               (>= (getd date 'high) entry-long)
;	       (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
          ;     (> (/ long-gains longs)
           ;       (if (> shorts 0.0)(/ short-gains shorts) 0))
              ; (plusp vri428 )
        ;       (member cdi5 '(1 2));(member wpp '(1 2))
              ; (/= vci428 2)(/= rri5 -4)
                )

           (setq long  (max entry-long (getd date 'open))
                 long-trade (list date 'long long)
                 cover-long (my-pretty-price (+ long reward))
                 )
           (setf (svref record 2) 1)
          
            )

;;;check if  out with objective on same day

   (when (and long cover-long (> (getd date 'high) cover-long)
           )
           (setq cover-long (max (getd date 'open) cover-long))
           (push (round (- (* (my-pretty-price (- cover-long long)) (calculate-point-value date))
                           (comm+slip date))) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'O
                                                          stop-long (my-pretty-price (- cover-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil cover-long nil
           ))

    (when (and short cover-short (< (getd date 'low) cover-short)
                 )
           (setq cover-short (min (getd date 'open) cover-short))
           (push (round (- (* (my-pretty-price (- short cover-short)) (calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'O
                                                           cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
           (setf (svref record 3) (+ (svref record 3) 
                (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil cover-short nil
           ))

;;;check if stopped out on same day
   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
              ; (<= (getd date 'low) stop-long)
           )
           (push (round (- (* (my-pretty-price (- stop-long long)) (calculate-point-value date))
                           (comm+slip date))) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'S
                                                          stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)

           (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
;               (>= (getd date 'high) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short)) (calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                           stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)

           (setf (svref record 3) (+ (svref record 3) 
                (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))



;;;check if met exit criteria on day of entry (exit at end of day)
     (when long  
            (setq cover-long (max (getd date 'close) stop-long))
          (push (round (- (* (my-pretty-price (- cover-long long)) (calculate-point-value date))
                          (comm+slip date))) trades)  
          (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)

            (setf (svref record 3) (+ (svref record 3)
              (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                               (comm+slip date))) trades) 
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)

           (setf (svref record 3) (+ (svref record 3)
                     (round  (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                             (comm+slip date)))))
           (setq short-trade nil short nil stop-short nil))

      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)

   );;;closes the dotimes

     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
           (round (svref ith 4)))
     ))


  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
;   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F TARGET FACTOR= ~5,3,0,'*,' F~%" *stop-loss-day* *objective-reward*)

    (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract= ~10D  "

       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/  (list-sum trades)
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) ))

     (multiple-value-setq (draw-ave draw-90 draw-99)(monte-carlo-drawdown trades))
     
     (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A~%99 PERCENTILE DRAWDOWN= ~A~%" 
         (round draw-ave) (round draw-90) (round draw-99)) 

   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
      (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 0)) (svref ith 8))))
        ));;;closes the with-open-file

  );;;closes the when outfile

  (values (round (list-sum trades))
          (length trades) trades)
   ))

(defun build-forex-warehouse2 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "forexwarehouse2.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "forexwarehouse2.backup"))
           (delete-file (string-append *upper-dir-warehouse* "forexwarehouse2.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "forexwarehouse2.dat"))
          (rename-file (string-append *upper-dir-warehouse* "forexwarehouse2.dat")
                            "forexwarehouse2.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "forextrades2.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


(defun find-best-forextrade2 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk ;vri428 
        stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long
        action directive1 ;offset
        retail-path2 mt-path2 ts-forex-path2
        (time-zone "UT")
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))
    (format T "~%~A~%" *data-name*)
    (setq *entry-factor* .3333
          *stop-loss-day*  .944 
          *max-day-risk* 500
          *min-epic-expected-value* 12 *commission* 0 *pips-slippage* 6)

    (setq ts-forex-path2 (string-append *daily-output-dir* "ts-forex-" (format nil "~A" date1) ".csv")
         ; cannon-path2 (string-append *daily-output-dir* "cannon-cross.csv")
         ; gain-path2 (string-append *daily-output-dir* "gain-cross.csv")
        ;  kingsview-starter-path3 (string-append *daily-output-dir* "kingsview-starter.csv")
        ;  striker-score-path3 (string-append *daily-output-dir* "striker-score.csv")
        ;  striker-starter-path3 (string-append *daily-output-dir* "striker-starter.csv")
        ;  cannon-score-path3 (string-append *daily-output-dir* "cannon-score.csv")
          mt-path2 (string-append *ninja-output-dir* "mt-epic.csv")
          retail-path2 (string-append *daily-output-dir* "forex-view.txt")
         )
  
    
    ; (setq vri428 (volatility-ratio-index tdate 4 28 1); ss15 (slow-stochastic-index tdate 15)
     ;      )
   (format T "~%EPSIGNAL = ~A longs = ~A  long-gains = ~A ~%"
            epsignal longs long-gains )
    (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains )

   (setq risk (volatility tdate 4 *stop-loss-day*))
   

   (if (and (member epsignal '(UP))
            (>= (/ long-gains longs) *min-epic-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
         ;   (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
         ;   (>= vri428 -2) 
            )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(DOWN))
            (>= (/ short-gains shorts) *min-epic-expected-value*)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
         ;   (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
         ;   (>= vri428 -2) 
             )
    (push 'DN trade-direction)(push 'FT trade-direction))
 ; (format T "~%~A 222 ~%" *data-name*)

   (multiple-value-setq (entry-short entry-long)(vprices tdate 21 *entry-factor* 1))


   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

      ;      (when (if (member *data-name* '(US.D1B TY.D1B))
      ;            (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
      ;                             (convert-to-decimal (convert-to-32 stop-long))) (calculate-point-value tdate)))
      ;                  *max-day-risk*)
      ;           (> (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
      ;                            (* (index-tick-size) (round entry-short (index-tick-size))))
      ;                            (calculate-point-value tdate))) *max-day-risk*)) (push "NOT TODAY" action))

             (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)))
                   ((member 'UP trade-direction) (push "NOT SHORT" action))
                   ((member 'DN trade-direction) (push "NOT LONG" action))
                   (t (push "NOT TODAY" action)))


         (setq action (cond ((member "NOT TODAY" action :test #'equalp) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equalp)
                                  (member "NOT SHORT" action :test #'equalp)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equalp)
                            ; (incf counter)
                              "NOT LONG")
                            ((member "NOT SHORT" action :test #'equalp)
                             ; (incf counter)
                              "NOT SHORT")
                            (t ; (incf counter)
                                "OK      ")))

      (format output " ~A ~%" action )
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~3F~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
                         (convert-to-decimal (convert-to-32 entry-short)))(index-point-value)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (index-point-value))))
         (format output
            (string-append "~%SELL= " directive1 " STOP= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
            (* (index-tick-size) (round entry-short (index-tick-size)))
            (* (index-tick-size) (round stop-short (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
                          (* (index-tick-size) (round entry-short (index-tick-size)))
                             )  (calculate-point-value tdate)))
            (* (index-tick-size) (round entry-long (index-tick-size)))
            (* (index-tick-size) (round stop-long (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round entry-long (index-tick-size)))
                          (* (index-tick-size) (round stop-long (index-tick-size)))
                       )  (calculate-point-value tdate)))
            ))

  ; (format T "~% ~A 333~%" *data-name*)
      (format T "~%~A  ~A~%" *data-name* action)

     (cond   ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'LONG date1 entry-long stop-long cover-long output1)
       ;       (setq offset (random-choice -1 0) pacific-exit-time (string-append (date-convert date1) " "
       ;                                               (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
               
              (with-open-file (ninja-output ts-forex-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-epic-qty*))
             
                   
               (write-forex-view retail-path2  'BUY entry-long stop-long)
               (with-open-file (ninja-output mt-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	                	 (write-mt-record ninja-output time-zone tdate date1 'BUY entry-long stop-long))
              
               )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'EPIC 'SHORT date1 entry-short stop-short cover-short output1)
              (write-forex-view retail-path2  'SELL entry-short stop-short)
              (with-open-file (ninja-output ts-forex-path2 :direction :output :if-exists :append :if-does-not-exist :create)
                 	(write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short *striker-epic-qty*))

               (with-open-file (ninja-output mt-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	                	 (write-mt-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
              
               ));;closes clause the cond


      )) ;;;closes the let and the defun


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-forextrades2b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal ; (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-forex-entry-record-list2 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
      (setq bin (encode-forex-trades2 record features))
      (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    
      (setf (nth 0 bin) -1)
      (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
      (setq contents
           (remove-if #'(lambda(s1) (eql nxdate (svref s1 1))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
       (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
    
    (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
        (incf num-winners-long) (setq gains-long (+ gains-long (svref kth 19))))
    (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short)(setq gains-short (+ gains-short (svref kth 19))))
      (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

        );;;closes dolist over contents
   
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

    (cond ;((and (plusp results-long)(plusp results-short)
          ;   (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
          ;   (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
          ;   (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc)
         ; 
         ;     )
         ;      (setq epsignal 'OK))
        ((and (plusp results-long)
             ;  (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
             ;  (>= (float (/ num-winners-long longs)) crit-acc)
               (> (/ gains-long longs)
                  (if (> shorts 0.0)(/ gains-short shorts) 0))
            
               )
           (setq epsignal 'UP))
        ((and (plusp results-short)
             ; (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
             ; (>= (float (/ num-winners-short shorts)) crit-acc)
              (>= (/ gains-short shorts)
                  (if (> longs 0.0) (/ gains-long longs) 0))

               )  
             (setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))
#|
(defun populate-all-epic--vectors (tdate &optional (markets (append *forex-warehouse-list* *day-list*)))
  (let (date  (period 5) num (path1 "~/exitpoints/all-epic-vectors.lisp"))
  (setq price-vectors nil)
  (dolist (ith markets)
   
    (setq num (available-days ith tdate 550) date (add-mkt-days tdate (- num))) 
   (dotimes (jth num)
     (push (create-epic-vector date period 'A) price-vectors)
     (setq date (getd date 'ndate))
    ))
  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (dolist (ith price-vectors)
     (format str "~A~%" ith)))
))

|#
;;;feature is an index into the vector
(defun epic-feature-stats (feature)
  (let ( (path1 "~/exitpoints/all-epic-vectors.lisp") lfeature results)
  (with-open-file (str path1 :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record price-vectors)
          ))

  (setq lfeature (1- (length (car price-vectors))))

   (dolist (kth price-vectors)
     (if (assoc (svref kth feature) results)
         (progn
             (setf (second (assoc (svref kth feature) results))
                   (+ (svref kth lfeature)(second (assoc (svref kth feature) results))))
             (setf (third (assoc (svref kth feature) results))
                   (1+ (third (assoc (svref kth feature) results))))
             (setf (fourth (assoc (svref kth feature) results))
                   (/ (second (assoc (svref kth feature) results))
                      (third (assoc (svref kth feature) results)))))
         (setq results (cons (list (svref kth feature) (svref kth lfeature) 1 
                             (svref kth lfeature)) results))))

 (vsort results #'> 'fourth)

))  
     
;;;direction is either low or high   
 (defun create-epic-vector (date period typ)
   (let (  ;(date-1 (getd date 'ydate)) 
       
             )
       ; (setq date-2 (getd date-1 'ydate))
      
       (vector *data-name* date typ
           
             (wpp date );;;feature 3
             (candle1 date);;feature 4
             (reversal-dayp date );;feature 5    
           
             (volatility-ratio-index date 4 28 1);;feature 6
             (rsi date 2);feature 7
             (rsi2x date 2);feature 8  
          
             (lproj-index date period);feature 9
             (lprojdelta date period);;feature 10
             (ldev-index date period);;feature 11           
                              
             (momentum-divergence2 date period (* 3 period));;;feature 12
             (momentum-divergence2 date (* 2 period) (* 3 period));;;feature 13
            
             (channel-direction date (* 1 period)) ;feature 14
             (channel-direction date (* 2 period));feature 15
             (channel-direction date (* 4 period));;;feature 16
         
             (pivot-turn date 'month);;;feature 17
             (pivot-turn date 'week);;; feature 18
            
             (ep-roc-index date period ) ;feature 19
             (ep-roc-index date (* 2 period)) ;feature 20
            
             (reflect3 date period);feature 21
             (body-range$ (getd date 'ndate)); feature 22
                         );;;closes the vector

))

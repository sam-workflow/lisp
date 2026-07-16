;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;;This is for the NEW FORE day trading system
;;;;This version does not use objectives.
;;;It does use a vector database instead of lists
;;;
;;; 
;;; entry is at the open
;;;
;;;;For DAY TRADING
;;;
(defvar *nt-pf-list* nil)
(defvar *ts-pf-list* nil)
;;;;the file has one record per trade each record is a vector. 
;;;The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1
;;;;
;;;;;;;
;;; #(*data-name* entry-date direction entry-price HT BT BR T5 T15 DO BR-1 CAN SST T45 ZBL BPL TIM)
;;;
;;;Feature 1 is direction
;;;Feature 2 is the volatility ratio 4 day versus 28 day
;;;Feature 3 is the day-bar-type
;;;Feature 4 is the slow stochastic 21
;;;Feature 5 is the num waves 3 9
;;;Feature 6 is mo-diver 5 (trend of momentum RSI(9))
;;;
;;;Feature 12 is now the pinpoint
;;;Feature 13 is mo-diver 45 (trend of momentum RSI(9))
;;;feature 14 is pivot-index date-1
;;;
;;This function is used to add trades to the warehouse3 for all the day
;;;markets (both score and non-score)
;;;*position-list* includes all 35 futures markets
(defun update-daytrade-warehouse4 (date  &optional (markets *micro-list*))
  (maind-x)(set-cat-list)

   (dolist (ith markets)
        (set-market ith)
       (populate-day-trades4 ith date (min (sub-mkt-dates 20110103 date) (available-days ith date 250))))
    (build-daytrade-warehouse4 markets)
    (setq *day-features4* (find-best-indicator-set4a t))
    (display-unfiltered-trades daytrades)
 ;   (apply #'day-trade-bins4a nil *day-features4*)
  ;  (indicator-study4a)
    (portfolio-simulation3 '(day4) date 3333 (list markets))
   
)

(defun populate-day-trades4 (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short  date-1 
     ; macdd vsos4
      ;sts
       ;dbt2  ; cci5h3 cci5l3  cci21 ccis1
      ; cci21d2  cci5d1;rri10 lproj5  md5-21 wppf  vi4-28  ;macdd
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
      (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades4.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

  (format outfile "~%~A~%" market)
 ; (setq   *stop-loss-4* 1.0 *objective-reward* 1.618)
  (if (member market *forex-warehouse-list*)
        (setq *commission* 0 *min-fore-expected-value* 12 *pips-slippage* 6)
     (setq *commission* 25 *min-fore-expected-value* 20 *pips-slippage* 0))
  (if (member market *micro-list*)(setq *commission* 6))
  
  (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))

   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
   
  ; (setq reward (fmin (index-limit) (volatility date 63 *objective-reward*)))
 ;  (multiple-value-setq (cover-long cover-short)(vtargets date))
;   (multiple-value-setq (cover-short cover-long)(vprices5 date 3 1.618 3));;;1.618
   (multiple-value-setq (stop-long stop-short)(vprices date *duration* *factor* 1 *type*));;;1.618
 ;  (setq stop-long (ave date 3 'low) stop-short (ave date 3 'high))
   (setq ;risk (min (volatility date 4 *stop-loss-4*) (/ *max-day-risk* (calculate-point-value date)))
       ; pivm (pivot-index date 'month)
;These are the best three indicator filters
      ;  dbt2 (day-bar-type2 date)
      ;  pivid (pivot-index date 'day) piviw (pivot-index date 'week);macdd  (ep-macd-signal date 21 63 11 )
       ; vsos4 (volume-openint-div date 4)
       ; sts (swing-trade-status date)
        ; can (candle-composite date 3) ;css (counter-swing-signal date)
        ;  ldev10 (ldev-index date 10) ri1 (range-index1 date 5)
      ;  cci21 (commodity-channel-index date 21)       
     ;   ccis1 (cci-signal1 date)
        ; lbds5-2 (lbounds date 5 2)
        ; vri4-28-2  (volume-ratio-index1 date 4 28 2) ;feature 13
        ; vi4-28 (volume-index date 4 28) ;vri1-20 (volume-ratio-index date 5 'vi1-20) ;;feature 8 
        ; chan21 (channel-direction date 21)
    ;    cci5d1 (cci-direction date 5 1)
     ;   cci21d2 (cci-direction date 21 2) ;fst (fast-stochastic-index date 21)
      )
   ;  (multiple-value-setq (cci5h3 cci5l3)(cci-high-low date 5 3))
;   (setq vri (volatility-ratio-index date 4 63 1)
        ; reward nil); (volatility date 4 1.0))
 ;  (setq outlook (forecast-currencies-day date))
 ;     (format T "1 ~A ~A~% " date date-1)     
      (setq date-1 date)
      (setq date (add-mkt-days date 1))
   ;   (format T "2 ~A ~A~% " date date-1)     
;  (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
;                                      entry-short (+ entry-short (getd date 'rollover))
;    ))

 
;;;check if new long entry
;;;enter on a stop order if opens below entry-stop.
;;;enter on a limit order of open above the entry-stop
;;;exit at the close

   (when (and (not short)(> (getd date 'open) stop-long)
             ; (>= (* risk (calculate-point-value date-1)) 20) ;;to avoid bad data
	     ; (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
             ; (< rri10 5);(> rri5 -4)(< rri5 3);(/= swi -2) 
             ; (minusp cci21d2)(minusp cci5d1)
        
           ;  (member macdd '(-1 -2 -3 4)) (> dbt2 0) 
           ;  (= sts -1)

             ; (<= ldev10 1)(not (member chan21 '(UC+)))
             ;  (/= cci11-2 1) (/= can 1)(/= ri1 -4) ;(/= fs2 -3)
      ;        (/= ss14 4)(/= ss14 -3);(not (member lbds5-2 '(2 8 10)))
              
            ; (< pivw 0)
             
             ; (/= vi4-28 5); (/= vri1-20 -1);(not (member rsi2x '(1 9)))
              ; (> cci5d1 -3)
             ;  (or (minusp cci5d1)(< cci21 -200))
             ; (/= fst 1)
					;  (< cci21d2 1)
	     ; (member pivid '(1 2 4))(member piviw '(-2 -4 1 -1))
              )
          (setq short (getd date 'open) ;entry-short
               ;short (nth-value 1 (selling-pressure1 date))
              ;  stop-short (+ short risk) ;cover-short (- short reward)
                short-trade (create-daytrade-entry-record-list4 date-1 -1 short)
                 ))
;(format t "~%date= ~A short= ~A risk= ~A stop-short= ~A reward= ~A" date short risk stop-short reward)
    (when (and (not long)(< (getd date 'open) stop-short)
      
     *              )
           (setq long (getd date 'open) ;entry-long
                 ;long (nth-value 1 (buying-pressure1 date))
                 ;cover-long (+ long reward)
                 long-trade (create-daytrade-entry-record-list4 date-1 1 long)

                 )) ;(format T "long-trade= ~A ~%" long-trade) )
;(format t "~%date= ~A Long= ~A risk= ~A stop-long= ~Areward= ~A~%" date long risk stop-long reward)

#|
;;;check if met exit criteria to exit at target
     (when (and long cover-long (> (getd date 'high) cover-long))
             (setq cover-long (max (getd date 'open) cover-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value1 date-1 market))
                            (comm+slip date-1))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                 (round (* (calculate-point-value1 date market) (my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil cover-long nil))

      (when (and short cover-short (< (getd date 'low)  cover-short))
            (setq cover-short (min (getd date 'open) cover-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value1 date market))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                  (round (* (calculate-point-value1 date market)(my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil cover-short nil))

|#
 ;;;check if stopped out on same day

   (when (and long stop-long
             ; (<= (getd date 'low) stop-long)
             (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
             ;     
                  (<= (getd date 'close) stop-long))

               )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value1 date-1 market))
                               (comm+slip date-1))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                  (round (* (calculate-point-value1 date market)(my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))
  ; (format t "~%1 Trades = ~A" trades)
    (when (and short stop-short
             ;  (>= (getd date 'high) stop-short)  
                (or (and (>= (getd date 'high) stop-short)
                         (<= (getd date 'open) (getd date 'close)))
                     (>= (getd date 'close) stop-short))
               
              )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value1 date-1 market))
                              (comm+slip date-1))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                  (round (* (calculate-point-value1 date market)(my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
 ; (format t "~%2 trades= ~A~%" trades)


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long ;(nth-value 2 (buying-pressure1 date)))
                            (getd date 'close))
            (push (round (- ;(max ;(* (my-pretty-price (- (buying-pressure1 date)(selling-pressure1 date)))
                                (* (- cover-long long)
                                  (calculate-point-value1 date-1 market)) ;-1000)
                               (comm+slip date-1))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                  (round (* (calculate-point-value1 date market) (my-pretty-price
                                        ; (- (buying-pressure1 date)(selling-pressure1 date))
                                        (- cover-long long)
                                        )))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short ;(nth-value 2 (selling-pressure1 date)))
                              (getd date 'close))
           (push (round (- ;(max ;(* (my-pretty-price (- (selling-pressure1 date)(buying-pressure1 date)))
                              (*  (- short cover-short) 
                              (calculate-point-value1 date-1 market)) ;-1000)
                             (comm+slip date-1))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                (round (* (calculate-point-value1 date market)(my-pretty-price
                                        ; (- (selling-pressure1 date)(buying-pressure1 date))
                                         (- short cover-short)
                                         )))))))
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
       (/  (length winners) 
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
(defun create-daytrade-entry-record-list4 (date direction entry)
   (let* ( );(date-1 (getd date 'ydate)) )
    
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
;;;;sector                           
                              (vsignals1 date)  ;;feature 2
                        
                             (ep-macd-signal date 5 10 3) ;(adx-index date 9);;;feature 3*
                             
;;;;daily patterns
                              (volatility-ratio-index date 4 21 1);;feature 4
                                                      
                              (truncate (vril date 4 4));;feature 5                                

                              (ep-macd-signal date 12 26 9);;feature 6
                                     
;;;trend indicators           
                             (zero-strength date 3); (cycle-proj date -252 10 30);;;feature 7
                              
                              (wpp date ) ;feature 8 
                              ;;feature 9*
                              (zero-strength date 8)
                             
                              (day-bar-type date);;feature 10 
                              
                              (zero-strength date 5);(cycle-signal date 10 30) ;; feature 11       
                             
                              (cci-direction date 5 1);;;feature 12*
;;;volatility         
                              (pivot-turn date 'week) ;feature 13*  
;;;Volume                 
                              (gann-slope-index date 21) ;; feature 14
                           
;;;open interest                                 
                               );;;closes the list

))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-daytrade-indicators4 (markets)
   (let (;(path (string-append *upper-dir-warehouse* "daytradewarehouse4.dat"))
          path1 date-1 date-2)
  (maind-x)(set-cat-list)

 (dolist (market markets)
     (setq daytrades nil
           path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades4.dat"))
 
  (with-open-file (str path1 :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate) date-2 (getd date-1 'ydate))

     (setf (svref ith 4) (rsi2x date-1 ));;feature 2
     (setf (svref ith 5)  (adx-index date-1 9));;feature 3 
     (setf (svref ith 6)  (volatility-ratio-index date-1 4 21));;;feature 4
     (setf (svref ith 7) (truncate (vril date-1 4 4))) ;feature 5 
     (setf (svref ith 8)(ep-macd-signal date-1 12 26 9)) ;feature 6
     (setf (svref ith 15)(pivot-turn date-1 'week));;feature 13    
     (setf (svref ith 16)(gann-slope-index date-1 21)) ;feature 14 
     (setf (svref ith 10)(wpp date-1)) ;feature 8
    ; (setf (svref ith 13)(cycle-signal date-1 10 30));;feature 11
     (setf (svref ith 11)(volume-openint-div date-1 4)) ;feature 9
;     (setf (svref ith 4)(case (reflect2 date-1 8)
;;				((1 2) 'S);
;				((-1 -2) 'L)
;				(otherwise 0)));;feature 2
;
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))
 );;;closes dolist over markets
  (build-daytrade-warehouse4 markets)
))


(defun encode-day-trades4 (record features)
  (let (bin-list)
    (dolist (ith features)
      (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction

;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
      (2 (push (svref record 4) bin-list))
;;;;Feature 3 with 9 levels bar type
      (3 (push (svref record 5) bin-list))

;;;;Feature 4 with 6 levels is the stochastic with parameter 21
      (4 (push (svref record 6) bin-list))

;;;;Feature 5 is a roc-index date 5 21
       (5 (push (svref record 7) bin-list))
;;;Feature 6 is mo-diver date 5 with 5 levels
       (6 (push (svref record 8) bin-list))
;;;Feature 7 is Gann-slope-index 5 63
       (7 (push (svref record 9) bin-list))
;;;Feature 8 with 6 levels pivot-index
       (8 (push (svref record 10) bin-list))
 ;;Feature 9 is range-index
       (9 (push (svref record 11) bin-list))

;;;Feature 10 with 5 levels 2-bar index date-1
       (10  (push (svref record 12) bin-list))
;;; feature 11 is the day-bar-type date-1
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
(defun bin-classifier-daytrades4b (date-1 features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .55)(crit-pf 1.750);*crit-pf*)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)(pf-longs 0)(pf-shorts 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date-1 'ndate))
        (num-winners-long 0)(num-winners-short 0) contentsl contentss)
    (setq record (create-daytrade-entry-record-list4 date-1 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith (- (length features) 1));;;changed to 3 to help heap exhaustion
     (setq bin (encode-day-trades4 record features))
    ; (format T "~%FEATURES = ~A BIN = ~A~%" features bin)
     (setq contentsl (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
   ;  (format T "CONTENTSl = ~A~%" contentsl)
    ;;; then look for short trades. rule All bins are long or short trades
      (setf (nth 0 bin) -1)
     (setq contents (append (setq contentss (gethash bin *day-trade-warehouse3*)) contentsl))
   ;  (format T "CONTENTSS = ~A~%" contentss)
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (eql nxdate (svref s1 1))) contents))
  ;       (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if contents ;(and contentsl contentss)
          (return) (setq features (butlast features)))
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
;     (when (eql (svref kth 2) 1)
;      (incf longs) (setq results-long (+ results-long (svref kth 19))))
;    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
;       (incf num-winners-long))
;     (when (eql (svref kth 2) -1)
;      (incf shorts)(setq results-short (+ results-short (svref kth 19))))
;     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
;        (incf num-winners-short))
        );;;closes dolist over contents
   
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents


  (cond ;((and (plusp results-long)(plusp results-short)
        ;      (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
        ;      (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
        ;      (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
        ;      (setq epsignal 'OK))
       ((not contents) (setq epsignal 'UNIQUE))       
       ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID1)) 
       ((and (plusp results-long)
            ; (> (/ results-long (if (zerop longs) 1 longs)) (* 3 *commission*))
             ;    (/ results-short (if (zerop shorts) 1 shorts)))
              (>= (setq pf-longs (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long))))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)
            )
              (setq epsignal 'UP))
        ((and (plusp results-short)
             ; (> (/ results-short (if (zerop shorts) 1 shorts))(* 3 *commission*))
             ;    (/ results-long (if (zerop longs) 1 longs)))
              (>= (setq pf-shorts (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short))))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc)
               )
            (setq epsignal 'DOWN))
               
        (t (setq epsignal 'AVOID)))


    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin pf-longs pf-shorts)
))

(defun daytrade-simulation-test4 (market date2 num &optional (features *day-features4*))
  (declare (special markets-ll)) 
 (let (date trades long short cover-long cover-short  record trading-dates  ;(grp (list market))
        date-1 epsignal longs long-gains shorts short-gains pf-longs pf-shorts
        (running-sum 0)  singles draw 
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc bin vsig ignore   
     ;  (path-in (string-append *config-dir* "day-features4.dat"))
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary4.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation4.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary4.dat")))

 
   (set-market market) (format T "~%~A~%" *data-name*)

      (if (and num (> num (available-days market date2 250)))
        (setq num nil))
     (ifn num (setq num (available-days market date2 250)))

    (setq *stop-loss-4* 1.0 *objective-reward* 1.618 )
    (if (member market *forex-warehouse-list*)
       (setq *commission* 0 *min-fore-expected-value* 12 *pips-slippage* 6)
     (setq *commission* 25 *min-fore-expected-value* 20 *pips-slippage* 0))
     (if (member market *micro-list*)(setq *commission* 6))
  
     (setq date (add-mkt-days date2 (- num)))
    (setq record (vector date (getd date 'close) 0 0 0))
    (push record trading-dates)

 
   (build-daytrade-warehouse4 *micro-list*)
   
    (setq features (find-best-indicator-set4a nil ))
    (multiple-value-setq (ignore ignore ignore singles) (apply #'day-trade-bins4b nil features))  
  ;  (display-unfiltered-trades daytrades) (set-market market)

 ;;;;from date1 to date2
 (dotimes (ith num)

 ;(format T "Market= ~A date=~A~%" *data-name* date)

  (setq vsig (vprices date *duration* *factor* 1 *type*))
  ; (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction date 3))

  ; (setq reward (volatility date 63 *objective-reward*))
;    (multiple-value-setq (cover-short cover-long)(vprices5 date 3 1.618 3));;;1.618
   (multiple-value-setq (stop-long stop-short)(vprices date *duration* *factor* 1 *type*))
 

   (setq  date-1 date date (add-mkt-days date 1))
;  (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
;                                      entry-short (+ entry-short (getd date 'rollover))
;    ))

#|
   (when  (/= (end-of-prior-month date)(end-of-prior-month date-1)) 
        ; (format T "~%date = ~A date-1 = ~A grp = ~A" date date-1 grp)
         (build-daytrade-warehouse4 grp (end-of-prior-month date))
         (setq features (find-best-indicator-set4a))
         (apply #'day-trade-bins4b nil features))
|#
   (setq record (vector date (getd date 'close) 0 0 0))

;;;;calculate bin-classifier 
      (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin pf-longs pf-shorts)
                   (bin-classifier-daytrades4b date-1 features))
 ;;     (format t "~%date-1 = ~A  epsignal = ~A longs = ~A long-gains = ~A long-acc = ~A~%" date-1 epsignal longs long-gains long-acc)
 ;     (format t "shorts = ~A short-gains = ~A short-acc = ~A bin = ~A~%" shorts short-gains short-acc bin)
 ;     (format t "pf-longs = ~A pf-shorts = ~A~%" pf-longs pf-shorts)
;;;check if new entry

   (when (and (not short)   (> (getd date 'open) stop-long)
	      (member epsignal '(OK DOWN))(> (* long-acc pf-longs)(* short-acc pf-shorts))
               ) 
          (setq short 
               (getd date 'open) 
                ;stop-short (+ short risk) 
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)
             )

    (when (and (not long)(< (getd date 'open) stop-short)
               (member epsignal '(OK UP))(< (* long-acc pf-longs)(* short-acc pf-shorts))	         
                )

           (setq long 
                 (getd date 'open) 
                ; stop-long (- long risk) 
                 long-trade (list date 'long long)
                 )
           (setf (svref record 2) 1)
            )


#|
;;;check if met exit at target on day of entry
     (when (and long cover-long (> (getd date 'high) cover-long))
            (setq cover-long (max (getd date 'open) cover-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1 market))
                                 (comm+slip date-1))) trades)
            (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'O
                                                           cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1 market))
                                 (comm+slip date-1)))))
            (setq long-trade nil long nil stop-long nil cover-long nil))

      (when (and short cover-short (< (getd date 'low) cover-short))
           (setq cover-short (min (getd date 'open) cover-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1 market))
                               (comm+slip date-1))) trades)
            (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'O
                                                           cover-short (my-pretty-price (- short cover-short))))))
            (push short-trade extended-trades)
            (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1 market))
                               (comm+slip date-1)))))
            (setq short-trade nil short nil stop-short nil cover-short nil))
|#

 ;;;check if stopped out on same day

   (when (and long stop-long
   ;            (<= (getd date 'low) stop-long)

              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
  ;                
                  (<= (getd date 'close) stop-long))
           )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date-1 market))
                               (comm+slip date-1))) trades)

           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'S
                                                           stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 3) (+ (svref record 3)
                   (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date-1 market))
                                      (comm+slip date-1)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
     ;        (>= (getd date 'high) stop-short)    

               (or (and (>= (getd date 'high) stop-short)
                       (<= (getd date 'open) (getd date 'close)))
                     (>= (getd date 'close) stop-short))
             )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date-1 market))
                              (comm+slip date-1))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                          stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
           (setf (svref record 3) (+ (svref record 3)
                  (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date-1 market))
                               (comm+slip date-1)))))
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long ;(nth-value 2 (buying-pressure1 date)))
                 (getd date 'close))
            ;(push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1 market))(comm+slip date-1))) trades)
             (push (round (-  ;(* (my-pretty-price (- (buying-pressure1 date)(selling-pressure1 date)))
                              (* (my-pretty-price (- cover-long long))
                                  (calculate-point-value date-1 market))
                               (comm+slip date-1))) trades)
          
            (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-long (my-pretty-price (max (- cover-long long)
                                                         ; (max (- (buying-pressure1 date)(selling-pressure1 date))
                                                               (/ -1000.0 (calculate-point-value date market))))
                                                           ))))
            (push long-trade extended-trades)
            (setf (svref record 3) (+ (svref record 3)
                 (round (- (max (* (my-pretty-price (- cover-long long))
                             ; (- (buying-pressure1 date)(selling-pressure1 date)))
                             (calculate-point-value date-1 market)) -1000)(comm+slip date-1)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           
           (setq cover-short ;(nth-value 2 (selling-pressure1 date)))
                  (getd date 'close))
           (push (round (-  (* (my-pretty-price (- short cover-short))
                               ;(- (selling-pressure1 date)(buying-pressure1 date)))
                               (calculate-point-value date-1 market))
                             (comm+slip date-1))) trades)
  
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-short (max (my-pretty-price (- short cover-short))
                                                           ;(max (- (selling-pressure1 date)(buying-pressure1 date))
                                                              (/ -1000.0 (calculate-point-value date market)))
                                                            ))))
                               
           (push short-trade extended-trades)
           (setf (svref record 3) (+ (svref record 3)
                 (round (-  (* (my-pretty-price (- short cover-short))
                               ;(- (selling-pressure1 date)(buying-pressure1 date)))
                              (calculate-point-value date-1 market))(comm+slip date-1)))))
           (setq short-trade nil short nil stop-short nil))

     
      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)

   );;;closes the dotimes

     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
          (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))(round (svref ith 4)))
     ))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) 
          )
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= OPEN~%" (length daytrades))
   
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLES RATIO = ~5,3,0,'*,' F~%" *stop-loss-4* singles)
    (format str "OBJECTIVE FACTOR= ~7,3,0,'*,' F ~%" *objective-reward*)
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  %DAYS IN TRADE=    ~F    COMMISSION=        ~D~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED OUT=    ~,1,2,'*,' F%~%"

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
      (my-round (* 100 (/ (length trades) num)) 1) *commission*
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
       (/ (count 'S (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1))
         )
       (format str "OBJECTIVE OUT= ~,1,2,'*,' F%~%" 
           (/ (count 'O (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1)))
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,2,'*,' F~%"
           (/ (/  (list-sum trades)
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) )


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

     (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3) market) (svref ith 8))))
        ));;;closes the with-open-file
  );;;closes the when outfile

  (values (round (list-sum trades))
          (length trades) trades)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun day-trade-bins4b (&optional (stream T) &rest features)
  (let (bin path)

  ;  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4.dat"))
  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith (- (length features) 1))
     (setq bin (encode-day-trades4 record (butlast features ith)))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
            (setf (gethash bin *day-trade-warehouse3*) (list record))
            (push bin day-bin-codes)))
     ))
    (format stream  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features stream)


  ))


(defun day-trade-bins4a (&optional (stream T) &rest features)
  (let (bin path)

  ;  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4.dat"))
  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades4 record features))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
              (push bin day-bin-codes))))

    (format stream  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades stream)


  ))


#|
(defun day-trade-bins4 ( &rest features)
  (let (bin path)

  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4.dat"))
;  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades4 record features))
     (pushnew bin day-bin-codes :test #'equal)
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*)
              (list record)))))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)


  ))
|#
(defun build-daytrade-warehouse4 (markets &optional (nxdate 99999999))
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse4.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse4.backup"))
           (delete-file (string-append *upper-dir-warehouse* "daytradewarehouse4.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse4.dat"))
          (rename-file (string-append *upper-dir-warehouse* "daytradewarehouse4.dat")
                            "daytradewarehouse4.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "daytrades4.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))
    (setq trades (remove-if #'(lambda(s1)(<= nxdate (svref s1 1))) trades))
    (setq daytrades trades)   
    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
 
));;closes the let and defun



(defun add-day-trades4 (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse4.dat")) trades)

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


(defun remove-market4 (market)
  (let (trades path)
  (setq path (setq path (string-append *upper-dir-warehouse* "daytradewarehouse4.dat")))
  (maind-x)(set-cat-list)

    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))

   (setq trades (remove-if #'(lambda (s) (eql (svref s 0) market)) trades))
   (with-open-file (str path :direction :output :if-exists :supersede)
       (dolist (ith trades)
        (format str "~S~%" ith)))
 ))




;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
;;;;result is the gain/loss 
;;;ith is the feature
;;;average profit is the average gain/loss per trade
;;;single-bins is the ratio of bins with one trade to all bins
(defun daytrades-add-one-in4a (base-features candidate-features &optional (str T))
  (let (winners-list (result 0) average-profit rtb single-bins rp csgof)

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'day-trade-bins4a str (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
;;;rp is 
    (vsort winners-list #'> 'seventh);;;seventh is the csgof value
   ; (vsort winners-list #'> 'first)
   (format str "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format str "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format str "Winners List = ~A ~%" winners-list)
   winners-list
 ))



;;;rtb is the ratio of trades to bins
;;;rp is the ratio of winners in winning bins to all winners 
;;;requires a base features list
(defun daytrades-leave-one-out4a (base-features &optional (str T))
  (let (winners-list (result 0) rtb average-profit single-bins rp csgof)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
                    (apply #'day-trade-bins4a str (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
 ; (vsort winners-list #'> 'seventh);;;seventh is the csgof value
  (vsort winners-list #'> 'car);;;car is the profit in winning bins
  (format str "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format str "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format str "Winners List = ~A ~%" winners-list)
  winners-list
 ))


;;;;this function expects that you have run (day-trade-bins ...) already
(defun display-daytrade-bins-by-expected-value4 (&optional (min-gain -1000)(min-num 00))
  (let (contents expected-value-list result (winners 0)(losers 0) longs shorts
        (path1 (string-append *output-upper-dir* "daytrade-expected-value4.dat")))
   (dolist (ith day-bin-codes)
     (setq contents
           (gethash ith *day-trade-warehouse3*))
     (setq result 0 winners 0 losers 0)
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19))(incf winners)(incf losers)))
      (setq expected-value-list (cons (list (/ result (length contents)) ith (length contents) result winners losers) expected-value-list)));;;closes the dolist over bin-codes
   (vsort expected-value-list #'> 'car)
   (setq expected-value-list (remove-if #'(lambda(s) (or (< (car s) min-gain)(< (third s) min-num))) expected-value-list))
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
       
         (format stream "Profit/trade  Bin         NUM   #WINNERS   #LOSERS   Profit~%~%")
         (dolist (jth expected-value-list)
             (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
         (format stream "~%Total #Trades = ~D  #WINNERS = ~D    $Profit = ~D"
             (list-sum (mapcar #'(lambda(s) (nth 2 s)) expected-value-list))
             (list-sum (mapcar #'(lambda(s) (nth 4 s)) expected-value-list))
             (round (list-sum (mapcar #'(lambda(s) (nth 3 s)) expected-value-list))))
          (format stream "~%CHI SQUARED PROB = ~2,4F~%~%" (daytrade-chi-squared-gof)) 

         (setq longs (remove-if #'(lambda(s) (eql (caadr s) -1)) expected-value-list))
           (vsort longs #'> 'car)
           (dolist (jth longs)
             (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
           (terpri stream)

         (setq shorts (remove-if #'(lambda(s) (eql (caadr s) 1)) expected-value-list))
           (vsort shorts #'> 'car)
           (dolist (jth shorts)
             (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
     
 
);; closes the with open file
))
;;;there is no stop loss or objective because TradeStation provides the entry/exit logic
(defun find-best-indicator-set4a (&optional (str T))
  (let (base-list candidate-list winners-list (result 0) bf); tdate best-features)

    (setq base-list '(1 ) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (daytrades-add-one-in4a base-list candidate-list str))
;;;;first value is the net profit for winning bins
;;;second value is the net gain/loss for all trades
;;;third value is the average profit per trade in winning bins
;;;fourth value is the fraction of single bins
;;;fifth is the ratio of bin profits to all winners
;;;sixth is the chi-squared value for the features
     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
  ;   (if (or (not candidate-list)(> result .80)(< (fifth (car winners-list)) 1.5))
  ;       (return))
      (if (or (not candidate-list)(> result .80)
              (< (fifth (car winners-list)) 1.75)
              (> (fourth (car winners-list)) .25))
         (return))

    )
  ;  (format T "First stage winners-list= ~A"  winners-list)

    (loop   
      (if (not (member (second (car winners-list)) '(1 )))
          (if (and (> (fifth (car winners-list)) 8.0)
                   (< (fourth (car winners-list)) .25))(return))
        (if (and (> (fifth (second winners-list)) 8.0)
                 (< (fourth (second winners-list)) .25))(return)))
        
      (setq winners-list (daytrades-leave-one-out4a base-list str))
      (if (not (member (second (car winners-list)) '(1 )))
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    


     (format str "~%Best indicator set = ~A~%" base-list)
;     base-list
      (setq bf (cons 1 (remove 1 (reverse (cdr (mapcar #'second winners-list))))))
      (if (> (length bf) (length base-list))(butlast bf) bf) 

   ))
 

;;;this is for the FORE product 
(defun find-best-daytrade4 (tdate &optional (output T)(date1 nil))
  (let (  ;stop-short stop-long  
        direction  trade-direction central-entry-time ;central-exit-time
         central-cancel-time central-end-session-time ;pivm pivw
        pivd dbt2 macdd sts
       ;cci21d2 cci21 ;rri5  lproj5  md5-21 wppf  vi4-28  ldev10 chan21 can ri1
        action directive1 ts-fore-path3 c2-fore-path3 oec-fore-path3
        ninja-fore-path3 edge-path sample-edge-path; (wob nil);'(w.d1b pa.d1b ho.d1b cl.d1b))
        oec-symbol oco-code  (time-zone "CT")
       ; offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter bin pf-longs pf-shorts))


     (setq *commission* 6 *min-fore-expected-value* 25  *pips-slippage* 0
            *stop-loss-4* 1.0  *max-day-risk* 20000)
  
    (setq ts-fore-path3 (string-append *ninja-output-dir* "ts-fore-" (format nil "~A" date1) ".csv")
          edge-path (string-append *daily-output-dir* "edgemailtemplate2.html")
          sample-edge-path (string-append *daily-output-dir* "sampleedgetemplate.txt")
          c2-fore-path3 (string-append *daily-output-dir* "c2-fore.txt")
          oec-fore-path3 (string-append *ninja-output-dir* "oec-fore-" (format nil "~A" date1) ".csv")
          ninja-fore-path3 (string-append *ninja-output-dir* "ninja-fore-" (format nil "~A" date1) ".csv"))
  
     (setq ;risk (volatility tdate 4 *stop-loss-4*)
         ;  rri10 (ep-roc-index10 tdate 10) rri5 (ep-roc-index10 tdate 5) ; swi (ww-swing-index1 tdate)
         ;  lproj5 (lproj-index tdate 5)  md5-21 (momentum-divergence3 tdate 5 21)
         ;  wppf (wpp-fore tdate) can (candle-composite tdate 3)
         ;  ldev10  (ldev-index tdate 10) chan21 (channel-direction tdate 21)
       ;  cci21 (commodity-channel-index tdate 21)
        ; vi4-28 (volume-index tdate 4 28)  ri1 (range-index1 tdate 5)
         ; pivm (pivot-index tdate 'month) pivw (pivot-index tdate 'week) 
          sts (swing-trade-status tdate)
          pivd (pivot-index tdate 'day)  dbt2 (day-bar-type2 tdate)
          macdd  (ep-macd-signal tdate 12 26 9 )   

      ; cci21d2 (cci-direction tdate 21 2) ;fst (fast-stochastic-index tdate 21)   
         )
      
 
   ;  (format T "~2%EPSIGNAL = ~A~%" epsignal)
   ;  (format T "  longs = ~A  long-gains = ~A ~%" longs long-gains )
   ;  (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains )
   ;  (format T " pivd = ~A wp = ~A~%" pivd wp)

    (if (and  (member epsignal '(OK DOWN)); (member *data-name* wob))
            
            ; (or (member *data-name* wob)(>= (/ short-gains shorts) *min-fore-expected-value*))
            ; (<= (* risk (calculate-point-value tdate )) *max-day-risk*)
             ;(< rri10 5);(> rri5 -4)(< rri5 4);(/= swi -2) 
             ; (> lproj5 -4)(neql md5-21 'DDN)(/= wppf 1)
            ; (<= pivm 1)(< pivw 0)
#|
|#           ; (>= pivd 0);(>= dbt2 -1)
           ; (member macdd '(-1 -2 -3 4))
           ; (= sts -1)
          ;  (/= cci11-2 1) (/= can 1)(/= ri1 -4)
            ;  (/= ss14 4)(/= ss14 -3);(not (member lbds5-2 '(2 8 10)))
            ; (/= vi4-28 5) ;(/= vri5-20 -1);(not (member rsi2x '(1 9)))
             ; (/= turns 4);(/= macdd 1)
           ;   (or (minusp cci5d1)(< cci21 -200))
            ; (/= fst 1)
            ;  (< cci21d2 2)
              )
         (push 'DN trade-direction)(push 'FT trade-direction))
       
    (if (and  (member epsignal '(OK UP));(member *data-name* wob))
            ; (or (member *data-name* wob)(>= (/ long-gains longs) *min-fore-expected-value*))
   	    ; (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
             ;  (> rri10 -5);(< rri5 4)(> rri5 -4);(/= swi 2)
              ; (< lproj5 4)(neql md5-21 'DUP)(/= wppf -1)
              ;(>= pivm -1)(> pivw 0)
#|  
            (<= pivd 1)(<= dbt2 1)
|#           ;   (member macdd '(1 2 3 -4)) (member vsos4 '(2 -2))
             ;   (= sts 1)
           ;  (/= cci11-2 -1)(/= can -1)(/= ri1 4)
            ;   (/= ss14 -4)(/= ss14 3)
              ; (not (member lbds5-2 '(2 8 10)))
             ; (/= vi4-28 5);(/= vri5-20 -5);(not (member rsi2x '(1 9)))
              ;(/= turns -4); (/= macdd -1)
             ;(or  (plusp cci5d1) (> cci21 200))
            ; (/= fst -1)
              ; (> cci21d2 -2)
              )
         (push 'UP trade-direction)(push 'FT trade-direction))
  
   ;    (if (member *data-name* '(US.D1B TY.D1B))
   ;            (setq risk (convert-to-decimal (convert-to-32 risk)))
   ;        (setq risk  (* (index-tick-size)
   ;                              (round risk (index-tick-size)))))
                               
   
  ;   (when (if (member *data-name* '(US.D1B TY.D1B))
  ;             (>  (* risk (index-point-value)) *max-day-risk*)
  ;           (>  (* risk (calculate-point-value tdate)) *max-day-risk*)) (push "NOT TODAY" action))
   
      (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)
                  (> pf-longs pf-shorts)) (push "NOT SHORT" action))     
            ((and (member 'UP trade-direction) (member 'DN trade-direction)
                  (<= pf-longs pf-shorts)) (push "NOT LONG" action))     
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

      (format output " ~A~% pivd = ~A deb2 = ~A macdd = ~A TD = ~A~%" action pivd dbt2 macdd trade-direction) 
      (format output " Num Longs = ~D P/L for Longs = ~A PF = ~4F Longs ACC  = ~3F~%" longs long-gains pf-longs long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A  PF = ~4F Short ACC= ~3F~%" shorts short-gains pf-shorts short-acc)
     ; (setq directive1 (string-append "~8," (format nil "~A" (index-digits)) ",0,'*,' F"))
       (setq directive1 "~D")

  ;    (if (member *data-name* '(US.D1B TY.D1B))
  ;        (format output "~%SELL= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  LONG-RISK= ~D~%"
  ;         'OPEN (round (- ;(convert-to-32 risk)
  ;         'OPEN  (round (* risk (calculate-point-value tdate))));(convert-to-32 risk))
  ;       (format output
  ;          (string-append "~%SELL= OPEN    SHORT-RISK= " directive1 "~%   BUY= OPEN  LONG-RISK= " directive1 "~%")
  ;          (round (* risk (calculate-point-value tdate))) (round (* risk (calculate-point-value tdate)))            
   ;         ))
         
 
    (setq oec-symbol (make-oec-symbol *data-name* tdate)
;           pacific-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time;
;	   pacific-end-session-time "default" 
 ;          pacific-cancel-time
  ;             (string-append (date-convert date1) " "
   ;                           (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
      (setq
           central-entry-time "default" 
	   central-end-session-time "default" 
           central-cancel-time
                (string-append (date-convert date1) " "
                               (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter)) ;;;(add-minutes1 exit-time -30))
      (format T "~%~A  ~A" *data-name* action)
     (cond 
             ((equal action "NOT SHORT")
           ;   (write-xml-record tdate "tblTradeRecs" 'MINI 'LONG date1 entry-long stop-long cover-long output1)
            ;   (write-edge-record edge-path 'Long tdate)   (write-edge sample-edge-path 'long tdate)          
  ;             (setq offset (random-choice -1 0)
  ;               pacific-exit-time
  ;               (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
       
       ;        (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
        ;                        pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
     
                (setq direction 'BUY)  
               (when (member *data-name* *micro-list*)
              ;  (write-fore cannon-fore-path3 oec-symbol *cannon-fore-block-acct* *cannon-fore-qty* direction risk tdate
              ;     pacific-exit-time)
                (push (list *data-name* pf-longs ninja-fore-path3 time-zone tdate date1 'BUY 0 0) *nt-pf-list*)
                (push (list *data-name* pf-longs ts-fore-path3 time-zone tdate date1 'BUY 0 0) *ts-pf-list*)
              
                ;  (with-open-file (ninja-output ninja-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create);
	      
               ;           (write-nt-edge-record ninja-output time-zone tdate date1 'BUY 0 0 ))
        
                );;;closes the when
             ;  (if (member *data-name* *cfd-list*)
              ;  (write-fore ifm-cfd-path3 oec-symbol *ifm-cfd-block-acct* *ifm-cfd-qty* direction risk tdate
               ;    pacific-exit-time))
             ;  (if (member *data-name* *forex-list*)
             ;   (write-fore c2-fore-path3 oec-symbol *c2-fore-block-acct* *c2-fore-qty* direction risk tdate
             ;      pacific-exit-time))
              ; (if (member *data-name* *score-list*)
              ;     (write-edge edge-path  'LONG date1))
        
                )

             ((equal action "NOT LONG")
            ;  (write-xml-record tdate "tblTradeRecs" 'MINI 'SHORT date1 entry-short stop-short cover-short output1)
             ;  (write-edge-record edge-path 'short tdate)  (write-edge sample-edge-path 'short tdate)                      
   ;           (setq offset (random-choice -1 0)
   ;                 pacific-exit-time
   ;                 (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
          ;       (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
          ;                         pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
              (setq direction 'SELL)
               (when (member *data-name* *micro-list*)
    ;                 (write-fore cannon-fore-path3 oec-symbol *cannon-fore-block-acct* *cannon-fore-qty* direction risk tdate
     ;                  pacific-exit-time)
                     (push (list *data-name* pf-shorts ninja-fore-path3 time-zone tdate date1 'SELL 0 0) *nt-pf-list*)
                     (push (list *data-name* pf-shorts ts-fore-path3 time-zone tdate date1 'SELL 0 0) *ts-pf-list*)
              ;       (with-open-file (ninja-output ninja-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	        	    
               ;              (write-nt-edge-record ninja-output time-zone tdate date1 'SELL 0 0 ))
                      )
             ;  (if (member *data-name* *cfd-list*)
             ;   (write-fore ifm-cfd-path3 oec-symbol *ifm-cfd-block-acct* *ifm-cfd-qty* direction risk tdate
             ;      pacific-exit-time))
             ;  (if (member *data-name* *forex-list*)
              ; (write-fore c2-fore-path3 oec-symbol *c2-fore-block-acct* *c2-fore-qty* direction risk tdate
               ;    pacific-exit-time))
       
              ;  (if (member *data-name* *score-list*)
              ;      (write-edge edge-path  'SHORT date1))
            
               ));;closes clause the cond
      )) ;;;closes the let and the defun





;;;;;;;;;;FORE FOREX from here on down

;;;
(defun update-daytrade-warehouse4x (date  &optional (markets *forex-warehouse-list*))
  
   (maind-x)(set-cat-list)
   (setq *stop-loss-4x* .764)
   (dolist (ith markets)
        (set-market ith)
       (populate-day-trades4x ith date (min 8000 (available-days ith date 500))))

      (build-daytrade-warehouse4x markets)
      (setq *day-features4x* (find-best-indicator-set4ax))
      (portfolio-simulation3 '(day4x) date 8000 (list markets))
      (indicator-study4x)
)

(defun populate-day-trades4x (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short  date-1  risk reward ;entry-short entry-long
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
          entry-long entry-short can wppf rsi2xh rsi2xl dirp ccis
         vri428 vri3 rri10 mcd615 gsi tkcdi eprci roc220 pivw pivm proj projd
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades4x.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

    (if (and num (> num (available-days market date2 500 ))) (setq num nil))
    (ifn num (setq num (available-days market date2 500)))

 (format outfile "~%~A~%" market)
 
 (setq  *max-day-risk* 500;.6667)  ;;;1.0557)
          *stop-loss-4* .764 *objective-reward* 1.618) ;.944)

  (if (member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 0)
       (setq *commission* 40 *pips-slippage* 0))

 
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
 
;   (multiple-value-setq (stop-long stop-short)(vprices date 4 *stop-loss-day* 1))

   (setq risk (volatility date 21 *stop-loss-4x*) reward (volatility date 21 *objective-reward*))
 
  (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction date 2))
  (setq vri428 (volatility-ratio-index date 4 28 1) vri3 (volatility-ratio-index date 3 63 1)
         rri10 (ep-roc-index10 date 10) mcd615 (macd-index1 date 6 15 3)
         gsi (gann-slope-index date 5 63) tkcdi (tkcd-index date) ccis (cci-direction date 20)
         can (candle-composite date 3)  wppf (wpp-fore date)
         eprci (ep-roc-change-index date 3 10) roc220 (ep-roc-index10 date 2)
         pivw (pivot-turn date 'week) pivm (pivot-turn date 'month)
         proj (lproj-index date 5) projd (lprojdelta date 5) 
         )
  ;  (setq css (cci-direction date 20 3))
  ;  (setq css (cond ((plusp css) 'LONG)
  ;                  ((minusp css) 'SHORT)
  ;                  (t NIL))) 

   (setq   date-1 date date (add-mkt-days date 1))


   (setq entry-long  (my-pretty-price (getd date 'open)) ;(max (getd date-1 'close)(getd date 'open))
         entry-short (my-pretty-price (getd date 'open))) ;(min (getd date-1 'close)(getd date 'open)))
        ; stop-short  (+ ;(getd date 'open)
        ;               (getd date-1 'close) 
        ;                 risk)
        ; stop-long   (- ;(getd date 'open)
        ;                 (getd date-1 'close)
        ;                risk))

     
  ;;;check if new short entry


;;;exit at the close

   (when (and (not short) (>= (getd date 'high) entry-short)
              (or (= can -1)(= wppf -1))
              (= ccis -1) 
 #|              (/= vri428 -4)(/= vri3 -4)
               (/= rri10 -4)(/= mcd615 5)  
               (/= tkcdi 5)(/= gsi -1) (/= eprci -4)
               (/= roc220 -5)(not (member pivw '(L H)))
               (neql pivm 'R0)(not (member proj '(5 -5)))
               (/= projd 9)          
 |#        
     
              (>= (* risk (calculate-point-value date)) 10) ;;to avoid bad data
	      (<= (* risk (calculate-point-value date)) *max-day-risk*)
              )
          (setq short entry-short
                stop-short (my-pretty-price (+ short risk))
                cover-short (my-pretty-price (- short reward))
                short-trade (create-daytrade-entry-record-list4x date-1 -1 short)
                 ))


    (when (and  (not long) (<= (getd date 'low) entry-long)
                (or (= can 1)(= wppf 1))
                (= ccis 1)
#|               (/= vri428 4)(/= vri3 4)
               (/ rri10 4)(/= mcd615 -5)
               (/= tkcdi -5)(/= gsi 1)(/= eprci 4)
               (/= roc220 5)(not (member pivw '(L H)))
               (neql pivm 'S0)(not (member proj '(5 -5)))
               (/= projd -9)
|# 
 
               (>= (* risk (calculate-point-value date)) 10)
	       (<= (* risk (calculate-point-value date)) *max-day-risk*)
               )
           (setq long entry-long
                 stop-long (- long risk) cover-long (+ long reward)
                 long-trade (create-daytrade-entry-record-list4x date-1 1 long)

                 )) ;(format T "long-trade= ~A ~%" long-trade) )


;;;check if exited with a target

     (when (and long cover-long (> (getd date 'high) cover-long))
          
            (push (- (* (- cover-long long)(calculate-point-value date))(comm+slip date)) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date) (- cover-long long)))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil cover-long nil))

      (when (and short cover-short (< (getd date 'low) cover-short))
        
           (push (- (* (- short cover-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date) (- short cover-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil cover-short nil))

 ;;;check if stopped out on same day

   (when (and long stop-long
            ;   (or (and (<= (getd date 'low) stop-long)
            ;            (>= (getd date 'open) (getd date 'close)))
            ;       (<= (getd date 'close) stop-long))
              (<= (getd date 'low) stop-long)
               )
           (push (- (* (- stop-long long)(calculate-point-value date))(comm+slip date)) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date) (- stop-long long)))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
            ;   (or (and (>= (getd date 'high) stop-short)
            ;            (<= (getd date 'open) (getd date 'close)))
            ;        (>= (getd date 'close) stop-short))
               (>= (getd date 'high) stop-short)      
              )
           (push (- (* (- short stop-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date) (- short stop-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd date 'close))
            (push (- (* (- cover-long long)(calculate-point-value date))(comm+slip date)) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date) (- cover-long long)))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- (* (- short cover-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date) (- short cover-short)))))))
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
(defun create-daytrade-entry-record-list4x (date direction entry)
   (let* ( ); (date-1 (getd date 'ydate)))

       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                           ;   (my-round (/ (volatility date 4 1) (volatility date 63 1)) 3) ;;;feature 2
                              (ep-roc-index date 5 ) ;;feature 2*
                              (ep-roc-change-index date 5 2);;#3*                             
                              (counter-swing-signal date) ;feature 4*  5 levels                                 
                              (wpp date);;feature  5 
                              
                              (wpp-index date) ;feature 6  8 levels
                              (accel-direction date 5 2);;feature 7*
                                                          
                              (rsi2-index date);#8*
                           
                              (day-bar-type date ) ;#9*                              
                              (channel-direction date 21) ;feature 10  
                                                       
                              (rsi2x-direction date 3)    ;;  feature 11  9 levels
                              (ep-roc-index date 2) ;; feature 12                 
                              
                              (volatility-ratio-index date 1 21);;;feature 13          
                                                   
                              (lproj-index date 5) ;;features 14 5 levels
                              );;;closes the list

))

;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-daytrade-indicators4x ()
   (let ((path (string-append *upper-dir-warehouse* "daytradewarehouse4x.dat"))
          date-1 date-2)
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate)  date-2 (getd date-1 'ydate))

    ; (setf (svref ith 4) (wpp-fx-composite date-1));;feature 2
    ; (setf (svref ith 8)(pivot-turn-fx-week  date-1 )) ;;feature 6
    ; (setf (svref ith 9)(accel-signal  date-1 5 2)) ;;;feature 7
    ; (setf (svref ith 16) (roc-rel-index date-1 12 84));;feature 14
  ;   (setf (svref ith 16) (day-bar-type2 date-2 8));;feature 14

;      (setf (svref ith 13) (day-bar-type2 (getd date-1 'ydate)));;feature 11

 ;    (setf (svref ith 6)(2day-change-index date-1));;feature 12
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


(defun encode-day-trades4x (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction

;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
      (2 (push (svref record 4) bin-list))

;;;;Feature 3 with 9 levels bar type
      (3 (push (svref record 5) bin-list))

;;;;Feature 4 with 6 levels is the stochastic with parameter 21
      (4 (push (svref record 6) bin-list))
 
;;;;Feature 5 is a roc-index date 5 84
       (5 (push (svref record 7) bin-list))
;;;Feature 6 is slow-stochastic-index  date 10 
       (6 (push (svref record 8) bin-list))
;;;Feature 7 is Gann-slope-index 5 63
       (7 (push (svref record 9) bin-list))
;;;Feature 8 is rsi-index 14
       (8 (push (svref record 10) bin-list))
 ;;Feature 9 is range-index 6 levels
       (9 (push (svref record 11) bin-list))

;;;Feature 10 with 5 levels 2-bar index date
       (10  (push (svref record 12) bin-list))
;;; feature 11 is the day-bar-type2 date-1
       (11 (push (svref record 13) bin-list))
;;Feature 12 day-bar-type date-1 
       (12  (push (svref record 14)  bin-list))

 ;;;;Feature 13 with 5 levels macd-index 5 13 3
       (13 (push (svref record 15) bin-list))

;;;;Feature 14 macd-index 12 26 9
       (14  (push (svref record 16) bin-list))

                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))

#|
;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-daytrades4bx (date features)
  (let (record bin (result 0) (counter 0) contents epsignal
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list4x date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

   (dotimes (ith 4)
    (setq bin (encode-day-trades4x record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
;    (remove-if #'(lambda(s1) (and (eql *data-name* (svref s1 0))
;                                  (eql nxdate (svref s1 1)))) contents))

;    (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
   (if contents (return) (setq features (butlast features)))
;    (if (or (>= (count-if #'(lambda (s) (= (svref s 2) 1)) contents) 1)
;            (>= (count-if #'(lambda (s) (= (svref s 2) -1)) contents) 1))
;        (return) (setq features (butlast features)))
);;closes the dotimes
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

  (cond ((and (plusp results-long)(plusp results-short)
              (>= (float (/ num-winners-long longs)) .8)(>= (float (/ num-winners-short shorts)) .8)) (setq epsignal 'OK))
        ((and (plusp results-long)(>= (float (/ num-winners-long longs)) .8)) (setq epsignal 'UP))
        ((and (plusp results-short)(>= (float (/ num-winners-short shorts)) .8))(setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))

|#
;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-daytrades4bx (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list4x date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-day-trades4x record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
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


  (cond ((not contents) (setq epsignal 'UNIQUE))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
      ;  ((and (plusp results-long)(plusp results-short)
      ;        (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
      ;        (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
      ;        (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
      ;        (setq epsignal 'OK))
        ((and (plusp results-long)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)
              (>= (/ gains-long longs) (if (zerop shorts) 0 (/ gains-short shorts))))
              (setq epsignal 'UP))
        ((and (plusp results-short)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc)
              (>=  (/ gains-short shorts)(if (zerop longs) 0 (/ gains-long longs))))
              (setq epsignal 'DOWN))        
        
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))




;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-daytrades4x (date features)
  (let (record bin (result 0) (counter 0) contents epsignal(crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list4x date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

    (setq bin (encode-day-trades4x record features))
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
        (incf num-winners-long) (setq gains-long (+ gains-long (svref kth 19))))
     (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

   ; (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
    ;   (incf num-winners-long))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short)(setq gains-short (+ gains-short (svref kth 19))))
      (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

;     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
;        (incf num-winners-short))


        );;;closes dolist over contents
  (setq result 0)
  (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

;  (cond ((and (plusp results-long)(plusp results-short)) (setq epsignal 'OK))
;        ((plusp results-long) (setq epsignal 'UP))
;        ((plusp results-short)(setq epsignal 'DOWN))
;        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
;        ((not contents) (setq epsignal 'UNIQUE)))


  (cond ((not contents) (setq epsignal 'UNIQUE))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))

        ((and (plusp results-long)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)
              (>= (/ gains-long longs) (if (zerop shorts) 0 (/ gains-short shorts))))
              (setq epsignal 'UP))
        ((and (plusp results-short)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc)
              (>=  (/ gains-short shorts)(if (zerop longs) 0 (/ gains-long longs))))
              (setq epsignal 'DOWN))        
        
        (t (setq epsignal 'AVOID)))


  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))



(defun daytrade-simulation-test4x (market date2 num &optional (features nil))
 (let (date trades long short cover-long cover-short  record trading-dates entry-long entry-short
        date-1 
       epsignal longs long-gains shorts short-gains
       (running-sum 0)  risk reward singles  ;css
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short draw
       long-acc short-acc  bin  
        gsi mcd615 pivm pivw proj projd roc220 rri10 tkcdi vri3 vri428 eprci
      ; (path-in (string-append *config-dir* "day-features4x.dat"))
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary4x.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation4x.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary4x.dat")))

       
      (set-market market) (format T "~%~A~%" *data-name*)
    
     (if (and num (> num (available-days market date2 500))) (setq num nil))
     (ifn num (setq num (available-days market date2 500)))

   (setq *stop-loss-4x* .764 *max-day-risk* 500 *objective-reward* 3.618);.944) ;.6667)

  (if (member market *forex-warehouse-list*)
       (setq *commission* 0 *min-fore-expected-value* 12 *pips-slippage* 6)
     (setq *commission* 25 *min-fore-expected-value* 100 *pips-slippage* 0))

 
  
   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

  (build-daytrade-warehouse4x *forex-list*);(setq features *day-features4x*)
 ; (when (and (not features)(member market (union *day-list* *forex-list*)))
           
 ;            (build-daytrade-warehouse4x (list market) )
             
  ;           (setq features (find-best-indicator-set4ax) grp (list market)))
 
  (setq singles (nth-value 3 (apply #'day-trade-bins4bx features)))
  
 ;;;;from date1 to date2
 (dotimes (ith num)

 ;(format T "Market= ~A date=~A~%" *data-name* date)

 (setq vri428 (volatility-ratio-index date 4 28 1) vri3 (volatility-ratio-index date 3 63 1)
         rri10 (ep-roc-index10 date 10) mcd615 (macd-index1 date 6 15 3)
         gsi (gann-slope-index date 5 63) tkcdi (tkcd-index date) ;css (counter-swing-signals date)
         eprci (ep-roc-change-index date 3 10) roc220 (ep-roc-index10 date 2)
         pivw (pivot-turn date 'week) pivm (pivot-turn date 'month)
         proj (lproj-index date 5) projd (lprojdelta date 5) 
         )


   (setq reward (volatility date 21 *objective-reward*) ;css (counter-swing-signals date)
         risk  (volatility date 21 *stop-loss-4x*)); css (parabolic-stops date))

   (setq  date-1 date date (add-mkt-days date 1))

   (setq record (vector date (getd date 'close) 0 0 0))

   (setq entry-long (my-pretty-price (getd date 'open)) ;(max (getd date-1 'close)(getd date 'open))
         entry-short (my-pretty-price (getd date 'open))) ;(min (getd date-1 'close)(getd date 'open))) 
;         stop-short  (my-pretty-price (+  ; (getd date 'open)
;                                        (getd date-1 'close)                      
;                                       risk))
;         stop-long   (my-pretty-price (-  ;(getd date 'open) 
;                                       (getd date-1 'close)                                       
;                                        risk)))


;;;;calculate bin-classifier only as needed
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades4bx date-1 features))

      ;  (format t "~%date = ~A  epsignal = ~A" date epsignal)     
;;;check if new entry

   (when (and (not short)(<= (getd date 'low) entry-short)
              (member epsignal '( DOWN))
#|             ;  (eql css 'LONG)        
               (/= vri428 -4)(/= vri3 -4)
               (/= rri10 -4)(/= mcd615 5)  
               (/= tkcdi 5)(/= gsi -1) (/= eprci -4)
               (/= roc220 -5)(not (member pivw '(L H)))
               (neql pivm 'R0)(not (member proj '(5 -5)))
               (/= projd 9)          
         
              (>= (* risk (calculate-point-value date)) 10)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
            ;  (> (/ short-gains shorts) (* 1.2 risk (calculate-point-value date)))
|#                        
                 )
          (setq short entry-short
              stop-short (+ short risk) cover-short (- short reward)
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)
           (setf (svref record 3) (- (svref record 3) (+ (/ *commission* (calculate-point-value date-1))
                                                         (* *pips-slippage* (index-tick-size)))))
                 )


    (when (and (not long)(>= (getd date 'high) entry-long)
               (member epsignal '( UP))
  #|            ; (eql css 'SHORT)

               (/= vri428 4)(/= vri3 4)
               (/= rri10 4)(/= mcd615 -5)
               (/= tkcdi -5)(/= gsi 1)(/= eprci 4)
               (/= roc220 5)(not (member pivw '(L H)))
               (neql pivm 'S0)(not (member proj '(5 -5)))
               (/= projd -9)
                   
               (>= (* risk (calculate-point-value date)) 10)
               (<= (* risk (calculate-point-value date)) *max-day-risk*)
              ; (> (/ long-gains longs) (* 1.2 risk (calculate-point-value date)));need to expect to make more than to lose
|#
               )

           (setq long  entry-long
                 stop-long (- long risk) cover-long (+ long reward)
                 long-trade (list date 'long long)
                 )
           (setf (svref record 2) 1)
           (setf (svref record 3) (- (svref record 3)(+ (/ *commission* (calculate-point-value date-1))
                                                        (* *pips-slippage* (index-tick-size)))))
                 )


;;;;check if exit based on target
   (when (and long cover-long (> (getd date 'high) cover-long)
           )
           (push (- (* (- cover-long long)(calculate-point-value date))(comm+slip date)) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'O
                                                           cover-long (my-pretty-price (- cover-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
           (setq long-trade nil long nil stop-long nil cover-long nil
           ))

    (when (and short cover-short (< (getd date 'low) cover-short)    
             )
           (push (- (* (- short cover-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'O
                                                          cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short stop-short)))
           (setq short-trade nil short nil stop-short nil cover-short nil
           ))

 ;;;check if stopped out on same day

   (when (and long stop-long
            ;  (or (and (<= (getd date 'low) stop-long)
            ;           (>= (getd date 'open) (getd date 'close)))
            ;       (<= (getd date 'close) stop-long))
               (<= (getd date 'low) stop-long)
           )
           (push (- (* (- stop-long long)(calculate-point-value date))(comm+slip date)) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'S
                                                           stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
            ;   (or (and (>= (getd date 'high) stop-short)
            ;            (<= (getd date 'open) (getd date 'close)))
            ;        (>= (getd date 'close) stop-short))
             (>= (getd date 'high) stop-short)    
             )
           (push (- (* (- short stop-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                          stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short stop-short)))
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (getd date 'close))
            (push (- (* (- cover-long long)(calculate-point-value date))(comm+slip date)) trades)
            (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3) (- (getd date 'close) long)))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- (* (- short cover-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short (getd date 'close))))
           (setq short-trade nil short nil stop-short nil))

      (setf (svref record 3)(round (* (calculate-point-value date-1)(svref record 3))))
      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes

     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
          (svref ith 0) (svref ith 1) (svref ith 2) (svref ith 3)
          (svref ith 4))
     ))


  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) 
          )
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= OPEN~%" (length daytrades))
   
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLES RATIO = ~5,3,0,'*,' F~%" *stop-loss-4* singles)
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  %DAYS IN TRADE=    ~F    COMMISSION=        ~D~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED OUT=    ~,1,2,'*,' F%"

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
      (my-round (* 100 (/ (length trades) num)) 1) *commission*
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
       (/ (count 'S (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1))
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,2,'*,' F%"
           (/ (/  (list-sum trades)
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) )


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

     (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3)) (svref ith 8))))
        ));;;closes the with-open-file
  );;;closes the 

  (values (round (list-sum trades))
          (length trades) trades)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun day-trade-bins4bx ( &rest features)
  (let (bin path)

  ;  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4.dat"))
  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4x.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
     (setq bin (encode-day-trades4x record (butlast features ith)))

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


(defun day-trade-bins4ax ( &rest features)
  (let (bin path)

  ;  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4.dat"))
  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4x.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades4x record features))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
              (push bin day-bin-codes))))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)


  ))



(defun day-trade-bins4x ( &rest features)
  (let (bin path)

  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4x.dat"))
;  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades4x record features))
     (pushnew bin day-bin-codes :test #'equal)
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*)
              (list record)))))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)


  ))

(defun build-daytrade-warehouse4x (markets  &optional (nxdate 99999999))
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse4x.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse4x.backup"))
           (delete-file (string-append *upper-dir-warehouse* "daytradewarehouse4x.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse4x.dat"))
          (rename-file (string-append *upper-dir-warehouse* "daytradewarehouse4x.dat")
                            "daytradewarehouse4x.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "daytrades4x.dat")) 
    (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record trades)
          )))
    (setq trades (remove-if #'(lambda(s1)(<= nxdate (svref s1 1))) trades))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun



(defun add-day-trades4x (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse4x.dat")) trades)

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



;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun daytrades-add-one-in4ax (base-features candidate-features)
  (let (winners-list (result 0) average-profit rtb rp single-bins csgof)

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb  average-profit single-bins rp csgof)
           (apply #'day-trade-bins4ax (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
   ; (vsort winners-list #'> 'seventh)
   (vsort winners-list #'> 'first)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

#|
;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun daytrades-add-one-in4x (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'day-trade-bins4x (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))
|#

;;;requires a base features list
(defun daytrades-leave-one-out4ax (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp csgof)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
                    (apply #'day-trade-bins4ax (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))

    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))



#|
;;;requires a base features list
(defun daytrades-leave-one-out4x (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'day-trade-bins4x (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))
|#

;;;;this function expects that you have run (day-trade-bins ...) already
(defun display-daytrade-bins-by-expected-value4x ()
  (let (contents expected-value-list result (path1 (string-append *output-upper-dir* "daytrade-expected-value4x.dat")))
   (dolist (ith day-bin-codes)
     (setq contents
           (gethash ith *day-trade-warehouse3*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (svref kth 19))))
     (setq expected-value-list (cons (list (/ result (length contents)) ith (length contents) result) expected-value-list)));;;closes the dolist over bin-codes
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (vsort expected-value-list #'> 'car)
         (format stream "Profit/trade  Bin      NUM   Profit~%~%")
         (dolist (jth expected-value-list)
             (format stream "~5D        ~10A  ~4D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (round (fourth jth))))
         (format stream "~%Trades = ~D   Profit = ~D"
             (list-sum (mapcar #'(lambda(s) (nth 2 s)) expected-value-list))
             (round (list-sum (mapcar #'(lambda(s) (nth 3 s)) expected-value-list))))
                ) ;;;;closes the with-open-file
 ))


(defun find-best-indicator-set4ax (&optional (candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14)))
  (let ((base-list '(1)) winners-list (result 0)); tdate best-features)

  ;  (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (daytrades-add-one-in4ax base-list candidate-list))
;;;;first value is the net profit for winning bins
;;;second value is the net gain/loss for all trades
;;;third value is the average profit per trade in winning bins
;;;fourth value is the fraction of single bins
;;;fifth is the ratio of bin profits to all winners
;;;sixth is the chi-squared value for the features
     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)(> result .80)
             (< (fifth (car winners-list)) 2.9)
             (> (fourth (car winners-list)) .10))
         (return))
         
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 3.00)(return))
        (if (> (fifth (second winners-list)) 3.00)(return)))

      (setq winners-list (daytrades-leave-one-out4ax base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))

         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
   base-list 
    ))
 
;;;this is for the FORE product 
(defun find-best-daytrade4x (tdate &optional (output T)(date1 nil))
  (let (risk     direction
        trade-direction  ; pacific-entry-time pacific-exit-time
;        pacific-cancel-time pacific-end-session-time
        central-entry-time ;central-exit-time
         central-cancel-time central-end-session-time
        action directive1 cannon-fore-path3 c2-fore-path3 ninja-forex-path3 ts-fore-path2
        oec-symbol oco-code  (time-zone "PT")
        offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))

  (if (member *data-name* *forex-warehouse-list*)
      (setq *commission* 0 *min-fore-expected-value* 12 *pips-slippage* 6)
     (setq *commission* 50 *min-fore-expected-value* 100 *pips-slippage* 0))
 
    (setq  *stop-loss-4x* .382 );.6667)
  
    (setq cannon-fore-path3 (string-append *daily-output-dir* "cannon-fore.txt")
          c2-fore-path3 (string-append *daily-output-dir* "c2-fore.txt")
          ts-fore-path2 (string-append *daily-output-dir* "ts-edge-" (format nil "~A" date1) ".csv")
          ninja-forex-path3 (string-append *ninja-output-dir* "ninja-forex-" (format nil "~A" date1) ".csv"))

  (format T "~%EPSIGNAL = ~A~%  longs = ~A  long-gains = ~A ~%" epsignal longs long-gains )
  (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains )

    (setq risk (volatility tdate 21 *stop-loss-4x*))

    (if (member epsignal '( UP))
        (push 'UP trade-direction)(push 'FT trade-direction))
    (if  (member epsignal '( DOWN))
        (push 'DN trade-direction)(push 'FT trade-direction))

        
 ;      (if (member *data-name* '(US.D1B TY.D1B))
 ;              (setq risk (convert-to-decimal (convert-to-32 risk)))
 ;          (setq risk  (* (index-tick-size)
 ;                                (round risk (index-tick-size)))))
                               
   
  ;   (when (if (member *data-name* '(US.D1B TY.D1B))
  ;             (>  (* risk (index-point-value))*max-day-risk*)
  ;           (>  (* risk (calculate-point-value tdate)) *max-day-risk*)) (push "NOT TODAY" action))

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

      (format output " ~A~%" action)
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~A~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~% SHORT-RISK= ~D~%  LONG-RISK= ~D~%"
            (convert-to-32 risk)   (convert-to-32 risk))
         (format output
            (string-append "~%SELL= OPEN    SHORT-RISK= " directive1 "~%   BUY= OPEN  LONG-RISK= " directive1 "~%")
            risk   risk            
            ))
     
 
    (setq oec-symbol (make-oec-symbol *data-name* tdate)
         ;  pacific-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
	 ;  pacific-end-session-time "default" 
         ;  pacific-cancel-time
         ;       (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
      (setq
           central-entry-time "default" 
	   central-end-session-time "default" 
           central-cancel-time
                (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter)) ;;;(add-minutes1 exit-time -30))
      (format T "~%~A  ~A" *data-name* action)
     (cond 
             ((equal action "NOT SHORT")
           ;   (write-xml-record tdate "tblTradeRecs" 'MINI 'LONG date1 entry-long stop-long cover-long output1)
            
               (setq offset (random-choice -1 0))
               ;  pacific-exit-time
               ;  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
       
       ;        (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
        ;                        pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
     
                (setq direction 'BUY)  
              ; (if (member *data-name* *fore-list*)
              ;  (write-fore cannon-fore-path3 oec-symbol *cannon-fore-block-acct* *cannon-fore-qty* direction risk tdate
              ;     pacific-exit-time))
              ; (when (member *data-name* *forex-list*)
              ;  (write-fore c2-fore-path3 oec-symbol *c2-fore-block-acct* *c2-fore-qty* direction risk tdate
              ;     pacific-exit-time)
       ;        (with-open-file (ninja-output ninja-forex-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	;        	 (write-ninja-record1 ninja-output time-zone tdate date1 'BUY 0 0 risk))  

              (with-open-file (ninja-output ts-fore-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	  ;   (write-ts-record ninja-output  time-zone tdate date1 'BUY 0.0 risk *striker-score-qty*))
                         (write-ts-record ninja-output  time-zone tdate date1 'BUY 0.0 risk ))
                  
                  
       
                )

             ((equal action "NOT LONG")
            ;  (write-xml-record tdate "tblTradeRecs" 'MINI 'SHORT date1 entry-short stop-short cover-short output1)

              (setq offset (random-choice -1 0))
              ;   pacific-exit-time      ;  (string-append (date-convert date1) " "
              ;              (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
          ;       (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
          ;                         pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
             ; (setq direction 'SELL)
              ; (if (member *data-name* *forex-warehouse-list*)
               ;(write-fore cannon-fore-path3 oec-symbol *cannon-fore-block-acct* *cannon-fore-qty* direction risk tdate
                ;   pacific-exit-time))
               (when (member *data-name* *forex-warehouse-list*)
               ;(write-fore c2-fore-path3 oec-symbol *c2-fore-block-acct* *c2-fore-qty* direction risk tdate
                ;   pacific-exit-time)
             ;  (with-open-file (ninja-output ninja-forex-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	      ;  	 (write-ninja-record1 ninja-output time-zone tdate date1 'SELL 0 0 risk))
               (with-open-file (ninja-output ts-fore-path2 :direction :output :if-exists :append :if-does-not-exist :create)
                 	    ; (write-ts-record ninja-output  time-zone tdate date1 'SELL 0.0 risk *striker-score-qty*))
                             (write-ts-record ninja-output  time-zone tdate date1 'SELL 0.0 risk))
                
                 )
       
            
               ));;closes clause the cond
      )) ;;;closes the let and the defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;EQUITES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun populate-day-trades4s (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short  date-1  risk
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades4s.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

  (format outfile "~%~A~%" market)
  (setq  *max-day-risk* 2000)  ;;;1.0557)
  (setq *stop-loss-day* 1.125); .944)


  (if (member market *forex-list*)(setq *commission* 0 *pips-slippage* 6)
       (setq *commission* 50 *pips-slippage* 0))

  (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))

   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
 ;  (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
;   (multiple-value-setq (stop-long stop-short)(vprices date 4 *stop-loss-day* 1))
 ;  (setq risk (* .5 (- stop-short stop-long)))
   (setq risk (volatility date 4 *stop-loss-day*))

   (setq   date-1 date date (add-mkt-days date 1))
 ;  (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
 ;   ))
 ;  (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))
 ;   ))
 ;  (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))
;;;check if new long entry
;;;enter on a stop order if opens below entry-stop.
;;;enter on a limit order of open above the entry-stop
;;;exit at the close

   (when (and 
              (>= (* risk (calculate-point-value date-1)) 20) ;;to avoid bad data
	      (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
              )
          (setq short (getd date 'open) ;entry-short
                stop-short (+ short risk)
                short-trade (create-daytrade-entry-record-list4s date-1 -1 short)
                 ))


    (when (and
               (>= (* risk (calculate-point-value date-1)) 20)
	       (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
               )
           (setq long (getd date 'open) ;entry-long
                 stop-long (- long risk)
                 long-trade (create-daytrade-entry-record-list4s date-1 1 long)

                 )) ;(format T "long-trade= ~A ~%" long-trade) )


 ;;;check if stopped out on same day

   (when (and long stop-long
              (<= (getd date 'low) stop-long)
               )
           (push (- (* (- stop-long long)(calculate-point-value date))(comm+slip date)) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date) (- stop-long long)))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (>= (getd date 'high) stop-short)      
              )
           (push (- (* (- short stop-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date) (- short stop-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd date 'close))
            (push (- (* (- cover-long long)(calculate-point-value date))(comm+slip date)) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date) (- cover-long long)))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- (* (- short cover-short)(calculate-point-value date))(comm+slip date)) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date) (- short cover-short)))))))
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




(defun day-trade-bins4bs ( &rest features)
  (let (bin path)

  ;  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades4.dat"))
  (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4s.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
     (setq bin (encode-day-trades4s record (butlast features ith)))

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

(defun update-daytrade-warehouse4s (date  &optional (markets *stocks-warehouse-list* ))
  (maind-x)(set-cat-list)

   (dolist (ith markets)
        (set-market ith)
       (populate-day-trades4s ith date (min 6000 (available-days ith date 750))))
    (build-daytrade-warehouse4s markets) (display-num-trades-by-market3 daytrades)
    (setq *day-features4s* (find-best-indicator-set4as))
    (setq markets (remove-if #'(lambda (s) (member s '(kmi.d3b fb.d3b ete.d3b grpn.d3b pbr.d3b))) markets)) 
    (portfolio-simulation3 '(day4s) date 3000 (list markets))
)

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun daytrades-add-one-in4as (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'day-trade-bins4as (append base-features (list ith))))
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
(defun daytrades-leave-one-out4as (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'day-trade-bins4as (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))

(defun daytrade-simulation-test4s (market date2 num &optional (features *day-features4s*))
 (let (date trades long short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk 
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw ignore singles
     ;  (path-in (string-append *config-dir* "day-features4.dat"))
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary4s.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation4s.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary4s.dat")))

 ;  (declare (ignore long-acc short-acc))
   (set-market market) (format T "~%~A~%" *data-name*)
;;;using .847 gives the same stop loss price as STARTER and SCORE.
;;;Only difference is that the entry is a little earlier
  
    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

   (setq *stop-loss-day* 1.125 ;.944  ;.6667
         *max-day-risk* 2000)

  (if (member market *forex-list*)(setq *commission* 0 *min-fore-expected-value* 20 *pips-slippage* 6)
     (setq *commission* 50 *min-fore-expected-value* 100 *pips-slippage* 0))
 
   (multiple-value-setq (ignore ignore ignore singles) (apply #'day-trade-bins4bs features))  

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

 ;(format T "Market= ~A date=~A~%" *data-name* date)

   (setq risk (volatility date 4 *stop-loss-day*))

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))


;;;;calculate bin-classifier 
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades4bs date-1 features))
      
       

;;;check if new entry
  
   (when (and               
              (member epsignal '(OK DOWN))(> shorts 0.0)
        ;      (>= (/ short-gains shorts)
        ;          (+  *min-fore-expected-value*
         ;              (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size))))             
             
              (>= (* risk (calculate-point-value date-1)) 20)
              (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
      ;        (>= (/ short-gains shorts)
       ;           (if (> longs 0.0) (/ long-gains longs) 0))
            
                 )
          (setq short (getd date 'open) ;entry-short
                stop-short (+ short risk)
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)
           (setf (svref record 3) (- (svref record 3) (+ (/ *commission* (calculate-point-value date-1))
                                                         (* *pips-slippage* (index-tick-size)))))
   ;          (format T "record1= ~A~%" record) 
             )


    (when (and
               (member epsignal '(OK UP))(> longs 0.0)
          ;     (>= (/ long-gains longs)
           ;        (+  *min-fore-expected-value* (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size))))
	     
          ;     (>= (* risk (calculate-point-value date-1)) 20)
          ;     (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
          ;     (> (/ long-gains longs)
          ;       (if (> shorts 0.0)(/ short-gains shorts) 0))
               )

           (setq long (getd date 'open) ; entry-long
                 stop-long (- long risk)
                 long-trade (list date 'long long)
                 )
           (setf (svref record 2) 1)
           (setf (svref record 3) (- (svref record 3)(+ (/ *commission* (calculate-point-value date-1))
                                                        (* *pips-slippage* (index-tick-size)))))
    ;           (format T "record2= ~A~%" record)
            )


 ;;;check if stopped out on same day

   (when (and long stop-long
               (<= (getd date 'low) stop-long)
           )
           (push (- (* (- stop-long long)(calculate-point-value date-1))(comm+slip date-1)) trades)
     ;      (format T "trade1= ~A point-value= ~A~%" (car trades)(calculate-point-value date-1))
           (setq long-trade
                 (apply #'vector (append long-trade (list date  *data-name* 
                                                          (contract-month *data-name* date) 'S
                                                           stop-long (my-pretty-price (- stop-long long))))))
      ;     (format T "long-trade2= ~A~%" long-trade)
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
       ;    (format T "record3= ~A~%" record)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
             (>= (getd date 'high) stop-short)    
             )
           (push (- (* (- short stop-short)(calculate-point-value date-1))(comm+slip date-1)) trades)
       ;    (format T "trade2= ~A  point-value= ~A~%" (car trades)(calculate-point-value date-1))
           (setq short-trade
                 (apply #'vector (append short-trade (list date  *data-name* 
                                                           (contract-month *data-name* date) 'S
                                                          stop-short (my-pretty-price (- short stop-short))))))
        ;   (format T "short-trade2= ~A~%" short-trade)
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short stop-short)))
 ;          (format T "record4= ~A~%" record)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (getd date 'close))
            (push (- (* (- cover-long long)(calculate-point-value date-1))(comm+slip date-1)) trades)
         ;  (format T "trade3= ~A point-value= ~A~%" (car trades)(calculate-point-value date-1))
            (setq long-trade
                  (apply #'vector (append long-trade (list date  *data-name*
                                                           (contract-month *data-name* date) 'N
                                                           cover-long (my-pretty-price (- cover-long long))))))
          ;  (format T "long-trade3= ~A~%" long-trade)
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3) (- (getd date 'close) long)))
          ; (format T "record5= ~A~%" record)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- (* (- short cover-short)(calculate-point-value date-1))(comm+slip date-1)) trades)
  ;         (format T "trade4= ~A point-value= ~A~%" (car trades)(calculate-point-value date-1))
           (setq short-trade
                 (apply #'vector (append short-trade (list date  *data-name* 
                                                           (contract-month *data-name* date) 'N
                                                           cover-short (my-pretty-price (- short cover-short))))))
    ;       (format T "short-trade4= ~A~%" short-trade)
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short (getd date 'close))))
    ;       (format T "record6= ~A~%" record)
           (setq short-trade nil short nil stop-short nil))

   ;   (format T "record7= ~A~%" record)
      (setf (svref record 3)(round (* (calculate-point-value date-1)(svref record 3))))
      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
   ;   (format T "Running-sum= ~A~%" running-sum)
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes

     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
          (svref ith 0) (svref ith 1) (svref ith 2) (svref ith 3)(svref ith 4))
     ))


  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) 
          )
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= OPEN~%" (length daytrades))
   
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLES RATIO = ~5,3,0,'*,' F~%" *stop-loss-day* singles)
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  %DAYS IN TRADE=    ~F    COMMISSION=        ~D~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED OUT=    ~,1,2,'*,' F%"

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
      (my-round (* 100 (/ (length trades) num)) 1) *commission*
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
       (/ (count 'S (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1))
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,2,'*,' F%"
           (/ (/  (list-sum trades)
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) )


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



(defun build-daytrade-warehouse4s (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse4s.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse4s.backup"))
           (delete-file (string-append *upper-dir-warehouse* "daytradewarehouse4s.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse4s.dat"))
          (rename-file (string-append *upper-dir-warehouse* "daytradewarehouse4s.dat")
                            "daytradewarehouse4s.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "daytrades4s.dat")) 
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


(defun find-best-indicator-set4as ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (daytrades-add-one-in4as base-list candidate-list))
;;;;first value is the net profit for winning bins
;;;second value is the net gain/loss for all trades
;;;third value is the average profit per trade in winning bins
;;;fourth value is the fraction of single bins
;;;fifth is the ratio of bin profits to all winners
;;;sixth is the chi-squared value for the features
     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
     (if (or (not candidate-list)(> result .97))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      (if (< (fourth (car winners-list)) .80)(return))
      (setq winners-list (daytrades-leave-one-out4as base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
     base-list
    ))
 

(defun day-trade-bins4as ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse4s.dat"))

  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades4s record features))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
              (push bin day-bin-codes))))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)


  ))



;;;this is for the FORE product with equities 
(defun find-best-daytrade4s (tdate &optional (output T)(date1 nil))
  (let (risk  ;stop-short stop-long  
       ; direction
        trade-direction  ; pacific-entry-time pacific-exit-time
       ; pacific-cancel-time pacific-end-session-time
       ; central-entry-time ;central-exit-time
       ;  central-cancel-time central-end-session-time
        action directive1 ;cannon-fore-path3 c2-fore-path3 ifm-cfd-path3
        ninja-fore-path3 edge-path ;sample-edge-path
        oec-symbol oco-code  ;(time-zone "PT")
       ; offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc  counter))

  (if (member *data-name* *forex-list*)(setq *commission* 0 *min-fore-expected-value* 0 *pips-slippage* 6)
     (setq *commission* 50 *min-fore-expected-value* 100 *pips-slippage* 0))
 
    (setq  *stop-loss-day* 1.125 ;.944 ;.6667
            *max-day-risk* 2000)
  
    (setq ;cannon-fore-path3 (string-append *daily-output-dir* "cannon-edge-equities.txt")
          edge-path (string-append *daily-output-dir* "edgemailtemplate2s.html")
        ;  sample-edge-path (string-append *daily-output-dir* "sampleedgetemplate.txt")
       ;   c2-fore-path3 (string-append *daily-output-dir* "c2-fore.txt")
        ;  ifm-cfd-path3 (string-append *daily-output-dir* "ifm-cfd.txt")
          ninja-fore-path3 (string-append *ninja-output-dir* "ninja-edge-" (format nil "~A" date1) ".csv"))
     

  (format T "~%EPSIGNAL = ~A~%  longs = ~A  long-gains = ~A ~%" epsignal longs long-gains )
  (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains )
  
    (setq risk (volatility tdate 4 *stop-loss-day*))
    (if (and (member epsignal '(OK UP))(> longs 0)
             (>= (/ long-gains longs)
                 (+  *min-fore-expected-value* (* 2 (calculate-point-value tdate) *pips-slippage* (index-tick-size))))
	     (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
             (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0)))
         (push 'UP trade-direction)(push 'FT trade-direction))
    (if (and (member epsignal '(OK DOWN))(> shorts 0)
             (>= (/ short-gains shorts)
                 (+  *min-fore-expected-value* (* 2 (calculate-point-value tdate) *pips-slippage* (index-tick-size))))
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
             (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0)))
         (push 'DN trade-direction)(push 'FT trade-direction))

   
     
;       (if (member *data-name* '(US.D1B TY.D1B))
;               (setq risk (convert-to-decimal (convert-to-32 risk)))
;           (setq risk  (* (index-tick-size)
;                                 (round risk (index-tick-size)))))
                               
   
;     (when (if (member *data-name* '(US.D1B TY.D1B))
;               (>  (* risk (index-point-value)) *max-day-risk*)
;             (>  (* risk (calculate-point-value tdate)) *max-day-risk*)) (push "NOT TODAY" action))

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

      (format output " ~A~%" action)
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~A~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))
       

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  LONG-RISK= ~D~%"
           'OPEN (convert-to-32 risk) 'OPEN  (convert-to-32 risk))
         (format output
            (string-append "~%SELL= OPEN    SHORT-RISK= " directive1 "~%   BUY= OPEN  LONG-RISK= " directive1 "~%")
            risk   risk            
            ))
         
 
    (setq oec-symbol (make-oec-symbol *data-name* tdate)
;           pacific-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
;	   pacific-end-session-time "default" 
;           pacific-cancel-time
;               (string-append (date-convert date1) " "
;                              (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
      (setq
 ;          central-entry-time "default" 
;	   central-end-session-time "default" 
;           central-cancel-time
;                (string-append (date-convert date1) " "
;                               (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter)) ;;;(add-minutes1 exit-time -30))
      (format T "~%~A  ~A" *data-name* action)
     (cond 
             ((equal action "NOT SHORT")
           ;   (write-xml-record tdate "tblTradeRecs" 'MINI 'LONG date1 entry-long stop-long cover-long output1)
               (write-edge-record edge-path 'Long tdate)             
           ;    (setq offset (random-choice -1 0))
 ;                pacific-exit-time
 ;                (string-append (date-convert date1) " "
 ;                               (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
       
       ;        (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
        ;                        pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
     
             ;   (setq direction 'BUY)  
             ;  (when (member *data-name* *fore-list*)
             ;   (write-fore cannon-fore-path3 oec-symbol *cannon-fore-block-acct* *cannon-fore-qty* direction risk tdate
             ;      pacific-exit-time)
             ;   (with-open-file (ninja-output ninja-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	     ;   	 (write-ninja-record1 ninja-output time-zone tdate date1 'BUY 0 0 risk))
        
                
             ;  (if (member *data-name* *cfd-list*)
             ;   (write-fore ifm-cfd-path3 oec-symbol *ifm-cfd-block-acct* *ifm-cfd-qty* direction risk tdate
             ;      pacific-exit-time))
             ;  (if (member *data-name* *forex-list*)
             ;   (write-fore c2-fore-path3 oec-symbol *c2-fore-block-acct* *c2-fore-qty* direction risk tdate
             ;      pacific-exit-time))
              ; (if (member *data-name* *score-list*)
               ;    (write-edge sample-edge-path  'LONG date1) 
                 );;;closes the clause
                

             ((equal action "NOT LONG")
            ;  (write-xml-record tdate "tblTradeRecs" 'MINI 'SHORT date1 entry-short stop-short cover-short output1)
               (write-edge-record edge-path 'Short tdate)             
        ;      (setq offset (random-choice -1 0)
             ;       pacific-exit-time
             ;       (string-append (date-convert date1) " "
;                                   (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
          ;       (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
          ;                         pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
            ;  (setq direction 'SELL)
            ;   (when (member *data-name* *fore-list*)
            ;         (write-fore cannon-fore-path3 oec-symbol *cannon-fore-block-acct* *cannon-fore-qty* direction risk tdate
            ;           pacific-exit-time)
            ;         (with-open-file (ninja-output ninja-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	    ;    	 (write-ninja-record1 ninja-output time-zone tdate date1 'SELL 0 0 risk))
            ;          )
            ;   (if (member *data-name* *cfd-list*)
            ;    (write-fore ifm-cfd-path3 oec-symbol *ifm-cfd-block-acct* *ifm-cfd-qty* direction risk tdate
            ;       pacific-exit-time))
            ;   (if (member *data-name* *forex-list*)
            ;   (write-fore c2-fore-path3 oec-symbol *c2-fore-block-acct* *c2-fore-qty* direction risk tdate
             ;      pacific-exit-time))
       
              ;  (if (member *data-name* *score-list*)
                ;    (write-edge sample-edge-path  'SHORT date1)
            
               ));;closes clause the cond
      )) ;;;closes the let and the defun




;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-daytrade-entry-record-list4s (date direction entry)
   (let* ((date-1 (getd date 'ydate)))

       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))

                              (r-squared-change-index date 20 1);;feature 2
                              (body-range-index (getd date-1 'ydate)) ;feature 3  9 levels
                          
                              (cloud-index date) ;feature 4
                              (r-squared-change-index date 5 1) ;feature 5  10 levels
                             ; (pivot-index date 2) ;;;feature 6  5 levels
                     ;         (macd-index2 date 6 13 4) ;;feature 6 5 levels
                    
                              (r-squared-change-index date 10 1);;feature 6  8 levels
                              (day-bar-type2 date);;feature 7
                              (body-range-index date) ;feature 8  8 levels
                              (tkcd-index date) ;; feature 9 6 levels
                           ;   (2-bar-index date);;;feature 10  5 levels
                              (gann-slope-index date 5);;feature 10  
                              (ep-roc-change-index date 3 10) ;feature 11  
                           ;   (daytrade-reward-risk date direction) ;;feature 12
                              (body-range-index date-1)      ;;  feature 12  8 levels
                            ;  (mo-diver date 45) ;;feature 13
                              (volatility-ratio-index date 1 14 1) ;;feature 13 5 levels
                              (r-squared-change-index date-1 5 1) ;;features 14 10 levels
                              );;;closes the list

))


(defun encode-day-trades4s (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction
;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
      (2 (push (svref record 4) bin-list))
;;;;Feature 3 with 9 levels bar type
      (3 (push (svref record 5) bin-list))
;;;;Feature 4 with 6 levels is the stochastic with parameter 21
      (4 (push (svref record 6) bin-list))
;;;;Feature 5 is a roc-index date 5 21
       (5 (push (svref record 7) bin-list))
;;;Feature 6 is mo-diver date 5 with 5 levels
       (6 (push (svref record 8) bin-list))
;;;Feature 7 is Gann-slope-index 5 63
       (7 (push (svref record 9) bin-list))
;;;Feature 8 with 6 levels pivot-index
       (8 (push (svref record 10) bin-list))
 ;;Feature 9 is range-index
       (9 (push (svref record 11) bin-list))
;;;Feature 10 with 5 levels 2-bar index date-1
       (10  (push (svref record 12) bin-list))
;;; feature 11 is the day-bar-type date-1
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
(defun bin-classifier-daytrades4bs (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.25)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list4s date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4);;;changed to 3 to help heap exhaustion
    (setq bin (encode-day-trades4s record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if (> (length contents) 0) (return) (setq features (butlast features)))
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

;     (when (eql (svref kth 2) 1)
;      (incf longs) (setq results-long (+ results-long (svref kth 19))))
;    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
;       (incf num-winners-long))
;     (when (eql (svref kth 2) -1)
;      (incf shorts)(setq results-short (+ results-short (svref kth 19))))
;     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
;        (incf num-winners-short))
        );;;closes dolist over contents
   
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents


  (cond ;((and (plusp results-long)(plusp results-short)
        ;      (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
        ;      (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
        ;      (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
        ;      (setq epsignal 'OK))
       ((not contents) (setq epsignal 'UNIQUE))       
       ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID)) 
       ((and (plusp results-long)
              (> (/ results-long (if (zerop longs) 1 longs))(/ results-short (if (zerop shorts) 1 shorts)))
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)) (setq epsignal 'UP))
        ((and (plusp results-short)
              (> (/ results-short (if (zerop shorts) 1 shorts))(/ results-long (if (zerop longs) 1 longs)))
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc))(setq epsignal 'DOWN))
               
        (t (setq epsignal 'AVOID)))


    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-daytrade-indicators4s ()
   (let ((path (string-append *upper-dir-warehouse* "daytradewarehouse4s.dat"))
          date-1 date-2)
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate)  date-2 (getd date-1 'ydate))

     (setf (svref ith 13) (ep-roc-change-index date-1 3 10));;feature 11
     (setf (svref ith 8)(r-squared-change-index  date-1 10 1)) ;;feature 6
     (setf (svref ith 4)(r-squared-change-index  date-1 20 1)) ;;;feature 2
     (setf (svref ith 12) (gann-slope-index date-1 5));;feature 10
  ;   (setf (svref ith 16) (day-bar-type2 date-2 8));;feature 14

;      (setf (svref ith 13) (day-bar-type2 (getd date-1 'ydate)));;feature 11

 ;    (setf (svref ith 6)(2day-change-index date-1));;feature 12
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



(defun forecast-currencies-day (tdate)
 (let (wppp  roc5 ch5  vol4 ldev can pivm pivw rsid projd mdv5)
 
   (setq wppp (wpp tdate)
      ;  lproj (lproj-index tdate 5)
        roc5 (ep-roc-index tdate 5)
        ch5 (channel-direction tdate 5)
      ;  ch10 (channel-direction tdate 10)
      ;  ch20 (channel-direction tdate 20)
        vol4 (volatility-ratio-index tdate 4 28 1)
        projd (lprojdelta tdate 5)
        ldev (ldev-index tdate 5)
        mdv5   (momentum-divergence2 tdate 5 15)
        can (candle1 tdate)
        pivm (pivot-turn tdate 'month)
        pivw (pivot-turn tdate 'week)
       
        rsid (rsi2x-index tdate  2)
    )
 
   (cond ((and (or (member wppp '(IUH ODF IDL ODH DDU)); UDD IDH IUF UDU DUD))
                   
                   (member can '(UPMB DNCC UPIH UPHM)); DNEP DNHM DNDJ DNST)) 
                  ; (member mdv5 '(DN2 UP2))
                   );(member ch5 '(IC+ IC- AC AT UC+ DC+))
                   ;(member ch10 '(AT BT2 BC BT AC UC)))
                   
               (/= roc5 4)(/= rsid 6)(not (member vol4 '(-4 -3)))
               (or ;(member ldev '(2 3))
                   (member ch5 '(AC DC+ IC- BC BT2 UC UC-)) 
                ;   (member pivm '(R2 9 S0))
                 ;  (member pivw '(S0 H))
                   
                    (member projd '(7 8 9))                                     
                 ;  (member ch20 '(IC+ AT BT2 BC US AC))
                   )) 'BULL)
               

         ((and (or (member wppp '(DUU OUL ODH IUL IDF)); OUF UUU IDL DUD UUD))
                  
                   (member can '(DNES DIHW DNMB DNHR)); UIHW UIST UPDJ UPMB))
                  ; (member mdv5 '(DN2 UP2))
                  ); (member ch5 '(BT DC- AT2 BC UC-))
                  ; (member ch10 '(DC- AT2 IC+ DC)))
                 
               (/= roc5 -4) (not (member vol4 '(-4 -3)))
               (or ;(member ldev '(4 -4))
                   (member ch5 '(DC IC- AT2 AT DC+ BT UC-))
                  ; (member pivm '(S1 R0 PP L R1 R2))
                  ; (member pivw '(S2 L))
                   (member projd '(-7 -8 -9))
                  ;  (member ch20 '(IC- DC- BT DS UC-))
                    )) 'BEAR)

          (t 'FLAT)

       )
)) 
;;;don't trust lproj-index for currencies
;(defun wpp-fore (tdate)
;  (let ((wppp (wpp tdate)))
;    (cond ((and (member wppp '(IUH ODF IDL ODH DDU UDD IDH IUF UDU DUD)) 
;                (member wppp '(DUU OUL ODH IUL IDF OUF UUU IDL DUD UUD))) 2)
;          ((member wppp '(IUH ODF IDL ODH DDU UDD IDH IUF UDU DUD)) 1)
;          ((member wppp '(DUU OUL ODH IUL IDF OUF UUU IDL DUD UUD)) -1)
;          (t 0))
;))


(defun wpp-fore (tdate)
  (let ((wppp (wpp tdate)))
   (cond ((and (member wppp '(UDD ODH IDF IDL DDU IDH IUH IUF DDD IUL))
                (member wppp '(DUD DUU OUL ODF UDU ODL UUU IDF ))) 2)
          ((member wppp '(UDD ODH IDF IDL DDU IDH IUH IUF DDD IUL)) 1)
         ((member wppp '(DUD DUU OUL ODF UDU ODL UUU IDF )) -1)
          (t 0))))



;(defun candle-fore (tdate)
;  (let ((can (candle1 tdate)))
;    (cond ((and (member can '(UPMB DNCC UPIH UPHM DNEP DNHM DNDJ DNST)) 
;                (member can '(DNES DIHW DNMB DNHR UIHW UIST UPDJ UPMB))) 2)
;          ((member can '(UPMB DNCC UPIH UPHM DNEP DNHM DNDJ DNST)) 1)
;          ((member can '(DNES DIHW DNMB DNHR UIHW UIST UPDJ UPMB)) -1)
;          (t 0))))



(defun candle-fore (tdate)
  (let ((can (candle1 tdate)))
      (cond ((and (member can '(DNES DNCC DNEP DNMB DNHR UPHR DNHM DNST UPHM DIHW UIHW))
                  (member can '(UPMB UPIH UIHW UPEP DNST UPHM DNHM UIST DIHW))) 2)
            ((member can '(DNES DNCC DNEP DNMB DNHR UPHR DNHM DNST UPHM DIHW UIHW)) 1)
            ((member can '(UPMB UPIH UIHW UPEP DNST UPHM DNHM UIST DIHW)) -1)
            (t 0))))




;(defun channel-fore (tdate)
;  (let ((can (channel-direction tdate 5)))
;    (cond ((and (member can '(AC DC+ IC- BC BT2 UC UC-)) 
;                (member can '(DC IC- AT2 AT DC+ BT UC-))) 2)
;          ((member can '(AC DC+ IC- BC BT2 UC UC-)) 1)
;          ((member can '(DC IC- AT2 AT DC+ BT UC-)) -1)
;          (t 0))))


(defun channel-fore5 (tdate)
  (let ((ch5 (channel-direction tdate 5)))
   (cond ((and (member ch5 '(BT2 AT IC+ AC DC BC DC+ UC+))
               (member ch5 '(BT UC- DC- AT2 DC UC DS BT2 ))) 2)
         ((member ch5 '(BT2 AT IC+ AC DC BC DC+ UC+)) 1)
         ((member ch5 '(BT UC- DC- AT2 DC UC DS BT2 )) -1)
         (t 0))))


(defun channel-fore10 (tdate)
  (let ((ch5 (channel-direction tdate 10)))
   (cond ((and (member ch5 '(AC BT2 US UC- AT DC))
               (member ch5 '(BT DS AT2 AT DC+ DC UC BT2))) 2)
         ((member ch5 '(AC BT2 US UC- AT DC)) 1)
         ((member ch5 '(BT DS AT2 AT DC+ DC UC BT2 )) -1)
         (t 0))))



(defun pivot-turn-fore-month (tdate)
   (let ((pivm (pivot-turn tdate 'month)))
    (cond ((and (member pivm '(L R2 H S0 PP))
                (member pivm '(S1 R0 H S2 S0))) 2) 
          ((member pivm '(L R2 H S0 PP)) 1)
          ((member pivm '(S1 R0 H S2 S0)) -1)
          (t 0))))

(defun pivot-turn-fore-week (tdate)
   (let ((pivw (pivot-turn tdate 'week)))
    (cond ((and (member pivw '(H S2 PP R2 R0 )) 
               (member pivw '(L S1 PP S0 R1))) 2)
          ((member pivw '(H S2 PP R2 R0)) 1)
          ((member pivw '(L S1 PP S0 R1)) -1)
          (t 0))))


(defun roc-fore (tdate)
   (let ((roc5 (ep-roc-index10 tdate 5)))
      (cond ((= roc5 4) 2)
            ((member roc5 '(-4 4 3)) 1)
            ((member roc5 '(4 -3 -2)) -1)
            (t 0))))


(defun display-daytrade-contents (date direction &optional (features *day-features4x*))
 (let (bin)
   (setq bin (nth-value 7 (bin-classifier-daytrades4bx date features)))
    (print bin)
   (setf (car bin) direction)
   (display-daytrade-bin bin)
))


(defun display-daytrade-bin (bin)
  (let (contents)
     (setq contents (gethash bin *day-trade-warehouse3*))     
     (dolist (ith contents)
       (print ith))))


;;;direction is 'BUY or 'SELL
(defun add-nt-recommendation (market direction tdate date1)
  (let ((time-zone 'CT) ninja-fore-path3)
    (set-market market)
    (setq ninja-fore-path3 (string-append "~/mk-data/111-dropbox/nt-e12.csv"))

   (with-open-file (ninja-output ninja-fore-path3 :direction :output :if-exists :append
                    :if-does-not-exist :create)
       
                    (write-nt-edge-record ninja-output time-zone tdate date1 direction 0 0 ))
       
)) 
                

              

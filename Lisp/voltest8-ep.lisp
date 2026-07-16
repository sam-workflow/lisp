;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;;This is for CONTRA TRADES
;;;;This version does not use objectives.
;;;
;;;
;;;;;Now we need first to read the day-trades file(s) and load into the *day-trade-warehouse* hash table
(defparameter *contra-trade-warehouse* (make-hash-table :test #'equal))
(defvar contratrades nil)
(defvar contra-bin-codes nil)
(defparameter *exitpoints-trade-warehouse* (make-hash-table :test #'equal))
(defvar exitpointstrades nil)
(defvar exitpoints-bin-codes nil)

(defparameter *exitpoints-commission* 0)
;;;
;;;;For DAY TRADING
;;;

;;;;the file has one record per trade each record is a vector. 
;;;The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1
;;;;
;;;;
;;;
;;;
;;; #(*data-name* entry-date direction entry-price HT BT BR T5 T15 DO BR-1 CAN SST T45 ZBL BPL TIM)
;;;
;;;Feature 1 is direction
;;;Feature 2 is the volatility ratio 4 day versus 63 day
;;;Feature 3 is the day-bar-type
;;;Feature 4 is the slow stochastic 21
;;;Feature 5 is roc-index 5 63
;;;Feature 6 is roc-index 21 252
;;;Feature 7 is Gann-slope-index 5 63
;;;Feature 8 is pivot-index
;;;Feature 9 is range-index
;;;Feature 10 is 2-bar index date-1
;;;Feature 11 is day-bar-type date-1
;;;Feature 12 is reflect 3
;;;Feature 13 is Gann-slope-index 9 63
;;;feature 14 is pivot-index date-1
;;;

;;This function is used to add trades to the warehouse3 for all the day
;;;markets (both score and non-score)
;;;*position-list* includes all 35 futures markets
(defun update-contratrade-warehouse (date  &optional (markets  *meats-list*))
    
   (maind-x)(set-cat-list)
     (dolist (ith markets)
       (set-market ith)
       (populate-contra-trades ith date (available-days ith date)))
    
 ;     (build-contratrade-warehouse markets)
  ;    (setq *contra-features* (find-best-contra-indicator-set))
   ;   (portfolio-simulation3 '(contra) date 3330 (list markets))
)

(defun populate-contra-trades (market date2 num &optional (outfile T))
 (let (date trades long short obj-long obj-short entry-long entry-short date-1
        prev-stop-long prev-stop-short risk
        trade-long trade-short   cover-long cover-short
        ave-win ave-loss losers winners extended-trades  stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "contratrades.dat")))

   (maind-x)(set-cat-list) (set-market market)
   (format outfile "~%~A~%" market)
    
    (if (member market *forex-list*)(setq *contra-commission* 0 *pips-slippage* 6)
         (setq *contra-commission* 60 *pips-slippage* 0))

  (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))
 
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
  ;   (format T "~%Date= ~A" date)
       
        (setq 
            entry-short (lregress date 5 'high) entry-long (lregress date 5 'low)
           obj-short entry-long obj-long entry-short 
           risk (* .5 (- entry-short entry-long))
            )
     

   ;   (format T "~2%OBJS = ~A" objs)
   ;   (format T "~%date = ~A entry-short = ~A  entry-long = ~A" date entry-short entry-long) 

     
   ;   (format T "~%signal= ~A risk = ~A obj-long =~A obj-short = ~A" rsi-signal risk obj-long obj-short)
  
       (setq   date-1 date date (add-mkt-days date 1))


      (if long (setq stop-long (fmax prev-stop-long stop-long) prev-stop-long stop-long
                   ))
      (if short (setq stop-short (fmin prev-stop-short stop-short) prev-stop-short stop-short))
                   

     (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover)) obj-long (+ obj-long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover))))
 
     (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover)) obj-short (+ obj-short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover))))

 
  ;;;;check if stopped out of prior position

;;;check if stopped out
   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (min stop-long (getd date 'open)))
                       (round (* (index-point-value) (- (min stop-long (getd date 'open)) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil))
         
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price (max stop-short (getd date 'open)))
                 (round (* (index-point-value) (- short (max stop-short (getd date 'open)))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil))


;;;check if exited with objective
   (when  (and long obj-long (> (getd date 'high) obj-long))
          (push (-  (max (getd date 'open) obj-long)  long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (max (getd date 'open) obj-long))
                       (round (* (index-point-value) (- (max (getd date 'open) obj-long) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil ))

   (when  (and short obj-short (< (getd date 'low) obj-short))
          (push (- short (min (getd date 'open) obj-short)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (min (getd date 'open) obj-short))
                 (round (* (index-point-value) (- short (min (getd date 'open) obj-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil ))

;;;;check if exited based on criteria
#|
   (when (and long (neql epsignal 'UP))
          (push (-  (getd date 'open) long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (getd date 'open))
                       (round (* (index-point-value) (-  (getd date 'open) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil ))
         
   (when (and short (neql epsignal 'DOWN))
          (push (- short (getd date 'open)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price (getd date 'open))
                 (round (* (index-point-value) (- short (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil ))

|#
  ; (format T "~% short = ~A long = ~A" short long)
;;;check if new long entry
;;;enter on a market order if opens above entry-long.
;;;enter on a  market order if opens below the entry-short.
;;;exit at the close

   (when (and (not short) 
              (> (getd date 'high) entry-short)
            ; (<= (getd date 'low) entry-short)
                 )

          (setq short (max (getd date 'open) entry-short) 
                stop-short (+ entry-short risk)
               ; obj-short entry-long
                trade-short (create-contratrade-entry-record-list date-1 -1 short)
                prev-stop-short stop-short 
                 ))
 ; (format T "~% date = ~A short = ~A stop-short = ~A obj short = ~A" date short stop-short obj-short)
    (when (and (not long) 
               (< (getd date 'low) entry-long)
              ; (>= (getd date 'high) entry-long)
                     )

           (setq long (min (getd date 'open) entry-long)
                 stop-long (- entry-long risk)
                ; obj-long entry-short
                 trade-long (create-contratrade-entry-record-list date-1 1 long)
                 prev-stop-long stop-long 
                 )); (format T "long-trade= ~A ~%" long-trade) )

;;;check if stopped out on same day

   (when (and long stop-long
               (<= (getd date 'low) stop-long)
                            )
           (push (- (* (calculate-point-value date)(- stop-long long))(comm+slip-contra date)) trades)
           (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date) (- stop-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil obj-long nil
           ))

    (when (and short stop-short
                (>= (getd date 'high) stop-short)
                                   )
           (push (- (* (calculate-point-value date)(- short stop-short))(comm+slip-contra date)) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date) (- short stop-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil
           ))

 ;;;check if exited with objective on day of entry
;;;;need to check if low came first
   (when  (and long obj-long
            (or (and (> (getd date 'high) obj-long)
                     (> (getd date 'close) (getd date 'open)))
                (> (getd date 'close) obj-long))
              )
          (push (-  obj-long long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price obj-long)
                       (round (* (index-point-value) (- obj-long long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil obj-long nil))

   (when  (and short obj-short 
               (or (and (< (getd date 'low) obj-short)
                        (> (getd date 'open)(getd date 'close)))
                   (< (getd date 'close) obj-short))
             )
          (push (- short obj-short) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  obj-short)
                 (round (* (index-point-value) (- short obj-short)))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil obj-short nil))


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd date 'close))
            (push (- (* (- cover-long long)(calculate-point-value date))(comm+slip-contra date)) trades)
            (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date) (- cover-long long)))))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil stop-long nil obj-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- (* (- short cover-short)(calculate-point-value date))(comm+slip-contra date)) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date) (- short cover-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil stop-short nil obj-short nil))



   );;;closes the dotimes

 
  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round  (list-sum trades))  (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (list-sum trades)  (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round  (drawdown trades))
   
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
(defun create-contratrade-entry-record-list (date direction entry)
   (let* ((date-1 (getd date 'ydate)))

       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                      
                              (volatility-ratio-index date 4 63 1);;feature 2
                              (bpl-index date 5) ;feature 3  9 levels
                              (slow-stochastic-index date 21) ;feature 4
                              (day-bar-type1 date ) ;feature 5  10 levels
                              (volatility-ratio-index date 1 14 1) ;;;feature 6  10 levels
                    ;         (trend-signal date 45) ;feature 7
                              (ave-buy-ratio-index date 5);;feature 7  5 levels
                              (pivot-index date 1) ;feature 8  6 levels
                              (r-squared-change-index date 5 1) ;; feature 9 
                              (day-bar-type date );;;feature 10  5 levels
                              (day-bar-type date-1) ;feature 11  9 levels
                           ;   (daytrade-reward-risk date direction) ;;feature 12
                              (macd-index date 6 15 3)      ;;  feature 12  5 levels
                            ;  (mo-diver date 45) ;;feature 13
                              (day-bar-type2 date ) ;;feature 13 5 levels
                              (body-range-index date) ;;features 14 
                              );;;closes the list

))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-contratradesb (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0)(gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0))
    (setq record (create-contratrade-entry-record-list date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

     (dotimes (ith 4)
    (setq bin (encode-contra-trades record features))
    (setq contents (gethash bin *contra-trade-warehouse*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *contra-trade-warehouse*) contents))
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
              (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
              (setq epsignal 'OK))
        ((and (plusp results-long) (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)) (setq epsignal 'UP))
        ((and (plusp results-short) (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc))(setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))


;;;enters with stop orders and exits with stop loss orders
;;;;
(defun contratrade-simulation-test (market date2 num &optional (features *contra-features*))
 (let (date stop-long stop-short trades long short   trade-long  entry-long entry-short
       prev-stop-long prev-stop-short cover-long cover-short
       ave-win ave-loss losers winners extended-trades trade-short 
       risk  
         epsignal longs long-gains long-acc trading-dates (trade-time 0) obj-long obj-short
       shorts short-gains short-acc  date-1 record (running-sum 0) bin draw
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "contra-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "contra-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "contra-diary1.dat")))


   
       (set-market market)(format T "~%~A~%" *data-name*)   
       (apply #'contra-trade-binsb features)  ; (apply #'exitpoints-trade-binsb *exitpoints-features*)
   
       (if (member market *forex-list*)(setq *contra-commission* 0 *pips-slippage* 6)
         (setq *contra-commission* 60 *pips-slippage* 0))

    (setq date (add-mkt-days date2 (- num)))
    (setq record (vector date (getd date 'close) 0 0 0))
;    (push record trading-dates)
 ;;;;from date1 to date2
   (dotimes (ith num)
;      (multiple-value-setq (mo-signal price-to-go dirp) (momentum-divergence2 date 9 6 15 3))
     ; (setq rsi-signal (rsi-cross date 14 3))
        (setq 
            entry-short (lregress date 5 'high) entry-long (lregress date 5 'low)
           obj-short entry-long obj-long entry-short 
           risk (* .5 (- entry-short entry-long))
            )
;  (format T "~%1 ~A = date  ~A = mo-signal  ~A = price-to-go ~A = dirp" date mo-signal price-to-go dirp)

      (setq date-1 date date (add-mkt-days date 1))

      (setq record (vector date (getd date 'close) 0 0 0))
       (if long (setf (svref record 2) 1))
       (if short (setf (svref record 2) -1))

   (if long (setq stop-long (fmax prev-stop-long stop-long) prev-stop-long stop-long
                 ))
   (if short (setq stop-short (fmin prev-stop-short stop-short) prev-stop-short stop-short
                ))

    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover))
                   obj-long (+ obj-long (getd date 'rollover)))
             (setf (svref record 3)(- (svref record 3) (getd date 'rollover)))
        )

    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover))
                  obj-short (+ obj-short (getd date 'rollover)))
            (setf (svref record 3) (+ (svref record 3) (getd date 'rollover)))
         )

;   (format T "~%2 ~A = date  ~A = long  ~A = short" date long short)
;;;;check if stopped out of prior position

   (when (and long stop-long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (apply #'vector
             (append trade-long
              (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'S
                    (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (svref record 3) (+ (svref record 3)(- (min stop-long (getd date 'open)) (getd date-1 'close))))
         
          (setq trade-long nil long nil stop-long nil obj-long nil))

   (when (and short stop-short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'S
                    (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (svref record 3) (+ (svref record 3)(- (getd date-1 'close) (max stop-short (getd date 'open)))))
         
          (setq trade-short nil short nil stop-short nil obj-short nil  ))


;;;check if exited with objective
   (when  (and long obj-long (> (getd date 'high) obj-long))
    ;     (format T "~%long = ~A obj-long = ~A CLOSE = ~A OPEN = ~A" long obj-long (getd date 'close) (getd date 'open))
          (push (-  (max (getd date 'open) obj-long)  long) trades)
          (setq trade-long (apply #'vector
               (append trade-long
                       (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'O
                             (my-pretty-price (max (getd date 'open) obj-long))
                             (- (max (getd date 'open) obj-long) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (svref record 3) (+ (svref record 3)(- (max obj-long (getd date 'open)) (getd date-1 'close))))
        
          (setq trade-long nil long nil stop-long nil obj-long nil))

   (when  (and short obj-short (< (getd date 'low) obj-short))
          (push (- short (min (getd date 'open) obj-short)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'O
                        (my-pretty-price  (min (getd date 'open) obj-short))
                          (- short (min (getd date 'open) obj-short))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (svref record 3) (+ (svref record 3)(- (getd date-1 'close) (min obj-short (getd date 'open)))))

          (setq trade-short nil short nil stop-short nil obj-short nil ))

#|
;;;;check if exited based on criteria
   (when (and long  (= dir -2)
              )
          (push (-  (getd date 'open) long) trades)
          (setq trade-long (apply #'vector
             (append trade-long
              (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'C
                     (getd date 'open) (- (getd date 'open) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (svref record 3) (+ (svref record 3)(- (getd date 'open) (getd date-1 'close))))
         
          (setq trade-long nil long nil ))

   (when (and short (= dir 2)
              )
          (push (- short (getd date 'open)) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'C
                     (getd date 'open) (- short (getd date 'open))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (svref record 3) (+ (svref record 3)(- (getd date-1 'close) (getd date 'open))))
         
          (setq trade-short nil short nil  ))
|#

;;;if long or short and not entry or exits
      (if long (setf (svref record 3) (+ (svref record 3)(- (getd date 'close) (getd date-1 'close)))))
      (if short (setf (svref record 3)(+ (svref record 3)(- (getd date-1 'close)(getd date 'close)))))
 ;     (format T "~%3 ~A = date  ~A = entry-short  ~A = entry-long" date entry-short entry-long)
;;;;calculate bin-classifier only as needed
   
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-contratradesb date-1 features))
        
       
      
;     (format T "~%3a long= ~A short= ~A epsignal= ~A" long short epsignal)
;;;check if new entry

    (when (and (not short) ;(= dir5 1)(minusp dir10)
              (> (getd date 'high) entry-short)
               (member epsignal '(OK DOWN))
               (>= (/ short-gains shorts) 100) 
               (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
               )

          (setq short (max (getd date 'open) entry-short)
                stop-short (+ entry-short risk)
             ;   obj-short entry-long
                trade-short (list date 'short short)
                ) 
          (setf (svref record 2) -1)
          (setf (svref record 3)
                 (- short (getd date 'close) (/ *contra-commission* (index-point-value))))
              )

    (when (and (not long);(= dir5 -1)(plusp dir10)
               (< (getd date 'low) entry-long)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) 100) 
               (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
                )
           (setq long (min (getd date 'open) entry-long)
                 stop-long (- entry-long risk)
              ;   obj-long entry-short
                 trade-long (list date 'long long) 
                 )
           (setf (svref record 2) 1)
           (setf (svref record 3)
                  (- (getd date 'close) long (/ *contra-commission* (index-point-value))))
             )
;     (format T "~%4 ~A = date  ~A = entry-short  ~A = entry-long" date entry-short entry-long)
;      (format T "~%4 ~A = epsignal  ~A = long  ~A = short" epsignal long short)
 ;;;check if stopped out on same day of entry
   (when (and long stop-long  (<= (getd date 'low) stop-long))
           (push (-  stop-long long) trades)
           (setq trade-long (apply #'vector
                 (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*)) 
                          (contract-month *data-name* date) 'S
                         (min (getd date 'open) stop-long) (- (min (getd date 'open) stop-long) long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
           (setf (svref record 3) (- (svref record 3)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil obj-long nil ))

;    (format T "~%5 date= ~A stop-short= ~A short = ~A (svref record 3) = ~A" date stop-short short (svref record 3))
    (when  (and short stop-short (>= (getd date 'high) stop-short))
           (push (- short  stop-short) trades)
           (setq trade-short
               (apply #'vector
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*)) 
                         (contract-month *data-name* date) 'S
                         (max (getd date 'open) stop-short) (- short (max (getd date 'open) stop-short))))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short stop-short)))
           (setf (svref record 3) (- (svref record 3)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil obj-short nil))

;   (format T "~%6 date= ~A long= ~A short = ~A stop-long = ~A stop-short= ~A" date long short stop-long stop-short)

;;;check if exited with objective on day of entry
   (when  (and long obj-long 
               (or (and (> (getd date 'high) obj-long)
                        (> (getd date 'close)(getd date 'open)))
                   (> (getd date 'close) obj-long)));;;makes more likely obj hit after fill.
   ;      (format T "~%long = ~A obj-long = ~A CLOSE = ~A OPEN = ~A" long obj-long (getd date 'close) (getd date 'open))
          (push (-  obj-long long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'O
                        (my-pretty-price  obj-long)
                       (-  obj-long long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (svref record 3) (+ (svref record 3)(-  obj-long long)))
          (setf (svref record 3) (- (svref record 3)(- (getd date 'close) long)))
          (setq trade-long nil long nil ))

   (when  (and short obj-short 
                (or (and (< (getd date 'low) obj-short)
                         (< (getd date 'close) (getd date 'open)))
                    (< (getd date 'close) obj-short)));;;makes more likely obj hit after fill
          (push (- short obj-short) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))  (contract-month *data-name* date) 'O
                (my-pretty-price  obj-short)
                 (- short obj-short)))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (svref record 3) (+ (svref record 3)(- short obj-short)))
          (setf (svref record 3) (- (svref record 3)(- short (getd date 'close))))
          (setq trade-short nil short nil ))


;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (getd date 'close))
            (push (- cover-long long) trades)
            (setq trade-long (apply #'vector (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push trade-long extended-trades)
            (setf (svref record 2) 1)
        ;    (setf (svref record 3) (+ (svref record 3) (- (getd date 'close) long)))
            (setq trade-long nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push trade-short extended-trades)
           (setf (svref record 2) -1)
         ;  (setf (svref record 3) (+ (svref record 3)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil))

    ;   (format T "~%5 ~A = date  ~A = entry-short  ~A = entry-long" date entry-short entry-long)
       (setf (svref record 4) (+ (svref record 3) running-sum))
       (setq running-sum (svref record 4))
       (push record trading-dates)
       (setq record nil);;;reset to nil at end of each trading date
      
     ) ;;;closes the dotimes
;;;;this writes out the diary file
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (* (index-point-value)(svref ith 3)))
           (round (* (index-point-value) (svref ith 4))))
     ))
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *contra-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

  (with-open-file (str summary-path :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )

     (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))

      (format str "P/L= ~16D  NUMBER TRADES=   ~6D   ACCURACY=       ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN=  ~8D   AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=    ~4,2,0,'*,' F  ~
        LARGEST LOSS=   ~7D   LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract=  ~10D  "
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
      (my-round (/ trade-time (if (zerop (length trades)) 1 (length trades))) 1)
      (setq draw (round (* (drawdown trades)(or (index-point-value) 1))))
      (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       )

     (format str "~%~%MIN INITIAL $=      ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/ (* (list-sum trades) (or (index-point-value) 1))
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                                           365.25))
                       (* 1000 (ceiling (max 1.0 (* 3 (abs draw))) 1000))) ))
        )
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)(svref ith 7)
         (svref ith 8)
         (round (* (index-point-value) (svref ith 8))))

     ));;closes the dolist and with-open-file
  ; ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun contra-trade-binsb ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "contratradewarehouse.dat"))
    (maind-x)(set-cat-list)
    (setq contratrades nil contra-bin-codes nil)(clrhash *contra-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record contratrades)))


;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *day-commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record contratrades)
     (dotimes (ith 4)
         (setq bin (encode-contra-trades record (butlast features ith)))
         
          (cond ((gethash bin *contra-trade-warehouse*)
                 (ifn (member record (gethash bin *contra-trade-warehouse*) :test #'equalp)
                     (setf (gethash bin *contra-trade-warehouse*)
                           (cons record (gethash bin *contra-trade-warehouse*)))))
                 ((not (gethash bin *contra-trade-warehouse*))
                  (setf (gethash bin *contra-trade-warehouse*) (list record))
              (push bin contra-bin-codes)))
          ));;;closes the doltimes and dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-contratrade-bins-by-profit contratrades features)


  ))


(defun contra-trade-bins (&rest features)
  (let (bin (path (string-append *upper-dir-warehouse* "contratradewarehouse.dat")))
 
 (maind-x)(set-cat-list)
  (setq contratrades nil contra-bin-codes nil)(clrhash *contra-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record contratrades)))


;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record contratrades)
     (setq bin (encode-contra-trades record features))
     (cond ((gethash bin *contra-trade-warehouse*)
            (ifn (member record (gethash bin *contra-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *contra-trade-warehouse*)
                       (cons record (gethash bin *contra-trade-warehouse*)))))
            ((not (gethash bin *contra-trade-warehouse*))
             (setf (gethash bin *contra-trade-warehouse*) (list record))
              (push bin contra-bin-codes)))

    )

    (format T  "~%FEATURES = ~A~%" features)
    (rank-contratrade-bins-by-profit contratrades features)


  ))





;;;reads the daytradewarehouse5.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-contra-trades (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "contratradewarehouse.dat")) trades)

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


(defun build-contratrade-warehouse (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "contratradewarehouse.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "contratradewarehouse.backup"))
           (delete-file (string-append *upper-dir-warehouse* "contratradewarehouse.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "contratradewarehouse.dat"))
          (rename-file (string-append *upper-dir-warehouse* "contratradewarehouse.dat")
                            "contratradewarehouse.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "contratrades.dat")) 
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



(defun encode-contra-trades (record features)
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
;;;Feature 6 is roc-index date 21 252
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


;;;requires a candidate list to add to the base features
(defun contratrades-add-one-in (base-features candidate-features)
 (let (winners-list (result 0) average-profit ignore single-bins)  

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'contra-trade-bins (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
   
   
    (vsort winners-list #'> 'car)
    (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
    (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
    (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))


;;;requires a base features list
(defun contratrades-leave-one-out (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
;  (apply #'day-trade-bins4 base-features)

  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'contra-trade-bins (remove ith base-features)))
   
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))


;;;;this function expects that you have run (day-trade-bins ...) already
(defun display-contratrade-bins-by-expected-value ()
  (let (contents expected-value-list result (path1 (string-append *output-upper-dir* "contratrade-expected-value.dat")))
   (dolist (ith contra-bin-codes)
     (setq contents
           (gethash ith *contra-trade-warehouse*))
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


(defun remove-contratrade-market (market)
  (let (trades path)
   (setq path (string-append *upper-dir-warehouse* "contratradewarehouse.dat"))
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

;;;this function removes all trades for a single market and replaces them
;;;with a new file of trades
(defun replace-contratrade-market (market &aux path)

  (remove-contratrade-market market)
  (setq path (string-append *upper-dir-warehouse* (format nil "~S" market) "contratrades.dat"))
  (add-contra-trades path)
)


(defun find-best-contra-indicator-set ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (contratrades-add-one-in base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
     (if (or (not candidate-list)(> result .97))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     

      (if (neql (second (car winners-list)) 1)
          (if (< (fourth (car winners-list)) .80)(return))
        (if (< (fourth (second winners-list)) .80)(return)))

      (setq winners-list (contratrades-leave-one-out base-list))
      (if (neql (cdr (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
     base-list
    ))
 

;;;
(defun find-best-contratrade (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk  stop-short stop-long  entry-long entry-short
        trade-direction  cover-short cover-long pacific-entry-time ;pacific-exit-time
        pacific-cancel-time pacific-end-session-time  
        central-entry-time; central-exit-time
         central-cancel-time central-end-session-time
        action directive1 cannon-mini-path3 
        oec-symbol oco-code ; (time-zone "CT")
         ;offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc  counter))

    (setq *entry-factor* .764 *stop-loss-day* .80) ;;;  1.0557)
    (setq cannon-mini-path3 (string-append *daily-output-dir* "cannon-mini.csv"))

    (if (and (member epsignal '(OK UP))(> longs 0)(>= (/ long-gains longs) 100.0))
         (push 'UP trade-direction)(push 'FT trade-direction))
    (if (and (member epsignal '(OK DOWN))(> shorts 0)(>= (/ short-gains shorts) 100.0))
         (push 'DN trade-direction)(push 'FT trade-direction))

    (multiple-value-setq (entry-short entry-long) (vprices tdate 4 *entry-factor* 1))
    (multiple-value-setq (stop-long stop-short)(vprices tdate 4 *stop-loss-day* 1))

   (setq risk  (* .5 (- entry-long entry-short)))
 
   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))


            (when (if (member *data-name* '(US.D1B TY.D1B))
                  (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                                   (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-day-risk*)
                 (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                                  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))))
                                  (index-point-value))) *max-day-risk*)) (push "NOT TODAY" action))

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
      (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))(convert-to-decimal (convert-to-32 entry-short)))
                  (index-point-value)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (index-point-value))))
         (format output
            (string-append "~%SELL= " directive1 " STOP= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))

            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
                             )  (index-point-value)))
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))

            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
                       )  (index-point-value)))
            ))

     (setq oec-symbol (make-oec-symbol *data-name* tdate)
           pacific-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
	   pacific-end-session-time "default" 
           pacific-cancel-time
                (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
      (setq
           central-entry-time "default" 
	   central-end-session-time "default" 
           central-cancel-time
                (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter)) ;;;(add-minutes1 exit-time -30))
      (format T "~%~A  ~A" *data-name* action)
     (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" 'MINI 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" 'MINI 'LONG date1 entry-long stop-long cover-long output1)

  ;           (when (member *data-name* *mini-list*);;
;
 ;              (setq offset (random-choice -1 0)
  ;                   pacific-exit-time
   ;                  (string-append (date-convert date1) " " 
    ;                        (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
 ;              (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
 ;                                pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
 ;              (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
 ;              (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
 ;                               pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

 ;             );;;closes when
       
             )

             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'MINI 'LONG date1 entry-long stop-long cover-long output1)

 ;             (when (member *data-name* *mini-list*)
 ;              (setq offset (random-choice -1 0)
 ;                pacific-exit-time
 ;                (string-append (date-convert date1) " "
 ;                                (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
 ;              (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
 ;                               pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
;
;               ) ;;closes the when score
                )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'MINI 'SHORT date1 entry-short stop-short cover-short output1)

 ;             (when (member *data-name* *mini-list*)
 ;                (setq offset (random-choice -1 0)
 ;                      pacific-exit-time
;                       (string-append (date-convert date1) " "
;                                       (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
;                 (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
;                                   pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code);

;                 );;;closes the when score
            
               ));;closes clause the cond
      )) ;;;closes the let and the defun





;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-contratrade-indicators ()
   (let ((path (string-append *upper-dir-warehouse* "contratradewarehouse.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))

   ;  (setf (svref ith 6) (pldot-trend date-1));;feature 4
   ;  (setf (svref ith 7)(pldot-index date-1)) ;;feature 5
   ;  (setf (svref ith 11) (range-index date-1 ));;feature 9
;     (setf (svref ith 6)(2day-change-index date-1));;feature 12
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

;;;This rank is for using multiple length features

(defun rank-contratrade-bins-by-profit (contratrades features)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0)
        longest-contra-bin-codes (longest-feature (length features)))
   (setq longest-contra-bin-codes (remove-if #'(lambda (s) (< (length s) longest-feature)) contra-bin-codes))
   (dolist (ith longest-contra-bin-codes)
     (setq contents (gethash ith *contra-trade-warehouse*))
     (setq result 0 counter 0 )
 ;    (format T "~%~A" ith)
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
       
         ) ;;;closes dolist over contents
      (if  (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))

     (setq profit-list
      (cons (list result  (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth contratrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length contratrades)(length (num-markets-in-warehouse3 contratrades)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length longest-contra-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length contratrades)(length longest-contra-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length longest-contra-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length contratrades)) 2))
     (format T "CHI SQUARE METRIC = ~7,5F~%" (round (contratrade-chi-squared-gof)))

    (values (round winners)(round (+ all-winners all-losers))(if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
           (my-round (/ only-one (length contratrades)) 2) (contratrade-chi-squared-gof)) 
       

 ))



;;;assumes the day-trade-bins has been run first
(defun contratrade-chi-squared-gof ()
  (let (contents (result 0) square-list (all-winners 0)(all-losers 0)
       square percentage-winners chi-squared)
;;;first calculate the profit per trade overall trades 
   (dolist (kth contratrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners 1))
           (setq all-losers (+ all-losers 1))))
   
   (setq percentage-winners (/ all-winners (+ all-winners all-losers)))

  (dolist (ith contra-bin-codes)
     (setq contents (gethash ith *contra-trade-warehouse*))
     (setq result 0 square 0 )
     (dolist (kth contents)
         (if (plusp (svref kth 19))
             (setq result (+ result 1)))
         ) ;;;closes dolist over contents
;;;;result is the observed number of winners in the bin
;;;;the expected number of winners in the bin is
     (setq square
         (/ (expt (- result (* percentage-winners (length contents))) 2) (* percentage-winners (length contents))))

     (push square square-list));;;closes dolist over day-bin-codes

     (setq chi-squared (list-sum square-list));;; number of bins less one is the degrees of freedom
;     (format T "~%CHI SQUARE = ~7,3F  DOF = ~D~%" chi-squared (1- (length day-bin-codes)))
     (chi-square-cdf chi-squared (1- (length contra-bin-codes)))
  
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;for day trades with currency futures

;;;this is for the meats day trades
(defun currencies-bins3 (&rest features)
  (let (bin path)
  (setq path (setq path (string-append *upper-dir-warehouse* "currencywarehouse3.dat")))
  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))


;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (setq bin (encode-day-trades3 record features))
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


;;;requires a candidate list to add to the base features
(defun currencies-add-one-in3 (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'currencies-bins3 base-features)

  (dolist (ith candidate-features)
    (setq result (apply #'currencies-bins3 (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))

;;;requires a base features list
(defun currencies-leave-one-out3 (base-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq *stocks* T)(setq *stocks* nil))
  (apply #'currencies-bins3 base-features)

  (dolist (ith base-features)
    (setq result (apply #'currencies-bins3 (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 ))

;;;reads the currencywarehouse3.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-currencies-trades3 (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "currencywarehouse3.dat")) ewaves-trades)

       (if (probe-file path-out)
        (with-open-file (str path-out :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (push record ewaves-trades))
          ))

     (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (pushnew record ewaves-trades :test #'equalp)
          ))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith ewaves-trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


(defun remove-currencies-market3 (market)
  (let (trades path)
  (setq path (setq path (string-append *upper-dir-warehouse* "currencywarehouse3.dat")))
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


(defun find-best-daytrade-entry3 (tdate num)
  (let ((best-param .5) (results 0) (total-trades 0) (prev-result -20000000)
         scores1 scores2 trades result)

  (do ((param best-param  (+ param .03)))
      ((> param .90) best-param)

      (setq *entry-factor* (my-round param 2))
      (format T "~%TRYING ENTRY= ~A~%" param)(setq results 0 total-trades 0)
      (dolist (ith  *currencies-list*)
        (format T "~%MARKET= ~A~%" ith)

        (multiple-value-setq (result trades) (populate-day-trades3 ith tdate num ))
        (setq results (+ results result) total-trades (+ total-trades trades)));;closes the dolist

      (setq scores1 (acons param results scores1))(print scores1)
      (setq scores2 (acons param (round (/ results total-trades)) scores2))(print scores2)

      (if (> results prev-result) (setq prev-result results best-param param))
      (format T "~%BEST ENTRY SO FAR= ~A" best-param));;;closes the do


      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;ExitPoints is used as a trade exit for swing trading

(defun update-exitpoints-warehouse (date  &optional (markets *forex-list*))
  
   (maind-x)(set-cat-list)
 
  (dolist (ith markets)
       (set-market ith)
       (populate-exitpoints-trades ith date (min 4040 (available-days ith date))))
 
      (build-exitpointstrade-warehouse markets)
      (setq *exitpoints-features* (find-best-exitpoints-indicator-set))
      (portfolio-simulation3 '(exitpoints) date 4040 (list markets))
)


;;;;calculates the deduction for commsission and slippage
(defun exitpoints-comm+slip (date)
 (+ *exitpoints-commission* (* *pips-slippage* (index-tick-size) (calculate-point-value date))))


;;;;builds a warehouse for exiting trades
;;;used as a means of deciding whether to hold a open position one more day or not.

(defun populate-exitpoints-trades (market date2 num &optional (outfile T))
 (let (date trades long short  date-1 risk stop-long stop-short
       cover-long cover-short trade-long trade-short  
        ave-win ave-loss losers winners extended-trades  
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "exitpointstrades.dat")))

   (maind-x)(set-cat-list) (set-market market)
   (format outfile "~%~A~%" market)
    
    (if (member market *forex-list*)
        (setq *day-commission* 0 *pips-slippage* 0 *max-day-risk* 500 *stop-loss-day* 1.125)
       (setq *exitpoints-commission* 0 *pips-slippage* 0 *max-day-risk* 1900 *stop-loss-day* 1.125))

  (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))

   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (setq risk  (volatility date 4 *stop-loss-day*))                  
   
   (setq   date-1 date date (add-mkt-days date 1))

  ; (format T "~% short = ~A long = ~A" short long)
;;;check if new long entry
;;;enter on a market order if opens above entry-long.
;;;enter on a  market order if opens below the entry-short.
;;;exit at the close

   (when (and (not short)
                     )
           (setq short (getd date 'open) stop-short (+ short risk))
          ; (if (getd date 'rollover) (setq short (+ short (getd date 'rollover))))   
          
           (setq  trade-short (create-exitpoints-entry-record-list date-1 -1 short))
                 )
 ; (format T "~% date = ~A short = ~A stop-short = ~A obj short = ~A" date short stop-short obj-short)
    (when (and (not long)
                   )
            (setq long (getd date 'open) stop-long (- long risk))
          ;  (if (getd date 'rollover) (setq long (+ long  (getd date 'rollover))))   
            (setq  trade-long (create-exitpoints-entry-record-list date-1 1 long))
            )

;;;;;check if stopped out 
   (when (and long stop-long
               (<= (getd date 'low) stop-long)
                             )
           (push (round (- (*  (calculate-point-value date)(my-pretty-price (- stop-long long)))
                            (exitpoints-comm+slip date))) trades)
           (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price stop-long)
                                   (round (* (calculate-point-value date)(my-pretty-price (- stop-long long))))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))

    (when (and short stop-short
                (>= (getd date 'high) stop-short)
                                   )
           (push (round (- (* (calculate-point-value date)(my-pretty-price (- short stop-short)))
                    (exitpoints-comm+slip date))) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price stop-short)
                                 (round (* (calculate-point-value date) (my-pretty-price (- short stop-short))))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil
           ))



;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd (getd date 'ndate) 'open))
            (push (round (- (* (my-pretty-price (- cover-long long))
                          (calculate-point-value date))(exitpoints-comm+slip date))) trades)
            (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price cover-long)
                                    (round (* (calculate-point-value date)
                                           (my-pretty-price (- cover-long long))))))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil ))

      (when short
           (setq cover-short (getd (getd date 'ndate) 'open))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                           (exitpoints-comm+slip date))) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price cover-short)
                            (round (* (calculate-point-value date) (my-pretty-price (- short cover-short))))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil ))

   );;;closes the dotimes

 
  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round  (list-sum trades))  (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (list-sum trades)  (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (or (min* losers) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round  (drawdown trades))
   
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
(defun create-exitpoints-entry-record-list (date direction entry)
   (let* ((date-1 (getd date 'ydate)))

       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                           ;   (my-round (/ (volatility date 4 1) (volatility date 63 1)) 3) ;;;feature 2
                              (body-range-index date) ;feature 2  10 levels
                              (body-range-index date-1) ;feature 3
                              (body-range-index (getd date-1 'ydate)) ;feature 4
  
                              (r-squared-change-index date 5 1);;feature 5
                              (wpp date) ;feature 6  
                              (candle1 date);;feature 7

                              (r-squared-change-index date 10 2);;feature 8  
                              (r-squared-change-index date 20 4) ;feature 9  
                              (r-squared-change-index date 40 8);;feature 10 
                              (r-squared-change-index date 80 16) ;;feature 11                           
                              (r-squared-change-index date 160 32);;feature 12                          

                              (tkcd-index date-1 );;feature 13
                              (roc-rel-index date 10 90) ;;features 14 10 levels
                              );;;closes the list

))



(defun exitpoints-trade-binsb ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "exitpointstradewarehouse.dat"))
    (maind-x)(set-cat-list)
    (setq exitpointstrades nil exitpoints-bin-codes nil)(clrhash *exitpoints-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record exitpointstrades)))

;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *day-commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record exitpointstrades)
     (dotimes (ith 4)
         (setq bin (encode-contra-trades record (butlast features ith)))
         
          (cond ((gethash bin *exitpoints-trade-warehouse*)
                 (ifn (member record (gethash bin *exitpoints-trade-warehouse*) :test #'equalp)
                     (setf (gethash bin *exitpoints-trade-warehouse*)
                           (cons record (gethash bin *exitpoints-trade-warehouse*)))))
                 ((not (gethash bin *exitpoints-trade-warehouse*))
                  (setf (gethash bin *exitpoints-trade-warehouse*) (list record))
              (push bin exitpoints-bin-codes)))
          ));;;closes the doltimes and dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-exitpointstrade-bins-by-profit exitpointstrades features)


  ))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-exitpointstradesb (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.40)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0)(gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0))
    (setq record (create-exitpoints-entry-record-list date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

    (dotimes (ith 4);;;changed to 3 to help with heap exhaustion
    (setq bin (encode-contra-trades record features))
    (setq contents (gethash bin *exitpoints-trade-warehouse*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *exitpoints-trade-warehouse*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
;    (remove-if #'(lambda(s1) (and (eql *data-name* (svref s1 0))
;                                  (eql nxdate (svref s1 1)))) contents))

;    (remove-if #'(lambda (s1) (> (svref s1 1) date)) content) 
     (if contents (return) (setq features (butlast features)))
;    (if (or (>= (count-if #'(lambda (s) (= (svref s 2) 1)) contents) 1)
;            (>= (count-if #'(lambda (s) (= (svref s 2) -1)) contents) 1))
;        (return) (setq features (butlast features)))
);;closes the dotimes
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
        ((and (plusp results-long)             
              ; (> results-long results-short)
               (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc))
            (> (/ results-long (if (zerop longs) 1 longs))(/ results-short (if (zerop shorts) 1 shorts)))
               (setq epsignal 'UP))
        ((and (plusp results-short)              
              ; (> results-short results-long)
               (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
               (>= (float (/ num-winners-short shorts)) crit-acc))
                (> (/ results-short (if (zerop shorts) 1 shorts))(/ results-long (if (zerop longs) 1 longs))) 
            (setq epsignal 'DOWN))
        
          (t (setq epsignal 'AVOID)))

  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))


;;;requires a candidate list to add to the base features
(defun exitpointstrades-add-one-in (base-features candidate-features)
 (let (winners-list (result 0) average-profit rtb rp single-bins csgof)  

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'exitpoints-trade-bins (append base-features (list ith))))
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
(defun exitpointstrades-leave-one-out (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp csgof)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
                    (apply #'exitpoints-trade-bins (remove ith base-features)))
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


(defun exitpoints-trade-bins (&rest features)
  (let (bin (path (string-append *upper-dir-warehouse* "exitpointstradewarehouse.dat")))
 
 (maind-x)(set-cat-list)
  (setq exitpointstrades nil exitpoints-bin-codes nil)(clrhash *exitpoints-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record exitpointstrades)))


;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record exitpointstrades)
     (setq bin (encode-contra-trades record features))
     (cond ((gethash bin *exitpoints-trade-warehouse*)
            (ifn (member record (gethash bin *exitpoints-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *exitpoints-trade-warehouse*)
                       (cons record (gethash bin *exitpoints-trade-warehouse*)))))
            ((not (gethash bin *exitpoints-trade-warehouse*))
             (setf (gethash bin *exitpoints-trade-warehouse*) (list record))
              (push bin exitpoints-bin-codes)))

    )

    (format T  "~%FEATURES = ~A~%" features)
    (rank-exitpointstrade-bins-by-profit exitpointstrades features)


  ))


(defun find-best-exitpoints-indicator-set ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (exitpointstrades-add-one-in base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)(> result .85)
             (< (fifth (car winners-list)) 1.9) 
             (> (fourth (car winners-list)) .20))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     

     (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 2.90)(return))
        (if (> (fifth (second winners-list)) 2.90)(return)))

      (setq winners-list (exitpointstrades-leave-one-out base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
     base-list
    ))
 
 

(defun build-exitpointstrade-warehouse (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "exitpointstradewarehouse.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "exitpointstradewarehouse.backup"))
           (delete-file (string-append *upper-dir-warehouse* "exitpointstradewarehouse.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "exitpointstradewarehouse.dat"))
          (rename-file (string-append *upper-dir-warehouse* "exitpointstradewarehouse.dat")
                            "exitpointstradewarehouse.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse*
                                           (format nil "~A" market) "exitpointstrades.dat")) 
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
;;;This rank is for using multiple length features

(defun rank-exitpointstrade-bins-by-profit (exitpointstrades features)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0)
        longest-exitpoints-bin-codes (longest-feature (length features)))
   (setq longest-exitpoints-bin-codes
          (remove-if #'(lambda (s) (< (length s) longest-feature)) exitpoints-bin-codes))
   (dolist (ith longest-exitpoints-bin-codes)
     (setq contents (gethash ith *exitpoints-trade-warehouse*))
     (setq result 0 counter 0 )

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
     
         ) ;;;closes dolist over contents
      (if (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))

     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth exitpointstrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if  (plusp (car jth))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length exitpointstrades)(length (num-markets-in-warehouse3 exitpointstrades)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length longest-exitpoints-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length exitpointstrades)(length longest-exitpoints-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length longest-exitpoints-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length exitpointstrades)) 2))
     (format T "CHI SQUARE METRIC = ~7,5F~%" (round (exitpointstrade-chi-squared-gof)))

    (values (round winners)
            (my-round (/ (length exitpointstrades)(length exitpoints-bin-codes)) 1);;;ratio of trades to bins
           ; (round (+ all-winners all-losers))
            (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
            (my-round (/ only-one (length exitpointstrades)) 2)
            (my-round (/ winners all-winners) 2)
            (exitpointstrade-chi-squared-gof)) 
       

 ))
#|
;;;assumes the day-trade-bins has been run first
(defun exitpointstrade-chi-squared-gof ()
  (let (contents (result 0) square-list (all-winners 0)(all-losers 0)
       square percentage-winners chi-squared)
;;;first calculate the profit per trade overall trades 
   (dolist (kth exitpointstrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners 1))
           (setq all-losers (+ all-losers 1))))
   
   (setq percentage-winners (/ all-winners (+ all-winners all-losers)))

  (dolist (ith exitpoints-bin-codes)
     (setq contents (gethash ith *exitpoints-trade-warehouse*))
     (setq result 0 square 0 )
     (dolist (kth contents)
         (if (plusp (svref kth 19))
             (setq result (+ result 1)))
         ) ;;;closes dolist over contents
;;;;result is the observed number of winners in the bin
;;;;the expected number of winners in the bin is
     (setq square
         (/ (expt (- result (* percentage-winners (length contents))) 2) (* percentage-winners (length contents))))

     (push square square-list));;;closes dolist over day-bin-codes

     (setq chi-squared (list-sum square-list));;; number of bins less one is the degrees of freedom
;     (format T "~%CHI SQUARE = ~7,3F  DOF = ~D~%" chi-squared (1- (length day-bin-codes)))
     (chi-square-cdf chi-squared (1- (length exitpoints-bin-codes)))
  
))
|#
;;;assumes the day-trade-bins has been run first
(defun exitpointstrade-chi-squared-gof ()
  (let (contents  square-list (all-winners 0)(all-losers 0) chi-squared-metric chi-squared-probability
       square percentage-winners chi-squared (win-result 0)(lose-result 0)(bin-dollars 0))
;;;first calculate the profit per trade overall trades 
   (dolist (kth exitpointstrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))
   
;;;;this is the percentage of dollars that are winners
   (setq percentage-winners (/  all-winners (+ all-winners (- all-losers))))

  (dolist (ith exitpoints-bin-codes)
     (setq contents (gethash ith *exitpoints-trade-warehouse*))
     (setq win-result 0 square 0 lose-result 0 bin-dollars 0)
     (dolist (kth contents)
        (if (plusp (svref kth 19))
            (setq win-result (+ win-result (svref kth 19)))
          (setq lose-result (+ lose-result (svref kth 19))))
         ) ;;;closes dolist over contents
;;;;result is the observed number of winner dollars in the bin
;;;;the expected number of winner dollars in the bin is
;;;;
    (setq bin-dollars (+ win-result (- lose-result))) 
    (if (zerop bin-dollars)(incf bin-dollars))

    (setq square
         (/ (expt (- win-result (* percentage-winners (+ win-result (- lose-result)))) 2)
             (* percentage-winners bin-dollars)))


     (push square square-list));;;closes dolist over day-bin-codes
  (setq chi-squared-metric
    (round  (setq chi-squared (/ (list-sum square-list)(1- (length square-list))))));;; number of bins less one is the degrees of freedom
  ;   (format T "~%CHI SQUARE = ~7,3F  DOF = ~D~%" chi-squared (1- (length day-bin-codes)))
     (setq chi-squared-probability (chi-square-cdf chi-squared (1- (length exitpoints-bin-codes))))
  (values chi-squared-metric chi-squared-probability)
))


;;;enters with stop orders and exits with stop loss orders
;;;;
(defun exitpoints-simulation-test (market date2 num &optional (features *exitpoints-features*))
 (let (date date+1 stop-long stop-short trades long short   trade-long  
       cover-long cover-short
       ave-win ave-loss losers winners extended-trades trade-short 
        risk epsignal longs long-gains long-acc trading-dates (trade-time 0) 
       shorts short-gains short-acc ; ctr
        date-1 record (running-sum 0) bin draw
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "exitpoints-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "exitpoints-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "exitpoints-diary1.dat")))

   ;   (declare (ignore  short-acc long-acc ctr))
   
       (set-market market)(format T "~%~A~%" *data-name*)   
       (apply #'exitpoints-trade-binsb *exitpoints-features*)

       (if (and num (> num (available-days market date2 )))
           (setq num nil))
       (ifn num (setq num (available-days market date2 )))

       (setq *day-commission* 0 *pips-slippage* 0 *stop-loss-day* 1.125)

    (setq date (add-mkt-days date2 (- num)))
    (setq record (vector date (getd date 'open) 0 0 0))
;    (push record trading-dates)
 ;;;;from date1 to date2
   (dotimes (ith num)
    
      (setq risk  (volatility date 4 *stop-loss-day*))

      (setq date-1 date date (add-mkt-days date 1) date+1 (add-mkt-days date 1))

      (setq record (vector date (getd date 'open) 0 0 0))
       (if long (setf (svref record 2) 1))
       (if short (setf (svref record 2) -1))
 

    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover)))
     ;              obj-long (+ obj-long (getd date 'rollover)))
             (setf (svref record 3)(- (svref record 3) (getd date 'rollover)))
        )

    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover)))
           ;       obj-short (+ obj-short (getd date 'rollover)))
            (setf (svref record 3) (+ (svref record 3) (getd date 'rollover)))
         )

  ;   (format T "~%2 ~A = date  ~A = long  ~A = short" date long short)

;;;;calculate bin-classifier only as needed
   
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-exitpointstradesb date-1 features))
           
       
      
;     (format T "~%3a long= ~A short= ~A epsignal= ~A" long short epsignal)
;;;check if new entry

    (when (and (not short)
               (member epsignal '(OK DOWN))
               (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
               )

          (setq short (getd date 'open) stop-short (my-pretty-price (+ short risk))
                trade-short (list date 'short short)
                ) 
          (setf (svref record 2) -1)
          (setf (svref record 3)
                 (round (- (* (my-pretty-price (- short (getd date 'close))) (calculate-point-value date))
                           (exitpoints-comm+slip date))))
           )

    (when (and (not long)
               (member epsignal '(OK UP))
               (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
                )
           (setq long (getd date 'open) stop-long (my-pretty-price (- long risk))
                 trade-long (list date 'long long)
                 )
           (setf (svref record 2) 1)
           (setf (svref record 3)
                (round (-  (* (my-pretty-price (- (getd date 'close) long)) (calculate-point-value date))
                    (exitpoints-comm+slip date))))
             )
;     (format T "~%4 ~A = date  ~A = entry-short  ~A = entry-long" date entry-short entry-long)
;      (format T "~%4 ~A = epsignal  ~A = long  ~A = short" epsignal long short)
 ;;;check if stopped out on same day of entry
   (when (and long stop-long  (<= (getd date 'low) stop-long))
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                           (exitpoints-comm+slip date))) trades)
           (setq trade-long (apply #'vector
                 (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*)) 
                          (contract-month *data-name* date) 'S
                         (min (getd date 'open) stop-long) (- (min (getd date 'open) stop-long) long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
        ;   (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
                                   (round (* (my-pretty-price (- stop-long long))(calculate-point-value date)))))
           (setf (svref record 3) (- (svref record 3)
                            (round (* (my-pretty-price (- (getd date 'close) long))(calculate-point-value date)))))
           (setq trade-long nil long nil stop-long nil))

;    (format T "~%5 date= ~A stop-short= ~A short = ~A (svref record 3) = ~A" date stop-short short (svref record 3))
    (when  (and short stop-short (>= (getd date 'high) stop-short))
           (push (round (- (* (my-pretty-price (- short  stop-short))(calculate-point-value date))
                            (exitpoints-comm+slip date))) trades)
           (setq trade-short
               (apply #'vector
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*)) 
                         (contract-month *data-name* date) 'S
                         (max (getd date 'open) stop-short)
                         (my-pretty-price (- short (max (getd date 'open) stop-short)))))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                   (round (- (* (my-pretty-price (- short stop-short)) (calculate-point-value date))
                      (exitpoints-comm+slip date)))))
           (setf (svref record 3) (- (svref record 3)
                 (round (- (* (my-pretty-price (- short (getd date 'close))))(exitpoints-comm+slip date)))))
           (setq trade-short nil short nil stop-short nil))

;   (format T "~%6 date= ~A long= ~A short = ~A stop-long = ~A stop-short= ~A" date long short stop-long stop-short)

;;;check if met exit criteria on day of entry (exit at end of day)
;;;;actually exits on the open of the next day.
     (when long
            (setq cover-long (getd  date+1 'open))
            (push (round (* (my-pretty-price (- cover-long long))(exitpoints-comm+slip date))) trades)
            (setq trade-long (apply #'vector (append trade-long (list date+1 (cdr (assoc *data-name* *ninja-symbol*))
                          (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push trade-long extended-trades)
        ;    (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                (* (my-pretty-price (- cover-long long))(calculate-point-value date))))
            (setq trade-long nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date+1 'open))
           (push (round (* (my-pretty-price (- short cover-short))(exitpoints-comm+slip date))) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                         (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push trade-short extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
              (* (my-pretty-price (- short cover-short))(calculate-point-value date))))
           (setq trade-short nil short nil stop-short nil))

    ;   (format T "~%5 ~A = date  ~A = entry-short  ~A = entry-long" date entry-short entry-long)
       (setf (svref record 4) (+ (svref record 3) running-sum))
       (setq running-sum (svref record 4))
       (push record trading-dates)
       (setq record nil);;;reset to nil at end of each trading date
      
     ) ;;;closes the dotimes
;;;;this writes out the diary file
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
           (round (svref ith 4)))
     ))


  (with-open-file (str summary-path :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) 
          )

     (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))

      (format str "P/L= ~16D  NUMBER TRADES=   ~6D   ACCURACY=       ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN=  ~8D   AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=    ~4,2,0,'*,' F  ~
        LARGEST LOSS=   ~7D   LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract=  ~10D  "
       (round (list-sum trades)) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (list-sum trades)  (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (or (min* losers) 0))

       (round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (/ trade-time (if (zerop (length trades)) 1 (length trades))) 1)
      (setq draw (round (drawdown trades)))
      (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
       )

     (format str "~%~%MIN INITIAL $=      ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/  (list-sum trades) 
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                                           365.25))
                       (* 1000 (ceiling (max 1.0 (* 3 (abs draw))) 1000))) ))
        )
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)(svref ith 7)
         (svref ith 8)
         (round (* (calculate-point-value (svref ith 0)) (svref ith 8))))

     ));;closes the dolist and with-open-file
  ; ); closes the when

    (values (round (list-sum trades))
            (length trades) trades)
   ));;;closes the let and the defun


;;;;this function expects that you have run (day-trade-bins ...) already
(defun display-exitpoints-bins-by-expected-value (feature)
  (let (contents expected-value-list result (winners 0)(losers 0) longs shorts
        num-trades first-date last-date
        (path1 (string-append *output-upper-dir* "exitpoints-expected-value"
               (format nil "~A" feature) ".dat")))

     (multiple-value-setq (num-trades first-date last-date )
               (num-trades-in-warehouse3 'all exitpointstrades))
     (format T "~%NUM-TRADES= ~A" num-trades) 

   (dolist (ith exitpoints-bin-codes)
     (setq contents
           (gethash ith *exitpoints-trade-warehouse*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (svref kth 19))))
     (setq expected-value-list
          (cons (list (/ result (length contents)) ith (length contents) result winners losers) expected-value-list)));;;closes the dolist over bin-codes

   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (vsort expected-value-list #'> 'car)
    ;     (format stream "Profit/trade  Bin         NUM   #WINNERS   #LOSERS   Profit~%~%")
    ;     (dolist (jth expected-value-list)
    ;         (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
    ;            (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
         (format stream "~%Total #Trades = ~D  #WINNERS = ~D    $Profit = ~D"
             (list-sum (mapcar #'(lambda(s) (nth 2 s)) expected-value-list))
             (list-sum (mapcar #'(lambda(s) (nth 4 s)) expected-value-list))
             (round (list-sum (mapcar #'(lambda(s) (nth 3 s)) expected-value-list))))
          (format stream "~%TOTAL DAYS = ~D CHI SQUARED METRIC = ~D~%~%"
             (total-available-days last-date (num-markets-in-warehouse3 swings))  
              (exitpointstrade-chi-squared-gof)) 
;;;;;;now show by direction
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
 
                ) ;;;;closes the with-open-file
 


 ))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-exitpointstrade-indicators ()
   (let ((path (string-append *upper-dir-warehouse* "exitpointstradewarehouse.dat"))
          date-1 date-2)
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate) date-2 (getd date-1 'ydate))

     (setf (svref ith 5) (ldev-index date-1 5));;feature 3
     (setf (svref ith 6)(body-range-index date-2));;;feature 4
     (setf (svref ith 7)(body-range-index date-1)) ;;feature 5
     (setf (svref ith 11) (body-range-index (getd date-2 'ydate)));;feature 9
     (setf (svref ith 9)(r-squared-change-index date-1 5 1));;feature 7
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
;;;X for Dubai futures markets
;;;;this is a day trading system with entry based on two-bar

(defun update-dubaitrade-warehouse5 (date &optional (markets *dubai-warehouse-list*))

   (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-dubai-trades5 ith date (available-days ith date 300)))
 
   (build-dubaitrade-warehouse5 markets) 
   (setq *dubai-features5* (find-best-dubai-indicator-set5a))
   (portfolio-simulation3 '(dubai5) date 600 (list *dubai-list*))
)



(defun build-dubaitrade-warehouse5 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "dubaitradewarehouse5.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "dubaitradewarehouse5.backup"))
           (delete-file (string-append *upper-dir-warehouse* "dubaitradewarehouse5.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "dubaitradewarehouse5.dat"))
          (rename-file (string-append *upper-dir-warehouse* "dubaitradewarehouse5.dat")
                            "dubaitradewarehouse5.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "dubaitrades5.dat")) 
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


(defun populate-dubai-trades5 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 cover-long cover-short entry-long entry-short 
       risk-long risk-short 2-bar-list
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "dubaitrades5.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

   (format outfile "~%~A~%" market)

   (setq  *max-day-risk* 2000) 
   (if (member market *forex-list*) (setq *day-commission* 0 *pips-slippage* 6)
        (setq *day-commission* 10 *pips-slippage* 0))
 
   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

;   (format T "~A ~A~%" date (getd date 'close))
 
  (setq 2-bar-list (2-bar date) entry-short (second 2-bar-list) entry-long (third 2-bar-list))
    
  (setq entry-short (- entry-short (* 3.0 (index-tick-size))) entry-long (+ entry-long (* 3.0 (index-tick-size))))
  (setq stop-long (max (getd date 'low) entry-short) stop-short (min (getd date 'high) entry-long))
  (setq risk-long (- entry-long stop-long) risk-short (- stop-short entry-short))
  
  
   (setq  date-1 date date (add-mkt-days date 1))

;   (format T "~%entry-long = ~A entry-short =~A  2-bar-list = ~A low= ~A high= ~A~%"
;          entry-long entry-short 2-bar-list (getd date 'low)(getd date 'high))
;;;
   (when (and (<= (- (getd date 'low) (* 3 (index-tick-size))) entry-short)
           
              (>= (* risk-short (calculate-point-value date-1))
                  (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size)))
	      (<= (* risk-short (calculate-point-value date-1)) *max-day-risk*)
              )
          (setq short (min (getd date 'open) entry-short)
               
               short-trade (create-dubaitrade-entry-record-list5 date-1 -1 short)
                 ))

    (when (and (>= (+ (getd date 'high) (* 3 (index-tick-size))) entry-long)
             
               (>= (* risk-long (calculate-point-value date-1))
                   (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size)))
               (<= (* risk-long (calculate-point-value date-1)) *max-day-risk*)
              )
           (setq long (max (getd date 'open) entry-long)
                
                 long-trade (create-dubaitrade-entry-record-list5 date-1 1 long)
                 ))

 ; (format T "~% long = ~A short = ~A~%" long short)
;;;check if stopped out on same day

   (when (and long stop-long
              ; (or (and (<= (getd date 'low) stop-long)
              ;          (>= (getd date 'open) (getd date 'close)))
              ;     (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5)) 
              ;    (<= (getd date 'close) stop-long))
               (<= (- (getd date 'low) (* 3 (index-tick-size))) stop-long)
               )
           (push (- stop-long long) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date) (- stop-long long)))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
              ; (or (and 
              ;          (<= (getd date 'open) (getd date 'close)))
              ;     (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))      
              ;      (>= (getd date 'close) stop-short))
                   
               (>= (+ (getd date 'high)(* 3 (index-tick-size))) stop-short)
                 )
           (push (- short stop-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date) (- short stop-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (- (getd date 'close)(* 3 (index-tick-size))))
            (push (- cover-long long) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date) (- cover-long long)))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (+ (getd date 'close)(* 3 (index-tick-size))))
           (push (- short cover-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date) (- short cover-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes

   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ *day-commission* (calculate-point-value date))
                                          (* *pips-slippage* (index-tick-size)))) trades))

  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (calculate-point-value date) 1))
                            (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )

   (format outfile "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D"
       (round (* (list-sum trades) (or (calculate-point-value date) 1))) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (/ (* (list-sum trades) (or (calculate-point-value date) 1)) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (round (* (or (min* losers) 0) (or (calculate-point-value date) 1)))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
       (round (* (drawdown trades)(or (calculate-point-value date) 1)))
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


(defun dubai-trade-bins5a ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "dubaitradewarehouse5.dat"))

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
     (setq bin (encode-day-trades3 record features))
    
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


(defun dubai-trade-bins5b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "dubaitradewarehouse5.dat"))

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
       (setq bin (encode-day-trades3 record (butlast features ith)))
      
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
(defun create-dubaitrade-entry-record-list5 (date direction entry)
   (let* ((date-1 (getd date 'ydate)))
     ;  (multiple-value-setq (csignal period) (cycle-signal date 10 20))
      ; (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))

                              (body-range-index date);;feature 2
                              (body-range-index date-1);;feature 3
                              (body-range-index (getd date-1 'ydate)) ;;features 4

                              (r-squared-change-index date 5 1);;feature 5  
                              (r-squared-change-index date-1 5 1);;;feature 6
                              (r-squared-change-index (getd date-1 'ydate) 5 1);;;feature 7

                              (tkcd-index date) ;;;feature 8  
                              (tkcd-index date-1 )  ;feature 9
                              (tkcd-index (getd date-1 'ydate)) ;;;feature 10

                              (cloud-index date) ;feature 11

                              (volatility-ratio-index date 1 5 1) ;; feature 12  
                              (volatility-ratio-index date-1 1 5 1) ;;feature 13 
                        
                              (range-index1 date 5)      ;;  feature 14 
                          
                              );;;closes the list

))

;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-dubaitrades5b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-dubaitrade-entry-record-list5 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-day-trades3 record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda (s1) (and ;(eql *data-name* (svref s1 0))
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

    (cond ((and (plusp results-long)(plusp results-short)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
             (setq epsignal 'OK))
        ((and (plusp results-long)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc))
              (setq epsignal 'UP))
        ((and (plusp results-short)
               (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
               (>= (float (/ num-winners-short shorts)) crit-acc))
              (setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


(defun find-best-dubaitrade5 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk   stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long
        action directive1  
         mt-dubai-path2  
         oco-code
         central-exit-time central-entry-time central-cancel-time central-end-session-time
	  (time-zone "DT") offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))

    (setq *entry-factor* .3333  *stop-loss-day* 1.125 *max-day-risk* 5000 *min-dubai-expected-value* 30) ; ;;; 1.0557)

    (setq ; cannon-epic-path2 (string-append *daily-output-dir* "cannon-epic.csv")
;          striker-epic-path2 (string-append *daily-output-dir* "striker-epic.csv")
;          kingsview-epic-path2 (string-append *daily-output-dir* "kingsview-epic.csv")
          mt-dubai-path2 (string-append *ninja-output-dir* "mt-epic-" (format nil "~A" date1) ".csv")
	  )

  (format T "~%EPSIGNAL = ~A~%  longs = ~A  long-gains = ~A ~%" epsignal longs long-gains )
  (format T "  shorts = ~A  short-gains = ~A ~%" shorts short-gains)


   (setq risk  (volatility tdate 4 *stop-loss-day*) risk (+ risk (* 3.0 (index-tick-size))))

   (if (and (member epsignal '(OK UP))(> longs 0)
     
             )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))(> shorts 0)
      
         )
    (push 'DN trade-direction)(push 'FT trade-direction))
  (format T "~%risk = ~A trade-direction= ~A" (* risk (calculate-point-value tdate)) trade-direction)
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

      (format output " ~A~%" action)
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~A~%" shorts short-gains short-acc)
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


(defun find-best-dubai-indicator-set5a ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (dubaitrades-add-one-in5a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
     (if (or (not candidate-list)(> result .97))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      
       (if (neql (second (car winners-list)) 1)
          (if (< (fourth (car winners-list)) .80)(return))
        (if (< (fourth (second winners-list)) .80)(return)))

      (setq winners-list (dubaitrades-leave-one-out5a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
    
     (format T "~%Best indicator set = ~A~%" base-list)
   base-list
    ))
 

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun dubaitrades-add-one-in5a (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'dubai-trade-bins5a (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

;;;requires a base features list
(defun dubaitrades-leave-one-out5a (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 

  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'dubai-trade-bins5a (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))


(defun dubaitrade-simulation-test5 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0)
        risk-long risk-short singles 2-bar-list
       (ave-win 0) (ave-loss 0) (losers 0) (winners 0) extended-trades long-trade
        short-trade stop-long stop-short
       long-acc short-acc bin draw ignore ;draw-ave draw-90
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "dubai5-summary5.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "dubai5-simulation5.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "dubai5-diary5.dat")))
    
 
    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

     (set-market market) (format T "~%~A~%" *data-name*)
  
   (setq  *max-day-risk* 2000)  
 
   (if (member market *forex-list*) (setq *day-commission* 0 *pips-slippage* 6)
      (setq *day-commission* 10 *pips-slippage* 0))
   
   (multiple-value-setq (ignore ignore ignore singles) (apply #'dubai-trade-bins5b features))

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

   (if (member market *forex-list*)
       (setq *min-epic-expected-value* (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
      (setq *min-epic-expected-value* 30))


 ;;;;from date1 to date2
 (dotimes (ith num)

  
  (setq 2-bar-list (2-bar date) entry-short (second 2-bar-list) entry-long (third 2-bar-list))
   (setq entry-short (- entry-short (* 3 (index-tick-size))) entry-long (+ entry-long (* 3 (index-tick-size))))
   (setq stop-short (min (getd date 'high) entry-long) stop-long (max (getd date 'low) entry-short))

   (setq risk-long (- entry-long stop-long) risk-short (- stop-short entry-short))

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))


;    (format T "~%~A~%" date)
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-dubaitrades5b date-1 features))
      
      
       )
;   (format T "~%longs= ~A twr-long = ~A long-gains = ~A epsignal = ~A" longs twr-long long-gains epsignal)
;;;check if new entry

   (when (and ;(> (getd date 'high) entry-short)
              (<= (- (getd date 'low)(* 3 (index-tick-size))) entry-short)
              (<= (* risk-short (calculate-point-value date-1)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
           
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
                 )
          (setq short (min (getd date 'open) entry-short)
                short-trade (list date 'short short)

                )
           (setf (svref record 2) -1)
           (setf (svref record 3)
                (- (svref record 3) (/ *day-commission* (calculate-point-value date))(* *pips-slippage* (index-tick-size))))
                 )

 ;    (format T "~%shorts = ~A short-gains = ~A risk= ~A entry-long = ~A" shorts short-gains risk entry-long)
    (when (and ;(< (getd date 'low) entry-long)
               (>= (+ (getd date 'high)(* 3 (index-tick-size))) entry-long)
	        (<= (* risk-long (calculate-point-value date-1)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
             
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
             )

           (setq long  (max (getd date 'open) entry-long)
                
                 long-trade (list date 'long long))
           (setf (svref record 2) 1)
           (setf (svref record 3)
                (- (svref record 3) (/ *day-commission* (calculate-point-value date))(* *pips-slippage* (index-tick-size))))
                 )

;;;check if stopped out on same day

   (when (and long stop-long
;              (or (and (<= (getd date 'low) stop-long)
;                       (>= (getd date 'open) (getd date 'close)))
;                   (and (<= (getd date 'low) stop-long)(<= (ave-buy-ratio date 1) .5))    
;                   (<= (getd date 'close) stop-long))
          (<= (- (getd date 'low)(* 3 (index-tick-size))) stop-long)
            )
           (push (- stop-long long) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                   (contract-month *data-name* date) 'S stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
 ;              (or (and (>= (getd date 'high) stop-short)
 ;                       (<= (getd date 'open) (getd date 'close)))
 ;                  (and (>= (getd date 'high) stop-short)(>= (ave-buy-ratio date 1) .5))   
 ;                (>= (getd date 'close) stop-short))
           (> (+ (getd date 'high)(* 3 (index-tick-size))) stop-short)    
             )
           (push (- short stop-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                   (contract-month *data-name* date) 'S stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short stop-short)))
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (- (getd date 'close)(* 3 (index-tick-size))))
            (push (- cover-long long) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'N cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3) (- (getd date 'close) long)))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (+ (getd date 'close)(* 3 (index-tick-size))))
           (push (- short cover-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                              (contract-month *data-name* date) 'N cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short (getd date 'close))))
           (setq short-trade nil short nil stop-short nil))


      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes


     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (svref ith 0) (svref ith 1) (svref ith 2) (round (* (calculate-point-value (svref ith 0))(svref ith 3)))
           (round (* (calculate-point-value (svref ith 0)) (svref ith 4))))
     ))

;;;
;;;

;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ *day-commission* (index-point-value)) (* *pips-slippage* (index-tick-size)))) trades))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win  (/ (* (list-sum winners) (or (calculate-point-value date) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (calculate-point-value date) 1))
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

       (round (* (list-sum trades) (or (calculate-point-value date) 1))) (length trades)
       (/ (- (length trades) (length losers))
          (if (zerop (length trades)) 1 (length trades)))

       (round (/  (* (list-sum trades) (or (calculate-point-value date) 1)) (if (zerop (length trades)) 1 (length trades))))
       (round ave-win)
       (round ave-loss)
       (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
       (round (* (or (min* losers) 0) (or (calculate-point-value date) 1)))

       (round (* (or (max* winners) 0)(or (calculate-point-value date) 1)))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (my-round (* 100 (/ (length trades) num)) 2)
      (setq draw (round (* (drawdown trades)(or (calculate-point-value date) 1))))
       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (calculate-point-value date) 1))) trades))) 0)
      (/ (count 'S (mapcar #'(Lambda(s1) (svref s1 6)) extended-trades)) 
         (if (zerop (length extended-trades)) 1 (length extended-trades)));;percentage of trades stopped out
         )
  ;    (setq ext extended-trades)
     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 
 ;   (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
   
       (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/ (* (list-sum trades) (or (calculate-point-value date) 1))
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

  (values (round (* (list-sum trades) (or (calculate-point-value date) 1)))
          (length trades) trades)
   ))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-dubaitrade-indicators5 ()
   (let ((path (string-append *upper-dir-warehouse* "dubaitradewarehouse5.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate))

      (setf (svref ith 6) (candle date-1 (1+ (- (n-day-extreme-dates1 date-1 9)))));;feature 4
 ;    (setf (svref ith 7)(pldot-index date-1)) ;;feature 5
  ;   (setf (svref ith 9) (r-squared-change-index date-1 5 1));;feature 7
      (setf (svref ith 14)(reversal-index date-1)) ;;feature 12
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun update-meats-warehouse2 (date &optional (markets  *meats-list* ))

  (maind-x)(set-cat-list)
  (dolist (ith markets)
        (set-market ith)
       (populate-meats-trades2 ith date (available-days ith date)))
 
   (build-meats-warehouse2 markets) 
   (setq *meats-features2m* (find-best-indicator-set2m))
   (portfolio-simulation3 '(meats) date 5040 (list *meats-list*))

  ; (portfolio-simulation4 '(forexday) date 3500 (list markets))
)


(defun populate-meats-trades2 (market date2 num &optional (outfile T))
 (let (date trades long short   date-1 cover-long cover-short entry-long entry-short risk vri
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "meatstrades2.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

   (format outfile "~%~A~%" market)
   (setq *entry-factor* .3333 *stop-loss-day* 1.25
        *max-day-risk* 2500) 
;   (setq *entry-factor* .764 *stop-loss-day* .944) ;;;;1.0557)
   (setq *day-commission* 75 *pips-slippage* 0)

    (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)
 
  (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))

   (setq risk (volatility date 4 *stop-loss-day*))
   (setq vri (volatility-ratio-index date 4 63 1))

   (setq  date-1 date date (add-mkt-days date 1))
    
    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk));;;needs to be after the rollover

   (when (and (<= (getd date 'low) entry-short)
             ; (> (getd date 'high) entry-short)
              (>= (* risk (calculate-point-value date))
                  (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (> vri -1)
              )
          (setq short  (min entry-short (getd date 'open))
                short-trade (create-meats-entry-record-list2 date-1 -1 short)
                 ))

    (when (and (>= (getd date 'high) entry-long)
             ;  (< (getd date 'low) entry-long)
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
	       (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (> vri -1)
                )
           (setq long (max entry-long (getd date 'open))
                 long-trade (create-meats-entry-record-list2 date-1 1 long)
                 ))
;;;check if stopped out on same day

   (when (and long stop-long
    ;           (or (and (<= (getd date 'low) stop-long)
    ;                    (>= (getd date 'open) (getd date 'close)))
    ;               (<= (getd date 'close) stop-long))
              (<= (getd date 'low) stop-long)     
          )
           (push (round (- (* (my-pretty-price (- stop-long long)) (calculate-point-value date)) (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date)(my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
          ;     (or (and (>= (getd date 'high) stop-short)
          ;              (<= (getd date 'open) (getd date 'close)))
          ;          (>= (getd date 'close) stop-short))
               (>= (getd date 'high) stop-short)
                    )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date)(my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd date 'close))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date)(my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date))) trades)
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
(defun create-meats-entry-record-list2 (date direction entry)
   (let* ((date-1 (getd date 'ydate)) ); period)
    
     ; (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                             (body-range-index date);;feature 2  
                             (body-range-index date-1 ) ;feature 3  
                             (body-range-index (getd date-1 'ydate)) ;; feature 4
 
                             (r-squared-change-index date 5 1) ;;;feature 5  
                             (r-squared-change-index date-1 5 1);;feature 6                             
                             (r-squared-change-index (getd date-1 'ydate) 5 1) ;feature 7

                             (r-squared-change-index date 10 2)      ;;  feature 8             
                             (r-squared-change-index date 20 4) ;;;feature 9
                             (r-squared-change-index date 40 8);;;feature 10                          

                             (r-squared-change-index date 80 16) ;;;feature 11
                             (r-squared-change-index date 160 32) ;;features 12 
                             (tkcd-index date) ;feature 13

                             (range-index1 date 5 )  ;feature 14
                                                    
                              );;;closes the list

))

;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-meats-indicators2 ()
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

   ;   (setf (svref ith 13) (reversal-index date-1));;feature 11
 ;    (setf (svref ith 7)(pldot-index date-1)) ;;feature 5
   ;  (setf (svref ith 6) (range-index date-1));;feature 4
   ;   (setf (svref ith 8)(lslope-change-index date-1 5 1)) ;;feature 6
      (setf (svref ith 16) (volatility-ratio-index date-1 4 63 1)) ;;;feature 14
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


(defun meats-trade-bins2a ( &rest features)
  (let (bin path)

  ; (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "currencytrades2.dat"))
   (setq path (string-append *upper-dir-warehouse*  "meatswarehouse2.dat"))
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
     (setq bin (encode-meats-trades2 record features))


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

(defun meats-trade-bins2b ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "meatswarehouse2.dat"))

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
       (setq bin (encode-meats-trades2 record (butlast features ith)))
      
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



(defun encode-meats-trades2 (record features)
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


(defun find-best-indicator-set2m ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (meats-add-one-in2a base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
     (if (or (not candidate-list)(> result .97))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
      
       (if (neql (second (car winners-list)) 1)
          (if (< (fourth (car winners-list)) .80)(return))
        (if (< (fourth (second winners-list)) .80)(return)))

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
(defun meats-add-one-in2a (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'meats-trade-bins2a (append base-features (list ith))))
     (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))



;;;requires a base features list
(defun meats-leave-one-out2a (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'meats-trade-bins2a (remove ith base-features)))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))



;;;
(defun meats-simulation-test2 (market date2 num &optional (features nil))
 (let (date trades long short entry-long entry-short cover-long cover-short  record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk vri
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw draw-ave draw-90 draw-99 
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "meats-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "meats-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "meats-diary2.dat")))
;   (declare (ignore long-acc short-acc))

     (if (and num (> num (available-days market date2)))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

   (set-market market) (format T "~%~A~%" *data-name*)
   
   (setq *entry-factor* .3333 *stop-loss-day* 1.250 
          *max-day-risk* 2500 *min-epic-expected-value* 75 
          *day-commission* 75 *pips-slippage* 0)
 
     (apply #'meats-trade-bins2b features) 
   
   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))

   (setq risk (volatility date 4 *stop-loss-day*))
   (setq vri (volatility-ratio-index date 4 63 1))

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-meatstrades2b date-1 features))
      
       )

;;;check if new entry

   (when (and ;(> (getd date 'high) entry-short)
              (<= (getd date 'low) entry-short)
              (<= (* risk (calculate-point-value date)) *max-day-risk*)
              (member epsignal '(OK DOWN))
              (>= (/ short-gains shorts) *min-epic-expected-value*)
            
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
              (> vri -1)
                  )
          (setq short  (min entry-short (getd date 'open))
                short-trade (list date 'short short)

                )
           (setf (svref record 2) -1)

                 )


    (when (and ;(< (getd date 'low) entry-long)
               (>= (getd date 'high) entry-long)
	       (<= (* risk (calculate-point-value date)) *max-day-risk*)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) *min-epic-expected-value*)
             
               (> (/ long-gains longs)
                  (if (> shorts 0.0)(/ short-gains shorts) 0))
               (> vri -1)
               )

           (setq long (max entry-long (getd date 'open))
                 long-trade (list date 'long long)

                 )
           (setf (svref record 2) 1)
          
            )

;;;check if stopped out on same day

   (when (and long stop-long
;              (or (and (<= (getd date 'low) stop-long)
;                       (>= (getd date 'open) (getd date 'close)))
;                   (<= (getd date 'close) stop-long))
               (<= (getd date 'low) stop-long)
           )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date))) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'S
                                                          stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
     ;      (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))(comm+slip date)))))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
;               (or (and (>= (getd date 'high) stop-short)
;                        (<= (getd date 'open) (getd date 'close)))
;                    (>= (getd date 'close) stop-short))
               (>= (getd date 'high) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                           stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))(comm+slip date)))))
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (getd date 'close))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date))) trades) 
            (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
         ;   (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)
                   (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))(comm+slip date)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))(comm+slip date))) trades)
           (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'N
                                                           cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)
         ;  (setf (svref record 2) -1)
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

(defun build-meats-warehouse2 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "meatswarehouse2.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "meatswarehouse2.backup"))
           (delete-file (string-append *upper-dir-warehouse* "meatswarehouse2.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "meatswarehouse2.dat"))
          (rename-file (string-append *upper-dir-warehouse* "meatswarehouse2.dat")
                            "meatswarehouse2.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "meatstrades2.dat")) 
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



(defun find-best-meatstrade2 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk vri  stop-short stop-long  entry-long entry-short
        trade-direction cover-short cover-long
        action directive1 offset
       ; ninja-forex-path2
        pacific-exit-time pacific-entry-time pacific-cancel-time pacific-end-session-time oec-symbol
        oco-code retail-path2 sol-score-path3 
        striker-score-path3   striker-starter-path3   kingsview-starter-path3
        central-exit-time central-entry-time central-cancel-time central-end-session-time
	;  (time-zone "CT")
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
    (format T "~%~A~%" *data-name*)
    (setq *entry-factor* .3333 *stop-loss-day* 1.250 *max-day-risk* 2500
          *min-epic-expected-value* 75
          *day-commission* 75 *pips-slippage* 0)

    (setq sol-score-path3 (string-append *daily-output-dir* "sol-score.csv")
          
          kingsview-starter-path3 (string-append *daily-output-dir* "kingsview-starter.csv")
          striker-score-path3 (string-append *daily-output-dir* "striker-score.csv")
          striker-starter-path3 (string-append *daily-output-dir* "striker-starter.csv")
         ; cannon-score-path3 (string-append *daily-output-dir* "cannon-score.csv")
         ; cannon-starter-path3 (string-append *daily-output-dir* "cannon-starter.csv")
          retail-path2 (string-append *daily-output-dir* "futures-view.txt")
         )
  
     (setq vri (volatility-ratio-index tdate 4 63 1))
   (format T "~%EPSIGNAL = ~A VRI= ~A~%  longs = ~A  long-gains = ~A twr-long = ~7,5F~%"
            epsignal vri longs long-gains  twr-long)
  (format T "  shorts = ~A  short-gains = ~A twr-short = ~7,5F~%" shorts short-gains  twr-short)

   (setq risk (volatility tdate 4 *stop-loss-day*))
   
   (if (and (member epsignal '(OK UP))(> longs 0)
            (>= (/ long-gains longs) *min-epic-expected-value*)
            (> twr-long 1.0)(<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
            (> vri -1)
            )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))(> shorts 0)
            (>= (/ short-gains shorts) *min-epic-expected-value*)
            (> twr-short 1.0) (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
            (> vri -1)
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

      (format output " ~A VRI= ~A~%" action vri)
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
(defun bin-classifier-meatstrades2b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-meats-entry-record-list2 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
      (setq bin (encode-meats-trades2 record features))
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

    (cond ((and (plusp results-long)(plusp results-short)
              (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
              (setq epsignal 'OK))
        ((and (plusp results-long) (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)) (setq epsignal 'UP))
        ((and (plusp results-short) (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc))(setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


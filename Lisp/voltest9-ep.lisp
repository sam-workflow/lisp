;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


;;;;This is the swing trade system that is 
;;; 
;;;;
;;;;entry is on the open of the next day.

;;;;;;;;;;;;;;;;;SWING TRADES FUNCTIONS with vectors
;;;;
(defun update-qswingtrade-warehouse (date &optional (markets *sp100-list*))
    (maind-x)(set-cat-list)

    (dolist (ith markets)
       (set-market ith)
       (populate-qswing-trades ith date (min 4000 (available-days ith date 900))))
 
     (build-qswingtrade-warehouse markets) 
;    (setq *qswing-features*   (find-best-qswing-indicator-set) )
;    (portfolio-simulation3 '(qswing) date 4000 (list markets))
;     (indicator-studyq)
    (apply #'qswing-trade-bins *qswing-features*);;;needed for display of unfltered trades
    (display-unfiltered-trades swings)
)

;;;with this system we enter a distance from the open price.

(defun populate-qswing-trades (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long  entry-short entry-long 
       ave-win ave-loss losers winners extended-trades trade-short date-1 ;chan7; slope
       (days-in-trade 0) wp can cci5l cci5h ccir21-11 ccir21-5 ;  cci-h cci-l can 
       dirp ;baseline ; wwasi 
        roc5 ave5  vri5 rsi2 rsi2xl rsi2xh ;aved ; vri ccir  
       early-exit prev-stop-long prev-stop-short (time-allowed 21) ;rsid proj (period 5)
       cover-long cover-short
       (stockp (member market (append *sp100-list* *stocks-list*))) 
      ; bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline ;dir sdate stop-loss
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "qswingtrades.dat")))

   (maind-x)(set-cat-list)(set-market market)

   (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 6))
          ((member market *dubai-list*)(setq *commission* 30  *pips-slippage* 0))
          ((member market (union *dow30-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0))
          (t (setq *commission* 40 *pips-slippage* 0)))

   (setq *entry-factor-swing* 0.0 )
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))


 ;;;;from date1 to date2
  (dotimes (ith num)


;;;stop or entry

    ; (multiple-value-setq (cover-short cover-long)(vprices5 date 4 1.382 1))
     (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction date 3))
     (multiple-value-setq (cci5h cci5l) (cci-high-low date 5 3))

     (setq can (candle-composite date 3)
            wp (wpp1-composite date 3) 

           rsi2 (rsi date 2) 
           roc5 (ep-roc date 5 50)
           ave5 (ave-exp date 5 )
             
           ccir21-11 (cci-range date 21 11) ccir21-5 (cci-range date 21 5)
          
           vri5 (volume-ratio-index date 5 'vi1-20);;;regresssion of volume slope past 5 days
       
               )
  
    (cond ((and long );(eql dirp 'DOWN))
                (setq stop-long nil ;(n-day-low date 5)
                     cover-long nil));(ave date 7)))
      ;    ((and long (eql dirp 'UP))
       ;         (setq stop-long nil ;(my-pretty-price stop-loss)
        ;              cover-long nil))
 ;         ((and (not long)(eql dirp 'DOWN))
  ;             (setq entry-long baseline
   ;                  stop-long nil cover-long nil)) 
    ;      ((and (not long)(eql dirp 'UP))
     ;          (setq entry-long nil
      ;               stop-long nil cover-long nil)) 
          ((and short );(eql dirp 'UP))
               (setq stop-short nil ;(n-day-high date 11)
                     cover-short nil));(ave date 7)))
     ;     ((and short (eql dirp 'DOWN))
      ;          (setq stop-short baseline;(my-pretty-price stop-loss)
       ;               cover-short nil))
        ;   ((and (not short)(eql dirp 'UP))
         ;      (setq entry-short baseline
          ;           stop-short nil cover-short nil)) 
  
            )
  ; (if (eql dir 'LONG)
;       (format T "~%1Date = ~A  cover-short= ~A  stop-short= ~A"
;                 date cover-short stop-short )
  
  ;    (format T "~%1Date = ~A cover-long= ~A  stop-long = ~A "
   ;             date cover-long stop-long  ))
  
 
    (setq  date-1 date date (add-mkt-days date 1))
   ; (setq date-2 (getd date-1 'ydate))
   (setq entry-short (- (getd date 'open) (volatility date-1 21 *entry-factor-swing*))
         entry-long (+  (getd date 'open) (volatility date-1 21 *entry-factor-swing*))
          )
   
  ;   (setq stop-long  (fmax prev-stop-long stop-long); (n-day-low date (- period 1)))
  ;         stop-short (fmin prev-stop-short stop-short)); (n-day-high date (- period 1))))
  ;   (setq prev-stop-long stop-long prev-stop-short stop-short)

   ;  (setq entry-long (fmin (my-pretty-price (getd date 'open)) 
 ;                            (nth-value 1 (pivot-points1 date-1 'week)))
 ;          entry-short (fmax (my-pretty-price (getd date 'open))
 ;                            (nth-value 3 (pivot-points1 date-1 'day))))

     
    (if (or long short) (incf days-in-trade))

    (when (and (getd date 'rollover) long)
          (setq long (+ long (getd date 'rollover))
         ;       stop-long (+ stop-long (getd date 'rollover)) 
          ;     entry-short (+ entry-short (getd date 'rollover))

    ))
   (when (and (getd date 'rollover) short)
          (setq short (+ short (getd date 'rollover))
    ;	        stop-short (+ stop-short (getd date 'rollover))
               ;obj-short (+ obj-short (getd date 'rollover))
     ;           entry-long (+ entry-long (getd date 'rollover))
       
         ))
  
   ;(format T "~%~%2date= ~A dir= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A macdd= ~A~%"
    ;          date dir long stop-long short stop-short macdd)
; (format T "~%entry-long = ~A entry-short = ~A" entry-long entry-short)

;;;check if exit based on criteria at open of the day  

   (when  (and long  
               (or  (> rsi2 65)
                    (= wp -1)(= can -1) ;;;to take profit    
                    (and (plusp roc5)
                         (> (getd date-1 'close) ave5))
                    )
                  )
      ;     (format T "~%Exited long with Criteria day = ~A long = ~A cover-long = ~A stop-long= ~A~%"
     ;              date long cover-long stop-long) 
          (push (round (- (* (my-pretty-price (- (getd date 'open) long))
                             (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (getd date 'open))
                       (round (* (calculate-point-value date)(my-pretty-price (- (getd date 'open)  long))))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0 ))

   (when  (and short  
                (or (< rsi2 35)                  
                    (= wp 1)(= can 1)                 
                    (and (minusp roc5)
                        (< (getd date-1 'close) ave5))
                   )  
                  )
       ;   (format T "~%Exited short with Criteria day = ~A short = ~A cover-short = ~A stop-short=~A~%" 
        ;            date short cover-short stop-short)
          (push (round (- (* (my-pretty-price (- short (getd date 'open)))
                             (calculate-point-value date))(comm+slip date))) trades)
           (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (getd date 'open))
                 (round (* (calculate-point-value date)(my-pretty-price (- short (getd date 'open)))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 ))


;;;;check if hit target #2 
   (when (and long cover-long (> (getd date 'high) cover-long) )
           (setq cover-long (max cover-long (getd date 'open)))                     
      ;    (format T "~%Exited long with Target day = ~A long = ~A cover-long = ~A" date long cover-long)
          (push (round (- (* (my-pretty-price (- cover-long long))
                             (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price cover-long)
                       (round (* (calculate-point-value date)
                                 (my-pretty-price (- cover-long long))))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil cover-long nil prev-stop-long nil days-in-trade 0 ))

   (when (and short cover-short (< (getd date 'low) cover-short) )
          (setq cover-short (min cover-short (getd date 'open)))
       ;   (format T "~%Exited short with Target day = ~A short = ~A cover-short = ~A" 
        ;        date short cover-short)
          (push (round (- (* (my-pretty-price (- short cover-short))
                             (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price cover-short)
                 (round (* (calculate-point-value date)
                           (my-pretty-price (- short cover-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil prev-stop-short nil  days-in-trade 0 ))


;;;check if stopped out #1
   (when (and long stop-long (<= (getd date 'low) stop-long))
           (setq stop-long (min stop-long (getd date 'open)))                     
         ; (format T "~%Stopped out long day = ~A long = ~A stop-long = ~A" date long stop-long)
          (push (round (- (* (my-pretty-price (- stop-long long))
                             (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price  stop-long)
                       (round (* (calculate-point-value date)
                                 (my-pretty-price (- stop-long long))))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil  prev-stop-long nil days-in-trade 0 ))

   (when (and short stop-short (>= (getd date 'high) stop-short))
          (setq stop-short (max stop-short (getd date 'open)))
         ; (format T "~%Stopped out short day = ~A short = ~A stop-short = ~A" date short stop-short)
          (push (round (- (* (my-pretty-price (- short stop-short))
                             (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price stop-short)
                 (round (* (calculate-point-value date)
                           (my-pretty-price (- short  stop-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil prev-stop-short nil  days-in-trade 0 ))

       ;(format T "~%2date= ~A  long = ~A stop-long = ~A short = ~A stop-short = ~A"
       ;       date long stop-long short stop-short)

 ;(format T "~%2entry-long = ~A entry-short = ~A" entry-long entry-short)

;;;check if exited with criterion change at end of the day

   (when  (and long  (> days-in-trade time-allowed)                                                 
                )
          (push (round (- (* (-  (getd date 'open) long)
                      (calculate-point-value date-1))(comm+slip date-1))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (getd date 'open))
                       (round (* (calculate-point-value date) (- (getd date 'open)  long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0 early-exit T))

   (when  (and short (> days-in-trade time-allowed)                       
                             )
          (push (round (- (* (- short (getd date 'open))
                      (calculate-point-value date-1))(comm+slip date-1))) trades)
           (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (getd date 'open))
                 (round (* (calculate-point-value date) (- short (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 early-exit T))

; (format T "~%2date= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A"
;           date  long stop-long short stop-short)


;;;means of entry short #1 on entry-price 
    (when   (and (not short) entry-short  (not stockp)
                 (<= (getd date 'low) entry-short) ;;;entry short is the open price   
            
                 (> (getd date-1 'close) ave5)        
                
                 (> rsi2xh 85)(> cci5h 100)
                 (> rsi2 75)
                ; (or (= wp -1)(= can -1))
                 (minusp vri5)

                 (< roc5 2)
                 (< ccir21-11 200)(< ccir21-5 175)
 
              );;;entry short 
          (setq short (fmin (getd date 'open) entry-short) 
                stop-short  nil;(max target baseline);(n-day-high date-1 11)
                cover-short nil ;(min target baseline);(max stop-loss projl)          
                 prev-stop-short stop-short
                 trade-short (create-qswingtrade-entry-record-list date-1 -1 short)
                 days-in-trade 0 
        ))

;;;check if new entry long #1

    (when  (and (not long) entry-long
                (>= (getd date 'high) entry-long)  ;;;entry long is the open price  
           
                (< (getd date-1 'close) ave5)              
                (< rsi2xl 15)(< cci5l -100)
                (< rsi2 25)
             ;   (eql mdiv3 'DDN);(neq mdiv 'CDN)
              ;  (or  (= wp 1)(= can 1))
                (minusp vri5);;;volume trend is decreasing
                           
               (> roc5 -2);(plusp aved);;;short term trend not too steep
               (< ccir21-11 200)(< ccir21-5 175)  ;;; range of 5-day cci21 not too large
                          
             );;; entry long
               
           (setq long  (fmax (getd date 'open) entry-long)  
                 stop-long  nil;stop-loss ;(fmin entry-short (n-day-low date-1 (1- period)))
                 cover-long nil ;(max target baseline);(min stop-loss projh)
                 ;stop-long (min target baseline) ;(n-day-low date-1 11)
                ; prev-stop-long stop-long
                 trade-long (create-qswingtrade-entry-record-list date-1 1 long)
                 days-in-trade 0 
                 )
       ;   (format T "~%3 date = ~A long = ~A  stop-long = ~A prev-stop-long = ~A" date
        ;          long stop-long prev-stop-long)
          )
  ; (format T "~%date= ~A Long = ~A short= ~A stop-long= ~A stop-short= ~A~%" date long short stop-long stop-short)
 ;;check if stopped out on same day of entry
 ;   (if (or long short) (print (readt2 date)))
    (when (and long stop-long 
             (<= (getd date 'low) stop-long)
            ;  (or (and (<= (getd date 'low) stop-long)
            ;           (>= (getd date 'open) (getd date 'close)))
            ;      
            ;      (<= (getd date 'close) stop-long))
              )
  ;       (format T "~%long stopped same day: Date = ~A long = ~A stop-long = ~A~%" date long stop-long)
          (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                             (comm+slip date))) trades)
           (setq trade-long (apply #'vector
                 (append trade-long
                    (list date (my-pretty-price stop-long)
                                (round (* (calculate-point-value date)
                                          (my-pretty-price (- stop-long long))))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil  prev-stop-long nil days-in-trade 0
           ))

    (when (and short stop-short
           (>= (getd date 'high) stop-short)
          ;    (or (and (>= (getd date 'high) stop-short)
          ;             (<= (getd date 'open) (getd date 'close)))
          ;        
           ;        (>= (getd date 'close) stop-short))
              )
  ;     (format T "~%long stopped same day: Date = ~A short = ~A stop-short = ~A~%" date short stop-short)
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq trade-short (apply #'vector
                (append trade-short
                 (list date (my-pretty-price stop-short)
                            (round (* (calculate-point-value date)
                                      (my-pretty-price (- short stop-short))))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 
           ))
#|
;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd date 'close))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades)
            (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date)
                                                     (my-pretty-price (- cover-long long))))))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))

      (when short
           (setq cover-short (getd date 'close))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date)
                                                          (my-pretty-price (- short cover-short))))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0))
|#

#|
;;;check if exited with objective on day of entry
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

|#

   );;;closes the dotimes

  ;;;apply commission of $100 per round turn and slippage of 0 ticks per round turn
  ; (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
     (format output "~%~A~%" *data-name*)
     (format output "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
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
        (round  (drawdown trades))   
     );close the format

;;;;writes out the trades
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~S~%" ith)

    ));;closes the dolist and with-open-file
   ); closes the when


    (values (round  (list-sum trades))
            (length trades) trades)
   ));;;closes the let and the defun

;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
(defun create-qswingtrade-entry-record-list (date direction entry)
   (let ((date-1 (getd date 'ydate)) date-2 )
        (setq date-2 (getd date-1 'ydate))
         
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
 
;;;;daily patterns           
             (volume-ratio-index date 5)  ;;feature 2 volume slope
             (roc-rel-index5 date) ;;;feature 3
;;;;trend
             (channel-direction date 7) ;feature 4
             (channel-direction date 11) ;feature 5
;;Momentum            
             (channel-direction date 21) ;;feature 6
             (wpp1-composite date 3);feature 7

            
             (round (commodity-channel-index date 21) 10);feature 8
             (cci-range-index date 21 5) ;;;;feature 9

;;;Volatility
            (volatility-ratio-index date 4 28 1);; 10
;;;VOLUME       
             (volume-index date 4 28)  ;feature 11
             (lbounds date 5 2);;;feature 12
             (volume-div date 10 25 2); feature 13
            
             (momentum-divergence3 date 5 21) ;feature 14
     
              
                  );;;closes the list

))

;;;;
(defun qswingtrade-simulation-test (market date2 num &optional (features *qswing-features*))
 (let (date stop-long stop-short trades long short 
       trade-long entry-long entry-short prev-stop-long prev-stop-short
       (time-allowed 99) dirp
        wp ave5 roc5 vri5 (days-in-trade 0)  rsi2xl rsi2xh rsi2 cci5l cci5h ccir21-11 ccir21-5 can 
       ave-win ave-loss losers winners extended-trades trade-short 
        trading-dates (trade-time 0) 
;       epsignal longs long-gains long-acc shorts short-gains short-acc bin 
        date-1 record (running-sum 0)  draw  early-exit
       singles  cover-long cover-short  (stockp (member market (append *dow30-list* *stocks-list*)))
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "qswing-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "qswing-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "qswing-diary1.dat")))
       

   (setq singles (nth-value 3 (apply #'qswing-trade-binsb features))) 
 
   (set-market market)(format T "~%~A~%" *data-name*)
 
    (if (and num (> num (available-days market date2 900)))  (setq num nil))
    (ifn num (setq num (available-days market date2 900)))

  (cond ((member market *forex-warehouse-list*)(setq *commission* 0  *pips-slippage* 6))
        ((member market *dubai-list*)(setq *commission* 30  *pips-slippage* 0))
        ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0))
        (t (setq *commission* 40  *pips-slippage* 0)))
  
    (setq *entry-factor-swing* 0.0 )

    (setq date (add-mkt-days date2 (- num)))

 ;;;;from date1 to date2
   (dotimes (ith num)

  ; (format T "~%1 ~A = date  ~A = short  ~A = long~%" date short long)
    (if (or long short) (incf days-in-trade))

     (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction date 3))
     (multiple-value-setq (cci5h cci5l) (cci-high-low date 5 3))

    (setq can (candle-composite date 3)
          ave5 (ave-exp date 5 'close)
          roc5 (ep-roc date 5 50) 

          rsi2 (rsi date 2) ;;;
             
          ccir21-11 (cci-range date 21 11) ccir21-5 (cci-range date 21 5)
          
           vri5 (volume-ratio-index date 5 'vi1-20) 
           wp (wpp1-composite date 3) 
          ) 

 #|      
    (if (eql dir 'long) (setq cover-short  (my-pretty-price stop-loss)
                             stop-short nil; (+ (n-day-high date (1- period))(comm+slip-points date))
                             stop-long nil cover-long nil
                              ); (+ (getd date 'close)(- (getd date 'high) cover-short)))
                       (setq cover-long (my-pretty-price stop-loss)
                             stop-long nil;(- (n-day-low date (1- period)) (comm+slip-points date))
                             stop-short nil cover-short nil
                              ));(- (getd date 'close)(- cover-long (getd date 'low)))))
|#

    
      (setq date-1 date date (add-mkt-days date 1))
;      (setq date-2 (getd date-1 'ydate))

   (setq entry-short (-  (getd date 'open) (volatility date-1 21 *entry-factor-swing*))
         entry-long (+   (getd date 'open) (volatility date-1 21 *entry-factor-swing*)))
    ;     stop-short  (+ (getd date 'open)
     ;                   (volatility date-1 21 *stop-loss-swing*))
      ;   stop-long   (- (getd date 'open)
       ;                 (volatility date-1 21 *stop-loss-swing*)))

   (setq entry-long (my-pretty-price entry-long) stop-long (my-pretty-price stop-long)
           entry-short (my-pretty-price entry-short) stop-short (my-pretty-price stop-short))

 ;    (setq stop-long  (fmax prev-stop-long stop-long); (n-day-low date (- period 1)))
 ;         stop-short (fmin prev-stop-short stop-short)); (n-day-high date (- period 1))))
 ;    (setq prev-stop-long stop-long prev-stop-short stop-short)

       (setq record (list date (getd date 'close) 0 0 0))
       (if long (setf (nth 2 record) 1))
       (if short (setf (nth 2 record) -1))
 
    (when (and (getd date 'rollover) long)
 ;            (setq long (+ long (getd date 'rollover))
 ;       	   stop-long (+ stop-long (getd date 'rollover)))
             (setf (nth 3 record)(- (nth 3 record) (getd date 'rollover)))
        )

    (when (and (getd date 'rollover) short)
 ;           (setq short (+ short (getd date 'rollover))
  ;  	          stop-short (+ stop-short (getd date 'rollover)))
            (setf (nth 3 record) (+ (nth 3 record) (getd date 'rollover)))
         )

;    (format T "~%2 date =~A  short= ~A  long= ~A stop-long = ~A stop-short= ~A ~%" 
;               date short long stop-long stop-short)

;;;calculate bin-classifier 
   
;      (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
 ;                 (bin-classifier-swingtradesb date-1 features))


;;;check if exited with signal change at the open of the day
        (when (and long 
              (or   (> rsi2 65)
                    (= wp -1)(= can -1) ;;;to take profit    
                    (and (plusp roc5)
                         (> (getd date-1 'close) ave5))
                    )
                   )
          (push (round (- (* (my-pretty-price (- (getd date 'open) long))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'C
                                        (my-pretty-price (getd date 'open))
                       (my-pretty-price  (- (getd date 'open)  long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(my-pretty-price (-  (getd date 'open) (getd date-1 'close)))))
          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0 ))

        (when (and short 
                   (or (< rsi2 35) 
                       (= wp 1)(= can 1)
                      (and (minusp roc5)
                           (< (getd date-1 'close) ave5))
 
              )) 
            (push (round (- (* (my-pretty-price (- short (getd date 'open)))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'C
                                           (my-pretty-price  (getd date 'open))
                 (my-pretty-price (- short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)(my-pretty-price (- (getd date-1 'close) (getd date 'open)))))
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 ))
 

;;;check if stopped out of prior position #1

   (when (and long stop-long (<= (getd date 'low) stop-long))
 ;          (format T "~%6 date = ~A long = ~A low = ~A stop-long = ~A trade-long = ~A"
  ;         date long (getd date 'low) stop-long trade-long)
         (setq stop-long (min stop-long (getd date 'open)))
         (push (round (- (* (my-pretty-price (- stop-long long))
                        (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
             (append trade-long  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                      (contract-month *data-name* date) 'S
                                      (my-pretty-price  stop-long)
                                    (my-pretty-price (-  stop-long long))))))

           (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                 (my-pretty-price (- stop-long (getd date-1 'close)))))


          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))

   (when (and short stop-short (>= (getd date 'high) stop-short))
  ;        (format T "~%6 date = ~A short = ~A high = ~A stop-short = ~A trade-short = ~A"
   ;           date short (getd date 'high) stop-short trade-short)
          (setq stop-short (max stop-short (getd date 'open)))
          (push (round (- (* (my-pretty-price (- short stop-short))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))
                    (contract-month *data-name* date) 'S
                   (my-pretty-price  stop-short )
                   (my-pretty-price (- short stop-short))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                        (my-pretty-price (- (getd date-1 'close) stop-short))))
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0))


;;;check if exited with objective
   (when  (and long cover-long (> (getd date 'high) cover-long))
          (push (round (- (* (-  (max (getd date 'open) cover-long)  long)
                  (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                       (my-pretty-price (- (max (getd date 'open) cover-long) long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (max cover-long (getd date 'open)) (getd date-1 'close))))

          (setq trade-long nil long nil stop-long nil cover-long nil prev-stop-long nil days-in-trade 0))

   (when  (and short cover-short (< (getd date 'low) cover-short))
          (push (round (- (* (- short (min (getd date 'open) cover-short))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                 (my-pretty-price (- short (min (getd date 'open) cover-short)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (getd date-1 'close) (min cover-short (getd date 'open)))))

          (setq trade-short nil short nil stop-short nil cover-short nil prev-stop-short nil days-in-trade 0))

;;;check if exited with time duration at the open of the day
        (when (and long (>= days-in-trade time-allowed)
                      )
          (push (round (- (* (my-pretty-price (- (getd date 'open) long))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'T
                                        (my-pretty-price (getd date 'open))
                       (my-pretty-price  (- (getd date 'open)  long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(my-pretty-price (-  (getd date 'open) (getd date-1 'close)))))
          (setq trade-long nil long nil stop-long nil prev-stop-long nil early-exit T days-in-trade 0 ))

        (when (and short (>= days-in-trade time-allowed) 
                        ) 
            (push (round (- (* (my-pretty-price (- short (getd date 'open)))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'T
                                           (my-pretty-price  (getd date 'open))
                 (my-pretty-price (- short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)(my-pretty-price (- (getd date-1 'close) (getd date 'open)))))
          (setq trade-short nil short nil stop-short nil prev-stop-short nil early-exit T days-in-trade 0 ))
 

;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (+ (nth 3 record)(my-pretty-price
                                       (- (getd date 'close) (getd date-1 'close))))))
      (if short (setf (nth 3 record)(+ (nth 3 record)(my-pretty-price
                                       (- (getd date-1 'close)(getd date 'close))))))

 ;     (format T "~%3 ~A = date  ~A = entry-short  ~A = entry-long ~A = stop-long "
 ;            date entry-short entry-long stop-long)
    
;;;check if new entry short
   ;    (format t "~%7date = ~A  short = ~A epsignal = ~A shorts = ~A short-gains = ~A"
   ;        date  short epsignal shorts short-gains) 
;
     (when (and (not short) entry-short (not stockp) (not early-exit)                   
                (<= (getd date 'low) entry-short)
       
                (> (getd date-1 'close) ave5)              
                (> rsi2xl 85) (> cci5h 100);; overbought
                (> rsi2 75)
               
               ; (or  (= wp -1)(= can -1))
                (minusp vri5);;;volume trend is decreasing

                (< ccir21-11 200)(< ccir21-5 175)  ;;; range of 11-day cci21 not too large
                (< roc5 2.0) ;;;price trend is not too steep downward                                   

                   );;entry short

          (setq short  entry-short   
                 trade-short (list date 'short short)
                  cover-short nil;stop-loss
                   stop-short nil
                 prev-stop-short stop-short
                 days-in-trade 0)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(comm+slip-points date))))
              )
;;new entry long 
 
    (when (and (not long) entry-long
               (>= (getd date 'high) entry-long)               
               (< (getd date-1 'close) ave5)              
               (< rsi2xl 15)  (< cci5l -100);; oversold
               (< rsi2 25)
             
              ; (or  (= wp 1)(= can 1))
               (minusp vri5);;;volume trend is decreasing

               (< ccir21-11 200)(< ccir21-5 175)  ;;; range of 11-day cci21 not too large
               (> roc5 -2.0) ;;;price trend is not too steep downward                                
                 );;first entry long

          (setq long   entry-long 
                 trade-long (list date 'long long)
                 cover-long nil;stop-loss      
                 stop-long nil;(- (n-day-low date-1 (1- period))(comm+slip-points date-1))
                 prev-stop-long stop-long    
                 days-in-trade 0)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                           (my-pretty-price (- (getd date 'close) long (comm+slip-points date)))))
             )

  ;   (format T "~%4 ~A = date  ~A = short  ~A = long" date entry-short entry-long)
;     (format T "~%4a ~A = epsignal  ~A = long  ~A = short stop-long = ~A" epsignal long short stop-long)
   ;  (print record)
 ;;;check if stopped out on same day of entry
    (when (and long stop-long 
               (<= (getd date 'low) stop-long)
             ;  (or (and (<= (getd date 'low) stop-long)
             ;           (>= (getd date 'open) (getd date 'close)))
             ;     
             ;     (<= (getd date 'close) stop-long))
           )
           (setq stop-long (my-pretty-price stop-long))
           (push (round (- (* (my-pretty-price (-  stop-long long))
                              (calculate-point-value date))(comm+slip date))) trades) 
;           (format T "~% Trades = ~A" trades)
          (setq trade-long (apply #'vector
                  (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'SS
                                           stop-long (my-pretty-price (- stop-long long))))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
 ;          (setf (nth 2 record) 1)
;           (format T "11 stop-long = ~A long = ~A" stop-long long)
           (setf (nth 3 record) (+ (nth 3 record) (my-pretty-price (- stop-long long))))
           (setf (nth 3 record) (- (nth 3 record)(my-pretty-price (- (getd date 'close) long))))
 
          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))

    (when (and short stop-short 
               (>= (getd date 'high) stop-short)
        ;       (or (and (>= (getd date 'high) stop-short)
        ;                (<= (getd date 'open) (getd date 'close)))
         ;         
         ;           (>= (getd date 'close) stop-short))
                 )
           (setq stop-short (my-pretty-price stop-short))
           (push (round (- (* (my-pretty-price (- short stop-short))
                              (calculate-point-value date))(comm+slip date))) trades)
           (setq trade-short (apply #'vector 
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'SS
                                            stop-short (my-pretty-price (- short stop-short))))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
   ;        (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record) (my-pretty-price (- short stop-short))))
           (setf (nth 3 record) (- (nth 3 record)(my-pretty-price (- short (getd date 'close)))))
 
           (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 ))

#|
;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1))
                               (comm+slip date-1))) trades)
            (setq trade-long (apply #'vector 
                  (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                          (contract-month *data-name* date) 'N
                                          cover-long (my-pretty-price (- cover-long long))))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))

            (setf (nth 3 record ) (+ (nth 3 record ) (my-pretty-price (- cover-long long))))
           (setf (nth 3 record) (- (nth 3 record)(my-pretty-price (- (getd date 'close) long))))
            (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
         ;    (format T "123 date=~A short-trade= ~A~% record= ~A" date short-trade  record) 
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1))
                              (comm+slip date-1))) trades)
           (setq trade-short (apply #'vector
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'N
                                           cover-short (my-pretty-price (- short cover-short))))))
           (push trade-short extended-trades)

           (setf (nth 3 record) (+ (nth 3 record)(my-pretty-price (- short cover-short))))
           (setf (nth 3 record) (- (nth 3 record)(my-pretty-price (- short (getd date 'close)))))
        
     ;   (format T "~%234 date=~A trade-short= ~A~% record= ~A" date trade-short  record) 
            (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0))


|#
#|

;;;check if exited with objective
   (when  (and long obj-long (> (getd date 'high) obj-long))
          (push (-  (max (getd date 'open) obj-long)  long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) obj-long))
                       (round (* (index-point-value) (- (max (getd date 'open) obj-long) long)))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (min stop-long (getd date 'open)) (getd date-1 'close))))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))

          (setq trade-long nil long nil stop-long nil ))

   (when  (and short obj-short (< (getd date 'low) obj-short))
          (push (- short (min (getd date 'open) obj-short)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) obj-short))
                 (round (* (index-point-value) (- short (min (getd date 'open) obj-short))))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (getd date-1 'close) (max stop-short (getd date 'open)))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade-short nil short nil stop-short nil ))
|#
  ;     (format T "~%5 ~A = date  ~A = short  ~A = long ~A" date short long trade-long)
  ;     (print record)
           (setf (nth 4 record) (+ (nth 3 record) running-sum))
           (setq running-sum (nth 4 record))

       (push record trading-dates)
       (setq record nil);;;reset to nil at end of each trading date

       
     ) ;;;closes the dotimes

;;;;writes out the diary
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (first ith) (second ith) (third ith) (round (* (calculate-point-value (first ith))(nth 3 ith)))
           (round (* (calculate-point-value (first ith)) (nth 4 ith))))
     ))
  ;;;apply commission of $100 per round turn and slippage of 0 ticks per round turn
 ;  (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

  (with-open-file (str summary-path :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )

     (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
     (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 swings)))
 
     (format str "NUM TRADES IN WAREHOUSE= ~A  ~%" (length swings) )
     (format str "SINGLE RATIO = ~A~%"  singles)

       (format str "P/L= ~16D  NUMBER TRADES=   ~6D   ACCURACY=       ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN=  ~8D   AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=    ~4,2,0,'*,' F  ~
        LARGEST LOSS=   ~7D   LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F~%~
        DRAWDOWN= ~11D  $/contract=  ~10D  "
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
      (my-round (/ trade-time (if (zerop (length trades)) 1 (length trades))) 1)
      (setq draw (round (drawdown trades)))
      (if (and losers (< (list-sum losers) 0) (plusp (list-sum trades)))
          (round (optimal-f trades)) 0)
       )

     (format str "~%~%MIN INITIAL $=      ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
          (* 100 (/ (/  (list-sum trades) 
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                                           365.25))
                       (* 1000 (ceiling (max 1.0 (* 3 (abs draw))) 1000))) ))
        );;;closes the with-open-file

;;;;writes out the trades
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)
         (svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 0)) (svref ith 8))))

     ));;closes the dolist and with-open-file
  ; ); closes the when

    (values (round  (list-sum trades))
            (length trades) trades)
   ));;;closes the let and the defun

;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-qswingtrade-indicators ()
   (let ((path (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat"))
          date-1 date-2 )
  (maind-x)(set-cat-list)(setq swings nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))

  (dolist (ith swings)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate) date-2 (getd date-1 'ydate))

     (setf (svref ith 10) (round (commodity-channel-index date-1 21) 10));;feature 3
    
      (setf (svref ith 11)(round (cci-range date-1 21 5) 10)) ;;feature 6
    ;  (setf (svref ith 9) (channel-direction date-1 10)) ;feature 7
   ;   (setf (svref ith 9)(day-bar-type2 date-2 ));;;feature 7 

   ;  (setf (svref ith 11) (r-squared-change-index date-1 5 1)) ;;feature 9
   ;  (setf (svref ith 7) (volatility-ratio-index date-1 1 126 1)) ;;feature 5
  ;   (setf (svref ith 16) (primitive-slope-trend date-1 5)) ;;feature 14

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
    (dolist (jth swings)
      (format str "~S~%" jth)))

))


(defun find-best-qswing-indicator-set ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (qswings-add-one-in base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)(> result .85)
             (< (fifth (car winners-list)) 2.9)
             (> (fourth (car winners-list)) .20))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

   (loop     
     (if (neql (second (car winners-list)) 1)
          (if (> (fifth (car winners-list)) 2.90)(return))
        (if (> (fifth (second winners-list)) 2.90)(return)))

      (setq winners-list (qswings-leave-one-out base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
      base-list
    ))

;;;requires a candidate list to add to the base features
(defun qswings-add-one-in (base-features candidate-features)
 (let (winners-list (result 0) average-profit rtb rp single-bins csgof)  

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'qswing-trade-bins (append base-features (list ith))))
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
;;;;;

;;;requires a base features list
(defun qswings-leave-one-out (base-features)
  (let (winners-list (result 0) rtb average-profit single-bins rp csgof)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
                    (apply #'qswing-trade-bins (remove ith base-features)))

    (setq winners-list (cons (list result ith average-profit single-bins rtb rp) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))



(defun find-best-qswingtrade-entry (tdate num)
  (let ((best-param 1.6) (results 0) (total-trades 0) (prev-result -20000000)
         scores1 scores2 trades result)

  (do ((param best-param  (+ param .05)))
      ((> param 2.60) best-param)

      (setq *entry-factor-swing* (my-round param 2))
      (format T "~%TRYING ENTRY= ~A~%" param)(setq results 0 total-trades 0)
      (dolist (ith  *position-list*)
        (format T "~%MARKET= ~A~%" ith)

        (multiple-value-setq (result trades) (populate-swing-trades ith tdate num ))
        (setq results (+ results result) total-trades (+ total-trades trades)));;closes the dolist

      (setq scores1 (acons param results scores1))(print scores1)
      (setq scores2 (acons param (round (/ results total-trades)) scores2))(print scores2)

      (if (> results prev-result) (setq prev-result results best-param param))
      (format T "~%BEST ENTRY SO FAR= ~A" best-param));;;closes the do


      ))


;;This function tests to find the current volatility signal
(defun vsignals4 (date &optional (period 4)(param .85)(days 3))
  (let (vhigh vlow hprice lprice oprice cprice epsignal (counter 0))

  (loop
   (setq hprice (getd date 'high) lprice (getd date 'low)
         oprice (getd date 'open) cprice (getd date 'close))
   (setq date (getd date 'ydate))

   (multiple-value-setq (vlow vhigh)(vprices4 date period param days))
   ;;;;need to check if high or low came first
   (when (<= oprice cprice) ;;;high came second
         (when (> hprice vhigh) (setq epsignal 'buy) (return))
         (when (< lprice vlow) (setq epsignal 'sell) (return)))
   (when (> oprice cprice) ;;;low came second
         (when (< lprice vlow) (setq epsignal 'sell) (return))
         (when (> hprice vhigh) (setq epsignal 'buy) (return)))
   (decf counter)
   )
   (values epsignal counter (getd date 'ndate)(if (eql epsignal 'buy) vhigh vlow))
   ))

#|
(defun build-swing-warehouse (date)
   (let (path)

  (when (probe-file (string-append *upper-dir-warehouse* "swingtradewarehouse.dat"))
      (rename-file (string-append *upper-dir-warehouse* "swingtradewarehouse.dat") (string-append *upper-dir-warehouse* "swingtradewarehouse.backup"))
    )
 (dolist (ith *market-days-available*)
   (print (car ith))
   (populate-swing-trades (car ith) date (cdr ith))

   (setq path (string-append *upper-dir-warehouse* (format nil "~S" (car ith)) "swingtrades.dat"))

   (add-swing-trades path)

;   (shell (string-append "cat " path " >> " "~/exitpoints/swingtradewarehouse3.dat"))

   )
))
|#

(defun build-qswingtrade-warehouse (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "qswingtradewarehouse.backup"))
           (delete-file (string-append *upper-dir-warehouse* "qswingtradewarehouse.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat"))
          (rename-file (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat")
                            "qswingtradewarehouse.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "qswingtrades.dat")) 
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




;;;reads the swingtradewarehouse3.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-qswing-trades (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat")) trades)

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


(defun remove-qswingtrade-market (market)
  (let (trades path)
   (setq path (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat"))
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


(defun qswing-trade-bins (&rest features)
  (let (bin path)
   (if *stocks* (setq path (string-append *upper-dir-warehouse* "qswingradewarehouse.dat"))
       (setq path (string-append *upper-dir-warehouse* "qswingtradewarehouse.dat")))
 
 (maind-x)(set-cat-list)
  (setq swings nil swing-bin-codes nil)(clrhash *swing-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))


;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swings)
     (setq bin (encode-swing-trades record features))
     (cond ((gethash bin *swing-trade-warehouse*)
            (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse*)
                       (cons record (gethash bin *swing-trade-warehouse*)))))
            ((not (gethash bin *swing-trade-warehouse*))
             (setf (gethash bin *swing-trade-warehouse*) (list record))
              (push bin swing-bin-codes)))

    )

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profit swings)


  ))


  

(defun swing-time-in-trade (swings)
  (let ((days 0)(winning-trade-days 0)(losing-trade-days 0)(winning-trades 0)(losing-trades 0))
  (dolist (ith swings)
    (set-market (svref ith 0))
  ;  (format T "~%~A  ~A  ~A" (svref ith 0) (svref ith 1)(svref ith 17))
    (setq days (+  (sub-mkt-dates (svref ith 1)(svref ith 17)) days))
    (if (plusp (svref ith 19)) (setq winning-trade-days (+ (sub-mkt-dates (svref ith 1)(svref ith 17)) winning-trade-days)
                                 winning-trades (1+ winning-trades))
         (setq losing-trade-days (+ (sub-mkt-dates (svref ith 1)(svref ith 17)) losing-trade-days)
               losing-trades (1+ losing-trades))))

  (values (float (/ days (length swings))) (float (/ winning-trade-days winning-trades))(float (/ losing-trade-days losing-trades)))

))



(defun qswing-trades (&optional tdate (market-list *swing-list*) (outfile t) date1)
   (let ( date  path1 path2  profit trades new-profit new-trades trade-list (twr-long 1) (twr-short 1) bin
           path3 epsignal longs long-gains long-acc shorts short-gains short-acc (recommendation nil))
       (declare (ignore profit trades new-profit new-trades trade-list))
       (declare (special epsignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin))

	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

        (setq path1  (string-append *daily-output-dir* "swing-trades.dat") path2 (string-append *daily-output-dir* "dcr.xml")
              path3 (string-append *daily-output-dir* "position-orders.csv"))

        (if (and outfile (probe-file path1))
            (delete-file path1))

         (if (and outfile (probe-file path3))
            (delete-file path3))

 ;       (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
 ;        (format stream "~%ExitPoints Recommendations for ~A~%~%" (date-convert date1))
 ;        (format stream "These swing recommendations were prepared by an artificial intelligence program~%")
 ;        (format stream "written by David Register CTA. The program scans thirty-three markets looking~%")
 ;        (format stream "for potential trades using a database of 31,642 historical swing trades.~%")
 ;        (format stream "ExitPoints also has day, position, cash forex, and individual stock trading systems~%")
 ;        )

        (with-open-file (stream3 path3 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream3 "EXITPOINTS, POSITION, SYSTEM, ORDERS, FOR, ~A~%" (date-convert date1))
          (format stream3 "Market,              Month,   Direction,      Entry,    Stop~%~%"))

        (apply #'qswing-trade-binsb *swing-features*) ;;;need just once for all markets

        (dolist (market market-list)
           (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

          (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

          (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

          (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-swingtradesb date *swing-features*))

           (setq twr-short (swing-bin-twr bin)) (setf (nth 0 bin) 1)
           (setq twr-long (swing-bin-twr bin))

           (with-open-file (stream3 path3 :direction :output :if-exists :append :if-does-not-exist :create)

           (setq recommendation (or (find-best-qswing-trade date stream stream1 stream3 date1) recommendation))
         ;  (find-best-contraswing-trade1 date stream stream1 date1)

           ));;closes stream1 and stream3

        ; (terpri stream)

          );;;closes the stream
          );;;closes the dolist

          (unless recommendation
            (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
              (format stream "~% NO RECOMMENDATIONS FOR  ~A~%" (date-convert date1))))

))


(defun qswing-trade-binsb ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "qswingtradewarehouse.dat"))
    (maind-x)(set-cat-list)
    (setq swings nil swing-bin-codes nil)(clrhash *swing-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))


;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *day-commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swings)
     (dotimes (ith 4)
         (setq bin (encode-swing-trades record (butlast features ith)))
         
          (cond ((gethash bin *swing-trade-warehouse*)
            (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse*)
                       (cons record (gethash bin *swing-trade-warehouse*)))))
            ((not (gethash bin *swing-trade-warehouse*))
             (setf (gethash bin *swing-trade-warehouse*) (list record))
              (push bin swing-bin-codes)))
          ));;;closes the doltimes and dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profit3b swings features)


  ))





;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for swing trades to decide to trade or not
;;;;assumes (swing-trade-bins1 features) has already been run
(defun find-best-qswing-trade (tdate &optional (output T)(output1 T) (output3 T)(date1 nil))
  (let* ( rollover entry-short entry-long  long short risk
         cover-long cover-short  directive1 prev-signal action stop-long stop-short  (time-frame 'Position)
           trade-direction prev-stop-long prev-stop-short)
    (declare (special epsignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin))
;    (declare (ignore  ctr))


    (if (member *data-name* *forex-list*) (setq time-frame 'FX-SWING))
   (multiple-value-setq (prev-signal) (vsignals tdate 4 *entry-factor-swing* 1))

   (setq trade-direction (cond ((and (member epsignal '(OK UP))(eql prev-signal 'SELL)
                                      (> longs 0)
                                      (>= (/ long-gains longs) 100.0)(> twr-long 1.0)
                                      ) 'UP)
                               ((and (member epsignal '(OK DOWN))(eql prev-signal 'BUY)
                                      (> shorts 0)
                                      (>= (/ short-gains shorts) 100.0)(> twr-short 1.0)
                                      ) 'DN)
                               (t 'FT)))

   (multiple-value-setq (entry-short entry-long)   (vprices tdate 4 *entry-factor-swing* 1))
   (setq risk (my-pretty-price (abs (- entry-long entry-short)))) ; (/ *max-swing-risk* (index-point-value))) ))

  (setq stop-short (+ entry-short risk) ;;;this is the init buy stop
          stop-long (- entry-long risk))    ;;;this is the init sell stop

;;;;sets long and short to be true if open position coming into today
    (setq long (eql (cadr (assoc *data-name* (append *forex-open-swings* *open-swings*))) 'long))
    (setq short (eql (cadr (assoc *data-name* (append *forex-open-swings* *open-swings*))) 'short))

   (when long (setq stop-long (fmax stop-long prev-stop-long (- (getd tdate 'high)(* .750 (- entry-long entry-short)))) prev-stop-long stop-long))
   (when short (setq stop-short (fmin stop-short prev-stop-short (+ (getd tdate 'low) (* .750 (- entry-long entry-short)))) prev-stop-short stop-short))

  
    (setq rollover (getd tdate 'rollover))
    (ifn rollover (setq rollover 0))

    (if short (setq stop-short (fmin entry-long (+ rollover (caddr (assoc *data-name* (append *forex-open-swings* *open-swings*)))))))
    (if long (setq stop-long (fmax entry-short (+ rollover (caddr (assoc *data-name* (append *forex-open-swings* *open-swings*)))))))

             (if (eql trade-direction 'up) (push "NOT SHORT" action))
             (if (eql trade-direction 'dn) (push "NOT LONG" action))
             (if (eql trade-direction 'FT) (push "NOT TODAY" action))

           (when (eql (cadr (assoc *data-name* *open-swings*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *open-swings*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))

           (when (eql (cadr (assoc *data-name* *forex-open-swings*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *forex-open-swings*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))

         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))


        (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))

       (when (equal action "NOT LONG")
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output "~%Place Stop Order to SELL @ ~7@A  ~%If filled Place Buy Stop @ ~7@A~%~
              ~%The Initial Risk is $~D\.~%"
             (convert-to-32nds entry-short)(convert-to-32nds stop-short)
             (round (* (-  (convert-to-decimal (convert-to-32 stop-short)) (convert-to-decimal (convert-to-32 entry-short))
                           )
                        (index-point-value))));;;closes the format

          (format output
             (string-append "~%Place Stop Order to Sell @ " directive1 "~%If filled Place Buy Stop @ " directive1
              "~%The Initial Risk is $~D\.~%")
            (* (index-tick-size) (round entry-short (index-tick-size)))
            (* (index-tick-size) (round stop-short (index-tick-size)))

            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
                          (* (index-tick-size) (round entry-short (index-tick-size)))
                           )  (index-point-value))));closes the format
           ));;closes the if and when


      (when (equal action "NOT SHORT")
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output "~%Place Stop Order to Buy @ ~7@A  ~%If filled Place Sell Stop @ ~7@A~%~
              ~%The Initial Risk is $~D\.~%"
             (convert-to-32nds entry-long)(convert-to-32nds stop-long)
             (round (* (-  (convert-to-decimal (convert-to-32 entry-long))(convert-to-decimal (convert-to-32 stop-long))
                           )   (index-point-value))));;;closes the format

          (format output
             (string-append "~%Place Stop Order to Buy @ " directive1 "~%If filled Place Sell Stop @ " directive1
              "~%The Initial Risk is $~D\.~%")
            (* (index-tick-size) (round entry-long (index-tick-size)))
            (* (index-tick-size) (round stop-long (index-tick-size)))

            (round (* (- (* (index-tick-size) (round entry-long (index-tick-size)))
                         (* (index-tick-size) (round stop-long (index-tick-size)))
                            )  (index-point-value))));closes the format
           ));;closes the if and when



      (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
             (write-txt-record  'SHORT-ENTRY entry-short stop-short cover-short output3)
             (write-txt-record  'LONG-ENTRY  entry-long stop-long cover-long output3)
                 )

             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
              (write-txt-record  'LONG-ENTRY  entry-long stop-long cover-long output3)
                )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
              (write-txt-record  'SHORT-ENTRY entry-short stop-short cover-short output3)

               ))


       (cond ((eql (cadr (assoc *data-name* *open-swings*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil stop-long cover-long output1)
              (write-txt-record  'OPEN-LONG  nil stop-long cover-long output3)
                  )
             ((eql (cadr (assoc *data-name* *open-swings*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'SHORT date1 nil stop-short cover-short output1)
              (write-txt-record  'OPEN-SHORT nil stop-short cover-short output3)
                  )
             ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'LONG date1 nil stop-long cover-long output1)
                  )
             ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'SHORT date1 nil stop-short cover-short output1)

                  ));;;;closes the cond

        (and (or (equal action "NOT LONG")(equal action "NOT SHORT"))  ;;;returns T if recommendation
             (member *data-name* *swing-list*))
      ));;;closes the let and the defun



(defun most-recent-warehouse-trade (market )
  (let (trades)
  (dolist (ith swings)
    (if (eql market (svref ith 0)) (push ith trades)))
  (vsort trades #'> #'(lambda (s) (svref s 17)))
 (/  (svref (car trades) 19) (volatility (svref (car trades) 17)))
))
;;;----------------------------------------------------------------------------------------------------

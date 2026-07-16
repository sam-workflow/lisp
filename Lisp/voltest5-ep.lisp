;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;;
;;;;This version does not use objectives.  THIS IS FOR THE TRIUMPH 21 system
;;;It does use a vector database instead of lists
;;;


(defparameter *position-features3* '(1 2 5 6 7 12));;5 6 and 9 for stocks
;(defparameter *charger-lite-list* '(si.d1b  pl.d1b cf.d1b ho.d1b s.d1b us.d1b ng.d1b ))
(defparameter *charger-lite-list*  nil);*day-warehouse-list* );'(cf.d1b   ))
;;;;For DAY TRADING
;;;

;;;;the file has one record per trade each record is a vector.
;;; The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1
;;;;
;;;;
;;;
;;;
;;; #(*data-name* entry-date direction entry-price HT BT BR T5 T15 DO BR-1 CAN SST T45 ZBL BPL TIM)
;;;
;;;Feature 1 is direction
;;;Feature 2 is the volatility ratio 4 day versus 28 day
;;;Feature 3 is the day-bar-type
;;;Feature 4 is the slow-stochastic of 21
;;;Feature 6 is mo-diver 5 (trend of momentum RSI(9))
;;;Feature 11 
;;;Feature 12 is now the pinpoint
;;;Feature 13 is mo-diver 45 (trend of momentum RSI(9))
;;;feature 14 is pivot-index date-1
;;;

;;This function is used to add trades to the warehouse3 for all the day
;;;markets 
;;;
(defun update-daytrade-warehouse3 (date &optional (markets *day-warehouse-list*))
;       (markets (set-difference *day-warehouse-list* (append *indexes-list* *currencies-list*))))
  
    (maind-x)(set-cat-list)
    (dolist (ith markets)
      (set-market ith)
        
       (populate-day-trades3 ith date (min 3050 (available-days ith date 500)))) 
 
    (build-daytrade-warehouse3 markets) 
  ;  (setq *day-features3* (find-best-indicator-set3a )) 
  ;  (indicator-study3 markets)
 ;  (portfolio-simulation3 '(day) date 2000 (list markets)) 
)

(defun populate-day-trades3 (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short entry-long entry-short date-1 
        risk reward   vri1-7  lproj5 vrt5 vril5-2 refl8
        rri20 rri13 rri10 rri5 ccis11-21 ccir21-5  vsos4;ccir21-11   ;md5-21 
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades3.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

  (format outfile "~%~A~%" market)
  ; (if (member *data-name* (append *indexes-list* *currencies-list*))
   ;        (setq *entry-factor* .382)
       (setq  *entry-factor* .764)
   (setq  *stop-loss-day* 1.0557 ;1.382
          *objective-reward*  11.09 *max-day-risk* 3000)
    ;6.854 1.0557  
   (if (member market *forex-warehouse-list*)
         (setq *commission* 0 *pips-slippage* 6)
        (setq  *commission* 6  *pips-slippage* 0))  ;;;1.0557)
   (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))

   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))


 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
 
   (setq risk (min (/ *max-day-risk* (calculate-point-value date)) (volatility date 4 *stop-loss-day*))
         reward (volatility date 21 *objective-reward*))

   (setq 
          vri1-7 (volatility-ratio-index date 1 7 1)
         vrt5 (volatility-trend date 5 1)     vril5-2 (vril date 5 2)  
         rri5 (ep-roc-index10 date 5)  rri10 (ep-roc-index10 date 10) 
         rri20 (ep-roc-index10 date 20) rri13 (ep-roc-index10 date 13)
         vsos4 (volume-openint-div date 4)
         ;lbds5-2 (lbounds date 5 2)
       ;  ccid (cci-direction date 21 2)
          refl8 (reflect2 date 8) 
;         chan21 (channel-direction date 21)
        ; ss14 (slow-stochastic-index date 14)
         lproj5 (lproj-index date 5)
         ccis11-21 (cci-slope-index date 11 21) ;feature 5
;         ccir21-11 (cci-range-index date 21 11)
         ccir21-5 (cci-range-index date 21 5) 
          )        

   (setq   date-1 date date (add-mkt-days date 1))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))))
   (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))))

   (setq stop-long (- entry-long  risk) cover-long
                      (fmin (+ entry-long reward)(add (getd date-1 'close)(index-limit)))
        stop-short (+ entry-short risk) cover-short 
                      (fmax (- entry-short reward)(sub (getd date-1 'close)(index-limit))))

     (setq stop-long (my-pretty-price stop-long) stop-short (my-pretty-price stop-short))

;;;check if new short entry
;;;enter on a stop order if opens below entry-stop.
;;;
;;;exit at the close

   (when (and (<= (getd date 'low) entry-short)            
             ; (>= (* risk (calculate-point-value date-1)) 75) ;;to avoid bad data
;	      (<= (* risk (index-point-value)) *max-day-risk*)
             ; (> vri4-40 -4)(< vrt5 4)(> vrt5 -4)
             ; (<= vri1-7 5) (< vril5-2 4)
             ; (> lproj5 -2);(/= ccid -3)(/= ccid 3);(/= lbds5-2 -2)
             ; (/= swi -3);(/= swi 3)
;              (not (member chan21 '(US AT BT BT2 UC+ IC- DC-))) 
             ; (< ccir21-11 200)
             ; (<= rri10 2) (<= rri5 3)(/= rri13 7)(/= rri20 6)
            ;  (< ss14 3);(/= ccir21-11 0)(/= ccir21-11 7)
             ; (< ccir21-5 5)(/= ccis11-21 -3)
              (/= refl8 -2)
              (cond ((eql *data-name* 'dj.d1b)(member vsos4 '(2 -1))) 
                    ((eql *data-name* 'nd.d1b)(member vsos4 '(-1)))
                    ((eql *data-name* 'sp.d1b)(member vsos4 '(-1 2)))
                    ((eql *data-name* 'ru.d1b)(member vsos4 '(2)))
                    ((eql *data-name* 'gc.d1b)(member vsos4 '(-1 1)))
                    ((eql *data-name* 'cl.d1b)(member vsos4 '(-2 2)))
                    ((eql *data-name* 's.d1b)(member vsos4 '(-1 2 1 -2)))
                    ((eql *data-name* 'w.d1b)(member vsos4 '(-1)))
                    ((eql *data-name* 'c.d1b)(member vsos4 '(2 -1)))
                  )
              )
          (setq short (min entry-short (getd date 'open)) 
                short-trade (create-daytrade-entry-record-list3 date-1 -1 short)
               ; cover-short nil
                 ))
;;;new long entry
    (when (and
               (>= (getd date 'high) entry-long)             
              ; (>= (* risk (calculate-point-value date-1)) 75)
;	       (<= (* risk (index-point-value)) *max-day-risk*)
              ; (>= vri4-40 -4)(< vrt5 4)(> vrt5 -4)
              ; (<= vri1-7 5)(< vril5-2 4)
               ;(<= lproj5 2);(/= ccid -3)(/= ccid 3);(/= lbds5-2 2)
               ;(/= swi 3);(/= swi -3)(/= ss 4)(/= ss -4)
 ;              (not (member chan21 '(DS AT BT BT2 UC+ IC- DC-)))
              ; (< ccir21-11 200)
               ; (>= rri10 -2)(>= rri5 -3)(/= rri13 -7)(/= rri20 -6)
             ;  (> ss14 -3);(/= ccir21-11 0)(/= ccir21-11 7)
               ; (< ccir21-5 5)(/= ccis11-21 3)
                 (/= refl8 2)
                 (cond ((eql *data-name* 'dj.d1b)(member vsos4 '(2 -1))) 
                    ((eql *data-name* 'nd.d1b)(member vsos4 '(2 -2 1)))
                    ((eql *data-name* 'sp.d1b)(member vsos4 '(2 -2)))
                    ((eql *data-name* 'ru.d1b)(member vsos4 '(2 -1)))
                    ((eql *data-name* 'gc.d1b)(member vsos4 '(1 2)))
                    ((eql *data-name* 'cl.d1b)(member vsos4 '(2 -1)))
                    ((eql *data-name* 's.d1b)(member vsos4 '(-2 1)))
                    ((eql *data-name* 'w.d1b)(member vsos4 '(2)))
                    ((eql *data-name* 'c.d1b)(member vsos4 '(-1 -2)))
                  )
                )
           (setq long (max entry-long (getd date 'open))
                 long-trade (create-daytrade-entry-record-list3 date-1 1 long)
                ; cover-long nil
                 )) ;(format T "long-trade= ~A ~%" long-trade) )


;;;check if met criteria to exit at target
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
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
                  
                  (<= (getd date 'close) stop-long))
       ;       (<= (getd date 'low) stop-long)
               )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date)
                                                  (my-pretty-price (- stop-long long))))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                  
                    (>= (getd date 'close) stop-short))
         ;   (>= (getd date 'high) stop-short)      
              )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date)
                                                     (my-pretty-price (- short stop-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))


;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date))
                               (comm+slip date))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date)
                                                     (my-pretty-price (- cover-long long))))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil cover-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date))
                              (comm+slip date))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date)
                                                          (my-pretty-price (- short cover-short))))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil cover-short nil))

   );;;closes the dotimes


  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win  (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
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
       (round  (or (min* losers) 0))
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
(defun create-daytrade-entry-record-list3 (date direction entry)
   (let* (  );(date-1 (getd date 'ydate))  )
      ;  (setq  period (dominant-cycle date 10 30))
       
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                  
;                             (sector-index *data-name*);;feature 2* 
                            ; (volatility-ratio-index date 2 21)
                             *data-name*;; feature 2
;;;daily patterns            
                             (wpp-index date ) ;feature  3
                             (modivpx date 4 21)  ;feature 4 
;                             (fast-stochastic-index date 21) ;; feature 5                             
                             
 ;                            (slow-stochastic-index date 21) ;feature   (now 6)                    
                             (candle-composite date 3)   ;;;feature 5
;;;Volume                    
;                             (fast-stochastic-index date 13) ;feature  (now 8)                                
                             
;;;;momentum             
 ;                            (momentum-divergence3 date 5 21) ;feature (now 9)                                
 ;                            (fast-stochastic-index date 5) ;feature   (now 10)          
 ;                            (fast-stochastic-index date 4) ;;;feature (now 11)
;Volatility
                                                     
 ;                            (modivpx date 4 13) ;;feature 12                                     
 ;                            (fast-stochastic-index date 2) ;;;feature  (now 13)
                            (vril date 4 1)  ;;feature  6
                           

            ; (plusp (accel date 5 1 'pivot) )   ;feature 5
             ;(plusp (accel1 date 5 1 'close));;;feature 6
            ; (day-bar-type2 date);;;feature 7
            ; (progn
            ;   (setq rating
            ;         (+ (/ (channel-direction-swing-index345 date) 3);;;feature 4
            ;            (/ (zero-strength-swing345 date) 3)))
            ;   (if (minusp rating)(floor rating)(ceiling rating)))
            
             (plusp (nth-value 1 (lregress date 5 'close)))   ;;;feature 7
          ;   (cci-context1 date );;
  ;          (zero-strength date 21);;feature 6
           ; (plusp (accel1 date 4 1))
            (ldev-index date 3);;;feature 8
            (reflect2 date 8);;feature 9
            ;(plusp (nth-value 1 (lregress date 4 'volume))) ;;;feature 7 
           ; (plusp (nth-value 1 (lregress date 4 'openint)));;;feature 8       
            (volume-openint-div date 4)  ;feature 10  
            (cci-level-index date 5);;;feature 11
            (plusp (roc date 1 'cci 5));;;feature 12*
            ;(cci-level-index date 10) ;;;
            ; (plusp (stochastic-delta  date 21 5))                        
            (pivot-index date 'day);;feature 13
                          
            (pivot-index date 'week);;feature 14
        ;  
                             
                              );;;closes the list

))

;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-daytrade-indicators3 (markets)
   (let (path1 date-1 date-2)
   (maind-x)(set-cat-list)
  (dolist (market markets)
     (setq daytrades nil
           path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades3.dat"))
   (with-open-file (str path1 :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (dolist (ith daytrades)
     (set-market (svref ith 0)) 
     (setq date-1 (getd (svref ith 1) 'ydate) date-2 (getd date-1 'ydate))
    
      ; (setf (svref ith 4)    (sector-index *data-name*)) ;;features 2
     ;  (setf (svref ith 5)     (wpp-index date-1));;;feature 3      

      ; (setf (svref ith 6)    (modivpx date-1 4 21)) ;feature 4      
     ;  (setf (svref ith 7)    (fast-stochastic-index date-1 21)) ;;;feature 5*
  
      (setf (svref ith 8)    (slow-stochastic-index date-1 21)) ;feature 6 
                             
    ;  (setf (svref ith 9)    (wpp-index  (getd date-2 'ydate))) ;feature 7 
    ;  (setf (svref ith 10)   (fast-stochastic-index date-1 13)) ;feature 8 
     ; (setf (svref ith 11)   (cci-direction date-1 5 2)) ;; feature 9                          
     ; (setf (svref ith 12)   (fast-stochastic-index date-1 1)) ;;feature 10
                                                      
     ; (setf (svref ith 13)   (fast-stochastic-index date-1  4)) ;;feature 11 
    ;  (setf (svref ith 14)   (modivpx date-1 4 13)) ;feature 12                           
     ;                                                   
     ; (setf (svref ith 15)   (fast-stochastic-index date-1 2)) ;;;feature 13                      
    ; (setf (svref ith 16)   (vril date-1 4 1)) ;;;feature 14*                   
                           
 ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist over daytrades


  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth daytrades)
      (format str "~S~%" jth)))
     );;;closes the dolist over markets
 (build-daytrade-warehouse3 markets)

))



;;;use same features as voltest2 *day-features1*
(defun daytrade-simulation-test3 (market date2 num &optional (features nil))
 (let (date trades long short cover-long cover-short entry-long entry-short record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk reward 
        lproj5 rri20 rri13 rri10 rri5  ccir21-5 ccis11-21 refl8
       vri4-40    vri1-7 vril5-2 vrt5 vsos4
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc   bin draw ignore singles unfiltered-trades  grp
     
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary3.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation3.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary3.dat")))
  ; (declare (ignore long-acc short-acc))
   (set-market market)
   (setq *charger-lite-list* nil);*day-warehouse-list*)
   (if (and num (> num (available-days market date2 )))
       (setq num nil))
    (ifn num (setq num (available-days market date2 )))

   (setq date (add-mkt-days date2 (- num)))
   
   (build-daytrade-warehouse3 (list market))
   (setq features (find-best-indicator-set3a))

  (format T "~%~A~%" *data-name*)
;    (if (member *data-name* (append *indexes-list* *currencies-list*))
 ;          (setq *entry-factor* .382)
         (setq  *entry-factor* .764)
  
     (setq  *stop-loss-day* 1.0557 ;1.382
          *objective-reward* 11.09 ;6.854 ;1.0557 
          *min-day-expected-value* 80 *max-day-risk* 3000 ) 

  (if (member market *forex-warehouse-list*)
         (setq *commission* 0 *pips-slippage* 6 *min-day-expected-value* 20)
        (setq  *commission* 6  *pips-slippage* 0))  ;;;1.0557)
 

      (multiple-value-setq (ignore ignore ignore singles)
             (apply #'day-trade-bins3b features))
             ;(apply #'day-trade-bins3a features))

   
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

 ;(format T "Market= ~A date=~A~%" *data-name* date)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
  
   (setq risk (min (/ *max-day-risk* (calculate-point-value date))(volatility date 4 *stop-loss-day*))
         reward (volatility date 21 *objective-reward*))

   (setq vri4-40 (volatility-ratio-index date 4 40 1)
         vri1-7 (volatility-ratio-index date 1 7 1)
         vril5-2 (vril date 5 2) vrt5 (volatility-trend date 5 1)  
          rri5 (ep-roc-index10 date 5) rri10 (ep-roc-index10 date 10) 
          rri20 (ep-roc-index10 date 20) rri13 (ep-roc-index10 date 13)
         ; swi (ww-swing-index1 date)
        ; lbds5-2 (lbounds date 5 2)
        ; ccid (cci-direction date 21 2)        
         ccis11-21 (cci-slope-index date 11 21) refl8 (reflect2 date 8)
        ; chan21 (channel-direction date 21)
       ;  ss14 (slow-stochastic-index date 14) 
        lproj5 (lproj-index date 5)  
        vsos4 (volume-openint-div date 4)  ;feature 10  
   ;      ccir21-11 (cci-range-index date 21 11)
         ccir21-5 (cci-range-index date 21 5) 
         )

   (setq  date-1 date date (add-mkt-days date 1))
#|
   (when  (/= (end-of-prior-month date)(end-of-prior-month date-1)) 
         (build-daytrade-warehouse3 grp (end-of-prior-month date-1))
 ;        (setq features (find-best-indicator-set3a))
         (apply #'day-trade-bins3b features))
|#
   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover))
    ))

   (setq record (vector date (getd date 'close) 0 0 0))

;;;;static stop loss
   (setq stop-long  (-  entry-long  risk) cover-long 
                        (fmin (+ entry-long reward)(add (getd date-1 'close)(index-limit)))
         stop-short (+ entry-short risk) cover-short 
                       (fmax (- entry-short reward)(sub (getd date-1 'close)(index-limit))))
  
   (if long
       (setq stop-long (my-pretty-price stop-long) cover-long (my-pretty-price cover-long)))
   (if short
       (setq stop-short (my-pretty-price stop-short) cover-short (my-pretty-price cover-short)))
;;;trailing stop loss
;    (setq stop-long (- (getd date 'high) risk) stop-short (+ (getd date 'low) risk))

;;;;calculate bin-classifier only as needed
 (when (and (or (>= (getd date 'high) entry-long)
                (<= (getd date 'low) entry-short))
           ; (not (member *data-name* *charger-lite-list*))
             )
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                  (bin-classifier-daytrades3b date-1 features))
         )
  ; (format T "EPSIGNAL = ~A date = ~A~%" epsignal date)
;;;check if new entry
   (when (and  (<= (getd date 'low) entry-short)
            #| (if (member *data-name* *charger-lite-list*) T 
             (if (member *data-name* *charger-lite-list*) T  (>= (/ short-gains shorts) *min-day-expected-value*)) 
	      (>= (* risk (calculate-point-value date-1)) 75)
           ;   (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
              (> vri4-40 -4)(< vrt5 4)(> vrt5 -4)
               (<= vri1-7 5)(< vril5-2 4)
              (> lproj5 -2);(/= ccid -3)(/= ccid 3);(/= lbds5-2 -2)
              ; (/= swi -3);(/= swi 3)
             ; (not (member chan21 '(US AT BT BT2 UC+ IC- DC-))) 
             ; (< ccir21-11 200)
              (<= rri10 2) (<= rri5 3)(/= rri13 7)(/= rri20 6)
              ;(< ss14 3);(/= ccir21-11 0)(/= ccir21-11 7)
              (< ccir21-5 5)(/= ccis11-21 -3)
                 )
             |# 
             (member epsignal '(OK DOWN))
              (/= refl8 -2)
              (cond ((eql *data-name* 'dj.d1b)(member vsos4 '(2 -1))) 
                    ((eql *data-name* 'nd.d1b)(member vsos4 '(-1)))
                    ((eql *data-name* 'sp.d1b)(member vsos4 '(-1 2)))
                    ((eql *data-name* 'ru.d1b)(member vsos4 '(2)))
                    ((eql *data-name* 'gc.d1b)(member vsos4 '(-1 1)))
                    ((eql *data-name* 'cl.d1b)(member vsos4 '(-2 2)))
                    ((eql *data-name* 's.d1b)(member vsos4 '(-1 2 1 -2)))
                    ((eql *data-name* 'w.d1b)(member vsos4 '(-1)))
                    ((eql *data-name* 'c.d1b)(member vsos4 '(2 -1)))
                  )
                  )          
   ;          (format T "~%date= ~A Short= ~A" date short)
          (setq short (min entry-short (getd date 'open))
                short-trade (list date 'short short)
                )
           (setf (svref record 2) (+ (svref record 2) 1));;;this is the quantity not trade direction
    
                 )

    (when (and 
               (>= (getd date 'high) entry-long)
      #|         (if (member *data-name* *charger-lite-list*) T (member epsignal '(OK UP))) 
               (if (member *data-name* *charger-lite-list*) T (>= (/ long-gains longs) *min-day-expected-value*)) 
               (>= (* risk (calculate-point-value date-1)) 75)
	   ;    (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
               (> vri4-40 -4) (< vrt5 4)(> vrt5 -4)
               (<= vri1-7 5)(< vril5-2 4)
               (< lproj5 2);(/= ccid -3)(/= ccid 3);(/= lbds5-2 2)
               ; (/= swi 3);(/= swi -3)(/= ss 4)(/= ss -4)
              ; (not (member chan21 '(DS AT BT BT2 UC+ IC- DC-)))
              ; (< ccir21-11 200)
               (>= rri10 -2) (>= rri5 -3)(/= rri13 -7)(/= rri20 -6)
              ; (> ss14 -3);(/= ccir21-11 0)(/= ccir21-11 7)
              (< ccir21-5 5)(/= ccis11-21 3)
                )
|#
              (member epsignal '(OK UP))
               (/= refl8 2)
                 (cond ((eql *data-name* 'dj.d1b)(member vsos4 '(2 -1))) 
                    ((eql *data-name* 'nd.d1b)(member vsos4 '(2 -2 1)))
                    ((eql *data-name* 'sp.d1b)(member vsos4 '(2 -2)))
                    ((eql *data-name* 'ru.d1b)(member vsos4 '(2 -1)))
                    ((eql *data-name* 'gc.d1b)(member vsos4 '(1 2)))
                    ((eql *data-name* 'cl.d1b)(member vsos4 '(2 -1)))
                    ((eql *data-name* 's.d1b)(member vsos4 '(-2 1)))
                    ((eql *data-name* 'w.d1b)(member vsos4 '(2)))
                    ((eql *data-name* 'c.d1b)(member vsos4 '(-1 -2)))
                  )
             )
 
    ;      (format T "~%Date= ~A Long = ~A" date long)
           (setq long (max entry-long (getd date 'open))
                 long-trade (list date 'long long)
                 )
           (setf (svref record 2)(+ (svref record 2) 1))
                 )

;;;check if met exit at target on day of entry
     (when (and long cover-long (> (getd date 'high) cover-long))
            (setq cover-long (max (getd date 'open) cover-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1))
                                 (comm+slip date-1))) trades)
            (setq long-trade
                  (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'O
                                                           cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1))
                                 (comm+slip date-1)))))
            (setq long-trade nil long nil stop-long nil cover-long nil))

      (when (and short cover-short (< (getd date 'low) cover-short))
           (setq cover-short (min (getd date 'open) cover-short))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1))
                               (comm+slip date-1))) trades)
            (setq short-trade
                 (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'O
                                                           cover-short (my-pretty-price (- short cover-short))))))
            (push short-trade extended-trades)
            (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1))
                               (comm+slip date-1)))))
            (setq short-trade nil short nil stop-short nil cover-short nil))

 ;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
                 
                   (<= (getd date 'close) stop-long))
  ;         (<= (getd date 'low) stop-long)
           )
           (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date-1))
                              (comm+slip date-1))) trades)
           (setq long-trade
                 (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'S
                                                          stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)

           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date-1))
                              (comm+slip date-1)))))
           (setq long-trade nil long nil stop-long nil cover-long nil
           ))
    
    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                  
                    (>= (getd date 'close) stop-short))
         ;      (>= (getd date 'high) stop-short)
                 )
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date-1))
                              (comm+slip date-1))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                       (contract-month *data-name* date) 'S
                                                                      stop-short (my-pretty-price (- short stop-short))))))
           (push short-trade extended-trades)

           (setf (svref record 3) (+ (svref record 3)
                (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date-1))
                            (comm+slip date-1)))))
           (setq short-trade nil short nil stop-short nil cover-short nil
           ))

;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (max (getd date 'close) stop-long))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date-1))
                               (comm+slip date-1))) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                       (contract-month *data-name* date) 'N
                                                                   cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)

            (setf (svref record 3) (+ (svref record 3)
                   (round (- (*  (my-pretty-price (- cover-long long))(calculate-point-value date-1))
                                 (comm+slip date-1)))))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (min (getd date 'close) stop-short))
         ;    (format T "123 date=~A short-trade= ~A~% record= ~A" date short-trade  record) 
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1))
                              (comm+slip date-1))) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                       (contract-month *data-name* date) 'N
                                                                  cover-short (my-pretty-price (- short cover-short))))))
           (push short-trade extended-trades)

           (setf (svref record 3) (+ (svref record 3)
                 (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date-1))
                              (comm+slip date-1)))))
        ;     (format T "~%234 date=~A short-trade= ~A~% record= ~A" date short-trade  record) 
            (setq short-trade nil short nil stop-short nil))


      (setf (svref record 4) (+ (svref record 3) running-sum))
      (setq running-sum (svref record 4))
      (push record trading-dates)
      (setq record nil)


   );;;closes the dotimes
;;;writes out the diary file
     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (dolist (ith trading-dates)
            (format stream "~A\,~F\,~D\,~D,~D~%"
               (svref ith 0) (svref ith 1) (svref ith 2) (round (svref ith 3))
              (round  (svref ith 4)))
     ))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) 
          )
;   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
    (format str "~A traded from ~A to ~A~%" (index-lname)
                   (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLE RATIO = ~A~%" *stop-loss-day* singles)
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  %DAYS IN TRADE=    ~F    COMMISSION=        ~D~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED OUT=    ~,1,2,'*,' F%"

       (round (list-sum trades))  (length trades)
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
      (my-round (* 100 (/ (length trades) num)) 1) *commission*
      (setq draw (round (drawdown trades)))
       (if (plusp (list-sum trades))
          (round (optimal-f trades)) 0)
       (/ (count 'S (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1))
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))

     (format str "~%AVE ANNUAL RETURN= ~5,1,2,'*,' F%"
           (/ (/ (list-sum trades) 
                  (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) )
 
    (setq unfiltered-trades (trim-trades daytrades *data-name* (add-mkt-days date2 (- num)) date2))
    (if unfiltered-trades
    (display-unfiltered-trades unfiltered-trades  (add-mkt-days date2 (- num)) str) )
 

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


(defun display-unfiltered-trades (swings &optional (sdate 0)(str T))
   (let (training-data markets-list num-trades first-date last-date ave-dur
         draw pl pf ave-pl)
        
   
    (setq training-data (num-markets-in-warehouse3 swings))
    (push 'all training-data)
    (dolist (ith training-data)
      (multiple-value-setq (num-trades first-date last-date ave-dur)
                         (num-trades-in-warehouse3 ith swings sdate))
      (multiple-value-setq (pl ave-pl pf draw) (gain-loss-trades-in-warehouse3 ith swings first-date))
      (setq markets-list
           (cons (list ith num-trades pl ave-pl pf draw first-date last-date ave-dur) markets-list))
       );;closes the dolist
    (vsort markets-list #'> #'fifth)
    
     (format str "~%~%Market    #trades    $P&L    $Ave    PF   Drawdown    FIRST    LAST    DUR~%")
    (dolist (jth markets-list)
     (format str"~%~10A ~5D ~9@D  ~4D   ~4,2F  ~8@D  ~A  ~A  ~A"
     (first jth) (second jth)(round (third jth)) (round (fourth jth))(fifth jth)
      (round (sixth jth))(seventh jth)(eighth jth)(ninth jth)))


  ;   (multiple-value-setq (num-trades first-date last-date )(num-trades-in-warehouse3 'all swings))
  ;   (multiple-value-setq (pl ave-pl pf draw) (gain-loss-trades-in-warehouse3 'all swings))
  ; (format str"~2%~10A ~4D ~10D   ~4D    ~3A  ~D  ~A  ~A" 'ALL  num-trades pl ave-pl pf draw first-date last-date)  

  ));;closes the let and defun.






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun day-trade-bins3b ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse3.dat"))
    (maind-x)(set-cat-list)
    (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith (- (length features) 1))
         (setq bin (encode-day-trades3 record (butlast features ith)))
         
          (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
            ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
              (push bin day-bin-codes)))
          ));;;closes the doltimes and dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)

  ))



(defun day-trade-bins3a ( &rest features)
  (let (bin path)

;      (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades3.dat"))
      (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse3.dat"))
  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades);;;record is a vector and bin is a list
     (setq bin (encode-day-trades3 record features))

     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
             (push bin day-bin-codes)))
         );;;closes the dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)

  ))

#|
(defun day-trade-bins3 ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades3.dat"))
 ;     (setq path (string-append *upper-dir-warehouse*  "daytradewarehouse3.dat"))
  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

;;;trades are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
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
)

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3 daytrades)


  ))
|#

(defun encode-day-trades3 (record features)
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
;;;Feature 6 is mo-div date 5 with 5 levels
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
;;;this function only looks at separability not predictability
;;;uses the composite warehouse
(defun daytrades-add-one-in3a (base-features candidate-features)
  (let (winners-list (result 0) average-profit rtb rp single-bins csgof)
 ;;;rtb is the ratio of trades to bins
;;;rp is the ratio of bin profits to all winners
  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'day-trade-bins3a (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
    (vsort winners-list #'> 'first)
 ;   (vsort winners-list #'> 'seventh);;; by csgof
 ;  (if (zerop (sixth (first winners-list)))
 ;     (vsort winners-list #'> 'third))
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))


;;;requires a base features list
(defun daytrades-leave-one-out3a (base-features)
  (let (winners-list (result 0) rtb rp average-profit single-bins csgof)

  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
                    (apply #'day-trade-bins3a (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;    (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
 ; (vsort winners-list #'> 'third);;;average profit
;  (vsort winners-list #'> 'seventh)
     (vsort winners-list #'> 'first);;;total P&L
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))

(defun find-best-indicator-set3a ()
  (let (base-list candidate-list winners-list (result 0) bf); tdate best-features)
        ; (pathout (string-append *config-dir* "day-features3.dat")))

;;;the base list are context indicators like direction and market sector
;    (setq base-list '(1 2) candidate-list '( 3 4 5 6 7 8 9 10 11 12 13 14))
    (setq base-list '(1 2)
                      candidate-list '(3 4 5 6 7 8 9 10 11 12 13 14))
   (loop
    (setq winners-list (daytrades-add-one-in3a base-list candidate-list));

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)(> result .85)
             (< (fifth (car winners-list)) 1.7)
             (> (fourth (car winners-list)) .23))
         (return))
     )
    (format T "First stage winners-list= ~A"  winners-list)
;   (setq base-list (append base-list candidate-list))
   (loop     
           
      (if (neql (second (car winners-list)) 1)
          (if (and (> (fifth (car winners-list)) 3.0)
                   (< (fourth (car winners-list)) .17))(return))
        (if (and (> (fifth (second winners-list)) 3.0)
                 (< (fourth (second winners-list)) .17))(return)));
        
      (setq winners-list (daytrades-leave-one-out3a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
     ;base-list
     (setq bf (cons 1 (remove 1 (reverse (cdr (mapcar #'second winners-list))))))
     (if (> (length bf) (length base-list))(butlast bf) bf) 
    ))
 
;;;This rank is for using multiple length features

(defun rank-daytrade-bins-by-profit3b (daytrades features &optional (stream T))
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)(twr 1)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0)
        longest-day-bin-codes (longest-feature (length features)))
   (setq longest-day-bin-codes (remove-if #'(lambda (s) (< (length s) longest-feature)) day-bin-codes))
   (dolist (ith longest-day-bin-codes)
     (setq contents (gethash ith *day-trade-warehouse3*))
     (setq result 0 counter 0 twr 1)

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
        ; (setq twr (* twr (+ 1 (/ (if (plusp (svref kth 2))
        ;                         (- (svref kth 18) (svref kth 3))
        ;                       (- (svref kth 3) (svref kth 18)))
        ;                     (svref kth 3)))))

         ) ;;;closes dolist over contents
      (if (and (plusp result));(> twr 1))
           (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))

     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth daytrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth)));(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format stream "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length daytrades)(length (num-markets-in-warehouse3 daytrades)))
     (format stream "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format stream "NUMBER of ALL BINS = ~D~%" (length longest-day-bin-codes))
     (format stream "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length daytrades)(length longest-day-bin-codes)) 1))
     (format stream "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format stream "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format stream "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format stream "PROFIT PER TRADE IN WINNING BINS = ~A~%~%"
           (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format stream "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format stream "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~3,2F~%~%" (my-round (/ only-one (length longest-day-bin-codes)) 2))
     (format stream "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~3,2F~%~%" (my-round (/ only-one (length daytrades)) 2))

    (values (round winners)(round (+ all-winners all-losers))
            (if (zerop num-in-winning-bins) 0 (my-round (/ winners num-in-winning-bins) 2))
           (my-round (/ only-one (length daytrades)) 2)) 

 ))

(defun rank-daytrade-bins-by-profit3 (daytrades &optional (stream T))
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0) 
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith day-bin-codes)
     (setq contents (gethash ith *day-trade-warehouse3*))
     (setq result 0 counter 0 )

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
     ;    (setq twr (* twr (+ 1.0 (/ (if (plusp (svref kth 2))
     ;                            (- (svref kth 18) (svref kth 3))
     ;                          (- (svref kth 3) (svref kth 18)))
     ;                        (svref kth 3)))))

         ) ;;;closes dolist over contents
      (if  (plusp result)
               (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))


     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth daytrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (plusp (car jth))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format stream "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length daytrades)(length (num-markets-in-warehouse3 daytrades)))
     (format stream "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format stream "NUMBER of ALL BINS = ~D~%" (length day-bin-codes))
     (format stream "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length daytrades)(length day-bin-codes)) 1))
     (format stream "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format stream "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (if (zerop all-winners) 0 (my-round (/ winners all-winners) 2)))
     (format stream "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format stream "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format stream "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format stream "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%" (my-round (/ only-one (length day-bin-codes)) 2))
     (format stream "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%" (my-round (/ only-one (length daytrades)) 2))
     (format stream "CHI SQUARE METRIC = ~D~%"  (daytrade-chi-squared-gof))

    (values (round winners)
            (my-round (/ (length daytrades)(length day-bin-codes)) 1);;;ratio of trades to bins
           ;  (round (+ all-winners all-losers))
            (if (zerop num-in-winning-bins) 0 (my-round (/ winners num-in-winning-bins) 2))

            (my-round (/ only-one (length daytrades)) 2)
            (if (zerop all-winners) 0 (my-round (/ winners all-winners) 2))
            (daytrade-chi-squared-gof)) 
  
 ))


(defun display-day-bin (bin)
  (let (contents)
     (setq contents (gethash bin *day-trade-warehouse3*))     
     (dolist (ith contents)
       (print ith))))




;;;;this function expects that you have run (day-trade-bins ...) already
(defun display-daytrade-bins-by-expected-value3 (feature)
  (let (contents expected-value-list result (winners 0)(losers 0) shorts longs metric prob
          (path1 (string-append *output-upper-dir* "daytrade-expected-value-"
                    (format nil "~A" feature) ".dat")))
   (dolist (ith day-bin-codes)
     (setq contents
           (gethash ith *day-trade-warehouse3*))
     (setq result 0 winners 0 losers 0)
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19))(incf winners)(incf losers)))
     (setq expected-value-list 
           (cons (list (/ result (length contents)) ith (length contents) result winners losers) expected-value-list)));;;closes the dolist over bin-codes
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (vsort expected-value-list #'> 'car)
       ;  (format stream "Profit/trade  Bin         NUM   #WINNERS   #LOSERS   Profit~%~%")
       ;  (dolist (jth expected-value-list)
       ;      (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
       ;         (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
         (format stream "~%Total #Trades = ~D  #WINNERS = ~D    $Profit = ~D"
             (list-sum (mapcar #'(lambda(s) (nth 2 s)) expected-value-list))
             (list-sum (mapcar #'(lambda(s) (nth 4 s)) expected-value-list))
             (round (list-sum (mapcar #'(lambda(s) (nth 3 s)) expected-value-list))))
           (multiple-value-setq (metric prob)(daytrade-chi-squared-gof))
          (format stream "~%CHI SQUARED METRIC = ~D  PROB= ~4,2F~%~%" metric prob ) 
   ; (print expected-value-list)
          (setq longs (remove-if #'(lambda(s) (eql (caadr s) -1)) expected-value-list))
           (vsort longs #'> 'car)
           (dolist (jth longs)
             (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
           (terpri stream)
;;;;Now compute the linear correlation of ave profit per bin to index value is a number
       ;   (format T "~%Correlation= ~A~%" (bin-regression longs))
 
         (setq shorts (remove-if #'(lambda(s) (eql (caadr s) 1)) expected-value-list))
           (vsort shorts #'> 'car)
           (dolist (jth shorts)
             (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
        
        ;   (format T "~%Correlation=  ~A~%" (bin-regression shorts))
                        

                ) ;;;;closes the with-open-file
 ))
;;;;this function will remove duplicate S-expressions from a file
;;;of S-expressions.
;;;It was written for trades in the warehouse but is actually much more general.
;;;must use #'equalp if the s-expressions are vectors
(defun remove-duplicate-trades3 (path)
   (let (trades)

    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))
   (setq trades (remove-duplicates trades :test #'equalp))

   (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith trades)
        (format stream "~S~%" ith)))

))

;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
;;;;date-1 is the current day that is known
(defun bin-classifier-daytrades3b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal ;(crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0);(twr-long 1.0)(twr-short 1.0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0) contentsl contentss)
    (setq record (create-daytrade-entry-record-list3 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith (- (length features) 1))
     (setq bin (encode-day-trades3 record features))
     (setq contentsl (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
   ;  (setq twr-long (day-bin-twr3 bin date))
     (setf (nth 0 bin) -1); (setq twr-short (day-bin-twr3 bin date))
    (setq contents (append (setq contentss (gethash bin *day-trade-warehouse3*)) contentsl))
;;;here we remove any trades in the same market with the same next day
   ; (format T "contents = ~A~% ~%" contents )
   (setq contents
    (remove-if #'(lambda(s1) (<= nxdate (svref s1 1))) contents))
 ;        (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if (and contentss contentsl) (return) (setq features (butlast features)))
  );;;closes the dotimes
     
 ;   (format T "~%contents = ~A" contents)
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
;;;now lets remove the commission   
    
      (setq result 0)
    (dolist (jth contents)
         (setq result (+ result (svref jth 19)))
        (if (plusp (svref jth 19)) (incf counter))) ;;;closes dolist over contents

    (cond ((and (plusp results-long)(plusp results-short)
             ; (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
             ; (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
             ; (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc)
           
               )
              (setq epsignal 'OK))
        ((and (plusp results-long)
             ; (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
             ; (>= (float (/ num-winners-long longs)) crit-acc)
             
                 )
              (setq epsignal 'UP))
        ((and (plusp results-short)
             ; (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
             ; (>= (float (/ num-winners-short shorts)) crit-acc)
            
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
;;;calculates the twr for a bin
;;;must run #'day-trade-bins3a before running this function
(defun day-bin-twr3 (bin &optional (date nil))
  (let ((twr 1.0d0) (contents (gethash bin *day-trade-warehouse3*)) nxdate)
     (if date
         (setq nxdate (getd date 'ndate)
               contents  (remove-if #'(lambda(s1) (eql nxdate (svref s1 1))) contents)))
     (dolist (record contents)
 ;    (print (+ 1.0d0 (/ (* (svref record 2)(- (svref record 18) (svref record 3)))
 ;                                (svref record 3))))
;;(svref record 2) is the direction +1 or -1 
         (setq twr (* twr (+ 1.0d0 (/ (* (svref record 2)(- (svref record 18) (svref record 3)))
                                        (svref record 3)))
                           )))

 twr
))

|#

(defun find-best-daytrade3 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk stop-short stop-long  entry-long entry-short 
        vri4-40 vri1-7 vril5-2 vrt5 rri20 rri13 rri10 rri5 lproj5 ccis11-21 ccir21-5;lbds5-2    
        trade-direction  cover-short cover-long pacific-entry-time pacific-exit-time
        pacific-cancel-time pacific-end-session-time  rjo-starter-path3
        central-entry-time central-exit-time central-cancel-time central-end-session-time
        action directive1 sol-starter-path3 sol-score-path3 ;foremost-starter-path3 
        kingsview-starter-path3 striker-starter-path3 striker-score-path3
        
        ninja-score-path3 ninja-entry-path3 ;daniels-starter-path3 daniels-score-path3 apex-path3
        rjobrien-starter-path3 rjobrien-score-path3
         ; exit-time entry-time cancel-time
           ts-score-path2 ts-starter-path2
          oec-symbol oco-code ninja-starter-path3 (time-zone "CT") offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc counter))

;     (if (member *data-name* (append *indexes-list* *currencies-list*))
 ;          (setq *entry-factor* .382)
         (setq  *entry-factor* .764)
      (setq *stop-loss-day* 1.0557
          *commission* 6 *max-day-risk* 3000 ;2500
          *min-day-expected-value* 80 *pips-slippage* 0) 
        (setq *charger-lite-list* nil)
    (setq 
          sol-starter-path3 (string-append *daily-output-dir* "sol-starter.csv")
          sol-score-path3 (string-append *daily-output-dir* "sol-score.csv")         
         
          striker-starter-path3 (string-append *daily-output-dir* "striker-starter.csv")
          striker-score-path3 (string-append *daily-output-dir* "striker-score.csv")
       ;   striker-entry-path3 (string-append *daily-output-dir* "striker-entry.csv")
         
          kingsview-starter-path3 (string-append *daily-output-dir* "kingsview-starter.csv")
          ninja-score-path3 (string-append *ninja-output-dir* "ninja-score-" (format nil "~A" date1) ".csv")
     ;     daniels-starter-path3 (string-append *daily-output-dir* "daniels-starter.csv")
     ;     daniels-score-path3 (string-append *daily-output-dir* "daniels-score.csv")
      ;    apex-path3 (string-append *daily-output-dir* "apex-starter.csv")
          ninja-starter-path3 (string-append *ninja-output-dir* "ninja-starter-" (format nil "~A" date1)  ".csv");;;for Pat Aucoin and NinjaScript
          ninja-entry-path3 (string-append *ninja-output-dir* "ninja-entry-" (format nil "~A" date1) ".csv")
          rjo-starter-path3 (string-append *daily-output-dir* "rjo-starter.csv");;;for Susan Green
          rjobrien-starter-path3 (string-append *daily-output-dir* "rjobrien-starter.csv")
          rjobrien-score-path3 (string-append *daily-output-dir* "rjobrien-score.csv")
          ts-score-path2 (string-append *daily-output-dir* "ts-triumph21-" (format nil "~A" date1) ".csv")
          ts-starter-path2 (string-append *daily-output-dir* "ts-triumph11-" (format nil "~A" date1) ".csv")
	  )
 
     (setq vri4-40 (volatility-ratio-index tdate 4 40 1)
           vri1-7 (volatility-ratio-index tdate 1 7 1) vril5-2 (vril tdate 5 2)
            vrt5 (volatility-trend tdate 5 1) 
          ; md5-21 (momentum-divergence3 tdate 5 21)
            rri5 (ep-roc-index10 tdate 5)  rri10 (ep-roc-index10 tdate 10)
            rri20 (ep-roc-index10 tdate 20) rri13 (ep-roc-index10 tdate 13)
          ; swi (ww-swing-index1 tdate)
          ;  lbds5-2 (lbounds tdate 5 2)
          ; ccid (cci-direction tdate 21 2)
           ccis11-21  (cci-slope-index tdate 11 21) 
          ; chan21 (channel-direction tdate 21)
       ;    ss14 (slow-stochastic-index tdate 14)
           lproj5 (lproj-index tdate 5)   
          
     ;      ccir21-11 (cci-range-index tdate 21 11)
           ccir21-5 (cci-range-index tdate 21 5) 
           )
 
    (format T "~%EPSIGNAL = ~A  VRI4-40 = ~A VRT5= ~A VRI1-7= ~A VRIL5-2 = ~A~%"
            epsignal vri4-40 vrt5 vri1-7 vril5-2 )
     (format T "CCIR21-5 = ~A CCIS11-21 = ~A~%"  ccir21-5 ccis11-21)
     (format T "lproj5 = ~A RRI20 = ~A RRI13= ~A RRI10 = ~A RRI5= ~A~%"  lproj5 rri20 rri13 RRI10 RRI5) 
     (format T "  longs = ~A  long-gains = ~A ave = ~A~%" longs long-gains (round (if (zerop longs) 0 (/ long-gains longs))))
     (format T "  shorts = ~A  short-gains = ~A ave = ~A~%" shorts short-gains (round (if (zerop shorts) 0 (/ short-gains shorts))))

  (setq risk (min (/ *max-day-risk* (calculate-point-value tdate))
                  (volatility tdate 4 *stop-loss-day*)))

   (if (and (if (member *data-name* *charger-lite-list*) T (member epsignal '(OK DOWN)))
            (if (member *data-name* *charger-lite-list*) T 
               (>= (/ short-gains shorts) *min-day-expected-value*))
            ;(<= (* risk (calculate-point-value tdate)) *max-day-risk*)
              (> vri4-40 -4) (< vrt5 4)(> vrt5 -4)
              (<= vri1-7 5)(< vril5-2 4)
              (> lproj5 -2);(/= ccid -3)(/= ccid 3);(/= lbds5-2 -2)
             ; (/= swi -3)
          ;    (not (member chan21 '(US AT BT BT2 UC+ IC- DC-))) 
             (<= rri10 2) (<= rri5 3)(/= rri13 7)(/= rri20 6)
             ; (< ss14 3);(/= ccir21-11 0)(/= ccir21-11 7)
            (< ccir21-5 5)(/= ccis11-21 -3)   
              )
          (push 'DN trade-direction)(push 'FT trade-direction))

  (if (and (if (member *data-name* *charger-lite-list*) T (member epsignal '(OK UP)))
            (if (member *data-name* *charger-lite-list*) T 
              (>= (/ long-gains longs) *min-day-expected-value*))
             ; (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
              (>= vri4-40 -4)(< vrt5 4)(> vrt5 -4)
              (<= vri1-7 5)(< vril5-2 4)
              (< lproj5 2);(/= ccid -3)(/= ccid 3);(/= lbds5-2 2)
              ; (/= swi 3)
      ;         (not (member chan21 '(DS AT BT BT2 UC+ IC- DC-))) 
              (>= rri10 -2)(>= rri5 -3)(/= rri13 -7)(/= rri20 -6)
           ;    (> ss14 -3);(/= ccir21-11 0)(/= ccir21-11 7)
           (< ccir21-5 5) (/= ccis11-21 3)
           )
         (push 'UP trade-direction)(push 'FT trade-direction))
 

   (multiple-value-setq (entry-short entry-long) (vprices tdate 4 *entry-factor* 1))

 
 ;  (setq risk (- entry-long stop-long))
   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))


;            (when (if (member *data-name* '(US.D1B TY.D1B))
;                  (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
;                                   (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-day-risk*)
;                 (> (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
;                                  (* (index-tick-size) (round entry-short (index-tick-size))))
;                                  (index-point-value))) *max-day-risk*)) (push "NOT TODAY" action))

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

     ; (format output " ~A vri2= ~A vri1= ~A rri10= ~A rri5= ~A~%" action vri2 vri1 rri10 rri5)
      (format output "~% Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
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
           pacific-entry-time "default" ;(second (assoc *data-name* *oec-market-times-list*)) ;;this is the release time
	   pacific-end-session-time "default" ;  (string-append (date-convert date1) " " (third (assoc *data-name* *cannon-market-times-list*)))
   ;        pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) 0))
           pacific-cancel-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter))
      (setq

          ; central-entry-time  (string-append (date-convert tdate) " " (second (assoc *data-name* *foremost-market-times-list*))) ;;this is the release time
            central-entry-time "default" ;;this is the release time "default"
	   central-end-session-time "default" ; (string-append (date-convert date1) " " (third (assoc *data-name* *foremost-market-times-list*)))
   ;        central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) 0))
           central-cancel-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26))
	   oco-code (format nil "OSO~A" counter)) ;;;(add-minutes1 exit-time -30))
      (format T "~%~A  ~A~%" *data-name* action)
     (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)

             (when (member *data-name* *triumph21-list*)

               (setq offset (random-choice -1 0)
                     pacific-exit-time (string-append (date-convert date1) " "
                                                  (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
  ;             (write-oec-short  cannon-score-path3 oec-symbol *cannon-score-block-acct* *cannon-score-qty* entry-short stop-short pacific-entry-time
  ;                               pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
  ;             (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
  ;             (write-oec-long  cannon-score-path3 oec-symbol *cannon-score-block-acct* *cannon-score-qty* entry-long stop-long pacific-entry-time
  ;                              pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

   ;            (setq offset (random-choice -1 0)
   ;                  pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
   ;            (write-oec-short  daniels-score-path3 oec-symbol *daniels-score-block-acct* *daniels-score-qty* entry-short stop-short pacific-entry-time
   ;                              pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
   ;            (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
   ;            (write-oec-long  daniels-score-path3 oec-symbol *daniels-score-block-acct* *daniels-score-qty* entry-long stop-long pacific-entry-time
   ;                             pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
              

  ;            (setq offset (random-choice -1 0)
  ;             central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
  ;            (write-oec-short rjobrien-score-path3 oec-symbol *rjobrien-score-block-acct* *rjobrien-score-qty* entry-short stop-short central-entry-time
  ;                             central-cancel-time central-exit-time central-end-session-time oco-code)
  ;            (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
  ;            (write-oec-long rjobrien-score-path3 oec-symbol *rjobrien-score-block-acct* *rjobrien-score-qty* entry-long stop-long central-entry-time
  ;                            central-cancel-time central-exit-time central-end-session-time oco-code)

              (setq offset (random-choice -1 0)
               central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
              (write-oec-short striker-score-path3 oec-symbol *striker-score-block-acct* *striker-score-qty* entry-short stop-short central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)
              (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
              (write-oec-long striker-score-path3 oec-symbol *striker-score-block-acct* *striker-score-qty* entry-long stop-long central-entry-time
                              central-cancel-time central-exit-time central-end-session-time oco-code)

              (with-open-file (ninja-output ts-score-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-score-qty*))
              (with-open-file (ninja-output ts-score-path2 :direction :output :if-exists :append :if-does-not-exist :create)
                 	     (write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short *striker-score-qty*))
               

      	       (with-open-file (ninja-output ninja-score-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	            	 (write-ninja-record ninja-output time-zone tdate date1 'SELL entry-short stop-short)
                     (write-ninja-record ninja-output time-zone tdate date1 'BUY  entry-long stop-long))
              );;;closes when *triumph21-list*


;             (when (member *data-name* (set-difference *score-list* *ice-list*));
;
;               (setq offset (random-choice -1 0)
;                    central-exit-time  (string-append (date-convert date1) " "
;                                                      (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)));
;
;               (write-oec-short sol-score-path3 oec-symbol *sol-score-block-acct* *sol-score-qty* entry-short stop-short central-entry-time
;                                central-cancel-time central-exit-time central-end-session-time oco-code)
;               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
;               (write-oec-long sol-score-path3 oec-symbol *sol-score-block-acct* *sol-score-qty* entry-long stop-long central-entry-time
;                               central-cancel-time central-exit-time central-end-session-time oco-code)

;	       );;closes the when
             (when (member *data-name* *triumph11-list*)
                 (setq offset (random-choice -1 0)
                    pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))

;               (write-oec-short  cannon-starter-path3 oec-symbol *cannon-starter-block-acct* *cannon-starter-qty* entry-short stop-short pacific-entry-time
;                                 pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
;               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
;               (write-oec-long  cannon-starter-path3 oec-symbol *cannon-starter-block-acct* *cannon-starter-qty* entry-long stop-long pacific-entry-time
;                                pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

;	       (with-open-file (rjo-output rjo-starter-path3 :direction :output :if-exists :append :if-does-not-exist :create);
;	        	 (write-rjo-record rjo-output tdate 'SELL entry-short stop-short)
;                         (write-rjo-record rjo-output tdate 'BUY  entry-long stop-long))

;               (setq offset 1 ;(random-choice -1 0) set to 1 to guarantee house account exites after customers
;                     central-exit-time  (string-append (date-convert date1) " "
;                                                       (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

  ;             (write-oec-short foremost-starter-path3 oec-symbol *foremost-starter-block-acct* *foremost-starter-qty* entry-short stop-short central-entry-time
  ;                              central-cancel-time central-exit-time central-end-session-time oco-code)
  ;             (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
  ;             (write-oec-long foremost-starter-path3 oec-symbol *foremost-starter-block-acct* *foremost-starter-qty* entry-long stop-long central-entry-time
  ;                             central-cancel-time central-exit-time central-end-session-time oco-code)

  ;             (setq offset (random-choice -1 0)
  ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

   ;            (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries

  ;             (setq offset (random-choice -1 0)
  ;               pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
;
;               (write-oec-short daniels-starter-path3 oec-symbol *daniels-starter-block-acct* *daniels-starter-qty* entry-short stop-short pacific-entry-time
;                                pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
;               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
;               (write-oec-long daniels-starter-path3 oec-symbol *daniels-starter-block-acct* *daniels-starter-qty* entry-long stop-long pacific-entry-time
;                               pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

               (setq offset (random-choice -1 0)
                   central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

               (write-oec-short kingsview-starter-path3 oec-symbol *kingsview-starter-block-acct* *kingsview-starter-qty* entry-short stop-short central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)
               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
               (write-oec-long kingsview-starter-path3 oec-symbol *kingsview-starter-block-acct* *kingsview-starter-qty* entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)

  ;             (setq offset (random-choice -1 0)
  ;             central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)));;
;               (write-oec-short rjobrien-starter-path3 oec-symbol *rjobrien-starter-block-acct* *rjobrien-starter-qty* entry-short stop-short central-entry-time
 ;                               central-cancel-time central-exit-time central-end-session-time oco-code)
  ;             (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
   ;            (write-oec-long rjobrien-starter-path3 oec-symbol *rjobrien-starter-block-acct* *rjobrien-starter-qty* entry-long stop-long central-entry-time
   ;                            central-cancel-time central-exit-time central-end-session-time oco-code)

               (setq offset (random-choice -1 0)
                    central-exit-time  (string-append (date-convert date1) " "
                                                      (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

               (write-oec-short striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty* entry-short stop-short central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)
               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
               (write-oec-long striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty* entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)

               (write-oec-short sol-starter-path3 oec-symbol *sol-starter-block-acct* *sol-starter-qty* entry-short stop-short central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)
               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
               (write-oec-long sol-starter-path3 oec-symbol *sol-starter-block-acct* *sol-starter-qty* entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)

              (with-open-file (ninja-output ts-starter-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-starter-qty*))
              (with-open-file (ninja-output ts-starter-path2 :direction :output :if-exists :append :if-does-not-exist :create)
                 	     (write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short *striker-starter-qty*))
          
             );;;closes when *triumph11-list*
             ;(when (member *data-name* *entry-list*)
             ; (setq offset (random-choice -1 0)
             ;      central-exit-time  (string-append (date-convert date1) " "
             ;                                        (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

              ; (write-oec-short striker-entry-path3 oec-symbol *striker-entry-block-acct* *striker-entry-qty* entry-short stop-short central-entry-time
               ;                 central-cancel-time central-exit-time central-end-session-time oco-code)
              ; (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
              ; (write-oec-long striker-entry-path3 oec-symbol *striker-entry-block-acct* *striker-entry-qty* entry-long stop-long central-entry-time
               ;                central-cancel-time central-exit-time central-end-session-time oco-code)

               ; (with-open-file (ninja-output ninja-entry-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	      ;  	 (write-ninja-record ninja-output time-zone tdate date1 'SELL entry-short stop-short)
              ;   (write-ninja-record ninja-output time-zone tdate date1 'BUY  entry-long stop-long))
             
               );;;closes the clause "OK"
             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
              (when (member *data-name* *triumph21-list*)
               (setq offset (random-choice -1 0)
                 pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
;               (write-oec-long  cannon-score-path3 oec-symbol *cannon-score-block-acct* *cannon-score-qty* entry-long stop-long pacific-entry-time
;                                pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

;               (setq offset (random-choice -1 0)
;                 pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
;               (write-oec-long  daniels-score-path3 oec-symbol *daniels-score-block-acct* *daniels-score-qty* entry-long stop-long pacific-entry-time
;                                pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

 ;               (setq offset (random-choice -1 0)
 ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
 ;              (write-oec-long foremost-score-path3 oec-symbol *foremost-score-block-acct* *foremost-score-qty* entry-long stop-long central-entry-time
 ;                              central-cancel-time central-exit-time central-end-session-time oco-code)

;               (setq offset (random-choice -1 0)
;                   central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
;               (write-oec-long rjobrien-score-path3 oec-symbol *rjobrien-score-block-acct* *rjobrien-score-qty* entry-long stop-long central-entry-time
;                               central-cancel-time central-exit-time central-end-session-time oco-code)

               (setq offset (random-choice -1 0)
                   central-exit-time  (string-append (date-convert date1) " "
                                               (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
               (write-oec-long striker-score-path3 oec-symbol *striker-score-block-acct* *striker-score-qty* entry-long stop-long central-entry-time
                               central-cancel-time central-exit-time central-end-session-time oco-code)
               (with-open-file (ninja-output ts-score-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-score-qty*))

     	       (with-open-file (ninja-output ninja-score-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	     	     (write-ninja-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long))
               ) ;;closes the when score
;              (when (member *data-name* (set-difference *triumph21-list* *ice-list*))
;
;               (write-oec-long sol-score-path3 oec-symbol *sol-score-block-acct* *sol-score-qty* entry-long stop-long central-entry-time
;                               central-cancel-time central-exit-time central-end-session-time oco-code)
;                 
;                  );;closes the when
              (when (member *data-name* *triumph11-list*)
                 (setq offset (random-choice -1 0)
                       pacific-exit-time (string-append (date-convert date1) " " 
                                                        (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
    ;             (write-oec-long  cannon-starter-path3 oec-symbol *cannon-starter-block-acct* *cannon-starter-qty* entry-long stop-long pacific-entry-time
    ;                               pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

	      
 ;                (with-open-file (rjo-output rjo-starter-path3 :direction :output :if-exists :append :if-does-not-exist :create)
;		                    (write-rjo-record rjo-output tdate 'BUY entry-long stop-long))

 ;               (setq offset (random-choice -1 0)
 ;                  central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
 ;               (write-oec-long foremost-starter-path3 oec-symbol *foremost-starter-block-acct* *foremost-starter-qty* entry-long stop-long central-entry-time
 ;                               central-cancel-time central-exit-time central-end-session-time oco-code)

  ;              (setq offset (random-choice -1 0)
  ;                     pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
;
 ;               (write-oec-long daniels-starter-path3 oec-symbol *daniels-starter-block-acct* *daniels-starter-qty* entry-long stop-long pacific-entry-time
  ;                              pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

                (setq offset (random-choice -1 0)
                     central-exit-time  (string-append (date-convert date1) " "
                                                     (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

                (write-oec-long kingsview-starter-path3 oec-symbol *kingsview-starter-block-acct* *kingsview-starter-qty* 
                                entry-long stop-long central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)

  ;              (setq offset (random-choice -1 0)
  ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

  ;              (write-oec-long rjobrien-starter-path3 oec-symbol *rjobrien-starter-block-acct* *rjobrien-starter-qty* entry-long stop-long central-entry-time
  ;                              central-cancel-time central-exit-time central-end-session-time oco-code)

  ;              (setq offset (random-choice -1 0)
  ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

  ;              (write-oec-long apex-path3 oec-symbol *apex-starter-block-acct* *apex-starter-qty* entry-long stop-long central-entry-time
  ;                              central-cancel-time central-exit-time central-end-session-time oco-code)

                (setq offset (random-choice -1 0)
                   central-exit-time  (string-append (date-convert date1) " "
                                                     (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

                (write-oec-long striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty*
                                entry-long stop-long central-entry-time
                                central-cancel-time central-exit-time central-end-session-time oco-code)

                (with-open-file (ninja-output ts-starter-path2 :direction :output :if-exists :append :if-does-not-exist :create)
         	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-starter-qty*))

              );;;closes when member *triumph11-list*
;               (when (member *data-name* (set-difference *triumph11-list* *ice-list*))
;                (setq offset (random-choice -1 0)
;                   central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
;                (write-oec-long sol-starter-path3 oec-symbol *sol-starter-block-acct* *sol-starter-qty*
;                                entry-long stop-long central-entry-time
;                                central-cancel-time central-exit-time central-end-session-time oco-code)
   

 ;             );;;closess the  when
              );;;closes clause "not short"

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)

              (when (member *data-name* *triumph21-list*)
                 (setq offset (random-choice -1 0)
                       pacific-exit-time (string-append (date-convert date1) " "
                                                        (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
 ;                (write-oec-short  cannon-score-path3 oec-symbol *cannon-score-block-acct* *cannon-score-qty*
 ;                                  entry-short stop-short pacific-entry-time
 ;                                  pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

   ;              (setq offset (random-choice -1 0)
   ;                    pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T))
;                 (write-oec-short  daniels-score-path3 oec-symbol *daniels-score-block-acct* *daniels-score-qty* entry-short stop-short pacific-entry-time
 ;                                  pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

   ;              (setq offset (random-choice -1 0)
   ;                central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
   ;              (write-oec-short foremost-score-path3 oec-symbol *foremost-score-block-acct* *foremost-score-qty* entry-short stop-short central-entry-time
   ;                               central-cancel-time central-exit-time central-end-session-time oco-code)


  ;               (setq offset (random-choice -1 0)
  ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
  ;               (write-oec-short rjobrien-score-path3 oec-symbol *rjobrien-score-block-acct* *rjobrien-score-qty* entry-short stop-short central-entry-time
  ;                                central-cancel-time central-exit-time central-end-session-time oco-code)

                (setq offset (random-choice -1 0)
                   central-exit-time  (string-append (date-convert date1) " "
                                                      (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
                 (write-oec-short striker-score-path3 oec-symbol *striker-score-block-acct* *striker-score-qty* 
                                  entry-short stop-short central-entry-time
                                  central-cancel-time central-exit-time central-end-session-time oco-code)
                 (with-open-file (ninja-output ts-score-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output time-zone tdate date1 'SELL entry-short stop-short *striker-score-qty*))
               



                (with-open-file (ninja-output ninja-score-path3 :direction :output :if-exists :append :if-does-not-exist :create)
    	          	 (write-ninja-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
                 );;;closes the when score
         ;      (when (member *data-name* (set-difference *triumph21-list* *ice-list*));
;
 ;                (write-oec-short sol-score-path3 oec-symbol *sol-score-block-acct* *sol-score-qty* 
  ;                                entry-short stop-short central-entry-time
   ;                               central-cancel-time central-exit-time central-end-session-time oco-code)


;                  (setq offset (random-choice -1 0)
;                        central-exit-time  (string-append (date-convert date1) " "
;                                                          (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
;                  (write-oec-short striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty* entry-short stop-short central-entry-time
;                                   central-cancel-time central-exit-time central-end-session-time oco-code)

    ;            );; closes the when set-difference	            
              (when (member *data-name* *triumph11-list*)
                  (setq offset (random-choice -1 0)
                        pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))


;	         (with-open-file (rjo-output rjo-starter-path3 :direction :output :if-exists :append :if-does-not-exist :create)
;	            	 (write-rjo-record rjo-output tdate 'SELL entry-short stop-short))

  ;                (setq offset (random-choice -1 0)
  ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
  ;             (write-oec-short foremost-starter-path3 oec-symbol *foremost-starter-block-acct* *foremost-starter-qty* entry-short stop-short central-entry-time
  ;                                central-cancel-time central-exit-time central-end-session-time oco-code)

  ;                (setq offset (random-choice -1 0)
  ;                 central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))

   ;              (setq offset (random-choice -1 0)
   ;                   pacific-exit-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
   ;              (write-oec-short daniels-starter-path3 oec-symbol *daniels-starter-block-acct* *daniels-starter-qty* entry-short stop-short pacific-entry-time
   ;                               pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

                  (setq offset (random-choice -1 0)
                       central-exit-time  (string-append (date-convert date1) " "
                                                       (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
                  (write-oec-short kingsview-starter-path3 oec-symbol *kingsview-starter-block-acct* *kingsview-starter-qty*
                                   entry-short stop-short central-entry-time
                                   central-cancel-time central-exit-time central-end-session-time oco-code)

;                  (setq offset (random-choice -1 0)
;                        central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
;                  (write-oec-short rjobrien-starter-path3 oec-symbol *rjobrien-starter-block-acct* *rjobrien-starter-qty* entry-short stop-short central-entry-time
;                                   central-cancel-time central-exit-time central-end-session-time oco-code)

;                  (setq offset (random-choice -1 0)
;                        central-exit-time  (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
;                  (write-oec-short apex-path3 oec-symbol *apex-starter-block-acct* *apex-starter-qty* entry-short stop-short central-entry-time
;                                   central-cancel-time central-exit-time central-end-session-time oco-code)

                  (setq offset (random-choice -1 0)
                        central-exit-time  (string-append (date-convert date1) " "
                                                          (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
                  (write-oec-short striker-starter-path3 oec-symbol *striker-starter-block-acct* *striker-starter-qty* entry-short stop-short central-entry-time
                                   central-cancel-time central-exit-time central-end-session-time oco-code)

                 (with-open-file (ninja-output ts-starter-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-ts-record ninja-output time-zone tdate date1 'SELL entry-short stop-short *striker-starter-qty*))
               

               );;;closes the when *triumph11-list*
   ;          (when (member *data-name* (set-difference *triumph11-list* *ice-list*))
   ;               (setq offset (random-choice -1 0)
   ;                     central-exit-time  (string-append (date-convert date1) " "
   ;                                                       (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)))
   ;              (write-oec-short sol-starter-path3 oec-symbol *sol-starter-block-acct* *sol-starter-qty* entry-short stop-short central-entry-time
   ;                               central-cancel-time central-exit-time central-end-session-time oco-code)
   ;            )
             ;   (with-open-file (ninja-output ninja-entry-path3 :direction :output :if-exists :append :if-does-not-exist :create)
	      ;  	 (write-ninja-record ninja-output time-zone tdate date1 'SELL entry-short stop-short)
                


               ));;closes clause the cond


      )) ;;;closes the let and the defun





(defun build-daytrade-warehouse3 (markets &optional (nxdate 99999999))
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse3.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse3.backup"))
           (delete-file (string-append *upper-dir-warehouse* "daytradewarehouse3.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "daytradewarehouse3.dat"))
          (rename-file (string-append *upper-dir-warehouse* "daytradewarehouse3.dat")
                            "daytradewarehouse3.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "daytrades3.dat")) 
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


(defun add-day-trades3 (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "daytradewarehouse3.dat")) trades)

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



(defun find-most-recent-daytrade ()

   (let ((path (string-append *upper-dir-warehouse* "daytradewarehouse3.dat"))
           )
  (maind-x)(set-cat-list)(setq daytrades nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))

  (apply #'max (mapcar #'(lambda(s) (svref s 1)) daytrades)) 

))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;for day trades with currency futures

;;;
(defun update-currencies-warehouse3 (date &optional (markets *currencies-list*))
  
   
    (dolist (ith markets)
      (set-market ith)
       (populate-currencies-trades3 ith date (available-days ith date))) 
 

   (build-currencies-warehouse3 markets)   
   (setq *features-currencies3*  (find-best-indicator-set3c ))
  
   (portfolio-simulation3 '(currencies) date 3500 (list markets))
)

(defun populate-currencies-trades3 (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short  date-1
      entry-long entry-short  risk-long risk-short
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "currencytrades3.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

  (format outfile "~%~A~%" market)
;  (setq *ommission* 85 *max-day-risk* 2700)  ;;;1.0557)
 
  (setq *entry-factor* .764 *stop-loss-day* .944 *commission* 25 *max-day-risk* 1900)  ;;;1.0557)
  (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))

   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))


 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
   (multiple-value-setq (stop-long stop-short)(vprices date 4 *stop-loss-day* 1))

   (setq risk-long (- entry-long stop-long) risk-short (- stop-short entry-short))
 ;  (setq risk-long  (adverse-excursion date 4 1.8) risk-short risk-long)
 ;  (setq risk-long  (volatility date 4 .764) risk-short risk-long)

   (setq   date-1 date date (add-mkt-days date 1))

  ; (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))))
  ; (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))))

   

;;;enter on a stop order if opens below entry-stop.
;;;enter on a limit order of open above the entry-stop
;;;exit at the close

   (when (and (<= (getd date 'low) entry-short) ;(- (getd date 'open) risk-short))
              (> (getd date 'high) entry-short)
              (>= (* risk-short (index-point-value)) 75) ;;to avoid bad data
	      (<= (* risk-short (index-point-value)) *max-day-risk*)
              )
          (setq short entry-short ;(- (getd date 'open) risk-short)
                ;stop-short (+ short (* 1.6 risk-short))
                short-trade (create-currencies-entry-record-list3 date-1 -1 short)
                 ))

    (when (and
               (>= (getd date 'high) entry-long) ;(+ (getd date 'open) risk-long))
               (< (getd date 'low) entry-long)
               (>= (* risk-long (index-point-value)) 75)
	       (<= (* risk-long  (index-point-value)) *max-day-risk*)
               )
           (setq long entry-long ;(+ (getd date 'open) risk-long)
                 ;stop-long (- long (* 1.6 risk-long))
                 long-trade (create-currencies-entry-record-list3 date-1 1 long)

                 )) ;(format T "long-trade= ~A ~%" long-trade) )


 ;;;check if stopped out on same day

   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
             ; (<= (getd date 'low) stop-long)
               )
           (push (- (* (- stop-long long)(calculate-point-value date))(comm+slip date)) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date) (- stop-long long)))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
                (or (and (>= (getd date 'high) stop-short)
                         (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
             ;(>= (getd date 'high) stop-short)      
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
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
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
       (round  (or (min* losers) 0))
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


(defun build-currencies-warehouse3 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "currencieswarehouse3.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "currencieswarehouse3.backup"))
           (delete-file (string-append *upper-dir-warehouse* "currencieswarehouse3.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "currencieswarehouse3.dat"))
          (rename-file (string-append *upper-dir-warehouse* "currencieswarehouse3.dat")
                            "currencieswarehouse3.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "currencytrades3.dat")) 
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


;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-currencies-entry-record-list3 (date direction entry)
   (let* ((date-1 (getd date 'ydate)))

       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                   ;          (my-round (/ (volatility date 4 1) (volatility date 63 1)) 3) ;;;feature 2
                              (volatility-ratio-index date 4 63 1);;;feature 2
                              (rsi2x-index date 2) ;;feature 3 5 levels
                       ;       (round (slow-stochastic date 21))  ;feature 4
                              (slow-stochastic-index date 21);;feature 4
                              (day-bar-type2 date) ;feature 5  10 levels
                              (ep-macd-index date 12 26 9) ;;;feature 6  5 levels
                              (trend-signal date 45) ;feature 7
                      ;        (gann-slope-index date 5 63);;feature 7  5 levels
                              (pivot-index date 1) ;feature 8  6 levels
                              (range-index date) ;; feature 9 6 levels
                              (2-bar-index date);;;feature 10  5 levels
                              (day-bar-type2 date-1) ;feature 11  5 levels
                           ;   (daytrade-reward-risk date direction) ;;feature 12
                              (reflect3 date 3)      ;;  feature 12  5 levels
                            ;  (mo-diver date 45) ;;feature 13
                              (range-index1 date 5) ;feature 13  6  levels                              
                              (pivot-index date-1 1) ;;features 14 6 levels
                              );;;closes the list

))



;;;use same features as voltest2 *day-features1*
(defun currencies-simulation-test3 (market date2 num &optional (features nil))
 (let (date trades long short cover-long cover-short entry-long entry-short record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk 
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  bin draw ignore singles;draw-ave draw-90
      
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "currencies-summary3.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "currencies-simulation3.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "currencies-diary3.dat")))
   ;(declare (ignore long-acc short-acc))
   (set-market market)

    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

   (format T "~%~A~%" *data-name*)
   (setq *entry-factor* .764 *stop-loss-day* .944 *commission* 25 *max-day-risk* 1900)  
  
    (multiple-value-setq (ignore ignore ignore singles)
             (apply #'currencies-bins3b features))

   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

; (format T "Market= ~A date=~A~%" *data-name* date)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
   (multiple-value-setq (stop-long stop-short)(vprices date 4 *stop-loss-day* 1))

   (setq risk (* .5 (- stop-short stop-long)))

   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover)))

    )

;;;;static stop loss
    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))
;;;trailing stop loss
;    (setq stop-long (- (getd date 'high) risk) stop-short (+ (getd date 'low) risk))

;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
 ;                  (bin-classifier-daytrades3 date-1 features))
                  (bin-classifier-currencies3b date-1 features))
    
        ;(setf (nth 0 bin) 1)
    
       )

;;;check if new entry

   (when (and (> (getd date 'high) entry-short)
              (<= (getd date 'low) entry-short)
              (member epsignal '(OK DOWN))(> shorts 0.0)
              (>= (/ short-gains shorts) 75.0)
	      (>= (* risk (calculate-point-value date-1)) 75)
              (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
                 )
          (setq short entry-short
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)
           (setf (svref record 3) (- (svref record 3) (/ *commission* (index-point-value))))
                 )


    (when (and (< (getd date 'low) entry-long)
               (>= (getd date 'high) entry-long)
               (member epsignal '(OK UP))(> longs 0.0)
               (>= (/ long-gains longs) 75.0) 
               (>= (* risk (calculate-point-value date-1)) 75)
	       (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
             )

           (setq long entry-long
                 long-trade (list date 'long long)
                 )
           (setf (svref record 2) 1)
           (setf (svref record 3) (- (svref record 3) (/ *commission* (index-point-value))))
                 )


 ;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
      ;      (<= (getd date 'low) stop-long)
           )
           (push (- stop-long long) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                     (contract-month *data-name* date) 'S
                                                                  stop-long (my-pretty-price (- stop-long long))))))
           (push long-trade extended-trades)
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
            ;   (>= (getd date 'high) stop-short)
                 )
           (push (- short stop-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
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
            (push (- cover-long long) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                       (contract-month *data-name* date) 'N
                                                                   cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3) (- (getd date 'close) long)))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                       (contract-month *data-name* date) 'N
                                                                  cover-short (my-pretty-price (- short cover-short))))))
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
           (svref ith 0) (svref ith 1) (svref ith 2) (round (* (index-point-value)(svref ith 3)))
           (round (* (index-point-value) (svref ith 4))))
     ))

   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
;   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
    (format str "STOP LOSS FACTOR= ~5,3,0,'*,' F  SINGLE RATIO = ~A~%" *stop-loss-day* singles)
      (format str "P/L= ~16D  NUMBER TRADES=  ~6D    ACCURACY=        ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D    AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D    LARGEST GAIN= ~7D~%~
        PROFIT FACTOR=   ~,2,0,'*,' F  %DAYS IN TRADE=    ~F    COMMISSION=        ~D~%~
        DRAWDOWN= ~11D  $/contract= ~10D    STOPPED OUT=    ~,1,2,'*,' F%"

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
      (my-round (* 100 (/ (length trades) num)) 1) *commission*
      (setq draw (round (* (drawdown trades)(or (index-point-value) 1))))
       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       (/ (count 'S (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1))
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))

     (format str "~%AVE ANNUAL RETURN= ~5,1,2,'*,' F%"
           (/ (/ (* (list-sum trades) (or (index-point-value) 1))
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) )
  ;   (setq trades1 trades)

 ;      (multiple-value-setq (draw-ave draw-90)(monte-carlo-drawdown trades))
 ;      (setq draw-ave (* draw-ave (index-point-value)) draw-90 (* draw-90 (index-point-value)))
 ;     (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A" draw-ave draw-90) 

   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

     (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (index-point-value) (svref ith 8))))
        ));;;closes the with-open-file
  );;;closes the when outfile

  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ))



(defun encode-currencies-trades3 (record features)
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
;;;Feature 6 is mo-div date 5 with 5 levels
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


(defun currencies-bins3b ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "currencieswarehouse3.dat"))
    (maind-x)(set-cat-list)
    (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))


;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record daytrades)
     (dotimes (ith 4)
         (setq bin (encode-currencies-trades3 record (butlast features ith)))
         
          (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
            ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*) (list record))
              (push bin day-bin-codes)))
          ));;;closes the doltimes and dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit3b daytrades features)


  ))



(defun currencies-bins3a ( &rest features)
  (let (bin path)

;      (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades3.dat"))
      (setq path (string-append *upper-dir-warehouse*  "currencieswarehouse3.dat"))
  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record daytrades)))


   (dolist (record daytrades)
     (setq bin (encode-currencies-trades3 record features))

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
;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
;;;uses the composite warehouse
(defun currencies-add-one-in3a (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'currencies-bins3a (append base-features (list ith))))
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
(defun currencies-leave-one-out3a (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 

  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'currencies-bins3a (remove ith base-features)))
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

(defun find-best-indicator-set3c ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)
        ; (pathout (string-append *config-dir* "day-features3.dat")))


    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (currencies-add-one-in3a base-list candidate-list))

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
  
    (setq winners-list (currencies-leave-one-out3a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
      base-list
    ))
 

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



;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-currencies3b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.20)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-currencies-entry-record-list3 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-currencies-trades3 record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (<= nxdate (svref s1 1)))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
 ;   (format T "~%contents = ~A" contents)
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

#|

;;;;;;;;;;;;;;;;;SWING TRADES FUNCTIONS with vectors
;;;;and improved objective exit
(defun update-swingtrade-warehouse1 (date &optional (markets  *swing-warehouse-list* ))
  
    (maind-x)(set-cat-list)
    (dolist (ith markets)
      (set-market ith)
       (populate-swing-trades1 ith date (available-days ith date))) 
 
    (build-swingtrade-warehouse1 markets) 
    (setq *swing-features1* (find-best-indicator-set1a )) 

   (portfolio-simulation3 '(swing) date 3330 (list markets))
)


(defun populate-swing-trades1 (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long rsi-signal entry-short entry-long
       ave-win ave-loss losers winners extended-trades trade-short prev-stop-long
       prev-stop-short 
       date-1 risk  cover-long cover-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "swingtrades1.dat")))
   
   (maind-x)(set-cat-list)(set-market market)
  (format T  "~%~A~%" market)
  (setq  *max-swing-risk* 2500)  ;;;1.0557)
  (setq *stop-loss-swing* 1.333)

   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

  (multiple-value-setq (entry-short entry-long) (vprices date 3 *entry-factor-swing* 1))
;  (multiple-value-setq (prev-entry-short prev-entry-long) (vprices date-1 4 *entry-factor-swing* 1))

  (setq risk (volatility date 4 *stop-loss-swing*))
                
  (setq  stop-short (+ (getd date 'close) risk) stop-long (- (getd date 'close) risk))
 ; (format T "~% DATE= ~A ENTRY-LONG= ~A  ENTRY-SHORT= ~A RISK= ~A" date entry-long entry-short risk)  

   
 
   (setq  date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))))
  
   (when long (setq stop-long (fmax prev-stop-long stop-long) prev-stop-long stop-long))
   (when short (setq stop-short (fmin prev-stop-short stop-short) prev-stop-short stop-short))


;;;check if met exit criteria to exit at end of previous day 
     (when (and long (eql rsi-signal 'DN) );(< (getd date-1 'high) prev-entry-long)) 
           (setq cover-long (getd date 'open))
           (push  (- cover-long long) trades)
           (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date) (- cover-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil prev-stop-long nil))

      (when (and short (eql rsi-signal 'UP));(> (getd date-1 'low) prev-entry-short))
            (setq cover-short (getd date 'open))
            (push (- short cover-short) trades)
            (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date) (- short cover-short)))))))
            (push trade-short extended-trades)
            (setq trade-short nil short nil  prev-stop-short nil ))
  
#|;;;check if stopped out
   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (min stop-long (getd date 'open)))
                       (round (* (index-point-value) (- (min stop-long (getd date 'open)) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil  prev-stop-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price (max stop-short (getd date 'open)))
                 (round (* (index-point-value) (- short (max stop-short (getd date 'open)))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil  prev-stop-short nil))
|#
  
;;;check if new entry
;  (format T "~%2date= ~A prev-signal= ~A stop-loss-price=  ~A entry-short= ~A" date prev-signal stop-loss-price entry-short)  
    (when (and (not short)
               (>= (* risk (calculate-point-value date-1)) 20) ;;to avoid bad data
	       (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
              ; (<= (getd date 'low) entry-short)
               (eql rsi-signal 'DN)
               )
           ;  (format T "~%6date= ~A short= ~A date-1= ~A stop-short= ~A" date short date-1 stop-short)
             (setq short (getd date 'open) stop-short (+ short risk)
                 trade-short (create-swing-entry-record-list1 date-1 -1 short)
                  prev-stop-short stop-short
         ) ;(format T "~%4date= ~A short= ~A date-1= ~A stop-short= ~A" date short date-1 stop-short)
         )


    (when (and (not long)
               (>= (* risk (calculate-point-value date-1)) 20) ;;to avoid bad data
	       (<= (* risk (calculate-point-value date-1)) *max-day-risk*)
            ;   (>= (getd date 'high) entry-long)
               (eql rsi-signal 'UP)
               )
          ; (format T "~%6date= ~A long= ~A date-1= ~A stop-long= ~A entry-short= ~A" date long date-1 stop-long entry-long)
           (setq long  (getd date 'open) stop-long (- long risk)
                 trade-long (create-swing-entry-record-list1 date-1 1 long)
                 prev-stop-long stop-long
              ) ;(format T "~%5date= ~A long= ~A date-1= ~A stop-long= ~A" date long date-1 stop-long)
              )

  ;  (format T "~%3date= ~A prev-signal= ~A stop-loss-price=  ~A" date prev-signal stop-loss-price)  
 ;;;check if stopped out on same day of entry
   (when (and long (<= (getd date 'low) stop-long))
            ; (or (and (> (getd date 'open)(getd date 'close))
            ;    (<= (getd date 'close) stop-long)))
           (push (- stop-long long) trades)
           (setq trade-long (apply #'vector
                 (append trade-long
                             (list date (my-pretty-price stop-long) (round (* (index-point-value)(- stop-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil prev-stop-long nil))


    (when  (and short (>= (getd date 'high) stop-short))
               ;(or (and (< (getd date 'open)(getd date 'close))
               ;                (>= (getd date 'high) stop-short))
               ;            (>= (getd date 'close) stop-short)))
           (push (- short stop-short) trades)
           (setq trade-short (apply #'vector
                 (append trade-short
                         (list date (my-pretty-price stop-short) (round (* (index-point-value)(- short stop-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil prev-stop-short nil))


   );;;closes the dotimes


  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format output "~%~A~%" *data-name*)
     (format output "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
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
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~S~%" ith)

    ));;closes the dolist and with-open-file
   ); closes the when


    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun


;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-swing-entry-record-list1 (date direction entry)
   (let* ((date-1 (getd date 'ydate)))
       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                   ;          (my-round (/ (volatility date 4 1) (volatility date 63 1)) 3) ;;;feature 2
                              (volatility-ratio-index date 4 63 1);;;feature 2
                              (ep-roc-change-index date 3 10) ;;feature 3 10 levels
                       ;       (round (slow-stochastic date 21))  ;feature 4
                              (rsi-cross date 14 3);;feature 4
                              (bar-pattern-low-index-1 date) ;feature 5  7 levels
                              (cloud-index date) ;;;feature 6  8 levels
                              (rsi-cross date 9 3) ;feature 7
                      ;        (gann-slope-index date 5 63);;feature 7  5 levels
                              (tkcd-index date ) ;feature 8  6 levels
                              (day-bar-type date-1) ;; feature 9 6 levels
                              (macd-index1 date 6 15 3 );;;feature 10  5 levels
                              (ep-roc-change-index1 date 10 20) ;feature 11  5 levels
                           ;   (daytrade-reward-risk date direction) ;;feature 12
                              (volatility-ratio-index date 1 14 1);;;feature 12 
                           ;  (mo-diver date 45) ;;feature 13
                              (bar-pattern-high-index-1 date) ;feature 13  6  levels                              
                              (gann-slope-index date 21 252) ;;features 14 8 levels
                              );;;closes the list

));;;closes the defun


(defun encode-swing-trades1 (record features)
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
;;;Feature 6 is mo-div date 5 with 5 levels
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



(defun swing-trade-bins1a (&rest features)
  (let (bin path)
   (setq path (string-append *upper-dir-warehouse*  "swingtradewarehouse1.dat"))
  (maind-x)(set-cat-list)
  (setq swings nil bin-codes nil)(clrhash *swing-trade-warehouse1*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))

;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swings);;;record is a vector and bin is a list
     (setq bin (encode-swing-trades1 record features))
   ;  (pushnew bin bin-codes :test #'equalp)
     (cond ((gethash bin *swing-trade-warehouse1*)
            (ifn (member record (gethash bin *swing-trade-warehouse1*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse1*)
                       (cons record (gethash bin *swing-trade-warehouse1*)))))
           ((not (gethash bin *swing-trade-warehouse1*))
             (setf (gethash bin *swing-trade-warehouse1*)(list record))
             (push bin bin-codes)))
      );;closes the dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swing-bins-by-profit1 swings)


  ))

(defun swing-trade-bins1b ( &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "swingtradewarehouse1.dat"))
    (maind-x)(set-cat-list)
    (setq swings nil bin-codes nil)(clrhash *swing-trade-warehouse1*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))


;;;trads are stored without commission deducted
;;;
;    (dolist (record daytrades)
;     (setf (svref record 19) (- (svref record 19) *commission*)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swings)
     (dotimes (ith 4)
         (setq bin (encode-swing-trades1 record (butlast features ith)))
         
          (cond ((gethash bin *swing-trade-warehouse1*)
            (ifn (member record (gethash bin *swing-trade-warehouse1*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse1*)
                       (cons record (gethash bin *swing-trade-warehouse1*)))))
            ((not (gethash bin *swing-trade-warehouse1*))
             (setf (gethash bin *swing-trade-warehouse1*) (list record))
              (push bin bin-codes)))
          ));;;closes the doltimes and dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swing-bins-by-profit1b swings features)


  ))

(defun rank-swing-bins-by-profit1b (swings features)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)(twr 1)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0)
        longest-bin-codes (longest-feature (length features)))
   (setq longest-bin-codes (remove-if #'(lambda (s) (< (length s) longest-feature)) bin-codes))
   (dolist (ith longest-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse1*))
     (setq result 0 counter 0 twr 1)

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
         (setq twr (* twr (+ 1 (/ (if (plusp (svref kth 2))
                                 (- (svref kth 18) (svref kth 3))
                               (- (svref kth 3) (svref kth 18)))
                             (svref kth 3)))))

         ) ;;;closes dolist over contents
      (if (and (plusp result)(> twr 1)) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))

     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth swings)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swings)(length (num-markets-in-warehouse3 swings)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length longest-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swings)(length longest-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length longest-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length swings)) 2))
     (format T "CHI SQUARE PROB = ~7,5F~%" (my-round (swingtrade-chi-squared-gof) 5))

    (values (round winners)(round (+ all-winners all-losers))(if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
           (my-round (/ only-one (length swings)) 2)) 
       
 ))


(defun find-best-indicator-set1a ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)
        ; (pathout (string-append *config-dir* "day-features3.dat")))


    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (swingtrades-add-one-in1a base-list candidate-list))

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

      (setq winners-list (swingtrades-leave-one-out1a base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
      base-list
    ))
 
;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
;;;uses the composite warehouse
(defun swingtrades-add-one-in1a (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'swing-trade-bins1a (append base-features (list ith))))
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
(defun swingtrades-leave-one-out1a (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 

  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'swing-trade-bins1a (remove ith base-features)))
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

(defun rank-swing-bins-by-profit1 (swings)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)(twr 1.0d0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse1*))
     (setq result 0 counter 0 twr 1.0)

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
         (setq twr (* twr (+ 1.0 (/ (if (plusp (svref kth 2))
                                 (- (svref kth 18) (svref kth 3))
                               (- (svref kth 3) (svref kth 18)))
                             (svref kth 3)))))

         ) ;;;closes dolist over contents
      (if (and (plusp result)(> twr 1)) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))

     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth swings)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1));;;a winning bin must have twr > 1.0 and be positive profit.
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swings)(length (num-markets-in-warehouse3 swings)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swings)(length bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%" (my-round (/ only-one (length bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%" (my-round (/ only-one (length swings)) 2))
     (format T "CHI SQUARE PROB = ~7,5F~%" (my-round (swingtrade-chi-squared-gof) 5))

    (values (round winners)(round (+ all-winners all-losers))(if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
           (my-round (/ only-one (length swings)) 2)(my-round (/ winners all-winners) 2) (swingtrade-chi-squared-gof)) 
       
 ))


;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun swing-simulation-test1 (market date2 num &optional (features *swing-features1*))

 (let (date stop-long stop-short trades long short trade-long ; entry-long entry-short 
       risk cover-long cover-short
       ave-win ave-loss losers winners extended-trades trade-short 
       epsignal  longs long-gains bin long-acc
        trading-dates (trade-time 0) ;prev-entry-long prev-entry-short
       shorts short-gains  short-acc
        prev-stop-long prev-stop-short  date-1 record (running-sum 0)  draw
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "swing-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "swing-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "swing-diary1.dat"))
        ); epsignal1)

    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

         (setq *stop-loss-swing* 1.0557  ;.6667
               *max-swing-risk* 2500)

   (apply #'swing-trade-bins1b features) 

    (set-market market)
   (format T "~%~A~%" *data-name*)
   
    (setq date (add-mkt-days date2 (- num)) date-1 (getd date 'ydate))
    (setq record (list date (getd date 'close) 0 0 0))
    (push record trading-dates)
 ;;;;from date1 to date2
 (dotimes (ith num)

  ; (multiple-value-setq (entry-short entry-long) (vprices date 3 *entry-factor-swing* 3))

   (setq risk (min (volatility date 4 *stop-loss-swing*)(/ *max-swing-risk* (calculate-point-value date))))
   (setq stop-short (+ (getd date 'close) risk) stop-long (- (getd date 'close) risk)) 
 
;;;this is a black swan objective
 ;  (setq ave4 (gethash date AT)
  ;     cover-short  (my-pretty-price (exp (- (log ave4) (* *objective-factor-swing* (gethash date VT)))))
   ;    cover-long (my-pretty-price (exp (+ (log ave4) (* *objective-factor-swing* (gethash date VT))))))

   (setq date-1 date date (add-mkt-days date 1))
   
 (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1))

;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (nth 3 record)(- (getd date-1 'close)(getd date 'close))))

    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	  ; entry-long (+ entry-long (getd date 'rollover))
    		   stop-long (+ stop-long (getd date 'rollover)))
             (setf (nth 3 record)(- (nth 3 record) (getd date 'rollover)))
)
    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    		;  entry-short (+ entry-short (getd date 'rollover))
    		  stop-short (+ stop-short (getd date 'rollover)))
            (setf (nth 3 record) (+ (nth 3 record) (getd date 'rollover)))
)

   (when (and stop-long long) (setq stop-long (fmax prev-stop-long stop-long) prev-stop-long stop-long))
   (when (and stop-short short) (setq stop-short (fmin prev-stop-short stop-short) prev-stop-short stop-short))

;;;;calculate bin-classifier 
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-swings1b date-1 features))
       (setf (nth 0 bin) 1)
       

; (format T "~%Date-1 = ~A  EPSIGNAL = ~A LONG = ~A SHORT = ~A STOP-LONG = ~A STOP-SHORT = ~A~%"
 ;           date-1 epsignal long short stop-long stop-short)
; (format T "~%OPEN = ~A~%" (getd date 'open))

 ;;;check if met criteria
      (when (and long ;(or ;(< (getd date-1 'high) prev-entry-long)))
                 (neql epsignal 'UP))
            (setq cover-long (getd date 'open))
            (push (- cover-long long) trades)
            (setq trade-long
                 (apply #'vector (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                          (contract-month *data-name* date) 'C
                                                          cover-long (my-pretty-price (- cover-long long))))))    
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
            (setq trade-long nil long nil stop-long nil prev-stop-long nil))
      
         (when (and short ;(or ;(> (getd date-1 'low) prev-entry-short)))
                    (neql epsignal 'DOWN))
            (setq cover-short (getd date 'open))
            (push (- short cover-short) trades)
            (setq trade-short (apply #'vector (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                                       (contract-month *data-name* date) 'C
                                                                      cover-short (my-pretty-price (- short cover-short))))))         
            (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (+ (nth 3 record) (- short cover-short)))
            (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
            (setq trade-short nil short nil stop-short nil prev-stop-short nil))

#|
;;;;check if stopped out of prior position
   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long
                (apply #'vector (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                           (min stop-long (getd date 'open))
                              (my-pretty-price (- (min stop-long (getd date 'open)) long))))))    
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
          (setq trade-long nil long nil epsignal nil))

   (when (and short (>= (getd date 'high) stop-short))
         (push (- short (max stop-short (getd date 'open))) trades)
         (setq trade-short 
              (apply #'vector (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                         (contract-month *data-name* date) 'S
                                                         (max stop-short (getd date 'open))
                      (my-pretty-price (- short (max stop-short (getd date 'open))))))))
  
          (push trade-short extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade-short nil short nil epsignal nil))

  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))
            (push (- (max (getd date 'open) cover-long) long) trades)
            (setq trade-long (apply #'vector
               (append trade-long
                  (list date 'exit (max (getd date 'open) cover-long) (- (max (getd date 'open) cover-long) long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (- (max (getd date 'open) cover-long) (getd date-1 'close)))
            (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
            (setq trade-long nil long nil stop-long nil))

      (when (and short (< (getd date 'low) cover-short))
            (push (- short (min (getd date 'open) cover-short)) trades)
            (setq trade-short (apply #'vector
               (append trade-short (list date 'exit cover-short (- short (min (getd date 'open) cover-short))))))
            (push trade-short extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (- (getd date-1 'close) (min (getd date 'open) cover-short)))
            (if (getd date 'rollover)(setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
            (setq trade-short nil short nil stop-short nil))
|#

;;;check if new entry

    (when (and (not short)
              ; (<= (getd date 'low) entry-short)
               (eql epsignal 'DOWN)
               (> shorts 0.0)  (> twr-short 1.0)
              (>= (/ short-gains shorts)
                  (+ *min-swing-expected-value* (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size))))             
              (>= (* risk (calculate-point-value date-1)) 20)
              (<= (* risk (calculate-point-value date-1)) *max-swing-risk*)
              (>= (/ short-gains shorts)
                  (if (> longs 0.0) (/ long-gains longs) 0))
                 )
          (setq short (getd date 'open);(min entry-short (getd date 'open))
                 trade-short (list date 'short short) stop-short (+ short risk)
                 prev-stop-short stop-short)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(/ *commission* (index-point-value)))))

              )

    (when (and (not long)
              ; (>= (getd date 'high) entry-long)
               (eql epsignal  'UP)
               (> longs 0.0) 
               (>= (* risk (calculate-point-value date-1)) 20)
               (>= (/ long-gains longs)
                   (+  *min-swing-expected-value* (* 2 (calculate-point-value date-1) *pips-slippage* (index-tick-size))))
	       (<= (* risk (calculate-point-value date-1)) *max-swing-risk*)
               (> (/ long-gains longs)
                 (if (> shorts 0.0)(/ short-gains shorts) 0))
                )
           (setq long (getd date 'open) ;(max entry-long (getd date 'open))
                 trade-long (list date 'long long) stop-long (- long risk)
                 prev-stop-long stop-long)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (- (getd date 'close) long (/ *commission* (index-point-value)))))

             )


 ;;;check if stopped out on same day of entry
   (when (and long stop-long (<= (getd date 'low) stop-long))
             ;(or (and (> (getd date 'open)(getd date 'close))
             ;                         (<= (getd date 'low) stop-long))
             ;                    (<= (getd date 'close) stop-long)))
          (push (- stop-long long) trades)
          (setq trade-long
                (apply #'vector (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                         (contract-month *data-name* date) 'S
                                                         stop-long (my-pretty-price (- stop-long long))))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil prev-stop-long nil))



    (when  (and short stop-short (>= (getd date 'high) stop-short))
                ;(or  (and (< (getd date 'open)(getd date 'close))
                ;                           (>= (getd date 'high) stop-short))
                ;                      (>= (getd date 'close) stop-short)))
           (push (- short stop-short) trades)
           (setq trade-short
                 (apply #'vector (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                                           (contract-month *data-name* date) 'S
                                                           stop-short (my-pretty-price (- short stop-short))))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil prev-stop-short nil))


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
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

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

 ;  (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

;    (dolist (ith extended-trades);
;
;    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
;         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)
;          (round (* (index-point-value) (svref ith 6))))
;    ));;closes the dolist and with-open-file

   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

     (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)
         (svref ith 6)(svref ith 7)(svref ith 8)(round (* (index-point-value) (svref ith 8))))
        ));;;closes the with-open-file


   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun



;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse
(defun bin-classifier-swings1a (date features)
  (let (record bin (result 0) (counter 0) contents epsignal nxdate (crit-acc .3)(crit-pf 1.25)
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0)
         (gains-long 0)(gains-short 0)   (losses-short 0)(losses-long 0))
 
    (setq nxdate (getd date 'ndate))
    (setq record (create-swing-entry-record-list1 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

    (setq bin (encode-swing-trades1 record features))
    (setq contents (gethash bin *swing-trade-warehouse1*))
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *swing-trade-warehouse1*) contents))

   (setq contents
    (remove-if #'(lambda(s1) (and (eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
   (dolist (kth contents)

     (when (eql (svref kth 2) 1)
      (incf longs) (setq results-long (+ results-long (svref kth 19))))

    (when (and (eql (svref kth 2) 1)(plusp (svref kth 19)))
       (incf num-winners-long))
    (when (and (eql (svref kth 2) 1)(not (plusp (svref kth 19))))
          (setq losses-long (+ losses-long (svref kth 19))))

     (when (eql (svref kth 2) -1)
      (incf shorts)(setq results-short (+ results-short (svref kth 19))))

     (when (and (eql (svref kth 2) -1)(plusp (svref kth 19)))
        (incf num-winners-short))
     (when (and (eql (svref kth 2) -1)(not (plusp (svref kth 19))))
          (setq losses-short (+ losses-short (svref kth 19))))

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

;  (cond ((and (plusp results-long)(plusp results-short)) (setq epsignal 'OK))
;        ((plusp results-long) (setq epsignal 'UP))
;        ((plusp results-short) (setq epsignal 'DOWN))
;        ((and (<= results-long 0)(<= results-short 0)) (setq epsignal 'AVOID))
;        ((not contents) (setq epsignal 'UNIQUE)))

  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-swings1b (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .3)(crit-pf 1.25)
        (gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-swing-entry-record-list1 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
    (dotimes (ith 4)
    (setq bin (encode-swing-trades1 record features))
    (setq contents (gethash bin *swing-trade-warehouse1*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *swing-trade-warehouse1*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
;         (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
     (if contents (return) (setq features (butlast features)))
  );;;closes the dotimes
 ;   (format T "~%contents = ~A" contents)
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
          ;    (>= (float (/ num-winners-long longs)) crit-acc)(>= (float (/ num-winners-short shorts)) crit-acc))
          ;    (setq epsignal 'OK))
        ((and (plusp results-long)
             (> (/ results-long (if (zerop longs) 1 longs))(/ results-short (if (zerop shorts) 1 shorts))) 
             (> (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))) crit-pf)
              (>= (float (/ num-winners-long longs)) crit-acc)) (setq epsignal 'UP))
        ((and (plusp results-short)
              (> (/ results-short (if (zerop shorts) 1 shorts))(/ results-long (if (zerop longs) 1 longs)))
              (> (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short)))) crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc))(setq epsignal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE))
        (t (setq epsignal 'AVOID)))

    (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin)
))


;;;calculates the twr for a bin
;;;must run #'swing-trade-bins3 before running this function
(defun swing-bin-twr1 (bin)
  (let ((twr 1))

   (dolist (record (gethash bin *swing-trade-warehouse1*))
     (setq twr (* twr (+ 1 (/ (if (plusp (svref record 2))
                                 (- (svref record 18) (svref record 3))
                               (- (svref record 3) (svref record 18)))
                             (svref record 3)))
                           )))
  twr
))


(defun build-swingtrade-warehouse1 (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "swingtradewarehouse1.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "swingtradewarehouse1.backup"))
           (delete-file (string-append *upper-dir-warehouse* "swingtradewarehouse1.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "swingtradewarehouse1.dat"))
          (rename-file (string-append *upper-dir-warehouse* "swingtradewarehouse1.dat")
                            "swingtradewarehouse1.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "swingtrades1.dat")) 
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
(defun add-swing-trades1 (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "swingtradewarehouse1.dat")) trades)

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


(defun find-best-swingtrade-entry1 (tdate  &optional (markets *swing-warehouse-list*))
  (let ((best-param 2.5) (results 0) (total-trades 0) (prev-result -20000000)
         scores1 scores2 trades result)

  (do ((param best-param  (+ param .10)))
      ((> param 3.6) best-param)

      (setq *entry-factor-swing* (my-round param 2))
      (format T "~%TRYING ENTRY= ~A~%" param)(setq results 0 total-trades 0)
      (dolist (ith markets)
        (format T "~%MARKET= ~A~%" ith)
        
        (multiple-value-setq (result trades) (populate-swing-trades1 ith tdate (available-days ith tdate)))
        (setq results (+ results result) total-trades (+ total-trades trades)));;closes the dolist

      (setq scores1 (acons param results scores1))(print scores1)
      (setq scores2 (acons param (round (/ results total-trades)) scores2))(print scores2)

      (if (> results prev-result) (setq prev-result results best-param param))
      (format T "~%BEST ENTRY SO FAR= ~A" best-param));;;closes the do


      ))


;;;;;;;;;;;;;;;;;POSITION TRADES FUNCTIONS with vectors
;;;;and improved objective exit

(defun populate-position-trades3 (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long  entry-short entry-long
       ave-win ave-loss losers winners extended-trades trade-short prev-signal ctr date-1
       (path1 (string-append *upper-dir-warehouse*  (format nil "~S" market) "positiontrades3.dat")))
 ;   (declare (ignore ctr))
   (maind-x)(set-cat-list)(set-market market)


   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long) (vprices date 18 2.53 7))
   (multiple-value-setq (prev-signal ctr)(vsignals date 18 2.53 7))

   (setq  date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))))

  ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long entry-short)))
   (when short (setq stop-short (min stop-short entry-long)))


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

;;;check if new entry

    (when (and (not short)
               (<= (getd date 'low) entry-short)
               (eql prev-signal 'buy)
               )
          (setq short (min entry-short (getd date 'open))
                 trade-short (create-position-entry-record-list1 date-1 -1 short)
                 stop-short entry-long
         ))


    (when (and (not long)
               (>= (getd date 'high) entry-long)
               (eql prev-signal 'sell)
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (create-position-entry-record-list1 date-1 1 long)
                 stop-long entry-short
              ))


 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (apply #'vector
                 (append trade-long
                             (list date (my-pretty-price stop-long) (round (* (index-point-value)(- stop-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))


    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (apply #'vector
                 (append trade-long
                     (list date (my-pretty-price stop-long) (round (* (index-point-value)(- stop-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))


    (when  (and short (or (< (getd date 'open)(getd date 'close))
                          (>= (getd date 'high) entry-long))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade-short (apply #'vector
                 (append trade-short
                         (list date (my-pretty-price stop-short) (round (* (index-point-value)(- short stop-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil))


    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade-short (apply #'vector
                (append trade-short
                        (list date (my-pretty-price stop-short) (round (* (index-point-value)(- short stop-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil
           ))

   );;;closes the dotimes


  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format output "~%~A~%" *data-name*)
     (format output "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
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
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~S~%" ith)

    ));;closes the dolist and with-open-file
   ); closes the when



    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun




;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun position-simulation-test3 (market date2 num &optional (stocks nil)(features *position-features3*))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade-short 
       risk risk-long risk-short  epsignal longs long-gains long-acc trading-dates (trade-time 0)
       shorts short-gains short-acc prev-signal ctr date-1 record (running-sum 0) bin draw
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "position-summary3.dat"))
       (AT (make-hash-table)) (VT (make-hash-table))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "position-simulation3.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "position-diary3.dat")))

    

  
    (if (and num (> num (available-days market date2 )))
        (setq num nil))
     (ifn num (setq num (available-days market date2 )))

    (if stocks (Setq *stocks* T))
   (apply #'position-trade-bins3 features)
   (set-market market)

   (setq date (add-mkt-days date2 (- num)))
    (setq record (list date (getd date 'close) 0 0 0))
    (push record trading-dates)
 ;;;;from date1 to date2
 (dotimes (ith num)

    (cond ((gethash  date  AT))
          (t (setf (gethash date AT) (ave date 5 'pivot))))

   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months

   (multiple-value-setq (entry-short entry-long)
        (vprices date 18 2.53 7))

   (setq entry-short (min entry-short (getd date 'close)) entry-long (max entry-long (getd date 'close)))
;;;this is a black swan objective
   (setq ave4 (gethash date AT)
       cover-short  (my-pretty-price (exp (- (log ave4) (* *objective-factor-position* (gethash date VT)))))
       cover-long (my-pretty-price (exp (+ (log ave4) (* *objective-factor-position* (gethash date VT))))))


   (setq risk-short (my-pretty-price (* *stop-loss-position* (abs (- entry-short (n-day-high date 7 'close))))))

   (setq risk-long  (my-pretty-price (* *stop-loss-position* (abs (- entry-long  (n-day-low date 7 'close))))))

   (setq risk (max risk-long risk-short))

   (setq date-1 date date (add-mkt-days date 1))

   (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1))

;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (nth 3 record)(- (getd date-1 'close)(getd date 'close))))

    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	   entry-long (+ entry-long (getd date 'rollover))
    		   stop-long (+ stop-long (getd date 'rollover)))
             (setf (nth 3 record)(- (nth 3 record) (getd date 'rollover)))
)
    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    		  entry-short (+ entry-short (getd date 'rollover))
    		  stop-short (+ stop-short (getd date 'rollover)))
            (setf (nth 3 record) (+ (nth 3 record) (getd date 'rollover)))
)


   (when long (setq stop-long (fmax stop-long entry-short)))

   (when short (setq stop-short (fmin stop-short entry-long)))

;;;;check if stopped out of prior position

   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (apply #'vector
             (append trade-long
              (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long)))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
          (setq trade-long nil long nil stop-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open)))))))
          (push trade-short extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade-short nil short nil stop-short nil))

  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))
            (push (- (max (getd date 'open) cover-long) long) trades)
            (setq trade-long (apply #'vector
               (append trade-long
                  (list date 'exit (max (getd date 'open) cover-long) (- (max (getd date 'open) cover-long) long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (- (max (getd date 'open) cover-long) (getd date-1 'close)))
            (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
            (setq trade-long nil long nil stop-long nil))

      (when (and short (< (getd date 'low) cover-short))
            (push (- short (min (getd date 'open) cover-short)) trades)
            (setq trade-short (apply #'vector
               (append trade-short (list date 'exit cover-short (- short (min (getd date 'open) cover-short))))))
            (push trade-short extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (- (getd date-1 'close) (min (getd date 'open) cover-short)))
            (if (getd date 'rollover)(setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
            (setq trade-short nil short nil stop-short nil))

;;;;calculate bin-classifier only as needed
 (when (or (<= (getd date 'low) entry-short)
           (>= (getd date 'high) entry-long)
               )
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-positions3 date-1 features))
       (setf (nth 0 bin) 1)
       
       (multiple-value-setq (prev-signal ctr)(vsignals date-1 18 2.53 7)))


;;;check if new entry

    (when (and (not short)
               (<= (getd date 'low) entry-short)
              ; (>= (getd date 'high) entry-short)
               (member epsignal '(OK DOWN))
               (eql prev-signal 'BUY)
               (>= (/ short-gains shorts) 150) 
               (<= (* risk (index-point-value)) *max-position-risk*)
               )
          (setq short (min entry-short (getd date 'open))
                 trade-short (list date 'short short)
                 stop-short (+ entry-short risk-short))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(/ *commission* (index-point-value)))))

              )

    (when (and (not long)
               (>= (getd date 'high) entry-long)
               ;(<= (getd date 'low) entry-long)
               (member epsignal '(OK UP))
               (eql prev-signal 'SELL)
               (>= (/ long-gains longs) 150) 
               (<= (* risk (index-point-value)) *max-position-risk*)
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long)
                 stop-long (- entry-long risk-long))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (- (getd date 'close) long (/ *commission* (index-point-value)))))

             )


 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (apply #'vector (append trade-long (list date 'exit stop-long (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil))


    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (apply #'vector (append trade-long (list date 'exit stop-long (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record) (- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil
           ))


    (when  (and short (or (< (getd date 'open)(getd date 'close))
                          (>= (getd date 'high) entry-long))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade-short (apply #'vector (append trade-short (list date 'exit stop-short (- short stop-short)))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil))


    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade-short (apply #'vector  (append trade-short (list date 'exit stop-short (- short stop-short)))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record) (- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil
           ))


 ;;;check if met objective
 ;;;adjust cover-long and cover-short prices
 ;;;to exit at the volatility bands in case the
 ;;;trade outlook changes.
#|
    (when (and short
               (<= (getd date 'low) entry-short)
               (> shorts 0)(<= short-gains 0)
               (< twr-short 1)
               )
          (setq cover-short (min entry-short (getd date 'open)))
              )

    (when (and long
               (>= (getd date 'high) entry-long)
               (> longs 0)(<= long-gains 0)
               (< twr-long 1)
               )
           (setq cover-long (max entry-long (getd date 'open)))
             )
|#

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade-long (apply #'vector (append trade-long (list date 'exit cover-long (- cover-long long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
            (setq trade-long nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade-short (apply #'vector (append trade-short (list date 'exit cover-short (- short cover-short)))))
            (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (+ (nth 3 record) (- short cover-short)))
            (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
            (setq trade-short nil short nil stop-short nil)))


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
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

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

   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)(round (* (index-point-value) (svref ith 6))))
    ));;closes the dolist and with-open-file
   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun


(defun position-trade-bins3 (&rest features)
  (let (bin path)
  (if *stocks* (setq path (string-append *upper-dir-warehouse* "stockspositionwarehouse3.dat"))
    (setq path (string-append *upper-dir-warehouse* "positiontradewarehouse3.dat")))
  (maind-x)(set-cat-list)
  (setq positions nil position-bin-codes nil)(clrhash *day-trade-warehouse3*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record positions)))


;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record positions)
     (setq bin (encode-position-trades3 record features))
     (pushnew bin position-bin-codes :test #'equal)
     (cond ((gethash bin *day-trade-warehouse3*)
            (ifn (member record (gethash bin *day-trade-warehouse3*) :test #'equalp)
                 (setf (gethash bin *day-trade-warehouse3*)
                       (cons record (gethash bin *day-trade-warehouse3*)))))
           ((not (gethash bin *day-trade-warehouse3*))
             (setf (gethash bin *day-trade-warehouse3*)
              (list record)))))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-position-bins-by-profit3 positions)


  ))



(defun rank-position-bins-by-profit3 (positions)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0) (twr 1)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith position-bin-codes)
     (setq contents (gethash ith *day-trade-warehouse3*))
     (setq result 0 counter 0 twr 1)

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
         (setq twr (* twr (+ 1 (/ (if (plusp (svref kth 2))
                                 (- (svref kth 18) (svref kth 3))
                               (- (svref kth 3) (svref kth 18)))
                             (svref kth 3)))))
         ) ;;;closes dolist over contents
      (if (and (plusp result)(> twr 1)) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))


     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over bin-codes

    (dolist (kth positions)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
          (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length positions)(length (num-markets-in-warehouse3 positions)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length position-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length positions)(length position-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length position-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length positions)) 2))

    (values (round winners)(round (+ all-winners all-losers)))

 ))


;;;returns a list of nine codes
(defun encode-position-trades3 (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
;;;;Feature 1 has 2 levels
     (1 (push (svref record 2) bin-list));;;adds the direction

;;;Feature 2 with 5 levels; This is the TS5

      (2 (case  (svref record 4)
             (DN (push -1 bin-list))
             (CD (push -2 bin-list))
             (UP (push 1 bin-list))
             (CU (push 2 bin-list))
             (FT (push 0 bin-list))
             ) )
;;;Feature 3 with 5 levels; This is TS15

       (3  (case  (svref record 5)
             (DN (push -1 bin-list))
             (CD (push -2 bin-list))
             (UP (push 1 bin-list))
             (CU (push 2 bin-list))
             (FT (push 0 bin-list))
             ) )

;;;Feature 4 with 5 levels; This is TS45

      (4   (case  (svref record 6)
             (DN (push -1 bin-list))
             (CD (push -2 bin-list))
             (UP (push 1 bin-list))
             (CU (push 2 bin-list))
             (FT (push 0 bin-list))
             ) )

 ;;;Feature 5 with 5 levels; This is TS135

      (5   (case  (svref record 7)
             (DN (push -1 bin-list))
             (CD (push -2 bin-list))
             (UP (push 1 bin-list))
             (CU (push 2 bin-list))
             (FT (push 0 bin-list))
             ) )
 ;;;;Feature 6 with 10 levels ;this is the HT ratio of volatility

      (6 (cond ((< (svref record 8) .60) (push 5 bin-list))
               ((and (>= (svref record 8) .60)(< (svref record 8) .70)) (push 4 bin-list))
               ((and (>= (svref record 8) .70)(< (svref record 8) .80)) (push 3 bin-list))
               ((and (>= (svref record 8) .80)(< (svref record 8) .90)) (push 2 bin-list))
               ((and (>= (svref record 8) .90)(< (svref record 8) 1.0)) (push 1 bin-list))
               ((and (>= (svref record 8) 1.0)(< (svref record 8) 1.10)) (push -1 bin-list))
               ((and (>= (svref record 8) 1.10)(< (svref record 8) 1.20)) (push -2 bin-list))
               ((and (>= (svref record 8) 1.20)(< (svref record 8) 1.30)) (push -3 bin-list))
               ((and (>= (svref record 8) 1.30)(< (svref record 8) 1.40)) (push -4 bin-list))
               ((>= (svref record 8) 1.40)(push -5 bin-list))))


     ;;;;Feature 7 with 9 levels bar type

          (7 (case  (svref record 9)
               (11 (push 0 bin-list))
               (12 (push 1 bin-list))
               (13 (push 2 bin-list))
               (21 (push 3 bin-list))
               (22 (push 4 bin-list))
               (23 (push 5 bin-list))
               (31 (push 6 bin-list))
               (32 (push 7 bin-list))
               (33 (push 8 bin-list)) ))

;;;;;;Feature 8 with 5 levels of bar relationship to the previous bar

       (8 (case (svref record 10)
               (IN (push -2 bin-list))
               (OU (push 2 bin-list))
               (DN (push -1 bin-list))
               (UP (push 1 bin-list))
               (FT (push 0 bin-list))
                             ))
;;;;feature 9 with 5 levels of previous bar relationship to its prior bar

         (9 (case (svref record 11)
               (IN (push -2 bin-list))
               (OU (push 2 bin-list))
               (DN (push -1 bin-list))
               (UP (push 1 bin-list))
               (FT (push 0 bin-list))
                     ))

 ;;;;Feature 10 with 7 levels ;item 12 is the stochastic with parameter 21
     (10 (cond ((<= (svref record 12) 10) (push 3 bin-list))
              ((and (> (svref record 12) 10)(<= (svref record 12) 20)) (push 2 bin-list))
              ((and (> (svref record 12) 20)(<= (svref record 12) 40)) (push 1 bin-list))
              ((and (> (svref record 12) 40)(<= (svref record 12) 60)) (push 0 bin-list))
              ((and (> (svref record 12) 60)(<= (svref record 12) 80)) (push -1 bin-list))
              ((and (> (svref record 12) 80)(<= (svref record 12) 90)) (push -2 bin-list))
              ((>= (svref record 12) 90) (push -3 bin-list))))


;;;Feature 11 with 5 levels; this is the rsi-trend 5

      (11  (push (svref record 13) bin-list))


;;;;Feature 12 with 6 levels ; This is rsi trend 15
      (12  (push (svref record 14) bin-list))


;;;Feature 13 with 5 levels is the trend-signal 45
       (13 (push (svref record 15) bin-list))


;;;Feature 14 with 6 levels is the pivot-index with date-1
       (14  (push (svref record 16) bin-list))



                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))

;;;calculates the twr for a bin
;;;must run #'position-trade-bins3 before running this function
(defun position-bin-twr3 (bin)
  (let ((twr 1))

   (dolist (record (gethash bin *day-trade-warehouse3*))
     (setq twr (* twr (+ 1 (/ (if (plusp (svref record 2))
                                 (- (svref record 18) (svref record 3))
                               (- (svref record 3) (svref record 18)))
                             (svref record 3)))
                           )))
  twr
))



;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse
(defun bin-classifier-positions3 (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (nxdate (getd date 'ndate))
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))

    (setq record (create-position-entry-record-list1 date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))


  (setq bin (encode-position-trades3 record features))
  (setq contents (gethash bin *day-trade-warehouse3*))
      ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))


   (setq contents
    (remove-if #'(lambda(s1) (and (eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))

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
        ((plusp results-short) (setq epsignal 'DOWN))
        ((and (<= results-long 0)(<= results-short 0)) (setq epsignal 'AVOID))
        ((not contents) (setq epsignal 'UNIQUE)))

  (values epsignal longs results-long (if (zerop longs) 0 (/ num-winners-long longs))
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
 ))
|#
;;;

(defun indicator-count (path1)
  (let (dayfeatures counts)
  
 (if (probe-file path1)
      (with-open-file (str path1 :direction :input)
       (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (setq dayfeatures (append (cdr record) dayfeatures)))))

   (dotimes (ith 14)
     (setq counts (acons (1+ ith) (count (1+ ith) dayfeatures) counts)))
 (vsort counts #'< 'cdr)
   ))     
  
  
(defun explain-trades (market fdate ldate)
  (let (unfiltered epsignal record)
    (apply #'day-trade-bins3b *day-features3*)
  (setq unfiltered (trim-trades daytrades market fdate ldate))
  (dolist (ith unfiltered)
    (set-market (svref ith 0))
    (setq epsignal (bin-classifier-daytrades3b (getd (svref ith 1) 'ydate) *day-features3*))
    (setq record (create-daytrade-entry-record-list3
                             (getd (svref ith 1) 'ydate) (svref ith 2) (svref ith 3)))
    (format T "~%~A   ~A~%~A"  ith epsignal record)
    )
))


(defun find-best-daytradeX (tdate &optional (output T)(date1 nil))
  (let (risk  stop-short stop-long  entry-long entry-short
        vri1 vri2   rri10 rri5   
        directive1 ts-charger-path2 (time-zone "CT") 
        )
    (declare (special counter))

    (setq *entry-factor* .764 *stop-loss-day* .764 ;.944 
          *max-day-risk* 2000 ;2500 
          *commission* 25  *pips-slippage* 0) 

    (setq                 
          ts-charger-path2 (string-append *daily-output-dir* "ts-charger-" (format nil "~A" date1) ".csv")
      	  )
   
   (setq risk (volatility tdate 4 *stop-loss-day*))
   (setq vri2 (volatility-ratio-index tdate 4 28 1) vri1 (volatility-ratio-index tdate 1 7 1)
         rri10 (ep-roc-index10 tdate 10) rri5 (ep-roc-index10 tdate 5))        
 
   (multiple-value-setq (entry-short entry-long) (vprices tdate 4 *entry-factor* 1))
   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

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

       
     ;     (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
     ;    (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
          (when (and (plusp vri2)(>= vri1 -2) (>= rri10 -2)(>= rri5 -3)
                       (<= (* risk (index-point-value)) *max-day-risk*) ) 
             (with-open-file (ninja-output ts-charger-path2
                       :direction :output :if-exists :append :if-does-not-exist :create)
      	     (write-ts-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long *striker-score-qty*))
                )
              (when (and (plusp vri2)(> vri2 -2) (<= rri10 2)(<= rri5 3)
                         (<= (* risk (index-point-value)) *max-day-risk*) )
             (with-open-file (ninja-output ts-charger-path2
                       :direction :output :if-exists :append :if-does-not-exist :create)
    	     (write-ts-record ninja-output  time-zone tdate date1 'SELL entry-short stop-short *striker-score-qty*))
               )
      )) ;;;closes the let and the defun



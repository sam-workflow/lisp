
;; -*- Mode: LISP; Package: user; Base: 10. -*-
  
#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;;

;;;;;;;;;;;;;;;;;SWING TRADES FUNCTIONS 
;;;;
;;;these are the parameters to adjust for vsignals
(defvar *duration* 105)
(defvar *factor* 1.125)
(defvar *type* 'median)
(defun straddle-risk (market date)
  (let (vpl vph (mkt *data-name*) ptv risk)
    (set-market market) (setq ptv (calculate-point-value date market))
    (multiple-value-setq (vpl vph risk)(vprices date *duration* *factor* 1 *type*))
    (set-market mkt)
  (values (round (* ptv risk)) risk)
  ))
(defun which-first (date)
  (if ;(minusp (roc date 1)) 'high-first 'low-first))
   (> (getd date 'open)(getd date 'close)) 'high-first 'low-first))

(defun update-swingtrade-warehouse (date &optional (markets (append *micro-list* *straddle-list*)))
    (maind-x)(set-cat-list)
 
 ;  (setq *entry-factor-swing* 1.35 *stop-loss-swing* 1.35)
   (setq *entry-factor-swing* 0); *stop-loss-swing* .333)
 
    (dolist (ith markets)
       (set-market ith)
       (populate-swing-trades ith date ;(available-days ith date 1200)))
			              ;(fmin (sub-mkt-dates 19950725 date) (available-days ith date 1800))))
                         (fmin ;(sub-mkt-dates 20180913 date) 
			       (available-days ith date 500)
			       (available-days ith date 330))))
 
    (build-swingtrade-warehouse markets) 
    (setq *counter-swing-features*  (find-best-swing-indicator-set) )
;   (portfolio-simulation3 '(swing) date 4275 (list markets)) ;(list *score-list*))
;    (indicator-study)
    (apply #'swing-trade-binsb nil *counter-swing-features*) ;;;needed for display of unfltered trades
    (display-unfiltered-trades swings)
)

;;;with this system we enter a distance from the open price.

(defun populate-swing-trades (market date2 num &optional  (output T))
 (block POP 
 (let (date stop-long stop-short trades long short  trade-long; dirpsar sdate stop-loss-psar
       ave-win ave-loss losers winners extended-trades trade-short date-1  date-2 date+1 
       (days-in-trade 0)   target-long target-short prev-entry-short; rangdev5 cylsig cylproj
        entry-long entry-short 
        prev-target-short prev-target-long ; aveh4 
       prev-stop-long prev-stop-short vpsig ctr vprev ; acc5  ccil5  cci21i ccix zst9 bpl9 volt
       ses sel risk
         vplety vphety vplext vphext; zs3 ZS5 zs13 zs8 vs4 os4 vsos4 vsos7 2bwi ccis1
       ; regr4cci high3 low3  chandir13 pst5 pivw adx9i regrsi3  can pivt  bull-cntr bear-cntr
        long-stopped-out short-stopped-out        
       cover-long cover-short ;(stockp (member market *stocks-list*))
        (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "swingtrades.dat")))
 
   (maind-x)(set-cat-list)(set-market market)(ifn num (return-from POP nil))
 
   (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 6   ))
          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 ))
          ((member market (append *sp100-list* *stocks-list* *space-list* *ff-list*))
	   (setq *commission* 6 *pips-slippage* 0))
          ((member market  *micro-list*) (setq *commission* 6 *pips-slippage* 0))
          ((member market  *straddle-list*) (setq *commission* 6 *pips-slippage* 0))
          ((member market *swing-list*)(setq *commission* 25 *pips-slippage* 0)) 
          (t (setq *commission* 6 *pips-slippage* 0)))

   (setq *entry-factor-swing* 0 )
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate) date+1 (getd date 'ndate))


 ;;;;from date1 to date2
  (dotimes (ith num)


;;;stop or entry
 
  
    (multiple-value-setq (vplety vphety risk)(vprices date *duration* *factor* 1 *type*))
  
    (multiple-value-setq (vplext vphext)(vprices date *duration* *factor* 1 *type*))
 
    (multiple-value-setq (vsig ctr vprev)(vsignals1 date))
 ;  (format t "~A ~A~%" date vsig)
   
    (setq entry-long (if (eql vsig 'SHORT) vprev vphety)
          entry-short (if (eql vsig 'LONG) vprev vplety))


    (setq stop-long entry-short
          stop-short entry-long)
     
    (and short (setq stop-short (fmin prev-stop-short stop-short) target-short (fmin prev-target-short target-short)))
    (and long (setq  stop-long (fmax prev-stop-long stop-long) target-long (fmax prev-target-long target-long)))
    
     (setq cover-short target-short
           cover-long target-long)
 
       
;  (if long
;       (format T "~%1Date = ~A  cover-short= ~A  short= ~A"
 ;               date cover-short short ))
;  (if short
 ;    (format T "~%1Date = ~A cover-long= ~A  long = ~A "
;                date cover-long long  ))
; (if (or long short)  (format t "~%Trades = ~A" trades))

    (and long (setq prev-stop-long stop-long prev-target-long target-long))
    (and short (setq prev-stop-short stop-short prev-target-short target-short))

   (setq cover-long target-long cover-short target-short)   
   (setq long-stopped-out nil short-stopped-out nil)
   
;;;which comes first the target or the stop loss?
;;;targethf for longs 
#|    (setq entrylf  (or (and (> (getd date 'high) entry-long)
                             (< (getd date 'open)(getd date 'close)) ;;;low came first but hit target not stop loss
                            ; (> (getd date 'low) stop-long)
                            )
                        (and (> (getd date 'high) cover-long)
                             (> (getd date 'open)(getd date 'close)))))                        
;;;targetlf for shorts
    (setq entrysf  (or (and (< (getd date 'low) cover-short)
                             (> (getd date 'open)(getd date 'close)) ;;high came first but hit target not stop loss
                            ; (< (getd date 'high) stop-short)
                              )
                        (and (< (getd date 'low) cover-short)
                             (< (getd date 'open)(getd date 'close)))))                        

|#
   (setq date-1 date date (add-mkt-days date 1))
   (setq date-2 (getd date-1 'ydate) date+1 (getd date 'ndate))
 
;  (setq entry-short (- (getd date 'open) (volatility date-1 4 *entry-factor-swing*))
 ;       entry-long (+  (getd date 'open) (volatility date-1 4 *entry-factor-swing*))
  ;         ) 
  ;
  ;   (setq prev-stop-long stop-long prev-stop-short stop-short)

;     (setq entry-long (my-pretty-price (getd date 'open)) stop-long (my-pretty-price stop-long)
;           entry-short (my-pretty-price (getd date 'open)) stop-short (my-pretty-price stop-short))

  ;  (setq target-short nil target-long nil)     
    (if (or long short) (incf days-in-trade))

    (if (getd date 'rollover)(setq entry-long (+ (getd date 'rollover) entry-long)
                                   entry-short (+ (getd date 'rollover) entry-short)
                                   stop-long (+ (getd date 'rollover) stop-long)
                                   stop-short (+ (getd date 'rollover) stop-short)
                                   ))

    (when (and (getd date 'rollover) long stop-long )
          (setq long (+ long (getd date 'rollover));(comm+slip-points date))
               ; stop-long (+ stop-long (getd date 'rollover)) 
                cover-long (add cover-long (getd date 'rollover))
               
    ))
   (when (and (getd date 'rollover) short stop-short )
          (setq short (+ short (getd date 'rollover));(- (comm+slip-points date)))
    	       ; stop-short (+ stop-short (getd date 'rollover))
               cover-short (add cover-short (getd date 'rollover))
                
       
         ))
  ;;;check if need to ratchet stop
 ;  (cond ((and long (>= (getd date 'high) entry-long)(eql (which-first date) 'high-first))
;;	  (setq stop-long (nth-value 1 (ratchet-stops date)) entry-short stop-long))
;	 ((and short (<= (getd date 'low) entry-short) (eql (which-first date) 'low-first))
;	  (setq stop-short (nth-value 0 (ratchet-stops date)) entry-long stop-short))
;	 (t nil))
   
;   (format T "~%~%2date= ~A dir= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A ~%"
 ;             date dir long stop-long short stop-short)


;;;check if exit based on criteria at open of the day to take profit 

   (when  (and long  
               (eql vsig 'SHORT)                                     
               )
     ;      (format T "~%Exited long with Criteria day = ~A long = ~A cover-long = ~A stop-long= ~A~%"
      ;            date long cover-long stop-long) 
          (push (round (- (* (my-pretty-price (- (getd date 'open) long))
                             (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (getd date 'open))
                       (round (* (calculate-point-value date market)(my-pretty-price (- (getd date 'open)  long))))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil ;stop-long nil
		prev-stop-long nil days-in-trade 0 ))

   (when  (and short  
             (eql vsig 'LONG)
               )                                  
     ;   (format T "~%Exited short with Criteria day = ~A short = ~A cover-short = ~A stop-short=~A~%" 
       ;             date short cover-short stop-short)
          (push (round (- (* (my-pretty-price (- short (getd date 'open)))
                             (calculate-point-value date market))(comm+slip date))) trades)
           (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (getd date 'open))
                 (round (* (calculate-point-value date market)(my-pretty-price (- short (getd date 'open)))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil ;stop-short nil
		prev-stop-short nil days-in-trade 0 ))


     
;;;means of entry long  on open price
   (when  (and (not long) 
	       (eql vsig 'LONG)	 
                 )
                 (setq long  
                       (getd date 'open)		       
                 trade-long  (create-swingtrade-entry-record-list date-1 1 long)
                 stop-long (nth-value 1 (ratchet-stops date))
            
                long-stopped-out nil  cover-long target-long prev-stop-short nil
                 prev-stop-long nil days-in-trade 1 
                ))
    ;  (format T "~%3date = ~A" date)

                                
        
;;;check if new entry short on open price
;(format t "~%date = ~A long = ~A early-exit = ~A macdd = ~A can = ~A wp = ~A" date long early-exit macdd can wp)
;(format t "~%rsi2xl = ~A cci5l = ~A reward = ~A" rsi2xl cci5l reward)

   (when   (and (not short)
                (eql vsig 'SHORT)
                          )      
      (setq short 
	   (getd date 'open)
            trade-short (create-swingtrade-entry-record-list date-1 -1 short)
     
            stop-short (nth-value 0 (ratchet-stops date))        
            short-stopped-out nil cover-short target-short
            prev-stop-short nil days-in-trade 1 
            prev-entry-short entry-short prev-stop-long nil
                 ))
#|   
;;;;check if hit target #2 
   (when (and long cover-long
           (>= (getd date 'high) cover-long))
          ; (>= fst21 80)(>= cci5 100))                 
           (setq cover-long (max cover-long (getd date 'open)))                     
      ;    (format T "~%Exited long with Target day = ~A long = ~A cover-long = ~A" date long cover-long)
          (push (round (- (* (my-pretty-price (- cover-long long))
                             (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price cover-long)
                       (round (* (calculate-point-value date market)
                                 (my-pretty-price (- cover-long long))))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil  cover-long nil prev-stop-long nil target-long nil ))

   (when (and short cover-short  
          (<= (getd date 'low) cover-short))
         ; (<= fst21 20)(<= cci5 -100))
          (setq cover-short (min cover-short (getd date 'open)))
       ;   (format T "~%Exited short with Target day = ~A short = ~A cover-short = ~A" 
        ;        date short cover-short)
          (push (round (- (* (my-pretty-price (- short cover-short))
                             (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price cover-short)
                 (round (* (calculate-point-value date market)
                           (my-pretty-price (- short cover-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil  cover-short nil prev-stop-short nil target-short nil  ))

|#

;;;check if stopped out type  #1
   (when (and long stop-long (<= (getd date 'low) stop-long)
	      (< (getd date 'high) entry-long))
           (setq stop-long (min stop-long (getd date 'open)))                     
         ; (format T "~%Stopped out long day = ~A long = ~A stop-long = ~A" date long stop-long)
          (push (round (- (* (my-pretty-price (- stop-long long))
                             (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price  stop-long)
                       (round (* (calculate-point-value date market)
                                 (my-pretty-price (- stop-long long))))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil ;stop-long nil
		prev-stop-long nil long-stopped-out T target-long nil))

   (when (and short stop-short (>= (getd date 'high) stop-short)
	      (> (getd date 'low) entry-short))
          (setq stop-short (max stop-short (getd date 'open)))
;          (format T "~%Stopped out short day = ~A short = ~A stop-short = ~A" date short stop-short)
          (push (round (- (* (my-pretty-price (- short stop-short))
                             (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price stop-short)
                 (round (* (calculate-point-value date market)
                           (my-pretty-price (- short  stop-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil ;stop-short nil
		prev-stop-short nil   short-stopped-out T target-short nil))

  ;     (format T "~%2date= ~A  long = ~A stop-long = ~A short = ~A stop-short = ~A"
   ;           date long stop-long short stop-short)

 ;(format T "~%2entry-long = ~A entry-short = ~A" entry-long entry-short)

;;;;check if exited with criterion change at end of the day
#|
   (when  (and long  (> days-in-trade time-allowed)                                                 
                )
          (push (round (- (* (-  (getd date 'open) long)
                      (calculate-point-value date-1 market))(comm+slip date-1))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (getd date 'open))
                       (round (* (calculate-point-value date market) (- (getd date 'open)  long)))))))
          (push trade-long extended-trades)
        (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0 early-exit T))

   (when  (and short (> days-in-trade time-allowed)                       
                             )
          (push (round (- (* (- short (getd date 'open))
                      (calculate-point-value date-1 market))(comm+slip date-1))) trades)
           (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (getd date 'open))
                 (round (* (calculate-point-value date market) (- short (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 early-exit T))

; (format T "~%2date= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A"
 ;         date  long stop-long short stop-short)
|# 
;;;means of entry long #1 on entry-price
   (when  (and (not long)(not short) (eql vsig  'SHORT)
	       (fgtr (getd date 'high) entry-long)
	        short-stopped-out
                 );;first entry long
                 (setq long  
                   (fmax (getd date 'open)  entry-long)
               
                 trade-long  (create-swingtrade-entry-record-list date-1 1 long)
                 stop-long (nth-value 1 (ratchet-stops date));(- long risk);vplext
            
                long-stopped-out nil  cover-long target-long prev-stop-short nil
                 prev-stop-long nil days-in-trade 1 
                ))
     ; (format T "~%3date = ~A" date)

                                
        
;;;check if new entry short #1
;(format t "~%date = ~A long = ~A early-exit = ~A macdd = ~A can = ~A wp = ~A" date long early-exit macdd can wp)
;(format t "~%rsi2xl = ~A cci5l = ~A reward = ~A" rsi2xl cci5l reward)

   (when   (and (not short)(not long)(eql vsig 'LONG)
		(fltn (getd date 'low) entry-short)	      
                 long-stopped-out
                          );;entry short
      
      (setq short 

	    (if (eql vsig 'SHORT)(getd date 'open)(fmin (getd date 'open) entry-short))
	 
            trade-short (create-swingtrade-entry-record-list date-1 -1 short)
     
            stop-short (nth-value 0 (ratchet-stops date));(+ short risk);vphext 
                  
            short-stopped-out nil cover-short target-short
            prev-stop-short nil days-in-trade 1 
            prev-entry-short entry-short prev-stop-long nil
                 ))
                 
 
 ; (format T "~%date= ~A short  = ~A stop-short = ~A -~%" date short stop-short)
 ;check if stopped out on same day of entry for a second reversal
;;;; second entry
    (when (and long stop-long (eql vsig 'SHORT)
	     (<= (getd date 'low) stop-long)
	      (eql (which-first date) 'high-first)
            ; (or (and (<= (getd date 'low) stop-long)
             ;         (> (getd date-1 'close) (getd date 'close))
              ;      )                
               ;  (<= (getd date 'close) stop-long))
              )
      ;   (format T "~%long stopped same day: Date = ~A long = ~A stop-long = ~A~%" date long stop-long)
          (push (round (- (* (my-pretty-price (- stop-long long))(calculate-point-value date market))
                             (comm+slip date))) trades)
           (setq trade-long (apply #'vector
                 (append trade-long
                    (list date (my-pretty-price stop-long)
                                (round (* (calculate-point-value date market)
                                          (my-pretty-price (- stop-long long))))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil   prev-stop-long nil days-in-trade 1 long-stopped-out T
           ))

    (when (and short stop-short (eql vsig 'LONG)
	      (>= (getd date 'high) stop-short)
	      (eql (which-first date) 'low-first) 
             ; (or (and (>= (getd date 'high) stop-short)
              ;         (< (getd date-1 'close) (getd date 'close))
               ;        )     
                ;  (>= (getd date 'close) stop-short))
              )
   ;  (format T "~%Short stopped same day: Date = ~A short = ~A stop-short = ~A~%" date short stop-short)
           (push (round (- (* (my-pretty-price (- short stop-short))(calculate-point-value date market))
                              (comm+slip date))) trades)
           (setq trade-short (apply #'vector
                (append trade-short
                 (list date (my-pretty-price stop-short)
                            (round (* (calculate-point-value date market)
                                      (my-pretty-price (- short stop-short))))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil  prev-stop-short nil days-in-trade 1 short-stopped-out T
           ))


;;;;this is the second entry for the day if stopped out on same day as first entry

;;;means of entry long #2 on short stop price
    (when  (and  (not short)(not long) (eql vsig 'LONG)
		 (eql (which-first date) 'low-first)
		 (>= (getd date 'high) stop-short)
		; (or (and (>= (getd date 'high) stop-short)
                 ;         (< (getd date-1 'close) (getd date 'close))
                  ;     )       
                   ;    (>= (getd date 'close) stop-short))	   	      
		    
                 );;second entry long
                 (setq long  stop-short
               
                 trade-long  (create-swingtrade-entry-record-list date-1 1 long)
                 stop-long (nth-value 1 (ratchet-stops date))
            
                long-stopped-out nil  cover-long target-long prev-stop-short nil
                 prev-stop-long nil days-in-trade 1 short-stopped-out nil
                ))
  ;    (format T "~%3date = ~A" date)


;;;;means of entry short #2 on long stop price on same day
    (when  (and (not short)(not long)long (eql vsig 'SHORT)
		(eql (which-first date) 'high-first)
		(<= (getd date 'low) stop-long)
	       ; (or (and (<= (getd date 'low) stop-long)
                ;         (> (getd date-1 'close) (getd date 'close))
                 ;                  )
					;   (<= (getd date 'close) stop-long))
              )
        
      (setq short stop-long
	 
            trade-short (create-swingtrade-entry-record-list date-1 -1 short)
     
            stop-short (nth-value 0 (ratchet-stops date));(+ short risk);vphext 
                  
             short-stopped-out nil cover-short target-short
             prev-stop-short nil days-in-trade 1 
             prev-entry-short entry-short prev-stop-long nil long-stopped-out nil
                 ))
       

#|
;;;check if met exit criteria to exit at end of day of entry for day trade
     (when (and long short (>= cci5i 0))
            (setq cover-long (getd date 'open))
            (push (round (- (* (my-pretty-price (- cover-long long))(calculate-point-value date market))
                               (comm+slip date))) trades)
            (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price cover-long)
                                           (round (* (calculate-point-value date market)
                                                     (my-pretty-price (- cover-long long))))))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))

      (when (and short long (<= cci5 0))
           (setq cover-short (getd date 'open))
           (push (round (- (* (my-pretty-price (- short cover-short))(calculate-point-value date market))
                              (comm+slip date))) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price cover-short)
                                                (round (* (calculate-point-value date market)
                                                          (my-pretty-price (- short cover-short))))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0))

|#
#|
;;;check if exited with objective on day of entry
   (when  (and long cover-long (>= (getd date 'high) cover-long) )
          (setq cover-long (max (getd date 'open) cover-long))
          (push (round (- (* (my-pretty-price (- cover-long long))
                             (calculate-point-value date market))(comm+slip date))) trades)
         
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (max (getd date 'open) cover-long))
                       (round (* (calculate-point-value date market) (- (max (getd date 'open) cover-long) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil cover-long nil prev-stop-long nil days-in-trade 1 target-long nil ))

   (when  (and short cover-short (<= (getd date 'low) cover-short))
          (setq cover-short (min (getd date 'open) cover-short))
          (push (round (- (* (my-pretty-price (- short cover-short))
                             (calculate-point-value date market))(comm+slip date))) trades)

          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (min (getd date 'open) cover-short))
                 (round (* (calculate-point-value date market) (- short (min (getd date 'open) cover-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil cover-short nil prev-stop-short nil days-in-trade 1 target-short nil))

    (setq long-stopped-out nil short-stopped-out nil)
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
   ));;;closes the let and the block
);;;closes the defun

;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;(defun create-swing-entry-record-list (date direction entry)
;   (let* ((date-1 (getd date 'ydate))(date-2 (getd date-1 'ydate)))
;       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
;                              (timing-index date);;;feature 2
;                              (timing-index date-1)
;                              (timing-index date-2) ;;;feature 4
;                              (round (slow-stochastic date 21))
;                              (round (slow-stochastic date-1 21)); feature 6
;                              (trend-signal date 5)
;                              (trend-signal date 15) ;feature 8
;                              (trend-signal date 45)
;                              (round (rsi-ave-diff date 14 2)) ;;feature 10
;                              (round (rsi-ave-diff date-1 14 2))
;                              (dev-distribution-close date 4 .875 'close);; feature 12
;                              (momentum-divergence1 date 5 13)
;                              (or (car (formation date)) 'FT));;;feature 14
;                             
;))

;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
(defun create-swingtrade-entry-record-list (date direction entry)
   (let ();(date-1 (getd date 'ydate)) 
       
       
 ; (setq date-2 (getd date-1 'ydate))
      
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
           
        
;	     (case (gann-slope-index date 8) ;feature 2
 ;              ((-4 -3) 'L)
;	       ((0 2) 'S)
;	       ((6 -7) 'X)
;	       (otherwise 'F))
	     (gann-slope-index date 8) ;features 
   ;           (case (ep-roc-change-index date 13 2); feature 3 ***
;	        ((-3 2) 'L)
;		(3 'S)
;		((-4 -1) 'X)
;		(otherwise 'F))
	      (ep-roc-change-index date 13 2); feature 3 ***
 ;             (case (zero-strength date 3) ;feature 4
;		  ((3SU SUP) 'B)
;	          ((LDU LDD BZT) 'L)
;	          ((WDN) 'S)
;	          ((AZT) 'X)
;	          (otherwise 'F))
             (zero-strength date 3)
 	      
;              (case (bpl-index date 9)  ;feature 5 ***;
;		(U3 'B)
;	       ((U2 -U2 -D1 U1) 'L)
;	       ((-U3 -D2 D1) 'S)
;	       ((-U1 -D3) 'X)
;	       (otherwise 'F))
              (bpl-index date 9)
  ;           (case (channel-direction date 3);;feature 6
 ;              (IC+ 'B)
;	       ((US DC) 'L)
;	       ((BT) 'S)
;	       ((IC- DS) 'X)
;	       (otherwise 'F))
           (channel-direction date 3)
					;
	    (candle date 2);;;feature 7
          ;  (case (zero-strength date 8);;;feature 8
	  ;    ((BZT AZT DNT WDN SUP) 'B)
	  ;    ((3SU) 'L)	      
	  ;    ((LDD WUP) 'X)
	  ;    (otherwise 'F))
	     (zero-strength date 8) 
          
;            (case (channel-direction date 8) ;;feature 9 ***
;	     ((AT DC US UC BT2 DC- UC+) 'B)
 ;            ((UC- BT) 'L)	     
;	     ((DC+ AC BC DS) 'S)
;	     ((AT2 IC+ IC-) 'X)
;	     (otherwise 'F))
	   (channel-direction date 8)
	 
;	   (case (wpp date);;;feature 10
;	     ((OUH IUF) 'B)
;	     ((UDU ODH ODF DDD UDD IDL UUD) 'L)
;	     ((IUH DDD7 UUU UUU7) 'S)
;	     ((ODL  IDF) 'X)   
;	     (otherwise 'F))
            (wpp date)

	   (channel-direction date 8);feature 11
	   
	   
;	     (case (gann-slope-index date 5)   ;;;feature 12
;	       ((-3 -7) 'B)
;	       ((-2 -4 -6 4) 'L)
;	       ((-5 5 0) 'S)
;	       (2 'X)
;	       (otherwise 'F))
	    (gann-slope-index date 5)
 ;           (case  (pivot-turn date 'month);;feature 13
;	       ((S1 S3 PP)  'B)
 ;              ((R2 H R1)  'L)
;	       ((R3) 'S)
;	       ((L R0) 'X)
;	       (otherwise 'F))
	     (pivot-turn date 'month)
           ; (case (candle date 3)
	    ;  (DNHM 'B)
	     ; ((DNHR UPHR UPTW 0 UIHW DIHW) 'L)
	      ;((UPPC UPDJ) 'S)
	     ; ((DNMB UPMB UPHM DIST) 'X)
					; (otherwise 'F))
	    
;            (case (ep-roc-change-index date 13 1)  ;;;feature 14
;	       ((-3 4) 'B)
 ;              ((3 -4) 'L)
;	       ((2 -2) 'S)
;	       (otherwise 'F))
	     (ep-roc-change-index date 13 1)
           ;;  (my-round (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
            ;;                    (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
             ;;;do not have volume for day date-1 in real time feature 13
            ;;               (/ (ave (getd date-1 'ydate) 3 'volume)
            ;;                  (ave (getd date-1 'ydate) 21 'volume)) 1) 3)

           ;;  (my-round (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
            ;                    (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
             ;;;do not have openint for day date-1 in real time feature 14
            ;               (/ (ave1 (getd date-1 'ydate) 3 'openint)
            ;                  (ave1 (getd date-1 'ydate) 21 'openint)) 1) 3)
      
          
                  );;;closes the list

))
;;;;wpp1-composite does not work for swing trades.

;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-swingtrade-indicators (markets)
   (let (;(path ;(string-append *upper-dir-warehouse* "swingtradewarehouse.dat"))
          path1 date-1 date-2  )
  (maind-x)(set-cat-list)

 (dolist (market markets)
     (setq swings nil
           path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "swingtrades.dat"))

  (with-open-file (str path1 :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))

  (dolist (ith swings)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate) date-2 (getd date-1 'ydate))
 
     (setf (svref ith 10)(zero-strength date-1 8))
 ;      	     ((-3 2) 'L)
;	     (3 'S)
;	     ((-4 -1) 'X)
;	     (otherwise 'F)))
 
 ;    (setf (svref ith 16)(case (ep-roc-change-index date-1 5 2)
;			   ((-3 -2) 'L)
;			   (3 'S)
;			   (otherwise 'F)))
 
  ;   (setf (svref ith 6) (volatility-ratio-index  date-1 4 21)) ;;;feature 4
					; (setf (svref ith 15) (ep-macd-index date-1 12 26 9)) ;;;feature 13
 ;   (setf (svref ith 13)  ;;feature 11
;	   (case (accel-percentile date-1 13)
 ;          ((0 6) 'L)
;	   ((10 1) 'S)
;	   (otherwise 'F)))

;	     (1 (cond ((minusp (- (getd date-1 'close) (ave date-1 5))) 'L1)
;		      ((plusp (- (getd date-1 'close) (ave date-1 5))) 'L2)
;	               (t 0)))
;	     (otherwise 0)));;;feature 11*
;     (setf (svref ith 11) (volume-openint-div date-1 4)) ;;feature 9 
;	   (setf (svref ith 6);;
;		 (candle date-1 4));;feature 4
      
    ; (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
    ;                                             (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
    ;                              (/ (ave (getd date-1 'ydate) 3 'volume)
    ;                                 (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
   ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist over swings


  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth swings)
      (format str "~S~%" jth)))
);;;closes dolist over markets
  (build-swingtrade-warehouse markets)
  (indicator-study)
))

;;;enters with limit orders and exits with objectives with stop loss orders
;;;;counter-trend system
(defun swingtrade-simulation-test (market date2 num &optional (features *swing-features*))
 (let (date stop-long stop-short trades long short (grp *forex-list*) tsignal
       trade-long ave-loss ave-win extended-trades losers trade-short 
        prev-stop-long prev-stop-short entry-long entry-short 
        vplety vphety vplext vphext      
        (days-in-trade 0) vsig (ctr 0) vprev   risk     
        trading-dates (trade-time 0)   winners target-long target-short  
       epsignal longs long-gains long-acc shorts short-gains short-acc bin pf-longs pf-shorts 
  
        date-1 record (running-sum 0)  draw  date-2    
       (singles 0)  cover-long cover-short long-stopped-out short-stopped-out
       prev-entry-short prev-entry-long   ;(stockp (member market *stocks-list*))
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "swing-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "swing-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "swing-diary1.dat")))
       
 
   (set-market market)(format T "~%~A~%" *data-name*)
 
   (if (and num (> num (or (fmin (sub-mkt-dates 20120913 date2)
				 (available-days market date2 500)(available-days market date2 200)))))
       (setq num nil))
   (ifn num (setq num (fmin (sub-mkt-dates 20120913 date2)
			    (available-days market date2 500)(available-days market date2 330))))
                          

   (setq date (add-mkt-days date2 (- num)))

    (build-swingtrade-warehouse (setq grp *swing-list*));*micro-list*))
    (ifn features (setq features (find-best-swing-indicator-set nil)))
  
    (setq singles (nth-value 3 (apply #'swing-trade-binsb nil features))) 
   


  (cond ((member market *forex-warehouse-list*)(setq *commission* 0  *pips-slippage* 6  ))
        ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0  ))
        ((and (not (index-commodityp))(not (index-futurep)))
	 (setq *commission* 6 *pips-slippage* 0))
        ((member market *micro-list*) (setq *commission* 6 *pips-slippage* 0))
        (t (setq *commission* 10  *pips-slippage* 0 )))
  
    (setq *entry-factor-swing* 0.0 )
  
     (dotimes (ith num)

  ; (format T "~%1 ~A = date  ~A = short  ~A = long~%" date short long)
    (if (or long short) (incf days-in-trade))
 
    (multiple-value-setq (vplety vphety risk)(vprices date *duration* *factor* 1 *type*))   
    (multiple-value-setq (vplext vphext)(vprices date *duration* *factor* 1 *type*))
    (multiple-value-setq (vsig ctr vprev)(vsignals1 date ))
   	 
    
   
   (setq stop-long (if (eql vsig 'LONG) vprev (- vprev risk))
         stop-short (if (eql vsig 'SHORT) vprev (+ vprev risk))
         )
   ; (format T "~%2 ~A = date  ~A = short  ~A = long~%" date short long)
 
  ; (and short (setq stop-short (fmin prev-stop-short stop-short)))
   ;(and long (setq stop-long (fmax prev-stop-long stop-long)))

   (setq entry-long stop-short 
         entry-short stop-long)

  
 ;  (setq entry-short (fmax prev-entry-short entry-short) entry-long (fmin prev-entry-long entry-long))

;   (and short (setq prev-stop-short stop-short))
 ;  (and long (setq prev-stop-long stop-long))
     ; prev-entry-short entry-short         prev-entry-long entry-long)
        (setq cover-short target-short
           cover-long target-long)

;;;calculate bin-classifier 
   (setq tsignal (find-best-edge-swing-trade1 date nil (getd date 'ndate)))
 ;  (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin pf-longs pf-shorts)
  ;               (bin-classifier-swingtradesb date features))
  ; (format T "~%~A  ~A  ~A~%" date epsignal features)


	
      (setq date-1 date date (add-mkt-days date 1))
      (setq date-2 (getd date-1 'ydate))

       (setq record (list date (getd date 'close) 0 0 0))
       (if long (setf (nth 2 record) 1))
       (if short (setf (nth 2 record) -1))
 

 ;  (when  (/= (end-of-prior-month date)(end-of-prior-month date-1)) 
 ;        (build-swingtrade-warehouse grp (end-of-prior-month date-1))
;'         (setq features (find-best-swing-indicator-set nil))
';         (apply #'swing-trade-binsb nil features);)
    (if (getd date 'rollover) (setq entry-long (add entry-long (getd date 'rollover))
                                    entry-short (add entry-short (getd date 'rollover))
                                    stop-long (add stop-long (getd date 'rollover))
                                    stop-short (add stop-short (getd date 'rollover))
                                  
                                 ))
   ;  (format T "~%3 ~A = date  ~A = short  ~A = long stop-long = ~A stop-short = ~A~%"
    ;date short long stop-long stop-short)
    (when (and (getd date 'rollover) long stop-long)
             (setq long (+ long (getd date 'rollover));(comm+slip-points date))
                   cover-long (add cover-long (getd date 'rollover))
        	 ; stop-long (+ stop-long (getd date 'rollover)))
                )
             (setf (nth 3 record)(- (nth 3 record)
                  (* (calculate-point-value date market) (getd date 'rollover))))
        )

    (when (and (getd date 'rollover) short stop-short)
            (setq short (+ short (getd date 'rollover)); (- (comm+slip-points date)))
                  cover-short (add cover-short (getd date 'rollover))
    	         ; stop-short (+ stop-short (getd date 'rollover))
                  )
            (setf (nth 3 record) (+ (nth 3 record)
             (* (calculate-point-value date market) (getd date 'rollover))))
         )

; (format T "~%4 ~A = date  ~A = short  ~A = long  ~A = stop-long  ~A = stop-short ~A = entry-short~%"
 ;   date short long stop-long stop-short entry-short)
     ;;;check if need to ratchet stop
   (cond ((and long (>= (getd date 'high) entry-long)(eql (which-first date) 'high-first))
	  (setq stop-long (nth-value 1 (ratchet-stops date)) entry-short stop-long))
	 ((and short (<= (getd date 'low) entry-short) (eql (which-first date) 'low-first))
	  (setq stop-short (nth-value 0 (ratchet-stops date)) entry-long stop-short))
	 (t nil))
  
  ;  (format T "~%5 date =~A  short= ~A  long= ~A stop-long = ~A stop-short= ~A ~%" 
  ;             date short long stop-long stop-short)

;;;check if exit based on criteria at open of the day to take profit 

   (when  (and long
               (eql vsig 'SHORT)   
                            )
                  
        ;  (format T "~%Exited long with Criteria day = ~A long = ~A cover-long = ~A stop-long= ~A~%"
         ;          date long cover-long stop-long) 
          (push (round (- (* (my-pretty-price (- (getd date 'open) long))
                             (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'C
                                        (my-pretty-price (getd date 'open))
                                        (my-pretty-price (- (getd date 'open)  long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date market)(my-pretty-price (- (getd date 'open) (getd date-1 'close))))))

          (setq trade-long nil long nil stop-long nil  cover-long nil prev-stop-long nil
		days-in-trade 0 long-stopped-out T))

   (when  (and short
               (eql vsig 'LONG)
                )
                  
       ;   (format T "~%Exited short with Criteria day = ~A short = ~A cover-short = ~A stop-short=~A~%" 
          ;          date short cover-short stop-short)
          (push (round (- (* (my-pretty-price (- short (getd date 'open)))
                             (calculate-point-value date market))(comm+slip date))) trades)
           (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                          (contract-month *data-name* date) 'C
                                           (my-pretty-price  (getd date 'open))
                                           (my-pretty-price (- short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record)(+ (nth 3 record)
                (* (calculate-point-value date market)(my-pretty-price (- (getd date-1 'close)  (getd date 'open))))))

          (setq trade-short nil short nil stop-short nil cover-short nil prev-stop-short nil
		days-in-trade 0 short-stopped-out T))

 #|   
;;;means of entry long  on open price
   (when  (and (not long) (not long-stopped-out)
	       (eql vsig 'LONG)	 
                 )
                 (setq long  
                       (getd date 'open)		       
                 trade-long (list date 'long long) 
                 stop-long (nth-value 1 (ratchet-stops date))
            
                long-stopped-out nil  cover-long target-long prev-stop-short nil
                 prev-stop-long nil days-in-trade 1 
                 )
	  (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date market)
                           (my-pretty-price (- (getd date 'close) long (comm+slip-points date)))))))
 ;     (format T "~%3date = ~A" date)

                                
        
;;;check if new entry short on open price

   (when   (and (not short)(not short-stopped-out)
                (eql vsig 'SHORT)
                          )      
      (setq short 
	   (getd date 'open)
            trade-short (list date 'short short)
    
            stop-short (nth-value 0 (ratchet-stops date))        
            short-stopped-out nil cover-short target-short
            prev-stop-short nil days-in-trade 1 
            prev-entry-short entry-short prev-stop-long nil
            ) 
        (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date market)
              (my-pretty-price  (- short (getd date 'close)(comm+slip-points date)))))))
 |# 
;;;check if stopped out of prior position #1(no stop loss for counter trend system)

   (when (and long stop-long (<= (getd date 'low) stop-long))
         ;  (format T "~%6 date = ~A long = ~A low = ~A stop-long = ~A trade-long = ~A"
         ;  date long (getd date 'low) stop-long trade-long)
     (setq stop-long (if (eql (which-first date) 'low-first)
			 (min stop-long (getd date 'open)) stop-long))
         (push (round (- (* (my-pretty-price (- stop-long long))
                        (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
             (append trade-long  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                      (contract-month *data-name* date) 'S
                                      (my-pretty-price  stop-long)
                                    (my-pretty-price (-  stop-long long))))))

           (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
               (*  (calculate-point-value date market)(my-pretty-price (- stop-long (getd date-1 'close))))))
          (setq trade-long nil long nil days-in-trade 0 stop-long entry-short prev-stop-long nil long-stopped-out T
                prev-entry-long nil ))

   (when (and short stop-short (>= (getd date 'high) stop-short))
         ; (format T "~%7 date = ~A short = ~A high = ~A stop-short = ~A trade-short = ~A"
          ;   date short (getd date 'high) stop-short trade-short)
     (setq stop-short (if (eql (which-first date) 'high-first)
	                  (max stop-short (getd date 'open)) stop-short))
          (push (round (- (* (my-pretty-price (- short stop-short))
                      (calculate-point-value date market))(comm+slip date))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))
                    (contract-month *data-name* date) 'S
                   (my-pretty-price  stop-short )
                   (my-pretty-price (- short stop-short))))))
          (push trade-short extended-trades);(format T "~%10 ~A = trade-short" trade-short)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
				  (* (calculate-point-value date market)  (my-pretty-price (- (getd date-1 'close) stop-short)))))
          (setq trade-short nil short nil days-in-trade 0 stop-short entry-long prev-stop-short nil short-stopped-out T
                prev-entry-short nil))


;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (+ (nth 3 record)
                           (* (calculate-point-value date market)
                              (my-pretty-price (- (getd date 'close) (getd date-1 'close)))))))
      (if short (setf (nth 3 record)(+ (nth 3 record)
                (* (calculate-point-value date market)
                   (my-pretty-price (- (getd date-1 'close)(getd date 'close)))))))

     ; (format T "~%13 ~A = date  ~A = entry-short  ~A = entry-long ~A = stop-long "
      ;      date entry-short entry-long stop-long)
    ; (print record)


;;;;2nd type of entry short at entry
     (when  
       	 (and (not short) entry-short  (<= (getd date 'low) entry-short)
	      (eql vsig 'LONG)(equal tsignal 'SELL-AT-ENTRY)
              )
 
          (setq short   
                (fmin (getd date 'open) entry-short) 
                
                 trade-short (list date 'short short)
                 cover-short nil target-short nil
                 stop-short vprev short-stopped-out nil
                 prev-stop-short nil prev-entry-short entry-short prev-stop-long nil
                 days-in-trade 1)
;(format t "~%7date = ~A  short = ~A epsignal = ~A shorts = ~A short-gains = ~A pf-shorts = ~A short-acc = ~A"
 ;          date  short epsignal shorts short-gains pf-shorts short-acc)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date market)
              (my-pretty-price  (- short (getd date 'close)(comm+slip-points date))))))
              )


;;;;entry long #2
     (when  (and (not long)  entry-long (>= (getd date 'high) entry-long)
                 (eql vsig 'SHORT)(equal tsignal 'BUY-AT-ENTRY)
		 )	    

      (setq long (fmax (getd date 'open) entry-long)   
    
                 trade-long (list date 'long long)
                 cover-long nil  target-long nil 
                 stop-long vprev long-stopped-out nil
                 prev-stop-long nil  prev-entry-long entry-long prev-stop-short nil
                 days-in-trade 1)
 ; (format t "~%8date = ~A  long = ~A epsignal = ~A longs = ~A long-gains = ~A pf-longs = ~A long-acc = ~A"
  ;         date long epsignal longs long-gains pf-longs long-acc) 
       
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date market)
                           (my-pretty-price (- (getd date 'close) long (comm+slip-points date))))))
             )
    
   ;  (format T "~%4 ~A = date  ~A = short  ~A = long" date short long)
    ; (print record)

;     (format T "~%4 ~A = date  ~A = short  ~A = long" date entry-short entry-long)
;     (format T "~%4a ~A = epsignal  ~A = long  ~A = short stop-long = ~A" epsignal long short stop-long)
   ;  (print record)

#|    
;;;check if exited with criteria
;;;;both long and short. One needs to exit.
     (when (and long short (>= bear-cntr bull-cntr)) 
           (setq stop-long (getd date 'open))
           (setq stop-long (my-pretty-price stop-long))
           (push (round (- (* (my-pretty-price (-  stop-long long))
                              (calculate-point-value date market))(comm+slip date))) trades) 
;           (format T "~% Trades = ~A" trades)
          (setq trade-long (apply #'vector
                  (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'CL
                                           stop-long (my-pretty-price (- stop-long long))))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
 ;          (setf (nth 2 record) 1)
;           (format T "11 stop-long = ~A long = ~A" stop-long long)
           (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date market) (my-pretty-price (- stop-long long)))))
           (setf (nth 3 record) (- (nth 3 record)
              (* (calculate-point-value date market)(my-pretty-price (- (getd date 'close) long)))))
 
          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))

    (when (and short long (>= bull-cntr bear-cntr))
       
           (setq stop-short (getd date 'open))
      
           (setq stop-short (my-pretty-price stop-short))
           (push (round (- (* (my-pretty-price (- short stop-short))
                              (calculate-point-value date market))(comm+slip date))) trades)
           (setq trade-short (apply #'vector 
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'CS
                                            stop-short (my-pretty-price (- short stop-short))))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
   ;        (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date market) (my-pretty-price (- short stop-short)))))
           (setf (nth 3 record) (- (nth 3 record)
                  (* (calculate-point-value date market)(my-pretty-price (- short (getd date 'close))))))
 
           (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0 ))
   
       |#
 
;;;check if stopped out on same day of entry
    (when (and long (setq stop-long (nth-value 1 (ratchet-stops date))) 
               (<= (getd date 'low) stop-long)
	       (eql (which-first date) 'high-first)        
                       )
           (setq stop-long (my-pretty-price stop-long))
           (push (round (- (* (my-pretty-price (-  stop-long long))
                              (calculate-point-value date market))(comm+slip date))) trades) 
;           (format T "~% Trades = ~A" trades)
          (setq trade-long (apply #'vector
                  (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'SS
                                           stop-long (my-pretty-price (- stop-long long))))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
 ;          (setf (nth 2 record) 1)
;           (format T "11 stop-long = ~A long = ~A" stop-long long)
           (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date market) (my-pretty-price (- stop-long long)))))
           (setf (nth 3 record) (- (nth 3 record)
              (* (calculate-point-value date market)(my-pretty-price (- (getd date 'close) long)))))
 
           (setq trade-long nil long nil stop-long entry-short prev-stop-long nil
		 long-stop-out T days-in-trade 0))

    (when (and short (setq stop-short (nth-value 0 (ratchet-stops date))) 
	        (>= (getd date 'high) stop-short)
	       (eql (which-first date) 'low-first)
                    )
           (setq stop-short (my-pretty-price stop-short))
           (push (round (- (* (my-pretty-price (- short stop-short))
                              (calculate-point-value date market))(comm+slip date))) trades)
           (setq trade-short (apply #'vector 
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'SS
                                            stop-short (my-pretty-price (- short stop-short))))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
   ;        (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date market) (my-pretty-price (- short stop-short)))))
           (setf (nth 3 record) (- (nth 3 record)
                  (* (calculate-point-value date market)(my-pretty-price (- short (getd date 'close))))))
 
           (setq trade-short nil short nil stop-short entry-long prev-stop-short nil
		 short-stopped-out T days-in-trade 0 ))


#|
;;;check if exited with objective on day of entry
   (when  (and long cover-long (> (getd date 'high) cover-long))
          (push (- (* (-  (max (getd date 'open) cover-long)  long)
                     (calculate-point-value date-1 market))(comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                        (- (max (getd date 'open) cover-long) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record ) (+ (nth 3 record )
               (* (calculate-point-value date market)
                  (my-pretty-price (-  cover-long (getd date 'close))))))
         
         ; (setf (nth 3 record) (- (nth 3 record)
         ;       (* (calculate-point-value date market)(my-pretty-price (- (getd date 'close) long)))))

       ; (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))

          (setq trade-long nil long nil stop-long nil cover-long nil days-in-trade 0 target-long t))

   (when  (and short cover-short (< (getd date 'low) cover-short))
          (push (- (* (- short (min (getd date 'open) cover-short))
                   (calculate-point-value date-1 market))(comm+slip date-1)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                                      (- short (min (getd date 'open) cover-short))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))

          (setf (nth 3 record) (+ (nth 3 record)
                   (* (calculate-point-value date market)
                      (my-pretty-price (- (getd date 'close)  cover-short)))))
;;;;
      ;    (setf (nth 3 record) (- (nth 3 record)
       ;             (* (calculate-point-value date market)(my-pretty-price (- (getd date-1 'close) (getd date 'close))))))
          
      ; (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade-short nil short nil stop-short nil cover-short nil target-short t ))
|#

					;  (format T "~%5 ~A = date  ~A = short  ~A = long ~A" date short long trade-long)


;;;;this is the second entry for the day if stopped out on same day as first entry
      (when  
       	  (and (not short) stop-long long-stopped-out
	      (<= (getd date 'low) stop-long)
                (eql (which-first date) 'high-first)
                  (equal tsignal 'SELL-AT-ENTRY)                                        
       
                   );;entry short closes the and

          (setq short  stop-long 
            
                 trade-short (list date 'short short)
                 cover-short nil target-short nil
                 stop-short vphext
                 prev-stop-short nil prev-entry-short entry-short prev-stop-long nil
                 days-in-trade 1)
;(format t "~%7date = ~A  short = ~A epsignal = ~A shorts = ~A short-gains = ~A pf-shorts = ~A short-acc = ~A"
 ;          date  short epsignal shorts short-gains pf-shorts short-acc)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date market)
              (my-pretty-price  (- short (getd date 'close)(comm+slip-points date))))))
              )
;;new entry long 
 
    (when  (and (not long) stop-short  short-stopped-out
                (>= (getd date 'high) stop-short)
		(eql (which-first date) 'low-first)
		(equal tsignal 'BUY-AT-ENTRY)
     
                             );;second entry long

      (setq long stop-short
                 trade-long (list date 'long long)
                 cover-long nil  target-long nil 
                 stop-long vplext long-stopped-out nil
                 prev-stop-long nil  prev-entry-long entry-long prev-stop-short nil
                 days-in-trade 1)
 ;    (format t "~%8date = ~A  long = ~A epsignal = ~A longs = ~A long-gains = ~A pf-longs = ~A long-acc = ~A"
 ;          date long epsignal longs long-gains pf-longs long-acc) 

           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date market)
                           (my-pretty-price (- (getd date 'close) long (comm+slip-points date))))))
             )
   ;  (format T "~%4 ~A = date  ~A = short  ~A = long" date short long)
    ; (print record)
           (setf (nth 4 record) (+ (nth 3 record) running-sum))
           (setq running-sum (nth 4 record))

       (push record trading-dates)
       (setq record nil);;;reset to nil at end of each trading date

       
     ) ;;;closes the dotimes

;;;;writes out the diary
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (first ith) (second ith) (third ith) (round (nth 3 ith))
           (round (nth 4 ith)))
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
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F    COMMISSION=   ~A~%~
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
      (if (zerop *commission*) *pips-slippage* *commission*)
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
;;;use the EXIT date to calculate-point-value NOT start date
    (format stream "~A\,~A\,~F\,~A\,~A\,~A\,~A\,~F\,~F\,~D~%"
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)
         (svref ith 7)(svref ith 8)(round (* (calculate-point-value (svref ith 3) market) (svref ith 8))))

     ));;closes the dolist and with-open-file
  ; ); closes the when

    (values (round  (list-sum trades))
            (length trades) trades)
   ));;;closes the let and the defun


(defun display-swing-contents (date direction &optional (features *counter-swing-features*))
 (let (bin)
   (setq bin (nth-value 7 (bin-classifier-swingtradesb date features)))
    (print bin)
   (setf (car bin) direction)
   (display-swing-bin bin)
))


(defun display-swing-bin (bin)
  (let (contents)
     (setq contents (gethash bin *swing-trade-warehouse*))     
     (dolist (ith contents)
       (print ith))))

(defun build-swingtrade-warehouse (markets &optional (nxdate 99999999))
  (let ((path-out (string-append *upper-dir-warehouse* "swingtradewarehouse.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "swingtradewarehouse.backup"))
           (delete-file (string-append *upper-dir-warehouse* "swingtradewarehouse.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "swingtradewarehouse.dat"))
          (rename-file (string-append *upper-dir-warehouse* "swingtradewarehouse.dat")
                            "swingtradewarehouse.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "swingtrades.dat")) 
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



;;;;this function expects that you have run (swing-trade-bins ...) already
(defun display-swingtrade-bins-by-expected-value (feature)
  (let (contents expected-value-list result (winners 0)(losers 0) longs shorts
        num-trades first-date last-date
        (path1 (string-append *output-upper-dir* "swingtrade-expected-value"
                 (format nil "~A" feature) ".dat")))
  
    (multiple-value-setq (num-trades first-date last-date )(num-trades-in-warehouse3 'all swings))
   (format T "~%NUM-TRADES= ~A" num-trades)
    (dolist (ith swing-bin-codes)
     (setq contents
           (gethash ith *swing-trade-warehouse*))
     (setq result 0 winners 0 losers 0)
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19))(incf winners)(incf losers)))
     (setq expected-value-list 
           (cons (list (/ result (length contents)) ith (length contents) result winners losers) expected-value-list))
     );;;closes the dolist over bin-codes

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
              (swingtrade-chi-squared-gof)) 
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


;;;reads the swingtradewarehouse3.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-swing-trades (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "swingtradewarehouse.dat")) trades)

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


(defun remove-swingtrade-market (market)
  (let (trades path)
   (setq path (string-append *upper-dir-warehouse* "swingtradewarehouse.dat"))
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



(defun swing-trade-bins (&optional (stream T) &rest features)
  (let (bin path)
   (if *stocks* (setq path (string-append *upper-dir-warehouse* "stocksswingwarehouse.dat"))
       (setq path (string-append *upper-dir-warehouse* "swingtradewarehouse.dat")))
 
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
    (format stream  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profit swings stream)
  ))

;;;This rank is for using multiple length features

(defun rank-swingtrade-bins-by-profit3b (swings features &optional (str T))
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0);(twr 1)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0)
        longest-swing-bin-codes (longest-feature (length features)))
   (setq longest-swing-bin-codes (remove-if #'(lambda (s) (< (length s) longest-feature)) swing-bin-codes))
   (dolist (ith longest-swing-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
     (setq result 0 counter 0 )

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
       ;  (setq twr (* twr (+ 1 (/ (if (plusp (svref kth 2))
        ;                         (- (svref kth 18) (svref kth 3))
         ;                      (- (svref kth 3) (svref kth 18)))
          ;                   (svref kth 3)))))

         ) ;;;closes dolist over contents
      (if (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))

     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth swings)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (plusp (car jth))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format str "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swings)(length (num-markets-in-warehouse3 swings)))
     (format str "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format str "NUMBER of ALL BINS = ~D~%" (length longest-swing-bin-codes))
     (format str "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swings)(length longest-swing-bin-codes)) 1))
     (format str "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format str "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format str "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format str "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format str "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format str "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length longest-swing-bin-codes)) 2))
     (format str "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length swings)) 2))

    (values (round winners)(round (+ all-winners all-losers))
           (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
           (my-round (/ only-one (length swings)) 2)) 
       

 ))


(defun swing-trade-binsb ( &optional (stream T) &rest features)
  (let (bin path)

      (setq path (string-append *upper-dir-warehouse*  "swingtradewarehouse.dat"))
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
     (dotimes (ith (- (length features) 1))
         (setq bin (encode-swing-trades record (butlast features ith)))
         
          (cond ((gethash bin *swing-trade-warehouse*)
            (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse*)
                       (cons record (gethash bin *swing-trade-warehouse*)))))
            ((not (gethash bin *swing-trade-warehouse*))
             (setf (gethash bin *swing-trade-warehouse*) (list record))
              (push bin swing-bin-codes)))
          ));;;closes the doltimes and dolist

    (format stream  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profit3b swings features stream)


  ))


;;;requires a base features list
(defun swings-leave-one-out (base-features &optional (str t))
  (let (winners-list (result 0) rtb average-profit single-bins rp csgof)
 
  (dolist (ith base-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
                    (apply #'swing-trade-bins str (remove ith base-features)))

    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
 ;  (vsort winners-list #'> 'seventh)
  (vsort winners-list #'> 'car)
 ; (vsort winners-list #'> 'sixth)
  (format str "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format str "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format str "Winners List = ~A ~%" winners-list)
  winners-list
 ))

#|
;;;requires a base features list
(defun swings-leave-one-out (base-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq *stocks* T))
  (apply #'swing-trade-bins base-features)

  (dolist (ith base-features)
    (setq result (apply #'swing-trade-bins (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 winners-list
 ))
|#
;;;requires a candidate list to add to the base features
;;;rtp is 
(defun swings-add-one-in (base-features candidate-features &optional (str t))
 (let (winners-list (result 0) average-profit rtb rp single-bins csgof)  

  (dolist (ith candidate-features)
    (multiple-value-setq (result rtb average-profit single-bins rp csgof)
           (apply #'swing-trade-bins str (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length day-bin-codes)))))
;     (setq result (daytrade-chi-squared-gof))
    (setq winners-list (cons (list result ith average-profit single-bins rtb rp csgof) winners-list))
    )
   
;   (vsort winners-list #'> 'first)
 ;   (vsort winners-list #'> 'third)
    (vsort winners-list #'> 'seventh)
   (format str "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format str "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format str "Winners List = ~A ~%" winners-list)
  winners-list
 ))
;;;;;



(defun rank-swingtrade-bins-by-profit (swings &optional (str t))
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0) 
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
 
   (dolist (ith swing-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
     (setq result 0 counter 0 )
     
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
       
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
      ;      (setq twr (* twr (+ 1L0 (/ (* (svref kth 2)(- (svref kth 18) (svref kth 3)))
     ;                              (svref kth 3)))))
        
      ) ;;;closes dolist over contents
 
    (if (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))
 
     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over bin-codes
 
    (dolist (kth swings)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))
 
     (setq counter 0)
     (dolist (jth profit-list)
         (if (plusp (car jth))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))
 
     (format str "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swings)(length (num-markets-in-warehouse3 swings)))
     (format str "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format str "NUMBER of ALL BINS = ~D~%" (length swing-bin-codes))
     (format str "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swings)(length swing-bin-codes)) 1))
     (format str "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format str "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format str "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format str "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format str "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format str "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length swing-bin-codes)) 2))
     (format str "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length swings)) 2))
     (format str "CHI SQUARE METRIC = ~5F~%" (round (swingtrade-chi-squared-gof)))

;    (values (round winners)(round (+ all-winners all-losers)))

;    (values (round winners)
;            (round (+ all-winners all-losers))
;           (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
;           (my-round (/ only-one (length swings)) 2)(my-round (/ winners all-winners) 2)
;           (swingtrade-chi-squared-gof)) 



   (values (round winners)
            (my-round (/ (length swings)(length swing-bin-codes)) 1);;;ratio of trades to bins
           ;  (round (+ all-winners all-losers))
            (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))

            (my-round (/ only-one (length swings)) 2)
            (my-round (/ winners all-winners) 2)
             (swingtrade-chi-squared-gof)) 
 


 ))


 

#|
(defun find-best-swing-indicator-set ()
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (swingtrades-add-one-in base-list candidate-list))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;
     (if (or (not candidate-list)(> result .500))
         (return))
    )
    (format T "First stage winners-list= ~A"  winners-list)

;   (loop     
;      (if (> (first (car winners-list)) .50)(return))
;      (setq winners-list (daytrades-leave-one-out3a base-list))
;      (if (neql (cdr (car winners-list)) 1)
;              (setq base-list (remove (second (car winners-list)) base-list))
;              (setq base-list (remove (second (second winners-list)) base-list)))
;         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
 base-list
    ))
 

|#

(defun swing-time-in-trade (swings)
  (let ((days 0)(winning-trade-days 0)(losing-trade-days 0)(winning-trades 0)(losing-trades 0))
  (dolist (ith swings)
    (set-market (svref ith 0))
  ;  (format T "~%~A  ~A  ~A" (svref ith 0) (svref ith 1)(svref ith 17))
    (setq days (+  (sub-mkt-dates (svref ith 1)(svref ith 17)) days))
    (if (plusp (svref ith 19)) (setq winning-trade-days
                                  (+ (sub-mkt-dates (svref ith 1)(svref ith 17)) winning-trade-days)
                                      winning-trades (1+ winning-trades))
         (setq losing-trade-days (+ (sub-mkt-dates (svref ith 1)(svref ith 17)) losing-trade-days)
               losing-trades (1+ losing-trades))))

  (values (float (/ days (length swings))) (float (/ winning-trade-days winning-trades))
           (float (/ losing-trade-days losing-trades)))

))



(defun swing-trades (&optional tdate (market-list *swing-list*)(holidayp nil)(outfile t))
   (let (date path1 path2 (counter 0)
         profit trades new-profit new-trades trade-list  bin date1 
            csignal longs long-gains long-acc shorts short-gains short-acc ) 
       (declare (ignore profit trades new-profit new-trades trade-list))
       (declare (special csignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin))

       (ifn tdate (setq tdate (car (last (month-days (get-latest-index-date))))))
       (setq date tdate)

      ; (build-swingtrade-warehouse *micro-list*)
					; (setq *swing-features* (find-best-swing-indicator-set nil))
       
      ; (apply #'swing-trade-binsb T *swing-features*)
      
        (setq path1  (string-append *daily-output-dir* "counterswingfx.csv")
             ; path3 (string-append *daily-output-dir* "edgeswingfx.csv")  
              path2 (string-append *daily-output-dir* "edgeswingfut.csv"))
     
    
        (if (and outfile (probe-file path1))
            (delete-file path1))
        (if (and outfile (probe-file path2))
            (delete-file path2))
        ;(if (and outfile (probe-file path3))
					;   (delete-file path3))

	 (build-swingtrade-warehouse market-list);(swing-trade-binsb nil 1 2)
	; (setq *swing-features* (find-best-swing-indicator-set nil))
         (apply #'swing-trade-binsb T *swing-features*)
#|          
         (with-open-file (stream path3 :direction :output :if-exists :append :if-does-not-exist :create)

        (dolist (market market-list)
           (set-market market)
           
           (ifn tdate (setq date (car (last (month-days (get-latest-index-date))))))
           (if (and tdate (readt2 tdate)) (setq date tdate))
           (if (and tdate (not (readt2 tdate)))
               (setq date (car (last (month-days( (get-latest-index-date))))))
           (setq date1 (if (or holidayp (not (readt2 tdate)))
                           (if (or (string-equal (day-of-week date) "friday")
                                   (string-equal (day-of-week date) "thursday"))
                                (add-days-to-date date 4)(add-days-to-date date 2))
                     (if (string-equal (day-of-week date) "friday")(add-days-to-date date 3)
                          (add-days-to-date date 1))))
         
         ; (multiple-value-setq (csignal longs long-gains long-acc shorts short-gains short-acc bin)
          ;       (bin-classifier-swingtradesb date *counter-swing-features*))
            (find-best-edge-swing-trade date stream date1) 
        );;;closes the dolist
       
         );;;closes the stream for forex trades
|#
       (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream1 " MARKET ACTION    , SYMBOL,   FOR DATE ,  ENTRY PRICE, STOP LOSS LONGS, STOP LOSS SHORTS  Risk,    CLOSE~2%")
        (dolist (market market-list)
           (incf counter)(set-market market)
           
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

	;   (build-swingtrade-warehouse (list market))(swing-trade-binsb nil 1 2)
	 ;  (if (< (num-trades-in-warehouse3 market swings) 75)
	;	  (build-swingtrade-warehouse market-list))
		  
	  
	;   (apply #'swing-trade-binsb T *swing-features*)
       
  ;       (find-best-edge-swing-trade date stream1 date1)  
         (find-best-edge-swing-trade1 date stream1 date1)  
        (if (zerop (mod counter 4)) (terpri stream1))
          );;;closes the dolist
       ;  (format stream1 "~5% ALL ENTRY ORDERS ARE STOPS. ~
       ;      ONLY ADJUST STOP LOSS ORDER PRICES ON OPEN TRADES IF CLOSER TO MARKET.~%")
        ; (format stream1 "~% ALL ENTRY ORDERS ARE STOPS. ONLY ADJUST STOP LOSS ORDER PRICES IF CLOSER TO MARKET.~%")
          );closes the stream1
          

  #|     (with-open-file (stream2 path2 :direction :output :if-exists :append :if-does-not-exist :create)

        (dolist (market *day-list*)
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
                
         
          (find-best-counter-swing-trade date stream2 date1)
          );;;closes the dolist
          );closes the stream2
|#         
   
 
     ;  (write-trend-swing-record output tdate date1 tsignal entry-price stop-long stop-short objective-price )
))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-swingtradesb (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .382)(crit-pf 2.618)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))(pf-longs 0)(pf-shorts 0)
        (num-winners-long 0)(num-winners-short 0)(gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0))
    (setq record (create-swingtrade-entry-record-list date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))
  ; (format t "~%record= ~A" record)
     (dotimes (ith (- (length features) 1))
    (setq bin (encode-swing-trades record features))
  ;   (format t "~%BIN= ~A ith= ~A features= ~A" bin ith features)
    (setq contents (gethash bin *swing-trade-warehouse*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
  ;   (format t "~%Long contents= ~A" contents)
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *swing-trade-warehouse*) contents))
  ;   (format t "~%both contents= ~A" contents)
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (eql nxdate (svref s1 1))) contents))
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

;   (format t "results-short = ~A results-long = ~A gains-long/losses-long = ~A ~
;           gains-long/longs =~A gains-short/shorts = ~A" results-short results-long
;           (float (/ gains-long (abs losses-long))) (float (/ gains-long longs))(float  (/ gains-short shorts)))
;   (format t "~%num-winners-long = ~A num-winners-short= ~A  gains-long = ~A gains-short= ~A" 
;            num-winners-long num-winners-short gains-long gains-short)
					;   (format t "~%losses-long = ~A losses-short = ~A" losses-long losses-short)
   (setq pf-shorts (float (/ gains-short (if (zerop losses-short) 1 (abs losses-short))))
         pf-longs (float (/ gains-long (if (zerop losses-long) 1 (abs losses-long)))))
   
  (cond  ((not contents) (setq epsignal 'UNIQUE))
         ((and (<= results-short 0)(<= results-long 0)) (setq epsignal 'AVOID))

          ((and (plusp results-long)(plusp results-short)
               (>  pf-longs crit-pf)
               (>= (float (/ num-winners-long longs)) crit-acc)
               (>  pf-shorts crit-pf)
               (>= (float (/ num-winners-short shorts)) crit-acc)
	      ; (>= longs 5)(>= shorts 5)
                )
              (setq epsignal 'OK))
 
          ((and (plusp results-long)
              (> (/ results-long (if (zerop longs) 1 longs))
                 (/ results-short (if (zerop shorts) 1 shorts)))
               (> pf-longs crit-pf)
               (>= (float (/ num-winners-long longs)) crit-acc)
	       ;(>= longs 5)
              )
          
              (setq epsignal 'UP))

        ((and (plusp results-short)
              (< (/ results-long (if (zerop longs) 1 longs))
                 (/ results-short (if (zerop shorts) 1 shorts)))
             
              (>  pf-shorts  crit-pf)
              (>= (float (/ num-winners-short shorts)) crit-acc)
	      ;(>= shorts 5)
               )
           
         (setq epsignal 'DOWN))       
       
        (t (setq epsignal 'AVOID)))

  (values epsignal longs results-long (if (zerop longs) 0 (float (/ num-winners-long longs)))
         shorts results-short
          (if (zerop shorts) 0 (float (/ num-winners-short shorts)))
          bin pf-longs pf-shorts)
))

(defun find-swing-trade-signal (tdate)

 (let* ((date tdate) tsignal    
           cci5 cci10 ccid10-2 ccil10 zs3 zs5 zs10 zs21 vplety vphety vplext vphext vri3 vri5 
           ldev5 refl5 refl8 avel3 aveh3
           bpl13 bpl5 bpl8 can ldev3 chandir5 chandir13 chandir21 chandir3 chandir8
           modiv5 modiv8 pivw rsi2 si tls4-8 wpi
          ; epsignal ;longs long-gains long-acc shorts short-gains short-acc bin
           ;cover-short cover-long 
           
            entry-long entry-short 
           stop-long stop-short          
           )
         
;  (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 0 ))
;          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 ))
;          ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 ))
;          (t (setq *commission* 25 *pips-slippage* 0  )))
      
         (setq  ccid10-2 (roc date 2 'cci 10)
              cci5 (commodity-channel-index date 5) cci10 (commodity-channel-index date 10)
              zs3 (zero-strength date 3) zs5 (zero-strength date 5) zs10 (zero-strength date 10)  
              vri3 (volatility-ratio-index date 3 63) vri5 (volatility-ratio-index date 5 63)  
              refl5 (reflect2 date 5) refl8 (reflect2 date 8)   
              bpl5 (bpl-index date 5) bpl8 (bpl-index date 8) bpl13 (bpl-index date 13) 
              ldev3 (ldev-index date 3) ldev5 (ldev-index date 5) rsi2 (rsi2x-ob-os date 2)
              tls4-8 (timing-line-signal3 date 4 8) ccil10 (cci-level-index date 10) 
              si (ww-swing-index1 date) avel3 (ave date 3 'low) aveh3 (ave date 3 'high)
               )
    
         (multiple-value-setq (vplext vphext)(vprices tdate *duration* *factor* 1 *type*))
          (multiple-value-setq (vplety vphety)(vprices tdate *duration* *factor* 1 *type*))
          (setq entry-long (fmin vphety aveh3) 
                entry-short (fmax vplety avel3))
    
          
          (setq stop-long (fmax vplext avel3) stop-short (fmin vphext aveh3))

   ;  (if (or (not (readt2 date))(< date sdate)) (return))
   ;   (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
    ;              (bin-classifier-swingtradesb date features))
 
   ;  (setq cover-short avel cover-long aveh)
 
     (cond ((and  (/= vri3 6)(plusp ccid10-2)(plusp bpl5)(/= bpl8 -1)(/= bpl13 -3)
                  (not (member vri5 '(5 6)))
                  (< rsi2 1) (/= tls4-8 -3)(not (member can '(DNST DNHM DNDJ DNTW)))
                 ; (not (member tls5-10 '(1 0 -4 -1 5 -5 -2)))
                  (/= ccil10 0)(/= ccil10 -1)
                  (/= ldev3 0)(not (member wpi '(DDD7 ODF DUD UDU UUU IDL IDH)))
                  (/= ldev5 4)(/= ldev5 -4)(/= ldev5 3)
               
               
                  (not (member zs3 '(SDN LDD 3SU)))
                  (member zs5 '(LDD SUP UPT 3SD)) 
                  (not (member zs10 '(WDN 3SD AZT BZT)))
                  (not (member zs21 '(BZT WUP DNT)))
                    

                (> si -2)(not (member pivw '(-4 -5)))
                (/= refl5 -1)(/= refl5 0)(not (member refl8 '(0 -1))) 
               (not (member chandir21 '(IC- IC+ DC DC+ BC UC+)))
               (not (member modiv5 '(FT)))(not (member modiv8 '(FT)))
               (not (member chandir8 '(DC- AC BT)))(not (member chandir5 '(IC+)))
               (not (member chandir3 '(AC DC)))(not (member chandir13 '(IC+ US)))
             ; (member epsignal '(DOWN OK)) 
 
 
            )
            (setq tsignal 'SELL-AT-STOP-ENTRY ))
        
           ((and  
                (/= vri3 6)(minusp ccid10-2)(minusp bpl5)(/= bpl8 1)(/= bpl13 3)
                (not (member vri5 '(5 6)))
                (> rsi2 -1)  (/= tls4-8 3) (not (member can '(UPPC UPHM UIST)))    
            
                (/= ccil10 0)(/= ccil10 1)
                (/= ldev3 0) (not (member wpi '(ODF IDH OUH IUH IUF OUL OUF)))
                (/= ldev5 4)(/= ldev5 -4)(/= ldev5 -3)
             
            
                (not (member zs3 '(3SD WDN 3SU)))
                (member zs5 '(3SD SDN 3SU DNT))
                (not (member zs10 '(BZT AZT 3SD LDU)))
                (not (member zs21 '(BZT WUP LDD)))
 
                 (< si 2)(not (member pivw '(4 5)))
                 (/= refl5 1)(/= refl5 0)(not (member refl8 '(0 1)))
                 (not (member chandir21 '(IC+ AT2 BC DC- AT)))
                 (not (member modiv5 '(FT)))(not (member modiv8 '(FT)))
                 (not (member chandir5 '(AC)))(not (member chandir8 '(AT2 IC+)))
                 (not (member chandir3 '(UC+)))(not (member chandir13 '(DC+)))
        

                 )
            (setq tsignal 'BUY-AT-STOP-ENTRY )
            )
        
          ; ((>= ccis5 0) (setq tsignal 'HOLD-LONG 
          ;                            objective-price 0))
          ; ((<= ccis5 0) (setq tsignal 'HOLD-SHORT entry-short 0 
          ;                           objective-price 0)))
           )
      tsignal
))

;;;;
(defun find-best-edge-swing-trade (tdate &optional (output T)(date1 nil))
  (let* ((date tdate) tsignal  
           cci5 cci10 ccid10-2 ccil10 zs3 zs5 zs10 zs21 vplety vphety vplext vphext vri3 vri5 
           ldev5 refl5 refl8 avel3 aveh3
           bpl13 bpl5 bpl8 can ldev3 chandir5 chandir13 chandir21 chandir3 chandir8
           modiv5 modiv8 pivw rsi2 si tls4-8 wpi
          ; epsignal ;longs long-gains long-acc shorts short-gains short-acc bin
           ;cover-short cover-long 
          
           entry-long entry-short objective-price entry-price stop-loss
           stop-long stop-short          
           )
         
;  (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 0 ))
;          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 ))
;          ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 ))
;          (t (setq *commission* 25 *pips-slippage* 0  )))
      
         (setq  ccid10-2 (roc date 2 'cci 10)
              cci5 (commodity-channel-index date 5) cci10 (commodity-channel-index date 10)
              zs3 (zero-strength date 3) zs5 (zero-strength date 5) zs10 (zero-strength date 10)  
              vri3 (volatility-ratio-index date 3 63) vri5 (volatility-ratio-index date 5 63)  
              refl5 (reflect2 date 5) refl8 (reflect2 date 8)   
              bpl5 (bpl-index date 5) bpl8 (bpl-index date 8) bpl13 (bpl-index date 13) 
              ldev3 (ldev-index date 3) ldev5 (ldev-index date 5) rsi2 (rsi2x-ob-os date 2)
              tls4-8 (timing-line-signal3 date 4 8) ccil10 (cci-level-index date 10) 
              si (ww-swing-index1 date) avel3 (ave date 3 'low) aveh3 (ave date 3 'high)
               )
    
         (multiple-value-setq (vplext vphext)(vprices date *duration* *factor* 1 *type*))
          (multiple-value-setq (vplety vphety)(vprices date *duration* *factor* 1 *type*))
          (setq entry-long (fmin vphety aveh3) 
                entry-short (fmax vplety avel3))
    
          
          (setq stop-long (fmax vplext avel3) stop-short (fmin vphext aveh3))

   ;  (if (or (not (readt2 date))(< date sdate)) (return))
   ;   (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
    ;              (bin-classifier-swingtradesb date features))
 
   ;  (setq cover-short avel cover-long aveh)
 
     (cond ((and  (/= vri3 6)(plusp ccid10-2)(plusp bpl5)(/= bpl8 -1)(/= bpl13 -3)
                  (not (member vri5 '(5 6)))
                  (< rsi2 1) (/= tls4-8 -3)(not (member can '(DNST DNHM DNDJ DNTW)))
                 ; (not (member tls5-10 '(1 0 -4 -1 5 -5 -2)))
                  (/= ccil10 0)(/= ccil10 -1)
                  (/= ldev3 0)(not (member wpi '(DDD7 ODF DUD UDU UUU IDL IDH)))
                  (/= ldev5 4)(/= ldev5 -4)(/= ldev5 3)
               
               
                  (not (member zs3 '(SDN LDD 3SU)))
                  (member zs5 '(LDD SUP UPT 3SD)) 
                  (not (member zs10 '(WDN 3SD AZT BZT)))
                  (not (member zs21 '(BZT WUP DNT)))
                    

                (> si -2)(not (member pivw '(-4 -5)))
                (/= refl5 -1)(/= refl5 0)(not (member refl8 '(0 -1))) 
               (not (member chandir21 '(IC- IC+ DC DC+ BC UC+)))
               (not (member modiv5 '(FT)))(not (member modiv8 '(FT)))
               (not (member chandir5 '(IC+)))(not (member chandir8 '(DC- AC BT)))
               (not (member chandir3 '(AC DC)))(not (member chandir13 '(IC+ US)))
             ; (member epsignal '(DOWN OK)) 
 
 
            )
            (setq tsignal 'SELL-AT-STOP-ENTRY stop-short (fmin vphext (getd date 'close)))
            )
        
           ((and  
                (/= vri3 6)(minusp ccid10-2)(minusp bpl5)(/= bpl8 1)(/= bpl13 3)
                (not (member vri5 '(5 6)))
                (> rsi2 -1)  (/= tls4-8 3) (not (member can '(UPPC UPHM UIST)))    
            
                (/= ccil10 0)(/= ccil10 1)
                (/= ldev3 0) (not (member wpi '(ODF IDH OUH IUH IUF OUL OUF)))
                (/= ldev5 4)(/= ldev5 -4)(/= ldev5 -3)
             
            
                (not (member zs3 '(3SD WDN 3SU)))
                (member zs5 '(3SD SDN 3SU DNT))
                (not (member zs10 '(BZT AZT 3SD LDU)))
                (not (member zs21 '(BZT WUP LDD)))
 
                 (< si 2)(not (member pivw '(4 5)))
                 (/= refl5 1)(/= refl5 0)(not (member refl8 '(0 1)))
                 (not (member chandir21 '(IC+ AT2 BC DC- AT)))
                 (not (member modiv5 '(FT)))(not (member modiv8 '(FT)))
                 (not (member chandir5 '(AC)))(not (member chandir8 '(AT2 IC+)))
                 (not (member chandir3 '(UC+)))(not (member chandir13 '(DC+)))
        

                 )
            (setq tsignal 'BUY-AT-STOP-ENTRY stop-long (fmax vplext (getd date 'close)))
            )
        
          ; ((>= ccis5 0) (setq tsignal 'HOLD-LONG 
          ;                            objective-price 0))
          ; ((<= ccis5 0) (setq tsignal 'HOLD-SHORT entry-short 0 
          ;                           objective-price 0)))
           )
      
       (cond ((equal tsignal 'BUY-AT-STOP-ENTRY ) (setq stop-loss stop-long entry-price entry-long))
             ((equal tsignal 'SELL-AT-STOP-ENTRY) (setq stop-loss stop-short entry-price entry-short))
             (t (setq entry-price 0) (setq stop-long (swing-stop-long tdate)
                                           stop-short (swing-stop-short tdate)))
              )
     ; (format T "~% DATE = ~A TSIGNAL = ~A ~%" date tsignal)       
     
    (when output  
     (write-trend-swing-record output tdate date1 tsignal entry-price stop-long stop-short objective-price )
       )
     ; (format t "~%market = ~A tdate = ~A tsignal = ~A  entry-price ~A stop-long = ~A stop-short = ~A~%" 
     ;         *data-name* tdate tsignal entry-price stop-long stop-short) 
(values tsignal )   
      ));;;closes the let and the defun

;;;;
(defun find-best-edge-swing-trade1 (tdate &optional (output T)(date1 nil))
  (let* ((date tdate) tsignal   
           vplow vphigh            
           vsig ctr vprev;reversal price
           epsignal longs long-gains long-acc shorts short-gains short-acc bin pf-longs pf-shorts
           cover-short cover-long 
          
           objective-price (entry-price 0) stop-loss
           stop-long stop-short          
           )
         
         (multiple-value-setq (vplow vphigh)(vprices date *duration* *factor* 1 *type*))         
	 (multiple-value-setq (vsig ctr vprev)(vsignals1 date ))
          
         (setq stop-long  vplow
	       stop-short  vphigh)

   ;  (if (or (not (readt2 date))(< date sdate)) (return))
	 (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc
					bin pf-longs pf-shorts)
                  (bin-classifier-swingtradesb date *swing-features*))
      (format output "~%~A  ~A~%" *data-name* epsignal)

 
     (cond ((and  (eql vsig 'LONG) 
              (member epsignal '(DOWN OK)))
            (setq entry-price vprev stop-short vphigh tsignal 'SELL-AT-ENTRY ))
        
           ((and  (eql vsig 'SHORT)    
                  (member epsignal '(UP OK)))		  	    
            (setq entry-price vprev stop-long vplow tsignal 'BUY-AT-ENTRY ))
	     
          ; ((and (eql vsig 'LONG)
	;	 (member epsignal '(UP)))
         ;   (setq stop-long vprev tsignal 'BUY-MOO))
	   
	  ; ((and (eql vsig 'SHORT)
	;	 (member epsignal '(DOWN)))
	 ;   (setq stop-short vprev tsignal 'SELL-MOO))
	   
        ;   ((and (eql vsig 'LONG)
	;	 (member epsignal '(OK)))
         ;   (setq stop-long vprev tsignal 'BUY-MOO-REV-STOP ))
	   
;	   ((and (eql vsig 'SHORT)
;		 (member epsignal '(OK)))
;	    (setq stop-short vprev tsignal 'SELL-MOO-REV-STOP))
           )
      (setq entry-price vprev)
     
      (format output " Num Longs = ~D P/L for Longs = ~A PF = ~4F Longs ACC  = ~3F~%" longs long-gains pf-longs long-acc) 
      (format output " Num Shorts = ~D P/L for shorts = ~A  PF = ~4F Short ACC= ~3F~%" shorts short-gains pf-shorts short-acc)
     ;  (cond ((equal tsignal 'BUY-AT-STOP-ENTRY ) (setq stop-loss stop-long entry-price entry-long))
      ;       ((equal tsignal 'SELL-AT-STOP-ENTRY) (setq stop-loss stop-short entry-price entry-short))
       ;      (t (setq entry-price 0))
        ;      )
					; (format T "~% DATE = ~A TSIGNAL = ~A ~%" date tsignal)
     
     
    (when output  
     (write-trend-swing-record output tdate date1 tsignal entry-price stop-long stop-short objective-price )
       )
     ; (format t "~%market = ~A tdate = ~A tsignal = ~A  entry-price ~A stop-long = ~A stop-short = ~A~%" 
     ;         *data-name* tdate tsignal entry-price stop-long stop-short) 
        (values tsignal longs long-gains long-acc shorts short-gains short-acc )   
      ));;;closes the let and the defun

;;this is NOT the initial stop
(defun swing-stop-long (tdate &optional (days 5))
   (let ((date tdate) epsignal stops-long vplext vphext avel3 )

      (dotimes (ith days)
         (setq epsignal (find-swing-trade-signal date))
       ;  (format T "epsignal= ~A~%" epsignal)
         (multiple-value-setq (vplext vphext)(vprices date *duration* *factor* 1 *type*))
         (setq avel3 (ave date 3 'low))
         (push (fmax vplext avel3) stops-long)
         (if (equal epsignal 'BUY-AT-STOP-ENTRY) (return))
        ; (format T "~A~%" stops-long)
         (setq date (getd date 'ydate))
       )
   (apply #'fmax stops-long)
))
;;this is NOT the initial stop
(defun swing-stop-short (tdate &optional (days 5))
   (let ((date tdate) epsignal stops-long vplext vphext aveh3 )

      (dotimes (ith days)
         (setq epsignal (find-swing-trade-signal date))
         ;(format T "epsignal= ~A~%" epsignal)
         (multiple-value-setq (vplext vphext)(vprices date *duration* *factor* 1 *type*))
         (setq aveh3 (ave date 3 'high))
        (push (fmin vphext aveh3) stops-long)
         (if (equal epsignal 'SELL-AT-STOP-ENTRY) (return))
        ; (format T "~A~%" stops-long)
         (setq date (getd date 'ydate))
       )
   (apply #'fmin stops-long)
))
#|
(defun find-best-trend-swing-trade (tdate &optional (output T)(date1 nil))
  (let* ((qty 1) (date tdate) tsignal ;  mean11
           entry-price stop-price ; epsignal ;longs long-gains long-acc shorts short-gains short-acc bin
          trade-direction dir sdate objective-price kc-lower kc-higher kc-bound (market *data-name*)
           )
         
  (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 0 kc-bound 1.0))
          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 kc-bound 1.0))
          ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 kc-bound 1.0))
          (t (setq *commission* 25 *pips-slippage* 0 kc-bound 1.0 )))

      ;(setq mean11 (ave tdate 11))
      (multiple-value-setq (dir sdate objective-price)(parabolic-stops tdate))      
      (multiple-value-setq (kc-lower kc-higher)(vprices5 tdate 11 kc-bound 11));;1.618
   ;   (if (eql dir 'long)(setq objective-price (max kc-lower objective-price)); mean11))
    ;      (setq objective-price (min kc-higher objective-price))); mean11)))

    
;;;find most recent entry
    (setq date tdate)
    
   ;  (if (or (not (readt2 date))(< date sdate)) (return))
   ;   (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
    ;              (bin-classifier-swingtradesb date features))
    
     
 ;     (cond ((and (eql dir 'long) (stringp (getd (getd date 'ndate) 'low)))
 ;             (setq epsignal 'down))
 ;           ((and (eql dir 'short)(stringp (getd (getd date 'ndate) 'high)))
 ;             (setq epsignal 'up))
 ;            ((and (eql dir 'long)(< (getd (getd date 'ndate) 'low) entry-price))   
 ;            (setq epsignal 'down))
 ;           ((and (eql dir 'short)(> (getd (getd date 'ndate) 'high) entry-price))
 ;            (setq epsignal 'up))
 ;           (t (setq epsignal 'AVOID)))
;;;
      (setq tsignal (trend-swing-signal date))
      (format T "~% DATE = ~A TSIGNAL = ~A CHAN21 = ~A" date tsignal (channel-direction date 21))
 ;     (format t "~%market = ~A date = ~A parabolic = ~A csignal = ~A epsignal= ~A~%"
  ;              *data-name* date (parabolic-stops date) csignal epsignal)
   ;   (if (and csignal (neql epsignal 'UNIQUE)
    ;            (not (and (eql csignal 'long)(eql epsignal 'down)))
     ;           (not (and (eql csignal 'short)(eql epsignal 'up)))
      ;         (neql epsignal 'AVOID)) (return) (setq date (getd date 'ydate)))


;;;;was there an exit since the signal?
;;;start from date of signal and walk forward.
 
               (if (or (eql tsignal 'LONG) (not tsignal)) (setq entry-price (max kc-higher objective-price)))
               (if (eql tsignal 'SHORT)(setq entry-price (min kc-lower objective-price)))
 
               (if (or (eql tsignal 'LONG) (not tsignal))(setq stop-price (min kc-lower objective-price)))
               (if (eql tsignal 'SHORT)(setq stop-price (max kc-higher objective-price)))
 
                                  
 ;             ((and (eql csignal 'LONG) (member epsignal '(OK UP))) csignal)
 ;             ((and (eql csignal 'SHORT)(member epsignal '(OK DOWN))) csignal)
 ;             (t 'OUT)))
;  (format t "~%market = ~A date = ~A direction = ~A~%" *data-name* date trade-direction)
  ; (setq sdate date)
;;;;now check if the trade exited with target
 ;   (loop 
 ;     (if (eql trade-direction 'OUT) (return))    
 ;     (if (< date tdate)(setq date (getd date 'ndate))(return)) 
 ;     (setq sexit (counter-swing-exit-target date trade-direction))
 ;    (format t "~%2market = ~A date = ~A sexit = ~A direction= ~A~%" *data-name* date sexit trade-direction)
 ;     (when (eql sexit 'OUT)(setq trade-direction 'OUT)(return))
    
  ;  )
;    (setq date sdate)
;;;check if trade exited with criteria
  ;  (loop
  ;     (if (eql trade-direction 'OUT) (return)) 
  ;     (if (< date tdate)(setq date (getd date 'ndate))(return))
  ;     (setq sexit (counter-swing-exit-criteria date))
  ;     (format t "~%3market = ~A date = ~A sexit = ~A~%" *data-name* date sexit)
  ;     (if (and (eql csignal 'LONG) (eql sexit 'COVERLONG)) (setq trade-direction 'OUT))
  ;     (if (and (eql csignal 'SHORT)(eql sexit 'COVERSHORT)) (setq trade-direction 'OUT))
         
   ;     )
     (ifn tsignal (setq trade-direction 'OUT))
     
    ; (setq qty (if (>= (1+ (sub-mkt-dates (nth-value 1 (parabolic-stops tdate)) tdate)) 19) 2 1))
 
     (write-trend-swing-record output tdate date1 tsignal entry-price stop-price qty)
     (format t "~%market = ~A tdate = ~A parabolic = ~A csignal = ~A  trade-direction = ~A~% entry-price = ~A~%" 
              *data-name* tdate (parabolic-stops tdate) tsignal  trade-direction entry-price) 
(values tsignal sdate)   
      ));;;closes the let and the defun
|#
(defun find-best-counter-swing-trade (tdate &optional (output T)(date1 nil))
  (let* ((qty 1) (date tdate)  idir idate sexit epcsignal
           entry-price ;  longs long-gains long-acc shorts short-gains short-acc bin
          trade-direction  sdate objective-price 
           )
 (declare (special csignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin ))
         
;;;find most recent entry
    (setq date tdate)
    (multiple-value-setq (idir idate objective-price)(parabolic-stops tdate))
   
   ;  (if (or (not (readt2 date))(< date sdate)) (return))
     ; (multiple-value-setq (csignal longs long-gains long-acc shorts short-gains short-acc bin)
     ;             (bin-classifier-swingtradesb date features))
 ;   (loop   
      (format t "~%market = ~A date = ~A parabolic = ~A csignal = ~A ~%"
                *data-name* date idir csignal)
      (setq epcsignal (counter-swing-signal date))
     ; (multiple-value-setq (dir sdate)(parabolic-stops date))
  ;    (if (neql idir dir)(return))
   ;   (if (not csignal)(setq date (getd date 'ydate))(return))
   ; )
                  
;;;;was there an exit since the signal?
;;;start from date of signal and walk forward.
 ; (format t "~%market = ~A date = ~A direction = ~A~%" *data-name* date trade-direction)
  ; (setq sdate date)
;;;;now check if the trade exited with target
 ;   (loop 
 ;     (if (not csignal) (return))    
 ;     (if (< date tdate)(setq date (getd date 'ndate))(return)) 
 ;     (setq sexit (counter-swing-exit-target date csignal))
 ;     (format t "~%2market = ~A date = ~A sexit = ~A direction= ~A~%" *data-name* date sexit trade-direction)
 ;     (when (eql sexit 'OUT)(setq trade-direction 'OUT)(return))
    
   ; )
;    (setq date sdate)
;;;check if trade exited with criteria
  ;  (loop
  ;     (if (eql trade-direction 'OUT) (return)) 
  ;     (if (< date tdate)(setq date (getd date 'ndate))(return))
       (setq sexit (counter-swing-exit-criteria date))
  ;     (format t "~%3market = ~A date = ~A sexit = ~A~%" *data-name* date sexit)
  ;     (if (and (eql csignal 'LONG) (eql sexit 'COVERLONG)) (setq trade-direction 'OUT))
  ;     (if (and (eql csignal 'SHORT)(eql sexit 'COVERSHORT)) (setq trade-direction 'OUT))
     (if (not epcsignal)(setq csignal sexit))    
   ;     )
     (if (eql sexit 'out)(setq epcsignal nil trade-direction 'OUT))
    
 
    ; (setq qty (if (>= (1+ (sub-mkt-dates (nth-value 1 (parabolic-stops tdate)) tdate)) 19) 2 1))
    ; (if csignal
     (write-counter-swing-record output tdate date1 csignal entry-price objective-price qty)
       ;)
     (format t "~%market = ~A tdate = ~A parabolic = ~A epcsignal = ~A ~%" 
              *data-name* tdate (parabolic-stops tdate) epcsignal) 
(values epcsignal sdate)   
      ));;;closes the let and the defun


(defun counter-swing-exit-criteria (tdate)
  (let (dir sdate objective-price sexit trig5 cci5d1 cci5 cci5h3 cci5l3)
      
      (multiple-value-setq (dir sdate objective-price)(parabolic-stops tdate))      
      (multiple-value-setq (cci5h3 cci5l3)(cci-high-low tdate 5 3))
      
      (setq trig5 (cci-signal1 tdate 5 2) cci5d1 (cci-direction tdate 5 1)
            cci5 (commodity-channel-index tdate 5))

     (if (and (eql dir 'LONG)
              (or  ;(eql dir 'SHORT)
                    (= trig5 1);;take profit
                    (and (>= cci5d1 1) (< cci5 100)(< cci5l3 0))
          
              )) (setq sexit 'COVERSHORT))

      (if (and (eql dir 'SHORT)
              (or ;(eql dir 'LONG)
              (= trig5 -1);;take profit
              (and (<= cci5d1 -1) (> cci5 -100)(> cci5h3 0))  
              )) (setq sexit 'COVERLONG))
 sexit

))

(defun counter-swing-exit-target (tdate trade-direction)
   (let (dir sdate objective-price sexit mean11)

    (multiple-value-setq (dir sdate objective-price) (parabolic-stops (getd tdate 'ydate)))
    (setq mean11 (ave tdate 11))
   
    (cond ((and (eql trade-direction 'SHORT)(< (getd tdate 'low)  objective-price)) (setq sexit 'OUT))
          ((and (eql trade-direction 'LONG)(> (getd tdate 'high) objective-price ))(setq sexit 'OUT))
          (t (setq sexit nil))) 
   sexit
))
(defun edge-swing-signal (tdate)
   (let (dir sdate objective-price cci-h cci-l macdd can  tsignal chan21
        rsi5h rsi5l rsi5l-bound rsi5h-bound (market *data-name*)
         dirp rsi2xl rsi2xh cci5h cci5l wp roc5h roc5l)
 (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 0  
              rsi5l-bound 38 rsi5h-bound 62))
          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 
              rsi5l-bound 30 rsi5h-bound 70))
          ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 
              rsi5l-bound 30 rsi5h-bound 70))
          (t (setq *commission* 25 *pips-slippage* 0 
              rsi5l-bound 30 rsi5h-bound 70 )))


    (multiple-value-setq (dir sdate objective-price)(parabolic-stops tdate))
    (multiple-value-setq (cci-h cci-l) (cci-high-low tdate 21 11))
     (multiple-value-setq (roc5h roc5l) (ep-roc-high-low tdate 5 3))
     (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction tdate 3))
     (multiple-value-setq (cci5h cci5l) (cci-high-low tdate 5 3))
     (multiple-value-setq (rsi5h rsi5l)(rsi-high-low tdate 5 3))
  
   
    (setq macdd (macd-direction tdate 12 26 9 2) ;rsi5 (rsi tdate 5))
          can  (candle-composite tdate 3)  wp (wpp1-composite tdate 3)
         chan21 (channel-direction tdate 21))   
               
   ;  (format T "~%market = ~A date = ~A dir= ~A ccir211-11 = ~A~% macdd= ~A can= ~A wp = ~A~%"
   ;      *data-name* tdate dir (- cci-h cci-l) macdd can wp)
   ;  (format T "~%rsix2xl = ~A  cci5l = ~A cci5h= ~A Roc5h = ~A rsi5h= ~A rsi5L= ~A~% " 
    ;          rsi2xl cci5l cci5h roc5h rsi5h rsi5l)
 ;(format T "~%cci-h= ~A cci-l= ~A " cci-h cci-l)
      (if  (and  (eql dir 'SHORT)
                 (>= (- cci-h cci-l) 145)          
            ;    (or (plusp macdd)
            ;        (= can 1)(= wp 1)
            ;        (and (< rsi2xl 15)(< cci5l -100))  
            ;         )
                (member chan21 '(AC BT2 AT2 DC UC ))
               ; (> roc5h -2.25);;;not too steep downwards
                ;(<= rsi5l 30)
             ;     (<= rsi5l rsi5l-bound)
              ;  (> (* (- objective-price (getd tdate 'close))(calculate-point-value tdate)) 25)       
               ; (not (member chan7 '(DS BC )))
              
              )  (setq tsignal 'LONG))
    ; (format T "~%rsi2xh = ~A cci5h = ~A roc5l = ~A" rsi2xh cci5h roc5l)
       (if  (and (eql dir 'LONG)
                (>= (- cci-h cci-l) 145)   
               ;  (or (minusp macdd)
               ;      (= can -1)(= wp -1)
               ;      (and (> rsi2xh 85)(> cci5h 100))
               ;      )  
                 (member chan21 '(BC AT2 BT2 UC DC))  
                ; (< roc5l 2.25);;;not too steep downwards
                 ;(>= rsi5h 70)
                ; (>= rsi5h rsi5h-bound)
               ;  (> (* (- (getd tdate 'close) objective-price)(calculate-point-value tdate)) 25)       
                ; (not (member chan7 '(US AC )))
                
                ) (setq tsignal 'SHORT))

tsignal

))
(defun trend-swing-signal (tdate)
   (let (dir sdate objective-price cci-h cci-l macdd can  tsignal chan21
        rsi5h rsi5l rsi5l-bound rsi5h-bound (market *data-name*)
         dirp rsi2xl rsi2xh cci5h cci5l wp roc5h roc5l)
 (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 0  
              rsi5l-bound 38 rsi5h-bound 62))
          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 
              rsi5l-bound 30 rsi5h-bound 70))
          ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 
              rsi5l-bound 30 rsi5h-bound 70))
          (t (setq *commission* 25 *pips-slippage* 0 
              rsi5l-bound 30 rsi5h-bound 70 )))


    (multiple-value-setq (dir sdate objective-price)(parabolic-stops tdate))
    (multiple-value-setq (cci-h cci-l) (cci-high-low tdate 21 11))
     (multiple-value-setq (roc5h roc5l) (ep-roc-high-low tdate 5 3))
     (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction tdate 3))
     (multiple-value-setq (cci5h cci5l) (cci-high-low tdate 5 3))
     (multiple-value-setq (rsi5h rsi5l)(rsi-high-low tdate 5 3))
  
   
    (setq macdd (macd-direction tdate 12 26 9 2) ;rsi5 (rsi tdate 5))
          can  (candle-composite tdate 2)  wp (wpp1-composite tdate 2)
         chan21 (channel-direction tdate 21))   
               
   ;  (format T "~%market = ~A date = ~A dir= ~A ccir21-11 = ~A~% macdd= ~A can= ~A wp = ~A~%"
   ;      *data-name* tdate dir (- cci-h cci-l) macdd can wp)
   ;  (format T "~%rsix2xl = ~A  cci5l = ~A cci5h= ~A Roc5h = ~A rsi5h= ~A rsi5L= ~A~% " 
    ;          rsi2xl cci5l cci5h roc5h rsi5h rsi5l)
 ;(format T "~%cci-h= ~A cci-l= ~A " cci-h cci-l)
      (if  (and ;(eql dir 'SHORT)
                (not (< (- cci-h cci-l) 145))          
            ;    (or (plusp macdd)
            ;        (= can 1)(= wp 1)
            ;        (and (< rsi2xl 15)(< cci5l -100))  
            ;         )
                (member chan21 '(AT DC- UC- IC+))
               ; (> roc5h -2.25);;;not too steep downwards
                ;(<= rsi5l 30)
             ;     (<= rsi5l rsi5l-bound)
              ;  (> (* (- objective-price (getd tdate 'close))(calculate-point-value tdate)) 25)       
               ; (not (member chan7 '(DS BC )))
              
              )  (setq tsignal 'LONG))
    ; (format T "~%rsi2xh = ~A cci5h = ~A roc5l = ~A" rsi2xh cci5h roc5l)
       (if  (and ;(eql dir 'LONG)
               (not  (< (- cci-h cci-l) 145))   
               ;  (or (minusp macdd)
               ;      (= can -1)(= wp -1)
               ;      (and (> rsi2xh 85)(> cci5h 100))
               ;      )  
                 (member chan21 '(BC US DC+))  
                ; (< roc5l 2.25);;;not too steep downwards
                 ;(>= rsi5h 70)
                ; (>= rsi5h rsi5h-bound)
               ;  (> (* (- (getd tdate 'close) objective-price)(calculate-point-value tdate)) 25)       
                ; (not (member chan7 '(US AC )))
                
                ) (setq tsignal 'SHORT))

tsignal

))
(defun counter-swing-signal (tdate)
   (let (dir sdate objective-price  epcsignal rsi2x rsi2 form1 wpp chan4
         reward  cci21s2 cci21  cci21x
        (market *data-name*)  cci5r5 trig5 )
   (declare (special csignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin ))

    (set-market market)
    (multiple-value-setq (dir sdate objective-price)(parabolic-stops tdate))
   
    (setq  
         reward (* (calculate-point-value tdate)(abs (- (getd tdate 'close) objective-price)))
          cci21s2 (cci-speed-index tdate 21)
          rsi2 (rsi tdate 2) cci5r5 (cci-range-index tdate 5 5)
          rsi2x (rsi2x-ob-os tdate 2)  cci21x (ccix-ob-os tdate 21 11)
          trig5 (cci-signal1 tdate 5 2) 
         
          cci21 (cci-level-index tdate 21)  wpp (wpp tdate)
          form1 (formation-signal tdate 2) chan4 (channel-direction tdate 4))
               
  ;   (format T "~%market = ~A date = ~A dir= ~A ~% ccis = ~A  rsi2xh = ~A rsi2xl = ~A~%~
  ;                   macdd =~A  cci5h = ~A cci5l = ~A~%"
 ;                   *data-name* tdate dir ccis rsi2xh rsi2xl macdd cci5h cci5l)
   ;  (format T "~%rsix2xl = ~A  cci5l = ~A cci5h= ~A Roc5h = ~A rsi5h= ~A rsi5L= ~A~% " 
    ;          rsi2xl cci5l cci5h roc5h rsi5h rsi5l)
 ;(format T "~%cci-h= ~A cci-l= ~A " cci-h cci-l)/ 
      (if  (and (eql dir 'SHORT)(eql csignal 'LONG)
                 (>= reward 50)
                 (= trig5 1)
                 (/= form1 -1);(= cci-text -1)
  
                 (not (member chan4 '(IC- UC- UC)))
                 (not (member wpp '(OUF ODF UUU)))            
              
                (>= cci21x -6) (<= cci21 0)
                 (>= rsi2x 1)
                 (<= rsi2x 3)(<= cci5r5 5)(< rsi2 55)
              
              )  (setq epcsignal 'LONG))
   ;  (format T "~%rsi2x = ~A cci5r5 = ~A cci21 = ~A cci21x = ~A cci21a2 = ~A" rsi2x cci5r5 cci21 cci21x cci21a2)
       (if  (and (eql dir 'LONG)(eql csignal 'SHORT)
                 (>= reward 50)     
                 (= trig5 -1)
                 (/= form1 1);(= cci-text -1)
   
                 (not (member chan4 '(IC- DC+ IC+ )))                
                 (not (member wpp '(ODF DDD7 DUD)))


                  (<= cci21x 6) (>= cci21 0)
                  (<= rsi2x -1)
                  (>= rsi2x -3) (<= cci5r5 5)  (> rsi2 45) 

                              
                ) (setq epcsignal 'SHORT))

epcsignal
 
))
#|
(defun counter-swing-signals (tdate days)
  (let ((date tdate) cssignal)

  (dotimes (ith days)
    (setq cssignal (counter-swing-signal date))
    (if cssignal (return) (setq date (getd date 'ydate)))
)
     cssignal
))
|#



(defun counter-swing-signals (tdate)
  (let* ( (date tdate) csignal dir idir idate sexit 
          trade-direction  sdate objective-price ;kc-lower kc-higher 
           )
         
;;;find most recent entry
    (setq date tdate)
    (multiple-value-setq (idir idate objective-price)(parabolic-stops tdate))
   ; (multiple-value-setq (kc-lower kc-higher)(vprices5 tdate 11 .8 11))
   
    (loop   
   ;   (format t "~%market = ~A date = ~A parabolic = ~A csignal = ~A ~%"
    ;            *data-name* date (parabolic-stops date) csignal )
      (setq csignal (counter-swing-signal date))
      (multiple-value-setq (dir sdate)(parabolic-stops date))
      (if (neql idir dir)(return))
      (if (not csignal)(setq date (getd date 'ydate))(return))
    )
                  
;;;;was there an exit since the signal?
;;;start from date of signal and walk forward.

 ; (format t "~%market = ~A date = ~A direction = ~A~%" *data-name* date trade-direction)
  ; (setq sdate date)
;;;;now check if the trade exited with target
    (loop 
      (if (not csignal) (return))    
      (if (< date tdate)(setq date (getd date 'ndate))(return)) 
      (setq sexit (counter-swing-exit-target date csignal))
  ;    (format t "~%2market = ~A date = ~A sexit = ~A direction= ~A~%" *data-name* date sexit trade-direction)
      (when (eql sexit 'OUT)(setq trade-direction 'OUT)(return)) 
    )

     (if (eql sexit 'out)(setq csignal nil trade-direction 'OUT))
  ;   (setq mean11 (ave tdate 11))
   ;  (if (eql csignal 'LONG)(setq entry-price (min kc-lower objective-price)
    ;                              objective-price (min objective-price mean11))
    ;                                       )
    ; (if (eql csignal 'SHORT)(setq entry-price (max kc-higher objective-price
    ;                               objective-price (max objective-price mean11)))
 
 ;    (write-counter-swing-record output tdate date1 csignal entry-price objective-price qty)
;     (format t "~%market = ~A tdate = ~A parabolic = ~A csignal = ~A  trade-direction = ~A~% entry-price = ~A~%" 
  ;            *data-name* tdate (parabolic-stops tdate) csignal  trade-direction entry-price) 
(values csignal sdate)   
      ));;;closes the let and the defun



;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for swing trades to decide to trade or not
;;;;assumes (swing-trade-bins1 features) has already been run
(defun find-best-counter-swing-trade-test (tdate); &optional (output T)(date1 nil))
  (let* (;(qty 1)
          (date tdate) csignal sexit ; mean11
          epsignal ;longs long-gains long-acc shorts short-gains short-acc bin
          trade-direction dir sdate objective-price kc-lower kc-higher kc-bound (market *data-name*)
           )
         
  (cond  ((member market *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 6 kc-bound 1.0))
          ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 kc-bound .7))
          ((and (not (index-commodityp))(not (index-futurep))) (setq *commission* 20 *pips-slippage* 0 kc-bound .7))
          (t (setq *commission* 25 *pips-slippage* 0 kc-bound .7)))


      (multiple-value-setq (dir sdate objective-price)(parabolic-stops tdate))      
      (multiple-value-setq (kc-lower kc-higher)(vprices5 tdate 11 kc-bound 11));;1.618
      ;(setq mean11 (ave tdate 11))
      (if (eql dir 'long)(setq objective-price (max kc-lower objective-price)); mean11))
          (setq objective-price (min kc-higher objective-price))); mean11)))
;;;find most recent entry
    (setq date tdate)
 
   
 ;   (build-swingtrade-warehouse (list *data-name*))
  ;  (setq features (find-best-swing-indicator-set nil))
   

   ;  (apply #'swing-trade-binsb str features) ;;;need just once for all dates
   
    (loop
     (if (or (not (readt2 date))(< date sdate)) (return))
 ;     (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
  ;                (bin-classifier-swingtradesb date features))

      (setq csignal (counter-swing-signal date))
    ;  (format t "~%market = ~A date = ~A parabolic = ~A csignal = ~A epsignal= ~A~%"
     ;           *data-name* date (parabolic-stops date) csignal epsignal)
      (if (and csignal (neql epsignal 'UNIQUE)
                (not (and (eql csignal 'long)(eql epsignal 'down)))
                (not (and (eql csignal 'short)(eql epsignal 'up)))
               (neql epsignal 'AVOID)) (return) (setq date (getd date 'ydate))));;;closes loop

    
  
;;;;was there an exit since the signal?
;;;start from date of signal and walk forward.
    (setq trade-direction
        (cond ((and (eql csignal 'LONG)(member epsignal '(OK UP)) (/= date tdate)) 'HOLDLONG)
              ((and (eql csignal 'SHORT)(member epsignal '(OK DOWN))(/= date tdate)) 'HOLDSHORT)
              ((and (eql csignal 'LONG)(member epsignal '(OK UP))(= date tdate)
                    (eql (counter-swing-signal (getd date 'ydate)) 'LONG)) 'LONG)
              ((and (eql csignal 'SHORT)(member epsignal '(OK DOWN))(= date tdate)
                    (eql (counter-swing-signal (getd date 'ydate)) 'SHORT)) 'SHORT)
                    
              ((and (eql csignal 'LONG) (member epsignal '(OK UP))) csignal)
              ((and (eql csignal 'SHORT)(member epsignal '(OK DOWN))) csignal)
              (t 'OUT)))
  ;(format t "~%market = ~A date = ~A direction = ~A~%" *data-name* date trade-direction)
   ;(setq edate date)
;;;;now check if the trade exited with target
    (loop 
      (if (eql trade-direction 'OUT) (return))    
      (if (< date tdate)(setq date (getd date 'ndate))(return)) 
      (setq sexit (counter-swing-exit-target date trade-direction))
   ;  (format t "~%2market = ~A date = ~A sexit = ~A direction= ~A~%" *data-name* date sexit trade-direction)
      (when (eql sexit 'OUT)(setq trade-direction 'OUT)(return))
    
    )
;    (setq date sdate)
;;;check if trade exited with criteria
    (loop
       (if (eql trade-direction 'OUT) (return)) 
       (if (< date tdate)(setq date (getd date 'ndate))(return))
       (setq sexit (counter-swing-exit-criteria date))
    ;   (format t "~%3market = ~A date = ~A sexit = ~A~%" *data-name* date sexit)
       (if (and (eql csignal 'LONG) (eql sexit 'COVERLONG)) (setq trade-direction 'OUT))
       (if (and (eql csignal 'SHORT)(eql sexit 'COVERSHORT)) (setq trade-direction 'OUT))
         
        )
     (ifn csignal (setq trade-direction 'OUT))
     
    ; (setq qty (if (>= (1+ (sub-mkt-dates (nth-value 1 (parabolic-stops tdate)) tdate)) 19) 2 1))
 
  ;   (write-counter-swing-record output tdate date1 trade-direction objective-price qty)
  ;   (format t "~%market = ~A tdate = ~A parabolic = ~A csignal = ~A epsignal= ~A~% trade-direction = ~A~%" 
   ;           *data-name* tdate (parabolic-stops tdate) csignal epsignal trade-direction) 
(values csignal sdate)   
      ));;;closes the let and the defun

(defun find-swing-entry-price (tdate)
  (let (entry-price csignal)
   
  (setq csignal (find-best-counter-swing-trade-test tdate)) 
       
  (if (eql csignal 'down)(setq entry-price (nth-value 1 (vprices5 tdate 11 1.0 11))))
  (if (eql csignal 'up)(setq entry-price (nth-value 0 (vprices5 tdate 11 1.0 11))))

  
   (values entry-price csignal)
))

#|
;;;assumes the day-trade-bins has been run first
(defun swingtrade-chi-squared-gof ()
  (let (contents (result 0) square-list (all-winners 0)(all-losers 0)
       square percentage-winners chi-squared)
;;;first calculate the profit per trade overall trades 
   (dolist (kth swings)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners 1))
           (setq all-losers (+ all-losers 1))))
   
   (setq percentage-winners (/ all-winners (+ all-winners all-losers)))

  (dolist (ith swing-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
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
  ;   (format T "~%CHI SQUARE = ~7,3F  DOF = ~D~%" chi-squared (1- (length swing-bin-codes)))
     (chi-square-cdf chi-squared (1- (length swing-bin-codes)))
  
))
|#
(defun most-recent-warehouse-trade (market )
  (let (trades)
  (dolist (ith swings)
    (if (eql market (svref ith 0)) (push ith trades)))
  (vsort trades #'> #'(lambda (s) (svref s 17)))
 (/  (svref (car trades) 19) (volatility (svref (car trades) 17)))
))


;;;returns a list of nine codes
(defun encode-swing-trades (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      (1 (push (svref record 2) bin-list));;;adds the direction

      (2 (push (svref record 4) bin-list))
;;;;Feature 3 with 5 levels bar type2
      (3 (push (svref record 5) bin-list))

;;;;Feature 4 volatility-ratio-index with 6 levels 
      (4 (push (svref record 6) bin-list))

;;;Feature 5 with 5 levels mo-diver
       (5  (push (svref record 7) bin-list))

;;;Feature 6 with 5 levels is the trend-signal 45
       (6  (push (svref record 8) bin-list))
;;;Feature 7 with 5 levels; DN CD UP CU FT This is TS135
       (7  (push (svref record 9) bin-list))
;;;;Feature 8 with 6 levels pivot-index for date-1
       (8  (push (svref record 10) bin-list))
;;;;Feature 9 has 10 levels ;this is the volatility ratio of 4 days to 28 days
       (9  (push (svref record 11) bin-list))
       
;;;;Feature 10 with 9 levels bar type
      (10 (push (svref record 12) bin-list))
;;;;;;Feature 11 with 5 levels of bar relationship to the previous bar

      (11 (push (svref record 13) bin-list))
;;;Feature 12 is pldot-index date
       (12  (push (svref record 14) bin-list))
;;;Feature 13 has 5 levels and is the 2-bar index
       (13  (push (svref record 15) bin-list))
 ;;;Feature 14 has 5 levels and is the ratio of ave openint 3 days versus 21 days date-1
       (14  (push (svref record 16) bin-list))
                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))



(defun find-best-swing-indicator-set (&optional (str T))
  (let (base-list candidate-list winners-list (result 0))

    (setq base-list '(1 ) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (swings-add-one-in base-list candidate-list str))

     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (sixth (car winners-list)));;;
     (if (or (not candidate-list)(> result .75);;;result is the ratio of bin profits to all winners
             (< (fifth (car winners-list)) 2.0);;;ratio of trades to bins
             (> (fourth (car winners-list)) .90));;;ratio of one trade bins to all trades
         (return))
    )
    (format str "First stage winners-list= ~A"  winners-list)

   (loop     
     (if (not (member (second (car winners-list)) '(1 )))
          (if (and (> (fifth (car winners-list)) 4.0)
                   (< (fourth (car winners-list)) .25))(return))
        (if (and (> (fifth (second winners-list)) 4.0)
                 (< (fourth (car winners-list)) .25))(return)))

      (setq winners-list (swings-leave-one-out base-list str))
      (if (not (member (second (car winners-list)) '(1 )))
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format str "~%Best indicator set = ~A~%" base-list)
      base-list
    ))



;;;----------------------------------------------------------------------------------------------------
;;;Dubai swing trade


;;;;;;;;;;;;;;;;;SWING TRADES FUNCTIONS with vectors
;;;;

(defun find-best-dubaiswing-indicator-set ()
  (let (base-list candidate-list winners-list (result 0)); tdate best-features)
        

    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (dubaiswings-add-one-in base-list candidate-list))

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

      (setq winners-list (dubaiswings-leave-one-out base-list))
      (if (neql (second (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
      base-list
    ))

(defun update-dubaiswingtrade-warehouse (date &optional (markets  *dubai-warehouse-list*))
    (maind-x)(set-cat-list)
   (apply #'swing-trade-binsb *dubai-features*)
    (dolist (ith markets)
       (set-market ith)
       (populate-dubaiswing-trades ith date (available-days ith date 440)))
 
    (build-dubaiswingtrade-warehouse markets) 
    (setq *dubaiswing-features*   (find-best-dubaiswing-indicator-set) )
    (portfolio-simulation3 '(dubaiswing) date 300 (list markets)) ;(list *score-list*))

)


(defun populate-dubaiswing-trades (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long  entry-long entry-short 
       ave-win ave-loss losers winners extended-trades trade-short date-1 ;forecast slope r-squared
       (days-in-trade 0)(time-allowed 5) 
       risk bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline (period 5) ;nstop-long nstop-short;  pdir sdate pstop fsdate 
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "dubaiswingtrades.dat")))

   (maind-x)(set-cat-list)(set-market market)

  (setq *commission* 10  *pips-slippage* 0 *max-dubaiswing-risk* 70000 )
       

   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

    (if (or long short)(incf days-in-trade))
    (multiple-value-setq (bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline)(channel-trend date period))

 
   (if (> p0 p1) (setq entry-short (- (price-to-go date period) (index-tick-size))
                        entry-long nil stop-long entry-short)
         (setq entry-long (+ (price-to-go date period) (index-tick-size))
               entry-short nil stop-short entry-long)
          )
     (multiple-value-bind (e1 e2)(price-to-go date period)
          (setq risk (abs (- e1 e2))))
          
    (bin-classifier-swingtradesb date *dubai-features*)

   (setq  date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long)
          (setq long (+ long (getd date 'rollover))
;                stop-long (+ stop-long (getd date 'rollover)) 
 ;              entry-short (+ entry-short (getd date 'rollover))
  
    ))
    (when (and (getd date 'rollover) short)
          (setq short (+ short (getd date 'rollover))
;    	        stop-short (+ stop-short (getd date 'rollover)) ;obj-short (+ obj-short (getd date 'rollover))
  ;              entry-long (+ entry-long (getd date 'rollover))

         )) 
  (if stop-short (setq stop-short (my-pretty-price stop-short)))
  (if stop-long (setq stop-long (my-pretty-price stop-long)))

 

 ;(format T "~%date= ~A dir= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A" date dir long stop-long short stop-short)
 ;(format T "~%prev-stop-short = ~A prev-stop-long = ~A" prev-stop-short prev-stop-long)
  ;;;;check if stopped out of prior position
  


;;;check if stopped out
   (when (and long (< (getd date 'low) stop-long))
    
          (push (- (* (- (min stop-long (getd date 'open)) long)
                      (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (min stop-long (getd date 'open)))
                       (round (* (calculate-point-value date) (- (min stop-long (getd date 'open)) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil days-in-trade 0))

   (when (and short (> (getd date 'high) stop-short))
 
          (push (- (* (- short (max stop-short (getd date 'open)))
                      (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price (max stop-short (getd date 'open)))
                 (round (* (calculate-point-value date) (- short (max stop-short (getd date 'open)))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil days-in-trade 0))

;;;check if exited with criterion change at end of 3rd day

   (when  (and long  (>= days-in-trade time-allowed)
                         )
       
          (push (- (* (-  (getd date 'close) long)
                      (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (getd date 'close))
                       (round (* (calculate-point-value date) (- (getd date 'close)  long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil days-in-trade 0 ))

   (when  (and short (>= days-in-trade time-allowed)   
                     )
       
          (push (- (* (- short (getd date 'close))
                      (calculate-point-value date-1))(comm+slip date-1)) trades)
           (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (getd date 'close))
                 (round (* (calculate-point-value date) (- short (getd date 'close))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil days-in-trade 0 ))
; (format T "~%2date= ~A dir= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A" date dir long stop-long short stop-short)




; (format T "~%3date= ~A dir= ~A long = ~A stop-long = ~A short = ~A stop-short = ~A" date dir long stop-long short stop-short)
;;;check if exited with objective
#|
   (when  (and long obj-long (> (getd date 'high) obj-long))
          (push (-  (max (getd date 'open) obj-long)  long) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (my-pretty-price (max (getd date 'open) obj-long))
                       (round (* (index-point-value) (- (max (getd date 'open) obj-long) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil stopped-out-long T ))

   (when  (and short obj-short (< (getd date 'low) obj-short))
          (push (- short (min (getd date 'open) obj-short)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (my-pretty-price  (min (getd date 'open) obj-short))
                 (round (* (index-point-value) (- short (min (getd date 'open) obj-short))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil Stopped-out-short T ))

|#
 ;   (format T "~%date= ~A Risk = ~A " date risk)


;;; means of entry short on open 
    (when (and (not short) entry-short
               (<= (getd date 'low) entry-short)
               (<= (* risk (calculate-point-value date-1)) *max-dubaiswing-risk*)        
                )
          (setq short (min entry-short (getd date 'open)) 
                stop-short (+ (nth-value 1 (price-to-go date-1 period)) (index-tick-size))          
                trade-short (create-dubaiswingtrade-entry-record-list date-1 -1 short)
                days-in-trade 1 
               ))

;;;check if new entry long

    (when (and (not long) entry-long
               (>= (getd date 'high) entry-long)
               (<= (* risk (calculate-point-value date-1)) *max-dubaiswing-risk*)
                )
           (setq long (max entry-long (getd date 'open)) 
                 stop-long (- (nth-value 1 (price-to-go date-1 period)) (index-tick-size))
                 trade-long (create-dubaiswingtrade-entry-record-list date-1 1 long)
                  days-in-trade 1
                 ))


 ;   (format T "~%date= ~A dir= ~A p0 = ~A p1 = ~A  p2 ~A Long = ~A short= ~A~%" date dir p0 p1 p2 long short)
 ;;check if stopped out on same day of entry
 
    (when (and long stop-long  (>= (getd date 'open)(getd date 'close))
                               (<= (getd date 'low) stop-long))
   
          (push (- (* (- stop-long long)(calculate-point-value date-1))(comm+slip date-1)) trades)
           (setq trade-long (apply #'vector
                 (append trade-long
                    (list date (my-pretty-price stop-long) (round (* (calculate-point-value date)(- stop-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil  ))
         
    (when (and short stop-short  (<= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'high) stop-short))
 
           (push (- (* (- short stop-short)(calculate-point-value date-1))(comm+slip date-1)) trades)
           (setq trade-short (apply #'vector
                (append trade-short
                 (list date (my-pretty-price stop-short) (round (* (calculate-point-value date)(- short stop-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil 
           ))

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
  ; (setq trades (mapcar #'(lambda (s) (- s (/ *commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
     (format output "~%~A~%" *data-name*)
#|     (format output "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
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
|#
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
(defun create-dubaiswingtrade-entry-record-list (date direction entry)
   (let ((date-1 (getd date 'ydate)) )
       ; (setq date-2 (getd date-1 'ydate))
      ;  (setq period (dominant-cycle date 10 30))
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
            
             (body-range-index date) ;;;;feature 2            
             (body-range-index date-1) ; feature 3
             (body-range-index (getd date-1 'ydate)); feature 4
 
             (r-squared-change-index date 5 1);;;feature 5
             (r-squared-change-index date-1 5 1) ;feature 6
             (r-squared-change-index (getd date-1 'ydate) 5 1) ;feature 7

             (tkcd-index date ) ;feature 8
             (tkcd-index date-1) ;feature 9
             (tkcd-index (getd date-1 'ydate)) ;feature 10
             (cloud-index date ) ;;feature 11 

             (volatility-ratio-index date 1 5 1) ;;;feature 12
             (volatility-ratio-index date-1 1 5 1)  ;feature 13
             (volatility-ratio-index (getd date-1 'ydate) 1 14 1);feature 14
      
            ; (ep-roc-change-index date 3 10);;feature 14
                               ; (car (member 'DS6 form-codes)))

           ;;  (my-round (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
            ;;                    (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
             ;;;do not have volume for day date-1 in real time feature 13
            ;;               (/ (ave (getd date-1 'ydate) 3 'volume)
            ;;                  (ave (getd date-1 'ydate) 21 'volume)) 1) 3)

           ;;  (my-round (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
            ;                    (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
             ;;;do not have openint for day date-1 in real time feature 14
            ;               (/ (ave1 (getd date-1 'ydate) 3 'openint)
            ;                  (ave1 (getd date-1 'ydate) 21 'openint)) 1) 3)
      
          
                  );;;closes the list

))



(defun build-dubaiswingtrade-warehouse (markets)
  (let ((path-out (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat"))
        new-trades-path trades)
   (if (probe-file (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.backup"))
           (delete-file (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.backup")))
    (when (probe-file (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat"))
          (rename-file (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat")
                            "dubaiswingtradewarehouse.backup")
        )

   (dolist (market markets)
       (setq new-trades-path (string-append *upper-dir-warehouse* (format nil "~A" market) "dubaiswingtrades.dat")) 
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



;;;;this function expects that you have run (swing-trade-bins ...) already
(defun display-dubaiswingtrade-bins-by-expected-value ()
  (let (contents expected-value-list result (winners 0)(losers 0) longs shorts
        num-trades first-date last-date
        (path1 (string-append *output-upper-dir* "dubaiswingtrade-expected-value.dat")))
  
    (multiple-value-setq (num-trades first-date last-date )(num-trades-in-warehouse3 'all swings))

    (dolist (ith swing-bin-codes)
     (setq contents
           (gethash ith *swing-trade-warehouse*))
     (setq result 0 winners 0 losers 0)
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19))(incf winners)(incf losers)))
     (setq expected-value-list 
           (cons (list (/ result (length contents)) ith (length contents) result winners losers) expected-value-list))
     );;;closes the dolist over bin-codes

     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (vsort expected-value-list #'> 'car)
         (format stream "Profit/trade  Bin         NUM   #WINNERS   #LOSERS   Profit~%~%")
         (dolist (jth expected-value-list)
             (format stream "~5D        ~10A  ~5D   ~5D    ~5D     ~7D~%"
                (round (car jth)) (cadr jth) (third jth) (fifth jth)(sixth jth)(round (fourth jth))))
         (format stream "~%Total #Trades = ~D  #WINNERS = ~D    $Profit = ~D"
             (list-sum (mapcar #'(lambda(s) (nth 2 s)) expected-value-list))
             (list-sum (mapcar #'(lambda(s) (nth 4 s)) expected-value-list))
             (round (list-sum (mapcar #'(lambda(s) (nth 3 s)) expected-value-list))))
          (format stream "~%TOTAL DAYS = ~D CHI SQUARED PROB = ~2,4F~%~%"
             (total-available-days last-date (num-markets-in-warehouse3 swings))  
              (swingtrade-chi-squared-gof)) 
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


;;;reads the swingtradewarehouse3.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-dubaiswing-trades (new-trades-path)
  (let ((path-out (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat")) trades)

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


(defun remove-dubaiswingtrade-market (market)
  (let (trades path)
   (setq path (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat"))
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

;;;enters with stop orders and exits with stop loss orders
;;;;
(defun dubaiswingtrade-simulation-test (market date2 num &optional (features *dubaiswing-features*))
 (let (date stop-long stop-short trades long short  trade-long 
        entry-long entry-short (time-allowed 3) 
        ave-win ave-loss losers winners extended-trades trade-short (twr-long 1)(twr-short 1)
        epsignal longs long-gains long-acc trading-dates (trade-time 0) (days-in-trade 0)
        shorts short-gains short-acc  date-1 record (running-sum 0) bin draw ;obj-long obj-short
        risk  bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline (period 5)  
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "dubaiswing-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "dubaiswing-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "dubaiswing-diary1.dat")))

   (apply #'dubaiswing-trade-binsb features)
   (set-market market)(format T "~%~A~%" *data-name*)

    (setq *commission* 10  *pips-slippage* 0 *max-dubaiswing-risk* 70000)

    (setq date (add-mkt-days date2 (- num)))

 ;;;;from date1 to date2
   (dotimes (ith num)

;   (format T "~%1 ~A = date  ~A = short  ~A = long~%" date short long)

    (if (or long short)(incf days-in-trade))
    (multiple-value-setq (bpl target p0 p1 p2 p3 p4 t0 t1 t2 t3 baseline)(channel-trend date period))

    (if (> p0 p1) (setq entry-short (- (price-to-go date period) (index-tick-size))
                        entry-long nil stop-long entry-short)
         (setq entry-long (+ (price-to-go date period) (index-tick-size))
               entry-short nil stop-short entry-long)
          )
     (multiple-value-bind (e1 e2)(price-to-go date period)
          (setq risk (abs (- e1 e2))))

    (bin-classifier-swingtradesb date *dubai-features*)
 ; (format T "~%nstop-long= ~A nstop-short= ~A" nstop-long nstop-short) 

     (setq date-1 date date (add-mkt-days date 1))

       (setq record (list date (getd date 'close) 0 0 0))
       (if long (setf (nth 2 record) 1))
       (if short (setf (nth 2 record) -1))
 
    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover)))
             (setf (nth 3 record)(- (nth 3 record) 
                        (*  (calculate-point-value date)(getd date 'rollover))))
        )

    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover)))
            (setf (nth 3 record) (+ (nth 3 record)
                   (* (calculate-point-value date) (getd date 'rollover))))
         )

  ;  (format T "~%2 ~A = date = ~A  dir = ~A short= ~A  long= ~A stop-long = ~A stop-short= ~A ~%" 
  ;date dir short long stop-long stop-short)

;;;;check if stopped out of prior position

   (when (and long (<= (getd date 'low) stop-long))
 ;          (format T "~%6 date = ~A long = ~A low = ~A stop-long = ~A trade-long = ~A"
 ;          date long (getd date 'low) stop-long trade-long)
         (push (- (* (- (min stop-long (getd date 'open)) long)
                        (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
             (append trade-long  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                      (contract-month *data-name* date) 'S
                                      (my-pretty-price  (min stop-long (getd date 'open)))
                                    (my-pretty-price (- (min stop-long (getd date 'open)) long))))))

           (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                        (* (calculate-point-value date)(- (min stop-long (getd date 'open)) (getd date-1 'close)))))


          (setq trade-long nil long nil stop-long nil days-in-trade 0))

   (when (and short (>= (getd date 'high) stop-short))
 ;         (format T "~%6 date = ~A short = ~A high = ~A stop-short = ~A trade-short = ~A"
 ;             date short (getd date 'high) stop-short trade-short)
          (push (- (* (- short (max stop-short (getd date 'open)))
                      (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))
                    (contract-month *data-name* date) 'S
                   (my-pretty-price (max stop-short (getd date 'open)))
                   (my-pretty-price (- short (max stop-short (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date)(- (getd date-1 'close) (max stop-short (getd date 'open))))))
          (setq trade-short nil short nil stop-short nil days-in-trade 0))

;;;check if exited with signal change at end of the 3rd day
          (when  (and long  (>= days-in-trade time-allowed) 
                          )
         
          (push (- (* (- (getd date 'close) long)(calculate-point-value date-1))
                      (comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'C
                                        (my-pretty-price (getd date 'close))
                       (my-pretty-price  (- (getd date 'close)  long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
           (setf (nth 3 record) (+ (nth 3 record)
                       (* (calculate-point-value date)(-  (getd date 'open) (getd date-1 'close)))))
          (setq trade-long nil long nil stop-long nil days-in-trade 0))

          (when  (and short (>= days-in-trade time-allowed)  
                          )
          (push (- short (getd date 'close)) trades)
          (push (- (* (- short (getd date 'close))(calculate-point-value date-1))
                      (comm+slip date-1)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'C
                                           (my-pretty-price  (getd date 'close))
                 (my-pretty-price (- short (getd date 'close)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                   (* (calculate-point-value date)(- (getd date-1 'close) (getd date 'open)))))
          (setq trade-short nil short nil stop-short nil days-in-trade 0))


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


          (setq trade-long nil long nil stop-long nil obj-long nil stopped-out-long nil))

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

          (setq trade-short nil short nil stop-short nil obj-short nil stopped-out-short nil))
|#
;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (+ (nth 3 record)
              (* (calculate-point-value date)(- (getd date 'close) (getd date-1 'close))))))
      (if short (setf (nth 3 record)
              (* (calculate-point-value date)(+ (nth 3 record)(- (getd date-1 'close)(getd date 'close))))))

 ;     (format T "~%3 ~A = date  ~A = entry-short  ~A = entry-long ~A = stop-long "
 ;            date entry-short entry-long stop-long)
;;;calculate bin-classifier 
   
      (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                  (bin-classifier-dubaiswingtradesb date-1 features))
      (setq twr-short (swing-bin-twr bin))(setf (nth 0 bin) 1)
      (setq twr-long (swing-bin-twr bin))


 ;   (format t "~%8date = ~A dir = ~A short = ~A epsignal = ~A shorts = ~A" date dir short epsignal shorts)
;;; means of entry

       (when (and (not short) entry-short 
                  (<= (getd date 'low) entry-short) 
                  (member epsignal '(OK DOWN)) (> shorts 0)
                  (>= (/ short-gains shorts) 100) (> twr-short 1)
                  (<= (* risk (calculate-point-value date-1)) *max-dubaiswing-risk*)                          
               )
          (setq short (my-pretty-price (min entry-short (getd date 'open))) 
                 trade-short (list date 'short short)
                 stop-short (+ (nth-value 1 (price-to-go date-1 period)) (index-tick-size))          
                 days-in-trade 0)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price (- short (getd date 'close)(comm+slip-points date-1))))))
              )

;;; means of entry on open
 
    (when (and (not long) entry-long
               (>= (getd date 'high) entry-long)    
               (member epsignal '(OK UP)) (> longs 0)
               (>= (/ long-gains longs) 100) (> twr-long 1)
               (<= (* risk (calculate-point-value date-1)) *max-dubaiswing-risk*)        
                )
           (setq long (my-pretty-price (max  entry-long (getd date 'open))) 
                 trade-long (list date 'long long)
                 stop-long (- (nth-value 1 (price-to-go date-1 period)) (index-tick-size))
                 days-in-trade 0)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                            (* (calculate-point-value date) (my-pretty-price (- (getd date 'close) long (comm+slip-points date-1))))))
             )

  ;   (format T "~%4 ~A = date  ~A = short  ~A = long" date entry-short entry-long)
;     (format T "~%4a ~A = epsignal  ~A = long  ~A = short stop-long = ~A" epsignal long short stop-long)
   ;  (print record)
 ;;;check if stopped out on same day of entry
    (when (and long stop-long  (>= (getd date 'open)(getd date 'close))
               (<= (getd date 'low) stop-long))

           (setq stop-long (my-pretty-price stop-long))
           (push (- (* (-  stop-long long)(calculate-point-value date-1))(comm+slip date-1)) trades) 
;           (format T "~% Trades = ~A" trades)
          (setq trade-long (apply #'vector
                  (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'S
                                           stop-long (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))

;           (format T "11 stop-long = ~A long = ~A" stop-long long)
           (setf (nth 3 record) (+ (nth 3 record) 
                    (* (calculate-point-value date)(- stop-long long))))
           (setf (nth 3 record) (- (nth 3 record)
                   (* (calculate-point-value date)(- (getd date 'close) long))))
 
          (setq trade-long nil long nil stop-long nil ))

    (when (and short stop-short  (<= (getd date 'open)(getd date 'close))
                 (>= (getd date 'high) stop-short))

           (setq stop-short (my-pretty-price stop-short))
           (push (- (* (- short stop-short)(calculate-point-value date-1))(comm+slip date-1)) trades)
           (setq trade-short (apply #'vector 
                 (append trade-short (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'S
                                            stop-short (- short stop-short)))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))

           (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date) (- short stop-short))))
           (setf (nth 3 record) (- (nth 3 record)
                 (* (calculate-point-value date)(- short (getd date 'close)))))

 
           (setq trade-short nil short nil stop-short nil ))
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
         (svref ith 7)(svref ith 8)(round (* (index-point-value) (svref ith 8))))

     ));;closes the dolist and with-open-file
  ; ); closes the when

    (values (round  (list-sum trades))
            (length trades) trades)
   ));;;closes the let and the defun


(defun dubaiswing-trade-bins (&rest features)
  (let (bin path)
     (setq path (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat"))
 
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

(defun dubaiswing-trade-binsb ( &rest features)
  (let (bin path)

    (setq path (string-append *upper-dir-warehouse*  "dubaiswingtradewarehouse.dat"))
    (maind-x)(set-cat-list)
    (setq swings nil swing-bin-codes nil)(clrhash *swing-trade-warehouse*)
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


;;;requires a base features list
(defun dubaiswings-leave-one-out (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'dubaiswing-trade-bins (remove ith base-features)))

    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))

;;;requires a candidate list to add to the base features
(defun dubaiswings-add-one-in (base-features candidate-features)
 (let (winners-list (result 0) average-profit ignore single-bins)  

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'dubaiswing-trade-bins (append base-features (list ith))))
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

;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-dubaiswingtrade-indicators ()
   (let ((path (string-append *upper-dir-warehouse* "dubaiswingtradewarehouse.dat"))
          date-1 date-2 )
  (maind-x)(set-cat-list)(setq swings nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))

  (dolist (ith swings)
     (set-market (svref ith 0))
     (setq date-1 (getd (svref ith 1) 'ydate) date-2 (getd date-1 'ydate))

   ;  (setf (svref ith 14) (volatility-ratio-index (add-mkt-days date-1 -2) 1 14 1));;feature 12
    
      (setf (svref ith 4)(r-squared-change-index (getd date-1 'ydate) 5 1)) ;;feature 2
   ;   (setf (svref ith 16) (body-range-index (getd date-2 'ydate))) ;feature 14
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


(defun dubaiswing-trades (&optional tdate (market-list *swing-list*) (outfile t) date1)
   (let ( date  path1 path2  profit trades new-profit new-trades trade-list (twr-long 1) (twr-short 1) bin
           path3 epsignal longs long-gains long-acc shorts short-gains short-acc (recommendation nil))
       (declare (ignore profit trades new-profit new-trades trade-list))
       (declare (special epsignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin))

	(ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

        (setq path1  (string-append *daily-output-dir* "dubaiswing-trades.dat") path2 (string-append *daily-output-dir* "dcr.xml")
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

     

        (apply #'swing-trade-binsb *swing-features*) ;;;need just once for all markets

        (dolist (market market-list)
           (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

          (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

          (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

          (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-dubaiswingtradesb date *swing-features*))

           (setq twr-short (swing-bin-twr bin)) (setf (nth 0 bin) 1)
           (setq twr-long (swing-bin-twr bin))

           (with-open-file (stream3 path3 :direction :output :if-exists :append :if-does-not-exist :create)

           (setq recommendation (or (find-best-swing-trade date stream date1) recommendation))
         ;  (find-best-contraswing-trade1 date stream stream1 date1)

           ));;closes stream1 and stream3

        ; (terpri stream)

          );;;closes the stream
          );;;closes the dolist

          (unless recommendation
            (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
              (format stream "~% NO RECOMMENDATIONS FOR  ~A~%" (date-convert date1))))

))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-dubaiswingtradesb (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (crit-acc .4)(crit-pf 2.0)
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0)(gains-long 0)(losses-long 0)(gains-short 0)(losses-short 0))
    (setq record (create-dubaiswingtrade-entry-record-list date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

     (dotimes (ith 4)
    (setq bin (encode-swing-trades record features))
    (setq contents (gethash bin *swing-trade-warehouse*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *swing-trade-warehouse*) contents))
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

;;;;;;;;;;;;;;;;;;;;;FOR FINDING OUT WHAT BOTTOMS and TOPS look like.
(defvar lextremes nil)
(defvar hextremes nil)
(defvar nextremes nil)
(defvar price-vectors nil)

(defun populate-swing-vectors (tdate &optional (markets (append *forex-warehouse-list* *day-list*)))
 (let (lextreme hextreme)
    (maind-x)(set-cat-list)
   (setq lextremes nil hextremes nil)
   (dolist (ith markets)
       (set-market ith)
       (multiple-value-setq (lextreme hextreme)(populate-vectors tdate (available-days ith tdate 550)))
       (setq lextremes (append lextreme lextremes) hextremes (append hextreme hextremes))
    )
    
))
(defun populate-all-vectors (tdate &optional (markets (append *forex-warehouse-list* *day-list*)))
  (let (date  (period 5) num (path1 "~/exitpoints/all-vectors.lisp"))
  (setq price-vectors nil)
  (dolist (ith markets)
   
    (setq num (available-days ith tdate 550) date (add-mkt-days tdate (- num))) 
   (dotimes (jth num)
     (push (create-record-vector date period 'A) price-vectors)
     (setq date (getd date 'ndate))
    ))
  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (dolist (ith price-vectors)
     (format str "~A~%" ith)))
))

;;;feature is an index into the vector
(defun feature-stats (feature)
  (let ( (path1 "~/exitpoints/all-vectors.lisp") lfeature results)
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
                   (round (+ (svref kth lfeature)(second (assoc (svref kth feature) results)))))
             (setf (third (assoc (svref kth feature) results))
                   (1+ (third (assoc (svref kth feature) results))))
             (setf (fourth (assoc (svref kth feature) results))
                   (round (/ (second (assoc (svref kth feature) results))
                             (third (assoc (svref kth feature) results))))))
         (setq results (cons (list (svref kth feature) (svref kth lfeature) 1 
                             (svref kth lfeature)) results))))

 (vsort results #'> 'fourth)

))  
         



(defun populate-vectors (tdate num)
   (let ((startdate (add-mkt-days tdate (- num)))(period 5) prices turn-dates low-dates high-dates
        lextremes hextremes extremes (filt *n-filt*))
;;;these are the dates of the extremes
  (setq *n-filt* (* 3 period)); *time-interval* 1440)
  (multiple-value-setq (prices turn-dates) (find-all-primitives (format nil "~A" startdate)
                                                                (format nil "~A" tdate)))
  (setq prices (butlast prices 3) turn-dates (butlast turn-dates 3) *n-filt* filt)
   ;     *time-interval* 'daily-high-low)
;  (format T "NUM Prices= ~A  NUM turn-dates= ~A~%" (length prices)(length turn-dates))
  (setq turn-dates (mapcar #'(lambda(s) (getnumdate s)) turn-dates))
  
    (do* ((prs prices (cdr prs))
         (inds turn-dates (cdr inds))
         (kth (first prices) (first prs))
         (kth+1 (second prices) (second prs))
         (ith (first turn-dates) (first inds)))

        ((null kth+1))
        (if (< kth kth+1)(push ith low-dates)(push ith high-dates))
      )
  ; (format t "NUM low-dates= ~A NUM high-dates= ~A~%" (length low-dates)(length high-dates))
   (setq low-dates (reverse low-dates) high-dates (reverse high-dates))
 
;;;;builds vectors of days
   (dolist (jth low-dates)
     (push (create-record-vector jth period 'L)  extremes)
    )
  (dolist (jth high-dates)
     (push (create-record-vector jth period 'H)  extremes)
    )
  (setq lextremes (remove-if #'(lambda(s) (eql (svref s 2) 'H)) extremes))
  (setq hextremes (remove-if #'(lambda(s) (eql (svref s 2) 'L)) extremes))
  (setq Nextremes (remove-if #'(lambda(s) (or (eql (svref s 2) 'H)
                                              (eql (svref s 2) 'L))) extremes))

;  (format t "NUM Lextremes= ~A  NUM Hextremes= ~A NUM Nextremes= ~A~%" (length lextremes)(length hextremes)(length nextremes))
(values lextremes hextremes) 
))
;;;;feature is an index into the record vector
;;;lst is either lextremes or hextremes
(defun create-bins (lst feature)
  (let (results (ilst (mapcar #'(lambda(s)(svref s feature)) lst))) 
;  (format t "ilst= ~A" ilst)
  (dolist (ith ilst)
    (ifn (assoc ith results)
         (setq results (acons ith 1 results))
         (setf (cdr (assoc ith results))(1+ (cdr (assoc ith results))))))
        ; (setf (cdr (assoc ith results))(+ (cdr (assoc ith results))))))
 (vsort results #'> 'cdr)
))

(defun display-indicators (lst)
  (dotimes (ith (-  (length (car lst)) 3))
      (format T "Feature= ~A ~% ~A~%~%" (1+ ith) (create-bins lst (+ 3 ith))))
)

;;;direction is either low or high   
 (defun create-record-vector (date period typ)
   (let (  (date-1 (getd date 'ydate)) 
       
             )
       ; (setq date-2 (getd date-1 'ydate))
      
       (vector *data-name* date typ
           
             (wpp date );;;feature 3
             (candle1 date);;feature 4
             (reversal-dayp date );;feature 5    
           
             (volatility-ratio-index date 4 28 1);;feature 6            
             (rsi2x-index date 3);feature 7            
             (lproj-index date period);feature 8

             (lprojdelta date period);;feature 9
             (ldev-index date period);;feature 10           
             (momentum-divergence2 date period (* 3 period));;;feature 11
                       
             (channel-direction date (* 1 period)) ;feature 12
             (channel-direction date (* 2 period));feature 13
             (channel-direction date (* 4 period));;;feature 14
         
             (pivot-turn date 'month);;;feature 15
             (pivot-turn date 'week);;; feature 16                                
             (reflect3 date period);feature 17

             (macd-index1 date 6 15 3);;feature 18
             (day-bar-type2 date);;feature 19
             (day-bar-type2 date-1);;;feature 20
             (ep-roc-index10 date 10);;feature 21
             (volatility-ratio-index date 1 14 1) ;;;feature 22   

             (slow-stochastic-index date 30) ;;feature 23
                                      
             (ep-roc-change-index date 3 10);;;feature 27

             (volatility-ratio-index date 3 63 1);;;feature 28
             (cloud-index date)  ;;;feature 29
             (ep-roc-index10 date 2);;;feature30
             (daily-change$ (getd date 'ndate))
             (body-range$ (getd date 'ndate)); feature 22;;must be last feature
                         );;;closes the vector

))
;;;this defines what a usual top looks like.
;;;
(defun swing-topp (tdate period days)
    (let ((date tdate)  md2 chan5 chan10  pivm pivw proj can rev roc5 rdel dev reflect wppp top)
    
    (dotimes (kth days)
        (setq md2 (momentum-divergence2 date period (* 3 period))
              chan5 (channel-direction date (* 1 period))
              chan10 (channel-direction date (* 2 period))
              
              rev (reversal-dayp date) dev (ldev-index date period)
              proj (lproj-index date period)
              pivw (pivot-turn date 'week) 
       ;  projd (lprojdelta date period)
             pivm (pivot-turn date 'month)
            roc5 (ep-roc-index10 date period )
             rdel (rsi2x-index date 3)
            reflect (reflect2 date (* 2 period))
         can (candle-composite date 2) wppp (wpp date ) 
         )
      ;   (format T "wppp= ~A chan5= ~A chan10= ~A chan20=~A~%" wppp chan5 chan10 chan20)
      ;   (format T "dev= ~A pivw= ~A pivm= ~A proj= ~A roc5=~A~%" dev pivw pivm proj roc5)
      ;   (format T "can= ~A rev= ~A reflect= ~A md2= ~A~%" can rev reflect md2)
        (setq top
             (and      
                 (or (member wppp '(UUU UDD UUD ODH OUH)) (eql rev 'D1))
                 (member chan5 '(UC+ UC AT US AT2 DC+))
                 (member chan10 '(UC+ UC AT US AT2 DC+))
                 (member dev '(-1 4 -3 3 2))
                 (member proj '(2 1 3 -1 4))
                 (or (member pivw '(R1 R2 PP S0 H))(member pivm '( PP R1 R0 ))
                    ; (>= roc5 4))
                     (= can -1)(>= roc5 4)(member md2 '(DN1 DN2)))
                 (member reflect '(0 -1 2))
                 ;(/= rdel 5)
                 (> rdel -4)(/= can 1)(neql rev 'U1)(neql rev 'D2)
                 (not (member md2 '(UP1 UP2)))    
                
           ))
         (if top (return) (setq date (getd date 'ydate)))
           )

 
    top 

      
  ))
;;;;defines what a usual bottom looks like.
(defun swing-bottomp (tdate period days)
 (let ((date tdate)  md2 chan5 chan10  pivm pivw proj can rev roc5 rdel dev reflect wppp bottom)
    
   (dotimes (kth days)
       (setq md2 (momentum-divergence2 date period (* 3 period))
         chan5 (channel-direction date (* 1 period))
         chan10 (channel-direction date (* 2 period))
       ;  chan20 (channel-direction date (* 4 period))
         rev (reversal-dayp date) dev (ldev-index date period)
         proj (lproj-index date period)
          pivw (pivot-turn date 'week)
       ;  projd (lprojdelta date period)
          pivm (pivot-turn date 'month)
         roc5 (ep-roc-index10 date period)
         rdel (rsi2x-index date 3)
         reflect (reflect2 date (* 2 period))
         can (candle-composite date 2) wppp (wpp date ) 
         
         ) 
     ;    (format T "wppp= ~A chan5= ~A chan10= ~A ~%" wppp chan5 chan10 )
     ;    (format T "dev= ~A pivw= ~A pivm= ~A proj= ~A roc5=~A~%" dev pivw pivm proj roc5)
     ;    (format T "can= ~A rev= ~A reflect= ~A md2= ~A~%" can rev reflect md2)

        (setq bottom
           (and 
                (or (member wppp '(DDD DUU DDU OUL ODL)) (eql rev 'U1))
                (member chan5 '(DC- DC BT DS BT2 UC-))
                (member chan10 '(DC- DC BT DS BT2 UC-))
                
                (member dev '(1 -4 3 -3 -2))
                (member proj '(-2 -1 -3 1 -4))
                (or (member pivw '(S1 S2  PP R0 L)) (member pivm '( PP S1 S0 ))
                    ; (<= roc5 -4))
                    (= can 1)(<= roc5 -4)(member md2 '(UP1 UP2)))
                (member reflect '(0 1 -2))
               ; (/= rdel -5)
                ;(< rdel 4)
                (/= can -1)(neql rev 'D1)(neql rev 'U2)
                (not (member  md2 '(DN1 DN2)))
             ))
         (if bottom (return) (setq date (getd date 'ydate))))


     bottom

))
;;;for exiting long trades
(defun swing-exit-topp (tdate period)
   (let (ch5 ch10 wppp can dirp rev dev mpiv wpiv rdel)
   (setq dirp (n-day-extreme-direction tdate period)
        ch5 (channel-direction tdate period) rev (reversal-dayp tdate)
        ch10 (channel-direction tdate (* 2 period)) dev (ldev-index tdate period)
        wppp (wpp tdate) wpiv (pivot-turn tdate 'week) mpiv (pivot-turn tdate 'month)
        can (candle-swing tdate) rdel (rsi2x-index tdate 3))

        (or (and (member ch5 '(US))(eql dirp 'UP)
                 (member ch10 '(AC))
                 (member wppp '(IDF))
                 (= can -1))  
           
            (and (member ch5 '(AT2))(eql dirp 'UP)
                 (member ch10 '(AT2))
                 (member wppp '(IDH))(>= rdel 4)
                 (= can -1))  
             
           (and (member ch5 '(DC+))(eql dirp 'DOWN)
             (member ch10 '(DC-))
             (member wppp '(IDL))
             (eql wpiv 'PP)
              (>= rdel 4)) 

            (and (member ch5 '(US))(eql dirp 'UP)
                  (member ch10 '(DC+))
                  (member wppp '(UDD))(eql rev 'D1)
                  (= can -1))  

            (and (member ch5 '(DC+))(eql dirp 'UP)
                 (member ch10 '(AT2))
                 (member wppp '(UUU))(eql mpiv 'L)
                 (>= dev 4))  

            ; (and (swing-bottomp tdate period 1)(= rdel 5))
            ; (>= rdel 5)

            (and (member ch5 '(BT))(member ch10 '(BT))  
                  (member wppp '(IUL)) (= rdel 5))
              (and (member ch5 '(AT2))(member wppp '(DUU))
                   (= can -1))
          )

))
;;;for exiting short trades
(defun swing-exit-bottomp (tdate period)
   (let (ch5 ch10 wppp can dirp rev dev mpiv wpiv rdel)
   (setq dirp (n-day-extreme-direction tdate period)
        ch5 (channel-direction tdate period) rev (reversal-dayp tdate)
        ch10 (channel-direction tdate (* 2 period)) dev (ldev-index tdate period)
        wppp (wpp tdate) wpiv (pivot-turn tdate 'week) mpiv (pivot-turn tdate 'month)
        can (candle-swing tdate) rdel (rsi2x-index tdate 3))

       (or  (and (member ch5 '(DS))(eql dirp 'DOWN)
                 (member ch10 '(BC))
                 (member wppp '(IUF))
                  (= can 1)) 

            (and (member ch5 '(BT2))(eql dirp 'DOWN)
                 (member ch10 '(BT2))
                   (member wppp '(IUL))(<= rdel -4)
                   (= can 1))  
        

             (and (member ch5 '(UC-))(eql dirp 'UP)
                  (member ch10 '(UC+))
                  (member wppp '(IUH))
                  (eql wpiv 'PP)
                  (<= rdel -4)) 

             (and (member ch5 '(DS))(eql dirp 'DOWN)
                  (member ch10 '(UC-))
                  (member wppp '(DUU))(eql rev 'U1)
                  (= can 1))  
       
             (and (member ch5 '(UC-))(eql dirp 'DOWN)
                 (member ch10 '(BT2))
                 (member wppp '(DDD))(eql mpiv 'H)
                 (<= dev -4))  

            ; (and (swing-topp tdate period 1)(= rdel -5))
            ;  (<= rdel -5)
              (and (member ch5 '(BT2))(member wppp '(UDD))
                   (= can 1))
        
              (and (member ch5 '(AT))(member ch10 '(AT));;USDZAR 20081016
                    (member wppp '(IDH)) (= rdel -5))
              (and (member ch5 '(BT2))(member wppp '(UDD))
                   (= can 1))
              

              )

))
;;;;foretells if the next day will have a close higher than the open

(defun forecast-forex-day (tdate)
 (let (wppp lproj roc5 ch5 ch10 ch20 vol4 ldev mdv5 can pivm pivw rev rsid)
 
   (setq wppp (wpp tdate)
        lproj (lproj-index tdate 5)
        roc5 (ep-roc-index10 tdate 5)
        ch5 (channel-direction tdate 5)
        ch10 (channel-direction tdate 10)
        ch20 (channel-direction tdate 20)
        vol4 (volatility-ratio-index tdate 4 28 1)
        ldev (ldev-index tdate 5)
        mdv5   (momentum-divergence2 tdate 5 15)
        can (candle-swing tdate)
        pivm (pivot-turn tdate 'month)
        pivw (pivot-turn tdate 'week)
        rev (reversal-dayp tdate)
     
    )
 
   (cond ((and  (or  (member wppp '(OUH IDL IUF IUH IDH UDU IDF))
                    )
                (or    (member ch5 '(IC+ IC- AC AT UC+ UC DC+ DS))
                   (member ch10 '(AT BT2 BC BT AC UC)))
               (or (eql mdv5 'DN1)
                   (member vol4 '(-4));; > $4 forex-warehouse-list
                    (= can 1)
                   (member pivm '(R0 H ))
                   (member pivw '(R1 PP R2))
                  
                   )
               (or (= roc5 -4)(eq rev 'D2)(member ldev '(2 3 ))
                  ; (member lproj '(5 -5))                   
                  ; (member ch20 '(IC+ AT BT2 BC US AC))
                   )) 'BULL)
               

         ((and  (or  (member wppp '(UUD ODF OUL DUD DUU DDD ))
                     )
                (or  (member ch5 '(BT DC- AT2 BC US UC-))
                     (member ch10 '(DC- AT2 IC+ DC)))
               (or 
                   (eql mdv5 'UP1)
                   (member vol4 '(4)) ;;; makes < $-4 forex-warehouse-list 
                   (= can -1)
                   ;(member pivm '(S0 PP R2 S1))
                   (member pivw '(H L S0))
                   (member rsid '(4 -5))
                    )
                (or (= roc5 4)(eql rev 'U2)(member ldev '(4 -4 ))
                    ;(member lproj '(-4 -3))
                   ; (member ch20 '(IC- DC- BT DS UC-))
                    )) 'BEAR)

          (t 'FLAT)

       )
)) 
;;;;based on all vectors forex-warehouse-list body range$
(defun wpp-fx-composite (tdate)
  (let ((wppp (wpp tdate)))
   (cond ;((and (member wppp '(IUF IDL IDF IUH ODH UDU UDD OUF OUH IDH IUL OUL))
         ;      (member wppp '(ODF UUD ODH OUL IDF DUU DUD IUL DDD IUH UUU UDD))) 2)
         ;((member wppp '(IUH IDL IUF UDD IDH IDF ODH OUH ODL)) 1)
         ;  ((member wppp '(IUF IDL IDF IUH ODH UDU UDD OUF OUH IDH IUL OUL)) 1)
         ((member wppp '(IDL OUH IDH IUF IUH)) 2)
         ((member wppp '(IDF UDU ODL UDD UUU)) 1);;;makes > $3 per body range forex-warehouse-list
        ; ((member wppp '(UUD ODF ODH UUU OUL DUU IUL )) -1)
        ((member wppp '(ODF UUD OUL DUD DUU)) -2)
        ((member wppp '(IUL DDD DDU OUF ODH)) -1)
        ; ((member wppp '(ODF UUD)) -1);;; these make $-5 bodyrange others worthless
          (t 0))))

;;;;based on all vectors *indexes-list* body range$
(defun wpp-indexes-composite (tdate)
  (let ((wppp (wpp tdate)))
   (cond 
         ((member wppp '(IUH OUD ODH  )) 1)
         ((member wppp '(IDF ODF OUF DDU IUF DUU UDD)) 2)
         ((member wppp '(  UDU DDD OUL)) -1)
       
        ; ((member wppp '(ODF UUD)) -1);;; these make $-5 bodyrange others worthless
          (t 0))))


(defun wpp-currency-composite (tdate)
  (let ((wppp (wpp tdate)))
   (cond ((member wppp '(DUD IUH ODF UDD ODL IDH)) 1)
         ((member wppp '(IUL DUU OUL OUF UUU IDF)) -1)
          (t 0))))

(defun reversal-day-index (tdate)
   (let ((rev (reversal-dayp tdate)))
    (cond ((member rev '(D2 )) 1);;D2 > $14
          ((member rev '(U2)) -1);;;U2 < $-6
          (t 0))))

(defun can-currency-composite (tdate)
  (let ((can (wpp tdate)))
   (cond ((member can '(UPMB UPHM DNCC DNDJ DNHM DNEP)) 1)
         ((member can '(DNES UPMS DNMB DIHW UIHW UIST)) -1)
          (t 0))))


(defun pivot-turn-fx-month (tdate)
   (let ((pivm (pivot-turn tdate 'month)))
    (cond ;((member pivm '(H R0)) 1);; makes > $3 for longs forex-warehouse-list
           ((member pivm '(S0 S1 S2)) 1)
         ; ((member pivm '(H L S0 R2 PP)) -1)
           ((member pivm '(R0 R1 R2)) -1)
          (t 0))))

(defun pivot-turn-fx-week (tdate)
   (let ((pivw (pivot-turn tdate 'week)))
    (cond ((member pivw '(L S0 S1 S2)) 1)
         ; ((member pivw '(L H )) 1)
         ; ((member pivw '(L H)) -1);; makes < $-4 for shorts forex-warehouse-list
           ((member pivw '(H R0 R1 R2)) -1)
          (t 0))))


(defun channel-fx5 (tdate)
  (let ((ch5 (channel-direction tdate 5)))
   (cond ;((and (member ch5 '(IC- AC UC BT2 UC+ DC+ AT DC))
         ;      (member ch5 '(BC DC- UC- AT2 BT DC UC BT2 ))) 2)
        ; ((member ch5 '(IC- AC UC BT2 UC+ DC+ AT DC)) 1)
         ((member ch5 '(IC- IC+ AC UC+)) 2);;makes >= $3 forex-warehouse-list
         ((member ch5 '(UC DC+ AT DS )) 1)
         ((member ch5 '(DC- BT BC AT2)) -2)
         ((member ch5 '(US UC- DC BT2)) -1) 
        ; ((member ch5 '(BC DC- UC- AT2 BT DC UC BT2 )) -1)
         (t 0))))
   
(defun channel-fx10 (tdate)
  (let ((ch10 (channel-direction tdate 10)))
   (cond ;((and (member ch10 '(BC BT2 AC UC IC+ AT UC+ UC-))
          ;     (member ch10 '(BC AT2 DC DC- UC- DC+ UC UC+))) 2)
       ;   ((member ch10 '(AT BT BT2 BC US)) 1)
        ;  ((member ch10 '(BC BT2 AC UC IC+ AT UC+ UC-)) 1)
         ((member ch10 '(AT BT2 AC BC)) 2);; makes >= $3 forex-warehouse list body-range
         ((member ch10 '(BT US UC UC+)) 1)
         ((member ch10 '(DC- AT2 DC DC+)) -2)
         ((member ch10 '(UC- DS IC- IC+)) -1)     
   ;  ((member ch10 '(AT2 AT US DC- DC BC)) -1)
        ; ((member ch10 '(BC AT2 DC DC- UC- DC+ UC UC+)) -1)
         (t 0))))
   
(defun channel-fx20 (tdate)
  (let ((ch20 (channel-direction tdate 20)))
   (cond ((member ch20  '(IC+ AT BT2 US))  2);; makes >= $3 forex-warehouse-list body-range
         ((member ch20 '(BC AT2 AC DC)) 1)
         ((member ch20  '(IC- DC- BT UC-)) -2);;makes <= $-4 
         ((member ch20 '(DS DC+ UC UC+)) -1)
         (t 0))))

   
(defun ldev-fx (tdate)
  (let ((dev (ldev-index tdate 5)))
     (cond ((member dev '(2)) 1);;makes $3
          ; ((member dev '(4 -4)) -1)
           (t 0))))

  
(defun lproj-fx (tdate)
  (let ((proj (lproj-index tdate 5)))
     (cond ((member proj '(5 -5)) 1);;makes >= $6
           ((member proj '(-4 )) -1);;;makes <= $-3
           (t 0))))

(defun roc-fx (tdate)
   (let ((roc5 (ep-roc-index10 tdate 5)))
      (cond ((>= roc5 4) 2)
            ((>= roc5 3) 1)
            ((<= roc5 -4) -2)
            ((<= roc5 -3) -1)
            (t 0))))



(defun candle-fx (tdate)
  (let ((can (candle1 tdate)))
      (cond ;  ((and  (member can '(DNES UPPC DNHR UPMS DNST DNTW DIST DNDJ DNHM UPTW))
            ;      (member can '(DNCC UPMB DNMB UPIH UPHR UIST DNEP DNST UPTW UIHW))) 2) 
            ((member can '(DNES DNHR UPPC DNDJ DNHM)) 2)
            ((member can '(DIST DNTW UPDJ DNST UPEP)) 1)
            ((member can '(DNCC UPMS DNMB UPMB UPHR)) -2)
            ((member can '(DNEP UIHW UPIH UIST DIHW)) -1)
            ((member can '(NIL UPTW UPHM)) 0)
       ;     ((member can '(DNES UPPC DNHR UPMS DNST DNTW DIST DNDJ DNHM UPTW)) 1)
           
       
           ; ((member can '(DNCC UPMB DNMB UPIH UPHR UIST DNEP DNST UPTW UIHW)) -1)
           ; ((member can '(DNCC UPMS DNMB UPMB UPHR DNEP )) -1);;makes < $-3 forex-warehouse-list
            (t 0))))

(defun vol-fx (tdate)
  (let ((vol4 (volatility-ratio-index tdate 4 28 1)))
    (cond ((= vol4 -4) 1);;makes $4 forex-warehouse list body-range
          ((= vol4 4) -1);;makes $-4 forex-warehouse list body-range
          (t 0))))

(defun lprojdelta-fx (tdate)
   (let ((pdelta (lprojdelta tdate 5)))
     (cond ((member pdelta '(9 -4 )) 1)
           ((member pdelta '(-9 7 -10 -8 -6 6 -7)) -1)
           (t 0))))

(defun forecast-fore-day (tdate)
 (let (wppp lproj roc5 ch5 ch10 ch20 vol4 ldev can pivm pivw  rsid projd )
 
   (setq wppp (wpp tdate)
        lproj (lproj-index tdate 5)
        roc5 (ep-roc-index10 tdate 5)
        ch5 (channel-direction tdate 5)
        ch10 (channel-direction tdate 10)
        ch20 (channel-direction tdate 20)
        vol4 (volatility-ratio-index tdate 4 28 1)
        projd (lprojdelta tdate 5)
        ldev (ldev-index tdate 5)
      ;  mdv5   (momentum-divergence2 tdate 5 15)
        can (candle1 tdate)
        pivm (pivot-turn tdate 'month)
        pivw (pivot-turn tdate 'week)
       
        rsid (rsi2x-index tdate 3)
    )
 
   (cond ((and (or ;(member wppp '(OUH IDL IUF IUH IDH UDU IDF))
                   (member wppp '(DUD IUH ODF UDD ODL IDH ))
                   (member can  '(UPMB UPHM DNCC DNDJ DNHM DNEP))
                   ;(member ch5 '(IC+ IC- AC AT UC+ DC+))
                   ;(member ch10 '(AT BT2 BC BT AC UC)))
                   )
               (or (member ldev '(2 3))
                   (member vol4 '(4))
                  
                   (member pivm '(R2 ))
                   (member pivw '(S0 H))
                   (= roc5 3)
                   (member lproj '(5 ))  (member projd '(7 8 9))                                     
                 ;  (member ch20 '(IC+ AT BT2 BC US AC))
                   )) 'BULL)
               

         ((and (or ;(member wppp '(UUD ODF OUL DUD DUU DDD ))
                   (member wppp '(IUL DUU OUL OUF UUU IDF))
                   (member can '(DNES UPMS DNMB DIHW UIHW UIST))
                  ; (member ch5 '(BT DC- AT2 BC UC-))
                  ; (member ch10 '(DC- AT2 IC+ DC)))
                 )
               (or (member ldev '(4 -4))

                   (member vol4 '(-4))
                   (member pivm '(S1 S2 L))
                   (member pivw '(S2 L))
                   (member rsid '(-6))
                   (= roc5 4)
                    (member lproj '(-4 -5))(member projd '(-7 -8 -9))
                  ;  (member ch20 '(IC- DC- BT DS UC-))
                    )) 'BEAR)

          (t 'FLAT)

       )
)) 

;;;; swing trade trend logic NO PSAR
(defun swingtrade-simulation-test2 (market date2 num)
 (let (date stop-long stop-short trades long short (features *swing-features*)
       trade-long  prev-stop-long prev-stop-short 
      ; epsignal longs long-gains long-acc shorts short-gains short-acc bin
        avel3 aveh3  
        grp    cci5 tls5-10
       long-stopped-out short-stopped-out  ccid5-2
         entry-long entry-short
        cci21  vplety vphety vplext vphext 
        cover-long cover-short
        (days-in-trade 0) 
       ave-win ave-loss losers winners extended-trades trade-short 
        trading-dates (trade-time 0)  
        date-1 record (running-sum 0)  draw   
       (singles 0)   ; (stockp (member market *stocks-list*))
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "swing2-summary2.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "swing2-simulation2.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "swing2-diary2.dat")))
       
   (set-market market)(format T "~%~A~%" *data-name*)
 
    (if (and num (> num (available-days market date2 600)))  (setq num nil))
    (ifn num (setq num (available-days market date2 600)))
   
    
 ;   (setq features (find-best-swing-indicator-set nil) grp  *forex-list*)

 ;  (setq singles (nth-value 3 (apply #'swing-trade-binsb nil features))) 

  (cond ((member market *forex-warehouse-list*) (build-swingtrade-warehouse (setq grp *forex-list*) )
           (setq *commission* 0  *pips-slippage* 6          ))
        ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 ))
        ((member market *micro-list*)(setq *commission* 10 *pips-slippage* 0))
        ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 ))
       ; (t (warehouse market)
        ;   (setq features (find-best-swing-indicator-set nil))
        ;   (format T "~%market = ~A features = ~A" market features)
        ;   (apply #'swing-trade-binsb nil features)
        ;      (setq *commission* 25  *pips-slippage* 0 )))
    
      (t (build-swingtrade-warehouse (setq grp *micro-list*)) ;(list market))
          ; (setq features (find-best-swing-indicator-set nil))
         (setq features '(6 13 9))
         ;  (if (eql market 'dj.d1b)(setq features '(1 2 3 4 6 7 10)))
         ;  (if (eql market 'sp.d1b)(setq features '(1 4 5 6 11 14 )))
         ;  (if (eql market 'nd.d1b)(setq features '(1 2 3 4 6 7 10)))
         ;  (if (eql market 'ru.d1b)(setq features '(1 2 3 4 6 7 10)))
         ;  (if (eql market 'gc.d1b)(setq features '(1 5 6 8 11)));;;good
         ;  (if (eql market 'si.d1b)(setq features '(1 5 6 8 11)));;;good
         ;  (if (eql market 's.d1b)(setq features '(1 5 6 8 11)));;;good
         ;  (if (eql market 'e1.d1b)(setq features '(1 2 )));;;good
         ;  (format T "~%market = ~A features = ~A" market features)
           (apply #'swing-trade-binsb nil features)
           (setq *commission* 25  *pips-slippage* 0 )))

  
 ;    (build-swingtrade-warehouse (setq grp *day-list*) )
 ;    (apply #'swing-trade-binsb nil *swing-features*)
   
    (setq date (add-mkt-days date2 (- num)))

 ;;;;from date1 to date2
   (dotimes (ith num)

  ; (format T "~%1 ~A = date  ~A = short  ~A = long~%" date short long)
    (if (or long short) (incf days-in-trade))

   
    (multiple-value-setq (vplety vphety)(vprices date *duration* *factor* 1 *type*))
    (multiple-value-setq (vplext vphext)(vprices date *duration* *factor* 1 *type*))
    (setq      
      ; cci3 (commodity-channel-index date 3)  
      ; rsis9-21 (rsi-signal date 9 21) ref4 (reflect3 date 4)
      ccid5-2 (roc date 2 'cci 5)
      ; wp (wpp1 date) wpp2 (wpp date) pivw (pivot-index date 'week)
       tls5-10 (timing-line-signal3 date 5 10)     
     ; pivd (pivot-index date 'day)
      ; cci21x (ccix-ob-os date 21 11) ldv (ldev-index date 7)
      ;  tcr (time-cycle-ratios date) tcr1 (time-cycle-ratios (getd date 'ydate))
     ; fsts21-5 (fast-stochastic-signal date 21 5) 
     ; fsts5-3 (fast-stochastic-signal date 5 3) ldev3 (ldev-index date 3)
      aveh3  (ave date 3 'high) ;exth (n-day-high date 3) rsi2 (rsi2x date 2)
      cci21 (commodity-channel-index date 21) cci5 (commodity-channel-index date 5)
       ;  aveh (my-pretty-price (n-day-high date mvpar 'high)) cci21 (commodity-channel-index date 21)
      avel3 (ave date 3 'low) ;extl (n-day-low date 3) mvhl (mvhl-trend date 3)
      ;gsi (gann-slope-index date 5 63)
       ; pivm (pivot-index date 'month) pivw (pivot-index date 'week)
         )
    ; (multiple-value-setq (stop-long stop-short)(vprices5 date 4 1.236 1))
    ; (multiple-value-setq (cover-short cover-long)(vprices5 date 4 1.618 1))
     (setq entry-short (fmax vplety (n-day-low date 2)) cover-short nil ;(- avel (index-tick-size)) 
          stop-short vphext  long-stopped-out nil 
          entry-long (fmin vphety (n-day-high date 2)) cover-long nil;(+ aveh (index-tick-size))
          stop-long vplext  short-stopped-out nil) 
  

    (setq stop-short (fmin prev-stop-short stop-short) stop-long (fmax prev-stop-long stop-long))


    (setq prev-stop-short stop-short prev-stop-long stop-long)
#|
;;;which comes first the target or the stop loss?
;;;targethf for longs 
    (setq targethf  (or (and cover-long (> (getd date 'high) cover-long)
                             (< (getd date 'open)(getd date 'close)) ;;;low came first but hit target not stop loss
                             (> (getd date 'low) stop-long))
                        (and cover-long (> (getd date 'high) cover-long)
                             (> (getd date 'open)(getd date 'close)))))                        
;;;targetlf for shorts
    (setq targetlf  (or (and cover-short (< (getd date 'low) cover-short)
                             (> (getd date 'open)(getd date 'close)) ;;high came first but hit target not stop loss
                             (< (getd date 'high) stop-short))
                        (and cover-short (< (getd date 'low) cover-short)
                             (< (getd date 'open)(getd date 'close)))))                        

|#

    (setq date-1 date date (add-mkt-days date 1))
      
;      (setq date-2 (getd date-1 'ydate))

       (setq record (list date (getd date 'close) 0 0 0))
       (if long (setf (nth 2 record) 1))
       (if short (setf (nth 2 record) -1))
 
    (when (and (getd date 'rollover) long stop-long)
             (setq long (+ long (getd date 'rollover)));(comm+slip-points date))
                  ; cover-long (+ cover-long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover))
           
              (setf (nth 3 record)(- (nth 3 record)(* (calculate-point-value date) (getd date 'rollover))))
        )

    (when (and (getd date 'rollover) short stop-short)
            (setq short (+ short (getd date 'rollover))); (- (comm+slip-points date)))
                  ;cover-short (+ cover-short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover))
             (setf (nth 3 record) (+ (nth 3 record) (* (calculate-point-value date)(getd date 'rollover))))
         )

;    (format T "~%2 date =~A  short= ~A  long= ~A stop-long = ~A stop-short= ~A entry-short= ~A entry-long= ~A~%" 
;              date short long stop-long stop-short entry-short entry-long)
 ;
;;;calculate bin-classifier 
   
;      (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
;                  (bin-classifier-swingtradesb date-1 features))
#|
;;;check if exited with cci-signal change at the open of the day
        (when (and long  
                 (>= cci3 100)  
                ; (or (and (> rsi2 90) (> cci5l-2 100))
                ;     (= mvhl -1)); (> ccid21-1 0)
                     )
          (push (round (- (*  (- (getd date 'open) long)(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'C
                                        (getd date 'open)
                       (my-pretty-price  (- (getd date 'open)  long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
               (* (calculate-point-value date)(my-pretty-price (-  (getd date 'open) (getd date-1 'close))))))
          (setq trade-long nil long nil stop-long nil  prev-stop-long nil   long-stopped-out nil))

        (when (and short
                  (<= cci3 -100)
                 ; (or (and (< rsi2 10) (< cci5l-2 -100))
                 ;     (= mvhl 1)); (> ccid21-1 0)
                            )                                             
              
            (push (round (- (*  (- short (getd date 'open))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'C
                                            (getd date 'open)
                 (my-pretty-price (- short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
              (* (calculate-point-value date)(my-pretty-price (- (getd date-1 'close) (getd date 'open))))))
          (setq trade-short nil short nil stop-short nil  prev-stop-short nil  short-stopped-out nil))



;;;check if exited with objective
   (when  (and long cover-long targethf)
          (push (round (- (* (-  (max (getd date 'open) cover-long)  long)
                  (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                       (my-pretty-price (- (max (getd date 'open) cover-long) long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (*  (calculate-point-value date)
                    (my-pretty-price (- (max cover-long (getd date 'open)) (getd date-1 'close))))))

          (setq trade-long nil long nil stop-long nil  cover-long nil  prev-stop-long nil))

   (when  (and short cover-short targetlf)
          (push (round (- (* (- short (min (getd date 'open) cover-short))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                 (my-pretty-price (- short (min (getd date 'open) cover-short)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)
                  (my-pretty-price (- (getd date-1 'close) (min cover-short (getd date 'open)))))))

          (setq trade-short nil short nil stop-short nil  cover-short  nil  prev-stop-short nil))

|#

;;;check if stopped out

   (when (and long stop-long (< (getd date 'low) stop-long)
             )
         ;  (format T "~%6 date = ~A long = ~A low = ~A stop-long = ~A trade-long = ~A"
         ;  date long (getd date 'low) stop-long trade-long)
         (push (round (- (* (-  (min stop-long
                                     (getd date 'open)) long)
                               (calculate-point-value date))
                         (comm+slip date))) trades)
          (setq trade-long (apply #'vector
             (append trade-long  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                      (contract-month *data-name* date) 'S
                                       (min stop-long (getd date 'open))
                                    (my-pretty-price (- (min stop-long
                                                            (getd date 'open)) long))))))

           (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date) (my-pretty-price (- (min stop-long (getd date 'open))
                                                                    (getd date-1 'close))))))

          (setq trade-long nil long nil stop-long nil prev-stop-long nil  long-stopped-out T))

   (when (and short stop-short (> (getd date 'high) stop-short)
            )
       ;   (format T "~%6 date = ~A short = ~A high = ~A stop-short = ~A trade-short = ~A"
       ;       date short (getd date 'high) stop-short trade-short)
          (push (round (- (* (- short (max stop-short
                                          (getd date 'open)))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))
                    (contract-month *data-name* date) 'S
                    (max stop-short (getd date 'open))
                   (my-pretty-price (- short (max stop-short 
                                             (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price (- (getd date-1 'close)
                                                                   (max stop-short
                                                                        (getd date 'open)))))))
          (setq trade-short nil short nil stop-short nil  prev-stop-short nil  short-stopped-out T))

;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date)(my-pretty-price
                                                (- (getd date 'close) (getd date-1 'close)))))))
      (if short (setf (nth 3 record)(+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price
                                               (- (getd date-1 'close)(getd date 'close)))))))

 ;     (format T "~%3 date= ~A  entry-short= ~A  entry-long= ~A stop-long= ~A cover-long= ~A "
  ;           date entry-short entry-long stop-long cover-long)
    
;;;check if new entry short
 ;      (format t "~%1date= ~A  ccis5= ~A   fstd= ~A rsix= ~A roc21d1= ~A  avel = ~A aveh = ~A short= ~A "
 ;          date-1  ccis5 fstd rsix roc21d1 avel aveh short) 
;       (format t "~%2date = ~A epsignal = ~A chan21= ~A" date epsignal chan21)
;
       (when  (and (not short)(not long)(not short-stopped-out) 
                                                        
                  (fltn (getd date 'low) entry-short)
                  
                  (minusp ccid5-2)
                  (or (= tls5-10 2)(= tls5-10 3))
                  (> cci5 0)
                      
              ;(member epsignal '(DOWN OK)) 
         
                   );;entry short

          (setq short (fmin (getd date 'open) entry-short)
              
                      ; entry-short
                 trade-short (list date 'short short)
                 ; cover-short (nth-value 0 (vprices5 date-1 4 1.382 4))
		 stop-short (fmin (+ vphext (index-tick-size)) (+ aveh3 (index-tick-size)))
                 long-stopped-out nil cover-short nil;(- avel (index-tick-size))    
                 prev-stop-short stop-short days-in-trade 1
                )
  ;     (format t "~%7date = ~A  short = ~A epsignal = ~A shorts = ~A short-gains = ~A"
   ;        date  short epsignal shorts short-gains) 

          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)
                   (my-pretty-price; (- short (getd date 'close))
                          (- short (getd date 'close)(comm+slip-points date)))))
              
              ))
;;new entry long 
 
    (when  (and (not long)(not short)(not long-stopped-out)         
               
                (fgtr (getd date 'high) entry-long)
              
                 (plusp ccid5-2)
                 (or (= tls5-10 -2)(= tls5-10 -3)) ;trend reversal
                 (< cci5 0)
                          
                         
               ;   (member epsignal '(UP OK))             
                    );;first entry long

          (setq long (fmax (getd date 'open) entry-long) 
                    
                 trade-long (list date 'long long)
                 cover-long nil
                 stop-long (fmax (- vplext (index-tick-size)) (- avel3 (index-tick-size)))
                 short-stopped-out nil cover-long nil;(+ aveh (index-tick-size)) 
                 prev-stop-long stop-long days-in-trade 1
                 )
 ;      (format t "~%8date = ~A  long = ~A epsignal = ~A longs = ~A long-gains = ~A"
  ;         date long epsignal longs long-gains) 

           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                         (* (calculate-point-value date) ; (my-pretty-price; (- (getd date 'close) long)))))
                           (my-pretty-price (- (getd date 'close) long (comm+slip-points date)))))
             ))

  ;   (format T "~%4 ~A = date  ~A = short  ~A = long" date entry-short entry-long)
;     (format T "~%4a ~A = epsignal  ~A = long  ~A = short stop-long = ~A" epsignal long short stop-long)
   ;  (print record)


 ;;;check if stopped out on same day of entry
    (when (and long stop-long 
                 (<= (getd date 'low) stop-long)
              ; (or (and (<= (getd date 'low) stop-long)
              ;          (>= (getd date 'open) (getd date 'close))
              ;            )
                  
              ;    (<= (getd date 'close) stop-long))
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
           (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date) (my-pretty-price (- stop-long long)))))
           (setf (nth 3 record) (- (nth 3 record)
                     (* (calculate-point-value date)(my-pretty-price (- (getd date 'close) long)))))
 
           (setq trade-long nil long nil stop-long nil cover-long nil prev-stop-long nil
		 days-in-trade 1 long-stopped-out T))



    (when (and short stop-short 
                 (>= (getd date 'high) stop-short)
                 ;(or (and (>= (getd date 'high) stop-short)
                 ;       (<= (getd date 'open) (getd date 'close))
                 ;         )
                 ; 
                 ;   (>= (getd date 'close) stop-short))
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
           (setf (nth 3 record) (+ (nth 3 record)
                     (* (calculate-point-value date) (my-pretty-price (- short stop-short)))))
           (setf (nth 3 record) (- (nth 3 record)
                     (* (calculate-point-value date)(my-pretty-price (- short (getd date 'close))))))
 
           (setq trade-short nil short nil stop-short nil cover-short nil prev-stop-short nil
		 days-in-trade 1 long-stopped-out T))

#|
;;;check if exited with objective on same day as entry
   (when  (and long cover-long 
                     (or (and (> (getd date 'high) cover-long)
                              (<= (getd date 'open) (getd date 'close)))
                  
                    (>= (getd date 'close) cover-long)))  

          (push (- (* (-  (max (getd date 'open) cover-long)  long)
                       (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                       (- (max (getd date 'open) cover-long) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (max cover-long (getd date 'open)) (getd date-1 'close))))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))

          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 1))

   (when  (and short cover-short
                     (or (and (<= (getd date 'low) cover-short)
                        (>= (getd date 'open) (getd date 'close)))
                  
                  (<= (getd date 'close) cover-short)))
            

          (push (- (* (- short (min (getd date 'open) cover-short))
                   (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                  (- short (min (getd date 'open) cover-short))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (getd date-1 'close) (min cover-short (getd date 'open)))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 1))
|#
  ;     (format T "~%5 ~A = date  ~A = short  ~A = long ~A" date short long trade-long)
  ;     (print record)
           (setf (nth 4 record) (+ (nth 3 record) running-sum))
           (setq running-sum (nth 4 record))

       (push record trading-dates)
       (setq record nil long-stopped-out nil short-stopped-out nil);;;reset to nil at end of each trading date

       
     ) ;;;closes the dotimes

;;;;writes out the diary
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (first ith) (second ith) (third ith) (round (nth 3 ith))
           (round (nth 4 ith)))
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
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F    COMMISSION=   ~A~%~
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
      *commission*
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

;;;; trend system
#|
;;;; PSAR+ trend trading with exit logic
(defun swingtrade-simulation-test3 (market date2 num );&optional (min-days 0))
 (let (date stop-long stop-short trades long short ;(features *swing-features*)
       (features '(1 4)) cci21 ccil5 cci5l-2 cci5h-2
       trade-long  prev-stop-long prev-stop-short  fstk-d  fstk 
       (mvpar 3) ccid5-1 rsi2x fstd21-5 refl16 ccisp5 dbt rsi2
       dirpsar ccis5 fsts21-5 fsts5-3 wp pivd pivw ldev3
       grp wpp2 lregh3 lregl3 ccil21 pivm rsii2 mvhl
       stop-loss-psar  ;late-entry
       long-stopped-out short-stopped-out  sdate ;vlow vhigh
       cci5 aveh avel (days-in-trade 0) cover-long cover-short
       ave-win ave-loss losers winners extended-trades trade-short 
        trading-dates (trade-time 0) 
       ;epsignal longs long-gains long-acc shorts short-gains short-acc bin 
        date-1 record (running-sum 0)  draw  early-exit  entry-long entry-short
       (singles 0)   ; (stockp (member market *stocks-list*))
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "swing3-summary3.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "swing3-simulation3.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "swing3-diary3.dat")))
       
    ; (declare (ignore min-days))
   (set-market market)(format T "~%~A~%" *data-name*)
 
    (if (and num (> num (available-days market date2 600)))
        (setq num  (available-days market date2 600)))
   
  
 ;   (setq features (find-best-swing-indicator-set nil) grp  *forex-list*)
 
  ; (setq singles (nth-value 3 (apply #'swing-trade-binsb nil features))) 

  (cond ((member market *forex-warehouse-list*) (build-swingtrade-warehouse (setq grp *forex-list*) )
         (setq singles (nth-value 3 (apply #'swing-trade-binsb nil features))) 
           (setq *commission* 0  *pips-slippage* 6          ))
        ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 ))
        ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 ))
        (t (build-swingtrade-warehouse (setq grp *day-list*) )(setq *commission* 25  *pips-slippage* 0 )))
  
   ; (setq *entry-factor-swing* 0.0 )

    (setq date (add-mkt-days date2 (- num)))

 ;;;;from date1 to date2
   (dotimes (ith num)

  ; (format T "~%1 ~A = date  ~A = short  ~A = long~%" date short long)
(    (if (or long short) (incf days-in-trade))
 
     (multiple-value-setq (dirpsar sdate stop-loss-psar)(parabolic-stops date))
     (setq dirpsar (if (eql dirpsar 'long) 1 -1))
    (multiple-value-setq (cci5h-2 cci5l-2)(cci-high-low date 5 2))
   
    (setq
        rsi2x (rsi2x-index date 2) mvhl (mvhl-trend date 3)
        cci5 (commodity-channel-index date 5) cci21 (commodity-channel-index date 21) 
        ccid5-1 (roc date 1 'cci 5) ccis5 (cci-signal1 date 5 2) ccil5 (cci-level-index date 5) 
        fstk-d (nth-value 1 (fast-stochastic date 21 5))  ldev3 (ldev-index date 3)      
        lregh3 (lregress date 3 'high) lregl3 (lregress date 3 'low)
        aveh (my-pretty-price (ave date mvpar 'high)) ccisp5 (cci-speed-index date 5)
        avel (my-pretty-price (ave date mvpar 'low)) ccil21 (cci-level-index date 21)
        fstk (+ (fast-stochastic date 21 5) fstk-d)  fstd21-5 (stochastic-delta date 21 5 1)
        fsts21-5 (fast-stochastic-signal date 21 5)  fsts5-3 (fast-stochastic-signal date 5 3)
        pivd (pivot-index date 'day) refl16 (reflect3 date 16) rsi2 (rsi2x-index date 2)
        pivm (pivot-index date 'month) rsii2 (rsi-index date 2)
        wp (wpp1 date) wpp2 (wpp date) pivw (pivot-index date 'week) dbt (day-bar-type date)
         )

   (multiple-value-setq (entry-short entry-long)(vprices5 date 4 1.125 1))
   (setq stop-long (fmax prev-stop-long entry-short)
         stop-short (fmin prev-stop-short entry-long))
   (setq prev-stop-long stop-long prev-stop-short stop-short)


  (setq date-1 date date (add-mkt-days date 1))
                             
 
        
       (setq record (list date (getd date 'close) 0 0 0))
       (if long (setf (nth 2 record) 1))
       (if short (setf (nth 2 record) -1))
 
    (when (and (getd date 'rollover) long stop-long)
             (setq long (+ long (getd date 'rollover)));(comm+slip-points date))

                ;   cover-long (+ cover-long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover))
            
              (setf (nth 3 record)(- (nth 3 record)(* (calculate-point-value date) (getd date 'rollover))))
        )

    (when (and (getd date 'rollover) short stop-short)
            (setq short (+ short (getd date 'rollover))); (- (comm+slip-points date)))
                ;  cover-short (+ cover-short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover))
         
             (setf (nth 3 record) (+ (nth 3 record) (* (calculate-point-value date)(getd date 'rollover))))
         )

  ;  (format T "~%2 date =~A  short= ~A  long= ~A stop-long = ~A stop-short= ~A ~%" 
   ;            date short long stop-long stop-short)

;;;calculate bin-classifier 
   
;      (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
;                  (bin-classifier-swingtradesb date-1 features))

;;;check if exited with exit criteria
        (when (and long 
                   (or (and (> rsi2 90) (> cci5h-2 100))
                       (= mvhl 1))
                           
                           )
                   ;  ))
          (push (round (- (*  (- (getd date 'open) long)(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'C
                                        (getd date 'open)
                       (my-pretty-price  (- (getd date 'open)  long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
               (* (calculate-point-value date)(my-pretty-price (-  (getd date 'open) (getd date-1 'close))))))
          (setq trade-long nil long nil stop-long nil  prev-stop-long nil  early-exit T long-stopped-out nil))

        (when (and short 
                   (or (and (< rsi2 10) (< cci5l-2 -100))
                       (= mvhl 1)); (> ccid21-1                             )                                      
                       )
              ;))
            (push (round (- (*  (- short (getd date 'open))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'C
                                            (getd date 'open)
                 (my-pretty-price (- short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
              (* (calculate-point-value date)(my-pretty-price (- (getd date-1 'close) (getd date 'open))))))
          (setq trade-short nil short nil stop-short nil  prev-stop-short nil  early-exit T short-stopped-out nil))



;;;check if stopped out of prior position #2 (preparing to reverse position)


   (when (and long stop-long (< (getd date 'low) stop-long)
             )
         ;  (format T "~%6 date = ~A long = ~A low = ~A stop-long = ~A trade-long = ~A"
         ;  date long (getd date 'low) stop-long trade-long)
         (push (round (- (* (-  (min stop-long
                                     (getd date 'open)) long)
                               (calculate-point-value date))
                         (comm+slip date))) trades)
          (setq trade-long (apply #'vector
             (append trade-long  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                      (contract-month *data-name* date) 'S
                                       (min stop-long (getd date 'open))
                                    (my-pretty-price (- (min stop-long
                                                            (getd date 'open)) long))))))

           (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)
                   (my-pretty-price (- (min stop-long (getd date 'open)) (getd date-1 'close))))))


          (setq trade-long nil long nil stop-long nil  early-exit nil long-stopped-out T))

   (when (and short stop-short (> (getd date 'high) stop-short) ;bottomp (eql epsignal 'UP)
            )
       ;   (format T "~%6 date = ~A short = ~A high = ~A stop-short = ~A trade-short = ~A"
       ;       date short (getd date 'high) stop-short trade-short)
          (push (round (- (* (- short (max stop-short
                                          (getd date 'open)))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))
                    (contract-month *data-name* date) 'S
                    (max stop-short (getd date 'open))
                   (my-pretty-price (- short (max stop-short 
                                             (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price (- (getd date-1 'close) (max stop-short
                                                                                             (getd date 'open)))))))
          (setq trade-short nil short nil stop-short nil  early-exit nil short-stopped-out T))

#|
;;;check if exited with objective
   (when  (and long cover-long (>  (getd date 'high) cover-long))
          (push (round (- (* (-  (max (getd date 'open) cover-long)  long)
                  (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                       (my-pretty-price (- (max (getd date 'open) cover-long) long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (*  (calculate-point-value date)(- (max cover-long (getd date 'open)) (getd date-1 'close)))))

          (setq trade-long nil long nil stop-long nil early-exit nil cover-long nil  prev-stop-long nil))

   (when  (and short cover-short (<  (getd date 'low) cover-short))
          (push (round (- (* (- short (min (getd date 'open) cover-short))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                 (my-pretty-price (- short (min (getd date 'open) cover-short)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(- (getd date-1 'close) (min cover-short (getd date 'open))))))

          (setq trade-short nil short nil stop-short nil early-exit nil cover-short  nil prev-stop-short nil))

|#
;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date)(my-pretty-price
                                                (- (getd date 'close) (getd date-1 'close)))))))
      (if short (setf (nth 3 record)(+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price
                                               (- (getd date-1 'close)(getd date 'close)))))))

 ;     (format T "~%3 ~A = date  ~A = entry-short  ~A = entry-long ~A = stop-long "
 ;            date entry-short entry-long stop-long)
    
;;;check if new entry short
 ;      (format t "~%1date = ~A  cci5 = ~A  fstd = ~A cci5d1 = ~A mvhl= ~A macdd = ~A avel = ~A aveh = ~A short= ~A "
 ;          date-1  cci5 fstk-d cci5d1 mvhl macdd avel aveh short) 
;       (format t "~%2date = ~A epsignal = ~A chan21= ~A" date epsignal chan21)
;
       (when  (and (not short)(not short-stopped-out)
                  ;(= dirpsar 1)
                     (<= (getd date 'low) entry-short)
                    
               (or (= pivd 4)(member ccisp5 '(4 5 6))(member rsi2 '(-2))
                   (member wpp2 '(ODL ODF UDU OUL OUF))(member ldev3 '(0 2))
                   (= pivw 5))
               (or (member ccil5 '(1 -1 3 4))(member ccisp5 '(3 -3 7 4 1))
                   (member ccil21 '(-7 -5 -3 -1 2 3 5))(member rsi2 '(-2))
                   (member ldev3 '(0 -2 3))(member pivm '(5 -5 -2)))
               (or (member pivm '(-5 -4 3))(member rsii2 '(3 -2 4))(member ccil5 '(3 4)))
               (or (member pivw '(3 5 -1 1))(member pivd '(5 -1 -2 4))(member ccil5 '(-3 -1 3 4 1)))
              ;    (member epsignal '(OK DOWN))
               
                  ;(plusp fstd21-5);(plusp lreg3s)
                ;  (/= wp 1)
                ;  (not (member wpp2 '(DUD ODL IUH IDF IUL IDL OUL IUF DUU)))
          
                ; (minusp cci5d1);(>= cci21 -100);;too late    
                           
                   );;entry short

          (setq short ;(if (and (minusp mvhl));(minusp dirpsar))
                       ;   (getd date 'open)
                      (min (getd date 'open) entry-short)
                      ; entry-short
                trade-short (list date 'short short)
                cover-short nil days-in-trade 1
                stop-short entry-long
                long-stopped-out nil         
                prev-stop-short stop-short
                )
  ;     (format t "~%7date = ~A  short = ~A epsignal = ~A shorts = ~A short-gains = ~A"
   ;        date  short epsignal shorts short-gains) 

          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)
                   (my-pretty-price; (- short (getd date 'close))
                          (- short (getd date 'close)(comm+slip-points date)))))
              
              ))
;;new entry long 
 
    (when  
            (and (not long) (not long-stopped-out)
                  ;(= dirpsar -1)
                     (>= (getd date 'high) entry-long)
                     
               
               (or (= ccil21 3)(= ccisp5 6)(member rsi2 '(-1 4))(= dbt 33)
                     (member wpp2 '(UDU OUH ODH DDU ODL))(member ldev3 '(0))
                     (= pivw -5))
               (or (member ccil5 '(-1 1 -3 4))(member ccisp5 '(1 2 4 5 6 -3 -4))
                     (member ccil21 '(1 -3 -4 -6 -7 1)) (member rsi2 '(1 3 4))
                     (member ldev3 '(2 0 1))(member pivm '(5 -5 -2)))
               (or (member pivm '(5 -2 -5))(member rsii2 '(-4 -1 -3 1 3))(member ccil5 '(-2 1)))
               (or (member pivw '(-5 -1 2 -2))(member pivd '(1 -2 2 -1))(member ccil5 '(-4 -3 -2 2 1 -1)))
                 
             ;    (member epsignal '(OK UP))
                ; (minusp fstd21-5)
                ; (/= wp -1)
               ;  (not (member wpp2 '(IUH OUL IUL UUU7 ODH UDD UDU DDU IDH)));(/= fsts5-3 1)                 
               
              
                 );;first entry long

          (setq long ;(if (and (plusp mvhl));(plusp dirpsar)) 
                      ;   (getd date 'open)
                         (max (getd date 'open) entry-long)
                 trade-long (list date 'long long)
                 cover-long nil;stop-loss-psar;lregh3         
                 stop-long entry-short days-in-trade 1
                 short-stopped-out nil     
                 prev-stop-long stop-long    
                 )
 ;      (format t "~%8date = ~A  long = ~A epsignal = ~A longs = ~A long-gains = ~A"
  ;         date long epsignal longs long-gains) 

           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                         (* (calculate-point-value date) ; (my-pretty-price; (- (getd date 'close) long)))))
                           (my-pretty-price (- (getd date 'close) long (comm+slip-points date)))))
             ))

 ;;;check if stopped out on same day of entry
    (when (and long stop-long 
             ;  (or (and (<= (getd date 'low) stop-long)
               ;         (>= (getd date 'open) (getd date 'close)))
                  (<= (getd date 'low) stop-long)
              ;    (<= (getd date 'close) stop-long))
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
           (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date) (my-pretty-price (- stop-long long)))))
           (setf (nth 3 record) (- (nth 3 record)
                  (* (calculate-point-value date)(my-pretty-price (- (getd date 'close) long)))))
 
          (setq trade-long nil long nil stop-long nil prev-stop-long nil days-in-trade 0))



    (when (and short stop-short 
              ; (or (and (>= (getd date 'high) stop-short)
               ;         (<= (getd date 'open) (getd date 'close)))
                   (>= (getd date 'high) stop-short)
                ;    (>= (getd date 'close) stop-short))
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
           (setf (nth 3 record) (+ (nth 3 record) 
                (* (calculate-point-value date)(my-pretty-price (- short stop-short)))))
           (setf (nth 3 record) (- (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price (- short (getd date 'close))))))
 
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
   (when  (and long cover-long (> (getd date 'high) cover-long))
          (push (- (* (-  (max (getd date 'open) cover-long)  long)
                       (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                       (- (max (getd date 'open) cover-long) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (max cover-long (getd date 'open)) (getd date-1 'close))))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))

          (setq trade-long nil long nil stop-long nil ))

   (when  (and short cover-short (< (getd date 'low) cover-short))
          (push (- (* (- short (min (getd date 'open) cover-short))
                   (calculate-point-value date-1))(comm+slip date-1)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                  (- short (min (getd date 'open) cover-short))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (getd date-1 'close) (min cover-short (getd date 'open)))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade-short nil short nil stop-short nil ))
|#
  ;     (format T "~%5 ~A = date  ~A = short  ~A = long ~A" date short long trade-long)
  ;     (print record)
           (setf (nth 4 record) (+ (nth 3 record) running-sum))
           (setq running-sum (nth 4 record))

       (push record trading-dates)
       (setq record nil long-stopped-out nil short-stopped-out nil);;;reset to nil at end of each trading date

       
     ) ;;;closes the dotimes

;;;;writes out the diary
   (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (first ith) (second ith) (third ith) (round (nth 3 ith))
           (round (nth 4 ith)))
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
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F    COMMISSION=   ~A~%~
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
      *commission*
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
|#
;;;; trend system





(defun swingtrade-simulation-test4 (market date2 num)
 (let (date stop-long stop-short trades long short (features *swing-features*)
       trade-long entry-long entry-short prev-stop-long prev-stop-short  ;csignal
         dirp  rsi2xl rsi2xh cci5h3 cci5l3 roc5h roc5l rsi5h rsi5l rsi5 cci5 bpl11
        (days-in-trade 0) macdd cci21h11 cci21l11 can wp pt rsi5l-bound rsi5h-bound
       ave-win ave-loss losers winners extended-trades trade-short  cci21 cci21d2 cci21r11 ccis5
        risk  trading-dates (trade-time 0) prev-dir (time-in-signal 0)  
      ; epsignal longs long-gains long-acc shorts short-gains short-acc bin 
        date-1 record (running-sum 0)  draw  early-exit late-entry 
       (singles 0)  cover-long cover-short  dir sdate stop-loss ;(stockp (member market *stocks-list*))
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "swing3-summary3.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "swing3-simulation3.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "swing3-diary3.dat")))
       
    ; (declare (ignore min-days))
   (set-market market)(format T "~%~A~%" *data-name*)
 
    (if (and num (> num (available-days market date2 400)))  (setq num nil))
    (ifn num (setq num (available-days market date2 400)))
   
;   (setq singles (nth-value 3 (apply #'swing-trade-binsb nil features))) 


  (cond ((member market *forex-warehouse-list*)(setq *commission* 0  *pips-slippage* 6 
          rsi5l-bound 38 rsi5h-bound 62))
        ((member market *dubai-list*)(setq *commission* 10  *pips-slippage* 0 
          rsi5l-bound 30 rsi5h-bound 70))
        ((member market (union *sp100-list* *stocks-list*)) (setq *commission* 20 *pips-slippage* 0 
          rsi5l-bound 30 rsi5h-bound 70))
        (t (setq *commission* 25  *pips-slippage* 0 
          rsi5l-bound 30 rsi5h-bound 70)))
  
    (setq date (add-mkt-days date2 (- num)))

    
 ;;;;from date1 to date2
   (dotimes (ith num)

   ;(format T "~%1 ~A = date  ~A = short  ~A = long~%" date short long)
    (if (or long short) (incf days-in-trade))

   ; (multiple-value-setq (forecast slope)(lregress date period 'pivot))
    (multiple-value-setq (dir sdate stop-loss)(parabolic-stops date))
    (setq time-in-signal (1+ (sub-mkt-dates sdate date)))
     (multiple-value-setq (dirp rsi2xl rsi2xh) (rsi2x-direction date 3))
     (multiple-value-setq (cci5h3 cci5l3) (cci-high-low date 5 3))
    (multiple-value-setq (cci21h11 cci21l11) (cci-high-low date 21 11))  
     (multiple-value-setq (roc5h roc5l) (ep-roc-high-low date 11 3))
 
    (multiple-value-setq (rsi5h rsi5l)(rsi-high-low date 5 3))

    (setq  can (candle-composite date 2) wp (wpp1-composite date 2) pt (/ (price-turns date) 2.0)
          bpl11 (bpl-index date 5) 
          ccis5 (cci-signal date 5 3)  
          cci21 (commodity-channel-index date 21)
          macdd (macd-direction date 10 21 5 2) rsi5 (rsi date 5)
          risk (dollar-risk date 3000) cci21r11 (- cci21h11 cci21l11)
          cci21d2 (cci-direction date 21 2) cci5 (commodity-channel-index date 5)) 

    (multiple-value-setq (entry-long cover-long)(vprices4 date 3 .85 3))
    (setq entry-short cover-long cover-short entry-long) 
    (if (neql prev-dir dir)(setq early-exit nil late-entry nil))
     (setq prev-dir dir)
   ;  (multiple-value-setq (kc-lower kc-higher)(vprices5 date 4 kc-bound 4));;1.618
   ;(multiple-value-setq (tbps-dir tbps-target tbps-stop)(tbps-signal date ))
   ;(setq reflect (reflect3 date 5))
  
 
             (setq stop-long   stop-loss; kc-lower)
                            ; cover-long nil;(+ (n-day-high date (1- period))(comm+slip-points date))
                               
                              ); (+ (getd date 'close)(- (getd date 'high) cover-short)))
             (setq stop-short  stop-loss; kc-higher)
                            ; cover-short nil;(- (n-day-low date (1- period)) (comm+slip-points date))
                                     
                              );(- (getd date 'close)(- cover-long (getd date 'low)))))
   
    
      (setq date-1 date date (add-mkt-days date 1))
;      (setq date-2 (getd date-1 'ydate))

   ; (setq entry-short stop-loss
    ;     entry-long stop-loss );kc-higher)
   
 ;    (setq stop-long  (fmax prev-stop-long stop-long); (n-day-low date (- period 1)))
 ;         stop-short (fmin prev-stop-short stop-short)); (n-day-high date (- period 1))))
 ;    (setq prev-stop-long stop-long prev-stop-short stop-short)

;   (setq csignal (find-best-counter-swing-trade-test date))

       (setq record (list date (getd date 'close) 0 0 0))
       (if long (setf (nth 2 record) 1))
       (if short (setf (nth 2 record) -1))
 
    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover));(comm+slip-points date))
                  ; cover-long (+ cover-long (getd date 'rollover))
        	   stop-long (+ stop-long (getd date 'rollover))
                    )
             (setf (nth 3 record)(* (calculate-point-value date)(- (nth 3 record) (getd date 'rollover))))
        )

    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover)); (- (comm+slip-points date)))
                ;  cover-short (+ cover-short (getd date 'rollover))
    	          stop-short (+ stop-short (getd date 'rollover))
                   )
            (setf (nth 3 record)(* (calculate-point-value date) (+ (nth 3 record) (getd date 'rollover))))
         )

;    (format T "~%2 date =~A  short= ~A  long= ~A stop-long = ~A stop-short= ~A ~%" 
;               date short long stop-long stop-short)

;;;calculate bin-classifier 
   
 ;     (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
  ;                (bin-classifier-swingtradesb date-1 features))

;;;check if stopped out of prior position #2 (preparing to reverse position)
#|
   (when (and long (<= (getd date 'low) entry-short) topp (eql epsignal 'DOWN)
             )
 ;          (format T "~%6 date = ~A long = ~A low = ~A stop-long = ~A trade-long = ~A"
 ;          date long (getd date 'low) stop-long trade-long)
         (push (round (- (* (my-pretty-price (- (min entry-short (getd date 'open)) long))
                        (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
             (append trade-long  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                      (contract-month *data-name* date) 'S
                                      (my-pretty-price  (min entry-short (getd date 'open)))
                                    (my-pretty-price (- (min entry-short (getd date 'open)) long))))))

           (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                 (my-pretty-price (- (min stop-long (getd date 'open)) (getd date-1 'close)))))


          (setq trade-long nil long nil stop-long nil days-in-trade 0))

   (when (and short (>= (getd date 'high) entry-long) bottomp (eql epsignal 'UP)
            )
 ;         (format T "~%6 date = ~A short = ~A high = ~A stop-short = ~A trade-short = ~A"
 ;             date short (getd date 'high) stop-short trade-short)
          (push (round (- (* (my-pretty-price (- short (max entry-long (getd date 'open))))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short (apply #'vector
            (append trade-short
              (list date (cdr (assoc *data-name* *ninja-symbol*))
                    (contract-month *data-name* date) 'S
                   (my-pretty-price (max entry-long (getd date 'open)))
                   (my-pretty-price (- short (max entry-long (getd date 'open))))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                        (my-pretty-price (- (getd date-1 'close) (max entry-long (getd date 'open))))))
          (setq trade-short nil short nil stop-short nil days-in-trade 0))
|#

;;;check if exited with signal change at the open of the day
        (when (and long 
                   (or (eql dir 'SHORT)
                       ;(and (minusp cci21d3)(< cci21 100))
                      ; (<= (+ can wp (- (abs pt))) -1.5)
                     ;  (= bpl11 3)
                     ;  (and (= ccis5 -1) (> rsi2xh 85)
                      ;      (or 
                       ;         (and (< cci21h11 200) (minusp ccid))))
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
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price (-  (getd date 'open) (getd date-1 'close))))))
          (setq trade-long nil long nil stop-long nil  prev-stop-long nil days-in-trade 0 ))

        (when (and short
                   (or (eql dir 'LONG)
                      ; (and (plusp cci21d3)(> cci21 -100))
                      ;  (= bpl11 -3)
                      ; (and  (= ccis5 1)(< rsi2xl 15)       
                       ;      (or (< cci21r11 100)(> cci21l11 -100)
                        ;         (and (> cci21l11 -200) (plusp ccid))))
                         )
              )
            (push (round (- (* (my-pretty-price (- short (getd date 'open)))(calculate-point-value date))
                      (comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'C
                                           (my-pretty-price  (getd date 'open))
                 (my-pretty-price (- short (getd date 'open)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price (- (getd date-1 'close) (getd date 'open))))))
          (setq trade-short nil short nil stop-short nil  prev-stop-short nil days-in-trade 0 ))
 

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
                 (* (calculate-point-value date)(my-pretty-price (- stop-long (getd date-1 'close))))))


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
                 (* (calculate-point-value date)  (my-pretty-price (- (getd date-1 'close) stop-short)))))
          (setq trade-short nil short nil stop-short nil prev-stop-short nil days-in-trade 0))

#|
;;;check if exited with objective
   (when  (and long cover-long (>  (getd date 'high) cover-long))
          (push (round (- (* (-  (max (getd date 'open) cover-long)  long)
                  (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                       (my-pretty-price (- (max (getd date 'open) cover-long) long))))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)
               (* (calculate-point-value date) (- (max cover-long (getd date 'open)) (getd date-1 'close)))))

          (setq trade-long nil long nil stop-long nil early-exit nil cover-long nil  prev-stop-long nil days-in-trade 0))

   (when  (and short cover-short (<  (getd date 'low) cover-short))
          (push (round (- (* (- short (min (getd date 'open) cover-short))
                      (calculate-point-value date))(comm+slip date))) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                 (my-pretty-price (- short (min (getd date 'open) cover-short)))))))
          (push trade-short extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
          (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(- (getd date-1 'close) (min cover-short (getd date 'open))))))

          (setq trade-short nil short nil stop-short nil early-exit nil cover-short  nil  prev-stop-short nil days-in-trade 0))

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
 
|#
;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (+ (nth 3 record)
                (* (calculate-point-value date)(my-pretty-price
                                       (- (getd date 'close) (getd date-1 'close)))))))
      (if short (setf (nth 3 record)(+ (nth 3 record)
               (* (calculate-point-value date)(my-pretty-price
                                       (- (getd date-1 'close)(getd date 'close)))))))

 ;     (format T "~%3 ~A = date  ~A = entry-short  ~A = entry-long ~A = stop-long "
  ;           date entry-short entry-long stop-long)
    
;;;check if new entry short
   ;    (format t "~%1date = ~A  dir = ~A  macdd = ~A can = ~A wp= ~A rsi2xh= ~A cci5h3= ~A"
   ;        date  dir macdd can wp rsi2xh cci5h3) 
   ;    (format t "~%2date = ~A  chan21= ~A" date  chan21)

       (when  (and (not short)
                   (or ;(and (eql dir 'LONG) (< (getd date 'low) entry-short))
                  ; (not stockp) 
                    (eql dir 'SHORT)
                    )
                 ;  (or (< cci21l11 -150)(> cci21r11 150))
                  (> (getd date 'high) entry-short)
                  (or (minusp cci21d2)(< cci21 -100))
                  ; (minusp (+ can wp))
                   (< rsi2xl 15)(< cci5l3 -100) 
                   ; (>= roc5h 2.75);;;not too steep upwards
                 ;  (or (minusp cci21d2)(minusp cci))
               ;   (>= time-in-signal min-days)
                ;  (> reward 25)
   ;               (member epsignal '(OK DOWN ))
                   );;entry short

          (setq short  (fmax (getd date 'open) entry-short)
                   
                 trade-short (list date 'short short)
                 ; cover-short 
                 ; stop-short (n-day-high date-1 11)
                 prev-stop-short stop-short
                 days-in-trade 0)
    ;   (format t "~%7date = ~A  short = ~A"
     ;      date  short ) 

          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                  (* (calculate-point-value date)
                     (my-pretty-price (- short (getd date 'close)(comm+slip-points date))))))
              )
;;new entry long 
 
    (when  
            (and (not long)
                 (or ;(and (eql dir 'SHORT) (> (getd date 'high) entry-long)) 
                      (eql dir 'LONG)
                      )
                   ;  (or (> cci21h11 150) (> cci21r11 150))
                 
                  (or (plusp cci21d2) (> cci21 100))
                  (< (getd date 'low) entry-long)
                  (> rsi2xh 85)(> cci5h3 100)
                 ;(<= roc5l -2.75);;;not too steep downwards
              ;   (>= time-in-signal min-days)           
               ;  (> reward 25)
    ;            (member epsignal '(OK UP ))
                 );;first entry long

          (setq long  (fmin (getd date 'open) entry-long) 
                    
                 trade-long (list date 'long long)
                 ;cover-long nil      
                 ;stop-long (n-day-low date-1 11)
                 prev-stop-long stop-long    
                 days-in-trade 0)
  ;     (format t "~%8date = ~A  long = ~A  dir= ~A rsi2xh= ~A cci5h3= ~A range= ~A"
   ;        date long dir rsi2xh cci5h3 (- cci21h11 cci21l11)) 

           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (* (calculate-point-value date)
                     (my-pretty-price (- (getd date 'close) long (comm+slip-points date))))))
             )

;;;2nd entry long based on time
#|    (when  
            (and (not long) 
                 (eql dirp 'UP)(eql dirp-1 'DOWN)
               (< rdel 5)(/= dev 4)(/= can -1)(neql wppp 'ODF)
               (not topp)
                (member epsignal '(UP ))
                (>= (/ long-gains longs) 12)
                 );;first entry long

          (setq long  (getd date 'open)  
                 trade-long (list date 'long long)
                 stop-long (fmax (- long risk)(n-day-low date-1 (1- period)))          
                 days-in-trade 0)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                                 (my-pretty-price (- (getd date 'close) long (comm+slip-points date)))));;
             )

|#

  ;   (format T "~%4 ~A = date  ~A = short  ~A = long" date entry-short entry-long)
;     (format T "~%4a ~A = epsignal  ~A = long  ~A = short stop-long = ~A" epsignal long short stop-long)
   ;  (print record)
#| ;;;check if stopped out on same day of entry
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
   (when  (and long cover-long (> (getd date 'high) cover-long))
          (push (- (* (- (max (getd date 'open) cover-long) long)
                      (calculate-point-value date-1)) (comm+slip date-1))  trades)
          (setq trade-long (apply #'vector
               (append trade-long (list date (cdr (assoc *data-name* *ninja-symbol*))
                                        (contract-month *data-name* date) 'O
                                        (my-pretty-price (max (getd date 'open) cover-long))
                        (- (max (getd date 'open) cover-long) long)))))
          (push trade-long extended-trades)
          (setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
          (setf (nth 3 record) (+ (nth 3 record)(- (min stop-long (getd date 'open)) (getd date-1 'close))))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))

          (setq trade-long nil long nil stop-long nil ))

   (when  (and short cover-short (< (getd date 'low) cover-short))
          (push (- (* (- short (min (getd date 'open) cover-short))
                     (calculate-point-value date))(comm+slip date)) trades)
          (setq trade-short  (apply #'vector
                (append trade-short  (list date (cdr (assoc *data-name* *ninja-symbol*))
                                           (contract-month *data-name* date) 'O
                                           (my-pretty-price  (min (getd date 'open) cover-short))
                                            (- short (min (getd date 'open) cover-short))))))
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
;;;date close position gain/loss running-sum
;;;diary is off because the running sum is multiplied by today's point value
     (format stream "~A\,~F\,~D\,~D,~D~%"
           (first ith) (second ith) (third ith) (round (nth 3 ith))
           (round (nth 4 ith)))
     ));;closes with-open-file
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
        PROFIT FACTOR=   ~,2,0,'*,' F  AVE DAYS IN TRADE= ~F    COMMISSION=   ~A~%~
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
      *commission*
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
         (svref ith 7)(my-pretty-price (svref ith 8))
         (round (* (calculate-point-value (svref ith 3)) (my-pretty-price (svref ith 8)))))

     ));;closes the dolist and with-open-file
  ; ); closes the when

    (values (round  (list-sum trades))
            (length trades) trades)
   ));;;closes the let and the defun

(defun swing-trade-status (date)
  (let (vplety vphety (counter 0) (ydate (getd date 'ydate)) (tdate date) low-second high-second)
   (loop
     (multiple-value-setq (vplety vphety)(vprices ydate *duration* *factor* 1 *type*))
     (if (> (getd tdate 'open)(getd tdate 'close)) (setq low-second t high-second nil)
         (setq low-second nil high-second t))
     (cond ((and high-second (>= (getd tdate 'high) vphety))(incf counter) (return counter))
           ((and low-second (<= (getd tdate 'low) vplety))(incf counter) (return (- counter))))
     (setq tdate ydate ydate (getd tdate 'ydate)))
    
))


(defun swing-hedge-test (entry-date exit-date &optional entry-price strike-price (cprice 0)(pprice 0)(output t))
  (let ((date (getd entry-date 'ydate))(pl 0) vsig ctr vprev status reverse vlow vhigh risk payoff rlong rshort)
    
    (ifn entry-price (setq entry-price (getd entry-date 'open)))
    (ifn strike-price (setq strike-price (getd entry-date 'open)))
     (multiple-value-setq (vsig ctr vprev)(vsignals1 date ))
     (multiple-value-setq (vlow vhigh risk)(vprices date))
     (cond ((and (eql vsig 'short)(>= entry-price vprev))
	    (setq vsig 'LONG vprev (getd date 'close)))
	   ((and (eql vsig 'long)(<= entry-price vprev))
	    (setq vsig 'SHORT vprev (getd date 'close))))
     (if (eql vsig 'LONG) (setq status 'Long) (setq status 'Short))
     (format output "OPEN ~A  ~A  ~A  ~A ~A~%" entry-date status entry-price  vprev pl)
 ;;;entry at the open
     (if (zerop cprice)(setq cprice (* .03 entry-price)))
     (if (zerop pprice) (setq pprice (* .03 entry-price)))
     
     (loop
           (setq date (getd date 'ndate))
    ;;;1st test if long is reversed
      (multiple-value-setq (rlong rshort)(ratchet-stops date))
   
      (when (and (eql status 'long)(eql vsig 'short))
	(setq pl (+ pl (- (getd date 'open) (+ entry-price (or (getd date 'rollover) 0))))
	      status 'short reverse nil entry-price (getd date 'open))
	(format output "~A 0 ~A ~A ~A ~A~%" date status (getd date 'open) vprev (my-pretty-price pl)))

      (when (and (eql status 'short)(eql vsig 'long))
	(setq pl (+ pl (-  entry-price (+ (or (getd date 'rollover) 0)(getd date 'open)))) 
	      status 'long reverse nil entry-price (getd date 'open))
		(format output "~A 0 ~A ~A ~A ~A%" date status (getd date 'open) vprev (my-pretty-price pl)))

      (if (and (eql status 'long) (eql (which-first date) 'high-first)
	       (>= (getd date 'high) rlong))
          (setq vprev rshort));(getd (getd date 'ydate) 'close)))
      
      (if (and (eql status 'short)(eql (which-first date) 'low-first)
	       (<= (getd date 'low) rshort));vlow))
	  (setq vprev rlong));(getd (getd date 'ydate) 'close)))
     ; (format output "~A ~A ~A after adjust~%" date status vprev)


 ;;;1st test if long
           (when (and (eql status 'long)(<= (getd date 'low) vprev)(not reverse))
	     (setq pl (+ pl (- (min (getd date 'open) vprev) (+ entry-price (or (getd date 'rollover) 0))))
		   status 'short
		   reverse T  entry-price vprev)
	     (format output "~A 1  ~A  ~A  ~A  ~A  ~A~%"
		     date status (min (getd date 'open) vprev) vsig (+ vprev risk) pl)
    	            (setq vprev (+ vprev risk)) )
      ;;if reversed to short
      ;if high comes first do not do this
      (when (and (eql status 'short)(eql (which-first date) 'low-first)
		 (or (and (>= (getd date 'high) vprev)
                          (< (getd (getd date 'ydate) 'close) (getd date 'close))
                                   )
		      (>= (getd date 'close) vprev))
		 reverse)
	       (setq pl (+ pl (- entry-price vprev)) status 'long  reverse T entry-price vprev
		     )
	       (format output "~A 2 ~A  ~A  ~A  ~A ~A~%" date status vprev vsig entry-price pl)
		     )

     ;;;1st test if short
            (when (and (eql status 'short)(>= (getd date 'high) vprev) (not reverse))
	      (setq pl (+ pl (- (+ entry-price (or (getd date 'rollover) 0))(max (getd date 'open) vprev)))
		    status 'long  reverse T
		     entry-price vprev)
	       (format output "~A 1  ~A  ~A  ~A  ~A  ~A~%"
		       date status (max (getd date 'open) vprev) vsig (- vprev risk) pl)
	       (setq vprev (- vprev risk)))

      (when (and (eql vsig 'short)(eql status 'long)(> (getd (getd date 'ydate) 'close) (getd date 'close))
		  (not reverse))
	      (setq pl (+ pl (- (+ entry-price (or (getd date 'rollover) 0))(max (getd date 'open) vprev)))
		    status 'short  reverse T
		     entry-price (getd date 'open))
	       (format output "~A 1  ~A  ~A  ~A  ~A  ~A~%"
		       date status (max (getd date 'open) vprev) vsig (- vprev risk) pl)
		    (setq vprev (- vprev risk)))

      ;;;;if low comes first do not do this
      (when (and (eql status 'long)
		 (eql (which-first date) 'high-first)
		 (or (and (<= (getd date 'low) vprev)
                         (> (getd (getd date 'ydate) 'close) (getd date 'close))
                                   )
                     (<= (getd date 'close) vprev))
		 reverse)
	       (setq pl (+ pl (- vprev entry-price)) status 'short  reverse T entry-price vprev
		     )
	       (format output "~A 2  ~A  ~A  ~A  ~A ~A~%" date status vprev vsig entry-price pl)
    	             )
      
               
      (multiple-value-setq (vsig ctr vprev)(vsignals1 date ))
    
     ; (format output "~A 3 ~A ~A ~A~%" date vsig status entry-price)
      (multiple-value-setq (vlow vhigh risk)(vprices date))
     ; (format output "~A I ~A  ~A  ~A  ~A~%" date vsig ctr vprev pl)
      (setq reverse nil)
      (if (>= date exit-date)(return))
        )
     ;(format t "~%~A ~A~%" date entry-price)
          
     (if (eql vsig 'LONG)(setq pl (+ pl (- (getd date 'close) entry-price)))
	 (setq pl (+ pl (- entry-price (getd date 'close)))))
      (setq payoff (abs (- strike-price (getd exit-date 'close) (or (getd exit-date 'rollover) 0))))
    (format t "CLOSE ~A  ~A  ~A  ~A~%" exit-date vsig (getd date 'close) pl) 
    (values (round (* (calculate-point-value exit-date) pl));;;hedge p&L
	    (round (+ (* -1 (calculate-point-value exit-date) payoff) cprice pprice)); straddle p&l
	    (Round (+ (* (calculate-point-value exit-date) (- pl payoff)) cprice pprice)));;campaign p&l
     ))

;;;;This is the version Thomas requested; uses 3 rung ladder
(defun swing-hedge-test1 (entry-date exit-date &optional entry-price strike-price (cprice 0)(pprice 0)(output t))
  (let ((date (getd entry-date 'ydate))(pl 0) vsig ctr vprev status reverse  payoff ladder-list rshort rlong)
    
    (ifn entry-price (setq entry-price (getd entry-date 'open)))
    (ifn strike-price (setq strike-price (getd entry-date 'open)))
     (multiple-value-setq (vsig ctr vprev)(vsignals1 date ))
    ; (multiple-value-setq (vlow vhigh risk)(vprices date))
     (setq ladder-list (toms-ladder entry-date strike-price))
     
     (if (eql vsig 'LONG) (setq status 'Long rshort (first ladder-list) rlong (third ladder-list))
	 (setq status 'Short rlong (third ladder-list) rshort (first ladder-list)))
     
     (format output "OPEN ~A  ~A  ~A  ~A~%" entry-date status entry-price  ladder-list)
 ;;;entry at the open
     (if (zerop cprice)(setq cprice (* .03 entry-price)))
     (if (zerop pprice) (setq pprice (* .03 entry-price)))
     
     (loop
      (setq date (getd date 'ndate))
      (setq reverse nil)
    ;;;1st test if long is reversed
#|
      (if (and (eql status 'long) (eql (which-first date) 'high-first)
	       (>= (getd date 'high) (+ strike-price risk)))
          (setq vprev (- strike-price risk)));(getd (getd date 'ydate) 'close)))
      
      (if (and (eql status 'short)(eql (which-first date) 'low-first)
	       (<= (getd date 'low) (- strike-price risk)))
	  (setq vprev strike-price));(getd (getd date 'ydate) 'close)))
  |#    
   ;   (format output "~A ~A ~A after adjust~%" date status vprev)
   ;;;1st test if long
      (when (and (eql status 'long)(<= (getd date 'low) rshort)(not reverse))
	;(eql (which-first date) 'high-first)
	     (setq pl (+ pl (- (min (getd date 'open) rshort) (+ entry-price (or (getd date 'rollover) 0))))
		   status 'short 
		   reverse T  entry-price rshort )
	     (format output "~A 1  ~A  ~A  ~A   ~A~%"
		     date status (min (getd date 'open) (first ladder-list)) (second ladder-list) pl)
    	            );(+ vprev risk)) )
      ;;if reversed to short
      ;if high comes first do not do this
      (when (and (eql status 'short)
		 (eql (which-first date) 'low-first)
		 (or (and (>= (getd date 'high) rlong)
                          (< (getd (getd date 'ydate) 'close) (getd date 'close))
                                   )
		      (>= (getd date 'close) rlong))
  		 reverse)
              (setq pl (+ pl (- entry-price rlong)) status 'long  reverse T
	            entry-price rlong 
		     )
	       
	       (format output "~A 2 ~A   ~A~%" date status   pl)
		     )

     ;;;1st test if short
      (when (and (eql status 'short)(>= (getd date 'high) rlong) (not reverse)
		 )
	      (setq pl (+ pl (- (+ entry-price (or (getd date 'rollover) 0))(max (getd date 'open) rlong)))
		    status 'long  reverse T
		     entry-price rlong )
	       (format output "~A 1  ~A  ~A  ~A  ~A~%"
		       date status (max (getd date 'open) rlong) (second ladder-list)  pl)
		);(- vprev risk)))

      ;;;;if low comes first do not do this
      (when (and (eql status 'long)
		 (eql (which-first date) 'high-first)
		 (or (and (<= (getd date 'low) rshort)
                         (> (getd (getd date 'ydate) 'close) (getd date 'close))
                                   )
                     (<= (getd date 'close) rshort))
		 reverse)
	(setq pl (+ pl (- rshort entry-price)) status 'short  reverse T entry-price rshort)
	       (format output "~A 2  ~A  ~A~%" date status  pl)
    	             )
      
      (setq reverse nil)
            (if (>= date exit-date)(return)))
          
     (if (eql status 'LONG)(setq pl (+ pl (- (getd date 'close) entry-price)))
	 (setq pl (+ pl (- entry-price (getd date 'close)))))
      (setq payoff (abs (- strike-price (getd exit-date 'close) (or (getd exit-date 'rollover) 0))))
    (format t "CLOSE ~A  ~A  ~A~%" exit-date  (getd date 'close) pl) 
    (values (round (* (calculate-point-value exit-date) pl));;;hedge p&L
	    (round (+ (* -1 (calculate-point-value exit-date) payoff) cprice pprice)); straddle p&l
	    (Round (+ (* (calculate-point-value exit-date) (- pl payoff)) cprice pprice)));;campaign p&l
     ))


(defun straddle-ladder (date)
  (let (vsig ctr vprev vlow vhigh risk ladder-list)
  (multiple-value-setq (vsig ctr vprev) (vsignals1 date))
  (multiple-value-setq (vlow vhigh risk)(vprices date))

  (setq ladder-list
	(list (- vprev (* 5 risk))(- vprev (* 4 risk))(- vprev (* 3 risk))(- vprev (* 2 risk))(- vprev risk)
               vprev
	(+ vprev (* 1 risk)) (+ vprev (* 2 risk)) (+ vprev (* 3 risk))(+ vprev (* 4 risk)) (+ vprev (* 5 risk))
          ))
 ; (if (member *data-name* '(us.d1b ty.d1b))
  ;    (setq ladder-list (mapcar #'(lambda (s) (convert-to-32nds s)) ladder-list)))

 (mapcar #'(lambda (s) (my-pretty-price s)) ladder-list)
  ))
(defun toms-ladder (date &optional (strike))
 (let (vlow vhigh risk ladder-list)
    (ifn strike (setq strike (getd date 'open)))
   (multiple-value-setq (vlow vhigh risk)(vprices date))

  (setq ladder-list
	(list (- strike risk) strike (+ strike (* 1 risk)))
          )
 ; (if (member *data-name* '(us.d1b ty.d1b))
  ;    (setq ladder-list (mapcar #'(lambda (s) (convert-to-32nds s)) ladder-list)))

 (mapcar #'(lambda (s) (my-pretty-price s)) ladder-list)
    ))


(defun toms-ratchet-stops (date)
   (let* ((date-1 (getd date 'ydate))(ladder-list (toms-ladder date-1))(phigh (getd date 'high))
	 (plow (getd date 'low)))
  
    (values (second (remove-if #'(lambda (s) (> plow s)) ladder-list));;reverse to long
	    (second (reverse (remove-if #'(lambda (s) (< phigh s)) ladder-list))));;;reverse to short
    ))


(defun run-hedge-report (date)
  (reset-all-indexes aa)(data-dump date)
  (straddle-report date *micro-list*))



(defun AIM-ladder (date)
  (let (vsig ctr vprev vlow vhigh risk ladder-list)
  (multiple-value-setq (vsig ctr vprev) (vsignals1 date))
  (multiple-value-setq (vlow vhigh risk)(vprices date))

  (setq ladder-list
	(list (- vprev (* 5 risk))(- vprev (* 4 risk))(- vprev (* 3 risk))(- vprev (* 2 risk))(- vprev risk)
               vprev
	(+ vprev (* 1 risk)) (+ vprev (* 2 risk)) (+ vprev (* 3 risk))(+ vprev (* 4 risk)) (+ vprev (* 5 risk))
          ))
 ; (if (member *data-name* '(us.d1b ty.d1b))
  ;    (setq ladder-list (mapcar #'(lambda (s) (convert-to-32nds s)) ladder-list)))

 (mapcar #'(lambda (s) (my-pretty-price s)) ladder-list)
  ))

(defun ratchet-stops (date)
  (let* ((date-1 (getd date 'ydate))(ladder-list (AIM-ladder date-1))(phigh (getd date 'high))
	 (plow (getd date 'low)))
  
    (values (second (remove-if #'(lambda (s) (> plow s)) ladder-list));;reverse to long
	    (second (reverse (remove-if #'(lambda (s) (< phigh s)) ladder-list))));;;reverse to short
    ))

(defun run-aim-report (date)
  (reset-all-indexes aa)(data-dump date *aim-list*)
  (AIM-report date *aim-list*))

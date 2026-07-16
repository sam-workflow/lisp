;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;;This is for the MINI day trading system
;;;;This version does not use objectives.
;;;It does use a vector database instead of lists
;;;
;;; This is the small day trading system that uses only 6 very liquid markets
;;; entry is at .667 of ATR.
;;;;

;;;
;;;;For DAY TRADING
;;;

;;;;the file has one record per trade each record is a vector. The indicator values are for the previous date before the entry date.
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
(defun update-daytrade-warehouse (date  &optional (markets *mini-list*))
  
   (mapcar #'(lambda (s1) (populate-day-trades s1 date (cdr (assoc s1 *market-days-available*)))) markets)

;    (portfolio-simulation4 '(day4) date 3525 (list markets))
)

(defun populate-day-trades (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short entry-long entry-short date-1
       risk
        ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "daytrades7.dat")))

   (maind-x)(set-cat-list)
   (set-market market)

  (format outfile "~%~A~%" market)
  (setq *entry-factor* .667 *stop-loss-day* .800)  ;;;1.0557)
;  (setq *entry-factor* .764 *stop-loss-day* .944) 
;  (setq *entry-factor* .500 *stop-loss-day* .9440) 

  (unless date2
       (setq date2 (car (last (month-days (get-latest-index-date))))))

   (setq date (add-mkt-days date2 (- num)))

   (setq date-1 (getd date 'ydate))


 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
   (multiple-value-setq (stop-long stop-short)(vprices date 4 *stop-loss-day* 1))

   (setq risk (* .5 (- stop-short stop-long)))

   (setq   date-1 date date (add-mkt-days date 1))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
    ))
   (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))
    ))

   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))


;;;check if new long entry
;;;enter on a stop order if opens below entry-stop.
;;;enter on a limit order of open above the entry-stop
;;;exit at the close

   (when (and (<= (getd date 'low) entry-short)
              (> (getd date 'high) entry-short)
              (>= (* risk (index-point-value)) 100) ;;to avoid bad data
	      (<= (* risk (index-point-value)) *max-day-risk*)
 
              )
          (setq short entry-short
                short-trade (create-daytrade-entry-record-list date-1 -1 short)
                 ))


    (when (and
               (>= (getd date 'high) entry-long)
               (< (getd date 'low) entry-long)
               (>= (* risk (index-point-value)) 100)
	       (<= (* risk (index-point-value)) *max-day-risk*)

               )
           (setq long entry-long
                 long-trade (create-daytrade-entry-record-list date-1 1 long)

                 )) ;(format T "long-trade= ~A ~%" long-trade) )


 ;;;check if stopped out on same day

   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
               )
           (push (- stop-long long) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price stop-long)
                                        (round (* (index-point-value) (- stop-long long)))))))
           (push long-trade extended-trades)
           (setq long-trade nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
                    )
           (push (- short stop-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (index-point-value) (- short stop-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))



;;;check if met exit criteria to exit at end of day of entry for day trade
     (when long
            (setq cover-long (getd date 'close))
            (push (- cover-long long) trades)
            (setq long-trade (apply #'vector (append long-trade (list date (my-pretty-price cover-long)
                                           (round (* (index-point-value) (- cover-long long)))))))
            (push long-trade extended-trades)
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (index-point-value) (- short cover-short)))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))

   );;;closes the dotimes



   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
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

;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
;;;there are 13 indicators besides the direction (total 14)
(defun create-daytrade-entry-record-list (date direction entry)
   (let* ((date-1 (getd date 'ydate)))

       (list *data-name* (getd date 'ndate) direction
             (and entry (my-pretty-price entry))
                              (my-round (/ (volatility date 4 1) (volatility date 63 1)) 3) ;;;feature 2
                              (day-bar-type date) ;feature 3  9 levels
                              (round (slow-stochastic date 21))  ;feature 4
                        ;      (roc-index date 5 63) ;feature 5  10 levels
                              (mo-diver date 5);; feature 5
                              (roc-index date 21 252) ;;;feature 6  10 levels
                    ;         (trend-signal date 45) ;feature 7
                              (gann-slope-index date 5 63);;feature 7  5 levels
                              (pivot-index date) ;feature 8  6 levels
                              (range-index date) ;; feature 9 6 levels
                              (2-bar-index date);;;feature 10  5 levels
                              (day-bar-type date-1) ;feature 11  9 levels
                           ;   (daytrade-reward-risk date direction) ;;feature 12
                              (reflect3 date 3)      ;;  feature 12  5 levels
                            ;  (mo-diver date 45) ;;feature 13
                              (gann-slope-index date 9 63) ;;feature 13 5 levels
                              (pivot-index date-1) ;;features 14 6 levels
                              );;;closes the list

))

(defun encode-day-trades (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
      ;;;;number 1 is the direction and must be included
      (1 (push (svref record 2) bin-list));;;adds the direction

;;;;Feature 2 has 5 levels ;this is the volatility ratio of 4 days to 63 days
      (2 (cond ((< (svref record 4) .70) (push 2 bin-list))
               ((and (>= (svref record 4) .70)(< (svref record 4) .90)) (push 1 bin-list))
               ((and (>= (svref record 4) .90)(< (svref record 4) 1.1)) (push 0 bin-list))
               ((and (>= (svref record 4) 1.10)(< (svref record 4) 1.30)) (push -1 bin-list))
               ((>= (svref record 4) 1.30)(push -2 bin-list))))
 
;;;;Feature 3 with 9 levels bar type
      (3 (push (svref record 5) bin-list))

;;;;Feature 4 with 6 levels is the stochastic with parameter 21
  
      (4 (cond ((<= (svref record 6) 10) (push 3 bin-list))
               ((and (> (svref record 6) 10)(<= (svref record 6) 30)) (push 2 bin-list))
               ((and (> (svref record 6) 30)(<= (svref record 6) 50)) (push 1 bin-list))
               ((and (> (svref record 6) 50)(<= (svref record 6) 70)) (push -1 bin-list))
               ((and (> (svref record 6) 70)(< (svref record 6) 90)) (push -2 bin-list))
               ((>= (svref record 6) 90) (push -3 bin-list))))

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



;;;use same features as voltest2 *day-features1*
(defun daytrade-simulation-test (market date2 num &optional (features nil))
 (let (date trades long short cover-long cover-short entry-long entry-short record trading-dates
        date-1 epsignal longs long-gains shorts short-gains (running-sum 0) risk
       ave-win ave-loss losers winners extended-trades long-trade short-trade stop-long stop-short
       long-acc short-acc  (twr-long 1) (twr-short 1) bin draw ignore minifeatures singles
       (path-in (string-append *config-dir* "day-features7.dat"))
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "day-summary7.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "day-simulation7.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "day-diary7.dat")))

   (declare (ignore long-acc short-acc))
   (set-market market) (format T "~%~A~%" *data-name*)
;;;using .847 gives the same stop loss price as STARTER and SCORE.
;;;Only difference is that the entry is a little earlier
   (setq *entry-factor* .667 *stop-loss-day* .800)  
;   (setq *entry-factor* .764 *stop-loss-day* .944)
;   (setq *entry-factor* .50 *stop-loss-day* .944) 


   (if (and (not features)(probe-file path-in))
      (with-open-file (str path-in :direction :input)
       (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record minifeatures))))     
  
   (ifn features
         (setq features (cdr (assoc market minifeatures))))

   (multiple-value-setq (ignore ignore ignore singles) (apply #'day-trade-bins features))  
    
   (setq date (add-mkt-days date2 (- num)))
   (setq record (vector date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

; (format T "Market= ~A date=~A~%" *data-name* date)

   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))
   (multiple-value-setq (stop-long stop-short)(vprices date 4 *stop-loss-day* 1))

   (setq risk  (* .5 (- stop-short stop-long)))


   (setq  date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
                                      entry-short (+ entry-short (getd date 'rollover)))

    )

    (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

 ;   (format T "~%entry-long= ~A epsignal= ~A long-gains= ~A longs= ~A risk= ~A twr-long= ~A~%" entry-long epsignal long-gains longs risk twr-long)
;;;;calculate bin-classifier only as needed
 (when (or (>= (getd date 'high) entry-long)
           (<= (getd date 'low) entry-short))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades7a date-1 features))
       (setq twr-short (day-bin-twr3 bin))(setf (nth 0 bin) 1)
       (setq twr-long (day-bin-twr3 bin))
       )

;;;check if new entry

   (when (and (> (getd date 'high) entry-short)
              (<= (getd date 'low) entry-short)
              (member epsignal '(OK DOWN))(> shorts 0.0)
              (>= (/ short-gains shorts) 100.0) (> twr-short 1.0)
	          (>= (* risk (index-point-value)) 100)
              (<= (* risk (index-point-value)) *max-day-risk*)
;              (>= (/ short-gains shorts)
 ;                 (if (> longs 0.0) (/ long-gains longs) 0))
            
                 )
          (setq short entry-short
                short-trade (list date 'short short)
                )
           (setf (svref record 2) -1)
           (setf (svref record 3) (- (svref record 3) (/ *day-commission* (index-point-value))))
                 )


    (when (and (< (getd date 'low) entry-long)
               (>= (getd date 'high) entry-long)
               (member epsignal '(OK UP))(> longs 0.0)
               (>= (/ long-gains longs) 100.0) (> twr-long 1.0)
               (>= (* risk (index-point-value)) 100)
	       (<= (* risk (index-point-value)) *max-day-risk*)
  ;             (> (/ long-gains longs)
   ;               (if (> shorts 0.0)(/ short-gains shorts) 0))
           
             )

           (setq long entry-long
                 long-trade (list date 'long long)
                 )
           (setf (svref record 2) 1)
           (setf (svref record 3) (- (svref record 3) (/ *day-commission* (index-point-value))))
                 )


 ;;;check if stopped out on same day

   (when (and long stop-long
              (or (and (<= (getd date 'low) stop-long)
                       (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
           )
           (push (- stop-long long) trades)
           (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*)) (contract-month *data-name* date) 'S
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
                 )
           (push (- short stop-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*)) (contract-month *data-name* date) 'S
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
            (setq long-trade (apply #'vector (append long-trade (list date (cdr (assoc *data-name* *ninja-symbol*)) (contract-month *data-name* date) 'N
                                                                   cover-long (my-pretty-price (- cover-long long))))))
            (push long-trade extended-trades)
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3) (- (getd date 'close) long)))
            (setq long-trade nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (apply #'vector (append short-trade (list date (cdr (assoc *data-name* *ninja-symbol*)) (contract-month *data-name* date) 'N
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
  (setq trades (mapcar #'(lambda (s) (- s (/ *day-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
    (format str "~A traded from ~A to ~A~%" (index-lname) (date-convert (add-mkt-days date2 (- num)))(date-convert date2))
    (format str "FEATURES= ~A  NUM MARKETS IN WAREHOUSE= ~A~%" features (length (num-markets-in-warehouse3 daytrades)))
    (format str "NUM TRADES IN WAREHOUSE= ~A  ENTRY FACTOR= ~5,3,0,'*,' F~%" (length daytrades) *entry-factor*)
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
      (my-round (* 100 (/ (length trades) num)) 1) *day-commission*
      (setq draw (round (* (drawdown trades)(or (index-point-value) 1))))
       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       (/ (count 'S (mapcar #'(lambda(s1) (svref s1 6)) extended-trades)) (max (length extended-trades) 1))
         )

     (format str "~%~%MIN INITIAL $=     ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
 ;    (format T "List-sum trades= ~A~%" (list-sum trades))
 ;    (format T "dates-diff = ~A~%" (subtract-dates (add-mkt-days date2 (- num)) date2))
 ;    (format T "init-acct- ~A~%" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
     (format str "~%AVE ANNUAL RETURN= ~5,1,2,'*,' F%"
           (/ (/ (* (list-sum trades) (or (index-point-value) 1))
                       (/ (subtract-dates (add-mkt-days date2 (- num)) date2)
                          365.25))
                    (* 1000 (ceiling (max 1 (* 3 (abs draw))) 1000))) )


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun day-trade-bins ( &rest features)
  (let (bin path)

 ;     (setq path (string-append *upper-dir-warehouse* "daytradewarehouse4.dat"))


    (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "daytrades7.dat"))

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
     (setq bin (encode-day-trades record features))
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
;;;this function only looks at separability not predictability
(defun daytrades-add-one-in (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)

  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'day-trade-bins (append base-features (list ith))))
    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))



;;;requires a base features list
(defun daytrades-leave-one-out (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'day-trade-bins (remove ith base-features)))
    (setq result (float (/ result (sqrt (length day-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))



;;;;this function expects that you have run (day-trade-bins ...) already
(defun display-daytrade-bins-by-expected-value4 ()
  (let (contents expected-value-list result (path1 (string-append *output-upper-dir* "daytrade-expected-value4.dat")))
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

;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-daytrades7a (date features)
  (let (record bin (result 0) (counter 0) contents epsignal
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-daytrade-entry-record-list date 1 nil))
    (setq record (apply #'vector (append record '(nil nil nil))))

    (setq bin (encode-day-trades record features))
    (setq contents (gethash bin *day-trade-warehouse3*)) ;;;look for bin with long trades first
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *day-trade-warehouse3*) contents))
;;;here we remove any trades in the same market with the same next day
   (setq contents
    (remove-if #'(lambda(s1) (and (eql *data-name* (svref s1 0))
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


;;;calculates the twr for a bin
;;;must run #'day-trade-bins3 before running this function
(defun day-bin-twr3 (bin)
  (let ((twr 1))

   (dolist (record (gethash bin *day-trade-warehouse3*))
     (setq twr (* twr (+ 1 (/ (if (plusp (svref record 2))
                                 (- (svref record 18) (svref record 3))
                               (- (svref record 3) (svref record 18)))
                             (svref record 3)))
                           )))
  twr
))



(defun find-best-indicator-set ( &optional (markets *mini-list*) (period 3500))
  (let (base-list candidate-list winners-list (result 0) tdate best-features
         (pathout (string-append *config-dir* "day-features7.dat")))
    (if (probe-file pathout)
       (with-open-file (str pathout :direction :input)
            (do ((record (read str nil 'eof) (read str nil 'eof)))
                ((eql record 'eof))
                 (push record best-features))))
    (if (probe-file pathout)(delete-file pathout))
    (dolist (market markets)
    (set-market  market) (setq result 0)
    (if (and period (> period (cdr (assoc *data-name* *market-days-available*))))
        (setq period nil))
    (ifn period (setq period (cdr (assoc *data-name* *market-days-available*))))
    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (daytrades-add-one-in base-list candidate-list))
     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;fraction of single trade bins
     (if (or (not candidate-list)(> result .90))
         (return))
    )
    (format T "First stage winners-list= ~A" winners-list)

    (setq tdate (car (last (month-days (get-latest-index-date)))))
  
   (loop     
    
      (if (< (fourth (car winners-list)) .60)(return))
      (setq winners-list (daytrades-leave-one-out base-list))    
      (if (neql (cdr (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
     (if (assoc market best-features)
          (setf (cdr (assoc market best-features)) base-list)
         (setq best-features (cons (cons market base-list) best-features)))
     (daytrade-simulation-test  market tdate period base-list)
     (format T "~%Best indicator set = ~A~%" base-list)

    
);;closes the dotimes
     (with-open-file (str pathout :direction :output :if-exists :append :if-does-not-exist :create)
       (dolist (ith best-features)
          (format str "~A ~%" ith)))
   
  (diary-composite3 '(mini) markets)
    ))


;;;this is for the MINI product for OEC firms only
(defun find-best-daytrade7 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk  stop-short stop-long  entry-long entry-short
        trade-direction  cover-short cover-long pacific-entry-time pacific-exit-time
        pacific-cancel-time pacific-end-session-time  
        central-entry-time; central-exit-time
         central-cancel-time central-end-session-time
        action directive1 cannon-mini-path3 
        oec-symbol oco-code  ;(time-zone "CT")
         offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))

    (setq *entry-factor* .667 *stop-loss-day* .850) ;;;  1.0557)
    (setq cannon-mini-path3 (string-append *daily-output-dir* "cannon-mini.csv"))

    (if (and (member epsignal '(OK UP))(> longs 0)(>= (/ long-gains longs) 100.0)(> twr-long 1.0))
         (push 'UP trade-direction)(push 'FT trade-direction))
    (if (and (member epsignal '(OK DOWN))(> shorts 0)(>= (/ short-gains shorts) 100.0)(> twr-short 1.0))
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

         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal)
                             (incf counter) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal)
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

             (when (member *data-name* *mini-list*)

               (setq offset (random-choice -1 0)
                     pacific-exit-time
                     (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
               (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
                                 pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)
               (setq oco-code (format nil "OSO~A" (incf counter)));;need to add 1 to the OCO code if both buy and sell entries
               (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
                                pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

              );;;closes when
       
             )

             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'MINI 'LONG date1 entry-long stop-long cover-long output1)

              (when (member *data-name* *mini-list*)
               (setq offset (random-choice -1 0)
                 pacific-exit-time
                 (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
               (write-oec-long  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-long stop-long
                                pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

               ) ;;closes the when score
                )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'MINI 'SHORT date1 entry-short stop-short cover-short output1)

              (when (member *data-name* *mini-list*)
                 (setq offset (random-choice -1 0)
                       pacific-exit-time
                       (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *cannon-market-times-list*)) offset T)))
                 (write-oec-short  cannon-mini-path3 oec-symbol *cannon-mini-block-acct* *cannon-mini-qty* entry-short stop-short
                                   pacific-entry-time pacific-cancel-time pacific-exit-time pacific-end-session-time oco-code)

                 );;;closes the when score
            
               ));;closes clause the cond
      )) ;;;closes the let and the defun





;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-daytrade-indicators4 ()
   (let ((path (string-append *upper-dir-warehouse* "daytradewarehouse4.dat"))
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
     (setf (svref ith 6)(2day-change-index date-1));;feature 12
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
     (setq bin (encode-day-trades record features))
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





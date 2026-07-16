;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


(defvar swingtrades nil)
(defvar swing-bin-codes nil)


;;This function is used to add trades to the warehouse for all the swing-list
;;;markets
(defun update-swing-warehouse (date &optional (markets *swing-list*))
   (let (unfilter-total)
  (setq unfilter-total 
   (mapcar #'(lambda (s1) (populate-swing-trades s1 date (cdr (assoc s1 *market-days-available*)))) markets)
    )
  (list-sum unfilter-total)
   (find-best-indicator-swing-set markets)
   (build-swingtrade-warehouse markets)
)) 


(defun populate-swing-trades (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long trade-short entry-short entry-long
       ave-win ave-loss losers winners extended-trades date-1 risk prev-signal
       (path1 (string-append *upper-dir-warehouse* (format nil "~S" market) "swingtrades.dat")))
    
    (maind-x)(set-cat-list)(set-market market)
   (setq *entry-factor* 1.382)

   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-short entry-long) (vprices1 date 4 *entry-factor* 3))
;   (multiple-value-setq (entry-short entry-long) (vprices1 date 4 .764 4))
   (multiple-value-setq (prev-signal )(vsignals1 date 4 *entry-factor* 3))
  
   

    (setq  date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))))

  ;;;;adjust stop loss prices for existing trade
;;; 
   (if (or long short)(setq risk (* 1.0 risk)));;;this ratchets and tightens the risk closer to the market each day
   (when long (setq stop-long (max stop-long entry-short (- entry-long risk))))
   (when short (setq stop-short (min stop-short entry-long (+ entry-short risk))))
   

;;;check if stopped out of prior position

   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price (min stop-long (getd date 'open)))
                                        (round (* (calculate-point-value date) (- (min stop-long  (getd date 'open)) long)))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil
           ))

    (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price (max stop-short (getd date 'open)))
                                           (round (* (calculate-point-value date) (- short (max stop-short (getd date 'open)))))))))
          (push trade-short extended-trades)
          (setq trade-short nil short nil stop-short nil
           ))


;;;check if new entry

    (when (and (not short)  (eql prev-signal 'buy)
              (setq risk (volatility date 4 (* 1.618 *entry-factor*)))
               (<= (getd date 'low) entry-short)
               (> (getd date 'high) entry-short)
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
	      (<= (* risk (calculate-point-value date)) *max-swing-risk*)
          
               )
          (setq short (min entry-short (getd date 'open))
                trade-short (create-swingtrade-entry-record-list date-1 -1 short)
                stop-short (min entry-long (+ entry-short risk))
         ))


    (when (and (not long)  (eql prev-signal 'sell)
               (setq risk (volatility date 4 (* 1.618 *entry-factor*)))
               (>= (getd date 'high) entry-long)
               (< (getd date 'low) entry-long)
               (>= (* risk (calculate-point-value date))
                   (* 2 (calculate-point-value date) *pips-slippage* (index-tick-size)))
               (<= (* risk (calculate-point-value date)) *max-swing-risk*)
          
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (create-swingtrade-entry-record-list date-1 1 long)
                 stop-long (max entry-short (- entry-long risk))
              ))

 ;;;check if stopped out on same day of entry
   (when (and long stop-long
               (or (and (<= (getd date 'low) stop-long)
                        (>= (getd date 'open) (getd date 'close)))
                   (<= (getd date 'close) stop-long))
               )
           (push (- stop-long long) trades)
           (setq trade-long (apply #'vector (append trade-long (list date (my-pretty-price stop-long)
                                        (round (* (calculate-point-value date) (- stop-long long)))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))

    (when (and short stop-short
               (or (and (>= (getd date 'high) stop-short)
                        (<= (getd date 'open) (getd date 'close)))
                    (>= (getd date 'close) stop-short))
                    )
           (push (- short stop-short) trades)
           (setq trade-short (apply #'vector (append trade-short (list date (my-pretty-price stop-short)
                                           (round (* (calculate-point-value date) (- short stop-short)))))))
           (push trade-short extended-trades)
           (setq trade-short nil short nil stop-short nil
           ))


   );;;closes the dotimes


  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

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
(defun create-swingtrade-entry-record-list (date direction entry)
   (let ((date-1 (getd date 'ydate)) date-2)
        (setq date-2 (getd date-1 'ydate))

       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
            
             (day-bar-type2 date) ;;;feature 2
             (macd-index date 12 26 9) ; (or (car (member 'DB6 form-codes)) ;feature 3
             (roc-index date 5 84)  ;feature 4
             (slow-stochastic-index date 21) ;feature 5
            
             (roc-index date 10 126) ;;feature 6
             (range-dev date 9) ;feature 7
             (day-bar-type1 date);;;feature 8
          ;   (pivot-index date-1) ;feature 8
             (macd-index date 5 13 3) ;;feature 9 5 levels
             
             (day-bar-type2 date-1);feature 10
             (pivot-index date 1);feature 11
             (trend-signal date 15);;feature 12
                               ; (car (member 'DS6 form-codes)))
             (volatility-ratio-index date 4 63 1); feature 13
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
          ;   (mo-diver date 45) ;feature 14
              (rsi-ave-diff-index date 14 2) ;;;;feature 14          
                  );;;closes the list

))
;;;;here are some candidate indicators
;                              (timing-index date)
;                              (timing-index date-1)(timing-index date-2)
;
;                              (round (rsi-ave-diff date-1 14 2))
;                              (dev-distribution-close date 4 .875 'close)
;                              (momentum-divergence1 date 5 13)
;                              (or (car (formation date)) 'FT))



;;;;;Now we need first to read the swing-trades file(s) and load into the *swing-trade-warehouse* hash table

;(defvar swings nil)
;(defvar bin-codes nil)

(defun swing-trade-binsb ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "swingtradewarehouse.dat"))

  (maind-x)(set-cat-list)
  (setq swingtrades nil swing-bin-codes nil)(clrhash *swing-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swingtrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swingtrades)
     (dotimes (ith 4)
       (setq bin (encode-swing-trades record (butlast features ith)));;;ith = 0 returns the same features
      
        (cond ((gethash bin *swing-trade-warehouse*)
               (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equalp)
               (setf (gethash bin *swing-trade-warehouse*)
                     (cons record (gethash bin *swing-trade-warehouse*)))))
              ((not (gethash bin *swing-trade-warehouse*))
               (setf (gethash bin *swing-trade-warehouse*) (list record))
               (push bin swing-bin-codes)))
        ))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profitb swingtrades features)

  ))



(defun swing-trade-binsa ( &rest features)
  (let (bin path)

   (setq path (string-append *upper-dir-warehouse*  "swingtradewarehouse.dat"))

  (maind-x)(set-cat-list)
  (setq swingtrades nil swing-bin-codes nil)(clrhash *swing-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swingtrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swingtrades)
     (setq bin (encode-swing-trades record features))
    
     (cond ((gethash bin *swing-trade-warehouse*)
            (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse*)
                       (cons record (gethash bin *swing-trade-warehouse*)))))
           ((not (gethash bin *swing-trade-warehouse*))
             (setf (gethash bin *swing-trade-warehouse*) (list record))
             (push bin swing-bin-codes)))
    );;closes the dolist

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profit swingtrades)

  ))


(defun swing-trade-bins ( &rest features)
  (let (bin path)

  (setq path (string-append *upper-dir-warehouse* (format nil "~A" *data-name*) "swingtrades.dat"))

  (maind-x)(set-cat-list)
  (setq swingtrades nil swing-bin-codes nil)(clrhash *swing-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swingtrades)))

;;record is a vector of length 20 that represents a trade and its initial indicator values
;
;;;;now all the trades are in the list daytrades.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record swingtrades)
     (setq bin (encode-swing-trades record features))
 
     (cond ((gethash bin *swing-trade-warehouse*)
            (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equalp)
                 (setf (gethash bin *swing-trade-warehouse*)
                       (cons record (gethash bin *swing-trade-warehouse*)))))
           ((not (gethash bin *swing-trade-warehouse*))
            (setf (gethash bin *swing-trade-warehouse*)(list record))
            (push bin swing-bin-codes)))
)

    (format T  "~%FEATURES = ~A~%" features)
    (rank-swingtrade-bins-by-profit swingtrades)

  ))

;;;This rank is for using multiple length features

(defun rank-swingtrade-bins-by-profitb (swingtrades features)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)(twr 1)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0)
        longest-swing-bin-codes (longest-feature (length features)))
   (setq longest-swing-bin-codes (remove-if #'(lambda (s) (< (length s) longest-feature)) swing-bin-codes))
   (dolist (ith longest-swing-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
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
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over swing-bin-codes

    (dolist (kth swingtrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swingtrades)(length (num-markets-in-warehouse3 swingtrades)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length longest-swing-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swingtrades)(length longest-swing-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length longest-swing-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length swingtrades)) 2))
     (format T "CHI SQUARE PROB = ~7,5F~%" (my-round (swingtrade-chi-squared-gof) 5))
    (values (round winners)(round (+ all-winners all-losers))(if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
           (my-round (/ only-one (length swingtrades)) 2)) 
       

 ))



(defun remove-swing-trade-market1 (market &optional (stocks nil))
  (let (trades path)
  (if stocks (setq path (string-append *upper-dir* "stocksswingwarehouse1.dat"))
     (setq path (string-append *upper-dir* "swingtradewarehouse1.dat")))
  (maind-x)(set-cat-list)

    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))

   (setq trades (remove-if #'(lambda (s) (eql (car s) market)) trades))
   (with-open-file (str path :direction :output :if-exists :supersede)
       (dolist (ith trades)
        (format str "~S~%" ith)))
 ))

;;;writes out the new warehouse with the added trades.
;;;
(defun add-swing-trades1 (new-trades-path)
  (let ((path-out (string-append *upper-dir* "swingtradewarehouse1.dat")) ewaves-trades)

       (if (probe-file path-out)
        (with-open-file (str path-out :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (push record ewaves-trades))
          ))

     (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (pushnew record ewaves-trades :test #'equal)
          ))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith ewaves-trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-swing-indicators ()
   (let ((path (string-append *upper-dir* "swingtradewarehouse1.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq swings nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record swings)))

  (dolist (ith swings)
     (set-market (nth 0 ith))
     (setq date-1 (getd (nth 1 ith) 'ydate))

     ;(setf (nth 5 ith) (trend-signal1 date-1 3));;feature 3
    ; (setf (nth 13 ith) (mo-diver date-1 5));;feature 11

    ; (setf (nth 14 ith)(mo-diver date-1 15)) ;;feature 12
     (setf (nth 15 ith)(my-round (float (if (and (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)
                                                 (plusp (getd (add-mkt-days (getd date-1 'ydate) -21) 'volume)))
                                  (/ (ave (getd date-1 'ydate) 3 'volume)
                                     (ave (getd date-1 'ydate) 21 'volume)) 1)) 3));;feature 13
    ; (setf (nth 16 ith)(my-round (float (if (and (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))
    ;                                     (plusp (car (getd (add-mkt-days (getd date-1 'ydate) -21) 'openint))))
    ;                              (/ (ave1 (getd date-1 'ydate) 3 'openint)
    ;                                 (ave1 (getd date-1 'ydate) 21 'openint)) 1)) 3)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth swings)
      (format str "~S~%" jth)))

));;;closes the defun

;;;Feature number is 2 less than the slot number
;;;item #4 is the stochastic 21
;;;item #5 is yesterday's stochastic 21
;;;item #6 is the TS5
;;;item #7 is the TS15
;;;item #8 is the TS45
;;;item #9 is the TS135
;;;item #10 is the RSD
;;;item #11 is the volatility ratio
;;;item #12 is the bar type
;;;item #13 is the current bar to previous bar
;;;itme #14 is the MACD
;;;item #15 is the candlesticks
;;;item #16 is the pinpoint




(defun rank-bins-by-accuracy1 ()
  (let (contents result accuracy-list)
    (dolist (ith swing-bin-codes)
     (setq contents
          (gethash ith *swing-trade-warehouse1*))
     (setq result 0)
     (dolist (kth contents)
        (if (> (nth 19 kth) 0) (incf result)))
     (setq accuracy-list (acons (/ result (length contents)) ith accuracy-list)));;;closes the dolist over bin-codes
     (vsort accuracy-list #'> 'car)
     (format T "Accuracy  Bin~%")
     (dolist (jth accuracy-list)
       (format T "~4F     ~A ~%"
                   (my-round (car jth) 2)(cdr jth)))
 ))

;;;;this function expects that you have run (swing-trade-bins ...) already
(defun rank-bins-by-expected-value1 ()
  (let (contents expected-value-list result (path1 (string-append *output-upper-dir* "swing-expected-value1.dat")))
   (dolist (ith swing-bin-codes)
     (setq contents
           (gethash ith *swing-trade-warehouse1*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (nth 19 kth))))
     (setq expected-value-list (cons (list (and contents (/ result (length contents))) ith (length contents)) expected-value-list)));;;closes the dolist over bin-codes
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (vsort expected-value-list #'> 'car)
         (format stream "Expected          Bin           NUM~%Value~%")
         (dolist (jth expected-value-list)
             (format stream "~8A      ~A       ~D~%" (round (car jth)) (cadr jth) (third jth)))) ;;;;closes the with-open-file
 ))



(defun rank-swingtrade-bins-by-profit (swingtrades)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)(twr 1.0d0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith swing-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
     (setq result 0.0 counter 0 twr 1.0d0)
;    (print ith)
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (svref kth 19)))
         (if (plusp (svref kth 19)) (incf counter))
         (setq twr (* twr (+ 1.0d0 (/ (if (plusp (svref kth 2))
                                 (- (svref kth 18) (svref kth 3))
                               (- (svref kth 3) (svref kth 18)))
                             (svref kth 3)))))

         ) ;;;closes dolist over contents
      (if (and (plusp result)(> twr 1.0d0)) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))


     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes

    (dolist (kth swingtrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swingtrades)
                         (length (num-markets-in-warehouse3 swingtrades)))
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length swing-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swingtrades)(length swing-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length swing-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length swingtrades)) 2))

    (values (round winners)(round (+ all-winners all-losers))
            (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins)))
           (my-round (/ only-one (length swingtrades)) 2)) 
 ))


(defun find-best-indicator-swing-set (markets &optional (period 3500))
  (let (base-list candidate-list winners-list (result 0) tdate best-features
         (pathout (string-append *config-dir* "swing-features.dat")))

   (if (probe-file pathout)
       (with-open-file (str pathout :direction :input)
            (do ((record (read str nil 'eof) (read str nil 'eof)))
                ((eql record 'eof))
                 (push record best-features))))
 
     (dolist (market markets)
    (set-market  market) (setq result 0)
    (if (and period (> period (cdr (assoc *data-name* *market-days-available*))))
        (setq period nil))
    (ifn period (setq period (cdr (assoc *data-name* *market-days-available*))))
    (setq base-list '(1) candidate-list '(2 3 4 5 6 7 8 9 10 11 12 13 14))
    (loop
     (setq winners-list (swingtrades-add-one-in base-list candidate-list))
     (format T "~%~A~%" market)    
     (setq base-list (append base-list (list (second (car winners-list)))))
     (setq candidate-list (remove (second (car winners-list)) candidate-list))
     (setq result (fourth (car winners-list)));;;fraction of single trade bins
     (if (or (not candidate-list)(> result .90))
         (return))
    )
    (format T "~A First stage winners-list= ~A" market winners-list)

    (setq tdate (car (last (month-days (get-latest-index-date)))))
  
   (loop     
    
      (if (< (fourth (car winners-list)) .60)(return))
      (setq winners-list (swingtrades-leave-one-out base-list))
      (format T "~%~A~%" market)        
      (if (neql (cdr (car winners-list)) 1)
              (setq base-list (remove (second (car winners-list)) base-list))
              (setq base-list (remove (second (second winners-list)) base-list)))
         );;closes the loop
     (format T "~%Best indicator set = ~A~%" base-list)
     (if (assoc market best-features)
          (setf (cdr (assoc market best-features)) base-list)
         (setq best-features (cons (cons market base-list) best-features)))

     (format T "~%~A Best indicator set = ~A~%" market base-list)

);;;cloes the dolist
;   (if (probe-file pathout)(delete-file pathout))
  (with-open-file (str pathout :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith best-features)
          (format str "~A ~%" ith)))
   
   ;  (diary-composite3 '(day) markets)
   (indicator-count pathout)
    ))


(defun display-bin1 (bin)
  (let (contents)
     (setq contents (gethash bin *swing-trade-warehouse1*))
     (dolist (ith contents)
       (print ith))))



;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse
(defun bin-classifier-swingtradesb (date features)
  (let (record bin (result 0) (counter 0) contents epsignal
        (longs 0)(shorts 0) (results-long 0) (results-short 0) (nxdate (getd date 'ndate))
        (num-winners-long 0)(num-winners-short 0))
    (setq record (create-swingtrade-entry-record-list date 1 nil))
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
 ;   (remove-if #'(lambda (s1) (> (svref s1 1) date)) contents))
    (if contents (return) (setq features (butlast features)))
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



;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse
(defun bin-classifier-swingtrades (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (nxdate (getd date 'ndate))
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))

    (setq record (create-swingtrade-entry-record-list date 1 nil))
    (setq record (append record '(nil nil nil)))


  (setq bin (encode-swing-trades record features))
  (setq contents (gethash bin *swing-trade-warehouse*))
    ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *swing-trade-warehouse*) contents))

   (setq contents
    (remove-if #'(lambda(s1) (and ;(eql *data-name* (svref s1 0))
                                  (eql nxdate (svref s1 1)))) contents))
   (dolist (kth contents)

     (when (eql (nth 2 kth) 1)
      (incf longs) (setq results-long (+ results-long (nth 19 kth))))

    (when (and (eql (nth 2 kth) 1)(plusp (nth 19 kth)))
       (incf num-winners-long))

     (when (eql (nth 2 kth) -1)
      (incf shorts)(setq results-short (+ results-short (nth 19 kth))))

     (when (and (eql (nth 2 kth) -1)(plusp (nth 19 kth)))
        (incf num-winners-short))
        );;;closes dolist over contents
  (setq result 0)
  (dolist (jth contents)
         (setq result (+ result (nth 19 jth)))
        (if (plusp (nth 19 jth)) (incf counter))) ;;;closes dolist over contents

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


;;;requires a base features list
(defun swingtrades-leave-one-out (base-features)
  (let (winners-list (result 0) ignore average-profit single-bins)
 
  (dolist (ith base-features)
    (multiple-value-setq (result ignore average-profit single-bins)
                    (apply #'swing-trade-bins (remove ith base-features)))
;    (setq result (float (/ result (sqrt (length swing-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
  (vsort winners-list #'> 'car)
;  (vsort winners-list #'> 'third)
  (format T "Most Valuable Feature = ~A~%" (second (car (last winners-list))))
  (format T "Least Valuable Feature = ~A~%" (second (car winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
  winners-list
 ))

;;;requires a candidate list to add to the base features
;;;this function only looks at separability not predictability
(defun swingtrades-add-one-in (base-features candidate-features)
  (let (winners-list (result 0) average-profit ignore single-bins)
 
  (dolist (ith candidate-features)
    (multiple-value-setq (result ignore average-profit single-bins)
           (apply #'swing-trade-bins (append base-features (list ith))))
;    (setq result (float (/ result (sqrt (length swing-bin-codes)))))
    (setq winners-list (cons (list result ith average-profit single-bins) winners-list))
    )
    (vsort winners-list #'> 'car)
;    (vsort winners-list #'> 'third)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
   winners-list
 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;calculates the twr for a bin
;;;must run #'swing-trade-bins1 before running this function
(defun swing-bin-twr (bin)
  (let ((twr 1.0d0))

   (dolist (record (gethash bin *swing-trade-warehouse*))
     (setq twr (* twr (+ 1.0d0 (/ (* (svref record 2)(- (svref record 18) (svref record 3)))
                                 (svref record 3)))
                           )))
  twr
))


(defun build-swingtrade-warehouse (markets)
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

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun swingtrade-simulation-test (market date2 num &optional (features nil))
 (let (date stop-long stop-short trades long short  trade-long trade-short  entry-long entry-short
        ave-win ave-loss losers winners extended-trades (twr-long 1)(twr-short 1) ignore
       risk epsignal longs long-gains long-acc trading-dates (trade-time 0) singles
       shorts short-gains short-acc prev-signal date-1 record (running-sum 0) bin draw 
       (path-in (string-append *config-dir* "swing-features.dat"))
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "swing-summary.dat"))      
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "swing-simulation.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "swing-diary.dat")))

      (declare (ignore short-acc long-acc ))
     (setq *entry-factor* 1.382)

 ;    (if (and (not features)(probe-file path-in))
 ;     (with-open-file (str path-in :direction :input)
 ;      (do ((record (read str nil 'eof) (read str nil 'eof)))
 ;         ((eql record 'eof))
 ;      (push record swingfeatures))))     
  
 ;  (ifn features
 ;        (setq features (cdr (assoc market swingfeatures))))
  
    (multiple-value-setq (ignore ignore ignore singles)
               (apply #'swing-trade-binsb features))

     (set-market market)(format T "~%~A~%" *data-name*)

   (setq date (add-mkt-days date2 (- num)))
    (setq record (vector date (getd date 'close) 0 0 0))
    (push record trading-dates)
 ;;;;from date1 to date2
 (dotimes (ith num)

     (multiple-value-setq (entry-short entry-long)
        (vprices1 date 4 *entry-factor* 3))
      
   (setq date-1 date date (add-mkt-days date 1))
   (setq record (vector date (getd date 'close) 0 0 0))

   (if long (setf (svref record 2) 1))
   (if short (setf (svref record 2) -1))


    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	   entry-long (+ entry-long (getd date 'rollover))
    		   stop-long (+ stop-long (getd date 'rollover)))
             (setf (svref record 3)(- (svref record 3) (getd date 'rollover)))
)
    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    		  entry-short (+ entry-short (getd date 'rollover))
    		  stop-short (+ stop-short (getd date 'rollover)))
            (setf (svref record 3) (+ (svref record 3) (getd date 'rollover)))
)

   (if (or long short)(setq risk (* 1.000 risk)));;;this ratchets and tightens the risk closer to the market each day
   (when long (setq stop-long (fmax stop-long entry-short (- entry-long risk))))
   (when short (setq stop-short (fmin stop-short entry-long (+ entry-short risk))))

;;;;check if stopped out of prior position

   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long
             (apply #'vector 
                (append trade-long
                        (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long)))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0)
                                                                 (svref trade-long 3))))
          (setf (svref record 2) 1)
          (setf (svref record 3) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (svref record 3)(- (svref record 3) (getd date 'rollover))))
          (setq trade-long nil long nil stop-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade-short
            (apply #'vector (append trade-short
              (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open)))))))
          (push trade-short extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0)
                                                                 (svref trade-short 3))))
          (setf (svref record 2) -1)
          (setf (svref record 3) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if (getd date 'rollover) (setf (svref record 3)(+ (svref record 3) (getd date 'rollover))))
          (setq trade-short nil short nil stop-short nil))
#|
  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))
            (push (- (max (getd date 'open) cover-long) long) trades)
            (setq trade-long
               (apply #'vector 
                  (append trade-long
                  (list date 'exit (max (getd date 'open) cover-long) (- (max (getd date 'open) cover-long) long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-long 0) (svref trade-long 3))))
            (setf (svref record 2) 1)
            (setf (svref record 3) (- (max (getd date 'open) cover-long) (getd date-1 'close)))
            (if (getd date 'rollover)(setf (svref record 3)(- (svref record 3) (getd date 'rollover))))
            (setq trade-long nil long nil stop-long nil))

      (when (and short (< (getd date 'low) cover-short))
            (push (- short (min (getd date 'open) cover-short)) trades)
            (setq trade-short
              (apply #'vector  (append trade-short (list date 'exit cover-short (- short (min (getd date 'open) cover-short))))))
            (push trade-short extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (svref trade-short 0) (svref trade-short 3))))
            (setf (svref record 2) -1)
            (setf (svref record 3) (- (getd date-1 'close) (min (getd date 'open) cover-short)))
            (if (getd date 'rollover)(setf (svref record 3)(+ (svref record 3) (getd date 'rollover))))
            (setq trade-short nil short nil stop-short nil))
|#


;;;if long or short and not entry or exits
      (if long (setf (svref record 3) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (svref record 3)(- (getd date-1 'close)(getd date 'close))))

;;;;calculate bin-classifier
 
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-swingtradesb date-1 *swing-features*))
       (setq twr-short (swing-bin-twr bin))(setf (nth 0 bin) 1)
       (setq twr-long (swing-bin-twr bin))
       (multiple-value-setq (prev-signal )(vsignals1 date-1 4 *entry-factor* 3))


;;;check if new entry

    (when (and (not short) (eql prev-signal 'BUY)(> shorts 0)             
               (<= (getd date 'low) entry-short)
               (> (getd date 'high) entry-short)
               (member epsignal '(OK DOWN))              
               (>= (/ short-gains shorts) 150) (> twr-short 1)
               (setq risk (volatility date 4 (* 1.618 *entry-factor*)))
               (<= (* risk (index-point-value)) *max-swing-risk*)
               )
          (setq short entry-short 
                trade-short (list date 'short short)
                stop-short (+ entry-short risk))
          (setf (svref record 2) -1)
          (setf (svref record 3) (+ (svref record 3)
                (- short (getd date 'close)(/ *swing-commission* (index-point-value)))))
              )

    (when (and (not long)(eql prev-signal 'SELL)(> longs 0)              
               (>= (getd date 'high) entry-long)
               (< (getd date 'low) entry-long)
               (member epsignal '(OK UP))
               (>= (/ long-gains longs) 150) (> twr-long 1)
               (setq risk (volatility date 4 (* 1.618 *entry-factor*)))
               (<= (* risk (index-point-value)) *max-swing-risk*)
               )
           (setq long entry-long 
                 trade-long (list date 'long long)
                 stop-long (- entry-long risk))
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)
                                  (- (getd date 'close) long (/ *swing-commission* (index-point-value)))))
             )


 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long 
                 (apply #'vector (append trade-long (list date 'exit stop-long (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3)(- stop-long long)))
           (setf (svref record 3) (- (svref record 3)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil))


    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long
                (apply #'vector (append trade-long (list date 'exit stop-long (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (svref record 2) 1)
           (setf (svref record 3) (+ (svref record 3) (- stop-long long)))
           (setf (svref record 3) (- (svref record 3)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil
           ))


    (when  (and short (or (< (getd date 'open)(getd date 'close))
                          (>= (getd date 'high) entry-long))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade-short
                (apply #'vector (append trade-short (list date 'exit stop-short (- short stop-short)))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3)(- short stop-short)))
           (setf (svref record 3) (- (svref record 3)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil))


    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade-short
                (apply #'vector (append trade-short (list date 'exit stop-short (- short stop-short)))))
           (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
           (setf (svref record 2) -1)
           (setf (svref record 3) (+ (svref record 3) (- short stop-short)))
           (setf (svref record 3) (- (svref record 3)(- short (getd date 'close))))
           (setq trade-short nil short nil stop-short nil
           ))

#|
 ;;;check if met objective on day of entry

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade-long
                 (apply #'vector (append trade-long (list date 'exit cover-long (- cover-long long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
            (setf (svref record 2) 1)
            (setf (svref record 3) (+ (svref record 3)(- cover-long long)))
            (setf (svref record 3) (- (svref record 3)(- (getd date 'close) long)))
            (setq trade-long nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade-short
                 (apply #'vector (append trade-short (list date 'exit cover-short (- short cover-short)))))
            (push trade-short extended-trades)(setq trade-time (+ trade-time 1))
            (setf (svref record 2) -1)
            (setf (svref record 3) (+ (svref record 3) (- short cover-short)))
            (setf (svref record 3) (- (svref record 3)(- short (getd date 'close))))
            (setq trade-short nil short nil stop-short nil)))
|#

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
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

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
         (svref ith 0)(svref ith 1)(svref ith 2)(svref ith 3)(svref ith 4)(svref ith 5)(svref ith 6)
         (round (* (index-point-value) (svref ith 6))))
    ));;closes the dolist and with-open-file
   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun


 ;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for swing trades to decide to trade or not
;;;;assumes (swing-trade-bins1 features) has already been run
(defun find-best-swing-trade (tdate &optional (output T)(output1 T) (output3 T)(date1 nil))
  (let* ((ctr 0) rollover entry-short entry-long  long short
         cover-long cover-short   directive1
        prev-signal action stop-long stop-short risk-short risk-long (time-frame 'Position)
          trade-direction
        )
    (declare (special epsignal longs long-gains long-acc shorts short-gains short-acc twr-long twr-short bin))
    (declare (ignore  ctr))


    (if (member *data-name* *forex-list*) (setq time-frame 'FX-SWING))

  (setf (gethash tdate VT)(volatility-log tdate 60 1)) ;;volatility over the past three months


   (multiple-value-setq (prev-signal ctr) (vsignals1 tdate 4 *entry-factor* 1))

   (setq trade-direction (cond ((and (member epsignal '(OK UP))(eql prev-signal 'SELL)
                                      (> longs 0)
                                      (>= (/ long-gains longs) 120.0)(> twr-long 1.0)
                                      ) 'UP)
                               ((and (member epsignal '(OK DOWN))(eql prev-signal 'BUY)
                                      (> shorts 0)
                                      (>= (/ short-gains shorts) 120.0)(> twr-short 1.0)
                                      ) 'DN)
                               (t 'FT)))

    (setq cover-short (exp (- (log ave4) (* *objective-factor-swing* (gethash tdate VT))))
          cover-long (exp (+ (log ave4) (* *objective-factor-swing* (gethash tdate VT))))
                              )

    (multiple-value-setq (entry-short entry-long);(vprices tdate 4 1.66 3))
                                     (vprices1 tdate 4 *entry-factor* 1))
    (setq entry-short (min entry-short (getd tdate 'close)) entry-long (max entry-long (getd tdate 'close)))
;;;;sets long and short to be true if open position coming into today
    (setq long (eql (cadr (assoc *data-name* (append *forex-open-swings* *open-positions*))) 'long))
    (setq short (eql (cadr (assoc *data-name* (append *forex-open-swings* *open-positions*))) 'short))

    (if short (setq risk-short (- entry-long entry-short))
        (setq risk-short (* *stop-loss-swing* (abs (- entry-short  (n-day-high tdate 3 'close)))))
                   )

    (if long (setq risk-long (- entry-long entry-short))
        (setq risk-long (* *stop-loss-swing* (abs (- entry-long  (n-day-low tdate 3 'close)))))
        )


    (setq stop-short (+ entry-short risk-short) ;;;this is the init buy stop
          stop-long (- entry-long risk-long))    ;;;this is the init sell stop

    (setq rollover (getd tdate 'rollover))
    (ifn rollover (setq rollover 0))


    (if short (setq stop-short (fmin entry-long (+ rollover (caddr (assoc *data-name* (append *forex-open-swings* *open-positions*)))))))

    (if long (setq stop-long (fmax entry-short (+ rollover (caddr (assoc *data-name* (append *forex-open-swings* *open-positions*)))))))


         (when
          (if (member *data-name* '(US.D1B TY.D1B))
              (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                               (convert-to-decimal (convert-to-32 stop-long)));;;closes the -
                           (index-point-value)));;;closes the round
                  *max-swing-risk* );;;closes the >
              (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))));;closes the *
                               (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))));;closes the -
                           (index-point-value)));;;closes the round
                  *max-swing-risk*));;closes the if
                   (push "NOT TODAY" action));;;closes the when

             (if (eql trade-direction 'up) (push "NOT SHORT" action))
             (if (eql trade-direction 'dn) (push "NOT LONG" action))
             (if (eql trade-direction 'FT) (push "NOT TODAY" action))

             (if (< (/ (- cover-long entry-long) risk-long) *reward-risk-ratio-swing*) (push "NOT LONG" action))
             (if (< (/ (- entry-short cover-short) risk-short) *reward-risk-ratio-swing*) (push "NOT SHORT" action))

           (when (eql (cadr (assoc *data-name* *open-positions*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *open-positions*)) 'short)
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

      (unless (or (equal action "NOT TODAY")(not (member *data-name* *swing-list*)))
       (if (or (equal action "NOT LONG")(equal action "NOT SHORT"))
           (format output "~%--------------------------------------------------------------~%"))
       (if (equal action "NOT LONG") (format output "~A  " "POTENTIAL SELL"))
       (if (equal action "NOT SHORT")(format output "~A  " "POTENTIAL BUY"))
       (when (or (equal action "NOT LONG")(equal action "NOT SHORT"))
            (ifn (eql *data-name* 'sp.d1b) (format output "~A" (nth 3 (assoc *data-name* *c-list*)))
             (format output "~A" (string-append "S&" (subseq (nth 3 (assoc *data-name* *c-list*)) 6 18))))
             (format output "  ~A  ~%" (nth 2 (assoc *data-name* *c-list*)))
             );closes the when
;       (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
;                                   (zerop (nth 1 (assoc *data-name* *C-list*))))  " ~D "
;                          (string-append " ~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F  ")))
;       (if (member *data-name* '(US.D1B TY.D1B))
;           (format output "~9@A  " (convert-to-32nds (getd tdate 'close)))
;         (format output directive
;                     (* (nth 4 (assoc *data-name* *C-list*)) (round (getd tdate 'close) (nth 4 (assoc *data-name* *C-list*))))
;                     ))
       (decode-swing-bin1 bin action output)
      ;(format output " Prev Signal= ~A Current bin= ~3A Direction = ~3A ~% BIN= ~A~%"
       ;              prev-signal epsignal trade-direction bin)

       (when (equal action "NOT SHORT")
        (format output "~%WHEN THE ABOVE COMBINATION OF INDICATORS HAS OCCURRED~%")
        (format output "ALONG WITH SUFFICIENT MOMENTUM TO RISE TO THE ENTRY PRICE~%")
        (format output "HISTORY SHOWS THE FOLLOWING RESULTS USING THE HOBO ENTRY APPROACH~%")
        (format output "~%\# Previous Trades = ~D  Total Gain = $~D  Accuracy = ~4,1,,,F%~%"
                longs long-gains (* 100 long-acc))
        (format output "Average Profit per trade = $~D~%" (round (/ long-gains longs))))
       (when (equal action "NOT LONG")
        (format output "~%WHEN THE ABOVE COMBINATION OF INDICATORS HAS OCCURRED~%")
        (format output "ALONG WITH SUFFICIENT MOMENTUM TO DECLINE TO THE ENTRY PRICE~%")
        (format output "HISTORY SHOWS THE FOLLOWING RESULTS USING THE HOBO ENTRY APPROACH~%")
        (format output "~%\# Previous Trades = ~D  Total Gain = $~D Accuracy = ~4,1,,,F%~%"
                shorts short-gains (* 100 short-acc))
        (format output "Average Profit per trade = $~D~%" (round (/ short-gains shorts))))

      ;;;bin = (direction stochastic trend-signal1(3) TS135 volatility bartype)

        (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))


       (when (equal action "NOT LONG")
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output "~%Place Stop Order to SELL @ ~7@A  ~%If filled Place Buy Stop @ ~7@A~%~
              If filled Place Buy Obective @ ~7@A~%The Initial Risk is $~D\.~%"
             (convert-to-32nds entry-short)(convert-to-32nds stop-short)(convert-to-32nds cover-short)
             (round (* (-  (convert-to-decimal (convert-to-32 stop-short)) (convert-to-decimal (convert-to-32 entry-short))
                           )
                        (index-point-value))));;;closes the format

          (format output
             (string-append "~%Place Stop Order to Sell @ " directive1 "~%If filled Place Buy Stop @ " directive1
              "~%If filled Place Buy Objective @ " directive1 "~%The Initial Risk is $~D\.~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-short (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
                           )  (index-point-value))));closes the format
           ));;closes the if and when


      (when (equal action "NOT SHORT")
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output "~%Place Stop Order to Buy @ ~7@A  ~%If filled Place Sell Stop @ ~7@A~%~
            If filled Place Sell Objective @ ~7@A~%The Initial Risk is $~D\.~%"
             (convert-to-32nds entry-long)(convert-to-32nds stop-long)(convert-to-32nds cover-long)
             (round (* (-  (convert-to-decimal (convert-to-32 entry-long))(convert-to-decimal (convert-to-32 stop-long))
                           )   (index-point-value))));;;closes the format

          (format output
             (string-append "~%Place Stop Order to Buy @ " directive1 "~%If filled Place Sell Stop @ " directive1
              "~%If filled Place Sell Objective @ " directive1 "~%The Initial Risk is $~D\.~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-long (nth 4 (assoc *data-name* *C-list*))))
            (round (* (- (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
                         (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
                            )  (index-point-value))));closes the format
           ));;closes the if and when


         );;;closes the unless





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

       (cond ((eql (cadr (assoc *data-name* *open-positions*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil stop-long cover-long output1)
              (write-txt-record  'OPEN-LONG  nil stop-long cover-long output3)
                  )
             ((eql (cadr (assoc *data-name* *open-positions*)) 'short)
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



 ;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for swing trades to decide to trade or not
;;;;assumes (swing-trade-bins date features) has already been run
(defun find-best-contraswing-trade1 (tdate &optional (output T)(output1 T) (date1 nil))
  (let* ((ctr 0)  entry-short entry-long  long short
         cover-long cover-short   directive1
        prev-signal action stop-long stop-short risk-short risk-long ;(time-frame 'SWING)
         trade-direction
        )
    (declare (special longs shorts long-gains short-gains long-acc short-acc twr-short twr-long))
    (declare (ignore  ctr))

   ; (if (member *data-name* *forex-list*) (setq time-frame 'FX-SWING))

    (multiple-value-setq (entry-long entry-short)  (vprices1 tdate 4 1.66 3))

   (multiple-value-setq (prev-signal ctr) (vsignals1 tdate 4 1.66 3))

   (setq trade-direction (cond  ((and (eql prev-signal 'SELL)
                                     (> longs 0)
                                     (<= (/ long-gains longs) -200.0) (< twr-long 1.0)
                                      ) 'DN)
                               ((and (eql prev-signal 'BUY)
                                     (> shorts 0)
                                     (<= (/ short-gains shorts) -200.0) (< twr-short 1.0)
                                     ) 'UP)
                               (t 'FT)))


   (setq entry-short (max entry-short (getd tdate 'close)) entry-long (min entry-long (getd tdate 'close)))

   (setq risk-short  (* (/ 1 *stop-loss-swing*) (abs (- entry-short (n-day-low tdate 3 'close)))))
   (setq risk-long  (* (/ 1 *stop-loss-swing*) (abs (- entry-long  (n-day-high tdate 3 'close)))))



  ;;;;sets long and short to be true if open position coming into today
    (setq long (eql (cadr (assoc *data-name* *open-contraswings*)) 'long))
    (setq short (eql (cadr (assoc *data-name* *open-contraswings*)) 'short))
    (if long (setq stop-long (nth 2 (assoc *data-name* *open-contraswings*))))
    (if short (setq stop-short (nth 2 (assoc *data-name* *open-contraswings*))))

   (if long (setq cover-long (nth 3 (assoc *data-name* *open-contraswings*)))
         (setq cover-long entry-short))
   (if short (setq cover-short (nth 3 (assoc *data-name* *open-contraswings*)))
         (setq cover-short entry-long))


   (when (and (getd tdate 'rollover) long) (setq
            					 entry-long (+ entry-long (getd tdate 'rollover))
    						 cover-long (+ cover-long (getd tdate 'rollover))
    						 stop-long (+ stop-long (getd tdate 'rollover))))
   (when (and (getd tdate 'rollover) short)(setq
    						 entry-short (+ entry-short (getd tdate 'rollover))
    						 cover-short (+ cover-short (getd tdate 'rollover))
    						 stop-short (+ stop-short (getd tdate 'rollover))))


    (setq stop-long (fmax stop-long (- entry-long risk-long)))

    (setq stop-short (fmin stop-short (+ entry-short risk-short)))


         (when
          (if (member *data-name* '(US.D1B TY.D1B))
              (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                               (convert-to-decimal (convert-to-32 stop-long)));;;closes the -
                           (index-point-value)));;;closes the round
                  *max-swing-risk* );;;closes the >
              (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))));;closes the *
                               (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))));;closes the -
                           (index-point-value)));;;closes the round
                  *max-swing-risk*));;closes the if
                   (push "NOT TODAY" action));;;closes the when

             (if (eql trade-direction 'up) (push "NOT SHORT" action))
             (if (eql trade-direction 'dn) (push "NOT LONG" action))
             (if (eql trade-direction 'FT) (push "NOT TODAY" action))

          ;   (if (< (/ (- cover-long entry-long) risk-long) *reward-risk-ratio-swing*) (push "NOT LONG" action))
          ;   (if (< (/ (- entry-short cover-short) risk-short) *reward-risk-ratio-swing*) (push "NOT SHORT" action))


           (when (eql (cadr (assoc *data-name* *open-contraswings*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *open-contraswings*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))


           (when (eql (cadr (assoc *data-name* *forex-open-contraswings*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *forex-open-contraswings*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))

         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))


      (format output "~%Contra Swing   ~A  ~%" action)

      (format output " Prev Signal= ~A  Direction = ~3A ~%"
                     prev-signal  trade-direction )

      (format output " Num Longs = ~D P/L for Longs = ~4,2,,,F Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~4,2,,,F Accuracy for Short = ~A~%" shorts short-gains short-acc)

      (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
      (format output "~%SELL= ~7@A  INIT-BUY-STOP= ~7@A COVER-SHORT= ~7@A RISK= ~D~%BUY= ~7@A  INIT-SELL-STOP= ~7@A  COVER-LONG= ~7@A RISK= ~D~%"
          (convert-to-32nds entry-short)(convert-to-32nds stop-short)(convert-to-32nds cover-short)
          (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
                        (convert-to-decimal (convert-to-32 entry-short))
                       ) (index-point-value)))
          (convert-to-32nds entry-long) (convert-to-32nds stop-long)(convert-to-32nds cover-long)
          (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                        (convert-to-decimal (convert-to-32  stop-long))
                       ) (index-point-value))));;;closes the format

      (format output
        (string-append "~%SELL= " directive1 "  INIT-BUY-STOP= " directive1 " COVER-SHORT= " directive1 " RISK= ~D~% BUY= "
        directive1 " INIT-SELL-STOP= " directive1 "  COVER-LONG= " directive1 " RISK= ~D~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-short (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
                             )  (index-point-value)))
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-long (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long
                                                                         (nth 4 (assoc *data-name* *C-list*))))
                       )  (index-point-value)))
            ));;;closes the format and the if



    ;  (cond ((equal action "OK      ")
    ;         (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
    ;         (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
    ;             )
    ;
    ;         ((equal action "NOT SHORT")
    ;          (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
    ;            )
    ;
    ;         ((equal action "NOT LONG")
    ;          (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
    ;           ))

       (cond ((eql (cadr (assoc *data-name* *open-contraswings*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'OPEN 'LONG date1 nil stop-long cover-long output1)
                  )
             ((eql (cadr (assoc *data-name* *open-contraswings*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil stop-short cover-short output1)
                  )

             ((eql (cadr (assoc *data-name* *forex-open-contraswings*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'LONG date1 nil stop-long cover-long output1)
                  )
             ((eql (cadr (assoc *data-name* *forex-open-contraswings*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'SHORT date1 nil stop-short cover-short output1)

                  ));;;;closes the cond



      ));;;closes the let and the defun


;;;this version has fade breakout systems
;;;;
(defun find-best-contraday-trade1 (tdate &optional (output T)(output1 T)(date1 nil))
  (let (risk risk-short risk-long  stop-short stop-long  entry-long entry-short
        trade-direction (VT (make-hash-table))
        cover-long cover-short ave4 action directive1
        )

       (declare (special longs shorts long-gains short-gains long-acc short-acc))


   (if (and (> longs 0)(<= (/ long-gains longs) -200)) (push 'DN trade-direction))
   (if (and (> shorts 0)(<= (/ short-gains shorts) -200)) (push 'UP trade-direction))

   (setf (gethash tdate VT)(volatility-log tdate 60 1))
    (multiple-value-setq (entry-long entry-short) (vprices1 tdate 4 *entry-factor* 1))
   (setq ave4 (ave tdate 4 'pivot)
           cover-short (exp (- (log ave4) (* *objective-factor* (gethash tdate VT))))
           cover-long (exp (+ (log ave4) (* *objective-factor* (gethash tdate VT))))
           )
   (setq cover-short
      (max cover-short (- entry-short (* *stop-loss-day* (abs (- entry-short (getd tdate 'close)))))))
    (setq cover-long
      (min cover-long (+ entry-long (* *stop-loss-day* (abs (- entry-long (getd tdate 'close)))))))



   (setq risk-short  (*  (/ 1 *stop-loss-day*) (abs (- entry-short (getd tdate 'close)))))
   (setq risk-long  (*  (/ 1 *stop-loss-day*) (abs (- entry-long (getd tdate 'close)))))
   (setq risk (max risk-long risk-short))

   (setq stop-short (+ entry-short risk) stop-long (- entry-long risk))


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
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))

      (format output " CONTRA ~A~%" action)
      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~A~%" shorts short-gains short-acc)
      (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A  STOP= ~7@A  COVER-SHORT= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A  COVER-LONG= ~7@A   LONG-RISK= ~D~%"
           (convert-to-32nds entry-short) (convert-to-32nds stop-short)(convert-to-32nds cover-short)
           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))(convert-to-decimal (convert-to-32 entry-short)))(index-point-value)))
           (convert-to-32nds entry-long)(convert-to-32nds stop-long) (convert-to-32nds cover-long)
           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                         (convert-to-decimal (convert-to-32 stop-long))
                       ) (index-point-value))))
         (format output

            (string-append "~%SELL= " directive1 " STOP= " directive1 " COVER-SHORT= " directive1
                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  COVER-LONG= " directive1 "  LONG-RISK= ~D~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-short (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
                             )  (index-point-value)))
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-long (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
                       )  (index-point-value)))
            ))

     (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" "CONTRA DAY" 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" "CONTRA DAY" 'LONG date1 entry-long stop-long cover-long output1))

             ((equal action "NOT SHORT")
               (write-xml-record tdate "tblTradeRecs" "CONTRA DAY" 'LONG date1 entry-long stop-long cover-long output1))

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" "CONTRA DAY" 'SHORT date1 entry-short stop-short cover-short output1)
                                   ))


      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;enters with stop orders and exits with stop loss orders always in the market
;;;Plain vanilla volatility breakout with 35 2.618 and 7 parameter values.
;;;;For Position TRADING
;;;Designed to write out the file for the position Trades Warehouse.

;;;;the file has one record per trade each record is a list. The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1
;;;;SST is the stocastic with parameter 21
;;;;T5 is the trend-signal with parameter 5
;;;;T15 is the trend-signal with parameter 15
;;;T45 is the trend-signal with parameter 45
;;;RSD is the rsi14 less the rsi14 two days prior. (2 day rate of change in the rsi with paramter 14

;;;the value is sell buy or flat
;;;Mom-div is the momentum divergence using the MACD with parameters 5 13 5. The value is up dn or ft.
;;;
;;; (*data-name* entry-date direction entry-price T5 T15 T45 T135 VR BT BR BR-1 SST mom-div RSD CAN PIN)
;;;the values of 18 2.53 and 7 were determined from extensive testing of the plain vanilla system

(defun populate-position-trades1 (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long  entry-short entry-long
       ave-win ave-loss losers winners extended-trades trade date-1
       (path1 (string-append *upper-dir* (format nil "~S" market) "positiontrades1.dat")))
   (maind-x)(set-cat-list)
   (set-market market)
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))


 ;;;;from date1 to date2
 (dotimes (ith num)

;   (multiple-value-setq (entry-short entry-long) (vprices1 date 18 2.53 7))

   (setq  date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
;        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))

    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
 ;   						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))
    ))

  ;;;;check if stopped out of prior position


;;;check if stopped out
   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long
          (append trade-long (list date (my-pretty-price (min stop-long (getd date 'open)))
                (round (* (index-point-value) (- (min stop-long (getd date 'open)) long))))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date (my-pretty-price (max stop-short (getd date 'open)))
                 (round (* (index-point-value) (- short (max stop-short (getd date 'open))))))))
          (push trade extended-trades)
          (setq trade nil short nil stop-short nil))


   (when long (setq stop-long (max stop-long (- (getd date-1 'close) risk))))
   (when short (setq stop-short (min stop-short (+ (getd date-1 'close) risk))))


;;;check if new entry

    (when (and (not short)
               (<= (getd date 'low) entry-short)
               )
          (setq short (min entry-short (getd date 'open))
                 trade (create-position-entry-record-list1 date-1 -1 short)
                 stop-short
                 entry-long
         ))


    (when (and (not long)
               (>= (getd date 'high) entry-long)
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (create-position-entry-record-list1 date-1 1 long)
                 stop-long
                 entry-short
              ))


 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long
                             (list date (my-pretty-price stop-long) (round (* (index-point-value)(- stop-long long))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))


    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long
                     (list date (my-pretty-price stop-long) (round (* (index-point-value)(- stop-long long))))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))


    (when  (and short (or (< (getd date 'open)(getd date 'close))
                          (>= (getd date 'high) entry-long))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade
                         (list date (my-pretty-price stop-short) (round (* (index-point-value)(- short stop-short))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))


    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade
                        (list date (my-pretty-price stop-short) (round (* (index-point-value)(- short stop-short))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           ))

   );;;closes the dotimes


  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )

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
(defun create-position-entry-record-list1 (date direction entry)
   (let* ((date-1 (getd date 'ydate)))
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))

             (trend-signal date 5);;;feature 2
             (trend-signal date 15);;;feature 3
             (trend-signal date 45);;;feature 4
             (trend-signal date 135);;;feature 5
             (my-round (/ (volatility date 7 1) (volatility date 49 1)) 3);;feature 6
             (day-bar-type date);;;feature 7
             (day-bar-type1 date);;feature 8
             (day-bar-type1 date-1);;feature 9
             (round (slow-stochastic date 21));;feature 10

             (mo-diver date 5);;feature 11
             (mo-diver date 15) ;;feature 12
             (mo-diver date 45);;feature 13
             (2-bar-index date) ;;feature 14


          ;   (macd-signal date 12 26 9);;;feature 11
          ;   (round (rsi-ave-diff date 14 2)) ;;;feature 12
          ;   (candle date 6 3) ;;;feature 13
          ;   (pinpoint date);;;feature 14

                            );;;closes the list

))


(defun position-trade-bins1 (&rest features)
  (let (bin path)
  (if *stocks* (setq path (string-append *upper-dir* "stockspositionwarehouse1.dat"))
    (setq path (string-append *upper-dir* "positiontradewarehouse1.dat")))
  (maind-x)(set-cat-list)
  (setq positions nil position-bin-codes nil)(clrhash *position-trade-warehouse1*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record positions)))



;;;;now all the trades are in the list swings.
;;;we now assign/sort the trades into bins
;;;basically process a record list into a code or bin
;;;and create a code

   (dolist (record positions)
     (setq bin (encode-position-trades1 record features))
     (pushnew bin position-bin-codes :test #'equal)
     (cond ((gethash bin *position-trade-warehouse1*)
            (ifn (member record (gethash bin *position-trade-warehouse1*) :test #'equal)
                 (setf (gethash bin *position-trade-warehouse1*)
                       (cons record (gethash bin *position-trade-warehouse1*)))))
           ((not (gethash bin *position-trade-warehouse1*))
             (setf (gethash bin *position-trade-warehouse1*)
              (list record)))))

    (format T  "~%FEATURES = ~A~%" features)
    (rank-position-bins-by-profit1 positions)


  ))


(defun remove-position-trade-market1 (market &optional (stocks nil))
  (let (trades path)
  (if stocks (setq path (string-append *upper-dir* "stockspositionwarehouse1.dat"))
     (setq path (string-append *upper-dir* "positiontradewarehouse1.dat")))
  (maind-x)(set-cat-list)

    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))

   (setq trades (remove-if #'(lambda (s) (eql (car s) market)) trades))
   (with-open-file (str path :direction :output :if-exists :supersede)
       (dolist (ith trades)
        (format str "~S~%" ith)))
 ))


;;;;this program can be edited to change the function and the arguments to replace an existing slot
;;;replaces features  4 5 6 8 (slots  6 7 8 10)
(defun replace-position-indicators ()
   (let ((path (string-append *upper-dir* "positiontradewarehouse1.dat"))
          date-1 )
  (maind-x)(set-cat-list)(setq positions nil)
  (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record positions)))

  (dolist (ith positions)
     (set-market (nth 0 ith))
     (setq date-1 (getd (nth 1 ith) 'ydate))
 ;    (format T "~%~A ~A ~A"  (nth 0 ith) (nth 1 ith) date-1)

     (setf (nth 13 ith) (mo-diver date-1 5));;feature 11 rsi-trend

     (setf (nth 14 ith)(mo-diver date-1 15)) ;;feature 12
     (setf (nth 15 ith)(mo-diver date-1 45));;feature 13
     (setf (nth 16 ith)(2-bar-index date-1)) ;;feature 14

     );;;closes the dolist


  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth positions)
      (format str "~S~%" jth)))

));;;closes the defun
;;;returns a list of nine codes
(defun encode-position-trades1 (record features)
  (let (bin-list)

    (dolist (ith features)
     (case ith
;;;;Feature 1 has 2 levels
     (1 (push (nth 2 record) bin-list));;;adds the direction

;;;Feature 2 with 5 levels; This is the TS5
      (2 (cond ((eql (nth 4 record) 'DN) (push -1 bin-list))
             ((eql (nth 4 record) 'CD) (push -2 bin-list))
             ((eql (nth 4 record) 'UP) (push 1 bin-list))
             ((eql (nth 4 record) 'CU) (push 2 bin-list))
             ((eql (nth 4 record) 'FT) (push 0 bin-list))
             ) )
;;;Feature 3 with 5 levels; This is TS15
     (3  (cond ((eql (nth 5 record) 'DN) (push -1 bin-list))
             ((eql (nth 5 record) 'CD) (push -2 bin-list))
             ((eql (nth 5 record) 'UP) (push 1 bin-list))
             ((eql (nth 5 record) 'CU) (push 2 bin-list))
             ((eql (nth 5 record) 'FT) (push 0 bin-list))
             ) )

;;;Feature 4 with 5 levels; This is TS45
    (4   (cond ((eql (nth 6 record) 'DN) (push -1 bin-list))
             ((eql (nth 6 record) 'CD) (push -2 bin-list))
             ((eql (nth 6 record) 'UP) (push 1 bin-list))
             ((eql (nth 6 record) 'CU) (push 2 bin-list))
             ((eql (nth 6 record) 'FT) (push 0 bin-list))
             ) )
 ;;;Feature 5 with 5 levels; This is TS135
    (5   (cond ((eql (nth 7 record) 'DN) (push -1 bin-list))
             ((eql (nth 7 record) 'CD) (push -2 bin-list))
             ((eql (nth 7 record) 'UP) (push 1 bin-list))
             ((eql (nth 7 record) 'CU) (push 2 bin-list))
             ((eql (nth 7 record) 'FT) (push 0 bin-list))
             ) )


 ;;;;Feature 6 with 10 levels ;this is the HT ratio of volatility
      (6 (cond ((< (nth 8 record) .60) (push 5 bin-list))
               ((and (>= (nth 8 record) .60)(< (nth 8 record) .70)) (push 4 bin-list))
               ((and (>= (nth 8 record) .70)(< (nth 8 record) .80)) (push 3 bin-list))
               ((and (>= (nth 8 record) .80)(< (nth 8 record) .90)) (push 2 bin-list))
               ((and (>= (nth 8 record) .90)(< (nth 8 record) 1.0)) (push 1 bin-list))
               ((and (>= (nth 8 record) 1.0)(< (nth 8 record) 1.10)) (push -1 bin-list))
               ((and (>= (nth 8 record) 1.10)(< (nth 8 record) 1.20)) (push -2 bin-list))
               ((and (>= (nth 8 record) 1.20)(< (nth 8 record) 1.30)) (push -3 bin-list))
               ((and (>= (nth 8 record) 1.30)(< (nth 8 record) 1.40)) (push -4 bin-list))
               ((>= (nth 8 record) 1.40)(push -5 bin-list))))


     ;;;;Feature 7 with 9 levels bar type
      (7 (cond ((eql (nth 9 record) 11) (push 0 bin-list))
               ((eql (nth 9 record) 12) (push 1 bin-list))
               ((eql (nth 9 record) 13) (push 2 bin-list))
               ((eql (nth 9 record) 21) (push 3 bin-list))
               ((eql (nth 9 record) 22) (push 4 bin-list))
               ((eql (nth 9 record) 23) (push 5 bin-list))
               ((eql (nth 9 record) 31) (push 6 bin-list))
               ((eql (nth 9 record) 32) (push 7 bin-list))
               ((eql (nth 9 record) 33) (push 8 bin-list)) ))

;;;;;;Feature 8 with 5 levels of bar relationship to the previous bar

      (8 (cond ((eql (nth 10 record) 'IN) (push -2 bin-list))
               ((eql (nth 10 record) 'OU) (push 2 bin-list))
               ((eql (nth 10 record) 'DN) (push -1 bin-list))
               ((eql (nth 10 record) 'UP) (push 1 bin-list))
               ((eql (nth 10 record) 'FT) (push 0 bin-list))
                     ))
;;;;feature 9 with 5 levels of previous bar relationship to its prior bar
      (9 (cond ((eql (nth 11 record) 'IN) (push -2 bin-list))
               ((eql (nth 11 record) 'OU) (push 2 bin-list))
               ((eql (nth 11 record) 'DN) (push -1 bin-list))
               ((eql (nth 11 record) 'UP) (push 1 bin-list))
               ((eql (nth 11 record) 'FT) (push 0 bin-list))
                     ))


 ;;;;Feature 10 with 7 levels ;item 12 is the stochastic with parameter 21
     (10 (cond ((<= (nth 12 record) 10) (push 3 bin-list))
              ((and (> (nth 12 record) 10)(<= (nth 12 record) 20)) (push 2 bin-list))
              ((and (> (nth 12 record) 20)(<= (nth 12 record) 40)) (push 1 bin-list))
              ((and (> (nth 12 record) 40)(<= (nth 12 record) 60)) (push 0 bin-list))
              ((and (> (nth 12 record) 60)(<= (nth 12 record) 80)) (push -1 bin-list))
              ((and (> (nth 12 record) 80)(<= (nth 12 record) 90)) (push -2 bin-list))
              ((>= (nth 12 record) 90) (push -3 bin-list))))


;;;Feature 11 with 5 levels; this is the rsi-trend 5

     (11    (push (nth 13 record) bin-list))



;;;;Feature 12 with 6 levels ; This is rsi trend 15
      (12  (push (nth 14 record) bin-list))


;;;Feature 13 with 5 levels is the trend-signal 45
       (13 (push (nth 15 record) bin-list))


;;;Feature 14 with 6 levels is the pivot-index with date-1
       (14  (push (nth 16 record) bin-list))



                ));;;closes the case and the dolist over features

      (reverse bin-list)
 ))

(defun rank-position-bins-by-accuracy1 ()
  (let (contents result accuracy-list)
    (dolist (ith position-bin-codes)
     (setq contents
          (gethash ith *position-trade-warehouse1*))
     (setq result 0)
     (dolist (kth contents)
        (if (> (nth 19 kth) 0) (incf result)))
     (setq accuracy-list (acons (/ result (length contents)) ith accuracy-list)));;;closes the dolist over bin-codes
     (vsort accuracy-list #'> 'car)
     (format T "Accuracy  Bin~%")
     (dolist (jth accuracy-list)
       (format T "~4F     ~A ~%"
                   (my-round (car jth) 2)(cdr jth)))
 ))

;;;;this function expects that you have run (swing-trade-bins ...) already
(defun rank-position-bins-by-expected-value1 ()
  (let (contents expected-value-list result (path1 (string-append *output-upper-dir* "position-expected-value1.dat")))
   (dolist (ith position-bin-codes)
     (setq contents
           (gethash ith *position-trade-warehouse1*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (nth 19 kth))))
     (setq expected-value-list (cons (list (/ result (length contents)) ith (length contents)) expected-value-list)));;;closes the dolist over bin-codes
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
         (vsort expected-value-list #'> 'car)
         (format stream "Expected          Bin           NUM~%Value~%")
         (dolist (jth expected-value-list)
             (format stream "~8A      ~A       ~D~%" (round (car jth)) (cadr jth) (third jth)))) ;;;;closes the with-open-file
 ))


(defun rank-position-bins-by-profit1 (positions)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0) (twr 1)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith position-bin-codes)
     (setq contents (gethash ith *position-trade-warehouse1*))
     (setq result 0 counter 0 twr 1)

     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)
         (setq result (+ result (nth 19 kth)))
         (if (plusp (nth 19 kth)) (incf counter))
         (setq twr (* twr (+ 1 (/ (if (plusp (nth 2 kth))
                                 (- (nth 18 kth) (nth 3 kth))
                               (- (nth 3 kth) (nth 18 kth)))
                             (nth 3 kth)))))
         ) ;;;closes dolist over contents
      (if (and (plusp result)(> twr 1)) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))


     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over bin-codes

    (dolist (kth positions)
       (if (plusp (nth 19 kth)) (setq all-winners (+ all-winners (nth 19 kth)))
           (setq all-losers (+ all-losers (nth 19 kth)))))

     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))

     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length positions)(length (num-markets-in-warehouse positions)))
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

(defun display-position-bin1 (bin)
  (let (contents)
     (setq contents (gethash bin *position-trade-warehouse1*))
     (dolist (ith contents)
       (print ith))))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse
(defun bin-classifier-positions1 (date features)
  (let (record bin (result 0) (counter 0) contents epsignal (nxdate (getd date 'ndate))
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))

    (setq record (create-position-entry-record-list1 date 1 nil))
    (setq record (append record '(nil nil nil)))


  (setq bin (encode-position-trades1 record features))
  (setq contents (gethash bin *position-trade-warehouse1*))
      ;;; then look for short trades. rule All bins are long or short trades
    (setf (nth 0 bin) -1)
    (setq contents (append (gethash bin *position-trade-warehouse1*) contents))


   (setq contents
    (remove-if #'(lambda(s1) (and (eql *data-name* (car s1))
                                  (eql nxdate (cadr s1)))) contents))

   (dolist (kth contents)

     (when (eql (nth 2 kth) 1)
      (incf longs) (setq results-long (+ results-long (nth 19 kth))))

    (when (and (eql (nth 2 kth) 1)(plusp (nth 19 kth)))
       (incf num-winners-long))

     (when (eql (nth 2 kth) -1)
      (incf shorts)(setq results-short (+ results-short (nth 19 kth))))

     (when (and (eql (nth 2 kth) -1)(plusp (nth 19 kth)))
        (incf num-winners-short))
        );;;closes dolist over contents
  (setq result 0)
  (dolist (jth contents)
         (setq result (+ result (nth 19 jth)))
        (if (plusp (nth 19 jth)) (incf counter))) ;;;closes dolist over contents

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


;;;requires a base features list
(defun positions-leave-one-out1 (base-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq stocks T))
  (apply #'position-trade-bins1 base-features)

  (dolist (ith base-features)
    (setq result (apply #'position-trade-bins1 (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 ))

;;;requires a candidate list to add to the base features
(defun positions-add-one-in1 (base-features candidate-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (Setq *stocks* T))
  (apply #'position-trade-bins1 base-features)

  (dolist (ith candidate-features)
    (setq result (apply #'position-trade-bins1 (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))


;;;calculates the twr for a bin
;;;must run #'position-trade-bins1 before running this function
(defun position-bin-twr1 (bin)
  (let ((twr 1))

   (dolist (record (gethash bin *position-trade-warehouse1*))
     (setq twr (* twr (+ 1 (/ (if (plusp (nth 2 record))
                                 (- (nth 18 record) (nth 3 record))
                               (- (nth 3 record) (nth 18 record)))
                             (nth 3 record)))
                           )))
  twr
))


;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for position trades to decide to trade or not
;;;;assumes (position-trade-bins1 date features) has already been run
(defun find-best-position-trade1 (tdate &optional (output T)(output1 T) (output3 T)(date1 nil))
  (let* ((ctr 0) rollover entry-short entry-long  long short twr-long twr-short bin
         cover-long cover-short (ave4 (ave tdate 5 'pivot))  directive1
        prev-signal action stop-long stop-short risk-short risk-long
        (VT (make-hash-table)) epsignal longs long-gains long-acc shorts short-gains short-acc trade-direction
        )
      (declare (ignore  ctr))

  (setf (gethash tdate VT)(volatility-log tdate 60 1)) ;;volatility over the past three months

   (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
          (bin-classifier-positions1 tdate *position-features1*))
   (setq twr-short (position-bin-twr1 bin))(setf (nth 0 bin) 1)
   (Setq twr-long (position-bin-twr1 bin))
   (multiple-value-setq (prev-signal ctr) (vsignals1 tdate 18 2.53 7))

   (setq trade-direction (cond ((and (member epsignal '(OK UP))(eql prev-signal 'SELL)
                                      (> longs 0)
                                      (>= (/ long-gains longs) 150)(> twr-long 1)
                                      ) 'UP)
                               ((and (member epsignal '(OK DOWN))(eql prev-signal 'BUY)
                                      (> shorts 0)
                                      (>= (/ short-gains shorts) 150)(> twr-short 1)
                                      ) 'DN)
                               (t 'FT)))

    (setq cover-short (exp (- (log ave4) (* *objective-factor-position* (gethash tdate VT))))
          cover-long (exp (+ (log ave4) (* *objective-factor-position* (gethash tdate VT))))
                              )

    (multiple-value-setq (entry-short entry-long)(vprices1 tdate 18 2.53 7))

    (setq entry-short (min entry-short (getd tdate 'close)) entry-long (max entry-long (getd tdate 'close)))

    (setq risk-short
         (min (- entry-long entry-short)(abs (- entry-short  (n-day-high tdate 7 'close)))))

    (setq risk-long
        (min (- entry-long entry-short)(abs (- entry-long  (n-day-low tdate 7 'close)))))


    (setq stop-short (+ entry-short risk-short) ;;;this is the init buy stop
          stop-long (- entry-long risk-long))    ;;;this is the init sell stop

    (setq rollover (getd tdate 'rollover))
    (ifn rollover (setq rollover 0))
;;;;sets long and short to be true if open position coming into today
    (setq long (eql (cadr (assoc *data-name* *open-positions*)) 'long))
    (setq short (eql (cadr (assoc *data-name* *open-positions*)) 'short))


    (if short (setq stop-short (fmin entry-long (+ rollover (caddr (assoc *data-name* *open-positions*))))))
    (if long (setq stop-long (fmax entry-short (+ rollover (caddr (assoc *data-name* *open-positions*))))))

    (format output " Prev Signal= ~A Current bin= ~3A Direction = ~3A "
                     prev-signal epsignal trade-direction )


         (when
          (if (member *data-name* '(US.D1B TY.D1B))
              (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                               (convert-to-decimal (convert-to-32 stop-long)));;;closes the -
                           (index-point-value)));;;closes the round
                  *max-position-risk* );;;closes the >
              (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))));;closes the *
                               (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))));;closes the -
                           (index-point-value)));;;closes the round
                  *max-position-risk*));;closes the if
                   (push "NOT TODAY" action));;;closes the when

             (if (eql trade-direction 'up) (push "NOT SHORT" action))
             (if (eql trade-direction 'dn) (push "NOT LONG" action))
             (if (eql trade-direction 'FT) (push "NOT TODAY" action))

           (if (< (/ (- cover-long entry-long) risk-long) *reward-risk-ratio-swing*) (push "NOT LONG" action))
           (if (< (/ (- entry-short cover-short) risk-short) *reward-risk-ratio-swing*) (push "NOT SHORT" action))


           (when (eql (cadr (assoc *data-name* *open-positions*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *open-positions*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))


         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))


      (format output "~A  ~%" action)
      (format output " Num Longs = ~D P/L for Longs = ~4,2,,,F Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~4,2,,,F Accuracy for Short = ~A~%" shorts short-gains short-acc)

      (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
      (format output "~%SELL= ~7@A  INIT-BUY-STOP= ~7@A COVER-SHORT= ~7@A RISK= ~D~%BUY= ~7@A  INIT-SELL-STOP= ~7@A  COVER-LONG= ~7@A RISK= ~D~%"
          (convert-to-32nds entry-short)(convert-to-32nds stop-short)(convert-to-32nds cover-short)
          (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
                        (convert-to-decimal (convert-to-32 entry-short))
                       ) (index-point-value)))
          (convert-to-32nds entry-long) (convert-to-32nds stop-long)(convert-to-32nds cover-long)
          (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                        (convert-to-decimal (convert-to-32  stop-long))
                       ) (index-point-value))));;;closes the format

      (format output
        (string-append "~%SELL= " directive1 "  INIT-BUY-STOP= " directive1 " COVER-SHORT= " directive1 " RISK= ~D~% BUY= "
        directive1 " INIT-SELL-STOP= " directive1 "  COVER-LONG= " directive1 " RISK= ~D~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-short (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
                             )  (index-point-value)))
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-long (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long
                                                                         (nth 4 (assoc *data-name* *C-list*))))
                       )  (index-point-value)))
            ));;;closes the format and the if


      (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" 'POSITION 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" 'POSITION 'LONG date1 entry-long stop-long cover-long output1)
             (write-txt-record  'SHORT-ENTRY entry-short stop-short cover-short output3)
             (write-txt-record  'LONG-ENTRY  entry-long stop-long cover-long output3)
                 )

            ((equal action "NOT SHORT")
             (write-xml-record tdate "tblTradeRecs" 'POSITION 'LONG date1 entry-long stop-long cover-long output1)
             (write-txt-record  'LONG-ENTRY  entry-long stop-long cover-long output3)
                )

            ((equal action "NOT LONG")
             (write-xml-record tdate "tblTradeRecs" 'POSITION 'SHORT date1 entry-short stop-short cover-short output1)
             (write-txt-record  'SHORT-ENTRY entry-short stop-short cover-short output3)
               ))

       (cond ((eql (cadr (assoc *data-name* *open-positions*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil stop-long cover-long output1)
              (write-txt-record  'OPEN-LONG  nil stop-long cover-long output3)
                  )
             ((eql (cadr (assoc *data-name* *open-positions*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'SHORT date1 nil stop-short cover-short output1)
              (write-txt-record  'OPEN-SHORT nil stop-short cover-short output3)
                  )
            ; ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'long)
            ;  (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'LONG date1 nil stop-long cover-long output1)
            ;      )
            ; ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'short)
            ;  (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'SHORT date1 nil stop-short cover-short output1)
            ;    )
                );;;;closes the cond

      ));;;closes the let and the defun


;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun position-simulation-test1 (market date2 num &optional (stocks nil) (features *position-features1*))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade (twr-long 1)(twr-short 1)
       risk risk-long risk-short  epsignal longs long-gains long-acc record trading-dates bin draw
       shorts short-gains short-acc prev-signal ctr date-1 (running-sum 0)(trade-time 0)
       (summary-path (string-append *output-upper-dir* (format nil "~S" market) "position-summary1.dat"))
       (AT (make-hash-table)) (VT (make-hash-table)) next-date last-trading-date
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "position-simulation1.dat"))
       (diary-path (string-append *output-upper-dir* (format nil "~S" market) "position-diary1.dat")))
      (declare (ignore  short-acc long-acc ctr))

    (if stocks (setq *stocks* T))
   (apply #'position-trade-bins1 features)
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
        (vprices1 date 18 2.53 7))

   (setq entry-short (min entry-short (getd date 'close)) entry-long (max entry-long (getd date 'close)))

   (setq ave4 (gethash date AT)
       cover-short  (exp (- (log ave4) (* *objective-factor-position* (gethash date VT))))
       cover-long (exp (+ (log ave4) (* *objective-factor-position* (gethash date VT)))))

   (setq risk-short  (* *stop-loss-position* (abs (- entry-short (n-day-high date 7 'close)))
                          ))
   (setq risk-long  (* *stop-loss-position* (abs (- entry-long  (n-day-low date 7 'close)))
                       ))

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
          (setq trade-long
              (append trade-long
                   (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
          (setq trade-long nil long nil stop-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
            (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade nil short nil stop-short nil))

  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))

            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (- (max (getd date 'open) cover-long) (getd date-1 'close)))
            (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
            (setq trade-long nil long nil stop-long nil))

      (when (and short (< (getd date 'low) cover-short))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (- (getd date-1 'close) (min (getd date 'open) cover-short)))
            (if (getd date 'rollover)(setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
            (setq trade nil short nil stop-short nil))

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(<= (getd date 'low) entry-short)
                (<= (* risk (index-point-value)) *max-position-risk*))
           (and (not long) (>= (getd date 'high) entry-long)
                (<= (* risk (index-point-value)) *max-position-risk*)))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-positions1 date-1 features))
        (setq twr-short (position-bin-twr1 bin))(setf (nth 0 bin) 1)
        (setq twr-long (position-bin-twr1 bin))
       (multiple-value-setq (prev-signal ctr)(vsignals1 date-1 18 2.53 7)))


;;;check if new entry

    (when (and (not short)
               (<= (getd date 'low) entry-short)
               (member epsignal '(OK DOWN))
               (eql prev-signal 'BUY)(> shorts 0)
               (>= (/ short-gains shorts) 150) (> twr-short 1)
               (<= (* risk (index-point-value)) *max-position-risk*)
               )
          (setq short (min entry-short (getd date 'open))
                 trade (list date 'short short)
                 stop-short (+ entry-short risk-short))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(/ *swing-commission* (index-point-value)))))
         )


    (when (and (not long)
               (>= (getd date 'high) entry-long)
               (member epsignal '(OK UP))
               (eql prev-signal 'SELL)(> longs 0)
               (>= (/ long-gains longs) 150) (> twr-long 1)
               (<= (* risk (index-point-value)) *max-position-risk*)
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long)
                 stop-long (- entry-long risk-long))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (- (getd date 'close) long (/ *swing-commission* (index-point-value)))))
              )


 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil))


    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
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
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade nil short nil stop-short nil))


    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade nil short nil stop-short nil
           ))


 ;;;check if met objective on day of entry

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
            (setq trade-long nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (+ (nth 3 record) (- short cover-short)))
            (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
            (setq trade nil short nil stop-short nil)))


      (setf (nth 4 record) (+ (nth 3 record) running-sum))
      (setq running-sum (nth 4 record))
      (push record trading-dates)
      (setq record nil)

   );;;closes the dotimes

   (setq last-trading-date (caar trading-dates) trading-dates (reverse trading-dates))

   (with-open-file (stream diary-path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith trading-dates)
        (format stream "~A\,~F\,~D\,~D,~D~%"
           (first ith) (second ith) (third ith) (round (* (index-point-value)(nth 3 ith)))
           (round (* (index-point-value) (nth 4 ith))))
         ;;;now to fill in the holidays
        (setq next-date (add-days-to-date2 (car ith) 1))
         (loop

            (cond ((and ith (> next-date last-trading-date)) (return))
                  ((not (week-day-p next-date)))
                  ((and (week-day-p next-date)
                        (not (assoc next-date trading-dates)))
                   (format stream "~A\,~F\,~D\,~D,~D~%"
                           next-date (second ith) (third ith) 0
                        (round (* (index-point-value) (nth 4 ith)))))
                  (t (return)))
            (setq next-date (add-days-to-date2 next-date 1))

                  )

     ));;;closes dolist and with-open-file

  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (with-open-file (str summary-path :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" (index-lname)
                           (date-convert (add-mkt-days date2 (- num)))(date-convert date2))

      (format str "P/L= ~16D  NUMBER TRADES=    ~6A ACCURACY=       ~,1,2,'*,' F%~%~
        P/L PER TRADE= ~6D  AVERAGE GAIN= ~8D   AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=    ~4,2,0,'*,' F  ~
        LARGEST LOSS=  ~7D   LARGEST GAIN= ~7D~%~
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
      (my-round (/ trade-time (length trades)) 1)
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


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun

;;;time frames is a list of time-frames
;;;markets-ll is a list of lists of markets
;;;example '(*day-list* *swing-list* *position-list*)
;;;each list corresponds to the time frame
;;;this allow you to specify a different market list for each time frame

(defun portfolio-simulation (time-frames date num markets-ll)

   (do* ((mkts markets-ll (cdr mkts))
         (tmf time-frames (cdr tmf))
         (time-frame (car tmf) (car tmf))
         (markets (car mkts) (car mkts)))
       ((null tmf))

       (dolist (ith markets)
         (case time-frame
            (position (position-simulation-test1 ith date num))
            (swing (swing-simulation-test1 ith date num))
            (day (daytrade-simulation-test1 ith date num))
     )))


   (apply #'diary-composite time-frames markets-ll)
)




;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun contraswing-simulation-test1
 (market date2 num &optional (stocks nil)(extent 4) (multiplier 1.66)(dur 3) (features *swing-features1*))
 (let (date stop-long stop-short trades long short  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade twr-short twr-long bin
       risk risk-long risk-short  epsignal longs long-gains long-acc trading-dates (trade-time 0)
       shorts short-gains short-acc  date-1 prev-signal ctr record (running-sum 0)
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "contraswing-summary1.dat"))
        rollover
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "contraswing-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "contraswing-diary1.dat")))

      (declare (ignore  epsignal short-acc long-acc ctr))
    (if stocks (setq *stocks* T))
   (apply #'swing-trade-bins1 features)
   (set-market market)

   (setq date (add-mkt-days date2 (- num)))
   (setq record (list date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-long entry-short)  (vprices1 date extent multiplier dur))
    (multiple-value-setq (prev-signal ctr)(vsignals1 date extent multiplier dur))

   (setq entry-short (max entry-short (getd date 'close)) entry-long (min entry-long (getd date 'close)))

   (setq risk-short  (my-pretty-price (* (/ 1 *stop-loss-swing*) (abs (- entry-short (n-day-low date dur 'close))))))

   (setq risk-long  (my-pretty-price (* (/ 1 *stop-loss-swing*) (abs (- entry-long  (n-day-high date dur 'close))))) )

   (setq risk (max risk-long risk-short))

   (setq date-1 date date (add-mkt-days date 1))

   (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1))

;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (nth 3 record)(- (getd date-1 'close)(getd date 'close))))
   (setq rollover (my-pretty-price (getd date 'rollover)))

    (when (and rollover long)
          (setq long (+ long rollover)
                entry-long (+ entry-long rollover)
                cover-long (+ cover-long rollover)
                stop-long (+ stop-long rollover))
          (setf (nth 3 record)(- (nth 3 record) rollover)))

    (when (and rollover short)
          (setq short (+ short rollover)
    	        entry-short (+ entry-short rollover)
                cover-short (+ cover-short rollover)
                stop-short (+ stop-short rollover))
          (setf (nth 3 record) (+ (nth 3 record) rollover)))


   (when long (setq stop-long (my-pretty-price (fmax stop-long (- entry-long risk-long)))
                    cover-long (my-pretty-price (fmin cover-long entry-short))))

   (when short (setq stop-short (my-pretty-price (fmin stop-short (+ entry-short risk-short)))
                     cover-short (my-pretty-price (fmax cover-short entry-long))))


 ;;;;check if stopped out of prior position

   (when (and long (<= (getd date 'low) stop-long))
          (push (my-pretty-price (- (min stop-long (getd date 'open)) long)) trades)
          (setq trade-long
           (append trade-long
            (list date 'exit (my-pretty-price (min stop-long (getd date 'open)))
               (my-pretty-price (- (min stop-long (getd date 'open)) long)))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if rollover (setf (nth 3 record)(- (nth 3 record) rollover)))
        ;  (format T "111  ~A~%" record)
          (setq trade-long nil long nil stop-long nil cover-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (my-pretty-price (- short (max stop-short (getd date 'open)))) trades)
          (setq trade
           (append trade (list date 'exit (my-pretty-price (max stop-short (getd date 'open)))
                              (my-pretty-price (- short (max stop-short (getd date 'open)))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if rollover (setf (nth 3 record)(+ (nth 3 record) rollover)))
         ;  (format T "112  ~A~%" record)
          (setq trade nil short nil stop-short nil cover-short nil))

  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))

            (push (my-pretty-price (- (max (getd date 'open) cover-long) long)) trades)
            (setq trade-long
             (append trade-long (list date 'exit
              cover-long (my-pretty-price (- (max (getd date 'open) cover-long) long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (- (max (getd date 'open) cover-long) (getd date-1 'close)))
            (if rollover (setf (nth 3 record)(- (nth 3 record) rollover)))
          ;   (format T "113  ~A~%" record)
            (setq trade-long nil long nil stop-long nil cover-long nil))

      (when (and short (< (getd date 'low) cover-short))

            (push (my-pretty-price (- short (min (getd date 'open) cover-short))) trades)
            (setq trade (append trade (list date 'exit
             cover-short (my-pretty-price (- short (min (getd date 'open) cover-short))))))
            (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (- (getd date-1 'close) (min (getd date 'open) cover-short)))
            (if rollover (setf (nth 3 record)(+ (nth 3 record) rollover)))
           ;  (format T "114  ~A~%" record)
            (setq trade nil short nil stop-short nil cover-short nil))

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(> (getd date 'high) entry-short)
                (<= (* risk (index-point-value)) *max-swing-risk*))
           (and (not long) (< (getd date 'low) entry-long)
                (<= (* risk (index-point-value)) *max-swing-risk*)))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-swings date-1 features))
        (setq twr-short (swing-bin-twr bin))(setf (nth 0 bin) 1)
        (setq twr-long (swing-bin-twr bin))
         )


;;;check if new entry

    (when (and (not short)
               (> (getd date 'high) entry-short)
               (eql prev-signal 'SELL)
               (> longs 0)
               (<= (/ long-gains longs) -200) (< twr-long 1)
               (<= (* risk (index-point-value)) *max-swing-risk*)
               )
          (setq short (my-pretty-price (max entry-short (getd date 'open)))
                 trade (list date 'short short)
                 stop-short  (my-pretty-price (+ entry-short risk-short))
                cover-short entry-long)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                               (- short (getd date 'close)(/ *swing-commission* (index-point-value)))))
         )


    (when (and (not long)
               (< (getd date 'low) entry-long)
               (eql prev-signal 'BUY)
               (> shorts 0)
               (<= (/ short-gains shorts) -200) (< twr-short 1)
               (<= (* risk (index-point-value)) *max-swing-risk*)
               )
           (setq long (my-pretty-price (min entry-long (getd date 'open)))
                 trade-long (list date 'long long)
                 stop-long
                   (my-pretty-price (- entry-long risk-long))
                cover-long entry-short)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- (getd date 'close) long (/ *swing-commission* (index-point-value)))))
              )
      ;  (format T "115  ~A~%" record)

 ;;;check if stopped out on same day of entry
   (when (and long (<= (getd date 'low) stop-long))
           (push (my-pretty-price (- stop-long long)) trades)
           (setq trade-long
             (append trade-long (list date 'exit stop-long (my-pretty-price (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)));;;remove to avoid double counting
          ;  (format T "116  ~A~%" record)
           (setq trade-long nil long nil stop-long nil cover-long nil))

    (when  (and short (>= (getd date 'high) stop-short))
           (push (my-pretty-price (- short stop-short)) trades)
           (setq trade
           (append trade (list date 'exit stop-short (my-pretty-price (- short stop-short)))))
           (push trade extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
          ;  (format T "117  ~A~%" record)
           (setq trade nil short nil stop-short nil cover-short nil))


 ;;;check if met objective on day of entry

     (cond ((and long (> (getd date 'high) cover-long))
            (push (my-pretty-price (- cover-long long)) trades)
            (setq trade-long
             (append trade-long (list date 'exit cover-long (my-pretty-price (- cover-long long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)));;;remove to avoid double counting
           ;  (format T "118  ~A~%" record)
            (setq trade-long nil long nil stop-long nil cover-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (my-pretty-price (- short cover-short)) trades)
            (setq trade
             (append trade (list date 'exit cover-short (my-pretty-price (- short cover-short)))))
            (push trade extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (+ (nth 3 record)(- short cover-short)))
            (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           ;  (format T "119  ~A~%" record)
            (setq trade nil short nil stop-short nil cover-short nil)))





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
   (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D ~%$/CONTRACT= ~D  AVE DAYS IN TRADE= ~F"
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
       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       (my-round (/ trade-time (length trades)) 1)
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;Contra-trend day trade system simulation

(defun contradaytrade-simulation-test1 (market date2 num
          &optional (stocks nil)(extent 4)(multiplier *entry-factor*) (features *day-features1*))
 (let (date trades long short cover-long cover-short entry-long entry-short  record trading-dates
       risk-short risk-long  date-1 epsignal longs long-gains shorts short-gains (running-sum 0)
       ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short twr-long twr-short bin
       ave3 risk long-acc short-acc (VT (make-hash-table))
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "contraday-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "contraday-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "contraday-diary1.dat")))
   (declare (ignore long-acc short-acc epsignal))


   (if stocks (setq *stocks* T)(setq *stocks* nil))
   (apply #'day-trade-bins1 features)
   (set-market market)

   (setq date (add-mkt-days date2 (- num)))


 ;;;;from date1 to date2
 (dotimes (ith num)


   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months

   (multiple-value-setq (entry-long entry-short)(vprices1 date extent multiplier 1))

   (setq ave3 (ave date extent 'pivot)
         cover-short (exp (- (log ave3) (* *objective-factor* (gethash date VT))))
         cover-long (exp (+ (log ave3) (* *objective-factor* (gethash date VT))))
         )
    (setq cover-short
      (max cover-short (- entry-short (* *stop-loss-day* (abs (- entry-short (getd date 'close)))))))
    (setq cover-long
      (min cover-long (+ entry-long (* *stop-loss-day* (abs (- entry-long (getd date 'close)))))))

   (setq risk-short  (* (/ 1 *stop-loss-day*) (abs (- entry-short (getd date 'close)))))
   (setq risk-long   (* (/ 1 *stop-loss-day*) (abs (- entry-long (getd date 'close)))))
   (setq risk (max risk-long risk-short))

   (setq  date-1 date date (add-mkt-days date 1))
    (setq record (list date (getd date 'close) 0 0 0))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))

    ))
   (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))
    	                             cover-short (+ cover-short (getd date 'rollover))

    ))

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(> (getd date 'high) entry-short)
                (<= (* risk (index-point-value)) *max-day-risk*))
           (and (not long) (< (getd date 'low) entry-long)
                (<= (* risk (index-point-value)) *max-day-risk*)))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades1 date-1 features))
       (setq twr-short (day-bin-twr1 bin))(setf (nth 0 bin) 1)
       (setq twr-long (day-bin-twr1 bin))
       )

;;;check if new entry

   (when (and (not short)
              (<= (* risk (index-point-value)) *max-day-risk*)
              (> (getd date 'high) entry-short)
              (> longs 0)
              (<= (/ long-gains longs) -200) (< twr-long 1)
                 )
          (setq short (max (getd date 'open) entry-short)
                short-trade (list date 'short short)
                stop-short  (+ short risk) )
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (nth 3 record) (/ *swing-commission* (index-point-value))))
                 )


    (when (and (not long)
               (<= (* risk (index-point-value)) *max-day-risk*)
               (< (getd date 'low) entry-long)
               (> shorts 0)
               (<= (/ short-gains shorts) -200) (< twr-short 1)
             )

           (setq long (min (getd date 'open) entry-long)
                 trade (list date 'long long)
                 stop-long (- long risk))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (- (nth 3 record) (/ *swing-commission* (index-point-value))))
                 )


 ;;;check if stopped out on same day

   (when (and long stop-long
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setq trade nil long nil stop-long nil
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


;;;check if met objective  before end of day   low before high
     (when (and long  (> (getd date 'high) cover-long)
                (<= (getd date 'open)(getd date 'close)))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setq trade nil long nil stop-long nil))

;;;high before low
      (when (and short (< (getd date 'low) cover-short)
                (>= (getd date 'open)(getd date 'close)))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short cover-short)))
           (setq short-trade nil short nil stop-short nil))


;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (getd date 'close))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- (getd date 'close) long)))
            (setq trade nil long nil stop-long nil))

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
  (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
   (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D   $/CONTRACT= ~D"
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
       (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
             )


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
        ));;;closes the with-open-file
  );;;closes the when outfile

  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ));


 ;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for swing trades to decide to trade or not
;;;;assumes (position-trade-bins date features) has already been run
(defun find-best-contraposition-trade1 (tdate &optional (output T)(output1 T) (date1 nil))
  (let* ((ctr 0)  entry-short entry-long  long short
         cover-long cover-short   directive1
        prev-signal action stop-long stop-short risk-short risk-long ;(time-frame 'POSITION)
         trade-direction
        )
    (declare (special longs shorts long-gains short-gains long-acc short-acc twr-long twr-short))
    (declare (ignore  ctr))

   ; (if (member *data-name* *forex-list*) (setq time-frame 'FX-SWING))

    (multiple-value-setq (entry-long entry-short)  (vprices1 tdate 18 2.53 7))

   (multiple-value-setq (prev-signal ctr) (vsignals1 tdate 18 2.53 7))

   (setq trade-direction (cond  ((and (eql prev-signal 'SELL)
                                     (> longs 0)
                                     (<= (/ long-gains longs) -200) (< twr-long 1)
                                      ) 'DN)
                               ((and (eql prev-signal 'BUY)
                                     (> shorts 0)
                                     (<= (/ short-gains shorts) -200) (< twr-short 1)
                                     ) 'UP)
                               (t 'FT)))


   (setq entry-short (max entry-short (getd tdate 'close)) entry-long (min entry-long (getd tdate 'close)))

   (setq risk-short  (* (/ 1 *stop-loss-position*) (abs (- entry-short (n-day-low tdate 7 'close)))))
   (setq risk-long  (* (/ 1 *stop-loss-position*) (abs (- entry-long  (n-day-high tdate 7 'close)))))



  ;;;;sets long and short to be true if open position coming into today
    (setq long (eql (cadr (assoc *data-name* *open-contrapositions*)) 'long))
    (setq short (eql (cadr (assoc *data-name* *open-contrapositions*)) 'short))
    (if long (setq stop-long (nth 2 (assoc *data-name* *open-contrapositions*))))
    (if short (setq stop-short (nth 2 (assoc *data-name* *open-contrapositions*))))

   (if long (setq cover-long (nth 3 (assoc *data-name* *open-contrapositions*)))
         (setq cover-long entry-short))
   (if short (setq cover-short (nth 3 (assoc *data-name* *open-contrapositions*)))
         (setq cover-short entry-long))


   (when (and (getd tdate 'rollover) long) (setq
            					 entry-long (+ entry-long (getd tdate 'rollover))
    						 cover-long (+ cover-long (getd tdate 'rollover))
    						 stop-long (+ stop-long (getd tdate 'rollover))))
   (when (and (getd tdate 'rollover) short)(setq
    						 entry-short (+ entry-short (getd tdate 'rollover))
    						 cover-short (+ cover-short (getd tdate 'rollover))
    						 stop-short (+ stop-short (getd tdate 'rollover))))


    (setq stop-long (fmax stop-long (- entry-long risk-long)))

    (setq stop-short (fmin stop-short (+ entry-short risk-short)))


         (when
          (if (member *data-name* '(US.D1B TY.D1B))
              (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                               (convert-to-decimal (convert-to-32 stop-long)));;;closes the -
                           (index-point-value)));;;closes the round
                  *max-position-risk* );;;closes the >
              (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))));;closes the *
                               (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))));;closes the -
                           (index-point-value)));;;closes the round
                  *max-position-risk*));;closes the if
                   (push "NOT TODAY" action));;;closes the when

             (if (eql trade-direction 'up) (push "NOT SHORT" action))
             (if (eql trade-direction 'dn) (push "NOT LONG" action))
             (if (eql trade-direction 'FT) (push "NOT TODAY" action))

          ;   (if (< (/ (- cover-long entry-long) risk-long) *reward-risk-ratio-swing*) (push "NOT LONG" action))
          ;   (if (< (/ (- entry-short cover-short) risk-short) *reward-risk-ratio-swing*) (push "NOT SHORT" action))


           (when (eql (cadr (assoc *data-name* *open-contrapositions*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))

           (when (eql (cadr (assoc *data-name* *open-contrapositions*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))


         ;  (when (eql (cadr (assoc *data-name* *forex-open-contrapositions*)) 'long)
         ;        (push "NOT LONG" action)(push "OPEN LONG" action))

         ;  (when (eql (cadr (assoc *data-name* *forex-open-contrapositions*)) 'short)
         ;        (push "NOT SHORT" action)(push "OPEN SHORT" action))

         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))


      (format output "Contra Position   ~A  ~%" action)

      (format output " Prev Signal= ~A  Direction = ~3A ~%"
                     prev-signal  trade-direction )

      (format output " Num Longs = ~D P/L for Longs = ~4,2,,,F Accuracy for Longs = ~A~%" longs long-gains long-acc)
      (format output " Num Shorts = ~D P/L for shorts = ~4,2,,,F Accuracy for Short = ~A~%" shorts short-gains short-acc)

      (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))

      (if (member *data-name* '(US.D1B TY.D1B))
      (format output "~%SELL= ~7@A  INIT-BUY-STOP= ~7@A COVER-SHORT= ~7@A RISK= ~D~%BUY= ~7@A  INIT-SELL-STOP= ~7@A  COVER-LONG= ~7@A RISK= ~D~%"
          (convert-to-32nds entry-short)(convert-to-32nds stop-short)(convert-to-32nds cover-short)
          (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
                        (convert-to-decimal (convert-to-32 entry-short))
                       ) (index-point-value)))
          (convert-to-32nds entry-long) (convert-to-32nds stop-long)(convert-to-32nds cover-long)
          (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                        (convert-to-decimal (convert-to-32  stop-long))
                       ) (index-point-value))));;;closes the format

      (format output
        (string-append "~%SELL= " directive1 "  INIT-BUY-STOP= " directive1 " COVER-SHORT= " directive1 " RISK= ~D~% BUY= "
        directive1 " INIT-SELL-STOP= " directive1 "  COVER-LONG= " directive1 " RISK= ~D~%")
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-short (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
                             )  (index-point-value)))
            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-long (nth 4 (assoc *data-name* *C-list*))))
            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
                          (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long
                                                                         (nth 4 (assoc *data-name* *C-list*))))
                       )  (index-point-value)))
            ));;;closes the format and the if



    ;  (cond ((equal action "OK      ")
    ;         (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
    ;         (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
    ;             )
    ;
    ;         ((equal action "NOT SHORT")
    ;          (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
    ;            )
    ;
    ;         ((equal action "NOT LONG")
    ;          (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
    ;           ))

       (cond ((eql (cadr (assoc *data-name* *open-contrapositions*)) 'long)
              (write-xml-record tdate "tblTradeRecs" "OPEN CONTRA POSITIONS" 'LONG date1 nil stop-long cover-long output1)
                  )
             ((eql (cadr (assoc *data-name* *open-contrapositions*)) 'short)
              (write-xml-record tdate "tblTradeRecs" "OPEN CONTRA POSITIONS" 'SHORT date1 nil stop-short cover-short output1)
                  )

           ;  ((eql (cadr (assoc *data-name* *forex-open-contraswings*)) 'long)
           ;   (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'LONG date1 nil stop-long cover-long output1)
           ;       )
           ;  ((eql (cadr (assoc *data-name* *forex-open-contraswings*)) 'short)
           ;   (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'SHORT date1 nil stop-short cover-short output1)
           ;        )
                   );;;;closes the cond



      ));;;closes the let and the defun


;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun contraposition-simulation-test1 (market date2 num
                       &optional (stocks nil)(extent 18) (multiplier 2.53)(dur 7) (features *position-features1*))
 (let (date stop-long stop-short trades long short  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade twr-long twr-short bin
       risk risk-long risk-short  epsignal longs long-gains long-acc trading-dates (trade-time 0)
       shorts short-gains short-acc  date-1 prev-signal ctr record (running-sum 0)
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "contraposition-summary1.dat"))
        rollover
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "contraposition-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "contraposition-diary1.dat")))

      (declare (ignore  epsignal short-acc long-acc ctr))
    (if stocks (Setq *stocks* T))
   (apply #'position-trade-bins1 features)
   (set-market market)

   (setq date (add-mkt-days date2 (- num)))
   (setq record (list date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (entry-long entry-short) (vprices1 date extent multiplier dur))
   (multiple-value-setq (prev-signal ctr)(vsignals1 date extent multiplier dur))

   (setq entry-short (max entry-short (getd date 'close)) entry-long (min entry-long (getd date 'close)))

   (setq risk-short  (my-pretty-price (* (/ 1 *stop-loss-position*) (abs (- entry-short (n-day-low date dur 'close))))))

   (setq risk-long  (my-pretty-price (* (/ 1 *stop-loss-position*) (abs (- entry-long  (n-day-high date dur 'close))))) )

   (setq risk (max risk-long risk-short))

   (setq date-1 date date (add-mkt-days date 1))

   (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1))

;;;if long or short and not entry or exits
      (if long (setf (nth 3 record) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (nth 3 record)(- (getd date-1 'close)(getd date 'close))))
   (setq rollover (my-pretty-price (getd date 'rollover)))

    (when (and rollover long)
          (setq long (+ long rollover)
                entry-long (+ entry-long rollover)
                cover-long (+ cover-long rollover)
                stop-long (+ stop-long rollover))
          (setf (nth 3 record)(- (nth 3 record) rollover)))

    (when (and rollover short)
          (setq short (+ short rollover)
    	        entry-short (+ entry-short rollover)
                cover-short (+ cover-short rollover)
                stop-short (+ stop-short rollover))
          (setf (nth 3 record) (+ (nth 3 record) rollover)))


   (when long (setq stop-long (my-pretty-price (fmax stop-long (- entry-long risk-long)))
                    cover-long (my-pretty-price (fmin cover-long entry-short))))

   (when short (setq stop-short (my-pretty-price (fmin stop-short (+ entry-short risk-short)))
                     cover-short (my-pretty-price (fmax cover-short entry-long))))


 ;;;;check if stopped out of prior position

   (when (and long (<= (getd date 'low) stop-long))
          (push (my-pretty-price (- (min stop-long (getd date 'open)) long)) trades)
          (setq trade-long
           (append trade-long
            (list date 'exit (my-pretty-price (min stop-long (getd date 'open)))
               (my-pretty-price (- (min stop-long (getd date 'open)) long)))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if rollover (setf (nth 3 record)(- (nth 3 record) rollover)))
        ;  (format T "111  ~A~%" record)
          (setq trade-long nil long nil stop-long nil cover-long nil))

   (when (and short (>= (getd date 'high) stop-short))
          (push (my-pretty-price (- short (max stop-short (getd date 'open)))) trades)
          (setq trade
           (append trade (list date 'exit (my-pretty-price (max stop-short (getd date 'open)))
                              (my-pretty-price (- short (max stop-short (getd date 'open)))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if rollover (setf (nth 3 record)(+ (nth 3 record) rollover)))
         ;  (format T "112  ~A~%" record)
          (setq trade nil short nil stop-short nil cover-short nil))

  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))

            (push (my-pretty-price (- (max (getd date 'open) cover-long) long)) trades)
            (setq trade-long
             (append trade-long (list date 'exit
              cover-long (my-pretty-price (- (max (getd date 'open) cover-long) long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (- (max (getd date 'open) cover-long) (getd date-1 'close)))
            (if rollover (setf (nth 3 record)(- (nth 3 record) rollover)))
          ;   (format T "113  ~A~%" record)
            (setq trade-long nil long nil stop-long nil cover-long nil))

      (when (and short (< (getd date 'low) cover-short))

            (push (my-pretty-price (- short (min (getd date 'open) cover-short))) trades)
            (setq trade (append trade (list date 'exit
             cover-short (my-pretty-price (- short (min (getd date 'open) cover-short))))))
            (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (- (getd date-1 'close) (min (getd date 'open) cover-short)))
            (if rollover (setf (nth 3 record)(+ (nth 3 record) rollover)))
           ;  (format T "114  ~A~%" record)
            (setq trade nil short nil stop-short nil cover-short nil))

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(> (getd date 'high) entry-short)
                (<= (* risk (index-point-value)) *max-swing-risk*))
           (and (not long) (< (getd date 'low) entry-long)
                (<= (* risk (index-point-value)) *max-swing-risk*)))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-positions1 date-1 features))
        (setq twr-short (position-bin-twr1 bin))(setf (nth 0 bin) 1)
        (setq twr-long (position-bin-twr1 bin))
         )


;;;check if new entry

    (when (and (not short)
               (> (getd date 'high) entry-short)
               (eql prev-signal 'SELL)
               (> longs 0)
               (<= (/ long-gains longs) -200)  (< twr-long 1)
               (<= (* risk (index-point-value)) *max-position-risk*)
               )
          (setq short (my-pretty-price (max entry-short (getd date 'open)))
                 trade (list date 'short short)
                 stop-short  (my-pretty-price (+ entry-short risk-short))
                cover-short entry-long)
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (+ (nth 3 record)
                               (- short (getd date 'close)(/ *swing-commission* (index-point-value)))))
         )


    (when (and (not long)
               (< (getd date 'low) entry-long)
               (eql prev-signal 'BUY)
               (> shorts 0)
               (<= (/ short-gains shorts) -200) (< twr-short 1)
               (<= (* risk (index-point-value)) *max-position-risk*)
               )
           (setq long (my-pretty-price (min entry-long (getd date 'open)))
                 trade-long (list date 'long long)
                 stop-long
                   (my-pretty-price (- entry-long risk-long))
                cover-long entry-short)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- (getd date 'close) long (/ *swing-commission* (index-point-value)))))
              )
      ;  (format T "115  ~A~%" record)

 ;;;check if stopped out on same day of entry
   (when (and long (<= (getd date 'low) stop-long))
           (push (my-pretty-price (- stop-long long)) trades)
           (setq trade-long
             (append trade-long (list date 'exit stop-long (my-pretty-price (- stop-long long)))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)));;;remove to avoid double counting
          ;  (format T "116  ~A~%" record)
           (setq trade-long nil long nil stop-long nil cover-long nil))

    (when  (and short (>= (getd date 'high) stop-short))
           (push (my-pretty-price (- short stop-short)) trades)
           (setq trade
           (append trade (list date 'exit stop-short (my-pretty-price (- short stop-short)))))
           (push trade extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
          ;  (format T "117  ~A~%" record)
           (setq trade nil short nil stop-short nil cover-short nil))


 ;;;check if met objective on day of entry

     (cond ((and long (> (getd date 'high) cover-long))
            (push (my-pretty-price (- cover-long long)) trades)
            (setq trade-long
             (append trade-long (list date 'exit cover-long (my-pretty-price (- cover-long long)))))
            (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)));;;remove to avoid double counting
           ;  (format T "118  ~A~%" record)
            (setq trade-long nil long nil stop-long nil cover-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (my-pretty-price (- short cover-short)) trades)
            (setq trade
             (append trade (list date 'exit cover-short (my-pretty-price (- short cover-short)))))
            (push trade extended-trades)(setq trade-time (+ trade-time 1))
            (setf (nth 2 record) -1)
            (setf (nth 3 record) (+ (nth 3 record)(- short cover-short)))
            (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           ;  (format T "119  ~A~%" record)
            (setq trade nil short nil stop-short nil cover-short nil)))


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
   (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D ~%$/CONTRACT= ~D  AVE DAYS IN TRADE= ~F"
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
       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       (my-round (/ trade-time (length trades)) 1)
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun


;;;this is for the meats day trades
(defun currencies-bins1 (&rest features)
  (let (bin path)
  (setq path (setq path (string-append *upper-dir* "currencywarehouse1.dat")))
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
(defun currencies-add-one-in1 (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'currencies-bins1 base-features)

  (dolist (ith candidate-features)
    (setq result (apply #'currencies-bins1 (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))

;;;requires a base features list
(defun currencies-leave-one-out1 (base-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq *stocks* T)(setq *stocks* nil))
  (apply #'currencies-bins1 base-features)

  (dolist (ith base-features)
    (setq result (apply #'currencies-bins1 (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 ))

;;;reads the meatsdaywarehouse1.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-currencies-trades1 (new-trades-path)
  (let ((path-out (string-append *upper-dir* "currencywarehouse1.dat")) ewaves-trades)

       (if (probe-file path-out)
        (with-open-file (str path-out :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (push record ewaves-trades))
          ))

     (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (pushnew record ewaves-trades :test #'equal)
          ))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith ewaves-trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun


(defun remove-currencies-market1 (market)
  (let (trades path)
  (setq path (setq path (string-append *upper-dir* "currencywarehouse1.dat")))
  (maind-x)(set-cat-list)

    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))

   (setq trades (remove-if #'(lambda (s) (eql (car s) market)) trades))
   (with-open-file (str path :direction :output :if-exists :supersede)
       (dolist (ith trades)
        (format str "~S~%" ith)))
 ))


;;;this is for the meats day trades
(defun meats-day-bins1 (&rest features)
  (let (bin path)
  (setq path (setq path (string-append *upper-dir* "meatsdaywarehouse1.dat")))
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
(defun meats-add-one-in1 (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'meats-day-bins1 base-features)

  (dolist (ith candidate-features)
    (setq result (apply #'meats-day-bins1 (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))

;;;requires a base features list
(defun meats-leave-one-out1 (base-features &optional (stocks nil))
  (let (winners-list (result 0))
  (if stocks (setq *stocks* T)(setq *stocks* nil))
  (apply #'meats-day-bins1 base-features)

  (dolist (ith base-features)
    (setq result (apply #'meats-day-bins1 (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 ))



(defun remove-meats-day-market1 (market)
  (let (trades path)
  (setq path (setq path (string-append *upper-dir* "meatsdaywarehouse1.dat")))
  (maind-x)(set-cat-list)

    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))

   (setq trades (remove-if #'(lambda (s) (eql (car s) market)) trades))
   (with-open-file (str path :direction :output :if-exists :supersede)
       (dolist (ith trades)
        (format str "~S~%" ith)))
 ))


;;;reads the meatsdaywarehouse1.dat
;;;reads and adds the new trades file
;;;only adds the new trades if it is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-meats-day-trades1 (new-trades-path)
  (let ((path-out (string-append *upper-dir* "meatsdaywarehouse1.dat")) ewaves-trades)

       (if (probe-file path-out)
        (with-open-file (str path-out :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (push record ewaves-trades))
          ))

     (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
          (pushnew record ewaves-trades :test #'equal)
          ))

    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith ewaves-trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun

(defun contrameattrade-simulation-test1 (market date2 num
          &optional (extent 4)(multiplier *entry-factor*) (features *meats-features1*))
 (let (date trades long short cover-long cover-short entry-long entry-short  record trading-dates
       risk-short risk-long  date-1 epsignal longs long-gains shorts short-gains (running-sum 0) twr-long twr-short bin
       ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short
       ave3 risk long-acc short-acc (VT (make-hash-table))
       (outfile (string-append *output-upper-dir* (format nil "~S" market) "contrameat-summary1.dat"))
       (path1 (string-append *output-upper-dir* (format nil "~S" market) "contrameat-simulation1.dat"))
       (path2 (string-append *output-upper-dir* (format nil "~S" market) "contrameat-diary1.dat")))
   (declare (ignore long-acc short-acc epsignal))


   (apply #'meats-day-bins1 features)
   (set-market market)

   (setq date (add-mkt-days date2 (- num)))


 ;;;;from date1 to date2
 (dotimes (ith num)


   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months

   (multiple-value-setq (entry-long entry-short)(vprices1 date extent multiplier 1))

   (setq ave3 (ave date extent 'pivot)
         cover-short (exp (- (log ave3) (* *objective-factor* (gethash date VT))))
         cover-long (exp (+ (log ave3) (* *objective-factor* (gethash date VT))))
         )
    (setq cover-short
      (max cover-short (- entry-short (* *stop-loss-day* (abs (- entry-short (getd date 'close)))))))
    (setq cover-long
      (min cover-long (+ entry-long (* *stop-loss-day* (abs (- entry-long (getd date 'close)))))))

   (setq risk-short  (* (/ 1 *stop-loss-day*) (abs (- entry-short (getd date 'close)))))
   (setq risk-long   (* (/ 1 *stop-loss-day*) (abs (- entry-long (getd date 'close)))))
   (setq risk (max risk-long risk-short))

   (setq  date-1 date date (add-mkt-days date 1))
    (setq record (list date (getd date 'close) 0 0 0))

   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))

    ))
   (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))
    	                             cover-short (+ cover-short (getd date 'rollover))

    ))

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(> (getd date 'high) entry-short)
                (<= (* risk (index-point-value)) *max-day-risk*))
           (and (not long) (< (getd date 'low) entry-long)
                (<= (* risk (index-point-value)) *max-day-risk*)))
       (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                   (bin-classifier-daytrades1 date-1 features))
        (setq twr-short (day-bin-twr1 bin))(setf (nth 0 bin) 1)
        (setq twr-long (day-bin-twr1 bin))
       )

;;;check if new entry

   (when (and (not short)
              (<= (* risk (index-point-value)) *max-day-risk*)
              (> (getd date 'high) entry-short)
              (> longs 0)
              (<= (/ long-gains longs) -200) (< twr-long 1)
                 )
          (setq short (max (getd date 'open) entry-short)
                short-trade (list date 'short short)
                stop-short  (+ short risk) )
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (nth 3 record) (/ *swing-commission* (index-point-value))))
                 )


    (when (and (not long)
               (<= (* risk (index-point-value)) *max-day-risk*)
               (< (getd date 'low) entry-long)
               (> shorts 0)
               (<= (/ short-gains shorts) -200)  (< twr-short 1)
             )

           (setq long (min (getd date 'open) entry-long)
                 trade (list date 'long long)
                 stop-long (- long risk))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (- (nth 3 record) (/ *swing-commission* (index-point-value))))
                 )


 ;;;check if stopped out on same day

   (when (and long stop-long
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
           (setq trade nil long nil stop-long nil
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


;;;check if met objective  before end of day   low before high
     (when (and long  (> (getd date 'high) cover-long)
                (<= (getd date 'open)(getd date 'close)))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- cover-long long)))
            (setq trade nil long nil stop-long nil))

;;;high before low
      (when (and short (< (getd date 'low) cover-short)
                (>= (getd date 'open)(getd date 'close)))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record)(- short cover-short)))
           (setq short-trade nil short nil stop-short nil))


;;;check if met exit criteria on day of entry (exit at end of day)
     (when long
            (setq cover-long (getd date 'close))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setf (nth 2 record) 1)
            (setf (nth 3 record) (+ (nth 3 record)(- (getd date 'close) long)))
            (setq trade nil long nil stop-long nil))

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
  (setq trades (mapcar #'(lambda (s) (- s (/ *swing-commission* (index-point-value)) (* 0 (index-tick-size)))) trades))

  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)
   (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D   $/CONTRACT= ~D"
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
       (if (plusp (list-sum trades))(round (* (optimal-f trades)(or (index-point-value) 1))) 0)
             )


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)

    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
        ));;;closes the with-open-file
  );;;closes the when outfile

  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ));


(defun decode-swing-bin1 (bin action output)
 (unless (equal action "NOT TODAY")

   (format output "~%Osillator = ~A~%" (decode-stochastic (second bin)))
   (format output "Very Short Term Trend = ~A~%"
           (decode-trend-signal1-3 (third bin)))
   (format output "Long Term Trend = ~A~%" (decode-long-term (fourth bin)))

   (format output "Volatility = ~A~%" (decode-volatility (fifth bin)))
   (format output "Pattern Of Current Price Bar = ~A~%"
           (decode-bar-type (sixth bin)))


 ))

(defun decode-stochastic (code)
   (case code
     (3    "VERY OVERSOLD" )
     (2    "OVERSOLD" )
     (1    "SLIGHTLY BELOW NORMAL" )
     (-1   "SLIGHTLY ABOVE NORMAL" )
     (-2   "OVERBOUGHT" )
     (-3   "VERY OVERBOUGHT" )
     ))

(defun decode-trend-signal1-3 (code)
   (case code
     (4 "UP WITH STRONG MOMENTUM")
     (3 "UP WITH NORMAL MOMENTUM")
     (2 "UP")
     (1 "UP WITH WEAK MOMENTUM")
     (-1 "DOWN WITH WEAK MOMENTUM")
     (-2 "DOWN")
     (-3 "DOWN WITH NORMAL MOMENTUM")
     (-4 "DOWN WITH STRONG MOMENTUM")
     (0 "SIDEWAYS")
        ))

(defun decode-long-term (code)
   (case code
     (DN  "FADING UPWARD")
     (CD  "CONTINUING DOWN")
     (UP  "FADING DOWNWARD")
     (CU  "CONTINUING UP")
     (FT  "SIDEWAYS")
    ))

(defun decode-volatility (code)
   (case code
     (5   "EXTREMELY LOW")
     (4   "VERY LOW")
     (3   "LOW")
     (2   "SLIGHTLY LOW")
     (1   "JUST BELOW NORMAL")
     (-1  "JUST ABOVE NORMAL")
     (-2  "SLIGHTLY HIGH")
     (-3  "HIGH")
     (-4  "VERY HIGH")
     (-5  "EXTREMELY HIGH")
  ))
(defun decode-bar-type (code)
   (case code
     (11 "OPENS IN LOWER THIRD; CLOSES IN LOWER THIRD")
     (12 "OPENS IN LOWER THIRD; CLOSES IN MIDDLE THIRD")
     (13 "OPENS IN LOWER THIRD; CLOSES IN UPPER THIRD")
     (21 "OPENS IN MIDDLE THIRD; CLOSES IN LOWER THIRD")
     (22 "OPENS IN MIDDLE THIRD; CLOSES IN MIDDLE THIRD")
     (23 "OPENS IN MIDDLE THIRD; CLOSES IN UPPER THIRD")
     (31 "OPENS IN UPPER THIRD; CLOSES IN LOWER THIRD")
     (32 "OPENS IN UPPER THIRD; CLOSES IN MIDDLE THIRD")
     (33 "OPENS IN UPPER THIRD; CLOSES IN UPPER THIRD")
     ))





(defun write-txt-record (direction entry-price stop-price objective-price output3)
 (let (directive)

   (setq directive
        (case (index-digits)
         (1 "~8,1F ")
         (2 "~8,2F ")
         (3 "~8,3F ")
         (4 "~8,4F ")))

   (format output3 "~19A, ~7A, ~11A, "
        (nth 3 (assoc *data-name* *C-list*))
        (third (assoc *data-name* *C-list*))
        direction
      )

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
        (format output3 "~8@A ," (convert-to-32nds entry-price))
         (if (and (nth 1 (assoc *data-name* *C-list*))
                 (zerop (nth 1 (assoc *data-name* *C-list*))))
             (format output3 "~8@A ," (round entry-price))
           (format output3 (string-append directive ",")
             (* (nth 4 (assoc *data-name* *C-list*))
               (round entry-price (nth 4 (assoc *data-name* *C-list*)))));;;closes the format
           ))
      (format output3 "~8@A ," "   "))

      (if stop-price
             (if (member *data-name* '(US.D1B TY.D1B))
           (format output3 "~8@A ," (convert-to-32nds stop-price))
         (if (and (nth 1 (assoc *data-name* *C-list*))
                  (zerop (nth 1 (assoc *data-name* *C-list*))))
               (format output3 "~8@A ,"  (round stop-price))
             (format output3 (string-append directive ",")
              (* (nth 4 (assoc *data-name* *C-list*))
                   (round stop-price (nth 4 (assoc *data-name* *C-list*)))));;;closes the format
                   )))

     (if (not objective-price) (format output3 "~%"))
      (if objective-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output3 "~8@A~%" (convert-to-32nds objective-price))
         (if (and (nth 1 (assoc *data-name* *C-list*))
                  (zerop (nth 1 (assoc *data-name* *C-list*))))
              (format output3 "~8@A~%" (round objective-price))
            (format output3 (string-append directive "~%")
             (* (nth 4 (assoc *data-name* *C-list*))
                (round objective-price (nth 4 (assoc *data-name* *C-list*)))));;;closes the format
                ))
                )


 ))


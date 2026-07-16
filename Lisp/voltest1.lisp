;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

(defparameter *swing-trade-warehouse* (make-hash-table :test #'equal))
(defparameter *day-trade-warehouse* (make-hash-table :test #'equal))
(defparameter *position-trade-warehouse* (make-hash-table :test #'equal))

 
;;;enters with stop orders and exits with stop loss orders always in the market
;;;Plain vanilla volatility breakout with 15 1.618 and 3 parameter values.
;;;;For SWING TRADING
;;;Designed to write out the file for the Swing Trades Warehouse.  

;;;;the file has one record per trade each record is a list. The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1 
;;;;TI  is the timing index it is a natural number
;;;;SST is the stocastic with parameter 21
;;;;T5 is the trend-signal with parameter 5
;;;;T15 is the trend-signal with parameter 15
;;;T45 is the trend-signal with parameter 45
;;;RSD is the rsi14 less the rsi14 two days prior. (2 day rate of change in the rsi with paramter 14
;;Dev-obj is the deviation of closes from the 4 day moving average using the .875 percentile
;;;the value is sell buy or flat
;;;Mom-div is the momentum divergence using the MACD with parameters 5 13 5. The value is up dn or ft.
;;;Formation is one of sixteen bullish and bearish patterns. example DB1 thru DB8 and DS1 thru DS8. 
;;; (*data-name* entry-date direction entry-price TI SST T5 T15 T45 RSD dev-obj mom-div Formation)

(defun populate-swing-trades (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long  entry-short entry-long
       ave-win ave-loss losers winners extended-trades trade         date-1  
       (path1 (string-append "~/cycles/" (format nil "~S" market) "swingtrades.dat")))
   (maind-x)(set-cat-list)   
   (set-market market)
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))
 
;   (if  (probe-file path1)
;        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
       
   (multiple-value-setq (entry-short entry-long) (vprices date 15 1.618 3))      
 
   (setq  date-1 date date (add-mkt-days date 1))
         
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))
    
    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))
    ))
    
  ;;;;check if stopped out of prior position   
   
   (when long (setq stop-long (max stop-long entry-short)))
   (when short (setq stop-short (min stop-short entry-long))) 
          

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
                      

           
;;;check if new entry          
  
    (when (and (not short)              
               (<= (getd date 'low) entry-short)
               
               )
          (setq short (min entry-short (getd date 'open))                    
                 trade (create-swing-entry-record-list date-1 -1 short)
                 stop-short 
                 entry-long 
         ))
 
                   
    (when (and (not long)               
               (>= (getd date 'high) entry-long)
               
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (create-swing-entry-record-list date-1 1 long)
                
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
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
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
(defun create-swing-entry-record-list (date direction entry)
   (let* ((date-1 (getd date 'ydate))(date-2 (getd date-1 'ydate)))
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
                              (timing-index date)
                              (timing-index date-1)(timing-index date-2) 
                              (round (slow-stochastic date 21))(round (slow-stochastic date-1 21))
                              (trend-signal date 5)(trend-signal date 15)(trend-signal date 45)
                              (round (rsi-ave-diff date 14 2))(round (rsi-ave-diff date-1 14 2))
                              (dev-distribution-close date 4 .875 'close)
                              (momentum-divergence1 date 5 13)
                              (or (car (formation date)) 'FT));;;closes the list
                             
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;Now we need first to read the swing-trades file(s) and load into the *swing-trade-warehouse* hash table

(defvar swings nil)
(defvar bin-codes nil)

(defun swing-trade-bins (&rest features)
  (let (bin path)
  (setq path (setq path "~/cycles/tradewarehouse.dat"))
  (maind-x)(set-cat-list)
  (setq swings nil bin-codes nil)(clrhash *swing-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record swings)))
       
       
 (if *out-of-sample*
     (setq swings (remove-if #'(lambda (s) (eql (car s) *data-name*)) swings)))      
     
;;;;now all the trades are in the list swings.  
;;;we now assign/sort the trades into bins   
;;;basically process a record list into a code or bin
;;;and create a code
   
   (dolist (record swings)
     (setq bin (encode-swing-trades record features))
     (pushnew bin bin-codes :test #'equal)
     (cond ((gethash bin *swing-trade-warehouse*)
            (ifn (member record (gethash bin *swing-trade-warehouse*) :test #'equal)
                 (setf (gethash bin *swing-trade-warehouse*)
                       (cons record (gethash bin *swing-trade-warehouse*)))))
           ((not (gethash bin *swing-trade-warehouse*))
             (setf (gethash bin *swing-trade-warehouse*)
              (list record)))))
  
    (format T  "~%FEATURES = ~A~%" features)
    (rank-bins-by-profit swings) 
     
         
  ))
 

(defun remove-swing-trade-market (market)
  (let (trades path)
  (setq path (setq path "~/cycles/tradewarehouse.dat"))
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
       
 
;;;returns a list of nine codes 
(defun encode-swing-trades (record features) 
  (let (bin-list) 
  
    (dolist (ith features)
     (case ith
 ;;;;Feature 1 with 4 levels ;items 4, 5 , and 6 are the TI index. TI TI-1 TI-2
     (1  (cond ((>= (nth 4 record) 4)(push 1 bin-list))
               ((>= (nth 5 record) 4)(push 1 bin-list))
               ((>= (nth 6 record) 4)(push 1 bin-list))
               (t (push 0 bin-list))))
             
             
 ;;;;Feature 2 with 6 levels ;item 7 is the stochastic with parameter 21            
     (2 (cond ((<= (nth 7 record) 10) (push 3 bin-list))
              ((and (> (nth 7 record) 10)(<= (nth 7 record) 20)) (push 2 bin-list))
              ((and (> (nth 7 record) 20)(<= (nth 7 record) 50)) (push 1 bin-list))
              ((and (> (nth 7 record) 50)(<= (nth 7 record) 80)) (push -1 bin-list))  
              ((and (>= (nth 7 record) 80)(< (nth 7 record) 90)) (push -2 bin-list))
              ((>= (nth 7 record) 90) (push -3 bin-list))))

 ;;;;Feature 3 with 5 levels ;this is the stochastic today less the stochastic yesterday.            
      (3 (cond ((> (- (nth 7 record)(nth 8 record)) 10) (push 2 bin-list))
               ((> (- (nth 7 record)(nth 8 record))  0)(push 1 bin-list))
             ((< (- (nth 7 record)(nth 8 record)) -10) (push -2 bin-list))
             ((< (- (nth 7 record)(nth 8 record)) 0) (push -1 bin-list))
             ((= (nth 7 record)(nth 8 record)) (push 0 bin-list))))

;;;Feature 4 with 5 levels; This is the TS5        
      (4 (cond ((eql (nth 9 record) 'DN) (push -1 bin-list))
             ((eql (nth 9 record) 'CD) (push -2 bin-list))
             ((eql (nth 9 record) 'UP) (push 1 bin-list))
             ((eql (nth 9 record) 'CU) (push 2 bin-list))
             ((eql (nth 9 record) 'FT) (push 0 bin-list))
             ) )     
;;;Feature 5 with 5 levels; This is TS15          
     (5  (cond ((eql (nth 10 record) 'DN) (push -1 bin-list))
             ((eql (nth 10 record) 'CD) (push -2 bin-list))
             ((eql (nth 10 record) 'UP) (push 1 bin-list))
             ((eql (nth 10 record) 'CU) (push 2 bin-list))
             ((eql (nth 10 record) 'FT) (push 0 bin-list))
             ) )
              
;;;Feature 6 with 5 levels; This is TS45         
    (6   (cond ((eql (nth 11 record) 'DN) (push -1 bin-list))
             ((eql (nth 11 record) 'CD) (push -2 bin-list))
             ((eql (nth 11 record) 'UP) (push 1 bin-list))
             ((eql (nth 11 record) 'CU) (push 2 bin-list))
             ((eql (nth 11 record) 'FT) (push 0 bin-list))
             ) )
             
;;;;Feature 7 with 5 levels ; This is RSD             
      (7  (cond ((> (nth 12 record) 10)(push 2 bin-list))
                ((> (nth 12 record) 0)(push 1 bin-list))
                ((< (nth 12 record) -10)(push -2 bin-list))
                ((< (nth 12 record) 0)(push -1 bin-list))              
                ((zerop (nth 12 record))(push 0 bin-list))))
              
;;;Feature 8 with 3 levels; The is the RSD less the RSD for yesterday        
      (8  (cond ((> (nth 12 record)(nth 13 record)) (push 1 bin-list))
              ((< (nth 12 record)(nth 13 record)) (push -1 bin-list))
              ((= (nth 12 record)(nth 13 record)) (push 0 bin-list))))
              
;;;Feature 9 with 3 levels; this is the dev-distribution-close              

       (9  (cond ((eql 'sell (nth 14 record))  
                  (push -1 bin-list))
               ((eql 'buy (nth 14 record))
                  (push 1 bin-list))
                ((eql 'flat (nth 14 record))
                 (push 0 bin-list))))
;;;Feature 10 with 3 levels is the momentum divergence                 
       (10 (cond ((eql (nth 15 record) 'dn)
                  (push -1 bin-list))
                 ((eql (nth 15 record) 'up)
                 (push 1 bin-list))
                 ((eql (nth 15 record) 'ft)
                 (push 0 bin-list))))
;;;Feature 11 with 17 levels is the formation                 
       (11 (cond ((eql (nth 16 record) 'DB1)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB2)
                  (push 2 bin-list))
                 ((eql (nth 16 record) 'DB3)
                  (push 3 bin-list))
                 ((eql (nth 16 record) 'DB4)
                  (push 4 bin-list))
                 ((eql (nth 16 record) 'DB5)
                  (push 5 bin-list))
                 ((eql (nth 16 record) 'DB6)
                  (push 6 bin-list))
                 ((eql (nth 16 record) 'DB7)
                  (push 7 bin-list))
                 ((eql (nth 16 record) 'DB8)
                  (push 8 bin-list))
                 ((eql (nth 16 record) 'DS1)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS2)
                  (push -2 bin-list))
                 ((eql (nth 16 record) 'DS3)
                  (push -3 bin-list))
                 ((eql (nth 16 record) 'DS4)
                  (push -4 bin-list))
                 ((eql (nth 16 record) 'DS5)
                  (push -5 bin-list))
                 ((eql (nth 16 record) 'DS6)
                  (push -6 bin-list))
                 ((eql (nth 16 record) 'DS7)
                  (push -7 bin-list))
                 ((eql (nth 16 record) 'DS8)
                  (push -8 bin-list))
                 (t (push 0 bin-list))
                    ))
       
       
                ));;;closes the case and the dolist over features
                
      (reverse bin-list)            
 ))          


(defun rank-bins-by-accuracy ()
  (let (contents result accuracy-list)
    (dolist (ith bin-codes)
     (setq contents
          (gethash ith *swing-trade-warehouse*))
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
(defun rank-bins-by-expected-value ()
  (let (contents expected-value-list result (path1 "~/cycles/swing-expected-value.dat"))
   (dolist (ith bin-codes)
     (setq contents
           (gethash ith *swing-trade-warehouse*))
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
 
   
(defun rank-bins-by-profit (swings)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
     (setq result 0 counter 0)
     
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)          
         (setq result (+ result (nth 19 kth)))
         (if (plusp (nth 19 kth)) (incf counter))
         
         ) ;;;closes dolist over contents
      (if (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents)))) 
          
 
     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over bin-codes
   
    (dolist (kth swings)
       (if (plusp (nth 19 kth)) (setq all-winners (+ all-winners (nth 19 kth)))
           (setq all-losers (+ all-losers (nth 19 kth))))) 
   
     (setq counter 0)
     (dolist (jth profit-list)
         (if (plusp (car jth))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))
      
     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length swings)(length (num-markets-in-warehouse swings)))   
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length swings)(length bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length bin-codes)) 1))
      
    ; (if bin-codes (round (/ winners (length bin-codes))) 0)
    winners
 ))  

(defun display-bin (bin)
  (let (contents)
     (setq contents (gethash bin *swing-trade-warehouse*))     
     (dolist (ith contents)
       (print ith))))



;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse 
(defun bin-classifier-swings (date features)
  (let (record bin (result 0) (counter 0) contents signal
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))
  
    (setq record (create-swing-entry-record-list date nil nil))
    (setq record (append record '(nil nil nil)))
    
;    (setf (nth 4 record) (timing-index date))
;    (setf (nth 5 record) (timing-index (getd date 'ydate)))
;    (setf (nth 6 record) (timing-index (getd (getd date 'ydate) 'ydate)))
;    
;    (setf (nth 7 record) (round (slow-stochastic date 21)))
;    (setf (nth 8 record) (round (slow-stochastic (getd date 'ydate) 21)))
;    
;    (setf (nth 9 record) (trend-signal date 5))
;    (setf (nth 10 record)(trend-signal date 15))
;    (setf (nth 11 record)(trend-signal date 45))
;    
;    (setf (nth 12 record)(round (rsi-ave-diff date 14 2)))
;    (setf (nth 13 record)(round (rsi-ave-diff (getd date 'ydate) 14 2)))
;      
;   (setf (nth 14 record)(or (standard-reward-risk1 date) 'FT))
;   (setf (nth 15 record)(or (standard-reward-risk1 (getd date 'ydate)) 'FT))     
;   (setf (nth 16 record)(or (standard-reward-risk1 (getd (getd date 'ydate) 'ydate)) 'FT))
   
   
  (setq bin (encode-swing-trades record features))
  (setq contents (gethash bin *swing-trade-warehouse*))
   
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
         
  (cond ((and (plusp results-long)(plusp results-short)) (setq signal 'OK))
        ((plusp results-long) (setq signal 'UP))
        ((plusp results-short) (setq signal 'DOWN))
        ((and (<= results-long 0)(<= results-short 0)) (setq signal 'AVOID))
        ((not contents) (setq signal 'UNIQUE)))
  
  (values signal longs results-long (if (zerop longs) 0 (/ num-winners-long longs)) 
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))

  
;;;requires a base features list       
(defun swings-leave-one-out (base-features )
  (let (winners-list (result 0))
  (apply #'swing-trade-bins base-features)
  
  (dolist (ith base-features)
    (setq result (apply #'swing-trade-bins (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 )) 
;;;requires a candidate list to add to the base features         
(defun swings-add-one-in (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'swing-trade-bins base-features)
  
  (dolist (ith candidate-features)
    (setq result (apply #'swing-trade-bins (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))  
 
(defun check-swings (date &optional (features *swing-features*))
 (let ((initial-market *data-name*) prev-signal ctr lower-band upper-band
        signal results longs long-gains long-acc shorts short-gains short-acc)
  
  (apply #'swing-trade-bins features)
  (dolist (market (append *swing-list* *forex-list*))
    (set-market market)
    (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc) (bin-classifier-swings date features))
    (multiple-value-setq (prev-signal ctr)(vsignals date 15 1.628 3))
    (multiple-value-setq (lower-band upper-band)(vprices date 15 1.616 3))
    (when (member signal '(OK UP DOWN)) 
       (format T "~% ~A closed at ~F     ~A band last hit ~D days ago~%" 
                  *data-name* (my-pretty-price (getd date 'close))(if (eql prev-signal 'SELL) 'LOWER 'UPPER) ctr)
       (format T " The lower and upper bands for tomorrow are ~8F  and  ~8F~%" (my-pretty-price lower-band)(my-pretty-price upper-band))
       (format T " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~A~%" longs long-gains long-acc)
       (format T " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~A~%" shorts short-gains short-acc)
       (if (and (eql prev-signal 'SELL)(> longs 0)
                (>= (/ long-gains longs) 100))
           (format T "BUY ~A at ~8F on a stop entry for a swing trade~%" *data-name* (my-pretty-price upper-band)))
       (if (and (eql prev-signal 'BUY)(> shorts 0)
                (>= (/ short-gains shorts) 100))
           (format T "SELL ~A at ~8F on a stop entry for a swing trade~%" *data-name* (my-pretty-price lower-band)))
       )
    (setq results (acons market signal results))    
     
       );;;closes the dolist over markets
    
    (set-market initial-market)   (format T "~%") 
   (setq results (reverse results))
   (dolist (ith results)
     (format T "~A is ~A~%" (car ith) (cdr ith))) 
   )) 


;;;this function works for both daytrades and swings
;;;does assume you have already run swing-trade-bins and/or day-trade-bins
(defun num-trades-in-warehouse (market swings)
  (let ((ctr 0) start-dates end-dates first-date last-date trading-degree)
    
    (dolist (record swings)   
      (when (eql market (car record))
           (incf ctr)
           (pushnew (cadr record) start-dates)
           (pushnew (nth 17 record) end-dates)
           (setq trading-degree (nth 5 record))
           ))
    (setq first-date (car (sort start-dates #'< )))
    (setq last-date (car (sort end-dates #'>)))
    
      (values ctr first-date last-date trading-degree)
      
      ))
(defun gain-loss-trades-in-warehouse (market swings)
  (let ((pl 0))
     (dolist (record swings)
       (if (eql market (car record)) (setq pl (+ pl (nth 19 record)))))
    pl))
   
;;;use waves instead of swings with ewaves trades      
(defun display-num-trades-by-market (swings)
   (let (training-data markets-list num-trades first-date last-date
         (path "~/cycles/market-trade-data.dat"))
   
    (setq training-data (num-markets-in-warehouse swings))
    (dolist (ith training-data)
    (multiple-value-setq (num-trades first-date last-date )(num-trades-in-warehouse ith swings))
      (setq markets-list 
           (cons (list ith (gain-loss-trades-in-warehouse ith swings) num-trades first-date last-date) markets-list))
       );;closes the dolist
    (vsort markets-list #'> 'second)
   ; (vsort markets-list #'< #'(lambda(s)(first-char-code (car s))))
    (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "Market    #trades  Sum $P&L Ave $P&L    FIRST     LAST~%")    
    (dolist (jth markets-list)
     (format str"~%~10A ~4D ~10D   ~4D    ~A  ~A" 
     (car jth) (third jth)(second jth) (round (/ (second jth)(third jth)))(fourth jth)(fifth jth))))
  ));;closes the let and defun.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             
 ;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for swing trades to decide to trade or not
;;;;assumes (swing-trade-bins date features) has already been run
(defun find-best-volatility8 (tdate &optional (output T)(output1 T) (date1 nil)) 
  (let* ((ctr 0) rollover entry-short entry-long  long short 
         cover-long cover-short (ave4 (ave tdate 3 'pivot))  directive1  
        prev-signal action stop-long stop-short risk-short risk-long (time-frame 'SWING) 
        (VT (make-hash-table))        signal longs long-gains long-acc shorts short-gains short-acc trade-direction       
        )
    
    (declare (ignore  ctr))
    (if (member *data-name* *forex-list*) (setq time-frame 'FX-SWING))
      
  (setf (gethash tdate VT)(volatility-log tdate 60 1)) ;;volatility over the past three months
                                       
   (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc) (bin-classifier-swings tdate *swing-features*))
   (multiple-value-setq (prev-signal ctr) (vsignals tdate 15 1.618 3))
    
   (setq trade-direction (cond ((and (member signal '(OK UP))(eql prev-signal 'SELL)
                                      (> longs 0)
                                      (>= (/ long-gains longs) 100)
                                      ) 'UP)
                               ((and (member signal '(OK DOWN))(eql prev-signal 'BUY)
                                      (> shorts 0)
                                      (>= (/ short-gains shorts) 100)
                                      ) 'DN)
                               (t 'FT)))
    
    (setq cover-short (exp (- (log ave4) (* *objective-factor-swing* (gethash tdate VT)))) 
          cover-long (exp (+ (log ave4) (* *objective-factor-swing* (gethash tdate VT)))) 
                              )   
    
    (multiple-value-setq (entry-short entry-long)(vprices tdate 15 1.618 3))
    
    (setq entry-short (min entry-short (getd tdate 'close)) entry-long (max entry-long (getd tdate 'close)))
    
    (setq risk-short 
         (min (- entry-long entry-short)(abs (- entry-short  (n-day-high tdate 3 'close))))
                   )
                    
    (setq risk-long 
        (min (- entry-long entry-short)(abs (- entry-long  (n-day-low tdate 3 'close)))) 
        )   
    
   
    (setq stop-short (+ entry-short risk-short) ;;;this is the init buy stop
          stop-long (- entry-long risk-long))    ;;;this is the init sell stop
      
    (setq rollover (getd tdate 'rollover))
    (ifn rollover (setq rollover 0)) 
;;;;sets long and short to be true if open position coming into today    
    (setq long (eql (cadr (assoc *data-name* *open-swings*)) 'long))
    (setq short (eql (cadr (assoc *data-name* *open-swings*)) 'short))           
  
    
    (if short (setq stop-short (fmin entry-long (+ rollover (caddr (assoc *data-name* *open-swings*)))))) 
    
    (if long (setq stop-long (fmax entry-short (+ rollover (caddr (assoc *data-name* *open-swings*))))))    
    
    
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
             
        
                      
           (when (eql (cadr (assoc *data-name* *open-swings*)) 'long) 
                 (push "NOT LONG" action)(push "OPEN LONG" action))
                 
                 
           (when (eql (cadr (assoc *data-name* *open-swings*)) 'short) 
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))  
             
                 
         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY") 
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))
         
                                        
      (format output "   ~A  ~%" action) 
      
      (format output " Prev Signal= ~A Current bin= ~3A Direction = ~3A ~%"  
                     prev-signal signal trade-direction )
                     
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
             (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
                 )
             
             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1) 
                )
                     
             ((equal action "NOT LONG") 
              (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
               ))
                
       (cond ((eql (cadr (assoc *data-name* *open-swings*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'OPEN 'LONG date1 nil stop-long cover-long output1)
                  )                    
             ((eql (cadr (assoc *data-name* *open-swings*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil stop-short cover-short output1)
                  )
             ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'LONG date1 nil stop-long cover-long output1)
                  )                    
             ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'SHORT date1 nil stop-short cover-short output1) 
                 
                  ));;;;closes the cond
                  
           
     
      ));;;closes the let and the defun       
       
#|  
;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage   
(defun swing-simulation-test (market date2 num &optional (features *swing-features*))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade 
       risk risk-long risk-short  signal longs long-gains long-acc
       shorts short-gains short-acc prev-signal ctr date-1 
       (outfile (string-append "~/cycles/" (format nil "~S" market) "swing-summary.dat"))
       (AT (make-hash-table)) (VT (make-hash-table))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "swing-simulation.dat")))
      (declare (ignore  short-acc long-acc ctr))
    
   (apply #'swing-trade-bins features)  
   (set-market market)
      
   (setq date (add-mkt-days date2 (- num)))
  ; (if  (probe-file path1)
  ;      (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
    
    (cond ((gethash  date  AT))
          (t (setf (gethash date AT) (ave date 3 'pivot))))      
  
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months  

                      
   (multiple-value-setq (entry-short entry-long)
        (vprices date 15 *entry-factor-swing* 3)) 
      
   (setq entry-short (min entry-short (getd date 'close)) entry-long (max entry-long (getd date 'close)))
    
   (setq ave4 (gethash date AT) 
       cover-short  (exp (- (log ave4) (* *objective-factor-swing* (gethash date VT))))                     
       cover-long (exp (+ (log ave4) (* *objective-factor-swing* (gethash date VT))))
                             )
 
 
   (setq risk-short  (* *stop-loss-swing* (abs (- entry-short (n-day-high date 3 'close))) 
                          ))
                          
   (setq risk-long  (* *stop-loss-swing* (abs (- entry-long  (n-day-low date 3 'close)))
                       ))
                       
                        
   (setq risk (max risk-long risk-short));;;
   
  
   ;(format T "date= ~A param1= ~A Risk-short= ~A Risk-long=~A ~%" date param1 risk-short risk-long) 
   
   (setq date-1 date date (add-mkt-days date 1)) 
    
        
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 ;cover-long (+ cover-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))
    
    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
    						 ;cover-short (+ cover-short (getd date 'rollover))
    						 stop-short (+ stop-short (getd date 'rollover))
    ))
  
  
 
   (when long (setq stop-long (fmax stop-long entry-short)))  
                                                
   (when short (setq stop-short (fmin stop-short entry-long)))
     
                              
 ;;;;check if stopped out of prior position   
                  
   
   (when (and long (<= (getd date 'low) stop-long))       
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil))
          
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)
          (setq trade nil short nil stop-short nil))
                      
  ;;;check if met objective       
     (when (and long (> (getd date 'high) cover-long))
                        
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))
            
      (when (and short (< (getd date 'low) cover-short))
                            
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))    

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(<= (getd date 'low) entry-short)
                (<= (* risk (index-point-value)) *max-swing-risk*))
           (and (not long) (>= (getd date 'high) entry-long)
                (<= (* risk (index-point-value)) *max-swing-risk*)))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-swings date-1 features))
       (multiple-value-setq (prev-signal ctr)(vsignals date-1 15 *entry-factor-swing* 3)))


;;;check if new entry          
  
    (when (and (not short)              
               (<= (getd date 'low) entry-short)
               (member signal '(OK DOWN)) 
               (eql prev-signal 'BUY)(> shorts 0)
               (>= (/ short-gains shorts) 100)  
               (<= (* risk (index-point-value)) *max-swing-risk*)              
               )
          (setq short (min entry-short (getd date 'open))                    
                 trade (list date 'short short)
                 stop-short 
                  (+ entry-short risk-short) 
                
         ))
 
                   
    (when (and (not long)               
               (>= (getd date 'high) entry-long)
               (member signal '(OK UP))
               (eql prev-signal 'SELL)(> longs 0)
               (>= (/ long-gains longs) 100)             
               (<= (* risk (index-point-value)) *max-swing-risk*)                       
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long) 
                 stop-long 
                   (- entry-long risk-long)  
               
              ))
                 
 
 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
     
             
    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))            
          
           
    (when  (and short (or (< (getd date 'open)(getd date 'close))
                          (>= (getd date 'high) entry-long))
               (>= (getd date 'high) stop-short))         
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))
           
     
    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           ))       
              
           
 ;;;check if met objective on day of entry      

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil)))    
           
          
   );;;closes the dotimes
 
      
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)    
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D  $/CONTRACT= ~D"
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
     );close the format   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   ));;;closes the let and the defun       
      
|#   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;enters with stop orders and exits with stop loss orders or objectives and is always out at the end of the day
;;;Plain vanilla volatility breakout with 4 .764 and 1 parameter values.
;;;;For DAY TRADING
;;;Designed to write out the file for the DAY Trades Warehouse.  

;;;;the file has one record per trade each record is a list. The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1 
;;;;TI  is the timing index it is a natural number
;;;;TI-1 is the timing index for the previous day
;;;;HT is the ratio of average true range for 4 days compared to 28 days
;;;;BT is the bar type
;;;;BR is the Bar relationship to the prior day
;;;;SST is the stochastic with parameter 21
;;;;T3 is the trend-signal with parameter 3
;;;;T5 is the trend-signal with parameter 5
;;;T15 is the trend-signal with parameter 15
;;;RSD is the rsi14 less the rsi14 two days prior. (2 day rate of change in the rsi with parameter 14
;;SRR is the standard reward to risk using the day trading paramters. It is either OB OS or FT
;;; (*data-name* entry-date direction entry-price TI TI-1 HT BT BR TS3 TS5 TS15 RSD RSD-1 SRR SRR-1 SRR-2)
   
   
(defun populate-day-trades (market date2 num &optional (outfile T))
 (let (date trades long short cover-long cover-short long-entry short-entry  
        risk-short risk-long  date-1 
        ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short 
       (VT (make-hash-table)) ;(AT (make-hash-table))
       ave4 risk (path1 (string-append "~/cycles/" (format nil "~S" market) "daytrades.dat")))
    
   (maind-x)(set-cat-list)
   (set-market market)
   (setq date (add-mkt-days date2 (- num)))
 ;  (if  (probe-file path1)
 ;       (delete-file path1))
   (setq date-1 (getd date 'ydate))
 
  
 ;;;;from date1 to date2
 (dotimes (ith num)
     
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months
 
 
   (multiple-value-setq (short-entry long-entry)(vprices date 4 *entry-factor* 1))
   
   (setq ave4 (ave date 4 'pivot)
         cover-short (exp (- (log ave4) (* *objective-factor* (gethash date VT)))) 
         cover-long (exp (+ (log ave4) (* *objective-factor* (gethash date VT)))))
         
 
   (setq risk-short (abs (* *stop-loss-day* (- short-entry (getd date 'close)))))
   (setq risk-long  (abs (* *stop-loss-day* (- long-entry (getd date 'close)))))
  
   (setq risk (max risk-long risk-short))
  
   (setq   date-1 date date (add-mkt-days date 1))
    
           
   (when (getd date 'rollover) (setq  long-entry (+ long-entry (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))
    				      
    
    ))
   (when (getd date 'rollover) (setq short-entry (+ short-entry (getd date 'rollover))
    	                              cover-short (+ cover-short (getd date 'rollover))
    						 
    ))
      
;;;check if new entry   
      
   (when (and (not short)             
              ;(<= (* risk (index-point-value)) *max-day-risk*) 
              (<= (getd date 'low) short-entry)               
                                                       
              )
          (setq short (min (getd date 'open) short-entry)                     
                short-trade (create-daytrade-entry-record-list date-1 -1 short)
                stop-short (+ short risk) 
                 ))
 
                 
    (when (and (not long) 
              ; (<= (* risk (index-point-value)) *max-day-risk*) 
               (>= (getd date 'high) long-entry)               
                                                   
             )                                         
           (setq long (max (getd date 'open) long-entry)                     
                 trade (create-daytrade-entry-record-list date-1 1 long)
                 stop-long (- long risk)  
                 ))
              
            
 ;;;check if stopped out on same day 

   (when (and long stop-long  (> (getd date 'open)(getd date 'close))
                                 
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date (my-pretty-price stop-long)
                                        (round (* (index-point-value) (- stop-long long))))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
     (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                (or (<= (getd date 'low) short-entry)
                    (<= (getd date 'close) stop-long)))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date (my-pretty-price stop-long)
                                         (round (* (index-point-value) (- stop-long long))))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))       
    (when (and short stop-short (< (getd date 'open)(getd date 'close))
                                
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (index-point-value) (- short stop-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
     (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                (or (>= (getd date 'high) long-entry)
                    (>= (getd date 'close) stop-short)))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price stop-short)
                                       (round (* (index-point-value) (- short stop-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met objective  before end of day     
     (when (and long  (> (getd date 'high) cover-long))              
                              
            (push (- cover-long long) trades) 
            
            (setq trade (append trade (list date (my-pretty-price cover-long)
                                          (round (* (index-point-value) (- cover-long long))))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
            
      (when (and short (< (getd date 'low) cover-short))
           
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price cover-short)
                                         (round (* (index-point-value) (- short cover-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))   

           
;;;check if met exit criteria to exit at end of day of entry for day trade     
     (when long                
            (setq cover-long (getd date 'close))                   
            (push (- cover-long long) trades)
            (setq trade (append trade (list date (my-pretty-price cover-long)
                                           (round (* (index-point-value) (- cover-long long))))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
            
      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (index-point-value) (- short cover-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))    
                
           
   );;;closes the dotimes
  
  
     
   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
    
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
  
 ));     

;;;the date is actually date-1
;;;direction is -1 or 1
;;;entry is the entry price
(defun create-daytrade-entry-record-list (date direction entry)
   (let* ((date-1 (getd date 'ydate)))
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
                              (timing-index date)(timing-index date-1)
                              (my-round (/ (volatility date 4 1) (volatility date 28 1)) 3)
                              (day-bar-type date) (day-bar-type1 date)                             
                              (trend-signal date 3)(trend-signal date 5)(trend-signal date 15)
                              (round (rsi-ave-diff date 14 2))(round (rsi-ave-diff date-1 14 2))
                              (dev-distribution-close date 4 .875 'close)
                              (momentum-divergence1 date 5 13)
                              (or (car (formation date)) 'FT));;;closes the list
                            
))


;;;;FOR DAY TRADES 
;;;returns a list of up to 13 codes 
;;; (*data-name* entry-date direction entry-price TI TI-1 HT BT BR TS3 TS5 TS15 RSD RSD-1 DEV-OBJ MOM-DIV FORMATION)
   
(defun encode-day-trades (record features) 
  (let (bin-list) 
  
    (dolist (ith features)
     (case ith
 ;;;;Feature 1 with 4 levels ;item 4 is the TI index. 
      (1 (cond ((and (>= (nth 4 record) 0) (<= (nth 4 record) 1)) (push 0 bin-list)) ;;; 0 1 -> 0
              ((and (>= (nth 4 record) 2)(<= (nth 4 record) 3)) (push 0 bin-list)) ;;; 2 3 -> 1
               ((and (>= (nth 4 record) 4)(<= (nth 4 record) 5)) (push 1 bin-list)) ;;; 4 5 > 2
              ((>= (nth 4 record) 6) (push 1 bin-list)))) ;;; 6 or more -> 3
            
 ;;;;Feature 2 with 4 levels ;item 5 is the TI-1 index            
     (2 (cond ((and (>= (nth 5 record) 0) (<= (nth 5 record) 1)) (push 0 bin-list)) ;;; 0 1  -> 0
              ((and (>= (nth 5 record) 2)(<= (nth 5 record) 3)) (push 0 bin-list)) ;;; 2 3 -> 1
              ((and (>= (nth 5 record) 4)(<= (nth 5 record) 5)) (push 1 bin-list)) ;;; 4 5 -> 2
              ((>= (nth 5 record) 6) (push 1 bin-list)))) ;;; 6 or more -> 3

 ;;;;Feature 3 with 10 levels ;this is the HT ratio of volatility            
      (3 (cond ((< (nth 6 record) .60) (push 5 bin-list))
             ((and (>= (nth 6 record) .60)(< (nth 6 record) .70)) (push 4 bin-list))
             ((and (>= (nth 6 record) .70)(< (nth 6 record) .80)) (push 3 bin-list))
             ((and (>= (nth 6 record) .80)(< (nth 6 record) .90)) (push 2 bin-list))
             ((and (>= (nth 6 record) .90)(< (nth 6 record) 1.00)) (push 1 bin-list))
             ((and (>= (nth 6 record) 1.00)(< (nth 6 record) 1.10))(push -1 bin-list))
             ((and (>= (nth 6 record) 1.10)(< (nth 6 record) 1.20)) (push -2 bin-list))
             ((and (>= (nth 6 record) 1.20)(< (nth 6 record) 1.30)) (push -3 bin-list))
             ((and (>= (nth 6 record) 1.30)(< (nth 6 record) 1.40))(push -4 bin-list))
             ((>= (nth 6 record) 1.4)(push -5 bin-list))))

;;;;Feature 4 with 9 levels bar type
      (4 (cond ((eql (nth 7 record) 11) (push 0 bin-list))
               ((eql (nth 7 record) 12) (push 1 bin-list)) 
               ((eql (nth 7 record) 13) (push 2 bin-list)) 
               ((eql (nth 7 record) 21) (push 3 bin-list)) 
               ((eql (nth 7 record) 22) (push 4 bin-list)) 
               ((eql (nth 7 record) 23) (push 5 bin-list)) 
               ((eql (nth 7 record) 31) (push 6 bin-list)) 
               ((eql (nth 7 record) 32) (push 7 bin-list)) 
               ((eql (nth 7 record) 33) (push 8 bin-list)) ))

;;;;;;Feature 5 with 5 levels of bar relationship to the previous bar

      (5 (cond ((eql (nth 8 record) 'IN) (push -2 bin-list))
               ((eql (nth 8 record) 'OU) (push 2 bin-list))
               ((eql (nth 8 record) 'DN) (push -1 bin-list))
               ((eql (nth 8 record) 'UP) (push 1 bin-list))
               ((eql (nth 8 record) 'FT) (push 0 bin-list))
                     ))

;;;Feature 6 with 5 levels; This is the TS3        
     
      (6 (cond ((eql (nth 9 record) 'DN) (push -1 bin-list))
               ((eql (nth 9 record) 'CD) (push -2 bin-list))
               ((eql (nth 9 record) 'UP) (push 1 bin-list))
               ((eql (nth 9 record) 'CU) (push 2 bin-list))
               ((eql (nth 9 record) 'FT) (push 0 bin-list))
             ) )
                  
;;;Feature 7 with 5 levels; This is TS5          
     (7  (cond ((eql (nth 10 record) 'DN) (push -1 bin-list))
             ((eql (nth 10 record) 'CD) (push -2 bin-list))
             ((eql (nth 10 record) 'UP) (push 1 bin-list))
             ((eql (nth 10 record) 'CU) (push 2 bin-list))
             ((eql (nth 10 record) 'FT) (push 0 bin-list))
             ) )
              
;;;Feature 8 with 5 levels; This is TS15         
    (8   (cond ((eql (nth 11 record) 'DN) (push -1 bin-list))
               ((eql (nth 11 record) 'CD) (push -2 bin-list))
               ((eql (nth 11 record) 'UP) (push 1 bin-list))
               ((eql (nth 11 record) 'CU) (push 2 bin-list))
               ((eql (nth 11 record) 'FT) (push 0 bin-list))
             ) )
             
;;;;Feature 9 with 5 levels ; This is RSD             
      (9  (cond ((> (nth 12 record) 10)(push 2 bin-list))
                ((> (nth 12 record) 0) (push 1 bin-list))
                ((< (nth 12 record) -10)(push -2 bin-list))
                ((< (nth 12 record) 0)(push -1 bin-list))              
                ((zerop (nth 12 record))(push 0 bin-list))))
              
;;;Feature 10 with 5 levels; The is the RSD less the RSD for yesterday        
      (10  (cond ((> (- (nth 12 record)(nth 13 record)) 10) (push 2 bin-list))
                 ((> (- (nth 12 record)(nth 13 record)) 0) (push 1 bin-list))
                 ((< (- (nth 12 record)(nth 13 record)) -10)(push -2 bin-list))
                 ((< (- (nth 12 record)(nth 13 record)) 0)(push -1 bin-list))
                 ((= (nth 12 record)(nth 13 record)) (push 0 bin-list))))
 
               
;;;Feature 11 with 3 levels; this is the dev-distribution-close              

       (11  (cond ((eql 'sell (nth 14 record))  
                  (push -1 bin-list))
               ((eql 'buy (nth 14 record))
                  (push 1 bin-list))
                ((eql 'flat (nth 14 record))
                 (push 0 bin-list))))
;;;;feature 12 with 3 levels is momentum divergence                 
       (12 (cond ((eql (nth 15 record) 'dn)
                  (push -1 bin-list))
                 ((eql (nth 15 record) 'up)
                 (push 1 bin-list))
                 ((eql (nth 15 record) 'ft)
                 (push 0 bin-list))))
;;;feature 13 is formation                 
       (13 (cond ((eql (nth 16 record) 'DB1)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB2)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB3)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB4)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB5)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB6)
                    (push 1 bin-list))
                 ((eql (nth 16 record) 'DB7)
                   (push 1 bin-list))
                 ((eql (nth 16 record) 'DB8)
                   (push 1 bin-list))
                 ((eql (nth 16 record) 'DS1)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS2)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS3)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS4)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS5)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS6)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS7)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS8)
                  (push -1 bin-list))
                 (t (push 0 bin-list))
                    ))
       
                
                ));;;closes the case and the dolist over features
                
      (reverse bin-list)            
 ))  
 
 

;;;;;Now we need first to read the day-trades file(s) and load into the *day-trade-warehouse* hash table

(defvar daytrades nil)
(defvar day-bin-codes nil)

(defun day-trade-bins (&rest features)
  (let (bin path)
  (setq path (setq path "~/cycles/daytradewarehouse.dat"))
  (maind-x)(set-cat-list)
  (setq daytrades nil day-bin-codes nil)(clrhash *day-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record daytrades)))
       
;;;;if *out-of-sample* is set to T the historical database is stripped of the trades
;;;;for the current market        
 (if *out-of-sample*
     (setq daytrades (remove-if #'(lambda (s) (eql (car s) *data-name*)) daytrades)))      
  
     
;;;;now all the trades are in the list swings.  
;;;we now assign/sort the trades into bins   
;;;basically process a record list into a code or bin
;;;and create a code
   
   (dolist (record daytrades)
     (setq bin (encode-day-trades record features))
     (pushnew bin day-bin-codes :test #'equal)
     (cond ((gethash bin *day-trade-warehouse*)
            (ifn (member record (gethash bin *day-trade-warehouse*) :test #'equal)
                 (setf (gethash bin *day-trade-warehouse*)
                       (cons record (gethash bin *day-trade-warehouse*)))))
           ((not (gethash bin *day-trade-warehouse*))
             (setf (gethash bin *day-trade-warehouse*)
              (list record)))))
  
    (format T  "~%FEATURES = ~A~%" features)
    (rank-daytrade-bins-by-profit daytrades) 
     
         
  ))          
  
  
   
(defun rank-daytrade-bins-by-profit (daytrades)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith day-bin-codes)
     (setq contents (gethash ith *day-trade-warehouse*))
     (setq result 0 counter 0)
     
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)          
         (setq result (+ result (nth 19 kth)))
         (if (plusp (nth 19 kth)) (incf counter))
         
         ) ;;;closes dolist over contents
      (if (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents)))) 
          
 
     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over day-bin-codes
   
    (dolist (kth daytrades)
       (if (plusp (nth 19 kth)) (setq all-winners (+ all-winners (nth 19 kth)))
           (setq all-losers (+ all-losers (nth 19 kth))))) 
   
     (setq counter 0)
     (dolist (jth profit-list)
         (if (plusp (car jth))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))
      
     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length daytrades)(length (num-markets-in-warehouse daytrades)))   
     (format T "ALL WINNERS = ~A   ALL LOSERS = ~A~%~%" all-winners all-losers)
     (format T "NUMBER of ALL BINS = ~D~%" (length day-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length daytrades)(length day-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~A  IN ~D WINNING BINS~%~%" winners counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~A~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length day-bin-codes)) 1))
      
    ; (if day-bin-codes (round (/ winners (length day-bin-codes))) 0)
    winners
 ))         
 

;;;;this function expects that you have run (day-trade-bins ...) already   
(defun rank-daytrade-bins-by-expected-value ()
  (let (contents expected-value-list result (path1 "~/cycles/daytrade-expected-value.dat"))
   (dolist (ith day-bin-codes)
     (setq contents
           (gethash ith *day-trade-warehouse*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (nth 19 kth))))      
     (setq expected-value-list (cons (list (/ result (length contents)) ith (length contents)) expected-value-list)));;;closes the dolist over bin-codes
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
         (vsort expected-value-list #'> 'car)
         (format stream "Expected      Bin       NUM~%Value~%")
         (dolist (jth expected-value-list)
             (format stream "~A      ~A       ~D~%" (round (car jth)) (cadr jth) (third jth)))) ;;;;closes the with-open-file
 ))   
 
(defun display-day-bin (bin)
  (let (contents)
     (setq contents (gethash bin *day-trade-warehouse*))     
     (dolist (ith contents)
       (print ith))))
       
;;; (*data-name* entry-date direction entry-price TI TI-1 HT BT BR TS3 TS5 TS15 RSD RSD-1 SRR SRR-1 SRR-2)

;;;;returns if date is bullish or bearish and expected value and accuracy and number in daytradewarehouse 
(defun bin-classifier-daytrades (date features)
  (let (record bin (result 0) (counter 0) contents signal
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))
  
    (setq record (create-daytrade-entry-record-list date nil nil))
    (setq record (append record '(nil nil nil)))
    
    (setq bin (encode-day-trades record features))
    (setq contents (gethash bin *day-trade-warehouse*))
   
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
         
  (cond ((and (plusp results-long)(plusp results-short)) (setq signal 'OK))
        ((plusp results-long) (setq signal 'UP))
        ((plusp results-short)(setq signal 'DOWN))
        ((and (<= results-short 0)(<= results-long 0)) (setq signal 'AVOID))
        ((not contents) (setq signal 'UNIQUE)))
  
  (values signal longs results-long (if (zerop longs) 0 (/ num-winners-long longs)) 
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))

 
(defun check-daytrades (date &optional (features *day-features*))
 (let ((initial-market *data-name*)  lower-band upper-band
        signal results longs long-gains long-acc shorts short-gains short-acc)
  
  (apply #'day-trade-bins features)
  (dolist (market *day-list*)
    (set-market market)
    (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc) (bin-classifier-daytrades date features))
   
    (multiple-value-setq (lower-band upper-band)(vprices date 4 *entry-factor* 1))
    (when (member signal '(OK UP DOWN)) 
       (format T "~% ~A   ~F     ~%" *data-name* (my-pretty-price (getd date 'close)))
       (format T " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~A~%" longs long-gains long-acc)
       (format T " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~A~%" shorts short-gains short-acc)
       (if (and (> longs 0)(> long-gains 0)(>= (/ long-gains longs) 100))
           (format T "BUY ~A at ~8F on a stop entry for a day trade~%" *data-name* (my-pretty-price upper-band)))
       (if (and (> shorts 0)(> short-gains 0)(>= (/ short-gains shorts) 100))
           (format T "SELL ~A at ~8F on a stop entry for a day trade~%" *data-name* (my-pretty-price lower-band)))
       )
    (setq results (acons market signal results)) 
    
       );;;closes the dolist over markets
    
    (set-market initial-market)   (format T "~%") 
   (setq results (reverse results))
   (dolist (ith results)
     (format T "~A is ~A~%" (car ith) (cdr ith))) 
   ))   
   
;;;requires a candidate list to add to the base features         
(defun daytrades-add-one-in (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'day-trade-bins base-features)
  
  (dolist (ith candidate-features)
    (setq result (apply #'day-trade-bins (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))    

;;;requires a base features list       
(defun daytrades-leave-one-out (base-features )
  (let (winners-list (result 0))
  (apply #'day-trade-bins base-features)
  
  (dolist (ith base-features)
    (setq result (apply #'day-trade-bins (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 )) 
 


(defun find-best-day-trade8 (tdate &optional (output T)(output1 T)(date1 nil)) 
  (let (risk risk-short risk-long  stop-short stop-long  entry-long entry-short 
        trade-direction (VT (make-hash-table)) 
        cover-long cover-short ave4 action directive1  
        )
      (declare (special signal longs shorts long-gains short-gains long-acc short-acc))
                                   
 ;  (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc) (bin-classifier-daytrades tdate *day-features*))
      
   (if (and (member signal '(OK UP))(> longs 0)(>= (/ long-gains longs) 100)) (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member signal '(OK DOWN))(> shorts 0)(>= (/ short-gains shorts) 100)) (push 'DN trade-direction)(push 'FT trade-direction))
  
   (setf (gethash tdate VT)(volatility-log tdate 60 1))
    
   (setq ave4 (ave tdate 4 'pivot)
           cover-short (exp (- (log ave4) (* *objective-factor* (gethash tdate VT))))
           cover-long (exp (+ (log ave4) (* *objective-factor* (gethash tdate VT))))
           )  

   (multiple-value-setq (entry-short entry-long) (vprices tdate 4 *entry-factor* 1))
       
   (setq risk-short  (*  *stop-loss-day* (abs (- entry-short (getd tdate 'close)))))                         
   (setq risk-long  (*  *stop-loss-day* (abs (- entry-long (getd tdate 'close)))))
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
       
      (format output " ~A~%" action)  
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
             (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
             (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1))
             
             ((equal action "NOT SHORT")
               (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1))
                     
             ((equal action "NOT LONG")   
              (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1) 
                                   ))
                
     
      )) 
   

(defun remove-day-trade-market (market)
  (let (trades path)
  (setq path (setq path "~/cycles/daytradewarehouse.dat"))
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

(defun daytrade-simulation-test (market date2 num &optional (features *day-features*)) ; (out-of-sample nil))
 (let (date trades long short cover-long cover-short entry-long entry-short  
       risk-short risk-long  date-1 signal longs long-gains shorts short-gains
       ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short 
       ave3 risk long-acc short-acc (VT (make-hash-table))
       (outfile (string-append "~/cycles/" (format nil "~S" market) "day-summary.dat"))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "day-simulation.dat")))    
   (declare (ignore long-acc short-acc))
   
;   (when out-of-sample 
;      (apply #'day-trade-bins features)
;      (setq swings (remove-if #'(lambda (s) (eql (car s) market)) swings))
   
   (apply #'day-trade-bins features)
   (set-market market)
   
   (setq date (add-mkt-days date2 (- num)))

 
 ;;;;from date1 to date2
 (dotimes (ith num)

         
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months
 
   (multiple-value-setq (entry-short entry-long)(vprices date 4 *entry-factor* 1))   
   
   (setq ave3 (ave date 4 'pivot)
         cover-short (exp (- (log ave3) (* *objective-factor* (gethash date VT)))) 
         cover-long (exp (+ (log ave3) (* *objective-factor* (gethash date VT))))  
         )
      
   (setq risk-short  (* *stop-loss-day* (abs (- entry-short (getd date 'close)))))
   (setq risk-long   (* *stop-loss-day* (abs (- entry-long (getd date 'close)))))
   (setq risk (max risk-long risk-short))
  
   (setq  date-1 date date (add-mkt-days date 1))
    
           
   (when (getd date 'rollover) (setq  entry-long (+ entry-long (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))
    				      
    ))
   (when (getd date 'rollover) (setq entry-short (+ entry-short (getd date 'rollover))
    	                              cover-short (+ cover-short (getd date 'rollover))
    						 
    ))

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(<= (getd date 'low) entry-short)
                (<= (* risk (index-point-value)) *max-day-risk*))
           (and (not long) (>= (getd date 'high) entry-long)
                (<= (* risk (index-point-value)) *max-day-risk*)))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-daytrades date-1 features))
       )
      
;;;check if new entry   
      
   (when (and (not short)             
              (<= (* risk (index-point-value)) *max-day-risk*) 
              (<= (getd date 'low) entry-short)               
              (member signal '(OK DOWN))(> shorts 0)
              (>= (/ short-gains shorts) 100)                          
                 )
          (setq short (min (getd date 'open) entry-short)                     
                short-trade (list date 'short short)
                stop-short  (+ short risk-short) 
                 ))
 
                 
    (when (and (not long) 
               (<= (* risk (index-point-value)) *max-day-risk*) 
               (>= (getd date 'high) entry-long)               
               (member signal '(OK UP))(> longs 0)
               (>= (/ long-gains longs) 100)                       
             )
                                         
           (setq long (max (getd date 'open) entry-long)                     
                 trade (list date 'long long) 
                 stop-long (- long risk-long)  
                 ))
              
            
 ;;;check if stopped out on same day 

   (when (and long stop-long  (> (getd date 'open)(getd date 'close))                                 
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
     (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                (or (<= (getd date 'low) entry-short)
                    (<= (getd date 'close) stop-long)))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))       
    (when (and short stop-short (< (getd date 'open)(getd date 'close))                                
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date 'exit stop-short (- short stop-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
     (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                (or (>= (getd date 'high) entry-long)
                    (>= (getd date 'close) stop-short)))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date 'exit stop-short (- short stop-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met objective  before end of day     
     (when (and long  (> (getd date 'high) cover-long))                              
            (push (- cover-long long) trades)           
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
            
      (when (and short (< (getd date 'low) cover-short))           
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))   

           
;;;check if met exit criteria on day of entry (exit at end of day)     
     (when long                
            (setq cover-long (getd date 'close))                   
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
            
      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))    
                
           
   );;;closes the dotimes
  
  
     
   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
    
  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
 
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2) 
   (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D  $/CONTRACT= ~D"
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
     (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
        ));;;closes the with-open-file
  );;;closes the when outfile  
   
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)  
   ));     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;;;enters with stop orders and exits with stop loss orders always in the market
;;;Plain vanilla volatility breakout with 35 2.618 and 7 parameter values.
;;;;For Position TRADING
;;;Designed to write out the file for the position Trades Warehouse.  

;;;;the file has one record per trade each record is a list. The indicator values are for the previous date before the entry date.
;;;;direction is either 1 or -1 
;;;;TI  is the timing index it is a natural number
;;;;SST is the stocastic with parameter 21
;;;;T5 is the trend-signal with parameter 5
;;;;T15 is the trend-signal with parameter 15
;;;T45 is the trend-signal with parameter 45
;;;RSD is the rsi14 less the rsi14 two days prior. (2 day rate of change in the rsi with paramter 14
;;Dev-obj is the deviation of closes from the 4 day moving average using the .875 percentile
;;;the value is sell buy or flat
;;;Mom-div is the momentum divergence using the MACD with parameters 5 13 5. The value is up dn or ft.
;;;Formation is one of sixteen bullish and bearish patterns. example DB1 thru DB8 and DS1 thru DS8. 
;;; (*data-name* entry-date direction entry-price TI SST T5 T15 T45 RSD dev-obj mom-div Formation)

(defun populate-position-trades (market date2 num &optional  (output T))
 (let (date stop-long stop-short trades long short  trade-long  entry-short entry-long
       ave-win ave-loss losers winners extended-trades trade         date-1  
       (path1 (string-append "~/cycles/" (format nil "~S" market) "positiontrades.dat")))
   (maind-x)(set-cat-list)   
   (set-market market)
   (setq date (add-mkt-days date2 (- num)))
   (setq date-1 (getd date 'ydate))
 

 ;;;;from date1 to date2
 (dotimes (ith num)
       
   (multiple-value-setq (entry-short entry-long) (vprices date 18 2.53 7))      
 
   (setq  date-1 date date (add-mkt-days date 1))
         
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))
    
    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))
    ))
    
  ;;;;check if stopped out of prior position   
   
   (when long (setq stop-long (max stop-long entry-short)))
   (when short (setq stop-short (min stop-short entry-long))) 
          

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
                      

           
;;;check if new entry          
  
    (when (and (not short)              
               (<= (getd date 'low) entry-short)               
               )
          (setq short (min entry-short (getd date 'open))                    
                 trade (create-position-entry-record-list date-1 -1 short)
                 stop-short 
                 entry-long 
         ))
 
                   
    (when (and (not long)               
               (>= (getd date 'high) entry-long)               
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (create-position-entry-record-list date-1 1 long)                
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
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
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
(defun create-position-entry-record-list (date direction entry)
   (let* ((date-1 (getd date 'ydate))(date-2 (getd date-1 'ydate)))
       (list *data-name* (getd date 'ndate) direction (and entry (my-pretty-price entry))
                              (timing-index date)
                              (timing-index date-1)(timing-index date-2) 
                              (round (slow-stochastic date 21))(round (slow-stochastic date-1 21))
                              (trend-signal date 5)(trend-signal date 15)(trend-signal date 45)
                              (trend-signal date 135)
                              (round (rsi-ave-diff date 14 2))
                              (round (rsi date 9))
                              (my-round (/ (volatility date 7 1) (volatility date 49 1)) 3)
                              (or (car (formation date)) 'FT));;;closes the list
                             
))


;;;;;Now we need first to read the position-trades file(s) and load into the *position-trade-warehouse* hash table

(defvar positions nil)
(defvar position-bin-codes nil)

(defun position-trade-bins (&rest features)
  (let (bin path)
  (setq path (setq path "~/cycles/positiontradewarehouse.dat"))
  (maind-x)(set-cat-list)
  (setq positions nil position-bin-codes nil)(clrhash *position-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record positions)))
       
         
 (if *out-of-sample*
     (setq positions (remove-if #'(lambda (s) (eql (car s) *data-name*)) positions)))      
  
     
;;;;now all the trades are in the list swings.  
;;;we now assign/sort the trades into bins   
;;;basically process a record list into a code or bin
;;;and create a code
   
   (dolist (record positions)
     (setq bin (encode-position-trades record features))
     (pushnew bin position-bin-codes :test #'equal)
     (cond ((gethash bin *position-trade-warehouse*)
            (ifn (member record (gethash bin *position-trade-warehouse*) :test #'equal)
                 (setf (gethash bin *position-trade-warehouse*)
                       (cons record (gethash bin *position-trade-warehouse*)))))
           ((not (gethash bin *position-trade-warehouse*))
             (setf (gethash bin *position-trade-warehouse*)
              (list record)))))
  
    (format T  "~%FEATURES = ~A~%" features)
    (rank-position-bins-by-profit positions) 
     
         
  ))
 

(defun remove-position-trade-market (market)
  (let (trades path)
  (setq path (setq path "~/cycles/positiontradewarehouse.dat"))
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
       
 
;;;returns a list of nine codes 
(defun encode-position-trades (record features) 
  (let (bin-list) 
  
    (dolist (ith features)
     (case ith
 ;;;;Feature 1 with 4 levels ;items 4, 5 , and 6 are the TI index. TI TI-1 TI-2
     (1  (cond ((>= (nth 4 record) 4)(push 1 bin-list))
               ((>= (nth 5 record) 4)(push 1 bin-list))
               ((>= (nth 6 record) 4)(push 1 bin-list))
               (t (push 0 bin-list))))
             
             
 ;;;;Feature 2 with 6 levels ;item 7 is the stochastic with parameter 21            
     (2 (cond ((<= (nth 7 record) 10) (push 3 bin-list))
              ((and (> (nth 7 record) 10)(<= (nth 7 record) 20)) (push 2 bin-list))
              ((and (> (nth 7 record) 20)(<= (nth 7 record) 50)) (push 1 bin-list))
              ((and (> (nth 7 record) 50)(<= (nth 7 record) 80)) (push -1 bin-list))  
              ((and (>= (nth 7 record) 80)(< (nth 7 record) 90)) (push -2 bin-list))
              ((>= (nth 7 record) 90) (push -3 bin-list))))

 ;;;;Feature 3 with 5 levels ;this is the stochastic today less the stochastic yesterday.            
      (3 (cond ((> (- (nth 7 record)(nth 8 record)) 10) (push 2 bin-list))
               ((> (- (nth 7 record)(nth 8 record))  0)(push 1 bin-list))
             ((< (- (nth 7 record)(nth 8 record)) -10) (push -2 bin-list))
             ((< (- (nth 7 record)(nth 8 record)) 0) (push -1 bin-list))
             ((= (nth 7 record)(nth 8 record)) (push 0 bin-list))))

;;;Feature 4 with 5 levels; This is the TS5        
      (4 (cond ((eql (nth 9 record) 'DN) (push -1 bin-list))
             ((eql (nth 9 record) 'CD) (push -2 bin-list))
             ((eql (nth 9 record) 'UP) (push 1 bin-list))
             ((eql (nth 9 record) 'CU) (push 2 bin-list))
             ((eql (nth 9 record) 'FT) (push 0 bin-list))
             ) )     
;;;Feature 5 with 5 levels; This is TS15          
     (5  (cond ((eql (nth 10 record) 'DN) (push -1 bin-list))
             ((eql (nth 10 record) 'CD) (push -2 bin-list))
             ((eql (nth 10 record) 'UP) (push 1 bin-list))
             ((eql (nth 10 record) 'CU) (push 2 bin-list))
             ((eql (nth 10 record) 'FT) (push 0 bin-list))
             ) )
              
;;;Feature 6 with 5 levels; This is TS45         
    (6   (cond ((eql (nth 11 record) 'DN) (push -1 bin-list))
             ((eql (nth 11 record) 'CD) (push -2 bin-list))
             ((eql (nth 11 record) 'UP) (push 1 bin-list))
             ((eql (nth 11 record) 'CU) (push 2 bin-list))
             ((eql (nth 11 record) 'FT) (push 0 bin-list))
             ) )
 ;;;Feature 7 with 5 levels; This is TS135         
    (7   (cond ((eql (nth 12 record) 'DN) (push -1 bin-list))
             ((eql (nth 12 record) 'CD) (push -2 bin-list))
             ((eql (nth 12 record) 'UP) (push 1 bin-list))
             ((eql (nth 12 record) 'CU) (push 2 bin-list))
             ((eql (nth 12 record) 'FT) (push 0 bin-list))
             ) )            
;;;;Feature 8 with 5 levels ; This is RSD             
      (8  (cond ((> (nth 13 record) 10)(push 2 bin-list))
                ((> (nth 13 record) 0)(push 1 bin-list))
                ((< (nth 13 record) -10)(push -2 bin-list))
                ((< (nth 13 record) 0)(push -1 bin-list))              
                ((zerop (nth 13 record))(push 0 bin-list))))

;;;;Feature 9 with 6 levels ;item 14 is the RSI with parameter 9            
     (9 (cond ((<= (nth 14 record) 10) (push 3 bin-list))
              ((and (> (nth 14 record) 10)(<= (nth 14 record) 20)) (push 2 bin-list))
              ((and (> (nth 14 record) 20)(<= (nth 14 record) 50)) (push 1 bin-list))
              ((and (> (nth 14 record) 50)(<= (nth 14 record) 80)) (push -1 bin-list))  
              ((and (>= (nth 14 record) 80)(< (nth 14 record) 90)) (push -2 bin-list))
              ((>= (nth 14 record) 90) (push -3 bin-list))))           
             
           

 ;;;;Feature 3 with 10 levels ;this is the HT ratio of volatility            
      (10 (cond ((< (nth 15 record) .60) (push 5 bin-list))
             ((and (>= (nth 15 record) .60)(< (nth 15 record) .70)) (push 4 bin-list))
             ((and (>= (nth 15 record) .70)(< (nth 15 record) .80)) (push 3 bin-list))
             ((and (>= (nth 15 record) .80)(< (nth 15 record) .90)) (push 2 bin-list))
             ((and (>= (nth 15 record) .90)(< (nth 15 record) 1.00)) (push 1 bin-list))
             ((and (>= (nth 15 record) 1.00)(< (nth 15 record) 1.10))(push -1 bin-list))
             ((and (>= (nth 15 record) 1.10)(< (nth 15 record) 1.20)) (push -2 bin-list))
             ((and (>= (nth 15 record) 1.20)(< (nth 15 record) 1.30)) (push -3 bin-list))
             ((and (>= (nth 15 record) 1.30)(< (nth 15 record) 1.40))(push -4 bin-list))
             ((>= (nth 15 record) 1.4)(push -5 bin-list))))

     
;;;Feature 11 with 17 levels is the formation                 
       (11 (cond ((eql (nth 16 record) 'DB1)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB2)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB3)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB4)
                  (push 1 bin-list))
                 ((eql (nth 16 record) 'DB5)
                  (push 2 bin-list))
                 ((eql (nth 16 record) 'DB6)
                  (push 2 bin-list))
                 ((eql (nth 16 record) 'DB7)
                  (push 2 bin-list))
                 ((eql (nth 16 record) 'DB8)
                  (push 2 bin-list))
                 ((eql (nth 16 record) 'DS1)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS2)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS3)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS4)
                  (push -1 bin-list))
                 ((eql (nth 16 record) 'DS5)
                  (push -2 bin-list))
                 ((eql (nth 16 record) 'DS6)
                  (push -2 bin-list))
                 ((eql (nth 16 record) 'DS7)
                  (push -2 bin-list))
                 ((eql (nth 16 record) 'DS8)
                  (push -2 bin-list))
                 (t (push 0 bin-list))
                    ))
       
       
                ));;;closes the case and the dolist over features
                
      (reverse bin-list)            
 ))          


(defun rank-position-bins-by-accuracy ()
  (let (contents result accuracy-list)
    (dolist (ith position-bin-codes)
     (setq contents
          (gethash ith *position-trade-warehouse*))
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
(defun rank-position-bins-by-expected-value ()
  (let (contents expected-value-list result (path1 "~/cycles/position-expected-value.dat"))
   (dolist (ith position-bin-codes)
     (setq contents
           (gethash ith *position-trade-warehouse*))
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
 
   
(defun rank-position-bins-by-profit (positions)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith position-bin-codes)
     (setq contents (gethash ith *position-trade-warehouse*))
     (setq result 0 counter 0)
     
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)          
         (setq result (+ result (nth 19 kth)))
         (if (plusp (nth 19 kth)) (incf counter))
         
         ) ;;;closes dolist over contents
      (if (plusp result) (setq num-in-winning-bins (+ num-in-winning-bins (length contents)))) 
          
 
     (setq profit-list
      (cons (list result (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over bin-codes
   
    (dolist (kth positions)
       (if (plusp (nth 19 kth)) (setq all-winners (+ all-winners (nth 19 kth)))
           (setq all-losers (+ all-losers (nth 19 kth))))) 
   
     (setq counter 0)
     (dolist (jth profit-list)
         (if (plusp (car jth))
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
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%~%" (my-round (/ only-one (length position-bin-codes)) 1))
      
    ; (if bin-codes (round (/ winners (length bin-codes))) 0)
    winners
 ))  

(defun display-position-bin (bin)
  (let (contents)
     (setq contents (gethash bin *position-trade-warehouse*))     
     (dolist (ith contents)
       (print ith))))



;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse 
(defun bin-classifier-positions (date features)
  (let (record bin (result 0) (counter 0) contents signal
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))
  
    (setq record (create-position-entry-record-list date nil nil))
    (setq record (append record '(nil nil nil)))
    
  
  (setq bin (encode-position-trades record features))
  (setq contents (gethash bin *position-trade-warehouse*))
   
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
         
  (cond ((and (plusp results-long)(plusp results-short)) (setq signal 'OK))
        ((plusp results-long) (setq signal 'UP))
        ((plusp results-short) (setq signal 'DOWN))
        ((and (<= results-long 0)(<= results-short 0)) (setq signal 'AVOID))
        ((not contents) (setq signal 'UNIQUE)))
  
  (values signal longs results-long (if (zerop longs) 0 (/ num-winners-long longs)) 
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
))

  
;;;requires a base features list       
(defun positions-leave-one-out (base-features )
  (let (winners-list (result 0))
  (apply #'position-trade-bins base-features)
  
  (dolist (ith base-features)
    (setq result (apply #'position-trade-bins (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 )) 
;;;requires a candidate list to add to the base features         
(defun positions-add-one-in (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'position-trade-bins base-features)
  
  (dolist (ith candidate-features)
    (setq result (apply #'position-trade-bins (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))  
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            
;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for position trades to decide to trade or not
;;;;assumes (position-trade-bins date features) has already been run
(defun find-best-position-trade (tdate &optional (output T)(output1 T) (date1 nil)) 
  (let* ((ctr 0) rollover entry-short entry-long  long short 
         cover-long cover-short (ave4 (ave tdate 5 'pivot))  directive1  
        prev-signal action stop-long stop-short risk-short risk-long  
        (VT (make-hash-table)) signal longs long-gains long-acc shorts short-gains short-acc trade-direction       
        )
      (declare (ignore  ctr))
          
  (setf (gethash tdate VT)(volatility-log tdate 60 1)) ;;volatility over the past three months
                                       
   (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
          (bin-classifier-positions tdate *position-features*))
   (multiple-value-setq (prev-signal ctr) (vsignals tdate 18 2.53 7))
    
   (setq trade-direction (cond ((and (member signal '(OK UP))(eql prev-signal 'SELL)
                                      (> longs 0)
                                      (>= (/ long-gains longs) 100)
                                      ) 'UP)
                               ((and (member signal '(OK DOWN))(eql prev-signal 'BUY)
                                      (> shorts 0)
                                      (>= (/ short-gains shorts) 100)
                                      ) 'DN)
                               (t 'FT)))
    
    (setq cover-short (exp (- (log ave4) (* *objective-factor-position* (gethash tdate VT)))) 
          cover-long (exp (+ (log ave4) (* *objective-factor-position* (gethash tdate VT)))) 
                              )   
    
    (multiple-value-setq (entry-short entry-long)(vprices tdate 18 2.53 7))
    
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
                     prev-signal signal trade-direction )
  
    
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
                 )
             
            ((equal action "NOT SHORT")
             (write-xml-record tdate "tblTradeRecs" 'POSITION 'LONG date1 entry-long stop-long cover-long output1) 
                )
                     
            ((equal action "NOT LONG") 
             (write-xml-record tdate "tblTradeRecs" 'POSITION 'SHORT date1 entry-short stop-short cover-short output1)
               ))
                
       (cond ((eql (cadr (assoc *data-name* *open-positions*)) 'long)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil stop-long cover-long output1)
                  )                    
             ((eql (cadr (assoc *data-name* *open-positions*)) 'short)
              (write-xml-record tdate "tblTradeRecs" 'OPENP 'SHORT date1 nil stop-short cover-short output1)
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
(defun position-simulation-test (market date2 num &optional (features *position-features*))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade 
       risk risk-long risk-short  signal longs long-gains long-acc
       shorts short-gains short-acc prev-signal ctr date-1 
       (outfile (string-append "~/cycles/" (format nil "~S" market) "position-summary.dat"))
       (AT (make-hash-table)) (VT (make-hash-table))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "position-simulation.dat")))
      (declare (ignore  short-acc long-acc ctr))
    
   (apply #'position-trade-bins features)  
   (set-market market)
      
   (setq date (add-mkt-days date2 (- num)))
  ; (if  (probe-file path1)
  ;      (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
    
    (cond ((gethash  date  AT))
          (t (setf (gethash date AT) (ave date 5 'pivot))))      
  
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months  
                      
   (multiple-value-setq (entry-short entry-long)
        (vprices date 18 2.53 7)) 
 
      
   (setq entry-short (min entry-short (getd date 'close)) entry-long (max entry-long (getd date 'close)))
    
   (setq ave4 (gethash date AT) 
       cover-short  (exp (- (log ave4) (* *objective-factor-position* (gethash date VT))))                     
       cover-long (exp (+ (log ave4) (* *objective-factor-position* (gethash date VT))))) 
 
   (setq risk-short  (* *stop-loss-swing* (abs (- entry-short (n-day-high date 7 'close))) 
                          ))                          
   (setq risk-long  (* *stop-loss-swing* (abs (- entry-long  (n-day-low date 7 'close)))
                       ))
                                               
   (setq risk (max risk-long risk-short))   
  
   (setq date-1 date date (add-mkt-days date 1)) 
    
        
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 ;cover-long (+ cover-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))
    
    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
    						 ;cover-short (+ cover-short (getd date 'rollover))
    						 stop-short (+ stop-short (getd date 'rollover))
    ))
  
  
 
   (when long (setq stop-long (fmax stop-long entry-short)))  
                                                
   (when short (setq stop-short (fmin stop-short entry-long)))
     
                              
 ;;;;check if stopped out of prior position   
                  
   
   (when (and long (<= (getd date 'low) stop-long))       
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil))
          
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)
          (setq trade nil short nil stop-short nil))
                      
  ;;;check if met objective       
     (when (and long (> (getd date 'high) cover-long))
                        
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))
            
      (when (and short (< (getd date 'low) cover-short))
                            
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))    

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(<= (getd date 'low) entry-short)
                (<= (* risk (index-point-value)) *max-position-risk*))
           (and (not long) (>= (getd date 'high) entry-long)
                (<= (* risk (index-point-value)) *max-position-risk*)))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-positions date-1 features))
       (multiple-value-setq (prev-signal ctr)(vsignals date-1 18 2.53 7)))


;;;check if new entry          
  
    (when (and (not short)              
               (<= (getd date 'low) entry-short)
               (member signal '(OK DOWN)) 
               (eql prev-signal 'BUY)(> shorts 0)
               (>= (/ short-gains shorts) 100)  
               (<= (* risk (index-point-value)) *max-position-risk*)              
               )
          (setq short (min entry-short (getd date 'open))                    
                 trade (list date 'short short)
                 stop-short 
                  (+ entry-short risk-short) 
                
         ))
 
                   
    (when (and (not long)               
               (>= (getd date 'high) entry-long)
               (member signal '(OK UP))
               (eql prev-signal 'SELL)(> longs 0)
               (>= (/ long-gains longs) 100)             
               (<= (* risk (index-point-value)) *max-position-risk*)                       
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long) 
                 stop-long 
                   (- entry-long risk-long)  
               
              ))
                 
 
 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) entry-short))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
     
             
    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))            
          
           
    (when  (and short (or (< (getd date 'open)(getd date 'close))
                          (>= (getd date 'high) entry-long))
               (>= (getd date 'high) stop-short))         
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))
           
     
    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           ))       
              
           
 ;;;check if met objective on day of entry      

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil)))    
           
          
   );;;closes the dotimes
 
      
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)    
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D  $/CONTRACT= ~D"
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
     );close the format   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   ));;;closes the let and the defun       
      

;;;;;;;;Contra-trend day trade system simulation

(defun contradaytrade-simulation-test (market date2 num 
          &optional (extent 4)(multiplier *entry-factor*) (features *day-features*)) ; (out-of-sample nil))
 (let (date trades long short cover-long cover-short entry-long entry-short  
       risk-short risk-long  date-1 signal longs long-gains shorts short-gains
       ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short 
       ave3 risk long-acc short-acc (VT (make-hash-table))
       (outfile (string-append "~/cycles/" (format nil "~S" market) "contraday-summary.dat"))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "contraday-simulation.dat")))    
   (declare (ignore long-acc short-acc signal))
   
;   (when out-of-sample 
;      (apply #'day-trade-bins features)
;      (setq swings (remove-if #'(lambda (s) (eql (car s) market)) swings))
   
   (apply #'day-trade-bins features)
   (set-market market)
   
   (setq date (add-mkt-days date2 (- num)))

 
 ;;;;from date1 to date2
 (dotimes (ith num)

         
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months
 
   (multiple-value-setq (entry-long entry-short)(vprices date extent multiplier 1))   
   
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
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-daytrades date-1 features))
       )
      
;;;check if new entry   
      
   (when (and (not short)             
              (<= (* risk (index-point-value)) *max-day-risk*) 
              (> (getd date 'high) entry-short)               
              (> longs 0)
              (<= (/ long-gains longs) -175)                          
                 )
          (setq short (max (getd date 'open) entry-short)                     
                short-trade (list date 'short short)
                stop-short  (+ short risk) 
                 ))
 
                 
    (when (and (not long) 
               (<= (* risk (index-point-value)) *max-day-risk*) 
               (< (getd date 'low) entry-long)               
               (> shorts 0)
               (<= (/ short-gains shorts) -175)                       
             )
                                         
           (setq long (min (getd date 'open) entry-long)                     
                 trade (list date 'long long) 
                 stop-long (- long risk)  
                 ))
              
            
 ;;;check if stopped out on same day 

   (when (and long stop-long                                   
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
  
    (when (and short stop-short                                 
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date 'exit stop-short (- short stop-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
  

;;;check if met objective  before end of day   low before high  
     (when (and long  (> (getd date 'high) cover-long)
                (<= (getd date 'open)(getd date 'close)))                              
            (push (- cover-long long) trades)           
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))

;;;high before low            
      (when (and short (< (getd date 'low) cover-short)
                (>= (getd date 'open)(getd date 'close)))           
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))   

           
;;;check if met exit criteria on day of entry (exit at end of day)     
     (when long                
            (setq cover-long (getd date 'close))                   
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
            
      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date 'exit cover-short (- short cover-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))    
                
           
   );;;closes the dotimes
  
  
     
   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
    
  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
 
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
   (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2) 
   (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D  $/CONTRACT= ~D"
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
     (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
        ));;;closes the with-open-file
  );;;closes the when outfile  
   
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)  
   ));     


;;;;This produces the orders for the next day.
;;;this version has fade breakout systems
;;;;
(defun find-best-day-trade9 (tdate &optional (output T)(output1 T)(date1 nil)) 
  (let (risk risk-short risk-long  stop-short stop-long  entry-long entry-short 
        trade-direction (VT (make-hash-table)) 
        cover-long cover-short ave4 action directive1  
        )    
    
       (declare (special longs shorts long-gains short-gains long-acc short-acc))                            
  
      
   (if (and (> longs 0)(<= (/ long-gains longs) -175)) (push 'DN trade-direction))
   (if (and (> shorts 0)(<= (/ short-gains shorts) -175)) (push 'UP trade-direction))
  
   (setf (gethash tdate VT)(volatility-log tdate 60 1))
    (multiple-value-setq (entry-long entry-short) (vprices tdate 4 *entry-factor* 1)) 
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
         
         
;;;;;;;contra swing trade simulation  

  
;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage   
(defun contraswing-simulation-test (market date2 num 
                      &optional (extent 15) (multiplier *entry-factor-swing*)(dur 3) (features *swing-features*))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade 
       risk risk-long risk-short  signal longs long-gains long-acc
       shorts short-gains short-acc  date-1 prev-signal ctr
       (outfile (string-append "~/cycles/" (format nil "~S" market) "contraswing-summary.dat"))
       (AT (make-hash-table)) (VT (make-hash-table))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "contraswing-simulation.dat")))
      (declare (ignore  signal short-acc long-acc ))
    
   (apply #'swing-trade-bins features)  
   (set-market market)
      
   (setq date (add-mkt-days date2 (- num)))
 
 
 ;;;;from date1 to date2
 (dotimes (ith num)
    
    (cond ((gethash  date  AT))
          (t (setf (gethash date AT) (ave date dur 'pivot))))      
  
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months  
                      
   (multiple-value-setq (entry-long entry-short)
        (vprices date extent multiplier dur)) 
    (multiple-value-setq (prev-signal ctr)(vsignals date extent multiplier dur))
      
   (setq entry-short (max entry-short (getd date 'close)) entry-long (min entry-long (getd date 'close)))
    
   (setq ave4 (gethash date AT) 
       cover-short  (exp (- (log ave4) (* *objective-factor-swing* (gethash date VT))))                     
       cover-long (exp (+ (log ave4) (* *objective-factor-swing* (gethash date VT))))
                             ) 
   (if long (setq cover-long (min cover-long entry-short)))
   (if short (setq cover-short (max cover-short entry-long)))
 
 
   (setq risk-short  (* (/ 1 *stop-loss-swing*) (abs (- entry-short (n-day-low date dur 'close))) 
                          ))
                          
   (setq risk-long  (* (/ 1 *stop-loss-swing*) (abs (- entry-long  (n-day-high date dur 'close)))
                       ))
                       
                        
   (setq risk (max risk-long risk-short));;;
   
  
   ;(format T "date= ~A param1= ~A Risk-short= ~A Risk-long=~A ~%" date param1 risk-short risk-long) 
   
   (setq date-1 date date (add-mkt-days date 1)) 
    
        
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 cover-long (+ cover-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))
    
    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
    						 cover-short (+ cover-short (getd date 'rollover))
    						 stop-short (+ stop-short (getd date 'rollover))
    ))
  
  
 
   (when long (setq stop-long (fmax stop-long (- entry-long (/ risk-long (+ 1 (- ctr)))))))  
                                                
   (when short (setq stop-short (fmin stop-short (+ entry-short (/ risk-short (+ 1 (- ctr)))))))
     
                              
 ;;;;check if stopped out of prior position                  
   
   (when (and long (<= (getd date 'low) stop-long))       
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade-long extended-trades)
          (setq trade-long nil long nil stop-long nil))
          
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)
          (setq trade nil short nil stop-short nil))
                      
  ;;;check if met objective       
     (when (and long (> (getd date 'high) cover-long))
                        
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))
            
      (when (and short (< (getd date 'low) cover-short))
                            
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))    

;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(> (getd date 'high) entry-short)
                (<= (* risk (index-point-value)) *max-swing-risk*))
           (and (not long) (< (getd date 'low) entry-long)
                (<= (* risk (index-point-value)) *max-swing-risk*)))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-swings date-1 features))
       ;(multiple-value-setq (prev-signal ctr)(vsignals date-1 15 1.628 3))
       )


;;;check if new entry          
  
    (when (and (not short)              
               (> (getd date 'high) entry-short)
               (eql prev-signal 'SELL)
               (> longs 0)
               (<= (/ long-gains longs) -175)  
               (<= (* risk (index-point-value)) *max-swing-risk*)              
               )
          (setq short (max entry-short (getd date 'open))                    
                 trade (list date 'short short)
                 stop-short 
                  (+ entry-short risk-short) 
                
         ))
 
                   
    (when (and (not long)               
               (< (getd date 'low) entry-long)
               (eql prev-signal 'BUY)
               (> shorts 0)
               (<= (/ short-gains shorts) -175)             
               (<= (* risk (index-point-value)) *max-swing-risk*)                       
               )
           (setq long (min entry-long (getd date 'open))
                 trade-long (list date 'long long) 
                 stop-long 
                   (- entry-long risk-long)  
               
              ))
                 
 
 ;;;check if stopped out on same day of entry
   (when (and long (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))    
           
    (when  (and short (>= (getd date 'high) stop-short))         
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))
           
 
 ;;;check if met objective on day of entry      

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil)))    
           
          
   );;;closes the dotimes
 
      
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)    
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D  $/CONTRACT= ~D"
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
     );close the format   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   ));;;closes the let and the defun       
      
;;;;;;;this is the plain vanilla swing trade simulation 

(defun swingbreakout-simulation-test (market date2 num 
          &optional (extent 15)(multiplier *entry-factor-swing*)(dur 3))
 (let (date stop-long stop-short trades long short  trade-long  entry-short entry-long
       ave-win ave-loss losers winners extended-trades trade prev-signal ctr risk risk-short risk-long         
       (output (string-append "~/cycles/" (format nil "~S" market) "swingbreakout-summary.dat"))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "swingbreakout-simulation.dat")))
       (declare (ignore ctr))
   (maind-x)(set-cat-list) (set-market market)  
   
   (setq date (add-mkt-days date2 (- num)))
  
 
 ;;;;from date1 to date2
 (dotimes (ith num)
       
   (multiple-value-setq (entry-short entry-long) (vprices date extent multiplier dur))      
   (multiple-value-setq (prev-signal ctr)(vsignals date extent multiplier dur))
   
   
   (setq risk-short  (* *stop-loss-swing* (abs (- entry-short (n-day-high date dur 'close)))))                          
   (setq risk-long  (* *stop-loss-swing* (abs (- entry-long  (n-day-low date dur 'close)))))                       
                        
   (setq risk (max risk-long risk-short))
 
   (setq  date (add-mkt-days date 1))
         
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover))
    
    ))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
     						 stop-short (+ stop-short (getd date 'rollover))
    ))
    
  ;;;;check if stopped out of prior position   
   
   (when long (setq stop-long (max stop-long entry-short)))
   (when short (setq stop-short (min stop-short entry-long))) 
          

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

           
;;;check if new entry          
  
    (when (and (not short)              
               (<= (getd date 'low) entry-short)
               (eql prev-signal 'BUY)               
               )
          (setq short (min entry-short (getd date 'open))                    
                 trade (list date 'short short)    
                 stop-short (+ entry-short risk) 
         ))
 
                   
    (when (and (not long)               
               (>= (getd date 'high) entry-long) 
               (eql prev-signal 'SELL)              
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long)                
                 stop-long (- entry-long risk)  
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
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
  (with-open-file (str output :direction :output :if-exists :supersede :if-does-not-exist :create)    
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
          
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
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
     ));close the format and with-open-file   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~S~%" ith) 
         
    ));;closes the dolist and with-open-file
   
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   ));;;closes the let and the defun        


(defun positionbreakout-simulation-testA (date  num extent multiplier dur)
  (let ((profit 0) (total-profit 0))
  (terpri)
  (dolist (ith *position-list*)  
    (setq profit (swingbreakout-simulation-test ith date num extent multiplier dur))
    (format T "~A   ~D~%" ith profit) 
    (setq total-profit (+ total-profit profit)))       
   total-profit
))  

(defun swingbreakout-simulation-testA (date  num extent multiplier dur)
  (let ((profit 0) (total-profit 0))
  (terpri)
  (dolist (ith *swing-list*)  
    (setq profit (swingbreakout-simulation-test ith date num extent multiplier dur))
    (format T "~A   ~D~%" ith profit) 
    (setq total-profit (+ total-profit profit)))       
   total-profit
))  


(defun daybreakout-simulation-testA (date num extent multiplier)
  (let ((profit 0) (total-profit 0) (num-trades 0) (total-num-trades 0))
  (terpri)
  (dolist (ith *day-list*)  
    (multiple-value-setq (profit num-trades) (daybreakout-simulation-test ith date num extent multiplier))
    (format T "~A   ~D  ~D~%" ith profit num-trades) 
    (setq total-profit (+ total-profit profit) total-num-trades (+ total-num-trades num-trades)))       
  (values total-profit (round (/ total-profit total-num-trades)))
))  

;;;;this tests the plain-vanilla day trade volatility breakout system   
(defun daybreakout-simulation-test (market date2 num &optional (extent 4)(multiplier *entry-factor*))
 (let (date trades long short cover-long cover-short  
        risk-short risk-long   risk long-entry short-entry 
        ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short 
       (output (string-append "~/cycles/" (format nil "~S" market) "daybreakout-summary.dat"))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "daybreakout-simulation.dat")))
    
   (maind-x)(set-cat-list)(set-market market)

   (setq date (add-mkt-days date2 (- num)))

 ;  (setq date-1 (getd date 'ydate))
 
 ;;;;from date1 to date2
 (dotimes (ith num)
     

   (multiple-value-setq (short-entry long-entry)(vprices date extent multiplier 1))
   
   (setq risk-short (abs (* *stop-loss-day* (- short-entry (getd date 'close)))))
   (setq risk-long  (abs (* *stop-loss-day* (- long-entry (getd date 'close)))))
  
   (setq risk (max risk-long risk-short))
  
   (setq  date (add-mkt-days date 1))
    
           
   (when (getd date 'rollover) (setq  long-entry (+ long-entry (getd date 'rollover))))
  
   (when (getd date 'rollover) (setq short-entry (+ short-entry (getd date 'rollover))))    	                         
    						 
    
      
;;;check if new entry   
      
   (when (and (not short)             
              (<= (* risk (index-point-value)) *max-day-risk*) 
              (<= (getd date 'low) short-entry)               
                                                       
              )
          (setq short (min (getd date 'open) short-entry)                     
                short-trade (list date 'short short)
                stop-short (+ short risk) 
                 ))
 
                 
    (when (and (not long) 
               (<= (* risk (index-point-value)) *max-day-risk*) 
               (>= (getd date 'high) long-entry)               
                                                   
             )                                         
           (setq long (max (getd date 'open) long-entry)                     
                 trade (list date 'long long)
                 stop-long (- long risk)  
                 ))
              
            
 ;;;check if stopped out on same day 

   (when (and long stop-long  (> (getd date 'open)(getd date 'close))
                                 
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date (my-pretty-price stop-long)
                                        (round (* (index-point-value) (- stop-long long))))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
     (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                (or (<= (getd date 'low) short-entry)
                    (<= (getd date 'close) stop-long)))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date (my-pretty-price stop-long)
                                         (round (* (index-point-value) (- stop-long long))))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))       
    (when (and short stop-short (< (getd date 'open)(getd date 'close))
                                
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price stop-short)
                                           (round (* (index-point-value) (- short stop-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
     (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                (or (>= (getd date 'high) long-entry)
                    (>= (getd date 'close) stop-short)))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price stop-short)
                                       (round (* (index-point-value) (- short stop-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))

;;;check if met objective  before end of day     
;     (when (and long  (> (getd date 'high) cover-long))              
;            (push (- cover-long long) trades) 
;            (setq trade (append trade (list date (my-pretty-price cover-long)
;                                         (round (* (index-point-value) (- cover-long long))))))
;            (push trade extended-trades)
;            (setq trade nil long nil stop-long nil))
            
;      (when (and short (< (getd date 'low) cover-short))
;           (push (- short cover-short) trades)
;           (setq short-trade (append short-trade (list date (my-pretty-price cover-short)
;                                         (round (* (index-point-value) (- short cover-short))))))
;           (push short-trade extended-trades)
;           (setq short-trade nil short nil stop-short nil))   

           
;;;check if met exit criteria to exit at end of day of entry for day trade     
     (when long                
            (setq cover-long (getd date 'close))                   
            (push (- cover-long long) trades)
            (setq trade (append trade (list date (my-pretty-price cover-long)
                                           (round (* (index-point-value) (- cover-long long))))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
            
      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq short-trade (append short-trade (list date (my-pretty-price cover-short)
                                                (round (* (index-point-value) (- short cover-short))))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil))    
                
           
   );;;closes the dotimes
  
  
     
   ;;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn.
  (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))    
     
  (with-open-file (str output :direction :output :if-exists :supersede :if-does-not-exist :create) 
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )
  
   (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
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
       ));;;closes the with-open-file
   
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)    
       (format stream "~S~%" ith) 
    ));;closes the dolist and with-open-file
   (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades)  (round (/ (* (list-sum trades) (or (index-point-value) 1)) (length trades)))
            trades)
 )); closes the let and defun     
  
  
;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage   
(defun swing-simulation-test (market date2 num &optional (features *swing-features*))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-long entry-short
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade 
       risk risk-long risk-short  signal longs long-gains long-acc trading-dates (trade-time 0)
       shorts short-gains short-acc prev-signal ctr date-1 record (running-sum 0)
       (outfile (string-append "~/cycles/" (format nil "~S" market) "swing-summary.dat"))
       (AT (make-hash-table)) (VT (make-hash-table))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "swing-simulation.dat"))
       (path2 (string-append "~/cycles/" (format nil "~S" market) "swing-diary.dat")))
      (declare (ignore  short-acc long-acc ctr))
    
   (apply #'swing-trade-bins features)  
   (set-market market)
      
   (setq date (add-mkt-days date2 (- num)))
   (setq record (list date (getd date 'close) 0 0 0))
    (push record trading-dates)
  ; (if  (probe-file path1)
  ;      (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
    
    (cond ((gethash  date  AT))
          (t (setf (gethash date AT) (ave date 3 'pivot))))      
  
   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months  

                      
   (multiple-value-setq (entry-short entry-long)
        (vprices date 15 *entry-factor-swing* 3)) 
      
   (setq entry-short (min entry-short (getd date 'close)) entry-long (max entry-long (getd date 'close)))
    
   (setq ave4 (gethash date AT) 
       cover-short  (exp (- (log ave4) (* *objective-factor-swing* (gethash date VT))))                     
       cover-long (exp (+ (log ave4) (* *objective-factor-swing* (gethash date VT))))
                             )
 
 
   (setq risk-short  (* *stop-loss-swing* (abs (- entry-short (n-day-high date 3 'close))) 
                          ))
                          
   (setq risk-long  (* *stop-loss-swing* (abs (- entry-long  (n-day-low date 3 'close)))
                       ))
                       
                        
   (setq risk (max risk-long risk-short));;;
   
  
   ;(format T "date= ~A param1= ~A Risk-short= ~A Risk-long=~A ~%" date param1 risk-short risk-long) 
   
   (setq date-1 date date (add-mkt-days date 1)) 
   (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1))  
   
;;;if long or short and not entry or exits  
      (if long (setf (nth 3 record) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (nth 3 record)(- (getd date-1 'close)(getd date 'close))))
       
    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))
        					 entry-long (+ entry-long (getd date 'rollover))
    						 ;cover-long (+ cover-long (getd date 'rollover))
    						 stop-long (+ stop-long (getd date 'rollover)))
          (setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))
    						 entry-short (+ entry-short (getd date 'rollover))
    						 ;cover-short (+ cover-short (getd date 'rollover))
    						 stop-short (+ stop-short (getd date 'rollover)))
           (setf (nth 3 record) (+ (nth 3 record) (getd date 'rollover))))
  
  
 
   (when long (setq stop-long (fmax stop-long entry-short)))  
                                                
   (when short (setq stop-short (fmin stop-short entry-long)))
     
                              
 ;;;;check if stopped out of prior position   
                  
   
   (when (and long (<= (getd date 'low) stop-long))       
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
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
                (<= (* risk (index-point-value)) *max-swing-risk*))
           (and (not long) (>= (getd date 'high) entry-long)
                (<= (* risk (index-point-value)) *max-swing-risk*)))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-swings date-1 features))
       (multiple-value-setq (prev-signal ctr)(vsignals date-1 15 *entry-factor-swing* 3)))


;;;check if new entry          
  
    (when (and (not short)              
               (<= (getd date 'low) entry-short)
               (member signal '(OK DOWN)) 
               (eql prev-signal 'BUY)(> shorts 0)
               (>= (/ short-gains shorts) 100)  
               (<= (* risk (index-point-value)) *max-swing-risk*)              
               )
          (setq short (min entry-short (getd date 'open))                    
                 trade (list date 'short short)
                 stop-short 
                  (+ entry-short risk-short)) 
           (setf (nth 2 record) -1)      
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(/ 75 (index-point-value)))))
            )
 
                   
    (when (and (not long)               
               (>= (getd date 'high) entry-long)
               (member signal '(OK UP))
               (eql prev-signal 'SELL)(> longs 0)
               (>= (/ long-gains longs) 100)             
               (<= (* risk (index-point-value)) *max-swing-risk*)                       
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long) 
                 stop-long 
                   (- entry-long risk-long))  
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (- (getd date 'close) long (/ 75 (index-point-value)))))
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
           (setf (nth 3 record) (+ (nth 3 record)(- stop-long long)))
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
 
     (with-open-file (stream path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith trading-dates)
        (format stream "~A\,~F\,~D\,~D,~D~%" 
           (first ith) (second ith) (third ith) (round (* (index-point-value)(nth 3 ith)))
           (round (* (index-point-value) (nth 4 ith))))  
     )) 
      
  ;;;apply commission of $75 per round turn and slippage of 0 ticks per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 75 (index-point-value)) (* 0 (index-tick-size)))) trades))
      
 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)    
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D  $/CONTRACT= ~D"
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
     );close the format   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   ));;;closes the let and the defun       
      
    
  
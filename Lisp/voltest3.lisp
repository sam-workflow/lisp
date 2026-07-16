;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)



;;;moved to utils5
;(defparameter *ewaves-list* '(dj.d1b sp.d1b nd.d1b ru.d1b us.d1b
;;    cl.d1b gc.d1b si.d1b e1.d1b jy.d1b))

(defparameter *ewaves-trade-warehouse* (make-hash-table :test #'equal))

;;;;;first need a function to find the primary degree
;;;;must read the .d3b2 file and put trades into separate lists
;;; a trade is a change in direction at a certain degree
;;;only interested in degrees 2 3 4 and 5.
;;;deg2-list is a list of waves at degree 2 one per day.
;;;each list is '(date start-date label degree direction)
;;;then it produces a trade list for each degree 2 3 4 and 5.
;;;then it finds the trading degree. That's the degree with the nearest average time length to 13 market days.

(defun populate-ewaves-trades (path)
  (let (date dates dates1 trading-degree 
       path1 path2  path3 path4 deg2-list deg3-list deg4-list deg5-list market first-degree 
       first-degree-label first-degree-start first-degree-direction fdrl fdrl1 fdrl2 fdrl3
       deg2-trades deg3-trades deg4-trades deg5-trades deg2-ave deg3-ave deg4-ave deg5-ave
       deg2-p&l deg3-p&l deg4-p&l deg5-p&l trading-degree-trades)
     ; (setq deg2-list nil deg3-list nil deg4-list nil deg5-list nil dates nil)
       (maind-x)(set-cat-list)(setq dates1 nil)
;;;first find the market name from the path.   
       (setq path1 (file-namestring path))
       (setq market (read-from-string 
                     (subseq path1 0 (+ (position #\. path1) 4))))
      (set-market market)
      (setq path2 (string-append "~/cycles/" path1 "ewaves-degree-info.dat"))
      (setq path3 (string-append "~/cycles/" path1 "ewavestrades.dat"))
      (setq path4 (string-append "~/cycles/" path1 "ewaveslabels.dat"))
;;;;read the file in line by line
     (if (probe-file path2)(delete-file path2))
     (if (probe-file path3)(delete-file path3))
     (if (probe-file path4)(delete-file path4))
   (with-open-file (str path :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))      
       (push record dates)))        
;;;replaces all spaces with commas in each string record. 
 (setq dates (mapcar #'(lambda(s) (substitute #\, #\space s)) dates))
 
 (setq dates (mapcar #'(lambda(s) (my-split-sequence #\, s)) dates))
 (dolist (ith dates)
    (push (mapcar #'read-from-string ith) dates1))   
   
 (setq dates dates1)
        
     (dolist (ith dates)
       (setq date (nth 0 ith) first-degree (or (nth 4 ith) 0))
       ;(format T "~A ~A~%" date first-degree)
       (setq first-degree-label (nth 3 ith) first-degree-direction (or (nth 5 ith) 0))
       ;(format T "~A ~A~%" first-degree-label first-degree-direction)
       (setq first-degree-start (nth 2 ith) fdrl (cadr (assoc first-degree-label *wavels2*)))
       (setq fdrl1 (cadr (assoc fdrl *wavels2*)) fdrl2 (cadr (assoc fdrl1 *wavels2*)))
       (setq fdrl3 (cadr (assoc fdrl2 *wavels2*)))
       (case first-degree
         ((1 0) (push (list date first-degree-start fdrl 2 first-degree-direction first-degree-label) deg2-list)
            (push (list date first-degree-start fdrl1 3 first-degree-direction fdrl) deg3-list)
            (push (list date first-degree-start fdrl2 4 first-degree-direction fdrl1) deg4-list)
            (push (list date first-degree-start fdrl3 5 first-degree-direction fdrl2) deg5-list))
         (2 (push (list date first-degree-start fdrl 3 first-degree-direction first-degree-label) deg3-list)
            (push (list date first-degree-start fdrl1 4 first-degree-direction fdrl) deg4-list)
            (push (list date first-degree-start fdrl2 5 first-degree-direction fdrl1) deg5-list))
         (3 (push (list date first-degree-start fdrl 4 first-degree-direction first-degree-label) deg4-list)
            (push (list date first-degree-start fdrl1 5 first-degree-direction fdrl) deg5-list))
         (4 (push (list date first-degree-start fdrl 5 first-degree-direction first-degree-label) deg5-list))) ;;;closes the case   
  
;   (dolist (ith dates)
;       (setq date (nth 0 ith) first-degree (or (nth 4 ith) 0))
;       (case first-degree
;         ((1 0) (push (list date nil nil 2 0 nil) deg2-list)
;            (push (list date nil nil 3 0 nil) deg3-list)
;            (push (list date nil nil 4 0 nil) deg4-list)
;            (push (list date nil nil 5 0 nil) deg5-list))
;         (2 (push (list date nil nil 3 0 nil) deg3-list)
;            (push (list date nil nil 4 0 nil) deg4-list)
;            (push (list date nil nil 5 0 nil) deg5-list))
;         (3 (push (list date nil nil 4 0 nil) deg4-list)
;            (push (list date nil nil 5 0 nil) deg5-list))
;         (4 (push (list date nil nil 5 0 nil) deg5-list))) ;;;closes the case  
      
;;;;the first degree is also the number of waves in the list
;;;;each wave is 5 more on the index into the list

;;;;(list date first-date label degree direction ww-label)
  
      (dotimes (jth (1+ first-degree))
      (when (eql jth 2)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg2-list))
       (when (eql jth 3)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg3-list))
        (when (eql jth 4)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg4-list))
        (when (eql jth 5)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg5-list))
            
      ))
;;;now we have a list of waves in order of dates for each of degrees 2 3 4 and 5      
      
;;;check to see if today's direction for degrees 2 3 4 are the same as yesterday's
     (setq deg2-list (reverse deg2-list) deg3-list (reverse deg3-list) deg4-list (reverse deg4-list)
           deg5-list (reverse deg5-list))
;;;(nth 2 s) is the direction                
     (setq deg2-trades (find-trades deg2-list))
     (setq deg2-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg2-trades))
     (setq deg3-trades (find-trades deg3-list))
     (setq deg3-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg3-trades))
     (setq deg4-trades (find-trades deg4-list))
     (setq deg4-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg4-trades))
     (setq deg5-trades (find-trades deg5-list))
     (setq deg5-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg5-trades))
;;;;each element of the deg-trades is a list
;;;;;(market-name start-date direction start-price label degree ww-label 0 0 0 0 0 0 0 0       
     (setq deg2-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg2-trades))(length deg2-trades)))   
      (setq deg3-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg3-trades)) (length deg3-trades)))
      (setq deg4-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg4-trades))(length deg4-trades))) 
      (setq deg5-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg5-trades))(length deg5-trades))) 
      (setq deg2-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg2-trades))))   
      (setq deg3-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg3-trades))))   
      (setq deg4-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg4-trades)))) 
      (setq deg5-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg5-trades)))) 
;;;find the degree with average time length nearest 13 market days.      
     (setq trading-degree (find-trading-degree deg2-ave deg3-ave deg4-ave deg5-ave))
     
     (with-open-file (stream path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (kth (case trading-degree
                            (2 deg2-list)
                            (3 deg3-list)
                            (4 deg4-list)
                            (5 deg5-list)))
          (format stream "~S~%" (cons *data-name* kth))))
      
;;;add the eight indictors to the trades with the trading degree
     (setq trading-degree-trades (add-indicators (case trading-degree 
                                                   (2  deg2-trades)
                                                   (3  deg3-trades)
                                                   (4  deg4-trades)
                                                   (5  deg5-trades))))      
       
    (with-open-file (str path2 :direction :output :if-exists :supersede :if-does-not-exist :create)  
     (format str "Num deg2 trades= ~D  Ave Deg2 Time Per trade= ~F  Deg2 P&L= ~D ~%"
        (length deg2-trades) (my-round (float deg2-ave) 2) deg2-p&l)
     (format str "Num deg3 trades= ~D  Ave Deg3 Time Per trade= ~F  Deg3 P&L= ~D ~%"
        (length deg3-trades) (my-round (float deg3-ave) 2) deg3-p&l)
     (format str "Num deg4 trades= ~D  Ave Deg4 Time Per trade= ~F  Deg4 P&L= ~D ~%"
        (length deg4-trades) (my-round (float deg4-ave) 2) deg4-p&l)  
     (format str "Num deg5 trades= ~D  Ave Deg5 Time Per trade= ~F  Deg5 P&L= ~D ~%"
        (length deg5-trades) (my-round (float deg5-ave) 2) deg5-p&l)    
         )    
    (with-open-file (stream path3 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith trading-degree-trades)
        (format stream "~S~%" ith)))
  ))

;;;  
(defun find-trades (deg-list)
  (let (ith-1 trade-record trades)
;;;the deg-list is a list of dates with the count info for that day. 
;;;; each item is a list  (date start-date label degree direction) 
  (dolist (ith deg-list)
   
     (when (and ith-1 (neql (nth 4 ith) (nth 4 ith-1)))
           ;(print (car trades))
           (when (car trades)
              
               (setf (nth 17 (car trades)) (getd (car ith) 'ndate)) ;;;adds the exit date 
               (setf (nth 18 (car trades)) (getd (getd (car ith) 'ndate) 'open));;;adds the exit price
               (setf (nth 16 (car trades)) (sub-mkt-dates (nth 1 (car trades)) (nth 17 (car trades))));;adds time-length
               (setf (nth 19 (car trades))    ;;;adds the gain or loss in dollars
                     (round (* (nth 2 (car trades))   ;;;this is the direction -1 0 1
                               (index-point-value)
                               (- (nth 18 (car trades))  ;;;this is the exit price
                                  (nth 3 (car trades)))))) ;;;this is the entry price
                                                     
                           );;;closes the inner when
           
         (setq trade-record (list *data-name* (getd (car ith) 'ndate) (nth 4 ith);;three slots market entry-date direction 
                  (getd (getd (car ith) 'ndate) 'open);;;entry-price
                  (nth 2 ith) (nth 3 ith) (nth 5 ith) ;;;four slots for ewaves info label degree and and ww-label
                   0 0 0 0 0 0 0 0 0;;;nine slots for indicators
                    0 0 0 0))      ;;;four slots for time-length, exit date, price, gain/loss in dollars        
         (push trade-record trades)                         
         ) ;;;this closes the upper when
         (setq ith-1 ith)
    );;;closes the dolist
    (if (zerop (nth 17 (car trades))) (setq trades (cdr trades)))
    
    trades 
    
  ))  ;;;closes the let and defun
  
;;;adds the indicators to the trade-record
(defun add-indicators (trades) 
  (let (date)
  (dolist (jth trades)
     (setq date (getd (nth 1 jth) 'ydate))
     (setf (nth 9 jth)(round (slow-stochastic date 21)))
     (setf (nth 10 jth)(trend-signal date 5))
     (setf (nth 11 jth)(trend-signal date 15))
 
     (setf (nth 12 jth)(trend-signal date 135))
     (setf (nth 13 jth)(my-round (/ (volatility date 4 1) (volatility date 28 1)) 3))
     (setf (nth 14 jth)(day-bar-type date))
     (setf (nth 15 jth)(day-bar-type1 (getd date 'ydate)))
   
     )
   trades
)) 

;;;example of record with 20 slots
;;;;(market entry-date direction entry-price label degree ww-label pw-pattern ct-score
;;;;seven indicator slots
;;;;time-length exit-date exit-price gain/loss)
;;;;(CL.D1B 20100429 1 83.33 2 4 CI ZIG -5.300 36 CU CU DN CD 1.137 13 
;;; 13 20100506 79.63 -3700.0046)

  
(defvar waves nil)
(defvar ewaves-bin-codes nil)  

(defun ewaves-trade-bins (&rest features)
  (let (bin path)
  (setq path "~/cycles/ewavestradewarehouse178.dat")
  (maind-x)(set-cat-list)
  (setq waves nil ewaves-bin-codes nil)(clrhash *ewaves-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record waves)))
      
         
 (if *out-of-sample*
     (setq waves (remove-if #'(lambda (s) (eql (car s) *data-name*)) waves)))      
  
     
;;;;now all the trades are in the list swings.  
;;;we now assign/sort the trades into bins   
;;;basically process a record list into a code or bin
;;;and create a code
   
   (dolist (record waves)
     (setq bin (encode-ewaves-trades record features))
    ; (print bin)
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin *ewaves-trade-warehouse*)
            (ifn (member record (gethash bin *ewaves-trade-warehouse*) :test #'equal)
                 (setf (gethash bin *ewaves-trade-warehouse*)
                       (cons record (gethash bin *ewaves-trade-warehouse*)))))
           ((not (gethash bin *ewaves-trade-warehouse*))
             (setf (gethash bin *ewaves-trade-warehouse*)
              (list record)))))
  
    (format T  "~%FEATURES = ~A~%" features)
    (rank-ewaves-bins-by-profit waves) 
     
         
  ))

;;;returns a list of nine codes 
(defun encode-ewaves-trades (record features) 
  (let (bin-list) 
  
    (dolist (ith features)
     (case ith
 ;;;;number 1 is the direction
     (1 (push (nth 2 record) bin-list));;;adds the direction  
;;;Feature 2 is the label of the wave of trading-degree
     (2 (push (nth 4 record) bin-list))   
;;;Feature 3 is the ww-label of the within wave of the wave of trading-degree
     (3 (push (nth 6 record) bin-list))

;;;feature 4 is the pw-pattern
     (4 (push (nth 7 record) bin-list))
     
;;;feature 5 is the ct-score

     (5 (cond ((> (nth 8 record) 0) 0)
              ((and (> (nth 8 record) -5)(<= (nth 8 record) 0)) -1)
              ((and (> (nth 8 record) -10)(<= (nth 8 record) -5)) -2)
              ((and (> (nth 8 record) -15)(<= (nth 8 record)-10)) -3) 
              ((<= (nth 8 record) -15) -4)))
     
                       
 ;;;;Feature 6 with 6 levels is the stochastic with parameter 21            
     (6 (cond ((<= (nth 9 record) 10) (push 3 bin-list))
              ((and (> (nth 9 record) 10)(<= (nth 9 record) 20)) (push 2 bin-list))
              ((and (> (nth 9 record) 20)(<= (nth 9 record) 50)) (push 1 bin-list))
              ((and (> (nth 9 record) 50)(<= (nth 9 record) 80)) (push -1 bin-list))  
              ((and (>= (nth 9 record) 80)(< (nth 9 record) 90)) (push -2 bin-list))
              ((>= (nth 9 record) 90) (push -3 bin-list))))

;;;Feature 7 with 5 levels; This is the TS5        
      (7 (cond ((eql (nth 10 record) 'DN) (push -1 bin-list))
               ((eql (nth 10 record) 'CD) (push -2 bin-list))
               ((eql (nth 10 record) 'UP) (push 1 bin-list))
               ((eql (nth 10 record) 'CU) (push 2 bin-list))
               ((eql (nth 10 record) 'FT) (push 0 bin-list))
             ) )     
;;;Feature 8 with 5 levels; This is TS15          
     (8  (cond ((eql (nth 11 record) 'DN) (push -1 bin-list))
             ((eql (nth 11 record) 'CD) (push -2 bin-list))
             ((eql (nth 11 record) 'UP) (push 1 bin-list))
             ((eql (nth 11 record) 'CU) (push 2 bin-list))
             ((eql (nth 11 record) 'FT) (push 0 bin-list))
             ) )
              
;;;Feature 9 with 5 levels; This is TS45         
    (9   (cond ((eql (nth 12 record) 'DN) (push -1 bin-list))
             ((eql (nth 12 record) 'CD) (push -2 bin-list))
             ((eql (nth 12 record) 'UP) (push 1 bin-list))
             ((eql (nth 12 record) 'CU) (push 2 bin-list))
             ((eql (nth 12 record) 'FT) (push 0 bin-list))
             ) )
;;;Feature 10 with 5 levels; This is TS135         
    (10   (cond ((eql (nth 13 record) 'DN) (push -1 bin-list))
             ((eql (nth 13 record) 'CD) (push -2 bin-list))
             ((eql (nth 13 record) 'UP) (push 1 bin-list))
             ((eql (nth 13 record) 'CU) (push 2 bin-list))
             ((eql (nth 13 record) 'FT) (push 0 bin-list))
             ) )        


;;;;Feature 9 with 10 levels ;this is the HT ratio of volatility            
;      (9 (cond ((< (nth 12 record) .60) (push 5 bin-list))
;             ((and (>= (nth 12 record) .60)(< (nth 12 record) .70)) (push 4 bin-list))
;             ((and (>= (nth 12 record) .70)(< (nth 12 record) .80)) (push 3 bin-list))
;             ((and (>= (nth 12 record) .80)(< (nth 12 record) .90)) (push 2 bin-list))
;             ((and (>= (nth 12 record) .90)(< (nth 12 record) 1.00)) (push 1 bin-list))
;             ((and (>= (nth 12 record) 1.00)(< (nth 12 record) 1.10))(push -1 bin-list))
;             ((and (>= (nth 12 record) 1.10)(< (nth 12 record) 1.20)) (push -2 bin-list))
;             ((and (>= (nth 12 record) 1.20)(< (nth 12 record) 1.30)) (push -3 bin-list))
;             ((and (>= (nth 12 record) 1.30)(< (nth 12 record) 1.40))(push -4 bin-list))
;             ((>= (nth 12 record) 1.4)(push -5 bin-list))))


       (11 (cond ((< (nth 14 record) .80) (push 2 bin-list))
                ((and (>= (nth 14 record) .80)(< (nth 14 record) .90)) (push 1 bin-list))
                ((and (>= (nth 14 record) .90)(< (nth 14 record) 1.10)) (push 0 bin-list))
                ((and (>= (nth 14 record) 1.10)(< (nth 14 record) 1.20))(push -1 bin-list))
                ((>= (nth 14 record) 1.2)(push -2 bin-list))))


;;;;Feature 10 with 9 levels bar type
      (12 (cond ((eql (nth 15 record) 11) (push 0 bin-list))
               ((eql (nth 15 record) 12) (push 1 bin-list)) 
               ((eql (nth 15 record) 13) (push 2 bin-list)) 
               ((eql (nth 15 record) 21) (push 3 bin-list)) 
               ((eql (nth 15 record) 22) (push 4 bin-list)) 
               ((eql (nth 15 record) 23) (push 5 bin-list)) 
               ((eql (nth 15 record) 31) (push 6 bin-list)) 
               ((eql (nth 15 record) 32) (push 7 bin-list)) 
               ((eql (nth 15 record) 33) (push 8 bin-list)) ))

;;;;;;Feature 11 with 5 levels of yesterday's bar relationship to the previous bar

      (13 (cond ((eql (nth 16 record) 'IN) (push -2 bin-list))
               ((eql (nth 16 record) 'OU) (push 2 bin-list))
               ((eql (nth 16 record) 'DN) (push -1 bin-list))
               ((eql (nth 16 record) 'UP) (push 1 bin-list))
               ((eql (nth 16 record) 'FT) (push 0 bin-list))
                     ))
              
;;;;Feature 12 with 5 levels ; This is RSD             
      (14  (cond ((> (nth 17 record) 10)(push 2 bin-list))
                ((> (nth 17 record) 0)(push 1 bin-list))
                ((< (nth 17 record) -10)(push -2 bin-list))
                ((< (nth 17 record) 0)(push -1 bin-list))              
                ((zerop (nth 17 record))(push 0 bin-list))))
    
      
                ));;;closes the case and the dolist over features
              
      (reverse bin-list)            
 ))          

   
(defun rank-ewaves-bins-by-profit (waves)
  (let (contents (result 0) profit-list (all-winners 0)(all-losers 0) (twr 1.0)
       (winners 0)(losers 0) (counter 0) (only-one 0) (num-in-winning-bins 0))
   (dolist (ith ewaves-bin-codes)
     (setq contents (gethash ith *ewaves-trade-warehouse*))
     (setq result 0 counter 0 twr 1)
     
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)          
         (setq result (+ result (nth 19 kth)))
         (if (plusp (nth 19 kth)) (incf counter))
         (setq twr (* twr (+ 1.0 (/ (* (nth 2 kth)(- (nth 18 kth) (nth 3 kth)))                               
                                    (nth 3 kth)))))
         
         ) ;;;closes dolist over contents
         
      (if (and (plusp result)(> twr 1.0)) (setq num-in-winning-bins (+ num-in-winning-bins (length contents))))          
 
     (setq profit-list
      (cons (list result twr (/ result (length contents))
                (/ counter (length contents)) (length contents) ith) profit-list)));;;closes dolist over bin-codes
   
    (dolist (kth waves)
       (if (plusp (nth 19 kth)) (setq all-winners (+ all-winners (nth 19 kth)))
           (setq all-losers (+ all-losers (nth 19 kth))))) 
   
     (setq counter 0)
     (dolist (jth profit-list)
         (if (and (plusp (car jth))(> (second jth) 1))
             (setq winners (+ winners (car jth)) counter (+ 1 counter))
         (setq losers (+ losers (car jth)))))
      
     (format T "NUMBER OF TRADES = ~A  in ~D MARKETS~%" (length waves)(length (num-markets-in-warehouse waves)))   
     (format T "P&L FOR ALL WINNERS = ~D   ALL LOSERS = ~D~%~%" (round all-winners) (round all-losers))
     (format T "NUMBER of ALL BINS = ~D~%" (length ewaves-bin-codes))
     (format T "RATIO OF TRADES TO BINS = ~F~%" (my-round (/ (length waves)(length ewaves-bin-codes)) 1))
     (format T "WINNING BIN PROFITS = ~D  IN ~D WINNING BINS~%~%" (round winners) counter)
     (format T "RATIO OF BIN PROFITS TO ALL WINNERS = ~F~%" (my-round (/ winners all-winners) 2))
     (format T "NUMBER OF TRADES IN THE WINNING BINS = ~D~%" num-in-winning-bins)
     (format T "PROFIT PER TRADE IN WINNING BINS = ~D~%~%" (if (zerop num-in-winning-bins) 0 (round (/ winners num-in-winning-bins))))
     (format T "NUMBER OF BINS WITH ONLY 1 TRADE = ~D~%" only-one)
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL BINS = ~F~%" (my-round (/ only-one (length ewaves-bin-codes)) 2))
     (format T "RATIO OF ONLY 1 TRADE BINS TO ALL TRADES = ~F~%~%" (my-round (/ only-one (length waves)) 2)) 
    
    (values (round winners)(round (+ all-winners all-losers)));;;all losers is a negative number
 ))  
 
 
  
;;;requires a base features list       
(defun ewaves-leave-one-out (base-features )
  (let (winners-list (result 0))
  (apply #'ewaves-trade-bins base-features)
  
  (dolist (ith base-features)
    (setq result (apply #'ewaves-trade-bins (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 )) 
;;;requires a candidate list to add to the base features         
(defun ewaves-add-one-in (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'ewaves-trade-bins base-features)
  
  (dolist (ith candidate-features)
    (setq result (apply #'ewaves-trade-bins (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))  
 

(defun remove-ewaves-trade-market (market)
  (let (trades path)
  (setq path (setq path "~/cycles/ewavestradewarehouse.dat"))
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


(defun display-ewaves-bin (bin)
  (let (contents)
     (setq contents (gethash bin *ewaves-trade-warehouse*))     
     (dolist (ith contents)
       (print ith))))

(defun display-ewaves-bin-to-file (bin)
  (let (contents)
     (setq contents (gethash bin *ewaves-trade-warehouse*)) 
     (with-open-file (stream "~/cycles/bin-contents.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)    
     (dolist (ith contents)
       (format stream "~S~%" ith)))))


;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse 
;;;this version is for the simulator and requires the waves from the warehouse89.
(defun bin-classifier-ewaves (date features)
  (let (record bin (result 0) (counter 0) contents signal
        (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0))
   (labels ((new-trade (market-name new-date)
             (car (remove nil (mapcar #'(lambda(s) (if (and (eql market-name (car s))(eql new-date (second s))) s nil)) waves))))) 
 
   ; (setq record (create-ewaves-entry-record-list date nil nil))
   ; (setq record (append record '(nil nil nil)))    
   (setq record (new-trade *data-name* date))
   ;(ifn record (print date))
  (setq bin (encode-ewaves-trades record features))
  (setq contents (gethash bin *ewaves-trade-warehouse*))
  
 ; (format T "record = ~A  bin = ~A  contents = ~A~%" record bin contents) 
 
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
         
  (cond ((and (plusp results-long)
              (>= results-long results-short)) (setq signal 'UP))
        ((and (plusp results-short)
              (>= results-short results-long)) (setq signal 'DOWN))
        ((and (<= results-long 0)(<= results-short 0)) (setq signal 'AVOID))
        ((not contents) (setq signal 'UNIQUE)))
  
  (values signal longs results-long (if (zerop longs) 0 (/ num-winners-long longs)) 
         shorts results-short
          (if (zerop shorts) 0 (/ num-winners-short shorts))
          bin)
)))

(defun new-trade (market-name new-date)
           (car (remove nil (mapcar #'(lambda(s) (if (and (eql market-name (car s))(eql new-date (second s))) s nil)) waves))))
            
;;;limitation of the ewaves simulator. It can ONLY test within the market and time frame that
;;;historical ewaves trades have been entered
;;;enters with market orders on the next day's open and exits with stop loss orders 
;;;;also trails the stop loss based on slippage   
(defun ewaves-simulation-test (market date2 num &optional (features *ewaves-features*))
 (let (date trades long short  trade-long  
       ctr
       ave-win ave-loss losers winners extended-trades trade trade-record old-trade-record
       ;risk risk-long risk-short prev-signal
        signal longs long-gains  trading-dates (trade-time 0)
       shorts short-gains long-acc short-acc date-1 record (running-sum 0)
       (outfile (string-append "~/cycles/" (format nil "~S" market) "ewaves-summary.dat"))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "ewaves-simulation.dat"))
       (path2 (string-append "~/cycles/" (format nil "~S" market) "ewaves-diary.dat")))
   (declare (ignore short-acc long-acc ctr))
  (labels ((new-trade (market-name new-date)
           (car (remove nil (mapcar #'(lambda(s) (if (and (eql market-name (car s))(eql new-date (second s))) s nil)) waves))))) 
;;;new-trade is a local function               
  
    
   (apply #'ewaves-trade-bins features)  
   (set-market market)
    
   (setq date (add-mkt-days date2 (- num)))
    (setq record (list date (getd date 'close) 0 0 0))
    (push record trading-dates)
 ;;;;from date1 to date2
 (dotimes (ith num)
                     
  ; (multiple-value-setq (entry-short entry-long)
  ;      (vprices date 4 1.66 3)) 
 
  ; (multiple-value-setq (entry-long entry-short)  (vprices date 4 1.66 3))                                    
  ; (multiple-value-setq (prev-signal ctr) (vsignals date 4 1.66 3)) 

;   (setq risk-short (my-pretty-price (* 1.382 (abs (- entry-short (n-day-high date 3 'close))))))
;                           
;   (setq risk-long  (my-pretty-price (* 1.382 (abs (- entry-long  (n-day-low date 3 'close))))))
   
   
 ;  (setq risk-short  (* (/ 1 *stop-loss-swing*) (abs (- entry-short (n-day-low date 3 'close)))))                          
 ;  (setq risk-long  (* (/ 1 *stop-loss-swing*) (abs (- entry-long  (n-day-high date 3 'close)))))           
                        
                 
 ;  (setq risk (max risk-long risk-short))   
   
   (setq date-1 date date (add-mkt-days date 1)) 
   
   (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1)) 
   (setq trade-record (new-trade market date))
  ; (print trade-record)  

;;;if long or short and not entry or exits  
      (if long (setf (nth 3 record) (- (getd date 'close) (getd date-1 'close))))
      (if short (setf (nth 3 record)(- (getd date-1 'close)(getd date 'close))))
      
    (when (and (getd date 'rollover) long)
             (setq long (+ long (getd date 'rollover))
        	  ; entry-long (+ entry-long (getd date 'rollover))
    		  ; stop-long (+ stop-long (getd date 'rollover))
    		   )
             (setf (nth 3 record)(- (nth 3 record) (getd date 'rollover)))
          )
    (when (and (getd date 'rollover) short)
            (setq short (+ short (getd date 'rollover))
    		 ; entry-short (+ entry-short (getd date 'rollover))
    		 ; stop-short (+ stop-short (getd date 'rollover))
    		  )
            (setf (nth 3 record) (+ (nth 3 record) (getd date 'rollover)))
           )
   
  ; (multiple-value-setq (entry-short-d entry-long-d)(vprices date-1 4 .764 1))
    
;    (if (eql prev-signal 'SELL) 
;       (setq stop-long (fmax stop-long (- entry-long risk-long)))         
;      (setq stop-long (fmax stop-long entry-long)))
        
;   (if (eql prev-signal 'BUY)                                           
;    (setq stop-short (fmin stop-short (+ entry-short risk-short)))
;     (setq stop-short (fmin stop-short entry-short))) 
  ; (when long (setq stop-long (fmax stop-long (min entry-short entry-short-d))))  
                                                
  ; (when short (setq stop-short (fmin stop-short (max entry-long entry-long-d))))

;;;;check if count changed at the trading degree  
     
   (when (and long (member (nth 2 trade-record) '(0 -1))) 
          (setq old-trade-record (new-trade market (car trade-long)))      
          (push (- (nth 18 old-trade-record) long) trades)
          (setq trade-long 
             (append trade-long
              (list date 'exit (nth 18 old-trade-record) (my-pretty-price (- (nth 18 old-trade-record) long)))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (nth 18 old-trade-record) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
          (setq trade-long nil long nil )) ;stop-long nil))
          
   (when (and short (member (nth 2 trade-record) '(0 1)))
          (setq old-trade-record (new-trade market (car trade))) 
          (push (- short (nth 18 old-trade-record)) trades)
          (setq trade 
            (append trade 
              (list date 'exit (nth 18 old-trade-record)(my-pretty-price  (- short (nth 18 old-trade-record))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (nth 18 old-trade-record)))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade nil short nil )) ;stop-short nil))

                                
;;;;check if stopped out of prior position      
#|   
   (when (and long (<= (getd date 'low) stop-long))       
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade-long 
             (append trade-long
              (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (min stop-long (getd date 'open)) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
          (setq trade-long nil long nil stop-long nil trade-record nil))
          
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade 
            (append trade 
              (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade nil short nil stop-short nil trade-record nil))

|#
;;;;calculate bin-classifier only as needed
; (when (or (and (not short)(eql (third trade-record) -1)
;                (<= (* risk (index-point-value)) *max-swing-risk*))
;           (and (not long) (eql (third trade-record) 1)
;                (<= (* risk (index-point-value)) *max-swing-risk*)))
    (when trade-record
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-ewaves date features))
       
       )


;;;check if new entry          
  
    (when (and (not short)              
               (eql (third trade-record) -1)
               (eql signal 'DOWN) 
               (> shorts 0)
               (>= (/ (- short-gains long-gains) (+ shorts longs)) 100)  
               ;(<= (* risk (index-point-value)) *max-swing-risk*)              
               )          
          (setq short (fourth trade-record)                    
                 trade (list date 'short short)
                ; stop-short (+ (n-day-high date-1 3 'close) risk)
                 )   
          (setf (nth 2 record) -1)      
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(/ 75 (index-point-value)))))
         
              ) 
               
    (when (and (not long)               
               (eql (third trade-record) 1)
               (eql signal 'UP)
               (> longs 0)
               (>= (/ (- long-gains short-gains) (+ longs shorts)) 100)             
              ; (<= (* risk (index-point-value)) *max-swing-risk*)                       
               )
            
           (setq long (fourth trade-record)
                 trade-long (list date 'long long) 
                ; stop-long (- (n-day-low date-1 3 'close) risk)
                 )
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record)
                 (- (getd date 'close) long (/ 75 (index-point-value)))))
           
             )
                 
 #| 
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
           (setf (nth 3 record) (+ (nth 3 record) (- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade nil short nil stop-short nil
           ))       
|#
 
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
   (if (index-digits) (setq trades (mapcar #'(lambda(s) (my-pretty-price s)) trades)))  
 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)    
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D ~%$/CONTRACT= ~D AVE DAYS IN TRADE= ~F"
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
      ; (round (* (drawdown trades)(or (index-point-value) 1)))
       (round (* (drawdown (mapcar #'(lambda(s) (nth 3 s)) trading-dates)) (or (index-point-value) 1)))

       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       (if (zerop (length trades)) 0 (my-round (/ trade-time (length trades)) 1))
     );close the format   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   )));;;closes the let and the defun       
 


;;;;this function expects that you have run (swing-trade-bins ...) already   
(defun display-ewaves-bins-by-expected-value ()
  (let (contents expected-value-list result (path1 "~/cycles/ewaves-expected-value.dat"))
   (dolist (ith ewaves-bin-codes)
     (setq contents
           (gethash ith *ewaves-trade-warehouse*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (nth 19 kth))))      
     (setq expected-value-list
      (cons (list (/ result (length contents)) ith (length contents) result) expected-value-list)));;;closes the dolist over bin-codes
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
         (vsort expected-value-list #'> 'car)
         (format stream "Average      Bin      #/Trades  $P&L~%  $P&L~%")
         (dolist (jth expected-value-list)
             (format stream "~5D      ~10A    ~4D  ~6D~%" (round (car jth)) (cadr jth) (third jth) (round (fourth jth))))) ;;;;closes the with-open-file
 ))  
  

;;;;this function expects that you have run (swing-trade-bins ...) already   
(defun display-ewaves-bins-by-accuracy ()
  (let (contents accuracy-list result (path1 "~/cycles/ewaves-accuracy.dat"))
   (dolist (ith ewaves-bin-codes)
     (setq contents
           (gethash ith *ewaves-trade-warehouse*))
     (setq result 0 )
     (dolist (kth contents)       
        (if (> (nth 19 kth) 0) (incf result)))
     (setq accuracy-list (cons (list (/ result (length contents)) ith (length contents)) accuracy-list)));;;closes the dolist over bin-codes
     (vsort accuracy-list #'> 'car)
     
     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
         
         (format stream "Accuracy          Bin           NUM~%Value~%")
         (dolist (jth accuracy-list)
             (format stream "~8F      ~A       ~D ~%" (my-round (car jth) 2) (cadr jth) (third jth)))) ;;;;closes the with-open-file
 ))  
   

;;;;;first need a function to find the trading degree
;;;;must read the .d3b2 file and put trades into separate lists
;;; a trade is a change in direction at a certain degree
;;;only interested in degrees 2 3 4 and 5.
;;;deg2-list is a list of waves at degree 2 one per day.
;;;each list is '(date start-date label degree direction)
;;;then it produces a trade list for each degree 2 3 4 and 5.
;;;then it finds the trading degree from the .csv file of completed waves called path-completewaves


(defun populate-ewaves-trades1 (path-incompletewaves path-completewaves)
  (let (date  dates1 dates
       trading-degree elliottwaves
       path1 path2  path3 path4  deg2-list deg3-list deg4-list deg5-list degx-list
       market first-degree first-degree-label first-degree-direction first-degree-start
       fdrl fdrl1 fdrl2 fdrl3 ave-wave-deg2 ave-wave-deg3 ave-wave-deg4 ave-wave-deg5
       deg2-trades deg3-trades deg4-trades deg5-trades degx-trades 
       deg2-ave deg3-ave deg4-ave deg5-ave
       degx-ave deg2-p&l deg3-p&l deg4-p&l deg5-p&l degx-p&l trading-degree-trades)
     ; (setq deg2-trades nil deg3-trades nil deg4-trades nil deg5-trades nil degx-trades nil dates nil)
     ; (setq deg2-list nil deg3-list nil deg4-list nil deg5-list nil degx-list nil )
       (maind-x)(set-cat-list)(setq dates1 nil)
       (pushnew '(BF AI) *wavels2* :test #'equal)
       (pushnew '(BZ AI) *wavels2* :test #'equal)
;;;first find the market name from the path.   
       (setq path1 (file-namestring path-incompletewaves))
       (setq market (read-from-string 
                     (subseq path1 0 (+ (position #\. path1) 4))))
      (set-market market)
      (setq path2 (string-append "~/cycles/" path1 "ewaves-degree-info1.dat"))
      (setq path3 (string-append "~/cycles/" path1 "ewavestrades1.dat"))
      (setq path4 (string-append "~/cycles/" path1 "ewaveslabels1.dat"))
;;;;read the file in line by line
    
   (with-open-file (str path-incompletewaves :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))      
       (push record dates)))  
       
            
;;;replaces all spaces with commas in each string record. 
 (setq dates (mapcar #'(lambda(s) (substitute #\, #\space s)) dates))
 
 (setq dates (mapcar #'(lambda(s) (my-split-sequence #\, s)) dates))
 (dolist (ith dates)
  
    (push (mapcar #'read-from-string ith) dates1))   
   
 (setq dates dates1)
      
     (dolist (ith dates)
       (setq date (nth 0 ith) first-degree (or (nth 4 ith) 0))
       ;(format T "~A ~A~%" date first-degree)
       (setq first-degree-label (nth 3 ith) first-degree-direction (or (nth 5 ith) 0))
       ;(format T "~A ~A~%" first-degree-label first-degree-direction)
       (setq first-degree-start (nth 2 ith) fdrl (cadr (assoc first-degree-label *wavels2*)))
       (setq fdrl1 (cadr (assoc fdrl *wavels2*)) fdrl2 (cadr (assoc fdrl1 *wavels2*)))
       (setq fdrl3 (cadr (assoc fdrl2 *wavels2*)))
       (case first-degree
         ((1 0) (push (list date first-degree-start fdrl 2 first-degree-direction first-degree-label) deg2-list)
            (push (list date first-degree-start fdrl1 3 first-degree-direction fdrl) deg3-list)
            (push (list date first-degree-start fdrl2 4 first-degree-direction fdrl1) deg4-list)
            (push (list date first-degree-start fdrl3 5 first-degree-direction fdrl2) deg5-list))
         (2 (push (list date first-degree-start fdrl 3 first-degree-direction first-degree-label) deg3-list)
            (push (list date first-degree-start fdrl1 4 first-degree-direction fdrl) deg4-list)
            (push (list date first-degree-start fdrl2 5 first-degree-direction fdrl1) deg5-list))
         (3 (push (list date first-degree-start fdrl 4 first-degree-direction first-degree-label) deg4-list)
            (push (list date first-degree-start fdrl1 5 first-degree-direction fdrl) deg5-list))
         (4 (push (list date first-degree-start fdrl 5 first-degree-direction first-degree-label) deg5-list))) ;;;closes the case  
      ; (format T "~A ~A~%" (car deg4-list)(car deg5-list))
;;;;the first degree is also the number of waves in the list
;;;;each wave is 5 more on the index into the list

;;;;(list date first-date label degree direction ww-label)
  
      (dotimes (jth (1+ first-degree))
      (when (eql jth 2)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg2-list))
       (when (eql jth 3)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg3-list))
        (when (eql jth 4)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg4-list))
        (when (eql jth 5)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           )
           deg5-list))
            
      ));;;closes the dotimes and dolist over dates
;;;now we have a list of incomplete waves in order of dates for each of degrees 2 3 4 and 5      
      
;;;check to see if today's direction for degrees 2 3 4 5 are the same as yesterday's
     (setq deg2-list (reverse deg2-list) deg3-list (reverse deg3-list) deg4-list (reverse deg4-list)
           deg5-list (reverse deg5-list))

       
      (with-open-file (str path-completewaves :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))      
       (push record elliottwaves))) 
       
       
      (setq elliottwaves (mapcar #'(lambda(s) (my-split-sequence #\, s)) elliottwaves)) 

      (multiple-value-setq (ave-wave-deg2 ave-wave-deg3 ave-wave-deg4 ave-wave-deg5)
               (find-ewaves-time-lengths elliottwaves))

;;;now need to create degX-list
     (dolist (ith dates)
        (setq trading-degree (find-trading-degree2 (car ith) elliottwaves))
        (ifn trading-degree (setq trading-degree 0))           
        (case trading-degree
          ((1 0) (push (list (car ith) nil nil 0 0 nil) degx-list))
          (2 (push (assoc (car ith) deg2-list) degx-list))
          (3 (push (assoc (car ith) deg3-list) degx-list))
          (4 (push (assoc (car ith) deg4-list) degx-list))
          (5 (push (assoc (car ith) deg5-list) degx-list))
          ) ;(print degx-list)
          )
      (setq degx-list (reverse degx-list))
;;;for debugging     
;     (setq deg2-list-test deg2-list deg3-list-test deg3-list deg-4-list-test deg4-list
;           deg5-list-test deg5-list degx-list-test degx-list)
                 
;;;(nth 2 s) is the direction                
     (setq deg2-trades (find-trades deg2-list))
     (setq deg2-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg2-trades)) ;(break "deg2")
     (setq deg3-trades (find-trades deg3-list))
     (setq deg3-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg3-trades)) ;(break "deg3")
     (setq deg4-trades (find-trades deg4-list))
     (setq deg4-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg4-trades)) ;(break "deg4")
     (setq deg5-trades (find-trades deg5-list))
     (setq deg5-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg5-trades)) ;(break "deg5")
     (setq degx-trades (find-trades degx-list))
     (setq degx-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) degx-trades)) ;(break "degx")
;;;;each element of the deg-trades is a list
;;;;;(market-name start-date direction start-price label degree ww-label 0 0 0 0 0 0 0 0       
     (if deg2-trades 
          (setq deg2-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg2-trades))(length deg2-trades)))
         (setq deg2-ave 0))
     (if deg3-trades   
      (setq deg3-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg3-trades)) (length deg3-trades)))
      (setq deg3-ave 0))
      (setq deg4-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg4-trades))(length deg4-trades))) 
      (setq deg5-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg5-trades))(length deg5-trades))) 
      (setq degx-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) degx-trades))(length degx-trades))) 
      (if deg2-trades
      (setq deg2-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg2-trades))))  
       (setq deg2-p&l 0))
      (if deg3-trades 
      (setq deg3-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg3-trades))))  
      (setq deg3-p&l 0)) 
      (setq deg4-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg4-trades)))) 
      (setq deg5-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg5-trades)))) 
      (setq degx-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) degx-trades)))) 

     (with-open-file (stream path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (kth degx-list)
          (format stream "~S~%" (cons *data-name* kth))))
      
;;;add the eight indicators to the trades with the trading degree
     (setq trading-degree-trades (add-indicators degx-trades))      
       
    (with-open-file (str path2 :direction :output :if-exists :supersede :if-does-not-exist :create)  
     (format str "Num deg2 trades= ~D  Ave Deg2 Time Per trade= ~F  Deg2 P&L= ~D ~%"
        (length deg2-trades) (my-round (float deg2-ave) 2) deg2-p&l)
     (format str "Num deg3 trades= ~D  Ave Deg3 Time Per trade= ~F  Deg3 P&L= ~D ~%"
        (length deg3-trades) (my-round (float deg3-ave) 2) deg3-p&l)
     (format str "Num deg4 trades= ~D  Ave Deg4 Time Per trade= ~F  Deg4 P&L= ~D ~%"
        (length deg4-trades) (my-round (float deg4-ave) 2) deg4-p&l)  
     (format str "Num deg5 trades= ~D  Ave Deg5 Time Per trade= ~F  Deg5 P&L= ~D ~%"
        (length deg5-trades) (my-round (float deg5-ave) 2) deg5-p&l)  
     (format str "Num degX trades= ~D  Ave DegX Time Per trade= ~F  DegX P&L= ~D ~%"
        (length degX-trades) (my-round (float degX-ave) 2) degX-p&l)     
      
     (format str "~%Ave time deg2 waves= ~F~%" (my-round ave-wave-deg2 2))
     (format str "Ave time deg3 waves= ~F~%" (my-round ave-wave-deg3 2))
     (format str "Ave time deg4 waves= ~F~%" (my-round ave-wave-deg4 2))
     (format str "Ave time deg5 waves= ~F~%" (my-round ave-wave-deg5 2))
   ) 


   
    (with-open-file (stream path3 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith trading-degree-trades)
        (format stream "~S~%" ith)))
  ))
 
;;;;this function finds the average time length of completed waves by degree
;;;it is a weighted average by the number of days they are present
;;;; (nth 8 ith) is the degree of the completed wave
;;; (nth 6 ith) is the time length in hours of the completed wave
(defun find-ewaves-time-lengths (elliottwaves)
  (let (deg2-times deg3-times deg4-times deg5-times
        deg2-ave deg3-ave deg4-ave deg5-ave)

       
   (dolist (ith elliottwaves)
      (case (read-from-string (nth 8 ith))
         (2 (push (/ (read-from-string (nth 6 ith)) 24) deg2-times))
         (3 (push (/ (read-from-string (nth 6 ith)) 24) deg3-times))
         (4 (push (/ (read-from-string (nth 6 ith)) 24) deg4-times))
         (5 (push (/ (read-from-string (nth 6 ith)) 24) deg5-times))))
 
    
    (setq deg2-ave (if deg2-times (/ (list-sum deg2-times)(length deg2-times)) 0))  
    (setq deg3-ave (if deg3-times (/ (list-sum deg3-times)(length deg3-times)) 0)) 
    (setq deg4-ave (if deg4-times (/ (list-sum deg4-times)(length deg4-times)) 0)) 
    (setq deg5-ave (if deg5-times (/ (list-sum deg5-times)(length deg5-times)) 0)) 
    
  ;  (terpri)
    
  ;  (print deg2-ave)(print deg3-ave) (print deg4-ave) (print deg5-ave)
   
   ; (if (< deg4-ave deg5-ave)(setq deg4-ave deg5-ave))
   ; (if (< deg3-ave deg4-ave)(setq deg3-ave deg4-ave))
   ; (if (< deg2-ave deg3-ave)(setq deg2-ave deg3-ave))
   (values deg2-ave deg3-ave deg4-ave deg5-ave) 
      
))


;;;;;The ewaveshistory.dat file contains the 
;;;;;

(defun ewaves-trades (&optional tdate (market-list *position-list*) (outfile t) )     
   (let (date path1 directive signal longs shorts long-gains short-gains
          long-acc short-acc path4 history (ctr 0))
      
        (declare (special history signal longs shorts long-gains short-gains long-acc short-acc )) 
         
;        (setf *ewaves-home* (environment-variable "EWAVESHOME")
;	      *ewaves-local* (environment-variable "EWAVESLOCAL")) 		
;       (with-open-file (str (string-append *ewaves-home* "markets.path"))
;	  (setf *database-upper-dir* (read str)))

       (maind-x)
       (read-markets-config)                 
       (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
;       (setq date1 (if holidayp (if (or (string-equal (day-of-week tdate) "friday")
;                                           (string-equal (day-of-week tdate) "thursday")) 
;                                         (add-days-to-date tdate 4)(add-days-to-date tdate 2)) 
;                     (if (string-equal (day-of-week tdate) "friday")(add-days-to-date tdate 3)(add-days-to-date tdate 1))))                        
    
        	   
        (setq path1  "/home/register/cycles/ewaves-xp.dat" )
        (setq path4 "~/cycles/ewaveshistory.dat")
        
        (if (probe-file path4) 
        (with-open-file (str path4 :direction :input )
            (do ((record (read str nil 'eof) (read str nil 'eof)))
              ((eql record 'eof)) 
              (push record history)));;;closed the with-open-file
         )        
        (if (and outfile (probe-file path1))
            (delete-file path1))
                  
         (apply #'ewaves-trade-bins2 *ewaves-features*) ;;;need just once for all markets 
        
        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert date))
         (format stream "Market               Month    Close     Dir  LB WLB CHG Signal     Stop  ~%")) 
        
        (dolist (market market-list) 
            (set-market market)(incf ctr)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
          (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
                                   (zerop (nth 1 (assoc *data-name* *C-list*))))  " ~8D   "
                          (string-append " ~8," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F   ")))            
                        
         (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (if (zerop (mod ctr 3))(terpri stream))
           (if (eql market 'sp.d1b)
             (format stream "~%~19A" 
                 (string-append (subseq (nth 3 (assoc market *c-list*)) 0 2)
                       (subseq (nth 3 (assoc market *c-list*)) 6 (length (nth 3 (assoc market *c-list*))))))
            (format stream "~%~19A" (nth 3 (assoc market *c-list*))))
            ;(print-string stream (format nil "~%~A" market) 7)
            (format stream "  ~A " (nth 2 (assoc market *c-list*)))
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A   " (convert-to-32nds (getd tdate 'close)))                
               (format stream directive 
                     (* (nth 4 (assoc *data-name* *C-list*)) (round (getd tdate 'close) (nth 4 (assoc *data-name* *C-list*))))))
                       
           (with-open-file (output4 path4 :direction :output :if-exists :append :if-does-not-exist :create)
             
            (find-best-ewaves-trade date stream  output4 )
             );;;closes output4
                
          );closes the stream
          );closes the dolist
     
;       (shell "unix2dos ~/cycles/ewaves-xp.dat /home/mk-data/luis/ewaves-xp.txt")
        
  )) ;;closes the let and defun            
   
            
;;;;with objectives and adjusting initial stop loss for slippage
;;;;This uses the bin-classifier for ewaves trades to decide to trade or not
;;;;assumes (ewaves-trade-bins date features) has already been run
;;;history is the list (data-name date label direction ww-label signal stoploss entry-date entry-price)
;;;basically history has the status of the previous day. entry-date is nil if the trade was
;;;entered on the change in trading degree direction

(defun find-best-ewaves-trade (tdate &optional (output T) output4 ) 
  (let* ( rollover entry-short entry-long   ydate-history
          trade-signal trade-entry direction
         action stop-long stop-short risk-short risk-long  stop
         signal longs long-gains long-acc shorts short-gains short-acc   
         prev-direction prev-stoploss   
         ewaves-labels  direction-change  
          )
      (declare (special history))   
      (declare (ignore long-acc short-acc))
    
    (setq ydate-history (find-if #'(lambda(s) (and (eql (getd tdate 'ydate) (nth 1 s))
                                                   (eql (nth 0 s) *data-name*))) history))
    (setq prev-direction (nth 2 ydate-history) prev-stoploss (nth 3 ydate-history))
   
   

;;;the ewaves-labels list comes from reading the ".d1b3" file from Ewaves
;;;these are the current day's labels and direction                                    
   (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc ewaves-labels)
          (bin-classifier-ewaves2 tdate *ewaves-features*))

;;;;ewaves-labels is a list of (label direction ww-label pw-label ct-score trading-degree) 
    
    
    (setq direction-change (neql prev-direction (nth 1 ewaves-labels)))
    
   (setq trade-entry (cond ((and (eql signal 'UP)(member prev-direction '(-1 nil))
                                      (> longs 0)
                                      (>= (/ long-gains longs) 100)
                                      ) 'UP)
                               ((and (eql signal 'DOWN)(member prev-direction '(1 nil))
                                      (> shorts 0)
                                      (>= (/ short-gains shorts) 100)
                                      ) 'DN)
                               (t 'FT)))
    
   
    (multiple-value-setq (entry-short entry-long)(vprices tdate 18 5.55 1))
    
    (setq risk-short (* .5 (- entry-long entry-short)))                    
    (setq risk-long (* .5 (- entry-long entry-short)))   
   
    (setq stop-short (+ (getd tdate 'close) risk-short) 
          stop-long (- (getd tdate 'close) risk-long))
      
    (setq rollover (getd tdate 'rollover))
    (ifn rollover (setq rollover 0)) 

 
;;;;let's ratchet the stop loss for the same direction    
    (if (and (eql prev-direction -1)(eql (nth 1 ewaves-labels) -1))
        (setq stop-short (fmin stop-short prev-stoploss)))     
    (if (and (eql prev-direction 1)(eql (nth 1 ewaves-labels) 1))
        (setq stop-long (fmax stop-long prev-stoploss)))    
  
    (setq direction (cond ((eql (nth 1 ewaves-labels) 1) " UP ")
                          ((eql (nth 1 ewaves-labels) -1) "DOWN")))
             
    
         
        (cond ((eql trade-entry 'DN) (setq stop (my-pretty-price stop-short) trade-signal "Go Short"))
              ((eql trade-entry 'UP) (setq stop (my-pretty-price stop-long) trade-signal "Go Long"))
              ((and direction-change
                  (eql (nth 1 ewaves-labels) -1)) (setq stop (my-pretty-price stop-short) trade-signal "Get Out"))
              ((and direction-change
                  (eql (nth 1 ewaves-labels) 1)) (setq stop (my-pretty-price stop-long) trade-signal "Get Out"))
              ((eql (nth 1 ewaves-labels) -1) (setq stop (my-pretty-price stop-short) trade-signal "       "))
              ((eql (nth 1 ewaves-labels) 1) (setq stop (my-pretty-price stop-long) trade-signal "       ")))

    (if (numberp stop)
       (if (member *data-name* '(ty.d1b us.d1b))
           (format output "~4A  ~2@A ~2@A   ~A ~8@A  ~8@A" direction (nth 0 ewaves-labels)(nth 2 ewaves-labels)
                   (if direction-change "*" " ")  trade-signal (convert-to-32nds stop))
          (format output "~4A  ~2@A ~2@A   ~A ~8@A  ~8F" direction (nth 0 ewaves-labels)(nth 2 ewaves-labels)
                   (if direction-change "*" " ")     trade-signal stop))
    (format output "~2@A  ~4A   ~A ~8@A  ~8A" (nth 0 ewaves-labels)
                direction  (if direction-change "*" " ")  trade-signal stop))
                                    
  
 ;     (setq directive1 (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F"))
     
;      (if (member *data-name* '(US.D1B TY.D1B))
;      (format output "~%SELL= ~7@A  INIT-BUY-STOP= ~7@A COVER-SHORT= ~7@A RISK= ~D~%BUY= ~7@A  INIT-SELL-STOP= ~7@A  COVER-LONG= ~7@A RISK= ~D~%"
;          (convert-to-32nds entry-short)(convert-to-32nds stop-short)(convert-to-32nds cover-short)
 ;         (round (* (-  (convert-to-decimal (convert-to-32 stop-short)) 
  ;                      (convert-to-decimal (convert-to-32 entry-short))
;                       ) (index-point-value)))
;          (convert-to-32nds entry-long) (convert-to-32nds stop-long)(convert-to-32nds cover-long)
;          (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
 ;                       (convert-to-decimal (convert-to-32  stop-long))
;                       ) (index-point-value))));;;closes the format
          
;      (format output 
;        (string-append "~%SELL= " directive1 "  INIT-BUY-STOP= " directive1 " COVER-SHORT= " directive1 " RISK= ~D~% BUY= " 
;        directive1 " INIT-SELL-STOP= " directive1 "  COVER-LONG= " directive1 " RISK= ~D~%")
 ;           (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
;            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
;            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-short (nth 4 (assoc *data-name* *C-list*))))
;            (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
;                          (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*))))
;                             )  (index-point-value)))
;            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
;            (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long (nth 4 (assoc *data-name* *C-list*))))
;            (* (nth 4 (assoc *data-name* *C-list*)) (round cover-long (nth 4 (assoc *data-name* *C-list*))))
 ;           (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-long (nth 4 (assoc *data-name* *C-list*))))
;                          (* (nth 4 (assoc *data-name* *C-list*)) (round stop-long
;                                                                         (nth 4 (assoc *data-name* *C-list*))))
;                       )  (index-point-value)))
;            ));;;closes the format and the if      
            
;;;there is no OK with ewaves Must be up or down.       
      (cond ;((equal action "OK      ")
 ;            (write-xml-record tdate "tblTradeRecs" 'POSITION 'SHORT date1 entry-short stop-short cover-short output1)
 ;            (write-xml-record tdate "tblTradeRecs" 'POSITION 'LONG date1 entry-long stop-long cover-long output1)
          
 ;                )
             
            ((equal action "NOT SHORT")
;             (write-xml-record tdate "tblTradeRecs" 'POSITION 'LONG date1 entry-long stop-long cover-long output1) 
;              (write-ewaves-record *data-name* action stop-long (* 100 long-acc) (round (/ long-gains longs)) output4) 
                )
                     
            ((equal action "NOT LONG") 
;             (write-xml-record tdate "tblTradeRecs" 'POSITION 'SHORT date1 entry-short stop-short cover-short output1)
;              (write-ewaves-record *data-name* action stop-short (* 100 short-acc) (round (/ short-gains shorts)) output4) 
               ))
                
;       (cond ((eql (cadr (assoc *data-name* *open-positions*)) 'long)
;              (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil stop-long cover-long output1)
;                  )                    
;             ((eql (cadr (assoc *data-name* *open-positions*)) 'short)
;              (write-xml-record tdate "tblTradeRecs" 'OPENP 'SHORT date1 nil stop-short cover-short output1)
;                  )
            ; ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'long)
            ;  (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'LONG date1 nil stop-long cover-long output1)
            ;      )                    
            ; ((eql (cadr (assoc *data-name* *forex-open-swings*)) 'short)
            ;  (write-xml-record tdate "tblTradeRecs" 'FXOPEN 'SHORT date1 nil stop-short cover-short output1) 
            ;    )
;                );;;;closes the cond

;;;;ewaves-labels is a list of (label direction ww-label trading-degree)     
;;;;this is where the ewaveshistory file is written out.  
;;;; the file has a list (market date direction stop-loss)   
        (format output4 "~%(~A ~A ~A ~F)"
          *data-name* tdate (nth 1 ewaves-labels)
           (if (eql (nth 1 ewaves-labels) 1) stop-long stop-short))
    
    
      ));;;closes the let and the defun      


;;;;returns if date is bullish or bearish and expected value and accuracy and number in warehouse 
;;;this version is for the production

(defun bin-classifier-ewaves2 (date features)
  (let (record bin (result 0) (counter 0) contents signal ewaves-labels dates
       (longs 0)(shorts 0) (results-long 0) (results-short 0)(num-winners-long 0)(num-winners-short 0)
       (path3 (string-append "~/aewaves/out2/" (string-downcase (string *data-name*)) "3"))) 
       
    (with-open-file (str path3 :direction :input)
     (do ((record (read str nil 'eof) (read str nil 'eof)))
         ((eql record 'eof))      
       (push record dates)))  

    (setq ewaves-labels (cdr (assoc date dates)))
       
       
       
     ; (print ewaves-labels)
;;;;ewaves-labels is a list of (label direction ww-label pw-pattern ct-score trading-degree) 
     (setq record (list *data-name* date (nth 1 ewaves-labels) ;; market date direction 
       (getd date 'close)  (nth 0 ewaves-labels) (nth 5 ewaves-labels);;close label trading-degree 
       (nth 2 ewaves-labels) (nth 3 ewaves-labels) (nth 4 ewaves-labels) 0 0 0 0 0 0 0   ;;;add seven slots for indicators
       0 0 0 0)) ;;;add four slots for time length. exit date, exit price, p/l.
    
    
     (setf (nth 9 record)(round (slow-stochastic date 21)))
     (setf (nth 10 record)(trend-signal date 5))
     (setf (nth 11 record)(trend-signal date 15))
  
     (setf (nth 12 record)(trend-signal date 135))
     (setf (nth 13 record)(my-round (/ (volatility date 4 1) (volatility date 28 1)) 3))
     (setf (nth 14 record)(day-bar-type date))
     (setf (nth 15 record)(day-bar-type1 (getd date 'ydate)))
   ;(print record)
        
      
    
  (setq bin (encode-ewaves-trades2 record features))
  (setq contents (gethash bin *ewaves-trade-warehouse*))
  
  ;(format T "record = ~A  bin = ~A  contents = ~A~%" record bin contents) 
 
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
  
;;;the current direction must be 1 to go long
;;;the current direction must be -1 to go short         
  (cond ((and (plusp results-long)(eql (nth 1 ewaves-labels) 1)
              (>= results-long results-short)) (setq signal 'UP))
        ((and (plusp results-short)(eql (nth 1 ewaves-labels) -1)
              (>= results-short results-long)) (setq signal 'DOWN))
        ((and (<= results-long 0)(<= results-short 0)) (setq signal 'AVOID))
        ((not contents) (setq signal 'UNIQUE))
        (t 'AVOID))
  
   
  (values signal longs results-long (if (zerop longs) 0 (/ num-winners-long longs)) 
         shorts results-short (if (zerop shorts) 0 (/ num-winners-short shorts))
          ewaves-labels bin)
))
 
 

;;;enters with market orders on the next day's open and exits with stop loss orders
;;;;also trails the stop loss 
;;;this is NOT for production. It uses the waves to find new trades.
#| 
(defun ewaves-simulation-test1 (market date2 num &optional (features *ewaves-features*))
 (let (date stop-long stop-short trades long short  trade-long  entry-long entry-short
      (path3 (string-append "~/aewaves/out2/" (string-downcase (string *data-name*)) "3")) 
       stopped-out
       ave-win ave-loss losers winners extended-trades trade trade-record old-trade-record
       risk risk-long risk-short  signal longs long-gains  trading-dates (trade-time 0)
       shorts short-gains long-acc short-acc date-1 record (running-sum 0)
    
       (outfile (string-append "~/cycles/" (format nil "~S" market) "ewaves-summary.dat"))
       (path1 (string-append "~/cycles/" (format nil "~S" market) "ewaves-simulation.dat"))
       (path2 (string-append "~/cycles/" (format nil "~S" market) "ewaves-diary.dat")))
   (declare (ignore short-acc long-acc ))
      
    (with-open-file (str path3 :direction :input)
     (do ((record (read str nil 'eof) (read str nil 'eof)))
         ((eql record 'eof))      
       (push record dates)))  
;;;each element of dates has the list of five items (date label direction ww-label trading-degree)
 ;   (setq ewaves-labels (cdr (assoc date dates)))
        
    
   (apply #'ewaves-trade-bins features)  
   (set-market market)
    
   (setq date (add-mkt-days date2 (- num)))
   (setq record (list date (getd date 'close) 0 0 0))
   (push record trading-dates)

 ;;;;from date1 to date2

 (dotimes (ith num)
   ;;;use 18 2.53 7 for the position trades 4 1.66 3 for swing trades                  
   (multiple-value-setq (entry-short entry-long)
        (vprices date 18 5.55 1)) 
                                       
    (setq risk-short (my-pretty-price (* .5 (abs (- entry-long entry-short)))))
    (setq risk-long  (my-pretty-price (* .5 (abs (- entry-long  entry-short)))))
   
   (setq risk (max risk-long risk-short))   
    
   (setq date-1 date date (add-mkt-days date 1) date+1 (add-mkt-days date 1)) 
   
   (setq record (list date (getd date 'close) 0 0 0))
   (if long (setf (nth 2 record) 1))
   (if short (setf (nth 2 record) -1)) 
  
   (setq ewaves-labels (cdr (assoc date dates)))
   
;    (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
 ;                  (bin-classifier-ewaves2 date features)

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

;;;;check if count changed at the trading degree  
     
   (when (and long (member (nth 2 ewaves-labels) '(0 -1))) 
                
          (push (- (getd date+1 'open) long) trades)
          (setq trade-long 
             (append trade-long
              (list date+1 'exit (getd date+1 'open) (my-pretty-price (- (getd date+1 'open) long)))))
          (push trade-long extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade-long) (nth 3 trade-long))))
          (setf (nth 2 record) 1)
          (setf (nth 3 record) (- (getd date+1 'open) (getd date-1 'close)))
          (if (getd date 'rollover)(setf (nth 3 record)(- (nth 3 record) (getd date 'rollover))))
          (setq trade-long nil long nil stop-long nil stopped-out nil))
          
   (when (and short (member (nth 2 ewaves-labels) '(0 1)))
          (push (- short (getd date+1 'open)) trades)
          (setq trade 
            (append trade 
              (list date+1 'exit (getd date+1 'open)(my-pretty-price  (- short (getd date+1 'open))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (getd date+1 'open)))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade nil short nil stop-short nil stopped-out nil))

                                
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
          (setq trade-long nil long nil stop-long nil trade-record nil stopped-out 'long))
          
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade 
            (append trade 
              (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)(setq trade-time (+ trade-time 1 (sub-mkt-dates (car trade) (nth 3 trade))))
          (setf (nth 2 record) -1)
          (setf (nth 3 record) (- (getd date-1 'close) (max stop-short (getd date 'open))))
          (if (getd date 'rollover) (setf (nth 3 record)(+ (nth 3 record) (getd date 'rollover))))
          (setq trade nil short nil stop-short nil trade-record nil stopped-out 'short))


;;;;calculate bin-classifier only as needed
 (when (or (and (not short)(eql (third ewaves-labels) -1)(not (eql stopped-out 'short))
                ;(<= (* risk (index-point-value)) *max-swing-risk*)
                )
           (and (not long) (eql (third ewaves-labels) 1)(not (eql stopped-out 'long))
                ;(<= (* risk (index-point-value)) *max-swing-risk*)
                ))
       (multiple-value-setq (signal longs long-gains long-acc shorts short-gains short-acc)
                   (bin-classifier-ewaves2 date features))
       
       )


;;;check if new entry          
  
    (when (and (not short) (not (eql stopped-out 'short))             
               (eql (third ewaves-labels) -1)
               (eql signal 'DOWN) 
               (> shorts 0)
               (>= (/ short-gains shorts) 100)  
               ;(<= (* risk (index-point-value)) *max-swing-risk*)              
               )          
          (setq short (getd date+1 'open)                    
                 trade (list date+1 'short short)
                 stop-short (+ (getd date 'close) risk))   
          (setf (nth 2 record) -1)      
          (setf (nth 3 record) (+ (nth 3 record)
                (- short (getd date 'close)(/ 75 (index-point-value)))))
         
              ) 
               
    (when (and (not long) (not (eql stopped-out 'long))              
               (eql (third trade-record) 1)
               (eql signal 'UP)
               (> longs 0)
               (>= (/ long-gains longs) 100)             
               ;(<= (* risk (index-point-value)) *max-swing-risk*)                       
               )
            
           (setq long (getd date+1 'open)
                 trade-long (list date+1 'long long) 
                 stop-long (- (getd date-1 'close) risk))
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
           (setq trade-long nil long nil stop-long nil stopped-out 'long))
     
             
    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) 1)
           (setf (nth 3 record) (+ (nth 3 record) (- stop-long long)))
           (setf (nth 3 record) (- (nth 3 record)(- (getd date 'close) long)))
           (setq trade-long nil long nil stop-long nil stopped-out 'long
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
           (setq trade nil short nil stop-short nil stopped-out 'short))
           
     
    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)(setq trade-time (+ trade-time 1))
           (setf (nth 2 record) -1)
           (setf (nth 3 record) (+ (nth 3 record) (- short stop-short)))
           (setf (nth 3 record) (- (nth 3 record)(- short (getd date 'close))))
           (setq trade nil short nil stop-short nil stopped-out 'short
           ))       

 
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
   (if (index-digits) (setq trades (mapcar #'(lambda(s) (my-pretty-price s)) trades))) 

    
 (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)   
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format str "~A traded from ~A to ~A~%" *data-name* (add-mkt-days date2 (- num)) date2)    
     (format str "P/L= ~D  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~D AVERAGE GAIN= ~D  AVERAGE LOSS= ~D~%LARGEST LOSS= ~D  PROFIT FACTOR= ~,2,0,'*,' F  ~
          DRAWDOWN= ~D ~%$/CONTRACT= ~D AVE DAYS IN TRADE= ~F"
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
      ; (round (* (drawdown trades)(or (index-point-value) 1)))
       (round (* (drawdown (mapcar #'(lambda(s) (nth 3 s)) trading-dates)) (or (index-point-value) 1)))

       (if (plusp (list-sum trades))
          (round (optimal-f (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))) 0)
       (if (zerop (length trades)) 0 (my-round (/ trade-time (length trades)) 1))
     );close the format   
       
   
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create) 
    
    (dolist (ith extended-trades)
    
    (format stream "~A\,~A\,~F\,~A\,~A\,~F\,~F\,~D~%" 
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
;   ); closes the when
   
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)  
   )));;;closes the let and the defun      
|#
   
(defun write-ewaves-record (market action stop-loss accuracy expected-value output)
   (format output "~A ~A ~A ~F ~D~%" market action stop-loss accuracy expected-value))
   
(defun update-ewaves-counts (&optional (markets *select-list*))
  (dolist (ith markets)
     (mainp "--" (string-append "~/aewaves/" (format nil "~(~A~)" ith)) (string-append "~/aewaves/input2/" (format nil "~(~A~)" ith))))
  ) 
  
  
;;;;creates the file with 3 lists. The longs, the shorts and the outs.   
(defun dump-ewaves-bins-by-profit (waves)
  (let (contents (result 0) (long-winners nil)(short-winners nil)
       (counter 0) (only-one 0) (avoids nil)
       (path4 "~/cycles/ewaves-bins.dat"))
   (dolist (ith ewaves-bin-codes)
     (setq contents (gethash ith *ewaves-trade-warehouse*))
     (setq result 0 counter 0)
     
     (if (= (length contents) 1) (incf only-one))
     (dolist (kth contents)          
         (setq result (+ result (nth 19 kth))) (incf counter)
        ) ;;;closes dolist over contents 
     (if (zerop counter) (print ith)) 
     (cond ((and (plusp (car ith))(>= (/ result counter) 100))
                 (push ith long-winners))
            ((and (minusp (car ith))(>= (/ result counter) 100))
                 (push ith short-winners))
            (t (push ith avoids)))  
         
        );;;closes dolist over bin-codes


    (with-open-file (stream path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format stream "~S~%~S~%~S" long-winners short-winners avoids)) 
    
    ))    



;;;;version #2 is adds the PW subtype and the preferred count score to the indicators
;;;;
;;;;;first need a function to find the trading degree
;;;;must read the .d3b2 file and put trades into separate lists
;;; a trade is a change in direction at a certain degree
;;;only interested in degrees 2 3 4 and 5.
;;;deg2-list is a list of waves at degree 2 one per day.
;;;each list is '(date start-date label degree direction PWsubtype ct-score)
;;;then it produces a trade list for each degree 2 3 4 and 5.
;;;then it finds the trading degree from the .csv file of completed waves called path-completewaves

;(defvar elliottwaves nil)
(defun populate-ewaves-trades2 (path-incompletewaves path-completewaves)
  (let (date  dates1 dates
       trading-degree elliottwaves
       path1 path2  path3 path4  deg2-list deg3-list deg4-list deg5-list degx-list
       market first-degree first-degree-label first-degree-direction first-degree-start
       fdrl fdrl1 fdrl2 fdrl3 ave-wave-deg2 ave-wave-deg3 ave-wave-deg4 ave-wave-deg5
       deg2-trades deg3-trades deg4-trades deg5-trades degx-trades 
       deg2-ave deg3-ave deg4-ave deg5-ave
       degx-ave deg2-p&l deg3-p&l deg4-p&l deg5-p&l degx-p&l trading-degree-trades
       ct-score PWsubtype)
     ; (setq deg2-trades nil deg3-trades nil deg4-trades nil deg5-trades nil degx-trades nil dates nil)
     ; (setq deg2-list nil deg3-list nil deg4-list nil deg5-list nil degx-list nil )
       (maind-x)(set-cat-list)(setq dates1 nil)
       (pushnew '(BF AI) *wavels2* :test #'equal)
       (pushnew '(BZ AI) *wavels2* :test #'equal)
;;;first find the market name from the path.   
       (setq path1 (file-namestring path-incompletewaves))
       (setq market (read-from-string 
                     (subseq path1 0 (+ (position #\. path1) 4))))
      (set-market market)
      (setq path2 (string-append "~/exitpoints/" path1 "ewaves-degree-info2.dat"))
      (setq path3 (string-append "~/exitpoints/" path1 "ewavestrades2.dat"))
      (setq path4 (string-append "~/exitpoints/" path1 "ewaveslabels2.dat"))
;;;;read the file in line by line
    
   (with-open-file (str path-incompletewaves :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))      
       (push record dates)))  
       
            
;;;replaces all spaces with commas in each string record. 
 (setq dates (mapcar #'(lambda(s) (substitute #\, #\space s)) dates))
 
 (setq dates (mapcar #'(lambda(s) (my-split-sequence #\, s)) dates))
;;;this strips off the "P" or "A" from the start dates of each wave 

 (dolist (kth dates)
     (let ((lgth (length kth)) (counter 0))
         (loop 
           (if (< (+ 2 counter) lgth)
                (setf (nth (+ 2 counter) kth) (subseq (nth (+ 2 counter) kth) 0 8))
                (return))
           (setq counter (+ 5 counter)))))

 (dolist (ith dates)  
    (push (mapcar #'read-from-string ith) dates1))   
   
 (setq dates dates1)
      
     (dolist (ith dates)
       (setq date (nth 0 ith) ct-score (nth 1 ith) first-degree (or (nth 4 ith) 0))
       ;(format T "~A ~A~%" date first-degree)
       (setq first-degree-label (nth 3 ith) first-degree-direction (or (nth 5 ith) 0))
       ;(format T "~A ~A~%" first-degree-label first-degree-direction)
       (setq first-degree-start (nth 2 ith) fdrl (cadr (assoc first-degree-label *wavels2*)))
       (setq fdrl1 (cadr (assoc fdrl *wavels2*)) fdrl2 (cadr (assoc fdrl1 *wavels2*)))
       (setq fdrl3 (cadr (assoc fdrl2 *wavels2*)))
;;;the PWsubtype is the pattern of the previous wave of the trading degree wave
;;;PWsubtype is set to nil at this time, later it will be determined from the complete waves       
       (case first-degree
         ((1 0) (push (list date first-degree-start fdrl 2 first-degree-direction first-degree-label PWsubtype ct-score) deg2-list)
            (push (list date first-degree-start fdrl1 3 first-degree-direction fdrl PWsubtype ct-score) deg3-list)
            (push (list date first-degree-start fdrl2 4 first-degree-direction fdrl1 PWsubtype ct-score) deg4-list)
            (push (list date first-degree-start fdrl3 5 first-degree-direction fdrl2 PWsubtype ct-score) deg5-list))
         (2 (push (list date first-degree-start fdrl 3 first-degree-direction first-degree-label PWsubtype ct-score) deg3-list)
            (push (list date first-degree-start fdrl1 4 first-degree-direction fdrl PWsubtype ct-score) deg4-list)
            (push (list date first-degree-start fdrl2 5 first-degree-direction fdrl1 PWsubtype ct-score) deg5-list))
         (3 (push (list date first-degree-start fdrl 4 first-degree-direction first-degree-label PWsubtype ct-score) deg4-list)
            (push (list date first-degree-start fdrl1 5 first-degree-direction fdrl PWsubtype Ct-score) deg5-list ))
         (4 (push (list date first-degree-start fdrl 5 first-degree-direction first-degree-label PWsubtype ct-score) deg5-list)));;;closes the case  
      ; (format T "~A ~A~%" (car deg4-list)(car deg5-list))
;;;;the first degree (from each line in the .d12b2 file) is also the number of waves in the list
;;;;each wave is 5 more on the index into the list
;;;above put the first wave onto the deg -list. Now we add the other waves onto their respective list by degree
;;;;(list date first-date label degree direction ww-label PWsubtype score)
  
      (dotimes (jth (1+ first-degree))
      (when (eql jth 2)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           PWsubtype ct-score
           )
           deg2-list))
       (when (eql jth 3)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           PWsubtype ct-score
           )
           deg3-list))
        (when (eql jth 4)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           PWsubtype ct-score
           )
           deg4-list))
        (when (eql jth 5)
        (push (list date (nth (+ 2 (* 5 (- first-degree jth))) ith) (nth (+ 3 (* 5 (- first-degree jth))) ith)
           (nth (+ 4 (* 5 (- first-degree jth))) ith) (nth (+ 5 (* 5 (- first-degree jth))) ith)
           (nth (+ 3 (* 5 (+ 1 (- first-degree jth)))) ith)
           PWsubtype ct-score
           )
           deg5-list))
            
      ));;;closes the dotimes and dolist over dates
;;;now we have a list of incomplete waves in order of dates for each of degrees 2 3 4 and 5      
      
;;;check to see if today's direction for degrees 2 3 4 5 are the same as yesterday's
     (setq deg2-list (reverse deg2-list) deg3-list (reverse deg3-list) deg4-list (reverse deg4-list)
           deg5-list (reverse deg5-list))

       
      (with-open-file (str path-completewaves :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))      
       (push record elliottwaves))) 
       
      
      (setq elliottwaves
       (mapcar #'(lambda(cpwave) 
           (mapcar #'(lambda(s)(read-from-string s))
                          (subseq (my-split-sequence #\, cpwave) 0 21))) elliottwaves)) 
  ;;;elliottwaves is a list of completed waves
  ;;;each is a list with strings removed  
             
      (multiple-value-setq (ave-wave-deg2 ave-wave-deg3 ave-wave-deg4 ave-wave-deg5)
               (find-ewaves-time-lengths2 elliottwaves))

;;;now need to create degX-list
     (dolist (ith dates)
        (setq trading-degree (find-trading-degree2a (car ith) elliottwaves))
        (ifn trading-degree (setq trading-degree 0))           
        (case trading-degree
          ((1 0) (push (list (car ith) nil nil 0 0 nil nil 0) degx-list))
          (2 (push (assoc (car ith) deg2-list) degx-list))
          (3 (push (assoc (car ith) deg3-list) degx-list))
          (4 (push (assoc (car ith) deg4-list) degx-list))
          (5 (push (assoc (car ith) deg5-list) degx-list))
          ) ;(print degx-list)
          )
      (setq degx-list (reverse degx-list))
      
;;;for the degx list of waves we need to find and add the PWsubtype from the completed waves 
      (dolist (kth degx-list)
         (add-pwsubtype kth elliottwaves)
         )     
;;;for debugging     
;     (setq deg2-list-test deg2-list deg3-list-test deg3-list deg-4-list-test deg4-list
;           deg5-list-test deg5-list degx-list-test degx-list)
                 
;;;(nth 2 s) is the direction                
     (setq deg2-trades (find-trades2 deg2-list))
     (setq deg2-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg2-trades)); (break "deg2")
     (setq deg3-trades (find-trades2 deg3-list))
     (setq deg3-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg3-trades)); (break "deg3")
     (setq deg4-trades (find-trades2 deg4-list))
     (setq deg4-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg4-trades)); (break "deg4")
     (setq deg5-trades (find-trades2 deg5-list))
     (setq deg5-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) deg5-trades)); (break "deg5")
     (setq degx-trades (find-trades2 degx-list))
     (setq degx-trades (remove-if #'(lambda (s) (zerop (nth 2 s))) degx-trades)); (break "degx")
;;;;each element of the deg-trades is a list
;;;;;(market-name start-date direction start-price label degree ww-label PWsubtype ct-score 0 0 0 0 0 0 0 0  )     
     (if deg2-trades 
          (setq deg2-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg2-trades))(length deg2-trades)))
         (setq deg2-ave 0))
     (if deg3-trades   
      (setq deg3-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg3-trades)) (length deg3-trades)))
      (setq deg3-ave 0))
      (setq deg4-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg4-trades))(length deg4-trades))) 
      (setq deg5-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) deg5-trades))(length deg5-trades))) 
      (setq degx-ave (/ (list-sum (mapcar #'(lambda (s) (nth 16 s)) degx-trades))(length degx-trades))) 
      (if deg2-trades
      (setq deg2-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg2-trades))))  
       (setq deg2-p&l 0))
      (if deg3-trades 
      (setq deg3-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg3-trades))))  
      (setq deg3-p&l 0)) 
      (setq deg4-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg4-trades)))) 
      (setq deg5-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) deg5-trades)))) 
      (setq degx-p&l (round (list-sum (mapcar #'(lambda (s) (nth 19 s)) degx-trades)))) 

     (with-open-file (stream path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (kth degx-list)
          (format stream "~S~%" (cons *data-name* kth))))
      
;;;add the seven indicators to the trades with the trading degree
     (setq trading-degree-trades (add-indicators2 degx-trades))      
       
    (with-open-file (str path2 :direction :output :if-exists :supersede :if-does-not-exist :create)  
     (format str "Num deg2 trades= ~D  Ave Deg2 Time Per trade= ~F  Deg2 P&L= ~D ~%"
        (length deg2-trades) (my-round (float deg2-ave) 2) deg2-p&l)
     (format str "Num deg3 trades= ~D  Ave Deg3 Time Per trade= ~F  Deg3 P&L= ~D ~%"
        (length deg3-trades) (my-round (float deg3-ave) 2) deg3-p&l)
     (format str "Num deg4 trades= ~D  Ave Deg4 Time Per trade= ~F  Deg4 P&L= ~D ~%"
        (length deg4-trades) (my-round (float deg4-ave) 2) deg4-p&l)  
     (format str "Num deg5 trades= ~D  Ave Deg5 Time Per trade= ~F  Deg5 P&L= ~D ~%"
        (length deg5-trades) (my-round (float deg5-ave) 2) deg5-p&l)  
     (format str "Num degX trades= ~D  Ave DegX Time Per trade= ~F  DegX P&L= ~D ~%"
        (length degX-trades) (my-round (float degX-ave) 2) degX-p&l)     
      
     (format str "~%Ave time deg2 waves= ~F~%" (my-round ave-wave-deg2 2))
     (format str "Ave time deg3 waves= ~F~%" (my-round ave-wave-deg3 2))
     (format str "Ave time deg4 waves= ~F~%" (my-round ave-wave-deg4 2))
     (format str "Ave time deg5 waves= ~F~%" (my-round ave-wave-deg5 2))
   ) 


   
    (with-open-file (stream path3 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith trading-degree-trades)
        (format stream "~S~%" ith)))
  ))
 

;;;  this function is for populate-ewaves-trades2
;;;
(defun find-trades2 (deg-list)
  (let (ith-1 trade-record trades)
;;;the deg-list is a list of dates with the wave info for a particular degree on each date. 
;;;; each element of deg-list is a list 
;;;;(date start-date label degree direction ww-label PWsubtype score)
  (dolist (ith deg-list)
   
     (when (and ith-1 (neql (nth 4 ith) (nth 4 ith-1)))
           ;(print (car trades)) (print ith)
           (when (car trades)
              
               (setf (nth 17 (car trades)) (getd (car ith) 'ndate)) ;;;adds the exit date 
               (setf (nth 18 (car trades)) (getd (getd (car ith) 'ndate) 'open));;;adds the exit price
               (setf (nth 16 (car trades)) (sub-mkt-dates (nth 1 (car trades)) (nth 17 (car trades))));;adds time-length
               (setf (nth 19 (car trades))    ;;;adds the gain or loss in dollars
                     (round (* (nth 2 (car trades))   ;;;this is the direction -1 0 1
                               (index-point-value)
                               (- (nth 18 (car trades))  ;;;this is the exit price
                                  (nth 3 (car trades)))))) ;;;this is the entry price
                                                     
                           );;;closes the inner when
           
         (setq trade-record (list *data-name* (getd (car ith) 'ndate) (nth 4 ith);;three slots market entry-date direction 
                  (getd (getd (car ith) 'ndate) 'open);;;entry-price
                  (nth 2 ith) (nth 3 ith) (nth 5 ith) ;;;three slots for ewaves info: label degree and and ww-label
                  (nth 6 ith) (nth 7 ith) ;;;two slots for PWsubtype and ct-score
                    0 0 0 0 0 0 0    ;;;seven slots for indicators
                    0 0 0 0))      ;;;four slots for time-length, exit date, price, gain/loss in dollars        
         (push trade-record trades)                         
         ) ;;;this closes the upper when
         (setq ith-1 ith)
    );;;closes the dolist
    (if (zerop (nth 17 (car trades))) (setq trades (cdr trades)))
    
    trades 
   
  ))  ;;;closes the let and defun
 
;;;;this function finds the average time length of completed waves by degree
;;;it is a weighted average by the number of days they are present
;;;; (nth 8 ith) is the degree of the completed wave
;;; (nth 6 ith) is the time length in hours of the completed wave
;;;this is for populate-ewaves-trades2
(defun find-ewaves-time-lengths2 (elliottwaves)
  (let (deg2-times deg3-times deg4-times deg5-times
        deg2-ave deg3-ave deg4-ave deg5-ave)

       
   (dolist (ith elliottwaves)
      (case (nth 8 ith)
         (2 (push (/ (nth 6 ith) 24) deg2-times))
         (3 (push (/ (nth 6 ith) 24) deg3-times))
         (4 (push (/ (nth 6 ith) 24) deg4-times))
         (5 (push (/ (nth 6 ith) 24) deg5-times))))
 
    
    (setq deg2-ave (if deg2-times (/ (list-sum deg2-times)(length deg2-times)) 0))  
    (setq deg3-ave (if deg3-times (/ (list-sum deg3-times)(length deg3-times)) 0)) 
    (setq deg4-ave (if deg4-times (/ (list-sum deg4-times)(length deg4-times)) 0)) 
    (setq deg5-ave (if deg5-times (/ (list-sum deg5-times)(length deg5-times)) 0)) 
    
  ;  (terpri)
    
  ;  (print deg2-ave)(print deg3-ave) (print deg4-ave) (print deg5-ave)
   
   ; (if (< deg4-ave deg5-ave)(setq deg4-ave deg5-ave))
   ; (if (< deg3-ave deg4-ave)(setq deg3-ave deg4-ave))
   ; (if (< deg2-ave deg3-ave)(setq deg2-ave deg3-ave))
   (values deg2-ave deg3-ave deg4-ave deg5-ave) 
      
))
  
;;;need a function to find and add the PWsubtype to the degx-list of waves

;;;the PW is the found by matching market name, start date of trading degree wave and
;;;end date of previous wave
;;;there may be multiple wave matches as more than one wave completes on the given date
;;; the previous wave is the one with smallest degree number
;;;the completed waves are in elliottwaves
;;;PWsubtype needs to be added to the 7th slot of each element of degx-list (nth 6 ith)

(defun add-pwsubtype  (incomplete-wave elliottwaves)
  (let (candidates pwsubtype)
 
    (dolist (jth elliottwaves)
      (if (and (eql (nth 0 incomplete-wave);;;analysis date .d1b2
                    (nth 1 jth));;;analysis date .csv
               (eql (nth 4 jth);;;end date of the completed wave
                    (nth 1 incomplete-wave));;;start date of wave of trading degree
               (eql (nth 0 jth);;;market name for completed wave
                    *data-name*));;;market name for incomplete trading degree wave
          (pushnew jth candidates :test #'equal)));;;closes the dolist over elliottwaves (completed waves)
  
     ;;;now find the wave in candidates that has the lowest degree number
  
  (setq pwsubtype (nth 19 (car (vsort candidates #'< #'ninth))));;; the 20th element is the subtype of completed wave   
  
  (setf (nth 6 incomplete-wave) (or pwsubtype 'UNK));;;this should have the side effect of adding the PWsubtype to the degx-list
      
   (format T "~S  ~S  ~A ~A  ~% " (car incomplete-wave)(nth 1 incomplete-wave) pwsubtype (length candidates))   
 ));;closes the let and the defun     
 
 
   
;;;adds the indicators to the trade-record for populate-ewaves-trades2
(defun add-indicators2 (trades) 
  (let (date)
  (dolist (jth trades)
     (setq date (getd (nth 1 jth) 'ydate))
     (setf (nth 9 jth)(round (slow-stochastic date 21)))
     (setf (nth 10 jth)(trend-signal date 5))
     (setf (nth 11 jth)(trend-signal date 15))
   
     (setf (nth 13 jth)(trend-signal date 135))
     (setf (nth 14 jth)(my-round (/ (volatility date 4 1) (volatility date 28 1)) 3))
     (setf (nth 15 jth)(day-bar-type date))
     (setf (nth 15 jth)(day-bar-type1 (getd date 'ydate)))
     )
     
   trades
)) ;;;closes the let and defun

  
;;;;this function if for reading the /home/ewaves/cicalc/market.csv file
;;;the file has the completed wave info for populating the wave warehouse to
;;;be built one day hopefully.
;;;;the file has all the completed waves for the preferred count on a single date
;;;;
;;;the first item is the data name.
;;; the second item is the latest date of the data.
;;;the third item is the start date of the wave
;;;the fourth item is the "AM" or "PM" for the start date
;;;the fifth item is the end date of the wave
;;;the sixth item is the "AM" or "PM" of the end time of the wave
;;;the seventh item is the time length of the wave in hours.
;;;the eighth item is the refined label of the wave
;;;the ninth item is the wave degree
;;;the tenth item is the direction 1 or -1.
;(setq elliottwaves nil )
(defun find-trading-degree2a (date1 elliottwaves)
  (let (deg2-times deg3-times deg4-times deg5-times
        deg2-ave deg3-ave deg4-ave deg5-ave)
      
  
   (dolist (ith elliottwaves)
      (when (eql (nth 1 ith) date1)
       (case (nth 8 ith)
         (2 (push (/ (nth 6 ith) 24) deg2-times))
         (3 (push (/ (nth 6 ith) 24) deg3-times))
         (4 (push (/ (nth 6 ith) 24) deg4-times))
         (5 (push (/ (nth 6 ith) 24) deg5-times)))))
 
    
    (setq deg2-ave (if deg2-times (/ (list-sum deg2-times)(length deg2-times)) 0))  
    (setq deg3-ave (if deg3-times (/ (list-sum deg3-times)(length deg3-times)) 0)) 
    (setq deg4-ave (if deg4-times (/ (list-sum deg4-times)(length deg4-times)) 0)) 
    (setq deg5-ave (if deg5-times (/ (list-sum deg5-times)(length deg5-times)) 0)) 
    
    (terpri)(print date1)
    (print deg2-times)(print deg3-times)(print deg4-times)(print deg5-times)
    (print deg2-ave)(print deg3-ave) (print deg4-ave) (print deg5-ave)
   
    (if (< deg4-ave deg5-ave)(setq deg4-ave deg5-ave))
    (if (< deg3-ave deg4-ave)(setq deg3-ave deg4-ave))
    (if (< deg2-ave deg3-ave)(setq deg2-ave deg3-ave))
    
   (find-trading-degree deg2-ave deg3-ave deg4-ave deg5-ave)     
))

;;;;
;;;;(setq trade-record (list *data-name* (getd (car ith) 'ndate) (nth 4 ith);;three slots market entry-date direction 
;;;                  (getd (getd (car ith) 'ndate) 'open);;;entry-price
;;                (nth 2 ith) (nth 3 ith) (nth 5 ith) ;;;three slots for ewaves info: label degree and and ww-label
;;                  (nth 6 ith) (nth 7 ith) ;;;two slots for PWsubtype and ct-score
;;                    0 0 0 0 0 0 0    ;;;seven slots for indicators
;;                   0 0 0 0))      ;;;four slots for time-length, exit date, price, gain/loss in dollars  
;;;trade record slots
;;;;slot 0 is the market name
;;; slot 1 is the entry date
;;; slot 2 is the direction
;;; slot 3 is the entry price
;;; slot 4 is the label
;;; slot 5 is the degree
;;; slot 6 is the ww-label
;;; slot 7 is the PWsubtype
;;; slot 8 is the ct-score
;;; slot 9 is the stochastic 21
;;; slot 10 is the TS5
;;; slot 11 is the TS15
;;; slot 12 is the TS135
;;; slot 13 is the volatility ratio
;;; slot 14 is day-bar-type
;;; slot 15 is day-bar-type2

;;;returns a list of twelve codes 
(defun encode-ewaves-trades2 (record features) 
  (let (bin-list) 
  
    (dolist (ith features)
     (case ith
 ;;;;number 1 is the direction
     (1 (push (nth 2 record) bin-list));;;adds the direction  
;;;Feature 2 is the label of the wave of trading-degree
     (2 (push (nth 4 record) bin-list))   
;;;Feature 3 is the ww-label of the within wave of the wave of trading-degree
     (3 (push (nth 6 record) bin-list))
;;;;feature 4 is the PWsubtype with 12 levels    
     (4 (push (nth 7 record) bin-list))
;;;;
;;;;feature 5 is the ct-score with 5 levels
     (5 (cond ((> (nth 8 record) 0) (push 0 bin-list))
              ((and (> (nth 8 record) -5)(<= (nth 8 record) 0))(push -1 bin-list))
              ((and (> (nth 8 record) -10)(<= (nth 8 record) -5))(push -2 bin-list))
              ((and (> (nth 8 record) -15)(<= (nth 8 record)-10))(push -3 bin-list)) 
              ((<= (nth 8 record) -15) (push -4 bin-list))
          ))
       
             
 ;;;;Feature 6 with 6 levels is the stochastic with parameter 21            
     (6 (cond ((<= (nth 9 record) 10) (push 3 bin-list))
              ((and (> (nth 9 record) 10)(<= (nth 9 record) 20)) (push 2 bin-list))
              ((and (> (nth 9 record) 20)(<= (nth 9 record) 50)) (push 1 bin-list))
              ((and (> (nth 9 record) 50)(<= (nth 9 record) 80)) (push -1 bin-list))  
              ((and (>= (nth 9 record) 80)(< (nth 9 record) 90)) (push -2 bin-list))
              ((>= (nth 9 record) 90) (push -3 bin-list))))

;;;Feature 7 with 5 levels; This is the TS5        
      (7 (cond ((eql (nth 10 record) 'DN) (push -1 bin-list))
               ((eql (nth 10 record) 'CD) (push -2 bin-list))
               ((eql (nth 10 record) 'UP) (push 1 bin-list))
               ((eql (nth 10 record) 'CU) (push 2 bin-list))
               ((eql (nth 10 record) 'FT) (push 0 bin-list))
             ) )   
     ;;;Feature 8 with 5 levels; This is TS15          
     (8  (cond ((eql (nth 11 record) 'DN) (push -1 bin-list))
             ((eql (nth 11 record) 'CD) (push -2 bin-list))
             ((eql (nth 11 record) 'UP) (push 1 bin-list))
             ((eql (nth 11 record) 'CU) (push 2 bin-list))
             ((eql (nth 11 record) 'FT) (push 0 bin-list)) ))      

;;;Feature 9 with 5 levels; This is TS135         
     (9   (cond ((eql (nth 12 record) 'DN) (push -1 bin-list))
             ((eql (nth 12 record) 'CD) (push -2 bin-list))
             ((eql (nth 12 record) 'UP) (push 1 bin-list))
             ((eql (nth 12 record) 'CU) (push 2 bin-list))
             ((eql (nth 12 record) 'FT) (push 0 bin-list))

             ) )        
;;;;feature 10 with 5 levels is volatility ratio
       (10 (cond ((< (nth 13 record) .80) (push 2 bin-list))
                ((and (>= (nth 13 record) .80)(< (nth 13 record) .90)) (push 1 bin-list))
                ((and (>= (nth 13 record) .90)(< (nth 13 record) 1.10)) (push 0 bin-list))
                ((and (>= (nth 13 record) 1.10)(< (nth 13 record) 1.20))(push -1 bin-list))
                ((>= (nth 13 record) 1.2)(push -2 bin-list))))

   
;;;;Feature 11 with 9 levels bar type
      (11 (cond ((eql (nth 14 record) 11) (push 0 bin-list))
               ((eql (nth 14 record) 12) (push 1 bin-list)) 
               ((eql (nth 14 record) 13) (push 2 bin-list)) 
               ((eql (nth 14 record) 21) (push 3 bin-list)) 
               ((eql (nth 14 record) 22) (push 4 bin-list)) 
               ((eql (nth 14 record) 23) (push 5 bin-list)) 
               ((eql (nth 14 record) 31) (push 6 bin-list)) 
               ((eql (nth 14 record) 32) (push 7 bin-list)) 
               ((eql (nth 14 record) 33) (push 8 bin-list)) ))

;;;;;;Feature 12 with 5 levels of yesterday's bar relationship to the previous bar

      (12 (cond ((eql (nth 15 record) 'IN) (push -2 bin-list))
                ((eql (nth 15 record) 'OU) (push 2 bin-list))
                ((eql (nth 15 record) 'DN) (push -1 bin-list))
                ((eql (nth 15 record) 'UP) (push 1 bin-list))
                ((eql (nth 15 record) 'FT) (push 0 bin-list))
                     ))

                ));;;closes the case and the dolist over features
              
      (reverse bin-list)            
 ))          


(defun ewaves-trade-bins2 (&rest features)
  (let (bin path)
  (setq path "~/exitpoints/ewavestrade2warehouse89.dat")
  (maind-x)(set-cat-list)
  (setq waves nil ewaves-bin-codes nil)(clrhash *ewaves-trade-warehouse*)
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record waves)))
      
         
 (if *out-of-sample*
     (setq waves (remove-if #'(lambda (s) (eql (car s) *data-name*)) waves)))      
  
     
;;;;now all the trades are in the list swings.  
;;;we now assign/sort the trades into bins   
;;;basically process a record list into a code or bin
;;;and create a code
   
   (dolist (record waves)
     (setq bin (encode-ewaves-trades2 record features))
    ; (print bin)
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin *ewaves-trade-warehouse*)
            (ifn (member record (gethash bin *ewaves-trade-warehouse*) :test #'equal)
                 (setf (gethash bin *ewaves-trade-warehouse*)
                       (cons record (gethash bin *ewaves-trade-warehouse*)))))
           ((not (gethash bin *ewaves-trade-warehouse*))
             (setf (gethash bin *ewaves-trade-warehouse*)
              (list record)))))
  
    (format T  "~%FEATURES = ~A~%" features)
    (rank-ewaves-bins-by-profit waves) 
     
         
  ))

;;;reads the ewavestrade2warehouse89.dat
;;;reads and adds the new trades file 
;;;only adds the new trades is not already there.
;;;writes out the new warehouse with the added trades.
;;;
(defun add-ewaves-trades2 (new-trades-path)
  (let ((path-out "~/exitpoints/ewavestrade2warehouse89.dat") ewaves-trades) 
    
       (if (probe-file path-out)
        (with-open-file (str path-out :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof)) 
          (push record ewaves-trades))
          ))
     
     (with-open-file (str new-trades-path :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof)) 
          (pushnew record ewaves-trades)
          ))
    
    (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith ewaves-trades)
       (format stream "~S~%" ith))
       ) ;;;closes the with-open-file
));;closes the let and defun



  
;;;requires a base features list       
(defun ewaves-leave-one-out2 (base-features )
  (let (winners-list (result 0))
  (apply #'ewaves-trade-bins2 base-features)
  
  (dolist (ith base-features)
    (setq result (apply #'ewaves-trade-bins2 (remove ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
  (vsort winners-list #'> 'car)
  (format T "Most Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Least Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Winners List = ~A ~%" winners-list)
 )) 
;;;requires a candidate list to add to the base features         
(defun ewaves-add-one-in2 (base-features candidate-features)
  (let (winners-list (result 0))
  (apply #'ewaves-trade-bins2 base-features)
  
  (dolist (ith candidate-features)
    (setq result (apply #'ewaves-trade-bins2 (cons ith base-features)))
    (setq winners-list (acons result ith winners-list))
    )
    (vsort winners-list #'> 'car)
   (format T "Most Valuable Feature = ~A~%" (cdar winners-list))
  (format T "Least Valuable Feature = ~A~%" (cdar (last winners-list)))
  (format T "Winners List = ~A ~%" winners-list)
 ))  
 

(defun remove-ewaves-trade-market2 (market)
  (let (trades path)
  (setq path (setq path "~/exitpoints/ewavestrade2warehouse89.dat"))
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
;;;creates a file with the association lists with weights
;;;the file will be used in classifying a bin code

(defun make-feature-weights (features)
  (let ((path-out "~/exitpoints/bin-weights.dat")
         contents expected-value-list result)
 
  (if (probe-file path-out)(delete-file path-out))
         
   (dolist (jth features)
   
    (ewaves-trade-bins2 jth)
    (setq expected-value-list nil)
    (dolist (ith ewaves-bin-codes)
     (setq contents
           (gethash ith *ewaves-trade-warehouse*))
     (setq result 0 )
     (dolist (kth contents)
         (setq result (+ result (nth 19 kth))))      
     (setq expected-value-list
      (acons (car ith) (round (/ result (length contents))) expected-value-list)));;;closes the dolist over bin-codes
  
     (with-open-file (stream path-out :direction :output :if-exists :append :if-does-not-exist :create) 
         (vsort expected-value-list #'> 'cdr)
         (format stream "~S~%" expected-value-list)
        )) ;;;;closes the with-open-file
 ))  

;;;takes the highest average from each indicator 
;;;based on the bin
(defun bin-classifier-estimate (bin)
  (let ((path "~/exitpoints/bin-weights.dat")
        scores bin-scores)
  
    (with-open-file (str path :direction :input)
    (dotimes (kth (length bin))
      (push  (read str nil 'eof) scores)))
;;;;scores are the association lists for each indicator in order      
    (setq scores (reverse scores))  
     
     (dotimes (ith (length bin))
         (push (cdr (assoc (nth ith bin) (nth ith scores))) bin-scores))
     
     (round (/ (list-sum bin-scores)(length bin-scores))) 
        
  ))   
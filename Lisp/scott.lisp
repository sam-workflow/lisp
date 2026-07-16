
;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;store the four indicators in a list in a hashtable
;;;(trend cycle macd elmo) is stored by the market name
(defparameter *DMI* (make-hash-table))
(defparameter *SMI* (make-hash-table))

(defun scott (&optional (tdate nil) (market-list (append *position-list* *forex-list*)))
  (let ((counter 0) dfl dfh mac-s elmoturns  period   match  time-ratios
         date ignore 3step-signal5  can can1 pins  path1  ftz rsd rsd1
         mom-div cci-diff exitpoint rr-short rr-long date-1 date-2
         ts5 ts15 ts45 sst sst1 ti ti1 ti2 srr srr1 srr2)

    (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
    (set-cat-list)
    (setq path1 (string-append "~/exitpoints/" (format nil "prod.dat")))
    (if (probe-file path1)
        (delete-file path1))

;    (when (test-open-positions)
 ;         (format T "The symbol ~A is not in *position-list* or *forex-list*" (test-open-positions))(break))
 ;   (shell (string-append "cp " "/home/mk-data/luis/prod.dat" " /home/mk-data/luis/yprod.dat"))

    (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%   MKT   CLOSE  EP TS CAN PIN MAT CYCLE MCD TCR FTZ MDIV S21 T5 T15 T45 RSD RR~%"
                         (date-convert date)))
     (dolist (market market-list)

          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

          (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

            (print-string stream (format nil "~%~A" market) 8)

           ; (if (member market '(TY.D1B US.D1B))
           ;    (format stream "~8@A" (convert-to-32nds (getd date 'close)))
             (format stream "~8F"
               (* (nth 4 (assoc *data-name* *C-list*))(round (getd date 'close) (nth 4 (assoc *data-name* *C-list*)))))

         (multiple-value-setq (rr-short rr-long) (standard-reward-risk date))
         (setq can (candle1 date))
         (setq cci-diff (- (commodity-channel-index date 21) (commodity-channel-index (add-mkt-days date -1) 21)))
         (setq can (cond ((and (eql can 'UP) (plusp cci-diff)) 'UP)
                         ((and (eql can 'DN)(minusp cci-diff)) 'DN)
                         (t nil)))

          (setq pins (pinpoint date))
          (setq match (matches date))
          (setq can1 (candle-composite date 3))
          (multiple-value-setq (period ignore dfl dfh) (cycles-d date market 10 30 nil))

          (setq mac-s (macd-signal date 12 26 9))
          (setq mom-div (momentum-divergence1 date 5 13))
          (setq ftz (fibonacci-target-zones date))
          (setq time-ratios (time-cycle-ratios date))

         (setq date-1 (getd date 'ydate) date-2 (getd date-1 'ydate))

         (setq 3step-signal5 (zero-proj date 5))

         (setq ts5 (trend-signal date 5))
         (setq ts15 (trend-signal date 15))
         (setq ts45 (trend-signal date 45))

         (setq sst (round (slow-stochastic date 21)) sst1 (round (slow-stochastic date-1 21)))
         (setq rsd (round (rsi2x date 14 )) rsd1 (round (rsi2x date-1 14 )))
         (setq srr (or (standard-reward-risk1 date) 'FT) srr1 (or (standard-reward-risk1 date-1) 'FT)
               srr2 (or (standard-reward-risk1 date-2) 'FT))
         (setq ti (timing-line date) ti1 (timing-line date-1) ti2 (timing-line date-2))


  (setq exitpoint

   (cond ((and (>= rsd 0)
               (or (<= sst 20)(<= sst1 20))
               (or (>= ti 4)(>= ti1 4)(>= ti2 4))
               (neql srr 'OB)
               (member ts5 '(UP CU))
               (or (member ts15 '(UP CU))
                   (member ts45 '(UP CU)))
               )
                            'L1)
         ((and  (<= rsd 0)
                (or (>= sst 80)(>= sst1 80))
                (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                (neql srr 'OS)
                (member ts5 '(DN CD))
                (or (member ts15 '(DN CD))
                    (member ts45 '(DN CD)))
                   )
                             'H1)
         ((and (>= rsd rsd1)
               (< sst 80)
               (member ts5 '(UP CU))
               (member ts15 '(UP CU))
               (member ts15 '(UP CU))
               (or (eql srr 'OS)
                   (eql srr1 'OS)
                   (eql srr2 'OS))
                 )
                            'L2)
         ((and (<= rsd rsd1)
               (> sst 20)
               (member ts5 '(DN CD))
               (member ts15 '(DN CD))
               (member ts45 '(DN CD))
               (or (eql srr 'OB)
                   (eql srr1 'OB)
                   (eql srr2 'OB))
                 )
                             'H2)
          ((and (>= rsd 0)
                (< sst 80)
                (member ts5 '(UP CU))
                (member ts15 '(UP CU))
                (member ts45 '(UP CU))
                (neql srr 'OB)
                (neql srr1 'OB)
                  )
                           'L3)
          ((and (<= rsd 0)
                (> sst 20)
                (member ts5 '(DN CD))
                (member ts15 '(DN CD))
                (member ts45 '(DN CD))
                (neql srr 'OS)
                (neql srr1 'OS)
                   )
                          'H3)
           ((and (>= rsd rsd1)
                 (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                 (member ts5 '(UP CU))
                 (or (eql srr 'OS)
                     (eql srr1 'OS)
                     (eql srr2 'OS))
                     )
                             'L4)
           ((and (<= rsd rsd1)
                 (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                 (member ts5 '(DN CD))
                 (or (eql srr 'OB)
                     (eql srr1 'OB)
                     (eql srr2 'OB))
                   )
                          'H4)
           ((and (>= rsd rsd1)
                 (<= sst 20)
                 (member ts5 '(UP CU))
                 (or (member ts15 '(UP CU))
                     (member ts45 '(UP CU)))
                 (or (eql srr 'OS)
                     (eql srr1 'OS)
                     (eql srr2 'OS))
                     )
                           'L5)
           ((and (<= rsd rsd1)
                 (>= sst 80)
                 (member ts5 '(DN CD))
                 (or (member ts15 '(DN CD))
                     (member ts45 '(DN CD)))
                 (or (eql srr 'OB)
                     (eql srr1 'OB)
                     (eql srr2 'OB))
                     )
                          'H5)

              (t nil)) )

         (format stream "~3@A" (if exitpoint exitpoint "   "))
         (format stream "~3@A" ti)
         (format stream " ~3@A" (if can1 can1 "   "))
         (format stream " ~3@A" (if (> pins 0) pins "   "))
         (format stream "~3@A " (if (zerop (truncate match 3)) "   " (truncate match 3)))

         (format stream " ~2@A~4@A" period (cond ((and
                                                  (member dfl '(1 2))) 'UP)
                                                 ((and
                                                  (member dfh '(1 2))) 'DN)
                                                  (t "    ")))

          (format stream "~3@A " (cond ((member mac-s '(1 2)) 1)
                                        ((member mac-s '(-1 -2)) 1)
                                        (t "  ")))

          (format stream "~3@A" (if (zerop (truncate time-ratios 2)) "   " (truncate time-ratios 2)))

          (format stream "~3@A" (if (zerop ftz) "   " ftz))

          (format stream "~4@A " (cond ((eql mom-div 'DN) 'DN)
                                       ((eql mom-div 'UP) 'UP)
                                       (t "    ")))

          (format stream " ~3@A" sst)

          (format stream " ~3@A" ts5)
          (format stream " ~3@A" ts15)
          (format stream " ~3@A" ts45)

          (format stream "~3@A" rsd)

        ;  (format stream "~3@A " (cond ((eql 3step-signal5 'DN) 'DN)
        ;                               ((eql 3step-signal5 'UP) 'UP)
        ;;                               (t "   ")))



          (format stream "~3@A" (if (< rr-short rr-long)
                                    (if (minusp rr-short) 'OS " ")
                                  (if (minusp rr-long) 'OB " ")))

          (setf (gethash market *DMI*)
             (list
                   (cond ((eql can1 'UP) 'UP)
                         ((eql can1 'DN) 'DN)
                         (t 'FT))

                   ts5


                   (cond ((and (eql (n-day-low date 2)(n-day-low date (ceiling (/ period 2))))
                          (member dfl '(1 2))) 'UP) ;;;cycle turns
                         ((and (eql (n-day-high date 2)(n-day-high date (ceiling (/ period 2))))
                          (member dfh '(1 2))) 'DN)
                         (t 'FT))


                   (cond ((member mac-s '(1 2)) 'DN) ;;;macd signal worth about 58 points
                         ((member mac-s '(-1 -2)) 'UP)
                         (t 'FT))


                                  ))


          (setf (gethash market *SMI*)
             (list

                   (cond ((and (eql (n-day-low date 2)(n-day-low date (ceiling (/ period 2))))
                          (member dfl '(1 2))) 'UP) ;;;cycle low or high
                         ((and (eql (n-day-high date 2)(n-day-high date (ceiling (/ period 2))))
                          (member dfh '(1 2))) 'DN)
                         (t 'FT))

                   (cond ((member mac-s '(1 2)) 'DN)  ;;;MACD signal
                         ((member mac-s '(-1 -2)) 'UP)
                         (t 'FT))


                   (cond ((and (eql (n-day-high date 6) (n-day-high date 2));;;elmoturns
                               elmoturns) 'DN)
                         ((and (eql (n-day-low date 6)(n-day-low date 2))
                               elmoturns) 'UP)
                         (t 'FT))

                   (cond ((eql can 'UP) 'UP);;;candlesticks
                         ((eql can 'DN) 'DN)
                         (t 'FT))

                   (cond ((eql mom-div 'UP) 'UP) ;;;Divergence
                         ((eql mom-div 'DN) 'DN)
                         (t 'FT))


                   3Step-signal5

                   ts15


                         ));;closes the list and setf

                  
          (if (zerop (mod counter 5)) (terpri stream))
          );;closes with open file
   )
  ;  (shell (string-append "cp " path1 " /home/mk-data/luis"))
    ))



(defun forex-scott (&optional (tdate nil) (market-list *forex-list*))
  (let ((counter 0)  
        date path1 reward  sign aveh avel exth extl
        pivw ccis5   wpp2 pivd gsi
       epmad  wp fsts5-3 fsts21-5 ldv )
   
    (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
    (set-cat-list)
    (setq path1 (string-append "~/exitpoints/" (format nil "forex-prod.dat")))
    (if (probe-file path1)
        (delete-file path1))

    (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~% MKT      CLOSE    SIGN CCIS5  WP  WPP2  PIVD  PIVW LDV FST5-3 FST21-5   GSI   AVEH     AVEL      EXTH     EXTL~%" (date-convert date)))
    (dolist (market market-list)

          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

          (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

            (print-string stream (format nil "~%~A" market) 7)

            (if (member market '(TY.D1B US.D1B))
               (format stream " ~8@A" (convert-to-32nds (getd date 'close)))
             (format stream " ~8F"
               (* (nth 4 (assoc *data-name* *C-list*))(round (getd date 'close) (nth 4 (assoc *data-name* *C-list*)))) ))


          (setq fsts21-5 (fast-stochastic-signal date 21 5)
                fsts5-3 (fast-stochastic-signal date 5 3)
                ldv (ldev-index date 7) wp (wpp1 date) gsi (gann-slope-index date 5 63)
                 wpp2 (wpp tdate) 
             
                )
        
             (setq ;can (candle-composite date 3) macdd (ep-macd-diff-index date 21 63 11)
               ; pp (pinpoint date) cci5r5 (cci-range-index date 5 5)
                ; pt (price-turns date) zb11 (zero-strength-swing11 date ) chan11 (channel-direction11 date)
                pivw (pivot-index date 'week) pivd (pivot-index date 'day)
                epmad (ep-macd-diff-index date 9 18 4) 
                ccis5 (cci-signal1 date 5)
                reward (* (calculate-point-value date)(abs (- (getd date 'close) (nth-value 2 (parabolic-stops date))))))
       
              
          (setq aveh (my-pretty-price (ave date 3 'high)) avel (my-pretty-price (ave date 3 'low))
                exth (lregress date 3 'high) extl (lregress date 3 'low)
              
              )
          (setq sign 
                (cond ((and  (= ccis5 -1)(/= fsts21-5 -1)(/= fsts5-3 -1) 
                          (member ldv '(1 2 3)) (member gsi '(2 3 4))
                          (member wpp2 '(ODF UUD OUF OUH IUH UUU7 DUU DUD IUF UUU IDF UDD))
                          (>= pivd -1)(>= wp 0)(member pivw '(1 2 3 4))) 'Short)
                      ((and (= ccis5 1) (/= fsts21-5 1)(/= fsts5-3 1)
                           (member ldv '(-1 -2 -3))  (member gsi '(-2 -3 -4))
                           (member wpp2 '(OUL UDU ODL UDD DDD UUU DDU IUL IDL ODF IDF DDD7))
                          (<= pivd 1)(<= wp 0)(member pivw '(-1 -2 -3 -4))) 'Long)
                       (t nil)))
                     


          (format stream "   ~5@A ~5@A ~3@A  ~4@A ~4@A ~4@A   ~4@A   ~3@A   ~5@A  ~4@A  ~
                          ~6@A  ~6@A    ~6@A  ~7@A~%"
                sign ccis5 wp wpp2  pivd pivw ldv fsts5-3 fsts21-5
                gsi    
                aveh avel exth extl )
    
          (if (zerop (mod counter 5)) (terpri stream))
          );;closes with open file
   )
  ;  (shell (string-append "cp " path1 " /home/mk-data/luis"))
))


(defun fore-scott (&optional (tdate nil) (market-list *day-list*))
  (let ((counter 0) cci21h11 cci21l11 cci21r11 rsi2h3 rsi2l3 cci5h3 cci5l3 pp trig5   dirp wpp  chan11 wpp1 
         
         date path1 dir reward can form2  cci21 cci5x   pt  tcr rsi2x  cci5r5 contx  ctarg ch345 zb11 zb4 volr)
   
    (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
    (set-cat-list)
    (setq path1 (string-append "~/exitpoints/" (format nil "fore-prod.dat")))
    (if (probe-file path1)
        (delete-file path1))


    (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~% MKT    CLOSE  PSY  DIR   CAN+  TCR+PP  PT WPP1 VOLR FORM2    TARG    CH345 CONT RSI2X CCI5X CCI21 CH11  TRIG       CCI5R5  RWRD ~%" (date-convert date)))
    (dolist (market market-list)

          (incf counter)  (set-market market)
          (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))

          (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

            (print-string stream (format nil "~%~A" market) 6)

            (if (member market '(TY.D1B US.D1B))
               (format stream " ~7@A" (convert-to-32nds (getd date 'close)))
             (format stream " ~7F"
               (* (nth 4 (assoc *data-name* *C-list*))(round (getd date 'close) (nth 4 (assoc *data-name* *C-list*)))) ))

          (multiple-value-setq (dirp rsi2l3 rsi2h3)(rsi2x-direction date 2))
          (multiple-value-setq (cci5h3 cci5l3) (cci-high-low date 5 3))
          (multiple-value-setq (cci21h11 cci21l11) (cci-high-low date 21 11))  
          (setq dir (parabolic-stops date)  cci21r11 (cci-range-index date 21 11)
                rsi2x (rsi2x-ob-os date 2) contx (cci-context date)  volr (truncate (vril date 4 1) 3)
                tcr (time-cycle-ratios date) trig5 (cci-signal1 date 5) form2 (formation-signal date 2)
                cci5x (ccix-ob-os date 5 3) wpp (wpp-index0 date) wpp1 (wpp1-composite date 2)) 

          (setq can (candle-composite date 1)  
                cci21 (cci-level-index date 21) pp (pinpoint date) cci5r5 (cci-range-index date 5 5)
                 pt (price-turns date) zb11 (zero-strength-swing11 date ) chan11 (channel-direction11 date)
                zb4 (zero-strength-swing345 date )
                ch345 (channel-direction-swing-index345 date) ctarg (nth-value 1 (channel-trend date 4)); z2 (nth-value 4 (zero-proj1 date 5))
                reward (* (calculate-point-value date)(abs (- (getd date 'close) (nth-value 2 (parabolic-stops date))))))
          (format stream " ~5@A    ~2@A    ~2@A  ~3@A ~3@A  ~4@A  ~4@A  ~7,4F  ~5@A  ~3@A   ~4@A   ~4@A ~6@A ~
                         ~6@A  ~6@A  ~5@A   ~5@A  ~%"
                 dir (+ (- can) wpp) (truncate (+ tcr pp) 3) pt (- wpp1) volr form2 (my-pretty-price ctarg) ch345  contx rsi2x cci5x  cci21 
                chan11 trig5 (round cci5r5) (round reward) )
    
          (if (zerop (mod counter 5)) (terpri stream))
          );;closes with open file
   )
  ;  (shell (string-append "cp " path1 " /home/mk-data/luis"))
))


       
(defun test-timing-index (tdate market &optional (days 1000))
  (let* ((date tdate) pins (match 0) can1 ignore dfl dfh mac-s timing-score
           time-ratios  total-score1 times scores test-list test-list1
         (path1 "~/cycles/time-test.dat")  percent-power-score ftz
        deviations total-score percent-accuracy-score
        )
  
      (set-cat-list)(set-market market)(setq *n-filt* 21)
   (multiple-value-setq (ignore times)
      (find-all-primitives (format nil "~sA" (add-mkt-days date (- days))) (format nil "~sP" date)))
   (setq times (butlast (mapcar #'getnumdate times)))

;;;times is a list of actual dates of highs and lows
     (dotimes (ith days)
         (setq pins (pinpoint date))
         (setq match (matches date))
         (setq can1 (candle-composite date 3))
         (multiple-value-setq (ignore ignore dfl dfh) (cycles-d date market 8 21 nil))
         (setq mac-s (macd-signal date 12 26 9))

         (setq ftz (fibonacci-target-zones date))
          (setq time-ratios (time-cycle-ratios date))
          (setq timing-score (+ (if (member can1 '(up dn)) 1 0)
                                pins
                                (if (> match 2) (- match 2) 0)
                                (if (member dfh '(1 2)) 1 0)
                                (if (member dfl '(1 2)) 1 0)
                                (if (member mac-s '(-1 -2 1 2)) 1 0)
                                (if (> time-ratios 1) (1- time-ratios) 0)
                                ftz
                                 ))
                             
         (push (list date timing-score (if (member date times) T nil)) test-list)
         (format T "~%~A  ~2@A  ~A" date timing-score (if (member date times) T nil))
         (setq date (getd date 'ydate)))

    (setq test-list1
      (mapcar #'(lambda(s) (if (> (cadr s) 4) (car s) nil))  test-list))
    (setq test-list1
       (remove nil test-list1))


;;;need to remove all actuals that are before the predictions started.
;     (setq times (remove-if #'(lambda(s) (< s first-prediction-date)) times))
;;;First measure how near a prediction date is to every actual high or low date?
;;;next what is the minimum distance?
;;; repeat for all prediction dates
     (dolist (jth test-list1)
         (dolist (kth times)
           (push (eql kth jth) deviations))
         (push (if (member T deviations) 1 0) scores)
         (setq deviations nil))
 ;;;count the number of predictions that are one day from an actual
 ;;;the percent-accuracy-score is the probability that a prediction will be within one day
 ;;;of a actual turn
     (setq total-score (count-if #'(lambda(s) (= s 1)) scores))
     (setq percent-accuracy-score  (/ total-score (length scores)))

     (setq scores nil deviations nil)
 ;;;Power of the indicator is how how will and actual turn be predicted?
        (dolist (kth times)
         (dolist (jth test-list1)
           (push (eql jth kth) deviations))
         (push (if (member T deviations) 1 0) scores)
         (setq deviations nil))

     (setq total-score1 (count-if #'(lambda(s) (= s 1)) scores))
     (setq percent-power-score  (/ total-score1 (length scores)))


     (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (jth test-list)
        (format stream "~%~A  ~A   ~A" (first jth)(second jth)(third jth))
        ))

     (format T "~%Prob. of prediction= ~A  Prob. of Actual turn= ~A ~% ~
         Cond. P(Ac/Pr)= ~A Cond. P(Pr/Ac)= ~A~%"
      (my-round (/ (length test-list1) days) 3)
      (my-round (/ (length times)  days) 3)
      (my-round percent-accuracy-score 3)
      (my-round percent-power-score 3))

      (format T "~% Total number of days= ~A  Number of Predictions= ~A ~
         Number of Actuals= ~A  Number predictions correct= ~A Number of Actuals predicted= ~A"
          days (length test-list1) (length times) total-score  total-score1)
))


;;;;;testing function of timing

(defun timing-test (date market &optional (days 100))
 (let (ignore  times (path1 "~/exitpoints/time-test.dat") time-list
        )
   
   (setq time-list nil)
   (set-cat-list)(set-market market)(setq *n-filt* 21)
   (multiple-value-setq (ignore times)
      (find-all-primitives (format nil "~s" (add-mkt-days date (- days))) (format nil "~s" date)))
   (setq times (mapcar #'getnumdate times))

   (setq time-list (mapcar #'(lambda (s)
                                (list s (round (time-cycle-ratios s)) (round (timing-line-signal2 s 3 11)))) times))


   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (jth time-list)
       (format stream "~%~A  ~A   ~A" (first jth)(second jth)(third jth))))

    (format T "~%Number of Actual Turns = ~A  Timing-index = ~A   CS321 = ~A" (length time-list)
      (count-if  #'(lambda(s) (>= (cadr s) 1)) time-list)
    (length (remove nil (mapcar #'(lambda(s) (or (>= (third s) 80)(<= (third s) 20))) time-list))) )
  ))

(defun foobar (date market days)
  (let (ss14-list)
  (declare (special averages))
  (set-market market)
  (dotimes (ith days)
    (push (fibonacci-target-zones date) ss14-list)
    (setq date (getd date 'ydate)))
  (format T "~%number of days = ~A   Number overbought/oversold = ~A" (length ss14-list)
                                     (count 0 ss14-list))
 (setq averages (push (/ (count-if #'(lambda(s) (> s 0)) ss14-list) (length ss14-list)) averages))

))


;(setq record-list nil)

(defun greg (&optional (tdate nil) (market 'dj.d1b)(days 60))
  (let ((counter 0)  date date-1 date-2  path1 prices times
          record exitpoint srr srr1 srr2 sst sst1 rsd rsd1 ti ti1 ti2
         ts5 ts15 ts45 rr-short rr-long  prev-signal
          (filt *n-filt*))
   
    (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
    (set-cat-list)(set-market market)
    (setq path1 (string-append "/home/register/cycles/" (format nil "~(~S~)-turns.dat" market)))
    (if (probe-file path1)
        (delete-file path1))

    (setq *n-filt* 14 *time-interval* 1440)
    (multiple-value-setq (prices times)
       (find-all-primitives (format nil "~sC" (add-mkt-days date (- (* 2 days)))) (format nil "~sC" date)))
    (setq times (butlast (mapcar #'getnumdate times)))
    (setq *n-filt* filt *time-interval* 'daily-high-low)

    (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A  ~A~%   DATE     CLOSE  EP  TS SS21 RSD T5 T15 T45 RR~%"
           (date-convert date) market));;;closes this with-open-file

    (setq date (add-mkt-days date (1+ (- days))))
    (setq times (remove-if #'(lambda (s) (< s date)) times))
    (dotimes (ith days)
       (incf counter)
       (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
            (format stream "~%~A~A" date (if (member date times) "*" " "))
             (push date record)
            (if (member market '(TY.D1B US.D1B))
               (format stream " ~8@A" (convert-to-32nds (getd date 'close)))
             (format stream " ~8F"
               (* (nth 4 (assoc *data-name* *C-list*))(round (getd date 'close) (nth 4 (assoc *data-name* *C-list*)))) ))
             (push (getd date 'close) record)
          (setq date-1 (getd date 'ydate) date-2 (getd date-1 'ydate))

          (setq prev-signal (vsignals date 15 *entry-factor-swing* 3))

          (multiple-value-setq (rr-short rr-long) (standard-reward-risk date))

         (setq  sst (round (slow-stochastic date 21)) sst1 (round (slow-stochastic date-1 21))
                rsd (round (rsi2x date 14 )) rsd1 (round (rsi2x date-1 14 ))
                srr (or (standard-reward-risk1 date) 'FT) srr1 (or (standard-reward-risk1 date-1) 'FT)
                srr2 (or (standard-reward-risk1 date-2) 'FT)
                ti (timing-line date) ti1 (timing-line date-1) ti2 (timing-line date-2)
                ts5 (trend-signal date 5) ts15 (trend-signal date 15)
                ts45 (trend-signal date 45))

   (setq exitpoint

   (cond ((and (>= rsd 0)
               (or (<= sst 20)(<= sst1 20))
               (or (>= ti 4)(>= ti1 4)(>= ti2 4))
               (neql srr 'OB)
               (member ts5 '(UP CU))
               (or (member ts15 '(UP CU))
                   (member ts45 '(UP CU)))
               )
                            'L1)
         ((and  (<= rsd 0)
                (or (>= sst 80)(>= sst1 80))
                (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                (neql srr 'OS)
                (member ts5 '(DN CD))
                (or (member ts15 '(DN CD))
                    (member ts45 '(DN CD)))
                   )
                             'H1)
         ((and (>= rsd rsd1)
               (< sst 80)
               (member ts5 '(UP CU))
               (member ts15 '(UP CU))
               (member ts15 '(UP CU))
               (or (eql srr 'UP)
                   (eql srr1 'OS)
                   (eql srr2 'OS))
                 )
                            'L2)
         ((and (<= rsd rsd1)
               (> sst 20)
               (member ts5 '(DN CD))
               (member ts15 '(DN CD))
               (member ts45 '(DN CD))
               (or (eql srr 'OB)
                   (eql srr1 'OB)
                   (eql srr2 'OB))
                 )
                             'H2)
          ((and (>= rsd 0)
                (< sst 80)
                (member ts5 '(UP CU))
                (member ts15 '(UP CU))
                (member ts45 '(UP CU))
                (neql srr 'OB)
                (neql srr1 'OB)
                  )
                           'L3)
          ((and (<= rsd 0)
                (> sst 20)
                (member ts5 '(DN CD))
                (member ts15 '(DN CD))
                (member ts45 '(DN CD))
                (neql srr 'OS)
                (neql srr1 'OS)
                   )
                          'H3)
           ((and (>= rsd rsd1)
                 (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                 (member ts5 '(UP CU))
                 (or (eql srr 'OS)
                     (eql srr1 'OS)
                     (eql srr2 'OS))
                     )
                             'L4)
           ((and (<= rsd rsd1)
                 (or (>= ti 4)(>= ti1 4)(>= ti2 4))
                 (member ts5 '(DN CD))
                 (or (eql srr 'OB)
                     (eql srr1 'OB)
                     (eql srr2 'OB))
                   )
                          'H4)
           ((and (>= rsd rsd1)
                 (<= sst 20)
                 (member ts5 '(UP CU))
                 (or (member ts15 '(UP CU))
                     (member ts45 '(UP CU)))
                 (or (eql srr 'OS)
                     (eql srr1 'OS)
                     (eql srr2 'OS))
                     )
                           'L5)
           ((and (<= rsd rsd1)
                 (>= sst 80)
                 (member ts5 '(DN CD))
                 (or (member ts15 '(DN CD))
                     (member ts45 '(DN CD)))
                 (or (eql srr 'OB)
                     (eql srr1 'OB)
                     (eql srr2 'OB))
                     )
                          'H5)

              (t nil)) )


         (format stream "~3@A ~3@A" (or exitpoint "   ") ti)

         (format stream " ~3@A " sst)
         (format stream "~2@A" rsd)

          (format stream "~3@A " ts5)
          (format stream "~3@A " ts15)
           (format stream "~3@A" ts45)

          (format stream "~3@A " (if (< rr-short rr-long)
                                     (if (minusp rr-short) 'OS " ")
                                  (if (minusp rr-long) 'OB " ")))
          (format stream "~4@A " prev-signal)

                  
          (if (zerop (mod counter 5)) (terpri stream))
          (setq date (getd date 'ndate))
          );;closes the with-openfile
   );;;closes the dotimes


  ;  (shell (string-append "cp " path1 " /home/mk-data/luis"))


    ))


(defun data-dump (&optional (tdate nil) (market-list *micro-list*));(append *position-list* *forex-list*)))
  (let ((counter 0) date  path1 test1)

    (maind-x)(set-cat-list)(set-market 'dj.d1b)
    (ifn tdate (setq tdate (car (last (month-days (get-latest-index-date))))))

    (setq path1 (string-append *output-upper-dir* (format nil "dump.txt")))


    (if (probe-file path1)
        (delete-file path1))
    (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
        (format stream "~A~%  MKT   MONTH    OPEN     HIGH     LOW     CLOSE     VOL       OI   ~%" (date-convert tdate)))

     (dolist (market market-list)

          (incf counter)  (set-market market)
          (setq date (car (last (month-days (get-latest-index-date)))))
          ;(ifn (<= tdate date)(break "~A is latest date for data in ~A market~%" date market))
          (format T "~A is latest date for data in ~A market~%" date market)
          (setq test1 (test-data (getd tdate 'ydate) tdate))
          (ifn (equal "DATA OK" test1)
           (format T (string-append test1 " for ~A market") market))

          (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

            (print-string stream (format nil "~%~A" market) 8)
            (format stream " ~A " (contract-month market tdate))
            (if (member market '(TY.D1B US.D1B))
               (format stream "~8@A" (convert-to-32nds (getd tdate 'open)))
             (format stream "~8F"
               (* (index-tick-size)(round (getd tdate 'open) (index-tick-size)))))

            (format stream " ")

            (if (member market '(TY.D1B US.D1B))
               (format stream "~8@A" (convert-to-32nds (getd tdate 'high)))
             (format stream "~8F"
               (* (index-tick-size)(round (getd tdate 'high) (index-tick-size)))))

             (format stream " ")
            (if (member market '(TY.D1B US.D1B))
               (format stream "~8@A" (convert-to-32nds (getd tdate 'low)))
             (format stream "~8F"
               (* (index-tick-size)(round (getd tdate 'low) (index-tick-size)))))

             (format stream " ")
            (if (member market '(TY.D1B US.D1B))
               (format stream "~8@A" (convert-to-32nds (getd tdate 'close)))
             (format stream "~8F"
               (* (index-tick-size)(round (getd tdate 'close) (index-tick-size)))))
 ;;;limit
            (format stream "   ~6A" (getd tdate 'volume))
            (format stream "  ~7D" (getd tdate 'openint))
          


           (if (zerop (mod counter 4)) (terpri stream))
          ));;;closes the with-open-file and dolist
         (rollovers-due tdate)
          ));;closes the let and defun
 ;;;;This is the version for EWI

(defun scott1 (date market days) 
  (let (can limit path1 date-1 date-2)
    
    (set-cat-list)
    (setq path1 (string-append "~/exitpoints/"  "lreg.csv"))
    (if (probe-file path1)
        (delete-file path1))

    (set-market market)
    (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "DATE,      $CHG,  LIMIT, CAN~%")
        (dotimes (ith days)
            
            (format stream "~A," date)
           ; (get-ninja-symbol market)
            (format stream " ~5A," (round (* (calculate-point-value date)(- (getd date 'close)(getd date 'open)))))
         

         (setq date-1 (getd date 'ydate) date-2 (getd date-1 'ydate))
      ;   (format T "~%cci21t = ~A cci21y = ~A cci21d = ~A date-1= ~A date-2 =~A"
       ;          (commodity-channel-index date-1 21) (commodity-channel-index date-2 21) 
       ;          (- (commodity-channel-index date-1 21) (commodity-channel-index  date-2 21))
       ;          date-1 date-2)
         (setq limit  (* (calculate-point-value date)(volatility date 63 3)))
        ; (setq cci21d (- (commodity-channel-index date-1 21) (commodity-channel-index  date-2 21)))
         (setq can (candle-composite date 2))
         (format stream " ~6,1F, ~2@A~%" limit can)
         (setq date date-1)
       );;;closes the dotimes
          );;closes with open file

    ))



(defun write-data-out (market date1 date2)
  (let (path (tdate date1) ninja-mkt)
    (setq ninja-mkt (cdr (assoc market *ninja-symbol*)))
  (maind-x)(set-cat-list) (set-market market)
   (setq path (string-append *output-upper-dir* (format nil "~A.csv" market)))
   (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop 
      
       (format str "~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A ~A~%" tdate ninja-mkt (contract-month market tdate)
               (getd tdate 'open) (getd tdate 'high)(getd tdate 'low)(getd tdate 'close)(getd tdate 'rollover)
	       (pinpoint tdate))
          (if (eql tdate date2) (return) (setq tdate (getd tdate 'ndate)))
))))



(defun ninjascript-trade-action-files (sdate edate)
  (let ((tdate sdate) nxdate hol)
     (maind-x)(set-cat-list)(set-market 'us.d1b)
;    (apply #'day-trade-bins3b *day-features3*);;for SCORE STARTER ENTRY
;     (apply #'day-trade-bins2b *day-features2*);;for EPIC
;     (apply #'day-trade-bins4b *day-features4*);;for FORE
     (apply #'day-trade-bins4bx *day-features4x*);;for FOREX

  (loop
     (if (> tdate edate)(return))
     (if (plusp (num-holidays (getd tdate 'ndate))) (setq hol T)(setq hol nil))
 ;    (day-swing-trades tdate hol)
;    (epic-trades tdate hol)
;      (fore-trades tdate hol)
     ; (forex-trades tdate hol)
      (setq nxdate (getd tdate 'ndate))

;    (when (probe-file  (string-append "ExitPoints/Ninja/ninja-epic-" (format nil "~A" nxdate) ".csv"))
;          (rename-file (string-append "ExitPoints/Ninja/ninja-epic-" (format nil "~A" nxdate) ".csv")
;                       (string-append "~/NinjaArchive/ninja-epic-" (format nil "~A" nxdate) ".csv")))

;     (when (probe-file  (string-append "ExitPoints/Ninja/ninja-triumph21-" (format nil "~A" nxdate) ".csv"))
;           (rename-file (string-append "ExitPoints/Ninja/ninja-triumph21-" (format nil "~A" nxdate) ".csv")
;                        (string-append "~/NinjaArchive/ninja-triumph21-" (format nil "~A" nxdate) ".csv")))

;      (when (probe-file  (string-append "ExitPoints/Ninja/ninja-triumph11-" (format nil "~A" nxdate) ".csv"))
;            (rename-file (string-append "ExitPoints/Ninja/ninja-triumph11-" (format nil "~A" nxdate) ".csv")
;                        (string-append "~/NinjaArchive/ninja-triumph11-" (format nil "~A" nxdate) ".csv")))

;      (when (probe-file  (string-append "ExitPoints/Ninja/ninja-entry-" (format nil "~A" nxdate) ".csv"))
;            (rename-file (string-append "ExitPoints/Ninja/ninja-entry-" (format nil "~A" nxdate) ".csv")
;                        (string-append "~/NinjaArchive/ninja-entry-" (format nil "~A" nxdate) ".csv")))


;      (when (probe-file  (string-append "ExitPoints/Ninja/ninja-fore-" (format nil "~A" nxdate) ".csv"))
;            (rename-file (string-append "ExitPoints/Ninja/ninja-fore-" (format nil "~A" nxdate) ".csv")
;                         (string-append "~/NinjaArchive/ninja-fore-" (format nil "~A" nxdate) ".csv")))
  
     
      (when (probe-file  (string-append "ExitPoints/Ninja/ninja-forex-" (format nil "~A" nxdate) ".csv"))
            (rename-file (string-append "ExitPoints/Ninja/ninja-forex-" (format nil "~A" nxdate) ".csv")
                        (string-append "~/NinjaArchive/ninja-forex-" (format nil "~A" nxdate) ".csv")))

   (set-market 'us.d1b)
     (setq tdate (getd tdate 'ndate))

     )
 ))
;;;this is for the override of the instrument.txt file with NinjaTrader
(defun dump-rollover-dates (markets start-date end-date)
   (let ((path-out (string-append *output-upper-dir* "ninja-rollovers.txt"))
        (date start-date))
    (maind-x) (set-cat-list)
   (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
     
      (dolist (market markets)
        (set-market market)(setq date start-date)
        (format str "@RollOver    ; ~A    ; 0    ; " (cdr (assoc market *ninja-symbol*)))
        (loop
           (if (getd date 'rollover)
               (format str "~A,~A " (ninja-contract-month (contract-month market date)) date))
           (setq date (getd date 'ndate))
           (if (or (not date) (> date end-date))(return))
            )
         (format str "~A~%" #\return)) ;;;writes the carriage return line feed  characters at end of line

)))

;;;;;;;;;these are for Nick
;;;add cloing prices for a file with a list of dates
(defun add-closing-prices (market path-in path-out)
   (let (dates dmys (path-temp "~/ExitPoints/temp.txt") )
       (maind-x)(set-cat-list)(set-market market)
       (rewrite-file path-in path-temp)
       (with-open-file (str path-temp :direction :input)
          (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
              ((eql record 'eof))
            (print record)
            (ifn (equal record "")
              (setq dates (cons (+ 20000000 (conv-to-ewaves-date record)) dates)))
            ))
;        (setq dates1 dates)

        (dolist (date dates)
           (push (list date (getd date 'close)) dmys))
;       (setq dmys1 dmys)
        (with-open-file (str1 path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (dmy dmys)
            (format str1 "~A, ~A~%" (car dmy)(second dmy))))
       
       


))


;;;this rewrites a file that has no newline characters
;;;substitutes #\newline for #\return
(defun rewrite-file ( path-in path-out)
   (let (chars )
      
       (with-open-file (str path-in :direction :input)
          (do ((record (read-char str nil 'eof) (read-char str nil 'eof)))
              ((eql record 'eof))
              (setq chars (cons record chars)))
            )
      
        (setq chars (substitute #\newline #\return chars))

        
     
        (with-open-file (str1 path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (dmy (reverse chars))
            (format str1 "~A" dmy)))      


))

(defun test-indicator (market tdate &optional (days 21))
  (let (date (path (string-append  "~/exitpoints/" (format nil "~S" market) "test.csv"))
	       (period 8) turns turn-dates high-dates low-dates sdate 
      ; dir dfh dfl ;paraturns
        )
     (maind-x)(set-cat-list)
     (set-market market)(setq *n-filt* (* 1 period))
    (setq sdate (add-mkt-days tdate (- (+ (* 8 period) days)))
          date (add-mkt-days tdate (- days)))
  ; (setq *time-interval* 1440)
     (setq  sdate (add-mkt-days tdate (- (+ days (* 8 *n-filt*))))) (print sdate)
   (multiple-value-setq (turns turn-dates)  (find-all-primitives (format nil "~A" sdate)  
                                                                   (format nil "~A" tdate)))
  ; (setq *time-interval* 'daily-high-low)
  (setq turn-dates (mapcar #'(lambda(s) (getnumdate s)) turn-dates)) 
  (setq turn-dates (butlast turn-dates 3) turns (butlast turns 3))


    (do* ((prs turns (cdr prs))
         (inds turn-dates (cdr inds))
         (kth (first turns) (first prs))
         (kth+1 (second turns) (second prs))
         (ith (first turn-dates) (first inds)))

        ((null kth+1))
        (if (< kth kth+1)(push ith low-dates)(push ith high-dates))
      )
  ; (format t "NUM low-dates= ~A NUM high-dates= ~A~%" (length low-dates)(length high-dates))
   (setq low-dates (reverse low-dates) high-dates (reverse high-dates))

  
  (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format str
    "  DATE      CAN   WPP      ACTION     ACC13  MACS   CLOSE    GSIP GSIC   VSIG  CSIG     REV      RISK~%")
  (dotimes (ith (1+ days))
   ; (multiple-value-setq (dir dfh dfl)(amplitude date 11)) 
    (format str "~8A ~1@A ~4@A  ~4@A  ~15A ~3@A  ~3@A  ~8@A   ~5@A ~5@A  ~5@A ~6@A  ~6@A ~7@A~%"
	            date 
                    (cond ((member date high-dates) "H")
                          ((member date low-dates) "L")
                          (t " "))
                     (candle date 3)(wpp date )
                     (find-best-edge-swing-trade1 date nil nil)
                
                     (ep-roc-change-index date 13 2)
                    
                     (ep-macd-signal date 12 26 9)
                   
                     (if (member *data-name* '(ty.d1b us.d1b))(convert-to-32nds (getd date 'close))
			 (getd date 'close))
		     (nth-value 1 (gann-slope-index date 8))(gann-slope-index date 8)
		     (vsignals1 date) 
		     (bin-classifier-swingtradesb date *swing-features*)
		     (if (member *data-name* '(ty.d1b us.d1b))
			 (convert-to-32nds (nth-value 2 (vsignals1 date)))
			 (nth-value 2 (vsignals1 date)))
		     (my-pretty-price (nth-value 2 (vprices date))))
      (setq date (getd date 'ndate))    
   );;;closes the dotimes
  (terpri str)
    (dotimes (ith (1- (length turns)))
   (format str "~A   ~A~%" (nth ith turn-dates)(nth ith turns)))
    
)))

(defun test-indicator2 (tdate &optional (markets *micro-list*) (days 15))
  
       (build-swingtrade-warehouse aa) ;*micro-list*)
      ; (setq *swing-features* (find-best-swing-indicator-set nil))
       (apply #'swing-trade-binsb T *swing-features*)
  
  (dolist (ith markets)

    (test-indicator ith tdate days)))

(defun test-indicator1 (tdate &optional
       (markets  *micro-list*))
  (let ((path "~/exitpoints/test1.csv")  (counter 0) (bull-cntr 0) (bear-cntr 0)
        gsic gsip)
  
   (maind-x)(set-cat-list)
   
   (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format str "~A~%" (date-convert tdate))
    (format str "MKT      VS4  RSI9   ROC5  CCI5   ZS5 ZS13  CD8   WPP   PIVT  ROC13 GSIP GSIC   VSIG  ~%")  
  (dolist (ith markets)
    (incf counter)(set-market ith)(multiple-value-setq (gsic gsip) (gann-slope-index tdate 8))
   (format str "~4@A~3A ~4@A  ~3@A  ~5,0@F  ~5,0@F ~4@A ~4@A ~4@A  ~4@A  ~4@A  ~4@A ~4@A ~4@A    ~4@A~%"
           (get-ts-symbol ith)
	  (ts-contract-month (contract-month *data-name* tdate)) 
	                       
                 
            (if (and (getd tdate 'volume)(neql (getd tdate 'volume) 0))
		     (plusp (nth-value 1 (lregress tdate 4 'volume))) "NA") 
            ;(if (and (getd tdate 'openint)(neql (getd tdate 'openint) 0)) 
					;		(plusp (nth-value 1 (lregress tdate 4 'openint))) "NA")
	
                     (rsi tdate 9)
		 
                     (ep-roc-change-index tdate 5 2)
                     (my-round (commodity-channel-index tdate 5) 1)		     
                    
                     (zero-strength tdate 5) 
                     (zero-strength tdate 13)
		     
                 
		     (channel-direction tdate 8)
		     (wpp tdate)		     
		
		     (pivot-turn tdate 'month)
	       
		  
		    (or (ep-roc-change-index tdate 13 1) 0) gsip 
		    gsic 
		     (vsignals1 tdate )
		     
		     
                   )
        (if (zerop (mod counter 4)) (terpri str)))) 
   
   (with-open-file (str path :direction :output :if-exists :append :if-does-not-exist :create)
    (format str "~%~A~%" (date-convert tdate)) (setq counter 0)
  (format str "MKT        VS4     PREV    CD13   ZS3   CD8  ZS13   ZS8 BPL9 PIVTM GSI5 WPP ROC13  L     S  VSIG~%")
  
  (dolist (ith markets)
    (incf counter)(set-market ith)(setq bull-cntr 0)(setq bear-cntr 0 gsic (gann-slope-index tdate 8)
							  gsip (nth-value 1 (gann-slope-index tdate 8)))
    (format str "~4@A~3A  ~4@A ~10@A  ~4@A  ~4@A  ~4@A  ~4@A  ~4@A ~4@A ~4@A ~4@A ~4@A ~
                 ~4@A ~4@A  ~4@A  ~4@A~%" (get-ts-symbol ith)(ts-contract-month (contract-month *data-name* tdate)) 
		 
                 (if (and (getd tdate 'volume) (neql (getd tdate 'volume) 0))
		     (plusp (nth-value 1 (lregress tdate 4 'volume))) "NA")
                ;   (if (and (getd tdate 'openint) (neql (getd tdate 'openint) 0))
		;	 (plusp (nth-value 1 (lregress tdate 4 'openint))) "NA")
		   (nth-value 2 (vsignals1 tdate ))  
                  ;   (cond ((and (getd tdate 'openint)(neql (getd tdate 'openint) 0) (minusp gsic)
		;		 (member (volume-openint-div tdate 4) '(UD DD)))(incf bull-cntr) 'L) 
		 ;          ((and (getd tdate 'openint)(neql (getd tdate 'openint) 0)(plusp gsic)
		;		 (member (volume-openint-div tdate 4) '(UD)))(incf bear-cntr) 'S)
		;	   (t 0))              
		      (case (channel-direction tdate 13);;feature 6
	                    ((AT UC+ UC)(incf bull-cntr) 'L)
	                    ((IC- BT) (decf bull-cntr)(decf bear-cntr)'X)
	                    (otherwise 'F))	        
                        (case (zero-strength tdate 3) ;feature 4
	                      ((3SU SUP)(incf bull-cntr)(incf bear-cntr) 'B)
	                      ((LDU LDD BZT)(incf bull-cntr) 'L)
	                      ((WDN)(incf bear-cntr) 'S)
	                      ((AZT)(decf bull-cntr)(decf bear-cntr) 'X)
	                      (otherwise 'F))
	       	     
		  
                         (case (channel-direction tdate 8) ;;feature 9 ***
	                       ((AT US)(incf bull-cntr)(incf bear-cntr) 'B)
                               ((DC UC- UC+ UC)(incf bull-cntr) 'L)	     
	                       ((BT2 DS)(incf bear-cntr) 'S)
	                       ((AT2 IC+ IC-)(decf bull-cntr)(decf bear-cntr) 'X)
	                       (otherwise 'F))
                     (case (zero-strength tdate 13) ;feature 4
	                   ((AZT 3SD) (incf bull-cntr) 'L)
	                   (WUP (incf bear-cntr) 'S)
	                   ((BZT WDN)(decf bull-cntr)(decf bear-cntr) 'X)
	                   (otherwise 'F))    
		     (case (zero-strength tdate 8);;;feature 8
	                   ((BZT AZT DNT WDN SUP)(incf bull-cntr)(incf bear-cntr) 'B)
	                   ((3SU)(incf bull-cntr) 'L)	      
	                   ((LDD WUP)(decf bull-cntr)(decf bear-cntr) 'X)
	                   (otherwise 'F))
	     		     
	       	      (case (bpl-index tdate 9)  ;feature 5 ***;
		            (U3 (incf bull-cntr)(incf bear-cntr) 'B)
	                    ((U2 -U2 -D1 U1)(incf bull-cntr) 'L)
	                    ((-U3 -D2 D1)(incf bear-cntr) 'S)
	                    ((-U1 -D3)(decf bull-cntr)(decf bear-cntr) 'X)
	                    (otherwise 'F))	 
                    (case (pivot-turn tdate 'month);;feature 13
	                  (PP  (incf bull-cntr)(incf bear-cntr) 'B)
                          ((R2 H R1 S3 S0 R0) (incf bull-cntr)  'L)
	                  ((S1 S2) (incf bear-cntr) 'S)
	                  (L (decf bull-cntr)(decf bear-cntr)'X)
	                  (otherwise 'F))	      
		      (case (gann-slope-index tdate 5)   ;;;feature 12
	                    (0 (incf bull-cntr)(incf bear-cntr)'B)
	                    ((-2 -4 7 4) (incf bull-cntr) 'L)
	                    ((-1 3) (decf bull-cntr)(decf bear-cntr) 'X)
	                    (otherwise 'F))
                     (case (wpp tdate)
	                (IUH (incf bull-cntr)(incf bear-cntr) 'B)
	                ((IDH OUL DDD UDD ODH) (incf bull-cntr) 'L)
	                ((IUF IDL)(incf bear-cntr) 'S)
	                ((ODL UDU OUH) (decf bull-cntr)(decf bear-cntr) 'X)
		        (otherwise 'F))		     
                     (case (ep-roc-change-index tdate 13 1)  ;;;feature 14
	                   ((-3 4)(incf bull-cntr)(incf bear-cntr) 'B)
                           ((3 -4)(incf bull-cntr) 'L)
	                   ((2 -2)(incf bear-cntr) 'S)
	                   (otherwise 'F))	             	   
					   
		    bull-cntr ;(if (<= (cci-index1 tdate 5) -2) bull-cntr 0)
		    bear-cntr ;(if (>= (cci-index1 tdate 5) 2) bear-cntr 0)
		      (vsignals1 tdate )
                   )
        (if (zerop (mod counter 4)) (terpri str))))
   (values bull-cntr bear-cntr)
   
))


(defun straddle-report (tdate &optional (markets (append *straddle-list* *straddle-list1*)))
  (let ((path1 "~/exitpoints/daily-out/straddle.csv") vhprice1 vlprice1 ladder-list action
	risk vsig  ctr vprev vlprice vhprice (counter 0) cqg-symbol ts-symbol contract-month)    
    (cond ((equal markets *swing-list*)
           (setq path1 "~/exitpoints/daily-out/swing.csv"))
	  ((equal markets "micro-list*")
	   (setq path1 "~/exitpoints/daily-out/hedge.csv"))
	  ((equal markets *space-list*)
	   (setq path1 "~/exitpoints/daily-out/space.csv"))
	  ((equal markets *longevity-list*)
           (setq path1 "~/exitpoints/daily-out/longevity.csv"))
	  )
    (build-swingtrade-warehouse aa)
    (apply #'swing-trade-binsb T *swing-features*)
    
  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "~A~%" (date-convert tdate))
     (format str "    Market       SIGNAL       ACTION     Reverse      Risk     Risk$  CLOSE~%") 
     (dolist (ith markets)
       (incf counter)(set-market ith)
        (setq cqg-symbol (get-cqg-symbol ith) ts-symbol (get-ts-symbol ith)
              contract-month (ts-contract-month (contract-month ith tdate)))
       (setq action (find-best-edge-swing-trade1 tdate nil nil))
       (multiple-value-setq (vsig ctr vprev)(vsignals1 tdate ))
       (multiple-value-setq (vlprice vhprice risk)(vprices tdate *duration* *factor* 1 *type*))
       (setq vlprice1 (my-pretty-price (- vlprice risk))
	     vhprice1 (my-pretty-price (+ vhprice risk)))
       (if (member ith '(us.d1b ty.d1b)) (setq vprev (convert-to-32nds vprev)
						vhprice (convert-to-32nds vhprice)
						vlprice (convert-to-32nds vlprice)
						vhprice1 (convert-to-32nds vhprice1)
						vlprice1 (convert-to-32nds vlprice1)))

       (setq ladder-list (straddle-ladder tdate))
       (format str "~8@A~3A      ~5@A ~16@A  ~8@A   ~8@A  ~5@A  ~A~%~A~%~%"
	       (if (index-futurep ith) cqg-symbol ts-symbol) contract-month
	       vsig action
	       vprev
	
	       (if (member ith '(ty.d1b us.d1b))
		   (convert-to-32nds (my-pretty-price risk))(my-pretty-price risk))
	  
	    
	       (round (* (calculate-point-value tdate) risk))
	       (getd tdate 'close)
	       (if (member *data-name* '(us.d1b ty.d1b))
                   (mapcar #'(lambda (s) (convert-to-32nds s)) ladder-list)
	       ladder-list)
	       )
       (if (zerop (mod counter 4)) (terpri str))) 
     ))
  )


(defun AIM-report (tdate &optional (markets *aim-list*))
  (let ((path1 "~/exitpoints/daily-out/straddle.dat"); vhprice1 vlprice1
	ladder-list
	risk vsig  ctr vprev vlprice vhprice (counter 0) cqg-symbol ninja-symbol contract-month)    
    (cond ((equal markets *aim-list*)
           (setq path1 "~/exitpoints/daily-out/aim.csv"))
	   ((equal markets *micro-list*)
           (setq path1 "~/exitpoints/daily-out/micros.csv"))
	  ((equal markets *space-list*)
	   (setq path1 "~/exitpoints/daily-out/space.csv")))
    
  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
     
					;(format str "Market     SIGNAL    Reverse     Risk   Risk$~%")
     (format str "~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
	     'Date 'Symbol "Other names" 'SIGNAL 'FIRST 'SECOND 'THIRD 'FOURTH 'FIFTH 'SIXTH
	     'SEVENTH 'EIGHTH 'NINTH 'TENTH 'ELEVENTH)
     (dolist (ith markets)
       (incf counter)(set-market ith)
        (setq cqg-symbol (get-cqg-symbol ith) ninja-symbol (get-ninja-symbol ith)
              contract-month (ts-contract-month (contract-month ith tdate)))
	
       (multiple-value-setq (vsig ctr vprev)(vsignals1 tdate ))
       (multiple-value-setq (vlprice vhprice risk)(vprices tdate *duration* *factor* 1 *type*))
     ;  (setq vlprice1 (my-pretty-price (- vlprice risk))
;	     vhprice1 (my-pretty-price (+ vhprice risk)))
       ;(if (member ith '(us.d1b ty.d1b)) (setq 
       ;						vhprice (convert-to-32nds vhprice)
;						vlprice (convert-to-32nds vlprice)
;						vhprice1 (convert-to-32nds vhprice1)
;						vlprice1 (convert-to-32nds vlprice1)))

       (setq ladder-list (AIM-ladder tdate))
       (setq ladder-list (if (member *data-name* '(us.d1b ty.d1b))
			     (mapcar #'(lambda (s) (convert-to-32nds s)) ladder-list) ladder-list))
      
       (format str "~A,~A~A,~A~3A/~A,~A,~
                    ~A,~A,~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
	       (date-convert tdate)   
	       (if (and (< counter 5)(index-futurep ith)) cqg-symbol ninja-symbol) contract-month
	       (get-barchart-symbol ith) contract-month
	       ith
	       vsig	       
	      
	     ;  (if (member ith '(ty.d1b us.d1b)) (convert-to-32nds vprev)
	;	   (my-pretty-price vprev)
	    ;   (my-pretty-price vprev)   
	
	     ;  (if (member ith '(ty.d1b us.d1b))
	;	   (convert-to-32nds (my-pretty-price risk))
	     ; (my-pretty-price risk)
	    
	    ;   (round (* (calculate-point-value tdate) risk))
	        (first ladder-list) 
	        (second ladder-list)
	        (third ladder-list) 
	        (fourth ladder-list)
	        (fifth ladder-list) 
	        (sixth ladder-list) 
	        (seventh ladder-list)
	        (eighth ladder-list) 
	        (ninth ladder-list) 
	        (tenth ladder-list) 
	        (nth 10 ladder-list)
	       )
	       
      ; (if (zerop (mod counter 4)) (terpri str))) 
     )))
  )

#|
;;;from the simulation trades do a feature study.
;;;given a file of simulation trades; years are replaced by the indicator values.
(defun exit-statistics-by-year (path1 &optional (path2 nil))
 (let (records ;sexits (sexits-winners 0)(sexits-losers 0) 
      years  years-list num-wins num-losses ave-win ave-loss 
      (gains 0)(losses 0) (pl 0) pf draw ave-pl  (path (string-append *output-upper-dir* "exit-year-data.csv"))
       (tpl 0)(tgains 0)(tlosses 0)(tnum-wins 0)(tnum-losses 0)
      (tdraw 0)(tave-win 0) (tave-loss 0)(tave-pl 0)(tpf 0)
      trades record1);  (cexits-winners 0)(cexits-losers 0) cexits)
      (labels ((set-comm (rec)
                  (cond ((member  (get-exitpoints-symbol (nth 4 rec)) *dubai-list*)
                         (setq  *day-commission* 10 *pips-slippage* 0))
                        ((member (get-exitpoints-symbol (nth 4 rec)) *forex-warehouse-list*)
                         (setq *day-commission* 0 *pips-slippage* 6))
                        ((member (get-exitpoints-symbol (nth 4 rec)) *equity-warehouse-list*)
                         (setq *day-commission* 35 *pips-slippage* 0))
                        (t (setq *day-commission* 40 *pips-slippage* 0)))
            ))
   (maind-x)(set-cat-list)
   (if path2 (setq path path2))
   (when (probe-file path1)
     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (push record1 records));;closes the do
       ))
;;;first find the years in the records
    (dolist (record records)
           (pushnew (truncate (nth 0 record) 10000) years))

    (dolist (year years)
      (setq pl 0 gains 0 losses 0 trades nil num-wins 0 num-losses 0 num-wins 0 num-losses 0
            ave-win 0 ave-loss 0 draw 0)
      
      (dolist (record records)
        (set-market (get-exitpoints-symbol (nth 4 record)))
         (set-comm record)   
         (when (eql year (truncate (nth 0 record) 10000))
               (setq pl (+ pl (nth 9 record) (- (comm+slip (nth 3 record)))) trades (cons record trades))
             (if (plusp (nth 9 record)) (setq gains (+ gains (nth 9 record)(- (comm+slip (nth 3 record))))
                                              num-wins (1+ num-wins))
                (setq losses (+ losses (nth 9 record)(- (comm+slip (nth 3 record))))
                              num-losses (1+ num-losses))

                  ))
    
        );;;closes dolist over records
;;;order is important for drawdowns
     (setq trades (vsort trades #'< #'car))
     (setq trades (mapcar #'(lambda (s) (progn (set-market (get-exitpoints-symbol (nth 4 s)))
                                               (set-comm s)
                                               (- (nth 9 s) (comm+slip (nth 3 s))))) trades))
     (setq draw (round (drawdown trades))
          ave-pl (round (/ pl (if (zerop (length trades))  1 (length trades)))) 
          pf (my-round (abs (/ gains (if (zerop losses) 1 losses))) 2)
          ave-win (round (/ gains (if (zerop num-wins) 1 num-wins)))
          ave-loss (round (/ losses (if (zerop num-losses) 1 num-losses))))
  
    (setq years-list
           (cons (list year (length trades) pl ave-pl pf draw gains num-wins ave-win losses num-losses ave-loss) years-list))
  );;;closes the dolist over years

       (dolist (record records)
          (set-market (get-exitpoints-symbol (nth 4 record))) (set-comm record)
          (setq tpl (+ tpl (nth 9 record) (- (comm+slip (nth 3 record)))))
          (if (plusp (nth 9 record)) (setq tgains (+ tgains (nth 9 record)(- (comm+slip (nth 3 record))))
                                           tnum-wins (1+ tnum-wins))
                (setq tlosses (+ tlosses (nth 9 record)(- (comm+slip (nth 3 record))))
                              tnum-losses (1+ tnum-losses)))
      )

    (setq records (vsort records #'< #'car))
    (setq records (mapcar #'(lambda (s) (progn (set-market (get-exitpoints-symbol (nth 4 s)))
                                              (set-comm s)(- (nth 9 s) (comm+slip (nth 3 s))))) records))
    (setq tdraw  (drawdown records)
          tave-pl (round (/ tpl (if (zerop (length records))  1 (length records)))) 
          tpf (my-round (abs (/ tgains (if (zerop tlosses) 1 tlosses))) 2)
          tave-win (round (/ tgains (if (zerop tnum-wins) 1 tnum-wins)))
          tave-loss (round (/ tlosses (if (zerop tnum-losses) 1 tnum-losses))))
  
  ;;;;find rank based on ave-pl pf and drawdown

   (setq years-list  (vsort years-list #'> #'fourth))  ;;;#'fourth is the ave-pl
;;;;add the rank to the 0th field
    (dotimes (ith (length years-list))
       (setf (nth ith years-list)
          (cons ith (nth ith years-list))))
   
    (setq years-list (vsort years-list #'> #'sixth)) ;;;#'sixth is the profit factor
    (dotimes (ith (length years-list))
       (setf (car (nth ith years-list))
             (+ ith (car (nth ith years-list)))))
   
    (setq years-list (vsort years-list #'> #'seventh)) ;;;#'seventh is the drawdown
    (dotimes (ith (length years-list))
       (setf (car (nth ith years-list))
             (+ ith (car (nth ith years-list)))))
 
    (vsort years-list #'< #'second);;;where 'second is the year

    (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "Year  #trades   $P&L  $Ave    PF  Drawdown  Gains #wins Ave-Win  Losses #loss Ave-Loss ~%")
   
     (format str "~%All  ~6D  ~7,0F  ~4,0F  ~4,2F  ~6,0F  ~7,0F  ~3A   ~3,0F  ~8,0F  ~3A   ~3,0F~%"
        (length records) tpl tave-pl  tpf tdraw tgains tnum-wins tave-win tlosses tnum-losses tave-loss)
     (dolist (jth years-list)
     (format str"~%~6A~6D  ~7D ~4,0F  ~4,2F  ~6D ~7D   ~3A  ~3,0F  ~8,0F  ~3A  ~3,0F "
      (round (second jth))(third jth)(round  (fourth jth))(round (fifth jth)) (sixth jth)(round (seventh jth))
      (round (nth 7 jth))(nth 8 jth)(nth 9 jth)(nth 10 jth)(nth 11 jth)(nth 12 jth)
            )
      ))
   

)));;;


|#
(defun run-straddle-report (date)
  (straddle-report date *space-list*)
  (straddle-report date *swing-list*)
  (straddle-report date *longevity-list*)
  (straddle-report date (append *ff-list* *straddle-list*))
  )

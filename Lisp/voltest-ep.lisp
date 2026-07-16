;;; -*- Mode: LISP; Package: user; Base: 10. -*-


#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;these parameters restrict the type of pattern for entries. The first number represents the position of the open price in the daily bar.
;;;the second digit represents the position of the close price in the daily bar.
;;;for example 12 means the open price is in the lower one third ofthe daily bar and the close is in the middle one third of the daily bar.
;;;the 31 means the open price is in the top one third of the daily bar and the close price is in the bottom one third.
(defparameter *day-type-S* nil ) ;'(12 13))
(defparameter *day-type-L* nil ) ;'(32 31))


;;;enters and exits with stop orders
;;;does include stop loss but no objectives
(defun volatility-test (date2 num &optional (param .85) (dur 5) (output T))
 (let (date stop-long stop-short trades long short vlow vhigh
       ratio clean-up
       ave-win ave-loss losers winners extended-trades trade
        (path1 "/home/register/cycles/trend.dat"))
   (declare (special HT))
   (unless (boundp 'HT) (setf HT (make-hash-table) clean-up T))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)

   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility date dur 1)(volatility date (* 5 dur) 1)))
            (setf (gethash date HT) ratio)))

   ;(multiple-value-setq (vlow vhigh) (vprices date dur (* ratio param)))
   (multiple-value-setq (vlow vhigh) (vprices date dur param))

   (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long vlow)))
   (when short (setq stop-short (min stop-short vhigh)))


   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade (append trade (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade extended-trades)
          (setq trade nil long nil stop-long nil))
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)
          (setq trade nil short nil stop-short nil))


;;;check if new entry
   (when (and (not short) (> (getd date 'open) vlow)
              (<= (getd date 'low) vlow)

                        )
          (setq short (min (getd date 'open) vlow)
                 trade (list date 'short short)
                 stop-short (min vhigh (getd (getd date 'ydate) 'high)))
         )
    (when (and (not long) (< (getd date 'open) vhigh)
               (>= (getd date 'high) vhigh)

               )
           (setq long (max (getd date 'open) vhigh)
                 trade (list date 'long long)
                 stop-long (max vlow (getd (getd date 'ydate) 'low)))
              )
 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) vlow))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil))
   (when  (and short (or (< (getd date 'open)(getd date 'close))
                            (>= (getd date 'high) vhigh))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))


   );;;closes the dotimes

  (when clean-up (setq HT nil) (makunbound 'HT))
 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format output "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
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
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ));;closes the dolist and with-open-file
   ); closes the when
   (list-sum trades)

   ));;;closes the let and the defun

;;;enters and exits with stop orders and stop loss orders
;;;does include objectives
(defun volatility-test2 (date2 num &optional (param1 .85) (param2 .01)(dur 5) (output T))
 (let (date stop-long stop-short trades long short vlow vhigh ave4
       cover-long cover-short ratio ave-win ave-loss losers winners extended-trades trade
        (path1 "/home/register/cycles/trend.dat"))
     (declare (special HT))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)


   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility date dur 1)(volatility date (* 5 dur) 1)))
            (setf (gethash date HT) ratio)))

   ;(multiple-value-setq (vlow vhigh) (vprices date dur (* ratio param1)))
   (multiple-value-setq (vlow vhigh) (vprices date dur param1))

   (setq ave4 (ave date (1- dur) 'pivot)
         cover-short (* (- 1 param2) ave4)
         cover-long (* (+ 1 param2) ave4))


   (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long vlow)))
   (when short (setq stop-short (min stop-short vhigh)))


   (when (and long (<= (getd date 'low) stop-long))
          (push (- (min stop-long (getd date 'open)) long) trades)
          (setq trade (append trade (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
          (push trade extended-trades)
          (setq trade nil long nil stop-long nil))
   (when (and short (>= (getd date 'high) stop-short))
          (push (- short (max stop-short (getd date 'open))) trades)
          (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
          (push trade extended-trades)
          (setq trade nil short nil stop-short nil))

  ;;;check if met objective
     (when (and long (> (getd date 'high) cover-long))

            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))

      (when (and short (< (getd date 'low) cover-short))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))

;;;check if new entry
   (when (and (not short)
              (< cover-short vlow)
              (>= (getd date 'open) vlow)
              (<= (getd date 'low) vlow)

                        )
          (setq short (min (getd date 'open) vlow)
                 trade (list date 'short short)
                 stop-short (min vhigh (getd (getd date 'ydate) 'high)))
         )
 ;   (when (and (not short)
 ;             (< cover-short vlow)
 ;             (< (getd date 'open) vlow)
 ;             (> (getd date 'high) vlow)
 ;
 ;                       )
 ;         (setq short vlow
 ;               trade (list date 'short short)
 ;               stop-short (min vhigh (getd (getd date 'ydate) 'high)))
 ;        )
    (when (and (not long)
               (> cover-long vhigh)
               (<= (getd date 'open) vhigh)
               (>= (getd date 'high) vhigh)

               )
           (setq long (max (getd date 'open) vhigh)
                 trade (list date 'long long)
                 stop-long (max vlow (getd (getd date 'ydate) 'low)))
              )
 ;   (when (and (not long)
 ;              (> cover-long vhigh)
 ;              (> (getd date 'open) vhigh)
 ;              (< (getd date 'low) vhigh)
 ;
 ;              )
 ;          (setq long vhigh
 ;                trade (list date 'long long)
 ;                stop-long (max vlow (getd (getd date 'ydate) 'low)))
 ;             )
 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) vlow))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil))
   (when   (and short (or (< (getd date 'open)(getd date 'close))
                            (>= (getd date 'high) vhigh))
               (>= (getd date 'high) stop-short))

           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))
 ;;;check if met objective on day of entry

     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil)))


   );;;closes the dotimes


 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )

     (format output "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
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
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ));;closes the dolist and with-open-file
   ); closes the when
   (list-sum trades)

   ));;;closes the let and the defun



;;;enters and exits on the open of the next day.
;;;;does not use stops
(defun macd-test (date2 num &optional (param1 5)(param2 34)(param3 5))
 (let (date trades long short cover-long cover-short (outfile T)
       signal losers winners extended-trades trade  ;stop-long stop-short
       ave-win ave-loss (path1 "/home/register/cycles/trend.dat"))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)

    (setq signal (macd date param1 param2 param3))

    (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

;;;check if met objective or exit criteria
     (when (and long (minusp signal)
                                 )
            (setq cover-long (getd date 'open))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil ))

      (when (and short (plusp signal)
                           )
            (setq cover-short (getd date 'open))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil ))



;;;check if new entry
   (cond ((and (not short)
               (minusp signal)

               )
          (setq short (getd date 'open)
                trade (list date 'short short)
              ;  stop  nil
                 ))


         ((and (not long)
               (plusp signal)
                                         )
           (setq long (getd date 'open)
                 trade (list date 'long long)
               ;  stop nil
                 ))

              )

   );;;closes the dotimes

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
          ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
         )


    (format outfile "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
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
       )




   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))

   (macd date param1 param2 param3)
   ));

 ;;;enters and exits on the open of the next day.
;;;;does not use stops
(defun macd-test1 (date2 num &optional (param1 12)(param2 26)(param3 9))
 (let (date trades long short cover-long cover-short  macd0 macd1
       signal losers winners extended-trades trade
       ave-win ave-loss (path1 "/home/register/cycles/trend.dat"))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)

    (multiple-value-setq(macd0 macd1) (macd date param1 param2 param3))
    (setq signal (- macd0 macd1))

    (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))



;;check if met objective or exit criteria
     (when (and long (minusp signal)
                                 )
            (setq cover-long (getd date 'open))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil ))

      (when (and short (plusp signal)
                           )
            (setq cover-short (getd date 'open))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil ))



;;;check if new entry
   (cond ((and (not short)
               (minusp signal)

               )
          (setq short (getd date 'open)
                trade (list date 'short short)
              ;  stop  nil
                 ))


         ((and (not long)
               (plusp signal)
                                         )
           (setq long (getd date 'open)
                 trade (list date 'long long)
               ;  stop nil
                 ))

              )


   );;;closes the dotimes

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
          ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
         )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F"

       (round (* (list-sum trades) (index-point-value))) (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))


   ));



;;;enters and exits on the open of the next day.
;;;;does use stops

(defun tbps-test (date2 num)
 (let (date trades long short cover-long cover-short stop-long stop-short
       losers winners extended-trades trade  winner-pers loser-pers trade-long
       ave-win ave-loss winner-durations loser-durations signal target stop
       date-1 (path1 "/home/register/cycles/trend.dat"))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))

 ;;;;from date1 to date2
 (dotimes (ith num)
     (multiple-value-setq (signal target stop)(tbps-signal date 2))

    (if (eql signal 'UP) (setq cover-long target stop-long stop ))
    (if (eql signal 'DN) (setq cover-short target stop-short stop ))

    (setq date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long)
          (setq long (+ long (getd date 'rollover)))(setf (third trade-long) long))
    (when (and (getd date 'rollover) short)
               (setq short (+ short (getd date 'rollover)))(setf (third trade) short))




;;;check if met exit criteria
     (when (and long (neql signal 'UP)
                               )
            (setq cover-long (getd date-1 'close))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil cover-long nil))

      (when (and short (neql signal 'DN)
                            )
            (setq cover-short (getd date-1 'close))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil cover-short nil))


;;;check if new entry
   (cond ((and (not short)
               (eql signal 'DN)
               )
          (setq short
                (getd date-1 'close)
                trade (list date 'short short)
                                 ))

         ((and (not long)
               (eql signal 'UP)
                       )
           (setq long (getd date-1 'close)
                 trade-long (list date 'long long)
                                 ))
              )
;;;check if stopped out on same day of entry


   (when (and long (or (< (getd date 'high) cover-long)
                       (< (getd date 'open)(getd date 'close)))
                   (<= (getd date 'low) stop-long))
           (push (- (min stop-long (getd date 'open)) long) trades)
           (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil cover-long nil))
   (when (and short (or (> (getd date 'low) cover-short)
                        (> (getd date 'open)(getd date 'close)))
              (>= (getd date 'high) stop-short))
           (push (- short (max stop-short (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil cover-short nil))

;;;;check if met objective
      (when (and long (> (getd date 'high) cover-long)
                               )
            (setq cover-long (max cover-long (getd date 'open)))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil cover-long nil))

      (when (and short (< (getd date 'low) cover-short)
                            )
            (setq cover-short (min cover-short (getd date 'open)))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil cover-short nil))






   );;;closes the dotimes


    (dolist (ith extended-trades)
      (cond ((plusp (nth 6 ith))
             (push (subtract-dates (nth 0 ith)(nth 3 ith)) winner-durations)
             (push (* 100 (/ (abs (- (nth 5 ith) (nth 2 ith))) (nth 2 ith))) winner-pers))


            (t (push (subtract-dates (nth 0 ith)(nth 3 ith)) loser-durations)
               (push (* 100 (/ (abs (- (nth 5 ith)(nth 2 ith))) (nth 2 ith))) loser-pers))

             ))

;;;apply commission of $60 oer round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 60 (index-point-value)))) trades))


   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
          ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
         )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F"

       (round (* (list-sum trades) (index-point-value))) (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       )


   (format T "~%AVE-WIN = ~,2,,F%  AVE-WIN DUR= ~,1,,F DAYS  75% DUR= ~,1,,F DAYS MAX DUR= ~,1,,F DAYS ~%~
             AVE-LOSS = ~,2,,F%  AVE-LOSS DUR= ~,1,,F DAYS 75% DUR= ~,1,,F DAYS  MAX DUR= ~,1,,F DAYS"
        (/ (list-sum winner-pers)(length winner-pers))
        (/ (list-sum winner-durations)(length winner-durations))
        (percentile .75 winner-durations) (max* winner-durations)
        (/ (list-sum loser-pers)(length loser-pers))
        (/ (list-sum loser-durations)(length loser-durations))
        (percentile .75 loser-durations)(max* loser-durations)
        )



   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))


   ));

;;;enters and exits with stop orders
(defun weekly-volatility-test (date2 num)
 (let (date stop trades long short vlow vhigh
       ;ave4 deltat deltaa dev cover-long cover-short (param 0)
       ave-win ave-loss losers winners extended-trades trade
        (path1 "/home/register/cycles/trend.dat"))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)

   (multiple-value-setq (vlow vhigh) (weekly-vsignal-prices date 5 .85))

   (setq date (add-mkt-days date 1))

    (cond ((and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
          ((and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover)))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (cond (long (setq stop (max stop vlow)))
         (short (setq stop (min stop vhigh)))
          )

   (cond ((and long (<= (getd date 'low) stop))
           (push (- (min stop (getd date 'open)) long) trades)
           (setq trade (append trade (list date 'exit (min stop (getd date 'open)) (- (min stop (getd date 'open)) long))))
          (push trade extended-trades)
           (setq trade nil long nil stop nil))
         ((and short (>= (getd date 'high) stop))
           (push (- short (max stop (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop (getd date 'open)) (- short (max stop (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop nil)))

;;;;check if exit signal
;    (cond ((and long (minusp signal)(minusp slope))
;           (push (- (getd date 'open) long) trades)
;           (setq trade (append trade (list date 'exit (getd date 'open) (- (getd date 'open) long))))
;           (push trade extended-trades)
;           (setq trade nil long nil stop nil))
;         ((and short (plusp signal)(plusp slope))
;           (push (- short (getd date 'open)) trades)
;           (setq trade (append trade (list date 'exit (getd date 'open) (- short (getd date 'open)))))
;           (push trade extended-trades)
;           (setq trade nil short nil stop nil)))


;;;check if new entry
   (cond ((and (not short)
               (<= (getd date 'low) vlow)
               )
          (setq short (min (getd date 'open) vlow)
                 trade (list date 'short short)
                 stop (min (n-day-high date 5) vhigh)))

         ((and (not long)
               (>= (getd date 'high) vhigh)

               )
           (setq long (max (getd date 'open) vhigh)
                 trade (list date 'long long)
                 stop (max (n-day-low date 5) vlow)))
              )
 ;;;check if stopped out on same day of entry
   (cond ((and long (> (getd date 'open)(getd date 'close))
                   (<= (getd date 'low) stop))
           (push (- stop long) trades)
           (setq trade (append trade (list date 'exit stop (- stop long))))
           (push trade extended-trades)
           (setq trade nil long nil stop nil))
         ((and short (< (getd date 'open)(getd date 'close))
                     (>= (getd date 'high) stop))
           (push (- short stop) trades)
           (setq trade (append trade (list date 'exit stop (- short stop))))
           (push trade extended-trades)
           (setq trade nil short nil stop nil)))

;;;check if met objective
#|
     (cond ((and long (> (getd date 'high) cover-long))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop nil))
           ((and short (< (getd date 'low) cover-short))
            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop nil)))
|#


   );;;closes the dotimes



   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F"
       (round (* (list-sum trades) (index-point-value))) (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))


   ));;;closes the let and the defun



;;;enters and exits with stop orders with no stop loss order always in the market
(defun structure-test (date2 num &optional (size 3) (outfile T))
 (let (date stop trades long short trend rprice  date-1

       ave-win ave-loss losers winners extended-trades trade
        (path1 "/home/register/cycles/trend.dat"))


   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)

  ; (setq slope (swing-slope date size))
   (multiple-value-setq (trend rprice) (structure-trend date size nil))

   (setq date-1 date date (add-mkt-days date 1))

    (cond ((and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
          ((and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover)))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (if long (setq stop (- rprice (index-tick-size))))
   (if short (setq stop (+ rprice (index-tick-size))))


   (cond ((and long  (<= (getd date 'low) stop))
           (push (- (min stop (getd date 'open)) long) trades)
           (setq trade (append trade (list date 'exit (min stop (getd date 'open)) (- (min stop (getd date 'open)) long))))
           (push trade extended-trades)
           (setq trade nil long nil stop nil))
         ((and short (>= (getd date 'high) stop))
           (push (- short (max stop (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop (getd date 'open)) (- short (max stop (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop nil)))


;;;check if new entry
   (cond ((and (not short)
               (eql trend 'UP)
               (< (getd date 'low) rprice)
               ;(eql slope 'DN)
               )
          (setq short (min (getd date 'open) (- rprice (index-tick-size)))
                 trade (list date 'short short)
                 stop (+ (n-day-high date-1 (truncate size 2)) (index-tick-size))))

         ;((and (not short)
         ;      (eql trend 'DN)
         ;      (< (getd date 'low) (n-day-low date-1 (truncate size 2)))
         ;      (eql slope 'DN)
         ;      )
         ; (setq short (min (getd date 'open) (- (n-day-low date-1 (truncate size 2)) (index-tick-size)))
          ;       trade (list date 'short short)
          ;       stop (+ (n-day-high  date-1 (truncate size 2)) (index-tick-size))))

         ((and (not long)
               (eql trend 'DN)
               (> (getd date 'high) rprice)
               ;(eql slope 'UP)
               )
           (setq long (max (getd date 'open) (+ rprice (index-tick-size)))
                 trade (list date 'long long)
                 stop (- (n-day-low date-1 (truncate size 2))(index-tick-size))))

         ;((and (not long)
         ;      (eql trend 'UP)
         ;      (> (getd date 'high) (n-day-high date-1 (truncate size 2)))
         ;      (eql slope 'UP)
         ;      )
         ;  (setq long (max (getd date 'open) (+ (n-day-high date-1 (truncate size 2)) (index-tick-size)))
         ;        trade (list date 'long long)
         ;        stop (- (n-day-low (getd date 'ydate) (truncate size 2))(index-tick-size))))
              )
 ;;;check if stopped out on same day of entry
   (cond ((and long stop (> (getd date 'open)(getd date 'close))
                   (<= (getd date 'low) stop))
           (push (- stop long) trades)
           (setq trade (append trade (list date 'exit stop (- stop long))))
           (push trade extended-trades)
           (setq trade nil long nil stop nil))
         ((and short stop (< (getd date 'open)(getd date 'close))
                     (>= (getd date 'high) stop))
           (push (- short stop) trades)
           (setq trade (append trade (list date 'exit stop (- short stop))))
           (push trade extended-trades)
           (setq trade nil short nil stop nil)))


   );;;closes the dotimes

   ;;;apply commission of $50 oer round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 50 (index-point-value)))) trades))


   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )


    (format outfile "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
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
       )



   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
    (structure-trend date2 size outfile)
    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades))
   ));;;closes the let and the defun



;;;DAY trade test enter on open of next day and sell on close
;;;this does not use objectives
(defun day-trade-test (date2 num &optional (param .0105) (param3 1.0)(dur 4) (outfile T))
 (let (date trades long short cover-long cover-short long-entry short-entry clean-up trade-long
       ratio ave-win ave-loss losers winners extended-trades trade stop-long stop-short
       (path1 "/home/register/cycles/trend.dat"))
   (declare (special HT PT))
   (unless (boundp 'HT) (setf HT (make-hash-table) clean-up T))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility-log date dur 1)(volatility-log date (* 5 dur) 1)))
            (setf (gethash date HT) ratio)))

     (cond ((gethash date PT))
           (t (setf (gethash date PT) (expt (/ (getd date 'close) (ave date 500 'close)) .500))))

   (multiple-value-setq (short-entry long-entry)(vprices date dur (* ratio param)))
   ; (multiple-value-setq (short-entry long-entry)(vprices date dur param))

  ; (setq day-type (day-bar-type date))

    (setq date (add-mkt-days date 1))


;;;check if new entry

   (when (and (not short)
              (< (getd date 'open) long-entry)
              (> (getd date 'open) short-entry)
              (<= (getd date 'low) short-entry)
              (<= ratio *ratio*)

               )
          (setq short short-entry
                trade (list date 'short short)
                stop-short ;(getd (getd date 'ydate) 'close)
                (+ short-entry (* param3 (- (getd (getd date 'ydate) 'close) short-entry)))
                 ))
    (when (and (not short)
              ;(< (getd date 'open) long-entry)
              (<= (getd date 'open) short-entry)
              ;(<= (getd date 'low) short-entry)
              (<= ratio *ratio*)

               )
          (setq short (getd date 'open)
                trade (list date 'short short)
                stop-short ;(- (getd (getd date 'ydate) 'close)
                            ;  (- short-entry (getd date 'open)))
                           (+ (getd date 'open) (* param3 (- (getd (getd date 'ydate) 'close) short-entry)))
                 ))
    (when (and (not long)
               (> (getd date 'open) short-entry)
               (< (getd date 'open) long-entry)
               (>= (getd date 'high) long-entry)
               (<= ratio *ratio*)

               )

           (setq long long-entry
                 trade-long (list date 'long long)
                 stop-long ;(getd (getd date 'ydate) 'close)
                 (- long-entry (* param3 (- long-entry (getd (getd date 'ydate) 'close))))
                 ))

    (when (and (not long)
               ;(> (getd date 'open) short-entry)
               (>= (getd date 'open) long-entry)
              ; (>= (getd date 'high) long-entry)
               (<= ratio *ratio*)

             )

           (setq long (getd date 'open)
                 trade (list date 'long long)
                 stop-long ;(+ (getd (getd date 'ydate) 'close)
                            ;  (- (getd date 'open) long-entry))
                 (- (getd date 'open) (* param3 (- long-entry (getd (getd date 'ydate) 'close))))
                 ))

 ;;;check if stopped out on same day

   (when (and long stop-long (or (> (getd date 'open)(getd date 'close))
                                 (<= (getd date 'low) short-entry))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))
     (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))
    (when (and short stop-short (or (< (getd date 'open)(getd date 'close))
                                    (>= (getd date 'high) long-entry))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           ))
     (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil
           ))


;;;check if met objective or exit criteria on day of entry
     (when long
            (setq cover-long (getd date 'close))
            (push (- cover-long long) trades)
            (setq trade-long (append trade-long (list date 'exit cover-long (- cover-long long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))

      (when short
           (setq cover-short (getd date 'close))
           (push (- short cover-short) trades)
           (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))


   );;;closes the dotimes
  (when clean-up (setq HT nil) (makunbound 'HT))
  ;;;apply commision of $50 per round turn
   (setq trades (mapcar #'(lambda (s) (- s (/ 50 (index-point-value)))) trades))

  (when outfile

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (or (index-point-value) 1)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (or (index-point-value) 1))
          )

   (format outfile "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
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
       )


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
  );;;closes the when outfile
  ;   (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
     ; (/ (length (remove-if #'minusp trades))(length trades))
  (list-sum trades)
   ));



(defun find-best-volatility (tdate num &optional (dur 4) (output T))
  (let ((best-param 0) (result 0) (prev-result -20000) vlow vhigh (HT (make-hash-table)))
   (declare (special HT))
  (setf (gethash tdate HT) (/ (volatility tdate dur 1)(volatility tdate (* 5 dur) 1)))
  (do ((param .01 (+ param .01)))
      ((> param 2) best-param)
      ;(format T "~A~%" param)
      (setq result
          (volatility-test tdate num param dur nil))

      (if (> result prev-result) (setq prev-result result best-param param)))
  (when output
    (format output "~7,2,,,F" best-param)
    (format output "    Current Signal= ~A~%" (vsignals tdate dur best-param))
    (volatility-test tdate num best-param dur output)
    (multiple-value-setq (vlow vhigh)(vprices tdate dur best-param))

      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "~%SELL= ~7@A BUY= ~7@A~%" (convert-to-32nds vlow)(convert-to-32nds vhigh))
         (format output "~%SELL= ~7,4,,,F BUY= ~7,4,,,F~%" vlow vhigh))
      (if (member *data-name* '(US.D1B TY.D1B))
          (format output "today's LOW= ~7@A today's HIGH= ~7@A" (convert-to-32nds (getd tdate 'low))(convert-to-32nds (getd tdate 'high)))
         (format output "today's LOW= ~7,4,,,F today's HIGH= ~7,4,,,F~%" (getd tdate 'low) (getd tdate 'high))))

    (* (gethash tdate HT) best-param)
      ))

;;;;with objectives
(defun find-best-volatility2 (tdate num &optional (dur 4) (output T))
  (let ((best-param1 0) (best-param2 0)(result 0) (prev-result -20000) vlow vhigh
        cover-long cover-short (HT (make-hash-table)))
   (declare (special HT))
  (setf (gethash tdate HT) (/ (volatility tdate dur 1)(volatility tdate (* 5 dur) 1)))
  (do ((param1 .01 (+ param1 .01)))
      ((> param1 2) best-param1)

      (setq result
          (volatility-test tdate num param1 dur nil))

      (if (> result prev-result) (setq prev-result result best-param1 param1)))

    (setq prev-result -20000)
    (do ((param2 .001 (+ param2 .001)))
      ((> param2 .10) best-param2)
      (setq result
          (volatility-test2 tdate num best-param1 param2 dur nil))

      (if (> result prev-result) (setq prev-result result best-param2 param2)))

;     (setq prev-result -10000)
;    (do ((param1 (/ best-param1 2) (+ param1 .01)))
;      ((> param1 2) best-param1)
;
;      (setq result
;          (volatility-test2 tdate num param1 best-param2 dur nil))
 ;
;      (if (> result prev-result) (setq prev-result result best-param1 param1)))

  (when output
    (format output "~7,2,,,F" best-param1)
    (format output "    Current Signal= ~A   ~A~%" (vsignals tdate dur best-param1) best-param2)
    (volatility-test2 tdate num best-param1 best-param2 dur output)
    ;(multiple-value-setq (vlow vhigh)(vprices tdate dur (* (gethash tdate HT) best-param1)))
    (multiple-value-setq (vlow vhigh)(vprices tdate dur best-param1))

    (setq cover-long (* (+ 1 best-param2) (ave tdate dur 'pivot))
          cover-short (* (- 1 best-param2)(ave tdate dur 'pivot)))

    (if (member *data-name* '(US.D1B TY.D1B))
        (format output "~%SELL= ~7@A COVER-SHORT= ~7@A ~%BUY= ~7@A   COVER-LONG= ~7@A~%"
          (convert-to-32nds vlow)(convert-to-32nds cover-short)(convert-to-32nds vhigh) (convert-to-32nds cover-long))
         (format output "~%SELL= ~7,4,,,F COVER=SHORT= ~7,4,,,F ~%BUY= ~7,4,,,F  COVER-LONG= ~7,4,,,F~%"
            vlow cover-short vhigh cover-long))
    (if (member *data-name* '(US.D1B TY.D1B))
        (format output "today's LOW= ~7@A today's HIGH= ~7@A" (convert-to-32nds (getd tdate 'low))(convert-to-32nds (getd tdate 'high)))
         (format output "today's LOW= ~7,4,,,F today's HIGH= ~7,4,,,F~%" (getd tdate 'low) (getd tdate 'high))))


    (values best-param1 best-param2)
      ))

(defun rsi-grade (tdate period)
  (let ((date tdate) (min-rsi2 100) (max-rsi2 0) (min-rsi 100) (max-rsi 0))
 ;;;;first check if RSI is higher or lower than extreme in 2 times period.
  (declare (special RS))
    (dotimes (ith (* 2 period))
        (setq max-rsi2 (max (gethash (add-mkt-days date (- ith)) RS) max-rsi2)
              min-rsi2 (min (gethash (add-mkt-days date (- ith)) RS) min-rsi2)))
    (dotimes (ith period)
         (setq max-rsi (max (gethash (add-mkt-days date (- ith)) RS) max-rsi)
               min-rsi (min (gethash (add-mkt-days date (- ith)) RS) min-rsi)))

   (cond ((and (> (gethash tdate RS) min-rsi)
               (< (gethash tdate RS) max-rsi2)
               (< max-rsi max-rsi2)
               (>= (gethash tdate RS)(gethash (add-mkt-days tdate (- 1 period)) RS))) 'DN)
         ((and (< (gethash tdate RS) max-rsi)
               (> (gethash tdate RS) min-rsi2)
               (> min-rsi min-rsi2)
               (<= (gethash tdate RS)(gethash (add-mkt-days tdate (- 1 period)) RS))) 'UP))

 ))

;;;
(defun trend-change-test (tdate num param1 param2 param3)
 (let (date stop-long stop-short trades long short
       long-entry short-entry
       (MT (make-hash-table)) (SS (make-hash-table))
       (CL (make-hash-table))
        date-1
        (stop-ticks 1)(entry-ticks 1) ;cover-long cover-short
       ave-win ave-loss losers winners extended-trades trade-long trade
        (path1 "/home/register/cycles/trend.dat"))
   (declare (special  MT  SS CL ))

   (setq date (add-mkt-days tdate (- num)))
   (if  (probe-file path1)
        (delete-file path1))

; (fill-hash-macd tdate (+ num 5) 12 26 9)
; (fill-hash-macd-signal-line tdate (+ num 5) 3 10 20)
;  (fill-adx tdate (+ num 5) 14)
;;;fill the first five rsi before starting
; (dotimes (kth 10)
;  (setf (gethash (add-mkt-days date (- kth 10)) RS) (rsi (add-mkt-days date (- kth 10)) 9)))
 (dotimes (kth 50)
  (setf (gethash (add-mkt-days date (- kth 10)) CL) (commodity-channel-index (add-mkt-days date (- kth 50)) param2)))


 ;;;;from date1 to date2
 (dotimes (ith num)

  (cond ((gethash date SS))
          (t (setf (gethash date SS) (momentum1 date param1))))

  (cond ((gethash date CL))
        (t (setf (gethash date CL) (commodity-channel-index date param2))))



    (setq date-1 (getd date 'ydate))

    (setq short-entry (- (n-day-low date param3) (* entry-ticks (index-tick-size))))
    (setq long-entry (+ (n-day-high date param3) (* entry-ticks (index-tick-size))))


   (if long (setq stop-long (max stop-long
                                (- (n-day-low date param3)
                                   (* stop-ticks (index-tick-size)))
                                       );closes the max
                                       ))


   (if short (setq stop-short (min stop-short
                              (+
                                (n-day-high date param3)
                                  (* stop-ticks (index-tick-size)))

                                       )))

    ; (setq cover-long nil cover-short nil)


   (setq date-1 date date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long)
           (setq long (+ long (getd date 'rollover)))
           (setf (nth 2 trade-long) long))
     (when (and (getd date 'rollover) short)
           (setq short (+ short (getd date 'rollover)))
           (setf (nth 2 trade) short)
           )
;;;;check if exit at market on the open of next bar

   (when (and long (eql (gethash date-1 SS) 'DN))

           (push (-  (getd date 'open) long) trades)
           (setq trade-long (append trade-long (list date 'exit (getd date 'open) (- (getd date 'open) long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
   (when (and short (eql (gethash date-1 SS) 'UP))

           (push (- short (getd date 'open)) trades)
           (setq trade (append trade (list date 'exit (getd date 'open) (- short (getd date 'open)))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))

 ;;;;check if stopped out of prior position


   (when (and long (<= (getd date 'low) stop-long))
           (push (- (min stop-long (getd date 'open)) long) trades)
           (setq trade-long (append trade-long (list date 'exit (min stop-long (getd date 'open)) (- (min stop-long (getd date 'open)) long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil ))
   (when (and short (>= (getd date 'high) stop-short))
           (push (- short (max stop-short (getd date 'open))) trades)
           (setq trade (append trade (list date 'exit (max stop-short (getd date 'open)) (- short (max stop-short (getd date 'open))))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil ))


    (when (and (not short) short-entry

              (eql (gethash date-1 SS) 'DN)

              (<= (gethash date-1 CL) 0)
              (<= (getd date 'low) short-entry)


               )
          (setq short  (min (getd date 'open) short-entry)
                trade (list date 'short short)
                stop-short (+ (n-day-high date-1 param3)(* stop-ticks (index-tick-size)))
                ;cover-short nil
                ))


   (when (and (not long) long-entry
              (eql (gethash date-1 SS) 'UP)
              (>= (gethash date-1 CL) 0)

              (>= (getd date 'high) long-entry)

                       )
          (setq long  (max (getd date 'open) long-entry)
                 trade-long (list date 'long long)
                 stop-long (- (n-day-low date-1 param3) (* stop-ticks (index-tick-size)))
                 ;cover-long nil
                 ))


 ;;;check if stopped out on same day of entry
   (when (and long (> (getd date 'open)(getd date 'close))
                   (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
    (when (and short (< (getd date 'open)(getd date 'close))
                     (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))



   );;;closes the dotimes

   (setq trades (mapcar #'(lambda (s) (- s (/ 60 (index-point-value)) (* 0 (index-tick-size)))) trades))

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
       P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
       (round (* (list-sum trades) (index-point-value)))
       (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss))))

       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))


    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades))
   ));;;closes the let and the defun




;;;enters on a stop buys dips and trails a stop using daily volatility
;;;;use the best day-trade param as third argument
(defun trnd-test1 (date2 num &optional (param .85))
 (let (date stop-long stop-short trades long short cover-long cover-short
       vlow vhigh trend3 trend9 long-entry short-entry
       ave-win ave-loss losers winners extended-trades trade-long trade
        (path1 "/home/register/cycles/trend.dat"))
   (declare (ignore slope))
   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)
   (setq trend3 (my-trend date 3) trend9 (my-trend date 9))
  ; (setq ratio (/ (volatility date 4 1)(volatility date 20 1)))
  ;(multiple-value-setq (vlow vhigh)(vprices date 4 (* ratio param)))
  (multiple-value-setq (vlow vhigh)(vprices date 4 param))
  (setq long-entry vhigh short-entry vlow)

   (setq date (add-mkt-days date 1))

    (cond ((and (getd date 'rollover) long)
           (setq long (+ long (getd date 'rollover)))
           (setf (nth 2 trade-long) long))
          ((and (getd date 'rollover) short)
           (setq short (+ short (getd date 'rollover)))
           (setf (nth 2 trade) short)
           ))

 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long short-entry)))
   (when short (setq stop-short (min stop-short long-entry)))

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


   ;;;check if met objective or exit criteria
     (when (and long ;(or (member trend9 '(DOWN EDOWN1 EDOWN2))
                           (>= (getd (getd date 'ydate) 'high) (n-day-high date 5)))
                          ;       )
            (setq cover-long (getd date 'open))
            (push (- cover-long long) trades)
            (setq trade (append trade (list date 'exit cover-long (- cover-long long))))
            (push trade extended-trades)
            (setq trade nil long nil stop-long nil))

      (when (and short ;(or (member trend9 '(UP EUP1 EUP2))
                           (<= (getd (getd date 'ydate) 'low)(n-day-low (getd date 'ydate) 5)))
                        ;   )
            (setq cover-short (getd date 'open))

            (push (- short cover-short) trades)
            (setq trade (append trade (list date 'exit cover-short (- short cover-short))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))

;;;check if new entry
   (when (and (not short)
              (member trend3 '(UP EUP1 EUP2))
              (member trend9 '(DOWN EDOWN1 EDOWN2))
              (<= (getd date 'low) short-entry)
               )
          (setq short (min (getd date 'open) short-entry)
                trade (list date 'short short)
                stop-short long-entry))

    (when (and (not long)
              (member trend3 '(DOWN EDOWN1 EDOWN2))
              (member trend9 '(UP EUP1 EUP2))
              (>= (getd date 'high) long-entry)
                                          )
           (setq long (max (getd date 'open) long-entry)
                 trade-long (list date 'long long)
                 stop-long short-entry))

 ;;;check if stopped out on same day of entry
   (cond ((and long (> (getd date 'open)(getd date 'close))
                   (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))
         ((and short (< (getd date 'open)(getd date 'close))
                     (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil)))

   );;;closes the dotimes

   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (length winners))
         ave-loss  (* (/ (list-sum losers) (length losers)) (index-point-value))
          )
   (format T "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
       P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
       (round (* (list-sum trades) (index-point-value)))
       (length trades)
       (/ (- (length trades) (length losers)) (length trades))
       (abs (/ ave-win ave-loss))
       (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
       (round ave-win)
       (round ave-loss)
       (round (* (min* losers) (index-point-value)))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss))))

       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))


   ));;;closes the let and the defun






(defun find-best-day-trade (tdate num &optional (dur 4) (output T)(output1 T)(date1 nil))
  (let ((best-param1 *entry-factor*) (confirmations 0) ddate
         risk risk-short risk-long
        ; sector exit-time entry-time cancel-time oec-symbol
         (RC (make-hash-table))   exitpoint
        stop-short stop-long (VT (make-hash-table)) entry-long entry-short
        (best-param2 *objective-factor*) (best-param3 *stop-loss-day*)
        trades new-P&L new-plt equity-signal draw
          (HT (make-hash-table)) (RS (make-hash-table))
          (SRR (make-hash-table))(TS5 (make-hash-table))(TS15 (make-hash-table))
        (initial-reward-risk-ratio *reward-risk-ratio*)(TI (make-hash-table))
        cover-long cover-short ave4 action directive1 plt P&L (AT (make-hash-table)))


  (declare (special  HT   TI VT RC AT output-oec))


  (setf (gethash tdate RS)(rsi-ave-diff tdate 14 2))
  (setf (gethash (getd tdate 'ydate) RS)(rsi-ave-diff (getd tdate 'ydate) 14 2))
  (setf (gethash tdate SRR)(or (standard-reward-risk1 tdate) 'FT))
  (setf (gethash (getd tdate 'ydate) SRR)(or (standard-reward-risk1 (getd tdate 'ydate)) 'FT))
  (setf (gethash (getd (getd tdate 'ydate) 'ydate) SRR)
        (or (standard-reward-risk1 (getd (getd tdate 'ydate) 'ydate)) 'FT))

  (setf (gethash tdate TS5)(trend-signal tdate 5))
  (setf (gethash tdate TS15)(trend-signal tdate 15))



  (setf (gethash tdate HT) (/ (volatility-log tdate dur 1)(volatility-log tdate (* 7 dur) 1)))
  (setf (gethash tdate VT)(volatility-log tdate 60 1))




    (cond ((gethash tdate RC))
          ((setf (gethash tdate RC) (or (trend-signal tdate 3) 'FT))))


  (setq ddate tdate)
  (dotimes (ith 3)
     (cond ((gethash ddate TI))
           (t (setf (gethash ddate TI)(timing-index ddate))))
     (setq ddate (getd ddate 'ydate)))


;;; first iteration to revise best-param1 the entry parameter

;   (setq prev-result -110000 )
;
;   (do ((param  .50  (+ param .05)))
;      ((> param .90) best-param1)
;       (setq result
;          (day-trade-test1 tdate num param best-param2 best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))


;;;first iteration to revise parameter 3 the stop loss

;  (setq prev-result -50000)
;  (do ((param  (* 1.0 best-param1)  (+ param .10)))
;      ((> param (* 1.5 best-param1)) best-param3)
;      (setq result
;          (day-trade-test1 tdate num best-param1 best-param2 param dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param3 param)))

 ;;first iteration to revise best-param2 the objective

;  (setq prev-result -50000 )
;
;  (do ((param (* 2 best-param1) (+ param .2)))
;      ((> param (* 4 best-param1)) best-param2)
;      (setq result
;          (day-trade-test1 tdate num best-param1 param best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param2 param)))


;;;;first iteration is over the dur parameter
;   (setq prev-result -50000 )
;   (dolist (param '(2 3 4 5 7))
;       (setq result
;          (day-trade-test1 tdate num best-param1 best-param2  param nil))
;       (if (> result prev-result) (setq prev-result result dur param)))

;;;first iteration on the volatility ratio
;   (setq prev-result -50000 )
;   (do ((param  .75  (+ param .05)))
;      ((> param 1.05) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test1 tdate num param best-param2 best-param3 dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)


;;;second iteration to revise best-param1

;   (setq prev-result -50000 )
;   (setq dmy-param1 best-param1)
;   (do ((param (- dmy-param1 .10) (+ param .02)))
;       ((> param (+ dmy-param1 .10))  best-param1)
;       (setq result
;          (day-trade-test1 tdate num param best-param2 best-param3 dur nil))
;       (if (> result prev-result) (setq prev-result result best-param1 param)))

;;;second iteration on the volatility ratio
; (setq prev-result -50000 )
;   (do ((param  .70  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test1 tdate num param best-param2 best-param3 dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)

;;;first iteration to revise *reward-risk-ratio*

;  (setq prev-result -110000)
;  (do ((param .60 (+ param .1)))
;      ((> param 1.8) *reward-risk-ratio*)
;      (setq *reward-risk-ratio* param)
;      (setq result
;          (day-trade-test1 tdate num best-param1 best-param2 best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;  (setq *reward-risk-ratio* best-ratio)

;;;second iteration is over the dur parameter
;   (setq prev-result -50000 )
;   (dolist (param '(2 3 4 5 7))
;     (setq result
;          (day-trade-test1 tdate num best-param1 best-param2 best-param3 param nil))
;     (if (> result prev-result) (setq prev-result result dur param)))



;;; third iteration on the volatility ratio
;   (setq prev-result -50000 )
;   (do ((param  .90  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test1 tdate num param best-param2 best-param3 dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)



    (setf (gethash tdate AT) (ave tdate  dur 'pivot))


  (when output  (format output "~5,2,,,F  ~5,2,,,F ~5,2,,,F ~A ~4,2,,,F ~4,2,,F"
                        best-param1  best-param2 best-param3 dur *ratio* *reward-risk-ratio*)

      (setq ave4 (gethash tdate AT)
           cover-short (exp (- (log ave4) (* best-param2 (gethash tdate VT))))
           cover-long (exp (+ (log ave4) (* best-param2 (gethash tdate VT))))
           )


      (multiple-value-setq (entry-short entry-long)
       ;(vprices-log tdate dur (* (gethash tdate HT) best-param1))
       (vprices tdate dur best-param1)
       )


    ;  (setq risk (* (/ best-param3 best-param1) (abs (- entry-short (ave tdate 1 'pivot)))))

     (setq risk-short (min (*  best-param3 (abs (- entry-short (getd tdate 'close))))
                          ))


     (setq risk-long (min (*  best-param3 (abs (- entry-long (getd tdate 'close))))
                       ))

     (setq risk (max risk-long risk-short))

 (setq exitpoint (cond ((and (>= (round (rsi-ave-diff tdate 14 2)) 0)
                            ; (or (<= (round (slow-stocastic tdate (* 3 dur))) 20)
                            ;     (<= (round (slow-stocastic (getd tdate 'ydate) (* 3 dur))) 20))
                             (neql (gethash tdate SRR) 'OB)
                             (> (/ (- cover-long entry-long) risk) *min-reward-risk-ratio-position*)
                             (member (trend-signal tdate 5) '(UP CU))
                             (member (trend-signal tdate 15) '(UP CU)))
                             'exit-short)
                         ((and (> (gethash tdate RS)(gethash (getd tdate 'ydate) RS))
                             (or (eql (gethash tdate SRR) 'OS)(eql (gethash (getd tdate 'ydate) SRR) 'OS)
                                 (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OS)))
                             'exit-short)
                        ((and  (>= (round (rsi-ave-diff tdate (* 2 dur) 2)) 0)
                              (or (>= (gethash tdate TI) 4)(>= (gethash (getd tdate 'ydate) TI) 4)
                                  (>= (gethash (getd (getd tdate 'ydate) 'ydate) TI) 4))
                              (<= (round (slow-stocastic tdate (* 3 dur))) 80))
                              'exit-short)

                       ((and (<= (round (rsi-ave-diff tdate 14 2)) 0)
                            ; (or (>= (round (slow-stocastic tdate (* 3 dur))) 80)
                            ;     (>= (round (slow-stocastic (getd tdate 'ydate) (* 3 dur))) 80))
                             (neql (gethash tdate SRR) 'OS)
                             (> (/ (- entry-short cover-short) risk) *min-reward-risk-ratio-position*)
                             (member (trend-signal tdate 5) '(DN CD))
                             (member (trend-signal tdate 15) '(DN CD)))
                             'exit-long)

                       ((and (< (gethash tdate RS)(gethash (getd tdate 'ydate) RS))
                             (or (eql (gethash tdate SRR) 'OB)(eql (gethash (getd tdate 'ydate) SRR) 'OB)
                                 (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OB)))
                             'exit-long)
                       ((and  (<= (round (rsi-ave-diff tdate (* 2 dur) 2)) 0)
                              (or (>= (gethash tdate TI) 4)(>= (gethash (getd tdate 'ydate) TI) 4)
                                  (>= (gethash (getd (getd tdate 'ydate) 'ydate) TI) 4))
                              (>= (round (slow-stocastic tdate (* 3 dur))) 20))
                              'exit-long)

                       ((and (>= (gethash tdate TI) 4)
                             (> (/ (- entry-short cover-short) risk) *min-reward-risk-ratio-position*))
                              'exit-long)
                        ((and (>= (gethash tdate TI) 4)
                              (> (/ (- cover-long entry-long) risk) *min-reward-risk-ratio-position*))
                              'exit-short)


                        (t nil)))

      (multiple-value-setq (P&L plt trades)
         (day-trade-test1 tdate num best-param1 best-param2 best-param3 dur nil))


      (multiple-value-setq (equity-signal new-P&L new-plt draw)
         (equity-filter trades 3 nil))

      (setq stop-short (+ entry-short risk) stop-long (- entry-long risk))



;     (if (and equity-signal (>= new-P&L P&L)(>= (/ new-P&L new-plt)(/ P&L plt)))
;         (setf (gethash *data-name* *DMI*) (append (gethash *data-name* *DMI*) '(UP DN))))

 ;;;
     (if  (< P&L 0)(incf confirmations))
     (if (and (< P&L 0)(< new-P&L 0))(incf confirmations))

       (progn

          ; (if (and (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*)
          ;          (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*)) (push "NOT TODAY" action))


           (if (and (not (member exitpoint '(exit-short exit-long)))
                    (< (length (intersection (gethash *data-name* *DMI*) '(CU UP T))) confirmations)
                    (< (length (intersection (gethash *data-name* *DMI*) '(CD DN T))) confirmations)) (push "NOT TODAY" action))

           ;(if (and (not (member exitpoint '(exit-short exit-long)))
           ;         (< (length (intersection (gethash *data-name* *DMI*) '(CU UP T))) confirmations)
           ;         (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*)) (push "NOT TODAY" action))

           ;(if (and (not (member exitpoint '(exit-short exit-long)))
           ;         (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*)
           ;         (< (length (intersection (gethash *data-name* *DMI*) '(CD DN T))) confirmations)) (push "NOT TODAY" action))



            (if (and (neql exitpoint 'exit-long)
                     (> (gethash tdate HT) *ratio*)(< (/ (- entry-short cover-short) (- stop-short entry-short)) 5)
                     (> (/ (- cover-long entry-long) (- entry-long stop-long)) .2)
                     (neql (gethash tdate RC) 'DN)
                     )
                (push "NOT SHORT" action))
            (if (and (neql exitpoint 'exit-short)
                     (> (gethash tdate HT) *ratio*)(< (/ (- cover-long entry-long) (- entry-long stop-long)) 5)
                     (> (/ (- entry-short cover-short) (- stop-short entry-short)) .2)
                     (neql (gethash tdate RC) 'UP)
                     )
                 (push "NOT LONG" action))

          ; (if (< (length (intersection (gethash *data-name* *DMI*) '(UP T))) confirmations) (push "NOT LONG" action))
          ; (if (< (length (intersection (gethash *data-name* *DMI*) '(DN T))) confirmations) (push "NOT SHORT" action))

           (if (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*) (push "NOT LONG" action))
           (if (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*) (push "NOT SHORT" action))

           (when (eql exitpoint 'exit-long)
            (setq action (remove "NOT SHORT" action :test #'equal))
            (setq action (remove "NOT TODAY" action :test #'equal))
            )
           (when (eql exitpoint 'exit-short)
            (setq action (remove "NOT LONG" action :test #'equal))
            (setq action (remove "NOT TODAY" action :test #'equal))
            )
            (when (if (member *data-name* '(US.D1B TY.D1B))
                  (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                                   (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-day-risk*)
                 (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                                  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))))
                                  (index-point-value))) *max-day-risk*)) (push "NOT TODAY" action))


               );;closes the when


         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))
         (format output "   ~A ~D ~4,2,,,F ~5,2,,,F ~5,2,,,F~%" action confirmations (gethash tdate HT)
           (/ (- entry-short cover-short) risk) (/ (- cover-long entry-long) risk))

       (day-trade-test1 tdate num best-param1 best-param2 best-param3 dur output)

       (setq *reward-risk-ratio* initial-reward-risk-ratio)

;      (shell (string-append "cp /home/register/cycles/trend.dat /home/register/cycles/" "day-"(make-oec-symbol *data-name* tdate) ".dat"))

;      (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
;                               (zerop (nth 1 (assoc *data-name* *C-list*))))  "~D,~D,~D,~D,~%"
;                          (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,~D,~%")))
;



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

      (format output "~% Equity-signal ~A  New P&L= ~D  Num Trades= ~A  Drawdown= ~D~%" equity-signal new-P&L new-plt draw)

;     (setq sector (get-sector *data-name*) oec-symbol (make-oec-symbol *data-name* tdate)
;           entry-time (second (assoc *data-name* *market-times-list*)) exit-time (third (assoc *data-name* *market-times-list*))
;           cancel-time (add-minutes exit-time -15))

      (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
             ;(write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output2)


             ;(write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;(write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;(write-oec-record "True" sector "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;(write-oec-record "True" sector "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;(when (member *data-name* *select-list*)
             ;      (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;  )
             (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
            ; (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output2)


           ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order

           ;  (when (member *data-name* *select-list*)
           ;        (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
           ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
           ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
           ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
           ;    )

                      )

             ((equal action "NOT SHORT")
               (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
              ; (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output2)

            ;   (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
            ;   (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
            ;   (write-oec-record "True" sector "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
            ;   (write-oec-record "True" sector "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order

            ;  (when (member *data-name* *select-list*)
            ;        (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
            ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
            ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
            ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
            ;     )
			 )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
             ; (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output2)

             ;  (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;  (write-oec-record "True" sector "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;  (write-oec-record "True" sector "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;  (when (member *data-name* *select-list*)
             ;         (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;         )
                      ))


     );;closes the when
      (values P&L plt new-P&L new-plt (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))
      ))




 ;;;;with this version you do not check opening prices
;;;;you adjust the stop if there is overnight slippage
(defun day-trade-test1 (date2 num &optional (param1 .75)(param2 .02) (param3 1) (dur 4) (outfile T))
 (let (date trades long short cover-long cover-short long-entry short-entry
       ratio  risk-short risk-long  date-1
        ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short
       ave4 risk (path1 "/home/register/cycles/trend.dat"))
   (declare (special  HT   VT  AT RC  ))
   (declare (ignore ignore))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility-log date dur 1)(volatility-log date (* 7 dur) 1)))
            (setf (gethash date HT) ratio)))

   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months

   (cond ((gethash date AT))
         (t (setf (gethash date AT) (ave date  dur 'pivot))))


;    (cond ((gethash date RC))
;          ((setf (gethash date RC) (or (trend-signal date 3) 'FT))))



   (multiple-value-setq (short-entry long-entry)(vprices date dur param1))


   (setq ave4 (gethash date AT)
        ; cover-short (exp (- (log ave4) (* param2 (gethash date VT) (gethash date PT))))
        ; cover-long (exp (+ (log ave4) (* param2 (gethash date VT) (gethash date PT))))
         cover-short (exp (- (log ave4) (* param2 (gethash date VT))))
         cover-long (exp (+ (log ave4) (* param2 (gethash date VT))))
         )


   (setq risk-short  (abs (* param3 (- short-entry (getd date 'close)))))
   (setq risk-long   (abs (* param3 (- long-entry (getd date 'close)))))

   (setq risk (max risk-long risk-short))

   (setq  date-1 date date (add-mkt-days date 1))


   (when (getd date 'rollover) (setq  long-entry (+ long-entry (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))


    ))
   (when (getd date 'rollover) (setq short-entry (+ short-entry (getd date 'rollover))
    	                              cover-short (+ cover-short (getd date 'rollover))

    ))

;;;check if new entry

   (when (and (not short)
              (<= (* risk (index-point-value)) *max-day-risk*)
              (<= (getd date 'low) short-entry)
              (>= (/ (- short-entry cover-short) risk) *reward-risk-ratio*)

             ; (or (< (gethash date-1 HT) *ratio*)
             ;     (>= (/ (- short-entry cover-short) risk) 5)
             ;     (<= (/ (- cover-long long-entry) risk) .2)
             ;      )

               )
          (setq short (min (getd date 'open) short-entry)
                short-trade (list date 'short short)
                stop-short (+ short risk)
                 ))


    (when (and (not long)
               (<= (* risk (index-point-value)) *max-day-risk*)
               (>= (getd date 'high) long-entry)
               (>= (/ (- cover-long long-entry) risk) *reward-risk-ratio*)

             ;  (or (< (gethash date-1 HT) *ratio*)
             ;      (>= (/ (- cover-long long-entry) risk) 5)
             ;      (<= (/ (- short-entry cover-short) risk) .2)
             ;       )
             )

           (setq long (max (getd date 'open) long-entry)
                 trade (list date 'long long)
                 stop-long (- long risk)
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
                (or (<= (getd date 'low) short-entry)
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
                (or (>= (getd date 'high) long-entry)
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


;;;check if met exit criteria to exit at end of day of entry for day trade
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


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ;(format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
  );;;closes the when outfile
  ;   (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
     ; (/ (length (remove-if #'minusp trades))(length trades))
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ));


(defun find-best-day-trade2 (tdate num &optional (best-dur 4) (output T)(output1 T)(date1 nil))
  (let ((best-param1 *entry-factor*) trades draw (confirmations 0)
         risk risk-short risk-long  ddate
         stop-short stop-long (VT (make-hash-table)) entry-long entry-short
        (best-param2 *objective-factor*) (best-param3 *stop-loss-day*)
        equity-signal new-P&L new-plt exitpoint (SRR (make-hash-table))
          (HT (make-hash-table)) (RS (make-hash-table))
          (TS5 (make-hash-table))(TS15 (make-hash-table)) (TI (make-hash-table))
        cover-long cover-short ave4 action directive1 plt P&L (AT (make-hash-table)))

  (declare (special HT VT TI AT ))

  (setf (gethash tdate RS)(rsi-ave-diff tdate 14 2))
  (setf (gethash (getd tdate 'ydate) RS)(rsi-ave-diff (getd tdate 'ydate) 14 2))
  (setf (gethash tdate SRR)(or (standard-reward-risk1 tdate) 'FT))
  (setf (gethash (getd tdate 'ydate) SRR)(or (standard-reward-risk1 (getd tdate 'ydate)) 'FT))
  (setf (gethash (getd (getd tdate 'ydate) 'ydate) SRR)
        (or (standard-reward-risk1 (getd (getd tdate 'ydate) 'ydate)) 'FT))

  (setf (gethash tdate TS5)(trend-signal tdate 5))
  (setf (gethash tdate TS15)(trend-signal tdate 15))



  (setf (gethash tdate HT) (/ (volatility-log tdate 4 1)(volatility-log tdate (* 5 4) 1)))
  (setf (gethash tdate VT)(volatility-log tdate 60 1))

  (setq ddate tdate)
  (dotimes (ith 3)
     (cond ((gethash ddate  TI))
           (t (setf (gethash ddate TI)(timing-index ddate))))
     (setq ddate (getd ddate 'ydate)))



;;; first iteration to revise best-param1 the entry parameter

;   (setq prev-result -50000 )
;
;   (do ((param  .6  (+ param .02)))
;      ((> param .9) best-param1)
;       (setq result
;          (day-trade-test2 tdate num param best-param2 best-param3 best-dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))


;;;first iteration on the volatility ratio
;   (setq prev-result -50000 )
;   (do ((param  .90  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test2 tdate num param best-param2 best-param3 best-dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)
;;;first iteration to revise parameter 3 the stop loss

;  (setq prev-result -50000)
;  (do ((param  (* .8 best-param1) (+ param .05)))
;      ((> param (* 1.2 best-param1)) best-param3)
;      (setq result
;          (day-trade-test2 tdate num best-param1 best-param2 param best-dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param3 param)))

 ;;first iteration to revise best-param2 the objective

;  (setq prev-result -50000 )
;
;  (do ((param 1.7 (+ param .1)))
;      ((> param 2.6) best-param2)
;      (setq result
;          (day-trade-test2 tdate num best-param1 param best-param3 best-dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param2 param)))


;;;;first iteration is over the dur parameter
;   (setq prev-result -50000 )
;   (dolist (param '(2 3 4 5 7))
;       (setq result
;          (day-trade-test2 tdate num best-param1 best-param2 best-param3 param nil))
;       (if (> result prev-result) (setq prev-result result best-dur param)))
;
;;;second iteration to revise best-param1

;   (setq prev-result -50000 )
;
;   (do ((param  .60  (+ param .02)))
;       ((> param .80)  best-param1)
;       (setq result
;          (day-trade-test2 tdate num param best-param2 best-param3 best-dur nil))
;       (if (> result prev-result) (setq prev-result result best-param1 param)))  ;

;;;second iteration on the volatility ratio
; (setq prev-result -50000 )
;   (do ((param  .90  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test2 tdate num param best-param2 best-param3 best-dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)

;;;second iteration to revise parameter 3 the stop loss
;
;  (setq prev-result -50000)
;  (do ((param  (* .8 best-param1) (+ param .05)))
;      ((> param (* 1.1 best-param1)) best-param3)
;      (setq result
;          (day-trade-test2 tdate num best-param1 best-param2 param best-dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param3 param)))


;;;second iteration is over the dur parameter
;   (setq prev-result -50000 )
;   (dolist (param '(2 3 4 5 7))
;     (setq result
;          (day-trade-test2 tdate num best-param1 best-param2 best-param3 param nil))
;     (if (> result prev-result) (setq prev-result result best-dur param)))



;;; third iteration on the volatility ratio
;   (setq prev-result -50000 )
;   (do ((param  .90  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test2 tdate num param best-param2 best-param3 best-dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)



    (setf (gethash tdate AT) (ave tdate best-dur 'pivot))


  (when output  (format output "~6,2,,,F  ~6,2,,,F ~6,2,,,F ~A ~4,2,,,F ~4,2,,,F"
                        best-param1  best-param2 best-param3 best-dur *ratio* *reward-risk-ratio*)
      (setq ave4 (gethash tdate AT)
           cover-short (exp (- (log ave4) (* best-param2 (gethash tdate VT))))
           cover-long (exp (+ (log ave4) (* best-param2 (gethash tdate VT))))
           )

      (multiple-value-setq (entry-short entry-long)
          (vprices tdate best-dur best-param1 1))


     (setq risk-short (min (*  best-param3 (abs (- entry-short (getd tdate 'close))))
                           ))
                           ;(/ *max-day-risk* (index-point-value))))

     (setq risk-long (min (*  best-param3 (abs (- entry-long (getd tdate 'close))))
                       ))
                       ;(/ *max-day-risk* (index-point-value))))

     (setq risk (max risk-long risk-short))

     (setq exitpoint (cond ((and (>= (round (rsi-ave-diff tdate (* 2 best-dur) 2)) 0)
                             ;(or (<= (round (slow-stocastic tdate (* 3 best-dur))) 20)
                             ;    (<= (round (slow-stocastic (getd tdate 'ydate) (* 3 best-dur))) 20))
                             ;(or (>= (gethash tdate RS) 4)(>= (gethash (getd tdate 'ydate) RS) 4)
                             ;    (>= (gethash (getd (getd tdate 'ydate) 'ydate) RS) 4))
                             (plusp risk)(> (/ (- cover-long entry-long) risk) *min-reward-risk-ratio-position*)
                             (member (trend-signal tdate 5) '(UP CU))
                             (member (trend-signal tdate 15) '(UP CU)))
                             'exit-short)
                           ((and (> (gethash tdate RS)(gethash (getd tdate 'ydate) RS))
                             (neql (gethash tdate SRR) 'OB)
                             (or (eql (gethash tdate SRR) 'OS)(eql (gethash (getd tdate 'ydate) SRR) 'OS)
                                 (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OS)))
                             'exit-short)
                           ((and  (>= (round (rsi-ave-diff tdate (* 2 best-dur) 2)) 0)
                              (or (>= (gethash tdate TI) 4)(>= (gethash (getd tdate 'ydate) TI) 4)
                                  (>= (gethash (getd (getd tdate 'ydate) 'ydate) TI) 4))
                              (<= (round (slow-stocastic tdate (* 3 best-dur))) 80))
                              'exit-short)
                           ((and (<= (round (rsi-ave-diff tdate (* 2 best-dur) 2)) 0)
                             ;(or (>= (round (slow-stocastic tdate (* 3 best-dur))) 80)
                             ;    (>= (round (slow-stocastic (getd tdate 'ydate) (* 3 best-dur))) 80))
                             ;(or (>= (gethash tdate RS) 4)(>= (gethash (getd tdate 'ydate) RS) 4)
                             ;    (>= (gethash (getd (getd tdate 'ydate) 'ydate) RS) 4))
                             (plusp risk)(> (/ (- entry-short cover-short) risk) *min-reward-risk-ratio-position*)
                             (member (trend-signal tdate 5) '(DN CD))
                             (member (trend-signal tdate 15) '(DN CD)))
                             'exit-long)

                           ((and (< (gethash tdate RS)(gethash (getd tdate 'ydate) RS))
                             (neql (gethash tdate SRR) 'OS)
                             (or (eql (gethash tdate SRR) 'OB)(eql (gethash (getd tdate 'ydate) SRR) 'OB)
                                 (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OB)))
                             'exit-long)

                           ((and  (<= (round (rsi-ave-diff tdate (* 2 best-dur) 2)) 0)
                              (or (>= (gethash tdate TI) 4)(>= (gethash (getd tdate 'ydate) TI) 4)
                                  (>= (gethash (getd (getd tdate 'ydate) 'ydate) TI) 4))
                              (>= (round (slow-stocastic tdate (* 3 best-dur))) 20))
                              'exit-long)

                        (t nil)))

      (multiple-value-setq (P&L plt trades)
         (day-trade-test2 tdate num best-param1 best-param2 best-param3 best-dur nil))

      (multiple-value-setq (equity-signal new-P&L new-plt draw)
         (equity-filter trades 3 nil))

      (setq stop-short (+ entry-short risk) stop-long (- entry-long risk))




 ;    (if (and equity-signal (>= new-P&L P&L)(>= (/ new-P&L new-plt)(/ P&L plt)))
 ;        (setf (gethash *data-name* *DMI*) (append (gethash *data-name* *DMI*) '(UP DN))))

       ;;;list of losers union list of equity-signal beneficiaries
     (if  (< P&L 0) (incf confirmations))
     (if (and (< P&L 0)(< new-P&L 0)) (incf confirmations))

       (progn
          (when (if (member *data-name* '(US.D1B TY.D1B))
                  (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                                   (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-day-risk*)
                 (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                                  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))))
                                  (index-point-value))) *max-day-risk*)) (push "NOT TODAY" action))


          ; (if (and (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*)
          ;          (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*)) (push "NOT TODAY" action))


          ; (if (and (< (length (intersection (gethash *data-name* *DMI*) '(CU UP T))) confirmations)
          ;          (< (length (intersection (gethash *data-name* *DMI*) '(CD DN T))) confirmations)) (push "NOT TODAY" action))

          ; (if (and (< (length (intersection (gethash *data-name* *DMI*) '(CU UP T))) confirmations)
          ;              (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*)) (push "NOT TODAY" action))

          ; (if (and (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*)
          ;          (< (length (intersection (gethash *data-name* *DMI*) '(CD DN T))) confirmations)) (push "NOT TODAY" action))

          ; (if (< (length (intersection (gethash *data-name* *DMI*) '(CU UP T))) confirmations) (push "NOT LONG" action))
          ; (if (< (length (intersection (gethash *data-name* *DMI*) '(CD DN T))) confirmations) (push "NOT SHORT" action))

           (if (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*) (push "NOT LONG" action))
           (if (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*) (push "NOT SHORT" action))

           (if (and (neql exitpoint 'exit-short)
                    (< (/ (- cover-long entry-long) (- entry-long stop-long)) 5)
                    (> (/ (- entry-short cover-short) (- stop-short entry-short)) .2))  (push "NOT LONG" action))

           (if (and (neql exitpoint 'exit-long)
                    (< (/ (- entry-short cover-short) (- stop-short entry-short)) 5)
                    (> (/ (- cover-long entry-long) (- entry-long stop-long)) .2)) (push "NOT SHORT" action))


           (if (eql exitpoint 'exit-long)
            (setq action (remove "NOT SHORT" action :test #'equal)))
           (if (eql exitpoint 'exit-short)
            (setq action (remove "NOT LONG" action :test #'equal)))

               )


         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))
         (format output "    ~A ~D ~5,2,,,F ~5,2,,,F ~5,2,,,F~%" action confirmations (gethash tdate HT)
           (/ (- entry-short cover-short) risk) (/ (- cover-long entry-long) risk))

       (day-trade-test2 tdate num best-param1 best-param2 best-param3 best-dur output)


;      (shell (string-append "cp /home/register/cycles/trend.dat /home/register/cycles/" "day-"(make-oec-symbol *data-name* tdate) ".dat"))

;      (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
;                               (zerop (nth 1 (assoc *data-name* *C-list*))))  "~D,~D,~D,~D,~%"
;                          (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,~D,~%")))
;
;


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

     (format output "~% Equity-signal ~A  New P&L= ~D  Num Trades= ~A  Drawdown= ~D~%" equity-signal new-P&L new-plt draw)


;     (setq sector (get-sector *data-name*) oec-symbol (make-oec-symbol *data-name* tdate)
;           entry-time (second (assoc *data-name* *market-times-list*)) exit-time (third (assoc *data-name* *market-times-list*))
;           cancel-time (add-minutes exit-time -15))

      (cond ((equal action "OK      ")
              (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
        ;     (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output2)
        ;
         ;
          ;   (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
           ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
           ;  (write-oec-record "True" sector "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
            ; (write-oec-record "True" sector "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;(when (member *data-name* *select-list*)
              ;     (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
              ;     (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
               ;    (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
                ;   (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;  )
             (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
           ;  (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output2)


           ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
            ; (write-oec-record "True" sector "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order

           ;  (when (member *data-name* *select-list*)
           ;        (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
             ;      (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
              ;     (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
               ;)

                      )

             ((equal action "NOT SHORT")
               (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
             ;  (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output2)
             ;
             ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
              ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
              ; (write-oec-record "True" sector "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
              ; (write-oec-record "True" sector "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order

             ; (when (member *data-name* *select-list*)
             ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
              ;      (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
               ;     (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
                ;    (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
                 ;)
			 )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
          ;    (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output2)

         ;      (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
         ;      (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
          ;     (write-oec-record "True" sector "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
           ;    (write-oec-record "True" sector "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
            ;   (when (member *data-name* *select-list*)
             ;         (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;         )
                      ))




     );;closes the when

      (values P&L plt new-P&L new-plt (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))
      ))


 ;;;;with this version you do not check opening prices
;;;;you adjust the stop if there is overnight slippage
(defun day-trade-test2 (date2 num &optional (param1 .75)(param2 .02) (param3 1.0)(dur 4) (outfile T))
 (let (date trades long short cover-long cover-short long-entry short-entry
       ratio  risk-short risk-long
        ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short
       ave4 risk (path1 "/home/register/cycles/trend.dat"))
   (declare (special  HT  VT  AT RC ))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility-log date 4 1)(volatility-log date (* 5 4) 1)))
            (setf (gethash date HT) ratio)))

   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months


   (cond ((gethash date AT))
         (t (setf (gethash date AT) (ave date dur 'pivot))))


   (multiple-value-setq (short-entry long-entry)(vprices date dur param1 1))


   (setq ave4 (gethash date AT)
         cover-short (exp (- (log ave4) (* param2 (gethash date VT))))
         cover-long (exp (+ (log ave4) (* param2 (gethash date VT))))
        )


  (setq risk-short (* param3 (abs (- short-entry (getd date 'close)))))
  (setq risk-long (*  param3 (abs (- long-entry (getd date 'close)))))
  (setq risk (max risk-long risk-short))

   (setq date (add-mkt-days date 1))


   (when (getd date 'rollover) (setq  long-entry (+ long-entry (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))


    ))
   (when (getd date 'rollover) (setq short-entry (+ short-entry (getd date 'rollover))
    	                              cover-short (+ cover-short (getd date 'rollover))

    ))


;;;check if new entry

   (when (and (not short)
              (<= (* risk (index-point-value)) *max-day-risk*)
              (<= ratio *ratio*)
              (>= (/ (- short-entry cover-short) risk) *reward-risk-ratio*)
              (<= (getd date 'low) short-entry)
             ; (or (>= (/ (- short-entry cover-short) risk) 5)
             ;     (<= (/ (- cover-long long-entry) risk) .2))

               )
          (setq short (min (getd date 'open) short-entry)

                short-trade (list date 'short short)
                stop-short (+ short risk-short)
                 ))


    (when (and (not long)
               (<= (* risk (index-point-value)) *max-day-risk*)
               (<= ratio *ratio*)
               (>= (/ (- cover-long long-entry) risk) *reward-risk-ratio*)
               (>= (getd date 'high) long-entry)
              ; (or (>= (/ (- cover-long long-entry) risk) 5)
              ;     (<= (/ (- short-entry cover-short) risk) .2))

               )

           (setq long (max (getd date 'open) long-entry)

                 trade (list date 'long long)
                 stop-long (- long risk-long)
                 ))


 ;;;check if stopped out on same day

   (when (and long stop-long (or (> (getd date 'open)(getd date 'close))
                                 (<= (getd date 'low) short-entry))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
     (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))
    (when (and short stop-short (or (< (getd date 'open)(getd date 'close))
                                    (>= (getd date 'high) long-entry))
               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq short-trade (append short-trade (list date 'exit stop-short (- short stop-short))))
           (push short-trade extended-trades)
           (setq short-trade nil short nil stop-short nil
           ))
     (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                                 (>= (getd date 'close) stop-short))
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


;;;check if met exit criteria on day of entry
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



   ;;;;apply commission of $60 per round turn and slippage of 0 ticks per round turn.
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
       )

   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ;(format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
  );;;closes the when outfile

  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ));


 ;;;;this is for the forex day trades

(defun find-best-day-trade4 (tdate num &optional (dur 4) (output T)(output1 T)(date1 nil))
  (let ((best-param1 *entry-factor*) (HT (make-hash-table))
         risk risk-short risk-long (prev-result -20000) (result 0)
         (CAN (make-hash-table))(TL49 (make-hash-table))
          best-ratio (MT (make-hash-table))(RC (make-hash-table))
        stop-short stop-long (VT (make-hash-table)) entry-long entry-short
        (best-param2 *objective-factor*) (best-param3 *stop-loss-day*)
          trades new-P&L new-plt equity-signal draw (MO (make-hash-table))(CCI (make-hash-table))
         (confirmations 1)
        cover-long cover-short ave4 action directive1 plt P&L (AT (make-hash-table)))


  (declare (special EL MT HT MO RC  VT CAN TL49 AT CCI ))
  ;DX1 is for calculating dx and adx
  (setf (gethash tdate HT) (/ (volatility-log tdate dur 1)(volatility-log tdate (* 5 dur) 1)))
  (setf (gethash tdate VT)(volatility-log tdate 60 1))

  ;(setf (gethash tdate VT)(volatility-log tdate 125 1));;volatility over the past 1/2 years
;  (setq reversal (list (reversal-dayp date)(smash-day-type1 date)(smash-day-type2 date)))

  (setf (gethash tdate CAN) (or (candle tdate 6 2) 'FT))
  (setf (gethash tdate RC)(roc tdate 4))
  (setf (gethash tdate CCI) (commodity-channel-index tdate 21))
  (setf (gethash tdate MO)(rsi-ave-diff tdate 9 2))


;;; first iteration to revise best-param1 the entry parameter

   (setq prev-result -50000 )

   (do ((param  .60  (+ param .02)))
      ((> param 1.10) best-param1)
       (setq result
          (day-trade-test4 tdate num param best-param2  dur nil))

      (if (> result prev-result) (setq prev-result result best-param1 param)))


;;;first iteration to revise parameter 3 the stop loss

  (setq prev-result -50000)
  (do ((param  (* .8 best-param1)  (+ param .05)))
      ((> param (* 1.2 best-param1)) best-param3)
      (setq result
          (day-trade-test4 tdate num best-param1 best-param2 dur nil))

      (if (> result prev-result) (setq prev-result result best-param3 param)))

 ;;first iteration to revise best-param2 the objective

  (setq prev-result -50000 )

  (do ((param (* 1.75 best-param1) (+ param .1)))
      ((> param 3.0) best-param2)
      (setq result
          (day-trade-test4 tdate num best-param1 param  dur nil))

      (if (> result prev-result) (setq prev-result result best-param2 param)))


;;;;first iteration is over the dur parameter
;   (setq prev-result -50000 )
;   (dolist (param '(2 3 4 5 7))
;       (setq result
;          (day-trade-test4 tdate num best-param1 best-param2  param nil))
;       (if (> result prev-result) (setq prev-result result dur param)))

;;;first iteration on the volatility ratio
;   (setq prev-result -50000 )
;   (do ((param  .70  (+ param .05)))
;      ((> param 1.3) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test4 tdate num param best-param2  dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)


;;;second iteration to revise best-param1

   (setq prev-result -50000 )

   (do ((param  .6 (+ param .02)))
       ((> param 1.10)  best-param1)
       (setq result
          (day-trade-test4 tdate num param best-param2  dur nil))
       (if (> result prev-result) (setq prev-result result best-param1 param)))

;;;second iteration on the volatility ratio
; (setq prev-result -50000 )
;   (do ((param  .70  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test4 tdate num param best-param2 dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)

;;;first iteration to revise *reward-risk-ratio*

  (setq prev-result -50000)
  (do ((param .80 (+ param .05)))
      ((> param 3.00) *reward-risk-ratio*)
      (setq *reward-risk-ratio* param)
      (setq result
          (day-trade-test4 tdate num best-param1 best-param2 dur nil))
      (if (> result prev-result) (setq prev-result result best-ratio param)))
  (setq *reward-risk-ratio* best-ratio)

;;;second iteration is over the dur parameter
;   (setq prev-result -50000 )
;   (dolist (param '(2 3 4 5 7))
;     (setq result
;          (day-trade-test4 tdate num best-param1 best-param2 param nil))
;     (if (> result prev-result) (setq prev-result result dur param)))



;;; third iteration on the volatility ratio
;   (setq prev-result -50000 )
;   (do ((param  .90  (+ param .05)))
;      ((> param 1.2) *ratio*)
;      (setq *ratio* param)
;      (setq result
;          (day-trade-test4tdate num param best-param2 dur nil))
;      (if (> result prev-result) (setq prev-result result best-ratio param)))
;   (setq *ratio* best-ratio)



    (setf (gethash tdate AT) (ave tdate dur 'pivot))


  (when output  (format output "~5,2,,,F  ~5,2,,,F ~5,2,,,F ~A ~4,2,,,F "
                        best-param1  best-param2 best-param3 dur *reward-risk-ratio*)

      (setq ave4 (gethash tdate AT)
          ; cover-short (exp (- (log ave4) (* best-param2 (gethash tdate VT)(gethash tdate PT))))
          ; cover-long (exp (+ (log ave4) (* best-param2 (gethash tdate VT)(gethash tdate PT))))
           cover-short (exp (- (log ave4) (* best-param2 (gethash tdate VT))))
           cover-long (exp (+ (log ave4) (* best-param2 (gethash tdate VT))))
           )



      (multiple-value-setq (entry-short entry-long)
       ;(vprices-log tdate dur (* (gethash tdate HT) best-param1))
       (vprices tdate dur best-param1)
       )


    ;  (setq risk (* (/ best-param3 best-param1) (abs (- entry-short (ave tdate 1 'pivot)))))

     (setq risk-short (*  best-param3 (abs (- entry-short (getd tdate 'close)))))
     (setq risk-long (*  best-param3 (abs (- entry-long (getd tdate 'close)))))

     (setq risk (max risk-long risk-short))

      (multiple-value-setq (P&L plt trades)
         (day-trade-test4 tdate num best-param1 best-param2 dur nil))


      (multiple-value-setq (equity-signal new-P&L new-plt draw)
         (equity-filter trades 3 nil))

      (setq stop-short (+ entry-short risk) stop-long (- entry-long risk))

;       (if (> (/ (- cover-long entry-long) (- entry-long stop-long)) 5)
;           (setf (gethash *data-name* *DMI*) (push 'UP (gethash *data-name* *DMI*))))
;       (if (> (/ (- entry-short cover-short) (- stop-short entry-short)) 5)
;            (setf (gethash *data-name* *DMI*) (push 'DN (gethash *data-name* *DMI*))))
;
;       (if (< (/ (- cover-long entry-long) (- entry-long stop-long)) .2)
;           (setf (gethash *data-name* *DMI*) (push 'DN (gethash *data-name* *DMI*))))
;       (if (< (/ (- entry-short cover-short) (- stop-short entry-short)) .2)
;            (setf (gethash *data-name* *DMI*) (push 'UP (gethash *data-name* *DMI*))))
;

      ; (if (<= (gethash tdate HT) *ratio*)
       ;   (setf (gethash *data-name* *DMI*) (append (gethash *data-name* *DMI*) '(UP DN))))



 ;    (if (and equity-signal (>= new-P&L P&L)(>= (/ new-P&L new-plt)(/ P&L plt)))
 ;        (setf (gethash *data-name* *DMI*) (append (gethash *data-name* *DMI*) '(UP DN))))

 ;;;
 ;    (if  (or (< P&L 0)(< new-P&L 0)
 ;         (<= new-P&L P&L))
 ;         (incf confirmations))

       (progn
          (when (if (member *data-name* '(US.D1B TY.D1B))
                  (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                                   (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-day-risk*)
                 (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                                  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))))
                                  (index-point-value))) *max-day-risk*)) (push "NOT TODAY" action))

           (if (and (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*)
                    (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*)) (push "NOT TODAY" action))

           (if (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio*) (push "NOT LONG" action))
           (if (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio*) (push "NOT SHORT" action))

           (ifn (or (and (< (gethash tdate MO) -10)
                        (> (gethash tdate CCI) 0)
                        (member (gethash tdate TL49) '(UP CU))
                        (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(> (gethash tdate RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(< (gethash tdate RC) 0))
                          ((eur.d3b chf.d3b) t))
                          )
                   (and (eql (gethash tdate TL49) 'UP)
                        (eql (gethash tdate CAN) 'UP)
                        (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(> (gethash tdate RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(< (gethash tdate RC) 0))
                          ((eur.d3b chf.d3b) t))
                         )
                   (or (>= (/ (- cover-long entry-long) risk) 5)
                       (<= (/ (- entry-short cover-short) risk) .2)
                        )
                    (and (eql (gethash tdate MT) 'UP)
                         (eql (gethash tdate CAN) 'UP)
                         (case  *data-name*
                          ((aud.d3b  jpy.d3b eurgbp.d3b)(> (gethash tdate RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(< (gethash tdate RC) 0))
                          ((eur.d3b chf.d3b) t))
                           ))
                           (push "NOT LONG" action))

               (ifn (or (and (> (gethash tdate MO) 10);;;rsi-ave-diff
                       (< (gethash tdate CCI) 0)
                       (member (gethash tdate TL49) '(DN CN));;trend-signal
                       (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(< (gethash tdate RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(> (gethash tdate RC) 0))
                          ((eur.d3b chf.d3b) t))
                       )
                  (and (eql (gethash tdate TL49) 'DN);;trend-signal
                       (eql (gethash tdate CAN) 'DN)
                       (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(< (gethash tdate RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(> (gethash tdate RC) 0))
                          ((eur.d3b chf.d3b) t))
                       )
                  (or (>= (/ (- entry-short cover-short) risk) 5)
                      (<= (/ (- cover-long entry-long) risk) .2)
                      )
                   (and (eql (gethash tdate MT) 'DN);;bpl-signal1
                        (eql (gethash tdate CAN) 'DN)
                        (case  *data-name*
                          ((aud.d3b  jpy.d3b eurgbp.d3b)(< (gethash tdate RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(> (gethash tdate RC) 0))
                          ((eur.d3b chf.d3b) t))
                         )) (push "NOT SHORT" action))


               );;closes the progn?


         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))
         (format output "   ~A ~D ~7,2,,,F  ~7,2,,,F  ~7,2,,,F~%" action confirmations (gethash tdate HT)
           (/ (- entry-short cover-short) risk) (/ (- cover-long entry-long) risk))

       (day-trade-test4 tdate num best-param1 best-param2  dur output)

;      (shell (string-append "cp /home/register/cycles/trend.dat /home/register/cycles/" "day-"(make-oec-symbol *data-name* tdate) ".dat"))

;      (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
;                               (zerop (nth 1 (assoc *data-name* *C-list*))))  "~D,~D,~D,~D,~%"
;                          (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,~D,~%")))
;



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

      (format output "~% Equity-signal ~A  New P&L= ~D  Num Trades= ~A  Drawdown= ~D~%" equity-signal new-P&L new-plt draw)

;     (setq sector (get-sector *data-name*) oec-symbol (make-oec-symbol *data-name* tdate)
;           entry-time (second (assoc *data-name* *market-times-list*)) exit-time (third (assoc *data-name* *market-times-list*))
;           cancel-time (add-minutes exit-time -15))

      (cond ((equal action "OK      ")
             (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
             ;(write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output2)


             ;(write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;(write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;(write-oec-record "True" sector "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;(write-oec-record "True" sector "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;(when (member *data-name* *select-list*)
             ;      (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;  )
             (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
            ; (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output2)


           ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
           ;  (write-oec-record "True" sector "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order

           ;  (when (member *data-name* *select-list*)
           ;        (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
           ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
           ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
           ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
           ;    )

                      )

             ((equal action "NOT SHORT")
               (write-xml-record tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output1)
              ; (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'LONG date1 entry-long stop-long cover-long output2)

            ;   (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
            ;   (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
            ;   (write-oec-record "True" sector "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
            ;   (write-oec-record "True" sector "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order

            ;  (when (member *data-name* *select-list*)
            ;        (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time cancel-time ) ;;;main entry order
            ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" stop-long entry-time exit-time ) ;;; stop loss order
            ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "None" cover-long entry-time exit-time ) ;;; objective limit order
            ;        (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
            ;     )
			 )

             ((equal action "NOT LONG")
              (write-xml-record tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output1)
             ; (write-xml-record-ninja tdate "tblTradeRecs" 'DAY 'SHORT date1 entry-short stop-short cover-short output2)

             ;  (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;  (write-oec-record "True" sector "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;  (write-oec-record "True" sector "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;  (when (member *data-name* *select-list*)
             ;         (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time cancel-time ) ;;;main entry order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" stop-short entry-time exit-time ) ;;; stop loss order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "None" cover-short entry-time exit-time ) ;;; objective limit order
             ;         (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnClose" "None" "Default" entry-time exit-time ) ;;; objective limit order
             ;         )
                      ))


     );;closes the when
      (values P&L plt new-P&L new-plt (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))
      ))




 ;;;;with this version you do not check opening prices
;;;;you adjust the stop if there is overnight slippage
(defun day-trade-test4 (date2 num &optional (param1 .75)(param2 .02) (dur 4) (outfile T))
 (let (date trades long short cover-long cover-short long-entry short-entry
       ratio  risk-short risk-long  date-1
        ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short
       ave4 risk (path1 "/home/register/cycles/trend.dat"))
   (declare (special EL MT HT MO RC TL49 VT CCI AT CAN ))
   (declare (ignore ignore))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))

 ;;;;from date1 to date2
 (dotimes (ith num)

   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility-log date dur 1)(volatility-log date (* 5 dur) 1)))
            (setf (gethash date HT) ratio)))

   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months

   (cond ((gethash date AT))
         (t (setf (gethash date AT) (ave date dur 'pivot))))

   (cond ((gethash date MO))
         (t (setf (gethash date MO) (rsi-ave-diff date 9 2))))

   (cond ((gethash date CAN))
         (t (setf (gethash date CAN) (or (candle date 6 2) 'FT))))

   (cond ((gethash date MT))
         (t (setf (gethash date MT) (bpl-signal1 date 5))))


   (cond ((gethash date RC))
         (t (setf (gethash date RC)(roc date 4))
            ))
   (cond ((gethash date CCI))
         (t (setf (gethash date CCI) (commodity-channel-index date 21))))

   (cond ((gethash date TL49))
         (t (setf (gethash date TL49) (trend-signal date 3))))


   (multiple-value-setq (short-entry long-entry)(vprices date dur param1))


   (setq ave4 (gethash date AT)
        ; cover-short (exp (- (log ave4) (* param2 (gethash date VT) (gethash date PT))))
        ; cover-long (exp (+ (log ave4) (* param2 (gethash date VT) (gethash date PT))))
         cover-short (exp (- (log ave4) (* param2 (gethash date VT))))
         cover-long (exp (+ (log ave4) (* param2 (gethash date VT))))
         )


   (setq risk-short  (abs (- short-entry (getd date 'close))))
   (setq risk-long   (abs (- long-entry (getd date 'close))))

   (setq risk (max risk-long risk-short))

   (setq  date-1 date date (add-mkt-days date 1))


   (when (getd date 'rollover) (setq  long-entry (+ long-entry (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))


    ))
   (when (getd date 'rollover) (setq short-entry (+ short-entry (getd date 'rollover))
    	                              cover-short (+ cover-short (getd date 'rollover))

    ))

;;;check if new entry

   (when (and (not short)
              (<= (* risk (index-point-value)) *max-day-risk*)
              (<= (getd date 'low) short-entry)
              (plusp risk)(>= (/ (- short-entry cover-short) risk) *reward-risk-ratio*)
              (or (and (> (gethash date-1 MO) 10);;;rsi-ave-diff
                       (< (gethash date-1 CCI) 0)
                       (member (gethash date-1 TL49) '(DN CN));;trend-signal
                       (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(< (gethash date-1 RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(> (gethash date-1 RC) 0))
                          ((eur.d3b chf.d3b) t))
                       )
                  (and (eql (gethash date-1 TL49) 'DN);;trend-signal
                       (eql (gethash date-1 CAN) 'DN)
                       (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(< (gethash date-1 RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(> (gethash date-1 RC) 0))
                          ((eur.d3b chf.d3b) t))
                       )
                  (or (and (plusp risk)(>= (/ (- short-entry cover-short) risk) 5))
                      (and (plusp risk)(<= (/ (- cover-long long-entry) risk) .2))
                      )
                   (and (eql (gethash date-1 MT) 'DN);;bpl-signal1
                        (eql (gethash date-1 CAN) 'DN)
                        (case  *data-name*
                          ((aud.d3b  jpy.d3b eurgbp.d3b)(< (gethash date-1 RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(> (gethash date-1 RC) 0))
                          ((eur.d3b chf.d3b) t))
                         )

                    )
               )
          (setq short (min (getd date 'open) short-entry)
                short-trade (list date 'short short)
                stop-short (+ short risk-short)
                 ))


    (when (and (not long)
               (<= (* risk (index-point-value)) *max-day-risk*)
               (>= (getd date 'high) long-entry)
               (plusp risk)(>= (/ (- cover-long long-entry) risk) *reward-risk-ratio*)
               (or (and (< (gethash date-1 MO) -10)
                        (> (gethash date-1 CCI) 0)
                        (member (gethash date-1 TL49) '(UP CU))
                        (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(> (gethash date-1 RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(< (gethash date-1 RC) 0))
                          ((eur.d3b chf.d3b) t))
                          )
                   (and (eql (gethash date-1 TL49) 'UP)
                        (eql (gethash date-1 CAN) 'UP)
                        (case  *data-name*
                          ((aud.d3b jpy.d3b eurgbp.d3b)(> (gethash date-1 RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(< (gethash date-1 RC) 0))
                          ((eur.d3b chf.d3b) t))
                         )
                   (or (>= (/ (- cover-long long-entry) risk) 5)
                       (<= (/ (- short-entry cover-short) risk) .2)
                        )
                    (and (eql (gethash date-1 MT) 'UP)
                         (eql (gethash date-1 CAN) 'UP)
                         (case  *data-name*
                          ((aud.d3b  jpy.d3b eurgbp.d3b)(> (gethash date-1 RC) 0))
                          ((gbp.d3b cad.d3b eurjpy.d3b gbpjpy.d3b)(< (gethash date-1 RC) 0))
                          ((eur.d3b chf.d3b) t))
                           )

                  )
             )

           (setq long (max (getd date 'open) long-entry)
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
                (or (<= (getd date 'low) short-entry)
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
                (or (>= (getd date 'high) long-entry)
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


;;;check if met exit criteria on day of entry
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


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ;(format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
  );;;closes the when outfile
  ;   (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
     ; (/ (length (remove-if #'minusp trades))(length trades))
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ));



 ;;;;with this version you do not check opening prices
;;;;you adjust the stop if there is overnight slippage
(defun day-trade-test3 (date2 num &optional (param1 .75)(param2 .02) (param3 1)(dur 3) (outfile T))
 (let (date trades long short cover-long cover-short long-entry short-entry
         risk-short risk-long
        ave-win ave-loss losers winners extended-trades trade short-trade stop-long stop-short
       ave3 risk (path1 "/home/register/cycles/trend.dat"))
   (declare (special VT  AT SRR RS TS5 TS15))
   (declare (ignore ignore))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))

 ;;;;from date1 to date2
 (dotimes (ith num)


   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date 60 1))));;;volatility over the past three months

   (cond ((gethash date AT))
         (t (setf (gethash date AT) (ave date  dur 'pivot))))


   (multiple-value-setq (short-entry long-entry)(vprices date dur param1))

   (setq ave3 (gethash date AT)
         cover-short (exp (- (log ave3) (* param2 (gethash date VT))))
         cover-long (exp (+ (log ave3) (* param2 (gethash date VT))))
         )

   (setq risk-short  (* param3 (abs (- short-entry (getd date 'close)))))
   (setq risk-long   (* param3 (abs (- long-entry (getd date 'close)))))

   (setq risk (max risk-long risk-short))

   (setq     date (add-mkt-days date 1))


   (when (getd date 'rollover) (setq  long-entry (+ long-entry (getd date 'rollover))
    			              cover-long (+ cover-long (getd date 'rollover))

    ))
   (when (getd date 'rollover) (setq short-entry (+ short-entry (getd date 'rollover))
    	                              cover-short (+ cover-short (getd date 'rollover))

    ))

;;;check if new entry

   (when (and (not short)
              (<= (* risk (index-point-value)) *max-day-risk*)
              (<= (getd date 'low) short-entry)
              (>= (/ (- short-entry cover-short) risk) *reward-risk-ratio*)

             ; (or (>= (/ (- short-entry cover-short) risk) 5)
             ;     (<= (/ (- cover-long long-entry) risk) .2)
             ;       )
                 )
          (setq short (min (getd date 'open) short-entry)
                short-trade (list date 'short short)
                stop-short (+ short risk-short)
                 ))


    (when (and (not long)
               (<= (* risk (index-point-value)) *max-day-risk*)
               (>= (getd date 'high) long-entry)
               (>= (/ (- cover-long long-entry) risk) *reward-risk-ratio*)

              ; (or (>= (/ (- cover-long long-entry) risk) 5)
              ;     (<= (/ (- short-entry cover-short) risk) .2)
              ;     )
             )

           (setq long (max (getd date 'open) long-entry)
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
                (or (<= (getd date 'low) short-entry)
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
                (or (>= (getd date 'high) long-entry)
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


;;;check if met exit criteria on day of entry
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


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)
     (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ;(format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ))
  );;;closes the when outfile
  ;   (round (/ (* (list-sum trades) (index-point-value)) (length trades)))
     ; (/ (length (remove-if #'minusp trades))(length trades))
  (values (round (* (list-sum trades) (or (index-point-value) 1)))
          (length trades) trades)
   ));



 ;;; a function to apply an equity filter to the trades
 ;;; uses a 3 day moving average minus a 10 day moving average.
 ;;;returns signal, value of filtered trades, and number of filtered trades

 (defun equity-filter (trades size2 &optional (rev nil))
     (let ((filtered-trades nil) 10-days signal
           (counter 0)(equity-line (list 0)) (rev-trades (reverse trades)))
      (dolist (jth rev-trades)
         (setq equity-line (push (+ jth (car equity-line)) equity-line)))

     (dolist (ith (cdr (reverse equity-line)))

       (when (and (eql (length 10-days) size2)
            ;(plusp (- (car 10-days) (/ (list-sum 10-days) size2))))
            (= (car 10-days)(max* 10-days)))
           (setq signal T))
       (when (and (eql (length 10-days) size2)
            ;(minusp (- (car 10-days)(/ (list-sum 10-days) size2))))
            (= (car 10-days)(min* 10-days)))
            (setq signal nil))

       (if rev (setq signal (not signal)))

       (if signal (push (nth counter rev-trades) filtered-trades))

       (if (< (length 10-days) (1+ size2)) (push ith 10-days))

       (when (= (length 10-days) (1+ size2))
             (setq 10-days (butlast 10-days)))
       (incf counter)
                    );;;closes the dolist

     (values  signal
               (round (* (list-sum filtered-trades) (or (index-point-value) 1)))
               (length filtered-trades)
               (round (* (drawdown filtered-trades) (or (index-point-value) 1)))

               )

))


;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun volatility-test3 (date2 num &optional (param1 .85) (param2 .01)(param3 1.5)(dur 4) (output T))
 (let* (date stop-long stop-short trades long short ave4  trade-long  entry-short entry-long
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade
       risk risk-long risk-short ;(num-days (* 2 dur)) prices times (filt *n-filt*)
       ;trend-signal-short15 trend-signal-long15
           (path1 "/home/register/cycles/trend.dat"))
     (declare (special RS TS5 TS15  AT VT ))


   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)


;    (cond ((gethash date TS15))
;          (t (setf (gethash date TS15) (trend-signal date 15))))

;    (cond ((gethash date TS5))
;          (t (setf (gethash date TS5) (trend-signal date 5))))

   (cond ((gethash date AT))
         (t (setf (gethash date AT) (ave date dur 'pivot))))

   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date (* 7 dur) 1))));;;volatility over the past three months


   (multiple-value-setq (entry-short entry-long) (vprices date dur param1))


   (setq ave4 (gethash date AT)
       cover-short (exp (- (log ave4) (* param2 (gethash date VT))))
       cover-long (exp (+ (log ave4) (* param2 (gethash date VT))))
              )


   (setq risk-short (* param3  (abs (- entry-short (getd date 'close)))))
   (setq risk-long (* param3  (abs (- entry-long (getd date 'close)))))
   (setq risk (max risk-long risk-short))

;   (multiple-value-setq (trend-signal-short15 trend-signal-long15)(swing-trend-signal15 *data-name*))

   (setq   date (add-mkt-days date 1))


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


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long entry-short)))
   (when short (setq stop-short (min stop-short entry-long)))


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

  ;;;check if met exit criteria  next at next day's open
     (when (and long (<= (/ (- cover-long entry-long) risk) *min-reward-risk-ratio-position*))

            (push (- (getd date 'open) long) trades)
            (setq trade-long (append trade-long (list date 'exit (getd date 'open) (- (getd date 'open) long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))

      (when (and short  (<= (/ (- entry-short cover-short) risk) *min-reward-risk-ratio-position*))

            (push (- short (getd date 'open)) trades)
            (setq trade (append trade (list date 'exit (getd date 'open) (- short (getd date 'open)))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))

;;;check if new entry

    (when (and (not short)
               (<= (getd date 'low) entry-short)
               (<= (* risk (index-point-value)) *max-swing-risk*)
               (>= (/ (- entry-short cover-short) risk) *reward-risk-ratio-swing*)
               )
          (setq short (min entry-short  (getd date 'open))
                 trade (list date 'short short)
                 stop-short
                  (+ short risk-short)
         ))

    (when (and (not long)
               (>= (getd date 'high) entry-long)
               (<= (* risk (index-point-value)) *max-swing-risk*)
               (>= (/ (- cover-long entry-long) risk) *reward-risk-ratio-swing*)
               )
           (setq long (max entry-long (getd date 'open))
                 trade-long (list date 'long long)
                 stop-long
                   (- long risk-long)
              ))

 ;;;check if stopped out on same day of entry
   (when (and long stop-long (> (getd date 'open)(getd date 'close))

               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))


    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                (or (<= (getd date 'low) entry-short)
                    (<= (getd date 'close) stop-long)))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil
           ))


    (when  (and short stop-short  (< (getd date 'open)(getd date 'close))

               (>= (getd date 'high) stop-short))
           (push (- short stop-short) trades)
           (setq trade (append trade (list date 'exit stop-short (- short stop-short))))
           (push trade extended-trades)
           (setq trade nil short nil stop-short nil))


    (when (and short stop-short (>= (getd date 'open)(getd date 'close))
                 (or (>= (getd date 'high) entry-long)
                     (>= (getd date 'close) stop-short)))
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
     ; (if (plusp (list-sum trades)) (round (* (optimal-f trades) (or (index-point-value) 1))) 0)
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

;;;enters and exits with stop orders
;;;does include adjusting stop loss by amount of slippage but no objectives
;;;Do not need to check open price on entry
(defun volatility-test0 (date2 num &optional (param .85) (param3 1.5) (dur 5) (output T))
 (let (date stop-long stop-short trades long short vlow vhigh
       ratio clean-up trade-long  short-entry long-entry
       ave-win ave-loss losers winners extended-trades trade
        (path1 "/home/register/cycles/trend.dat"))
   (declare (special HT))
   (unless (boundp 'HT) (setf HT (make-hash-table) clean-up T))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)

   (cond ((gethash date HT) (setq ratio (gethash date HT)))
         (t (setq ratio (/ (volatility date dur 1)(volatility date (* 5 dur) 1)))
            (setf (gethash date HT) ratio)))

   ;(multiple-value-setq (vlow vhigh) (vprices date dur (* ratio param)))
   (multiple-value-setq (vlow vhigh) (vprices date dur param))

   (setq short-entry vlow long-entry vhigh)


   (setq date (add-mkt-days date 1))

    (when (and (getd date 'rollover) long) (setq long (+ long (getd date 'rollover))))
    (when (and (getd date 'rollover) short)(setq short (+ short (getd date 'rollover))))


   ;(format T "date=~A  entry= ~A  slope=~A  long=~A short=~A stop=~A~%" date entry slope long short stop)
 ;;;;check if stopped out of prior position

   (when long (setq stop-long (max stop-long vlow)))
   (when short (setq stop-short (min stop-short vhigh)))


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


;;;check if new entry
   (when (and (not short)
              (> (getd date 'open) vlow)
              (<= (getd date 'low) vlow)
              (<= ratio *ratio-swing*)
                                  )
          (setq short vlow
                 trade (list date 'short short)
                 stop-short ;(/ (+ vhigh (getd (getd date 'ydate) 'close)) 2))
                 (+ short-entry (* param3 (- (getd (getd date 'ydate) 'close) short-entry)))
         ))

      (when (and (not short)
                 (<= (getd date 'open) vlow)
                 (<= ratio *ratio-swing*)
                                   )
          (setq short (getd date 'open)
                 trade (list date 'short short)
                 stop-short  (+ (getd date 'open) (* param3 (- (getd (getd date 'ydate) 'close) short-entry)))
         ))


    (when (and (not long)
               (< (getd date 'open) vhigh)
               (>= (getd date 'high) vhigh)
               (<= ratio *ratio-swing*)
               )
           (setq long vhigh
                 trade-long (list date 'long long)
                 stop-long  ;(/ (+ vlow (getd (getd date 'ydate) 'close)) 2))
                 (- long-entry (* param3 (- long-entry (getd (getd date 'ydate) 'close))))
              ))

    (when (and (not long)
               (>= (getd date 'open) vhigh)
               (<= ratio *ratio-swing*)
                              )
           (setq long (getd date 'open)
                 trade-long (list date 'long long)
                 stop-long (- (getd date 'open) (* param3 (- long-entry (getd (getd date 'ydate) 'close))))
                  ))
 ;;;check if stopped out on same day of entry
   (when (and long (or (> (getd date 'open)(getd date 'close))
                       (<= (getd date 'low) vlow))
               (<= (getd date 'low) stop-long))
           (push (- stop-long long) trades)
           (setq trade-long (append trade-long (list date 'exit stop-long (- stop-long long))))
           (push trade-long extended-trades)
           (setq trade-long nil long nil stop-long nil))

    (when (and long stop-long (<= (getd date 'open)(getd date 'close))
                               (<= (getd date 'close) stop-long))
           (push (- stop-long long) trades)
           (setq trade (append trade (list date 'exit stop-long (- stop-long long))))
           (push trade extended-trades)
           (setq trade nil long nil stop-long nil
           ))

    (when  (and short (or (< (getd date 'open)(getd date 'close))
                            (>= (getd date 'high) vhigh))
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

   );;;closes the dotimes

  (when clean-up (setq HT nil) (makunbound 'HT))
  ;;;apply commission of 50 per trade
  (setq trades (mapcar #'(lambda (s) (- s (/ 50 (index-point-value)))) trades))

 (when output
   (setq losers (remove-if #'plusp trades) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/ (* (list-sum winners) (index-point-value)) (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (* (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers))) (index-point-value))
          )
     (format output "P/L= ~F  NUMBER TRADES= ~A  ACCURACY= ~,1,2,'*,' F%  PAYOFF RATIO= ~,2,0,'*,' F~%~
        P/L PER TRADE= ~F AVERAGE GAIN= ~F  AVERAGE LOSS= ~F~%LARGEST LOSS= ~F  PROFIT FACTOR= ~,2,0,'*,' F"
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
     );close the format


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A ~A: ~A ~A ~A: ~A P/L: ~A~%" (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith))
    ));;closes the dolist and with-open-file
   ); closes the when
   (list-sum trades)

   ));;;closes the let and the defun


 ;;;;with objectives and adjusting initial stop loss for slippage
(defun find-best-volatility3 (tdate num &optional (dur 4) (output T)(output1 T) (date1 nil))
  (let* ((best-param1 *entry-factor*) (best-param2 *objective-factor-swing*) rollover
         (best-param3 *stop-loss-swing*)  entry-short entry-long
         equity-signal new-P&L new-plt draw trades
        (num-days (* 4 dur))(RS (make-hash-table)) exitpoint ddate
        cover-long cover-short ave4 risk directive1 plt P&L (confirmations 1)
        prev-signal ctr action stop-long stop-short  prices times (filt *n-filt*)
        (TD (make-hash-table))(TT (make-hash-table))(TS15 (make-hash-table))
        (TS5 (make-hash-table))(SRR (make-hash-table))
        (AT (make-hash-table)) (VT (make-hash-table))
        )
   (declare (special  VT RS  AT  TS5 TS15 TD TT  output-oec ))


  (setf (gethash tdate VT)(volatility-log tdate (* 7 dur) 1)) ;;volatility over the past three months

  (setf (gethash tdate AT)(ave tdate dur 'pivot))
  (setf (gethash tdate TS15)(trend-signal tdate 15))
  (setf (gethash tdate TS5)(trend-signal tdate 5))


    (setq ddate tdate)
   (dotimes (ith 3)
    (cond ((gethash ddate SRR))
          (t (setf (gethash ddate SRR)(or (standard-reward-risk1 ddate) 'FT))))
     (setq ddate (getd ddate 'ydate)))


  (setq ddate tdate)
  (dotimes (ith 3)
     (cond ((gethash ddate  RS))
           (t (setf (gethash ddate RS)(timing-index ddate))))
     (setq ddate (getd ddate 'ydate)))

 (setq exitpoint (cond ((and (>= (round (rsi-ave-diff tdate (* 2 dur) 2)) 0)
                             (or (<= (round (slow-stocastic tdate (* 3 dur))) 20)
                                 (<= (round (slow-stocastic (getd tdate 'ydate) (* 3 dur))) 20))
                             (or (>= (gethash tdate RS) 4)(>= (gethash (getd tdate 'ydate) RS) 4)
                                 (>= (gethash (getd (getd tdate 'ydate) 'ydate) RS) 4))
                             (member (gethash tdate TS5) '(UP CU))
                             (or (member (gethash tdate TS15) '(UP CU))
                                 (member (trend-signal tdate 45) '(UP CU)))
                                 )
                             'exit-short)
                         ((and (>= (round (rsi-ave-diff tdate (* 2 dur) 2))
                                  (round (rsi-ave-diff (getd tdate 'ydate) (* 2 dur) 2)))
                                 (< (round (slow-stocastic tdate (* 3 dur))) 80)
                               (member (gethash tdate TS5) '(UP CU))
                               (member (gethash tdate TS15) '(UP CU))
                               (member (trend-signal tdate 45) '(UP CU))
                               (neql (gethash tdate SRR) 'OB)
                               (or (eql (gethash tdate SRR) 'OS)
                                   (eql (gethash (getd tdate 'ydate) SRR) 'OS)
                                   (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OS))
                                   )
                               'exit-short)
                          ((and (>= (round (rsi-ave-diff tdate (* 2 dur) 2)) 0)
                               (< (round (slow-stocastic tdate (* 3 dur))) 80)
                               (member (gethash tdate TS5) '(UP CU))
                               (member (gethash tdate TS15) '(UP CU))
                               (member (trend-signal tdate 45) '(UP CU))
                               (neql (gethash tdate SRR) 'OB)
                                        )
                               'exit-short)

                         ((and (>= (round (rsi-ave-diff tdate (* 2 dur) 2))
                                  (round (rsi-ave-diff (getd tdate 'ydate) (* 2 dur) 2)))
                               (or (>= (gethash tdate RS) 4)(>= (gethash (getd tdate 'ydate) RS) 4)
                                 (>= (gethash (getd (getd tdate 'ydate) 'ydate) RS) 4))
                               (member (gethash tdate TS5) '(UP CU))
                               (or (eql (gethash tdate SRR) 'OS)
                                   (eql (gethash (getd tdate 'ydate) SRR) 'OS)
                                   (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OS))
                                  )
                               'exit-short)
                          ((and  (>= (round (rsi-ave-diff tdate (* 2 dur) 2))
                                  (round (rsi-ave-diff (getd tdate 'ydate) (* 2 dur) 2)))
                                 (<= (round (slow-stocastic tdate (* 3 dur))) 20)
                                 (member (trend-signal tdate 5) '(UP CU))
                                 (or (member (trend-signal tdate 15) '(UP CU))
                                    (member (trend-signal tdate 45) '(UP CU)))
                                (or (eql (gethash tdate SRR) 'OS)
                                  (eql (gethash (getd tdate 'ydate) SRR) 'OS)
                                  (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OS))
                                      )
                               'exit-short)
                         ((and (<= (round (rsi-ave-diff tdate (* 2 dur) 2)) 0)
                               (or (>= (round (slow-stocastic tdate (* 3 dur))) 80)
                                   (>= (round (slow-stocastic (getd tdate 'ydate) (* 3 dur))) 80))
                               (or (>= (gethash tdate RS) 4)(>= (gethash (getd tdate 'ydate) RS) 4)
                                   (>= (gethash (getd (getd tdate 'ydate) 'ydate) RS) 4))
                               (member (gethash tdate TS5) '(DN CD))
                               (or (member (gethash tdate TS15) '(DN CD))
                                  (member (trend-signal tdate 45) '(DN CD)))
                                 )
                             'exit-long)
                          ((and (<= (round (rsi-ave-diff tdate (* 2 dur) 2))
                                 (round (rsi-ave-diff (getd tdate 'ydate) (* 2 dur) 2)))
                                 (> (round (slow-stocastic tdate (* 3 dur))) 20)
                               (member (gethash tdate TS5) '(DN CD))
                               (member (gethash tdate TS15) '(DN CD))
                               (member (trend-signal tdate 45) '(DN CD))
                               (neql (gethash tdate SRR) 'OS)
                               (or (eql (gethash tdate SRR) 'OB)
                                   (eql (gethash (getd tdate 'ydate) SRR) 'OB)
                                   (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OB))
                                 )
                              'exit-long)
                           ((and (<= (round (rsi-ave-diff tdate (* 2 dur) 2)) 0)
                               (> (round (slow-stocastic tdate (* 3 dur))) 20)
                               (member (gethash tdate TS5) '(DN CD))
                               (member (gethash tdate TS15) '(DN CD))
                               (member (trend-signal tdate 45) '(DN CD))
                               (neql (gethash tdate SRR) 'OS)
                                        )
                               'exit-long)
                          ((and  (<= (round (rsi-ave-diff tdate (* 2 dur) 2))
                                     (round (rsi-ave-diff (getd tdate 'ydate) (* 2 dur) 2)))
                                 (or (>= (gethash tdate RS) 4)(>= (gethash (getd tdate 'ydate) RS) 4)
                                     (>= (gethash (getd (getd tdate 'ydate) 'ydate) RS) 4))
                                 (member (gethash tdate TS5) '(DN CD))
                                 (or (eql (gethash tdate SRR) 'OB)
                                     (eql (gethash (getd tdate 'ydate) SRR) 'OB)
                                     (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OB))
                                      )
                               'exit-long)
                            ((and  (<= (round (rsi-ave-diff tdate (* 2 dur) 2))
                                   (round (rsi-ave-diff (getd tdate 'ydate) (* 2 dur) 2)))
                              (>= (round (slow-stocastic tdate (* 3 dur))) 80)
                              (member (trend-signal tdate 5) '(DN CD))
                              (or (member (trend-signal tdate 15) '(DN CD))
                                  (member (trend-signal tdate 45) '(DN CD)))
                              (or (eql (gethash tdate SRR) 'OB)
                                  (eql (gethash (getd tdate 'ydate) SRR) 'OB)
                                  (eql (gethash (getd (getd tdate 'ydate) 'ydate) SRR) 'OB))
                                      )
                               'exit-long)
                        (t nil)))


  (cond ((gethash tdate TD))
        (t (setq *n-filt* (round (* 3 dur)))
           (loop
              (multiple-value-setq (prices times)
               (find-all-primitives (format nil "~sA" (add-mkt-days tdate (- num-days))) (format nil "~sP" tdate)))
              (if (>= (length prices) 4)(return) (setq num-days (+ (* 2 dur) num-days))))
           (setf (gethash tdate TD)(if (> (car prices)(second prices)) 'DN 'UP))
           (setf (gethash tdate TT)(sub-mkt-dates (getnumdate (car times)) tdate))
           (setq *n-filt* filt)))


;;;now find best param3 the stop loss parameter
;    (setq prev-result -10000)
;    (do ((param3 1.0 (+ param3 .05)))
;      ((> param3 1.5) best-param3)
;      (setq result
;          (volatility-test3 tdate num best-param1 best-param2 param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param3 param3)))

 ;;;;first iteration to refine the first param1 the entry
;   (setq prev-result -20000 )
;   (do ((param .60  (+ param .03)))
;      ((> param 1.0 ) best-param1)
;
;      (setq result
;          (volatility-test3 tdate num param best-param2 best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))

;;;second iteration to revise param3 the stop loss
;
;  (setq prev-result -20000)
;  (do ((param3  1.0 (+ param3 .05)))
;      ((> param3 1.5) best-param3)
;      (setq result
 ;         (volatility-test3 tdate num best-param1 best-param2 param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param3 param3)))

 ;;;first iteration to refine the  second param2 the objective
;    (setq prev-result -20000)
;    (do ((param 2.00 (+ param .1)))
;        ((> param 2.60) best-param2)
;      (setq result
;          (volatility-test3 tdate num best-param1 param best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param2 param)))


  ;;;;first iteration to refine the dur parameter
;   (setq prev-result -20000 )
;   (do ((param 3  (+ param 1)))
;      ((> param 7 ) best-param1)
;
;      (setq result
;          (volatility-test3 tdate num best-param1 best-param2 best-param3 param nil))
;
;      (if (> result prev-result) (setq prev-result result dur param)))

 ;;;second iteration to revise param3 the stop loss
;
;  (setq prev-result -20000)
;  (do ((param  best-param1 (+ param .05)))
;      ((> param 1.5) best-param3)
;      (setq result
;          (volatility-test3 tdate num best-param1 best-param2 param dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param3 param)))

 ;;;second iteration to refine the  second param2 the objective
;    (setq prev-result -20000)
;    (do ((param 2.00 (+ param .1)))
;        ((> param 2.6) best-param2)
;      (setq result
;          (volatility-test3 tdate num best-param1 param best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param2 param)))

;;;;;second iteration to refine the first param1 the entry
;   (setq prev-result -20000 )
;   (do ((param .60  (+ param .03)))
;      ((> param .90 ) best-param1)
;
 ;     (setq result
 ;         (volatility-test3 tdate num param best-param2 best-param3 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))


  (when output
     (multiple-value-setq (prev-signal  ctr) (vsignals tdate dur best-param1))
    (format output "~5,2,,,F " best-param1)
    (format output " ~5,2,,,F ~D Signal= ~A CTR= ~A ~3,3,,,F "
                     best-param3 dur prev-signal ctr best-param2 )

    (setq ave4 (gethash tdate AT)
          cover-short (exp (- (log ave4) (* best-param2 (gethash tdate VT))))
          cover-long (exp (+ (log ave4) (* best-param2 (gethash tdate VT))))
                )

    (multiple-value-setq (entry-short entry-long)(vprices tdate dur best-param1))

    (setq risk (* best-param3 (abs (- entry-short (getd tdate 'close)))))

    (multiple-value-setq (P&L plt trades)
      (volatility-test3 tdate num best-param1 best-param2 best-param3 dur nil))

    (multiple-value-setq (equity-signal new-P&L new-plt draw)(equity-filter trades 3 nil))

    (setq stop-short (+ entry-short risk)
          stop-long (- entry-long risk))

    (setq rollover (getd tdate 'rollover))

    (ifn rollover (setq rollover 0))
      ;;; The low volatility that is less that .90 is worth about $32 per trade.
;     (if (and (<= (gethash tdate HT) *ratio-swing*)
;              (member *data-name* lowvol)) (setf (gethash *data-name* *SMI*) (append (gethash *data-name* *SMI*) '(UP DN))))


     ; (multiple-value-setq (swing-trend-short15 swing-trend-long15)(swing-trend-signal15  *data-name*))
     ;(multiple-value-setq (swing-trend-short5 swing-trend-long5)(swing-trend-signal5 *data-name*))

     (if (and equity-signal (>= new-P&L P&L)(>= (/ new-P&L new-plt)(/ P&L plt)))
         (setf (gethash *data-name* *SMI*) (append (gethash *data-name* *SMI*) '(UP DN))))

;;;list of losers union list of equity-signal-list
    (if (or (< P&L 0)(>= new-P&L P&L))
       (incf confirmations))

    (progn

           (if (and (< (length (intersection (gethash *data-name* *SMI*) '(UP T))) confirmations)
                    (< (length (intersection (gethash *data-name* *SMI*) '(DN T))) confirmations)) (push "NOT TODAY" action))

           (if (and (< (length (intersection (gethash *data-name* *SMI*) '(UP T))) confirmations)
                    (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio-swing*)) (push "NOT TODAY" action))

           (if (and (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio-swing*)
                    (< (length (intersection (gethash *data-name* *SMI*) '(DN T))) confirmations)) (push "NOT TODAY" action))

           (if (< (length (intersection (gethash *data-name* *SMI*) '(UP T))) confirmations) (push "NOT LONG" action))
           (if (< (length (intersection (gethash *data-name* *SMI*) '(DN T))) confirmations) (push "NOT SHORT" action))


          ; (if (not (member (gethash tdate TS15) swing-trend-short15))(push "NOT SHORT" action))
          ; (if (not (member (gethash tdate TS15) swing-trend-long15))(push "NOT LONG" action))

          ; (if (not (member (gethash tdate TS5) swing-trend-short5))(push "NOT SHORT" action))
          ; (if (not (member (gethash tdate TS5) swing-trend-long5))(push "NOT LONG" action))


           (if (eql exitpoint 'exit-long) (push "NOT LONG" action))
           (if (eql exitpoint 'exit-short)(push "NOT SHORT" action))

  	  (if (and (eql (gethash tdate TD) 'UP)(<= (gethash tdate TT) dur))(push "NOT SHORT" action))
          (if (and (eql (gethash tdate TD) 'DN)(<= (gethash tdate TT) dur))(push "NOT LONG" action))

           (if (and (neql exitpoint 'exit-short)
                   (< (/ (- cover-long entry-long) (- entry-long stop-long)) *reward-risk-ratio-swing*)) (push "NOT LONG" action))

          (if (and (neql exitpoint 'exit-long)
                   (< (/ (- entry-short cover-short) (- stop-short entry-short)) *reward-risk-ratio-swing*)) (push "NOT SHORT" action))

          (if (member exitpoint '(exit-long exit-short))
            (setq action (remove "NOT TODAY" action :test 'equal)))
         (when
          (if (member *data-name* '(US.D1B TY.D1B))
                  (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                                   (convert-to-decimal (convert-to-32 stop-long))) (index-point-value))) *max-swing-risk*)
                 (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))))
                                  (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))))
                                 (index-point-value))) *max-swing-risk*))
               (push "NOT TODAY" action))


           (when (eql (cadr (assoc *data-name* *open-swings*)) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))
           (when (eql (cadr (assoc *data-name* *open-swings*)) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))

               );;;;closes the progn?

         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))

         (format output "~A  ~4,2,,,F ~4,2,,,F~%" action
           (/ (- entry-short cover-short) risk) (/ (- cover-long entry-long) risk))


    (volatility-test3 tdate num best-param1 best-param2 best-param3 dur output)
;    (shell (string-append "cp /home/register/cycles/trend.dat /home/register/cycles/" "swing-" (make-oec-symbol *data-name* tdate) ".dat"))


;      (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
;                               (zerop (nth 1 (assoc *data-name* *C-list*))))  "~D,~D,~D,~D,~%"
;                          (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,~D,~%")))
;
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
                       ) (index-point-value))))

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
            ))

      (format output "~% Equity-signal ~A  New P&L= ~D  Num Trades= ~A  Drawdown= ~D~%" equity-signal new-P&L new-plt draw)

 ;    (setq sector (get-sector *data-name*) oec-symbol (make-oec-symbol *data-name* tdate)
 ;          entry-time (second (assoc *data-name* *market-times-list*)) exit-time (third (assoc *data-name* *market-times-list*))
 ;          )

      (cond ((equal action "OK      ")

             (write-xml-record tdate "tblTradeRecs" 'SWING 'SHORT date1 entry-short stop-short cover-short output1)
            ; (write-xml-record-ninja tdate "tblTradeRecs" 'SWING 'SHORT date1 entry-short stop-short cover-short output2)


             ;(write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
             ;(write-oec-record "True" sector "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
             ;(write-oec-record "True" sector "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order

             ;(when (member *data-name* *select-list*)
             ;      (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order

             ;  )

             (write-xml-record tdate "tblTradeRecs" 'SWING 'LONG date1 entry-long stop-long cover-long output1)
            ; (write-xml-record-ninja tdate "tblTradeRecs" 'SWING 'LONG date1 entry-long stop-long cover-long output2)


             ;(write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
             ;(write-oec-record "True" sector "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
             ;(write-oec-record "True" sector "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order


            ; (when (member *data-name* *select-list*)
            ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order

             ;  )

                      )

             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" 'SWING 'LONG date1 entry-long stop-long cover-long output1)
             ; (write-xml-record-ninja tdate "tblTradeRecs" 'SWING 'LONG date1 entry-long stop-long cover-long output2)


             ; (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
             ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
             ; (write-oec-record "True" sector "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order


            ;  (when (member *data-name* *select-list*)
            ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order

             ;  )

                            )

             ((equal action "NOT LONG")

               (write-xml-record tdate "tblTradeRecs" 'SWING 'SHORT date1 entry-short stop-short cover-short output1)
              ; (write-xml-record-ninja tdate "tblTradeRecs" 'SWING 'SHORT date1 entry-short stop-short cover-short output2)

         ;     (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
          ;    (write-oec-record "True" sector "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
           ;   (write-oec-record "True" sector "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order

            ;  (when (member *data-name* *select-list*)
             ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
              ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
               ;     (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order
                ;
              ; )

                      ))


             (cond
               ((and (eql (cadr (assoc *data-name* *open-swings*)) 'long)
                     (or (<= (/ (- cover-long entry-long) (- entry-long stop-long)) *min-reward-risk-ratio-position*)
                         (eql exitpoint 'exit-long)
                    ))


                    (write-xml-record tdate "tblTradeRecs" 'OPEN 'LONG date1 nil "Market on Open to Exit" " ----- " output1)
         ;           (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'LONG date1 nil "Market on Open to Exit" " ----- " output2)
         ;
         ;           (write-oec-record "True" sector "Sell" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ) ;;; exit open position on open
         ;           (when (member *data-name* *select-list*)
         ;            (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ))
                    )

                   ((and (eql (cadr (assoc *data-name* *open-swings*)) 'short)
                         (or (<= (/ (- entry-short cover-short) (- stop-short entry-short)) *min-reward-risk-ratio-position*)
                           (eql exitpoint 'exit-short)
                        ))


                    (write-xml-record tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil "Market on Open to Exit" " ----- " output1)
         ;           (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil "Market on Open to Exit" " ----- " output2)
         ;
         ;           (write-oec-record "True" sector "Buy" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ) ;;; exit open position on open
         ;           (when (member *data-name* *select-list*)
         ;            (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ))
                    )

                   ((eql (cadr (assoc *data-name* *open-swings*)) 'long)
                    (setq entry-short (max entry-short (+ rollover (caddr (assoc *data-name* *open-swings*)))))
                    (write-xml-record tdate "tblTradeRecs" 'OPEN 'LONG date1 nil entry-short cover-long output1)
                   ; (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'LONG date1 nil entry-short cover-long output2)

                   ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "GTC" stop-long entry-time "Default" ) ;;; exit open position with stop loss
                   ; (write-oec-record "True" sector "Buy" oec-symbol "Limit" "GTC" cover-long entry-time "Default")
                   ; (when (member *data-name* *select-list*)
                   ;  (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "GTC" stop-long entry-time "Default" )
                   ;  (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "GTC" cover-long entry-time "Default"))
                    )


                   ((eql (cadr (assoc *data-name* *open-swings*)) 'short)
                    (setq entry-long (min entry-long (+ rollover (caddr (assoc *data-name* *open-swings*)))))
                    (write-xml-record tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil entry-long cover-short output1)
                  ;  (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil entry-long cover-short output2)

                  ;  (write-oec-record "True" sector "Buy" oec-symbol "Stop" "GTC" stop-short entry-time "Default" ) ;;; exit open position with stop loss
                  ;  (write-oec-record "True" sector "Sell" oec-symbol "Limit" "GTC" cover-long entry-time "Default") ;;;exit open position with objective
                  ;  (when (member *data-name* *select-list*)
                  ;   (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "GTC" stop-short entry-time "Default" )
                  ;   (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "GTC" cover-long entry-time "Default"))
                    )


                  );;closes the cond

      );;;closes the when

    (values P&L plt new-P&L new-plt (mapcar #'(lambda (s)(* s (or (index-point-value) 1))) trades))
      ))

#|
;;;this writes the day , swing , and open positions records to the dcr.dat file.
(defun write-xml-record (cdate table-name trade-type direction tdate entry-price stop-price cover-price output1)

       (format output1 "<record id=\"~A\">~%" (cdr (assoc *data-name* *ninja-symbol*)))
       (format output1 "<field id=\"TradeType\" type=\"text\">~A</field>~%" trade-type)
       (format output1 "<field id=\"Direction\" type=\"text\">~A</field>~%" direction)
       (format output1 "<field id=\"Market\" type=\"text\">~A</field>~%" (index-lname))
       (format output1 "<field id=\"ContractMonth\" type=\"text\">~A</field>~%" (contract-month *data-name* cdate))

       (format output1 "<field id=\"TradeDate\" type=\"date\">~A</field>~%" tdate)
       (format output1 "<field id=\"TableName\" type=\"text\">")
       (format output1 "~A</field>~%" table-name)

       (if (and ;(nth 3 (assoc *data-name* *open-swings*))
                 (getd cdate 'rollover)
                 (member trade-type '(OPEN OPENP OPENFX))(not (stringp stop-price)))
          (format output1 "<field id=\"Footnote\" type=\"text\"> \* </field>~%"))

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"EntryPrice\" type=\"text\">~7@A</field>~%" (convert-to-32nds entry-price))
         (format output1
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "<field id=\"EntryPrice\" type=\"text\" >~D</field>~%"
             (string-append  "<field id=\"EntryPrice\" type=\"text\">~7," (format nil "~A" (index-digits)) ",0,'*,' F</field>~%"))
             (* (index-tick-size) (round entry-price (index-tick-size)))))

         )

       (if stop-price
        (if (stringp stop-price)
            (format output1 "<field id=\"StopPrice\" type=\"text\">~A</field>~%" stop-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output1 "<field id=\"StopPrice\" type=\"text\">~7@A</field>~%" (convert-to-32nds stop-price))
            (format output1
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "<field id=\"StopPrice\" type=\"text\">~D</field>~%"
              (string-append "<field id=\"StopPrice\" type=\"text\">~7," (format nil "~A" (index-digits)) ",0,'*,' F</field>~%"))
             (* (index-tick-size) (round stop-price (index-tick-size)))))
                  )
             )

        (when cover-price
         (format output1 "<field id=\"CoverPrice\" type=\"text\">")

        (if (stringp cover-price)(format output1 "~A</field>~%" cover-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output1 "~7@A</field>~%" (convert-to-32nds cover-price))
             (format output1
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D</field>~%"
              (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F</field>~%"))
             (* (index-tick-size) (round cover-price (index-tick-size)))))
               )
           )
        ;(format output1 "<field id=\"Risk\">")

        (if (and entry-price stop-price)
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"Risk\" type=\"text\">~D</field>~%"
               (round (* (abs (- (convert-to-decimal (convert-to-32 stop-price))(convert-to-decimal (convert-to-32 entry-price))))
                           (index-point-value))))
         (format output1  "<field id=\"Risk\" type=\"text\">~D</field>~%"
               (round (* (abs (- (* (index-tick-size) (round stop-price (index-tick-size)))
                            (* (index-tick-size) (round entry-price (index-tick-size))))
                       ) (index-point-value))) ))

          )

          (format output1 "</record>~%")

          )

;;;This writes the counter trend records to the dcr.dat file
(defun write-xml-record1 (tdate buy-price sell-price buy-objective sell-objective output1)

       (format output1 "<record id=\"~S\">~%" *data-name*)


       (format output1 "<field id=\"Market\" type=\"text\">~A</field>~%" (nth 5 (assoc *data-name* *X-list*)))

       (format output1 "<field id=\"ContractMonth\" type=\"text\">~A</field>~%" (third (assoc *data-name* *C-list*)) )

       (format output1 "<field id=\"TradeDate\" type=\"date\">~A</field>~%" tdate)

       (format output1 "<field id=\"TableName\" type=\"text\">")


       (format output1 "~A</field>~%" "tblExitPoints")

      (if buy-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"BuyPrice\" type=\"text\">~7@A</field>~%" (convert-to-32nds buy-price))
         (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "<field id=\"BuyPrice\" type=\"text\" >~D</field>~%"
             (string-append  "<field id=\"BuyPrice\" type=\"text\">~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F</field>~%"))
             (* (nth 4 (assoc *data-name* *C-list*)) (round buy-price (nth 4 (assoc *data-name* *C-list*))))))

         )

       (if sell-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"SellPrice\" type=\"text\">~7@A</field>~%" (convert-to-32nds sell-price))
         (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "<field id=\"SellPrice\" type=\"text\" >~D</field>~%"
             (string-append  "<field id=\"SellPrice\" type=\"text\">~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F</field>~%"))
             (* (nth 4 (assoc *data-name* *C-list*)) (round sell-price (nth 4 (assoc *data-name* *C-list*))))))

         )


      (if buy-objective
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"BuyObjective\" type=\"text\">~7@A</field>~%" (convert-to-32nds buy-objective))
         (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "<field id=\"BuyObjective\" type=\"text\" >~D</field>~%"
             (string-append  "<field id=\"BuyObjective\" type=\"text\">~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F</field>~%"))
             (* (nth 4 (assoc *data-name* *C-list*)) (round buy-objective (nth 4 (assoc *data-name* *C-list*))))))

         )


       (if sell-objective
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"SellObjective\" type=\"text\">~7@A</field>~%" (convert-to-32nds sell-objective))
         (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "<field id=\"SellObjective\" type=\"text\" >~D</field>~%"
             (string-append  "<field id=\"SellObjective\" type=\"text\">~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F</field>~%"))
             (* (nth 4 (assoc *data-name* *C-list*)) (round sell-objective (nth 4 (assoc *data-name* *C-list*))))))

         )


          (format output1 "</record>~%")

          )


;;;this writes the day , swing , and open positions records to the ninja.dat file.
(defun write-xml-record-ninja (cdate table-name trade-type direction tdate entry-price stop-price cover-price output1)

       (format output1 "<record id=\"~S\">~%" *data-name*)
       (format output1 "<field id=\"TradeType\" type=\"text\">~A</field>~%" trade-type)
       (format output1 "<field id=\"Direction\" type=\"text\">~A</field>~%" direction)
       (format output1 "<field id=\"Market\" type=\"text\">~A</field>~%" (nth 3 (assoc *data-name* *C-list*)))
       (format output1 "<field id=\"NinjaSymbol\" type=\"text\">~A</field>~%" (cdr (assoc *data-name* *ninja-symbol*)))
       (format output1 "<field id=\"ContractMonth\" type=\"text\">~A</field>~%" (third (assoc *data-name* *C-list*)) )
       (format output1 "<field id=\"OpenTime\" type=\"text\">~A</field>~%" (nth 1 (assoc *data-name* *market-times-list*)))
       (format output1 "<field id=\"CloseTime\" type=\"text\">~A</field>~%" (nth 2 (assoc *data-name* *market-times-list*)))
       (format output1 "<field id=\"TradeDate\" type=\"date\">~A</field>~%" tdate)

       (format output1 "<field id=\"TableName\" type=\"text\">")


       (format output1 "~A</field>~%" table-name)

       (if (and ;(nth 3 (assoc *data-name* *open-swings*))
                (getd cdate 'rollover)
                (eql trade-type 'OPEN)) ;(not (stringp stop-price)))
          (format output1 "<field id=\"Footnote\" type=\"text\"> \* </field>~%"))


      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"EntryPrice\" type=\"text\">~7@A</field>~%" (convert-to-32nds entry-price))
         (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "<field id=\"EntryPrice\" type=\"text\" >~D</field>~%"
             (string-append  "<field id=\"EntryPrice\" type=\"text\">~7,"
                      (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F</field>~%"))
              (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
             (* (nth 4 (assoc *data-name* *C-list*)) (round entry-price (nth 4 (assoc *data-name* *C-list*)))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-price
        (if (stringp stop-price)
            (format output1 "<field id=\"StopPrice\" type=\"text\">~A</field>~%" stop-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output1 "<field id=\"StopPrice\" type=\"text\">~7@A</field>~%" (convert-to-32nds stop-price))
            (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "<field id=\"StopPrice\" type=\"text\">~D</field>~%"
              (string-append "<field id=\"StopPrice\" type=\"text\">~7,"
              (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F</field>~%"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
             (* (nth 4 (assoc *data-name* *C-list*)) (round stop-price (nth 4 (assoc *data-name* *C-list*)))))))
                  )
             )

          ; (format T "cover-price=~A ~%" cover-price)

         (format output1 "<field id=\"CoverPrice\" type=\"text\">")

        (if (stringp cover-price)(format output1 "~A</field>~%" cover-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output1 "~7@A</field>~%" (convert-to-32nds cover-price))
             (format output1
             (if (and (nth 1 (assoc *data-name* *C-list*))
                      (zerop (nth 1 (assoc *data-name* *C-list*))))
                 "~D</field>~%"
              (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F</field>~%"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
             (* (nth 4 (assoc *data-name* *C-list*)) (round cover-price (nth 4 (assoc *data-name* *C-list*)))))))
               )

        ;(format output1 "<field id=\"Risk\">")

        (if (and entry-price stop-price)
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"Risk\" type=\"text\">~D</field>~%"
               (round (* (abs (- (convert-to-decimal (convert-to-32 stop-price))(convert-to-decimal (convert-to-32 entry-price))))
                           (index-point-value))))
         (format output1  "<field id=\"Risk\" type=\"text\">~D</field>~%"
               (round (* (abs (- (* (nth 4 (assoc *data-name* *C-list*)) (round stop-price (nth 4 (assoc *data-name* *C-list*))))
                            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-price (nth 4 (assoc *data-name* *C-list*)))))
                       ) (index-point-value))) ))

          )

        (if (and entry-price stop-price)
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"PotentialLoss\" type=\"text\">~7@A</field>~%"
               (convert-to-32 (abs (- (convert-to-decimal (convert-to-32 stop-price))(convert-to-decimal (convert-to-32 entry-price))))
                           ))
         (format output1  "<field id=\"PotentialLoss\" type=\"text\">~D</field>~%"
                (abs (- (* (nth 4 (assoc *data-name* *C-list*)) (round stop-price (nth 4 (assoc *data-name* *C-list*))))
                            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-price (nth 4 (assoc *data-name* *C-list*))))))
                        ))

          )

        (if (and entry-price cover-price)
        (if (member *data-name* '(US.D1B TY.D1B))
           (format output1 "<field id=\"PotentialGain\" type=\"text\">~7@A</field>~%"
               (convert-to-32 (abs (- (convert-to-decimal (convert-to-32 cover-price))(convert-to-decimal (convert-to-32 entry-price))))
                           ))
         (format output1  "<field id=\"PotentialGain\" type=\"text\">~D</field>~%"
                (abs (- (* (nth 4 (assoc *data-name* *C-list*)) (round cover-price (nth 4 (assoc *data-name* *C-list*))))
                            (* (nth 4 (assoc *data-name* *C-list*)) (round entry-price (nth 4 (assoc *data-name* *C-list*))))))
                        ))

          )

          (format output1 "</record>~%")

          )
|#


 ;;;;with objectives and adjusting initial stop loss for slippage
(defun find-best-volatility7 (tdate num &optional (dur 7) (output T)(output1 T) (date1 nil))
  (let* ((best-param1 (if (<= dur 4) *entry-factor-swing* *entry-factor-position*)) ;(* (sqrt (/ dur 7)) 2.5))
         (best-param2 (if (<= dur 4) *objective-factor-swing* *objective-factor-position*))
          rollover  (best-param3 *stop-loss-position*) entry-short entry-long
         equity-signal new-P&L new-plt draw trades (open-risk 0) long short reward-long reward-short
         cover-long cover-short (ave4 (ave tdate dur 'pivot))  directive1 plt P&L (num-days (* 4 dur))
        prev-signal action stop-long stop-short risk-short risk-long time-frame prices times
        (VT (make-hash-table))(HT (make-hash-table)) (RS (make-hash-table)) (filt *n-filt*)
        (AT (make-hash-table)) (adj 0) (TD (make-hash-table)) (TS45 (make-hash-table))
        (TT (make-hash-table))(TS15 (make-hash-table))(TS5 (make-hash-table)) exitpoint
        )
    (declare (special  RS VT HT AT TD TS5 TS15 TS45 TT adj ))


   (if (<= dur 4)(setq time-frame 'SWING) (setq time-frame 'POSITION adj -2))
   (if (member *data-name* *forex-list*) (setq time-frame 'FX-SWING))

  (setf (gethash tdate VT)(volatility-log tdate (* dur 7) 1)) ;;volatility over the past three months
 ; (setf (gethash tdate TS15)(trend-signal tdate 15))
 ; (setf (gethash tdate TS5)(trend-signal tdate 5))


  ; (multiple-value-setq (swing-trend-short5 swing-trend-long5)(swing-trend-signal5 *data-name*))


  (setq exitpoint (exitpoint-signal tdate))

  (cond ((gethash tdate TD))
        (t (setq *n-filt* (round (* 3 dur)))
           (loop
              (multiple-value-setq (prices times)
               (find-all-primitives (format nil "~sA" (add-mkt-days tdate (- num-days))) (format nil "~sP" tdate)))
              (if (>= (length prices) 4)(return) (setq num-days (+ (* 2 dur) num-days))))
           (setf (gethash tdate TD)(if (> (car prices)(second prices)) 'DN 'UP))
           (setf (gethash tdate TT)(sub-mkt-dates (getnumdate (car times)) tdate))
           (setq *n-filt* filt)))

 ;;;;first iteration to refine the first param1 the entry
;   (setq prev-result -20000 )
;   (do ((param (* (sqrt (/ dur 7)) 1.0) (* param 1.2)))
 ;     ((> param (* (sqrt (/ dur 7)) 3.0)) best-param1)
 ;
 ;    (setq result
  ;       (volatility-test7 tdate num param best-param2  dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))


;;;first iteration to refine the  second param2 the objective
;    (setq prev-result -20000)
;    (do ((param (* best-param1 1.2) (* param 1.2)))
;        ((> param (* best-param1 1.8)) best-param2)
;      (setq result
;          (volatility-test7 tdate num best-param1 param  dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param2 param)))

;;;first iteration on the *reward-risk-ratio-swing* variable

;   (setq prev-result -20000 *best-reward-risk-ratio-swing* 1)
;   (do ((param .6 (+ param .05)))
;      ((> param 1.2) best-param1)
;      (setq *reward-risk-ratio-swing* param)
;     (setq result
;         (volatility-test7 tdate num best-param1 best-param2 dur nil))
;
;      (if (> result prev-result) (setq prev-result result *best-reward-risk-ratio-swing* param)))
;
;    (setq *reward-risk-ratio-swing* *best-reward-risk-ratio-swing*)


;;;;;second iteration to refine the first param1 the entry
;   (setq prev-result -20000 )
;   (do ((param (* best-param1 .82) (* param 1.04)))
;       ((> param (* best-param1 1.18)) best-param1)
;
;      (setq result
;          (volatility-test7 tdate num param best-param2 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))

 ;;;second iteration to refine the  second param2 the objective
;    (setq prev-result -20000)
;    (do ((param 2.50 (+ param .1)))
;        ((> param 3.5) best-param2)
;      (setq result
;          (volatility-test7 tdate num best-param1 param  dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param2 param)))


;;;;;third iteration to refine the first param1 the entry
;   (setq prev-result -20000 )
;   (do ((param 1.5 (+ param .1)))
;       ((> param 3.5) best-param1)
;
;      (setq result
;          (volatility-test7 tdate num param best-param2 dur nil))
;
;      (if (> result prev-result) (setq prev-result result best-param1 param)))


     (setf (gethash tdate AT)(ave tdate (round (+ adj (* 1 dur))) 'pivot))


  (when output
     (multiple-value-setq (prev-signal ) (vsignals tdate (* 5 dur) best-param1 dur))

    (setq ave4 (gethash tdate AT)
          cover-short (exp (- (log ave4) (* best-param2 (gethash tdate VT))))
          cover-long (exp (+ (log ave4) (* best-param2 (gethash tdate VT))))
                              )

    (multiple-value-setq (entry-short entry-long)(vprices tdate (* 5 dur) best-param1 dur))

    (setq entry-short (min entry-short (getd tdate 'close)) entry-long (max entry-long (getd tdate 'close)))

    (setq risk-short (* *stop-loss-position* (min (abs (- entry-short  (n-day-high tdate dur 'close))))
                   ))

    (setq risk-long (* *stop-loss-position* (min (abs (- entry-long  (n-day-low tdate dur 'close))))
                    ))



    (multiple-value-setq (P&L plt trades)
      (volatility-test7 tdate num best-param1 best-param2  dur nil))


    (multiple-value-setq (equity-signal new-P&L new-plt draw)
         (equity-filter trades 3 nil))


    (setq stop-short (+ entry-short risk-short) ;;;this is the init buy stop
          stop-long (- entry-long risk-long))    ;;;this is the init sell stop

    (setq rollover (getd tdate 'rollover))
    (ifn rollover (setq rollover 0))

    (setq long (eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                   ((eql time-frame 'SWING) *open-swings*)
                                                   ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'long))
    (setq short (eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                    ((eql time-frame 'SWING) *open-swings*)
                                                    ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'short))





    (if short (setq stop-short
         (fmin entry-long
             (+ rollover (caddr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*);;stop max dollar risk
                                                         ((eql time-frame 'SWING) *open-swings*)
                                                         ((eql time-frame 'FX-SWING) *forex-open-swings*))))))))

    (if long (setq stop-long
         (fmax entry-short
              (+ rollover (caddr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                          ((eql time-frame 'SWING) *open-swings*)
                                                          ((eql time-frame 'FX-SWING) *forex-open-swings*))))))))



    (if long (setq open-risk (- (getd tdate 'close) stop-long)))
    (if short (setq open-risk (- stop-short (getd tdate 'close))))

    (format output "~5,2,,,F " best-param1)
    (format output " ~5,2,,,F ~D Signal= ~A R/R= ~3,2,,,F ~3,3,,,F "
                     best-param3 dur prev-signal *reward-risk-ratio-swing* best-param2 )

;    (multiple-value-setq (swing-trend-short swing-trend-long)
;      (case  time-frame
;        (SWING (swing-trend-signal15  *data-name*))
;        (POSITION (position-trend-signal45 *data-name*))
;        (FX-SWING (swing-trend-signal15 *data-name*))))
;


   ;  (if (and equity-signal (>= new-P&L P&L)(>= (/ new-P&L new-plt)(/ P&L plt)))
   ;      (setf (gethash *data-name* *SMI*) (append (gethash *data-name* *SMI*) '(UP DN))))
;;;list of losers union list of equity-signal-list
   ; (if (or (< P&L 0)(>= new-P&L P&L))
   ;    (setq confirmations 1)(setq confirmations 0))

    (progn
          (when
          (if (member *data-name* '(US.D1B TY.D1B))
              (> (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
                               (convert-to-decimal (convert-to-32 stop-long)));;;closes the -
                           (index-point-value)));;;closes the round
                 (if (<= dur 4) *max-swing-risk* *max-position-risk*));;;closes the >
              (> (round (* (-  (* (nth 4 (assoc *data-name* *C-list*)) (round stop-short (nth 4 (assoc *data-name* *C-list*))));;closes the *
                               (* (nth 4 (assoc *data-name* *C-list*)) (round entry-short (nth 4 (assoc *data-name* *C-list*)))));;closes the -
                           (index-point-value)));;;closes the round
                 (if (<= dur 4) *max-swing-risk* *max-position-risk*))) (push "NOT TODAY" action));;;closes the when


           (if (and (not (member exitpoint '(L1 L2 L3 L4 L5)))
                    (< (/ (- cover-long entry-long) risk-long) *reward-risk-ratio-swing*)) (push "NOT LONG" action))

           (if (and (not (member exitpoint '(H1 H2 H3 H4 H5)))
                    (< (/ (- entry-short cover-short) risk-short) *reward-risk-ratio-swing*)) (push "NOT SHORT" action))

            ;(if (and (member time-frame '(swing fx-swing))(not (member (gethash tdate TS15) swing-trend-short)))(push "NOT SHORT" action))
            ;(if (and (member time-frame '(swing fx-swing))(not (member (gethash tdate TS15) swing-trend-long)))(push "NOT LONG" action))

           ; (if (and (eql time-frame 'position)(not (member (gethash tdate TS15) swing-trend-short)))(push "NOT SHORT" action))
           ; (if (and (eql time-frame 'position)(not (member (gethash tdate TS15) swing-trend-long)))(push "NOT LONG" action))

           ; (if (and (eql time-frame 'swing)(not (member (gethash tdate TS5) swing-trend-short5)))(push "NOT SHORT" action))
           ; (if (and (eql time-frame 'swing)(not (member (gethash tdate TS5) swing-trend-long5)))(push "NOT LONG" action))

           (if (member exitpoint '(H1 H2 H3 H4 H5))(push "NOT LONG" action))
           (if (member exitpoint '(L1 L2 L3 L4 L5))(push "NOT SHORT" action))

           (if (and (eql (gethash tdate TD) 'UP)(> (gethash tdate TT) dur))(push "NOT LONG" action))
           (if (and (eql (gethash tdate TD) 'DN)(> (gethash tdate TT) dur))(push "NOT SHORT" action))

          (if (member exitpoint '(H1 H2 H3 H4 H5))(setq action (remove "NOT SHORT" action :test #'equal)))
          (if (member exitpoint '(L1 L2 L3 L4 L5))(setq action (remove "NOT LONG" action :test #'equal)))


           (when (eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                     ((eql time-frame 'SWING) *open-swings*)
                                                     ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'long)
                 (push "NOT LONG" action)(push "OPEN LONG" action))


           (when (eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                     ((eql time-frame 'SWING) *open-swings*)
                                                     ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'short)
                 (push "NOT SHORT" action)(push "OPEN SHORT" action))

               )


         (setq action (cond ((member "NOT TODAY" action :test #'equal) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equal)
                                  (member "NOT SHORT" action :test #'equal)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equal) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equal) "NOT SHORT")
                            (t  "OK      ")))


         (if long (setq reward-long (- cover-long (getd tdate 'close)))
             (setq reward-long (- cover-long entry-long)))
         (if short (setq reward-short (- (getd tdate 'close) cover-short))
             (setq reward-short (- entry-short cover-short)))
         (if (= open-risk 0)(setq open-risk (+ open-risk (index-tick-size))));;;prevents dividing by zero



         (format output "~A  ~4,2,,,F ~4,2,,,F~%" action
           (/ reward-short (if short open-risk risk-short)) (/ reward-long (if long open-risk risk-long)))



    (volatility-test7 tdate num best-param1 best-param2  dur output)
;    (shell (string-append "cp /home/register/cycles/trend.dat /home/register/cycles/" "swing-" (make-oec-symbol *data-name* tdate) ".dat"))


;      (setq directive (if (and (nth 1 (assoc *data-name* *C-list*))
;                               (zerop (nth 1 (assoc *data-name* *C-list*))))  "~D,~D,~D,~D,~%"
;                          (string-append "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,"
;                                     "~7," (format nil "~A" (nth 1 (assoc *data-name* *C-list*))) ",0,'*,' F,~D,~%")))
;
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
                       ) (index-point-value))))

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
            ))

      (format output "~% Equity-signal ~A  New P&L= ~D  Num Trades= ~A  Drawdown= ~D~%" equity-signal new-P&L new-plt draw)

;     (setq sector (get-sector *data-name*) oec-symbol (make-oec-symbol *data-name* tdate)
;           entry-time (second (assoc *data-name* *market-times-list*)) exit-time (third (assoc *data-name* *market-times-list*))
;           )

      (cond ((equal action "OK      ")

             (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
            ; (write-xml-record-ninja tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output2)


            ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
            ; (write-oec-record "True" sector "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
            ; (write-oec-record "True" sector "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order

             ;(when (member *data-name* *select-list*)
             ;      (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
             ;      (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order
             ;
             ;  )

             (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
            ; (write-xml-record-ninja tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output2)


            ; (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
            ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
            ; (write-oec-record "True" sector "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order


            ; (when (member *data-name* *select-list*)
            ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order

             ;  )

                      )

             ((equal action "NOT SHORT")
              (write-xml-record tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output1)
              ;(write-xml-record-ninja tdate "tblTradeRecs" time-frame 'LONG date1 entry-long stop-long cover-long output2)


             ; (write-oec-record "True" sector "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
             ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
             ; (write-oec-record "True" sector "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order


            ;  (when (member *data-name* *select-list*)
            ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "None" entry-long entry-time exit-time ) ;;;main entry order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "GTC" stop-long "Default" "Default" ) ;;; stop loss order
            ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "GTC" cover-long "Default" "Default" ) ;;; objective limit order

             ;  )

                            )

             ((equal action "NOT LONG")

               (write-xml-record tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output1)
               ;(write-xml-record-ninja tdate "tblTradeRecs" time-frame 'SHORT date1 entry-short stop-short cover-short output2)

             ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
             ; (write-oec-record "True" sector "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
             ; (write-oec-record "True" sector "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order
             ;
             ; (when (member *data-name* *select-list*)
             ;       (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "None" entry-short entry-time exit-time ) ;;;main entry order
             ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "GTC" stop-short "Default" "Default" ) ;;; stop loss order
             ;       (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "GTC" cover-short "Default" "Default" ) ;;; objective limit order
             ;
             ;  )

                      ))

             (cond
               ((and (eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                         ((eql time-frame 'SWING) *open-swings*)
                                                         ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'long)
                      (or (<= open-risk 0)
                          (< (/ (- cover-long (getd tdate 'close)) open-risk) *min-reward-risk-ratio-position*)
                          (member exitpoint '(H1 H2 H3 H4 H5)))
                          )

                      (cond ((eql time-frame 'POSITION)
                             (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil "Market on Open to Exit" " ----- " output1))
                            ((eql time-frame 'SWING)
                              (write-xml-record tdate "tblTradeRecs" 'OPEN 'LONG date1 nil "Market on Open to Exit" " ----- " output1))
                            ((eql time-frame 'FX-SWING)
                             (write-xml-record tdate "tblTradeRecs" 'OPENFX 'LONG date1 nil "Market on Open to Exit" " ----- " output1)))

         ;           (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'LONG date1 nil "Market on Open to Exit" " ----- " output2)
         ;
         ;           (write-oec-record "True" sector "Sell" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ) ;;; exit open position on open
         ;           (when (member *data-name* *select-list*)
         ;            (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ))
                    )

                   ((and (eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                             ((eql time-frame 'SWING) *open-swings*)
                                                             ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'short)
                         (or (<= open-risk 0)
                             (< (/ (- (getd tdate 'close) cover-short) open-risk) *min-reward-risk-ratio-position*)
                             (member exitpoint '(L1 L2 L3 L4 L5)))
                         )
                    (cond ((eql time-frame 'POSITION)
                           (write-xml-record tdate "tblTradeRecs" 'OPENP 'SHORT date1 nil "Market on Open to Exit" " ----- " output1))
                          ((eql time-frame 'SWING)
                           (write-xml-record tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil "Market on Open to Exit" " ----- " output1))
                          ((eql time-frame 'FX-SWING)
                           (write-xml-record tdate "tblTradeRecs" 'OPENFX 'SHORT date1 nil "Market on Open to Exit" " ----- " output1)))

         ;           (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil "Market on Open to Exit" " ----- " output2)
         ;
         ;           (write-oec-record "True" sector "Buy" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ) ;;; exit open position on open
         ;           (when (member *data-name* *select-list*)
         ;            (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "MarketOnOpen" "Default" "Default" entry-time "Default" ))
                    )

                   ((eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                        ((eql time-frame 'SWING) *open-swings*)
                                                        ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'long)


                     (cond ((eql time-frame 'POSITION)
                            (write-xml-record tdate "tblTradeRecs" 'OPENP 'LONG date1 nil stop-long cover-long output1))
                           ((eql time-frame 'SWING)
                            (write-xml-record tdate "tblTradeRecs" 'OPEN 'LONG date1 nil stop-long cover-long output1))
                           ((eql time-frame 'FX-SWING)
                            (write-xml-record tdate "tblTradeRecs" 'OPENFX 'LONG date1 nil stop-long cover-long output1)))
                    ;(write-xml-record-ninja tdate "tblTradeRecs" 'OPENP 'LONG date1 nil entry-short cover-long output2)

                   ; (write-oec-record "True" sector "Sell" oec-symbol "Stop" "GTC" stop-long entry-time "Default" ) ;;; exit open position with stop loss
                   ; (write-oec-record "True" sector "Buy" oec-symbol "Limit" "GTC" cover-long entry-time "Default")
                   ; (when (member *data-name* *select-list*)
                   ;  (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Stop" "GTC" stop-long entry-time "Default" )
                   ;  (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Limit" "GTC" cover-long entry-time "Default"))
                    )


                   ((eql (cadr (assoc *data-name* (cond ((eql time-frame 'POSITION) *open-positions*)
                                                        ((eql time-frame 'SWING) *open-swings*)
                                                        ((eql time-frame 'FX-SWING) *forex-open-swings*)))) 'short)

                    (cond ((eql time-frame 'POSITION)
                           (write-xml-record tdate "tblTradeRecs" 'OPENP 'SHORT date1 nil stop-short cover-short output1))
                          ((eql time-frame 'SWING)
                           (write-xml-record tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil stop-short cover-short output1))
                          ((eql time-frame 'FX-SWING)
                           (write-xml-record tdate "tblTradeRecs" 'OPENFX 'SHORT date1 nil stop-short cover-short output1)))

                   ; (write-xml-record-ninja tdate "tblTradeRecs" 'OPEN 'SHORT date1 nil entry-long cover-short output2)

                   ; (write-oec-record "True" sector "Buy" oec-symbol "Stop" "GTC" stop-short entry-time "Default" ) ;;; exit open position with stop loss
                   ; (write-oec-record "True" sector "Sell" oec-symbol "Limit" "GTC" cover-long entry-time "Default") ;;;exit open position with objective
                   ; (when (member *data-name* *select-list*)
                   ;  (write-oec-record "True" 'EPSELECT "Buy" oec-symbol "Stop" "GTC" stop-short entry-time "Default" )
                   ;  (write-oec-record "True" 'EPSELECT "Sell" oec-symbol "Limit" "GTC" cover-long entry-time "Default"))
                    )


                  )

      );;;closes the when

    (values P&L plt new-P&L new-plt (mapcar #'(lambda (s) (* s (or (index-point-value) 1))) trades))
      ))



;;;enters with stop orders and exits with stop loss orders and includes objectives
;;;;also trails the stop loss based on slippage
(defun volatility-test7 (date2 num &optional (param1 2.5) (param2 3.0)(dur 7) (output T))
 (let (date stop-long stop-short trades long short ave4  trade-long  entry-short entry-long
       cover-long cover-short ave-win ave-loss losers winners extended-trades trade
       risk risk-long risk-short  (open-risk 0) date-1 ;prices times (filt *n-filt*) (num-days (* 4 dur))
       ;trend-signal-short15 trend-signal-long15
       ignore       (path1 "/home/register/cycles/trend.dat"))
     (declare (special  RS VT  TS5 TS15 AT adj))
     (declare (ignore ignore))

   (setq date (add-mkt-days date2 (- num)))
   (if  (probe-file path1)
        (delete-file path1))
 ;;;;from date1 to date2
 (dotimes (ith num)


    (cond ((gethash  date  AT))
          (t (setf (gethash date AT) (ave date (round (+ adj (* 1 dur))) 'pivot))))

;    (cond ((gethash date TS15))
;          (t (setf (gethash date TS15)(trend-signal date (case dur (3 15)(4 15)(7 45))))))

;    (cond ((gethash date TS5))
;          (t (setf (gethash date TS5)(trend-signal date (case dur (3 5)(4 5)(7 15))))))


   (cond ((gethash date VT))
         (t (setf (gethash date VT) (volatility-log date (* dur 7) 1))));;;volatility over the past three months


   (multiple-value-setq (entry-short entry-long)
        (vprices date (* 5 dur) param1 dur))

   (setq entry-short (min entry-short (getd date 'close)) entry-long (max entry-long (getd date 'close)))

   (setq ave4 (gethash date AT)
       cover-short  (exp (- (log ave4) (* param2 (gethash date VT))))
       cover-long (exp (+ (log ave4) (* param2 (gethash date VT))))
                             )


   (setq risk-short  (* *stop-loss-position* (abs (- entry-short (n-day-high date dur 'close)))
                          ))

   (setq risk-long  (* *stop-loss-position* (abs (- entry-long  (n-day-low date dur 'close)))
                       ))


   (setq risk (max risk-long risk-short));;;this is the risk for a new position not an existing position

;   (multiple-value-setq (trend-signal-short15 trend-signal-long15)
;       (if (<= dur 4)(swing-trend-signal15 *data-name*)(position-trend-signal45 *data-name*)))

   ;(format T "date= ~A param1= ~A Risk-short= ~A Risk-long=~A ~%" date param1 risk-short risk-long)

   (setq  date-1 date date (add-mkt-days date 1))


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



   (when long (setq stop-long
                 (fmax stop-long entry-short
                   ;(+ (- long (/ (if (= dur 3) *max-swing-risk* *max-position-risk*) (index-point-value)))
                   ;   (* .7 (- (getd date-1 'close) long)))
                 )))

   (when short (setq stop-short
                 (fmin stop-short entry-long
                  ;(- (+ short (/ (if (= dur 3) *max-swing-risk* *max-position-risk*) (index-point-value)))
                  ;    (* .7 (- short (getd date-1 'close))))
                  )))

    (if long (setq open-risk (abs (- (getd date-1 'close) stop-long))))
    (if short (setq open-risk (abs (- stop-short (getd date-1 'close)))))

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


  ;;;check if met exit criteria  next at next day's open
     (when (and long (or (<= open-risk 0)
                         (<= (/ (- cover-long (getd date-1 'close)) open-risk) *min-reward-risk-ratio-position*)))

            (push (- (getd date 'open) long) trades)
            (setq trade-long (append trade-long (list date 'exit (getd date 'open) (- (getd date 'open) long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))

      (when (and short (or (<= open-risk 0)
                       (<= (/ (- (getd date-1 'close) cover-short) open-risk) *min-reward-risk-ratio-position*)))

            (push (- short (getd date 'open)) trades)
            (setq trade (append trade (list date 'exit (getd date 'open) (- short (getd date 'open)))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))
#|
  ;;;check if met second exit criteria  next at next day's open
     (when (and long (>= (gethash date-1 RS) 5)
                 (>= (getd date-1 'close) ave8))

            (push (- (getd date 'open) long) trades)
            (setq trade-long (append trade-long (list date 'exit (getd date 'open) (- (getd date 'open) long))))
            (push trade-long extended-trades)
            (setq trade-long nil long nil stop-long nil))

      (when (and short (>= (gethash date-1 RS) 5)
                 (<= (getd date-1 'close) ave8))
            (push (- short (getd date 'open)) trades)
            (setq trade (append trade (list date 'exit (getd date 'open) (- short (getd date 'open)))))
            (push trade extended-trades)
            (setq trade nil short nil stop-short nil))
|#

;;;check if new entry

    (when (and (not short)
               (<= (getd date 'low) entry-short)
             ;  (member (gethash date-1 TS15) trend-signal-short15)
             ;  (member (gethash date-1 TS5) '(DN))
               (if (<= dur 4)
                   (<= (* risk (index-point-value)) *max-swing-risk*)
                  (<= (* risk (index-point-value)) *max-position-risk*))
              (plusp risk)(>= (/ (- entry-short cover-short) risk) *reward-risk-ratio-swing*)
               )
          (setq short (min entry-short (getd date 'open))
                 trade (list date 'short short)
                 stop-short
                  (+ entry-short risk-short)

         ))


    (when (and (not long)
               (>= (getd date 'high) entry-long)
             ;  (member (gethash date-1 TS15) trend-signal-long15)
             ;  (member (gethash date-1 TS5) '(UP))
               (if (<= dur 4)
                   (<= (* risk (index-point-value)) *max-swing-risk*)
                 (<= (* risk (index-point-value)) *max-position-risk*))
               (plusp risk)(>= (/ (- cover-long entry-long) risk) *reward-risk-ratio-swing*)
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


   (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)

    (dolist (ith extended-trades)

    (format stream "~A\,~A\,~A\,~A\,~A\,~A\,~A\,~A~%"
         (first ith)(second ith)(third ith)(nth 3 ith)(nth 4 ith)(nth 5 ith)(nth 6 ith)(round (* (index-point-value) (nth 6 ith))))
    ));;closes the dolist and with-open-file
   ); closes the when

    (values (round (* (list-sum trades) (or (index-point-value) 1)))
            (length trades) trades)
   ));;;closes the let and the defun


;;;;;;based on a list of markets that have been researched
;;;;checks to see what trend-signal codes work best for swing trades and fx-swing
(defun swing-trend-signal15 ( market)
 (let (short long)

   (if (member market '(sp.d1b ))
        (setq short '(CU) long '(CD)))

   (if (member market '(eurgbp.d3b ))
        (setq short '(CD) long '(CU)))

   (if (member market '(nd.d1b nk.d1b cc.d1b))
         (setq short '(DN) long '(UP)))

   (if (member market '(lh.d1b can.d3b jpy.d3b))
          (setq short '(UP) long '(DN)))

   (if (member market '(dj.d1b us.d1b cf.d1b))
       (setq short '(DN CU) long '(UP CD)))

   (if (member market '(ru.d1b su.d1b lc.d1b))
         (setq short '(DN CD) long '(UP CU)))

   (if (member market '(ty.d1b e1.d1b cp.d1b))
         (setq short '(UP CU) long '(DN CD)))

   (if (member market '(bp.d1b sf.d1b s.d1b bo.d1b))
        (setq short '(CD UP) long '(CU DN)))

   (if (member market '(gc.d1b jy.d1b))
        (setq short '(CD CU) long '(CU CD)))

   (if (member market '(us.d1b cd.d1b))
         (setq short '(UP DN CU) long '(DN UP CD)))

   (if (member market '(si.d1b ho.d1b hu.d1b c.d1b chf.d3b))
        (setq short '(CD DN CU) long '(CU UP CD)))

   (if (member market '(pl.d1b ct.d1b oj.d1b sm.d1b gbp.d3b eur.d3b eurjpy.d3b))
        (setq short '(CD UP CU) long '(CU DN CD)))

   (if (member market '(cl.d1b))
        (setq short '(CD UP DN) long '(CU DN UP)))

   (if (member market '(ad.d1b aud.d3b gbpjpy.d3b))
        (setq short '(CD UP DN CU) long '(CD DN UP CU)))


 (values short long)
 ))

;;;;;;based on a list of markets that have been researched
;;;;checks to see what trend-signal codes work best for swing trades
(defun position-trend-signal45 ( market)
 (let (short long)

   (if (member market '(ru.d1b))
         (setq short '(CD) long '(CU)))

   (if (member market '(cc.d1b))
         (setq short '(CU) long '(CD)))

   (if (member market '(dj.d1b sp.d1b))
       (setq short '(DN) long '(UP)))

   (if (member market '(nd.d1b nk.d1b cl.d1b ho.d1b hu.d1b ng.d1b))
        (setq short '(CD DN) long '(CU UP)))

   (if (member market '(gc.d1b si.d1b cp.d1b pl.d1b pa.d1b bp.d1b e1.d1b sf.d1b oj.d1b bo.d1b))
         (setq short '(CD UP) long '(CU DN)))

   (if (member market '(jy.d1b))
        (setq short '(CU CD) long '(CD CU)))

   (if (member market '(mx.d1b cf.d1b))
        (setq short '(UP DN) long '(DN UP)))

   (if (member market '(c.d1b lc.d1b))
        (setq short '(CU UP) long '(CD DN)))

   (if (member market '(ty.d1b sm.d1b))
         (setq short '(CU UP DN) long '(CD DN UP)))

   (if (member market '(ad.d1b ))
        (setq short '(CD UP DN) long '(CU DN UP)))

   (if (member market '(ct.d1b s.d1b lh.d1b))
        (setq short '(CU CD UP) long '(CD CU DN)))

   (if (member market '(su.d1b))
        (setq short '(CU CD DN) long '(CD CU UP)))


   (if (member market '(us.d1b w.d1b))
         (setq short '(CU CD UP DN) long '(CD CU DN UP)))

 (values short long)
 ))

;;;;;;based on a list of markets that have been researched
;;;;checks to see what trend-signal codes work best for swing trades
(defun swing-trend-signal5 ( market)
 (let (short long)

   (if (member market '(cd.d1b ))
         (setq short '(CU) long '(CD)))

   (if (member market '(nd.d1b ))
       (setq short '(UP) long '(DN)))

   (if (member market '(oj.d1b))
        (setq short '(CD DN) long '(CU UP)))

   (if (member market '(sp.d1b ))
         (setq short '(CD UP) long '(CU DN)))

   (if (member market '(e1.d1b ))
        (setq short '(CU CD) long '(CD CU)))

   (if (member market '(dj.d1b lh.d1b))
        (setq short '(UP DN) long '(DN UP)))

   (if (member market '(sm.d1b lc.d1b ))
        (setq short '(CU UP) long '(CD DN)))

   (if (member market '(ru.d1b ))
        (setq short '(CU DN) long '(CD UP)))

   (if (member market '(gc.d1b bp.d1b ho.d1b))
         (setq short '(CU UP DN) long '(CD DN UP)))

   (if (member market '(ad.d1b jy.d1b cl.d1b c.d1b))
        (setq short '(CD UP DN) long '(CU DN UP)))

   (if (member market '(us.d1b ct.d1b))
        (setq short '(CU CD UP) long '(CD CU DN)))

   (if (member market '(nk.d1b cf.d1b))
        (setq short '(CU CD DN) long '(CD CU UP)))

   (if (member market '(ty.d1b si.d1b cp.d1b pl.d1b sf.d1b hu.d1b cc.d1b su.d1b s.d1b bo.d1b))
         (setq short '(CU CD UP DN) long '(CD CU DN UP)))

 (values short long)
 ))



;;; -*- Mode: LISP; Package: common-lisp-user; Base: 10. -*-

#+:SBCL (in-package :common-lisp-user)

;;;;;;;;


;;;time frames is a list of time-frames
;;;markets-ll is a list of lists of markets
;;;example '(*day-list* *swing-list* *position-list*)
;;;each list corresponds to the time frame
;;;this allow you to specify a different market list for each time frame

(defun portfolio-simulation3 (time-frames date num  markets-ll &optional
                             (outdirectory2  *test-output-upper-dir* ))
  (declare (special markets-11))
  (let (sim-path sum-path)
   (maind-x)(set-cat-list)
   (do* ((mkts markets-ll (cdr mkts))
         (tmf time-frames (cdr tmf))
         (time-frame (car tmf) (car tmf))
         (markets (car mkts) (car mkts)))
       ((null tmf))

       (dolist (ith markets)
         (case time-frame
          ;  (position (position-simulation-test3 ith date num))
            (swing (swingtrade-simulation-test ith date num *swing-features*))
            (swing2 ;(build-swingtrade-warehouse markets)
                    ;(apply #'swing-trade-binsb nil *swing-features*) 
                    (swingtrade-simulation-test2 ith date num )) ;*swing-features*))
            (swing3 (swingtrade-simulation-test3 ith date num )) ;*swing-features*))
            (qswing (qswingtrade-simulation-test ith date num  *qswing-features*))
          ;  (dubaiswing (dubaiswingtrade-simulation-test ith date num  *dubaiswing-features*))
           ; (currencies (currencies-simulation-test3 ith date num *currencies-features3*))
           ; (meats (meats-simulation-test2 ith date num *meats-features2m*))
            (day (daytrade-simulation-test3 ith date num nil))
            (epic (daytrade-simulation-test2 ith date num *day-features2*))
            (epicc (currency-simulation-test2 ith date num *day-features2c*))
            (dubai (dubaitrade-simulation-test2 ith date num *dubai-features2*))
            (equity (equitytrade-simulation-test2 ith date num *equity-features2*))
            (retail (daytrade-simulation-test2r ith date num *day-features2*))
            (forex (forex-simulation-test2 ith date num *forex-features2*))
            (charger (daytrade-simulation-test31 ith date num ))
            (day4 (daytrade-simulation-test4 ith date num *day-features4*))
            (day4x (daytrade-simulation-test4x ith date num '(1 5 10 2)))
            (day4s (daytrade-simulation-test4s ith date num *day-features4s*))
           ; (contra (contratrade-simulation-test ith date num *contra-features*))
           ; (exitpoints (exitpoints-simulation-test ith date num *exitpoints-features*))
           ; (dubai5 (dubaitrade-simulation-test5 ith date num *dubai-features5*))
		  )
     ));;closes the dolist and do


   (setq sum-path (apply #'diary-composite3 time-frames outdirectory2 T markets-ll))
   (setq sim-path (apply #'simulation-composite3 time-frames markets-ll))
     
    (exit-statistics sim-path sum-path)
    (exit-statistics-by-year sim-path sum-path)    
))

;;;takes multiple markets and combines their diary files
;;;provides a list of ALL dates
;;;opt T means to calculate the optimal-f and write the optimal-f file; nil means do not 
(defun diary-composite3 (time-frames outdirectory2 opt  &rest markets-ll)
 (let (paths  time-frame records record1 (running-sum 0)
      (path-out nil) path-out1 path-out-dates  mkts type (num-trades 0)
      )
   (declare (special num-trades))
   (maind-x)(set-cat-list)

   (do* ((mkts markets-ll (cdr mkts))
         (tmf time-frames (cdr tmf))
         (time-frame (car tmf) (car tmf))
         (markets (car mkts) (car mkts)))
       ((null tmf))
        (cond ((eql time-frame 'epic)(setq type '2 time-frame 'day))
              ((eql time-frame 'epicc)(setq type '2 time-frame 'epicc))
              ((eql time-frame 'meats)(setq type '2 time-frame 'meats))
              ((eql time-frame 'dubai)(setq type '2 time-frame 'dubai))
              ((eql time-frame 'equity)(setq type '2 time-frame 'equity))
              ((eql time-frame 'retail)(setq type '2 time-frame 'day))
              ((eql time-frame 'forex)(setq type '2 time-frame 'forex))
                          
              ((eql time-frame 'day) (setq type '3))
              ((eql time-frame 'charger)(setq type '31 time-frame 'charger))
              ((eql time-frame 'currencies) (setq type '3))
              ((eql time-frame 'day4) (setq type '4 time-frame 'day))
              ((eql time-frame 'day4x)(setq type '4x time-frame 'day))
              ((eql time-frame 'day4s)(setq type '4s time-frame 'day))
              ((eql time-frame 'day5) (setq type 5 time-frame 'day))
              ((eql time-frame 'mini)(setq type 7 time-frame 'day))
              ((eql time-frame 'exitpoints) (setq type 1 ))
              ((eql time-frame 'forexday)(setq type 2))
              ((eql time-frame 'contra)(setq type 1))
              ((eql time-frame 'dubai5)(setq type 5))
              ((eql time-frame 'swing2)(setq type 2))
              ((eql time-frame 'swing3)(setq type 3))
              ((member time-frame '(swing dubaiswing qswing))(setq type 1)))
       (dolist (market markets)
          (push (string-append *output-upper-dir*
            (format nil "~A~(~A~)-diary~A.dat" market time-frame type)) paths));;;closes dolist over markets
            );;closes the do over time-frames

        (setq path-out1 (string-append *output-upper-dir* (format nil "~(~A~)-p&ls~A.dat" (car  time-frames) type))
              path-out-dates (string-append *output-upper-dir* (format nil "~(~A~)-dates~A.dat" (car time-frames) type)))
  ;  (print paths)
   (dolist (ith paths)
     (when (probe-file ith)
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (if (assoc (car record1) records)
            (setf (third (assoc (car record1) records))
                  (+ (abs (third (assoc (car record1) records))) (abs (third record1)))
              (fourth (assoc (car record1) records))
              (+ (fourth (assoc (car record1) records)) (fourth record1))
           ;   (fifth (assoc (car record1) records))
           ;   (+ (fifth (assoc (car record1) records)) (fifth record1))
              )
         (push record1 records)));;closes the if and the do
       ))) ;;closes with-open-file and the dolist over paths

   (setq records (vsort records #'< 'car))  ; (setq records-test records)

   (setq num-trades 0)
   (dolist (ith records)
     (setq num-trades (+ (abs (third ith)) num-trades))
     )

;;;now calculate the running sum

   (dolist (ith records)
     (setf (fifth ith) (+ (fourth ith) running-sum))
     (setq running-sum (fifth ith)))

   (setq time-frames (mapcar #'(lambda (s) (format nil "~A" s)) time-frames))
   (setq time-frame (apply #'string-append time-frames) time-frame (read-from-string time-frame))
   (setq path-out (string-append *output-upper-dir* "diary-" (format nil "~(~A~)-" time-frame) "composite.csv"))

   (if (probe-file path-out)
       (delete-file path-out))
    (if (probe-file path-out1)
       (delete-file path-out1))


   (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith records)
         (dotimes (jth 5)
          (if (eql jth 4)(format stream "~A" (nth jth ith))
             (if (/= jth 1)(format stream "~A\," (nth jth ith)))))
          (format stream "~%")));;;closes the with-open-file

   (with-open-file (stream1 path-out1 :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (ith (reverse records))
          (format stream1 "~A~%" (nth 3 ith))))

   (with-open-file (stream2 path-out-dates :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (ith (reverse records))
          (format stream2 "~A~%" (nth 0 ith))))

   (setq mkts nil)
   (dolist (ith markets-ll)
          (setq mkts (union ith mkts)))

;   (setq records-test records
;         mkts-test mkts
;         path-out1-test path-out1
;         time-frame-test time-frame)

   (summary-gains-losses path-out1 path-out-dates time-frame (caar records) (caar (last records)) mkts outdirectory2 opt)

;   (shell (string-append "unix2dos ~/exitpoints/diary-" (format nil "~(~A~)-" time-frame) "composite.csv "
;          "/home/mk-data/luis/" (format nil "~(~A~)-" time-frame) "diary-composite.csv"))
;   (shell "unix2dos ~/exitpoints/summary.dat /home/mk-data/luis/summary.txt")
;   (shell "unix2dos ~/exitpoints/optimal-f.csv /home/mk-data/luis/optimal-f.csv")
))


;;;;assumes the path is the name of a file with a series of trade gains and losses (one per line)
;;;; the gains and losses are already in $.
;;;; The gains and losses should be most recent first

(defun summary-gains-losses (path path-dates time-frame &optional (date1 0) (date2 0) (markets nil)
           (outdirectory2 *test-output-upper-dir*)(opt T)(jth -1))
 (let (trades (outfile nil) (outfile1 nil) (outfile2 nil) winners losers ave-win ave-loss temp
         zeros-list collection max-inactive-time inactive-time draw flat-time (limit 10000)
        (qty 1)(qty2 1) (qty3 1)(dollar-contract 0)(conser-dollar-contract 0) (date date1)
        (running-sum 0)(running-sum2 0)(running-sum3 0)(ctr 0) ranges largest-possible-loss
         daily-equity-line2 daily-equity-line3 draw-end-date flat-start-date dates draw-50 draw-90 draw-99
         monthly-totals num-months losing-months winning-months best-month worst-month median-month 
         mean-month trades-monthly)

     (dolist (market markets)
       (set-market market)
       (setq date (max (first-available-date market) date1))
       (push (max-true-range (corrected-date date) date2) ranges))
    ; (format T "Date1= ~A Date2= ~A path-dates= ~A" date1 date2 path-dates)
     (setq largest-possible-loss
       (case time-frame
         ((day currencies epic epicc epico equity dubai dubai5 day4 day4x day4s day5 day9 dayepic dayday forex forexday daydayepic dayepicday4 meats charger)
          (min *max-day-risk* (max* ranges)))
         ((swing dubaiswing qswing swing2 swing3) (min *max-swing-risk* (max* ranges)))
         ((position swingposition) (min *max-position-risk* (max* ranges)))
         (t (min *max-day-risk* (max* ranges)))))
   (setq outfile (string-append *output-upper-dir* (format nil "summary-~A" time-frame) ".txt")
         outfile1 (string-append *output-upper-dir* (format nil "optimal-f-~A" time-frame) ".csv")
         outfile2 (string-append outdirectory2 (format nil "analysis.csv")))
  ; (format T "~%1234" )
   (with-open-file (str path-dates :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record dates)))
   (setq dates (reverse dates))

   (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))
  ; (format T "~%234" )
    (setq trades (reverse trades))
    (dolist (ith trades)
         (if (zerop ith) (push ith zeros-list))
         (when (and (not (zerop ith)) zeros-list)
               (push (length zeros-list) collection)(setq zeros-list nil)))
    (if zeros-list (push (length zeros-list) collection))

    (if collection (setq max-inactive-time (apply #'max collection)) (setq max-inactive-time 0))
    (setq inactive-time (count-if #'zerop trades))
 ; (format T "~%345" )

   (multiple-value-setq (draw flat-time draw-end-date flat-start-date) (drawdown1 trades dates))
   (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'zerop (remove-if #'plusp trades)) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )
;  (format T "~%3456" )
      (format str "~A trades from ~A to ~A~%" time-frame (date-convert date1) (date-convert date2))

      (dolist (ith markets)
          (set-market ith)(incf ctr)
         (format str "~A  " (index-lname))
         (if (zerop (mod ctr 3))(terpri str))
         )

     (format str "~%~%P&L= ~16D  " (round (list-sum trades)))
 ; (format T "~%456" )
     (format str "NUMBER DAYS=      ~6A"  (length trades))
     (if (<= (length markets) 1)
        (format str "P&L PER TRADE= ~6D " (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades)))))
        (format str "P&L PER DAY=   ~6D " (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))))
    ;  (setq aa trades)
  ; (format T "~%4567 draw= ~A inactive-time=~A" draw  inactive-time )
     (format str "~%%WINNERS=        ~,1,2,'*,' F% %UNCHANGED=       ~,1,2,'*,' F%  %LOSERS=          ~,1,2,'*,' F%   ~%~
        AVERAGE GAIN=~8D  AVERAGE LOSS=~8D~%~
        PAYOFF RATIO=   ~5,2,0,'*,' F  MAX INACTIVE TIME=~4D   %TIME-IN-MARKET= ~4F~%~
        LARGEST LOSS= ~7D  LARGEST GAIN=  ~7D   PROFIT FACTOR=   ~,2,0,'*,' F~%~
        DRAWDOWN= ~11D  FLAT TIME=        ~4D   MAX-DRAW-END=  ~A~%MAX-FLAT-START= ~A~%" 

       (/ (- (length trades) (length losers)(count 0 trades :test #'=))
          (if (zerop (length trades)) 1 (length trades)))
       (/ (count 0 trades :test #'=) (if (zerop (length trades)) 1 (length trades)))
       (/ (length losers)(if (zerop (length trades)) 1 (length trades)))
 
       (round ave-win)
       (round ave-loss) (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
        max-inactive-time (my-round (- 100 (* 100 (/ inactive-time (length trades)))) 1)
       (round  (or (min* losers) 0))(round (or (max* winners) 0))
       (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                          (abs (if (zerop ave-loss) 1 ave-loss) )))
      (round draw) flat-time (date-convert draw-end-date) (date-convert flat-start-date)
     
     ); closes the format
   ; (format t "~%abc")
     (when opt (format str "1/2F $/contract=~10D"
                      (if (and losers (plusp (list-sum trades)))(round (* 2 (optimal-f trades))) 0))
       (multiple-value-setq (draw-50 draw-90 draw-99)(monte-carlo-drawdown trades)))
     (format str "~%~%SUGGESTED INITIAL $=     ~6D" (* 1000 (ceiling (* 3  (abs draw)) 1000)))
    (ifn (zerop draw)
     (format str "~%AVE ANNUAL RETURN= ~6,1,0,'*,' F%"
          (* 100 (/ (/ (list-sum trades)(/ (subtract-dates date1 date2) 365.25))
                       (* 1000 (ceiling (* 3 (abs draw)) 1000)) ))))
  ; (format T "~%789" )
     );closes with-open-file
    (if (not losers) (setq opt nil))
    (when opt
     (with-open-file (stream outfile1 :direction :output :if-exists :supersede :if-does-not-exist :create)
     

     (dolist (ith (reverse trades))
        (push ith temp)(incf jth)
        (setq qty (min qty limit) qty2 (min qty2 limit) qty3 (min qty3 limit))
        (setq running-sum (round (+ running-sum (* qty ith)))
              running-sum2 (round (+ running-sum2 (* qty2 ith)))
              running-sum3 (round (+ running-sum3 (* qty3 ith))))
 ; (format T "~%890" )
        (format stream "~A\,~D\,~D\,~D\,~D\,~D\,~D\,~D\,~D\,~D~%" (nth jth (reverse dates)) ith conser-dollar-contract dollar-contract
              qty running-sum qty2 running-sum2 qty3 running-sum3)

        (setq dollar-contract (max (if (> (count-if #'(lambda(s) (/= s 0)) temp) 30)(round (optimal-f temp)) 0)
                                   (abs largest-possible-loss))
              conser-dollar-contract (max (if (> (count-if #'(lambda(s) (/= s 0)) temp) 30) 
                                                 (round (abs (drawdown temp))) 0)
                                   (abs largest-possible-loss))
              qty (if (and (plusp running-sum)(plusp dollar-contract))
                      (1+ (floor (/ running-sum (* 2.0 dollar-contract)))) 1)
              qty2 (if (and (plusp running-sum2)(plusp dollar-contract))
                      (1+  (floor (/ running-sum2 (* 3.0 dollar-contract)))) 1)
              qty3 (if (and (plusp running-sum3)(plusp conser-dollar-contract))
                      (1+  (floor (/ running-sum3 7000))) 1))
         (push running-sum2 daily-equity-line2)
         (push running-sum3 daily-equity-line3)
        ));;closes the dolist and with-open-file
       );closes the when opt
      (with-open-file (str outfile :direction :output :if-exists :append :if-does-not-exist :create)
       (when opt
       (format str "~%MAX CONSERVATIVE DRAWDOWN=    ~4,2F%" (drawdown% trades daily-equity-line3 3))
       (format str "~%MAX AGGRESSIVE DRAWDOWN=      ~4,2F%" (drawdown% trades daily-equity-line2 3))
      
       (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A~%99 PERCENTILE =~A~%" draw-50 draw-90 draw-99) 
        );;;closes the when      
        (multiple-value-setq (monthly-totals num-months losing-months winning-months best-month worst-month median-month 
                             mean-month) (monthly-gains-losses time-frame outfile))
   
  ;      (format T "~%012" )
       );;;closes the with-open-file
      (write-analysis-file markets trades winners losers ave-win ave-loss draw  flat-time draw-end-date
                           date1 date2 num-months losing-months winning-months best-month worst-month median-month mean-month
                           trades-monthly outfile2)
      ; daily-equity-changes ;;;P7Ls weighted bythe qty3 with conservative approach
     outfile
))

(defun write-analysis-file1 (markets trades winners losers ave-win ave-loss draw  flat-time draw-start-date
                            date1 date2 num-months losing-months winning-months best-month worst-month median-month mean-month
                            trades-monthly outfile2 )

  (with-open-file (str outfile2 :direction :output :if-exists :append :if-does-not-exist :create)
   (dolist (ith  markets)
     (format str "~A " (cdr (assoc ith *ninja-symbol*))));;;closes the dolist
;;;total p&l
     (format str "~10D  " (round (list-sum trades)))
;;;;first is the average win average loss and payoff ratio
    (format str "~3D ~4D ~,2,0,'*,' F" (round ave-win) (round ave-loss) (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss))))
;;;the average p&L per day 
    (format str " ~4D" (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades)))))
;;;largest single loss and largest single gain
    (format str " ~5D ~5D" (round  (or (min* losers) 0))(round (or (max* winners) 0)))
;;;profit factor
    (format str " ~,2,0,'*,' F" (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                                                   (abs (if (zerop ave-loss) 1 ave-loss)))))
;;;drawdown flat time and drawdown start date
    (format str "~6D ~3D ~A"   (round draw) flat-time (date-convert draw-start-date))
;;;;suggested initial acct size and ave annual return
    (format str " ~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
    (ifn (zerop draw)
    (format str " ~5,1,0,'*,' F"
          (* 100 (/ (/ (list-sum trades)(/ (subtract-dates date1 date2) 365.25))
                       (* 1000 (ceiling (* 3  (abs draw)) 1000))) )))
;;;;# losing months #winning months # months
    (format str "~3D ~3D ~3D ~6D ~6D ~6D ~6D ~3D~%" losing-months winning-months num-months best-month 
                                              worst-month median-month mean-month trades-monthly)

))

#|
;;;assumes the day-trade-bins has been run first
(defun daytrade-chi-squared-gof ()
  (let (contents (result 0) square-list (all-winners 0)(all-losers 0)
       square percentage-winners chi-squared)
;;;first calculate the profit per trade overall trades 
   (dolist (kth daytrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners 1))
           (setq all-losers (+ all-losers 1))))
   
   (setq percentage-winners (/ all-winners (+ all-winners all-losers)))

  (dolist (ith day-bin-codes)
     (setq contents (gethash ith *day-trade-warehouse3*))
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
     (chi-square-cdf chi-squared (1- (length day-bin-codes)))
  
))
|#

;;;assumes the day-trade-bins has been run first
(defun daytrade-chi-squared-gof ()
  (let (contents  square-list (all-winners 0)(all-losers 0) chi-squared-metric chi-squared-probability
       square percentage-winners chi-squared (win-result 0)(lose-result 0)(bin-dollars 0))
;;;first calculate the profit per trade overall trades 
   (dolist (kth daytrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))
   
;;;;this is the percentage of dollars that are winners
   (setq percentage-winners (/  all-winners (+ all-winners (- all-losers))))

  (dolist (ith day-bin-codes)
     (setq contents (gethash ith *day-trade-warehouse3*))
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
           (if (zerop percentage-winners) 0
            (/ (expt (- win-result (* percentage-winners (+ win-result (- lose-result)))) 2)
             (* percentage-winners bin-dollars))))


     (push square square-list));;;closes dolist over day-bin-codes
  (if (<= (length square-list) 1) (setq chi-squared-metric 0)
  (setq chi-squared-metric
    (round  (setq chi-squared (/ (list-sum square-list)(1- (length square-list)))))));;; number of bins less one is the degrees of freedom
   ;  (format T "~%CHI SQUARE = ~7,3F  DOF = ~D~%" chi-squared (1- (length day-bin-codes)))
 (if (or (zerop chi-squared)(<= (length day-bin-codes) 1)) (setq chi-squared-probability 0)
     (setq chi-squared-probability (chi-square-cdf chi-squared (1- (length day-bin-codes)))))
  (values chi-squared-metric chi-squared-probability)
))

(defun max-chi (daytrades)
  (let ((all-winners 0)(all-losers 0)(percentage-winners 0) square square-list)
  ;;;first calculate the profit per trade overall trades 
   (dolist (kth daytrades)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))
;;;;this is the percentage of dollars that are winners
   (setq percentage-winners (/  all-winners (+ all-winners (- all-losers))))

;;;lets assume all trades have their own bin. Complete separability.
  (dolist (ith daytrades)
    (setq square (/ (expt (- (if (plusp (svref ith 19))(svref ith 19) 0)
                       (* percentage-winners (abs (svref ith 19)))) 2)
                     (if (zerop (* percentage-winners (abs (svref ith 19))))
                          1 (* percentage-winners (abs (svref ith 19))))))
    (push square square-list))
   
    (/ (list-sum square-list)(1- (length square-list)))
     
))

;;;assumes the swing-trade-bins has been run first
(defun swingtrade-chi-squared-gof ()
  (let (contents  square-list (all-winners 0)(all-losers 0) chi-squared-metric chi-squared-probability
       square percentage-winners chi-squared (win-result 0)(lose-result 0)(bin-dollars 0))
;;;first calculate the profit per trade overall trades 
   (dolist (kth swings)
       (if (plusp (svref kth 19)) (setq all-winners (+ all-winners (svref kth 19)))
           (setq all-losers (+ all-losers (svref kth 19)))))
   
   (setq percentage-winners (/ all-winners (+ all-winners (- all-losers))))

  (dolist (ith swing-bin-codes)
     (setq contents (gethash ith *swing-trade-warehouse*))
     (setq win-result 0 square 0 lose-result 0 bin-dollars 0)
     (dolist (kth contents)
         (if (plusp (svref kth 19))
             (setq win-result (+ win-result (svref kth 19)))
            (setq lose-result (+ lose-result (svref kth 19))))
         ) ;;;closes dolist over contents
;;;;result is the observed number of winners in the bin
;;;;the expected number of winners in the bin is
   (setq bin-dollars (+ win-result (- lose-result))) 
    (if (zerop bin-dollars)(incf bin-dollars))

    (setq square
         (/ (expt (- win-result (* percentage-winners (+ win-result (- lose-result)))) 2)
             (* percentage-winners bin-dollars)))

     (push square square-list));;;closes dolist over day-bin-codes
;    (print square-list)
     (if (<= (length square-list) 1) (setq chi-squared-metric 0)
     (setq chi-squared-metric;;;number of bins less one is the degrees of freedom
           (round  (setq chi-squared (/ (list-sum square-list)(1- (length square-list)))))))

;     (format T "~%CHI SQUARE = ~7,3F  DOF = ~D~%" chi-squared (1- (length swing-bin-codes)))
      (if (<= (length swing-bin-codes) 1) (setq chi-squared-probability 0)
     (setq chi-squared-probability (chi-square-cdf chi-squared (1- (length swing-bin-codes)))))
     (values chi-squared-metric chi-squared-probability)

))



;;;reads a diary file
(defun monthly-gains-losses (time-frame outfile)
 (let (daytrades date gain-loss monthly-totals bins path-in (num-trades 0) (total-num-trades 0)
      num-months losing-months winning-months best-month worst-month median-month mean-month)
 
;   (setq time-frames (mapcar #'(lambda (s) (format nil "~A" s)) time-frames))
;   (setq time-frame (apply #'string-append time-frames) time-frame (read-from-string time-frame))
    (setq path-in (string-append *output-upper-dir* "diary-" (format nil "~(~A~)-" time-frame) "composite.csv"))
    (with-open-file (str path-in :direction :input)

      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
          (push record daytrades)))
  
    (dolist (record daytrades)
 
         (setq date (truncate (read-from-string (n-field-csv record 0)) 100)
              num-trades (abs (read-from-string (n-field-csv record 1)))
              total-num-trades (+ total-num-trades num-trades)
              gain-loss (read-from-string (n-field-csv record 2)))
          (if (not (assoc date bins))
              (setq bins (acons date (list gain-loss) bins))
             (setf (cdr (assoc date bins)) (cons gain-loss (cdr (assoc date bins)))))
        )
      (dolist (ith bins)
        (push (apply #'+ (cdr ith)) monthly-totals))

   (with-open-file (str outfile :direction :output :if-exists :append :if-does-not-exist :create)
     (format str "~%~%NUM LOSING MONTHS = ~A" (setq losing-months (count-if #'minusp monthly-totals)))
     (format str "~%NUM WINNING MONTHS = ~A" (setq winning-months (count-if #'plusp monthly-totals)))
     (format str "~%TOTAL NUMBER OF MONTHS = ~A" (setq num-months (length monthly-totals)))

     (format str "~%~%BEST MONTH GAIN = ~A" (setq best-month (apply #'max monthly-totals)))
     (format str "~%WORST MONTH LOST = ~A" (setq worst-month (apply #'min monthly-totals)))
     (format str "~%MONTHLY MEDIAN P&L = ~A" (setq median-month (percentile2 50 monthly-totals)))

     (format str "~%~%MONTHLY MEAN P&L = ~A" (setq mean-month (round (/ (list-sum monthly-totals) (length monthly-totals)))))
     
    )
   
   (with-open-file (str (string-append *output-upper-dir* "monthly-totals-" (format nil "~(~A~)" time-frame) ".txt")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (ith bins)
       (format str "~A ~A~%" (car ith) (apply #'+ (cdr ith))))
    )

(values monthly-totals num-months losing-months winning-months best-month worst-month median-month mean-month ) 
 ))

;;;I think this was for Pat Johson and her request for yearly backtest results.

(defun portfolio-study (time-frames num &optional
                       (outdirectory2  *test-output-upper-dir*) (full-mkt-list *day-list*))
  (let (all-subsets)
     (dolist (ith (directory (string-append outdirectory2 "*.csv")))
             (delete-file ith))
      (with-open-file (str (string-append outdirectory2 "analysis.csv") :direction :output 
                       :if-exists :supersede :if-does-not-exist :create)   
  ;;;write out headings for the analysis file
        (format str "MARKETS,$TOTAL,AVE WIN,AVE LOSS,PAYOFF,P&L PER DAY,LARGEST LOSS,LARGEST GAIN,~
                     PROFIT FACTOR,DRAWDOWN,FLAT TIME,DRAW START,ACCT SIZE,%RETURN,#LOSING MONTHS,~
                     #WINNING MONTHS,#MONTHS,BEST MONTH,WORST MONTH,MEDIAN MONTH,MEAN MONTH,#TRADES MONTHLY~%")
          )       
       
      (setq all-subsets (subsets1 num full-mkt-list))
;      (format T "~A" all-subsets)
     (dolist (ith all-subsets)
         (apply #'diary-composite time-frames outdirectory2 (list ith)))

))



;;;;combines the trade files from a portfolio simulation
(defun simulation-composite3 (time-frames &rest markets-ll)
 (let (paths  time-frame records record1 (path-out nil) type path1)

;   (maind-x)(set-cat-list)

   (do* ((mkts markets-ll (cdr mkts))
         (tmf time-frames (cdr tmf))
         (time-frame (car tmf) (car tmf))
         (markets (car mkts) (car mkts)))
       ((null tmf))
        (cond ((member time-frame '(epic forexday retail))(setq type 2 time-frame 'day))
              ((eql time-frame 'epicc) (setq type '2 time-frame 'epicc))
              ((eql time-frame 'meats) (setq type '2 time-frame 'meats))
              ((eql time-frame 'dubai) (setq type '2))
              ((eql time-frame 'equity) (setq type '2))
              ((eql time-frame 'forex) (setq type '2))
              ((eql time-frame 'day)(setq type '3))
              ((eql time-frame 'charger)(setq type '31))
              ((eql time-frame 'currencies)(setq type '3 ))
              ((eql time-frame 'day4)(setq type 4 time-frame 'day))
              ((eql time-frame 'day4x)(setq type '4x time-frame 'day))
              ((eql time-frame 'day4s)(setq type '4s time-frame 'day))
              ((eql time-frame 'day5)(setq type 5 time-frame 'day))
              ((eql time-frame 'exitpoints)(setq type 1))
              ((eql time-frame 'swing)(setq type 1))
              ((eql time-frame 'swing2)(setq type 2))
              ((eql time-frame 'swing3)(setq type 3))
              ((eql time-frame 'qswing)(setq type 1))
              ((eql time-frame 'dubaiswing)(setq type 1))
              ((eql time-frame 'contra)(setq type 1))
              ((eql time-frame 'dubai5)(setq type 5))
               )
       (dolist (market markets)
          (push (string-append *output-upper-dir*
            (format nil "~A~(~A~)-simulation~A.dat" market time-frame type)) paths));;;closes dolist over markets
            );;closes the do over time-frames

  ; (print paths)
   (dolist (ith paths)
     (when (probe-file ith)
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
 ;       (if (assoc (car record1) records)
 ;           (setf (third (assoc (car record1) records))
 ;                 (+ (abs (third (assoc (car record1) records))) (abs (third record1)))
 ;             (fourth (assoc (car record1) records))
 ;             (+ (fourth (assoc (car record1) records)) (fourth record1))
           ;   (fifth (assoc (car record1) records))
           ;   (+ (fifth (assoc (car record1) records)) (fifth record1))
 ;             )
         (push record1 records));;closes the do
       ))) ;;closes with-open-file and the dolist over paths

   (setq records (vsort records #'< 'car))

   (setq time-frames (mapcar #'(lambda (s) (format nil "~A" s)) time-frames))
   (setq time-frame (apply #'string-append time-frames) time-frame (read-from-string time-frame))
   (setq path-out (string-append *output-upper-dir* "simulation-" (format nil "~(~A~)-" time-frame) "composite.csv"))

   (if (probe-file path-out)
       (delete-file path-out))

   (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith records)
         (dotimes (jth (length ith))
          (if (= jth (1- (length ith)))(format stream "~A~%" (nth jth ith))
             (format stream "~A\," (nth jth ith))));;closes the dotimes
        ));;;closes the with-open-file
   (setq path1 (string-append *output-upper-dir* "summary-" (format nil "~A.txt" time-frame)))
  (with-open-file (str path1 :direction :output :if-exists :append :if-does-not-exist :create)
    (format str "~%#TOTAL TRADES= ~D  STOPPED OUT = ~5,2F%  OBJECTIVE OUT = ~5,2F% CRITERIA OUT = ~5,2F% SAME DAY = ~5,2F%~%"
     (length records)
     (my-round (* 100 (/ (count 'S (mapcar #'(lambda(s1) (nth 6 s1)) records)) (max (length records) 1))) 1)
     (my-round (* 100 (/ (count 'O (mapcar #'(lambda(s1) (nth 6 s1)) records)) (max (length records) 1))) 1)
     (my-round (* 100 (/ (count 'C (mapcar #'(lambda(s1) (nth 6 s1)) records)) (max (length records) 1))) 1)
     (my-round (* 100 (/ (count 'SS (mapcar #'(lambda(s1) (nth 6 s1)) records)) (max (length records) 1))) 1)
    )
     (format str "Largest single winning trade = ~D   Largest single losing trade = ~D~2%"
        (apply #'max (mapcar #'(lambda(s1) (nth 9 s1)) records))
        (apply #'min (mapcar #'(lambda(s1) (nth 9 s1)) records))
      ))

path-out
)) ;;closes the let and defun




  
(defun factorial (n)
   (do ((j n (- j 1))
        (f 1 (* j f)))
      ((= j 0) f)))

;;;this is the number of combinations of n items taken x at a time.
(defun combinations (x n)
  (/ (factorial n) (* (factorial x)(factorial (- n x)))))


(defun subsets (set)
  "Return a list of all subsets of the given set (represented as a list)"
  (let ((first (first set)) (rest (rest set)))
    (if rest
        (let ((others (subsets rest)))
          (nconc others
                 (mapcar (lambda (subset)
                           (cons first subset))
                         others)))
        (list nil (list first)))))


(defun subsets1 (num set &aux num-subsets)
  "Return a list of all subsets of length num of the given set (represented as a list)"
  (setq num-subsets
  (let ((first (first set)) (rest (rest set)))
    (if rest
        (let ((others (subsets rest)))
          (nconc others
                 (mapcar (lambda (subset)
                           (cons first subset))
                         others)))
        (list nil (list first)))))
   (delete-if #'(lambda (s) (/= (length s) num)) num-subsets) 
)

;;;takes multiple markets and combines their diary files
;;;provides a list of ALL dates
(defun diary-composite (time-frames outdirectory2 &rest markets-ll)
 (let (paths  time-frame records record1 (running-sum 0)
      (path-out nil) path-out1 path-out-dates  mkts type (num-trades 0)
      )
   (declare (special num-trades))
   (maind-x)(set-cat-list)

   (do* ((mkts markets-ll (cdr mkts))
         (tmf time-frames (cdr tmf))
         (time-frame (car tmf) (car tmf))
         (markets (car mkts) (car mkts)))
       ((null tmf))
        (cond ((eql time-frame 'epic)(setq type 2 time-frame 'day))
               ((eql time-frame 'epicc)(setq type 2 time-frame 'epicc))
              ((eql time-frame 'meats)(setq type 2 time-frame 'meats))
              ((eql time-frame 'dubai)(setq type 2 time-frame 'day))
              ((eql time-frame 'equity)(setq type 2 time-frame 'day))
              ((eql time-frame 'forex)(setq type 2 time-frame 'day))
              ((eql time-frame 'day) (setq type 3))
              ((eql time-frame 'charger) (setq type 31))
              ((eql time-frame 'currencies) (setq type 3))
              ((eql time-frame 'day4) (setq type 4 time-frame 'day))
              ((eql time-frame 'day4x)(setq type '4x time-frame 'day))
              ((eql time-frame 'day5) (setq type 5 time-frame 'day))
              ((eql time-frame 'mini)(setq type 7 time-frame 'day))
              ((eql time-frame 'day9) (setq type 9 time-frame 'day))
              ((eql time-frame 'forexday)(setq type 2))
              ((eql time-frame 'contra)(setq type 1))
              ((eql time-frame 'swing3)(setq type 3))
              ((eql time-frame 'swing2)(setq type 2))
              ((member time-frame '(swing qswing))(setq type 1)))
       (dolist (market markets)
          (push (string-append *output-upper-dir*
            (format nil "~A~(~A~)-diary~A.dat" market time-frame type)) paths));;;closes dolist over markets
            );;closes the do over time-frames

        (setq path-out1 (string-append *output-upper-dir* (format nil "~(~A~)-p&ls~A.dat" (car  time-frames) type))
              path-out-dates (string-append *output-upper-dir* (format nil "~(~A~)-dates~A.dat" (car time-frames) type)))
    (print paths)
   (dolist (ith paths)
     (when (probe-file ith)
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (if (assoc (car record1) records)
            (setf (third (assoc (car record1) records))
                  (+ (abs (third (assoc (car record1) records))) (abs (third record1)))
              (fourth (assoc (car record1) records))
              (+ (fourth (assoc (car record1) records)) (fourth record1))
           ;   (fifth (assoc (car record1) records))
           ;   (+ (fifth (assoc (car record1) records)) (fifth record1))
              )
         (push record1 records)));;closes the if and the do
       ))) ;;closes with-open-file and the dolist over paths

   (setq records (vsort records #'< 'car))

   (setq num-trades 0)
   (dolist (ith records)
     (setq num-trades (+ (abs (third ith)) num-trades))
     )

;;;now calculate the running sum

   (dolist (ith records)
     (setf (fifth ith) (+ (fourth ith) running-sum))
     (setq running-sum (fifth ith)))

   (setq time-frames (mapcar #'(lambda (s) (format nil "~A" s)) time-frames))
   (setq time-frame (apply #'string-append time-frames) time-frame (read-from-string time-frame))
   (setq path-out (string-append *output-upper-dir* "diary-" (format nil "~(~A~)-" time-frame) "composite.csv"))

   (if (probe-file path-out)
       (delete-file path-out))
    (if (probe-file path-out1)
       (delete-file path-out1))


   (with-open-file (stream path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
       (dolist (ith records)
         (dotimes (jth 5)
          (if (eql jth 4)(format stream "~A" (nth jth ith))
             (if (/= jth 1)(format stream "~A\," (nth jth ith)))))
          (format stream "~%")));;;closes the with-open-file

   (with-open-file (stream1 path-out1 :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (ith (reverse records))
          (format stream1 "~A~%" (nth 3 ith))))

   (with-open-file (stream2 path-out-dates :direction :output :if-exists :supersede :if-does-not-exist :create)
        (dolist (ith (reverse records))
          (format stream2 "~A~%" (nth 0 ith))))

   (setq mkts nil)
   (dolist (ith markets-ll)
          (setq mkts (union ith mkts)))

;   (setq records-test records
;         mkts-test mkts
;         path-out1-test path-out1
;         time-frame-test time-frame)

   (summary-gains-losses2 path-out1 path-out-dates time-frame (caar records) (caar (last records)) mkts outdirectory2)

;   (shell (string-append "unix2dos ~/exitpoints/diary-" (format nil "~(~A~)-" time-frame) "composite.csv "
;          "/home/mk-data/luis/" (format nil "~(~A~)-" time-frame) "diary-composite.csv"))
;   (shell "unix2dos ~/exitpoints/summary.dat /home/mk-data/luis/summary.txt")
;   (shell "unix2dos ~/exitpoints/optimal-f.csv /home/mk-data/luis/optimal-f.csv")
))






(defun write-analysis-file (markets trades winners losers ave-win ave-loss draw  flat-time draw-start-date
                            date1 date2 num-months losing-months winning-months best-month worst-month median-month mean-month
                            trades-monthly outfile2 )

  (with-open-file (str outfile2 :direction :output :if-exists :append :if-does-not-exist :create)
   (dolist (ith  markets)
     (format str "~A " (cdr (assoc ith *ninja-symbol*))));;;closes the dolist
;;;total p&l
     (format str ",~10D  " (round (list-sum trades)))
;;;;first is the average win average loss and payoff ratio
    (format str ",~3D,~4D,~,2,0,'*,' F" (round ave-win) (round ave-loss) (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss))))
;;;the average p&L per day 
    (format str ",~4D" (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades)))))
;;;largest single loss and largest single gain
    (format str ",~5D,~5D" (round  (or (min* losers) 0))(round (or (max* winners) 0)))
;;;profit factor
    (format str ",~,2,0,'*,' F" (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
                                                                   (abs (if (zerop ave-loss) 1 ave-loss)))))
;;;drawdown flat time and drawdown start date
    (format str ",~6D,~3D,~A"   (round draw) flat-time (date-convert draw-start-date))
;;;;suggested initial acct size and ave annual return
    (format str ",~5D" (* 1000 (ceiling (* 3 (abs draw)) 1000)))
    (ifn (zerop draw)
    (format str ",~5,1,0,'*,' F"
          (* 100 (/ (/ (list-sum trades)(/ (subtract-dates date1 date2) 365.25))
                       (* 1000 (ceiling (* 3  (abs draw)) 1000))) )))
;;;;# losing months #winning months # months
    (format str ",~3D,~3D,~3D,~6D,~6D,~6D,~6D,~3D~%" losing-months winning-months num-months best-month 
                                              worst-month median-month mean-month trades-monthly)

))

;;;;assumes the path is the name of a file with a series of trade gains and losses (one per line)
;;;; the gains and losses are already in $.
;;;; The gains and losses should be most recent first
;;;This version is for the portfolio-study
(defun summary-gains-losses2 (path path-dates time-frame &optional (date1 0) (date2 0) (markets nil)
           (outdirectory2 *test-output-upper-dir*))
 (let (trades  (outfile2 nil) winners losers ave-win ave-loss 
         zeros-list collection max-inactive-time inactive-time draw flat-time 
       ; (qty 1)(qty2 1) (qty3 1)(dollar-contract 0)(conser-dollar-contract 0) 
       ; (running-sum 0)(running-sum2 0)(running-sum3 0)(ctr 0) 
         ranges largest-possible-loss
          draw-start-date dates draw-50 draw-90 draw-99
         monthly-totals num-months losing-months winning-months best-month worst-month median-month 
         mean-month trades-monthly)

     (dolist (market markets)
       (set-market market)
       (push (max-true-range date1 date2) ranges))

     (setq largest-possible-loss
       (case time-frame
         ((day epic charger day4 day4x day5 day9 dayepic dayday forex forexday daydayepic dayepicday4)
           (min *max-day-risk* (max* ranges)))
         ((swing swing2 swing3) (min *max-swing-risk* (max* ranges)))
         (qswing (min *max-swing-risk* (max* ranges)))
         ((position swingposition) (min *max-position-risk* (max* ranges)))
         (t (min *max-day-risk* (max* ranges)))))
   (setq ;outfile (string-append *output-upper-dir* (format nil "summary-~A" time-frame) ".txt")
         ;outfile1 (string-append *output-upper-dir* (format nil "optimal-f-~A" time-frame) ".csv")
         outfile2 (string-append outdirectory2 (format nil "analysis.csv")))
 
   (with-open-file (str path-dates :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record dates)))
   (setq dates (reverse dates))

   (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))
       (push record trades)))

    (setq trades (reverse trades))
    (dolist (ith trades)
         (if (zerop ith) (push ith zeros-list))
         (when (and (not (zerop ith)) zeros-list)
               (push (length zeros-list) collection)(setq zeros-list nil)))
    (if zeros-list (push (length zeros-list) collection))

    (if collection (setq max-inactive-time (apply #'max collection)) (setq max-inactive-time 0))
    (setq inactive-time (count-if #'zerop trades))


   (multiple-value-setq (draw flat-time draw-start-date) (drawdown1 trades dates))
 ;  (with-open-file (str outfile :direction :output :if-exists :supersede :if-does-not-exist :create)
   (setq losers (remove-if #'zerop (remove-if #'plusp trades)) winners (remove-if #'zerop (remove-if #'minusp trades))
         ave-win   (/  (list-sum winners)  (if (zerop (length winners)) 1 (length winners)))
         ave-loss  (/ (list-sum losers) (if (zerop (length losers)) 1 (length losers)))
          )

  ;    (format str "~A trades from ~A to ~A~%" time-frame (date-convert date1) (date-convert date2))

  ;    (dolist (ith markets)
  ;        (set-market ith)(incf ctr)
  ;       (format str "~A  " (index-lname))
  ;       (if (zerop (mod ctr 3))(terpri str))
  ;       )

  ;   (format str "~%~%P&L= ~16D  " (round (list-sum trades)))

   ;  (format str "NUMBER DAYS=      ~6A"  (length trades))
   ;  (if (<= (length markets) 1)
   ;     (format str "P&L PER TRADE= ~6D " (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades)))))
   ;     (format str "P&L PER DAY=   ~6D " (round (/  (list-sum trades) (if (zerop (length trades)) 1 (length trades))))))
     ; (setq aa trades)
   ;  (format str "~%%WINNERS=        ~,1,2,'*,' F% %UNCHANGED=       ~,1,2,'*,' F%  %LOSERS=          ~,1,2,'*,' F%   ~%~
   ;     AVERAGE GAIN=~8D  AVERAGE LOSS=~8D~%~
   ;     PAYOFF RATIO=   ~5,2,0,'*,' F  MAX INACTIVE TIME=~4D   %TIME-IN-MARKET= ~4F~%~
   ;     LARGEST LOSS= ~7D  LARGEST GAIN=  ~7D   PROFIT FACTOR=   ~,2,0,'*,' F~%~
   ;     DRAWDOWN= ~11D  FLAT TIME=        ~4D   MAX-DRAW-START=  ~A~%1/2F $/contract=~10D"

    ;   (/ (- (length trades) (length losers)(count 0 trades :test #'=))
    ;      (if (zerop (length trades)) 1 (length trades)))
    ;   (/ (count 0 trades :test #'=) (if (zerop (length trades)) 1 (length trades)))
    ;   (/ (length losers)(if (zerop (length trades)) 1 (length trades)))

    ;   (round ave-win)
    ;   (round ave-loss) (abs (if (zerop ave-loss) 0 (/ ave-win ave-loss)))
    ;    max-inactive-time (my-round (- 100 (* 100 (/ inactive-time (length trades)))) 1)
    ;   (round  (min* losers))(round (max* winners))
    ;   (/ (* (length winners) ave-win) (* (if (zerop (length losers)) 1 (length losers))
    ;                                      (abs (if (zerop ave-loss) 1 ave-loss) )))
    ;  (round draw) flat-time (date-convert draw-start-date)
    ;  (if (plusp (list-sum trades))(round (* 2 (optimal-f trades))) 0)

    ; ); closes the format
 
     (multiple-value-setq (draw-50 draw-90 draw-99)(monte-carlo-drawdown trades))
    ; (format str "~%~%SUGGESTED INITIAL $=     ~6D" (* 1000 (ceiling (* 3 (max (abs draw)(abs draw-50))) 1000)))
    ; (format str "~%AVE ANNUAL RETURN= ~5,1,0,'*,' F%"
    ;      (* 100 (/ (/ (list-sum trades)(/ (subtract-dates date1 date2) 365.25))
    ;                   (* 1000 (ceiling (* 3 (max (abs draw)(abs draw-50))) 1000))) ))

    ; );closes with-open-file

   ;  (with-open-file (stream outfile1 :direction :output :if-exists :supersede :if-does-not-exist :create)

   ;  (dolist (ith (reverse trades))
   ;     (push ith temp)
   ;     (setq qty (min qty limit) qty2 (min qty2 limit) qty3 (min qty3 limit))
   ;     (setq running-sum (round (+ running-sum (* qty ith)))
   ;           running-sum2 (round (+ running-sum2 (* qty2 ith)))
   ;           running-sum3 (round (+ running-sum3 (* qty3 ith))));

   ;     (format stream "~D\,~D\,~D\,~D\,~D\,~D\,~D\,~D\,~D~%" ith conser-dollar-contract dollar-contract qty
   ;                     running-sum qty2 running-sum2 qty3 running-sum3)

   ;     (setq dollar-contract (max (if (> (count-if #'(lambda(s) (/= s 0)) temp) 30)(round (optimal-f temp)) 0)
   ;                                (abs largest-possible-loss))
   ;           conser-dollar-contract (max (if (> (count-if #'(lambda(s) (/= s 0)) temp) 30) (round (abs (drawdown temp))) 0)
   ;                                (abs largest-possible-loss))
   ;           qty (if (and (plusp running-sum)(plusp dollar-contract))
   ;                   (1+ (floor (/ running-sum (* 1.5 dollar-contract)))) 1)
   ;           qty2 (if (and (plusp running-sum2)(plusp dollar-contract))
   ;                   (1+  (floor (/ running-sum2 (* 2 dollar-contract)))) 1)
   ;           qty3 (if (and (plusp running-sum3)(plusp conser-dollar-contract))
   ;                   (1+  (floor (/ running-sum3 (* 3 (max conser-dollar-contract dollar-contract))))) 1))
   ;      (push running-sum2 daily-equity-line2)
   ;      (push running-sum3 daily-equity-line3)
   ;     ));;closes the dolist and with-open-file
   ;  (with-open-file (str outfile :direction :output :if-exists :append :if-does-not-exist :create)
   ;    (format str "~%MAX CONSERVATIVE DRAWDOWN=    ~4,2F%" (drawdown% trades daily-equity-line3 3))
   ;    (format str "~%MAX AGGRESSIVE DRAWDOWN=      ~4,2F%" (drawdown% trades daily-equity-line2 3))
 
   ;    (format str "~%~%MEDIAN DRAWDOWN= ~A~%90 PERCENTILE DRAWDOWN= ~A~%99 PERCENTILE =~A~%" draw-50 draw-90 draw-99) 
       (multiple-value-setq (monthly-totals num-months losing-months winning-months best-month worst-month median-month 
                             mean-month) (monthly-gains-losses1 time-frame ))
       (setq trades-monthly (truncate (/ (length trades) num-months)))
       
   ;    );;;closes the with-open-file
      (write-analysis-file markets trades winners losers ave-win ave-loss draw  flat-time draw-start-date
                           date1 date2 num-months losing-months winning-months best-month worst-month median-month mean-month
                           trades-monthly outfile2)
      ; daily-equity-changes ;;;P7Ls weighted bythe qty3 with conservative approach
))

(defun monthly-gains-losses1 (time-frame )
 (let (daytrades date gain-loss monthly-totals bins path-in (num-trades 0) (total-num-trades 0)
      num-months losing-months winning-months best-month worst-month median-month mean-month)
 
;   (setq time-frames (mapcar #'(lambda (s) (format nil "~A" s)) time-frames))
;   (setq time-frame (apply #'string-append time-frames) time-frame (read-from-string time-frame))
    (setq path-in (string-append *output-upper-dir* "diary-" (format nil "~(~A~)-" time-frame) "composite.csv"))
    (with-open-file (str path-in :direction :input)

      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
          (push record daytrades)))
  
    (dolist (record daytrades)
 
         (setq date (truncate (read-from-string (n-field-csv record 0)) 100)
              num-trades (abs (read-from-string (n-field-csv record 1)))
              total-num-trades (+ total-num-trades num-trades)
              gain-loss (read-from-string (n-field-csv record 2)))
          (if (not (assoc date bins))
              (setq bins (acons date (list gain-loss) bins))
             (setf (cdr (assoc date bins)) (cons gain-loss (cdr (assoc date bins)))))
        )
      (dolist (ith bins)
        (push (apply #'+ (cdr ith)) monthly-totals))

      (setq losing-months (count-if #'minusp monthly-totals))
      (setq winning-months (count-if #'plusp monthly-totals))
      (setq num-months (length monthly-totals))

      (setq best-month (apply #'max monthly-totals))
      (setq worst-month (apply #'min monthly-totals))
      (setq median-month (percentile2 50 monthly-totals))

      (setq mean-month (round (/ (list-sum monthly-totals) (length monthly-totals))))
     
   

(values monthly-totals num-months losing-months winning-months best-month worst-month median-month mean-month ) 
 ))


;;;;computes the daily change as a percent of the ave range over period n
(defun range-change (tdate period)
  (let ((date tdate)(ydate (getd tdate 'ydate)) rc num)
    ; (setq num (cdr (assoc *data-name* *market-days-available*)))
      (setq num (available-days *data-name* date))
     (dotimes (ith num)
        (push (/ (- (getd date 'close) (getd ydate 'close))(volatility date period 1)) rc)
        (setq date (getd date 'ydate) ydate (getd date 'ydate)))

     rc))


;;;trades is a list of gains and losses with the most recent first.
;;;daily equity changes is a list of gains and losses from
;;;weighting the trades gains and losses by number of contracts traded

(defun drawdown% (trades daily-equity-line &optional (factor 3))
  (let ((max-draw-ratio 1) (max-equity 0) equity-line init-acct)
 ; (setq aa trades bb daily-equity-line)
  (setq init-acct (* -1 factor (drawdown (copy-list trades))))
  (setq equity-line (mapcar #'(lambda(s) (+ init-acct s)) (reverse daily-equity-line)))
;;;the daily equity line is the running sum of the weighted trade p&ls
 ;  (setq ff equity-line)

      (dolist (ith equity-line)
        (if (> ith max-equity)(setq max-equity ith))
    ;    (format T "max-equity= ~A  ith= ~A  max-equity-ratio= ~A~%"
     ;      max-equity ith max-draw-ratio)
        (if (and (< ith max-equity) (> max-draw-ratio (/ ith max-equity)))
             (setq max-draw-ratio (/ ith max-equity))))
    ;   (setq cc init-acct dd max-equity ee max-draw-ratio)
   (values (* 100 (1- max-draw-ratio)))
 ))


 ;;trades is a list of gains and losses with the most recent first.
(defun drawdown (trades)
  (let ((max-draw 0)(max-equity 0)(equity-line (list 0))
        (flat-time 0)(max-flat-time 0))


 ;;;the equity line is the running sum of the trade p&ls
      (dolist (jth (reverse trades))
         (setq equity-line (cons (+ jth (car equity-line)) equity-line)))

       (dolist (ith (reverse equity-line))

        (if (> ith max-equity)
            (setq max-equity ith flat-time 0))

        (if (and (< ith max-equity) (< max-draw (- max-equity ith)))
            (setq max-draw (- max-equity ith)))

        (if (<= ith max-equity) (incf flat-time))
        (if (> flat-time max-flat-time)(setq max-flat-time flat-time))

             )
   (values (- max-draw) max-flat-time)
 ))

;;;;what is the average drawdown and the distribution?
(defun monte-carlo-drawdown  (trades)
  (let ((dmy-list (copy-list trades)) draws)
   
    (dotimes (jth 1000)
       (setq dmy-list (randomize-list dmy-list))
       (push  (drawdown dmy-list) draws))

   (values (round (percentile2 50 draws))(round (percentile2 10 draws))(round (percentile2 1 draws))(min* draws))
)) 



 ;;trades is a list of gains and losses with the most recent first.
 ;;;dates are the dates of the gains and losses
(defun drawdown1 (trades dates)
  (let ((max-draw 0)(max-equity 0)(equity-line (list 0)) (max-ctr 0)
        (flat-time 0)(max-flat-time 0)(ctr 0) lowest-draw-date flat-start-date)

   (setq dates (reverse dates))
 ;;;the equity line is the running sum of the trade p&ls
      (dolist (jth (reverse trades))
         (setq equity-line (cons (+ jth (car equity-line)) equity-line)))

       (dolist (ith (reverse equity-line))
         (incf ctr)
        (if (> ith max-equity)
            (setq max-equity ith flat-time 0))

        (if (and (< ith max-equity) (< max-draw (- max-equity ith)))
            (setq max-draw (- max-equity ith) max-ctr ctr))

        (if (<= ith max-equity) (incf flat-time))
        (if (> flat-time max-flat-time)(setq max-flat-time flat-time flat-start-date (nth (- ctr flat-time) dates)))

             )
        (setq lowest-draw-date (nth (- max-ctr 2) dates)); (setq dates1 dates)
     ; (format T "~%max-ctr= ~A~%" max-ctr)
   (values (- max-draw) max-flat-time lowest-draw-date flat-start-date)
 ))




;;;returns the optimal amount of money to trade one contract
;;;
(defun optimal-f (trades)
  (block nil
   (let ((biggest-loss 0.0d0)(twr 1.0)(max-twr 1.0d0) (op-f 1.0d0))
   (setq biggest-loss (apply #'min trades))
   (if (minusp (list-sum trades)) (return 0.0))

   (do ((f 0.001d0 (+ f .001d0)))
       ((> f .999d0) (values (float (/ biggest-loss (- op-f))) op-f))

       (dolist (ith trades)
         (setq twr (* twr (+ 1.0d0 (/ (* f (- ith)) biggest-loss)))))

       (if (> twr max-twr) (setq max-twr twr op-f f twr 1.0d0)
           (setq twr 1.0d0)))
)))


;;;given a file of simulation trades
;20010104,LONG,63.9,20010108,KC,MAR01,O,67.75,3.85,1444
;;first the start date, direction, start-price, exit date, market, contract month, exit reason, exit price,
; points gain/loss, and last $P&L does not include slippage and commission
(defun exit-statistics (path1 &optional (path2 nil))
 (let (records ; sexits (sexits-winners 0)(sexits-losers 0)
       excursion max-excursion trhigh trlow  markets markets-list ;dcr comm-factor
      (gains 0)(losses 0) (pl 0) (pf 0)(draw 0)(ave-pl 0) records1
       (path (string-append *output-upper-dir* "exit-trade-data.csv"))
      trades record1 ; (cexits-winners 0)(cexits-losers 0) cexits
      (tave-win 0)(tave-loss 0)(tnum-wins 0)(tnum-losses 0)(tgains 0)(tlosses 0)(tpl 0)(tpf 0)(tdraw 0)(tave-pl 0) )
    (labels ((set-comm (rec)
              (cond ((member  (get-exitpoints-symbol  rec) *dubai-list*)
                      (setq  *commission* 10 *pips-slippage* 0))
                      ((member (get-exitpoints-symbol rec) *forex-warehouse-list*)
                       (setq *commission* 0 *pips-slippage* 6))
                      ((member (get-exitpoints-symbol  rec) (append *equity-warehouse-list* *ff-list* *space-list*))
                       (setq *commission* 35 *pips-slippage* 0))
                      ((member (get-exitpoints-symbol rec) *micro-list*)
                       (setq *commission* 6 *pips-slippage* 0))))
            )
   (maind-x)(set-cat-list)
  (if path2 (setq path path2))
   (when (probe-file path1)
     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (push record1 records));;closes the do
       ))

;;;lets find the total of gains plus losses
   ;  (setq comm-factor (/ 0.0 (/  (list-sum (mapcar #'(lambda(s) (abs (nth 9 s))) records)) (length records))))
     ;(print comm-factor)
    
;;;first find the markets in the records
    (dolist (record records)
           (pushnew (nth 4 record) markets))
   ;(print markets)
    (dolist (market markets)
      (setq pl 0 gains 0 losses 0 trades nil excursion 0 max-excursion 0)
      (set-comm market)
      ;(setq dcr *commission*)
        (set-market (get-exitpoints-symbol market))
          
      (dolist (record records)
                
         (when (eql market (nth 4 record))
             ; (setq *commission* (+ dcr (* comm-factor (abs (nth 9 record)))))
             (setq excursion 0)
             (setq pl (+ pl (nth 9 record) (- (comm+slip (nth 3 record)))) trades (cons record trades))
             (if (plusp (nth 9 record)) (setq gains (+ gains (nth 9 record)(- (comm+slip (nth 3 record)))))
                (setq losses (+ losses (nth 9 record)(- (comm+slip (nth 3 record))))))
            (multiple-value-setq (trlow trhigh)(extreme-low-high (nth 0 record)(nth 3 record)))  
          (setq excursion  (* (if (eql (nth 1 record) 'LONG) (- trlow (nth 2 record))
                                  (- (nth 2 record) trhigh))
                              (calculate-point-value (nth 3 record))))
           (setq excursion (round (- excursion (comm+slip (nth 3 record)))))
            
           (if (< excursion max-excursion)(setq max-excursion excursion))
          ; (format T "~%date = ~A dirp = ~A trlow= ~A trhigh= ~A EX = ~A MAX= ~A"
           ;           (nth 0 record)(nth 1 record) trlow trhigh excursion max-excursion)
          );;;closes the when
        );;;closes dolist over records
;;;order is important for drawdowns
     (setq trades (vsort trades #'< #'car))
     (setq trades (mapcar #'(lambda (s) (progn (set-market (get-exitpoints-symbol (nth 4 s)))
                                   (- (nth 9 s) (comm+slip (nth 3 s))
                                                            ))) trades))
     (setq draw  (round (drawdown trades))
          ave-pl (round (/ pl (if (zerop (length trades))  1 (length trades)))) 
          pf (my-round (abs (/ gains (if (zerop losses) 1 losses))) 2))
  
    (setq markets-list
           (cons (list market (length trades) pl ave-pl pf draw max-excursion) markets-list))
  );;;closes the dolist over markets
; (setq markets-list1 markets-list)
;;;;find rank based on ave-pl pf and drawdown
       (dolist (record records)
         
          (set-market (get-exitpoints-symbol (nth 4 record)))(set-comm (nth 4 record))
         ; (setq *commission* (+ dcr (* comm-factor (abs (nth 9 record)))))
          (setq tpl (+ tpl (nth 9 record) (- (comm+slip (nth 3 record)))))
          (if (plusp (nth 9 record)) (setq tgains (+ tgains (nth 9 record)(- (comm+slip (nth 3 record))))
                                           tnum-wins (1+ tnum-wins))
                (setq tlosses (+ tlosses (nth 9 record)(- (comm+slip (nth 3 record))))
                              tnum-losses (1+ tnum-losses)))
      )

    (setq records1 (vsort records #'< #'car))
    (setq records1 (mapcar #'(lambda (s) (progn (set-market (get-exitpoints-symbol (nth 4 s)))
                                       (- (nth 9 s) (comm+slip (nth 3 s))))) records))
  

    (setq tdraw  (drawdown records1)
          tave-pl (round (/ tpl (if (zerop (length records1))  1 (length records1)))) 
          tpf (my-round (abs (/ tgains (if (zerop tlosses) 1 tlosses))) 2)
          tave-win (round (/ tgains (if (zerop tnum-wins) 1 tnum-wins)))
          tave-loss (round (/ tlosses (if (zerop tnum-losses) 1 tnum-losses))))

   (setq markets-list  (vsort markets-list #'> #'fourth))  ;;;#'fourth is the ave-pl
;;;;add the rank to the 0th field
    (dotimes (ith (length markets-list))
       (setf (nth ith markets-list)
          (cons ith (nth ith markets-list))))
   
    (setq markets-list (vsort markets-list #'> #'sixth)) ;;;#'sixth is the profit factor
    (dotimes (ith (length markets-list))
       (setf (car (nth ith markets-list))
             (+ ith (car (nth ith markets-list)))))
   
    (setq markets-list (vsort markets-list #'> #'seventh)) ;;;#'seventh is the drawdown
    (dotimes (ith (length markets-list))
       (setf (car (nth ith markets-list))
             (+ ith (car (nth ith markets-list)))))
    (vsort markets-list #'< #'car);;;where 'car is the composite rank
   ; (dolist (jth markets-list)
   ;   (format T "~%~A" jth))
    (with-open-file (str path :direction :output :if-exists :append :if-does-not-exist :create)
    
     (format str "Market    #trades    $P&L    $Ave     PF   Drawdown  Excursion~%")
    
     (format str "~%All      ~6D    ~7D   ~4D    ~4,2F ~8D     ~%"
              (length records) (round tpl) (round tave-pl)  tpf (round tdraw))

     (dolist (jth markets-list)
       (format str"~%~10A ~4D ~10D   ~4D    ~4,2F  ~7D  ~7D"
                  (second jth)(round (third jth))(round (fourth jth))(round (fifth jth))(sixth jth)
                  (seventh jth)(round (eighth jth))))
      (format str "~3%" )

       );;;closes the with-open-file

        
  ;    (dolist (ith records)
  ;      (if (eql (nth 6 ith) 'S)(push (nth 9 ith) sexits))
  ;      (if (eql (nth 6 ith) 'C)(push (nth 9 ith) cexits)))
      

  ;    (dolist (jth sexits)
  ;        (if (plusp jth) (setq sexits-winners (+ jth sexits-winners))
  ;             (setq  sexits-losers (+ jth sexits-losers))))
    
  ;    (dolist (jth cexits)
  ;        (if (plusp jth) (setq cexits-winners (+ jth cexits-winners))
  ;             (setq  cexits-losers (+ jth cexits-losers))))
    
  ;    (format T "~%S P&L/TRADE = ~A Profit factor = ~A" 
   ;       (round (/ (+ sexits-winners sexits-losers) (length sexits)))   
   ;       (my-round (/ sexits-winners (- sexits-losers)) 2))   
   ;  (when cexits
   ;   (format T "~%C P&L/TRADE = ~A Profit factor = ~A" 
   ;         (round (/ (+ cexits-winners cexits-losers) (length cexits)))          
   ;         (my-round (/ cexits-winners (- cexits-losers)) 2)))
)));;;


;;;given a file of simulation trades
(defun exit-statistics-by-year (path1 &optional (path2 nil))
 (let (records ;sexits (sexits-winners 0)(sexits-losers 0) 
      years  years-list num-wins num-losses ave-win ave-loss 
      (gains 0)(losses 0) (pl 0) pf draw ave-pl  (path (string-append *output-upper-dir* "exit-year-data.csv"))
       (tpl 0)(tgains 0)(tlosses 0)(tnum-wins 0)(tnum-losses 0)
      (tdraw 0)(tave-win 0) (tave-loss 0)(tave-pl 0)(tpf 0)
      trades record1);  (cexits-winners 0)(cexits-losers 0) cexits)
      (labels ((set-comm (rec)
                  (cond ((member  (get-exitpoints-symbol  rec ) *dubai-list*)
                         (setq  *commission* 10 *pips-slippage* 0))
                        ((member (get-exitpoints-symbol  rec ) *forex-warehouse-list*)
                         (setq *commission* 0 *pips-slippage* 6))
                        ((member (get-exitpoints-symbol   rec) *equity-warehouse-list*)
                         (setq *commission* 35 *pips-slippage* 0))
                        ((member (get-exitpoints-symbol  rec) *micro-list*)
                         (setq *commission* 10 *pips-slippage* 0))

                        (t (setq *commission* 25 *pips-slippage* 0))))
            )
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
         (set-comm (nth 4 record))   
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
                                               (set-comm (nth 4 s))
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
          (set-market (get-exitpoints-symbol (nth 4 record))) (set-comm (nth 4 record))
          (setq tpl (+ tpl (nth 9 record) (- (comm+slip (nth 3 record)))))
          (if (plusp (nth 9 record)) (setq tgains (+ tgains (nth 9 record)(- (comm+slip (nth 3 record))))
                                           tnum-wins (1+ tnum-wins))
                (setq tlosses (+ tlosses (nth 9 record)(- (comm+slip (nth 3 record))))
                              tnum-losses (1+ tnum-losses)))
      )

    (setq records (vsort records #'< #'car))
    (setq records (mapcar #'(lambda (s) (progn (set-market (get-exitpoints-symbol (nth 4 s)))
                                              (set-comm (nth 4 s))(- (nth 9 s) (comm+slip (nth 3 s))))) records))
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

    (with-open-file (str path :direction :output :if-exists :append :if-does-not-exist :create)
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

(defun find-largest-trade-loss (path1)
  (let ( record1 records)
   
   (when (probe-file path1)
     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (push record1 records));;closes the do
       ))
  (vsort records #'< #'ninth)
  (setq records (subseq records 0 10))
 (dolist (record records)
    (format T "~A~%" record))
))

;;;given a csv file of TS simulation trades
;;;contains date market name and P&L 
(defun exit-statistics1 (records)
 (let ( markets markets-list
      (gains 0)(losses 0) (pl 0) (pf 0)(draw 0)(ave-pl 0) 
       trades records1 (path "~/exitpoints/exit-ts-statistics.lisp")
      (tave-win 0)(tave-loss 0)(tnum-wins 0)(tnum-losses 0)(tgains 0)(tlosses 0)(tpl 0)(tpf 0)(tdraw 0)(tave-pl 0))

    (setq *commission* 0 *pips-slippage* 0)
  ;first find the markets in the records
    (dolist (record records)
           (pushnew (nth 1 record) markets))
   ; (format T "~%~A " markets)
    (dolist (market markets)
      (setq pl 0 gains 0 losses 0 trades nil)
     ; (set-market (car (rassoc market *ts-symbol*)))
     ;(format T "~%~A ~A" market (car (rassoc market *ninja-symbol*)))
      (set-market (car (rassoc market *ninja-symbol*)))
     ; (format T "~%~A" *data-name*)    
      (dolist (record records)
                
         (when (eql market (nth 1 record))
               (setq pl (+ pl (nth 2 record) (- (comm+slip (nth 0 record)))) trades (cons record trades))
             (if (plusp (nth 2 record)) (setq gains (+ gains (nth 2 record)(- (comm+slip (nth 0 record)))))
                (setq losses (+ losses (nth 2 record)(- (comm+slip (nth 0 record)))))))
        );;;closes dolist over records
;;;order is important for drawdowns
     (setq trades (vsort trades #'< #'car))
     (setq trades (mapcar #'(lambda (s) (progn ;(set-market (car (rassoc (nth 1 s) *ts-symbol*)))
                                               (set-market (car (rassoc (nth 1 s) *ninja-symbol*)))
                                     (- (nth 2 s) (comm+slip (nth 0 s))))) trades))
     (setq draw  (round (drawdown trades))
          ave-pl (round (/ pl (if (zerop (length trades))  1 (length trades)))) 
          pf (my-round (abs (/ gains (if (zerop losses) 1 losses))) 2))
  
    (setq markets-list
           (cons (list market (length trades) pl ave-pl pf draw ) markets-list))
  );;;closes the dolist over markets
; (setq markets-list1 markets-list)
;;;;find rank based on ave-pl pf and drawdown
       (dolist (record records)
         ; (set-market (car (rassoc (nth 1 record) *ts-symbol*)))
          (set-market (car (rassoc (nth 1 record) *ninja-symbol*)))
          (setq tpl (+ tpl (nth 2 record) (- (comm+slip (nth 0 record)))))
          (if (plusp (nth 2 record)) (setq tgains (+ tgains (nth 2 record)(- (comm+slip (nth 0 record))))
                                           tnum-wins (1+ tnum-wins))
                (setq tlosses (+ tlosses (nth 2 record)(- (comm+slip (nth 0 record))))
                              tnum-losses (1+ tnum-losses)))
      );;;;closes dolist over records

    (setq records1 (vsort records #'< #'car))
    (setq records1 (mapcar #'(lambda (s) (progn ;(set-market (car (rassoc  (nth 1 s) *ts-symbol*)))
                                                (set-market (car (rassoc  (nth 1 s) *ninja-symbol*)))
                                               (- (nth 2 s) (comm+slip (nth 0 s))))) records))
  

    (setq tdraw  (drawdown records1)
          tave-pl (round (/ tpl (if (zerop (length records1))  1 (length records1)))) 
          tpf (my-round (abs (/ tgains (if (zerop tlosses) 1 tlosses))) 2)
          tave-win (round (/ tgains (if (zerop tnum-wins) 1 tnum-wins)))
          tave-loss (round (/ tlosses (if (zerop tnum-losses) 1 tnum-losses))))

   (setq markets-list  (vsort markets-list #'> #'fourth))  ;;;#'fourth is the ave-pl
;;;;add the rank to the 0th field
    (dotimes (ith (length markets-list))
       (setf (nth ith markets-list)
          (cons ith (nth ith markets-list))))
   
    (setq markets-list (vsort markets-list #'> #'sixth)) ;;;#'sixth is the profit factor
    (dotimes (ith (length markets-list))
       (setf (car (nth ith markets-list))
             (+ ith (car (nth ith markets-list)))))
   
    (setq markets-list (vsort markets-list #'> #'seventh)) ;;;#'seventh is the drawdown
    (dotimes (ith (length markets-list))
       (setf (car (nth ith markets-list))
             (+ ith (car (nth ith markets-list)))))
    (vsort markets-list #'< #'car);;;where 'car is the composite rank

    (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
    
     (format str "Market    #trades    $P&L    $Ave     PF   Drawdown  ~%")
    
     (format str "~%All      ~6D    ~7D   ~4D    ~4,2F ~8D~%"
              (length records) (round tpl) (round tave-pl)  tpf (round tdraw))

      (dolist (jth markets-list)
      (format str"~%~10A ~4D ~10D   ~4D    ~4,2F  ~7D "
                  (second jth)(round (third jth))(round (fourth jth))(round (fifth jth))(sixth jth)(seventh jth)))
      (format str "~3%" )

       );;;closes the with-open-file
));;;


;-------------------------------------------------------------------------------
;;;FOR UNFILTERED TRADES in warehouse

;;;returns list of markets in the warehouse
(defun num-markets-in-warehouse3 (swings)
  (let (markets-in-training-data)
   (dolist (record swings)
      (pushnew (svref record 0) markets-in-training-data)
      )
   markets-in-training-data))


;;this function works for both daytrades and swings
;;;does assume you have already run swing-trade-bins and/or day-trade-bins
(defun num-trades-in-warehouse3 (market swings &optional (sdate 0))
  (let ((ctr 0) start-dates end-dates first-date last-date duration)

    (dolist (record swings)
      (when (and (>= (svref record 1) sdate)(or (eql market (svref record 0))(eql market 'all)))
           (incf ctr)
           (pushnew (svref record 1) start-dates :test #'equal)
           (pushnew (svref record 17) end-dates :test #'equal)
           (set-market (svref record 0))
           (push (1+ (sub-mkt-dates (svref record 1)(svref record 17))) duration)
           ))
    (setq first-date (car (sort start-dates #'< )))
    (setq last-date (car (sort end-dates #'>)))

      (values ctr first-date last-date
              (if duration  (my-round (/ (list-sum duration)(length duration)) 1) 0))
      ))
;;;

(defun trim-trades (daytrades market fdate ldate)
  (remove-if #'(lambda(s) (or (neql (svref s 0) market)(< (svref s 1) fdate)
                              (> (svref s 1) ldate))) daytrades))

(defun display-num-trades-by-market3 (swings &optional (comm 10) path)
   (let (training-data markets-list num-trades first-date last-date ave-dur
         draw pl pf ave-pl)
         (if (not path)(setq path (string-append *output-upper-dir* "unfiltered-trades.txt")))
      
    (setq *commission* comm *pips-slippage* 6) 
    (setq training-data (num-markets-in-warehouse3 swings))
    (dolist (ith training-data)
      (multiple-value-setq (num-trades first-date last-date ave-dur)(num-trades-in-warehouse3 ith swings))
      (multiple-value-setq (pl ave-pl pf draw) (gain-loss-trades-in-warehouse3 ith swings))
      (setq markets-list
           (cons (list ith num-trades pl ave-pl pf draw first-date last-date ave-dur) markets-list))
       );;closes the dolist
    (vsort markets-list #'> #'fifth)
   ; (vsort markets-list #'< #'(lambda(s)(first-char-code (car s))))
    (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "Market    #trades    $P&L    $Ave    PF   Drawdown    FIRST    LAST  DUR~%")
    (dolist (jth markets-list)
     (format str"~%~10A ~5D ~9@D  ~4D   ~4,2F  ~8@D  ~A  ~A  ~A"
     (first jth) (second jth)(round (third jth)) (round (fourth jth))(fifth jth)(round (sixth jth))(seventh jth)(eighth jth) (ninth jth)))

     (multiple-value-setq (num-trades first-date last-date ave-dur)(num-trades-in-warehouse3 'all swings))
     (multiple-value-setq (pl ave-pl pf draw) (gain-loss-trades-in-warehouse3 'all swings))
   (format str"~2%~10A ~4D ~10D   ~4D    ~3A  ~D  ~A  ~A  ~A" 'ALL  num-trades pl ave-pl pf draw first-date last-date ave-dur)  
);;;closes the with-open-file
  ));;closes the let and defun.

(defun gain-loss-trades-in-warehouse3 (market swings &optional (first-date 0))
  (let ((pl 0) (losses 0) (gains 0) draw trades (gl 0))
      (set-market market)
     (dolist (record swings)
        
      (cond  ((member (svref record 0) *forex-warehouse-list*)(setq *commission* 0 *pips-slippage* 6))
             ((member (svref record 0) *dubai-list*)(setq *commission* 10  *pips-slippage* 0))
             ((member (svref record 0) *micro-list*)(setq *commission* 10  *pips-slippage* 0))
             ((member (svref record 0) (union *sp100-list* *stocks-list*))(setq *commission* 20  *pips-slippage* 0))
             (t (setq *commission* 25 *pips-slippage* 0)))

       (when (and (>= (svref record 1) first-date)(or (eql market (svref record 0))(eql market 'all)))
             (set-market (svref record 0))
             (setq gl (- (svref record 19) (comm+slip (svref record 1)))
                   pl (+ pl gl) 
                  trades (cons record trades))
             (if (plusp gl) (setq gains (+ gains gl))
                (setq losses (+ losses gl)))
        ));;;closes the dolist
    ; (format T "~%market = ~A pl = ~A ~%trades = ~A~%" market pl trades)
    
     (setq trades (vsort trades #'< #'(lambda(s) (svref s 1))))
     (setq trades (mapcar #'(lambda (s)(progn (set-market (svref s 0)) (- (svref s 19) (comm+slip (svref s 1))))) trades))

  ;  (setq trades (mapcar #'(lambda (s) (svref s 19) ) trades)) 
   
 (setq draw  (drawdown trades))
     (values pl (round (/ pl (if (zerop (length trades))  1 (length trades)))) 
            (my-round (abs (/ gains (if (zerop losses) 1 losses))) 2) draw)
    ))

(defun find-largest-loss (swings)
  (let (worse-record)
   (setq worse-record (car swings))
 (dolist (record swings)
    (if (< (svref record 19) (svref worse-record 19)) (setq  worse-record record)))
 (format T "~A" worse-record)))


;;;depends on the market
(defun calculate-point-value (tdate &optional market)
  (if market (set-market market))
  (let ((dname *data-name*) point-value)
  ; (format T "CAL ~A   ~A~%" tdate dname (getd tdate 'close))
   (setq point-value
     (cond
      ((member dname '(usdhkd.d3b jpy.d3b cad.d3b chf.d3b mxn.d3b inr.d3b rub.d3b usdzar.d3b
                       usdcny.d3b brl.d3b))
        (/ (index-point-value) (getd tdate 'close)))
      ((member dname '(aud.d3b gbp.d3b eur.d3b nzdusd.d3b)) (index-point-value))
      ((member dname '(gbpjpy.d3b))
       (set-market 'jpy.d3b)
       (/ (index-point-value) (getd tdate 'close)))
       
      ((member dname '(eurjpy.d3b))
       (set-market 'jpy.d3b)
       (/ (index-point-value)(getd tdate 'close)))
      
      ((eql dname 'eurgbp.d3b)
       (set-market 'gbp.d3b)
       (* (index-point-value) (getd tdate 'close)))
    
      ((and (not (index-commodityp))(not (index-futurep)))
            (* (/ 1000.00 (getd tdate 'open))(index-point-value)))
      (t (index-point-value))))
   (set-market dname)
   point-value))

;;;;calcuclates the deduction for commsission and slippage
(defun comm+slip (date)
  (add ;(* *commission* (portf date *data-name*))
   *commission*
    (if (or (neql *data-name* 'all) (readt2 date))
        (mul *pips-slippage* (index-tick-size) (calculate-point-value1 date *data-name*)) 0)
  ))
#|
;;;calcualtes the $ for deduction for commission and slippage
(defun comm+slip-swing (date)
  (+ *swing-commission* 
    (if (neql *data-name* 'all)(* *pips-slippage* (index-tick-size) (calculate-point-value date)) 0)))
|#
(defun comm+slip-points (date)
  (+ (/ *commission* (calculate-point-value date))
                           (* *pips-slippage* (index-tick-size))))
#|
;;;;calculates the deduction for commsission and slippage
(defun comm+slip-contra (date)
 (+ *contra-commission* (* *pips-slippage* (index-tick-size) (calculate-point-value date))))
|#
(defun point+tick (date markets)
  (maind-x)(set-cat-list)
 (dolist (ith markets)
  (set-market ith)(format T "CLOSE= ~A POINT= ~A TICK= ~A~%" (getd date 'close) (index-point-value)(index-tick-size)))
)


;;;path1 is the location of the ts test file.
;;;"~/exitpoints/ts-triumph21-results1.csv"
(defun ts-brochure (path1 &optional (commission 25)(min-acct 31000)(vendor 300))
  (let (record1 records draw flat num-days ave-loss-day ave-win-day dates trades pls
         num-months draw-low-date draw-start-date)
   (maind-x)(set-cat-list)(set-market 'dj.d1b)
   (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (print record);(break "111")
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
     ; (if (member (second record1) '(sm ct pl kc sb lh hg cc ng cl lc))
  ;      (setq *fore-list* (set-difference *day-list* (append  *softs-list* 
   ;                           '(nk.d1b  ng.d1b nd.d1b hg.d1b gc.d1b us.d1b s.d1b w.d1b jy.d1b ho.d1b))))
       
   ;   (if (and (member (second record1) '(YM ES NQ  GC HG ZB  RB 6S HE ))
 ;     (if (and (member (second record1) )
               ; (member (second record1) '(YM ES NQ RTY GC SI HG ZB RB CL HE ))
       ;         (> (first record1) 20170700)(< (first record1) 20210700))
    ;   (push record1 records) )
        (print record1)
        (push record1 records) 
       ) );;closes with-open-file and the dolist over paths
  ; (print records)
   (setq records11 records)
   (setq records (vsort records #'> 'car)) 
  (setq num-months (nth-value 1 (ts-monthly-gains-losses records nil)))
  ;(print num-months)
  (setq commission (+  (/ (* vendor num-months) (length records)) commission));;;$100 per month
    (dolist (ith records)
   ;  (print ith)
    (setf (third ith)(- (third ith) commission)))     

   (setq trades (mapcar #'(lambda (s)(third s)) records)
         dates (mapcar #'(lambda (s)(first s)) records))
  
   ; (setq tdates dates)
  ;;;deducts commission of $15 per round turn
 ; (setq trades (mapcar #'(lambda (s) (- s 15)) trades))
 ; (setq trades1 trades dates1 dates)
   (multiple-value-setq (draw flat draw-low-date draw-start-date)(drawdown1 trades dates))
  ; (format T "~% ~A ~A" (car (last dates)) (first dates))
   (setq num-days (1+ (sub-mkt-dates (car (last dates))(first dates))))
  ; (print "1234")
   (setq pls (build-ts-diary records))
   (format T "~%Total Period Profit($) = ~D~%" (round (list-sum trades)))
   
   (format T "Number of days = ~A~%Number of Trades = ~A~%" num-days (length records))

   (format T "Average profit per day($) = ~D~%" (round (/ (list-sum trades) num-days))) 
   (format T "Average profit per trade($) = ~D~%" (round (/ (list-sum trades) (length trades)))) 
   (format T "maximum drawdown($) = ~D~%" (round draw))
    (format T "Flat time = ~A~%" flat)
   (format T "Max-drawdown start = ~A~%~%" draw-start-date)
   (format T "Max-drawdown Low date = ~A~%~%" draw-low-date)
   (format T "Trades with a gain(%)= ~3,1F~%" (* 100.0 (/ (count-if #'(lambda(s) (plusp s)) trades)
                                                       (length trades))))
   
   (format T "Trades with a breakeven(%)= ~3,1F~%" (* 100.0 (/ (count-if #'(lambda(s) (zerop s)) trades)
                                                          (length trades))))
   (format T "Trades with a loss(%)= ~3,1F~%~%" (* 100.0 (/ (count-if #'(lambda(s) (minusp s)) trades)
                                                       (length trades))))


   (format T "Days with a gain(%) = ~A~%" 
        (my-round (* 100 (/ (count-if #'(lambda(s) (plusp (cdr s))) pls) num-days)) 1))

   (format T "Days with no trades(%) = ~A~%" 
        (my-round (* 100 (/ (- num-days (length pls)) num-days)) 1))
   (format T "Days with a loss(%) = ~A~%~%" 
        (my-round (* 100 (/ (count-if #'(lambda(s) (minusp (cdr s))) (build-ts-diary records)) num-days)) 1))
   (format T "Round Turn Deduction for Commission and Vendor fee($) = ~D~%" (round commission))

   (setq ave-win-day  (/  (list-sum (remove-if #'(lambda (s)(not (plusp s))) trades))
           (length (remove-if #'(lambda (s)(not (plusp s))) trades))))
   
   (format t "Average winning trade($) = ~A~%" (round ave-win-day))
   
   (setq ave-loss-day   (/  (list-sum (remove-if #'(lambda (s)(plusp s)) trades))
           (length (remove-if #'(lambda (s)(plusp s)) trades))))
    
   (format T "Average losing trade ($) = ~A~%" (round ave-loss-day))
      
   (format T "Payoff Ratio = ~4,2F~%~%" (abs (/ ave-win-day ave-loss-day)))
   (format T "Max consecutive Days with no Trades = ~A~%" (max-inactive-time dates))
   (format T "Time in the market(%) = ~A~%" (my-round (* 100 (/ (length pls) num-days)) 1))

   (format T "Largest single losing day($) = ~D~%" 
           (round  (apply #'min (mapcar #'cdr pls))))
   (format T "Largest single winning day($) = ~D~%" (round (apply #'max (mapcar #'cdr pls))))
;;;sum of the winning days divided by te sum of the losung days.

   (format T "Profit factor = ~4,2F ~%"
       (abs  (/  (list-sum (mapcar #'(lambda (s)(if (plusp (cdr s)) (cdr s) 0)) pls))
                     (list-sum (mapcar #'(lambda (s)(if (plusp (cdr s)) 0 (cdr s))) pls)))))
   (format T "Largest single losing trade = ~D~%" (round (apply #'min trades)))
   (format T "Largest single winning trade = ~D~%" (round (apply #'max trades)))

   (setq num-months (ts-monthly-gains-losses records t))
   (format T "~%Recommended Minimum Equity($) = ~D~%" min-acct) 
   (format T "~%Num Trades Per Month = ~A~%" (round (/ (length records) (length num-months))))
   (format T "Annual Rate of Return(%) = ~4,1F~%"
      (* 100  (/ (/ (list-sum trades)(/ (length num-months) 12)) min-acct)))
   (format T "Max drawdown(%) = ~4,1F~%" (* 100 (/ draw min-acct)))

   (format T "~%1/2 Optimal-F = ~A~%" (round (* 2 (optimal-f trades))))
   (exit-statistics1 records)
))

(defun build-ts-diary (records)
     (let ( pls)
;;;need an assoc list of dates and gains/losses
   
      (dolist (ith records)
        (ifn (assoc (car ith) pls)
            (push (cons (car ith) (third ith)) pls)
             (setf (cdr (assoc (car ith) pls))
                   (+ (cdr (assoc (car ith) pls)) (third ith)))))
 
  pls
                   
            
))
      
(defun max-inactive-time (dates)
  (let ((inactive-days 0) (diff 0))
   (set-market 'dj.d1b)
   (do ((mktdates dates (cdr mktdates))
        (ith (car dates) (car mktdates))
        (ith+1 (second dates) (second mktdates)))
       ((not ith+1) inactive-days)
     ;  (ifn (week-day-p ith) (format T "Bad day ~A" ith))
    ;  (format T "~%ith = ~A ith+1 = ~A diff = ~A" ith ith+1 (sub-mkt-dates ith+1 ith))
      (setq diff (sub-mkt-dates (corrected-date ith+1) (corrected-date ith)))
     (if (> diff inactive-days)(setq inactive-days diff))
)))


;;;reads a diary file
(defun ts-monthly-gains-losses (records &optional (str t))
 (let (daytrades date gain-loss monthly-totals bins 
      num-months losing-months winning-months best-month worst-month median-month mean-month)

      (dolist (record records) 
         (setq date (truncate (nth 0 record) 100)
               gain-loss (nth 2 record))
          (if (not (assoc date bins))
              (setq bins (acons date (list gain-loss) bins))
             (setf (cdr (assoc date bins)) (cons gain-loss (cdr (assoc date bins)))))
        )
    ;  (print bins)
      (dolist (ith bins)
        (push (round (apply #'+ (cdr ith))) monthly-totals))

 ;  (with-open-file (str outfile :direction :output :if-exists :append :if-does-not-exist :create)
     (format str "~%~%NUM LOSING MONTHS = ~A" (setq losing-months (count-if #'minusp monthly-totals)))
     (format str "~%NUM WINNING MONTHS = ~A" (setq winning-months (count-if #'plusp monthly-totals)))
     (format str "~%TOTAL NUMBER OF MONTHS = ~A" (setq num-months (length monthly-totals)))

     (format str "~%~%BEST MONTH GAIN($) = ~D" (setq best-month (round (apply #'max monthly-totals))))
     (format str "~%WORST MONTH LOST($) = ~D" (setq worst-month (round (apply #'min monthly-totals))))
  ;   (format str "~%MONTHLY MEDIAN P&L($) = ~D" (setq median-month (round (percentile2 50 monthly-totals))))
     (format str "~%MONTHLY MEDIAN P&L($) = ~D" (setq median-month (round (median monthly-totals))))

     (format str "~%~%MONTHLY MEAN P&L($) = ~A~%"
          (setq mean-month (round (/ (list-sum monthly-totals) (length monthly-totals)))))
     
    
   
   (with-open-file (str (string-append *output-upper-dir* "monthly-totals-"
                                (format nil "~(~A~)" "ts") ".txt")
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (ith bins)
       (format str "~A ~A~%" (car ith) (round (apply #'+ (cdr ith)))))
   )

(values monthly-totals num-months losing-months winning-months best-month worst-month median-month mean-month ) 
 ))


;;;;program to combine and compare simulation gains and losses with TS gains and losses
;;;;

(defun ts-ep-sim (path1 path2)
 (let (records1 records2 record1 records3)
;;;;this reads in the TS file results
 (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       ; (print record)
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
       (push record1 records1)
       )) ;;closes with-open-file and the dolist over paths
 ; (print records)
   (setq records1 (vsort records1 #'> 'car))


;;;;;reads the EP file for comparison. the records are of lenth 10
;;;;
     (with-open-file (str path2 :direction :input)   
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
        (setq record1 (mapcar #'(lambda(s) (read-from-string s)) (my-split-sequence #\, record)))
        (push record1 records2));;closes the do
       ) ;;closes with-open-file 

   (setq records2 (vsort records2 #'< 'car))
   (dolist (kth records2)
      (push (list (first kth)(tenth kth)) records3))



))


(defun triumph21-test (tdate market-list &optional (num 50))
   (maind-x)(set-cat-list)(set-market 'dj.d1b)
  (let ((date (add-mkt-days tdate (- num))) (counter 0))
   (declare (special counter))
    (dolist (ith (directory (string-append "~/mk-data/111-dropbox/ts/" "ts-triumph21*.*")))
         (delete-file ith))
  (dolist (ith (directory (string-append "~/mk-data/111-dropbox/ts/" "ts-triumph11*.*")))
         (delete-file ith))
   
    
   (setq *striker-score-qty* 1 *striker-starter-qty* 1)
   (dotimes (ith num)
     (day-trades date market-list (plusp (num-holidays (getd date 'ndate))))
  ;   (day-trades2c date *epicc-list* (plusp (num-holidays (getd date 'ndate))));;;only for Score
     (setq date (getd date 'ndate)))

    (dolist (ith (directory (string-append *daily-output-dir* "ts-triumph21*.*")))
         (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))
   (dolist (ith (directory (string-append *daily-output-dir* "ts-triumph11*.*")))
         (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))


    (append-ts-triumph21)
    (ts-triumph21-taf-by-market)
)) 


;;;;need to append the ts-triumph21 trade action files into ts-triumph21.csv

(defun append-ts-triumph21 ()
 (let (paths records (path-out "~/mk-data/111-dropbox/ts-triumph21.csv"))
  (setq paths (directory (string-append "mk-data/111-dropbox/ts/" "ts-triumph21*.csv")))
       
   (dolist (ith paths)
  
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file

       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (format str "~A~%" jth)))


   )))


;;;separates the composite trade action file into one per market
(defun ts-triumph21-taf-by-market ()
 (let (records (path-in "~/mk-data/111-dropbox/ts-triumph21.csv") mkt mkts path-out)
  
     (with-open-file (str path-in :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file
;;;;first find all markets in the file ts-triumph21.csv

     (dolist (ith records)
           (setq mkt (second (my-split-sequence #\, ith)))
           (pushnew mkt mkts :test #'equal)
           )
;;;

    (dolist (kth mkts)
      (setq path-out (string-append "~/mk-data/111-dropbox/ts/ts-triumph21-"
             (format nil "~A"  (read-from-string kth)) ".csv"))
       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (if (equal (second (my-split-sequence #\, jth)) kth)
                (format str "~A~%" jth)))))


   ))


(defun charger-test (tdate market-list &optional (num 50))
   (maind-x)(set-cat-list)(set-market 'dj.d1b)
  (let ((date (add-mkt-days tdate (- num))) (counter 0))
   (declare (special counter))
    (dolist (ith (directory (string-append "~/mk-data/111-dropbox/ts/" "ts-charger*.*")))
         (delete-file ith))
      
   (setq *striker-score-qty* 1 *striker-starter-qty* 1)
   (dotimes (ith num)
     (day-tradesx date market-list (plusp (num-holidays (getd date 'ndate))))
         (setq date (getd date 'ndate)))

;;;moves the TS files for charger to the 111-dropbox/ts 
    (dolist (ith (directory (string-append *daily-output-dir* "ts-charger*.*")))
         (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))
 


    (append-ts-charger)
)) 


(defun forex9-test (tdate market-list &optional (num 50))
   (maind-x)(set-cat-list)(set-market 'eur.d3b)
  (let ((date (add-mkt-days tdate (- num))) (counter 0))
   (declare (special counter))
    (dolist (ith (directory (string-append "~/mk-data/111-dropbox/ts/" "ts-forex9*.*")))
         (delete-file ith))
   
    
   (setq *striker-score-qty* 1 *striker-starter-qty* 1)
   (dotimes (ith num)
     (forex-trades2 date market-list (plusp (num-holidays (getd date 'ndate))))
  ;   (day-trades2c date *epicc-list* (plusp (num-holidays (getd date 'ndate))));;;only for Score
     (setq date (getd date 'ndate)))

    (dolist (ith (directory (string-append *daily-output-dir* "ts-forex9*.*")))
         (rename-file ith (string-append "~/mk-data/111-dropbox/ts/" (pathname-name ith) "." (pathname-type ith))))


    (append-ts-forex9)
    (ts-forex9-taf-by-market)
)) 


;;;;need to append the ts-triumph21 trade action files into ts-triumph21.csv

(defun append-ts-forex9 ()
 (let (paths records (path-out "~/mk-data/111-dropbox/ts-forex9.csv"))
  (setq paths (directory (string-append "mk-data/111-dropbox/ts/" "ts-forex9*.csv")))
       
   (dolist (ith paths)
  
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file

       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (format str "~A~%" jth)))


   )))



;;;separates the composite trade action file into one per market
(defun ts-forex9-taf-by-market ()
 (let (records (path-in "~/mk-data/111-dropbox/ts-forex9.csv") mkt mkts path-out)
  
     (with-open-file (str path-in :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file
;;;;first find all markets in the file ts-triumph21.csv

     (dolist (ith records)
           (setq mkt (second (my-split-sequence #\, ith)))
           (pushnew mkt mkts :test #'equal)
           )
;;;

    (dolist (kth mkts)
      (setq path-out (string-append "~/mk-data/111-dropbox/ts/ts-forex9-"
             (format nil "~A"  (read-from-string kth)) ".csv"))
       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (if (equal (second (my-split-sequence #\, jth)) kth)
                (format str "~A~%" jth)))))


   ))
;;;this is the factor to weight the markets in a portfolio
(defun portf (date market); &optional (markets *micro-list*))
;  (let (result)
   ; (setq result (mapcar #'(lambda(s) (progn (set-market s)
;					     (* (volatility
;						(cond ((not (corrected-date date))(first-available-date s 84))
;						      ((< (corrected-date date)(first-available-date s 84))
;						       (first-available-date s 84))
;						      (t (corrected-date date))) 84 .875 'median)
;						(calculate-point-value (corrected-date date))))) markets))
   
   (set-market market)
   (floor (/ *max-swing-risk* (* (calculate-point-value date) (volatility date *duration* *factor*  'median))))
   )
;)

(defun calculate-point-value1 (date market)
  (set-market market)
  (* (portf date market) (calculate-point-value date))) 

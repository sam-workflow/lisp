
;; -*- Mode: LISP; Package: USER; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;this is the list of markets for the Flash traders's product
;(defparameter *ewaves-list* '(dj.d1b sp.d1b nd.d1b ru.d1b us.d1b cl.d1b gc.d1b si.d1b e1.d1b jy.d1b))



;;;this little function will take a symbol and return the code of the first character

(defun first-char-code (sym)
  (let (sym-string)
  (setq sym-string
    (format nil "~A" sym))
  (char-code (char sym-string 0))))
   
;;;;performs and alphabetical sort of a list of symbols (first character only)

(defun alpha-sort (lst)
   (vsort lst #'< #'first-char-code))



;;;;;;;;;;;;;;This function is for reading an out2 file and converting it to Deepak's specification

(defparameter *label-codes*
  '((1 . 1)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 5)
    (AI . 6)
    (B . 7)
    (CI . 8)
    (1C . 11)
    (2C . 12)
    (3C . 13)
    (4C . 14)
    (5C . 15)
    (AC . 16)
    (CC . 18)
    (D . 19)
    (E . 20)
    (W . 81)
    (X . 82)
    (Y . 83)
    (XX . 84)
    (Z . 85)
    (Y+ . 93)
    (? . 0)))


;;;returns a list; each element is a string
(defun my-split-sequence (delimiter sequence)
  (let ((new-seq nil) (indx 0)  ith)
  
     (loop
       
       (setq ith (position delimiter sequence :start indx :end nil))
       (push (subseq sequence indx ith) new-seq)
       (if (not ith)(return (reverse new-seq)))
       (setq indx (1+ ith))
       (if (>= indx (length sequence)) (return (reverse new-seq))))
  
   ))
#|
;;;this function processes a line from the out2 file and creates a line for Deepak's file
(defun encode-sequence (seq)
   (let ((new-line nil) length-new-line (aa 5) aa1)   
    (print seq)
     (dotimes (ith 13)
        
        (when (stringp (nth aa seq))
              (setq aa1 (+ (* 1000 (read-from-string (nth (+ aa 1) seq)))
                         (* 10 (cdr (assoc (read-from-string (nth aa seq)) *label-codes*))) 
                         (case (read-from-string (nth (+ aa 2) seq))
                               (1 1)
                               (-1 2))))
           
             
             (push aa1 new-line))
      
        (setq aa (+ aa 4)));;;closes the dotimes
               
        (setq new-line (reverse new-line))
        (setq length-new-line (length new-line))
       (dotimes (ith 13)
        (if (> ith length-new-line)(push (* 1000 ith) new-line)))   

    (setq new-line (sort new-line #'< ))
    (push (dir-zero-deg seq) new-line) 
    (push (car seq) new-line) 
new-line
));;closes the let and the defun
|#
(defun dir-zero-deg (seq)
  (let (dir label)
    (setq dir (read-from-string (nth (- (length seq) 2) seq))
          label (read-from-string (nth (- (length seq) 4) seq)))
    (if (not (member label '(AI AC CI CC E 1 3 5 1C 3C 5C W Y Y+ Z)))
        (case dir
           (1 2)
           (-1 1))
       (case dir
          (1 1)
          (-1 2)))
))  
  
  

;;;;this function takes a string a splits into several strings and
;;;and returns them in a list of strings. The splits are based on a delimiter
;;; Usually this is for parsing comma delimited files. So the delimiter is usually
;;;#\, but it could be #\space for space delimited data.

(defun out2-file-conversion (path-in)
  (let (path-out seq)
    (setq path-out (concatenate 'string path-in "-C"))
    (if (probe-file path-out)(delete-file path-out))
    (with-open-file (str path-in :direction :input)
    (with-open-file (str2 path-out :direction :output :if-exists :supersede)
       (do ((line (read-line str nil 'eof)
                  (read-line str nil 'eof)))
           ((eql line 'eof))
           (setq seq (encode-sequence (my-split-sequence #\, line)))
           
           (dolist (ith seq)
               (if (equal ith (car (last seq)))
                      (format str2 "~A~%" ith)
                    (format str2 "~A\," ith)))
           );;;closes the do    
        ));;;closes the two with-open-files
));;;closes the let and the defun


;;;;this program is to create a market, add to the config file and set up the directories.
;;;this function is missing something 
#|
(defun create-market (market)
 (let (upper-dir (path2 "index.cfg")
(path3 "/home/ewaves/markets.path"))
  
   (with-open-file (stream path3 :direction :input)
         (setq upper-dir (read stream)))
   (setq path2 (string-append upper-dir path2))  

   (ifn (probe-file path)
      (shell "format nil "mkdir ~A" path))


 (with-open-file (strm (format nil "~(~a~)index.cfg" 
				       *database-upper-dir*)
			       :direction :output :if-exists :supersede)
	   (prin1 *all-markets* strm))
))
|#


(defun find-trading-degree (deg2-ave deg3-ave deg4-ave deg5-ave &optional (time-length 89))
  ;(declare (ignore deg5-ave))
  (let (distances a-list)  
   (setq distances (mapcar #'(lambda (s) (abs (- time-length s))) (list deg2-ave deg3-ave deg4-ave deg5-ave)))
   (setq a-list (pairlis '(2 3 4 5) distances))
   (vsort a-list #'< 'cdr)
   (caar a-list)
  ))
  
;;;;this version finds the trading degree from a count 
(defun find-trading-degree1 (ct &optional (time-length 89))
  (let (deg2-times deg3-times deg4-times deg5-times
        deg2-ave deg3-ave deg4-ave deg5-ave)
  (dolist (ith (eval ct))
    (if (getv (car ith) ET)
     (case (getv (car ith) DG) 
       (2 (pushnew (car ith) deg2-times))
       (3 (pushnew (car ith) deg3-times))
       (4 (pushnew (car ith) deg4-times))
       (5 (pushnew (car ith) deg5-times)))
       ));;;closes the dolist
    (setq deg2-times (mapcar #'(lambda(s) (/ (getv s TL) 24)) deg2-times))   
    (setq deg3-times (mapcar #'(lambda(s) (/ (getv s TL) 24)) deg3-times))        
    (setq deg4-times (mapcar #'(lambda(s) (/ (getv s TL) 24)) deg4-times))   
    (setq deg5-times (mapcar #'(lambda(s) (/ (getv s TL) 24)) deg5-times)) 
    
    (setq deg2-ave (if deg2-times (/ (list-sum deg2-times)(length deg2-times)) 0))  
    (setq deg3-ave (if deg3-times (/ (list-sum deg3-times)(length deg3-times)) 0)) 
    (setq deg4-ave (if deg4-times (/ (list-sum deg4-times)(length deg4-times)) 0)) 
    (setq deg5-ave (if deg5-times (/ (list-sum deg5-times)(length deg5-times)) 0)) 
 ;   (print deg2-times)
 ;   (print deg3-times)
 ;   (print deg4-times)
 ;   (print deg5-times)
 ;   (print deg2-ave)(print deg3-ave) (print deg4-ave) (print deg5-ave)
    (if (< deg4-ave deg5-ave)(setq deg4-ave deg5-ave))
    (if (< deg3-ave deg4-ave)(setq deg3-ave deg4-ave))
    (if (< deg2-ave deg3-ave)(setq deg2-ave deg3-ave))
   
   
    (values (find-trading-degree deg2-ave deg3-ave deg4-ave deg5-ave time-length)
       deg2-ave deg3-ave deg4-ave deg5-ave)
    ))
  
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
(defun find-trading-degree2 (date1 elliottwaves)
  (let (deg2-times deg3-times deg4-times deg5-times
        deg2-ave deg3-ave deg4-ave deg5-ave)

;  (with-open-file (str path :direction :input)
;      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
;          ((eql record 'eof))      
;       (push record elliottwaves))) 
       
;   (setq elliottwaves (mapcar #'(lambda(s) (my-split-sequence #\, s)) elliottwaves)) 
  
   (dolist (ith elliottwaves)
      (when (eql (read-from-string (nth 1 ith)) date1)
       (case (read-from-string (nth 8 ith))
         (2 (push (/ (read-from-string (nth 6 ith)) 24) deg2-times))
         (3 (push (/ (read-from-string (nth 6 ith)) 24) deg3-times))
         (4 (push (/ (read-from-string (nth 6 ith)) 24) deg4-times))
         (5 (push (/ (read-from-string (nth 6 ith)) 24) deg5-times)))))
 
    
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

;;;removes duplicate lines from the .d1b2 file  
(defun remove-duplicate-lines (path)
   (let (trades)
 
    (with-open-file (str path :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))      
       (pushnew record trades :test #'equal)))
  
 
   (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith (reverse trades))
        (format stream "~A~%" ith)))
        
))

;;;removes duplicate waves from the .csv file
(defun remove-initial-dates (path)
   (let (trades )
 
    (with-open-file (str path :direction :input)
      (do* ((record (read-line str nil 'eof) (read-line str nil 'eof))
           (initial-date (subseq record 0 13)))
          ((eql record 'eof)) 
       (unless (equal (subseq record 0 13) initial-date) 
               (push record trades))))
  
 
   (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith (reverse trades))
        (format stream "~A~%" ith)))
)) 

#|
;;;takes multiple markets and combines their diary files
;;;provides a list of ALL dates
(defun diary-composite (time-frames &rest markets-ll)
 (let (paths  time-frame records record1 (running-sum 0)
      (path-out nil) mkts
      (path-out1 "~/exitpoints/p&ls.dat")(path-out-dates "~/exitpoints/dates.dat")
      ) 
   
   
    
   (do* ((mkts markets-ll (cdr mkts))
         (tmf time-frames (cdr tmf))
         (time-frame (car tmf) (car tmf))
         (markets (car mkts) (car mkts)))
       ((null tmf))
   
       (dolist (market markets)
          (push (string-append "~/exitpoints/" 
            (format nil "~A~(~A~)-diary1.dat" market time-frame)) paths));;;closes dolist over markets
            );;closes the do over time-frames 
      
   ;(print paths)
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
;;;now calculate the running sum
  
   (dolist (ith records)
     (setf (fifth ith) (+ (fourth ith) running-sum))   
     (setq running-sum (fifth ith)))
     
   (setq time-frames (mapcar #'(lambda (s) (format nil "~A" s)) time-frames))
   (setq time-frame (apply #'string-append time-frames) time-frame (read-from-string time-frame)) 
   (setq path-out (string-append "~/exitpoints/diary-" (format nil "~(~A~)-" time-frame) "composite.csv")) 
  
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
          
   (summary-gains-losses path-out1 path-out-dates time-frame (caar records) (caar (last records)) mkts)
   
   
             
;   (shell (string-append "unix2dos ~/exitpoints/diary-" (format nil "~(~A~)-" time-frame) "composite.csv " 
;          "/home/mk-data/luis/" (format nil "~(~A~)-" time-frame) "diary-composite.csv")) 
;   (shell "unix2dos ~/exitpoints/summary.dat /home/mk-data/luis/summary.txt") 
;   (shell "unix2dos ~/exitpoints/optimal-f.csv /home/mk-data/luis/optimal-f.csv")
)) 
|# 

;;;;this function writes the recommendations out for the 5-indicator pure
;;;Ewaves trading strategy
;;;; there are several files
;;;one is the ewaves-trade-memory.dat 
;;;;this is a file of 3 lists. First is the combinations that are winners long (> $100)
;;;the second list are the combinations that are winners short (> 100)
;;;third is the list of avoids. These combinations made less than $100.

;;;the second file is the ewaves-5-report. This is the output recommendations for the
;;;; next trading day with the 3-indicator system.
;;;*ewave-list* is defined at the top of this file. It is the a list of the markets
;;;that are traded with the 3-indicator system (flash traders product at EWI)

;;;the third file is ewaves-xp-3. This file needs to contain the 
;;;(date market position entry-date-1 stop-price) for each trading day and market
;;;these are from the recommendations. The position is NOT the position actually held on
;;;on the date. It is the position to be entered or held on the next trading day.
;;;this is used to calculate the open equity
;;;the stop price is the stop loss to be used on the next trading day.
;;;Please note the entry date is the trading day before the actual entry.

;;;path3 is the ewaves created file that has (date label direction wwlabel pw-pattern ct-score trading-degree)
;;;for all the dates run with proc2 or proc66. There is a separate file for each market
;;;;
#|
(defun ewaves-flash-report (tdate &optional (market-list *all-list*))     
   (let (directive signal entry-long entry-short direction direction-1 label wwlabel
         stop prev-stop position prev-position stopped-outp dates pw-pattern ct-score
         winners-long winners-short avoids prev-position-list trading-degree
         entry-date prev-entry-date risk trade-action equity
        (path-bins "/home/ewaves/aewaves/ewaves-4eyes-bins.dat")
        (path-flash-report "/home/register/cycles/ewaves-flash-report.dat")
        (path-ewaves-trade-history "~/cycles/ewaves-trade-history.dat")
         path3 positions)
            
       (setf *ewaves-home* (environment-variable "EWAVESHOME")
             *ewaves-local* (environment-variable "EWAVESLOCAL")) 		
       (with-open-file (str (string-append *ewaves-home* "markets.path"))
	  (setf *database-upper-dir* (read str)))
       (maind-x)
       (read-markets-config)                 
       
       (if (probe-file path-flash-report)(delete-file path-flash-report))               
       (with-open-file (str path-bins :direction :input);;;;reads the warehouse89 derived history of bins
          (setq winners-long (read str) winners-short (read str) avoids (read str)))
;;;need to read the path-ewaves-xp file
       (when (probe-file "~/cycles/ewaves-trade-history.dat")
       (with-open-file (strm path-ewaves-trade-history :direction :input)
            (do ((record (read strm nil 'eof) (read strm nil 'eof)))
                 ((eql record 'eof)) 
                 (push record positions))))
             
            
       (with-open-file (stream path-flash-report :direction :output :if-exists :append :if-does-not-exist :create)
         (format stream "~A~%" (date-convert tdate))
         (format stream "Market               Month    Close   Dir  LB WLB CHG Signal     Stop Equity ~%")) 
        
       (dolist (market market-list) 
           (set-market market)
;           (ifn tdate (setq date (car (last (month-days (get-latest-index-date)))))(setq date tdate))
;;;need to find the previous stop-price and position
          (setq prev-position-list
            (find-if #'(lambda (s1) (and (eql (first s1) (getd tdate 'ydate))(eql (second s1) market))) positions))
          (setq prev-position (third prev-position-list)
                prev-stop (car (last prev-position-list)) prev-entry-date (fourth prev-position-list))
         (ifn prev-position (setq prev-position 0)) 
          (setq dates nil)
          (setq path3 (string-append "~/aewaves/out2/" (string-downcase (format nil "~A" *data-name*)) "3"))
;;;creates the list dates from the proc2 and proc66 runs
;;;each element of dates is a list of (date label direction wwlabel pw-pattern ct-score trading-degree)
          (with-open-file (strm path3 :direction :input)
             (do ((record (read strm nil 'eof) (read strm nil 'eof)))
                 ((eql record 'eof)) 
                 (push record dates)));;;closes the with-open-file to path3
       
        (setq direction (third (assoc tdate dates))
            direction-1 (third (assoc (getd tdate 'ydate) dates))
            label (second (assoc tdate dates))
            wwlabel (fourth (assoc tdate dates))
            pw-pattern (fifth (assoc tdate dates))
            ct-score (sixth (assoc tdate dates))
            trading-degree (seventh (assoc tdate dates))
            ) 
;;;;Must check if there is a change in direction at the trading degree
;;;set signal to true if it is
         (if (neql direction-1 direction) 
             (setq signal T) (setq signal nil))	
	 
	 (if (and (eql direction-1 1) (lte (getd tdate 'low) prev-stop))
	     (setq stopped-outp T))
	 (if (and (eql direction-1 -1) (gte (getd tdate 'high) prev-stop))
	     (setq stopped-outp T))   
	     
        (multiple-value-setq (entry-short entry-long)
                (vprices tdate 18 5.55 1)) 
         (setq risk (* .5 (- entry-long entry-short)))       
	 
	 (if (and signal (member (list direction label wwlabel pw-pattern) winners-long :test #'equal))
	    (setq  position 1 entry-date tdate stop (- (getd tdate 'close) risk)
	           stopped-outp nil)) 
	 (if (and signal (member (list direction label wwlabel pw-pattern) winners-short :test #'equal))
	    (setq  position -1 entry-date tdate  stop (+ (getd tdate 'close) risk)
	           stopped-outp nil)) 
        (if (and signal (member (list direction label wwlabel pw-pattern) avoids :test #'equal))
	    (setq position 0 entry-date nil stop nil stopped-outp nil))   
        (if (and signal (not (member (list direction label wwlabel pw-pattern)
                                      (append winners-long winners-short avoids) :test #'equal)))
	    (setq position 0 entry-date nil stop nil stopped-outp nil))   
       
         (cond ((and (eql prev-position 1) (not signal)(not stopped-outp))
                (setq position 1 entry-date prev-entry-date stop (fmax prev-stop entry-short)))
               ((and (eql prev-position 1) (not signal) stopped-outp)
                (setq position 0 entry-date nil stop nil))
               ((and (eql prev-position -1) (not signal)(not stopped-outp))
                (setq position -1 entry-date prev-entry-date
                      stop (fmin prev-stop entry-long)))
               ((and (eql prev-position -1) (not signal) stopped-outp)
                (setq position 0 entry-date prev-entry-date stop nil))
               ((and (eql prev-position 0) (not signal))
                 (setq position 0 entry-date nil stop nil))                
               ((and (not prev-position)(not position))
                (setq position 0 entry-date nil stop nil))
                )
	; (format T "market= ~A signal= ~A position= ~A prev-position= ~A~%" market signal position prev-position)
	 (setq trade-action
	     (cond ((and signal (eql position 1)(eql prev-position -1))
	            "Reverse to Long")
	           ((and signal (eql position -1)(eql prev-position 1))
	            "Reverse to Short")
	           ((and signal (eql position 1)(eql prev-position 0))
	            "Enter Long")
	           ((and signal (eql position -1)(eql prev-position 0))
	            "Enter Short")
	           ((and signal (eql position 0)(eql prev-position 1))
	            "Exit Long")                 
            (format stream "  ~5A " (contract-month market tdate))
          
            (if (member *data-name* '(US.D1B TY.D1B))
                (format stream "~9@A   " (convert-to-32nds (getd tdate 'close)))                
               (format stream directive 
                     (* (index-tick-size) (round (getd tdate 'close) (index-tick-size)))))
           
           (if (numberp stop)
               (if (member *data-name* '(ty.d1b us.d1b))
                   (format stream "~2@A ~2@A  ~2@A  ~A ~8@A  ~8@A ~D" direction label wwlabel
                           (if signal "*" " ")  trade-action (convert-to-32nds stop) equity)
                 (format stream "~2@A ~2@A  ~2@A  ~A ~8@A  ~8F ~D" direction label wwlabel
                   (if signal "*" " ")     trade-action stop equity))
              (format stream "~2@A  ~2A ~A ~2@A ~8@A ~A" direction label wwlabel
                      (if signal "*" " ")  trade-action stop))
                   
                
          );closes the stream
          );closes the dolist over markets
     
;;;now write out the updated positions file called ewaves-                   
         (with-open-file (output3 path-ewaves-trade-history :direction :output :if-exists :supersede :if-does-not-exist :create)
            (dolist (ith positions)
             (format output3 "~S~%" ith))
                 );;;closes output3
     
      ; (shell "cp ~/cycles/ewaves-3-report.dat /home/mk-data/luis")
        
  )) ;;closes the let and defun            
   
|#            
;;;;this function will remove duplicate S-expressions from a file 
;;;of S-expressions. 
;;;It was written for trades in the warehouse but is actually much more general.
  
(defun remove-duplicate-trades (path)
   (let (trades)
 
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record trades)))
   (setq trades (remove-duplicates trades :test #'equal))
 
   (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith trades)
        (format stream "~S~%" ith)))

))


;;;this function finds all the trades that are the same market
;;; and the same start date. They are written out to a file. No
;;; trades are removed.
   
(defun find-bad-trades (path)
   (let (trades (path1 "~/cycles/bad-trades.dat")
         bad-ones)
 
    (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record trades)))
   
    (dolist (ith trades)
      (dolist (jth trades)
       (if (and (not (equal ith jth))
                (eql (nth 0 ith)(nth 0 jth))
                (eql (nth 1 ith)(nth 1 jth)))
          (push ith bad-ones))))
   
 
   (with-open-file (stream path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (ith bad-ones)
        (format stream "~S~%" ith)))
;   (print bad-ones)
))
;;;;path is the ewavestradewarehouse file.
;;;;this function creates the trade memory file needed for T89  
;;;the min-trades is the minimum number of trades in a bin for acceptance

(defun make-trade-memory (path  &optional (min-trades 1))
  (let (contents (result 0) (long-winners nil)(short-winners nil)
       (counter 0) (only-one 0) (avoids nil) unfiltered-trades
       ewaves-bin-codes bin (twr 1) score-code (uncertains nil)
       (ewaves-bins-table (make-hash-table :test #'equal))
       (path4 "~/cycles/ewaves-bins.dat"))

;;;first read in the warehouse89 file       
     (with-open-file (str path :direction :input)
      (do ((record (read str nil 'eof) (read str nil 'eof)))
          ((eql record 'eof))      
       (push record unfiltered-trades)))    

;;;;now fill the hash table with bins as the key and trades into each bin  
;;;;each trade is a record in  the warehouse 
;;;direction is (nth 2 record)  
;;;label is (nth 4 record)
;;;wwlabel is (nth 6 record)
;;;(nth 19 kth) is the P&L for the trade 
;;;counter is variable that keeps track of the number of trades in a bin
;;;(nth 3 kth) is the start price of the trade
;;;(nth 18 kth) is the end price of the trade
       
   (dolist (record unfiltered-trades)
   ;;;must encode the score into 5 levels; other indicators do not need encoding
    (setq score-code
     (cond ((> (nth 8 record) 0) 0)
              ((and (> (nth 8 record) -5)(<= (nth 8 record) 0)) -1)
              ((and (> (nth 8 record) -10)(<= (nth 8 record) -5)) -2)
              ((and (> (nth 8 record) -15)(<= (nth 8 record)-10)) -3) 
              ((<= (nth 8 record) -15) -4)))
              
     ;;;create 5 indicator bins
     (setq bin (list (nth 2 record)(nth 4 record)(nth 6 record)(nth 7 record) score-code))
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin ewaves-bins-table)
            (ifn (member record (gethash bin ewaves-bins-table) :test #'equal)
                 (setf (gethash bin ewaves-bins-table)
                       (cons record (gethash bin ewaves-bins-table)))))
           ((not (gethash bin ewaves-bins-table))
             (setf (gethash bin ewaves-bins-table)(list record))))
              
  ;;;create 4 indicator bins  label removed
     (setq bin (list (nth 2 record)(nth 6 record)(nth 7 record) score-code))
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin ewaves-bins-table)
            (ifn (member record (gethash bin ewaves-bins-table) :test #'equal)
                 (setf (gethash bin ewaves-bins-table)
                       (cons record (gethash bin ewaves-bins-table)))))
           ((not (gethash bin ewaves-bins-table))
             (setf (gethash bin ewaves-bins-table)(list record))))          
              

 ;;;create 3 indicator bins  label and score removed    
     (setq bin (list (nth 2 record)(nth 6 record)(nth 7 record)))
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin ewaves-bins-table)
            (ifn (member record (gethash bin ewaves-bins-table) :test #'equal)
                 (setf (gethash bin ewaves-bins-table)
                       (cons record (gethash bin ewaves-bins-table)))))
           ((not (gethash bin ewaves-bins-table))
             (setf (gethash bin ewaves-bins-table)(list record))))          
                      
 ;;;create 2 indicator bins  label, score and pwpattern removed
     (setq bin (list (nth 2 record)(nth 6 record)))
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin ewaves-bins-table)
            (ifn (member record (gethash bin ewaves-bins-table) :test #'equal)
                 (setf (gethash bin ewaves-bins-table)
                       (cons record (gethash bin ewaves-bins-table)))))
           ((not (gethash bin ewaves-bins-table))
             (setf (gethash bin ewaves-bins-table)(list record))))          
                                               
  ;;;create 1 indicator bins  label, score, pwpattern, wwlabel removed
     (setq bin (list (nth 2 record)))
     (pushnew bin ewaves-bin-codes :test #'equal)
     (cond ((gethash bin ewaves-bins-table)
            (ifn (member record (gethash bin ewaves-bins-table) :test #'equal)
                 (setf (gethash bin ewaves-bins-table)
                       (cons record (gethash bin ewaves-bins-table)))))
           ((not (gethash bin ewaves-bins-table))
             (setf (gethash bin ewaves-bins-table)(list record))))         
        
        
        
              ) ;;closes the dolist over unfiltered trades  

;;;put the bins into four lists that have combinations of all 5 lengths       
   (dolist (ith ewaves-bin-codes)
     (setq contents (gethash ith ewaves-bins-table));;;contents is a list of all trades for a bin
     (setq result 0 counter 0 twr 1)
     (if (= (length contents) 1) (incf only-one));;;keeps track of number of bins with only one trade
     (dolist (kth contents)          
         (setq result (+ result (nth 19 kth))) (incf counter) ;;;result is the total profit/loss for the bin
         (setq twr (* twr (+ 1 (/ (if (plusp (nth 2 kth))
                                      (- (nth 18 kth) (nth 3 kth))
                                     (- (nth 3 kth) (nth 18 kth)))
                                  (nth 3 kth)))
         ))          ;;;twr is the compound total return for the bin
        ) ;;;closes dolist over contents 
     (if (zerop counter) (print ith)) ;;;all bins should have at least one trade
     ;;;the cond below is the logic for filtering the trades
     ;;;;separates them into four lists 
     ;;; first is the list of acceptable long bins called long-winners
     ;;; second is the list of acceptable short bins called short-winners
     ;;;third is the list of bins with too few trades in the warehouse called uncertains
     ;;;fourth is the list of the unacceptable bins 
     (cond ((and (plusp (car ith))(>= (/ result counter) 100)   ;;;(car ith) is the direction
                 (> twr 1);;;terminal wealth return be greater than 1 (positive % change)
                 (>= counter min-trades);;;the bin must have at least min-trades to be acceptable
                 )
                 (push ith long-winners))
            ((and (minusp (car ith))(>= (/ result counter) 100)
                  (> twr 1)
                  (>= counter min-trades)
                  )
                 (push ith short-winners))
            ((< counter min-trades)(push ith uncertains))     
                 
            (t (push ith avoids)))  
         
        );;;closes dolist over ewaves-bin-codes

;;;;now create the trade memory file for T89
    (with-open-file (stream path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format stream "~S~%~%~S~%~%~S~%~%~S" long-winners short-winners avoids uncertains)) 
))  



;;;for rule 1 finds the rollover month  
;;;given market it finds a rollover date for each contract month that is expiring
;;;for the current year so a date is needed 
(defun find-rollover-dates (market date param1)
    (let (num-days-per-month  
          (month-list (cdr (assoc market *M-list*)))
           num-days-in-month rollover-dates dy
           pm (ctr 0)(yr (getnumyear date))) 
  
   (setq num-days-per-month '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                (4 . 30) (5 . 31) (6 . 30)
                                (7 . 31) (8 . 31) (9 . 30)
                                (10 . 31) (11 . 30) (12 . 31)))
    (if (leap-yearp yr)
        (setf (cdr (assoc 2 num-days-per-month)) 29)
        (setf (cdr (assoc 2 num-days-per-month)) 28))
  
    (dolist (ith month-list)
     ;;;; this is for rule 1 the X weekdays into the previous month 
    ;;;find the previous month
       (setq pm (1- ith))
       (cond ((zerop pm)(setq pm 12 yr (1- yr)))
             (t (setq yr (getnumyear date))))
    ;;; now count the weekdays backward 
       (setq ctr 0);(print num-days-per-month)
       (setq num-days-in-month (cdr (assoc pm num-days-per-month)))
      ; (print num-days-in-month)
       (dotimes (kth num-days-in-month) 
         (setq dy (make-edate yr pm (- num-days-in-month kth)))
        ; (print dy)
         (if (week-day-p dy) (incf ctr))  
         (when (= ctr param1)
               (setq rollover-dates (acons ith dy rollover-dates))
               (return dy)))
               
               );;closes the dolist
     
     ; (print rollover-dates)
      rollover-dates
    )) 
    

;;;for rule 2 finds the rollover month  
;;;given market it finds a rollover date for each contract month that is expiring
;;;for the current year so a date is needed 
(defun find-rollover-dates2 (market date)
    (let (num-days-per-month  
          (month-list (cdr (assoc market *M-list*)))
           num-days-in-month rollover-dates dy
           (ctr 0)(yr (getnumyear date)))
   (setq num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                       (4 . 30) (5 . 31) (6 . 30)
                                       (7 . 31) (8 . 31) (9 . 30)
                                       (10 . 31) (11 . 30) (12 . 31)))           
    (if (leap-yearp yr)
        (setf (cdr (assoc 2 num-days-per-month)) 29)
        (setf (cdr (assoc 2 num-days-per-month)) 28))
  
    (dolist (ith month-list)
     ;;;; this is for rule2 the X weekdays into the current month 
  
    
    ;;; now count the weekdays backward 
       (setq ctr 0)
       (setq num-days-in-month (cdr (assoc ith num-days-per-month)))

       (dotimes (kth num-days-in-month) 
         (setq dy (make-edate yr ith (1+ kth)))
         (if (and (equal (day-of-week dy) "MONDAY")
                  (>= (1+ kth) 4)) 
            (incf ctr))  
         (if (and (equal (day-of-week dy) "TUESDAY")
                  (>= (1+ kth) 5)) 
            (incf ctr))  
         (when (>= ctr 2)
               (setq rollover-dates (acons ith (+ dy 3) rollover-dates))
               (return dy)))
               
               );;closes the dolist

     ; (print rollover-dates)
      rollover-dates
    )) 
 
 
;;;for rule8 finds the rollover month  
;;;given market it finds a rollover date for each contract month that is expiring
;;;for the current year so a date is needed 
(defun find-rollover-dates3 (market date param1)
    (let (num-days-per-month  (month-list (cdr (assoc market *M-list*)))
         num-days-in-month rollover-dates dy 
          (ctr 0)(yr (getnumyear date)))  
         
          (setq num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                      (4 . 30) (5 . 31) (6 . 30)
                                      (7 . 31) (8 . 31) (9 . 30)
                                      (10 . 31) (11 . 30) (12 . 31)))

     
    (if (leap-yearp yr)
        (setf (cdr (assoc 2 num-days-per-month)) 29)
       (setf (cdr (assoc 2 num-days-per-month)) 28))
    
     (dolist (ith month-list)
     ;;;; this is for rule 8 the X weekdays into the current month 
      
    ;;; now count the weekdays backward 
       (setq ctr 0)
       (setq num-days-in-month (cdr (assoc ith num-days-per-month)))

       (dotimes (kth num-days-in-month) 
         (setq dy (make-edate yr ith (1+ kth)))
         (if (week-day-p dy) (incf ctr))  
         (when (>= ctr param1)
               (setq rollover-dates (acons ith dy rollover-dates))
               (return dy)))
               
               );;closes the dolist
     
     ; (print rollover-dates)
      rollover-dates
    )) 
    
 
;;;for rule 10 finds the rollover month  
;;;given market it finds a rollover date for each contract month that is expiring
;;;for the current year so a date is needed 
(defun find-rollover-dates4 (market date param1)
    (let (num-days-per-month  (month-list (cdr (assoc market *M-list*)))
         num-days-in-month rollover-dates dy 
          (ctr 0)(yr (getnumyear date)))  
         
          (setq num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                      (4 . 30) (5 . 31) (6 . 30)
                                      (7 . 31) (8 . 31) (9 . 30)
                                      (10 . 31) (11 . 30) (12 . 31)))

 
       
    (if (leap-yearp yr)
        (setf (cdr (assoc 2 num-days-per-month)) 29)
      (setf (cdr (assoc 2 num-days-per-month)) 28))

    (dolist (ith month-list)
     
      
    ;;; now count the weekdays backward 
       (setq ctr 0)
       (setq num-days-in-month (cdr (assoc ith num-days-per-month)))

       (dotimes (kth num-days-in-month) 
         (setq dy (make-edate yr ith (- num-days-in-month kth)))
         (if (week-day-p dy) (incf ctr))  
         (when (>= ctr param1)
               (setq rollover-dates (acons ith dy rollover-dates))
               (return dy)))
               
               );;closes the dolist
     
     ; (print rollover-dates)
      rollover-dates
    )) 
    

;;;finds the third wednesday of the month for deu.d1b and dbp.d1b

 
;;;for rule 11 finds the rollover month  
;;;given market it finds a rollover date for each contract month that is expiring
;;;for the current year so a date is needed 
;;;looks for dates X days before the third Wednesday of the rollover month.
(defun find-rollover-dates5 (market date param1)
    (let (num-days-per-month  (month-list (cdr (assoc market *M-list*)))
         num-days-in-month rollover-dates dy 
          (ctr 0)(yr (getnumyear date)))  
         
          (setq num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                      (4 . 30) (5 . 31) (6 . 30)
                                      (7 . 31) (8 . 31) (9 . 30)
                                      (10 . 31) (11 . 30) (12 . 31)))

        
    (if (leap-yearp yr)
        (setf (cdr (assoc 2 num-days-per-month)) 29)
       (setf (cdr (assoc 2 num-days-per-month)) 28))
 
    (dolist (ith month-list)
     ;;;; this is for rule 11 the param1 weekdays before the third Wednesday of the current month 
      
    ;;; now count the weekdays backward 
       (setq ctr 0)
       (setq num-days-in-month (cdr (assoc ith num-days-per-month)))

       (dotimes (kth num-days-in-month) 
         (setq dy (make-edate yr ith (1+ kth)))
         (if (equal (day-of-week dy) "WEDNESDAY") (incf ctr))  
         (when (>= ctr 3)
              (setq dy (add-days-to-date1 dy param1));;;finds the Friday before the third Wednesday
              (setq dy (add-days-to-date1 dy                  ;;;adjusts for holidays in Dubai
                              (cond ((and (eql (getnummonth dy) 9)
                                          (eql (getnumday dy) 11)) -1)
                                    ((and (eql (getnummonth dy) 9)
                                           (eql (getnumday dy) 12)) -2)
                                    ((and (eql (getnummonth dy) 12)
                                          (eql (getnumday dy) 12)) -1)
                                     (t 0))))

               (setq rollover-dates (acons ith dy rollover-dates))
               (return dy)))
               
               );;closes the dolist
     
     ; (print rollover-dates)
      rollover-dates
    )) 
 
;;;for rule 12 finds the rollover month  
;;;given market it finds a rollover date for each contract month that is expiring
;;;for the current year so a date is needed 
;;;looks for dates X days before the last Thursday of the rollover month.
(defun find-rollover-dates6 (market date param1)
    (let (num-days-per-month  (month-list (cdr (assoc market *M-list*)))
         num-days-in-month rollover-dates dy 
          (ctr 0)(yr (getnumyear date)))  
         
          (setq num-days-per-month  '((0 . 0) (1 . 31) (2 . 28) (3 . 31)
                                      (4 . 30) (5 . 31) (6 . 30)
                                      (7 . 31) (8 . 31) (9 . 30)
                                      (10 . 31) (11 . 30) (12 . 31)))

  
    (if (leap-yearp yr)
        (setf (cdr (assoc 2 num-days-per-month)) 29)
        (setf (cdr (assoc 2 num-days-per-month)) 28))
  
    (dolist (ith month-list)
     ;;;; this is for rule 12 the param1 weekdays before the last Thursday of the current month 
      
    ;;; now count the weekdays backward 
       (setq ctr 0)
       (setq num-days-in-month (cdr (assoc ith num-days-per-month)))

       (dotimes (kth num-days-in-month) 
         (setq dy (make-edate yr ith (- num-days-in-month kth)))
         (if (equal (day-of-week dy) "THURSDAY") (incf ctr))  
         (when (>= ctr 1)
              (setq dy (add-days-to-date1 dy param1));;;finds the Wednesday before the last Thursday
              (setq dy (add-days-to-date1 dy                  ;;;adjusts for holidays in Dubai
                              (cond ((and (eql (getnummonth dy) 9)
                                          (eql (getnumday dy) 11)) -1)
                                    ((and (eql (getnummonth dy) 9)
                                           (eql (getnumday dy) 12)) -2)
                                    ((and (eql (getnummonth dy) 12)
                                          (eql (getnumday dy) 12)) -1)
                                    ((and (eql (getnummonth dy) 12)
                                          (eql (getnumday dy) 25)) -1)
                                 
                                       (t 0))))

               (setq rollover-dates (acons ith dy rollover-dates))
               (return dy)))
               
               );;closes the dolist
     
     ; (print rollover-dates)
      rollover-dates
    )) 
    
      
;;;these are the contract months to trade for each futures market
 
(defparameter *M-list* 
    '(
    (dj.d1b 3 6 9 12)
    (sp.d1b 3 6 9 12)
    (nd.d1b 3 6 9 12)
    (ru.d1b 3 6 9 12)
    (nk.d1b 3 6 9 12)
    
    (ty.d1b 3 6 9 12)
    (us.d1b 3 6 9 12)
    
    (gc.d1b 2 4 6 8 12)
    (si.d1b 3 5 7 9 12)
    (cp.d1b 3 5 7 9 12)
    
    (pa.d1b 3 6 9 12)
    (pl.d1b 1 4 7 10)
  
    (ad.d1b 3 6 9 12)
    (bp.d1b 3 6 9 12)
    (cd.d1b 3 6 9 12)
    (e1.d1b 3 6 9 12)
    (jy.d1b 3 6 9 12)
    
    (mx.d1b 3 6 9 12)
    (nz.d1b 3 6 9 12)   
    (sf.d1b 3 6 9 12)
        
   
    (cl.d1b 1 2 3 4 5 6 7 8 9 10 11 12)
    (cl1.d1b 1 2 3 4 5 6 7 8 9 10 11 12)
    (ho.d1b 1 2 3 4 5 6 7 8 9 10 11 12)
    (hu.d1b 1 2 3 4 5 6 7 8 9 10 11 12)
   
    (ng.d1b 1 2 3 4 5 6 7 8 9 10 11 12)     
    
    (ct.d1b 3 5 7 12)
    (cc.d1b 3 5 7 9 12)
    (cf.d1b 3 5 7 9 12)
    
    (lb.d1b 1 3 5 7 9 11)
    (oj.d1b 1 3 5 7 9 11)
   
    (su.d1b 3 5 7 10)
     
    (c.d1b 3 5 7  12)    
    (s.d1b 1 3 5 7  11)
    (bo.d1b 1 3 5 7  12)
    (sm.d1b 1 3 5 7  12)
    (w.d1b 3 5 7 9 12)
    
    (lc.d1b 2 4 6 8 10 12)
    (lh.d1b 2 4 6 7 8 10 12)   

    (dinr.d1b 1 2 3 4 5 6 7 8 9 10 11 12)
    (dbp.d1b 3 6 9 12)
    (deu.d1b 3 6 9 12)
    (ddg.d1b 2 4 6 8 10 12 )
    (dbsx.d1b 1 2 3 4 5 6 7 8 9 10 11 12)
#|    
;;;cash currencies    
    (aud.d3b 4 "CASH" "AUD-USD" .0001) 
    (brl.d3b 4 "CASH" "USD-BRL" .0001)
    (gbp.d3b 4 "CASH" "GBP-USD" .0001)
    (gbpusd.d3b 4 "CASH" "GBP-USD" .0001);;;EWI symbol
    (cad.d3b 4 "CASH" "USD-CAD" .0001)
    (eur.d3b 4 "CASH" "EUR-USD" .0001)
    (jpy.d3b 2 "CASH" "USD-JPY" .01)
    (chf.d3b 4 "CASH" "USD-CHF" .0001)
    (mxn.d3b 4 "CASH" "USD-MXN" .0001)
    
    (cny.d3b 4 "CASH" "USD-CNY" .0001)
    (inr.d3b 4 "CASH" "USD-INR" .0025)
    (rub.d3b 4 "CASH" "USD-RUB" .0001)
    
    (chfjpy.d3b 2 "CASH" "CHF-JPY" .01)
    
    (euraud.d3b 4 "CASH" "EUR-AUD" .0001)
    (eurgbp.d3b 4 "CASH" "EUR-GBP" .0001)
    (eurjpy.d3b 2 "CASH" "EUR-JPY" .01)
    (eurchf.d3b 4 "CASH" "EUR-CHF" .0001)
     
    (gbpjpy.d3b 2 "CASH" "GBP-JPY" .01) 

 ;;;;stocks   
    (aa.d3b 2 "CASH" "ALCOA" .01) 
    (aapl.d3b 2 "CASH" "APPLE" .01)
    (adbe.d3b 2 "CASH" "ADOBE SYSTEMS" .01)
    (amzn.d3b 2 "CASH" "AMAZON.COM" .01)
    (bac.d3b 2 "CASH" "BANK OF AMERICA CORP" .01)
    
    (brk_b.d3b 2 "CASH" "BERKSHIRE HATHAWAY B" .01)
    (cvx.d3b 2 "CASH" "CHEVRON CORP" .01)
    (dd.d3b 2 "CASH" "DUPONT CO" .01)
    (dis.d3b 2 "CASH" "DISNEY" .01)
    (fosl.d3b 2 "CASH" "FOSSIL" .01)
  
    (gd.d3b 2 "CASH" "GENERAL DYNAMICS CORP" .01)
    (ge.d3b 2 "CASH" "GENERAL ELECTRIC" .01)
    (goog.d3b 2 "CASH" "GOOGLE" .01)
    (ibm.d3b 2 "CASH" "IBM" .01)
    (jnj.d3b 2 "CASH" "JOHNSON & JOHNSON" .01)
   
    (jpm.d3b 2 "CASH" "J P MORGAN CHASE" .01)
    (ko.d3b 2 "CASH" "COCA COLA CO" .01)
    (mrk.d3b 2 "CASH" "MERCK" .01)
    (msft.d3b 2 "CASH" "MICROSOFT CORP" .01)
    (orcl.d3b 2 "CASH" "ORACLE" .01)
     
    (pfe.d3b 2 "CASH" "PFIZER" .01)
    (pg.d3b 2 "CASH" "PROCTOR & GAMBLE" .01) 
    (pot.d3b 2 "CASH" "POTASH" .01)  
    (slb.d3b 2 "CASH" "SHLUMBERGER" .01)
    (spy.d3b 2 "CASH" "S&P 500 SPDR" .01)
  
    (t.d3b 2 "CASH" "AT&T" .01)
    (vz.d3b 2 "CASH" "VERIZON" .01)
    (wfc.d3b 2 "CASH" "WELLS FARGO" .01)
    (wmt.d3b 2 "CASH" "WAL-MART" .01)
 |#   
    
    ))   

   

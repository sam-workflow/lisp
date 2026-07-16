;;;;; -*- Mode: LISP; Package: user; Base: 10
#+:SBCL (in-package :common-lisp-user)
;;;the function barchart-conversion reads and converts the watchlist from barchart which contains
;;;the markets used by Ilan Levy-meyer. It has the daily open high low and close prices
;;;and volume and the barchart symbol for a single day
;;;at he common lisp prompt type (barchart-conversion date) where date is the current day you are entering.

;;;Do no confuse barchart-conversion with the file barchart-converter.lisp.
;;;;barchart-converter.lisp is a standalone program that is run in batch mode for
;;;repair/update a specific market and month.
;;;reuters-SBCL-converter.lisp and barchart-converter.lisp are in the Reuters-SBCL-converter directory
;;; c:\users\ep-da\Reuters-SBCL-converter is the path to them.

(defparameter *cannon-list* '(ZB NM ET CY))
(defparameter *barchart-dir* "C:/Users/ep-da/Downloads/")

(require "asdf")

(defun newest-file (dir)
  (car
   (sort
    (remove-if #'uiop:directory-pathname-p
               (directory (merge-pathnames "*.*" dir)))
    #'>
    :key #'file-write-date)))

(defun read-newest-download ()
  (with-open-file (in (newest-file #P"C:/Users/ep-da/Downloads/"))
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun parse-csv-line (line)
  (loop with field = ""
         with fields = '()
        with in-quotes = nil
        for ch across line
        do (cond
             ((char= ch #\")
              (setf in-quotes (not in-quotes)))
             ((and (char= ch #\,) (not in-quotes))
              (push field fields)
              (setf field ""))
             (t
              (setf field
                    (concatenate 'string field (string ch)))))
        finally
          (push field fields)
        (return (nreverse fields))))

(defun maybe-number (s)
  (handler-case
      (read-from-string s)
    (error () s)))

(defun convert-barchart-data (lines)
  (loop for line in lines
        for fields = (parse-csv-line line)

        ;; Only keep real market rows
        when (= (length fields) 10)

        collect
        (list
         ;; symbol
         (nth 0 fields)

         ;; open
         (maybe-number (nth 5 fields))

         ;; high
         (maybe-number (nth 6 fields))

         ;; low
         (maybe-number (nth 7 fields))

         ;; last
         (maybe-number (nth 2 fields))

         ;; volume
         (maybe-number (nth 8 fields)))))


;;;this fixes the next day number like (1 . 3) for a weekend
(defun market-data-adjustment (market tdate)
   (let (data month-path month  (days-apart 1) most-recent-day record)
   (set-market market)
   (setq month (get-latest-index-date))
   (setq month-path (string-append *database-upper-dir*
                                  (format nil "~(~A~)/" market)
                                  (format nil "d~A.dat" month)))
   (with-open-file (strm month-path :direction :input)
            (do ((record (read strm nil 'eof) (read strm nil 'eof)))
                ((eql record 'eof)) 
                 (push record data))
                 );;closes the with-open-file  
  ;  (format t "~%data= ~A~%" data)            
   (if (probe-file month-path)(delete-file month-path))             
;;;open is 16
;;;high is 17
;;;low  is  18
;;;close is 11  
;;;rollover is 19
;;;next day is 1
;;;priorday is 0
   
   (setq record (assoc (setq  most-recent-day (car (last (month-days (get-latest-index-date))))) data))
  ; (format t "~%most-recent-day= ~A record= ~A~%" most-recent-day record)
   (setq days-apart (- tdate most-recent-day))
   (if (cdr (assoc 1 (cdr record)))(setf (cdr (assoc 1 (cdr record))) days-apart))
   (ifn (cdr (assoc 1 (cdr record))) (setf (cdr record)(append (cdr record) (list `(1 . ,days-apart)))))
  ; (format t "~%~A~%" record)
                                       ;  (* factor (cdr (assoc 1 (cdr record))))))
       ; (if (cdr (assoc 16 (cdr record)))(setf (cdr (assoc 16 (cdr record))) 
         ;                                (* factor (cdr (assoc 16 (cdr record))))))
       ;; (if (cdr (assoc 17 (cdr record)))(setf (cdr (assoc 17 (cdr record))) 
          ;                               (* factor (cdr (assoc 17 (cdr record)))))) 
       ; (if (cdr (assoc 18 (cdr record)))(setf (cdr (assoc 18 (cdr record))) 
       ;                                  (* factor (cdr (assoc 18 (cdr record))))))
       ; (if (cdr (assoc 19 (cdr record)))(setf (cdr (assoc 19 (cdr record))) 
        ;                                 (* factor (cdr (assoc 19 (cdr record)))))) 
       ; (if (assoc 11 (cdr record))(setf (cadr (assoc 11 (cdr record))) 
        ;                           (* factor (cadr (assoc 11 (cdr record))))))
       ;  ) );;closes the dolist
        
    (setq data (reverse data))
    (with-open-file (strm month-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (record data)
        (format strm "~A~%" record)))
    days-apart
;    (setq month (next-month month) data nil)
 ;   (if (> month month-last)(return))
;     );closes the loop

));;closes the let and the defun
  

(defun barchart-conversion (tdate)
  (let (market sym data-list open high low last vol path1 path2 days-apart day-line data month
	       )
  (setq data-list (cdr  (convert-barchart-data (read-newest-download))))
  (dolist (ith data-list)
    (setq sym (car ith))
    (setq sym (subseq sym 0 (- (length sym) 3)))
    (setq market (car (rassoc (read-from-string sym) *barchart-symbol*)))
    (setf (car ith) market)
	    )
  (format t "~%~A~%" data-list)
  
  
;;;;now convert the data list to look like a line in exitpoints database
  (dolist (jth data-list)
       
       (setq data nil)
       (setq market (first jth) open (second jth) high (third jth) low (fourth jth) last (fifth jth) vol (sixth jth))
       (set-market market)(setq days-apart (market-data-adjustment  market tdate))
     
       (setq month (get-latest-index-date))
       (setq day-line
	     `(,tdate (16 . ,open)(17 . ,high)(18 . ,low)(11 ,last)(19)(20 . ,vol)(21 . 0)(0 . ,days-apart)(1 . 1)))
   
       (setq path1
       (string-append *database-upper-dir*
                                  (format nil "~(~A~)/" market)
                                  (format nil "d~A.dat" month)))
       (setq path2
       (string-append *database-upper-dir*
                                  (format nil "~(~A~)/" market)
                                  (format nil "d~A.dat" (next-month month))))
     ;  (format t "day-line= ~A~%" day-line)
      ; (format t "~%path2= ~A~%" path2)
       ;(format t "~%days-apart= ~A~%" days-apart)
       
       (with-open-file (strm path1 :direction :input)
            (do ((record (read strm nil 'eof) (read strm nil 'eof)))
                ((eql record 'eof)) 
                 (push record data))
            );;closes the with-open-file
       (unless (neql (nummonth (car (last (month-days month)))) (nummonth tdate)) 
               (push day-line data) ;(format t "~%~A~%" data)
               (setq data (reverse data))
               (with-open-file (strm path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
                  (dolist (record data)
	             (let ((*print-right-margin* 80))
                          (format strm "~A~%" record))))
               );;;closes the unless
       (when (neql (nummonth (car (last (month-days month)))) (nummonth tdate)) 
	 (setq data (reverse data))
	 (setq data (subseq data 0 3))
	 (setq data (append data (list day-line)))
;	 (format t "~%~A~%" data)
          (with-open-file (strm path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
                  (dolist (record data)
	             (let ((*print-right-margin* 80))
                          (format strm "~A~%" record)))))
       
       (reset-all-indexes `(,market))
        )
       
  
  ))

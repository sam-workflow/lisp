
;;; -*- Mode: LISP; Package: user; Base: 10. -*-


;;;this writes the day , swing , and open positions records to the oec.csv file.
;;;flag is either GTC or DAY
;;;;this writes one order on one single line.
;;; Main order: Buying 1 ESH8 Limit 1290.00 GTC
;;; True:DEM086272;Buy;1;ESH8;Limit;GTC;1290;1309.25;default;default;0;OSO1
#|
(defun write-oec-record (output-oec enter-park sector direction qty oec-symbol order-type flag price entry-time cancel-time status)


   (format output-oec "~A\;~A\;~A\;~A\;~A\;~A\;~A\;" enter-park sector direction qty oec-symbol order-type flag) ;;;first 7 fields
 ;;;the price is in field 8.
   (cond ((not (numberp price))
           (format output-oec "~A" price))
         ((not (nth 1 (assoc *data-name* *C-list*)))
           (format output-oec "~A" (convert-to-oec-32nds price)))
         (t
           (format output-oec
            (cond ((and (nth 1 (assoc *data-name* *C-list*))
                       (zerop (nth 1 (assoc *data-name* *C-list*)))) "~D")
                 (t (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *oec-price-conversion-factor*))) ",0,'*,' F")))
                    ;(string-append "~9," (format nil "~A" 4) ",0,'*,' F")))
           (* (cadr (assoc *data-name* *oec-price-conversion-factor*))
            (* (nth 4 (assoc *data-name* *C-list*)) (round price (nth 4 (assoc *data-name* *C-list*)))))
                   )))

      (format output-oec "\;0") ;;;price 2 for field 9 is filled with a zero
     ;;;;add seconds if time is not Default
      (format output-oec "\;~A" entry-time);;adds field 10 the release time

      (format output-oec "\;~A" cancel-time);;;adds field 11 this is cancel time

     (format output-oec "\;0\;0\;None\;Unspecified\;\;~A~%" status) ;;;adds fields 12,13,14,15 and 16.


 )
|#

(defun write-oec-record (output-oec enter-park sector direction qty oec-symbol order-type flag price entry-time cancel-time status)


   (format output-oec "~A\;~A\;~A\;~A\;~A\;~A\;~A\;" enter-park sector direction qty oec-symbol order-type flag) ;;;first 7 fields
 ;;;the price is in field 8.
   (cond ((not (numberp price))
           (format output-oec "~A" price))
         ;((not (index-digits))
          ((eql (index-digits) 5)
           (format output-oec "~A" (convert-to-oec-32nds price)))
         (t
           (format output-oec
            (cond ((and (index-digits)
                       (zerop (index-digits))) "~D")
                 (t (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *oec-price-conversion-factor*))) ",0,'*,' F")))
                    ;(string-append "~9," (format nil "~A" 4) ",0,'*,' F")))
           (* (cadr (assoc *data-name* *oec-price-conversion-factor*))
            (* (index-tick-size) (round price (index-tick-size))))
                   )))

      (format output-oec "\;0") ;;;price 2 for field 9 is filled with a zero
     ;;;;add seconds if time is not Default
      (format output-oec "\;~A" entry-time);;adds field 10 the release time

      (format output-oec "\;~A" cancel-time);;;adds field 11 this is cancel time

     (format output-oec "\;0\;0\;None\;Unspecified\;\;~A~%" status) ;;;adds fields 12,13,14,15 and 16.


 )


;;;given a time in 24 hour format "hh:mm" subtract xx minutes

(defun add-minutes (tim0 amt)
 (let ((colon-pos 0) (num-hours 0) (minutes 0) (tim1 (copy-seq tim0)) )
 (setq colon-pos (position #\: tim1))

 (setf (aref tim1 colon-pos) #\.)
 (setq num-hours (read-from-string tim1))

 (setq minutes (rem num-hours 1) minutes (/ (* 100 minutes) 60))

 (setq minutes (+ minutes (/  amt  60)))

 (setq num-hours (my-round (+ (truncate num-hours) minutes) 2))

 (if (>= num-hours 24)(setq num-hours (- num-hours 24)))
 (if (< num-hours 0)(setq num-hours (+ num-hours 24)))
;;convert from decimal to hours and minutes

 (setq num-hours (convert-to-60 num-hours))

 (setq tim1 (format nil "~A" num-hours))

 (setq colon-pos (position #\. tim1))

 (setf (aref tim1 colon-pos) #\:)
 (case colon-pos
   (1 (if (eql (length tim1) 3)(string-append "0" tim1 "0")(string-append "0" tim1)))
   (2 (if (eql (length tim1) 4) (string-append tim1 "0") tim1)))
 ) )


;;;given a time in 24 hour format "hh:mm:00"  add/subtract amt minutes
;;;replaves the seconds with a random number of seconds.
;;;this function is for exiting triumph11 triumph21 and EPIC trades at random times
;;;second argument is T for random nil for not random
(defun add-minutes1 (tim0 amt &optional (randm nil))
 (let ((colon-pos 0) (num-hours 0) (minutes 0) (tim1 (copy-seq tim0)) tim2 sec1)
   (setq sec1 (subseq tim1 (- (length tim0) 3) (length tim0)))
   (setq tim2 (subseq tim1 0 (- (length tim0) 3)))

   (if randm (setq sec1 (format nil ":~A~A" (random 6)(random 10))));;replaces seconds with random seconds

   (setq colon-pos (position #\: tim2))

 (setf (aref tim2 colon-pos) #\.)
 (setq num-hours (read-from-string tim2))

 (setq minutes (rem num-hours 1) minutes (/ (* 100 minutes) 60))

 (setq minutes (+ minutes (/  amt  60)))

 (setq num-hours (my-round (+ (truncate num-hours) minutes) 2))

 (if (>= num-hours 24)(setq num-hours (- num-hours 24)))
 (if (< num-hours 0)(setq num-hours (+ num-hours 24)))
;;convert from decimal to hours and minutes

 (setq num-hours (convert-to-60 num-hours))

 (setq tim2 (format nil "~A" num-hours))

 (setq colon-pos (position #\. tim2))

 (setf (aref tim2 colon-pos) #\:)
 (case colon-pos
   (1 (if (eql (length tim2) 3)(string-append "0" tim2 "0" sec1)(string-append "0" tim2 sec1)))
   (2 (if (eql (length tim2) 4) (string-append tim2 "0" sec1) (string-append tim2 sec1))))
 ) )



;;;from a time in 24 hour format "hh:mm:00"  use the hour part
;;;to use the date1 or the date prior for the release date

(defun get-release-date (date1)
 (let (tim0 tim2 (colon-pos 0) (num-hours 0))
  
  (setq tim0 
    (or (second (assoc *data-name* *foremost-market-times-list*)) "09:00:03"))
  (setq colon-pos (position #\: tim0))
  (setq tim2 (subseq tim0  0 colon-pos))
 
 (setq num-hours (read-from-string tim2))

 (if (>= num-hours 17) (add-days-to-date date1 -1) date1)

 ) )



;;;returns list of markets in the warehouse
(defun num-markets-in-warehouse (swings)
  (let (markets-in-training-data)
   (dolist (record swings)
      (pushnew (car record) markets-in-training-data)
      )
   markets-in-training-data))


(defun write-fore (cannon-fore-path3 oec-symbol fore-block-acct fore-qty direction risk date exit-time)
  (let (directive1)
    (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"))
    (with-open-file (str cannon-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create)
      (format str "~4A ~A ~7A at market for account= ~A  risk = " direction fore-qty oec-symbol fore-block-acct) 
      (format str directive1 risk)
      (format str " points or ~A ~A or $~A EXIT at ~A PT~%~%" (round (/ risk (index-tick-size)))
          (if (member *data-name* *forex-list*) "pips" "ticks")
            (round  (* risk fore-qty (calculate-point-value date)))  exit-time ))
))


(defun write-edge (edge-path  direction date1)
    
    (with-open-file (str edge-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format str "~20A ~15A ~5A~%"
                (index-lname)  (edge-contract-month (contract-month *data-name* (getnumdate date1)))
                 direction ))
 
)

(defun sample-edge (sample-edge-path)
  (let (records)
   (with-open-file (str sample-edge-path :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))
    (setq records (randomize-list records))
    (with-open-file (str sample-edge-path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (format str "Sample #EPEdge: tinyurl.com/okzpdyw~%")
     (dotimes (ith 2)
       (when records
          (format str "~A~%" (car records)))
       (setq records (cdr records))))
 
))


(defun edge-contract-month (contract-month)
  (let ((months '(("JAN" . "JANUARY")("FEB" . "FEBRUARY")("MAR" . "MARCH")("APR" . "APRIL")
                 ("MAY" . "MAY")("JUN" . "JUNE")("JUL" . "JULY")("AUG" . "AUGUST")("SEP" . "SEPTEMBER")
                 ("OCT" . "OCTOBER")("NOV" . "NOVEMBER")("DEC" . "DECEMBER"))))
       (if (equal contract-month "CASH") "CASH "
            (string-append (cdr (assoc (subseq contract-month 0 3) months :test 'equalp))
                           " 20" (subseq contract-month 3 5))
           )
))


(defun write-edge-record1 (edge-path direction  tdate)
   
     (with-open-file (str edge-path :direction :output :if-exists :append :if-does-not-exist :create)
    (if (eql direction 'Long) (format str "<tr style=\"background-color:rgba(0,255,0,0.25);\">~%")
           (format str "<tr style=\"background-color:rgba(255,0,0,0.25);\">~%"))
       (format str "<td>~A</td>~%"  (index-lname))
       (format str "<td style=\"text-align:center;\">~A</td>~%" (edge-contract-month (contract-month *data-name* (getnumdate tdate))))
       (format str "<td style=\"text-align:center;\">~A</td>~%" direction)
      ; (format str "<td style=\"text-align:center;\">~A</td>~%" (my-pretty-price risk))
      ; (format str "<td style=\"text-align:center;\">~A</td>~%" (round (* (calculate-point-value tdate) risk)))
       (format str "</tr>~%")
     ))


(defun write-edge-record (edge-path direction  tdate)
   
     (with-open-file (str edge-path :direction :output :if-exists :append :if-does-not-exist :create)
    (if (eql direction 'Long) (format str "<tr>~%")
           (format str "<tr>~%"))
       (format str "<td>~A</td>~%"  (index-lname))
       (format str "<td style=\"text-align:center;\">~A</td>~%" (edge-contract-month (contract-month *data-name* (getnumdate tdate))))
       (format str "<td style=\"text-align:center;\">~A</td>~%" direction)
      ; (format str "<td style=\"text-align:center;\">~A</td>~%" (my-pretty-price risk))
      ; (format str "<td style=\"text-align:center;\">~A</td>~%" (round (* (calculate-point-value tdate) risk)))
       (format str "</tr>~%")
     ))

(defun write-edge-header (edge-path)
     (with-open-file (str edge-path :direction :output :if-exists :supersede :if-does-not-exist :create)
       (format str "table border=0 cellspacing=0 padding=0 width=\"40%\">~%")
       (format str "<tr styled=\"background-color:rgba(211,211,211,0.25);\">~%")
       (format str "<th>Market</th>~%" )
       (format str "<th>Contract Month</th>~%")
       (format str "<th>Direction</th>~%")
      ; (format str "<th>Risk</th>~%")
       (format str "</tr>~%")
))

(defun write-edge-tail (edge-path)
     (with-open-file (str edge-path :direction :output :if-exists :append :if-does-not-exist :create)
         (format str "</table>")))

;;;;combine edgemailtemplate1.html edgemailtemplate2.html edgemailtemplate3.html
;;;;path1 is first part of template.
;;;;path2 is the part with trade suggestions
;;;path3 is the end part of the template
;;;path4 is the completed html file
(defun combine-edge ()
  (let (records (path1 (string-append *config-dir* "edgemailtemplate1.html"))
        (path2 (string-append *daily-output-dir* "edgemailtemplate2.html"))
        (path3 (string-append *config-dir* "edgemailtemplate3.html"))
        (path4 (string-append *daily-output-dir* "edgemailtemplate4.html")))

     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))
 
  (with-open-file (str path2 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))

   (with-open-file (str path3 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))
 
   (with-open-file (str path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith (reverse records))
       (format str "~A~%" ith)))

)) 

;;;;combine edgemailtemplate1.html edgemailtemplate2.html edgemailtemplate3.html
;;;;path1 is first part of template.
;;;;path2 is the part with trade suggestions
;;;path3 is the end part of the template
;;;path4 is the completed html file
(defun combine-equities-edge ()
  (let (records (path1 (string-append *config-dir* "edgemailtemplate1.html"))
        (path2 (string-append *daily-output-dir* "edgemailtemplate2s.html"))
        (path3 (string-append *config-dir* "edgemailtemplate3.html"))
        (path4 (string-append *daily-output-dir* "edgemailtemplate4s.html")))

     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))
 
  (with-open-file (str path2 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))

   (with-open-file (str path3 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
       (push record records)))
 
   (with-open-file (str path4 :direction :output :if-exists :supersede :if-does-not-exist :create)
     (dolist (ith (reverse records))
       (format str "~A~%" ith)))

)) 


(defun write-futures-view (retail-path2 date direction entry stop-loss )
  
    (setq entry 
       (case direction
         (buy  (+ entry (* (1+ (random 6)) (index-tick-size)))) 
         (sell (- entry (* (1+ (random 6))(index-tick-size)))))
      )
    (setq stop-loss 
       (case direction
         (buy  (- stop-loss (* (1+ (random 6)) (index-tick-size)))) 
         (sell (+ stop-loss (* (1+ (random 6))(index-tick-size)))))
      )
 
    (with-open-file (str retail-path2 :direction :output :if-exists :append :if-does-not-exist :create)
      (format str "~4A ~A ~A with stop order at ~A and stop loss at ~A  risk = ~A~%" 
            direction (edge-contract-month (contract-month *data-name* (getnumdate date))) (index-lname)
          ; (* (cadr (assoc *data-name* *oec-price-conversion-factor*)) entry)
           ;(* (cadr (assoc *data-name* *oec-price-conversion-factor*)) stop-loss)


      (if entry
       (if (member *data-name* '(US.D1B TY.D1B))
           (format nil "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry) 2)) 5))
         (format nil
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
              (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round entry (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss
        (if (stringp stop-loss)
            (format nil "~A ~%" stop-loss)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format nil "~7@A ~%" (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss) 2)) 5))
            (format nil
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D ~%"
              (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F ~%"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss (index-tick-size))))))
                  ))

         (round (abs (* (- entry stop-loss)(index-point-value))))) 
      )
)

(defun write-equity-view (retail-path2  direction entry stop-loss )
  
    (setq entry 
       (case direction
         (buy  (+ entry (* (1+ (random 6)) (index-tick-size)))) 
         (sell (- entry (* (1+ (random 6))(index-tick-size)))))
      )
    (setq stop-loss 
       (case direction
         (buy  (- stop-loss (* (1+ (random 6)) (index-tick-size)))) 
         (sell (+ stop-loss (* (1+ (random 6))(index-tick-size)))))
      )
 
    (with-open-file (str retail-path2 :direction :output :if-exists :append :if-does-not-exist :create)
      (format str "~4A ~A ~A with stop order at ~A and stop loss at ~A~%" 
            direction  (get-ninja-symbol *data-name*) (index-lname)
          ; (* (cadr (assoc *data-name* *oec-price-conversion-factor*)) entry)
           ;(* (cadr (assoc *data-name* *oec-price-conversion-factor*)) stop-loss)


           (format nil
             (string-append  "~7,"  "2,0,'*,' F, ")
             (my-pretty-price  entry))

         

           ; (format nil "~A ~%" stop-loss)

            (format nil
              (string-append "~7,"  "2,0,'*,' F ~%")
               (my-pretty-price stop-loss))
           )
        ; (round (abs (* (- entry stop-loss)(calculate-point-value tdate))))) 
      )
)


(defun write-forex-view (retail-path2  direction entry stop-loss )
  
    (setq entry 
       (case direction
         (buy  (+ entry (* (1+ (random 6)) (index-tick-size)))) 
         (sell (- entry (* (1+ (random 6))(index-tick-size)))))
      )
    (setq stop-loss 
       (case direction
         (buy  (- stop-loss (* (1+ (random 6)) (index-tick-size)))) 
         (sell (+ stop-loss (* (1+ (random 6))(index-tick-size)))))
      )
 
    (with-open-file (str retail-path2 :direction :output :if-exists :append :if-does-not-exist :create)
      (format str "~4A ~A with stop order at ~A and stop loss at ~A~%" 
            direction  (index-lname)
          ; (* (cadr (assoc *data-name* *oec-price-conversion-factor*)) entry)
           ;(* (cadr (assoc *data-name* *oec-price-conversion-factor*)) stop-loss)

           (format nil
             (string-append  "~7," (format nil "~A" (index-digits)) ",0,'*,' F, ")
             (my-pretty-price  entry))
           ; (format nil "~A ~%" stop-loss)

            (format nil
              (string-append "~7," (format nil "~A" (index-digits))  ",0,'*,' F ~%")
               (my-pretty-price stop-loss))
           )
        ; (round (abs (* (- entry stop-loss)(calculate-point-value tdate))))) 
      )
)



;;;;works for equity markets and other markets
(defun get-ninja-symbol (market)
  (let (name-string)
  (cond ((member market *equity-warehouse-list*)
         (setq name-string (format nil "~S" market))
         (read-from-string  (subseq name-string 0 (- (length name-string) 4))))
        (t (cdr (assoc market *ninja-symbol*))))
))


;;;;works for equity markets and other markets
;;;;market is the exitpoints symbol
(defun get-ts-symbol (market)
  (let (name-string)
  (cond ((member market *equity-warehouse-list*)
         (setq name-string (format nil "~S" market))
         (read-from-string  (subseq name-string 0 (- (length name-string) 4))))
        (t (cdr (assoc market *ts-symbol*))))
))

(defun get-cqg-symbol (market)
      (or (cdr (assoc market *cqg-symbol*)) (cdr (assoc market *ts-symbol*))))
  
;;;;works for equity markets and other markets
(defun get-exitpoints-symbol (ninja-symbol)
  
  (cond ((car (rassoc ninja-symbol *ninja-symbol*)))
          ;  (cdr (rassoc ninja-symbol *ninja-symbol*)))
         
        (t (read-from-string (format nil "~S.d3b" ninja-symbol)))) 
)


(defun get-barchart-symbol (market)
      (or (cdr (assoc market *barchart-symbol*)) (cdr (assoc market *ts-symbol*))))

;;;this writes the day , swing , and open positions records to the dcr.dat file.
(defun write-xml-record (cdate table-name trade-type direction tdate entry-price stop-price cover-price output1)
  ;     (print *data-name*) (print (get-ninja-symbol *data-name*))
       (format output1 "<record id=\"~A\">~%" (get-ts-symbol *data-name*))
  ;     (format T "<record id=\"~A\">~%" (get-ninja-symbol *data-name*))
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
             (string-append  "<field id=\"EntryPrice\" type=\"text\">~8," (format nil "~A" (index-digits)) ",0,'*,' F</field>~%"))
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
              (string-append "<field id=\"StopPrice\" type=\"text\">~8," (format nil "~A" (index-digits)) ",0,'*,' F</field>~%"))
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
              (string-append "~8," (format nil "~A" (index-digits)) ",0,'*,' F</field>~%"))
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
                       ) (calculate-point-value cdate))) ))

          )
          (format output1 "</record>~%")
          )

;;;
;;;the entry time is the later of 19:01:00 or the start of the electronic session plus one minute
(defun write-ninja-record (output-ninja time-zone date date1 direction entry-price stop-loss-price)
   (let (ninja-symbol contract-month entry-time exit-time date0 central-cancel-time; (offset (random-choice -1 0)))
        (offset (random-choice -5 -4)))
   (setq ninja-symbol (cdr (assoc *data-name* *ninja-symbol*))
         exit-time (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)
         entry-time (second (assoc *data-name* *foremost-market-times-list*))
	     date0 (add-days-to-date date1 -1)
	     contract-month (ninja-contract-month (contract-month *data-name* date)) 
         central-cancel-time (string-append (date-convert date1) " "
                                           (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26)))

       (format output-ninja "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A ~A\, "
	         direction ninja-symbol contract-month time-zone (if (> (length entry-time) 7) 
                (date-convert date0) (date-convert date1))
                entry-time central-cancel-time (date-convert date1) exit-time)

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
         (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
              (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-ninja "~A ~%" stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output-ninja "~7@A ~%" (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D ~%"
              (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F ~%"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )

    ));;closes the let and defun

;;;
;;;
;;;the entry time is the later of 19:01:00 or the start of the electronic session plus one minute
;;;this version adds a field to offset the entry price for FORE and FORE FOREX
;;;;works for STARTER SCORE ENTRY EPIC FORE and FORE FOREX
(defun write-ninja-record1 (output-ninja time-zone date date1 direction entry-price stop-loss-price
                              &optional (stop-offset 0))
   (let (ninja-symbol contract-month entry-time exit-time date0 central-cancel-time ;(offset (random-choice -1 0)))
        (offset (random-choice -5 -4))) 
   (setq ninja-symbol (cdr (assoc *data-name* *ninja-symbol*))
         exit-time (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) offset T)
         entry-time (second (assoc *data-name* *foremost-market-times-list*))
	     date0 (add-days-to-date date1 -1)
	     contract-month (ninja-contract-month (contract-month *data-name* date)) 
         central-cancel-time (string-append (date-convert date1) " " (add-minutes1 (third (assoc *data-name* *foremost-market-times-list*)) -26)))

       (format output-ninja "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A ~A\, "
	         direction ninja-symbol  contract-month time-zone (if (> (length entry-time) 7) (date-convert date0) (date-convert date1))
             entry-time central-cancel-time (date-convert date1) exit-time)

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
         (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
              (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-ninja "~A, " stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )
         
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 stop-offset) 2)) 5))
    
            (format output-ninja
               (if (and (index-digits)
                        (zerop (index-digits)))
                  "~D ~%"
                   (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F ~%"))
              (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round stop-offset (index-tick-size)))))
                  
             )
         
            
          

    ));;closes the let and defun



;;;write-ninja-record and write-rjo-record both use the ninja price conversion table in open.lisp
(defun write-rjo-record (output-rjo tdate direction entry-price stop-loss-price)
   (let (rjo-symbol contract-month)
   (setq rjo-symbol (cdr (assoc *data-name* *rjo-symbol*))
       ;  contract-month (third (assoc *data-name* *C-list*)))
          contract-month (contract-month *data-name* tdate))
   (format output-rjo "~%~A\, ~A\, ~A\,  " direction rjo-symbol contract-month)


      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output-rjo "~7@A\, " (convert-to-32nds entry-price))
         (format output-rjo
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
              (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-rjo "~A ~%" stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output-rjo "~7@A ~%" (convert-to-32nds stop-loss-price))
            (format output-rjo
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D ~%"
              (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F ~%"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )

));    closes the let and defun

(defun write-oec-short (path3 oec-symbol block-account quantity entry-short stop-short entry-time cancel-time exit-time end-session-time oco-code)
      (with-open-file (cannon-oec path3 :direction :output :if-exists :append :if-does-not-exist :create)
              (write-oec-record cannon-oec "True" block-account "Sell" quantity oec-symbol "Stop" "None" entry-short
				entry-time cancel-time oco-code) ;;;main entry order
              (write-oec-record cannon-oec "True" block-account "Buy" quantity oec-symbol "Stop" "None" stop-short
				entry-time end-session-time oco-code) ;;; stop loss order
           ;  (write-oec-record cannon-oec "False" *cannon-score-block-acct* "Buy" *cannon-score-qty* oec-symbol "Limit" "None" cover-short
	   ;  cannon-entry-time cannon-exit-time "None") ;;; objective limit order
              (write-oec-record cannon-oec "True" block-account "Buy" quantity oec-symbol "Market" "None" ""
				exit-time end-session-time oco-code) ;;; exit order
))

(defun write-oec-long (path3 oec-symbol block-account quantity entry-long stop-long entry-time cancel-time exit-time end-session-time oco-code)
       (with-open-file (cannon-oec path3 :direction :output :if-exists :append :if-does-not-exist :create)
              (write-oec-record cannon-oec "True" block-account "Buy" quantity oec-symbol "Stop" "None" entry-long
				entry-time cancel-time oco-code) ;;;main entry order
              (write-oec-record cannon-oec "True" block-account "Sell" quantity oec-symbol "Stop" "None" stop-long
				entry-time end-session-time oco-code) ;;; stop loss order
            ; (write-oec-record cannon-oec "False" *cannon-score-block-acct* "Sell" *cannon-score-qty* oec-symbol "Limit" "None" cover-long
	    ; cannon-entry-time cannon-exit-time "None" ) ;;; objective limit order
              (write-oec-record cannon-oec "True" block-account "Sell" quantity oec-symbol "Market" "None" ""
				exit-time end-session-time oco-code) ;;; exit order
))


;;;the entry time is the later of 19:01:00 or the start of the electronic session plus one minute

;;;Each line contains:
;;;direction, ninja-symbol, contract-month, time-zone, date, entry-time, central-cancel-time, date1, exit-time,
;;;entry-price, stop-loss-price, upper-limit or lower-limit, spread, reel rate
(defun write-mt-record (output-ninja time-zone date date1 direction entry-price stop-loss-price)
   (let (ninja-symbol contract-month entry-time exit-time  central-cancel-time (offset (random-choice -1 0)))
   (setq ninja-symbol (cdr (assoc *data-name* *mt-symbol*))
         exit-time (add-minutes1 (third (assoc *data-name* *mt-market-times-list*)) offset T)
         entry-time (second (assoc *data-name* *mt-market-times-list*))
	  
	     contract-month (mt-contract-month (contract-month *data-name* date)) 
         central-cancel-time (string-append (mt-date-convert date1) " "
                                           (add-minutes1 (third (assoc *data-name* *mt-market-times-list*)) -26)))

       (format output-ninja "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A ~A\, "
	         direction ninja-symbol contract-month time-zone  
                (mt-date-convert (if (member *data-name* *forex-list*) date date1))
                entry-time central-cancel-time (mt-date-convert date1) exit-time)

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
         (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (nth 2 (assoc *data-name* *mt-price-conversion-factor*))) ",0,'*,' F, "))
              (* (cadr (assoc *data-name* *mt-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-ninja "~A, " stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output-ninja "~7@A," (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (nth 2 (assoc *data-name* *mt-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *mt-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )
;;;;How to write a carriage return character
           ;(format output-ninja " ~A~C~%" (second (assoc *data-name* *dubai-spread-list*)) #\cr)
  ;;;lower limit and upper limit
           (if (eql direction 'BUY)
               (format output-ninja " ~A," (if (index-limit)
                                             (my-pretty-price  (+ (getd date 'close)
                                                                  (index-limit)(- (index-tick-size)))) 0))
              (format output-ninja " ~A," (if (index-limit)
                                          (my-pretty-price (+ (getd date 'close)
                                                              (- (index-limit))(index-tick-size))) 0)))
;;;number of ticks to allow between bid and ask 
           (format output-ninja " ~A," (second (assoc *data-name* *mt-spread-list*)))   
;;;most number of contracts to trade at a time between delays           
           (format output-ninja " ~A"   (or (index-reel) 2))
          
           (if (member *data-name* *forex-list*)
               (format output-ninja ", 20, 21"))
          (format output-ninja "~%")

    ));;closes the let and defun


;;;TradeStation
;;;;type, symbol, contract, time zone, release date/time, cancel date/time, exit date/time, entry price,
;;;stop loss price, objective(limit) price, qty
(defun write-ts-record (output-ninja time-zone tdate date1 direction entry-price stop-loss-price)
   (let (ts-symbol contract-month entry-time exit-time  central-cancel-time );(offset (random-choice -1 0)))
   (setq ts-symbol (get-ts-symbol *data-name*)
        ; exit-time (add-minutes (or  (third (assoc *data-name* *foremost-market-times-list*)) "14:58") offset T)
         exit-time  (or  (third (assoc *data-name* *foremost-market-times-list*)) "14:58:00") 
         entry-time (or (second (assoc *data-name* *foremost-market-times-list*)) "09:00:00") 
         contract-month (ts-contract-month (contract-month *data-name* tdate)) 
         central-cancel-time (string-append (date-convert date1) " "
                                           (add-minutes1  (or (third (assoc *data-name* *foremost-market-times-list*))
                                                         "15:00:00") -28)))
         

       (format output-ninja "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A ~A\, "
	         direction ts-symbol contract-month time-zone  
;                (date-convert (get-release-date date1))
                (date-convert date1)
                entry-time central-cancel-time (date-convert date1) exit-time)

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
          ; (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
            (format output-ninja "~7@A, " (my-round entry-price 5))
         (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F, "))
              (*  (cadr (assoc *data-name* *ts-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-ninja "~A, " stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
              ; (format output-ninja "~7@A," (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
               (format output-ninja "~7@A," (my-round stop-loss-price 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )

           ;(format output-ninja " ~A~C~%" (second (assoc *data-name* *dubai-spread-list*)) #\cr)
  ;;;lower limit and upper limit
           (if (eql direction 'BUY)
               (format output-ninja " ~A" (if (index-limit)
                                          (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                                             (my-pretty-price                                            
                                                       (+ (getd tdate 'close)
                                                              (index-limit)(- (index-tick-size))))) 0))
              (format output-ninja " ~A" (if (index-limit)
                                          (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                                             (my-pretty-price
                                                       (+ (getd tdate 'close)
                                                               (- (index-limit))(index-tick-size)))) 0)))
    ;         (format output-ninja ", ~A" qty);;;quantity of contracts
             
;           Field eleven is the tick spread allowed
            (format output-ninja " ,~A"(if (member *data-name* '(pa.d1b)) 12 7)) ;(second (assoc *data-name* *dubai-spread-list*)))              
;;;for testing limits
;         (if (eql direction 'BUY)
;               (format output-ninja " ~A," (if (index-limit) (+ (getd date 'close) (index-limit))
;                  (+ (getd date 'close)(* 2 (- entry-price (getd date 'close))))))
;              (format output-ninja " ~A," (if (index-limit)(- (getd date 'close) (index-limit))
;                 (- (getd date 'close) (* 2 (- (getd date 'close) entry-price))))))



        (format output-ninja "~C~%" #\cr)
        ;   (format output-ninja " ~A," 3) ;(second (assoc *data-name* *dubai-spread-list*)))              
        ;   (format output-ninja " ~A~%"   (if (index-reel)(index-reel) 1))
           ;(format output-ninja " ~A~%" (index-tick-size))

    ));;closes the let and defun

;;;Edge for Ninja Trader
;;;;type, symbol, contract, time zone, release date/time, cancel date/time, exit date/time, entry price,
;;;stop loss price, objective(limit) price, qty
(defun write-nt-edge-record (output-ninja time-zone tdate date1 direction entry-price stop-loss-price)
   (let (nt-symbol contract-month entry-time exit-time  central-cancel-time; (offset (random-choice -1 0)))
        (offset (random-choice -5 -4)))
   (setq nt-symbol (get-ninja-symbol *data-name*)
         exit-time (add-minutes1 (or  (third (assoc *data-name* *foremost-market-times-list*)) "14:58:00") offset T)
         entry-time  (if (eql *data-name* 'lh.d1b) "08:30:03" "06:00:03") entry-price 0  stop-loss-price 0
         contract-month (ninja-contract-month (contract-month *data-name* tdate)) 
         central-cancel-time (string-append (date-convert date1) " "
                                           (add-minutes1  (or (third (assoc *data-name* *foremost-market-times-list*))
                                                         "15:00:00") -28)))
         

       (format output-ninja "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A ~A\, "
	         direction nt-symbol contract-month time-zone  
               ; (date-convert (get-release-date date1))
                 (date-convert date1)
                entry-time central-cancel-time (date-convert date1) exit-time)

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
          ; (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
            (format output-ninja "~7@A, " (my-round entry-price 5))
         (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (third (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
              (*  (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-ninja "~A, " stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
              ; (format output-ninja "~7@A," (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
               (format output-ninja "~7@A," (my-round stop-loss-price 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (third (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )

           ;(format output-ninja " ~A~C~%" (second (assoc *data-name* *dubai-spread-list*)) #\cr)
  ;;;lower limit and upper limit
           (if (eql direction 'BUY)
               (format output-ninja " ~A,"
                (my-pretty-price (fmin (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                         (+ (getd tdate 'close)(volatility tdate 21 1.618)))  ;;average true range over the past 3 months          
                  (if (index-limit)
                      (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                         (my-pretty-price                                            
                                      (+ (getd tdate 'close)
                                         (index-limit)(- (index-tick-size))))) nil))))
              (format output-ninja " ~A," 
                (my-pretty-price (fmax (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                                (- (getd tdate 'close)(volatility tdate 63 1.618)))               
            
                    (if (index-limit)
                        (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                           (my-pretty-price
                                   (+ (getd tdate 'close)
                                      (- (index-limit))(index-tick-size)))) nil)))))
             
;;;for testing limits
;         (if (eql direction 'BUY)
;               (format output-ninja " ~A," (if (index-limit) (+ (getd date 'close) (index-limit))
;                  (+ (getd date 'close)(* 2 (- entry-price (getd date 'close))))))
;              (format output-ninja " ~A," (if (index-limit)(- (getd date 'close) (index-limit))
;                 (- (getd date 'close) (* 2 (- (getd date 'close) entry-price))))))
     
     ;   (format output-ninja " ~A," 6) ;(second (assoc *data-name* *dubai-spread-list*)))              
        (format output-ninja " ~A,"(if (member *data-name* '(pa.d1b)) 12 7)) ;(second (assoc *data-name* *dubai-spread-list*)))              
        (format output-ninja " ~A"  12); (if (index-reel)(index-reel) 10))
           ;(format output-ninja " ~A~%" (index-tick-size))
      ;  (format output-ninja ", ~A" qty);;;quantity of contracts
        (format output-ninja "~C~%" #\cr)
    ));;closes the let and defun


(defun write-nt-edge-best-pf (num)
   (let (;ninja-fore-path3
         ts-fore-path3 record)
  ; (vsort *nt-pf-list* #'> #'second)
   (vsort *ts-pf-list* #'> #'second)
   ;(setq ninja-fore-path3 (third (car *nt-pf-list*))
    (setq  ts-fore-path3 (third (car *ts-pf-list*)))
 ;  (with-open-file (ninja-output ninja-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create)
 ;  (dotimes (ith (min 6 (length *nt-pf-list*)))
 ;     (setq record (nth ith *nt-pf-list*))(set-market (car record))
 ;     (write-nt-edge-record ninja-output (fourth record) (fifth record) (sixth record)(seventh record) 0 0 )
    
;      ));;;closes the dotimes
   (with-open-file (ninja-output ts-fore-path3 :direction :output :if-exists :append :if-does-not-exist :create)
   (dotimes (ith (min num (length *ts-pf-list*)))
      (setq record (nth ith *ts-pf-list*))(set-market (car record))
      (write-ts-record ninja-output (fourth record) (fifth record) (sixth record)(seventh record) 0 0 )
      ));;;closes the dotimes

))

(defun write-edge-template-best-pf (edge-path num)
   (let (record direction dir)
    (vsort *nt-pf-list* #'> #'second)
      (with-open-file (template-output edge-path :direction :output :if-exists :append :if-does-not-exist :create)
     (dotimes (ith (min num (length *nt-pf-list*)))
       (setq record (nth ith *nt-pf-list*))(set-market (car record))
      (setq direction (seventh record)) 
      (setq dir (if (eql direction 'BUY) 'LONG 'SHORT)) ;(format T "~%record = ~A" record)
      (write-edge-record template-output dir (fifth record))
     ))

))


;;;Edge for OEC Trader
;;;;type, symbol, contract, time zone, release date/time, cancel date/time, exit date/time, entry price,
;;;stop loss price, objective(limit) price, qty
(defun write-oec-edge-record (output-oec time-zone tdate date1 direction entry-price stop-loss-price)
   (let (oec-symbol; contract-month 
        entry-time exit-time  central-cancel-time; (offset (random-choice -1 0)))
        (offset (random-choice -5 -4)))
   (setq oec-symbol (make-oec-symbol *data-name* tdate)
         exit-time (add-minutes1 (or  (third (assoc *data-name* *foremost-market-times-list*)) "14:58:00") offset T)
         entry-time  "07:30:03" entry-price 0  stop-loss-price 0
       ;  contract-month (ninja-contract-month (contract-month *data-name* tdate)) 
         central-cancel-time (string-append (date-convert date1) " "
                                           (add-minutes1  (or (third (assoc *data-name* *foremost-market-times-list*))
                                                         "15:00:00") -28)))
         

       (format output-oec "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A ~A\, "
	         direction oec-symbol " " time-zone  
               ; (date-convert (get-release-date date1))
                 (date-convert date1)
                entry-time central-cancel-time (date-convert date1) exit-time)

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
          ; (format output-oec "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
            (format output-oec "~7@A, " (my-round entry-price 5))
         (format output-oec
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (third (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F, "))
              (*  (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-oec "~A, " stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
              ; (format output-ninja "~7@A," (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
               (format output-oec "~7@A," (my-round stop-loss-price 5))
            (format output-oec
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (third (assoc *data-name* *ninja-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )

           ;(format output-ninja " ~A~C~%" (second (assoc *data-name* *dubai-spread-list*)) #\cr)
  ;;;lower limit and upper limit
           (if (eql direction 'BUY)
               (format output-oec " ~A," (if (index-limit)
                                          (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                                             (my-pretty-price                                            
                                                       (+ (getd tdate 'close)
                                                              (index-limit)(- (index-tick-size))))) 0))
              (format output-oec " ~A," (if (index-limit)
                                          (* (cadr (assoc *data-name* *ninja-price-conversion-factor*))
                                             (my-pretty-price
                                                       (+ (getd tdate 'close)
                                                               (- (index-limit))(index-tick-size)))) 0)))
             
     
        (format output-oec " ~A," 6) ;(second (assoc *data-name* *dubai-spread-list*))) allowed spread             
        (format output-oec " ~A,"   (if (index-reel)(index-reel) 10));;number of contracts per order
         (format output-oec " ~A," 2);;;reset hour
        (format output-oec ", ~A" 3);;;hour to load trade action file
        (format output-oec "~C~%" #\cr)
    ));;closes the let and defun

;;;converts my ExitPoints prices to ninja-prices
;(defun ninja-price (price)
;)

;;;writes out the date martket and contract month in ninjatrader format for
;;;the stop loss study from Mark Whiting
(defun dump-contract-months (market sdate)
   (let (path (date  sdate))
   (maind-x) (set-cat-list)
   (set-market market)
   (setq path (string-append *output-upper-dir*
                             (format nil "~A" (cdr (assoc market *ninja-symbol*))) "contract-months.csv"))
   (print path)

   (with-open-file (str path :direction :output :if-exists :supersede :if-does-not-exist :create)

   (loop
       (format str "~A\,  ~A\,  ~A~%" date (cdr (assoc market *ninja-symbol*))
       (ninja-contract-month (contract-month market date)))
       (format t "~A\, ~A\, ~%" date (ninja-contract-month (contract-month market date)))
       (setq date (getd date 'ndate))
       (if (or (stringp date) (not date)) (return))))
))



(defun build-mt4-trade-action-files (&optional tdate num (market-list *dubai-list*))
   (let ((date tdate)  directive epsignal longs shorts long-gains short-gains
          long-acc short-acc  twr-short twr-long bin features date1
   ;      (path-in (string-append *config-dir* "day-features2.dat"))
          )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))
    
    (ifn (boundp 'counter)(setq counter 0))
     (set-cat-list)
  
    (dolist (ith (directory (string-append "mk-data/zzz-test/Epic MT4/action-files/" "mt-epic*.csv")))
             (delete-file ith))
       (setq date (add-mkt-days tdate (- num)))
       (setq features *dubai-features2*)
       (apply #'dubai-trade-bins2b features)
      (dotimes (kth num)
         (setq date1 (getd date 'ndate))
        (dolist (market market-list)
            (set-market market)
        ;   (format T "~%Market = ~A   Features = ~A ~%" market features)          
          
         

          (setq directive (if (and (index-digits) (zerop (index-digits)))  " ~D   "
                          (string-append " ~7," (format nil "~A" (index-digits)) ",0,'*,' F   ")))

        ; (format T "Directive = ~A" directive) 
 ;        (with-open-file (stream path1 :direction :output :if-exists :append :if-does-not-exist :create)
  ;          (print-string stream (format nil "~%~A" market) 7)
           ; (format stream "  ~A  " (nth 2 (assoc market *c-list*)))
 ;           (format stream "  ~A  " (contract-month market date))
 ;           (if (member *data-name* '(US.D1B TY.D1B))
 ;               (format stream "~9@A    " (convert-to-32nds (getd date 'close)))
 ;              (format stream directive
 ;                    (* (index-tick-size) (round (getd date 'close) (index-tick-size)))))
 ;        ;  (format T "~%path2 = ~A" (contract-month market date))
 ;        (with-open-file (stream1 path2 :direction :output :if-exists :append :if-does-not-exist :create)

         (multiple-value-setq (epsignal longs long-gains long-acc shorts short-gains short-acc bin)
                 (bin-classifier-dubaitrades2b date features))

       ;  (setq twr-short (day-bin-twr3 bin))

        (setf (nth 0 bin) 1)
        ; (setq twr-long (day-bin-twr3 bin))
   ; (format T "~%111 date = ~A  date1 = ~A" date date1)
         (find-best-dubaitradeX date date1)
    ; (format T "~%222")
          );;;closes the dolist
        (setq date (getd date 'ndate))
          );closes the dotimes
 ;  (if (probe-file (string-append "~/exitpoints/ninja/mt-epic-" (format nil "~A" date1) ".csv"))
 ;      (rename-file (string-append "~/exitpoints/ninja/mt-epic-" (format nil "~A" date1) ".csv")
 ;                   (string-append "~/mk-data/zzz-test/mt-epic-" (format nil "~A" date1) ".csv")))
  )) ;;closes the let and defun


(defun find-best-dubaitradeX (tdate &optional (date1 nil))
  (let (risk vri  stop-short stop-long  entry-long entry-short
        trade-direction ;cover-short cover-long
        action ;directive1  
         mt-dubai-path2  
         
        ; central-exit-time central-entry-time central-cancel-time central-end-session-time
	  (time-zone "UTC") ;offset
        )
    (declare (special epsignal longs shorts long-gains short-gains long-acc short-acc twr-short twr-long counter))

    (setq *entry-factor* .3333  *stop-loss-day* 1.125 *max-day-risk* 2500 *min-dubai-expected-value* 30) ; ;;; 1.0557)

    (setq  mt-dubai-path2 (string-append "mk-data/zzz-test/Epic MT4/action-files/" "mt-epic.csv")
	  )

    (setq vri (volatility-ratio-index tdate 4 63 1))
;    (format T "~%EPSIGNAL = ~A VRI= ~A~%  longs = ~A  long-gains = ~A twr-long = ~7,5F~%"
;            epsignal vri longs long-gains  twr-long)
;     (format T "  shorts = ~A  short-gains = ~A twr-short = ~7,5F~%" shorts short-gains  twr-short)

   (setq risk  (volatility tdate 4 *stop-loss-day*) risk (+ risk (* 3.0 (index-tick-size))))

   (if (and (member epsignal '(OK UP))(> longs 0)
            (>= (/ long-gains longs) *min-dubai-expected-value*)(> twr-long 1.0)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ long-gains longs)(if (> shorts 0) (/ short-gains shorts) 0))
            (> vri -1)
            )
       (push 'UP trade-direction)(push 'FT trade-direction))
   (if (and (member epsignal '(OK DOWN))(> shorts 0)
            (>= (/ short-gains shorts) *min-dubai-expected-value*)(> twr-short 1.0)
            (<= (* risk (calculate-point-value tdate)) *max-day-risk*)
            (> (/ short-gains shorts)(if (> longs 0)(/ long-gains longs) 0))
            (> vri -1)      
             )
    (push 'DN trade-direction)(push 'FT trade-direction))
;  (format T "~%risk = ~A trade-direction= ~A" (* risk (calculate-point-value tdate)) trade-direction)
   (multiple-value-setq (entry-short entry-long)(vprices tdate 4 *entry-factor* 1))

   (setq entry-long (+ entry-long (* 3.0 (index-tick-size))) entry-short (- entry-short (* 3.0 (index-tick-size))))
   (setq stop-long (- entry-long risk) stop-short (+ entry-short risk))

             (cond ((and (member 'UP trade-direction) (member 'DN trade-direction)))
                   ((member 'UP trade-direction) (push "NOT SHORT" action))
                   ((member 'DN trade-direction) (push "NOT LONG" action))
                   (t (push "NOT TODAY" action)))

         (setq action (cond ((member "NOT TODAY" action :test #'equalp) "NOT TODAY")
                            ((and (member "NOT LONG" action :test #'equalp)
                                  (member "NOT SHORT" action :test #'equalp)) "NOT TODAY")
                            ((member "NOT LONG" action :test #'equalp)
                             (incf counter) "NOT LONG")
                            ((member "NOT SHORT" action :test #'equalp)
                              (incf counter) "NOT SHORT")
                            (t (incf counter) "OK      ")))
;
;      (format output " ~A VRI= ~A~%" action vri)
;      (format output " Num Longs = ~D P/L for Longs = ~A Accuracy for Longs = ~3F~%" longs long-gains long-acc)
;      (format output " Num Shorts = ~D P/L for shorts = ~A Accuracy for Short = ~3F~%" shorts short-gains short-acc)
;      (setq directive1 (string-append "~7," (format nil "~A" (index-digits)) ",0,'*,' F"));

;      (if (member *data-name* '(US.D1B TY.D1B))
;          (format output "~%SELL= ~7@A  STOP= ~7@A  SHORT-RISK= ~D~% BUY= ~7@A  STOP= ~7@A   LONG-RISK= ~D~%"
;           (convert-to-32nds entry-short) (convert-to-32nds stop-short)
;           (round (* (-  (convert-to-decimal (convert-to-32 stop-short))
;                         (convert-to-decimal (convert-to-32 entry-short)))(index-point-value)))
;           (convert-to-32nds entry-long)(convert-to-32nds stop-long)
;           (round (* (-  (convert-to-decimal (convert-to-32 entry-long))
;                         (convert-to-decimal (convert-to-32 stop-long))
;                       ) (index-point-value))))
;         (format output;
;
;            (string-append "~%SELL= " directive1 " STOP= " directive1
;                           " SHORT-RISK= ~D~% BUY= " directive1 " STOP= " directive1 "  LONG-RISK= ~D~%")
;            (* (index-tick-size) (round entry-short (index-tick-size)))
;            (* (index-tick-size) (round stop-short (index-tick-size)));
;
;            (round (* (-  (* (index-tick-size) (round stop-short (index-tick-size)))
;                          (* (index-tick-size) (round entry-short (index-tick-size)))
;                             )  (index-point-value)))
;            (* (index-tick-size) (round entry-long (index-tick-size)))
;            (* (index-tick-size) (round stop-long (index-tick-size)));
;
;            (round (* (-  (* (index-tick-size) (round entry-long (index-tick-size)))
;                          (* (index-tick-size) (round stop-long (index-tick-size)))
;                       )  (index-point-value)))
;            ))

     
;     (format T "~%~A  ~A~%" *data-name* action)

     (cond   ((equal action "NOT SHORT")
               (with-open-file (ninja-output mt-dubai-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	         	     (write-mt-record ninja-output  time-zone tdate date1 'BUY entry-long stop-long))
              )

             ((equal action "NOT LONG")
              (with-open-file (ninja-output mt-dubai-path2 :direction :output :if-exists :append :if-does-not-exist :create)
	                    (write-mt-record ninja-output time-zone tdate date1 'SELL entry-short stop-short))
               ));;closes clause the cond
      )) ;;;closes the let and the defun

;;;returns the gain or loss in $ for a point change
(defun pl (market points date)
 (set-market market)
  (round (* points (calculate-point-value date))))


(defun append-ts-charger ()
 (let (paths records (path-out "~/mk-data/111-dropbox/ts-charger.csv"))
  (setq paths (directory (string-append "mk-data/111-dropbox/ts/" "ts-charger*.csv")))
       
   (dolist (ith paths)
  
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file

       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (format str "~A~%" jth)))


   )))



(defun append-ts-triumph21-test ()
 (let (paths records (path-out "~/mk-data/111-dropbox/ts-triumph21-test.csv"))
  (setq paths (directory (string-append "~/exitpoints/" "*-test.csv")))
       
   (dolist (ith paths)
  
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file

       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (format str "~A~%" jth)))


   )))
;;;converts an OEC trade ction file to a TS trade action file
;;;;;
(defun convert-oec-to-ts (path1 )
  (let ((path-ts "~/exitpoints/daily-out/ts-triumph21.csv") contract-month mon ts-symbol yr mkt
        record1 direction sym cancel-time exit-time stop-loss entry-price records)

;;(defun write-ts-record (output-ninja time-zone tdate date1 direction entry-price stop-loss-price qty)

     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof))
           (ctr 1  (1+ ctr)))
          ((eql record 'eof))
        
           (setq record1 (my-split-sequence #\; record))
           
           (cond ((= (mod ctr 3) 1)
                  (setq direction (nth 2 record1)  sym (nth 4 record1)
                        entry-price (read-from-string (nth 7 record1))
                        cancel-time (nth 10 record1)))
                ((= (mod ctr 3) 2)
                   (setq stop-loss (read-from-string (nth 7 record1))))
                ((= (mod ctr 3) 0)
                   (setq exit-time (nth 9 record1))
                   (push (list direction sym cancel-time exit-time entry-price stop-loss
                                ) records)))));;closes the do and with-open-file

      (with-open-file (str path-ts :direction :output :if-exists :supersede :if-does-not-exist :create);

        (dolist (kth records)
         (setq sym (nth 1 kth)) 
         (setq mkt (car (rassoc (read-from-string (subseq sym 0 (- (length sym) 2))) *oec-symbol*)))
         
         (set-market mkt)
         (setq mon (subseq sym (- (length sym) 2) (- (length sym) 1))
               yr (subseq sym (- (length sym) 1) (- (length sym) 0)))
        
         (setq ts-symbol (get-ts-symbol *data-name*)
               contract-month (string-append mon "1" yr)) 
          (write-ts-record1 str ts-symbol contract-month 'CT 
                      (car kth) (nth 4 kth)(nth 2 kth) (nth 3 kth)(nth 5 kth))
     ))

records

))

;;;used solely to convert OEC trade action files to tradeStation trade action files
(defun write-ts-record1 (output-ninja ts-symbol contract-month time-zone 
                         direction entry-price central-cancel-time exit-time stop-loss-price )
   (let (entry-time entry-date  tdate (qty 1))
     
       (setq entry-time (or (second (assoc *data-name* *foremost-market-times-list*)) "09:00:03")           
             entry-date (get-release-date (conv-to-ewaves-date central-cancel-time))
             tdate (getd (conv-to-ewaves-date exit-time) 'ydate))
       (format output-ninja "~A\, ~A\, ~A\, ~A\, ~A ~A\, ~A\, ~A\, "
	         direction ts-symbol contract-month time-zone  
                 (date-convert entry-date)  entry-time
                   central-cancel-time exit-time)

;;;need to convert entry and stop loss price from oec to exitpoints
      (setq entry-price (/ entry-price (second (assoc *data-name* *oec-price-conversion-factor*))) 
            stop-loss-price (/ stop-loss-price (second (assoc *data-name* *oec-price-conversion-factor*))))

      (if entry-price
       (if (member *data-name* '(US.D1B TY.D1B))
           (format output-ninja "~7@A, " (my-round (convert-to-decimal (my-round (convert-to-32 entry-price) 2)) 5))
         (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
             (string-append  "~7," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F, "))
              (*  (cadr (assoc *data-name* *ts-price-conversion-factor*))
                 (* (index-tick-size) (round entry-price (index-tick-size))))))

         )

      ;  (format T "stop-price=~A ~%" stop-price)

       (if stop-loss-price
        (if (stringp stop-loss-price)
            (format output-ninja "~A, " stop-loss-price)
           (if (member *data-name* '(US.D1B TY.D1B))
               (format output-ninja "~7@A," (my-round (convert-to-decimal (my-round (convert-to-32 stop-loss-price) 2)) 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round stop-loss-price (index-tick-size))))))
                  )
             )

           ;(format output-ninja " ~A~C~%" (second (assoc *data-name* *dubai-spread-list*)) #\cr)
  ;;;lower limit and upper limit
           (if (eql direction 'BUY)
               (format output-ninja " ~A" (if (index-limit)
                                          (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                                             (my-pretty-price                                            
                                                       (+ (getd tdate 'close)
                                                              (index-limit)(- (index-tick-size))))) 0))
              (format output-ninja " ~A" (if (index-limit)
                                          (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                                             (my-pretty-price
                                                       (+ (getd tdate 'close)
                                                               (- (index-limit))(index-tick-size)))) 0)))
              (format output-ninja ", ~A" qty);;;quantity of contracts

        (format output-ninja "~C~%" #\cr)

    ));;closes the let and defun



;;;separates the composite TS trade results file into a Starter trade results file
;;;the market-list is usually the *triumph11-list*
(defun ts-triumph21-trades-to-triumph11 (market-list)
 (let (records  records1 mkt  path-out (path1 "~/exitpoints/ts-triumph21-results.csv"))
  
     (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file
;;;;first find all markets in the file ts-triumph21.csv

     (dolist (ith records)
           (setq mkt (read-from-string (second (my-split-sequence #\, ith))))
           (setq mkt (car (rassoc mkt *ts-symbol*)))
           (if (member mkt market-list)
               (push ith records1 ))
           )
;;;
       
      (setq path-out  "~/exitpoints/ts-starter-results.csv")
             
       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records1)
               (format str "~A~%" jth)))


   ))


(defun append-ts-triumph21-results ()
 (let (paths records (path-out "~/exitpoints/ts-triumph21-results.csv")
      (path1 "~/mk-data/111-dropbox/triumph21-results/tsperformance.csv")
      (path2 "~/mk-data/111-dropbox/triumph212014/tsperformance.csv"))
  (setq paths (list path1 path2))
       
   (dolist (ith paths)
  
     (with-open-file (str ith :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))

         (push record records)));;closes the do and with-open-file

       (with-open-file (str path-out :direction :output :if-exists :supersede :if-does-not-exist :create)
          (dolist (jth records)
              (format str "~A~%" jth)))


   )))


;;;TradeStation

;;;type refers to buy or sell or cover
;;;TradeStation
;;;;type, symbol, contract, time zone, release date/time, cancel date/time, exit date/time, entry price,
;;;stop loss price, objective(limit) price, qty
;;;this is for a counter trend systme with no sltop loss
;;;there is no cancel time or exit time for a swing system
(defun write-counter-swing-record (output-ninja tdate date1 csignal entry-price objective-price qty)
   (let (ts-symbol contract-month entry-time)
   (setq ts-symbol (get-ts-symbol *data-name*)
         
         entry-time (or (second (assoc *data-name* *foremost-market-times-list*)) "09:00:03") 
         contract-month (ts-contract-month (contract-month *data-name* tdate)) 
        )
         

       (format output-ninja "~A\, ~A\, ~A\,"
	          csignal ts-symbol contract-month               
                 )

       (if (member csignal '(OUT))
            (format output-ninja " ")
          (format output-ninja " ~A ~A\," (date-convert (get-release-date date1)) entry-time))
        
       (if entry-price
        (if (stringp entry-price)
             (format output-ninja "~A, " entry-price)
           (if (member *data-name* '(US.D1B TY.D1B))
            
               (format output-ninja "~7@A," (my-round entry-price 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round entry-price (index-tick-size))))))
                  )
             )
      
       (if objective-price
        (if (stringp objective-price)
             (format output-ninja "~A, " objective-price)
           (if (member *data-name* '(US.D1B TY.D1B))
            
               (format output-ninja "~7@A," (my-round objective-price 5))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~D, "
              (string-append "~7," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round objective-price (index-tick-size))))))
                  )
             )

              (format output-ninja " ~A" qty);;;quantity of contracts

        (format output-ninja "~C~%" #\cr)
        ;   (format output-ninja " ~A," 3) ;(second (assoc *data-name* *dubai-spread-list*)))              
        ;   (format output-ninja " ~A~%"   (if (index-reel)(index-reel) 1))
           ;(format output-ninja " ~A~%" (index-tick-size))

    ));;closes the let and defun

;;;TradeStation

;;;type refers to buy or sell or cover
;;;TradeStation
;;;;type, symbol, contract, time zone, release date/time, cancel date/time, exit date/time, entry price,
;;;stop loss price, objective(limit) price, qty
;;;this is for a counter trend systme with no sltop loss
;;;there is no cancel time or exit time for a swing system
(defun write-trend-swing-record (output-ninja tdate date1 csignal stop-entry stop-long stop-short objective-price)
   (let (ts-symbol contract-month)

        (setq ts-symbol (get-ts-symbol *data-name*)
         
      ;   entry-time (or (second (assoc *data-name* *foremost-market-times-list*)) "09:00:03") 
         contract-month (ts-contract-month (contract-month *data-name* tdate)) 
        )
    
     ;  (if (eql csignal 'BUY-MOO)(format output-ninja "~13@A" "IF NOT LONG  ")) 
     ;  (if (eql csignal 'BUY-AT-STOP-ENTRY)(format output-ninja "~13@A"  "IF NOT LONG  ")) 
     ;  (if (eql csignal 'SELL-MOO)(format output-ninja "~13@A" "IF NOT SHORT ")) 
     ;  (if (eql csignal 'SELL-AT-STOP-ENTRY)(format output-ninja "~13@A"  "IF NOT SHORT ")) 
     ;  (if (eql csignal 'HOLD-LONG)(format output-ninja "~9@A" "IF LONG ")) 
     ;  (if (eql csignal 'HOLD-SHORT)(format output-ninja "~9@A" "IF SHORT ")) 

       (format output-ninja "~18A\, ~3@A~3A\, "
	          csignal ts-symbol contract-month               
                 )

     ;  (if (member csignal '(OUT))
      ;      (format output-ninja " ")
          (format output-ninja " ~A\," (date-convert (get-release-date date1)))
    ;  (format output-ninja "~6@A, ~7,0@F, ~7,0@F," stop-entry stop-loss objective-price)  
					; (format output-ninja " STOP ENTRY = ")
	  
       (ifn (zerop stop-entry)
        (if (stringp stop-entry)
             (format output-ninja "~13@A , " stop-entry)
           (if (member *data-name* '(US.D1B TY.D1B))
            
               (format output-ninja "~13@A , " (convert-to-32nds (my-round stop-entry 5)))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~13@A, "
              (string-append "~13," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))
               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round stop-entry (index-tick-size))))))
                  )
          (format output-ninja "~9A" "     N/A     ,"))
	  
   ;    (when (eql csignal 'BUY-AT-STOP-ENTRY)
      ; (format output-ninja " STOP LONG = ")
       (if stop-long
        (if (stringp stop-long)
             (format output-ninja "~A, " stop-long)
           (if (member *data-name* '(US.D1B TY.D1B))
            
               (format output-ninja "~13@A, " (convert-to-32nds (my-round stop-long 5)))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~13@A, "
              (string-append "~13," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F, "))

               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round stop-long (index-tick-size))))))
                  )
             )
     ;  (when (eql csignal 'SELL-AT-STOP-ENTRY)
      ; (format output-ninja " STOP SHORT = ")
       (if stop-short
        (if (stringp stop-short)
             (format output-ninja "~A, " stop-short)
           (if (member *data-name* '(US.D1B TY.D1B))
            
               (format output-ninja "~13@A," (convert-to-32nds (my-round stop-short 5)))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~16@A,"
              (string-append "~16," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))

               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round stop-short (index-tick-size))))))
                  )
             )

        
       (when objective-price
        (format output-ninja " OBJECTIVE EXIT = ")     
        (if (stringp objective-price)
             (format output-ninja "~A, " objective-price)
           (if (member *data-name* '(US.D1B TY.D1B))            
               (format output-ninja "~13@A," (convert-to-32nds (my-round objective-price 5)))
            (format output-ninja
             (if (and (index-digits)
                      (zerop (index-digits)))
                 "~9@A, "
              (string-append "~9," (format nil "~A" (third (assoc *data-name* *ts-price-conversion-factor*))) ",0,'*,' F,"))

               (* (cadr (assoc *data-name* *ts-price-conversion-factor*))
                   (* (index-tick-size) (round objective-price (index-tick-size))))))
                  )
             )
       (format output-ninja " ~9@A,"(if (member *data-name* '(ty.d1b us.d1b))
					(convert-to-32nds (my-pretty-price (/ (- stop-long stop-short) 2)))
					(my-pretty-price (/ (- stop-long stop-short) 2)))) 
 
  
       (format output-ninja "  ~9@A" (if (member *data-name* '(us.d1b ty.d1b))
					 (convert-to-32nds (getd tdate 'close))
					 (my-pretty-price (getd tdate 'close))))
;	 (format output-ninja " ~3A," (portf tdate *data-name* ))

            ;  (format output-ninja " ~A" qty);;;quantity of contracts
   #|    
       (if (eql csignal 'BUY-AT-MARKET-OPEN)(format output-ninja "~%~A~2%" " IF ALREADY LONG JUST ADJUST STOP LOSS AND OBJECTIVE PRICES")) 
       (if (eql csignal 'BUY-AT-STOP-ENTRY)(format output-ninja "~%~A~2%" " IF ALREADY LONG JUST ADJUST STOP LOSS AND OBJECTIVE PRICES ")) 
       (if (eql csignal 'HOLD-LONG)(format output-ninja "~%~A~2%" " JUST ADJUST STOP LOSS AND OBJECTIVE PRICES ")) 
       (if (eql csignal 'SELL-AT-MARKET-OPEN)(format output-ninja "~%~A~2%" " IF ALREADY SHORT JUST ADJUST STOP LOSS AND OBJECTIVE PRICES")) 
       (if (eql csignal 'SELL-AT-STOP-ENTRY)(format output-ninja "~%~A~2%" " IF ALREADY SHORT JUST ADJUST STOP LOSS AND OBJECTIVE PRICES")) 
       (if (eql csignal 'HOLD-SHORT)(format output-ninja "~%~A~2%" " JUST ADJUST STOP LOSS AND OBJECTIVE PRICES")) 
|#
    
       ; (format output-ninja "~C~%" #\cr)
         (format output-ninja "~%")

        ;   (format output-ninja " ~A," 3) ;(second (assoc *data-name* *dubai-spread-list*)))              
        ;   (format output-ninja " ~A~%"   (if (index-reel)(index-reel) 1))
           ;(format output-ninja " ~A~%" (index-tick-size))

    ));;closes the let and defun


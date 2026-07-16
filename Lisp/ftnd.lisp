;;; -*- Mode: LISP; Package: user; Base: 10. -*


#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


(defvar *trends* nil "assoc list of trend results")

;;;FIND ALL THE FIBO TRENDS IN THE DATA

;;;this is top level function for ELLIOTT's COUNTRY COUSIN

(defun elmo (start-time stop-time				    
			&optional (outfile t)(hww *terminal-io*)			
			end-time)
 
  (block FAT
    (let* ((startday (getnumdate start-time))
	   (starttime (getnumh start-time))
	   (startprice (getd startday 'pdata starttime))
	   (endday (if (stringp end-time)
		       (getnumdate end-time)		  
		     (getnumdate stop-time)))
	   (endtime (if (stringp end-time)
			(getnumh end-time)	
		      (getnumh stop-time))) ;signals 
		      williams-revs fibo-revs 
	   (degree 1) (filt *n-filt*) priorday priortime priorprice
	   nextday nexttime nextprice next-start-index ignore rwp
	   ;(weights '(1.0 1.0 1.0 1.0 1.0))	     
	   next-start-time trend-results last-index)
					;   (declare (ignore ignore))
   ;   (format t "~A  ~A ~A ~A ~A" startday starttime startprice endday endtime)
      (ifn end-time (setq end-time stop-time))
      (setq *trends* nil) (maind-x)(set-cat-list)
      
      (putprop '*trends* endday 'endday)
      (putprop '*trends* endtime 'endtime)
      (if (stringp (data-access startday))
	  (return-from fat (data-access startday)))
      ;;;sets the starttime if you don't know it
      (ifn starttime
	   (setq starttime
		 (multiple-value-bind (ignore ydate ndate times prices)
		     (data-access startday)
		   (declare (ignore ignore))
		   (if (eql (min* prices)
			    (min* (append
				    (nth 4 (multiple-value-list (data-access ydate)))
				    prices
				    (nth 4 (multiple-value-list	(data-access ndate))))))
		       (nth
			 (- (length prices)
			    (length (member (min* prices) prices))) times)
		     (nth
		       (- (length prices)
			  (length (member (max* prices) prices))) times)))))
      
      (multiple-value-setq (priorday priortime priorprice)
	(prior-data-point startday starttime startprice))
      (multiple-value-setq (nextday nexttime nextprice)
	(next-data-point startday starttime startprice))
      ;;;find index of first nil value
      (setq last-index
	    (if (and (vectorp (get-tpv 0))(svref (get-TPV 0) 3))
		(do ((ith 0 (1+ ith)))
		    ((or (stringp (and (vectorp (get-tpv ith))(svref (get-TPV ith) 1)))
			 (null (get-TPV ith)))
		     (and (vectorp (get-tpv (1- ith)))(svref (get-TPV (1- ith)) 0))))))

      (tpv-vector-data startday starttime startprice endday endtime
				 priorday priortime priorprice
				 nextday nexttime nextprice last-index hww)
      ;;;initially set the filter size to half the time length.(approximate)
      (setq *n-filt* (min (nearest-fibonum (truncate (1+ *end-index*) 8))
                      233))
      
      (and hww (format hww "~%SETTING LARGEST FILTER = ~A" *n-filt*))
      (when (<= *n-filt* 2)
	(and (format hww "*data-name*=~A *n-filt*=~A~%" *data-name* *n-filt*))
	(return-from FAT nil))
      
      ;;;for deg number 1	
      (multiple-value-setq
	  (next-start-index next-start-time ignore START-TIME
			    trend-results)
	(fibo-trend-finder 1 start-time end-time stop-time  hww))
      (loop
	(cond ((cdr (assoc 'pred-prev trend-results)) (return))
	      (t (setq *n-filt* (lower-fibonum *n-filt*))
		 (multiple-value-setq
		     (next-start-index next-start-time ignore START-TIME
				       trend-results)
		   (fibo-trend-finder 1 start-time end-time
				      stop-time  hww)))))
      (and hww (format hww "~% next-start-time = ~S~% trend-results= ~S" next-start-time
	      trend-results))
      (setq *trends* (acons degree trend-results  *trends*))
      
      (loop
	(incf degree)(setq *n-filt* (lower-fibonum *n-filt*))
	(and hww (format hww "~%DEGREE = ~A" degree))
	(if next-start-time
	    (multiple-value-setq
		(next-start-index next-start-time ignore start-time
				  trend-results)
	      (fibo-trend-finder next-start-index next-start-time
				 end-time stop-time hww))
	  (return))
	(if (<= *n-filt* 13)(return))
	(and (format hww "~% next-start-time = ~A ~% trend-results= ~A"
		next-start-time	trend-results))
	(if next-start-time
	    (setq *trends*
		  (acons degree trend-results  *trends*))
	  (return)));closes loop
      ;;;find index of stop-time
      (do ((ith (cadr (assoc 'index-list (cdr (car *trends*)))) (1+ ith)))
	  ((and (eql (and (vectorp (get-tpv ith))(svref (get-TPV ith) 1)) endday)
		(or (not endtime)
		    (eql (and (vectorp (get-tpv ith))(svref (get-TPV ith) 2)) endtime)))
	   (setq *end-index* ith)))
      
      (setq *trends* (reverse *trends*))
      (add-tcode) (zero-line)
    ;  (add-gcode)
      
      (if outfile 
           (with-open-file (stream (string-append "c:/users/ep-da/elmo/elmolevels.dat")
                           :direction :output :if-exists :append :if-does-not-exist :create)
            (when (eql *time-interval* 'daily-high-low)
            (print-string stream (format nil "~A" *data-name*) 3)
            	                
	    (format stream "  ~A  ~A  ~A" endday (if (member *data-name* '(TY.D1B US.D1B))
	             (convert-to-32nds (getd endday 'close))(getd endday 'close))(short-term-trend endday))
	    (format stream " ~A" (dow-trend endday 11))
	    (if (setq rwp (reversal-weekp endday)) (format stream " ~A" rwp))         
	   ; (setq signals (formation endday))	
	   ; (dolist (ith signals)
	   ;    (if ith (format stream " ~A" ith)))
	    (format stream "~%")	   
            (let ((resist (resistance-levels endday)))
                 (dotimes (jth (min 10 (length resist)))
                  (cond ((member *data-name* '(TY.D1B US.D1B)) (format stream "~A " (convert-to-32nds (nth jth resist))))
                        ((member *data-name* '(DJIA DJ.D1B CC.D1B NK.D1B))(format stream "~A " (round (nth jth resist))))
                        ((member *data-name* '(SP.D1B ND.D1B RU.D1B CL.D1B C.D1B BO.D1B SM.D1B S.D1B W.D1B GC.D1B SI.D1B CP.D1B PL.D1B PA.D1b HU.D1B CT.D1B CF.D1B
                                               SU.D1B OJ.D1B))
                         (format stream "~,2,,,F " (my-round (nth jth resist) 2)))
                         ;(print-object (my-round (nth jth resist) 2) stream))
                        ((member *data-name* '(LC.D1B LH.D1B NG.D1B MX.D1B))
                         (format stream "~,3,,,F " (my-round (nth jth resist) 3)))
                         ;(print-object (my-round (nth jth resist) 3) stream))
                        (t (format stream "~,4,,,F " (my-round (nth jth resist) 4)))
                          ;(print-object (my-round (nth jth resist) 4) stream)
                                         )
                     
                      
                      ))
             (format stream "~%")
             (let ((supp (support-levels endday)))
                 (dotimes (jth (min 10 (length supp)))
                   (cond ((member *data-name* '(TY.D1B US.D1B))(format stream "~A " (convert-to-32nds (nth jth supp)) ))
                         ((member *data-name* '(DJIA DJ.D1B CC.D1B NK.D1B))(format stream "~A " (round (nth jth supp))))
                         ((member *data-name* '(SP.D1B ND.D1B RU.D1B CL.D1B C.D1B BO.D1B SM.D1B S.D1B W.D1B GC.D1B SI.D1B CP.D1B PL.D1B PA.D1B HU.D1B CT.D1B CF.D1B
                                                SU.D1B OJ.D1B))
                          (format stream "~,2,,,F " (my-round (nth jth supp) 2)))
                          ;(print-object (my-round (nth jth supp) 2) stream))
                         ((member *data-name* '(LC.D1B LH.D1B NG.D1B MX.D1B))
                          (format stream "~,3,,,F " (my-round (nth jth supp) 3)))
                          ;(print-object (my-round (nth jth supp) 3) stream))
                         (t (format stream "~,4,,,F " (my-round (nth jth supp) 4)))
                           ;(print-object (my-round (nth jth supp) 4) stream)))
                           )
                       
                       ))
             (format stream "~%~%"))
                       ))

#|                      
       (if outfile
            (with-open-file (stream (string-append "/home/register/elmo/elmozero.dat")
                              :direction :output :if-exists :append :if-does-not-exist :create)

             (dolist (ith *trends*)
                  (when (= (cdr (assoc '*n-filt* (cdr ith))) 5)
                        (cond ((eql (cdr (assoc 'up-dn (cdr ith))) 'low)
	                         (pushnew (cdr (assoc 'pred-low (cdr ith))) supp :test #'=)
	                         (pushnew (cdr (assoc 'pred-high (cdr ith))) supp :test #'=)
	                         (if (>= (cdr (assoc 'recent-extreme (cdr ith)))
		                     (cdr (assoc 'pred-next (cdr ith))))
		                 (pushnew (cdr (assoc 'pred-next (cdr ith))) supp :test #'=)))
	                      (t (pushnew (cdr (assoc 'pred-high (cdr ith))) supp :test #'=)
	                         (pushnew (cdr (assoc 'pred-next (cdr ith))) supp :test #'=)))
                                 (if (eql (cdr (assoc 'up-dn (cdr ith))) 'low)
	                             (pushnew (/ (+ (second (assoc 'hl-list (cdr ith)))
		                                    (third (assoc 'hl-list (cdr ith)))) 2) supp :test #'=))))
            
                  (print-string stream (format nil "~A" *data-name*) 3)
                  (format stream "  ~7@A  ~@4@A   ~7@A   ~7@A   ~7@A   ~7@A~%"
                     (getd endday 'close)
                      dirp
                     (my-round (ave endday 3) 4);;entry                        
                     (my-round (cond ((eql dirp 'UP)(- (ave endday 4) obj))
                                     ((eql dirp 'DOWN)(+ (ave endday 4) obj))
                                     (t nil)) 4)
                     (my-round (cond ((eql dirp 'UP) (+ (ave endday 4) obj));;objective
                                     ((eql dirp 'DOWN) (- (ave endday 4) obj))
                                     (t nil)) 4)
                     (my-round (getd (add-mkt-days endday -3) 'close) 4)                )
                  
                 ))
|#
                              
      (setq williams-revs 
                  (sort
		      (do ((ith (car *trends*) (car trends))
			   (trends (cdr (copy-list *trends*)) (cdr trends))
			   (dmy nil))
			  ((null trends) dmy)
			
			(if (or (and (eql *time-interval* 'daily-high-low)
			             (>= (cdr (assoc '*n-filt* (cdr ith))) 13)
			             (<= (cdr (assoc '*n-filt* (cdr ith))) 144))
			        (and (neql *time-interval* 'daily-high-low)
			             (>= (cdr (assoc '*n-filt* (cdr ith))) 8)
			             (<= (cdr (assoc '*n-filt* (cdr ith))) 144)))
			    (setq dmy (append (cdr (assoc  'rcode (cdr ith))) dmy))))
		      #'<))
       (if (eql *time-interval* 'daily-high-low) (setq williams-revs (mapcar #'(lambda (s) (ceiling (/ s 2))) williams-revs)))
       
      (setq fibo-revs
            (sort     (do ((ith (car *trends*) (car trends))
			   (trends (cdr (copy-list *trends*)) (cdr trends))
			   (dmy nil))
			  ((null trends) dmy)
			
			(if (or (and (eql *time-interval* 'daily-high-low)
			             (>= (cdr (assoc '*n-filt* (cdr ith))) 21)
			             (<= (cdr (assoc '*n-filt* (cdr ith))) 144))
			        (and (neql *time-interval* 'daily-high-low)
			             (>= (cdr (assoc '*n-filt* (cdr ith))) 13)
			             (<= (cdr (assoc '*n-filt* (cdr ith))) 144)))
			    (setq dmy (append (cdr (assoc  'gcode (cdr ith)))  dmy))))
		      #'<))
      (if (eql *time-interval* 'daily-high-low) (setq fibo-revs (mapcar #'(lambda (s) (ceiling (/ s 2))) fibo-revs)))
      (if outfile
	  (with-open-file (stream
			    (string-append "/home/register/elmo/elmoturns.dat")
			    :direction :output :if-exists :append :if-does-not-exist :create)
	    
	    (unless (eql *time-interval* 'daily-high-low)
	            (format stream "~A\,~A\," *data-name* endday)	   
		   (format stream "~A~%" *time-interval*))
;  (format stream " CLOSEST WILLIAM's TIME REVERSAL PROJECTIONS~%")
	       
	      (dotimes (kth (min 24 (length williams-revs)))
		(print-object (nth kth williams-revs) stream)
		 (if (< kth (min 23 (- (length williams-revs) 1)))(format stream "\,")))
	      
	    (FORMAT STREAM "~%")
 ; (format stream " CLOSEST FIBO TIME RANGE REVERSAL PROJECTIONS~%")
	                    
	      (dotimes (kth (min 24 (length fibo-revs)))
		(print-object (nth kth fibo-revs) stream)
		(if (< kth (min 23 (- (length fibo-revs) 1)))(format stream "\,")))
	    (FORMAT STREAM "~%")
	    ));closes the output stream
      ;returns globals to original value
      (setq *n-filt* filt)
      (sort (append williams-revs fibo-revs) #'<)
      )))


;;;;asumes elmos is a complete sorted list of days
;;;;for projected market turns      
(defun cluster-elmos (elmos)
  (block nil
    (if (eql (length elmos) 1) (return elmos))
   (let (clust dmy lst)
      (do ((elmo-list elmos (cdr elmo-list))
           (ith (car elmos) (car elmo-list))
           (ith1 (second elmos) (second elmo-list))
           (ith2 (third elmos)(third elmo-list)))
       ((not ith1))
    (if (or (eql ith ith1)(eql ith (1- ith1)))
        (if (or (eql ith1 ith2)(not ith2)(eql ith1 (1- ith2)))
            (push ith clust)
           (setq clust (cons ith clust) clust (cons ith1 clust)
                 dmy (cons clust dmy) clust nil))))

     (dolist (ith dmy)
       (push (ceiling (/ (list-sum ith)(length ith))) lst))
  lst     
)))      

;;;
(defun get-elmo (stop-date num)
  (let ((time-interval *time-interval*) elmo-turns start-date)
  
   (setq start-date (format nil "~SA" (add-mkt-days stop-date (- num))))
   (ifn (stringp stop-date) (setq stop-date (format nil "~SP" stop-date)))
   (setq *time-interval* 'daily-high-low)
   ;(format T "~A  ~A" start-date stop-date)
   (setq elmo-turns (elmo start-date stop-date nil nil ))
   (setq *time-interval* 1440)
   (setq elmo-turns
    (append elmo-turns 
            (elmo (getstrdate-from-string start-date) (getstrdate-from-string stop-date) nil nil)))
   (setq *time-interval* time-interval) 
                        
					; (setq elmo-turns (cluster-elmos (sort elmo-turns #'<)))
   (setq elmo-turns (sort elmo-turns #'<))
  ; (setq elmo-turns (remove-duplicates elmo-turns :test #'=))   
   (setq elmo-turns (remove-if #'(lambda(s)(or (< s -1)(> s 1))) elmo-turns))
   (setq stop-date (getnumdate stop-date))
  ; (unless (or (n-day-highp stop-date 10)(n-day-lowp stop-date 10);
;	       (n-day-highp (getd stop-date 'ydate) 10)
;	       (n-day-lowp (getd stop-date 'ydate) 10))(setq elmo-turns '()))
  (length elmo-turns)
))

(defun fibo-trend-finder (start-index start-time 
				      &optional (end-time *curr-et*)
				 stop-time hww)
  (let (DJIA-LIST TIMES-LIST index-list PRED-LOW PRED-HIGH PRED-NEXT
		  PRED-PREV PRED-PREV-1 PRED-PREV-2 UP-DN ENDT-PTI
		  PDATA-PTI  price-to-go time-to-go
		  (endday (getnumdate end-time)) recent-extreme)

    (and hww (format hww "~%~A trying *n-filt*=~A" *data-name* *n-filt*))
    (multiple-value-setq
	(djia-list times-list index-list PRED-LOW PRED-HIGH PRED-NEXT
		   PRED-PREV PRED-PREV-1 PRED-PREV-2 UP-DN 
		   ENDT-PTI PDATA-PTI price-to-go time-to-go recent-extreme)
      (find-prims1 start-index start-time end-time hww stop-time))

    (ifn (zerop *n-filt*)
	 (values (cond ((nth 5 index-list))
		       ((car (last index-list)))) ;index of the new starttime
		 (cond ((nth 5 times-list))
		       ((car (last times-list)))) ; this is new start-time
		 START-INDEX START-TIME
		 `((*n-filt* . ,*n-filt*)
		   (HL-list . ,djia-list)
		   (TIMES-list . ,times-list)
		   (INDEX-LIST . ,INDEX-LIST)
		   (END-PRICE . ,(getd endday 'pdata
				       (getnumh end-time)))
				      ; (if (> (length end-time) 6)
				;	   (read-from-string
				;	     (subseq end-time 6 10)))))
		   (PRED-LOW . ,PRED-LOW)
		   (PRED-HIGH . ,PRED-HIGH)
		   (PRED-NEXT . ,PRED-NEXT)
		   (PRED-PREV . ,PRED-PREV)
		   (PRED-PREV-1 . ,PRED-PREV-1)
		   (PRED-PREV-2 . ,PRED-PREV-2)
		   (UP-DN . ,UP-DN)
		   (ENDT-KNOWN . ,ENDT-PTI)
		   (PRICE-KNOWN . ,PDATA-PTI)
		   (start-index . ,start-index)
		   (start-time . ,start-time)
		   (price-to-go . ,price-to-go)
		   (time-to-go . ,time-to-go)
		   (recent-extreme . ,recent-extreme))))
    ))


(defun find-prims1 (start-index start-time end-time hww &optional stop-time)
  (block nil
    (let ((DJIA-LIST 
	    (list (svref (get-TPV start-index) 3)))
	  (times-list (list start-time))
	  (index-list (list start-index)) recent-extreme
	  (endday (getnumdate end-time))(endtime (getnumh end-time))
	  UP-DN PRED-HIGH PRED-LOW PRED-NEXT PRED-PREV pred-prev-1 result
	  pred-prev-2 ENDT-PTI PDATA-PTI aa bb cc dd curr-ET ignore)
;	(declare (ignore ignore))
      (setq curr-ET start-time *maxq* -1)
      (spring-time nil 0 nil nil t nil)  ; used  for working storage
;      (spring-time nil 1 nil nil t nil)  ; used to convey primitive to main
;using slot 'dg to store the index of the time for the last primitive
;;;hope this isn't too confusing
      (do ((cur-start-time start-time (getv 0 ET))
	   (cur-start-index start-index (getv 0 DG))
	   (i 0 (1+ i)))
	  ((or (string= END-TIME curr-ET)
	       (and (=  endday (getnumdate curr-et))
		  (or (not endtime)
		      (case endtime
			(A t)
			(P (eql 'P (getnumh curr-et)))
			(t (<= (time-units-into-day endday endtime)
			       (time-units-into-day (getnumdate curr-et)
						    (getnumh curr-et)))))))

	       ))
	(setq endt-pti aa pdata-pti bb)
;THE cur-start-index sets prices-only true
	(if (stringp start-index) (break "111"))
	(multiple-value-setq (aa bb ignore ignore ignore cc dd)
	  (provide-primitive cur-start-time stop-time
			     cur-start-index))
	(if *primitive* (setq curr-ET (getv 0 ET)))
	(unless *PRIMITIVE*
	  ;(PRINT "YOU HAVE REACHED THE END OF YOUR DATA")

	  (RETURN))
	;;; INFORMATION ABOUT CURRENT POINT PTI
	(and hww (format hww " ~A" (1+ i)))
	(push (getv 0 EP) DJIA-LIST)
	(push (getv 0 ET) times-list)
	(push (getv 0 DG) index-list);not really 'dg index of start-time
	(case  (getv 0 DR)
	  (DOWN (setq  UP-DN 'LOW))
	  (UP (setq UP-DN 'HIGH)))
	(when (>= i 3)
	  (cond ((EQL (getv 0 DR) 'DOWN )
		 (setq PRED-LOW
		       (my-round (setq result
				       (exp (+ (log (nth 0 djia-list))
					 (- (log (nth 1 djia-list))
					    (log (nth 3 djia-list))))))
				 (cond ((< result 10) 4)
				       ((< result 100) 3)
				       (t 2)))
		       PRED-HIGH
		       (my-round (setq result
				       (exp (+ (log (nth 1 djia-list))
					       (- (log (nth 2 djia-list))
						  (log (nth 4 djia-list))))))
				 (cond ((< result 10) 4)
				       ((< result 100) 3)
				       (t 2)))
		      ))
		(t (setq PRED-HIGH
			 (my-round (setq result
					 (exp (+ (log (nth 0 djia-list))
						 (- (log (nth 1 djia-list))
						    (log (nth 3 djia-list))))))
				   (cond ((< result 10) 4)
					 ((< result 100) 3)
					 (t 2)))
			 PRED-LOW
			 (my-round (setq result
					 (exp (+ (log (nth 1 djia-list))
						 (- (log (nth 2 djia-list))
						    (log (nth 4 djia-list))))))
				   (cond ((< result 10) 4)
					 ((< result 100) 3)
					 (t 2)))
			 
			 ))))
	);closes the DO over primitives

      (if (>= (length djia-list) 3)
	  (setq pred-next
		(my-round
		  (setq result
			(exp (+ (log (if (eql up-dn 'high)
					 (getv 0 LP)(getv 0 HP)))
				(- (log (nth 0 djia-list))
				   (log (nth 2 djia-list))))))
			  (cond ((< result 10) 4)
				((< result 100) 3)
				(t 2)))
		recent-extreme (if (eql up-dn 'high)
				   (getv 0 LP)(getv 0 HP))))
      (if (>= (length djia-list) 6)
	  (setq pred-prev
		(my-round (setq result
				(exp (+ (log (nth 2 djia-list))
					(- (log (nth 3 djia-list))
					   (log (nth 5 djia-list))))))
			  (cond ((< result 10) 4)
				((< result 100) 3)
				(t 2)))
		))
      (if (>= (length djia-list) 7)
	  (setq pred-prev-1
		(my-round
		  (setq result (exp (+ (log (nth 3 djia-list))
				       (- (log (nth 4 djia-list))
					  (log (nth 6 djia-list))))))
		  (cond ((< result 10) 4)
			((< result 100) 3)
			(t 2)))
		))
      (if (>= (length djia-list) 8)
	  (setq pred-prev-2
		(my-round (setq result
				(exp (+ (log (nth 4 djia-list))
					(- (log (nth 5 djia-list))
					   (log (nth 7 djia-list))))))
			  (cond ((< result 10) 4)
				((< result 100) 3)
				(t 2)))
		))
      (values djia-list  TIMES-LIST index-list
	      PRED-LOW PRED-HIGH PRED-NEXT PRED-PREV PRED-PREV-1
	      PRED-PREV-2 UP-DN
	      ENDT-PTI PDATA-PTI cc dd recent-extreme))))

   


(defun zero-line ()
  (let (pos prev-hl pred-prev end-price pred-high pred-low); age)
    (dolist (ith *trends*)
      (setq prev-hl (nth 1 (assoc 'HL-LIST (cdr ith))))
      (setq pred-prev (cdr (assoc 'pred-prev (cdr ith))))
      (setq end-price (cdr (assoc 'end-price (cdr ith))))
      (setq pred-high (cdr (assoc 'pred-high (cdr ith))))
      (setq pred-low (cdr (assoc 'pred-low (cdr ith))))
     ; (setq age (cdr (assoc 'age ith)))
      
      (case  (and pred-prev (cdr (assoc 'UP-DN (cdr ith))))
	(LOW (if (or (> end-price pred-high)
		     (and (> end-price pred-prev)
			  (> prev-hl pred-prev)
					; (<= age 1.25))
			  ))
		 (setq pos 'A) (setq pos 'D)))
	(HIGH (if (or (< end-price pred-low)
		      (and (< end-price pred-prev)
			   (< prev-hl pred-prev)
					; (<= age 1.25))
			   ))
		  (setq pos 'D)(setq pos 'A)))
	(otherwise (setq pos 'F)))
      (nconc ith (list (cons 'ZERO-LINE  pos))))))


;;;returns the projections under the last price
(defun support-levels (endday &aux supp close-price)
  (dolist (ith *trends*)
    (unless (< (cdr (assoc '*n-filt* (cdr ith))) 4)
      (cond ((eql (cdr (assoc 'up-dn (cdr ith))) 'high)
	     (pushnew (cdr (assoc 'pred-low (cdr ith))) supp :test #'=)
	     (pushnew (cdr (assoc 'pred-high (cdr ith))) supp :test #'=)
	     (if (<= (cdr (assoc 'recent-extreme (cdr ith)))
		     (cdr (assoc 'pred-next (cdr ith))))
		 (pushnew (cdr (assoc 'pred-next (cdr ith))) supp :test #'=)))
	    (t (pushnew (cdr (assoc 'pred-low (cdr ith))) supp :test #'=)
	       (pushnew (cdr (assoc 'pred-next (cdr ith))) supp :test #'=)))
      (if (eql (cdr (assoc 'up-dn (cdr ith))) 'high)
	  (pushnew (/ (+ (second (assoc 'hl-list (cdr ith)))
			 (third (assoc 'hl-list (cdr ith)))) 2) supp :test #'=))))
			 
 ; (setq supp (append (pivot-points endday) supp))
 ; (setq supp (append (pivot-points-weekly endday) supp))
 ; (setq supp (append (pivot-points-momthly endday) supp))
 ; (setq supp (append (2-bar endday) supp))
 ; (setq supp (append (2-bar-weekly endday) supp))
  (setq close-price (getd endday 'close))
  			 		 
  (setq supp (remove-if
	       #'(lambda (s) (< close-price s))
	       supp))
  (vsort supp #'>)
  )
(defun resistance-levels (endday &aux supp close-price)
  (dolist (ith *trends*)
    (unless (< (cdr (assoc '*n-filt* (cdr ith))) 4)
      (cond ((eql (cdr (assoc 'up-dn (cdr ith))) 'low)
	     (pushnew (cdr (assoc 'pred-low (cdr ith))) supp :test #'=)
	     (pushnew (cdr (assoc 'pred-high (cdr ith))) supp :test #'=)
	     (if (>= (cdr (assoc 'recent-extreme (cdr ith)))
		     (cdr (assoc 'pred-next (cdr ith))))
		 (pushnew (cdr (assoc 'pred-next (cdr ith))) supp :test #'=)))
	    (t (pushnew (cdr (assoc 'pred-high (cdr ith))) supp :test #'=)
	       (pushnew (cdr (assoc 'pred-next (cdr ith))) supp :test #'=)))
      (if (eql (cdr (assoc 'up-dn (cdr ith))) 'low)
	  (pushnew (/ (+ (second (assoc 'hl-list (cdr ith)))
			 (third (assoc 'hl-list (cdr ith)))) 2) supp :test #'=))))
 
 ; (setq supp (append (pivot-points endday) supp))
 ; (setq supp (append (pivot-points-weekly endday) supp))
 ; (setq supp (append (pivot-points-monthly endday) supp))
 ; (setq supp (append (2-bar endday) supp))
  ;(setq supp (append (2-bar-weekly endday) supp))
   (setq close-price (getd endday 'close))
  			 
  (setq supp (remove-if
	       #'(lambda (s) (> close-price s)) supp))
  (vsort supp #'<)
  )

(defun nearest-fibonum (num &aux (nth+1 0) (nth 1) (nth-1 0))
  (loop
    (setq nth+1 (+ nth nth-1))
    (if (< num nth+1)
	(return (if (< (- num nth) (- nth+1 num)) nth nth+1)))
    (setq nth-1 nth nth nth+1)))

(defun lower-fibonum (num &aux (nth+1 0) (nth 1) (nth-1 0))
  (loop
    (setq nth+1 (+ nth nth-1))
    (if (< num nth+1)
	(return (if (= num nth) nth-1 nth)))
    (setq nth-1 nth nth nth+1)))

(defun fibo-trend-score (scale weights)
  (let ((score 0) ldeg dmy filt dagep dirp jth s-index dmy2)
;;;for *n-filt* 2 3 and 5 what are the corresponding degree numbers   
    (setq ldeg (dolist (ith *trends*)
		 (if (eql (cdr (assoc '*n-filt* (cdr ith))) scale)
		     (return (car ith)))))
    (setq score (+ score (age-score ldeg)))
    (setq filt (cdr (assoc '*n-filt* (assoc ldeg *trends*))))
    (setq ldeg (+ ldeg (1- (length weights))))
    (dotimes (ith (length weights))
     (setq score
	    (+ score (* (nth ith weights) (scale-score (- ldeg ith))))))

;;;SCODES for lower degrees
;;;first for 1 lower degree than weights goes
;;;size =2 for "21" filter    
    (setq jth (assoc (1+ ldeg) *trends*))

    (when jth
      (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))
      (cond ((and (> (cdr (assoc 'age jth)) .5)
		  (member (cdr (assoc 'scode jth)) '(0)))
	     (if dirp (setq score (+ -.5 score))(setq score (+ .5 score))))
	    ((member (cdr (assoc 'scode jth)) '(2))
	     (if dirp (setq score (+ -.25 score))(setq score (+ .25 score))))
	    ((member (cdr (assoc 'scode jth)) '(1 3))
	     (if dirp
		 (setq score (+ .15 score))(setq score (+ -.15 score)))))
      (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		  (> (cdr (assoc 'age jth)) 2.4))
	     (if dirp
		 (setq score (+ -.5 score))(setq score (+ .5 score)))))

      (cond ((equal (cdr (assoc 'tcode jth)) "OB")	     
		 (setq score (+ -.25 score)))
	    ((equal (cdr (assoc 'tcode jth)) "OS")
	     (setq score (+ .25 score))))
      (cond ((and (equal (cdr (assoc 'tcode jth)) "S1")
		  (equal (cdr (assoc 'mcode jth)) "UO")
		  (member (cdr (assoc 'scode jth)) '(1 3)))
	     (setq score (+ .5 score)))
	    ((and (equal (cdr (assoc 'tcode jth)) "S1")
		  (equal (cdr (assoc 'mcode jth)) "UO"))
		  (setq score (+ .25 score)))
	    ((and (equal (cdr (assoc 'tcode jth)) "W1")
		  (equal (cdr (assoc 'mcode jth)) "DU")
		  (member (cdr (assoc 'scode jth)) '(1 3)))
	     (setq score (+ -.5 score)))
	    ((and (equal (cdr (assoc 'tcode jth)) "W1")
		  (equal (cdr (assoc 'mcode jth)) "DU"))
	     (setq score (+ -.25 score))))

      (setq score (+ score
		     (* (if dirp -1 1)
			(cond ((> (cdr (assoc 'age jth)) 6.0) 3.0)
			      ((> (cdr (assoc 'age jth)) 5.0) 2.5)
			      ((> (cdr (assoc 'age jth)) 4.0) 2.0)
			      (t 0))))))
    
;;;4 degrees lower
;;;size= 3 for "21" filter    
    (setq jth (assoc ldeg *trends*))
    (setq filt (cdr (assoc '*n-filt* jth)))
    (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))
    (cond ((and (> (cdr (assoc 'age jth)) .5)
		(member (cdr (assoc 'scode jth)) '(0 2)))
	   (if dirp (setq score (+ -.5 score))(setq score (+ .5 score))))
	  ((member (cdr (assoc 'scode jth)) '(1 3))
	   (if dirp
	       (setq score (+ .15 score))(setq score (+ -.15 score)))))
    (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		(> (cdr (assoc 'age jth)) 2.4))
	   (if dirp
	       (setq score (+ -.5 score))(setq score (+ .5 score)))))
    (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		(equal (cdr (assoc 'tcode jth)) "OB"))
		 (setq score (+ -.25 score)))
	    ((and (member (cdr (assoc 'scode jth)) '(0 2))
		  (equal (cdr (assoc 'tcode jth)) "OS"))
	     (setq score (+ .25 score))))
;;;size =5 for "21" filter	    
    (setq jth (assoc (1- ldeg) *trends*))
    (setq filt (cdr (assoc '*n-filt* jth)))
    (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))
    (cond ((and (> (cdr (assoc 'age jth)) .5)
		(member (cdr (assoc 'scode jth)) '(0)))
	   (if dirp (setq score (+ -1.0 score))(setq score (+ 1.0 score))))
	  ((member (cdr (assoc 'scode jth)) '(2))
	   (if dirp (setq score (+ -.75 score))(setq score (+ .75 score))))
	  ((member (cdr (assoc 'scode jth)) '(1))
	   (if dirp
	       (setq score (+ .5 score))(setq score (+ -.5 score))))
	  ((member (cdr (assoc 'scode jth)) '(3))
	   (if dirp
	       (setq score (+ .25 score))(setq score (+ -.25 score)))))
    (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		(> (cdr (assoc 'age jth)) 2.4))
	   (if dirp
	       (setq score (+ -.5 score))(setq score (+ .5 score)))))

;;;3 degree smaller size=8 for "21" filter   
    (setq jth (assoc (- ldeg 2) *trends*))
    (setq filt (cdr (assoc '*n-filt* jth)))
    (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))
    (cond ((and (> (cdr (assoc 'age jth)) .5)
		(member (cdr (assoc 'scode jth)) '(0)))
	   (if dirp (setq score (+ -.5 score))(setq score (+ .5 score))))
	  ((member (cdr (assoc 'scode jth)) '(2))
	   (if dirp (setq score (+ -.5 score))(setq score (+ .5 score))))
	  ((member (cdr (assoc 'scode jth)) '(1 3))
	   (if dirp
	       (setq score (+ .25 score))(setq score (+ -.25 score)))))
    
    (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		(> (cdr (assoc 'age jth)) 2.4))
	   (if dirp
	       (setq score (+ -.5 score))(setq score (+ .5 score)))))
    

    (dolist (kth (cdr (assoc 'rcode jth)))

      (setq dagep (high-or-low-p ldeg kth filt))
      (cond ((not dagep))
	    ((not kth))
	    ((> kth (/ filt 5)))
	    ((< kth (round (- (/ filt 5)))))
	    ((and (plusp kth)(<= kth (round (/ filt 13))))
	     (setq score (+ (if (eql dagep 'LOW) .5 -.5) score)))
	    ((minusp kth)
	     (setq score (+ (if (eql dagep 'LOW) .5 -.5) score)))
	    ((zerop kth)
	     (setq score (+ (if (eql dagep 'LOW) .5 -.5) score)))))

;;;size = 13 for "21" filter    
    
   (setq jth (assoc (- ldeg 3) *trends*))
   (setq filt (cdr (assoc '*n-filt* jth)))
   (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))
   (cond ((and (> (cdr (assoc 'age jth)) .5)
	       (member (cdr (assoc 'scode jth)) '(0 2)))
	  (if dirp (setq score (+ -.5 score))(setq score (+ .5 score))))
	((member (cdr (assoc 'scode jth)) '(1 3))
	   (if dirp
	       (setq score (+ .25 score))(setq score (+ -.25 score)))))
   (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		(> (cdr (assoc 'age jth)) 2.4))
	   (if dirp
	       (setq score (+ -.5 score))(setq score (+ .5 score)))))

;;;13 day filter 8 day rate of change
    (setq s-index (- *end-index* (* 2 filt)))
    (dotimes (lth  (1+ (* 2 filt)))
      (push (-  (svref (get-tpv (+ s-index lth )) 3)
	        (svref (get-tpv
			    (+ s-index lth (- (lower-fibonum filt)))) 3)) dmy2))
    
    (cond ((< (el-index (max* dmy2) dmy2) (/ filt 5))
	   (setq score (+ 1.5 score)))
	  ((< (el-index (min* dmy2) dmy2) (/ filt 5))
	   (setq score (+ -1.5 score))))


	(dolist (kth (cdr (assoc 'rcode jth)))
;	  (setq dagep (high-or-low-p (- ldeg 3) kth filt))
	  (setq dagep (high-or-low-p (- ldeg 1) kth filt))
	  (cond ((not dagep))
		((not kth))
		((> kth (/ filt 5)))
		((< kth (- (/ filt 5))))
		((and (plusp kth)(<= kth (round (/ filt 13))))
		 (setq score (+ (if (eql dagep 'LOW) .5 -.5) score)))
		((minusp kth)
		 (setq score (+ (if (eql dagep 'LOW) .75 -.75) score)))
		((zerop kth)
		 (setq score (+ (if (eql dagep 'LOW) .75 -.75) score)))))

;;;;size = 21 for "21" filter
   (setq jth (assoc (- ldeg 4) *trends*))
   (setq filt (cdr (assoc '*n-filt* jth)))
   (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))
   (cond ((and (> (cdr (assoc 'age jth)) .5)
	       (member (cdr (assoc 'scode jth)) '(0 2)))
	   (if dirp (setq score (+ -.5 score))(setq score (+ .5 score))))
	((member (cdr (assoc 'scode jth)) '(1 3))
	   (if dirp
	       (setq score (+ .25 score))(setq score (+ -.25 score)))))
   (cond ((and (member (cdr (assoc 'scode jth)) '(0 2))
		(> (cdr (assoc 'age jth)) 2.4))
	   (if dirp
	       (setq score (+ -.5 score))(setq score (+ .5 score)))))

   
   (dolist (kth (cdr (assoc 'rcode jth)))

     (setq dagep (high-or-low-p (- ldeg 2) kth filt))
     (cond ((not dagep))
	   ((not kth))
	   ((> kth (/ filt 5)))
	   ((< kth (- (/ filt 5))))
	   ((and (plusp kth)(<= kth (round (/ filt 13))))
	    (setq score (+ (if (eql dagep 'LOW) .75 -.75) score)))
	   ((minusp kth)
	    (setq score (+ (if (eql dagep 'LOW) 1.0 -1.0) score)))
	   ((zerop kth)
	    (setq score (+ (if (eql dagep 'LOW) 1.0 -1.0) score)))))
   ;;;adds strong momentum points
;;;21 day filter 13 day rate of change
    (setq s-index (- *end-index* (* 2 filt)))
    (dotimes (lth  (1+ (* 2 filt)))
      (push (-  (svref (get-tpv (+ s-index lth )) 3)
	        (svref (get-tpv
			    (+ s-index lth (- (lower-fibonum filt)))) 3)) dmy2))
    
    (cond ((< (el-index (max* dmy2) dmy2) (/ filt 5))
	   
	   (setq score (+ 1.5 score)))
	  ((< (el-index (min* dmy2) dmy2) (/ filt 5))
	   (setq score (+ -1.5 score))))
    
;;;adds the time reversal points  for higher degrees

   (dotimes (jth (- ldeg (length weights)))
     (setq dmy (cdr (assoc 'rcode (assoc (1+ jth) *trends*))))

     (dolist (kth dmy)


       (setq dagep (high-or-low-p (+ jth 3) kth filt))
       (cond ((> kth (/ filt 5)))
	     ((< kth (- (/ filt 5))))
	     ((and (plusp kth)(<= kth (round (/ filt 13))))
	      (setq score (+ (* (if (eql dagep 'LOW) 1 -1)
				1.0) score)))
	     ((minusp kth)
	      (setq score (+ (* (if (eql dagep 'LOW) 1 -1)
				1.25 ) score)))
	     ((zerop kth)
	      (setq score (+ (* (if (eql dagep 'LOW) 1 -1)
				1.25) score))))))

       score))
(defun scale-score (ldg)
  (let ((score 0) jth  dirp )
    (setq jth (assoc ldg *trends*))
    (setq dirp (eql (cdr (assoc 'up-dn jth)) 'LOW))

    ;;;MCODES
    (cond ((equal (cdr (assoc 'mcode jth)) "UO")
	   (setq score (+ .25 score)))
	  ((equal (cdr (assoc 'mcode jth)) "DU")
	   (setq score (+ -.25 score)))
	  
	  ((and (equal (cdr (assoc 'mcode jth)) "UU")
		(> (cdr (assoc 'age jth)) 3.7))
	   (setq score (+ -.75 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "UU")
		(> (cdr (assoc 'age jth)) 2.4))
	   (setq score (+ -.5 score)))


	  ((and (equal (cdr (assoc 'mcode jth)) "DO")
		(> (cdr (assoc 'age jth)) 3.7))
	   (setq score (+ .75 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "DO")
		(> (cdr (assoc 'age jth)) 2.4))
	   (setq score (+ .5 score)))


	  ((and (equal (cdr (assoc 'mcode jth)) "DB")
		(> (cdr (assoc 'age jth)) 4.4))
	   (setq score (+ .75 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "DB")
		(> (cdr (assoc 'age jth)) 3.7))
	   (setq score (+ .5 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "DB")
		(> (cdr (assoc 'age jth)) 2.4))
	   (setq score (+ .25 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "DB")
		(<= (cdr (assoc 'age jth)) 1.4))
	   (setq score (+ -.25 score)))
	  
	  ((and (equal (cdr (assoc 'mcode jth)) "UB")
		(> (cdr (assoc 'age jth)) 4.4))
	   (setq score (+ -.75 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "UB")
		(> (cdr (assoc 'age jth)) 3.7))
	   (setq score (+ -.5 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "UB")
		(> (cdr (assoc 'age jth)) 2.4))
	   (setq score (+ -.25 score)))
	  ((and (equal (cdr (assoc 'mcode jth)) "UB")
		(<= (cdr (assoc 'age jth)) 1.4))
	   (setq score (+ .25 score))))
	  
    ;;;NCODES    
   (cond ((equal (cdr (assoc 'ncode jth)) "DO")
	   (setq score (+ .25 score)))
	  ((equal (cdr (assoc 'ncode jth)) "UO")
	   (setq score (+ .15 score)))
	  ((equal (cdr (assoc 'ncode jth)) "UU")
	   (setq score (+ -.25 score)))
	  ((equal (cdr (assoc 'ncode jth)) "DU")
	   (setq score (+ -.15 score)))
	  ((equal (cdr (assoc 'ncode jth)) "UA")
	   (setq score (+ -1.0 score)))
	  ((equal (cdr (assoc 'ncode jth)) "DA")
	   (setq score (+ 1.0 score))))
;
    ;;;AGE
    (setq score (+ score
		   (* (if dirp -1 1)
		      (cond ((> (cdr (assoc 'age jth)) 6.4) 1.5)
			    ((> (cdr (assoc 'age jth)) 5.4) 1.0)
			    ((> (cdr (assoc 'age jth)) 4.4) .75)


			    (t 0)))))
 ;   ;;;TCODE    	 
    (cond ((equal (cdr (assoc 'tcode jth)) "S1")
	   (setq score (+ .75 score)))	  
	  ((equal (cdr (assoc 'tcode jth)) "W1")
	   (setq score (+ -.75 score)))
	  
	  ((equal (cdr (assoc 'tcode jth)) "NS")
	   (if dirp
	       (setq score (+ .15 score))(setq score (+ -.15 score))))
	  ((equal (cdr (assoc 'tcode jth)) "NO")
	   (if dirp
	       (setq score (+ -.15 score))(setq score (+ .15 score))))
	  ((equal (cdr (assoc 'tcode jth)) "OB")
	   (setq score (+ -.5 score)))
	  ((equal (cdr (assoc 'tcode jth)) "OS")
	   (setq score (+ .5 score))))
    ;;;TCODE and SCODE
    (cond ((and (equal (cdr (assoc 'tcode jth)) "W2")
		(member (cdr (assoc 'scode jth)) '(0 2)))
	   (setq score (+ -1.0 score)))
	  ((and (equal (cdr (assoc 'tcode jth)) "S2")
		(member (cdr (assoc 'scode jth)) '(0 2)))
	   (setq score (+ 1.0 score))))
    (cond ((and (equal (cdr (assoc 'tcode jth)) "W2")
		(> (cdr (assoc 'age jth)) 1.5))
	   (setq score (+ -.5 score)))
	  ((and (equal (cdr (assoc 'tcode jth)) "S2")
		(> (cdr (assoc 'age jth)) 1.5))
	   (setq score (+ .5 score))))

   ;;;TCODE and age


    (cond ((and (equal (cdr (assoc 'tcode jth)) "NO")
		(member (cdr (assoc 'mcode jth)) '("DU" "DB") :test 'equal)
		(equal (cdr (assoc 'ncode jth)) "UU"))
	   (setq score (+ .35 score)))
	  ((and (equal (cdr (assoc 'tcode jth)) "NO")
		(member (cdr (assoc 'mcode jth)) '("UO" "UB") :test 'equal)
		(equal (cdr (assoc 'ncode jth)) "DO"))
	   (setq score (+ -.35 score))))
    (cond ((and (equal (cdr (assoc 'tcode jth)) "NS")
		(equal (cdr (assoc 'mcode jth)) "UB")
		(equal (cdr (assoc 'ncode jth)) "DO"))
	   (setq score (+ .15 score)))
	  ((and (equal (cdr (assoc 'tcode jth)) "NS")
		(equal (cdr (assoc 'mcode jth)) "DB")
		(equal (cdr (assoc 'ncode jth)) "UU"))
	   (setq score (+ -.15 score))))
    score))

(defun add-tcode (&aux dmy HLS ignore)
  (dolist (ith *trends*)
    (setq dmy (cdr (assoc 'hl-list (cdr ith))))
    (if (assoc 'tcode (cdr ith))
	(setf (cdr (assoc 'tcode (cdr ith)))
	      (tcode ith (car dmy) (cadr dmy) (caddr dmy)))
      (nconc ith
	     (list (cons 'TCODE
			 (tcode ith (car dmy) (cadr dmy) (caddr dmy))))))
    (when (>= (cdr (assoc '*n-filt* (cdr ith))) 8)
      (if (assoc 'rcode (cdr ith))
	  (setf (cdr (assoc 'rcode (cdr ith)))
		(multiple-value-setq (ignore hls)(rcode ith hls)))
	(nconc ith
	       (list (cons 'RCODE
			   (multiple-value-setq (ignore hls)
			     (rcode ith hls)))))))
    
      ))
(defun add-gcode (&aux (hls nil) ignore)
  
  (dolist (ith *trends*)
    (when (>= (cdr (assoc '*n-filt* (cdr ith))) 8)
      (if (assoc 'gcode (cdr ith))
	  (setf (cdr (assoc 'gcode (cdr ith)))
		(multiple-value-setq (ignore hls)(gcode ith hls)))
	(nconc ith
	       (list (cons 'GCODE
			   (multiple-value-setq (ignore hls)
			     (gcode ith hls))))))))
  )
;;;add
;(defun add-rsi ()
;  (dolist (ith *trends*)
;    (when (> (cdr (assoc '*n-filt* (cdr ith))) 5)
;      (if (assoc 'rsi (cdr ith))
;	  (setf (cdr (assoc 'rsi (cdr ith)))
;		(list (rsi (* 1 (cdr (assoc '*n-filt* (cdr ith)))))
;		      ))
;	(nconc ith
;	       (list (cons 'rsi
;			   (list (rsi (* 1 (cdr (assoc '*n-filt* (cdr ith)))))
;				 ))))))))
;;; p1 is the most recent extreme price
;;;p2 is the nextmost recent extreme price
(defun tcode (ith p1 p2 p3)
  (let ((t12 (- (first (cdr (assoc 'index-list (cdr ith))))
		(second (cdr (assoc 'index-list (cdr ith))))))
	(t23 (- (second (cdr (assoc 'index-list (cdr ith))))
		(third (cdr (assoc 'index-list (cdr ith))))))
	(t01 (- *end-index* (car (cdr (assoc 'index-list (cdr ith))))))
	 osc oscf (filt (cdr (assoc '*n-filt* (cdr ith)))))
    (setq osc (osc-test p1 p2 p3 t01 t12 t23 0))
    (if (and (member osc '("W1" "S1") :test #'equal)
	     (gtr (cdr (assoc 'age (cdr ith))) 1.5))
	(setq osc "NS"))

    (if (member osc '("OB" "OS") :test #'equal)
	(progn (setq oscf (osc-test p1 p2 p3 t01 t12 t23 filt))
	       (if (equal osc oscf) osc "NS"))
       osc)
    ))
       

;;;need to find out if over bought or over sold 1/2 filter size ago
(defun osc-test (p1 p2 p3 t01 t12 t23 filt)
  (block nil
    (let* ((filt2 (round (* .5 filt)))
	   (p0  (svref (get-tpv (- *end-index* filt2)) 3)))
      (if (<= t01 filt2)(return))
      (setq t01 (- t01 filt2))
      (cond ((and (> p1 p3) (> p2 p1)) ;low high low
	     (COND ((< (log p0)
		       (+ (log p1) (/ (* (- (log p1) (log p3)) t01)
				      (+ t12 t23)))) "W2")
		   ((> (log p0)
		       (+ (log p2) (/ (* (- (log p1) (log p3))(+ t01 t12))
					 (+ t12 t23)))) "OB")
		   (t "NS")))
	    
	    ((and (>= p1 p3) (<= p2 p1)) ;high low high
	     (cond ((< (log p0)
		       (+ (log p2) (/ (* (- (log p1) (log p3))
					 (+ t12 t01))
				      (+ t12 t23)))) "W1")
		   ((> (log p0) (+ (log p1) (/ (* (- (log p1) (log p3)) t01)
				       (+ t12 t23)))) "OB")
		   (t "NO")))
	    ((and (<= p1 p3) (>= p2 p1)) ;low high low
	     (cond ((> (log p0)
		       (+ (log p2) (/ (* (- (log p1) (log p3))
					 (+ t12 t01))
				      (+ t12 t23)))) "S1")
		   ((< (log p0)
		       (+ (log p1) (/ (* (- (log p1) (log p3)) t01)
				      (+ t12 t23)))) "OS")
		   (t "NO")))
	    ((and (<= p1 p3) (<= p2 p1)) ;high low high
	     (cond ((> (log p0)
		       (+ (log p1) (/ (* (- (log p1) (log p3)) t01)
				      (+ t12 t23)))) "S2")
		   ((< (log p0)
		       (+ (log p2) (/ (* (- (log p1) (log p3)) (+ t12 t01))
				       (+ t12 t23)))) "OS")
		   (t "NS")))))))



(defun rcode (ith hls)
 (let ((tim-list (cdr (assoc 'index-list (cdr ith))))
       (price-list (cdr (assoc 'hl-list (cdr ith))))
       (offset -1) (hl (cdr (assoc 'UP-DN (cdr ith))))
       (fibo-value 1.618) (fibo-sqrt-value 1.28) r-list)
#|
;;;adds the Williams 1.618 projection
   (if (or  (eql hl 'LOW))
   (unless (member (list (car tim-list)(third tim-list)) HLS :test #'equal)
     (setq hls (nconc  HLS (list (list (car tim-list)(third tim-list)))))
     (push
       (- (+ (car tim-list) offset
	     (round (* fibo-value (- (car tim-list) (third tim-list) offset))))
	  *end-index*) r-list)))

	  ; (format T "~%~A~%" hl)
;;;adds another Williams 1.618 projection
   (if (or  (eql hl 'HIGH))
   (unless (member (list (cadr tim-list)(nth 3 tim-list)) HLS :test #'equal)
     (nconc  HLS (list (list (cadr tim-list)(nth 3 tim-list))))
     (push 
       (- (+ (cadr tim-list) offset
	     (round (* fibo-value (- (cadr tim-list) (nth 3 tim-list) offset))))
	    *end-index*) r-list)))

   ;;adds a third Williams 1.618 projection
   (if (eql hl 'low)
   (unless (member (list (third tim-list)(nth 4 tim-list)) HLS :test #'equal)
     (nconc HLS (list (list (third tim-list)(nth 4 tim-list))))
     (push
       (- (+ (third tim-list) offset;;
	       (round (* fibo-value (- (third tim-list) (nth 4 tim-list) offset))))
	    *end-index*) r-list)))

;;;adds Williams 1.28 projection
   (if (or (eql hl 'LOW)(eql hl 'HIGH))
   (unless (member (list (car tim-list)(cadr tim-list)(nth 2 tim-list))
		   HLS :test #'equal)
     (nconc HLS (list (list (car tim-list)(cadr tim-list)(nth 2 tim-list))))
     (push
       (- (+ (cadr tim-list) offset
	     (round (* fibo-sqrt-value (- (car tim-list) (third tim-list) offset))))
   *end-index*) r-list)))
  |# 
;;;adds the Andrews projection
     (push
       (- (+ (second tim-list) (- (first tim-list)(third tim-list)))
        *end-index*) r-list)	  
#|
;;;;adds the pinpoint projection	  
     (if (or (and (> (car price-list)(cadr price-list))
		  (< (car price-list)(caddr price-list)))
	     (and (< (car price-list)(cadr price-list))
		  (> (car price-list)(caddr price-list))))
	  
     (push
       (- (+ (car tim-list)
	     (round (* (/ (/ (second price-list) (car price-list))
			  (/ (car price-list) (third price-list)))
		       (- (car tim-list)(third tim-list)))))
	  *end-index*) r-list))
	  
|#	  
	  
;;;;adds another Williams 1.28 projection
  (if (or (eql hl 'HIGH)(eql hl 'LOW))
   (unless (member (list (second tim-list)(third tim-list)(nth 3 tim-list))
		   HLS :test #'equal)
     (nconc HLS (list (list (second tim-list)(third tim-list)
			    (nth 3 tim-list))))
     (push
       (- (+ (third tim-list) offset
	     (round (* fibo-sqrt-value (- (second tim-list)
					  (nth 3 tim-list) offset))))
	  *end-index*) r-list)))
;;;adds another Andrews projection
     (push 
        (- (+ (third tim-list) (- (second tim-list) (nth 3 tim-list))) 
         *end-index*) r-list)	 

     
     
;;;adds another pinpoint projection	
     (if (or (and (> (cadr price-list)(caddr price-list))
		  (< (cadr price-list)(nth 3 price-list)))
	     (and (< (cadr price-list)(caddr price-list))
		  (> (cadr price-list)(nth 3 price-list))))
		  
     (push
	   (- (+ (cadr tim-list)
		 (round (* (/  (/ (caddr price-list) (nth 1 price-list))
	                       (/ (cadr price-list) (nth 3 price-list)))
		    (- (cadr tim-list)(nth 3 tim-list)))))
	      *end-index*) r-list))

        	      
   (unless (member (list (third tim-list)(nth 3 tim-list)(nth 4 tim-list))
		   HLS :test #'equal)
     (nconc HLS (list (list (third tim-list)(nth 3 tim-list)(nth 4 tim-list))))

;;;adds another Williams 1.28 projection     
    (push
       (- (+ (nth 3 tim-list) offset;
	     (round (* fibo-sqrt-value (- (third tim-list) (nth 4 tim-list) offset))))
	  *end-index*) r-list))
;;;adds another Andrews projection
     (push 
       (- (+ (nth 3 tim-list) (- (third tim-list)(nth 4 tim-list)))
       *end-index*) r-list) 
	  
;;;adds another pinpoint projection	  
     (if (or (and (> (caddr price-list)(nth 3 price-list))
		  (< (caddr price-list)(nth 4 price-list)))
	     (and (< (caddr price-list)(nth 3 price-list))
		  (> (caddr price-list)(nth 4 price-list))))
     (push
       (- (+ (caddr tim-list)
	     (round (* (/  (/ (nth 3 price-list) (nth 2 price-list))
			   (/ (caddr price-list) (nth 4 price-list)))
		       (- (caddr tim-list)(nth 4 tim-list)))))
	  *end-index*) r-list))
	  
	  

	     
     (setq r-list (sort r-list #'<))
     (values (remove-if #'(lambda (s)
			    (or (< s (- (/ (cdr (assoc '*n-filt* (cdr ith))) 4)))
				(> s (* 3 (cdr (assoc '*n-filt* (cdr ith)))))))
			r-list)
	     hls)))
	     
;;;;this calculates the time ratios of swings high to low or low to high
(defun gcode (lth hls)
  (let ((tim-list (cdr (assoc 'index-list (cdr lth)))) (offset -1)
	(hl-list (cdr (assoc 'hl-list (cdr lth)))) g-list dmy)
    (dotimes (ith (1- (length tim-list)))
      (dotimes (jth (- (length tim-list) 1 ith))
	(setq dmy (subseq hl-list ith (+ ith jth 2)))
	
      (unless (or (oddp jth)
		  (member (list (nth ith tim-list)
			    (nth (+ ith jth 1) tim-list)) HLS :test #'equal)
		  (not (or (and (eql (car dmy) (max* dmy))
				(eql (car (last dmy)) (min* dmy)))
			   (and (eql (car (last dmy)) (max* dmy))
				(eql (car dmy) (min* dmy))))))

	(setq hls (nconc  HLS (list (list (nth ith tim-list)
					  (nth (+ ith jth 1) tim-list)))))

;;;'(.618 1.618 2.618)			  
	(dolist (fibo-value '(1.618 2.618))
	  (push
	    (- (+ (nth ith tim-list) offset
		  (round (* fibo-value (- (nth ith tim-list)
					  (nth (+ ith jth 1) tim-list) offset))))
	       *end-index*) g-list)))))
    (setq g-list (sort g-list #'<))
    (values
      (remove-if #'(lambda (s)
      		    (or (< s (- (/ (cdr (assoc '*n-filt* (cdr lth))) 5)))
      			(> s (* 3 (cdr (assoc '*n-filt* (cdr lth)))))))
      		g-list)
      hls)))
    
(defun high-or-low-p (ldg tr filt)
  (let* ((jth (assoc ldg *trends*))
	 (index (cadr (assoc 'index-list jth)))
	   dmy hprice lprice nprice)
    (unless (or (> tr (/ filt 5)) 
		(> ldg (length *trends*)))
      (if (<= (- (+ *end-index* tr) index) (round (/ filt 5)))
	  (cdr (assoc 'up-dn jth))
	(progn
	  (setq hprice (get-tpv index)
		lprice hprice)
	  (dotimes (kth (- (1+ *end-index*) index) dmy)
	     (cond ((>  (svref (get-tpv (+ kth index)) 3)
		        (svref hprice 3))
		    (setq hprice (get-tpv (+ kth index))))
		   ((<  (svref (get-tpv (+ kth index)) 3)
		        (svref lprice 3))
		    (setq lprice (get-tpv (+ kth index))))))
	  (setq nprice
		(if (> (svref hprice 0)  (svref lprice 0)) hprice lprice))
	  (if (<= (- (+ *end-index* tr) (svref nprice 0)) (round (/ filt 5)))
	      (if (equal nprice hprice) 'HIGH 'LOW)

	    ))))
    ))

(defun fibo-preds ()
  (let (aa bb cc dd ee HL fibo-CO fibo-NO fibo-EO)
  (dolist (ith *trends*)
    (setq aa (nth 3 (assoc 'HL-LIST ith)))
    (setq bb (nth 2 (assoc 'HL-LIST ith)))
    (setq cc (nth 1 (assoc 'HL-LIST ith)))
    (setq dd (nth 4 (assoc 'HL-list ith)))
    (setq ee (nth 5 (assoc 'HL-list ith)))
    (setq HL (cdr (assoc 'UP-DN ith)))
    (cond ((or (and (eql HL  'LOW) (>= cc aa))
	       (and (eql HL 'HIGH) (<= cc aa)))
	   (setq fibo-CO (add CC (/ (* .618 (sub bb aa) cc) aa))
		 fibo-NO (add CC (/ (* 1.000 (sub bb aa) cc) aa))
		 fibo-EO (add CC (/ (* 1.618 (sub bb aa) cc) aa)))
	       (nconc ith (list (cons 'C1G-fibo-CO  fibo-CO)))
	       (nconc ith (list (cons 'C1G-fibo-NO  fibo-NO)))
	       (nconc ith (list (cons 'C1G-fibo-EO  fibo-EO))))
	  (t (setq fibo-CO (add cc (* .382 (sub bb cc)))
		   fibo-NO (add cc (* .500 (sub bb cc)))
		   fibo-EO (add cc (* .618 (sub bb cc))))
	         (nconc ith (list (cons 'C1R-fibo-CO  fibo-CO)))
		 (nconc ith (list (cons 'C1R-fibo-NO  fibo-NO)))
		 (nconc ith (list (cons 'C1R-fibo-EO  fibo-EO)))))
   (cond ((or (and (eql HL  'LOW) (>= cc aa)(>= bb dd)(>= aa ee))
	       (and (eql HL 'HIGH) (<= cc aa)(<= bb ee)(<= aa ee)))
	   (setq fibo-CO (add CC (/ (* .382 (sub bb ee) cc) ee))
		 fibo-NO (add CC (/ (* .500 (sub bb ee) cc) ee))
		 fibo-EO (add CC (/ (* .618 (sub bb ee) cc) ee)))
	       (nconc ith (list (cons 'C2G-fibo-CO  fibo-CO)))
	       (nconc ith (list (cons 'C2G-fibo-NO  fibo-NO)))
	       (nconc ith (list (cons 'C2G-fibo-EO  fibo-EO))))
	  ((or (and (eql HL  'LOW) (<= cc aa)(<= bb dd))
	       (and (eql HL 'HIGH) (>= cc aa)(>= bb dd)))
	   (setq fibo-CO (add cc (* .382 (sub dd cc)))
		 fibo-NO (add cc (* .500 (sub dd cc)))
		 fibo-EO (add cc (* .618 (sub dd cc))))
	   (nconc ith (list (cons 'C2R-fibo-CO  fibo-CO)))
	   (nconc ith (list (cons 'C2R-fibo-NO  fibo-NO)))
	   (nconc ith (list (cons 'C2R-fibo-EO  fibo-EO)))))

   );closes the dolist
 ))

(defun age-score (deg)
  (let ((score 0)(ndeg 0))
    (dotimes (kth 5)
      (setq ndeg (+ deg kth))
      (setq score
	    (+ score
	       (do* ((lith (cdr (assoc 'index-list (assoc ndeg *trends*)))
			   (cdr lith))
		     (ith (car lith) (car lith))
		     (ith+1 (cadr lith) (cadr lith))
		     (ctr 0 (1+ ctr))
		     (filt (cdr (assoc '*n-filt* (assoc ndeg *trends*))))
		     (dirp (eql 'low
				(cdr (assoc 'up-dn (assoc ndeg *trends*))))))
		    ((not ith+1) 0)
		 (when (and (>= (/ (- ith ith+1) filt) 4.5)
			    (<= (/ (- *end-index* ith) filt) 10))
		   (cond ((and dirp (evenp ctr))(return 1.0))
			 ((and dirp (oddp ctr)) (return -1.0))
			 ((and (not dirp) (evenp ctr)) (return -1.0))
			 ((and (not dirp) (oddp ctr)) (return 1.0))))))))
    score))

   
(defun count-elmo-turns (tdate days &optional (range 252))
 (let ((date tdate)(counter 0))
  (dotimes (ith days)
    (if (get-elmo date range)(incf counter))
    (setq date (getd date 'ydate)))
 (values (* 100 (float (/ counter days))) counter)
  ))

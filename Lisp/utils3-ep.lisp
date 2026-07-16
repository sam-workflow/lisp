;;; -*- Mode: LISP; Package: user; Base: 10. -*-
;;;
;;;
#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)


;;;;;;;;;;;;;;;;;;;;;FOR FINDING OUT WHAT BOTTOMS and TOPS look like.
(defvar lextremes nil)
(defvar hextremes nil)
(defvar nextremes nil)
(defvar price-vectors nil)

(defun populate-swing-vectors (tdate &optional (markets (append *forex-warehouse-list* *day-list*)))
 (let (lextreme hextreme)
    (maind-x)(set-cat-list)
   (setq lextremes nil hextremes nil)
   (dolist (ith markets)
       (set-market ith)
       (multiple-value-setq (lextreme hextreme)(populate-vectors tdate (available-days ith tdate 550)))
       (setq lextremes (append lextreme lextremes) hextremes (append hextreme hextremes))
    )
    
))
(defun populate-all-vectors (tdate &optional (markets (append *forex-warehouse-list* *day-list*)))
  (let (date  (period 5) num (path1 "~/exitpoints/all-vectors.lisp"))
  (setq price-vectors nil)
  (dolist (ith markets)
   
    (setq num (available-days ith tdate 550) date (add-mkt-days tdate (- num))) 
   (dotimes (jth num)
     (push (create-record-vector date period 'A) price-vectors)
     (setq date (getd date 'ndate))
    ))
  (with-open-file (str path1 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (dolist (ith price-vectors)
     (format str "~A~%" ith)))
))

;;;feature is an index into the vector
(defun feature-stats (feature)
  (let ( (path1 "~/exitpoints/all-vectors.lisp") lfeature results)
  (with-open-file (str path1 :direction :input)
         (do ((record (read str nil 'eof) (read str nil 'eof)))
             ((eql record 'eof))
             (push record price-vectors)
          ))

  (setq lfeature (1- (length (car price-vectors))))

   (dolist (kth price-vectors)
     (if (assoc (svref kth feature) results)
         (progn
             (setf (second (assoc (svref kth feature) results))
                   (round (+ (svref kth lfeature)(second (assoc (svref kth feature) results)))))
             (setf (third (assoc (svref kth feature) results))
                   (1+ (third (assoc (svref kth feature) results))))
             (setf (fourth (assoc (svref kth feature) results))
                   (round (/ (second (assoc (svref kth feature) results))
                             (third (assoc (svref kth feature) results))))))
         (setq results (cons (list (svref kth feature) (svref kth lfeature) 1 
                             (svref kth lfeature)) results))))

 (vsort results #'> 'fourth)

))  
         



(defun populate-vectors (tdate num)
   (let ((startdate (add-mkt-days tdate (- num)))(period 5) prices turn-dates low-dates high-dates
        lextremes hextremes extremes (filt *n-filt*))
;;;these are the dates of the extremes
  (setq *n-filt* (* 3 period)); *time-interval* 1440)
  (multiple-value-setq (prices turn-dates) (find-all-primitives (format nil "~A" startdate)
                                                                (format nil "~A" tdate)))
  (setq prices (butlast prices 3) turn-dates (butlast turn-dates 3) *n-filt* filt)
   ;     *time-interval* 'daily-high-low)
;  (format T "NUM Prices= ~A  NUM turn-dates= ~A~%" (length prices)(length turn-dates))
  (setq turn-dates (mapcar #'(lambda(s) (getnumdate s)) turn-dates))
  
    (do* ((prs prices (cdr prs))
         (inds turn-dates (cdr inds))
         (kth (first prices) (first prs))
         (kth+1 (second prices) (second prs))
         (ith (first turn-dates) (first inds)))

        ((null kth+1))
        (if (< kth kth+1)(push ith low-dates)(push ith high-dates))
      )
  ; (format t "NUM low-dates= ~A NUM high-dates= ~A~%" (length low-dates)(length high-dates))
   (setq low-dates (reverse low-dates) high-dates (reverse high-dates))
 
;;;;builds vectors of days
   (dolist (jth low-dates)
     (push (create-record-vector jth period 'L)  extremes)
    )
  (dolist (jth high-dates)
     (push (create-record-vector jth period 'H)  extremes)
    )
  (setq lextremes (remove-if #'(lambda(s) (eql (svref s 2) 'H)) extremes))
  (setq hextremes (remove-if #'(lambda(s) (eql (svref s 2) 'L)) extremes))
  (setq Nextremes (remove-if #'(lambda(s) (or (eql (svref s 2) 'H)
                                              (eql (svref s 2) 'L))) extremes))

;  (format t "NUM Lextremes= ~A  NUM Hextremes= ~A NUM Nextremes= ~A~%" (length lextremes)(length hextremes)(length nextremes))
(values lextremes hextremes) 
))
;;;;feature is an index into the record vector
;;;lst is either lextremes or hextremes
(defun create-bins (lst feature)
  (let (results (ilst (mapcar #'(lambda(s)(svref s feature)) lst))) 
;  (format t "ilst= ~A" ilst)
  (dolist (ith ilst)
    (ifn (assoc ith results)
         (setq results (acons ith 1 results))
         (setf (cdr (assoc ith results))(1+ (cdr (assoc ith results))))))
        ; (setf (cdr (assoc ith results))(+ (cdr (assoc ith results))))))
 (vsort results #'> 'cdr)
))

(defun display-indicators (lst)
  (dotimes (ith (-  (length (car lst)) 3))
      (format T "Feature= ~A ~% ~A~%~%" (1+ ith) (create-bins lst (+ 3 ith))))
)

;;;direction is either low or high   
 (defun create-record-vector (date period typ)
   (let (  (date-1 (getd date 'ydate)) 
       
             )
       ; (setq date-2 (getd date-1 'ydate))
      
       (vector *data-name* date typ
           
             (wpp date );;;feature 3
             (candle1 date);;feature 4
             (reversal-dayp date );;feature 5    
           
             (volatility-ratio-index date 4 28 1);;feature 6            
             (rsi-ave-diff-index date 14 2);feature 7            
             (lproj-index date period);feature 8

             (lprojdelta date period);;feature 9
             (ldev-index date period);;feature 10           
             (momentum-divergence2 date period (* 3 period));;;feature 11
                       
             (channel-direction date 5) ;feature 12
             (channel-direction date 7);feature 13
             (channel-direction date 9);;;feature 14
         
             (pivot-turn date 'month);;;feature 15
             (pivot-turn date 'week);;; feature 16                                
             (reflect3 date period);feature 17

             (ww-swing-index date);;feature 18
             (ww-asi-trend date 3);;feature 19
             (cci-direction date 21 2);;;feature 20
             (roc-rel-index date 10 90);;feature 21
             (obv date 21) ;;;feature 22   

             (volume-index date 4 63) ;;feature 23
                                      
             (ep-roc-change-index date 3 10);;;feature 27

             (asi-direction date);;;feature 28
             (candle-composite date 3)  ;;;feature 29
             (obv date 7);;;feature30
             (daily-change$ (getd date 'ndate))
             (body-range$ (getd date 'ndate)); feature 22;;must be last feature
                         );;;closes the vector

))



;;;************************************************************************
;;;	  GENERAL UTILTIES FOR LUICID VERSION FROM PC MENU SYSTEM
;;;************************************************************************

;;;
;;;THIS IS THE STUFF FOR THE CROSS RATES
(defvar *tem-dn*)
(defvar *pdata-vct* nil)
(defvar *pdata-vct1* nil)
(defvar *pdata-vct0* nil)
(defparameter *pps1* (make-array 1))
;;;FUNCTION TO SAVE PLOT PARAMETERS IN A GLOBAL FOR ALL PLOTS AND SUBPLOTS
(defun save-plot-params1 (splot &rest plist)
  (let ((ppsi (if (listp (aref *pps1* splot)) (aref *pps1* splot) '()))
	temp)
    (do ((el1 (car plist) (caddr plist))
	 (el2 (cadr plist) (cadddr plist))
	 (plist plist (cddr plist)))
	((null plist))
	(setq temp (assoc el1 ppsi))
	(if temp (setq ppsi (remove temp ppsi :test 'eql)))
      (push (cons el1 el2) ppsi))
    (setf (aref *pps1* splot) ppsi)))
;;;MACRO TO EXCTRACT PLOT PARAMETERS VALUES SAVED BY ABOVE FUNCTION
(defmacro getpp1 (param splot)
  `(cdr (assoc ,param (aref *pps1* ,splot))))



;;;MACROS FOR MODIFYING LIST VALUES USED FOR *infile* in men1?
(defmacro get-val (key lst)
  `(cdr (assoc ,key (cdr ,lst))))
(defmacro repl-val (key lst val)
  `(rplacd (assoc ,key  (cdr ,lst)) ,val))
(defmacro put-nval (key lst val)
  `(nconc ,lst (list (cons ,key ,val))))
;(defmacro put-val (key lst val)
;  `(let ((kkk ,key) (lll ,lst))
;     (if (assoc kkk (cdr lll))
;         (repl-val kkk lll ,val)
;       (put-nval kkk lll ,val))))
;(defun put-vals (key-list lst val-list)
;  (let ((nlst (pairlis key-list val-list)))
;    (dolist (pr nlst)
;      (put-val (car pr) lst (cdr pr)))))

(defmacro repl-vals (key-list lst val-list)
  `(mapcar #'rplacd
          (mapcar #'(lambda (s) (assoc s  (cdr ,lst))) ,key-list) ,val-list))
(defmacro del-val (key lst)
  `(let ((lll ,lst))
     (delete (assoc ,key  (cdr lll)) lll :test 'equal)))




(defun index-sname (&optional (dname *data-name*)(indxn *indx-cfg*))
  (car (assoc dname indxn)))
(defun scalar-data (&optional (dname *data-name*) (indxn *indx-cfg*))
  (cdr (assoc 'sdata (cdr (assoc dname indxn)))))
(defun index-lname (&optional (dname *data-name*) (indxn *indx-cfg*))
  (cdr (assoc 'ttl (cdr (assoc dname indxn)))))




(defun index-derivedp (&optional (dname *data-name*) (indxn *indx-cfg*))
  (cdr (assoc 'derived-p (cdr (assoc dname indxn)))))
(defun index-commodityp (&optional (dname *data-name*) (indxn *indx-cfg*))
  (cdr (assoc 'commodity-p (cdr (assoc dname indxn)))))
(defun index-futurep (&optional (dname *data-name*) (indxn *indx-cfg*))
  (cdr (assoc 'future-p (cdr (assoc dname indxn)))))



(defun index-digits (&optional (name *data-name*) (markets *indx-cfg*))
  (cdr (assoc 'digits (cdr (assoc name markets)))))
(defun index-tick-size (&optional (name *data-name*) (markets *indx-cfg*))
  (cdr (assoc 'tick-size (cdr (assoc name markets)))))
(defun index-point-value (&optional (name *data-name*) (markets *indx-cfg*))
 (* *qty* (cdr (assoc 'point-value (cdr (assoc name markets)))))
  )
(defun index-limit (&optional (name *data-name*) (markets *indx-cfg*))
  (cdr (assoc 'limit (cdr (assoc name markets)))))

(defun index-reel (&optional (name *data-name*)(markets *indx-cfg*))
  (cdr (assoc 'reel (cdr (assoc name markets)))))

(defun index-timeint (&optional (dname *data-name*)(indxn *indx-cfg*))
  (cdr (assoc 'ddelt (cdr (assoc dname indxn)))))
(defun index-32sp (&optional (dname *data-name*) (indxn *indx-cfg*))
    (if (eql (cdr (assoc '32s-p (cdr (assoc dname indxn)))) t)
	t nil))
(defun index-8sp (&optional (dname *data-name*) (indxn *indx-cfg*))
    (if (eql (cdr (assoc '32s-p (cdr (assoc dname indxn)))) 8)
	t nil))
(defmacro mrk-indx-name (i)
  `(car (nth ,i *indx-cfg*)))
(defmacro index-position ()
  `(position (assoc *data-name* *indx-cfg*) *indx-cfg*))

(defun checkoutdate (endt)
  (if (datep endt)
      (let ((dat (data-access endt)))
        (cond ((numberp dat) endt)
              (t (or (dotimes (i 3)
                       (setq dat (data-access
                                   (add-days-to-date2 endt (+ 1 i))))
                       (if (numberp dat) (return
                                           (add-days-to-date2 endt (+ 1 i)))))
                     (dotimes (i 3)
                       (setq dat (data-access
                                   (add-days-to-date2 endt (- -1 i))))
                       (if (numberp dat) (return
                                           (add-days-to-date2 endt (- -1 i))
                                           )))))))))
(defun get-nearest-date (endt)
  (when (datep endt)
    (let ((ifwd nil) (ibak nil) (dat (data-access endt)) dat1 dat2)
      (cond ((numberp dat) endt)
	    (t (dotimes (i 3)
		 (setq dat1 (data-access (add-days-to-date2 endt (1+ i))))
		 (when (numberp dat1) (setq ifwd (1+ i)) (return)))
	       (dotimes (i 3)
		 (setq dat2 (data-access (add-days-to-date2 endt (- -1 i))))
		 (when (numberp dat2) (setq ibak (1+ i)) (return)))
	       (cond ((and (numberp ifwd) (numberp ibak) (>= ifwd ibak)) dat2)
		     ((and (numberp ifwd) (numberp ibak) (< ifwd ibak)) dat1)
		     ((and (numberp ifwd) (not ibak)) dat1)
		     ((and (numberp ibak) (not ifwd)) dat2)
		     (t nil)))))))

;(defparameter *intervals* '(monthly-high-low 1440 720 daily-high-low 60 30))
;(defparameter *PERIODS*
;  '("M HIGH LOW" "DAILY" "TWICE DAILY" "D HIGH LOW"
;                 "HOURLY" "HALF-HOURLY" "OTHER"))
(defparameter *int-periods*
  '((monthly-high-low . "M HIGH LOW")
    (1440 . "DAILY")
    (720 . "TWICE DAILY")
    (daily-high-low . "D HIGH LOW")
    (60 . "HOURLY")
    (30 . "HALF-HOURLY")
    (15 . "Q-HOURLY")))

(defun get-lex-interval (mins)
  (let ((indx (car (assoc mins *int-periods*))))
    (if indx
        (cdr (assoc mins *int-periods*))
	(if (numberp mins)
            (string-append (format nil "~a MIN" mins))
	    "NONE"))))

(defun get-min-interval (lex-name)
  (car (rassoc lex-name *int-periods* :test #'string=)))

(defun code-ti (mins)
  (position (assoc mins *int-periods*) *int-periods*))
(defun decode-ti (code)
  (car (nth code *int-periods*)))


;;;FOR PC COMPATIBILITY: USED IN SUBT
#+:LUCID
(defun exit () nil)



;;;****************************************************************************
;;;	     CODE FOR CHANGING THE TIME INTERVAL OF A SET OF COUNTS
;;;
;;;   key words: tim-int-chg *time-interval* change-time tim-chng chng-tim
;;;****************************************************************************
;;;
;;;NOTE: CONDITIONS FOR ACCEPTABLE TIME-INTERVAL CHANGE REQUESTS
;;;							ARE IN men0 m-tim-int
;;;
;;;CODE FOR m-counts-tim-chg1 (ACTIONS FOR MENU STARTS)
;;;
(defun find-time-to-change (chng-code)
  (block timintc
    (let ((lastw (caar (symbol-value (car total-counts))))
	  lastw-dir time-delta ignore xpri xhour xday tday thour)
      (declare (ignore ignore))

      ;;The appropriate time to end that primitive will be the max or min
      ;; of the time period covered by the previous primitive and the end of
      ;; next primitive using the old time.

      (setq lastw-dir (getv lastw DR))

      (multiple-value-setq (xday xhour xpri tday thour)
	(get-adjecent-finer-mesh-extremum lastw lastw-dir chng-code))
      (if (stringp xday) (return-from timintc (values xday)))


      ;;Comupute time-delta correction from the old time that ends at day0 to
      ;;the new time.  Cannot change it yet bec it is still in the old units.
      (cond ((etime= (conv-to-string tday thour) (conv-to-string xday xhour))
	     (setq time-delta 0.0))
	    ((f-etime>= (conv-to-string tday thour)
			(conv-to-string xday xhour))
	     (fill-qvector xday xhour tday thour)
	     (setq time-delta (- 0.0 (getv 0 TL))))
	    (t (fill-qvector tday thour xday xhour)
	       (setq time-delta  (getv 0 TL))))

      ;;However I can change the prices here for the primitive's q.
      ;;Possible problem: Is it possible that the lastw q is repeated?
      (putv lastw EP xpri)
      (case (getv lastw DR)
	(UP (putv lastw HP xpri))
	(DOWN (putv lastw LP xpri)))
      (setq *curr-et* (conv-to-string xday xhour))

      time-delta)))

;;;Debugging aid
;(defun foo  (wv ch-code)
;  (let ((lastw wv)(dir (getv wv dr))(chg-code ch-code))
;    (get-adjecent-finer-mesh-extremum lastw dir chg-code)))

(defun get-adjecent-finer-mesh-extremum (lastw dir chg-code)
  (block gafme
    (let ((timlists nil)
	  (plists nil)
	  (daylist nil)
	  num-sday num-eday (num-shour nil) num-ehour num-tday num-thour
	  stim etim ttim  xday xhour xpri  timlist plist ignore)
      (declare (ignore ignore))

      ;;Start time of last primitive (also today time) - old time interval
      (setq stim (getv lastw ST) ttim (getv lastw ET))
      ;;Find endtime of next primitive - old time interval
      (let ((*time-interval* *cnts-delt*))
	(provide-primitive (getv lastw ET))
	(cond (*primitive*  (setq etim (getv 0 ET)))
	      ((let (dt1 tm1 bbb ignore)
		 (declare (ignore ignore))
		 (setq dt1 (car (last (month-days (get-latest-index-date))))
		       tm1 (progn
			     (multiple-value-setq (ignore ignore ignore bbb)
			       (data-access dt1))
			     (car (last bbb))))
		 (setq etim (conv-to-string (getnumdate-long dt1) tm1))
		 (etime> etim ttim)))
	      (t (return-from gafme (values "Not enough fwrd data")))))

      (setq num-sday (getnumdate stim)
	    num-tday (getnumdate ttim)
	    num-eday (getnumdate etim))

      ;;Get mk data between the two times in reverse order - new time interval
      (let (tody yday kpday)
	(do ((iday num-eday yday))
	    ((< iday num-sday))
	  (multiple-value-setq (tody yday ignore timlist plist)
	    (data-access iday))
	  (cond ((stringp tody)
		 (setq num-sday kpday num-shour t)
		 (return))
		(t (setq kpday tody)
		   (push timlist timlists)
		   (push plist plists)
		   (push iday daylist)))))

      ;;Get the last hour of the last day - new time interval
      ;;Get the first hour of the first day - new time interval
      ;;Get the old end hour of the old day - new time interval
      (case chg-code
	((d-to-h d-to-hh d-to-qh)
	 (multiple-value-setq (ignore ignore ignore timlist)
	   (data-access num-eday))
	 (setq num-ehour (car (last timlist)))
	 (multiple-value-setq (ignore ignore ignore timlist)
	   (data-access num-sday))
	 (setq num-shour (car (last timlist)))
	 (multiple-value-setq (ignore ignore ignore timlist)
	   (data-access num-tday))
	 (setq num-thour (car (last timlist))))
	(t (setq num-ehour (getnumhour etim) num-thour (getnumhour ttim))
	   (cond ((not num-shour) (setq num-shour (getnumhour stim)))
		 (t (multiple-value-setq (ignore ignore ignore timlist)
		      (data-access num-sday))
		    (setq num-shour (car timlist))))))

      ;;Chop off the part of the data earlier that stim, later than etim
      (dolist (tim (copy-list (car timlists)))
	(cond ((ehour< tim num-shour)(pop (car timlists))(pop (car plists)))
	      (t (return))))
      (dolist (tim (nreverse (copy-list (car (last timlists)))))
	(cond ((ehour> tim num-ehour)
	       (rplacd (penultimo (car (last timlists))) nil)
	       (rplacd (penultimo (car (last plists))) nil))
	      (t (return))))

      ;;Find extremum price and corresponding day and time
      (let ((tlsts timlists)(plsts plists) pls pri day)
	(setq xpri (caar plists) xhour (caar tlsts) day (car daylist))
	(dolist (tls tlsts)
	  (setq pls (pop plsts))
	  (setq day (pop daylist))
	  (dolist (hour tls)
	    (setq pri (pop pls))
	    (if (eql dir 'UP)
		(if (>= pri xpri) (setq xpri pri xday day xhour hour))
	      (if (< pri xpri) (setq xpri pri xday day xhour hour))))))

      (values xday xhour xpri num-tday num-thour))))

(defun dhl-timint-choice-protect (newti)
  (let ((*time-interval* newti) tim tims ignore)
    (declare (ignore ignore))
    (multiple-value-setq (ignore ignore ignore tims)
      (data-access (getnumdate *curr-et*)))
    ;;now take a look at a few getnumhours from waves and
    ;;make sure that they belong to the list of tims
    (dolist (wvpr  (eval (car total-counts)) t)
      (setq tim (getnumhour (getv (car wvpr) ET)))
      (if tim (ifn (member tim tims) (return (values nil))))
      (setq tim (getnumhour (getv (car wvpr) ST)))
      (if tim (ifn (member tim tims) (return (values nil))))
      )))

;;;Note: this assumes that *time-interval* is set to the new value.  This
;;;function can be applied only to find the factor to multiply a longer
;;;time interval to get hourly (h) or half-hourly (hh) and not any other
;;;time interval such as twice daily or daily high low.
(defun find-fctr (curr-et-date)
  (let (dmy1 tody yest ignore timlist daylength olddaylength lasthour)
    (declare (ignore ignore))

    ;;Get the old daylength using the old timint
    (let ((*time-interval* *cnts-delt*))
      (multiple-value-setq (tody yest ignore timlist)
	(data-access curr-et-date))
      (if (stringp tody) (setq dmy1 "Old interval not supported")
	(setq olddaylength (get-consistent-daylength timlist yest tody))))

    ;;If problems getting old timint data-access use dmy1 flag to get out.
    (unless dmy1
      ;;Get the new daylength (using new timint)
      (multiple-value-setq (tody yest ignore timlist)
	(data-access curr-et-date))
      (if (stringp tody) (setq dmy1 "New interval not supported")
	(multiple-value-setq (daylength lasthour)
	  (get-consistent-daylength timlist yest tody))))
    (cond ((not dmy1)
	   ;;Note: curr-et was set already to new value in find-time-to-change
	   (values (/ daylength olddaylength) lasthour))
	  (t (values dmy1)))))

(defun get-consistent-daylength (timlist yest tody)
  (let ((tl-list '())
	(last-hour (car (last timlist))) timlist1 dyl nyest today ignore)
    (declare (ignore ignore))

    ;;Go back 12 mrkt days, take the max tl and the latest last hour
    (dotimes (i 12)
      (multiple-value-setq (today nyest ignore timlist1) (data-access yest))
      (if (stringp today) (return))
      (fill-qvector yest (car timlist1) tody (car timlist))
      (push (getv 0 TL) tl-list)
      (ifn (ehour> last-hour (car (last timlist)))
	   (setq last-hour (car (last timlist))))
      (setq tody yest yest nyest timlist timlist1))
    (setq dyl (apply #'max tl-list))
    (values dyl last-hour)))

(defun m-counts-tim-chg1 ()
  (let* ((tim1 (get-lex-interval *cnts-delt*))
	 (tim2 (get-lex-interval *time-interval*))
	 (title (format nil "CHANGING TIME INTERVAL FROM ~a TO ~a"
		       tim1 tim2)))
    (setq *pmenu* (list " CANCEL: RECOVER THE PREVIOUS TIME INTERVAL "
			" CONTINUE "
			" "
			" WARNING\: PROCEEDING WILL CHANGE THE COUNTS "
			" PERMANENTLY UNLESS SAVED PREVIOUSLY "))
    title))

;;;CONDITIONS FOR ACCEPTABLE TIME-INTERVAL CHANGE REQUESTS FROM MEN2
;;;THIS CODE CAME FROM MEN0
(defun timint-chg-constraints (new-timint)
  (let* ((lei (get-lex-interval *time-interval*))
	 (periods (mapcar #'(lambda (s) (cdr s)) *int-periods*))
	 (idx (el-index lei periods :test 'equal))
	 (ccc (el-index (format nil "~a" new-timint) periods :test 'equal))
	 (idx1 (el-index "HOURLY" periods :test 'equal))
	 (idx2 (el-index "HALF-HOURLY" periods :test 'equal))
	 (idx3 (el-index "Q HOURLY" periods :test 'equal))
	 )
    (and (not (equal lei "NONE"))
	 (> ccc idx)
	 (or (= ccc idx1)(= ccc idx2)(= ccc idx3)))))

;;;THIS COMES FROM PLT2
;;;
(defun get-timint-code (&optional (timint *time-interval*))
  ;;Note: display of q-hourly data in monthly-hilo and daily plots needs to be
  ;;looked at. It may very well be supported with the present software, however
  ;;I will not put the appropriate code changes here until verified.
  ;; (ie: qh-to d and qh-to mhl).
  (cond ((eql timint 'monthly-high-low)
	 (cond ((eql *cnts-delt* 1440) 'd-to-mhl)
	       ((eql *cnts-delt* 720) 'twd-to-mhl)
	       ((eql *cnts-delt* 'daily-high-low) 'dhl-to-mhl)
	       ((eql *cnts-delt* 60) 'h-to-mhl)
	       ((eql *cnts-delt* 30) 'hh-to-mhl)))
	((eql timint 1440)
	 (cond ((eql *cnts-delt* 720) 'twd-to-d)
	       ((eql *cnts-delt* 'daily-high-low) 'dhl-to-d)
	       ((eql *cnts-delt* 60) 'h-to-d)
	       ((eql *cnts-delt* 30) 'hh-to-d)))
	((eql timint 'daily-high-low)
	 (cond ((eql *cnts-delt* 60) 'h-to-dhl)
	       ((eql *cnts-delt* 30) 'hh-to-dhl))
	 )
	;;Note that there is no capability to display qh and hh counts in
	;;hourly plots and this is reflected by the lack of appropriate
	;;chng-code: (ie: not qh-to-hh qh-to-h or hh-to-h)
	((eql timint 60)
	 (cond ((eql *cnts-delt* 1440) 'd-to-h)
	       ((eql *cnts-delt* 1440) 'd-to-h)
	       ((eql *cnts-delt* 'daily-high-low) 'dhl-to-h)))
	((eql timint 30)
	 (cond ((eql *cnts-delt* 'daily-high-low) 'dhl-to-hh)
	       ((eql *cnts-delt* 1440) 'd-to-hh)
	       ((eql *cnts-delt* 60) 'h-to-hh)) ; Won't work for displays. Will
	 )					; put waves at non minmax's.
						; But see comment below.
	;;This is for the time-interval change from d- h- & hh-to-qh so that
	;;counts can be changed to a finer messh.  This presents a problem for
	;;displays because sftwre is not in place to display daily, daily-hilo,
	;;hourly and half-hourly in q-hourly plots.  But do we ever want to?
	;;It seems to me that if we want to it is because we already changed
	;;the counts from coarser mesh to finer mesh.  No reason to display
	;;coarser mesh in finer mesh plot.  However if the plot starts earlier
	;;then where the cut/change took place then the waves might not be
	;;plotted at the q-hourly minmax's.
	((eql timint 15)
	 (cond ((eql *cnts-delt* 'daily-high-low) 'dhl-to-qh)
	       ((eql *cnts-delt* 1440) 'd-to-qh)
	       ((eql *cnts-delt* 60) 'h-to-qh)
	       ((eql *cnts-delt* 30) 'hh-to-qh))
	 )
	(t nil)))

;;;CALL THIS FUNCTION AFTER *time-interval* WAS CHANGED TO THE NEW
;;;REQUESTED TIME INTERVAL,  BUT *counts-delt* HAS NOT.  THIS CODE
;;;WILL RETURN nil AND CHANGE *counts-delt* IF SUCCESSFUL.  IT WILL
;;;RETURN A STRING IF NOT.
;;;
(defun f-call-change-counts-time-interval ()
  (let ((dmy nil) tdel chg-code)
    (block mctc1
      (setq chg-code (get-timint-code)) ; in plt2
      (case chg-code
	((d-to-h d-to-hh d-to-qh h-to-hh h-to-qh hh-to-qh)
	 (setq tdel (find-time-to-change chg-code))
	 (when (stringp tdel) (setq dmy tdel) (return-from mctc1))
	 (setq dmy (change-counts-time-interval chg-code tdel))
	 (if (stringp dmy) (return-from mctc1)))
	((dhl-to-hh dhl-to-qh)
	 (setq dmy (change-counts-time-interval chg-code 0))
	 (if (stringp dmy) (return-from mctc1)))
	(dhl-to-h
	  ;;Sometime later provide warning that if dhl data
	  ;;contains 1/2hrs then the result would be invalid
	  ;;with unpredictable results.
	  (setq dmy (change-counts-time-interval chg-code 0))
	  (if (stringp dmy) (return-from mctc1)))
	(t (setq dmy "Change not yet supported")
	   (return-from mctc1)))
      (setq *cnts-delt* *time-interval*
	    *latest-dtime* *curr-et*))
    dmy))
;;;
;;;END OF CHANGE COUNTS TIME INTERVAL
;;;**********************************


;;; -*- Mode: LISP; Package: user; Base: 10. -*-
;;;
;;;**************************************************************************
;;;				CROSS RATES
;;;**************************************************************************
;;;
(defun remap-times-required (timl1 timl2)
  (block rte
    (ifn (numberp (car timl1)) (return-from rte nil))
    (let ((tprev 0))
      (dolist (tim timl1 nil)
	(if (> tprev tim) (return-from rte t))
	(setq tprev tim))
      (dolist (tim timl2 nil)
	(if (> tprev tim) (return-from rte t))
	(setq tprev tim)))))

(defun remap-times (timeslist 1st-hour-of-day)
  (dolist (timl timeslist)
    (remap-times1 (cdr timl) 1st-hour-of-day)))
(defun unmap-times (timeslist 1st-hour-of-day)
  (dolist (timl timeslist)
    (unmap-times1 (cdr timl) 1st-hour-of-day)))
(defun remap-times1 (timl 1st-hour-of-day)
  (let ((ptr timl))
    (dolist (tim timl)
      (if (< tim 1st-hour-of-day)
	   (rplaca ptr (+ tim 2400)))
      (setq ptr (cdr ptr)))
    timl))
(defun unmap-times1 (timl 1st-hour-of-day)
  (setq 1st-hour-of-day 1st-hour-of-day)
  (let ((ptr timl))
    (dolist (tim timl)
      (if (> tim 2400)
	   (rplaca ptr (- tim 2400)))
      (setq ptr (cdr ptr)))
    timl))

;;;RETURNS PAIRS OF YYYYMM DATES ST NUM PRICES ARE < THEN 10,000 OR SO.
(defun get-yearly-iniend-times (s1date e1date)
  (let* ((yr1 (truncate (/ s1date 100)))
	 (yrz (truncate (/ e1date 100)))
	 (yrlst (list yr1))
	 (date-pairs '()))
    (cond ((and (numberp *time-interval*) (<= *time-interval* 720))
	   (dotimes (i (- yrz yr1))
	     (push (1+ (car yrlst))  yrlst)))
	  (t (dotimes (i (truncate (/ (- yrz yr1) 10)))
	       (push (+ 10 (car yrlst)) yrlst))))
    (setq yrlst (nreverse yrlst))
    (do ((cur-yr (car yrlst) (cadr partial-yrlst))
	 (partial-yrlst yrlst (cdr partial-yrlst))
	 (cur-ini)
	 (cur-end))
	((null partial-yrlst))
      (setq cur-ini (+ (* 100 cur-yr)
		       (if (= cur-yr yr1) (mod s1date 100) 1))
	    cur-end (if (cadr partial-yrlst)
			(+ (* 100  (1- (cadr partial-yrlst))) 12)
		        (+ (* 100  yrz) (mod e1date 100))))
      (push (list cur-ini cur-end) date-pairs))


    (setq date-pairs (nreverse date-pairs))
    date-pairs))

(defun top-do-cross-index (sdate edate)
  (let ((numi *data-name*)	; numerator index for ratios
	(deni (cadr *tem-dn*))	; denominator index for ratios
	(rati (car *tem-dn*))	; ratio/product index
	(operator-key (car (last *tem-dn*)))
	(offset 0)
	(scale 1))
    (ifn operator-key (setq operator-key 3)) ; return 1st index
    (do-cross-index sdate edate operator-key numi deni rati offset scale)))

(defun do-cross-index (s1date e1date operator-key numer-index
			      denom-index ratio-index offset scale)
 ;(set-index-config)
  (block doci
    (let ((times '())
	  (times-prev 'dummy)
	  (*data-name* numer-index)
	  file-info long-delts-set operator
	  s-date e-date dates0 dates1 times0 times1 hset hset0 hset1
	  hour-index1 hour-index0 hour-counter (date 0) (dates '())
	  next-hdate1 next-hdate0 date1 date0 hptr0 hptr1
	  next-hset1 next-hset0 nsk0 nsk1 1st-hour-of-day remap-p
	  inidate enddate prev-date1 last-ndate ignore)
      (declare (ignore ignore))

      (case operator-key
	(1 (setq operator #'(lambda (s r) (/ s r))))
	(2 (setq operator #'(lambda (s r) (* s r))))
	(3 (setq operator #'(lambda (s r) s))))


      ;;Note: lifted from plt1 function arrange-for-market-data which has
      ;;other functions nor of interest here.
      (ifn (and (boundp '*pdata-vct0*) (vectorp *pdata-vct0*))
	   (setq *pdata-vct* (make-array 16000 :fill-pointer 0)
		 *pdata-vct0* (make-array 16000 :fill-pointer 0)
		 *pdata-vct1* (make-array 16000 :fill-pointer 0)
		 ))

      ;Return pairs of dates such that dim *pdata-vct* will not be exceeded
      (setq long-delts-set (get-yearly-iniend-times s1date e1date))
      (dolist (delt-pair long-delts-set)

	(setf (fill-pointer *pdata-vct*)  0
	      (fill-pointer *pdata-vct0*) 0
	      (fill-pointer *pdata-vct1*) 0
	      dates '() times '() times-prev 'dummy)

	(setq s-date (car delt-pair) e-date (cadr delt-pair))
	(multiple-value-setq (inidate enddate prev-date1 last-ndate)
	  (get-common-iniend-dates numer-index denom-index s-date e-date))

	;;Read data set 1 - denominator index
	(setq *data-name* (change-data-name  denom-index))
	(setq dates1 (read-data-files1 inidate enddate 16000))
	(ifn (stringp dates1) (setq dates1 (cdr dates1))
	  (return-from doci dates1))
	(setq times1 (getpp1 'times 0))
	(dotimes (i (length *pdata-vct*))
	  (vector-push (aref *pdata-vct* i) *pdata-vct1*))
	(setq file-info (readt2 inidate))
	(setq 1st-hour-of-day (cdr (assoc 'first-time (cdr file-info))))

	;;Read data set 0 - numerator index yen
	(setq *data-name* (change-data-name numer-index))
	(setf (fill-pointer *pdata-vct*) 0)
	(setq dates0 (read-data-files1 inidate enddate 16000))
	(ifn (stringp dates0) (setq dates0 (cdr dates0))
	  (return-from doci dates0))
	(setq times0 (getpp1 'times 0))
	(dotimes (i (length *pdata-vct*))
	  (vector-push (aref *pdata-vct* i) *pdata-vct0*))


	;;Make the hour set monotonically increasing past 2400 if necessary
	(when (remap-times-required (cdar times0) (cdar times1))
	  (remap-times times0 1st-hour-of-day)
	  (remap-times times1 1st-hour-of-day)
	  (setq remap-p t))

	(setq hour-index0 (length *pdata-vct0*)
	      hour-index1 (length *pdata-vct1*)
	      hour-counter 0)


	;;Prepare *pdata-vct* to rec new ratio index data in reverse order
	(setf (fill-pointer *pdata-vct*) 0)
	(setq next-hdate1 (caar times1) next-hdate0 (caar times0))

	;;Format of timesx ((date1 hr1 hr2 ... hrn)(date2 hr1 hr2 ...hrm) ...
	(setq hset1 (pop times1)
	      hset1 (cdr hset1)		  ; Take date out on hset1
	      next-hset1 (pop times1)
	      next-hdate1 (pop next-hset1)) ; Take date out on next-hset1
	(setq hset0 (pop times0)
	      hset0 (cdr hset0)		  ; Take date out on hset0
	      next-hset0 (pop times0)
	      next-hdate0 (pop next-hset0)) ; Take date out on next-hset0

	;;START THE LOOP OVER ALL PRICES BETW INIDATE/ENDDATE TO CALC RATIO
	;;
	(dotimes (i (max (length dates0) (length dates1)))
	  (setq date1 (pop dates1))
	  (setq date0 (pop dates0))
	  (ifn (and date1 date0) (return))

	  ;;Get the set of times for that day for each FOR USE LATER
	  ;;Note: Not needed for i=0 but needed for i>0.
	  (if (eql next-hdate1 date1)
	      (setq hset1 next-hset1
		    next-hset1 (pop times1)
		    next-hdate1 (pop next-hset1)))
	  (if (eql next-hdate0 date0)
	      (setq hset0 next-hset0
		    next-hset0 (pop times0)
		    next-hdate0 (pop next-hset0)))

	  ;;If these are non coincident dates, look for the next common date:
	  (unless (= date0 date1)
	    (multiple-value-setq (date nsk0 nsk1)
	      (nxt-common-incr-num (cons date0 dates0) (cons date1 dates1)))
	    ;;Now skip the appropriate num or prices(hour-index0 & hour-index1)
	    ;;while updating the current hour set for each index(hset0 & hset1)
	    (when (plusp nsk0)
	      (dotimes (i nsk0)
		(setq date0 (pop dates0)
		      hour-index0 (- hour-index0 (length hset0)))
		(if (eql next-hdate0 date0)
		    (setq hset0 next-hset0
			  next-hset0 (pop times0)
			  next-hdate0 (pop next-hset0)))))
	    (when (plusp nsk1)
	      (dotimes (i nsk1)
		(setq date1 (pop dates1)
		      hour-index1 (- hour-index1 (length hset1)))
		(if (eql next-hdate1 date1)
		    (setq hset1 next-hset1
			  next-hset1 (pop times1)
			  next-hdate1 (pop next-hset1))))))

	  ;;Build up the resulting dates list for the new index
	  (setq date date1)
	  (push date dates)

	  ;;NOW insure that corresponding times are the same
	  ;;
	  (if (numberp (car hset0)) ; make sure the hours are numbers (not 'C)
	      (multiple-value-setq (ignore nsk0 nsk1)
		(nxt-common-num hset0 hset1))
	    (setq nsk0 0 nsk1 0))

	  (setq hptr0 hset0 hptr1 hset1 hset '())
	  (dotimes (i nsk0) (decf hour-index0)(setq hptr0 (cdr hptr0)))
	  (dotimes (i nsk1) (decf hour-index1)(setq hptr1 (cdr hptr1)))
	  (dotimes (i  (max (length hptr0)(length hptr1)))
	    (if hptr0 (decf hour-index0))
	    (if hptr1 (decf hour-index1))
	    (when (and hptr0 hptr1)
	      (push (car hptr0) hset) ; ie: either is the same
	      (incf hour-counter)
	      (vector-push
		(+ (* scale (funcall operator
				     (aref *pdata-vct0* hour-index0)
				     (aref *pdata-vct1* hour-index1)))
		   offset)
		*pdata-vct*))
	    (setq hptr0 (cdr hptr0) hptr1 (cdr hptr1)))
	  (setq hset (nreverse hset))

	  ;;Construct the times list for when hpd changes for result index
	  (when (not (equal hset times-prev))
	    (push (cons date hset) times)) ; Note: date1 shouldbe the same
	  (setq times-prev hset)
	  )



	(setq times (nreverse times) dates (nreverse dates))
	(setq *data-name* (change-data-name ratio-index))
	(when remap-p
	  (unmap-times times 1st-hour-of-day)
	  (setq remap-p nil))

	(save-data-files times dates file-info prev-date1 last-ndate)))))

(defun get-common-iniend-dates (numer-index denom-index s-date e-date)
  (let (dayl1 dayl0 endd1 endd0 iniday endday pdayl0 pdayl1 pyd0 prev-day
	      lnd1 lnd0 lnd el edayl0 edayl1 pmd1 p-edayl1 p-edayl0)
   (labels ((numyearmo (x)(+ (* 100 (getnumyear-long x)) (getnummonth x))))
    (setq *data-name* (change-data-name denom-index))
    (setq dayl1 (month-days s-date)
	  ;;The following 4 lines get the last 2 months in reverse order
	  edayl1 (month-days e-date)
	  pmd1 (cdr (assoc 'ydate (cdr (readt2 (car edayl1)))))
	  p-edayl1 (month-days (numyearmo pmd1))
	  edayl1 (append (nreverse edayl1) (nreverse p-edayl1))
	  endd1 (car edayl1))
    (setq *data-name* (change-data-name numer-index))
    (setq dayl0 (month-days s-date)
	  ;;The following 3 lines get the last 2 months in reverse order
	  edayl0 (month-days e-date)
	  p-edayl0 (month-days (numyearmo pmd1))
	  edayl0 (append (nreverse edayl0) (nreverse p-edayl0))
	  endd0 (car edayl0))


    ;;Extract the prev date from the numer-index market
    ;;Remember this is guaranteed to be in the previous to the first month
    (setq pyd0 (cdr (assoc 'ydate (cdr (readt2 (car dayl0))))))
    ;;So get its month-list (ie the prev month's) and reverse it
    (setq pdayl0 (nreverse (month-days (numyearmo pyd0))))

    ;;Pause to get the first date in common for both indices for this month
    (do ((dy1 (car dayl1))
	 (dy0 (car dayl0)))
	((or (null dayl1)(null dayl0)) (break "suspect lack of data"))
      (when (= dy1 dy0) (setq iniday dy1) (return))
      (when (> dy1 dy0) (pop dayl0)(setq dy0 (car dayl0)))
      (when (< dy1 dy0) (pop dayl1)(setq dy1 (car dayl1))))
    (if (> endd1 endd0) (setq endday endd0 el 0)
      (setq endday endd1 el 1))

    ;;Now get the first day in common going backwards, we know pyd0 exists.
    (setq *data-name* (change-data-name denom-index))
    (setq pdayl1 (month-days (numyearmo pyd0)))
    (dolist (dayi pdayl0 (setq prev-day (add-days-to-date1 (car dayl0) -1)))
      (when (member dayi pdayl1)
	(setq prev-day dayi)
	(return)))

    ;;Now search for the last day common to both indices
    (dolist (dayi  (case el (0 edayl0)(1 edayl1)))
      (when (case el (0 (member dayi edayl1)) (1 (member dayi edayl1)))
	(setq endday dayi)
	(return)))

    ;;Now get a last next-day from the data: both indices. Select the latest.
    (setq lnd1 (cdr (assoc 'ndate (cdr (readt2 endday)))))
    (setq *data-name* (change-data-name numer-index))
    (setq lnd0 (cdr (assoc 'ndate (cdr (readt2 endday)))))
    (setq *data-name* (change-data-name denom-index))
    (setq lnd (max lnd1 lnd0))


    (values iniday endday prev-day lnd))))


;;This makes a lot of unnecessary check but should work
(defun nxt-common-num (list0 list1)
 (block ncn
  (let ((nmax (min (length list0) (length list1)))
	jj)				;      Indices to be checked
    (dotimes (k nmax)			;	0 0	0 2	0 3
      (dotimes (j (1+ k))		;	0 1	1 2	1 3
	(setq jj (- k j))		;	1 0	2 1	2 3
	(dotimes (i (1+ k))		;		2 0	3 2
					;			3 1
					;			3 0
	  (if (= (nth i list1) (nth jj list0))
	      (return-from ncn (values (nth i list1) jj i)))))))))

;;;This works only if list0 and list1 are monotonically increasing
(defun nxt-common-incr-num (list0 list1)
  (let (common-num (i 0)(j 0))
    (do ((dy0 (car list0))
	 ;(dy0p nil dy0)
	 (dy1 (car list1))
	 ;(dy1p nil dy1)
	 (dayl0 list0)
	 (dayl1 list1))
	((or (null dayl0)(null dayl1))
	 (break "suspect lack of data in nxt-com"))
      (when (= dy0 dy1)(setq common-num dy0)(return (values common-num i j)))
      (when (> dy0 dy1)(pop dayl1)(setq dy1 (car dayl1))(incf j))
      (when (< dy0 dy1)(pop dayl0)(setq dy0 (car dayl0))(incf i)))))

#+(or :LUCID :ALLEGRO)
(defun change-data-name (new-index)
  (setq *data-name* new-index
    *data-read-dir* (string-append *database-upper-dir*
				   (string-downcase (format nil "~a" new-index))
				   "/")
   ;*counts-save-dir* (string-append *counts-upper-dir* new-index "/counts/")
    *time-interval* (or *time-interval* (index-timeint)))
  new-index)

#+:SUN-DATABASE
(defun change-data-name (new-index)
  (setq *data-name* new-index
    *DATA-READ-DIR* (string-append *database-upper-dir* new-index "\\")
    *counts-save-dir* (string-append *counts-upper-dir* new-index "\\counts\\")
    *time-interval* (or *time-interval* (index-timeint)))
  new-index)

;#-:SUN-DATABASE
;(defun change-data-name (new-index)
;  (setq *data-name* new-index
;  *DATA-READ-DIR* (string-append *database-upper-dir* *data-name* "\\data\\")
;  *counts-save-dir* (string-append *counts-upper-dir* *data-name* "\\counts\\")
;  *time-interval* (or *time-interval* (index-timeint)))
;  new-index)


(defun save-data-files (times dates file-info prev-date1 last-next-date1)
  ;; note times is a list of sublists where ea sublist is a list of
  ;; a numdate and a list of good good starting at that numdate until the next
  ;; sublist with a new numdate
  (let ((data-rd-dir *data-read-dir*)
	(hour-counter -1)
	(ftl (cdr (assoc 'first-time (cdr file-info))))
	(ctl (cdr (assoc 'close-time (cdr file-info))))
	(otl (cdr (assoc 'open-time (cdr file-info))))
	(isl *time-interval*) ; (cons 'index-sample-period *time-interval*))
	(nsamp (if (eql *time-interval* 1440) 1 nil))
	(currd (directory-namestring (chdir)))
	(mdates '()) (last-datep nil) yymm file-name day-data
	1dog  (dog-list '())
	curr-hset-date hset next-hset next-hdate curr-mo)

    (setq hset (pop times)
	  curr-hset-date (pop hset) ;     Take the date part out of the hset
	  next-hset (pop times)
	  next-hdate (pop next-hset)
	  curr-hset-date curr-hset-date)

    (setq curr-mo (getnummonth (car dates)))
    (do ((datelist dates (cdr datelist))
	 (prev-date prev-date1 (car datelist))
;	 (prev-date (add-days-to-date1 (car dates) -1) (car datelist))
;	 (prev-date (add-mkt-days (car dates) -1) (car datelist))
	 (date (car dates) (cadr datelist))
	 (next-date (cadr dates) (caddr datelist)))
	((null datelist))

      (if (eql next-hdate date)
	  (setq hset next-hset
		next-hset (pop times)
		next-hdate (pop next-hset)))

      (ifn nsamp
	   (setq ftl (car hset) ; (cons 'first-time (car hset))
		 ctl (car (last hset)) ; (cons 'close-time (car (last hset)))
		 otl (add-to-time (car hset) (- *time-interval*))))
			      ; (cons 'open-time
			      ;(add-to-time (car hset) (- *time-interval*)))))
;      (if (not nsamp)
;	  (ifn (and (cdr ftl) (cdr ctl) (cdr isl))
;	       (break "LISP is SCREWING UP AGAIN !!!")))

      (setq day-data '())
      (dolist (timei hset)
	(setq timei timei)
	(incf hour-counter)
	(push (aref *pdata-vct* hour-counter) day-data))
      (setq day-data (nreverse day-data))

      (unless next-date
	(setq last-datep t)
	(if (datep last-next-date1) (setq next-date last-next-date1)
	  (setq next-date (add-days-to-date1 date 1))))


      (if nsamp
	  (setq 1dog (list (cons 'ndate next-date) (cons 'ydate prev-date)
			   (cons 'pdata day-data) (cons 'index *data-name*)
			   (cons 'nsamp nsamp)))
	(setq 1dog (list (cons 'ndate next-date)
			 (cons 'ydate prev-date)
			 (cons 'pdata day-data) (cons 'ptime hset)
			 (cons 'first-time ftl)
			 (cons 'close-time ctl)
			 (cons 'open-time otl)
			 (cons 'index-sample-period isl))))
      (push date mdates)
      (push (append (list date) (copy-alist 1dog)) dog-list)

      ;;Pause at the end of every month to write that dog-list to disk
      ;;Also pause at the end of the data (ie: last-datep->t)
      (when (or (neql curr-mo (getnummonth next-date)) last-datep)

	(setq curr-mo (getnummonth next-date))
	(setq mdates (reverse mdates))
	(setq dog-list (reverse dog-list))

	;;Write data to disk
	(cd data-rd-dir)
	(setq yymm (truncate date 100)) ;;; 19xxxxxx format
	(setq file-name (string-append "d" (format nil "~A" yymm)))
	(write-file dog-list (string-append file-name ".dat") nil)
	(dolist (idate mdates)
	  (update-cat-index idate 'foo nil))
	(dolist (td (cdr dog-list))
	  (update-cat-index (car td) 'add nil))
	(update-cat-index (car (first dog-list)) 'add t)
	(cd currd)

	(setq dog-list '() mdates '()))
      )
  ))


(defun my-copy-file (path1 path2)
  (let (records)
   (with-open-file (str path1 :direction :input)
      (do ((record (read-line str nil 'eof) (read-line str nil 'eof)))
          ((eql record 'eof))
         (push record records))
)
    (setq records (reverse records))
 (with-open-file (str path2 :direction :output :if-exists :supersede :if-does-not-exist :create)
   (dolist (ith records)
     (format str "~A~%" ith)))
))
    

;;;
;;;*******************END OF CROSS RATES*********************
;;;
;;;
(defun read-data-files1 (start end no-days)
  (let ((err-flg nil) (flag nil)
	(dl '()) ; -->  dl = dates list as integers
	fxl
	(pdata-vct *pdata-vct*)
	(pd-cntr -1) (pd-cntr1 -1)
	(pd-dim '())
	tdate ydate ndate
	pdata ignore times tims (tims0 'dummy)
	(dailyp (if (eql *time-interval* 1440) '(C)))
	(p-mx -1.0F+18)(p-mn 1.0F+18) p-tem) ;(v-mx -1.0F18)(v-mn 1.0F18)v-tem)
    (declare (ignore ignore))
    (do* ((date end ydate)
	  (dayc 1 (1+ dayc))
	  )
	 ((or err-flg flag)	     ;note: if non-nil err-flg must be a string
	  (cond (err-flg (goto-err err-flg tdate dayc))
		(t (setq fxl (cons ydate dl)) ; *fxl* fxl)
		   (values fxl))))

      ;;READ FILES
      (multiple-value-setq
	  (tdate ydate ndate tims pdata ignore ignore ignore)
	(data-access date))
      (unless pdata (setq err-flg tdate tdate date))
      (if dailyp (setq tims dailyp))	     ; tims->'(C) for daily data always
      (unless err-flg

	(push tdate dl)
	(dolist (pdat (nreverse pdata))
	  (incf pd-cntr)
	  (setq p-tem (float pdat))
	  (if (< p-tem p-mn) (setq p-mn p-tem))
	  (if (> p-tem p-mx) (setq p-mx p-tem))
	  (unless (vector-push (float pdat) pdata-vct)
	    (setq err-flg "TOOBIG") (return)))
	(push (- pd-cntr pd-cntr1) pd-dim)	; pd-dim->list of no-hrs/day

	(when (not (equal tims tims0))
	  (push (cons ndate tims0) times))
	(setq tims0 tims)

	;;TEST TO END THE LOOP & DO LAST ITERATION ACTIONS:
	(when (or (and (if no-days (>= dayc no-days))
		      ;(setq *start* tdate))       ; not needed for cross rates
		       tdate)
		  (and (if start (edate>= start tdate))
		      ;(setq *no-days* dayc no-days dayc *start* tdate))) ; ""
		       (setq no-days dayc tdate tdate)))
	  (setq flag t)
	  (push (cons tdate tims) times)
	  (save-plot-params1 0 'hpdl pd-dim 'times (butlast times))
	  (unless (array-in-bounds-p *pdata-vct* (1+ pd-cntr))
	    (setq err-flg "TOOBIG"))
	  )
	(setq pd-cntr1 pd-cntr)))))

(defun goto-err (err-flg tdate dayc &aux errmsg)
  ;; note: at this point dayc is one too large
  (cond ((equal (subseq err-flg 0 6) "DATA N")
	 (setq errmsg
	       (format nil "~%ERROR - DATA NOT AVAILABLE FOR ~a, ~s "
		       (dayname tdate) tdate))
	 (if (plusp (- dayc 1))
	     (setq errmsg
		   (string-append errmsg
				  (format nil "~%MAX DAYS THAT CAN BE REQU~
					  ESTED  = ~s" (- dayc 2))))))
	((or (equal (subseq err-flg 0 6) "BAD TI")
	     (equal (subseq err-flg 0 6) "UNABLE"))
	 (setq errmsg
	       (format nil "~%ERROR - REQUESTED TIME INTERVA~
		       L = ~s MIN, NOT AVAILABLE ON ~a, ~s"
		       *time-interval* (dayname tdate) tdate)))
	((equal err-flg "TOOBIG")
	 (setq errmsg (format nil "~%ERROR - PLOT SIZE TOO LAR~
			GE - AT DATE = ~s  NO DAYS = ~s " tdate (1- dayc))))
	(t (setq errmsg (format nil "~%ERROR ??? -- Date requested: ~a,~s"
				(dayname tdate) tdate))))
  (values errmsg))




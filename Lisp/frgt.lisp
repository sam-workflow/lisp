;;; -*- Mode: LISP; Package: USER; Base: 10. -*-
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
;;;*maxw* is the largest index used for wave vectors
;;;
;;;THIS IS A PROCEDURE TO REMOVE WAVES FROM A COUNT WHEN THEY ARE NO LONGER
;;;NEEDED BY E-WAVES
(defvar *minct* nil "THE COUNT WITH THE WORST EVALUATION") 
(defun forget (&optional (stream *terminal-io*) (cts 'total-counts))
  (if *print-sprouts* (setq stream *terminal-io*))
  (let ((*extraneous-waves* nil) (no-waves 0) n1 flag wavei wvww)
    (declare (special *extraneous-waves* no-waves))
    (gen-status1 "~%SEARCHING FOR EXTRANEOUS WAVES .. " stream)
    ;;;loops over all waves in wave arrays
    ;;;waves 0, 1, and 2 have special uses and are excluded
    (DOTIMES (WAVE (- *MAXW* 2))
      (SETQ FLAG NIL WAVEI (+ 3 WAVE) *EXTRANEOUS-WAVES* NIL)
      ;;;is this a valid wave? (a vector and non zero counter)
      (AND (VECTORP (GET-WAVE-VECTOR WAVEI))
	   (NOT (ZEROP (GET-WAVE-COUNTER WAVEI)))
	   ;;;tests for extraneous wave
	   ;;;only first waves are identified
	   (FIRST-WAVE-P WAVEI)
	   (EXTRANEOUS-WAVE-P WAVEI)
	   (SETQ WVWW (GETV WAVEI 'WW))
	   ;;;if test true places the wave and its substructure in
	   ;;;*extraneous-waves*
	   ;;;finds at least one count with this wave and surgically
	   ;;;alters it sets root lower of 'WW to nil for all counts
	   ;;;the wave is in.
	   (DOLIST (CT (EVAL CTS) T)
	     (WHEN (ASSOC WAVEI (SYMBOL-VALUE CT))
	       (UNLESS FLAG 
		 (PUSH-NW-RL CT WAVEI)
		 ;;;lower counter to zero on extraneous waves in
		 ;;;*extraneous-waves* 
		 (DOLIST (WV *EXTRANEOUS-WAVES*)
		   (KILL-WAVE ct WV))
		 ;;;set flag so extraneous waves are killed only once
		 (SETQ FLAG T))
	       ;;;and removes the *extraneous-waves* from all counts by
	       ;;;surgical removal from one or more  counts
	       ;;;the "conses" may not be shared in all the counts
	       (DOLIST (WV *EXTRANEOUS-WAVES* T)
		 (SET CT (DELETE (ASSOC WV (eval CT)) (eval CT)))
	       (PUTV WVWW 'RL NIL CT)))
	     );closes the loop over counts
	   );closes the AND test for which waves to surgically remove	 
      );closes the loop over waves
    (IF (= NO-WAVES 0) (SETQ N1 "NO") (SETQ N1 NO-WAVES))
    (gen-status1
     (string-append "~%" (format nil "THERE WERE ~a WAVE(S) FORGOTTEN        "
				 N1)) stream)
    NO-WAVES))

(DEFUN EXTRANEOUS-WAVE-P (WV)
  (GETV (DOTIMES (I *FORGET-DEPTH* WV)
	  (SETQ WV (GETV WV 'WW))) 'ET))


;;a wave is killed by setting  its counter to 0
(DEFUN KILL-WAVE (ct-symb wave1)
      (update-w-status ct-symb wave1 nil))

;;;given a first wave this function returns a list of the next waves
;;;of same degree and all the subwaves of lower degrees within them
;;;adds their count to the total no-waves 
(DEFUN PUSH-NW-RL (CT WAVEI)
  (DECLARE (SPECIAL *EXTRANEOUS-WAVES* NO-WAVES))
  (LET ((WVRL (GETV WAVEI 'RL CT)) (WVNW (GETV WAVEI 'NW CT)))
    (COND ((NOT WAVEI) NIL)
	  (T (PUSH WAVEI *EXTRANEOUS-WAVES*)(INCF NO-WAVES)
	     (OR  (PUSH-NW-RL CT WVRL)
		  (PUSH-NW-RL CT WVNW))))))


;;;;this is for removing the  counts
;;;; when the *max-counts* is exceeded
(defun delete-excess-counts (flag2 primw i &aux (xs2 0) (ys2 1))
  (ifn flag2 (setq *minct* nil))
  (let ((no-excess-counts nil)
;;	 (minctci (if (and flag2 (boundp *minct*))
;;		      (ci-for-degree *minct* *ci-degree-limit*)))
	 )

    ;;;SUBSTITUTE FORMAT FOR SEND
    (setq xs2 xs2)(setq ys2 ys2)
    ;(send primw :clear-screen)
    (gen-status4 (format nil "~%~%") primw)
    (gen-status4
     (format nil
	     " PROCESSING PRIMITIVE NO ~a ENDING ON ~a AT ~a HRS"
	     (1+ i)(getnumdate *curr-ET*)(getnumhour-display *curr-ET*)) primw)
    (cond (flag2
	   ;(format t "~%DELETING EXCESS COUNTS - INDEX = ~s" *INDX*)

	   ;;;SUBSTITUTE FORMAT FOR SEND
	   ;(send primw :set-cursorpos xs2 ys2)
	   ;(format t "~%~%")
	    
	    (CI-over-counts (nthcdr *prev-count-len* *tot-partial-counts*)
			    nil *ci-degree-limit* primw flag2)
;;;;commented out 5/2/91 for study
;	    (when (> (- *floor-deg* *sb-degree-limit*) 3)
;	      (format primw
;		"~% REMOVING THE COUNTS WITH LOWEST DEGREES.")
;	      (kill-lowest-degree-counts)
;	       (decf *floor-deg*))

	    )
	  (t
	   (gen-status1 
           (format nil "~%THE CURRENT MAXIMUM NUMBER OF COUNTS ~s WAS EXCEEDED"
            *max-counts*) primw)
;;;commented out for study	   
;	   (format primw
;		"~% REMOVING THE COUNTS WITH LOWEST DEGREES.")
;	   (setq *floor-deg* (kill-lowest-degree-counts))
;;	   (kill-primitive-4-degree-levels '*tot-partial-counts*)
;;	   (when (> (- *floor-deg* *sb-degree-limit*) 3)
;;	     (decf *floor-deg*))
	   (CI-over-counts *tot-partial-counts*
			   nil *CI-degree-limit* primw flag2)
	   (setq *minct* nil);initialization for this primitive
	   ))

      (setq no-excess-counts (- (length *tot-partial-counts*)
				(round (* .6000 *max-counts*))))
      (unless (or flag2 (not (plusp no-excess-counts)))
	(delete-equivalents primw
			    '*tot-partial-counts* 'nearly)
;;	(if (> (length *tot-partial-counts*) *max-counts*)
;;	    (delete-equivalents primw '*ptc* 'nearly))
	(setq no-excess-counts
	      (- (length *tot-partial-counts*) (round (* .6000 *max-counts*)))))

      (unless (or  (not flag2) (not (plusp no-excess-counts)))
;	(format primw
;		"~% IDENTIFYING AND REMOVING THE  ~A LOWEST RANKED COUNTS."
;		no-excess-counts)
	(gen-status1 (string-append "~%" (format nil
		"IDENTIFYING AND REMOVING THE  ~A LOWEST RANKED COUNTS."
		no-excess-counts)) primw))

      (ifn (boundp *minct*) (setq *minct* nil))
      (when flag2
;;	(ifn *minct*
;;	     (multiple-value-setq (*minct* minctci)
;;	       (do* ((cts (ldiff *tot-partial-counts*
;;				 *partial-counts*) (cdr cts))
;;		     (ct (car cts) (car cts))
;;		     (ct-ci (get ct 'ci-count) (get ct 'ci-count))
;;		     (minci ct-ci)
;;		     (low-ct ct))
;;		    ((null (cdr cts)) (values low-ct minci))
;;		 (if (< ct-ci minci) (setq minci ct-ci low-ct ct)))))
;;	
;;	(setq *partial-counts*
;;	      (rank-count2 *partial-counts*))
;;	(dolist (ct (reverse *partial-counts*))
;;	  (ifn (plusp no-excess-counts) (return))
;;	  (cond ((<= (ci-for-degree ct) minctci)
;;		 (kill ct 'LOW-CI '*tot-partial-counts*)
;;		 (decf no-excess-counts))
;;		(t (kill *minct* 'LOW-CI '*tot-partial-counts*)
;;		   (decf no-excess-counts)
;;		   (setq *minct* nil)(return))))
	(setq *tot-partial-counts* (rank-count2 *tot-partial-counts*))
	(when (plusp no-excess-counts)
	  (dolist (ct (copy-list (nthcdr (round (* .6000 *max-counts*))
					 *tot-partial-counts*)))
	    (kill ct 'LOW-CI '*TOT-partial-counts*))
	  (setq *minct* nil))));closes the let
  (setq *prev-count-len* (length *tot-partial-counts*)))


;;;***************************************************************************
;;; FIND AND DELETE EQUIVALENT COUNTS  -  30-AUG-87 11:35am  -  LMF
;;; REVISED 24-MAR-89  - DTR
;;;***************************************************************************
;;;       
(defun delete-equivalents (wind-strm counts defn &optional (outp-p t))
  (let ((num-killed 0) t-dups mapped-ecodes lme (il 0) string-legend)
    (setq mapped-ecodes (map-equiv-codes wind-strm counts defn outp-p)
	  lme (length mapped-ecodes))
    (unless (zerop lme)
      (setq string-legend 
	    (string-append
	     "~%"
	     (format nil "VERIFYING EQUIVALENTS FOR ")
	     (format nil "~A" (case counts
				(total-counts         "ALL THE COUNTS")
				(*tot-partial-counts* "THE NEW COUNTS")
				(*ptc*                "UNPROCESSED CNTS")))))
      (if outp-p (gen-status1 string-legend wind-strm))
      (if outp-p (in-place-del-eq1 
       (format nil "...~%                                         COMPLETION STATUS: ") wind-strm))
      (dolist (hash-ct-el mapped-ecodes)
	(when (> (length hash-ct-el) 2)
	  (setq t-dups (verify-equivalents (cdr hash-ct-el) defn))
	  (if outp-p
	      (in-place-del-eq2 (round  (* 100 (/ (incf il) lme))) wind-strm))
;;;let's make sure we kill from the right list 
	  (if (eql counts '*ptc*)(setq counts 'total-counts))
	  (setq num-killed
		(+ num-killed
		   (select-and-kill (cdr hash-ct-el) t-dups counts)))))
;;;need to remove any counts in *partial-counts* that were killed by
;;;equivalent test on *tot-partial-counts*	
      (setq *partial-counts*
	    (remove-if #'(lambda (s) (not (boundp s))) *partial-counts*))
      (if (eql counts 'total-counts)
	  (setq *ptc*
		(remove-if #'(lambda (s) (not (boundp s))) *ptc*)))
      (if outp-p (in-place-del-eq3 100 wind-strm)))
    (if outp-p (gen-status1 (string-append "~%"
	  (format nil "THERE WERE ~s COUNTS KILLED                     "
		  (if (zerop num-killed) 'NO num-killed))) wind-strm))
    num-killed))

(defun map-equiv-codes (wind-strm counts defn outp-p)
;  (format wind-strm
;	  "~% FINDING ~A EQUIVALENT COUNTS ... ~s  COUNTS,"
;	  defn (length (symbol-value counts)))
;  (format wind-strm  "~%                                          ~
;;	  MAPPING COUNT NO:")
  (if outp-p (gen-status1 (string-append "~%" (format nil
	  "FINDING ~A EQUIVALENT COUNTS ... ~s  COUNTS,"
	  defn (length (symbol-value counts)))) wind-strm))
  (if outp-p
      (gen-status4 (format nil  "~%                                          MAPPING COUNT NO:") wind-strm))
  (if outp-p (in-place-eq-cts 1 0 nil wind-strm))
  (let (hcode h-el (hlist '()) (i 0))
    (dolist (ctsy (symbol-value counts))
      (setq hcode
	    (sxhash (count-equiv-map ctsy
				     (wv-for-degree ctsy *CI-degree-limit*)
				     defn)))
      (setq h-el (assoc hcode hlist))
      (if h-el
	  (rplacd h-el (cons ctsy (cdr h-el)))
	  (push (cons hcode (list ctsy)) hlist))
      (if outp-p (in-place-eq-cts 2 (incf i) nil wind-strm))
      )
    (if outp-p (in-place-eq-cts 3 (length total-counts) nil wind-strm))
    hlist))

;;;STARTS W/WAVE & MAPS ATR-LIST FOR COUNT (BY NCONC'ING ALL WAVES ATR-LIST)
(defun count-equiv-map (ct wave defn)
  (do* ((cwave wave (getv cwave 'NW ct))
	(rwave (getv cwave 'RL ct) (getv cwave 'RL ct))
	(result (list)))
       ((not cwave) (values result))
    (multiple-value-bind (atr-lst test) (wave-equiv-map ct cwave defn)
      (setq result (nconc atr-lst result))
;      (if test (setq result (nconc (count-equiv-map ct rwave defn) result)))
     (case test
       (1 (setq result (nconc (count-equiv-map ct rwave defn) result)))
       (5 (setq result (nconc (count-equiv-map ct (lastsw ct cwave) defn)
                               result)))
        (otherwise nil))
      )))

;;;MAPS ATR-LIST FOR ONE WAVE - DECIDES (BY 2ND VALUE) IF TO GO DOWN TO RL'S
(defun wave-equiv-map (ct wave defn)
  (if (getv wave 'ET)
      ;(if (or (eql (getv wave 'SB) 'NOR) (eql (getv wave 'SB) 'FAI))
;	  (if (and (member (getv wave 'LB) '(1 3 AI))
;		   (getv (getv wave 'NW ct) 'ET))
;	      (values (get-atr1 wave defn) nil)
;	      (values (nconc (get-atr1 wave defn)
;			     (list (getv-CX ct wave defn)))
;		    (if (not (getv (getv wave 'WW) 'ET)) 5)))
		    
	  (values (nconc (get-atr1 wave defn)
			 (list (getv-CX ct wave defn))) 
			 (if (not (getv (getv wave 'WW) 'ET)) 5 nil));;changed from nil to 5 on 9/25/05
			 
      (values (get-atr1 wave defn) 1)))

;;;BASIC ATR-LIST FOR A WAVE (TO THIS SB OR COMPLEXITY MAY BE ADDED ABOVE)
(defun get-atr1 (wave defn)
  (case defn
    (strictly
      (list (getv wave 'LB) (getv wave 'DG) (get-q-pointer wave)))
    ((almost nearly)
     (list (getv wave 'LB) (getv wave 'DG) (stringp (getv wave 'et))))))
    

;;;COMPUTES COMPLEXITIY OF WAVE IF EXISTS
(defun getv-CX (ct wave defn)
  (let ((subt (getv wave 'SB)))
  (declare (ignore ct defn))
;;;commented out 12/11/2003  
   ; (if (neql defn 'nearly)
;	(case (getv wave 'lb)
;	  ((4 B 4C X XX 1C 3C 5C CC D E) (setq defn 'nearly))
;	  (2 (if (getv (getv (getv wave 'nw ct) 'nw ct) 'et)
;		 (setq defn 'nearly)))))
;    (case defn
;      ((strictly almost)
;       (cond ;((or (eql subt 'DT1)(eql subt 'NOR)) 'IMP);
;	     ((member subt *SMP-list*) 'DSMP)
;	     ((member subt *CMX-list*) 'CMX)
;	     ((member subt *ETH-list*) 'ETH)
;	     (subt)
;	     ((not subt) t)))
;      (nearly (cond ((or (member subt *SMP-list*)
      (cond ((or (member subt *SMP-list*)
	         (member subt *CMX-list*)
		 (member subt *ETH-list*)) 'COR)
	    ((member subt '(NOR FAI DT1 DT2 UIP)) 'IMP)
	    ((eql subt 'UNK) 'UNK)
	    ((not subt) t))))

(defun verify-equivalents (counts defn)
  (do ((mapped (mapcar
		  #'(lambda (ct) 
		      (count-equiv-map ct
				       (wv-for-degree ct *CI-degree-limit*)
		      defn))
		  counts) (cdr mapped))
       (indx 0 (1+ indx))
       (totcts (length counts)) (dups '()) jndx asset)
      ((null mapped) (values (mapcar #'(lambda (d) (reverse d)) dups)))
    (and (car mapped)
	 (when (member (car mapped) (cdr mapped) :test #'equal)
	   (setq jndx (- totcts (length (member (car mapped) (cdr mapped)
						:test #'equal))))
	   (setq asset (assoc indx dups))
	   (ifn (or asset (member indx asset))
		(setf dups (push (list jndx indx) dups))
	     (setf dups (subst (nconc (list jndx) asset) asset dups)))))))

;;;SELECT THE COUNT WITH HIGHEST CI AND REMOVE IT FROM THE EQUIVALENTS DELETION
;;;LIST - KILL THE REMAINING EQUIVALENTS.
(defun select-and-kill (dups t-dups cts)
  (let (cix (cit 0) ix (num-k 0)	 ;dolist for if t-dups has > 1 sets of
	    jset ii idups)		 ; real dupls in the same list of dups
    (dolist (iset t-dups)	       	 ; iset->one set of indices of real 
      (setq cix -1000 ix 0)		 ; dupls from dups. Assume no CI < -100
      (dotimes (it (length iset))	 ;dotimes->find indx of dupl cts (withn
	(setq ii (nth it iset))		 ; dups) with highest CI for each iset
	(setq cit
	      (CI-for-degree (nth ii dups) *CI-degree-limit*))
	(if (gtr cit cix) (setq cix cit ix it)))
      (setq jset (delete-nth ix iset))
      (dotimes (index (length jset))
	(incf num-k)
	(setq idups (nth index jset))
	(kill (nth idups dups) 'EQUIVS cts)))
    num-k))

;;;;find the lowest degree for the set of counts


;;;remove counts with lowest degree
(defun kill-lowest-degree-counts ( &aux (gld 0))
  (dolist (ct  (copy-list *tot-partial-counts*))
    (if (> (max* (get ct 'LD)) gld)
	(setq gld (max* (get ct 'LD)))))
  (dolist (ct  (copy-list *tot-partial-counts*))
    (if (eql (max* (get ct 'LD)) gld)
	(kill ct 'LOW-DG '*tot-partial-counts*)))
  ;;;need to remove any counts in *partial-counts* that were killed by
  ;;;equivalent test on *tot-partial-counts*	
  (setq *partial-counts*
	(remove-if #'(lambda (s) (not (boundp s))) *partial-counts*)
	*ptc*
	(remove-if #'(lambda (s) (not (boundp s))) *ptc*))
  (dolist (ct  (copy-list *ptc*))
    (if (eql (max* (get ct 'LD)) gld)
	(kill ct 'LOW-DG 'total-counts)))
  (setq *ptc* (remove-if #'(lambda (s) (not (boundp s))) *ptc*))
  gld)

(defun kill-primitive-4-degree-levels (&optional (counts 'total-counts)
					     &aux (gld 0)(dhold nil))
  (dolist (ct (eval counts))
    (when (> (getv (caar (symbol-value ct)) 'dg) gld)
      (pushnew (getv (caar (symbol-value ct)) 'dg) dhold)
      (setq gld (max* dhold))))
  (when (and counts (> (- (max* dhold) (min* dhold)) 3))
    (setq gld (+ 3 (min* dhold)))
    (dolist (ct (eval counts))
      (if (> (getv (caar (symbol-value ct)) 'dg) gld)
	    (kill ct '4-DG counts)))
    (setq *floor-deg* gld)))
















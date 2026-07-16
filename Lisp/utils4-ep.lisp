;;; -*- Mode: lisp; Package: user; Base: 10. -*-
;;;
;;;****************************************************************************
;;;                      MAKE MINIMAL COUNTS SOFTWARE
;;;****************************************************************************
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)
;;;
(defun make-unlabeled-minimal-count (initime)
 (block mumc
  (let (inidate inihour err-ret tims pris pris1 pris2 ctsy ydate ndate
		itim pri+ pri- pri peakp
		ignore)
    (declare (ignore ignore))

    ;;Check for good date and hour
    (setq inidate (getnumdate initime)
	  inihour (getnumhour initime))
    (ifn (datep inidate)
	 (return-from mumc (format nil "~a invalid input date" inidate)))
    (multiple-value-setq (err-ret ydate ndate tims pris)
      (data-access inidate))
    (if (stringp err-ret) (return-from mumc err-ret))
    (setq itim (position inihour tims))
    (ifn itim 
	 (return-from mumc
		      (format nil "~a ~a invalid time of day" inidate inihour)))

    ;;Check for peak or valley and find if peak or valley
    (setq pri (nth itim pris))
    (cond ((zerop itim)
	   (multiple-value-setq (ignore ignore ignore ignore pris1)
	     (data-access ydate))
	   (cond ((cadr pris)
		  (setq pri- (car (last pris1))
			pri+ (cadr pris)))
		 (t (multiple-value-setq (ignore ignore ignore ignore pris2)
		      (data-access ndate))
		    (setq pri- (car (last pris1))
			  pri+ (car pris2)))))
	  ((= (length tims) (1+ itim))
	   (multiple-value-setq (ignore ignore ignore ignore pris1)
  	     (data-access ndate))
	   (setq pri+ (car pris1)
		 pri- (car (penultimo pris))))
	  (t (setq pri+ (nth (1+ itim) pris)
		   pri- (nth (1- itim) pris))))
    (cond ((and (> pri pri-) (>= pri pri+)) (setq peakp t))
	  ((and (< pri pri-) (<= pri pri+)) (setq peakp nil))
	  (t (return-from mumc (format nil "~a not peak or valley" initime))))

    ;;Create a new non-labeled count
    (init-new-count)
    (setq ctsy (car total-counts))
    (make-wave1-min ctsy initime peakp)
    (provide-vcts-defined-count (car total-counts))     ; in acce
    (fix-up-qs (car total-counts))			; in acce
   ;(fix-degrees)
    (setq *latest-dtime* *curr-et* *cnts-delt* *time-interval*)
    )))

;;;ORIGINALLY FROM PLT3 (VERY LITTLE OF PLT3 NEEDED IN THE BATCH VERSION)
;;;




(defun get-price (tim1)
  (let (tims pris ignore)
    (declare (ignore ignore))
    (multiple-value-setq (ignore ignore ignore tims pris)
      (data-access (getnumdate tim1)))
    (nth (position (getnumhour tim1) tims) pris)))

(defun read-label-degree (&rest foo) (setq foo foo))

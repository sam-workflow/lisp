;;; -*- Mode: LISP; Package: user; Base: 10. -*-
;;;

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

#-gclisp 
;(defun chmod (path)
;  (shell (format nil "chmod g+w ~a"  path)))
#-gclisp
;(defun chmod1 (path)
;  (shell (format nil "chmod g+w ~a"  path)))
#+gclisp
(defun chmod (path)
  (sys:dos (format nil "chmod g+w ~a"  path))
  (sys:8087-fpp :automatic))
#+gclisp
(defun chmod1 (path) nil)

#-gclisp 
(defun ewaves-database-path (index-name yyyymm)
  (format nil "~a~(~a~)/d~s.dat" *database-upper-dir*
	  index-name yyyymm))
#+gclisp
(defun ewaves-database-path (index-name yyyymm)
  (format nil "~a~a/d~s.dat" 
	  *database-upper-dir* index-name yyyymm))

#-gclisp
(defun ewaves-database-directory-string ()
  (format nil "~a~(~a~)" *database-upper-dir* *data-name*))
#+gclisp
(defun ewaves-database-directory-string ()
  (format nil "~a~a" *database-upper-dir* *data-name*))

#-gclisp
(defmacro my-rename-file (file1 file2)
  `(rename-file ,file1 ,file2))
#+gclisp
(defmacro my-rename-file (file1 file2)
  `(string-append "dos2unix "
		  (string-downcase (format nil "~a" ,file1))
		  (string-downcase (format nil "~a" ,file2))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Index Handling
;;;   write-cat-index: Updates Index file
;;;   Read-Index:  Input Index file
;;;   Find-Dates:  Creates list of Years & Months available
;;;   month-days: list of (days in month . date) with data available
;;;   Get-Latest-Index-Date: Finds the Most Recent date available
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Index File Format:
;;;   (... (yyyy . #(Jan Feb ... Dec) ...)
;;;   Notes:
;;;   Jan ... Dec are all 32 bit integers with the ith bit a boolean
;;;     test for the ith day
;;;   The twelve months are stored as an array: so the ith month
;;      data is (aref vector ith)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(defun get-cat-indices ()
   (let ((index (cdr (assoc *data-name* *cat-indices* :test 'eq))))
     (unless index
        (read-cat-indices)
	(setq index (cdr (assoc *data-name* *cat-indices* :test 'eq))))
     index))
#-gclisp
(defun write-cat-index (index)
  (let () ;; (cd (directory-namestring *default-pathname-defaults*)))

    ;;(cd (ewaves-database-directory-string))
    ;;;
    ;;; Write to temporary file
    ;;;
    ;;;;;;(unless index (setq index (get-cat-indices)))

    (unwind-protect
	(progn
	  (with-open-file (strm (format nil "~a/~a" (ewaves-database-directory-string) "bar.idx")
				:direction :output :if-exists :supersede :if-does-not-exist :create)

			  (prin1 'foo strm))
	  (sleep 0.1)
	  (setq index (reverse index))
	  (with-open-file (strm (format nil "~a/~a" (ewaves-database-directory-string) "foo.idx")
				:direction :output :if-exists :supersede :if-does-not-exist :create)

	    (dolist (year index)
	      (format strm "(~s . #(" (pop year))
	      (dotimes (i 12) (format strm "#x~8,'0x " (aref year i)))
	      (format strm "))~%")))
	  ;;; Rename temp file to permanent file
	  (delete-file (format nil "~a/~a" (ewaves-database-directory-string) "index.idx"))
	  (my-rename-file (format nil "~a/~a" (ewaves-database-directory-string) "foo.idx")
			  (format nil "~a/~a" (ewaves-database-directory-string) "index.idx"))
	 ; (chmod (format nil "~a/~a" (ewaves-database-directory-string) "index.idx"))
	  )

      ;;(cd (ewaves-database-directory-string))
      (when (probe-file  (format nil "~a/bar.idx" (ewaves-database-directory-string)))
	(delete-file (format nil "~a/bar.idx" (ewaves-database-directory-string)))
      ;;(cd cd)
      )
      )
    ))

#|
#+gclisp
(defun write-cat-index (index)
  (let ((cd (directory-namestring *default-pathname-defaults*)))

    (cd (ewaves-database-directory-string))
    ;;;
    ;;; Write to temporary file
    ;;;
    ;;;;;;(unless index (setq index (get-cat-indices)))

    (unwind-protect
	(progn
	  (with-open-file (strm "bar.idx"
				:direction :output :if-exists :supersede :if-does-not-exist :create)
	    (prin1 'foo strm))
	  (sleep 0.1)
	  (setq index (reverse index))

	  (with-open-file (strm "c:\\tem\\foo.idx" :direction :output :if-exists :supersede :if-does-not-exist :create)
	    (dolist (year index)
	      (format strm "~%(~s . #(" (pop year))
	      (dotimes (i 12) (format strm "#x~x " (aref year i)))
	      (format strm "))")))
	  ;;; Rename temp file to permanent file
	  (sys:dos "dos2unix c:\\tem\\foo.idx c:\\tem\\tem.idx")
	  (delete-file "index.idx")
	  (sys:dos 
	    (format nil "copy c:\\tem\\tem.idx ~a\\index.idx"
		    (ewaves-database-directory-string)))
	  ;(my-rename-file "foo.idx" "index.idx")
	  (cd (ewaves-database-directory-string))
	  (chmod "index.idx")
	  )
      (cd (ewaves-database-directory-string))
      (when (probe-file  (format nil "~a/bar.idx"
				 (ewaves-database-directory-string)))
	(delete-file "bar.idx"))
      (sys:8087-fpp :automatic)
      (cd cd))
    ))
|#
(defun read-cat-indices ()
  (let (temp indexa (i 0))
    (loop
      (cond ((and
	       (not (probe-file
		      (format nil "~a/bar.idx" (ewaves-database-directory-string))))
	       (probe-file 
		 (format nil "~a/index.idx" (ewaves-database-directory-string)))
	       )
	     (with-open-file
		 (infile (format nil "~a/index.idx" (ewaves-database-directory-string)))
	       (do ((item-info (read infile nil nil) (read infile nil nil)))
		   ((not item-info))
		 (setf temp (cons item-info temp))))
	     (setq indexa (assoc *data-name* *cat-indices* :test 'eq))
	     (if indexa
		 (setf (cdr indexa) temp)
	       (push (cons *data-name* temp) *cat-indices*))
	     (return)
	     )
	    (t (if (> (incf i) 3)
		   #-:CLIM (progn
			     (format t "~%Read index.idx failed ~a times" i)
			     (format t "~%Type 0 (zero) to attempt to continue")
			     (format t "~%Type anything else quit")
			     (if (eql (read-char) #\0) (setq i 0) (return)))
		   #+:CLIM (progn (gui::notify1 (format nil "~a~a~a~%" "Read index.idx failed " i " times")) (return))
		   (sleep 3)))

	    ) ; closes cond
      )))

(defun update-cat-index (tdate how writep)
  (declare (integer tdate))
  (let (indexa index year month day year-data datap)
    (declare (integer year month day))
    (setq index (get-cat-indices)
          indexa (assoc *data-name* *cat-indices* :test 'eq))
;;;
;;; Add New Index to *cat-indices*
;;;

    (unless index
      (setq indexa (cons *data-name* nil))
      (push indexa *cat-indices*))

    (multiple-value-setq (year day) (truncate tdate 100))
    (multiple-value-setq (year month) (truncate year 100))
    (setq year-data (cdr (assoc year index :test '=)))

;;;
;;; Add New Year to *cat-indices*
;;;
    (unless year-data
      (setf year-data (make-array 12 :initial-element 0))
      (push (cons year year-data) index)
      (setq index (coerce index 'vector))
      (qsort index #'> #'car)
      (setf index (coerce index 'list))
      (setf (cdr indexa) index))

;;;
;;; Update Day info
;;;
    (if (equal how 'add)
	(setf (aref year-data (1- month))
	      (logior (aref year-data (1- month)) (expt 2 day)))
        (setf (aref year-data (1- month))
	      (logxor (aref year-data (1- month)) (expt 2 day))))
    (if (<= 1 (aref year-data (1- month))) (find-dates))
    (setq datap nil)
    (when (equal how 'delete)
      (dotimes (i 12)
	(when (> (aref year-data i) 1)
	  (setq datap t)
	  (return)))
      (unless datap
	(setq index (remove (assoc year index :test '=) index ))
	(setf (cdr indexa) index)
	)
      )
    
    ;;;
    ;;; Needed to allow the convert-file to only update the
    ;;; index file once per month
    (when writep (write-cat-index index))
    ))

(defun find-dates ()
  ;;; get list of dates
  ;;; (... (yyyymm yyyymm) ... (yyyymm ...) ...)
  ;;; Notes: sorted yearly from present to past
  ;;; sorted per year Dec back to Jan
  ;;;
  (let ((index (get-cat-indices)) avail temp year)
    (cond (index
	   ;;;
	   ;;;;   Data Present
	   ;;;
	   (dolist (year-info index)
	     (setq temp nil)
	     (setq year (* 100 (pop year-info)))
	     (dotimes (i 12)
	       (if (> (aref year-info i) 0)
		   (setq temp (push (+ year (1+ i)) temp))))
	     (if temp (push temp avail)))
				     
	     (nreverse avail))
	  (t (ncons nil)))
   
    ))

(defun month-days (date)
  (declare (integer date))
  (let ((index (get-cat-indices)) days (bigdate (* date 100)))
    (cond (index
	   (multiple-value-bind (year month) (truncate date 100)
	     (declare (integer year month))
	     (setq index (cdr (assoc year index :test '=)))
	     (cond (index
		    (setq index (aref index (1- month)))
		    (dotimes (i 31)
		      (if (logbitp (1+ i) index)
			  (setf days (cons (+ bigdate (1+ i)) days))))
		    (setq days (nreverse days)))))))
    days))
;;;*********GET THE LATEST AVAILABLE DATE IN THE MARKET DATA DATA BASE*********
(defun get-latest-index-date ()
  (let ((temp (get-cat-indices)) year (dmax 1))
    (cond (temp
	   (setq temp (first temp)
                 year (car temp)
	         temp (cdr temp))
	   (dotimes (i 12)
	     (if (> (aref temp i) 0) (setq dmax i)))
	   (+ 1 dmax (* year 100)))
	  (t nil))))

(defun get-first-index-date ()
  (let ((temp (get-cat-indices)) year (dmin 1))
    (cond (temp
	   (setq temp (first (last temp))
                 year (car temp)
	         temp (cdr temp))
	   (dotimes (i 12)
	     (when (> (aref temp i) 0) 
	       (setq dmin i)
	       (return))
	     )
	   (+ 1 dmin (* year 100)))
	  (t nil))))


(defun day-in-index (tdate)
  (declare (integer tdate))
  (member tdate (month-days (truncate tdate 100))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data File Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun convert-item (item-info dio-parms)
  (let (temp new-item-info)
    (dolist (item item-info)
      (cond ((listp item)
	     (setq temp (assoc (car item) dio-parms :test 'eq))
	     (when temp 
	       (setq new-item-info (cons (cons (cdr temp) (cdr item))
					       new-item-info))))
	    (t (push item new-item-info))
	     ))
    (nreverse new-item-info)))

(defun write-cat-item (item-info statics ofile)
   (let ((temp (cdr item-info)) static ttemp cdate)
        (setq cdate (car item-info))
	(dolist (item temp)
	  (cond ((setq static (assoc (car item) statics :test 'eq))
		 (unless (equal (cdr static) (cdr item))                  
		   (setf (cdr static) (cdr item))
                   (print static ofile)))
                ((= 0 (car item))
	         (setf (cdr item) (- cdate (cdr item)))
	         (if (> (cdr item) 1) (setf ttemp (cons item ttemp))))
	        ((= 1 (car item))
		 (setf (cdr item) (- (cdr item) cdate))
	         (if (> (cdr item) 1) (setf ttemp (cons item ttemp))))
               (t (push item ttemp)))
	  )
	(format ofile "~%~s" (cons (car item-info) ttemp))
       ))

(defun write-file (items infile idx-parms)
  (let (temp dio-parms  tstatics
        (statics (list '(2) '(3) '(4) '(5) '(6) '(7) '(8) '(9) '(10) '(12))))
    (setq temp (length *dio-parms*))
    (setq tstatics (copy-alist statics))
    (dolist (parm idx-parms)
      (push (cons parm temp) dio-parms)
      (setq temp (1+ temp)))
    (setq dio-parms (append *dio-parms* (nreverse dio-parms)))

    ;;(print 'wf-1 t)
    ;;(print ofile t)
    ;;(print infile t)

    (with-open-file (ofile infile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print idx-parms ofile)
      (dolist (item-info items)
        (unless (assoc 'nsamp (cdr item-info) :test 'eq)
	  (setq item-info (append item-info '((nsamp)))))
	(setq temp (convert-item item-info dio-parms))
	(write-cat-item temp tstatics ofile))
	)
    (chmod1 infile)
    ;(shell (format nil "chmod g+w ~a" infile))
    ))


(defun expand-item (item-info dio-parms)
  (let (temp)
  (dolist (item item-info)
    (when (listp item)
      (setq temp (aref dio-parms (car item)))
      (setf (car item) temp)
      ))
  ))


(defun expand-file-aux (statics item value)
  ;;;
  ;;;  Maintain a list of equal items to put in *cat-list*
  ;;;  Will Keep *cat-list* size to a minimum
  ;;;
  (setf value (cdr value))
  (setf statics (copy-alist statics))
  (cond (value
         (if (assoc item statics :test 'eq)
	   (setf (cdr (assoc item statics)) value)
	   (push (cons item value) statics)))
	(t
	 (setq statics (remove (assoc item statics :test 'eq) statics))))
  statics
  )

;;;(defun expand-file (yyyy-mm)
(defun concat-read (yyyy-mm)
  (let (temp share filename (statics nil) expanded-file cdate dtemp
        idx-parms dio-parms)
    (if (not (stringp yyyy-mm)) (setq yyyy-mm (format nil "~A" yyyy-mm)))
    (setq filename (format nil "~a~a~a~a" *data-read-dir* "d" yyyy-mm ".dat"))
    (cond
      ((or (month-days (read-from-string yyyy-mm))
	   (probe-file filename))
       ;;;(data-for-month (read-from-string yyyy-mm))
       (with-open-file (infile filename)
         (setq temp (length *dio-parms*)
	       idx-parms (read infile))
         (dolist (parm idx-parms)
           (push (cons parm temp) dio-parms)
           (setq temp (1+ temp)))
           (setq dio-parms (append *dio-parms* (nreverse dio-parms)))
           (setq dio-parms (coerce (mapcar 'car dio-parms) 'vector))
	 (do ((item-info (read infile nil nil) (read infile nil nil)))
	     ((not item-info))
           (if (< (car item-info) 16)
	   (case (car item-info)	      
	     (2 (setf statics (expand-file-aux statics 'index item-info)))
	     (3 (setf statics (expand-file-aux
				statics 'index-sample-period item-info)))
	     (4 (setf statics (expand-file-aux statics 'open-time item-info)))
	     (5 (setf statics (expand-file-aux statics 'close-time item-info)))
	     (6 (setf statics (expand-file-aux statics 'first-time item-info)))
	     (7 (setf statics (expand-file-aux statics 'aux-data item-info)))
	     (8 (setf statics (expand-file-aux
				statics 'aux-data-sample-period item-info)))
	     (9
              (setf statics (expand-file-aux statics 'nsamp item-info)))
	     ;;; Special Treatment for sharing: 13 & 15
	     ;;; 1st: Try to use list when previously present in month
	     ;;; 2nd: Try to use list from *cat-list* when present
	     ;;; When all else fails use the list
	     (10
	       (setq share (assoc (cons 10 item-info) expanded-file
				  :test 'equal))
	       (cond (share (setq item-info (cdr share)))  ;;;List Present
		     (t
		      (setq share
			    (assoc (cons 10 item-info)
				   (cdr (assoc *data-name* *cat-list*))
				   :test 'equal))
		      (if share
			  (setf item-info (cdr share)))))	    
	       (setf statics (expand-file-aux statics 'ptime item-info)))
	     (12
	       (setq share (assoc (cons 12 item-info) expanded-file
				  :test 'equal))
	       (cond (share (setq item-info (cdr share)))  ;;;List Present
		     (t
		      (setq share
			    (assoc (cons 12 item-info)
				   (cdr (assoc *data-name* *cat-list*))
				   :test 'equal))
		      (if share
			  (setf item-info (cdr share)))))	    
	       (setf statics (expand-file-aux statics 'stime item-info))))
	       (progn 
	       ;;;
	       ;;; Have the list with only the "daily" data
	       ;;; 
	       (setf cdate (car item-info))
	       ;;;
	       ;;; Next Pair of actions expand Ydate & Ndate
	       ;;;   Ydate = cdate - offset, Ndate = cdate + offset
	       ;;;   Offset stored as cdr in alist
	       ;;;   If Offset = 1 then absent from alist
	       (setq dtemp (assoc 0 (cdr item-info) :test '=))
	       (if dtemp
		   (setf (cdr dtemp) ( - cdate (cdr dtemp)))
		   (setf (cdr item-info) (cons `(0 . ,(- cdate 1))
					       (cdr item-info))))
	       (setq dtemp (assoc 1 (cdr item-info) :test '=))
	       (if dtemp
		   (setf (cdr dtemp) (+ cdate (cdr dtemp)))
		   (setf (cdr item-info) (cons `(1 . ,(+ cdate 1))
					       (cdr item-info))))
	       ;;;
	       ;;;  Build the complete alist
	       (expand-item (cdr item-info) dio-parms)
	       (setf temp (append (cdr item-info) Statics))
	       (push (car item-info) temp)
	       (setq expanded-file (cons temp expanded-file))
	       )))
	 )
       ;;; Add current day alist to *cat-list*
       (setf (cdr (assoc *data-name* *cat-list* :test 'eq))
	     (append (list (cons (read-from-string yyyy-mm) expanded-file))
		     (cdr (assoc *data-name* *cat-list*))))
       t)
      (t nil))

    ;;; keep *cat-list* from becoming too large
    (when (> (length (cdr (assoc *data-name* *cat-list*)))
	   *cat-list-ceiling*)
	(nbutlast (cdr (assoc *data-name* *cat-list* :test 'eq)) 1000)
	)
    idx-parms))


;;;
;;;  READ A DAY'S WORTH OF DATA
;;;
(defun readt2 (yr-mn-dy &aux ym)  
  (if (stringp yr-mn-dy) (setq yr-mn-dy (read-from-string yr-mn-dy)))
;  (if (< yr-mn-dy 19000000) (setq yr-mn-dy (add 19000000 yr-mn-dy)))
  (block RDT2
    (ifn (day-in-index yr-mn-dy) (return-from RDT2 nil))
    (let (svals (data-list (cdr (assoc *DATA-NAME* *CAT-LIST* :test 'eq))))
      (unless (integerp yr-mn-dy)
	(PRINC '|THE FILE REQUESTED HAS AN IMPROPER NAME: |)
	(princ yr-mn-dy) (return-from RDT2))
      (setq ym (truncate yr-mn-dy 100))
      (setq svals (cdr (assoc ym data-list :test '=)))
      (when svals 
	(setq svals (assoc yr-mn-dy svals :test '=))
	(return-from RDT2 svals))
;;;
;;;   Read Missing Month
;;;
      (concat-read (truncate yr-mn-dy 100))
      (setq svals (cdr (assoc *data-name* *cat-list* :test 'eq)))
      (setq svals (cdr (assoc ym svals :test '=)))
      (assoc yr-mn-dy svals :test '=)
      )))

#-gclisp
(defun update-file (svals tdate &optional (save t))
  (if (stringp tdate) (setq tdate (read-from-string tdate)))
  (if (< tdate 19000000) (setq tdate (+ tdate 19000000)))
  (let (file-name dog-list temp yymm dio-parms month-list)
    
    (setq yymm (truncate (car svals) 100)) ;;; 19xxxxxx format
    (setq file-name (format nil "d~A" yymm))
    
    ;;(setq cd (directory-namestring *default-pathname-defaults*))
    ;;(cd *data-read-dir*)
    ;; Tried and tried and neither this nor anything else seems to work with Franz.
    ;; to get it to a default directory.
    ;; I am sure there is a way but I'm going to do a faster solution for now.
    ;;(cd (ewaves-database-directory-string))
    ;;(setq *defult-pathname-defaults* (ewaves-database-directory-string))

    ;;;
    ;;; Get all data in file containing tdate
    (setq dio-parms (concat-read yymm))
    (dolist (parm (cdr (assoc 'sdata 
			      (cdr (assoc *data-name* *indx-cfg* :test 'eq))
		       :test 'eq)))
      (pushnew parm dio-parms))
    
    ;;;
    ;;;  Update *cat-list*
    ;;;
    (setq month-list (assoc yymm 
			    (cdr (assoc *data-name* *cat-list* :test 'eq))))
    (setq temp (assoc tdate (cdr month-list) :test '=))
    (ifn month-list (setq month-list (list yymm)))
    (unless (and temp (equal (cdr temp) svals))
      (cond (temp
	     (setf (cdr (assoc tdate (cdr month-list) :test '=))
		   (cdr (copy-alist svals)))
	     )
	    (t
	     (setq temp (assoc *data-name* *cat-list* :test 'eq))
	     (if temp
		 (setf (cdr month-list)
		       (cons (copy-alist svals) 
			     (cdr month-list)))
		 ))
	    )
      ;;;
      ;;; Always Keep the alist sorted
      (setq dog-list (coerce (cdr month-list) 'vector))
      (qsort dog-list #'< #'car)
      (setq dog-list (coerce dog-list 'list))
      
      ;;;
      ;;; Write New file (safely)

      (when save
	(write-file dog-list (format nil "~a/~a.tmp" (ewaves-database-directory-string) file-name) dio-parms)
	(if (probe-file (format nil "~a/~a.dat"  (ewaves-database-directory-string) file-name))
	    (delete-file (format nil "~a/~a.dat" (ewaves-database-directory-string) file-name)))
	(my-rename-file (format nil "~a/~a.tmp" (ewaves-database-directory-string) file-name)
			(format nil "~a/~a.dat" (ewaves-database-directory-string) file-name)))
      ;;;;;;;;;;;;;;;;;;;;(cd cd)
      ;;;;;;;;;;;;;;;;;;;;(setq *defult-pathname-defaults* cd)


      (dolist (td dog-list)
	(unless (day-in-index (car td)) (update-cat-index (car td) 'add save)))
      ))
  )
#+gclisp
(DEFUN UPDATE-FILE (svals tdate &optional (save t))
  (if (stringp tdate) (setq tdate (read-from-string tdate)))
  (if (< tdate 19000000) (setq tdate (+ tdate 19000000)))
  (let (file-name dog-list temp yymm dio-parms month-list cd)
    
    (setq yymm (truncate (car svals) 100)) ;;; 19xxxxxx format
    (setq file-name (format nil "d~A" yymm))
    
    (setq cd (directory-namestring *default-pathname-defaults*))
    ;(cd *data-read-dir*)
    (cd (ewaves-database-directory-string))
    ;;;
    ;;; Get all data in file containing tdate
    (setq dio-parms (concat-read yymm))
    (dolist (parm (cdr (assoc 'sdata 
			      (cdr (assoc *data-name* *indx-cfg* :test 'eq))
		       :test 'eq)))
      (pushnew parm dio-parms))
    
    ;;;
    ;;;  Update *cat-list*
    ;;;
    (setq month-list (assoc yymm 
			    (cdr (assoc *data-name* *cat-list* :test 'eq))))
    (setq temp (assoc tdate (cdr month-list) :test '=))
    (ifn month-list (setq month-list (list yymm)))
    (unless (and temp (equal (cdr temp) svals))
      (cond (temp
	     (setf (cdr (assoc tdate (cdr month-list) :test '=))
		   (cdr (copy-alist svals)))
	     )
	    (t
	     (setq temp (assoc *data-name* *cat-list* :test 'eq))
	     (if temp
		 (setf (cdr month-list)
		       (cons (copy-alist svals) 
			     (cdr month-list)))
		 ))
	    )
      ;;;
      ;;; Always Keep the alist sorted
      (setq dog-list (coerce (cdr month-list) 'vector))
      (qsort dog-list #'< #'car)
      (setq dog-list (coerce dog-list 'list))
      
      ;;;
      ;;; Write New file (safely)

; This didn't work in PC as it should have so we went for the sledge hammer
; approach that follows this.  Needs a temporary dir c:\\tem
;      (when save
;	(write-file dog-list (format nil "~a.tmp" file-name) dio-parms)
;	(delete-file (format nil "~a.dat" file-name))
;	(setq file1 (format nil "~a.tmp" file-name)
;	      file2 (format nil "~a.dat" file-name))
;	  (my-rename-file  file1 file2))
      (when save
	(write-file dog-list (format nil "c:\\tem\\foo.tmp")  dio-parms)
	(sys:dos (format nil
			 "c:\\nfs\\dos2unix c:\\tem\\foo.tmp c:\\tem\\bar.tmp"
			 ))
	(delete-file (format nil "~a.dat" file-name))
	(sys:dos (format nil "copy c:\\tem\\bar.tmp ~a.dat" file-name))
	(sys:8087-fpp :automatic))
      (cd cd)
      (dolist (td dog-list)
	(unless (day-in-index (car td)) (update-cat-index (car td) 'add save)))
      ))
  )

#-gclisp
;;(DEFUN Delete-Date (tdate)
(defun erase-file (tdate)
  (if (stringp tdate) (setq tdate (read-from-string tdate)))
  (setq tdate (getnumdate-long tdate))
  (let (file-name dog-list temp dio-parms yymm month-list cd)
    
    (setq file-name (format nil "d~A" (truncate tdate 100)))
    ;;;
    ;;;  Update *cat-list* in memory
    ;;;
    (setq cd (directory-namestring *default-pathname-defaults*)
	  yymm (truncate tdate 100))
  ;  (cd *data-read-dir*)
    
    (let ((*cat-list* `((,*data-name* . nil))))
      (setq dio-parms (concat-read yymm))
      (setq month-list (assoc yymm 
			      (cdr (assoc *data-name* *cat-list* :test 'eq))
			      :test '=)
	    temp (assoc tdate (cdr month-list)))
      )
    (when temp
      (dolist (parm (cdr (assoc 'sdata (cdr (assoc *data-name* *indx-cfg* :test 'eq))
				:test 'eq)))
	(pushnew parm dio-parms))
      (setf (cdr month-list) (remove temp (cdr month-list)))
      ;;; If Month still present write file else delete old file
      (cond ((cdr month-list)
	     (setq dog-list (coerce (cdr month-list) 'vector))
	     (qsort dog-list #'<  #'car)
	     (setq dog-list (coerce dog-list 'list))
	     ;;(write-file dog-list (format nil "~a.tmp" file-name)  dio-parms)  ;; doesn't work in allegro
	     (write-file dog-list (format nil "~a/~a.tmp" (ewaves-database-directory-string) file-name) dio-parms)
	     ;;(delete-file (format nil "~a.dat" file-name)) ;; doesn't work in allegro
	     (if (probe-file (format nil "~a/~a.dat"  (ewaves-database-directory-string) file-name))
		 (delete-file (format nil "~a/~a.dat" (ewaves-database-directory-string) file-name)))
	     ;;(let ((file1 (format nil "~a.tmp" file-name))
	     ;;	   (file2 (format nil "~a.dat" file-name)))
	     (let ((file1 (format nil "~a/~a.tmp" (ewaves-database-directory-string) file-name))
		   (file2 (format nil "~a/~a.dat" (ewaves-database-directory-string) file-name)))
	       (my-rename-file  file1 file2)))
	    (t
	     ;;(delete-file  (format nil "~a.dat" file-name))
	     (delete-file (format nil "~a/~a.dat" (ewaves-database-directory-string) file-name)))
	    )
      (update-cat-index tdate 'delete t)
      )
 ;   (cd cd)
  ))

#+gclisp
;;(DEFUN Delete-Date (tdate)
(defun erase-file (tdate)
  (if (stringp tdate) (setq tdate (read-from-string tdate)))
  (setq tdate (getnumdate-long tdate))
  (let (file-name dog-list temp dio-parms yymm month-list cd)
    
    (setq file-name
	  (format nil "d~A" (truncate tdate 100)))
    ;;;
    ;;;  Update *cat-list* in memory
    ;;;
    (setq cd (directory-namestring *default-pathname-defaults*)
	  yymm (truncate tdate 100))
    (cd *data-read-dir*)
    
    (let ((*cat-list* `((,*data-name* . nil))))
      (setq dio-parms (concat-read yymm))
      (setq month-list (assoc yymm 
			      (cdr (assoc *data-name* *cat-list* :test 'eq))
			      :test '=)
	    temp (assoc tdate (cdr month-list)))
      )
    (when temp
      (dolist (parm (cdr (assoc 'sdata
				(cdr (assoc *data-name* *indx-cfg* :test 'eq))
				:test 'eq)))
	(pushnew parm dio-parms))
      (setf (cdr month-list)
	    (remove temp (cdr month-list)))
      ;;; If Month still present write file else delete old file
      (cond ((cdr month-list)
	     (setq dog-list (coerce (cdr month-list) 'vector))
	     (qsort dog-list #'<  #'car)
	     (setq dog-list (coerce dog-list 'list))

;            ;; Not working, leaves a tmp file in database, not doing dos2unix
;            (write-file dog-list (format nil "~a.tmp" file-name)
;                        dio-parms)
;            (delete-file (format nil "~a.dat" file-name))
;            (let ((file1 (format nil "~a.tmp" file-name))
;                  (file2 (format nil "~a.dat" file-name)))
;              (my-rename-file  file1 file2))

             ;; Lifted from write file, to update data files since the prev
             ;; was is not working, (now commented out aove)
             (write-file dog-list (format nil "c:\\tem\\foo.tmp")  dio-parms)
             (sys:dos
               (format nil
                       "c:\\nfs\\dos2unix c:\\tem\\foo.tmp c:\\tem\\bar.tmp" ))
             (delete-file (format nil "~a.dat" file-name))
             (sys:dos (format nil "copy c:\\tem\\bar.tmp ~a.dat" file-name))
             (sys:8087-fpp :automatic)
             )

	    (t
	     (delete-file  (format nil "~a.dat" file-name))
	     
	     ))
      (update-cat-index tdate 'delete t)
      )
    (cd cd)
  ))


;(DEFUN UPDATE-DINDEX (YYYYMM)
;    ;;;  Shadow *cat-list*
;  (let ((*cat-list* (list (list *data-name*))) cd dog-list)
;    (setq cd (directory-namestring cd))
;    (cd *data-read-dir*)
;    ;;; Get all data in file containing tdate
;    (concat-read yyyymm)
;    ;;; Udate the alist for the month
;    (setf dog-list (cadr (assoc *data-name* *cat-list* :test 'eq)))
;    (setq dog-list (cdr dog-list))
;    (cd cd)
;    (dolist (day (month-days yyyymm))
;      (update-cat-index day 'del nil))
;    (dolist (td dog-list)
;      (unless (day-in-index (car td)) (update-cat-index (car td) 'add nil)))
;    (update-cat-index (caar dog-list) 'add t))
;  )

;;;;
;;;;these functions used update index file when transfering data from floppy
(defun update-data-index (files)
  (let (lday)
  (dolist (file files)
    ;;; clear out out dates
    (setq lday (subseq (format nil "~a" file) 1 7))
    (setq lday (read-from-string lday))
    (dolist (day (month-days lday))
      (update-cat-index day 'del nil))
    ;;; add in new files
    (with-open-file (strm (format nil "~(~a~a~)" *data-read-dir* file))
      (read strm) ;;; read index-idx-parms & ignore
      (do ((day (read strm nil nil) (read strm nil nil)))
	  ((eql day nil))

	  (when (> (car day) 100)
	    (update-cat-index (car day) 'add nil)
	    (setq lday (car day))
	    )
	  ))
    (update-cat-index lday 'add t)
    )))

(defun next-month (yr-month)
  (declare (integer yr-month))
  (multiple-value-bind (yr mo) (truncate yr-month 100)
    (if (<= mo 11)(1+ yr-month)
      (+ (* 100 (1+ yr)) 1))))
(defun prev-month (yr-month)
  (multiple-value-bind (yr mo) (truncate yr-month 100)
    (if (>= mo 2)(1- yr-month)
      (+ (* 100 (1- yr)) 12))))

;;;the purpose of this function is to multiply all
;;;the open high low close and rollover prices by a 
;;; factor for all availabe data for a market
(defun market-price-adjustment (market factor)
   (let ( data month month-path month-last)
  (set-market market)
  (setq month (get-first-index-date) month-last (get-latest-index-date))
  (loop
  
   (setq month-path (string-append "/home/mk-data/"
                                  (format nil "~(~A~)/" market)
                                  (format nil "d~A.dat" month)))
   (with-open-file (strm month-path :direction :input)
            (do ((record (read strm nil 'eof) (read strm nil 'eof)))
                ((eql record 'eof)) 
                 (push record data))
                 );;closes the with-open-file  
                 
   (if (probe-file month-path)(delete-file month-path))              
;;;open is 16
;;;high is 17
;;;low  is  18
;;;close is 11  
;;;rollover is 19

     (dolist (record data)
       (when (and (numberp (car record))(listp (cdr record)))
        (if (cdr (assoc 16 (cdr record)))(setf (cdr (assoc 16 (cdr record))) 
                                         (* factor (cdr (assoc 16 (cdr record))))))
        (if (cdr (assoc 17 (cdr record)))(setf (cdr (assoc 17 (cdr record))) 
                                         (* factor (cdr (assoc 17 (cdr record)))))) 
        (if (cdr (assoc 18 (cdr record)))(setf (cdr (assoc 18 (cdr record))) 
                                         (* factor (cdr (assoc 18 (cdr record))))))
        (if (cdr (assoc 19 (cdr record)))(setf (cdr (assoc 19 (cdr record))) 
                                         (* factor (cdr (assoc 19 (cdr record)))))) 
        (if (assoc 11 (cdr record))(setf (cadr (assoc 11 (cdr record))) 
                                   (* factor (cadr (assoc 11 (cdr record))))))
         ) );;closes the dolist
          
    (setq data (reverse data))
    (with-open-file (strm month-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (record data)
        (format strm "~A~%" record)))
    (setq month (next-month month) data nil)
    (if (> month month-last)(return))
     );closes the loop

));;closes the let and the defun




(defmacro getv (wave key &optional (ct-symb nil))
  (ignore-errors (setq key (eval key)))
  (case key
    ((pw 2) `(wave-pw ,wave))
    ((ww 3) `(wave-ww ,wave))
    ((lb 4) `(wave-lb ,wave))
    ((dg 5) `(wave-dg ,wave))
    ((sb 6) `(wave-sb ,wave))
    ((c1 7) `(wave-c1 ,wave ,ct-symb))
    ((c2 8) `(wave-c2 ,wave ,ct-symb))
    ((nw 9) `(wave-nw ,wave ,ct-symb))
    ((rl 10) `(wave-rl ,wave ,ct-symb))
    ((st 11) `(wave-st ,wave))
    ((et 12) `(wave-et ,wave))
    ((tl 13) `(wave-tl ,wave))
    ((sp 14) `(wave-sp ,wave))
    ((ep 15) `(wave-ep ,wave))
    ((hp 16) `(wave-hp ,wave))
    ((lp 17) `(wave-lp ,wave))
    ((dr 18) `(wave-dr ,wave))
    ((vp 19) `(wave-vp ,wave))
    ((va 20) `(wave-va ,wave))
    ((as 21) `(wave-as ,wave))
    ((ap 22) `(wave-ap ,wave))
    ((vd 23) `(wave-vd ,wave))
    ((fl 24) `(wave-fl ,wave))
    ((ft 25) `(wave-ft ,wave))
    (t
     ;(print key)
     ;(Print `(getvold ,wave ,key ,ct-symb))
     `(getvold ,wave ,key ,ct-symb)))
  )



(defun reset-index-cfg ()

  (let (indexidxfile path patht  date days yeardata)

    (setq days 0)
;    (left-status t  "Resetting Data INDEX")

    (if (probe-file (format nil "~a~(~a~)/bar.idx" *database-upper-dir* *data-name*))
	(delete-file (format nil "~a~(~a~)/bar.idx" *database-upper-dir* *data-name*)))

    (dolist (file (directory (format nil "~a*.dat" *data-read-dir*)))
      (setq date (pathname-name file))
      (setq date (read-from-string date nil nil :start 1))

      (with-open-file (strm file)
	(setq days 0)
	(read strm) ;;; read index-idx-parms & ignore
	(do ((day (read strm nil nil) (read strm nil nil)))
	    ((eql day nil))
	  (when (> (car day) 100)
	    (setq day (mod (car day) 100))
	    (setf days (logior days (expt 2 day)))))

	(multiple-value-bind (cyr cmo) (truncate date 100)
	  (setq yeardata (cdr (assoc cyr indexidxfile)))
	  (unless yeardata 
	    (setq yeardata (make-array 12 :initial-element 0))
	    (push (cons cyr yeardata) indexidxfile)
	    )
	  (setf (aref yeardata (1- cmo)) days))
	) ;; with-open..
      ) ;; dolist

    ;; Keep sorted
    (setq indexidxfile (reverse indexidxfile))
    (setq indexidxfile (sort indexidxfile #'< :key #'car))
    ;;Rewrite the new version
    (setq path (format nil "~a~(~a~)/index.idx" *database-upper-dir* *data-name*))
    (setq patht (format nil "~a~(~a~)/indxt.idx" *database-upper-dir* *data-name*))
    (with-open-file (out-strm patht :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (linej indexidxfile)
	(format out-strm "(~s . #(" (pop linej))
	(dotimes (i 12) (format out-strm "#x~8,'0x " (aref linej i)))
	(format out-strm "))~%"))
      )


    ;;:(cd *database-upper-dir*)

    ;; Rename temp file to permanent file
    (if (probe-file path) (delete-file path))
    (rename-file patht path)
 ;   (user::shell (format nil "chmod g+w ~a" path))

    (setq *cat-list* nil)
    (dolist (nm *all-markets*)
      (push (cons (car nm) nil) *cat-list*))


  ;  (notify1 "Index has been Reset")
  ;  (left-status nil  "Resetting Data INDEX")
    nil))

;;;resets all indexes in the list of markets
(defun reset-all-indexes ( markets)

  (dolist (ith markets)
     (set-market ith)
     (reset-index-cfg))
     
  )
;;;;removes a day of data from the database
(defun delete-date (tdate)
   (let (dmy (path-in (format nil "~a~a~a~a" *data-read-dir* "d" yyyy-mm ".dat"))
         prev-path-in next-path-in dmy1 dmy2)


        (with-open-file (str path-in :direction :input)
              (do ((record (read str nil 'eof) (read str nil 'eof)))
                  ((eql record 'eof))
                   (push record dmy)))     

        (setq prev-path-in (format nil "~a~a~a~a" *data-read-dir* "d" (prev-month yyyy-mm) ".dat"))


        (with-open-file (str prev-path-in :direction :input)
              (do ((record (read str nil 'eof) (read str nil 'eof)))
                  ((eql record 'eof))
                   (push record dmy1)))     


         (with-open-file (str next-path-in :direction :input)
              (do ((record (read str nil 'eof) (read str nil 'eof)))
                  ((eql record 'eof))
                   (push record dmy2)))     


;;;first need to update the next day and previous day pointers
;;;;when removing a date
      (setq ydate-pointer (cdr (assoc 0 (assoc tdate dmy)))
            ndate-pointer (cdr (assoc 1 (assoc tdate dmy))))

 (setq filename (format nil "~a~a~a~a" *data-read-dir* "d" yyyy-mm ".dat"))

        (setq dmy (remove-if #'(lambda (s) (= (car s) tdate)) dmy))  
))


;;;
;;;**************************************************************************
;;;				END OF FILE - DIO2
;;;**************************************************************************



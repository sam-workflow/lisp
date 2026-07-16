;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro test-variables (&rest args)
  (let ((assertions nil))
    (dolist (arg args (append `(or ,@(nreverse assertions))))
      (let* ((name (first arg))
             (type (second arg))
             (test (case type
                     ((:probability :prob)
                      `(and (numberp ,name) (not (minusp ,name)) (<= ,name 1)))
                     ((:positive-integer :posint)
                      `(and (integerp ,name) (plusp ,name)))
                     ((:positive-number :posnum)
                      `(and (numberp ,name) (plusp ,name)))
                     ((:number-sequence :numseq)
                      `(and (typep ,name 'sequence) (every #'numberp ,name)
                            (not (null ,name))))
                     ((:nonzero-number-sequence :nonzero-numseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (numberp x) (not (= 0 x))))
                         ,name)))
                     ((:probability-sequence :probseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (numberp x) (not (minusp x))
                                                  (<= x 1.0))) ,name)))
                     ((:positive-integer-sequence :posintseq)
                      `(and (typep ,name 'sequence) (not (null ,name))
                        (every #'(lambda (x) (and (typep x 'integer) (plusp
                                                                      x)))
                         ,name)))
                     (:percentage
                      `(and (numberp ,name) (plusp ,name) (<= ,name 100)))
                     (:test (third arg))
                     (t `(typep ,name ',type))))
             (message `(error
                        ,(if (eql type :test)
                             name
                             (format nil "~a = ~~a is not a ~a" name
                                     (case type
                                       ((:positive-integer :posint)
                                        "positive integer")
                                       ((:positive-number :posnum)
                                        "positive number")
                                       ((:probability :prob) "probability")
                                       ((:number-sequence :numseq)
                                        "sequence of numbers")
                                       ((:nonzero-number-sequence
                                         :nonzero-numseq)
                                        "sequence of non-zero numbers")
                                       ((:positive-integer-sequence :posintseq)
                                        "sequence of positive integers")
                                       ((:probability-sequence :probseq)
                                        "sequence of probabilities")
                                       ((:percent :percentile) "percent")
                                       (t type))))
                        ,@(unless (eql type :test) `(,name)))))
        (push `(unless ,test ,message) assertions)))))

;; SQUARE

(defmacro square (x)
  `(* ,x ,x))


(defmacro underflow-goes-to-zero (&body body)
  "Protects against floating point underflow errors and sets the value to 0.0 instead."
  `(handler-case 
       (progn ,@body)
       (floating-point-underflow (condition)
	(declare (ignore condition))
	(values 0.0d0))))


) ;end eval-when


;; CHI-SQUARE
;; Rosner 187
;; Returns the point which is the indicated percentile in the Chi Square
;; distribution with dof degrees of freedom.

(defun chi-square (dof percentile)
  (test-variables (dof :posint) (percentile :prob))
  (find-critical-value #'(lambda (x) (chi-square-cdf x dof))
                       (- 1 percentile)))

;; Chi-square-cdf computes the left hand tail area under the chi square
;; distribution under dof degrees of freedom up to X. 

(defun chi-square-cdf (x dof)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (setq x (coerce x 'double-float))
  (test-variables (x :posnum) (dof :posint))
  (multiple-value-bind (cdf ignore)
      (gamma-incomplete (* 0.5 dof) (* 0.5 x))
    (declare (ignore ignore))
    cdf))
   

(defun gamma-incomplete (a x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (declare (optimize (safety 3)))
  (setq a (coerce a 'double-float))
  (let ((gln (the double-float (gamma-ln a))))
    (when (= x 0.0)
      (return-from gamma-incomplete (values 0.0d0 gln)))
    (if (< x (+ a 1.0d0))
        ;; Use series representation.  The following is the code of what
        ;; Numerical Recipes in C calls ``GSER'
        (let* ((itmax 1000)
               (eps   3.0d-7)
               (ap1    a)
               (sum   (/ 1d0 a))
               (del sum))
          (declare (type double-float ap1 sum del)
		   (type fixnum itmax))
          (dotimes (i itmax)
            (incf ap1 1.0d0)
            (setf del (* del (/ x ap1)))
            (incf sum del)
            (if (< (abs del) (* eps (abs sum)))
                (let ((result (underflow-goes-to-zero
                               (* sum (safe-exp (- (* a (log x)) x gln))))))
                  (return-from gamma-incomplete (values result gln)))))
          (error "Series didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a itmax))
        ;; Use the continued fraction representation.  The following is the
        ;; code of what Numerical Recipes in C calls ``GCF.'' Their code
        ;; computes the complement of the desired result, so we subtract from
        ;; 1.0 at the end.
        (let ((itmax 1000)
              (eps   3.0e-7)
              (gold 0d0) (g 0d0) (fac 1d0) (b1 1d0) (b0 0d0)
              (anf 0d0) (ana 0d0) (an 0d0) (a1 x) (a0 1d0))
          (declare (type double-float gold g fac b1 b0 anf ana an a1 a0))
          (dotimes (i itmax)
            (setf an  (coerce (1+ i) 'double-float)
                  ana (- an a)
                  a0  (* fac (+ a1 (* a0 ana)))
                  b0  (* fac (+ b1 (* b0 ana)))
                  anf (* fac an)
                  a1  (+ (* x a0) (* anf a1))
                  b1  (+ (* x b0) (* anf b1)))
            (unless (zerop a1)
              (setf fac (/ 1.0d0 a1)
                    g   (* b1 fac))
              (if (< (abs (/ (- g gold) g)) eps)
                  (let ((result (underflow-goes-to-zero
                                 (* (safe-exp (- (* a (log x)) x gln)) g))))
                    (return-from
                     gamma-incomplete (values (- 1.0d0 result) gln)))
                  (setf gold g))))
          (error "Continued Fraction didn't converge:~%~
                  Either a=~s is too large, or ITMAX=~d is too small." a
                  itmax)))))


(defun gamma-ln (x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (cond ((<= x 0) (error "arg to gamma-ln must be positive:  ~s" x))
	((> x 1.0d302)
	 (error "Argument too large:  ~e" x))
	((= x 0.5d0)
	 ;; special case for this arg, since it is used by the error-function
	 (log (sqrt pi)))
	((< x 1)
	 ;; Use reflection formula:  Gamma(1-z) = z*pi/(Gamma(1+z)sin(pi*z))
	 (let ((z (- 1.0d0 x)))
	   (- (+ (log z) (log pi)) (+ (gamma-ln (+ 1.0 z)) (log (sin (* pi z)))))))
	(t (let* ((xx  (- x 1.0d0))
		  (tmp (+ xx 5.5d0))
		  (ser 1.0d0))
	     (declare (type double-float xx tmp ser))
	     (decf tmp (* (+ xx 0.5d0) (log tmp)))
	     (dolist (coef '(76.18009173d0 -86.50532033d0 24.01409822d0
					   -1.231739516d0 0.120858003d-2 -0.536382d-5))
	       (declare (type double-float coef))
	       (incf xx 1.0d0)
	       (incf ser (/ coef xx)))
	     (- (log (* 2.50662827465d0 ser)) tmp)))))


(defun find-critical-value
       (p-function p-value &optional (x-tolerance .00001) (y-tolerance .00001))
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (let* ((x-low 0d0)
	 (fx-low 1d0)
	 (x-high 1d0)
	 (fx-high (coerce (funcall p-function x-high) 'double-float)))
    ;; double up
    (declare (type double-float x-low fx-low x-high fx-high))
    (do () (nil)
      ;; for general functions, we'd have to try the other way of bracketing,
      ;; and probably have another way to terminate if, say, y is not in the
      ;; range of f.
      (when (>= fx-low p-value fx-high)
	(return))
      (setf x-low x-high
	    fx-low fx-high
	    x-high (* 2.0 x-high)
	    fx-high (funcall p-function x-high)))
    ;; binary search
    (do () (nil)
      (let* ((x-mid  (/ (+ x-low x-high) 2.0))
	     (fx-mid (funcall p-function x-mid))
	     (y-diff (abs (- fx-mid p-value)))
	     (x-diff (- x-high x-low)))
	(when (or (< x-diff x-tolerance)
		  (< y-diff y-tolerance))
	  (return-from find-critical-value x-mid))
	;; Because significance is monotonically decreasing with x, if the
	;; function is above the desired p-value...
	(if (< p-value fx-mid)
	    ;; then the critical x is in the upper half
	    (setf x-low x-mid
		  fx-low fx-mid)
	    ;; otherwise, it's in the lower half
	    (setf x-high x-mid
		  fx-high fx-mid))))))


(defun safe-exp (x)
  "Eliminates floating point underflow for the exponential function.
Instead, it just returns 0.0d0"
  (setf x (coerce x 'double-float))
  (if (< x (log least-positive-double-float))
      0.0d0
      (exp x)))



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


;;;;;;;;; LENEAR REGRESSION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Correlation and Regression
;;;

;; LINEAR-REGRESSION
;; Rosner 431, 441 for t-test

;; Computes the regression equation for a least squares fit of a line to a
;; sequence of points (each a list of two numbers, e.g. '((1.0 0.1) (2.0 0.2)))
;; and report the intercept, slope, correlation coefficient r, R^2, and the
;; significance of the difference of the slope from 0. 

(defun linear-regression (points)
  (test-variables (points sequence))
  (let  ((xs (map 'list #'first points))
         (ys (map 'list #'second points)))
    (test-variables (xs :numseq) (ys :numseq))
   ; (print ys)(print xs)
    (let* ((x-bar (mean xs))
           (y-bar (mean ys))
           (n (length points))
           (Lxx (apply #'+ (mapcar (lambda (xi) (square (- xi x-bar))) xs)))
           (Lyy (apply #'+ (mapcar (lambda (yi) (square (- yi y-bar))) ys)))
           (Lxy (apply #'+ (mapcar (lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
                                    xs ys)))
           
           (b (/ Lxy Lxx))
           (a (- y-bar (* b x-bar)))
           (reg-ss (* b Lxy))
          
           (res-ms (/ (- Lyy reg-ss) (- n 2)))
           (r (/ Lxy (sqrt (* Lxx Lyy))))
           (r2 (/ reg-ss Lyy))
          ; (t-test (/ b (sqrt (/ res-ms Lxx)))) 
           t-test t-significance
            );;;closes the let*
          ; (format t "~%~%lyy = ~A Lxx =~A reg-ss = ~A~%~%" lyy lxx reg-ss)
           ;(format t "~%b = ~A Lxx =~A res-ms = ~A~%" b lxx res-ms)
        (if (minusp res-ms)(setq res-ms 0));;;corrects ofr floating point errors
        (ifn (zerop res-ms )
           (setq t-test (/ b (sqrt (/ res-ms Lxx)))
                 t-significance (t-significance t-test (- n 2) :tails :both)))

  ;    (format t "~%Intercept = ~f, slope = ~f, r = ~f, R^2 = ~f, p = ~f"
  ;            a b r r2 t-significance)
      (values a b r r2 (sqrt res-ms) t-significance)))) 


(defun mean (sequence)
  (test-variables (sequence :numseq))
  (/ (reduce #'+ sequence) (length sequence)))


;; T-SIGNIFICANCE
;;  Lookup table in Rosner; this is adopted from CLASP/Numeric Recipes

(defun t-significance (t-statistic dof &key (tails :both))
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
  (test-variables (t-statistic number) (dof :posint))
  (setf dof (float dof t-statistic))
  (let ((a (beta-incomplete (* 0.5 dof) 0.5 (/ dof (+ dof (square t-statistic))))))
    ;; A is 2*Integral from (abs t-statistic) to Infinity of t-distribution
    (ecase tails
      (:both a)
      (:positive (if (plusp t-statistic)
		     (* .5 a)
		     (- 1.0 (* .5 a))))
      (:negative (if (plusp t-statistic)
		     (- 1.0 (* .5 a))
		     (* .5 a))))))


(defun beta-incomplete (a b x)
  "Adopted from CLASP 1.4.3, http://eksl-www.cs.umass.edu/clasp.html"
   (flet ((betacf (a b x)
	    ;; straight from Numerical Recipes in C, section 6.3
	    (declare (type double-float a b x))
	    (let ((ITMAX 1000)
		  (EPS   3.0d-7)
		  (qap 0d0) (qam 0d0) (qab 0d0) (em  0d0) (tem 0d0) (d 0d0)
		  (bz  0d0) (bm  1d0) (bp  0d0) (bpp 0d0)
		  (az  1d0) (am  1d0) (ap1  0d0) (app 0d0) (aold 0d0))
	      (declare (type double-float qap qam qab tem d
			     bz bm bp bpp az am ap1 app aold em))
	      (setf qab (+ a b)
		    qap (+ a 1d0)
		    qam (- a 1d0)
		    bz  (- 1d0 (/ (* qab x) qap)))
	      (dotimes (m ITMAX)
		(setf em   (coerce (float (1+ m)) 'double-float))
                (setf
		      tem  (+ em em)
		      d    (/ (* em (- b em) x)
			      (* (+ qam tem) (+ a tem)))
		      ap1   (+ az (* d am))
		      bp   (+ bz (* d bm))
		      d    (/ (* (- (+ a em)) (+ qab em) x)
			      (* (+ qap tem) (+ a tem)))
		      app  (+ ap1 (* d az))
		      bpp  (+ bp (* d bz))
		      aold az
		      am   (/ ap1 bpp)
		      bm   (/ bp bpp)
		      az   (/ app bpp)
		      bz   1d0)
		(if (< (abs (- az aold)) (* EPS (abs az)))
		    (return-from betacf az)))
	      (error "a=~s or b=~s too big, or ITMAX too small in BETACF"
		     a b))))
      (declare (notinline betacf))
      (setq a (coerce a 'double-float) b (coerce b 'double-float)
            x (coerce x 'double-float))
      (when (or (< x 0d0) (> x 1d0))
	 (error "x must be between 0d0 and 1d0:  ~f" x))
      ;; bt is the factors in front of the continued fraction
      (let ((bt (if (or (= x 0d0) (= x 1d0))	    
		    0d0
		    (exp (+ (gamma-ln (+ a b))
			    (- (gamma-ln a))
			    (- (gamma-ln b))
			    (* a (log x))
			    (* b (log (- 1d0 x))))))))
	 (if (< x (/ (+ a 1d0) (+ a b 2.0)))
	     ;; use continued fraction directly
	     (/ (* bt (betacf a b x)) a) 
	     ;; use continued fraction after making the symmetry transformation
	     (- 1d0 (/ (* bt (betacf b a (- 1d0 x))) b))))))


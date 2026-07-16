;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;;bearish patterns 
;;;DNDJ DNHM DNEP DNCC DNST DNHR

;;;;bullish patterns
;;;;UPDJ UPHM UPEP UPPC UPIH UPHR
;;;;
(defun candle-composite (tdate &optional (period 1))
  (let  ((date tdate) can  (dur 0))
 ; (setq extd1 (n-day-extreme-dates tdate period))
 ; (setq deltat (1+ (sub-mkt-dates extd1 tdate)))
   (dotimes (ith period)
    (setq can (candle-swing date))
    (cond ((and (< date tdate)(= can -1)
                (> (getd tdate 'high)(n-day-high (getd tdate 'ydate) period)))
           (setq can 0))
          ((and (< date tdate)(= can 1)
                (< (getd tdate 'low)(n-day-low (getd tdate 'ydate) period)))
           (setq can 0)))
    (incf dur)
    (if (/= can 0)
           (return can)(setq date (getd date 'ydate))) 
    ) 
   (values can dur)
))

(defun candlefx-index0 (date)
 (let ((can (candle1 date))
       (bull '(DNMB DNHR DNTW UPMB DNHM DNDJ UPHM UPDJ DNST UPEP));; for 2x
       (bear '(UPMS DNCC UPPC DNEP UPHR DNST IDST  UPEP UPIH IDHW));; for 2x
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))


(defun candlefx-index1 (date)
 (let ((can (candle1 date))
       (bull '(UPMB UPHM DNEP DNDJ DNTW DNST DNHM IDST));; for 2x
       (bear '(UPMS DNMB DNEP DNHR UPTW UPHM IDHW DNDJ IDST DNST UPHR DNTW));; for 2x
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))


(defun candlefx-index2 (date)
 (let ((can (candle1 date))
       (bull '(DNST UPHM IDST UPEP UPDJ UPMB DNTW UPIH DNHM ));; for 2x
       (bear '(UPMS UPPC DNCC UPHR UPMB DNHM NIL UPIH IDHW UPEP));; for 2x
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))



(defun candle-index (date)
 (let ( (can (candle1 date))
       ; (bull '(UPDJ UPPC UPTW IDHW IDST UPHM ));; for 2c
         (bull '(DNES IDHW UPEP UPPC DNHM UPTW IDST UPIH))
     
      ;  (bear '(UPMS UPMB DNHM UPDJ IDST UPTW ));; for 2c
        (bear '(DNES UPMS DNHM DNHR DNEP IDST UPEP))
      
       )

   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
          )
))
;;;;
(defun candle-index-1 (date)
 (let ( (can-1 (candle1 (getd date 'ydate)))
          
       ; (bull-1 '(DNMB UPPC DNEP UPIH UPTW UPHM IDST DNHM));; for 2c
         (bull '(UPMB DNCC DNHR DNEP UPEP DNST DNES UPMS IDST DNTW UPHM))
     
       ; (bear-1 '(UPMS DNES DNCC UPHR DNST DNMB IDST DNEP UPTW UPMB DNHM IDHW DNTW UPPC )) for 2c
         (bear '(DNMB DNES DNCC UPMS UPHR DNEP UPDJ IDST UPIH UPEP IDHW))
       )

   (cond ((and (member can-1 bull)(member can-1 bear)) 2)
         ((and (member can-1 bull)(not (member can-1 bear))) 1)
         ((and (not (member can-1 bull))(member can-1 bear)) -1)
         ((and (not (member can-1 bull))(not (member can-1 bear))) 0) 
          )

))

;;;the date entered is current day days is the number of days to look back for a candle signal
(defun candle (tdate &optional (days 10))
 (let ((date tdate) can dirp cntr)
  (setq cntr         
  (dotimes (ith days)
    (if (setq can (candle1 date)) (return ith)
        (setq date (getd date 'ydate)))
    ))

 ; (format t "~%CAN = ~A CNTR = ~A"  can cntr)
  ;;;check validity
  (if (and  cntr (> (- (getd (add-mkt-days tdate (- cntr)) 'close) (ave (add-mkt-days tdate (- cntr)) days 'close)) 0))
      (setq dirp 'UP)(setq dirp 'DN))
  ;(format t " ~A" dirp)
  
  (cond ((and cntr (eql dirp 'up) (> (n-day-high tdate (1+ cntr)) (getd (add-mkt-days tdate (- cntr)) 'high)))
        (setq can 0))
	((and cntr (eql dirp 'DN) (< (n-day-low tdate (1+ cntr)) (getd (add-mkt-days tdate (- cntr)) 'low)))
	 (setq can 0))
	(t can))
  
  (values  can dirp cntr (and cntr (add-mkt-days tdate (- cntr))))
))

;;;traditional interpretation
(defun candle-swing (date)

 (let ((can (candle1 date))
          
        (bull '(UPDJ UPHM UPEP UPPC UPIH UPMS UPHR UPTW UPMB UIST UIHW DIST DIHW )) 

        (bear '(DNDJ DNHM DNEP DNCC DNST DNES DNHR DNTW DNMB DIST DIHW UIST UIHW)) 

        )
   (cond ((and (member can bull)(member can bear)) 2)
         ((and (member can bull)(not (member can bear))) 1)
         ((and (not (member can bull))(member can bear)) -1)
         ((and (not (member can bull))(not (member can bear))) 0) 
      )
))
;;;returns the symbol name for the candle
(defun candle1 (date )
 (let* ((topen (getd date 'open))(tclose (getd date 'close))(lvlh 50)(lvll -50)
       (thigh (getd date 'high))(tlow (getd date 'low))
       (date-1 (getd date 'ydate))(date-2 (getd date-1 'ydate))
       (yopen (getd date-1 'open))(yclose (getd date-1 'close))
       (yhigh (getd date-1 'high))(ylow (getd date-1 'low))(cci21 (commodity-channel-index date 21))
       (yrel-range (/ (volatility date-1 1 1)(volatility date-1 5 1)))
       (trel-range (if (zerop (volatility date 5 1)) 1.0 (/ (volatility date 1 1)(volatility date 5 1))))
       (trel-range3 (if (zerop (volatility date 3 1)) 1.0 (/ (volatility date 1 1)(volatility date 3 1))))
       (trel-bodyrange (if (zerop (true-range date)) 1.0 (abs (/ (body-range date)(true-range date)))))
 
       (dbyrel-range (/ (volatility date-2 1 1)(volatility date-2 5 1)))
       (dbyopen (getd date-2 'open))(dbyclose (getd date-2 'close))
       (dbyhigh (getd date-2 'high))(dbylow (getd date-2 'low))
      ; (cci (commodity-channel-index date 21))
      ; (ccid (roc date 3 'cci 21))
       cci5h3 cci5l3 ;(cci5 (commodity-channel-index date 5))
      ; (vi (volume-index date 1 5));;;current day volume to 5-day average
      ; (4-day-low (n-day-low date size 'close))(4-day-high (n-day-high date size 'close))
       )
      (multiple-value-setq (cci5h3 cci5l3)(cci-high-low date 5 2))      
              
      (cond ((= thigh tlow) 'DOJI)
	((and  (n-day-highp date 10)
               (>= cci21 lvlh)
               ; (> ccid 0)
                (or (<= (abs (- tclose topen))(* 3.1 (index-tick-size)))
                   (<= (if (zerop (- thigh tlow)) 1.0 (/ (abs (- tclose topen))(- thigh tlow))) .05))
               ) 'DNDJ) ;doji top
               
         ((and (n-day-highp date 10)
               (>= cci21 lvlh)
               ;(> ccid 0)
               (<= (- thigh (max topen tclose)) (* 0.25 (abs (- thigh tlow)))) ;small upper shadow less than fifth of the range
               (>= (- (min topen tclose) tlow) (* 2 (abs (- topen tclose)))) ;;;lower shadow more than twice body      
               ) 'DNHM) ;Hangman (may be white or black)
         
         ((and (or (n-day-highp date-1 10)(n-day-highp date 10))
               (>= cci21 lvlh)
               (> trel-range 1.000) ;;;larger than normal candle
               (< tclose topen) ;;;black candle
               (>= yclose yopen) ;;;previous white candle
               (>= topen (- yclose (index-tick-size)));;
               (<= tclose (+ yopen (index-tick-size)));;engulfing pattern
               ) 'DNEP) ;bearish engulfing pattern
                
         ((and (n-day-highp date 10)
               (>= cci21 lvlh)
               ;(> ccid 0)
               (> yrel-range 1);;;larger than average candle      
               (> yclose yopen);;previous white candle
               (>= topen (- yhigh (index-tick-size)));;opens higher than previous candles' high
               (< tclose topen);;black candle
               (<= tclose (/ (+ yopen yclose) 2)) ;;must close more than 50% retracement of previous candle's body
               ) 'DNCC) ;; black cloud cover
               
         ((and (n-day-highp date 10)
               (>= cci21 lvlh)
               ;(> ccid 0)
               (> yrel-range .8);;;not a small prior candle
               (> yclose yopen) ;;previous white candle
               (>= topen (- yclose (index-tick-size)));;;gaps open above previous days body
               (>= (- thigh (max topen tclose))(* 2 (abs (- topen tclose))));;upper shadow twice the body
               (<= (- (min topen tclose) tlow)(* .25 (abs (- thigh tlow))));;;lower shadow less than quarter of the range
               ) 'DNST) ;;shooting star
        
         ((and (n-day-highp date-1 10)
               (>= cci21 lvlh)
               ;(> ccid 0)
               (> dbyrel-range 1);;;First candle larger than average candle
               (> dbyclose dbyopen) ;;;first candle is white
               (> yopen  dbyhigh) ;;;second candle gaps open above high of first candle
               (>= (- yhigh (max yopen yclose))(* 1.618 (abs (- yopen yclose))));;upper shadow twice the body
               (<= (- (min yopen yclose) ylow)(* .382 (abs (- yhigh ylow))));;;lower shadow less than quarter of the range
               (< tclose topen);;;third candle is black
               (< thigh yhigh) ;;;third candle is not higher than second candle
               (< tlow (/ (+ dbyopen dbyclose) 2));;;third candle retreats below middle of first candle body
               ) 'DNES) ;;evening star is a three candle pattern
               
         ((and (n-day-highp date-1 10)
               (>= cci21 lvlh)
               ;(> ccid 0)
               (> yrel-range 1) ;;;larger than normal candle
               (> (- yclose yopen)(* 2 (- (- yhigh ylow)(- yclose yopen))));;;body is larger than twice the shadows 
               (< trel-range 1) ;;;smaller than normal candle
               (> yclose yopen) ;;;white candle
               (< tclose topen) ;;;black candle
               (< (max thigh tclose topen)(max yopen yclose yhigh));;;top of todays body and shadow lower than yesterday's top of body
               (> (min tlow tclose topen)(min yopen yclose ylow));;;bottom of todays body and shadow higher than yesterday's bottom of body.
               (<= (abs (- (max topen tclose)(min topen tclose)))
                   (* .5 (abs (- (max yopen yclose)(min yopen yclose)))));;;second candle body is 1/2 smaller than first candle
               (<= tlow (/ (+ yopen yclose) 2));;black candle must decline below 50% of white candle body
               ) 'DNHR) ;;bearish harami
            
         ((and (or (n-day-highp date-1 10)(n-day-highp date 10))
               (>= cci21 lvlh)
               (> trel-bodyrange .4)(> trel-range .8) ;;;not a small body candle
               (< (if (zerop yrel-range) 1.0 (/ trel-range yrel-range)) 1.618)
               (not (zerop (body-range date-1)))(< (/ (abs (body-range date))(abs (body-range date-1))) 2.0)
               (or (and (or (<= (abs (- yhigh thigh)) (* 2.1 (index-tick-size))) ;;;the highs must be essentially the same
                            (<= (abs (- yclose yopen))(* 1.1 (index-tick-size))));;;real body highs the same
                        (> yclose yopen);;;first candle is white
                        (< tclose topen)) ;;;second candle is black
                   (and (<= (abs (- dbyhigh thigh))
                             (* 1.1 (index-tick-size))) ;;;the highs must be essentially the same
                        (> dbyclose dbyopen);;;first candle is white (2 bars back)
                        (< tclose topen) ;;;second candle is black (current candle)
                        (<= yhigh (min thigh dbyhigh))))

                ) 'DNTW) ;;bearish tweezers 
         ((and (n-day-highp date 10)
               (>= cci21 lvlh)
               ;(> ccid 0)
               (> trel-range 1.618) ;;;larger than normal candle
               (> (- topen tclose)(* .95 (- thigh tlow)))
               ) 'DNMB) ;;bearish marubozu      
                  
          ((and (n-day-lowp date 10)
               (<= cci21 lvll)
               ;(< ccid 0)
               (or (<= (abs (- tclose topen))(* 2.1 (index-tick-size)))
                   (<= (if (zerop (- thigh tlow)) 1.0 (/ (abs (- tclose topen)) (- thigh tlow))) .05))
               ) 'UPDJ) ;doji bottom
               
         ((and (n-day-lowp date 10)
               (<= cci21 lvll)
               ;(< ccid 0)
               (<= (- thigh (max topen tclose)) (* 0.25 (abs (- thigh tlow)))) ;small upper shadow less than a fourth of range
               (>= (- (min topen tclose) tlow) (* 2 (abs (- topen tclose)))) ;;;lower shadow more than twice body      
               ) 'UPHM) ;Hammer (may be white or black)
               
         ((and (or (n-day-lowp date-1 10)(n-day-lowp date 10))
               (<= cci21 lvll)
               (> trel-range 1.000) ;;;larger than normal candle
                (> tclose topen) ;;;white candle
               (<= yclose yopen) ;;;previous black candle
               (<= topen (- yclose (index-tick-size)));;
               (>= tclose (+ yopen (index-tick-size)));;engulfing pattern
               ) 'UPEP) ;bullish engulfing pattern 
               
         ((and (n-day-lowp date 10)
               (<= cci21 lvll)
               ; (< ccid 0)
                (> yrel-range 1);;;larger than average candle      
               (< yclose yopen);;previous black candle
               (<= topen (+ ylow (index-tick-size)));;opens lower than previous candles' low
               (> tclose topen);;white candle
               (>= tclose (/ (+ yopen yclose) 2)) ;;must close more than 50% retracement of previous candle's body
               ) 'UPPC) ;;piercing candle 
                     
         ((and (n-day-lowp date 10)
               (<= cci21 lvll)
               ; (< ccid 0)
                (> yrel-range .8);;;larger than average candle
               (< yclose yopen) ;;previous black candle
               (<= topen (+ yclose (index-tick-size)));;;gaps open below previous days body
               (>= (- thigh (max topen tclose))(* 2 (abs (- topen tclose))));;upper shadow twice the body
               (<= (- (min topen tclose) tlow)(* .25 (abs (- thigh tlow))));;;lower shadow less than quarter of the range
               ) 'UPIH) ;;Inverted Hammer
       
         ((and (n-day-lowp date-1 10)
               (<= cci21 lvll)
              ; (< ccid 0)
               (> dbyrel-range 1);;;First candle larger than average candle
               (< dbyclose dbyopen) ;;;first candle is black
               (< yopen  dbylow) ;;;second candle gaps below the low of first candle
               (>= (- yhigh (max yopen yclose))(* 1.618 (abs (- yopen yclose))));;upper shadow twice the body
               (<= (- (min yopen yclose) ylow)(* .382 (abs (- yhigh ylow))));;;lower shadow less than quarter of the range
               (> tclose topen);;;third candle is white
               (> tlow ylow) ;;;third candle is not lower than second candle
               (> thigh (/ (+ dbyopen dbyclose) 2));;;third candle retraces above middle of first candle body
               ) 'UPMS) ;;morning star
        
         ((and (n-day-lowp date-1 10)
               (<= cci21 lvll)
               ;(< ccid 0)
               (> yrel-range 1) ;;;larger than normal candle
               (> (- yopen yclose)(* 2 (- (- yhigh ylow)(- yopen yclose))));;;body is larger than twice the shadows 
               (< trel-range 1) ;;;smaller than normal candle
               (> yopen yclose) ;;;black candle
               (< topen tclose) ;;;white candle
               (<= (max thigh tclose topen)(max yopen yclose yhigh));;;top of todays body and shadow lower than yesterday's top of body
               (>= (min tlow tclose topen)(min yopen yclose ylow));;;bottom of todays body and shadow higher than yesterday's bottom of body.
               (<= (abs (- (max topen tclose)(min topen tclose)))
                   (* .5 (abs (- (max yopen yclose)(min yopen yclose)))));;;second candle body is 1/2 smaller than first candle
               (>= thigh (/ (+ yopen yclose) 2));;white candle must rise above 50% of black candle body
               
               ) 'UPHR) ;;bullish harami 
             
          ((and (or (n-day-lowp date-1 10)(n-day-lowp date 10))
                (<= cci21 lvll)
                (> trel-bodyrange .4)(> trel-range .8) ;;;not a small body candle
                (< (if (zerop yrel-range) 1.0 (/ trel-range yrel-range)) 1.618)
                (not (zerop (body-range date-1)))(< (/ (abs (body-range date))(abs (body-range date-1))) 2.0)
                (or (and (or (<= (abs (- ylow tlow)) (* 2.1 (index-tick-size)));;;the shadow lows must be essentially the same 
                             (<= (abs (- (min yopen yclose)(min topen tclose)))(* 1.1 (index-tick-size))));;;body lows the same
                          (< yclose yopen) ;;;first candle is black
                         (> tclose topen)) ;;;second candle is white
                   (and (<= (abs (- dbylow tlow))(* 1.1 (index-tick-size))) ;;;the lows must be essentially the same
                        (< dbyclose dbyopen);;;first candle is black (2 bars back)
                        (> tclose topen) ;;;second candle is white (current candle)
                        (>= ylow (max tlow dbylow))))      
                  ) 'UPTW) ;;bullish tweezers     
           ((and (n-day-lowp date 10)
                 (<= cci21 lvll)
               
               (> trel-range 1.618) ;;;larger than normal candle
               (> (- tclose topen)(* .95 (- thigh tlow)))
               ) 'UPMB) ;;bullish marubozu      
           ((and (n-day-highp date 10)
                 (>= cci21 lvlh)
                
                 ;(< vi 0);;;below normal volume 
                 (< trel-range3 .5);;;range much smaller than normal
                 (> (abs (- topen tclose))(* .24 (- thigh tlow)));;;shadows are small
                 ) 'DIST);;;indecisive spinning top
            ((and (n-day-lowp date 10)
                 (<= cci21 lvll)
                 ;(< ccid 0)
                 ;(< vi 0);;;below normal volume
                 (< trel-range3 .5);;;range much smaller than normal
                 (> (abs (- topen tclose))(* .24 (- thigh tlow)));;;shadows are small
                 ) 'UIST);;;indecisive spinning top
           ((and (n-day-lowp date 10)
                 (<= cci21 lvll)
                 ;(< ccid 0)
                 ;(> vi 0)
                 (> trel-range 1.333);;;range larger than normal
                 (< (abs (- topen tclose))(* .3333 (- thigh tlow)));;;shadows are large
                 ) 'UIHW);;;indecisive high wave
           ((and (n-day-highp date 10)
                 (>= cci21 lvlh)
                 ;(> ccid 0)
                 ;(> vi 0) 
                 (> trel-range 1.333);;;range larger than normal
                 (< (abs (- topen tclose))(* .3333 (- thigh tlow)));;;shadows are large
                 ) 'DIHW);;;indecisive high wave
         (t nil)
             
               );;;closes the cond

          
               
));;;closes the let and the defun


;;;this is for best patterns single day for FORE
;;;returns the symbol name for the candle
(defun candle2 (date )
 (let* ((topen (getd date 'open))(tclose (getd date 'close))
       (thigh (getd date 'high))(tlow (getd date 'low))
       (date-1 (getd date 'ydate))(date-2 (getd date-1 'ydate))
       (yopen (getd date-1 'open))(yclose (getd date-1 'close))
       (yhigh (getd date-1 'high))(ylow (getd date-1 'low))(expave (ave-exp date 8))
       (yrel-range (/ (volatility date-1 1 1)(volatility date-1 25 1)))
       (trel-range (/ (volatility date 1 1)(volatility date 25 1)))
       (dbyrel-range (/ (volatility date-2 1 1)(volatility date-2 25 1)))
       (dbyopen (getd date-2 'open))(dbyclose (getd date-2 'close))
       (dbyhigh (getd date-2 'high))(dbylow (getd date-2 'low))
      ; (4-day-low (n-day-low date size 'close))(4-day-high (n-day-high date size 'close))
       )
                          
   (cond ;((and ; (eql tclose 4-day-high)(neql tclose 4-day-low)
         ;      (> tclose expave)
         ;       (or (<= (abs (- tclose topen))(* 2 (index-tick-size)))
         ;          (<= (/ (abs (- tclose topen))(- thigh tlow)) .05))
         ;      ) 'DNDJ) ;doji top
               
        ; ((and ;(eql tclose 4-day-high)(neql tclose 4-day-low)
        ;       (> tclose expave)
        ;       (<= (- thigh (max topen tclose)) (* 0.25 (abs (- thigh tlow)))) ;small upper shadow less than quarter of the  body
        ;       (>= (- (min topen tclose) tlow) (* 2 (abs (- topen tclose)))) ;;;lower shadow more than twice body      
        ;       ) 'DNHM) ;Hangman (may be white or black)
               
        ; ((and ;(eql (max tclose yclose) 4-day-high)(neql tclose 4-day-low)
        ;       (> tclose expave)
        ;       (< tclose topen) ;;;black candle
        ;       (>= yclose yopen) ;;;previous white candle
        ;       (>= topen (- yclose (index-tick-size)));;
        ;       (<= tclose (+ yopen (index-tick-size)));;engulfing pattern
        ;       ) 'DNEP) ;bearish engulfing pattern
                
         ((and ;(eql tclose 4-day-high)(neql tclose 4-day-low)
               (> tclose expave)
               (> yrel-range 1);;;larger than average candle      
               (> yclose yopen);;previous white candle
               (>= topen (- yhigh (index-tick-size)));;opens higher than previous candles' high
               (< tclose topen);;black candle
               (<= tlow (/ (+ yopen yclose) 2)) ;;must close more than 50% retracement of previous candle's body
               ) 'DNCC) ;; black cloud cover
               
         ((and ;(eql tclose 4-day-high)(neql tclose 4-day-low)
               (> tclose expave)
               (> yrel-range 1);;;larger than average candle
               (> yclose yopen) ;;previous white candle
               (>= topen (- yclose (index-tick-size)));;;gaps open above previous days body
               (>= (- thigh (max topen tclose))(* 2 (abs (- topen tclose))));;upper shadow twice the body
               (<= (- (min topen tclose) tlow)(* .25 (abs (- thigh tlow))));;;lower shadow less than quarter of the range
               ) 'DNST) ;;shooting star
        
         ((and ;(eql yclose 4-day-high)(neql tclose 4-day-low)
               (> tclose expave)
               (> dbyrel-range 1);;;First candle larger than average candle
               (> dbyclose dbyopen) ;;;first candle is white
               (> yopen  dbyhigh) ;;;second candle gaps open above high of first candle
               (>= (- yhigh (max yopen yclose))(* 1.618 (abs (- yopen yclose))));;upper shadow twice the body
               (<= (- (min yopen yclose) ylow)(* .382 (abs (- yhigh ylow))));;;lower shadow less than quarter of the range
               (< tclose topen);;;third candle is black
               (< thigh yhigh) ;;;third candle is not higher than second candle
               (< tlow (/ (+ dbyopen dbyclose) 2));;;third candle retreats below middle of first candle body
               ) 'DNES) ;;evening star is a three candle pattern
               
         ((and ;(eql yclose (n-day-high date (1+ size)))(neql yclose 4-day-low)(neql tclose 4-day-low)
               (> tclose expave)
               (> yrel-range 1) ;;;larger than normal candle
               (> (- yclose yopen)(* 2 (- (- yhigh ylow)(- yclose yopen))));;;body is larger than twice the shadows 
               (< trel-range 1) ;;;smaller than normal candle
               (> yclose yopen) ;;;white candle
               (< tclose topen) ;;;black candle
               (< (max tclose topen)(max yopen yclose));;;top of todays body lower than yesterday's top of body
               (> (min tclose topen)(min yopen yclose));;;bottom of todays body higher than yesterday's bottom of body.
               (<= (abs (- (max topen tclose)(min topen tclose)))
                   (* .5 (abs (- (max yopen yclose)(min yopen yclose)))));;;second candle body is 1/2 smaller than first candle
               ) 'DNHR) ;;bearish harami
            
         ((and ;(eql (max yclose tclose) 4-day-high)(neql tclose 4-day-low)
               (> tclose expave)
               (or (<= (abs (- yhigh thigh)) (* 2 (index-tick-size))) ;;;the highs must be essentially the same
                   (<= (abs (- (max yopen yclose)(max topen tclose))) (* 2 (index-tick-size))));;; 
               (> yclose yopen);;;first candle is white
               (< tclose topen) ;;;second candle is black
               ) 'DNTW) ;;bearish tweezers 
         ;((and (< tclose expave)
         ;      (> trel-range 1.618) ;;;larger than normal candle
         ;      (> (- topen tclose)(* .95 (- thigh tlow)))
         ;      ) 'DNMB) ;;bearish marubozu      
       
          ;((and ;(eql tclose 4-day-low) (neql tclose 4-day-high)
          ;     (< tclose expave)
          ;        (or (<= (abs (- tclose topen))(* 2 (index-tick-size)))
          ;         (<= (/ (abs (- tclose topen)) (- thigh tlow)) .05))
          ;     ) 'UPDJ) ;doji bottom
               
        ; ((and ;(eql tclose 4-day-low) (neql tclose 4-day-high)
        ;       (< tclose expave)
        ;       (<= (- thigh (max topen tclose)) (* 0.25 (abs (- thigh tlow)))) ;small upper shadow less than quarter of range
         ;      (>= (- (min topen tclose) tlow) (* 2 (abs (- topen tclose)))) ;;;lower shadow more than twice body      
         ;      ) 'UPHM) ;Hammer (may be white or black)
               
         ((and ;(eql (min tclose yclose) 4-day-low) (neql tclose 4-day-high)
               (< tclose expave)
                (> tclose topen) ;;;white candle
               (<= yclose yopen) ;;;previous black candle
               (<= topen (- yclose (index-tick-size)));;
               (>= tclose (+ yopen (index-tick-size)));;engulfing pattern
               ) 'UPEP) ;bullish engulfing pattern 
               
         ;((and ;(eql tclose 4-day-low) (neql tclose 4-day-high)
         ;       (< tclose expave)
         ;       (> yrel-range 1);;;larger than average candle      
         ;      (< yclose yopen);;previous black candle
         ;      (<= topen (+ ylow (index-tick-size)));;opens lower than previous candles' low
         ;      (> tclose topen);;white candle
         ;      (>= thigh (/ (+ yopen yclose) 2)) ;;must close more than 50% retracement of previous candle's body
         ;      ) 'UPPC) ;;piercing candle 
                     
         ((and ;(eql tclose 4-day-low) (neql tclose 4-day-high)
                (< tclose expave)
                (> yrel-range 1);;;larger than average candle
               (< yclose yopen) ;;previous black candle
               (<= topen (+ yclose (index-tick-size)));;;gaps open below previous days body
               (>= (- thigh (max topen tclose))(* 2 (abs (- topen tclose))));;upper shadow twice the body
               (<= (- (min topen tclose) tlow)(* .25 (abs (- thigh tlow))));;;lower shadow less than quarter of the range
               ) 'UPIH) ;;Inverted Hammer
       
         ((and ;(eql yclose 4-day-low) (neql tclose 4-day-high)
                (< tclose expave)
               (> dbyrel-range 1);;;First candle larger than average candle
               (< dbyclose dbyopen) ;;;first candle is black
               (< yopen  dbylow) ;;;second candle gaps below the low of first candle
               (>= (- yhigh (max yopen yclose))(* 1.618 (abs (- yopen yclose))));;upper shadow twice the body
               (<= (- (min yopen yclose) ylow)(* .382 (abs (- yhigh ylow))));;;lower shadow less than quarter of the range
               (> tclose topen);;;third candle is white
               (> tlow ylow) ;;;third candle is not lower than second candle
               (> thigh (/ (+ dbyopen dbyclose) 2));;;third candle retraces above middle of first candle body
               ) 'UPMS) ;;morning star
        
         ((and ;(eql yclose (n-day-low date (1+ size)))(neql yclose 4-day-high) (neql tclose 4-day-high)
                (< tclose expave)
               (> yrel-range 1) ;;;larger than normal candle
               (> (- yopen yclose)(* 2 (- (- yhigh ylow)(- yopen yclose))));;;body is larger than twice the shadows 
               (< trel-range 1) ;;;smaller than normal candle
               (> yopen yclose) ;;;black candle
               (< topen tclose) ;;;white candle
               (<= (max tclose topen)(max yopen yclose));;;top of todays body lower than yesterday's top of body
               (>= (min tclose topen)(min yopen yclose));;;bottom of todays body higher than yesterday's bottom of body.
               (<= (abs (- (max topen tclose)(min topen tclose)))
                   (* .5 (abs (- (max yopen yclose)(min yopen yclose)))));;;second candle body is 1/2 smaller than first candle
               ) 'UPHR) ;;bullish harami 
             
         ; ((and ;(eql (min yclose tclose) 4-day-low) (neql tclose 4-day-high)
         ;       (< tclose expave)
         ;       (or (<= (abs (- ylow tlow)) (* 2 (index-tick-size)));;;the lows must be essentially the same 
         ;          (<= (abs (- (min yopen yclose)(min topen tclose)))(* 2 (index-tick-size))));;;
         ;      (< yclose yopen) ;;;first candle is black
         ;      (> tclose topen) ;;;second candle is white
         ;      ) 'UPTW) ;;bullish tweezers     
           ((and (> tclose expave)
               (> trel-range 1.618) ;;;larger than normal candle
               (> (- tclose topen)(* .95 (- thigh tlow)))
               ) 'UPMB) ;;bullish marubozu      
           ((and (< trel-range .6666);;;range smaller than normal
                 (> (abs (- topen tclose))(* .3333 (- thigh tlow)));;;shadows are small
                 ) 'IDST);;;indecisive spinning top
           ((and (> trel-range 1);;;range larger than normal
                 (< (abs (- topen tclose))(* .3333 (- thigh tlow)));;;shadows are large
                 ) 'IDHW);;;indecisive high wave
         (t 'XXX)
             
               );;;closes the cond

          
               
));;;closes the let and the defun



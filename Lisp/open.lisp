;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;;;


;;;;first value in list is the market symbol
;;;;second value in list is the direction
;;;;third value is the previous stop loss price
;;;;fourth value is the entry date
;;;; fifth value is the entry price
(defparameter *open-contraswings*

   '(

   ))

(defparameter *open-positions*

   '(

     ;   (ru.d1b long 1161.50 20140213 1140.00)
     ;   (w.d1b long 675.50 20140303 616.00)
     ;
     ;   (nd.d1b short 3721.00 20140317 3613.50)
        ))

(defparameter *open-contrapositions*
   '(

   ))

(defparameter *forex-open-swings*

 '(

 ))
(defparameter *forex-open-contraswings*

 '(

 ))

#|
(defun test-open-positions ()
  (dolist (ith (append *open-swings* *open-positions* *forex-open-swings*))
    (ifn (member (car ith) (append *swing-list* *forex-list* *position-list*))(return (car ith)))))

;   nil);;for partial holidays
|#
(defparameter *X-list*
  '((sp.d1b .618 2 spu.d1b "JUN14" "S&amp;P 500 E-mini" .25 T)
    (nd.d1b 1.382 2 ndu.d1b "JUN14" "NASDAQ 100 E-mini" .25 T)
    (nk.d1b .618 0 nku.d1b "JUN14" "NIKKEI 225" 5 T)

    (ty.d1b .618 nil tyu.d1b "JUN14" "TEN YEAR NOTES" nil T)

    (gc.d1b 1.000 2 gcz.d1b "JUN14" "GOLD CMX" .10 nil)
    (si.d1b 1.382 4 svu.d1b "JUL14" "SILVER CMX" .0050 T)

    (ad.d1b .618 4 adu.d1b "JUN14" "AUSTRALIAN DOLLAR" .0001 T)
    (bp.d1b 1.382 4 bpu.d1b "JUN14" "BRITISH POUND" .0001 T)
    (cd.d1b .618 4 cdu.d1b "JUN14" "CANADIAN DOLLAR" .0001 T)
    (e1.d1b .618 4 e1u.d1b "JUN14" "EURO" .0001 T)
    (jy.d1b 1.618 4 jyu.d1b "JUN14" "JAPANESE YEN" .0001 T)
    (sf.d1b .618 4 sfu.d1b "JUN14" "SWISS FRANC" .0001 T)


    (cl.d1b .618 3 clu.d1b "JUN14" "CRUDE OIL" .01 T)
    (ng.d1b .618 3 ngu.d1b "JUN14" "NATURAL GAS" .001 T)

    (cf.d1b 1.382 2 cfu.d1b "JUL14" "COFFEE" .05 T)
    (ct.d1b 1.382 2 ctz.d1b "JUL14" "COTTON" .01 T)

    (s.d1b 1.000 2 s_u.d1b "JUL14" "SOYBEANS" .25 T)
    (w.d1b .618 2 wu.d1b "JUL14" "WHEAT CBOT" .25 T)

    ))


(defparameter *select-list*
  '(sp.d1b us.d1b gc.d1b e1.d1b cl.d1b su.d1b s.d1b))

(defparameter *gim-list*
  '(s.d1b bo.d1b sm.d1b c.d1b w.d1b ty.d1b us.d1b lc.d1b lh.d1b)
    )

(defparameter *major8-list*
   '(dj.d1b sp.d1b nd.d1b ru.d1b ty.d1b us.d1b gc.d1b si.d1b))

(defparameter *lucky7-list*
   '(dj.d1b sp.d1b nd.d1b ru.d1b ty.d1b us.d1b gc.d1b))

(defparameter *score-9-list*  ;;for day trades SCORE less STARTER markets
    '( dj.d1b nd.d1b nk.d1b
      gc.d1b ho.d1b
      ct.d1b cc.d1b su.d1b
      sm.d1b

    ))

(defparameter *starter1-list* ;;STARTER day trade list
  '(ru.d1b sp.d1b si.d1b cp.d1b e1.d1b cl.d1b hu.d1b ng.d1b s.d1b w.d1b))



(defparameter *dr13-list*  ;;for swing trades
   '(dj.d1b
     gc.d1b si.d1b  pl.d1b
     e1.d1b
     cl.d1b ng.d1b
     cc.d1b su.d1b
     c.d1b s.d1b bo.d1b sm.d1b

     ))
(defparameter *dr18-list*    ;;for position trades
    '(dj.d1b ru.d1b nk.d1b
    us.d1b si.d1b pl.d1b
    ad.d1b cl.d1b ho.d1b ng.d1b
    ct.d1b cc.d1b cf.d1b oj.d1b
    c.d1b bo.d1b sm.d1b w.d1b
    ))

(defparameter *EP-swing-list*
   '(sp.d1b nd.d1b ty.d1b
      cp.d1b
      bp.d1b
      cd.d1b e1.d1b mx.d1b ; sf.d1b
     ho.d1b hu.d1b ng.d1b
     ct.d1b cc.d1b
     c.d1b s.d1b bo.d1b sm.d1b w.d1b lh.d1b
     ))
(defparameter *EP-position-list*
    '(dj.d1b ru.d1b nk.d1b
      us.d1b gc.d1b si.d1b pl.d1b pa.d1b
      ad.d1b jy.d1b cl.d1b
      cf.d1b oj.d1b su.d1b
     lc.d1b
      ))
(defparameter *epic25-list*
  '(dj.d1b sp.d1b nd.d1b ru.d1b nk.d1b us.d1b ty.d1b
    gc.d1b si.d1b cp.d1b
    cl.d1b ho.d1b hu.d1b ng.d1b ct.d1b cc.d1b cf.d1b su.d1b
    c.d1b s.d1b sm.d1b bo.d1b w.d1b lc.d1b lh.d1b))


  (defparameter *all-list*
         '(
     dj.d1b sp.d1b
     nd.d1b ru.d1b nk.d1b
     ty.d1b
     us.d1b
     gc.d1b
     si.d1b cp.d1b
     pl.d1b pa.d1b
     ad.d1b bp.d1b cd.d1b e1.d1b jy.d1b sf.d1b
     mx.d1b
     cl.d1b ho.d1b   hu.d1b
     ng.d1b
     ct.d1b cc.d1b cf.d1b ;lb.d1b
     oj.d1b su.d1b
     c.d1b s.d1b
     bo.d1b sm.d1b w.d1b
     lc.d1b lh.d1b
     aud.d3b gbp.d3b cad.d3b
     eur.d3b jpy.d3b  chf.d3b
      ; brl.d3b cny.d3b inr.d3b rub.d3b
      ;chfjpy.d3b euraud.d3b eurchf.d3b
     eurgbp.d3b ;mxn.d3b
     eurjpy.d3b
     gbpjpy.d3b
     ))


(defparameter *ninja-symbol*
   '(
     (dj.d1b . YM)
     (sp.d1b . ES)
     (nd.d1b . NQ)
     (ru.d1b . TF)
     (nk.d1b . NKD)

     (ty.d1b . ZN)
     (us.d1b . ZB)

     (gc.d1b . GC)
     (si.d1b . SI)
     (cp.d1b . HG)
     (pl.d1b . PL)
     (pa.d1b . PA)

     (ad.d1b . 6A)
     (bp.d1b . 6B)
     (cd.d1b . 6C)
     (e1.d1b . 6E)
     (jy.d1b . 6J)
     (sf.d1b . 6S)
     (mx.d1b . 6M)

     (cl.d1b . CL)
     (ho.d1b . HO)
     (hu.d1b . RB)
     (ng.d1b . NG)

     (ct.d1b . CT)
     (cc.d1b . CC)
     (cf.d1b . KC)
     (oj.d1b . OJ)
     (su.d1b . SB)

     (c.d1b . ZC)
     (s.d1b . ZS)
     (bo.d1b . ZL)
     (sm.d1b . ZM)
     (w.d1b . ZW)

     (lc.d1b . LE)
     (lh.d1b . HE)


    (aud.d3b . AUD)
    (gbp.d3b . GBP)
    (cad.d3b . CAD)
    (eur.d3b . EUR)
    (jpy.d3b . JPY)
    (chf.d3b . CHF)

    (chfjpy.d3b . CHFJPY)
    (euraud.d3b . EURAUD)
    (eurchf.d3b . EURCHF)
    (eurgbp.d3b . EURGBP)
    (eurjpy.d3b . EURJPY)

    (gbpjpy.d3b . GBPJPY)

  ))

(defparameter *rjo-symbol*
   '(
     (dj.d1b . YM)
     (sp.d1b . EP)
     (nd.d1b . NQ)
     (ru.d1b . TFE)
     (nk.d1b . NKD)

     (ty.d1b . ZN)
     (us.d1b . ZB)

     (gc.d1b . GC)
     (si.d1b . SI)
     (cp.d1b . CP)
     (pl.d1b . PL)

     (e1.d1b . EU6)


     (cl.d1b . CL)
     (ho.d1b . HO)
     (hu.d1b . RB)
     (ng.d1b . NG)

     (ct.d1b . CT)
     (cc.d1b . CC)
     (cf.d1b . KC)

     (su.d1b . SB)

     (s.d1b . ZS)
     (w.d1b . ZW)

  ))

(defparameter *elmo-start-dates*

  '(
;;;Dow Jones 30
    (dj.d1b "20000114")

;;;S&P
    (SP.d1b "19871020")

;;;Nasdaq 100
    (nd.d1b "19901011")

;;;Russell 2000
    (ru.d1b "20000309")

;;;nikkei
    (nk.d1b "19891229")

;;;NOTES
    (TY.d1b "20000121")

;;;BONDS
    (US.d1b "19810929")
;;;GOLD
    (GC.d1b "19800121")

;;;;silver
    (si.d1b "19980831")
;;;copper
    (cp.d1b "19890123")

;;;Australian dollar
    (ad.d1b "19961202")

;;;british pound
    (bp.d1b "19920824")

;;;canadian dollar
    (cd.d1b "19911101")

;;;eurofx
    (e1.d1b "19990104")

;;;swiss franc
    (sf.d1b "19850225")

;;;japanese yen
    (jy.d1b "19950418")

;;;;crude oil
    (cl.d1b "19971003")

;;;heating oil
    (ho.d1b "19901009")

;;;natural gas
    (ng.d1b "19920218")

;;;unleaded gasoline
    (hu.d1b "19990211")

;;;RBOB gasoline
  ;  (rb.d1b "19990211")

;;;cotton
    (ct.d1b "19950526")

;;;coffee
    (cf.d1b "19970529")

;;;sugar
    (su.d1b "19900316")

;;;cocoa
    (cc.d1b "19971215")

;;;orange juice
    (oj.d1b "19930209")

;;;live cattle
    (lc.d1b "19860611")

;;;lean hogs
    (lh.d1b "19970417")

;corn
    (c.d1b "19960517")

;;;soybean oil
    (bo.d1b "19880715")

;;;soybean meal
    (sm.d1b "19880621")
;;soybeans
    (s.d1b "19880623")

;;;wheat
    (w.d1b "19960425")

;;;australian dollar cash
    (aud.d3b "20010402")
    (gbp.d3b "20010612")
    (cad.d3b "20020121")
    (eur.d3b "20001026")
    (jpy.d3b "19980811")
    (chf.d3b "20001026")

    (chfjpy.d3b "20000907")
    (euraud.d3b "19970805")
    (eurchf.d3b "19970220")
    (eurgbp.d3b "20000504")
    (eurjpy.d3b "20001026")

    (gbpjpy.d3b "20000912")
   ))

(defun get-sector (market)
 (case market
   ((dj.d1b sp.d1b nd.d1b ru.d1b nk.d1b) 'EPINDICES)
   ((ty.d1b us.d1b) 'EPFINANCIALS)
   ((gc.d1b si.d1b cp.d1b) 'EPMETALS)
   ((ad.d1b bp.d1b cd.d1b e1.d1b jy.d1b sf.d1b) 'EPCURRENCIES)
   ((cl.d1b ho.d1b hu.d1b ng.d1b) 'EPENERGY)
   ((ct.d1b cc.d1b cf.d1b oj.d1b su.d1b) 'EPSOFTS)
   ((c.d1b s.d1b bo.d1b sm.d1b w.d1b) 'EPGRAINS)
   ((lc.d1b lh.d1b) 'EPLIVESTOCK)
   ((aud.d3b gbp.d3b cad.d3b eur.d3b jpy.d3b chfjpy.d3b euraud.d3b eurchf.d3b eurgbp.d3b eurjpy.d3b gbpjpy.d3b) 'EPFOREX))
 )
(defun selectp (market)
  (case market
   ((sp.d1b us.d1b gc.d1b e1.d1b cl.d1b su.d1b s.d1b ) 'EPSELECT))
  )


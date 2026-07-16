;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

;;;;
(defparameter *cannon-starter-qty* 0)
(defparameter *cannon-starter-block-acct* "STARTER")
(defparameter *cannon-score-qty* 1)
(defparameter *cannon-score-block-acct* "SCORE")
(defparameter *cannon-epic-qty* 0)
(defparameter *cannon-epic-block-acct* "EPIC14")

(defparameter *cannon-timezone* 'pacific)

(defparameter *cannon-fore-qty* 0)
(defparameter *cannon-fore-block-acct* "FORE")

(defparameter *c2-fore-qty* 1)
(defparameter *c2-fore-block-acct* "FORE")

(defparameter *foremost-starter-qty* 1)
(defparameter *foremost-starter-block-acct* "E38169" ) 
(defparameter *foremost-score-qty* 1)
(defparameter *foremost-score-block-acct* "E38169")
(defparameter *foremost-timezone* 'central)


(defparameter *kingsview-starter-qty* 2)
(defparameter *kingsview-starter-block-acct* "StarterBlock")  ;"G54504")
(defparameter *kingsview-epic-qty* 1)
(defparameter *kingsview-epic-block-acct* "Epic1Lot")


(defparameter *daniels-starter-qty* 0)
(defparameter *daniels-starter-block-acct* "ExitPoints") ; "E62527" )
(defparameter *daniels-score-qty* 0)
(defparameter *daniels-score-block-acct* "Score")
(defparameter *daniels-timezone* 'pacific)

(defparameter *apex-starter-qty* 1)
(defparameter *apex-starter-block-acct* "DEMO056973" )

(defparameter *apex-timezone* 'central)

(defparameter *sol-starter-qty* 1)
(defparameter *sol-starter-block-acct* "DEMO059556")
(defparameter *sol-score-qty* 1)
(defparameter *sol-score-block-acct* "DEMO059556")
(defparameter *sol-epic-qty* 1)
(defparameter *sol-epic-block-acct* "DEMO059556")

(defparameter *striker-starter-qty* 1)
(defparameter *striker-starter-block-acct* "STARTER")
(defparameter *striker-score-qty* 1)
(defparameter *striker-score-block-acct* "SCORE")
(defparameter *striker-epic-qty* 1)
(defparameter *striker-epic-block-acct* "EPIC")
(defparameter *striker-timezone* 'central)


;------------------------------------------------------------------------------------

(defparameter *btr-score-qty* 1)
(defparameter *btr-score-block-acct* "DEMO061173")
(defparameter *btr-timezone* 'central)

(defparameter *rjobrien-starter-qty* 1)
(defparameter *rjobrien-starter-block-acct* "24XINSTD1") ; "24XBTREP")

(defparameter *rjobrien-score-qty* 1)
(defparameter *rjobrien-score-block-acct* "SCORE1") ;"767 10032") ;"24XINSTD2")
(defparameter *rjobrien-epic-qty* 1)
(defparameter *rjobrien-epic-block-acct* "EPIC1") ;"767 10032") ; "24XINSTD3")
(defparameter *rjobrien-timezone* 'central)

(defparameter *ifm-cfd-qty* 1)
(defparameter *ifm-cfd-block-acct* "DEMOCDF")

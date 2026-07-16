;;; -*- Mode: LISP; Package: user; Base: 10. -*-

#+:ALLEGRO (in-package :user)
#+:SBCL (in-package :common-lisp-user)

#+:LUCID
(defconstant lucid::sq-fp-for-call-back 81)
#+:LUCID
(defconstant lucid::fixnum-data-position 2)
#+:LUCID
(defconstant lucid::sq-unix-fp 80)
#+:LUCID
(defconstant lucid::*stack-machine* nil)
#+:ALLEGRO
(defconstant ff::sq-fp-for-call-back 81)
#+:ALLEGRO
(defconstant ff::fixnum-data-position 2)
#+:ALLEGRO
(defconstant ff::sq-unix-fp 80)
#+:ALLEGRO
(defconstant ff::*stack-machine* nil)

#+:LUCID
(def-foreign-function (gethostid (:return-type :signed-32bit)
				      (:name "_gethostid")
				      (:language :c)))

#+:ALLEGRO-V4.2
(ff:defforeign 'gethostid
               :return-type :integer
               :entry-point "_gethostid"
               :language :c)

#+(or :ALLEGRO-V5.0 :ALLEGRO-V5.0.1)
(defun gethostid ()
  #x809f13d6)

  

#+:ALLEGRO-V4.2
(ff:defforeign 'getenv
               :return-type :integer
               :entry-point "_getenv"
               :arguments '(simple-string)
               :language :c)

#+(or :ALLEGRO-V5.0 :ALLEGRO-V5.0.1)
(defun environment-variable (env)
  (sys:getenv env))

#+:ALLEGRO-V4.2
(defun environment-variable (env)
  (if (zerop (getenv env)) nil
    (ff:char*-to-string
     (getenv env))))











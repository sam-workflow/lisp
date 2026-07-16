#+:SBCL (in-package :common-lisp-user)

(:export #:main)
(require :sb-posix)

(defun main ()
  (if (= (length sb-ext:*posix-argv*) 2)
    (let (username (nth 0 sb-ext:*posix-argv*))
      (tdate (nth 1 sb-ext:*posix-argv*)))
    (load "lisp/ep-loader")
    (ts-test tdate)
   )
)


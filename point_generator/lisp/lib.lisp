(in-package :point-gen)

(define-foreign-library libach
  (t (:default "libach")))
(use-foreign-library libach)

(define-foreign-library libsns
  (t (:default "libsns")))
(use-foreign-library libsns)

(defcfun "sns_init" :void)
(defcfun "sns_end" :void)

(defcfun "sns_msg_dump" :void
  (file :pointer)
  (msg :pointer))

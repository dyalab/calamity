(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem point-gen
  :description "Point generator."
  :depends-on ("cffi" "amino" "sns")
  :author ("Matthew A. Schack")
  :license :bsd-3
  ;; Keep these components in sync with Makefile.am
  :components ((:file "package")
	       (cffi-grovel:grovel-file "grovel" :depends-on ("package"))
	       (:file "lib" :depends-on ("grovel"))
	       (cffi-grovel:wrapper-file "wrappers" :depends-on ("lib" "package"))
	       (:file "point-generator" :depends-on ("wrappers" "lib" "package"))))

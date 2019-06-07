(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cal-coms
  :description "Communication functions for Calamity robot"
  :depends-on ("sns" "tmsmt")
  :components (
	       (:file "package")
	       (:file "tmkit-sns-coms" :depends-on ("package"))))

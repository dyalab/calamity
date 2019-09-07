(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem cal-coms
  :description "Communication functions for Calamity robot"
  :depends-on ("sns" "tmsmt")
  :components (
	       (:file "package")
	       (:file "probabilities"      :depends-on ("package"))
	       (:file "sg-helpers"         :depends-on ("package"))

	       (:file "generate-new-start" :depends-on ("probabilities"))

	       (:file "tmkit-sns-coms"     :depends-on ("package"))
	       (:file "state-update"       :depends-on ("tmkit-sns-coms"))
	       (:file "scene-update"       :depends-on ("tmkit-sns-coms"))

	       (:file "sim-disturber"      :depends-on ("tmkit-sns-coms" "probabilities"))))

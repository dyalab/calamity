(progn
  (in-package :point-gen)

  (cc-flags "--std=gnu99")
  (cc-flags #.(concatenate 'string
                           "-I"
                           "/usr/local/include/amino-1.0"
			   " -lach -lsns"))
  (include "stdlib.h")
  (include "sns.h")

  (ctype uint32-t "uint32_t")
  (ctype ach-status-t "ach_status_t")

  (cstruct msg-header "struct sns_msg_header"
           (sec "sec" :type :int64)
           (dur-nsec "dur_nsec" :type :int64)
           (nsec "nsec" :type :uint32)
           (n "n" :type :uint32)
           (from-pid "from_pid" :type :int64)
           (from-host "from_host" :type :char :count "SNS_HOSTNAME_LEN")
           (ident "ident" :type :char :count "SNS_IDENT_LEN")
           )
  (cstruct sns-msg-vector "struct sns_msg_vector"
	   (header "header" :type (:struct msg-header))
	   (x "x" :type :pointer)))

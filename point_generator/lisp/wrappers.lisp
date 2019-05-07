(in-package :point-gen)

(progn
  (include "stdlib.h")
  (include "sns.h")

  (defwrapper ("sns_msg_vector_put" sns-msg-vector-put) ach::ach-status-t
    (chan :pointer)
    (msg :pointer))

  (defwrapper ("sns_msg_vector_heap_alloc" sns-msg-vector-heap-alloc) :pointer
    (n uint32-t))
  )

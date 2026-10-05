;; The `chat' protocol of examples/ENTRY.websocket: JSON messages in text
;; frames. Copy it to app/protocols/ of the app, `art work' loads it.
(define-ws-protocol chat
  #:type 'text
  #:decode (lambda (bv) (json-string->scm (utf8->string bv)))
  #:encode (lambda (obj) (string->utf8 (scm->json-string obj))))

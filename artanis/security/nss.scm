;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019-2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis security nss)
  #:use-module (artanis utils)
  #:use-module (artanis ffi)
  #:use-module (system foreign)
  #:use-module (rnrs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (nss:no-db-init
            nss:init
            nss:init-rw
            nss:is-initialized?
            nss:shutdown
            nss:init-context
            nss:base64-decode
            nss:base64-encode
            nss:hash

            string->md5
            string->sha-1
            string->sha-224
            string->sha-256
            string->sha-384
            string->sha-512
            generate-rule-uid

            SEC_OID_SHA1
            SEC_OID_SHA256
            SEC_OID_SHA384
            SEC_OID_SHA512

            MD5_DIGEST_LENGTH
            SHA1_DIGEST_LENGTH
            SHA224_DIGEST_LENGTH
            SHA256_DIGEST_LENGTH
            SHA384_DIGEST_LENGTH
            SHA512_DIGEST_LENGTH

            CKM_MD5_HMAC
            CKM_SHA1_HMAC
            CKM_SHA224_HMAC
            CKM_SHA256_HMAC
            CKM_SHA384_HMAC
            CKM_SHA512_HMAC

            nss:hash-it
            nss:pr-cleanup

            nss:hmac-raw))

(define *nss-error-msg*
  "
Please visit for more information:
https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS/SSL_functions/sslerr.html
")

(eval-when (eval load compile)
  (ffi-binding "libnss3"
    (define-c-function int NSS_NoDB_Init ('*))
    (define-c-function int NSS_Init ('*))
    (define-c-function int NSS_InitReadWrite ('*))
    (define-c-function int NSS_IsInitialized)
    (define-c-function int NSS_Initialize ('* '* '* '* uint32))
    (define-c-function int NSS_Shutdown)
    (define-c-function '* NSS_InitContext ('* '* '* '* '* uint32))
    (define-c-function '* NSSBase64_DecodeBuffer ('* '* '* unsigned-int))
    (define-c-function '* NSSBase64_EncodeItem ('* '* unsigned-int '*))
    ;; For HMAC
    (define-c-function int PK11_HashBuf (int '* '* uint32))
    (define-c-function '* PK11_GetInternalKeySlot)
    (define-c-function '* PK11_ImportSymKey ('* uint32 int int '* '*))
    (define-c-function '* PK11_CreateContextBySymKey (uint32 int '* '*))
    (define-c-function int PK11_DigestBegin ('*))
    (define-c-function int PK11_DigestOp ('* '* unsigned-int))
    (define-c-function int PK11_DigestFinal ('* '* '* unsigned-int))
    (define-c-function void PK11_DestroyContext ('* int))
    (define-c-function void PK11_FreeSymKey ('*))
    (define-c-function void PK11_FreeSlot ('*))
    )

  (ffi-binding "libssl3"
    (define-c-function int SSL_OptionSet ('* uint32 int))
    (define-c-function int SSL_ImportFD ('* int))
    )

  (ffi-binding "libnspr4"
    (define-c-function void PR_Init (int int int))
    (define-c-function int PR_ImportTCPSocket (int))
    (define-c-function int PR_Write (int '* size_t))
    (define-c-function int PR_Read (int '* size_t))
    (define-c-function int PR_Close (int))
    (define-c-function int PR_Cleanup)
    )
  )

;; Internal: make a SECItem struct pointing at a bytevector
;; SECItem = { int type, unsigned char* data, unsigned int len }
(define *SECItem* (list int '* unsigned-int))
(define* (make-sec-item #:optional (type 0) (data %null-pointer) (len 0))
  (make-c-struct *SECItem* (list type data len)))

(define (no-check x) #f)
(define (<0 x) (< x 0))
(define (not-nullptr? x) (not (null-pointer? x)))
(define (->nss-boolean x)
  (match x
    (1 #t)
    (0 #f)
    (else (throw 'artanis-err 500 ->nss-boolean "Invalid value `~a'" x))))

(define-syntax-rule (gen-nss-api checker caster expr ...)
  (call-with-values (lambda () expr ...)
    (lambda (ret errno)
      (cond
       ((checker ret)
        (throw 'artanis-err 500 "NSS error: ~a" errno *nss-error-msg*))
       (else (caster ret))))))

(define-syntax-rule (gen-common-api expr ...)
  (gen-nss-api <0 identity expr ...))

(define-syntax-rule (path->pointer path)
  (if (pointer? path)
      path
      (string->pointer path)))

(define* (nss:no-db-init #:optional (config-dir %null-pointer))
  (gen-common-api (%NSS_NoDB_Init (path->pointer config-dir))))

(define* (nss:init #:optional (config-dir %null-pointer))
  (gen-common-api (%NSS_Init (path->pointer config-dir))))

(define* (nss:init-rw #:optional (config-dir %null-pointer))
  (gen-common-api (%NSS_InitReadWrite (path->pointer config-dir))))

(define (nss:is-initialized?)
  (gen-nss-api no-check ->nss-boolean (%NSS_IsInitialized)))

(define (nss:shutdown)
  (gen-common-api (%NSS_Shutdown)))

(define (nss:init-context config-dir cert-prefix key-prefix sec-mod-name flags)
  (gen-nss-api not-nullptr? identity
               (%NSS_InitContext (string->pointer config-dir)
                                 (string->pointer cert-prefix)
                                 (string->pointer key-prefix)
                                 (string->pointer sec-mod-name)
                                 %null-pointer flags)))

(define (nss:hash-it algo str/bv)
  (let ((in (cond
             ((string? str/bv) (string->pointer str/bv))
             (((@ (rnrs) bytevector?) str/bv)
              (bytevector->pointer str/bv))
             (else (throw 'artanis-err 500 nss:hash-it
                          "need string or bytevector!" str/bv))))
        (len (cond
              ((string? str/bv) (string-utf8-length str/bv))
              ((bytevector? str/bv) (bytevector-length str/bv))
              (else (throw 'artanis-err 500 nss:hash-it
                           "Invalid input type `~a'" str/bv)))))
    (algo in len)))

(define (base64-decode ibuf len)
  (call-with-values
      (lambda ()
        (%NSSBase64_DecodeBuffer %null-pointer %null-pointer ibuf len))
    (lambda (ptr errno)
      (if (null-pointer? ptr)
          (throw 'artanis-err 500 nss:base64-decode
                 "Invalid string to be decoded from Base64!")
          (let ((item (parse-c-struct ptr *SECItem*)))
            (pointer->string (cadr item) (caddr item)))))))

(define (base64-encode ibuf len)
  (let ((item (make-sec-item 0 ibuf len)))
    (call-with-values
        (lambda ()
          (%NSSBase64_EncodeItem %null-pointer %null-pointer 0 item))
      (lambda (ret errno)
        (if (null-pointer? ret)
            (throw 'artanis-err 500 nss:base64-encode
                   "Invalid string to be encoded to Base64!")
            (pointer->string ret))))))

(define (nss:base64-encode str/bv)
  (nss:hash-it base64-encode str/bv))

(define (nss:base64-decode str/bv)
  (nss:hash-it base64-decode str/bv))

(define-public SEC_OID_MD5 3)
(define SEC_OID_SHA1 4)
;; NOTE: In NSS, sha-224 only supports by HMAC
;;(define SEC_OID_HMAC_SHA224 295)
(define SEC_OID_SHA256 191)
(define SEC_OID_SHA384 192)
(define SEC_OID_SHA512 193)

(define SHA224_DIGEST_LENGTH 28)
(define SHA256_DIGEST_LENGTH 32)
(define SHA384_DIGEST_LENGTH 48)
(define SHA512_DIGEST_LENGTH 64)
(define SHA1_DIGEST_LENGTH 20)
(define MD5_DIGEST_LENGTH 16)

;; For HMAC
;; PKCS#11 CKM mechanism types for HMAC
(define CKM_MD5_HMAC    #x00000211)
(define CKM_SHA1_HMAC   #x00000221)
(define CKM_SHA224_HMAC #x00000241)
(define CKM_SHA256_HMAC #x00000251)
(define CKM_SHA384_HMAC #x00000261)
(define CKM_SHA512_HMAC #x00000271)

;; PKCS#11 attribute/origin constants
(define CKA_SIGN          #x00000108)
(define PK11_OriginUnwrap 4)

;; PR_TRUE for PK11_DestroyContext freeit param
(define PR_TRUE 1)

(define (pk11-haskbuf algo out in len algo-len)
  (gen-common-api (%PK11_HashBuf algo out in len))
  (format #f "~{~2,'0x~}" (bytevector->u8-list (pointer->bytevector out algo-len))))

(define (nss:hash algo algo-len str/bv)
  (let ((out (bytevector->pointer (make-bytevector algo-len 0)))
        (in (cond
             ((string? str/bv) (string->pointer str/bv))
             ((bytevector? str/bv) (bytevector->pointer str/bv))
             (else (throw 'artanis-err 500 nss:hash
                          "Invalid input type `~a'" str/bv))))
        (len (cond
              ((string? str/bv) (string-utf8-length str/bv))
              ((bytevector? str/bv) (bytevector-length str/bv))
              (else (throw 'artanis-err 500 nss:hash
                           "Invalid input type `~a'" str/bv)))))
    (when (not (nss:is-initialized?))
      ;; Although it's safe to call NSS init multiple times, it's faster to check first
      (nss:no-db-init))
    (pk11-haskbuf algo out in len algo-len)))

(define (string->sha-512 str/bv)
  (nss:hash SEC_OID_SHA512 SHA512_DIGEST_LENGTH str/bv))

(define (string->sha-384 str/bv)
  (nss:hash SEC_OID_SHA384 SHA384_DIGEST_LENGTH str/bv))

(define (string->sha-256 str/bv)
  (nss:hash SEC_OID_SHA256 SHA256_DIGEST_LENGTH str/bv))

#;
(define (nss:sha-224 str)
(nss:hash SEC_OID_HMAC_SHA224 SHA224_DIGEST_LENGTH str))

(define (string->sha-1 str/bv)
  (nss:hash SEC_OID_SHA1 SHA1_DIGEST_LENGTH str/bv))

(define (string->md5 str/bv)
  (nss:hash SEC_OID_MD5 MD5_DIGEST_LENGTH str/bv))

(define PR_USER_THREAD 0)
(define PR_PRIORITY_NORMAL 1)
(define (nss:pr-init)
  (%PR_Init PR_USER_THREAD PR_PRIORITY_NORMAL 1)
  )

(define (nss:ssl-option-set)
  (%SSL_OptionSet sslSocket SSL_HANDSHAKE_AS_CLIENT PR_TRUE)
  )

(define (pr-import-tcp-socket sock)
  (%PR_ImportTCPSocket sock))

(define (ssl-import-fd null sock) #t)

(define (pr-write sock buffer size) #t)

(define (pr-read sock buffer size) #t)

(define (pr-close) #t)

(define (pr-cleanup) #t)

(define (nss:pr-cleanup)
  (when (nss:is-initialized?)
    (pr-cleanup)))

;; FIXME: MD5 may not be the best choice
(define-syntax-rule (generate-rule-uid rule)
  (string->md5 rule))


;; For HMAC

;; We reuse the existing *SECItem* definition: (list int uint64 unsigned-int)
(define (bv->sec-item bv)
  (make-c-struct *SECItem*
                 (list 0
                       (bytevector->pointer bv)
                       (bytevector-length bv))))

;; Internal: make an empty SECItem (for the HMAC param — must be present but empty)
(define (make-empty-sec-item)
  (make-c-struct *SECItem* (list 0 %null-pointer 0)))

;; Internal: core native HMAC using NSS PK11 HMAC context
;; Returns raw bytevector of length digest-len
(define (nss:hmac-raw ckm-mech digest-len key-bv data-bv)
  (define (call/check thunk who)
    (call-with-values thunk
      (lambda (ret errno)
        (when (and (integer? ret) (< ret 0))
          (throw 'artanis-err 500 who "NSS error errno=~a" errno))
        ret)))
  (define (call/ptr thunk who)
    (call-with-values thunk
      (lambda (ret errno)
        (when (null-pointer? ret)
          (throw 'artanis-err 500 who "NSS returned null, errno=~a" errno))
        ret)))
  (when (not (nss:is-initialized?))
    (nss:no-db-init))
  (let ((slot (call/ptr %PK11_GetInternalKeySlot 'nss:hmac-raw)))
    (let ((sym-key (call/ptr
                    (lambda ()
                      (%PK11_ImportSymKey slot ckm-mech
                                          PK11_OriginUnwrap CKA_SIGN
                                          (bv->sec-item key-bv) %null-pointer))
                    'nss:hmac-raw)))
      (let ((ctx (call/ptr
                  (lambda ()
                    (%PK11_CreateContextBySymKey ckm-mech CKA_SIGN
                                                 sym-key (make-empty-sec-item)))
                  'nss:hmac-raw)))
        (let* ((data-ptr (bytevector->pointer data-bv))
               (data-len (bytevector-length data-bv))
               (out-bv (make-bytevector digest-len 0))
               (out-ptr (bytevector->pointer out-bv))
               (out-len-bv (make-bytevector (sizeof unsigned-int) 0))
               (out-len-ptr (bytevector->pointer out-len-bv)))
          (call/check (lambda () (%PK11_DigestBegin ctx)) 'nss:hmac-raw)
          (call/check (lambda () (%PK11_DigestOp ctx data-ptr data-len)) 'nss:hmac-raw)
          (call/check (lambda () (%PK11_DigestFinal ctx out-ptr out-len-ptr digest-len)) 'nss:hmac-raw)
          (%PK11_DestroyContext ctx PR_TRUE)
          (%PK11_FreeSymKey sym-key)
          (%PK11_FreeSlot slot)
          out-bv)))))

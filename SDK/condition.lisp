(in-package :SinaWeiboSDK)

(define-condition sinaWeibosdk-error (error)
  ((status-code :initarg :status-code
                :reader status-code)
   (content-string :initarg :content-string
                   :reader content-string)))

(define-condition token-null-error (error)
  ((message :initform "access-token is null"
            :reader message)))

(define-condition request-code-error (sinaWeibosdk-error)
  ())
(define-condition request-token-error (sinaWeibosdk-error)
  ())

(defmethod print-object ((object sinaWeibosdk-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "status-code:~a~%content-string:~a~%" 
            (status-code object)
            (content-string object))))

(defmethod print-object ((object token-null-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "message:~a~%" 
            (message object))))
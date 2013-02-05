
(in-package :weiboSDK.test)

(defmacro f-test (f-name &rest parament)
  `(deftest ,(intern (format nil "test-~a" f-name))
       (let ((weibo-engine (make-instance 'SinaWeibo :user-name "rannger@sina.cn" :password "123")))
	 (login weibo-engine)
	 (multiple-value-bind (json-string status-code) (,f-name weibo-engine ,@parament)
	   (if (= status-code 200)
	       t
	       json-string)))
     t))

;(f-test login)
;(f-test update-status "lisp test")
(f-test show-user-counts '("1739928273"))
(f-test show-user-friends "1739928273")
(f-test show-user-followers "1739928273")
(f-test show-public-timeline 20)
(f-test show-mentions)
(f-test show-followers-active "1739928273")
(f-test show-user-timeline "1739928273")
(f-test show-friends-timeline)
(f-test show-home-timeline 20 1)
(f-test show-friends-timeline-ids 20 1)
(f-test show-friends-in-common 20 1 "1739928273")
(f-test show-friends-bilateral 20 1 "1739928273")

;;  (setf *app-key* "834094302")
;;  (setf *app-secret* "ba6cf551d61c4eb1839779ee127272bb")
;;  (setf *redirect_uri* "https://api.weibo.com/oauth2/default.html")
(defun run-tests ()
  (let ((save-app-key *app-key*) (save-app-secret *app-secret*) (save-redirect_uri *redirect_uri*))
    (setf *app-key* "834094302")
    (setf *app-secret* "ba6cf551d61c4eb1839779ee127272bb")
    (setf *redirect_uri* "https://api.weibo.com/oauth2/default.html")
    (setf drakma:*drakma-default-external-format* :utf-8)
    (do-tests)
    (setf *app-key* save-app-key)
    (setf *app-secret* save-app-secret)
    (setf *redirect_uri* save-redirect_uri)))












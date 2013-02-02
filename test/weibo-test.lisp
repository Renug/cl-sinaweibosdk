
(in-package :weiboSDK.test)

(defmacro f-test (f-name &rest parament)
  `(deftest ,(intern (format nil "test-~a" f-name))
       (let ((weibo-engine (make-instance 'SinaWeibo :user-name "rannger@sina.cn" :password "123")))
	 (login weibo-engine)
	 (multiple-value-bind (json-string reason-phrase status-code) (,f-name weibo-engine ,@parament)
	   (if (= status-code 200)
	       t
	       json-string)))
     t))

;(f-test login)
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


(defun run-tests ()
  (setf *app-key* "2453117888")
  (setf *app-secret* "75c04189726fd12a75b0d21ce1e10d54")
  (setf *redirect_uri* "https://api.weibo.com/oauth2/default.html")
  (do-tests))

(defun clean-tests ()
  (rem-all-tests))












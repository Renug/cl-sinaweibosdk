
(in-package :weiboSDK.test)

(defmacro f-test (f-name weibo &rest parament)
     `(multiple-value-bind (json-string reason-phrase status-code) (funcall #',f-name ,weibo ,@parament)
        (if (= status-code 200)
          (format t "~a success~%" (symbol-name ',f-name))
          (format t "~a failed ~a ~a ~a~%" (symbol-name ',f-name) reason-phrase status-code json-string))))

(defun do-tests ()
   (setf *app-key* "834094302")
  (setf *app-secret* "ba6cf551d61c4eb1839779ee127272bb")
  (setf *redirect_uri* "https://api.weibo.com/oauth2/default.html")
  (let ((weibo-engine (make-instance 'SinaWeibo :user-name "rannger@sina.cn" :password "yue_yang1963"))
        (uid "2892678074"))
     (login weibo-engine)
    (f-test show-user-counts weibo-engine (list uid))
    (f-test show-user-friends weibo-engine uid)
    (f-test show-user-followers weibo-engine uid)
    (f-test show-public-timeline weibo-engine 20)
    (f-test show-mentions weibo-engine)
    (f-test show-followers-active weibo-engine uid)
    (f-test show-user-timeline weibo-engine uid)
    (f-test show-friends-timeline weibo-engine)
    (f-test show-home-timeline weibo-engine 20 1)
    (f-test show-friends-timeline-ids weibo-engine 20 1)
    (f-test show-friends-in-common weibo-engine 20 1 3028151737)
    (f-test show-friends-bilateral weibo-engine 20 1 uid)))
    
    

    
  

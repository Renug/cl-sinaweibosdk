(in-package :SinaWeiboSDK)


(defvar *app-key* nil)
(defvar *app-secret* nil)
(defvar *redirect_uri* nil)

(defclass SinaWeibo ()
  ((user-name :initarg :user-name
	      :initform ""
	      :accessor user-name)
   (password :initarg :password
	     :initform ""
	     :accessor password)
   (access-token :initarg :access-token
		 :initform ""
		 :accessor access-token)
   (expires-in :initarg :expires-in
	       :initform 0
	       :accessor expires-in)
   (remind-in :initarg :remind-in
	      :initform ""
	      :accessor remind-in)
   (uid :initarg :uid
	:initform ""
	:accessor uid)))

(defun request-code (self)
  "OAuth2的authorize接口"
  (with-slots (user-name password) self
    (multiple-value-bind (body-or-stream status-code  headers)
	(drakma:http-request "https://open.weibo.cn/2/oauth2/authorize"
			     :method :post
			     :parameters `(("userId" . ,user-name)
					   ("passwd" . ,password)
					   ("action" . "submit")
					   ("redirect_uri" . ,*redirect_uri*)
					   ("client_id" . ,*app-key*)
					   ("withOfficalFlag" . "0")
					   ("response_type" . "code")
					   ("ticket" ."")
					   ("state" . "")
					   ("from" . "")
					   ("regCallback" . "")
					   ("isLoginSina" . ""))
			     :additional-headers `(("Referer" . ,(format nil "https://api.weibo.com/oauth2/authorize?response_type=code&redirect_uri=~a" *redirect_uri*))))
      (with-input-from-string (strm (puri:uri-query (puri:parse-uri (drakma:header-value :LOCATION headers))))
	(cdr (assoc "code" (parse-key-value-pairs strm) :test #'equalp))))))

(defun request-token (self code)
  "OAuth2的access_token接口"
    (multiple-value-bind (body-or-stream)
      (drakma:http-request "https://api.weibo.com/oauth2/access_token"
                         :method :POST
                         :parameters `(("code". ,code) 
				       ("redirect_uri" . ,*redirect_uri*)
				       ("client_id" . ,*app-key*) 
				       ("client_secret" . ,*app-secret*) 
				       ("grant_type" . "authorization_code")))
      (with-slots (access-token expires-in remind-in uid) self
	(let ((return-values 
	       (json:decode-json-from-string (if (typep body-or-stream 'string)
						 body-or-stream
						 (flexi-streams:octets-to-string body-or-stream :external-format :utf-8)))))
	  (setf access-token (cdr (assoc :ACCESS--TOKEN return-values)))
	  (setf expires-in (cdr (assoc :EXPIRES--IN return-values)))
	  (setf remind-in (cdr (assoc :REMIND--IN return-values)))
	  (setf uid (cdr (assoc :UID return-values)))))))
      

(defmethod login ((self SinaWeibo))
  (request-token self (request-code self)))

(defun generic-request (url weibo &optional http-paraments (method :GET))
  (with-slots (user-name password access-token) weibo
    (if (null access-token) 
	(error "access-token is null"))
    (drakma:http-request url
                         :method method
                         :parameters (append `(("access_token" . ,access-token))
                                             http-paraments))))

(defmacro defuntion (function-name paraments documentation &body body)
  `(progn 
     (defgeneric ,function-name (self ,@paraments)
       (:documentation ,documentation))
     (defmethod ,function-name ((self SinaWeibo) ,@paraments)
       (multiple-value-bind (body-or-stream 
                             status-code 
                             headers 
                             uri          
                             stream 
                             must-close reason-phrase) (funcall #'(lambda () ,@body))
       (let* ((retval (flexi-streams:octets-to-string body-or-stream)))
         (values retval reason-phrase status-code))))))

(defuntion update-status (text)
  "发布一条新微博"
  (generic-request "https://api.weibo.com/2/statuses/update.json" 
                    self (list (cons "status" text)) :POST))

(defuntion show-user-counts (uids)
  "批量获取用户的粉丝数、关注数、微博数"
  (generic-request "https://api.weibo.com/2/users/counts.json" 
                   self (list (cons "uids" (format nil "~{~a~^,~}" uids)))))

(defuntion show-user-friends (uid)
  "获取用户的关注列表"
  (generic-request "https://api.weibo.com/2/friendships/friends.json" 
                   self (list (cons "uid" uid))))

(defuntion show-user-followers (uid)
  "获取用户的粉丝列表"
  (generic-request "https://api.weibo.com/2/friendships/followers.json" 
                   self (list (cons "uid" (format nil "~a" uid)))))

(defuntion show-public-timeline (count)
  "返回最新的200条公共微博，返回结果非完全实时"
  (generic-request "https://api.weibo.com/2/statuses/public_timeline.json" 
                   self (list (cons "count" (format nil "~a" count)))))

(defuntion show-mentions ()
  "获取最新的提到登录用户的微博列表，即@我的微博"
  (generic-request "https://api.weibo.com/2/statuses/mentions.json" 
                   self))

(defuntion show-emotions ()
  "获取微博官方表情的详细信息"
  (generic-request "https://api.weibo.com/2/emotions.json" 
                   self))

(defuntion show-followers-active (uid)
  "获取用户的活跃粉丝列表"
  (generic-request "https://api.weibo.com/2/friendships/followers/active.json" 
                   self (list (cons "uid" (format nil "~a" uid)))))

(defuntion show-user-timeline (uid)
  "获取某个用户最新发表的微博列表"
  (generic-request "https://api.weibo.com/2/statuses/user_timeline.json" 
                   self (list (cons "uid" (format nil "~a" uid)))))

(defuntion show-friends-timeline ()
  "获取当前登录用户及其所关注用户的最新微博"
  (generic-request "https://api.weibo.com/2/statuses/friends_timeline.json" 
                   self))

(defuntion show-home-timeline (page-size page)
  "获取当前登录用户及其所关注用户的最新微博"
  (generic-request "https://api.weibo.com/2/statuses/home_timeline.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size)))))

(defuntion show-friends-timeline-ids (page-size page)
  "获取当前登录用户及其所关注用户的最新微博的ID"
  (generic-request "https://api.weibo.com/2/statuses/friends_timeline/ids.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size)))))

(defuntion show-friends-in-common (page-size page uid)
  "获取两个用户之间的共同关注人列表"
  (generic-request "https://api.weibo.com/2/friendships/friends/in_common.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size))
                              (cons "uid" (format nil "~a" uid)))))


(defuntion show-friends-bilateral (page-size page uid)
  "获取用户的双向关注列表，即互粉列表"
  (generic-request "https://api.weibo.com/2/friendships/friends/bilateral.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size))
                              (cons "uid" (format nil "~a" uid)))))



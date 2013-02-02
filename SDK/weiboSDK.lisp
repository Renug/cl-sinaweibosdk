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
  "The OAuth2 The authorize interface"
  (with-slots (user-name password) self
    (multiple-value-bind (body-or-stream status-code  headers uri stream must-close reason-phrase)
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
			     :additional-headers 
			     `(("Referer" .  ,(format nil  "https://api.weibo.com/oauth2/authorize?response_type=code&redirect_uri=~a" *redirect_uri*))))
      (if (= 302 status-code)
	  (with-input-from-string (strm (puri:uri-query 
					 (puri:parse-uri 
					  (drakma:header-value 
					   :LOCATION headers))))
	    (cdr (assoc "code" (parse-key-value-pairs strm) :test #'equalp)))
	  (error (format nil "request code error:~a" status-code))))))

(defun request-token (self code)
  "The OAuth2 The access_token interface"
    (multiple-value-bind (body-or-stream)
      (drakma:http-request "https://api.weibo.com/oauth2/access_token"
                         :method :POST
                         :parameters `(("code". ,code) 
				       ("redirect_uri" . ,*redirect_uri*)
				       ("client_id" . ,*app-key*) 
				       ("client_secret" . ,*app-secret*) 
				       ("grant_type" . "authorization_code")))
      (json:decode-json-from-string 
       (if (typep body-or-stream 'string)
	   body-or-stream
	   (flexi-streams:octets-to-string 
	    body-or-stream :external-format :utf-8)))))
      

(defmethod login ((self SinaWeibo))
  (let ((return-values (request-token self (request-code self))))
    (with-slots (access-token expires-in remind-in uid) self
	  (setf access-token (cdr (assoc :ACCESS--TOKEN return-values)))
	  (setf expires-in (cdr (assoc :EXPIRES--IN return-values)))
	  (setf remind-in (cdr (assoc :REMIND--IN return-values)))
	  (setf uid (cdr (assoc :UID return-values))))))

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
                             must-close 
			     reason-phrase) (funcall #'(lambda () ,@body))
       (let* ((retval (flexi-streams:octets-to-string body-or-stream 
						      :external-format :utf-8)))
         (values retval reason-phrase status-code))))))

(defuntion update-status (text)
  "Post a new weibo"
  (generic-request "https://api.weibo.com/2/statuses/update.json" 
                    self (list (cons "status" text)) :POST))

(defuntion show-user-counts (uids)
  "Volume to obtain the user's number of fans, concerned about the number, the number of weibo"
  (generic-request "https://api.weibo.com/2/users/counts.json" 
                   self (list (cons "uids" (format nil "~{~a~^,~}" uids)))))

(defuntion show-user-friends (uid)
  "Get the user watchlist"
  (generic-request "https://api.weibo.com/2/friendships/friends.json" 
                   self (list (cons "uid" uid))))

(defuntion show-user-followers (uid)
  "Get the user's fan list"
  (generic-request "https://api.weibo.com/2/friendships/followers.json" 
                   self (list (cons "uid" (format nil "~a" uid)))))

(defuntion show-public-timeline (count)
  "Return the latest 200 public weibo return incomplete results in real time"
  (generic-request "https://api.weibo.com/2/statuses/public_timeline.json" 
                   self (list (cons "count" (format nil "~a" count)))))

(defuntion show-mentions ()
  "Get the latest mentioned weibo list of the logged-on user"
  (generic-request "https://api.weibo.com/2/statuses/mentions.json" 
                   self))

(defuntion show-emotions ()
  "Get weibo official expressions"
  (generic-request "https://api.weibo.com/2/emotions.json" 
                   self))

(defuntion show-followers-active (uid)
  "Get active fan list"
  (generic-request "https://api.weibo.com/2/friendships/followers/active.json" 
                   self (list (cons "uid" (format nil "~a" uid)))))

(defuntion show-user-timeline (uid)
  "Access to a recently published list of weibo"
  (generic-request "https://api.weibo.com/2/statuses/user_timeline.json" 
                   self (list (cons "uid" (format nil "~a" uid)))))

(defuntion show-friends-timeline ()
  "Get the current logged-on user, and its latest concern weibo"
  (generic-request "https://api.weibo.com/2/statuses/friends_timeline.json" 
                   self))

(defuntion show-home-timeline (page-size page)
  "Get the current logged-on user, and its latest concern weibo"
  (generic-request "https://api.weibo.com/2/statuses/home_timeline.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size)))))

(defuntion show-friends-timeline-ids (page-size page)
  "he latest ID weiboGet the current logged-on user, its focus on users"
  (generic-request "https://api.weibo.com/2/statuses/friends_timeline/ids.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size)))))

(defuntion show-friends-in-common (page-size page uid)
  "Get between two users of common concern list"
  (generic-request "https://api.weibo.com/2/friendships/friends/in_common.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size))
                              (cons "uid" (format nil "~a" uid)))))


(defuntion show-friends-bilateral (page-size page uid)
  "Get the the user bidirectional concern list, that mutual powder list"
  (generic-request "https://api.weibo.com/2/friendships/friends/bilateral.json" 
                   self (list (cons "page" (format nil "~a" page))
                              (cons "count" (format nil "~a" page-size))
                              (cons "uid" (format nil "~a" uid)))))



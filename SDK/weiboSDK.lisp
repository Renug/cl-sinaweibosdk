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
    (multiple-value-bind (body-or-stream status-code headers)
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
      (if (equal 302 status-code)
	  (with-input-from-string (strm (puri:uri-query 
					 (puri:parse-uri 
					  (drakma:header-value 
					   :LOCATION headers))))
	    (cdr (assoc "code" (parse-key-value-pairs strm) :test #'equalp)))
	  (error 'request-code-error 
                 :status-code status-code 
                 :content-string body-or-stream)))))

(defun request-token (code)
  "The OAuth2 The access_token interface"
    (multiple-value-bind (body-or-stream status-code)
      (drakma:http-request "https://api.weibo.com/oauth2/access_token"
                         :method :POST
                         :parameters `(("code". ,code) 
				       ("redirect_uri" . ,*redirect_uri*)
				       ("client_id" . ,*app-key*) 
				       ("client_secret" . ,*app-secret*) 
				       ("grant_type" . "authorization_code")))
      (if (equal 200 status-code)
        (json:decode-json-from-string 
         (if (typep body-or-stream 'string)
	   body-or-stream
	   (flexi-streams:octets-to-string 
	    body-or-stream :external-format :utf-8)))
        (error 'request-token-error 
               :status-code status-code 
               :content-string body-or-stream))))
      

(defmethod login ((self SinaWeibo))
  (let ((return-values (request-token (request-code self))))
    (with-slots (access-token expires-in remind-in uid) self
	  (setf access-token (cdr (assoc :ACCESS--TOKEN return-values)))
	  (setf expires-in (cdr (assoc :EXPIRES--IN return-values)))
	  (setf remind-in (cdr (assoc :REMIND--IN return-values)))
	  (setf uid (cdr (assoc :UID return-values))))))

(defun generic-request (url weibo &optional http-paraments (method :GET))
  (with-slots (access-token) weibo
    (if (null access-token) 
	(error 'token-null-error)
        (drakma:http-request url
                           :method method
                           :parameters (append `(("access_token" . ,access-token))
                                               http-paraments)))))

(defmacro defuntion (function-name paraments documentation &body body)
  `(progn 
     (defgeneric ,function-name (self ,@paraments)
       (:documentation ,documentation))
     (defmethod ,function-name ((self SinaWeibo) ,@paraments)
       (multiple-value-bind (body-or-stream 
                             status-code) (funcall #'(lambda () ,@body))
         (let* ((retval (flexi-streams:octets-to-string body-or-stream 
                                                          :external-format :utf-8)))
           (if (equal 200 status-code)
             (values retval status-code)
           (error 'sinaWeibosdk-error 
                  :status-code status-code 
                  :content-string retval)))))))

(defuntion update-status (text)
  "Post a new weibo"
  (generic-request "https://api.weibo.com/2/statuses/update.json" 
                    self `(("status" . ,text)) :POST))

(defuntion show-user-counts (uids)
  "Batch get the user's number of fans, oncerned about the number of people, the number of weibo"
  (generic-request "https://api.weibo.com/2/users/counts.json" 
                   self `(("uids" . ,(format nil "~{~a~^,~}" uids)))))

(defuntion show-user-friends (uid)
  "Get the user's watchlist"
  (generic-request "https://api.weibo.com/2/friendships/friends.json" 
                   self `(("uid" . ,uid))))

(defuntion show-user-followers (uid)
  "Get the user's fan list"
  (generic-request "https://api.weibo.com/2/friendships/followers.json" 
                   self `(("uid" . ,(format nil "~a" uid)))))

(defuntion show-public-timeline (count)
  "Return the latest 200 public weibo ,but return results are not completely in real time."
  (generic-request "https://api.weibo.com/2/statuses/public_timeline.json" 
                   self `(("count" . ,(format nil "~a" count)))))

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
                   self `(("uid" . ,(format nil "~a" uid)))))

(defuntion show-user-timeline (uid)
  "Access to a recently published list of weibo"
  (generic-request "https://api.weibo.com/2/statuses/user_timeline.json" 
                   self `(("uid" . ,(format nil "~a" uid)))))

(defuntion show-friends-timeline ()
  "Get the current logged-on user, and its latest concern weibo"
  (generic-request "https://api.weibo.com/2/statuses/friends_timeline.json" 
                   self))

(defuntion show-home-timeline (page-size page)
  "Get the current logged-on user, and its latest concern weibo"
  (generic-request "https://api.weibo.com/2/statuses/home_timeline.json" 
                   self `(("page" . ,(format nil "~a" page))
                              ("count" . ,(format nil "~a" page-size)))))

(defuntion show-friends-timeline-ids (page-size page)
  "Get the current logged-in user and his attention to the user ID of the latest weibo"
  (generic-request "https://api.weibo.com/2/statuses/friends_timeline/ids.json" 
                   self `(("page" . ,(format nil "~a" page))
                          ("count" . ,(format nil "~a" page-size)))))

(defuntion show-friends-in-common (page-size page uid)
  "Get between two users of common concern list"
  (generic-request "https://api.weibo.com/2/friendships/friends/in_common.json" 
                   self `(("page" . ,(format nil "~a" page))
                          ("count" . ,(format nil "~a" page-size))
                          ("uid" . ,(format nil "~a" uid)))))


(defuntion show-friends-bilateral (page-size page uid)
  "Get the the user bidirectional concern list, that mutual powder list"
  (generic-request "https://api.weibo.com/2/friendships/friends/bilateral.json" 
                   self `(("page" . ,(format nil "~a" page))
                          ("count" . ,(format nil "~a" page-size))
                          ("uid" . ,(format nil "~a" uid)))))



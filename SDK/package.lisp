(in-package :weibo-system)

(defpackage SinaWeiboSDK
  (:use #:cl 
	#:drakma 
	#:cl-json)
  (:export #:SinaWeibo
	   #:login
	   #:update-status
	   #:show-user-counts
	   #:show-user-friends
	   #:show-user-followers
	   #:show-public-timeline
	   #:show-mentions
	   #:show-emotions
	   #:show-followers-active
	   #:show-user-timeline
	   #:show-friends-timeline
	   #:show-home-timeline
	   #:show-friends-timeline-ids
	   #:show-friends-in-common
	   #:show-friends-bilateral
	   #:*app-key*
	   #:*app-secret*
	   #:*redirect_uri*))
(in-package :weibotest-system)

(defpackage weiboSDK.test
  (:use #:cl #:SinaWeiboSDK #:rt)
  (:export #:run-tests
	   #:clean-tests))

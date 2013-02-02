(in-package :weibotest-system)

(defpackage weiboSDK.test
  (:use #:cl #:SinaWeiboSDK #:REGRESSION-TEST)
  (:export #:run-tests
	   #:clean-tests))

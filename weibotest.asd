(defpackage :weibotest-system (:use #:asdf #:cl))
(in-package :weibotest-system)

(defsystem weibotest
  :name "Weibo Test"
  :version "0.1"
  :author "Rannger L"
  :depends-on ("weibo" "rt")
  :components ((:file "test/package")
           (:file "test/weibo-test" :depends-on ("test/package"))))

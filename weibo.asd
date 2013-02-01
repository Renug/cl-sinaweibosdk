;;;; -*- Lisp -*-

(defpackage :weibo-system (:use #:asdf #:cl))
(in-package :weibo-system)

(defsystem weibo
  :name "Sina Weibo"
  :version "0.1"
  :author "Rannger L"
  :depends-on ("drakma" "cl-json")
  :components ((:file "SDK/package")
           (:file "SDK/pkvpl" :depends-on ("SDK/package"))
           (:file "SDK/weiboSDK" :depends-on ("SDK/package" "SDK/pkvpl"))))
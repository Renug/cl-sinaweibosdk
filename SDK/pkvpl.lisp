;;;
;;; $Header: /home/gene/library/website/docsrc/pkvpl/RCS/pkvpl.lisp,v 395.1 2008/04/20 17:25:50 gene Exp $
;;;
;;; Copyright (C) 2004  Gene Michael Stover.  All rights reserved.
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 2.1 of the
;;; License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;; USA
;;;

;;;
;;; by Gene Michael Stover
;;; created Sunday, 24  October 2004
;;; updated Sunday,  8 November 2004
;;;
;;; Documentation is at <http://lisp-p.org/pkvpl/>.
;;;
(in-package :SinaWeiboSDK)

(defun parse-key-value-pairs (strm &key (pairsep '(#\;))
				   (nvsep '(#\=)) (white-space ()))
  (declare (type stream strm) (type list pairsep nvsep white-space))
  (assert (input-stream-p strm))
  (labels
      ;; Return the next character or NIL.
      ((next-char ()
		  (peek-char nil strm nil nil))
       ;; Return the next token.  It might be the symbols
       ;; PAIRSEP or NVSEP, or the token as a string.  Return
       ;; NIL on end of input.
       (next-token ()
		   ;; Consume white-space
		   (loop while (member (next-char) white-space)
			 do (read-char strm))
		   (let ((retval
			  (cond ((null (next-char))
				 nil)	; end of input
				((member (next-char) pairsep)
				 (read-char strm)
				 'pairsep)
				((member (next-char) nvsep)
				 (read-char strm)
				 'nvsep)
				(t ; collect chars & return next string
				 (do ((lst () (cons (read-char strm) lst)))
				     ((or (null (next-char))
					  (member (next-char) pairsep)
					  (member (next-char) nvsep)
					  (member (next-char) white-space))
				      (coerce (nreverse lst) 'string)))))))
		     ;; (format t "~&next-token => ~S" retval)
		     retval))
       (next-name ()
		  ;; Skip over the separators between name=value pairs,
		  ;; returning the next name or Nil on end-of-input.
		  (let ((retval 
			 (loop for x = (next-token)
			       while (eq x 'pairsep)
			       finally (return x))))
		    ;; (format t "~&next-name => ~S" retval)
		    retval))
       (next-pair ()
		  ;; Skip pair separators
		  (let* ((name (next-name))
			 (sep (next-token))
			 (value (next-token)))
		    (and (stringp name)
			 (eq sep 'nvsep)
			 (stringp value)
			 (cons name value)))))
  (loop for pair = (next-pair)
	while pair
	collect pair)))

(defvar *tests* () "List of test functions")

(defmacro deftest (name &rest args-doc-body)
  `(progn
     (setq *tests* (append *tests* (list ',name)))
     (defun ,name ,@args-doc-body)))

(defun check (&optional (is-verbose t) (lst *tests*))
  "Executes all the tests in LST until they all execute or one of
them fails."
  (declare (type list lst))
  (let ((count 0))
    (labels 
	((failed (test)
		 (declare (type (or symbol function) test))
		 (when is-verbose
		   (format t "~&~2D%  ~A"
			   (floor (* (/ count (length lst)) 100))
			   test)
		   (force-output))
		 (let ((x (funcall test)))
		   (unless x
		     (if is-verbose
			 (format t "~&*** failed ***")
		       (format t "~&~A~&*** failed ***" test))
		     (force-output))
		   (incf count)
		   (null x))))
  (null (find-if #'failed lst)))))

(deftest test0000 ()
  "Null test.  Always succeeds."
  'test0000)

(deftest test0001 ()
  "Test parse-key-value-pairs on an empty input.  It should return
an empty list."
  (endp (with-input-from-string (strm "")
	  (parse-key-value-pairs strm))))

(deftest test0002 ()
  "Test parse-key-value-pairs on a simple input."
  (equal (with-input-from-string (strm "a=1")
	   (parse-key-value-pairs strm))
	 '(("a" . "1"))))

(deftest test0003 ()
  "Test parse-key-value-pairs on a slightly more complicated input."
  (equal (with-input-from-string (strm "a=1;b=2")
	   (parse-key-value-pairs strm))
	 '(("a" . "1") ("b" . "2"))))

(deftest test0004 ()
  "Like test0003 except with a different pair separator."
  (equal (with-input-from-string (strm "a=1&b=2")
	   (parse-key-value-pairs strm :pairsep '(#\&)))
	 '(("a" . "1") ("b" . "2"))))

(deftest test0005 ()
  "Like test0003 except that we inject some white-space.  We
don't use the white-space keyword argument, so the white-space
characters should find their way into the names & values."
  (equal (with-input-from-string (strm " a = 1 ; b = 2 ")
	   (parse-key-value-pairs strm))
	 '((" a " . " 1 ") (" b " . " 2 "))))

(deftest test0006 ()
  "Like test0005 except that we use the white-space keyword,
so the white-space characters should be removed from the output
we get."
  (equal (with-input-from-string (strm " a = 1 ; b = 2 ")
	   (parse-key-value-pairs strm :white-space '(#\Space)))
	 '(("a" . "1") ("b" . "2"))))

(deftest test0007 ()
  "Test parse-key-value-pairs allowing with ; and , to separate
pairs, like a sloppily formed HTTP Cookie might have.  Also has
a doubled separator character, which parse-key-value-pairs
should ignore."
  (equal (with-input-from-string (strm "a=1;b=2,c=3,d=4;;e=5")
	   (parse-key-value-pairs strm :pairsep '(#\; #\,)))
	 '(("a" . "1") ("b" . "2") ("c" . "3") ("d" . "4") ("e" . "5"))))

(deftest test0008 ()
  "Like test0007 except that it uses multi-character keys
& values."
  (equal
   (with-input-from-string
       (strm "a=1;bb=22,ccc=333,dddd=4444;;eeeee=55555")
     (parse-key-value-pairs strm :pairsep '(#\; #\,)))
   '(("a" . "1") ("bb" . "22") ("ccc" . "333") ("dddd" . "4444")
     ("eeeee" . "55555"))))

(deftest test0009 ()
  "Like test0008 except that it has some white-space
characters at the beginning of the string."
  (equal
   (with-input-from-string
       (strm "    a=1;bb=22,ccc=333,dddd=4444;;eeeee=55555")
     (parse-key-value-pairs strm
			    :pairsep '(#\; #\,)
			    :white-space '(#\Space #\Tab #\Newline)))
   '(("a" . "1") ("bb" . "22") ("ccc" . "333") ("dddd" . "4444")
     ("eeeee" . "55555"))))

(deftest test0010 ()
  "Like test0008 except that it has some white-space
characters at the end of the string."
  (equal
   (with-input-from-string
       (strm "a=1;bb=22,ccc=333,dddd=4444;;eeeee=55555    ")
     (parse-key-value-pairs strm
			    :pairsep '(#\; #\,)
			    :white-space '(#\Space #\Tab #\Newline)))
   '(("a" . "1") ("bb" . "22") ("ccc" . "333") ("dddd" . "4444")
     ("eeeee" . "55555"))))

(deftest test0011 ()
  "Test parse-key-value-pairs on a string that separates
pairs with newlines."
  (equal
   (with-input-from-string (strm (format nil "a=1~%bb=22~%ccc=333  ~% "))
     (parse-key-value-pairs strm
			    :pairsep '(#\Newline)
			    :white-space '(#\Space #\Tab)))
   '(("a" . "1") ("bb" . "22") ("ccc" . "333"))))

;;; --- end of file ---
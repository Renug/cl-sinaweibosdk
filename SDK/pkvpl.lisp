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


;;; --- end of file ---
;;; excorgorate.el --- Show Outlook calendar in Org Agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <rprospero@gmail.com>
;; Keywords: comm, calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;This latching code was taken, under the Unlicence, from
;;https://github.com/skeeto/elisp-latch/blob/master/latch.el
;;

(eval-when-compile
  (require 'cl)
  (require 'eieio))

(defclass excorgorate--latch ()
  ((process :initform (start-process "latch" nil nil))
   (value :initform nil))
  :documentation "A blocking latch that can be used any number of times.")

(defmethod excorgorate--wait ((latch latch) &optional timeout)
  "Blocking wait on LATCH for a corresponding `notify', returning
the value passed by the notification. Wait at most TIMEOUT
seconds (float allowed), returning nil if the timeout was reached
with no input. The Emacs display will not update during this
period but I/O and timers will continue to run."
  (accept-process-output (slot-value excorgorate--latch 'process) timeout)
  (slot-value excorgorate--latch 'value))

(defmethod excorgorate--notify ((latch latch) &optional value)
  "Release all execution contexts waiting on LATCH, passing them VALUE."
  (setf (slot-value excorgorate--latch 'value) value)
  (process-send-string (slot-value excorgorate--latch 'process) "\n"))

(defmethod excorgorate--destroy ((latch latch))
  "Destroy a latch, since they can't be fully memory managed."
  (ignore-errors
(delete-process (slot-value excorgorate--latch 'process))))

(defun excorgorate--make-latch ()
  "Make a latch which can be used any number of times. It must be
`destroy'ed when no longer used, because the underlying process
will not be garbage collected."
  (make-instance 'excorgorate--latch))

(defun excorgorate--destroy-all-latches ()
  "Destroy all known latches."
  (loop for process in (process-list)
        when (string-match-p "latch\\(<[0-9]+>\\)?" (process-name process))
        do (delete-process process)))

;; One-use latches

(defclass excorgorate--one-time-latch (excorgorate--latch)
  ()
  :documentation "A latch that is destroyed automatically after one use.")

(defmethod excorgorate--wait :after ((latch excorgorate--one-time-latch) &optional timeout)
  (excorgorate--destroy latch))

(defun excorgorate--make-one-time-latch ()
  "Make a latch that is destroyed automatically after a single use."
  (make-instance 'excorgorate--one-time-latch))

;; Promises

(defclass excorgorate--promise ()
  ((latch :initform (excorgorate--make-one-time-latch))
   (delivered :initform nil)
   (value :initform nil))
  :documentation "Promise built on top of a one-time latch.")

(defmethod excorgorate--deliver ((promise excorgorate--promise) value)
  "Deliver a VALUE to PROMISE, releasing any execution contexts
waiting on it."
  (if (slot-value promise 'delivered)
      (error "Promise has already been delivered.")
    (setf (slot-value promise 'value) value)
    (setf (slot-value promise 'delivered) t)
    (excorgorate--notify (slot-value promise 'latch) value)))

(defmethod retrieve ((promise excorgorate--promise))
  "Resolve the value for PROMISE, blocking if necessary. The
Emacs display will freeze, but I/O and timers will continue to
run."
  (if (slot-value promise 'delivered)
      (slot-value promise 'value)
    (excorgorate--wait (slot-value promise 'latch))))

(defun excorgorate--make-promise ()
  "Make a new, unresolved promise. `deliver' a value to it so
that it can be `retrieve'd."
(make-instance 'excorgorate--promise))

;;  Here's the original code within this project

(defgroup excorgorate nil "A method for loading Outlook calendars into the Org Agenda"
  :group 'excorporate)

(defcustom excorgorate-default-account user-mail-address
  "The default account to pull contacts from."
  :type 'string
  :group 'excorgorate)


(defun excorgorate-meetings (&optional mark)
  "Add the first outlook meeting for a given day to the Agenda.

This function takes an optional MARK argument, because
\"org-mode\" seems to pass one.  I have not idea what it means
and I'm currently ignoring it."
  (let
      ((result
	(if exco--connections
	    (letrec
		((meeting (excorgorate-get-meetings date))
		 (ident (car meeting))
		 (resp (cadr meeting)))
	      (if meeting
		  (exco-calendar-item-iterate
		   resp
		   (lambda (subject start end loc main opt)
		     (message "%s %s"
			      (excorgorate-relative-date-format
			       start end (encode-time 0 0 0 (cadr date) (car date) (caddr date)))
			      subject))))))))
    (destroy-all-latches)
    result))


(defun excorgorate-relative-date-format (begin end local)
  "Find the correct agenda formatting for a date in a a range.

Given a date range from BEGIN to END and a specific day LOCAL,
return the correct \"org-agenda\" representation of this date in the
range."
  (message "%s" (decode-time end) (current-time-zone))
  (if
   (and (< (car begin) (car local))
	 (> (car end) (car local)))
    ""
    (format "%s--%s"
	      (format "%2d:%02d"
		      (caddr (decode-time begin))
		      (cadr (decode-time begin)))
	      (format "%2d:%02d"
		      (caddr (decode-time end))
		      (cadr (decode-time end))))))

(defun excorgorate-get-meetings (date)
  "Get all of the outlook events on a given DATE."
  (lexical-let
      ((promise (excorgorate--make-promise))
       (month (car date))
       (day (cadr date))
       (year (caddr date)))
    (exco-get-meetings-for-day
     excorgorate-default-account
     month day year
     (lambda (ident resp) (excorgorate--deliver promise (list ident resp))))
    (excorgorate--retrieve promise)))


(provide 'excorgorate)
;;; excorgorate.el ends here

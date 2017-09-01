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


(add-to-list 'load-path "/home/adam/.emacs.d/scripts")

(require 'latch)

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
		       subject)))))))


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
      ((promise (make-promise))
       (month (car date))
       (day (cadr date))
       (year (caddr date)))
    (exco-get-meetings-for-day
     excorgorate-default-account
     month day year
     (lambda (ident resp) (deliver promise (list ident resp))))
    (retrieve promise)))


(provide 'excorgorate)
;;; excorgorate.el ends here

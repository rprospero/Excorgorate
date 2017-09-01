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


(defun excorporate-first-meeting (&optional mark)
  "Add the first outlook meeting for a given day to the Agenda.

This function takes an optional MARK argument, because
\"org-mode\" seems to pass one.  I have not idea what it means
and I'm currently ignoring it."
  (if exco--connections
      (let
	  ((meeting (car-safe (adam-get-meetings date))))
	(if meeting
	    (format
	     "%s %s"

	     (if (plist-get meeting 'all-day)
		""
	       (adam-relative-date-format
		(plist-get meeting 'start)
		(plist-get meeting 'stop)
		date))
	     (plist-get meeting 'subject))))))

(defun excorporate-second-meeting (&optional mark)
  "Add the second outlook meeting for a given day to the Agenda.

This function takes an optional MARK argument, because
\"org-mode\" seems to pass one.  I have not idea what it means
and I'm currently ignoring it."
  (if exco--connections
      (let
	  ((meeting (car-safe (cdr-safe (adam-get-meetings date)))))
	(if meeting
	    (format
	     "%s %s"

	     (if (plist-get meeting 'all-day)
		""
	       (adam-relative-date-format
		(plist-get meeting 'start)
		(plist-get meeting 'stop)
		date))
	     (plist-get meeting 'subject))))))

(defun adam-relative-date-format (begin end local)
     (pcase-let
	 ((`(,month ,day ,year) local)
       	  (`(,es ,em ,eh ,eD ,eM ,eY) begin)
       	  (`(,bs ,bm ,bh ,bD ,bM ,bY) end))
       (cond
	((and (= day eD) (= month eM) (= year eY)
	      (= day bD) (= month bM) (= year bY))
	 (format "%2d:%02d--%2d:%02d" bh bm eh em))
	((and (= day eD) (= month eM) (= year eY))
	 (format "%2d:%02d" eh em))
	((and (= day bD) (= month bM) (= year bY))
	 (format "%2d:%02d" bh bm))
       	 "")))

(defun adam-parse-calendar-item (item)
  (setq result '(all-day ()))
  (dolist (key item result)
    (if (listp key)
	(cond
	 ((eq 'Subject (car key))
	  (setq result
		(plist-put result 'subject (cdr key))))
	 ((eq 'End (car key))
	  (setq result
		(plist-put result 'stop
			   (decode-time (date-to-time (cdr key))))))
	 ((eq 'IsAllDayEvent (car key))
	  (setq result
		(plist-put result 'all-day (cdr key))))
	 ((eq 'Start (car key))
	  (setq result
		(plist-put result 'start
			   (decode-time (date-to-time (cdr key))))))))))

(defun adam-get-meetings (date)
  (lexical-let
      ((promise (make-promise))
       (month (car date))
       (day (cadr date))
       (year (caddr date)))
    (exco-get-meetings-for-day
     excorgorate-default-account
     month day year
     (lambda (ident resp) (deliver promise resp)))
     (-filter
      (lambda (x)
	(pcase-let
	    ((`(,second ,minute ,hour ,date)
	      (plist-get x 'stop)))
	  (not
	   (and (eq date day) (eq hour 0) (eq minute 0)))))
      (mapcar #'adam-parse-calendar-item
	      (cdar (last (car (last (cdr (cadaar (retrieve promise)))))))))))


(provide 'excorgorate)
;;; excorgorate.el ends here

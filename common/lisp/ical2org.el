;;; ics2org.el -- convert icalendar to org

;; Copyright (C) 2010, 2011 Michael Markert
;; Copytight (C) 2012 Ludovic Stordeur
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Author: Ludovic Stordeur <ludovic@okazoo.eu>
;; Created: 2010/12/29
;; Version: 0.3.1
;; Keywords: org, calendar

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;;
;; Installation:
;;
;;   (require 'ical2org)
;;

;;; Code:

(require 'icalendar)
(require 'org)
(eval-when-compile
  (require 'cl))

(defconst ical2org/version "0.3.1")

(defgroup ical2org nil
  "Convert iCalendar files to orgmode files."
  :link '(url-link :tag "Homepage" "http://github.com/cofi/ical2org")
  :group 'calendar
  :prefix "ical2org/")

(defcustom ical2org/event-format
"* {SUMMARY}
{LINES}{DESCRIPTION}
"
  "String used to format an event.
Syntax is {FIELD} valid values for FIELD are: SUMMARY, LOCATION, TIME, URL,
DESCRIPTION, ORGANIZER, CATEGORY.  Namely the slots of the `ical2org/event'
struct (capitalized)."
  :type '(string))

(defcustom ical2org/event-line-format
  '(
    (org-timestr . "{org-timestr}\n")
    (location . "Location: {location}\n")
    (organizer . "Organizer: {organizer}\n")
    (url . "Url: {url}\n")
    )
  "Syntax and order of fields for the special {LINES} field of `ical2org/event-format'"
  :group 'ical2org
  :type '(list (cons symbol string)
	       (cons symbol string)
	       (cons symbol string)
	       (cons symbol string)))

(defcustom ical2org/category-separator ":"
  "String used to separate multiple categories."
  :type '(string))

(defcustom ical2org/completing-read #'ido-completing-read
  "Function used for completing read.
Has to be compatible to `completing-read'."
  :type '(function))


(defun ical2org/import-buffer-to-buffer (bufin bufout nosave)
  "Convert ical events from buffer `BUFIN' to buffer `BUFOUT'."

  (interactive "bIn: \nBOut: ")

  (let ((events (ical2org/parse-ical-events bufin)))

    (save-current-buffer
      (set-buffer bufout)
      (goto-char (point-max))
      (newline)
      (dolist (e events)
        (insert (ical2org/format-event e)))
      (unless nosave
        (save-buffer))
      (org-mode))))


(defun ical2org/import-file-to-buffer (fname bufout &optional nosave)
  "Import ical events from file `FNAME' to the buffer `BUFOUT'."

  (interactive "fFile to convert: \nP")

  (with-temp-buffer
    (insert-file-contents (expand-file-name fname))
    (ical2org/import-buffer-to-buffer (current-buffer) bufout nosave)))


(defun ical2org/import-file-to-file (fname outfile &optional nosave)
  "Import ical events from file `FNAME' to `OUTFILE' and save unless `NOSAVE'
is non-nil."

  (interactive "fFile to convert: \nFSave as: \nP")

  (save-current-buffer
    (find-file outfile)
    (ical2org/import-file-to-buffer fname (current-buffer) nosave)))


(defun ical2org/convert-file (fname outfile &optional nosave)
  "Convert ical events from file `FNAME' to `OUTFILE' and save unless `NOSAVE'
is non-nil."

  (interactive "fFile to convert: \nFSave as: \nP")

  (save-current-buffer
      (find-file outfile)
      (goto-char (point-max))
      (erase-buffer)
      (ical2org/import-file-to-buffer fname (current-buffer) nosave)))

;; private

;; output formatting
(defun ical2org/format-event-lines (event)
  "Replace formatstrings of ical2org/event-line-format with slots of `EVENT'."
  (mapconcat (lambda (line-spec)
	       (let* ((fq-symbol (intern (concat "ical2org/event-" (symbol-name (car line-spec)))))
		      (symbol-string (symbol-name (car line-spec)))
		      (enclosed-symbol-string (concat "{" symbol-string "}"))
		      (fq-symbol-value (funcall (symbol-function fq-symbol) event)))

		 (if (string= fq-symbol-value "")
		     ""
		   (replace-regexp-in-string
		    enclosed-symbol-string
		    (lambda (z) fq-symbol-value)
		    (cdr line-spec)))))
	     ical2org/event-line-format
	     ""))


(defun ical2org/format-event (event)
  "Replace formatstrings with slots of `EVENT'."
  (replace-regexp-in-string
   "{.*?}"
   (lambda (z)
     (let ((subst (assoc z
			 `(("{SUMMARY}"     . ,(ical2org/event-summary event))
			   ("{LINES}"	     . ,(ical2org/format-event-lines event))
			   ("{DESCRIPTION}" . ,(ical2org/event-description event))
			   ("{CATEGORY}"    . ,(mapconcat 'identity
							  (ical2org/event-category event)
							  ical2org/category-separator)
			    )))))
       (if subst
	   (cdr subst)
	 "")))
   ical2org/event-format
   t t))


(defun ical2org/org-recurrent (event start-decoded start-time end-time)
  "Wrap `icalendar--convert-recurring-to-diary' diary in an org timestamp."
  (let* ((diary-entry (icalendar--convert-recurring-to-diary event
							     start-decoded
							     start-time
							     end-time))

	 ;; DIRTY HACK: `icalendar--convert-recurring-to-diary' introduces a
	 ;; trailing whitespace which Org seems not able to handle (making
	 ;; recurring events not appearing in the Agenda view). I think this
	 ;; issue should be addressed in Org (for robustness considerations)
	 ;; but currently, let's fix it here;
	 (diary-entry (replace-regexp-in-string " $" "" diary-entry)))
    (format "<%s>" diary-entry)))

(defun ical2org/org-timestamp (start end)
  "Format `START' and `END' as `org-time-stamp'."
  (let ((start-time (nth 2 start))
        (end-time (nth 2 end))
        (start (car start))
        (end (car end)))
    (if end
        (format "%s--%s" (ical2org/org-time-fmt start start-time)
                (ical2org/org-time-fmt end end-time))
      (if start
          (ical2org/org-time-fmt start start-time)))))

(defun ical2org/org-time-fmt (time &optional with-hm)
  "Format `TIME' as `org-time-stamp', if `WITH-HM' is non-nil included hh:mm.
`TIME' is an decoded time as returned from `decode-time'."
  (let ((fmt (if with-hm
                 (cdr org-time-stamp-formats)
               (car org-time-stamp-formats)))
        (encoded-time (apply 'encode-time time)))
    (format-time-string fmt encoded-time)))

;; entry processing

(defstruct ical2org/event
  (summary "")
  (location "")
  (org-timestr "")
  (url "")
  (description "")
  (organizer "")
  (category '()))

(defun ics2org/datetime (property event zone-map)
  "Return datetime values for `PROPERTY' of `EVENT' with `ZONE-MAP'.
Return a triple of (decoded isodate time).
Where `decoded' is a decoded datetime,
      `isodate' a date as yy mm dd string,
      `time' a time as hh:mm string."
  (let* ((dt (icalendar--get-event-property event property))
         (zone (icalendar--find-time-zone
                (icalendar--get-event-property-attributes event property) zone-map))
         (decoded (icalendar--decode-isodatetime dt nil zone-map)))
    (list decoded
          (icalendar--datetime-to-iso-date decoded)
          (ignore-errors
            (icalendar--datetime-to-colontime decoded)))))

(defun ical2org/get-property (event property &optional default clean)
  "Return `PROPERTY' of `EVENT' or `DEFAULT'."
  (let ((prop (or (icalendar--get-event-property event property)
                 default)))
    (if clean
        (icalendar--convert-string-for-import prop)
      prop)))

(defun ical2org/get-org-timestr (event zone-map)
  "Return org-timestring for `EVENT' with `ZONE-MAP'."
  (let* ((start (ics2org/datetime 'DTSTART event zone-map))
         (start-day (nth 1 start))
         (start-time (nth 2 start))
         (end (ics2org/datetime 'DTEND event zone-map))
         (end-day (or (nth 1 end) start-day))
         (end-time (or (nth 2 end) start-time))
         (rrule (icalendar--get-event-property event 'RRULE))
         (rdate (icalendar--get-event-property event 'RDATE))
         (duration (icalendar--get-event-property event 'DURATION)))
    (when duration
      (let ((new-end (icalendar--add-decoded-times
                      (car start)
                      (icalendar--decode-isoduration duration))))
        (setq end-day (icalendar--datetime-to-iso-date new-end))
        (setq end-time (icalendar--datetime-to-colontime new-end))
        (setq end (list new-end end-day end-time))))

    (cond
     (rrule (ical2org/org-recurrent event (car start) start-time end-time))
     (t (ical2org/org-timestamp start end)))))

(defun ical2org/ical-event->event (ical-event zone-map)
  "Make an `ical2org/event' object from the given `ICAL-EVENT'
using the timezone map `ZONE-MAP'."
  (let ((summary (ical2org/get-property ical-event 'SUMMARY "" t))
        (location (ical2org/get-property ical-event 'LOCATION "" t))
        (org-timestr (ical2org/get-org-timestr ical-event zone-map))
        (url (ical2org/get-property ical-event 'URL ""))
        (description (ical2org/get-property ical-event 'DESCRIPTION "" t))
        (organizer (ical2org/get-property ical-event 'ORGANIZER "" t))
        (category
	 (split-string (ical2org/get-property ical-event 'CATEGORIES "" t)
		       "," t)))
    (make-ical2org/event :summary summary
                         :location location
                         :org-timestr org-timestr
                         :url url
                         :description description
                         :organizer organizer
                         :category category)))

(defun ical2org/parse-ical-events (buffer)
  "Find all events in icalendar `BUFFER' and return them as a
list of `ical2org/event's."
  (save-current-buffer
    (set-buffer (icalendar--get-unfolded-buffer buffer))
    (goto-char (point-min))

    (let* ((ical-elements (icalendar--read-element nil nil))
	   (ical-events (icalendar--all-events ical-elements))
	   (zone-map (icalendar--convert-all-timezones ical-elements)))
      (mapcar (lambda (ical-event)
		(ical2org/ical-event->event ical-event zone-map))
	      ical-events))))

(provide 'ical2org)

;;; ical2org.el ends here

;;; dolmenwood-calendar.el --- A calendar for the Dolmenwood RPG -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 drpaepper
;;
;; Author: drpaepper <drpaepper@drpaepper.xyz>
;; Maintainer: drpaepper <drpaepper@drpaepper.xyz>
;; Created: August 10, 2026
;; Modified: August 10, 2026
;; Version: 0.0.1
;; Keywords: calendar
;; Human-Keywords: calendar, rpg
;; Homepage: https://github.com/drpaepper/xxx
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This collection of functions provides various calendar tools for
;; the Dolmenwood RPG. This can be helpful for creating org-agenda
;; view for running / playing various RPGs. All dates are referenced
;; from `dolmenwood-calendar-current-game-date' and the current date.
;; In otherwords, the current date in the real world will always
;; align to the in-game current date.

;;; Code:

(require 'calendar)

(defconst dolmenwood-calendar-day-name-array
  ["Colly" "Chime" "Hayme" "Moot" "Frisk" "Eggfast" "Sunning"]
  "Array of the strings giving names of the Dolmenwood days.")

(defconst dolmenwood-calendar-month-name-array
  ["Grimwold" "Lymewald" "Haggryme" "Symswald" "Harchment"
   "Iggwyld" "Chysting" "Lillipythe" "Haelhold" "Reedwryme"
   "Obthryme" "Braghold"]
  "Array of the strings giving names of the Dolmenwood months.")

(defconst dolmenwood-calendar-wysendays-in-month-array
  [["Hanglemas" "Dyboll's Day"]
   []
   ["Yarl's Day" "The Day of Virgins"]
   ["Symswald" "Hopfast"]
   ["Smithing"]
   ["Shortening" "Longshank's Day"]
   ["Bradging" "Copsewallow" "Chalice"]
   ["Old Dobey's Day"]
   []
   ["Shub's Day" "Druden Day"]
   []
   ["The Day of Doors" "Dolmenday"]])

(defvar dolmenwood-calendar-current-game-date nil
  "The current date within the Dolmenwood game.
User-defined variable representing the current date within
the game being played.")

(defun dolmenwood-calendar-last-day-of-month (month)
  "The last day in MONTH of the Dolmenwood calendar."
  (cond
   ((memq month (list 2 9 11)) 28)
   ((memq month (list 4 5 8)) 29)
   ((memq month (list 1 3 6 10 12)) 30)
   ((memq month (list 7)) 31)))

(defun dolmenwood-calendar-num-wysendays (month)
  "The number of wysendays in the MONTH of the Dolmenwood calendar."
  (let ((days-in-month (dolmenwood-calendar-last-day-of-month month)))
    (- days-in-month 28)))

(defun dolmenwood-calendar-is-wyesenday (day)
  "Return t if MONTH, DAY is wysenday in the Dolmenwood calendar, otherwise nil."
  (> day 28))

(defun dolmenwood-calendar-wysenday (month day)
  "Return name of wysenday in the Dolmenwood calendar for MONTH DAY, otherwise nil."
  (let* ((wysenday-index (- day 28 1))
         (month-index (1- month))
         (wysenday-array (elt dolmenwood-calendar-wysendays-in-month-array month-index)))
    (if (< wysenday-index 0) nil (elt wysenday-array wysenday-index))))

(defun dolmenwood-calendar-day-number (date)
  "Return the day number within the year of the Dolmenwood DATE."
  (let* ((month (calendar-extract-month date))
         (past-months (number-sequence 1 (1- month)))
         (days-in-past-months (mapcar 'dolmenwood-calendar-last-day-of-month past-months))
         (days-past (apply '+ days-in-past-months)))
    (+ days-past (calendar-extract-day date))))

(defun dolmenwood-calendar-to-absolute (date)
  "Absolute date of Dolmenwood DATE.
The absolute date is the number of days elapsed since the
\(imaginary\) Gregorian date Sunday, December 31, 1 BC.,
referenced from the current game date."
  (let* ((year (calendar-extract-year date))
         (game-year (calendar-extract-year dolmenwood-calendar-current-game-date)))
    (+ (-
        (+ (* (1- year) 352) (dolmenwood-calendar-day-number date))
        (+ (* (1- game-year) 352) (dolmenwood-calendar-day-number dolmenwood-calendar-current-game-date)))
       (calendar-absolute-from-gregorian (calendar-current-date)))))

(defun dolmenwood-calendar--cumulative-difference (lst)
  "Find the cumulative difference between elements in LST."
  (let ((result (list (car lst))))
    (dolist (n (cdr lst))
      (setq result (cons (- (car result) n) result)))
    (nreverse result)))

(defun dolmenwood-calendar--index-of-last-element-greater-than-zero (lst)
  "Return the index of the last number in LST greater than 0."
  (let ((index 0))
    (dolist (element lst)
      (if (> element 0) (setq index (1+ index))))
    (1- index)))

(defun dolmenwood-calendar-from-absolute (date)
  "Dolmenwood date (month day year) corresponding to absolute DATE."
  (let* ((today-real-world (calendar-absolute-from-gregorian (calendar-current-date)))
         (today-game (dolmenwood-calendar-to-absolute dolmenwood-calendar-current-game-date))
         (days (- date today-real-world))
         (date-game (+ today-game days))
         (year (/ date-game 352))
         (day-of-year (- date-game (* year 352)))
         (days-in-month (mapcar 'dolmenwood-calendar-last-day-of-month (list 1 2 3 4 5 6 7 8 9 10 11 12)))
         (day-diff (dolmenwood-calendar--cumulative-difference (append day-of-year days-in-month)))
         (idx (dolmenwood-calendar--index-of-last-element-greater-than-zero day-diff))
         (month (1+ idx))
         (day (nth idx day-diff)))
    (list month day year)))

(defun dolmenwood-calendar-to-gregorian (date)
  "Dolmenwood DATE in game to real world date."
  (calendar-gregorian-from-absolute (dolmenwood-calendar-to-absolute date)))

(defun dolmenwood-calendar-diary-holiday (month day string)
  "Holiday on MONTH, DAY of the Dolmenwood calendar, named STRING."
  (let* ((dolmen-date (dolmenwood-calendar-to-gregorian (list month day year)))
         (dd (calendar-extract-day dolmen-date))
         (mm (calendar-extract-month dolmen-date))
         (yy (calendar-extract-year dolmen-date)))
    (holiday-fixed mm dd string)))

(defun dolmenwood-calendar-diary-date (month day year &optional mark)
  "Diary sexp for Dolmenwood date in form MONTH, DAY, YEAR.

An optional MARK specifies a face or single-character string
to use to highlight."
  (let* ((dolmen-date (dolmenwood-calendar-to-gregorian (list displayed-month 15 year)))
         (dd (calendar-extract-day dolmen-date))
         (mm (calendar-extract-month dolmen-date))
         (yy (calendar-extract-year dolmen-date)))
    (diary-date mm dd yy mark)))

(provide 'dolmenwood-calendar)
;;; dolmenwood-calendar.el ends here

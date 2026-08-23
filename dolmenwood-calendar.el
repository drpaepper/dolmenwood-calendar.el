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
;; Homepage: https://github.com/drpaepper/dolmenwood-calendar.el
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

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
  ["Grimvold" "Lymewald" "Haggryme" "Symswald" "Harchment"
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
   ["The Day of Doors" "Dolmenday"]]
  "Array of arrays of strings with the names of the wysendays per month.")

(defconst dolmenwood-calendar-sunrise-times-array
  ["08:00" "08:00" "07:30" "06:30" "06:00" "05:00"
   "04:30" "05:00" "06:00" "06:30" "07:30" "07:30"])

(defconst dolmenwood-calendar-sunset-times-array
  ["16:00" "16:30" "17:00" "18:00" "20:00" "21:00"
   "21:30" "21:00" "20:30" "19:30" "18:00" "16:30"])

(defconst dolmenwood-calendar-twilight-minutes-array
  [20 20 25 25 25 30 30 30 25 25 25 20])

(defvar dolmenwood-calendar-current-game-date nil
  "The current date within the Dolmenwood game.
User-defined variable representing the current date within
the game being played.")

(defvar dolmenwood-calendar-current-game-time nil
  "The current time within the Dolmenwood game.")

(defun dolmenwood-calendar-current-season ()
  "Current season in game."
  (let* ((month (nth 0 dolmenwood-calendar-current-game-date))
         (day (nth 1 dolmenwood-calendar-current-game-date)))
    (cond
     ((and (= month 12) (= day 30)) "Autumn (check for Hitching)")
     ((and (= month 9) (<= day 5)) "Summer (check for Chame)")
     ((and (= month 6) (= day 1)) "Spring (check for Colliggwyld)")
     ((and (or (= month 2) (= month 3)) (= (mod day 7) 1) (not (dolmenwood-calendar-is-wyesenday day))) "Winter (check for Vague)")
     ((and (>= month 1) (<= month 3)) "Winter")
     ((and (>= month 4) (<= month 6)) "Spring")
     ((and (>= month 7) (<= month 9)) "Summer")
     ((and (>= month 10) (<= month 12)) "Autumn"))))

(defun dolmenwood-calendar--time-string-to-seconds (time)
  "Convert time string TIME to seconds."
  (let ((time-list (parse-time-string time)))
    (+ (nth 0 time-list) (* (nth 1 time-list) 60) (* (nth 2 time-list) 3600))))

(defun dolmenwood-calendar-time-of-day ()
  "Time of day in game (see `dolmenwood-calendar-daylight-names-array')."
  (let* ((month (nth 0 dolmenwood-calendar-current-game-date))
         (sunup (elt dolmenwood-calendar-sunrise-times-array (1- month)))
         (sundown (elt dolmenwood-calendar-sunset-times-array (1- month)))
         (sunrise (dolmenwood-calendar--time-string-to-seconds sunup))
         (sunset (dolmenwood-calendar--time-string-to-seconds sundown))
         (dawn (- sunrise (* 60 (elt dolmenwood-calendar-twilight-minutes-array (1- month)))))
         (dusk (- sunset (* 60 (elt dolmenwood-calendar-twilight-minutes-array (1- month)))))
         (current (dolmenwood-calendar--time-string-to-seconds dolmenwood-calendar-current-game-time)))
    (cond
     ((and (>= current 0) (< current dawn)) "Night")
     ((and (>= current dawn) (< current sunrise)) "Dawn")
     ((and (>= current sunrise) (< current dusk)) "Day")
     ((and (>= current dusk) (< current sunset)) "Dusk")
     ((>= current sunset) "Night"))))

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
  "Return t if DAY is wysenday in the Dolmenwood calendar, otherwise nil."
  (> day 28))

(defun dolmenwood-calendar-wysenday (month day)
  "Return name of wysenday in the Dolmenwood calendar for MONTH DAY, otherwise nil."
  (let* ((wysenday-index (- day 28 1))
         (month-index (1- month))
         (wysenday-array (elt dolmenwood-calendar-wysendays-in-month-array month-index)))
    (if (< wysenday-index 0) nil (elt wysenday-array wysenday-index))))

(defun dolmenwood-calendar-name-of-day (month day)
  "Return the name of day on MONTH, DAY (day of week or wysenday)."
  (if (dolmenwood-calendar-wysenday month day)
      (dolmenwood-calendar-wysenday month day)
    (elt dolmenwood-calendar-day-name-array (mod (1- day) 7))))

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
         (days (- date today-real-world))
         (years (/ days 352))
         (d (- days (* years 352)))
         (today-game-day-of-year (dolmenwood-calendar-day-number dolmenwood-calendar-current-game-date))
         (years (if (< (+ today-game-day-of-year d) 1) (1- years) years))
         (years (if (> (+ today-game-day-of-year d) 352) (1+ years) years))
         (days-in-month (mapcar 'dolmenwood-calendar-last-day-of-month (list 1 2 3 4 5 6 7 8 9 10 11 12)))
         (day-of-year (1+ (mod (1-(+ today-game-day-of-year days)) 352)))
         (day-diff (dolmenwood-calendar--cumulative-difference (append (list day-of-year) days-in-month)))
         (idx (dolmenwood-calendar--index-of-last-element-greater-than-zero day-diff))
         (month (1+ idx))
         (day (nth idx day-diff))
         (year (+ (calendar-extract-year dolmenwood-calendar-current-game-date) years)))
    (list month day year)))

(defun dolmenwood-calendar-to-gregorian (date)
  "Dolmenwood DATE in game to real world date."
  (calendar-gregorian-from-absolute (dolmenwood-calendar-to-absolute date)))

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

;; taken from cal-islam.el
(defun dolmenwood-calendar-holiday (month day string)
  "Holiday on MONTH, DAY (Dolmenwood) called STRING.
If MONTH, DAY (Dolmenwood) is visible, returns the corresponding
Gregorian date as the list (((month day year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((dolmen-date (dolmenwood-calendar-from-absolute
                       (calendar-absolute-from-gregorian
                        (list displayed-month 15 displayed-year))))
         (m (calendar-extract-month dolmen-date))
         (y (calendar-extract-year dolmen-date))
         date)
    (unless (< m 1)
      (calendar-increment-month m y (- 10 month))
      (and (> m 7)
           (calendar-date-is-visible-p
            (setq date (calendar-gregorian-from-absolute
                        (dolmenwood-calendar-to-absolute (list month day y)))))
           (list (list date string))))))


(autoload 'diary-date "diary-lib")

;;;###autoload
(defun dolmenwood-calendar-diary-date (month day year &optional mark)
  "Diary sexp for Dolmenwood date in form MONTH, DAY, YEAR.

An optional MARK specifies a face or single-character string
to use to highlight."
  (let* ((dolmen-date (dolmenwood-calendar-to-gregorian (list month day year)))
         (dd (calendar-extract-day dolmen-date))
         (mm (calendar-extract-month dolmen-date))
         (yy (calendar-extract-year dolmen-date)))
    (diary-date mm dd yy mark)))

(defun dolmenwood-calendar-org-agenda-format-date (date)
  "Nice format for `org-agenda' for DATE."
  (let* ((dolmen-date (dolmenwood-calendar-from-absolute (calendar-absolute-from-gregorian date)))
         (dolmen-day (calendar-extract-day dolmen-date))
         (dolmen-month (calendar-extract-month dolmen-date))
         (dolmen-month-name (elt dolmenwood-calendar-month-name-array (1- dolmen-month)))
         (dolmen-year (calendar-extract-year dolmen-date))
         (dayname (dolmenwood-calendar-name-of-day dolmen-month dolmen-day)))
    (format "%-20s %2d %s %4d" dayname dolmen-day dolmen-month-name dolmen-year)))

(defun dolmenwood-calendar-increment-game-day (days)
  "Increment `dolmenwood-calendar-current-game-date' by DAYS."
  (setq dolmenwood-calendar-current-game-date
        (dolmenwood-calendar-from-absolute
         (+
          (dolmenwood-calendar-to-absolute dolmenwood-calendar-current-game-date) days))))

(provide 'dolmenwood-calendar)
;;; dolmenwood-calendar.el ends here

;;; dolmenwood-calendar --- A calendar for the Dolmenwood RPG
;; Author: drpaepper
;; Maintainer: drpaepper
;; Keywords: calendar
;; Human-Keywords: calendar, rpg

;;; Commentary:

;; This collection of functions provides various calendar tools for
;; the Dolmenwood RPG. This can be helpful for creating org-agenda
;; view for running / playing various RPGs. All dates are referenced
;; from `calendar-dolmenwood-current-game-date' and the current date.
;; In otherwords, the current date in the real world will always
;; align to the in-game current date.

;;; Code:

(require 'calendar)

(defconst calendar-dolmenwood-day-name-array
  ["Colly" "Chime" "Hayme" "Moot" "Frisk" "Eggfast" "Sunning"]
  "Array of the strings giving names of the Dolmenwood days.")

(defconst calendar-dolmenwood-month-name-array
  ["Grimwold" "Lymewald" "Haggryme" "Symswald" "Harchment"
   "Iggwyld" "Chysting" "Lillipythe" "Haelhold" "Reedwryme"
   "Obthryme" "Braghold"]
  "Array of the strings giving names of the Dolmenwood months.")

(defconst calendar-dolmenwood-wysendays-in-month-array
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

(defun calendar-dolmenwood-last-day-of-month (month)
  "The last day in MONTH of the Dolmenwood calendar."
  (cond
   ((memq month (list 2 9 11)) 28)
   ((memq month (list 4 5 8)) 29)
   ((memq month (list 1 3 6 10 12)) 30)
   ((memq month (list 7)) 31)
   ))

(defun calendar-dolmenwood-num-wysendays (month)
  "The number of wysendays in the MONTH of the Dolmenwood calendar."
  (let ((days-in-month (calendar-dolmenwood-last-day-of-month month)))
    (- days-in-month 28)))

(defun calendar-dolmenwood-is-wyesenday (month day)
  "Return t if MONTH, DAY is wysenday in the Dolmenwood calendar, otherwise nil."
  (> day 28))

(defun caledar-dolmenwood-wysenday (month day)
  "Return name of wysenday in the Dolmenwood calendar for MONTH DAY, otherwise nil."
  (let* ((wysenday-index (- day 28 1))
         (month-index (1- month))
         (wysenday-array (elt calendar-dolmenwood-wysendays-in-month-array month-index)))
    (if (< wysenday-index 0) nil (elt wysenday-array wysenday-index))))

(defun calendar-dolmenwood-day-number (date)
  "Return the day number within the year of the Dolmenwood DATE."
  (let* ((month (calendar-extract-month date))
         (past-months (number-sequence 1 (1- month)))
         (days-in-past-months (mapcar 'calendar-dolmenwood-last-day-of-month past-months))
         (days-past (apply '+ days-in-past-months)))
    (+ days-past (calendar-extract-day date))))

(defun calendar-dolmenwood-to-absolute (date)
  "Absolute date of Dolmenwood DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC., referenced from the current game date."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (game-month (calendar-extract-month rpgtk-calendar-game-current-date))
         (game-day (calendar-extract-day rpgtk-calendar-game-current-date))
         (game-year (calendar-extract-year rpgtk-calendar-game-current-date)))
    (+ (-
        (+ (* (1- year) 352) (calendar-dolmenwood-day-number date))
        (+ (* (1- game-year) 352) (calendar-dolmenwood-day-number rpgtk-calendar-game-current-date)))
       (calendar-absolute-from-gregorian (calendar-current-date)))))

(defun calendar-dolmenwood-from-absolute (date)
  "Dolmenwood date (month day year) corresponding to absolute DATE."
  (let* ((today-real-world (calendar-absolute-from-gregorian (calendar-current-date)))
         (today-game (calendar-dolmenwood-to-absolute rpgtk-calendar-game-current-date))
         (days (- date today-real-world))
         (date-game (+ today-game days))
         (year (/ date-game 352))
         (day-of-year (- date-game (* year 352)))
         )
    ()))

(defun calendar-dolmenwood-to-gregorian (date)
  "Dolmenwood DATE in game to real world date."
  (calendar-gregorian-from-absolute (calendar-dolmenwood-to-absolute date)))

(defun holiday-dolmenwood (month day string)
  "Holiday on MONTH, DAY of the Dolmenwood calendar, named STRING."
  (let* ((dolmen-date (calendar-dolmenwood-to-gregorian (list month day year)))
         (dd (calendar-extract-day dolmen-date))
         (mm (calendar-extract-month dolmen-date))
         (yy (calendar-extract-year dolmen-date)))
    (holiday-fixed mm dd string)))

(defun diary-dolmenwood (month day year &optional mark)
  "Diary sexp for Dolmenwood date in form MONTH, DAY, YEAR.

An optional MARK specifies a face or single-character string to use to highlight."
  (let* ((dolmen-date (calendar-dolmenwood-to-gregorian (list displayed-month 15 year)))
         (dd (calendar-extract-day dolmen-date))
         (mm (calendar-extract-month dolmen-date))
         (yy (calendar-extract-year dolmen-date)))
    (diary-date mm dd yy mark)))

(provide 'dolmenwood-calendar)
;;; dolmenwood-calendar.el ends here

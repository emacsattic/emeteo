;; emeteo-parse.el --- Parsing

;; $Id$
;; Author: Sebastian Freundt (freundt@users.sf.net)
;; Keywords: metar, meteo, weather, emacs

;; Copyright (C) 2004  Sebastian Freundt

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;

;; For more information, see the following URLs:
;; * http://sf.net/projects/emeteo/
;; * http://gna.org/projects/emeteo/

;;; Changelog:
;; 2004/04/07:
;; - parsing of *emeteo* buffers is split into the raw parsing process and the valuation process
;; - further parsing of *emeteo* buffers has been modularized to easily add new parsers
;; - keywords are customizable now

;;; Code:

(require 'emeteo-utils)


(defvar emeteo-metspecs '(temp press)
  "Determins metar specifiers.")


(defun emeteo-wash (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (search-forward (concat (char-to-string 13)) nil t)
        (replace-match ""))

    ;;(encode-coding-region (point-min) (point-max))

    (goto-char (point-min))
    (while (re-search-forward "[ \t\n]+" nil t)
      (replace-match "  "))

    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " "))

    (goto-char (point-min))
    (while (re-search-forward "<.+?>" nil t)
      (replace-match " "))

    (goto-char (point-min))
    (while (re-search-forward "\\s-\\s-+" nil t)
      (replace-match "   "))))
    

(defcustom emeteo-temperature-units
  '(("&deg;?" "&ordm;?" "�")
    ("C" "F"))
  "This is a list of lists of possible temperature units.
Elements of different lists are semi-combined (semi-product set).
Regular expressions are allowed."
  :group 'emeteo)
;;; example usage:
;; (emeteo-utils-product-set emeteo-temperature-units 'concat)


(defcustom emeteo-temperature-introductory-strings
  '(
    ("current\\(?:ly\\)?\\s-?" "aktuelle?s?")
    ("wetter" "weather\\(?:.conditions?\\)" "temp\\(?:\\.\\|erature?\\)?")
    )
  "Some strings often used to introduce the current temperature.
Useful to adopt on pages where thousands of temperatures are
listed at the same time."
  :group 'emeteo)


;; general stuff

(defcustom emeteo-parse-hook
  '(
    emeteo-parse-temperature
    ;;emeteo-parse-pressure
    ;;emeteo-parse-wind-direction
    ;;emeteo-parse-wind-velocity
    ;;emeteo-parse-sight
    ;;emeteo-parse-light
    ;;emeteo-parse-humidity
    )
  "Hook called when an *emeteo* buffer is to be parsed.
This hook should contain all parse modules to be called.

The hook will be run with the buffer to parse and the
optionally given emeteo-spec as args.
The module itself should take the emeteo-spec as optional
arg either and return a cons cell with a unique
emeteo parse identifier in the car and the result in the
cdr."
  :group 'emeteo)


(defcustom emeteo-valuate-hooks-alist
  '(
    (temp . emeteo-valuate-temperature)
    )
  "Alist of hooks to be called after parsing the raw data.
Each entry is a cons cell with the emeteo parse identifier
in car and a hook in cdr

The hook may either be a hook's symbol which is then evaled
and called or a hook's value whose elements are called or
a single function which is called.

Currently there's only one function that does the whole
valuation of raw data, see `emeteo-valuate-data'."
  :group 'emeteo)



;;;; Now Code
;;; temperature parsing

(defun emeteo-parse-temperature (buffer &optional emeteo-spec)
  "Parses BUFFER for occurences of a current temperature.
Optional EMETEO-SPEC is ignored at the moment.

Returns a cons cell \('temp suspects) where 'temp is the
emeteo parse identifier and suspects are temperature/unit pairs.

THIS NEEDS TO BE MORE ABSTRACTED."
  (let* ((tempunits (mapconcat 'identity
                               (emeteo-utils-product-set emeteo-temperature-units 'concat)
                               "\\|"))
         (tempintr (mapconcat 'identity
                              (emeteo-utils-product-set emeteo-temperature-introductory-strings 'concat)
                              "\\|"))
         (temprexp (format "\\(?:%s\\):?\\s-+\\([0-9.,]+\\)\\s-*\\(%s\\)"
                           tempintr
                           tempunits))
         (result))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward temprexp nil t)
          (add-to-list 'result `(:temp ,(match-string 1)
                                 :unit ,(match-string 2)
                                 :match ,(match-string 0))))))
    (cons 'temp result)))
;; (setq emeteo-valuate-hooks-alist '((temp . identity)))
;; (emeteo-frob-uri "http://www.met.fu-berlin.de/de/wetter/")
;; (emeteo-frob-uri "http://weather.yahoo.com/forecast/GMXX0007.html")

(defun emeteo-valuate-temperature (parse-data)
  "Valuates a list of given temperatures."
  (let* ((parse-ident (car parse-data))
         (parse-data-cdr (cdr parse-data))
         (parse-data-car (car parse-data-cdr))
         (result (format "%s%s"
                         (emeteo-find-key-val ':temp parse-data-car)
                         (emeteo-find-key-val ':unit parse-data-car))))
    (cons 100 `(,parse-ident ,result))))




;;; more general parsing defuns

(defun emeteo-parse-buffer (buffer &optional emeteo-spec)
  "Returns a metar information alist of the form
  \( emeteo-parse-identifier value-suspects ).

You can plug into this by adding your own modules to `emeteo-parse-hook'
\(see documentation there).

Note: This one does the mere parsing, the output should be considered
raw data, look at `emeteo-valuate-data' for more information."
  (let ((raw-specs (mapcar (lambda (fun)
                             (funcall fun buffer emeteo-spec))
                           emeteo-parse-hook)))
    raw-specs))
;;(emeteo-parse-buffer (get-buffer "*emeteo*"))

(defun emeteo-valuate-data (raw-data)
  "Valuates the alist RAW-DATA according to `emeteo-valuate-hooks-alist'."
  (mapcar (lambda (val-hook)
            (let* ((parse-ident (car val-hook))
                   (val-hook-cdr (cdr val-hook))
                   (val-hook-hook (cond ((listp val-hook-cdr)
                                         val-hook-cdr)
                                        ((boundp val-hook-cdr)
                                         (eval val-hook-cdr))
                                        ((fboundp val-hook-cdr)
                                         (list val-hook-cdr))
                                        (t nil)))
                   (parse-ident-raw-data (assoc parse-ident raw-data)))
              (and val-hook-hook
                   (let* ((vals
                           (mapcar (lambda (fun)
                                     (funcall fun parse-ident-raw-data))
                                   val-hook-hook)))
                     (cdr (car (sort vals '>)))))))
          emeteo-valuate-hooks-alist))
;;(setq test (emeteo-parse-buffer (get-buffer "*emeteo*")))
;;(emeteo-valuate-data test)



(defun emeteo-parse (buffer)
  "Returns a metar information alist of the form
  \( metkey . value ).

Currently there are the following metkeys available:
- temp"
  (with-current-buffer buffer
    (let* (
;;           (uhrzeit (progn
;;                      (goto-char (point-min))
;;                      (and (re-search-forward "\\([0-9][0-9]:[0-9][0-9].Uhr\\)" nil t)
;;                           (match-string 1))))
;;            (allg (progn
;;                   (goto-char (point-min))
;;                   (and (re-search-forward "Ortszeit \\(.+\\) Temp" nil t)
;;                        (match-string 1))))
           (temp (progn
                   (goto-char (point-min))
                   (and (or (save-excursion
                              (re-search-forward "temp\\(?:.\\|erature?\\)?:?\\s-+\\([0-9.,]+\\s-*\\(?:&deg;\\|�\\)\\(?:C\\|F\\)\\)" nil t))
                            (save-excursion
                              (re-search-forward "current \\(?:weather\\|condition\\).+?\\s-+\\([0-9.,]+\\s-*\\(?:&deg;\\|�\\)?\\(?:C\\|F\\)\\)" nil t)))
                        (match-string 1))))
           (press (progn
                    (goto-char (point-min))
                    (and (re-search-forward "\\(?:\\(?:luft\\)?druck\\|\\(?:air ?\\)?press\\(?:ure\\)?\\):?\\s-\\(.+?\\(?:[hk]?Pa\\|m?bar\\|inche?s?\\)\\)" nil t)
                         (match-string 1))))
           (windd (progn
                    (goto-char (point-min))
                    (and (re-search-forward "wind\\(?:\\s-?rich[.t]?\\(?:\\.\\|ung\\)?\\|dir\\(?:ection\\)?\\):?\\s-+\\(\\S-+\\)\\s-" nil t)
                         (match-string 1))))
           (winds (progn
                    (goto-char (point-min))
                    (and (re-search-forward "wind\\(?:st.?\\(?:\\(?:&auml;\\|�\\)rke\\)?\\|geschw?.?\\(?:indigkeit\\)?\\| ?sp\\(?:eed\\)?.?\\|\\s-?vel\\(?:ocity\\)?\\):?\\s-\\(.+?\\(?:Bft\\|km/?h\\|m/?s\\|kn\\|mph\\|mls\\)\\)" nil t)
                         (match-string 1))))
           (hydr (progn
                   (goto-char (point-min))
                   (and (re-search-forward "\\(?:luftfeuchte\\|humidity\\):?\\s-\\(.+?%\\)" nil t)
                        (match-string 1))))
           (licht (progn
                    (goto-char (point-min))
                    (and (re-search-forward "lichtst\\(?:�\\|&auml;\\)rke:?\\s-\\(.+?lux\\)" nil t)
                         (match-string 1))))
           (sicht (progn
                    (goto-char (point-min))
                    (and (re-search-forward "sicht:\\s-\\(.+?m\\)" nil t)
                         (match-string 1))))
           (temp (and temp
                      (or (replace-regexp-in-string " " "" temp nil t) temp)))
           (temp (and temp
                      (or (replace-regexp-in-string "&deg;" "�" temp nil t) temp)))
           (press (and press
                       (or (replace-regexp-in-string " " "" press nil t) press)))
           (windd (and windd
                       (or (replace-regexp-in-string " " "" windd nil t) windd)))
           (winds (and winds
                       (or (replace-regexp-in-string " " "" winds nil t) winds)))
           (hydr (and hydr
                      (or (replace-regexp-in-string " " "" hydr nil t) hydr)))
           (licht (and licht
                       (or (replace-regexp-in-string " " "" licht nil t) licht)))
           (sicht (and sicht
                       (or (replace-regexp-in-string " " "" sicht nil t) sicht)))
           (metalist nil))
      (and temp
           ;;(emeteo-valid-temp-p temp)
           (add-to-list 'metalist `(temp ,(concat temp)) t))
      (and hydr
           ;;(emeteo-valid-temp-p temp)
           (add-to-list 'metalist `(hydr ,(concat hydr)) t))
      (and press
           ;;(emeteo-valid-temp-p press)
           (add-to-list 'metalist `(press ,(concat press)) t))
      (and windd
           ;;(emeteo-valid-temp-p press)
           (add-to-list 'metalist `(windd ,(concat windd)) t))
      (and winds
           ;;(emeteo-valid-temp-p press)
           (add-to-list 'metalist `(winds ,(concat winds)) t))
      (and licht
           ;;(emeteo-valid-temp-p press)
           (add-to-list 'metalist `(licht ,(concat licht)) t))
      (and sicht
           ;;(emeteo-valid-temp-p press)
           (add-to-list 'metalist `(sicht ,(concat sicht)) t))
      metalist)))
;; (emeteo-frob-uri "http://www.met.fu-berlin.de/de/wetter/")
;; (emeteo-frob-uri "http://weather.yahoo.com/forecast/GMXX0007.html")


(defun emeteo-valid-temp-p (temp)
  "Checks whether temp is a valid temperature value"
  (null (null (string-match "^[-+]?[0-9]+[.,]?[0-9]*$" temp))))


(provide 'emeteo-parse)

;;; emeteo-parse.el ends here

;; Local-variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:
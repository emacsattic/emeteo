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
;; * http://gna.org/projects/emeteo/

;;; Code:

(require 'emeteo-utils)


(defconst emeteo-parse-prefix "emeteo-parse-"
  "Prefix of emeteo-parse.el")
(defgroup emeteo-parse nil
  "Emeteo parsers."
  :group 'emeteo
  :prefix emeteo-parse-prefix)


(defcustom emeteo-temperature-units
  '(("&deg;?" "&#176;" "&ordm;?" "°")
    ("C" "F"))
  "This is a list of lists of possible temperature units.
Elements of different lists are combined specially
(half/semi-product set).
Regular expressions are allowed.

Thanks to hynek for this wonderful idea."
  :group 'emeteo)
;;; example usage:
;; (emeteo-utils-product-set emeteo-temperature-units 'concat)


(defcustom emeteo-wind-units
  '(("m/s"))
  "This is a list of lists of possible wind speed units.
Elements of different lists are combined specially
(half/semi-product set).
Regular expressions are allowed."
  :group 'emeteo)


(defcustom emeteo-temperature-introductory-strings
  '(
    ("current\\(?:ly\\)?\\s-?" "aktuelle?s?")
    ("wetter" "weather\\(?:.conditions?\\)" "temp\\(?:\\.\\|erature?\\)?")
    )
  "Some strings often used to introduce the current temperature.
Useful to adapt on pages where thousands of temperatures are
listed at the same time."
  :group 'emeteo)


(defcustom emeteo-wind-introductory-strings
  '(
    ("current\\(?:ly\\)?\\s-?" "aktuelle?s?")
    ("wind\\(?:geschw?\\(?:\\.\\|indigkeit\\)?\\|\\s-?speed\\|\\s-velocity\\)")
    )
  "Some strings often used to introduce current wind speed and direction.
Useful to adapt on pages where thousands of velocity values are listed
at the same time."
  :group 'emeteo)


(defcustom emeteo-status-introductory-strings
  '(
    ("current\\(?:ly\\)?\\s-?" "aktuelle?s?")
    ("weather" "status")
    )
  "Some strings often used to introduce current weather status.
Useful to adapt on pages where thousands of status values are listed
at the same time."
  :group 'emeteo)


;; general stuff

(defcustom emeteo-parse-hook
  '(
    emeteo-parse-temp
    emeteo-parse-wind
    ;;emeteo-parse-pressure
    ;;emeteo-parse-sight
    ;;emeteo-parse-light
    ;;emeteo-parse-humidity
;;    emeteo-parse-status
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
    (wind . emeteo-valuate-wind)
;;    (stat . emeteo-valuate-stat)
    )
  "Alist of hooks to be called after parsing the raw data.
Each entry is a cons cell with the emeteo parse identifier
in car and a hook in cdr

The hook may either be a hook's symbol which is then evaled
and called or a hook's value whose elements are called or
a single function which is called.

Currently there's only one function that does the whole
valuation of raw data, see `emeteo-valuate-data'.
Each valuation function should return a valuated/weighted
data list, i.e. the raw data list entries get scores.

Note: that there should be at least one valuation function
in each hook that modifies data to a more suitable output."
  :group 'emeteo)




(defcustom emeteo-decision-functions-alist
  '(
    (temp . emeteo-decide-min-score)
    (wind . emeteo-decide-min-score)
    )
  "Alist of functions to be called after the valuation of the
raw data (see `emeteo-valuate-hooks-alist').

The function is given the valuated data and then should take a
decision, i.e. return a value based on the data and the scores
assigned to the data.

There are several predefined decision-functions:
- emeteo-decide-max-score  decides for the maximal valuation
- emeteo-decide-min-score  decides for the minimal valuation"
  :group 'emeteo)



;;;; Now Code


(defmacro emeteo-parse-define-parser (name &rest parser-specs)
  "Defines an emeteo raw data parser.
A parser consists of NAME which defines a defun emeteo-parse-`NAME'
and some specs to control the parsing process."
  (let* ((pfunn (intern (format "%s%s" emeteo-parse-prefix name)))
         (units (emeteo-utils-eval-val
                 (emeteo-utils-find-key-val ':units parser-specs)))
         (istrs (emeteo-utils-eval-val
                 (emeteo-utils-find-key-val ':istrs parser-specs))))
    `(defun ,pfunn (buffer &optional emeteo-spec)
      ,(format "Parses BUFFER for occurences of %s.
Optional EMETEO-SPEC is ignored at the moment.

Returns a cons cell '\(temp suspects) where 'temp is the
emeteo parse identifier and suspects are temperature/unit pairs."
               name)
      (let* ((punits (mapconcat 'identity
                                (emeteo-utils-product-set ',units 'concat)
                                "\\|"))
             (pintrs (mapconcat 'identity
                                (emeteo-utils-product-set ',istrs 'concat)
                                "\\|"))
             (prexps (format "\\(?:%s\\):?\\s-+\\([0-9.,]+\\)\\s-*\\(%s\\)"
                             pintrs
                             punits))
             (result))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward prexps nil t)
              (add-to-list 'result
                           (list ':temp (match-string 1)
                                 ':unit (match-string 2)
                                 ':match (match-string 0))))))
        (cons ',name result)))))


;;; some predefined parsers

(emeteo-parse-define-parser
 temp
 :units emeteo-temperature-units
 :istrs emeteo-temperature-introductory-strings)
(emeteo-parse-define-parser
 wind
 :units emeteo-wind-units
 :istrs emeteo-wind-introductory-strings)





;;; temperature parsing

(defun emeteo-valuate-temperature (parse-data &optional temperature-format)
  "Valuates a list of given temperatures.
This defun currently gives position in list as score. :o."
  (let* ((temp-data (cdr parse-data))
         (init-score 0))
    (cons 'temp
          (mapcar (lambda (dat)
                    (cons (incf init-score)
                          (format "%s"
                                  (emeteo-utils-find-key-val ':temp dat)
                                  )))
                  temp-data))))
;;(emeteo-frob-uri "http://www.math.tu-berlin.de/~freundt/emeteo-test.html")
;;(setq test (emeteo-parse-buffer (get-buffer "*emeteo*")))
;;(setq tes2 (emeteo-valuate-data test))
;;(emeteo-decide-data tes2)


;;; wind parsing

(defun emeteo-valuate-wind (parse-data &optional wind-format)
  "Valuates a list of given wind data.
This defun currently gives position in list as score. :o."
  (let* ((wind-data (cdr parse-data))
         (init-score 0))
    (cons 'wind
          (mapcar (lambda (dat)
                    (cons (incf init-score)
                          (format "%s"
                                  (emeteo-utils-find-key-val ':wind dat)
                                  )))
                  wind-data))))
;;(emeteo-frob-uri "http://www.met.fu-berlin.de/de/wetter/")
;;(setq test (emeteo-parse-buffer (get-buffer "*emeteo*")))
;;(setq tes2 (emeteo-valuate-data test))
;;(emeteo-decide-data tes2)



;;; status parsing

(defun emeteo-parse-status (buffer &optional emeteo-spec)
  "Parses BUFFER for occurences of a current status spec.
Optional EMETEO-SPEC is ignored at the moment.

Returns a cons cell '\(wind suspects) where 'wind is the
emeteo parse identifier and suspects are wind/unit pairs.

THIS NEEDS TO BE MORE ABSTRACTED."
  (let* ((statintr (mapconcat 'identity
                              (emeteo-utils-product-set emeteo-status-introductory-strings 'concat)
                              "\\|"))
         (statrexp (format "\\(?:%s\\):?\\s-+\\(.*\\)"
                           statintr))
         (result))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward statrexp nil t)
          (add-to-list 'result `(:stat ,(match-string 1)
                                 :match ,(match-string 0))))))
    (cons 'stat result)))

(defun emeteo-valuate-stat (parse-data &optional stat-format)
  "Valuates a list of given status expressions.
This defun currently gives position in list as score. :o."
  (let* ((stat-data (cdr parse-data))
         (init-score 0))
    (cons 'stat
          (mapcar (lambda (dat)
                    (cons (incf init-score)
                          (format "%s"
                                  (emeteo-utils-find-key-val ':stat dat)
                                  )))
                  stat-data))))



;;; more general parsing defuns

(defun emeteo-parse-buffer (buffer &optional emeteo-spec)
  "Returns a metar information alist of the form
  \( emeteo-parse-identifier value-suspects ).

You can plug into this by adding your own modules to `emeteo-parse-hook'
\(see documentation there).

Note: This one does the mere parsing, the output should be considered
raw data, look at `emeteo-valuate-data' for more information."
  (when buffer
    (let ((raw-specs (mapcar (lambda (fun)
                               (funcall fun buffer emeteo-spec))
                             emeteo-parse-hook)))
      raw-specs)))
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
                   (mapc (lambda (fun)
                           (setq parse-ident-raw-data
                                 (funcall fun parse-ident-raw-data)))
                         val-hook-hook)
                   parse-ident-raw-data)))
          emeteo-valuate-hooks-alist))



(defun emeteo-decide-data (val-data)
  "Decides for data in VAL-DATA according to `emeteo-decision-functions-alist'"
  (mapcar (lambda (dec-funs)
            (let* ((val-ident (car dec-funs))
                   (dec-funs-fun (cdr dec-funs))
                   (val-ident-val-data (assoc val-ident val-data)))
              (funcall dec-funs-fun val-ident-val-data)))
          emeteo-decision-functions-alist))
;;(emeteo-frob-uri "http://www.math.tu-berlin.de/~freundt/emeteo-test.html")
;;(setq test (emeteo-parse-buffer (get-buffer "*emeteo*")))
;;(setq tes2 (emeteo-valuate-data test))
;;(emeteo-decide-data tes2)
;;(emeteo-frob "B")





;;; some predefined (general purpose) decision funs
(defun emeteo-decide-max-score (val-data)
  "Decides for the data with maximal score in VAL-DATA
and returns it.

This function expects to have a list with pairs like
\(score . value) in VAL-DATA.

Note: This will work only if the valuation function puts
integer scores to the data."
  (let* ((parse-ident (car val-data))
         (data (cdr val-data))
         (max-val-data (emeteo-decide-by-set-function data 'max)))
    (cons parse-ident max-val-data)))

(defun emeteo-decide-min-score (val-data)
  "Decides for the data with minimal score in VAL-DATA
and returns it.

This function expects to have a list with pairs like
\(score . value) in VAL-DATA.

Note: This will work only if the valuation function puts
integer scores to the data."
  (let* ((parse-ident (car val-data))
         (data (cdr val-data))
         (min-val-data (emeteo-decide-by-set-function data 'min)))
    (cons parse-ident min-val-data)))

;;; all doing wrapper function
(defun emeteo-decide-by-set-function (score-data-alist set-function)
  "Decides for the data with score returned by running SET-FUNCTION
on SCORE-DATA-ALIST"
  (and score-data-alist
       (cdr-safe (assoc (eval (cons set-function (mapcar 'car score-data-alist)))
                        score-data-alist))))





;;; this is the next to get into the abstraction process: Wash Modules

(defmacro emeteo-parse-define-wash (name &rest wash-specs)
  "Defines a wash module to be used for preprocessing the buffer
returned by some fetch functions.
A wash module consists of NAME which defines a defun emeteo-wash-`NAME'
and some specs to control the washing process.

Basically a wash module is a set of (rexp . replacement) pairs.

Supported keywords for WASH-SPECS

:replace -- list of \(rexp . replacement\) pairs."
  (let ((wfunn (intern (format "emeteo-wash-%s" name)))
        (wrepl (emeteo-utils-eval-val
                (emeteo-utils-find-key-val ':replace wash-specs))))
    `(defun ,wfunn (buffer)
      (when buffer
        (with-current-buffer buffer
          (mapc (lambda (rexprepl)
                  (let ((rexp (emeteo-utils-eval-val (car rexprepl) 'stringp))
                        (repl (emeteo-utils-eval-val (cdr rexprepl) 'stringp)))
                    (goto-char (point-min))
                    (while (re-search-forward rexp nil t)
                      (replace-match repl))))
                ,wrepl))
        buffer))))

;; some predefined wash modules

(emeteo-parse-define-wash
 html
 :replace '(((char-to-string 13) . "")
            ("[ \t\n]+" . "  ")
            ("&nbsp;?" . " ")
            ("<.+?>" . " ")
            ("\\s-\\s-+" . "   ")))
;;(symbol-function 'emeteo-wash-html)





(defun emeteo-valid-temp-p (temp)
  "Checks whether temp is a valid temperature value"
  (null (null (string-match "^[-+]?[0-9]+[.,]?[0-9]*$" temp))))




(provide 'emeteo-parse)

;;; emeteo-parse.el ends here

;; Local-variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

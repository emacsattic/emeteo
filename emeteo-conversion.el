;; emeteo-conversion.el --- Emeteo Conversion of Units

;; $Id$
;; Author: Sebastian Freundt (hroptatyr@gna.org)
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

;; ...

;; For more information, see the following URLs:
;; * http://gna.org/projects/emeteo/


;;; Code:

(defgroup emeteo-conversion nil
  ""
  :prefix "emeteo-conversion-"
  :group 'emeteo)

(defcustom emeteo-conversion-temperature-polynomials
  `((celsius (fahrenheit 1.8 32))
    (fahrenheit (celsius ,(/ 5.0 9.0) ,(/ -32 1.8))))
  "")

(defun emeteo-conversion-temperature (value from to)
  "Convert temperature VALUE given in unit FROM to unit TO."
  (let* ((from-ctable (cdr-safe (assoc from emeteo-conversion-temperature-polynomials)))
         (to-poly (cdr-safe (assoc to from-ctable)))
         (value (cond ((stringp value)
                       (string-to-number value))
                      (t value))))
    (and value
         (eval (list (emeteo-utils-generate-polynomial-fun to-poly) value)))))
;;(emeteo-conversion-temperature nil 'fahrenheit 'celsius)



(provide 'emeteo-conversion)

;;; emeteo-conversion.el ends here

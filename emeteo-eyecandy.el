;; emeteo-eyecandy.el --- some eyecandy functions

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
;; Put (require 'emeteo-eyecandy) in your ~/.gnus and try M-x emeteo-eyecandy-insinuate
;; Otherwise try adding:
;;   '((".*" ("X-Weather" (emeteo-eyecandy-generate-header 'somespec)) ...))
;; to your Gnus posting styles variable.


;; Usage:

;;; History:
;;

;;; Code:

(require 'emeteo-modeline)


(defgroup emeteo-eyecandy nil
  "Facilities to display metar data in outgoing mails of gnus."
  :prefix "emeteo-eyecandy-"
  :group 'emeteo)

(defcustom emeteo-eyecandy-icons-directory
  (concat (file-name-directory (locate-library "emeteo")) "etc")
  "Directory of modeline pixmaps."
  :group 'emeteo-eyecandy)
(defcustom emeteo-eyecandy-icons-list
  '((cloud . "cloud-mini.xpm")
    (fog . "fog-mini.xpm")
    (rain . "rain-mini.xpm")
    (snow . "snow-mini.xpm")
    (sun . "sun-mini.xpm")
    (suncloud . "suncloud-mini.xpm")
    (storm . "tstorm-mini.xpm")
    (unknown . "unknown-mini.xpm"))
  "foo")


;;;###autoload
(defun emeteo-eyecandy-compute-icon (&rest specs)
  "Computes from given SPECS a status usable to create a summary icon."
  ;; lol, if that function works, im smarter than all those
  ;; professional meteorologists
  )


;;;###autoload
(defun emeteo-eyecandy-generate-modeline-extent (&optional status)
  "Creates an extent based on STATUS"
  (cons (make-extent nil nil)
        (make-glyph (concat emeteo-eyecandy-icons-directory "/" (cdr (assoc status emeteo-eyecandy-icons-list))))))
;;(setq emeteo-modeline-string `(("huhu" ,(emeteo-eyecandy-generate-modeline-extent 'rain)) ("noch")))

(provide 'emeteo-eyecandy)

;;; emeteo-eyecandy.el ends here

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

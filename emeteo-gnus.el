;; emeteo-gnus.el --- gnus integration feature for emeteo

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
;; Put (require 'emeteo-gnus) in your ~/.gnus and try M-x emeteo-gnus-insinuate
;; Otherwise try adding:
;;   '((".*" ("X-Weather" (emeteo-gnus-generate-header 'somespec)) ...))
;; to your Gnus posting styles variable.


;; Usage:

;;; History:
;;

;;; Code:

(require 'timer)
(require 'emeteo)
(require 'emeteo-utils)


(defgroup emeteo-gnus nil
  "Facilities to display metar data in outgoing mails of gnus."
  :prefix "emeteo-gnus-"
  :group 'emeteo)

(defcustom emeteo-gnus-default-specname-keyword ':shortname
  "Keyword from `emeteo-data-sources' to be used when
displaying as place in the headers."
  :group 'emeteo-gnus
  :type 'sexp)

(defcustom emeteo-gnus-default-fail-indicator-string '"n/a"
  "String used as an indicator in the header to
announce that fetching has failed."
  :group 'emeteo-gnus
  :type 'string)


(defcustom emeteo-gnus-default-header-format '("%s%s (%s)" temp :unit-string :name)
  "Defines how the rendered header looks like.
This list consists of a format string in the car and arbitrary result
keys or keywords from the data-sources in the cdr."
  :group 'emeteo-gnus)




;;;###autoload
(defun emeteo-gnus-generate-header (spec &optional header-format)
;;; this is the fun i currently have in my posting-styles
  (let* ((result (emeteo-fetch spec))
         (format (or header-format
                     emeteo-gnus-default-header-format))
         (fullspec (assoc spec emeteo-data-sources)))
    (or (and result
             (emeteo-utils-format fullspec result format emeteo-gnus-default-fail-indicator-string))
        emeteo-gnus-default-fail-indicator-string)))
;;(emeteo-gnus-generate-header 'berlin)



(provide 'emeteo-gnus)

;;; emeteo-gnus.el ends here

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

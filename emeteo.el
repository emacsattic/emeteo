;; emeteo.el --- An Emacs Interface To Meteorological Data

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

;; emeteo is a general emacs interface to provide meteorologic data

;; For more information, see the following URLs:
;; * http://sf.net/projects/emeteo/
;; * http://gna.org/projects/emeteo/

;; Installation:

;; Put emeteo.el in your load-path, and put (require 'emeteo) in your .emacs.
;; To optionally use the modeline feature, try (require 'emeteo-modeline) and
;; M-x emeteo RET :)

;; Configuration:

;; Use M-x customize-group RET emeteo RET to get an overview
;; of all the variables you can tweak.

;; Usage:
;;

;;; Changelog:
;; 2004/04/07:
;; - parsing of *emeteo* buffers is split into the raw parsing process and the valuation process
;; - further parsing of *emeteo* buffers has been modularized to easily add new parsers
;; - keywords are customizable now

;;; Code:

(require 'emeteo-frob)
(require 'emeteo-parse)


(defconst emeteo-version-number "V0.1 $Revision$"
  "Version number.")
(defconst emeteo-version
  (format "emacs meteorological package %s"
          emeteo-version-number)
  "The full version string for emeteo.el")


(defgroup emeteo nil
  "General interface to meteorological data."
  :group 'applications)

(defcustom emeteo-timeout 2
  "Emeteo timeout value in seconds.
This defines how long to wait for the response of data requests."
  :group 'emeteo)


(defcustom emeteo-data-sources nil
  "Data source specifier list.
This defines how and where to fetch data.

It is a list of the form:
"
  :group 'emeteo)

;;; this will move to emeteo-stations soon
(defvar emeteo-url-alist
  '(
    ("B" "http://www.met.fu-berlin.de/de/wetter/")
    ;;("HRO" "http://www.landhaus-k.de/wetter/wetter.htm")
    ;;("MLT" "http://www.wetter.com/home/structure/control.php?sessionID=&Lang=DE&ms=1&ss=1&sss=1&id=700&type=WMO")
    )
  "old stuff ... soon to become obsolete.")

(defcustom emeteo-debug-p nil
  "Predicate for debugging retrieved weather buffers"
  :group 'emeteo)


(defvar emeteo-insinuated nil)

(defun emeteo-insinuate ()
  ;; (emeteo-init-glyphs)
  (sit-for 0)
  (setq emeteo-insinuated t))


(provide 'emeteo)

;;; emeteo.el ends here

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:
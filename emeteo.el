;; emeteo.el --- An Emacs Interface To Meteorological Data

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

;; emeteo is a general emacs interface to provide meteorologic data

;; For more information, see the following URLs:
;; * http://gna.org/projects/emeteo/


;; Installation:

;; Put emeteo.el in your load-path, and put (require 'emeteo) in your .emacs.
;; To optionally use the modeline feature, try (require 'emeteo-modeline) and
;; M-x emeteo RET :)


;; Configuration:

;; Use M-x customize-group RET emeteo RET to get an overview
;; of all the variables you can tweak.


;; Usage:
;; Within emacs-lisp you would probably want to invoke something like:
;;   (emeteo-fetch 'berlin)
;; or any other spec from `emeteo-data-sources'
;;
;; The return value will be of the form
;;   (berlin ((temp "10.0°")))
;; or something, a list with the specname in the car and an alist in the cdr
;;
;; BEWARE: the return value syntax might change during the alpha status.
;;
;;
;; The variable `emeteo-data-sources' is chunk-driven. This means you may add everything
;; you want as long as the list itself is of the form
;;   (data-identifier data)
;; _and_ data is entirely of the form :keyword value.
;; There is no predefined set of supported or necessary keywords. Everything that use this
;; variable has to make sure itself to not produce unexpected results due to missing 
;; keywords.
;;
;; On the other hand, most of the provided functionality lets you customize which keywords
;; to use for which purposes.
;;
;;
;; This project is just to demonstrate my definition of usability! >8) <- hroptatyr

;;; Code:

(require 'emeteo-frob)
(require 'emeteo-parse)
(require 'emeteo-utils)


(defconst emeteo-version-number "V0.3 $Revision$"
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
  :group 'emeteo
  :type 'integer)


(defcustom emeteo-data-sources nil
  "Data source specifier list.
This defines how and where to fetch data.

It is a list of entries of the form:

  \(data-identifier data\)

In turn data looks like a bunch of

  :keyword value

pairs.

This variable is chunk-driven. This means you may add everything
you want.
There is no predefined set of supported or necessary keywords.

On the other hand, most of the provided functionality within this 
repository lets you customize which keywords to use for which purposes.

See file `emeteo-stations.el' for an example initial value.
Also see example setups
(e.g. at http://www.math.tu-berlin.de/~freundt/.emeteo)."
  :group 'emeteo
  :type 'list)


(defcustom emeteo-default-spec-identifier 'berlin
  "Spec identifier (i.e. car in emeteo-data-sources alist) that should
be used as default."
  :group 'emeteo
  :type 'symbol)




(defcustom emeteo-fetch-chain
  '(
    emeteo-decide-data
    emeteo-valuate-data
    emeteo-parse-buffer
    emeteo-wash
    emeteo-frob-uri
    )
  "Chain of functions called from right to left, this defines
what a fetch should look like.

The functions return value should be the input of the next function,
i.e. a chain '\(A B C) is called like:
\(A (B (C param)))

The first function is given the params from the calling function."
  :group 'emeteo
  :type '(repeat (function :tag "emeteo fetch chain function")))



(defcustom emeteo-debug-p nil
  "Predicate for debugging retrieved weather buffers"
  :group 'emeteo
  :type 'boolean)


(defcustom emeteo-after-fetch-hook nil
  "Hook run after fetching (= frobbing and parsing) of metar data."
  :group 'emeteo
  :type '(repeat (symbol :value "" :tag "Function")))


(defvar emeteo-insinuated nil)


(defun emeteo-insinuate ()
  ;; (emeteo-init-glyphs)
  (sit-for 0)
  (setq emeteo-insinuated t))


;;; fetching

(defun emeteo-fetch-all (&optional specs-list)
  "Fetches metar information of all identifiers in `specs-list'
If optional `specs-list' is omitted `emeteo-data-sources' is used."
  (let* ((debug-buf (and emeteo-debug-p
                         (get-buffer-create "*emeteo debug*")))
         (specs-list (or specs-list
                         emeteo-data-sources)))
    (and emeteo-debug-p
         (erase-buffer debug-buf))
    (mapcar 'emeteo-fetch (mapcar 'car specs-list))))

(defun emeteo-fetch-random (&optional specs-list)
  (let* ((debug-buf (and emeteo-debug-p
                         (get-buffer-create "*emeteo debug*")))
         (specs-list (or specs-list
                         emeteo-data-sources)))
    (and emeteo-debug-p)
    (emeteo-fetch (emeteo-utils-random-choose (mapcar 'car specs-list)))))


(defun emeteo-fetch (&optional emeteo-spec specs-list)
  "Fetches metar information in the region `emeteo-spec'"
  (let* ((specs-list (or specs-list
                         emeteo-data-sources))
         (spec (assoc emeteo-spec specs-list))
         (uri (emeteo-utils-find-key-val ':uri spec))
         (debug-buf (and emeteo-debug-p
                         (get-buffer-create "*emeteo debug*"))))
    (and uri
         (let ((result (cons emeteo-spec (emeteo-fetch-uri uri))))
           (run-hook-with-args 'emeteo-after-fetch-hook result)
           result))))
;;(emeteo-fetch 'berlin)



(defun emeteo-fetch-uri (uri &optional fetch-chain)
  "Fetches according to `emeteo-fetch-chain'.
The rightmost function in `emeteo-fetch-chain' will be given URI.

Optional FETCH-CHAIN may specify another chain to be funcalled."
  (let* ((fetch-chain (append (or fetch-chain
                                  emeteo-fetch-chain)
                              uri)))
    (eval (emeteo-utils-composition-chain fetch-chain))))
;; (emeteo-fetch-uri "http://www.met.fu-berlin.de/de/wetter/")



(provide 'emeteo)

;;; emeteo.el ends here

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

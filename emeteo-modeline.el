;; emeteo-modeline.el --- modeline feature for emeteo

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
;; Put (require 'emeteo-modeline) in your ~/.emacs and try M-x emeteo

;; Usage:

;;; History:
;;

;;; Code:

(require 'timer)
(require 'emeteo)


(defgroup emeteo-modeline nil
  "Facilities to display metar data in the mode line or echo area."
  :prefix "emeteo-modeline-"
  :group 'emeteo)

(defcustom emeteo-modeline-interval 900
  "Seconds between updates of time in the mode line."
  :group 'emeteo-modeline
  :type 'integer)

(defvar emeteo-modeline-timer nil)
(defvar emeteo-modeline-string nil)


;;;###autoload
(defun emeteo ()
  "Display current weather information in mode line of each buffer.
Updates automatically every minute."
  (interactive)
  (or emeteo-insinuated
      (emeteo-insinuate))
  ;; if the "emeteo" timer already exists, nuke it first.
  (if (memq 'emeteo-modeline-string global-mode-string)
      (setq global-mode-string
            (remove 'emeteo-modeline-string global-mode-string)))

  (setq global-mode-string
        (append global-mode-string '(emeteo-modeline-string)))

  ;; initially display
  (emeteo-modeline-function)

  ;; ... and start a timer to do it automatically thereafter.
  (setq emeteo-modeline-timer
        (run-at-time emeteo-modeline-interval emeteo-modeline-interval 'emeteo-modeline-function)))


(defun emeteo-stop ()
  (interactive)
  (and emeteo-modeline-timer
       (cancel-timer emeteo-modeline-timer))
  (setq emeteo-modeline-timer nil)
  (setq emeteo-modeline-string nil))


(defvar emeteo-insinuated nil)

;; This used to be at top-level!
(defun emeteo-insinuate ()
  (sit-for 0)
  (setq emeteo-insinuated t))


(defun emeteo-modeline-function ()
  (let* ((metinfo (emeteo-froball)))
    (setq emeteo-modeline-string
          (concat " "
                  (mapconcat (lambda (metspec)
                               (let* ((temp (cdr-safe (assoc 'temp (cdr-safe metspec))))
                                      (region (car-safe metspec)))
                                 (concat region
                                         ":"
                                         (or (car-safe temp)
                                             temp
                                             "n/a"))))
                             metinfo " ")))
    ;; This is inside the let binding, but we are not going to document
    ;; what variables are available.
    (run-hooks 'emeteo-modeline-hook)
    (force-mode-line-update)
    ;; Do redisplay right now, if no input pending.
    (sit-for 0)))
;;(emeteo-modeline-function)

(provide 'emeteo-modeline)

;;; emeteo-modeline.el ends here

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

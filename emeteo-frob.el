;; emeteo-frob.el --- Frobnication

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

;; This is to provide frobnication of data from different sources

;; For more information, see the following URLs:
;; * http://sf.net/projects/emeteo/
;; * http://gna.org/projects/emeteo/

;;; History:
;;

;;; Code:


(defun emeteo-frob-split-uri (uri)
  "Splits URI `uri' to a list (hostname protocol filename)."
  (let* ((raw-split (split-string uri "://"))
         (protocol (car raw-split))
         (raw-split (cadr raw-split))
         (hostname (progn
                     (string-match "^\\([^/]+\\)/?\\(.*\\)$" raw-split)
                     (match-string 1 raw-split)))
         (filename (match-string 2 raw-split)))
    `(,hostname ,protocol ,filename)))
;; (emeteo-frob-split-uri "http://www.met.fu-berlin.de/de/wetter/")
;;;; we also wanna split uris like these "www.met.fu-berlin.de/de/wetter"


;;; thanks to Edward O'Connor (http://www.emacswiki.org/cgi-bin/wiki.pl/EdwardOConnor)
;;; thanks also to hynek who found this one :)
(defun emeteo-frob-uri (uri)
  "Fetch http-stream."
  (let* ((uri-spec (emeteo-frob-split-uri uri))
         (hostname (car uri-spec))
         (protocol (car (cdr-safe uri-spec)))
         (filename (car (cdr-safe (cdr-safe uri-spec))))
         (buf (get-buffer-create "*emeteo*"))
         (proc (open-network-stream "emeteo-frob-http"
                                    (prog1
                                        buf
                                      (with-current-buffer buf
                                        (erase-buffer)))
                                    hostname
                                    protocol))
         (debug-buf (and emeteo-debug-p
                         (get-buffer-create "*weather*"))))
    (process-send-string proc
                         ;; This should also not be hardcoded.
                         (format "GET /%s HTTP/1.0\r\n\r\n" filename))

    ;; Watch us spin and stop Emacs from doing anything else!
    (while (equal (process-status proc) 'open)
      (when (not (accept-process-output proc emeteo-timeout))
        (delete-process proc)
        (error "Network timeout!")))

    (with-current-buffer buf
      (goto-char (point-min))
      (if (looking-at "^HTTP/...\\s-+200\\s-OK$")
          buf))))
;; (emeteo-frob-uri "http://www.met.fu-berlin.de/de/wetter/")



(provide 'emeteo-frob)

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

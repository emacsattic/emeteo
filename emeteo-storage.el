;; emeteo-storage.el --- Store Incoming Metar Data

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

;; This file provides storage of incoming metar data.

;; For more information, see the following URLs:
;; * http://gna.org/projects/emeteo/


;; Installation:
;; 
;; Add (require 'emeteo-storage) to your .emacs
;; and invoke
;;   M-x emeteo-storage-enable RET

;; Configuration:


;; Usage:


;;; Code:

(require 'emeteo)


(defgroup emeteo-storage nil
  "General interface to meteorological data."
  :group 'emeteo
  :prefix "emeteo-storage-")


(defvar emeteo-storage-hash-table (make-hash-table :test 'equal)
  "Main hash table for `emeteo-storage-function'.")


(defun emeteo-storage-enable ()
  ""
  (interactive)
  (add-hook 'emeteo-after-fetch-hook 'emeteo-storage-function))

(defun emeteo-storage-disable ()
  ""
  (interactive)
  (remove-hook 'emeteo-after-fetch-hook 'emeteo-storage-function))


(defun emeteo-storage-function (result)
  ""
  (puthash (current-time) (vconcat result) emeteo-storage-hash-table))

(defun emeteo-storage-pump-hash-table-to-file (&optional file hash-table-name)
  ""
  (interactive)
  (let ((file (or file
                  (expand-file-name ".emeteo.storage.eld" "~")))
        (hash-table-name (or hash-table-name
                             'emeteo-storage-hash-table)))
    (with-temp-buffer 
      (insert ";;; -*- emacs-lisp -*-\n;; emeteo storage\n\n")
      (maphash (lambda (key val)
                 (insert (format "[%s %s]\n" key val)))
               emeteo-storage-hash-table)
      (write-file file))))

(defun emeteo-storage-pump-file-to-hash-table (&optional file hash-table-name)
  ""
  (interactive)
  (let* ((file (or file
                   (expand-file-name ".emeteo.storage.eld" "~")))
         (hash-table-name (or hash-table-name
                              'emeteo-storage-hash-table))
         (file-vector
          (with-temp-buffer
            (insert-file-contents file)
            (eval (read (format "(setq file-vector '(%s))" (buffer-string)))))))
    (mapvector (lambda (vecelt)
                 (let ((key (aref vecelt 0))
                       (val (aref vecelt 1)))
                   (puthash key val (eval hash-table-name))))
               file-vector)))


(provide 'emeteo-storage)

;;; emeteo-storage.el ends here

;; Local variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

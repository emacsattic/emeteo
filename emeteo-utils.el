;; emeteo-utils.el --- Parsing utilities

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
;; * these are entirely auxiliary defuns

;; For more information, see the following URLs:
;; * http://gna.org/projects/emeteo/

;;; History:
;;

;;; Code:

(require 'cl)


;;; combinations of sets
(defun emeteo-utils-product-set (set &optional mult-function)
  "Takes a list of lists and semi-multiplies its elements.
Returns a list mapconcattable for use as regexp."
  (let ((mult-function (or mult-function
                           'cons)))
    (emeteo-utils-flatten 
     (maplist (lambda (list)
                (let ((listcar (car list))
                      (listcdr (append '(()) (cdr list))))
                  (emeteo-utils-halfmultiply-set+sets listcar listcdr mult-function)))
              set))))
;; (emeteo-utils-product-set emeteo-temperature-units 'concat)
(defun emeteo-utils-halfmultiply-set+sets (set1 sets &optional mult-function)
  "Halfmultiplies each element from SET1 with all elements
of the elements of SETS."
  (let ((mult-function (or mult-function
                           'cons)))
    (mapcar (lambda (elt)
              (emeteo-utils-halfmultiply-elt+sets elt sets mult-function))
            set1)))
;; (emeteo-utils-halfmultiply-set+sets '(a b) '((a b) (c d)))
(defun emeteo-utils-halfmultiply-elt+sets (elt sets &optional mult-function)
  "Multiplies ELT with all elements of the elements
of SETS."
  (let ((mult-function (or mult-function
                           'cons)))
    (mapcar (lambda (set)
              (emeteo-utils-halfmultiply-elt+set elt set mult-function))
            sets)))
(defun emeteo-utils-halfmultiply-elt+set (elt set &optional mult-function)
  "Multiplies ELT with all elements of SET."
  (let ((mult-function (or mult-function
                           'cons)))
    (or (mapcar (lambda (setelt)
                  (funcall mult-function elt setelt))
                set)
        elt)))
;; (emeteo-utils-halfmultiply-elt+set 'a '(c d) 'cons)



;;; list/tree/set funs
(defun emeteo-utils-flatten (tree)
  "Flattens TREE."
  (cond
   ((null tree) nil)
   ((listp tree) (apply 'append
      (mapcar 'emeteo-utils-flatten tree)))
   (t (list tree))))
 
(defun emeteo-utils-find-key-val (keyword list)
  "Finds KEYWORD in list and returns its value"
  (let* ((keyw (cond ((keywordp keyword)
                      keyword)
                     ((symbolp keyword)
                      (intern (format ":%s" keyword)))
                     (t nil)))
         (keypos (position keyw list)))
    (and keypos
         (nth (1+ keypos) list))))

(defun emeteo-utils-composition-chain (chain)
  "Creates the composition of the CHAIN of functions, i.e.
'\(a b c d) => (a (b (c d))), the last argument in a chain
should be the param given to the function before the last."
  (and chain
       (or (and (car-safe chain)
                (cons (car chain) (list (emeteo-utils-composition-chain (cdr-safe chain)))))
           chain)))


(defun emeteo-utils-random-choose (list)
  "Chooses one element of list randomly and returns it."
  (nth (random (length list)) list))
;;(emeteo-utils-random-choose '(beer wine wodka whisky))




;;; Formatting and markup funs
(defcustom emeteo-default-fail-indicator-string "n/a"
  "String used as a generic indicator to announce
that fetching has failed."
  :group 'emeteo
  :type 'string)
(defun emeteo-utils-format (fullspec result format &optional fail-indicator-string)
  "This is the main formatting fun."
  (let* ((fail-indicator-string (or fail-indicator-string
                                    emeteo-default-fail-indicator-string))
         (formatstr (car format))
         (format (cdr-safe format))
         (raw
          (mapcar (lambda (keyw-or-res)
                    (cond ((keywordp keyw-or-res)
                           (or (emeteo-utils-find-key-val keyw-or-res fullspec)
                               " "))
                          ((symbolp keyw-or-res)
                           (or (cdr-safe (assoc keyw-or-res result))
                               fail-indicator-string))
                          ((stringp keyw-or-res)
                           keyw-or-res)
                          (t nil)))
                  format)))
    (or (and raw
             (eval (cons 'format
                         (cons formatstr raw))))
        fail-indicator-string)))
;;(emeteo-utils-format (assoc 'berlin emeteo-data-sources) (emeteo-fetch 'berlin) '("%s:%s%s" :shortname temp :unit-string))



(provide 'emeteo-utils)

;;; emeteo-utils.el ends here

;; Local-variables:
;; indent-tabs-mode: nil
;; tab-width: 2
;; End:

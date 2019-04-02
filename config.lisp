;;; Copyright (C) 2019  Sony Computer Science Laboratories Paris
;;;                     Remi van Trijp (www.remivantrijp.eu)
;;; 
;;;     This program is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation, version 3 of the License.
;;; 
;;;     This program is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;; 
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------------

(in-package :fcg)

;;;;; Setting the BENG pathname.
;;;;; --------------------------------------------------------------------------------
(defparameter *beng-pathname* (babel-pathname :directory '(:up "grammars" "English"))
  "Directory of the English grammar. Please modify this if you have not installed it in this path.")

(defun beng-pathname (&key directory name type)
  "Helper function for accessing files of the English grammar."
  (merge-pathnames  (make-pathname :directory  (cons :relative directory)
				   :name name :type type)
		    *beng-pathname*))
  
;;;;; Configuration utilities for manipulating the configuration of the grammar.
;;;;; --------------------------------------------------------------------------------
(defun set-parse-order (order &optional (cxn-inventory *fcg-constructions*))
  (set-configuration cxn-inventory :parse-order order)
  (set-configuration (processing-cxn-inventory cxn-inventory)
                     :parse-order order))

(defun set-production-order (order &optional (cxn-inventory *fcg-constructions*))
  (set-configuration cxn-inventory :production-order order)
  (set-configuration (processing-cxn-inventory cxn-inventory)
                     :production-order order))

;;;;; Header used for writing files.
;;;;; --------------------------------------------------------------------------------
(defun add-license-and-copyright-header (out)
  (format out ";;; Copyright (C) 2019  Sony Computer Science Laboratories Paris")
  (format out "~%;;;                     Remi van Trijp (www.remivantrijp.eu)")
  (format out "~%;;; ")
  (format out "~%;;;     This program is free software: you can redistribute it and/or modify")
  (format out "~%;;;     it under the terms of the GNU General Public License as published by")
  (format out "~%;;;     the Free Software Foundation, version 3 of the License.")
  (format out "~%;;; ")
  (format out "~%;;;     This program is distributed in the hope that it will be useful,")
  (format out "~%;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of")
  (format out "~%;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the")
  (format out "~%;;;     GNU General Public License for more details.")
  (format out "~%;;; ")
  (format out "~%;;;     You should have received a copy of the GNU General Public License")
  (format out "~%;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>.")
  (format out "~%;;; ----------------------------------------------------------------------------"))
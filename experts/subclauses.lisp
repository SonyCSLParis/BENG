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

(in-package :beng)

(defun beng-identify-subclauses (transient-structure &optional (cxn-inventory *fcg-constructions*))
  "Return all of the subclauses of a structure."
  (let* ((units (fcg-get-transient-unit-structure transient-structure))
         (boundaries (fcg-get-boundaries units))
         (sub-clause-pattern `(?clause
                               (syn-cat (==1 (is-matrix-clause -)))))
         ;; Step 1: We identify the subclauses and their boundaries.
         (subclauses (loop for unit in units
                         for match = (fcg::unify-units sub-clause-pattern
                                                       unit
                                                       (list +no-bindings+)
                                                       :cxn-inventory cxn-inventory)
                         when match
                         collect (assoc (unit-name unit) boundaries)))
         (utterance (get-data transient-structure :utterance-as-list)))
    (loop for subclause in subclauses
          collect (subseq utterance (second subclause) (third subclause)))))
;; (beng-identify-subclauses *saved-cfs*)

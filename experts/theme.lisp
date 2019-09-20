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

(defun beng-get-theme (transient-structure &optional (cxn-inventory *fcg-constructions*))
  (let* ((units (fcg-get-transient-unit-structure transient-structure))
         (boundaries (fcg-get-boundaries units))
         (main-clause-pattern `(?clause
                                (syn-cat (==1 (is-matrix-clause +)))
                                (theme-rheme (==1 (theme ?theme)))))
         ;; Step 1: We identify the main clause and get bindings for the info we want.
         (bindings (loop for unit in units
                         for match = (fcg::unify-units main-clause-pattern
                                                       unit
                                                       (list +no-bindings+)
                                          :cxn-inventory cxn-inventory)
                         when match
                         return (first match)))
         ;; Step 2: We retrieve the boundaries of the theme.
         (boundaries-theme (assoc (rest (get-binding '?theme bindings)) boundaries)))
    (subseq (get-data transient-structure :utterance-as-list) (second boundaries-theme) (third boundaries-theme))))
;; (beng-get-theme *saved-cfs*)

(defun beng-get-rheme (transient-structure &optional (cxn-inventory *fcg-constructions*))
  "Return the rheme."
    (let* ((units (fcg-get-transient-unit-structure transient-structure))
           (boundaries (fcg-get-boundaries units))
           (main-clause-pattern `(?clause
                                  (syn-cat (==1 (is-matrix-clause +)))
                                  (theme-rheme (==1 (theme ?theme)))))
           ;; Step 1: We identify the main clause and get bindings for the info we want.
           (bindings (loop for unit in units
                           for match = (fcg::unify-units main-clause-pattern
                                                         unit
                                                         (list +no-bindings+)
                                                         :cxn-inventory cxn-inventory)
                           when match
                           return (first match)))
           ;; Step 2: We retrieve the boundaries of the theme.
           (boundaries-theme (assoc (rest (get-binding '?theme bindings)) boundaries)))
      (subseq (get-data transient-structure :utterance-as-list) (third boundaries-theme))))

  
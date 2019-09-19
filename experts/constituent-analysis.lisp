;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

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

(defun filter-constituency-boundaries (boundaries units)
  (loop for boundary in boundaries
        for corresponding-unit = (assoc (first boundary) units)
        when (unit-feature-value corresponding-unit 'constituents)
        collect boundary))

(defun beng-get-constituent-structure (transient-structure)
  "Returns the sentence's constituent structure in bracketed notation."
  (let* ((utterance (get-data transient-structure :utterance-as-list))
         (l (length utterance))
         (sequence (get-data transient-structure :sequence))
         (units (fcg-get-transient-unit-structure transient-structure))
         (boundaries (filter-constituency-boundaries (fcg-get-boundaries units) units))
         (opening-bracket-positions (loop for i until (> i l)
                                          collect (list i 0)))
         (closing-bracket-positions (loop for i until (> i l)
                                          collect (list i 0))))
    (dolist (boundary boundaries)
      (incf (second (assoc (second boundary) opening-bracket-positions)))
      (incf (second (assoc (1- (third boundary)) closing-bracket-positions))))
    (let ((string ; (apply #'string-append
           (format nil "~{~a~}"
                   (loop for i until (= (1+ i) l)
                         collect (format nil "~a~s~a "
                                         (apply #'string-append
                                                (loop for ob until
                                                      (= ob (second (assoc i opening-bracket-positions)))
                                                      collect "("))
                                         (nth i utterance)
                                         (apply #'string-append
                                                (loop for ob until
                                                      (= ob (second (assoc i closing-bracket-positions)))
                                                      collect ")")))))))
      (read-from-string (subseq string 0 (1- (length string)))))))

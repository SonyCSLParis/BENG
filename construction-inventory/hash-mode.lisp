;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Author: Remi van Trijp (www.remivantrijp.eu)
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

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; Hash methods
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod hash ((construction construction)
                 (mode (eql :hash-english-grammar))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning)
            (attr-val construction :lex-id)
            (attr-val construction :evoked-by))
    (remove-duplicates
     (remove nil (list (attr-val construction :string)
                       (attr-val construction :meaning)
                       (attr-val construction :lex-id)
                       (attr-val construction :evoked-by))))))

(defun hash-lex-id (node)
  "We extract the values of the feature lex-id for each unit."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lex-id = (or (second (assoc 'sem-frame (unit-body unit) :test #'string=))
                         (second (assoc 'lex-id (unit-body unit) :test #'string=))) ;; lex-id should disappear from construction.
        when lex-id
        collect it))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-english-grammar))
                 &key (label t))
  "Checks the root and returns information for fast processing."
  (cond ((string= label "HASHED-LEX-ID") (hash-lex-id node))
        (t (let ((root (get-root (fcg-get-transient-unit-structure node))))
             (if (eql (fcg-get-direction node) '<-)
               ;; In parsing we are interested in strings
               (mapcar #'third (extract-strings (list root)))
               ;; In production we are interested in meaninsg
               (loop for meaning in (extract-meaning root)
                     for semantics = (or (get-configuration (construction-inventory node) :semantics)
                                         t)
                     when (beng-extract-hash-label meaning semantics)
                     collect it))))))

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

;; We overwrite FCG's function UNIFY-ATOM in order to allow:
;; (1) Strings to be case insensitive;
;; (2) To be reincorporated: use a FUSION hierarchy. This hiearchy can be
;;     considered to be a simplified version of Paul Van Eecke's category
;;     hierarchy system.
;; ------------------------------------------------------------------------------

(defun unify-atom (x y bindings &key cxn-inventory)
  "unify-atom function for use with type-hierarchies"
  (declare (ignore cxn-inventory))
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
        ;; Allow strings to be case-insensitive
        ((and (stringp x) (stringp y) (string= (downcase x) (downcase y))) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)) bindings))
        ;; unify variables
        ((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
        ;; To be reincorporated:
        ;; unify symbols on type-hierarchy-basis
        ;; ((and (symbolp x) (symbolp y)
        ;;      (beng::fuseable-p y x (beng::get-fusion-hierarchy cxn-inventory)))
        ;;  bindings)
	(t
         (values +fail+ x y))))


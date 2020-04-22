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

;; We overwrite FCG's function UNIFY-ATOM in order to allow symbol matching to
;; check for compatibility in a fusion hierarchy. This hiearchy is similar to
;; Paul Van Eecke's category hierarchy system, but is simpler in setup for
;; efficiency reasons.
;; ------------------------------------------------------------------------------

(defun unify-atom (x y bindings &key cxn-inventory)
  "unify-atom function for use with type-hierarchies"
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)) bindings))
        ;; unify variables
        ((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
        ;; unify symbols on type-hierarchy-basis
        ((and (symbolp x) (symbolp y)
              (beng::fuseable-p y x (beng::get-fusion-hierarchy cxn-inventory)))
         bindings)
	(t
         (values +fail+ x y))))

;;; (defun unify-features (f1 f2 bsl &key cxn-inventory)
;;;   (cond ((eq 'TAG (unit-name f1))
;;; 	 (unify-tags f1 f2 bsl #'(lambda (f1 f2 bsl &key cxn-inventory)
;;; 				   (unify-features f1 f2 bsl :cxn-inventory cxn-inventory))
;;;                      :cxn-inventory cxn-inventory))
;;; 	((unify-atom (feature-name f1) (feature-name f2) bsl :cxn-inventory cxn-inventory)
;;;          (unify (feature-value f1)
;;;                 (feature-value f2)
;;;                 bsl
;;;                 :cxn-inventory cxn-inventory))
;;;         (t
;;;          nil)))

;;; ;;; (defun merge-features (f1 f2 bindings &key cutoff cxn-inventory)
;;;   (cond ((eq 'TAG (feature-name f1))
;;; 	 (merge-tags f1 f2 bindings :merge-fn #'merge-features :cutoff cutoff :cxn-inventory cxn-inventory))
;;;         ((or (eq (feature-name f1) (feature-name f2))
;;;              (unify-atom (feature-name f1) (feature-name f2) bindings :cxn-inventory cxn-inventory))           
;;; 	 (let* ((pattern (feature-value f1))
;;; 		(value-merges
;;;                  (fcg-merge pattern
;;;                             (feature-value f2)
;;;                             bindings :cutoff cutoff :cxn-inventory cxn-inventory))
;;;                 (result nil))
;;;           (dolist (mr value-merges)
;;;             (setf (mr-expr mr) 
;;;                   (make-feature (feature-name f1) (mr-expr mr)))
;;;             (when (mr-added mr)
;;;               (setf (mr-added mr)
;;;                     (list (list (feature-name f1) (mr-added mr)))))
;;;             (if (= 1 (length (mr-bsl mr)))
;;;               (push mr result)
;;;               (dolist (bs (mr-bsl mr))
;;;                 (let* ((e (substitute-bindings bs (mr-expr mr)))
;;;                        (prev (find e result :key #'mr-expr :test #'equalp)))
;;;                   (if prev 
;;;                     (push bs (mr-bsl prev))
;;;                     (push (make-merge-result e (list bs))
;;;                           result))))))
;;;           result))))
;;; ;; (comprehend "the window broke")

;;; (defun unify-unit-features (fs1 fs2 bsl &key cxn-inventory)
;;;   (loop for f1 in fs1 while bsl do
;;;        (let* ((f1-name (if (tag-p f1)
;;;                          (feature-name (third f1))
;;;                          (feature-name f1)))
;;;               (f2 (loop for feature in fs2
;;;                         when (unify-atom f1-name (feature-name feature) bsl :cxn-inventory cxn-inventory)
;;;                         return feature)))
;;; 	 (if f2
;;;            ;; If feature with same feature-name is found in fs2 (transient structure unit)
;;;            ;; then unify f1 with f2
;;;            (setq bsl (unify-features f1 f2 bsl :cxn-inventory cxn-inventory))
;;;            ;; otherwise, unify it with (feature-name nil)
;;;            ;; This case is for example often used for footprints
;;;            (setq bsl (unify-features f1 (make-feature (feature-name f1) nil) bsl :cxn-inventory cxn-inventory)))))
;;;   bsl)
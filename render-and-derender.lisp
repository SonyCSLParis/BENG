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

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; CONSTITUENT STRUCTURE
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defgeneric represent-constituent-structure (analysis transient-structure key cxn-inventory &optional language))

;; TRANSLATING THE CONSTITUENT STRUCTURE FROM A SPACY DEPENDENCY ANALYSIS
;; ------------------------------------------------------------------------

;; Code based on and using the fcg-hybrids code:
;; https://github.com/SonyCSLParis/fcg-hybrids                                          

(defmethod represent-constituent-structure ((analysis list)
                                            (transient-structure coupled-feature-structure)
                                            (key t) ;; By default we assume that we have to translate dependencies
                                            (cxn-inventory t)
                                            &optional (language t)) ;; By default we assume English
  ;; When no statistical/neural constituent-analysis is provided, we translate the dependency tree.
  (declare (ignore key language))
  (let* ((boundaries (fcg-get-boundaries transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries analysis))
         (dependency-root (find "ROOT" word-specs :key #'word-dependency-spec-syn-role :test #'string=))
         (units-without-root (loop for unit in (fcg-get-transient-unit-structure transient-structure)
                                   unless (string= "ROOT" (unit-name unit))
                                   collect unit))
         (expanded-unit-structure (if (verbal-root-p dependency-root)
                                    (handle-verbal-root dependency-root word-specs 'matrix-clause '-
                                                        (make-const "clause") units-without-root)
                                    (handle-functional-unit "ROOT" dependency-root
                                                            (make-const 'unit)
                                                            '- word-specs units-without-root))))
    (setf (left-pole-structure transient-structure)
          (cons (get-root (left-pole-structure transient-structure))
                expanded-unit-structure))
    (let ((new-root (calculate-boundaries-and-form-constraints transient-structure nil cxn-inventory)))
      (setf (left-pole-structure transient-structure)
            (cons new-root expanded-unit-structure))
      transient-structure)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; FUNCTIONAL STRUCTURE
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defgeneric represent-functional-structure (dependency-tree transient-structure key &optional language))

(defmethod represent-functional-structure ((dependency-tree list)
                                           (transient-structure coupled-feature-structure)
                                           (key t) ;; The default is the spaCy model for English.
                                           &optional (language t))
  "Given a dependency tree from spacy"
  (declare (ignore key language))
  (let* ((boundaries (fcg-get-boundaries transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         (structure-to-append (loop for word-spec in word-specs
                                    for dependents =
                                    (loop for other-word-spec in word-specs
                                          when (and (not (equal word-spec other-word-spec))
                                                    (= (word-dependency-spec-head-id other-word-spec)
                                                       (word-dependency-spec-node-id word-spec)))
                                          collect (word-dependency-spec-unit-name other-word-spec))
                                    for functional-structure =
                                    (when (string= (word-dependency-spec-syn-role word-spec)
                                                   "ROOT")
                                      (loop for other-word-spec in word-specs
                                            for function = (word-dependency-spec-syn-role
                                                            other-word-spec)
                                            when (core-function-p function)
                                            collect (cond
                                                     ((subject-p function)
                                                      `(subject
                                                        ,(word-dependency-spec-unit-name
                                                          other-word-spec)))
                                                     ((object-p function)
                                                      `(object ,(word-dependency-spec-unit-name
                                                                 other-word-spec)))
                                                     (t
                                                      `(indirect-object
                                                        ,(word-dependency-spec-unit-name
                                                          other-word-spec))))))
                                    collect
                                    (make-unit :name (word-dependency-spec-unit-name word-spec)
                                               :features `((dependents ,dependents)
                                                           ,@(if functional-structure
                                                               `((functional-structure ,functional-structure)
                                                                 ,@(dolist (other-spec word-specs)
                                                                     (cond ((passive-subject-p
                                                                             (word-dependency-spec-syn-role
                                                                              other-spec))
                                                                            (return `((voice passive))))
                                                                           ((subject-p
                                                                             (word-dependency-spec-syn-role
                                                                              other-spec))
                                                                            (return `((voice active))))
                                                                           (t
                                                                            nil))))))))))
    (setf (left-pole-structure transient-structure)
          (append (left-pole-structure transient-structure) structure-to-append))
    transient-structure))

(defmethod de-render ((utterance string) (mode (eql :beng))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  ;; Step 1: We obtain a syntactic dependency analysis from SpaCy.
  (multiple-value-bind (dependency-tree utterance-as-list transient-structure)
      (preprocess-using-dependency-tree utterance :cxn-inventory cxn-inventory)
    (declare (ignorable utterance-as-list))
    ;; Step 2: We flesh out the dependency analysis by adding features and values
    (setf transient-structure
          (represent-functional-structure dependency-tree transient-structure t :english))
    ;; Step 3: We flesh out the constituency analysis by adding features and values
    (setf transient-structure
          (represent-constituent-structure dependency-tree transient-structure t cxn-inventory :english))
    ;; Finally we return the transient structure.
    transient-structure))
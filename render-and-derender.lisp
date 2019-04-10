;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :fcg)

(defmethod de-render ((utterance string) (mode (eql :beng))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (multiple-value-bind (dependency-tree utterance-as-list base-transient-structure)
      (preprocess-using-dependency-tree utterance :cxn-inventory cxn-inventory)
    (let* ((boundaries (fcg-get-boundaries base-transient-structure))
           (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
           (units-id-and-name
            (loop for spec in word-specs
                  collect (list (word-dependency-spec-node-id spec)
                                (word-dependency-spec-unit-name spec))))
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
                                              when (fcg::core-function-p function)
                                              collect (cond
                                                       ((fcg::subject-p function)
                                                        `(subject
                                                          ,(word-dependency-spec-unit-name
                                                            other-word-spec)))
                                                       ((fcg::object-p function)
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
                                                                       (cond ((fcg::passive-subject-p
                                                                               (word-dependency-spec-syn-role
                                                                                other-spec))
                                                                              (return `((voice passive))))
                                                                             ((fcg::subject-p
                                                                               (word-dependency-spec-syn-role
                                                                                other-spec))
                                                                              (return `((voice active))))
                                                                             (t
                                                                              nil))))))))))
      (setf (left-pole-structure base-transient-structure)
            (append (left-pole-structure base-transient-structure) structure-to-append))
      base-transient-structure)))

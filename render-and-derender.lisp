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

(defun terminal-node-p (node)
  (if (loop for x in (rest node)
            when (listp x)
            return t)
    nil
    t))
;; (terminal-node-p '(nnp luc steels))

(defun has-constituents-p (unit)
  (unit-feature-value unit 'constituents))
;; (has-constituents-p '(np (constituents det n)))
;; (has-constituents-p '(n (lex-id test)))

(defun make-unit-id (x)
  (make-id (format nil "~a-unit" x)))
;; (make-unit-id 's)

(defmethod represent-constituent-structure ((constituent-tree list)
                                            (transient-structure coupled-feature-structure)
                                            (key (eql :benepar))
                                            (cxn-inventory t)
                                            &optional (language t)) ;; English by default
  (declare (ignore key language))
  (let* ((original-unit-names (mapcar #'first (fcg-get-boundaries transient-structure)))
         (original-units (fcg-get-transient-unit-structure transient-structure))
         (units nil))
    ;; We will first traverse the tree and create a list of units that have parent- and
    ;; constituent-features. We will afterwards replace the terminal nodes with the original
    ;; units from the dependency tree so we merge the constituent- and dependency-tree information.
    (let ((queue (list (list nil (make-unit-id (first constituent-tree)) constituent-tree (first constituent-tree)))))
      (tagbody
       start
       ;; If we have finished every unit, go to the end.
       (when (null queue)
         (go end))
       ;; We now handle the first element in the queue.
       (let* ((current (pop queue))
              (parent (first current)) ;; E.g. s-unit
              (unit-name (second current)) ;; e.g. np-unit
              (unit-spec (third current)) ;; e.g. ((ART "the") (NN "cat")
              (category (fourth current))) ;; e.g. NP
         ;; If it is a terminal node, simply create a unit for it.
         (if (terminal-node-p unit-spec)
           (push `(,unit-name
                   (parent ,parent)) units)
           ;; If it is not a terminal node, create a unit with constituents.
           ;; We will add their children to the front of the queue.
           (let ((new-nodes-to-queue (loop for constituent in (rest unit-spec)
                                           collect (list unit-name
                                                         (make-unit-id (first constituent))
                                                         constituent
                                                         (first constituent)))))
             (setf queue (append new-nodes-to-queue queue))
             (push `(,unit-name
                     ,@(find-all-features-for-category (fetch-benepar-category category) *english-grammar-categories*
                                                       :features-so-far `(,@(if parent `((parent ,parent)))
                                                                          (constituents ,(mapcar #'second new-nodes-to-queue))
                                                                          (node-accessor
                                                                           ,(format nil "~a-~a" category (length new-nodes-to-queue))))))
                   units)))
       ;; Now we reiterate
       (go start))
       end))
    ;; Now we have all the units, we can collect all the terminal-node units.
    (setf units (reverse units))
    (let ((terminal-node-units (loop for unit in units
                                     unless (has-constituents-p unit)
                                     collect unit))
          (subst-pairs nil))
      ;; We now replace those units with the original units from the dependency tree for
      ;; unit name consistency.
      (loop for constituent-tree-unit in terminal-node-units
            for original-unit-name in original-unit-names
            do (let* ((original-unit (assoc original-unit-name original-units))
                      (merged-unit `(,(first original-unit)
                                     ,@(append (rest constituent-tree-unit)
                                               (rest original-unit)))))
                 (push (cons (first constituent-tree-unit) original-unit-name) subst-pairs)
                 (setf units (subst merged-unit constituent-tree-unit units :test #'equal))))
      (setf units (sublis subst-pairs units))
      ;; Now we have the new unit structure.
      (setf (left-pole-structure transient-structure)
            (cons (get-root original-units) units))
      ;; We need to recalculate the boundaries and add a root unit:
      (let ((new-root (calculate-boundaries-and-form-constraints transient-structure nil cxn-inventory)))
        (setf (left-pole-structure transient-structure)
              (cons new-root units))
        transient-structure))))
;; (de-render "Luc Steels was the first director of Sony CSL Paris." :beng-spacy-benepar)
;; (comprehend "if the can get sufficient funding")
;; (dev-tools:dev-construction-tutor)

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
         (expanded-unit-structure (cond ((verbal-root-p dependency-root)
                                         (handle-verbal-root dependency-root word-specs 'matrix-clause '-
                                                             (make-const "clause") units-without-root))
                                        ((word-dependency-spec-conjunct-type dependency-root)
                                         (handle-functional-conjuncts "ROOT" dependency-root
                                                                      (make-const 'unit)
                                                                      '- word-specs units-without-root))
                                        (t
                                         (handle-functional-unit "ROOT" dependency-root
                                                                 (make-const 'unit)
                                                                 '- word-specs units-without-root)))))
    (setf (left-pole-structure transient-structure)
          (cons (get-root (left-pole-structure transient-structure))
                expanded-unit-structure))
    (let ((new-root (calculate-boundaries-and-form-constraints transient-structure nil cxn-inventory)))
      (setf (left-pole-structure transient-structure)
            (cons new-root expanded-unit-structure))
      transient-structure)))
;; (comprehend "Mickey Mouse, Donald Duck and Goofy are popular cartoon characters.")

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
                                    for dependent-word-specs =
                                    (loop for other-word-spec in word-specs
                                          when (and (not (equal word-spec other-word-spec))
                                                    (= (word-dependency-spec-head-id other-word-spec)
                                                       (word-dependency-spec-node-id word-spec)))
                                          collect other-word-spec)
                                    for dependents = (mapcar #'word-dependency-spec-unit-name dependent-word-specs)
                                    for functional-structure =
                                    (loop for other-word-spec in dependent-word-specs
                                          for function = (word-dependency-spec-syn-role
                                                          other-word-spec)
                                          collect (cond
                                                   ((subject-p function)
                                                    `(subject
                                                      ,(word-dependency-spec-unit-name
                                                        other-word-spec)))
                                                   ((object-p function)
                                                    `(object ,(word-dependency-spec-unit-name
                                                               other-word-spec)))
                                                   ((indirect-object-p function)
                                                    `(indirect-object
                                                      ,(word-dependency-spec-unit-name
                                                        other-word-spec)))
                                                   ((adverbial-modifier-p function)
                                                    `(adv-modifier
                                                      ,(word-dependency-spec-unit-name
                                                        other-word-spec)))
                                                   (t
                                                    nil)))
                                    collect
                                    (make-unit :name (word-dependency-spec-unit-name word-spec)
                                               :features
                                               (find-all-features-for-category
                                                (english-retrieve-category word-spec)
                                                *english-grammar-categories*
                                                :features-so-far `((dependents ,dependents)
                                                                   (pos ,(word-dependency-spec-pos-tag word-spec))
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
                                                                                    nil)))))))))))
    (setf (left-pole-structure transient-structure)
          (append (left-pole-structure transient-structure) structure-to-append))
    transient-structure))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; THEME-RHEME STRUCTURE
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defgeneric represent-theme-rheme-structure (analysis transient-structure key cxn-inventory &optional language))

(defmethod represent-theme-rheme-structure ((analysis list)
                                            (transient-structure coupled-feature-structure)
                                            (key t) ;; By default we assume that we have to translate dependencies
                                            (cxn-inventory t)
                                            &optional (language t)) ;; By default we assume English
  (let* ((clausal-unit-pattern '(?unit
                                 (syn-cat (==1 (clause-type ?clause-type)))
                                 (constituents ?constituents)))
         (unit-structure (fcg-get-transient-unit-structure transient-structure))
         (boundaries (fcg-get-boundaries unit-structure)))
    (setf (left-pole-structure transient-structure)
          (loop for unit in unit-structure
                for match = (first (unify-units clausal-unit-pattern unit (list +no-bindings+)
                                          :cxn-inventory cxn-inventory))
                collect (if match
                          (let* ((subunit-names (rest (assoc '?constituents match)))
                                 (theme (loop for boundary in boundaries
                                              when (member (first boundary) subunit-names)
                                              return (first boundary)))
                                 (rheme (remove theme subunit-names)))
                            `(,(unit-name unit)
                              (theme-rheme ((theme ,theme)
                                            (rheme ,rheme)))
                              ,@(unit-body unit)))
                          unit)))
    transient-structure))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; DE-RENDER
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod de-render ((utterance string) (mode (eql :beng-spacy-benepar))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  ;; Step 1: We do preprocessing with SpaCy and Benepar.
  (multiple-value-bind (dependency-tree constituent-tree)
      (nlp-tools:get-beng-sentence-analysis utterance)
    ;; Step 2: Use the dependency tree for building an initial transient-structure
    (let* ((utterance-as-list (nlp-tools::dp-build-utterance-as-list-from-dependency-tree dependency-tree))
           (transient-structure (de-render utterance-as-list :de-render-with-scope
                                           :cxn-inventory cxn-inventory)))
      (set-data transient-structure :utterance-as-list utterance-as-list)
      ;; Step 3: We already infer the functional structure as much as possible.
      (setf transient-structure (represent-functional-structure dependency-tree transient-structure t :english))
      ;; Step 4: We also translate the constituent tree into units.
      (setf transient-structure (represent-constituent-structure
                                 constituent-tree transient-structure :benepar cxn-inventory :english))
      ;; We also keep the postag results from the dependency parser.
      (set-data transient-structure :postagger-results (loop for dependent in dependency-tree
                                                             collect (list (nlp-tools:dp-get-token dependent)
                                                                           (nlp-tools:dp-get-tag dependent))))
      transient-structure)))
;; (de-render "Luc Steels was the first director of Sony CSL Paris." :beng-spacy-benepar)

(defstruct (preprocessing-result (:conc-name pr-)) dependency-structure constituent-structure)

(defmethod copy-object ((pr preprocessing-result))
  (make-preprocessing-result :dependency-structure (pr-dependency-structure pr)
                             :constituent-structure (pr-constituent-structure pr)))

(export '(make-preprocessing-result preprocessing-result pr-dependency-structure pr-constituent-structure))

(defmethod de-render ((utterance t) (mode (eql :covid-19))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (let* ((utterance-as-list (nlp-tools:dp-build-utterance-as-list-from-dependency-tree (pr-dependency-structure utterance)))
         (transient-structure (de-render utterance-as-list :de-render-with-scope
                                         :cxn-inventory cxn-inventory)))
    (set-data transient-structure :utterance-as-list utterance-as-list)
    (setf transient-structure (represent-functional-structure (pr-dependency-structure utterance) transient-structure t :english))
    (setf transient-structure (represent-constituent-structure
                               (pr-constituent-structure utterance) transient-structure :benepar cxn-inventory :english))
    (set-data transient-structure :postagger-results (loop for dependent in (pr-dependency-structure utterance)
                                                           collect (list (nlp-tools:dp-get-token dependent)
                                                                         (nlp-tools:dp-get-tag dependent))))
    transient-structure))


(defmethod size ((construction-inventory null))
  "Default implementation returning (length (constructions ci))"
  0)

;;; (defmethod parse ((utterance t) (construction-inventory constructional-network-inventory)
;;;                   &optional silent)
;;;   (let* ((utterance-as-list (nlp-tools:dp-build-utterance-as-list-from-dependency-tree (pr-dependency-structure utterance)))
;;;          (initial-cfs (de-render utterance :covid-19
;;;                                  :cxn-inventory (original-cxn-set construction-inventory))))
;;;     (initialize-transient-structure-blackboard initial-cfs utterance-as-list construction-inventory '<-)
;;;     (unless silent (notify parse-started utterance-as-list initial-cfs))
;;;     (multiple-value-bind
;;;         (solution cip)
;;;         (fcg-apply construction-inventory initial-cfs '<- :notify (not silent))
;;;       (let ((meaning 
;;;              (and solution
;;;                   (extract-meanings
;;;                    (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
;;;         (unless silent (notify parse-finished meaning construction-inventory))
;;;         (values meaning solution cip)))))


;; (set-configuration *fcg-constructions* :de-render-mode :covid-19)
;; (comprehend (first beng::*test-preprocessing*))

;; (activate-monitor trace-fcg)
;; (setf *my-test* (parse (first beng::*test-preprocessing*) *fcg-constructions* t))
;; (set-configuration *fcg-constructions* :preprocessing-tools nil)

(defmethod de-render ((utterance string) (mode (eql :beng))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  ;; Step 1: We obtain a syntactic dependency analysis from SpaCy.
  (multiple-value-bind (dependency-tree utterance-as-list transient-structure)
      (preprocess-using-dependency-tree utterance :cxn-inventory cxn-inventory)
    (set-data transient-structure :utterance-as-list utterance-as-list)
    ;; Step 2: We flesh out the dependency analysis by adding features and values
    (setf transient-structure
          (represent-functional-structure dependency-tree transient-structure t :english))
    ;; Step 3: We flesh out the constituency analysis by adding features and values
    (setf transient-structure
          (represent-constituent-structure dependency-tree transient-structure t cxn-inventory :english))
    ;; Step 4: We represent the theme-rheme structure of each constituent if possible.
    (setf transient-structure
          (represent-theme-rheme-structure dependency-tree transient-structure t cxn-inventory :english))
    ;; Finally we return the transient structure.
    (set-data transient-structure :postagger-results (loop for dependent in dependency-tree
                                                           collect (list (nlp-tools:dp-get-token dependent)
                                                                         (nlp-tools:dp-get-tag dependent))))
    transient-structure))

;; (comprehend "Luc Steels is the founder of Sony Computer Science Laboratories Paris, which is located in Paris.")
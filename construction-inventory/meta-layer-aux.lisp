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

;;; ------------------------------------------------------------------------------------------
;;; Unknown Words
;;; ------------------------------------------------------------------------------------------

;;; Helper functions for diagnosing which lexical class the word belongs to.
(defun beng-adjective-p (unit)
  (fcg::unify-features '(syn-cat (==1 (lex-class adjective))) (unit-feature unit 'syn-cat)
                       +no-bindings+))

(defmethod diagnose ((diagnostic diagnose-unknown-BENG-word) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the structure contains untreated strings."
  (unless (member (current-label (cxn-supplier node))
                  '(hashed-string))
    (let* ((transient-unit-structure (fcg-get-transient-unit-structure node))
           (unknown-strings (fcg-extract-selected-form-constraints (list (get-root transient-unit-structure))
                                                                   '(string)))
           (diagnosis (loop for string-spec in unknown-strings
                            for unit = (assoc (second string-spec) transient-unit-structure)
                            for diag = (cond ((beng-adjective-p unit) :adjective)
                                             (t
                                              nil))
                            when diag return (list string-spec diag))))
      (when diagnosis
        (make-instance 'problem-unknown-beng-word
                       :diagnosis diagnosis)))))

;;; Helper functions for learning a new construction.
(defgeneric beng-learn-lexical-construction (string-spec lex-class cxn-inventory))

(defmethod beng-learn-lexical-construction ((string-spec list)
                                            (lex-class t)
                                            (cxn-inventory t))
  (declare (ignore string-spec lex-class cxn-inventory))
  nil)

(defmethod beng-learn-lexical-construction ((string-spec list)
                                            (lex-class (eql :adjective))
                                            (cxn-inventory constructional-network-inventory))
  (let* ((string (third string-spec))
         (category (make-symbol string))
         (unit-name (make-var (format nil "~a-unit" string))))
    (multiple-value-bind (cxn-set construction)
        (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append string "-cxn")))
                            ((,unit-name
                              (referent ?ref)
                              (args (?r ?x))
                              (lex-id ,category)
                              (footprints (lex morph number))
                              (parent ?parent)
                              (sem-cat (sem-class category))
                              (syn-cat (lex-class adjective)
                                       (agreement ?agr)))
                             <-
                             (,unit-name
                              (HASH meaning ((domain category ,category ?r ?x ?ref)))
                              --
                              (HASH form ((string ,unit-name ,string)))))
                            :disable-automatic-footprints t
                            :cxn-inventory ,cxn-inventory
                            :attributes (:label (hashed-string hashed-meaning) :apply-fast t :meaning ,category :string ,string :pos ("ADJ"))))
      (declare (ignore cxn-set))
      construction)))
  
(defmethod repair ((repair repair-unknown-beng-word)
                   (problem problem-unknown-beng-word)
                   (node cip-node)
                   &key &allow-other-keys)
  (let* ((diagnosis (diagnosis problem))
         (cxn-inventory (copy-object (original-cxn-set (construction-inventory node))))
         (construction (beng-learn-lexical-construction (first diagnosis) (second diagnosis) cxn-inventory)))
    (when construction
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data construction))))
;; (comprehend "the yellowish troops are going home")

;;; ------------------------------------------------------------------------------------------
;;; Noun Phrase Chunks
;;; ------------------------------------------------------------------------------------------

(defmethod diagnose ((diagnostic diagnose-unchunked-np) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the structure contains untreated strings."
  (unless (member (current-label (cxn-supplier node))
                  '(hashed-string))
    (let* ((transient-unit-structure (fcg-get-transient-unit-structure node))
           (diagnosis (dolist (unit transient-unit-structure)
                        (when (and (null (unit-feature-value unit 'footprints))
                                   (unify '(==1 (syn-cat (==1 phrase-type NP)))
                                          (unit-body unit)))
                          (return unit)))))
      (when diagnosis
        (make-instance 'problem-unchunked-np
                       :diagnosis diagnosis)))))

(defun beng-get-constituents (unit)
  (unit-feature-value unit 'constituents))

(defun find-pos (pos features)
  (find pos features :test #'equal :key #'second))

(defmethod repair ((repair repair-unchunked-np)
                   (problem problem-unchunked-np)
                   (node cip-node)
                   &key &allow-other-keys)
  "Create a chunked NP construction."
  (let* ((diagnosed-NP (diagnosis problem)) ;; the NP that wasn't covered yet.
         (unit-structure (fcg-get-transient-unit-structure node))
         (constituents (loop for constituent-name in (beng-get-constituents diagnosed-NP)
                             collect (assoc constituent-name unit-structure)))
         (sorted-boundaries (sort (loop for constituent-name in (beng-get-constituents diagnosed-NP)
                                        collect (assoc constituent-name (fcg-get-boundaries unit-structure)))
                                  #'< :key #'second))
         ;; Now some variables that we will need while iterating through the constituents
         (output-link (make-var 'link))
         (units-with-locks nil))
    ;; Now we iterate through the constituents in order of appearance.
    (dolist (boundary sorted-boundaries)
      (let* ((constituent (assoc (unit-name boundary) constituents))
             (features (unit-body constituent)))
        (cond ((unify '(==1 (syn-cat (==1 (lex-class ?lex-class)))) features) ;; A lexical unit.
               (let ((input-link (make-var 'link))
                     (lex-class (second (assoc 'lex-class (second (assoc 'syn-cat features))))))
                 (push `(,(unit-name boundary)
                         :comprehension-lock ((parent ?np-unit)
                                              (args (,output-link ,input-link ?referent))
                                              (syn-cat (lex-class ,lex-class)))
                         :formulation-lock ((syn-cat (agreement ?agr))))
                       units-with-locks)
                 (setf output-link input-link)))
              (t
               nil))))
    (let ((substitution-list (loop for constituent-name in (mapcar #'first units-with-locks)
                                   collect (cons constituent-name (make-var "constituent-unit")))))
      (setf units-with-locks (cons `(?np-unit
                                     :comprehension-lock ((HASH form ,(handle-form-predicate-in-de-render sorted-boundaries 'meets))
                                                          (constituents ,(cons '==p (beng-get-constituents diagnosed-NP)))
                                                          (syn-cat (phrase-type NP)))
                                     :formulation-lock ((referent ?referent)
                                                        (syn-cat (agreement ?agr))))
                                   (reverse units-with-locks)))
      (setf units-with-locks (sublis substitution-list units-with-locks))
      ;; We will write the NP-construction for when we want to load the grammar anew at a later stage:
      ;; (beng-write-chunked-NP units-with-locks)
      ;; And we will define the  construction here on the fly to continue processing:
      (multiple-value-bind (cxn-set construction)
          (eval `(def-fcg-cxn ,(make-const "NP-cxn")
                              ((?np-unit
                                (footprints (np-cxn)))
                               <-
                               ,@(loop for unit-spec in units-with-locks
                                       collect `(,(first unit-spec)
                                                 ,@(second (assoc :formulation-lock unit-spec))
                                                 --
                                                 ,@(second (assoc :comprehension-lock unit-spec)))))
                              :disable-automatic-footprints t
                              :feature-types ((constituents sequence))
                              :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))
                              :attributes (:label hashed-string :string ,(format nil "np-~a" (lengt constituents)))))
        (declare (ignore cxn-set))
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data construction)))))
                   
;; Case 1: lexical units (simple NP)
;;         (NP (Det a) (NNP cat))
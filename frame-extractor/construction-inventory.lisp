;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Author: Remi van Trijp (www.remivantrijp.eu)

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

(defun describe-frame-semantics ()
  (format nil "The semantics of a frame consists of multiple 'meaning predicates.'
               1. The frame-type, e.g. (frame-type ?frame cause)
               ~&2. The frame-evoking element, e.g. (frame-evoking-element ?frame due-to)
               ~&3. The frame elements, e.g. (causer ?frame ?x)
               ~&                            (effect ?frame ?effect-unit)
               ~%~%The fillers of the frame elements point to unit names from which the span
               ~%in the utterance can be retrieved."))

(defmethod beng-extract-hash-label ((meaning list)
                                    (key (eql :frame-semantics)))
  (case (first meaning)
    (frame-evoking-element (last-elt meaning))
    (t
     nil)))

(def-fcg-constructions english-frame-extractor
  :cxn-inventory-type hashed-fcg-construction-set
  :feature-types (;; Make sure that you know in which package these are exported from the :fcg package. See feature-types.lisp
                  ;; in the Signature folder.
                  (constituents set)
                  (dependents set)
                  (footprints set)
                  (boundaries set-of-predicates)
                  (agreement sequence)
                  (args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates))
 :fcg-configurations (;; Form predicates
                      (:form-predicates meets)
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction sets
                      ;; ----------------------------------------------------------------------------------
                      (:parse-order hashed-string hashed-lex-id cxn)
                      (:hashed-labels hashed-string hashed-lex-id)
                      ;; ----------------------------------------------------------------------------------
                      ;; Render and De-rendering
                      ;; ----------------------------------------------------------------------------------
                      (:de-render-mode . :english-hybrid)
                      ; (:render-mode . :english-render)
                      ;; ----------------------------------------------------------------------------------
                      ;; Node and Goal tests
                      ;; ----------------------------------------------------------------------------------
                      (:node-tests :update-references
                       :check-duplicate :restrict-nr-of-nodes)
                      (:update-boundaries-feature . constituents)
                      (:parse-goal-tests :no-applicable-cxns)
                      (:production-goal-tests :no-applicable-cxns) 
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction Supplier
                      ;; ----------------------------------------------------------------------------------
                      (:cxn-supplier-mode . :hashed-ordered-by-label)
                      (:hash-mode . :hash-english-grammar)
                      (:semantics . :frame-semantics)
                      ;; ----------------------------------------------------------------------------------
                      ;; For guiding search
                      ;; ----------------------------------------------------------------------------------
                      (:queue-mode . :depth-first-avoid-duplicates)
                      (:max-search-depth . 100)
                      (:max-nr-of-nodes . 1500)
                      ;; ----------------------------------------------------------------------------------
                      ;; Miscellaneous
                      ;; ----------------------------------------------------------------------------------
                      (:draw-meaning-as-network . t)
                      (:shuffle-cxns-before-application . nil)
                      ;; For learning
                      (:consolidate-repairs . t))
 :visualization-configurations ((:show-wiki-links-in-predicate-networks . nil)
                                (:show-constructional-dependencies . nil)
                                (:with-search-debug-data . t))
 :hierarchy-features (constituents dependents)
 
 (load-auxiliary "be"))

;; (comprehend "The draught are due to climate change.")

;; hashed-lex-id hashed-string
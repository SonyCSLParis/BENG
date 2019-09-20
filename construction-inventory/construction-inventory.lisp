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

(def-fcg-constructions 
 BENG
 :cxn-inventory-type constructional-network-inventory
 :feature-types (;; Make sure these are exported from the :fcg package. See feature-types.lisp
                 ;; in the Signature folder.
                 (alignment sequence)
                 (arg-struct set-of-predicates)
                 (categories set)
                 (constituents set)
                 (dependents set)
                 (footprints set)
                 (history set)
                 (subunits set)
                 (rheme set)
                 (boundaries set-of-predicates)
                 (agreement sequence)
                 (args sequence)
                 (ev-args sequence)
                 (sem-class set)
                 (lexical-alignment sequence)
                 (potential-lex-class set)
                 (syn-roles set-of-predicates)
                 (form set-of-predicates)
                 (meaning set-of-predicates))
 :fcg-configurations (;; Form predicates
                      (:form-predicates meets before fields first last)
                      ;; ----------------------------------------------------------------------------------
                      ;; Semantic Views
                      ;; ----------------------------------------------------------------------------------
                      (:slots-of-semantic-views . *english-grammar-semantics*)
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction sets
                      ;; ----------------------------------------------------------------------------------
                      (:production-order hashed-meaning voice phrasal arg-cxn hashed-lex-id)
                      (:parse-order hashed-string hashed-lex-id voice marked-phrasal unmarked-phrasal arg-cxn)
                      (:hashed-labels hashed-string hashed-meaning hashed-lex-id)
                      ;; ----------------------------------------------------------------------------------
                      ;; Render and De-rendering
                      ;; ----------------------------------------------------------------------------------
                      (:de-render-mode . :beng)
                      (:render-mode . :english-render)
                      ;; ----------------------------------------------------------------------------------
                      ;; Node and Goal tests
                      ;; ----------------------------------------------------------------------------------
                      (:node-tests :update-references
                       :check-duplicate :restrict-nr-of-nodes)
                      (:update-boundaries-feature . constituents)
                      (:parse-goal-tests :no-applicable-cxns) ;:connected-semantic-network :connected-structure)
                      (:production-goal-tests :no-applicable-cxns :connected-structure)
                      ;; ----------------------------------------------------------------------------------
                      ;; NLP Tools
                      ;; ----------------------------------------------------------------------------------
                      (:preprocessing-steps-for-dependency-parser
                       dependency-string-append-named-entities
                       dependency-string-append-compounds-in-np
                       dependency-string-append-compounds
                       dependency-promote-conjuncts)
                      (:preprocessing-tools
                       ;;(:postagger-results #'get-penelope-pos-tags) ;;#'get-stanford-and-universal-pos-tags)
                       (:named-entities #'get-penelope-named-entities))
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction Supplier
                      ;; ----------------------------------------------------------------------------------
                      (:cxn-supplier-mode . :hashed-english-grammar)
                      (:hash-mode . :hash-english-grammar)
                      ;; ----------------------------------------------------------------------------------
                      ;; For guiding search
                      ;; ----------------------------------------------------------------------------------
                      (:node-expansion-mode . :english-expand-cip-node)
                      (:priority-mode . :english-grammar-priority)
                      (:queue-mode . :depth-first-avoid-duplicates)
                      (:max-search-depth . 100)
                      (:processing-strategies ;; Includes a list of strategies and corresponding weights.
                       (:minimize-domains 1.0)
                       (:maximize-coherence 10.0))
                      (:max-nr-of-nodes . 1500)
                      ;; ----------------------------------------------------------------------------------
                      ;; Miscellaneous
                      ;; ----------------------------------------------------------------------------------
                      (:draw-meaning-as-network . t)
                      (:shuffle-cxns-before-application . nil)
                      ;; For learning
                      (:consolidate-repairs . t))
 ;:diagnostics (diagnose-unknown-propernoun-problem)
 ;:repairs (learn-missing-propernoun)      
 :visualization-configurations ((:show-wiki-links-in-predicate-networks . nil)
                                (:show-constructional-dependencies . nil)
                                (:with-search-debug-data . t))
 :hierarchy-features (constituents dependents)

 (set-data (blackboard *fcg-constructions*) :fusion-hierarchy *fusion-hierarchy*))

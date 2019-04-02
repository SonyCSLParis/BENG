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

(in-package #:asdf)

(defsystem :english-grammar
  :depends-on (:fcg
               :xmls
               :nlp-tools
               :category-hierarchies
               :fcg-hybrids)
  :serial t
  :components ((:file "config")
               (:file "feature-types")
               (:file "utilities")
               (:file "semantics")
               (:file "parameters-and-loading")
               (:file "cxn-processing")    
               (:module "make-lexicon"
                :serial t
                :components ((:file "list-adjectives")
                             (:file "list-adverbs")
                             (:file "list-nouns")
                             (:file "list-prepositions")
                             (:file "list-verbs")
                             (:file "verb-conjugation")
                             (:file "write-adjectives")
                             (:file "write-adverbs")
                             (:file "write-nouns")
                             (:file "write-prepositions")
                             (:file "write-verbs")
                             

                             ))))


               ;;(:file "package")))


               ;(:file "feature-types")
               ;;; First extending FCG...
               ;(:module "constructional-networks"
               ; :serial t
               ; :components ((:file "fcg-constructional-network")))
               ;;; Then loading the grammar files.
               ;(:file "package")
               ;(:file "utilities")
               ;(:file "semantics")
               ;(:file "temp-constructions")
               ;(:file "stanford-pos-tagger-interface")
               ;(:file "make-lexicon")
               ;(:module "inventory-support"
               ; :serial t
               ; :components ((:file "verb-conjugation")
               ;              (:file "verb-list")
               ;              (:file "verbs")
               ;              (:file "common-nouns")
               ;              (:file "nouns")
               ;              (:file "adjectives-list")
               ;              (:file "adjectives")
               ;              (:file "verbnet-interface")))
               ;(:module "learning"
               ; :serial t
               ; :components ((:file "unknown-words")))
               ;(:module "hybrids"
               ; :serial t
               ; :components ((:file "spacy-pos-tags")
               ;              ;; (:file "clear-dependency-tags") ;; To be deleted, now in sharing.
               ;              ;; (:file "semantic-dependency-parser") ;; To be deleted, now in sharing.
               ;              (:file "learning-operators")
               ;              (:file "configuration-hybrid-grammar")))
               ;(:file "render-and-de-render")
               ;(:file "configuration")
               ;(:module "evaluation"
               ;         :serial t
               ;         :components ((:file "monitors")))
               ;(:file "tests")))
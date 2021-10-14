
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

(in-package #:asdf)

(defsystem :beng
  :author "Remi van Trijp <remi.vantrijp@sony.com>"
  :license "GPL-3.0"
  :version "2.0.1"
  :description "Basic English Grammar implemented in Fluid Construction Grammar."
  :depends-on (:fcg
               :irl
               :category-hierarchies
               :xmls
               :cl-json :drakma
               :nlp-tools
               :fcg-hybrids)
  :serial t
  :components ((:file "package")
               (:module "signature"
                :serial t
                :components ((:file "config")
                             (:file "fusion-hierarchy")
                             (:file "fusion-matching") ;; To be reincorporated in the grammar.
                             (:file "semantics"))) ;; To be reincorporated in the grammar.
               (:file "utilities")
               (:module "make-lexicon"
                :serial t
                :components ((:file "list-adjectives")
                             (:file "list-adverbs")
                             (:file "list-nouns")
                             (:file "list-prepositions")
                             (:file "list-verbs")
                             (:file "verb-conjugation")))
               (:file "load-and-save")
               (:module "construction-inventory"
                :serial t
                :components ((:file "hash-mode")
                             (:file "constructional-network-aux")
                             (:file "construction-inventory")))
               (:module "frame-extractor"
                :serial t
                :components ((:file "class-and-macros")
                             (:file "semantic-frames")
                             (:file "construction-inventory")
                             (:file "frame-evoking-elements")))))

;;;                (:file "cxn-processing")                   ;; To be reintegrated

;;;                (:module "make-lexicon"
;;;                 :serial t
;;;                 :components ((:file "write-adjectives")   ;; To be reintegrated
;;;                              (:file "write-adverbs")      ;; To be reintegrated
;;;                              (:file "write-nouns")        ;; To be reintegrated
;;;                              (:file "write-prepositions") ;; To be reintegrated
;;;                              (:file "write-verbs")))      ;; To be reintegrated
;;;                (:module "construction-inventory"
;;;                 :serial t
;;;                 :components ((:file "spacy-pos-tags")
;;;                              (:file "constructional-network-aux")
;;;                              (:file "meta-layer-classes")
;;;                              (:file "meta-layer-aux")
;;;                              (:file "construction-inventory")))
;;;                (:module "experts"
;;;                 :serial t
;;;                 :components ((:file "named-entities")
;;;                              (:file "constituent-analysis")
;;;                              (:file "subclauses")
;;;                              (:file "theme")))))

;;; Copyright (C) 2019  Sony Computer Science Laboratories Paris
;;;                     Remi van Trijp (www.remivantrijp.eu)

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
               :category-hierarchies
               :xmls
               :nlp-tools
               :fcg-hybrids)
  :serial t
  :components ((:file "package")
               (:file "preprocessing")
               (:file "render-and-derender")
               (:module "signature"
                :serial t
                :components ((:file "config")
                             (:file "benepar-conversion")
                             (:file "fusion-hierarchy")
                             (:file "fusion-matching")
                             (:file "semantics")))
               (:file "utilities")
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
                             (:file "write-verbs")))
               (:file "load-and-save")
               (:module "construction-inventory"
                :serial t
                :components ((:file "spacy-pos-tags")
                             (:file "constructional-network-aux")
                             (:file "meta-layer-classes")
                             (:file "meta-layer-aux")
                             (:file "construction-inventory")))
               (:module "experts"
                :serial t
                :components ((:file "named-entities")
                             (:file "constituent-analysis")
                             (:file "subclauses")
                             (:file "theme")))))

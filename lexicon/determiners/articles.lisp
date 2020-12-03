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

;;;; Articles
;;;; ----------------------------------------------------------------------------------------------
(def-fcg-cxn a-lex
             ((?indefinite-article
               (referent ?x)
               (args (?x ?source))
               (lex-id a)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (lex-class determiner)
                        (agreement (- - + -))
                        (definite -)))
              <--
              (?indefinite-article
               (hash meaning ((referent-status indefinite-a ?x ?source)))
               --
               (hash form ((string ?indefinite-article "a")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning indefinite-a :string "a" :POS ("DET")))
;; (comprehend-and-formulate '("a"))

(def-fcg-cxn an-lex
             ((?indefinite-article
               (referent ?x)
               (args (?x ?source))
               (lex-id an)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (lex-class determiner)
                        (agreement (- - + -))
                        (definite -)))
              <--
              (?indefinite-article
               (hash meaning ((referent-status indefinite-an ?x ?source)))
               --
               (hash form ((string ?indefinite-article "an")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning indefinite-an :string "an" :POS ("DET")))
;; (comprehend-and-formulate '("an"))

(def-fcg-cxn the-lex
             ((?definite-article
               (referent ?x)
               (args (?x ?source))
               (lex-id the)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (lex-class determiner)
                        (agreement (- - ?3sg ?pl))
                        (definite +)))
              <--
              (?definite-article
               (hash meaning ((referent-status identifiable-ref ?x ?source)))
               --
               (hash form ((string ?definite-article "the")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning identifiable-ref :string "the" :POS ("DET")))
;; (comprehend-and-formulate '("the"))

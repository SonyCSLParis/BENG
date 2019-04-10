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

;;;; Possessive pronouns
;;;; ----------------------------------------------------------------------------------------------
(def-fcg-cxn my-poss-pron
             ((?my
               (referent ?x)
               (args (?x ?source))
               (lex-id my)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (possessive-pronoun determiner))
                        (lex-class possessive-pronoun)
                        (agreement (- - ?3sg ?pl))
                        (definite +)))
              <--
              (?my
               (hash meaning ((referent-status in-speaker-posession ?x ?source)))
               --
               (hash form ((string ?my "my")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning in-speaker-posession :string "my" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "my")

(def-fcg-cxn your-poss-pron
             ((?your
               (referent ?x)
               (args (?x ?source))
               (lex-id your)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (possessive-pronoun determiner))
                        (lex-class possessive-pronoun)
                        (agreement (- - ?3sg ?pl))
                        (definite +)))
              <--
              (?your
               (hash meaning ((referent-status in-addressee-posession ?x ?source)))
               --
               (hash form ((string ?your "your")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning in-addressee-posession :string "your" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "your")

(def-fcg-cxn his-poss-pron
             ((?his
               (referent ?x)
               (args (?x ?source))
               (lex-id his)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (possessive-pronoun determiner))
                        (lex-class possessive-pronoun)
                        (agreement (- - ?3sg ?pl))
                        (definite +)))
              <--
              (?his
               (hash meaning ((referent-status in-male-referent-posession ?x ?source)))
               --
               (hash form ((string ?his "his")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning in-male-referent-posession :string "his" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "his")

(def-fcg-cxn her-poss-pron
             ((?her
               (referent ?x)
               (args (?x ?source))
               (lex-id her)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (possessive-pronoun determiner))
                        (lex-class possessive-pronoun)
                        (agreement (- - ?3sg ?pl))
                        (definite +)))
              <--
              (?her
               (hash meaning ((referent-status female-referent ?x ?source)))
               --
               (hash form ((string ?her "her")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning female-referent :string "her" :POS ("PRP$")))
;; (comprehend-and-formulate "her")

(def-fcg-cxn our-poss-pron
             ((?our
               (referent ?x)
               (args (?x ?source))
               (lex-id our)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (possessive-pronoun determiner))
                        (lex-class possessive-pronoun)
                        (agreement (- - ?3sg ?pl))
                        (definite +)))
              <--
              (?our
               (hash meaning ((referent-status in-speakers-posession ?x ?source)))
               --
               (hash form ((string ?our "our")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning in-speakers-posession :string "our" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "our")


(def-fcg-cxn their-poss-pron
             ((?their
               (referent ?x)
               (args (?x ?source))
               (lex-id their)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (possessive-pronoun determiner))
                        (lex-class possessive-pronoun)
                        (agreement (- - ?3sg ?pl)) ;;??
                        (definite +)))
              <--
              (?their
               (hash meaning ((referent-status in-their-posession ?x ?source)))
               --
               (hash form ((string ?their "their")))))
             :attributes (:label (hashed-meaning hashed-string) :apply-fast t :meaning in-their-posession :string "their" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "their")
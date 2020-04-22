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

;;;; Demonstratives (THAT is a special case)
;;;; ----------------------------------------------------------------------------------------------
(def-fcg-cxn that-lex
             ((?that
               (referent ?x)
               (args (?x ?source))
               (lex-id that)
               (parent ?parent)
               (syn-cat (categories (determiner demonstrative relative-pronoun))
                        (lex-class demonstrative)
                        (agreement (- - + -))
                        (definite +)))
              <-
              (?that
               (HASH meaning ((discourse-function point-to-remote-referent ?x ?source)))
               --
               (HASH form ((string ?that "that")))))
             :attributes (:label (hashed-meaning hashed-string) :apply-fast t :meaning point-to-remote-referent :string "that" :pos ("DET" "PRON")))
;; (comprehend-and-formulate "that")

(def-fcg-cxn this-demonstrative
             ((?this
               (referent ?x)
               (args (?x ?source))
               (lex-id this)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (determiner demonstrative))
                        (lex-class demonstrative)
                        (agreement (- - + -))
                        (definite +)))
              <--
              (?this
               (hash meaning ((discourse-function point-to-nearby-referent ?x ?source)))
               --
               (hash form ((string ?this "this")))))
             :attributes (:label (hashed-meaning hashed-string) :apply-fast t :meaning point-to-nearby-referent :string "this" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "this")

(def-fcg-cxn these-demonstrative
             ((?these
               (referent ?x)
               (args (?x ?source))
               (lex-id these)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (determiner demonstrative))
                        (lex-class demonstrative)
                        (agreement (- - - +))
                        (definite +)))
              <--
              (?these
               (hash meaning ((discourse-function point-to-nearby-referents ?x ?source)))
               --
               (hash form ((string ?these "these")))))
             :attributes (:label (hashed-meaning hashed-string) :apply-fast t :meaning point-to-nearby-referents :string "these" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "these")

(def-fcg-cxn those-demonstrative
             ((?those
               (referent ?x)
               (args (?x ?source))
               (lex-id those)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (determiner demonstrative))
                        (lex-class demonstrative)
                        (agreement (- - - +))
                        (definite +)))
              <--
              (?those
               (hash meaning ((discourse-function point-to-distant-referents ?x ?source)))
               --
               (hash form ((string ?those "those")))))
             :attributes (:label (hashed-meaning hashed-string) :apply-fast t :meaning point-to-distant-referents :string "those" :POS ("DET" "PRON")))
;; (comprehend-and-formulate "those")

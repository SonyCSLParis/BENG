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

(def-fcg-cxn Poss-Pron-NP-cxn
             (
              <-
              (?NP
               (syn-cat (phrase-type NP))
               (referent ?ref)
               --
               (HASH form ((meets ?pers-pronoun ?other-unit ?NP)))
               (syn-cat (phrase-type NP)
                        (agreement ?agr))
               (constituents (?pers-pronoun ?other-unit)))
              (?pers-pronoun
               (referent ?ref)
               (args (?ref ?input))
               (parent ?NP)
               (syn-cat (lex-class possessive-pronoun))
               --
               (parent ?NP)
               (syn-cat (lex-class possessive-pronoun)
                        (agreement ?agr)))
              (?other-unit
               (referent ?ref)
               (args (?input ?context))
               (parent ?NP)
               --
               (parent ?NP)
               (syn-cat (lex-class ?any-class)
                        (agreement ?agr))))
              :disable-automatic-footprints nil
              :attributes (:label phrasal marked-phrasal))


               
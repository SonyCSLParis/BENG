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

(def-fcg-cxn some
             ((?quantifier
               (referent ?x)
               (args (?x ?source))
               (lex-id some)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (quantifier determiner))
                        (lex-class quantifier)
                        (agreement (- - - +))
                        (definite ?def)))
              <--
              (?quantifier
               (hash meaning ((referent-status some-quantifiable-ref ?x ?source)))
               --
               (hash form ((string ?quantifier "some")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning some-quantifiable-ref :string "some" :POS ("DET")))
;; (comprehend-and-formulate "some")

(def-fcg-cxn many
             ((?quantifier
               (referent ?x)
               (args (?x ?source))
               (lex-id many)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (quantifier determiner))
                        (lex-class adjectival-quantifier)
                        (agreement (- - - +))
                        (definite ?def)))
              <--
              (?quantifier
               (hash meaning ((referent-status many-quantifiable-ref ?x ?source)))
               --
               (hash form ((string ?quantifier "many")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning many-quantifiable-ref :string "many" :POS ("DET")))

(def-fcg-cxn all
             ((?quantifier
               (referent ?x)
               (args (?x ?source))
               (lex-id all)
               (parent ?parent)
               (sem-cat (sem-class selector))
               (syn-cat (categories (quantifier determiner))
                        (lex-class quantifier)
                        (agreement (- - - +))
                        (definite ?def)))
              <--
              (?quantifier
               (hash meaning ((referent-status all-quantifiable-ref ?x ?source)))
               --
               (hash form ((string ?quantifier "all")))))
             :attributes (:label (hashed-meaning hashed-string)
                          :meaning all-quantifiable-ref :string "all" :POS ("DET")))
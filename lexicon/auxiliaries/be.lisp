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

(fusion-add-parent 'be 'aux *fusion-hierarchy*)
(fusion-add-parent 'be 'verb *fusion-hierarchy*)
 
(def-fcg-cxn be-morph
             ((?be
               (footprints (number morph)))
               <-
              (?be
               (parent ?parent)
               (lex-id be)
               (footprints (not number morph))
               (syn-cat (lex-class ?lex-class)
                        (agreement ?agr)
                        (verb-form base-form)
                        (finite -))
               --
               (hash form ((string ?be "be")))))
              :disable-automatic-footprints t             
              :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "be" :pos ("VERB")))

(def-fcg-cxn am-morph
             ((?am
               (footprints (number morph)))
              <--
              (?am
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement (+ - - -))
                        (finite +)
                        (verb-form base-form))
               --
               (hash form ((string ?am "am")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "am" :pos ("VERB")))
;; (comprehend "am")

(def-fcg-cxn am-clitic-morph
             ((?am
               (footprints (number morph)))
              <--
              (?am
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (is-aux-clitic +)
                        (agreement (+ - - -))
                        (verb-form base-form)
                        (finite +))
               --
               (hash form ((string ?am "'m")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "'m" :pos ("VERB")))

(def-fcg-cxn is-morph
             ((?is
               (footprints (number morph)))
              <--
              (?is
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement (- - + -))
                        (verb-form base-form)
                        (finite +))
               --
               (hash form ((string ?is "is")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "is" :pos ("VERB")))
;; (comprehend "is")
;; (comprehend-and-formulate "the window is opening")
;; (fcg-show-cxn 'is-morph)

(def-fcg-cxn is-clitic-morph
             ((?is
               (footprints (number morph)))
              <--
              (?is
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (is-aux-clitic +)
                        (agreement (- - + -))
                        (verb-form base-form)
                        (finite +))
               --
               (hash form ((string ?is "'s")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "'s" :pos ("VERB")))

(def-fcg-cxn are-morph
             ((?are
               (footprints (number morph)))
               <--
              (?are
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement (- ?2sg - ?pl))                        
                        (verb-form base-form)
                        (finite +))
               --
               (hash form ((string ?are "are")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "are" :pos ("VERB")))
;; (comprehend "are")
;; (comprehend-and-formulate "the windows are opening")
;; (fcg-show-cxn 'are-morph)

(def-fcg-cxn are-clitic-morph
             ((?are
               (footprints (number morph)))
               <--
              (?are
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (is-aux-clitic +)
                        (agreement (- ?2sg - ?pl))                        
                        (verb-form base-form)
                        (finite +))
               --
               (hash form ((string ?are "'re")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "'re" :pos ("VERB")))
;; (comprehend "we're")

(def-fcg-cxn was-morph
             ((?was
               (footprints (number morph)))
               <--
              (?was
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement (?1sg - ?3sg -))
                        (verb-form participle)
                        (finite +))
               --
               (hash form ((string ?was "was")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "was" :pos ("VERB")))
;; (comprehend "was")

(def-fcg-cxn were-morph
             ((?were
               (footprints (number morph)))
              <--
              (?were
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement (- ?2sg - ?pl))                        
                        (verb-form participle)
                        (finite +))
               --
               (hash form ((string ?were "were")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "were" :pos ("VERB")))
;; (comprehend "were")

(def-fcg-cxn been-morph
             ((?been
               (footprints (number morph)))
               <--
              (?been
               (lex-id be)
               (parent ?parent)
               (footprints (not number morph))
               (syn-cat (lex-class ?lex-class)
                        (agreement ?agr)
                        (verb-form participle)
                        (finite -))
               --
               (hash form ((string ?been "been")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "been" :pos ("VERB")))
;; (comprehend "been")

(def-fcg-cxn being-morph
             ((?being
               (footprints (number morph)))
               <--
              (?being
               (lex-id be)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement ?agr)
                        (verb-form ing-form)
                        (finite -))
               --
               (hash form ((string ?being "being")))))
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "being" :pos ("VERB")))
;; (comprehend "being")


;; (comprehend-and-formulate "the window was opening")

;;; (def-fcg-cxn copula-be-lex
;;;               ((?be
;;;                 (referent ?state-or-property)
;;;                 (ev-args (?x ?y))
;;;                 (parent ?parent)
;;;                 (syn-cat (categories (aux verb copula)))
;;;                 (sem-valence (definiendum ?x) (definiens ?y)))
;;;                <-
;;;                (?be
;;;                 (hash meaning ((state-or-property-frame BE ?state-or-property)
;;;                                (BE-1 ?state-or-property ?x)
;;;                                (BE-2 ?state-or-property ?y)))
;;;                 --
;;;                 (lex-id be)
;;;                 (syn-cat (copula +)
;;;                          (lex-class verb)
;;;                          (agreement ?agr)
;;;                          (verb-form ?verb-form)
;;;                          (finite ?finite)
;;;                          (syn-function ?syn-function)
;;;                          (tam ?tam))))
;;;               :attributes (:label (hashed-lex-id hashed-meaning) :lex-id be :meaning state-or-property :pos ("VERB")))

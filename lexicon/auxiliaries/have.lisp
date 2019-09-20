;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :beng)

(fusion-add-parent 'be 'aux *fusion-hierarchy*)
(fusion-add-parent 'be 'verb *fusion-hierarchy*)

(def-fcg-cxn have-morph
             ((?have
               (footprints (number morph)))
               <-
              (?have
               (parent ?parent)
               (lex-id have)
               (footprints (not number morph))
               (syn-cat (lex-class ?lex-class)
                        (agreement ?agr)
                        (verb-form base-form)
                        (finite ?fin))
               --
               (hash form ((string ?have "have")))))
              :disable-automatic-footprints t             
              :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id have :string "have" :pos ("VERB")))
;; (comprehend "the windows have broken")

(def-fcg-cxn has-morph
             ((?has
               (footprints (number morph)))
              <--
              (?has
               (lex-id have)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement (- - + -))
                        (finite +)
                        (verb-form base-form))
               --
               (hash form ((string ?has "has")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id have :string "has" :pos ("VERB")))
;; (comprehend-and-formulate "the window has broken")

(def-fcg-cxn had-morph
             ((?had
               (footprints (number morph)))
              <--
              (?had
               (lex-id have)
               (footprints (not number morph))
               (parent ?parent)
               (syn-cat (lex-class ?lex-class)
                        (agreement ?agr)
                        (verb-form participle)
                        (finite ?fin))
               --
               (hash form ((string ?had "had")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id had :string "had" :pos ("VERB")))
;; (comprehend-and-formulate "the window had opened")
;; (fcg-show-cxn 'is-morph)

(def-fcg-cxn having-morph
             ((?having
               (footprints (number morph)))
               <--
              (?having
               (lex-id have)
               (footprints (not number morph))
               (parent ?parent)
               (sem-frame have)
               (syn-cat (lex-class verb)
                        (agreement ?agr)
                        (verb-form ing-form)
                        (finite -))
               --
               (hash form ((string ?having "having")))))
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id be :string "having" :pos ("VERB")))
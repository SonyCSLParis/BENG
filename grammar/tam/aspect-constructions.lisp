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

;; Progressive construction.
;; --------------------------
(def-fcg-cxn TAM-progressive-cxn
             ((?aux-be
               (args (?asp-ev ?ev))
               (referent ?ev-ref)
               (sem-frame be)
               (syn-cat (verb-form ?verb-form)
                        (finite ?finite)))
              (?semantic-head
               (dependents (?aux-be)))
              (?vp
               (head ?semantic-head)
               (footprints (aspect))
               (referent ?ev-ref)
               (form ((fields ?modal ?aux-have ?aux-be ?passive-be ?semantic-head ?vp))))
              <-
              (?vp
               --
               (footprints (not aspect))
               (constituents (?aux-be ?ing-form))
               (head ?semantic-head)
               (syn-cat (phrase-type vp)
                        (agreement ?agr))
               (tam (tense ?tense)
                    (aspect (perfect ?perfect)
                            (progressive +))
                    (modality ?mod)))                    
              (?ing-form
               (parent ?vp)
               (referent ?ev-ref)
               (args (?ev ?context))
               (syn-cat (verb-form ing-form))
               --
               (parent ?vp)
               (syn-cat (verb-form ing-form)
                        (agreement ?agr)
                        (finite -)))
              (?aux-be
               (HASH meaning ((qualify-event progressive ?asp-ev ?ev)))
               --
               (parent ?vp)
               (lex-id be)
               (syn-cat (lex-class aux)
                        (agreement ?agr)
                        (verb-form ?verb-form)
                        (finite ?finite))))
             :description "A marked TAM-construction that matches on the pattern AUX-BE + ing-form."
             :disable-automatic-footprints t
             :attributes (:label (phrasal marked-phrasal)))
;; (comprehend-and-formulate "the window was opening")
;; (comprehend-and-formulate "the window was being opened")



;;; ;; Non-Progressive construction: deprecated.
;;; ;; -----------------------------------------
;;; (def-fcg-cxn TAM-non-progressive-cxn
;;;              ((?vp
;;;                (footprints (aspect))
;;;                (form ((fields ?modal ?aux-have ?aux-be ?passive-be ?main-verb ?vp))))
;;;               <-
;;;               (?verb
;;;                (HASH meaning ((event-qualification non-progressive ?asp-ev ?ev)))
;;;                (parent ?vp)
;;;                (referent ?ev-ref)
;;;                (args (?asp-ev ?context))
;;;                (sem-frame verb)
;;;                --
;;;                (parent ?vp)
;;;                (sem-frame verb))
;;;               (?vp
;;;                --
;;;                (footprints (not aspect))
;;;                (referent ?ev-ref)
;;;                (constituents (?verb))
;;;                (syn-cat (phrase-type vp))
;;;                (tam (tense ?tense)
;;;                     (aspect (perfect ?perfect)
;;;                             (progressive -))
;;;                     (modality ?modality))))
;;;              :disable-automatic-footprints t
;;;              :description "An unmarked TAM-construction that matches on the pattern AUX-BE + ing-form."
;;;              :attributes (:label (deprecated-set))) ;;phrasal unmarked-phrasal)))
;;; ;; (fcg-show-cxn 'tam-non-progressive-cxn)
;;; ;; (comprehend "the window broke")

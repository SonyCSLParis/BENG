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

(def-fcg-cxn Voice-Passive-cxn
             ((?vp
               (referent ?ev-ref)
               (form ((fields ?aux-mod ?aux-have ?aux-be ?passive-aux ?main-verb ?vp))))
              (?passive-aux
               (referent ?ev-ref)
               (args (?ev-profile ?ev)))
              <-
              (?vp
               --
               (head ?main-verb)
               (syn-cat (phrase-type vp)
                        (agreement ?agr))
               (constituents (?passive-aux ?main-verb)))
              (?passive-aux
               (HASH meaning ((causal-chain-profile non-basic ?ev-profile ?ev)))
               --
               (lex-id be)
               (parent ?vp)
               (syn-cat (lex-class aux)
                        (is-passive-marker +)
                        (verb-form ?vf)
                        (agreement ?agr)
                        (finite ?fin)))
              (?main-verb
               (parent ?vp)
               (args (?ev ?context))
               (sem-frame verb)
               --
               (parent ?vp)
               (dependents (?passive-aux))
               (syn-cat (verb-form participle)
                        (finite -))
               (voice passive)))
             :description "Identifies the main verb in its active form."
             :disable-automatic-footprints nil
             :attributes (:label (voice) :score 0.5))
;; (comprehend-and-formulate "the ball was kicked")
;; (comprehend-and-formulate "the window opened")
               

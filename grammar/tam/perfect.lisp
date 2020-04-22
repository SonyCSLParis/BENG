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

(def-fcg-cxn TAM-perfect-cxn
             ((?aux-have
               (referent ?ev-ref)
               (args (?perf-ev ?asp-ev))
               (sem-frame have)
               (syn-cat (verb-form ?verb-form)
                        (finite ?finite)))
               (?vp
                (footprints (perfect))
                (form ((fields ?modal ?aux-have ?aux-be ?passive-be ?main-verb ?vp))))
               <-
               (?vp
                (referent ?ev-ref)
                (syn-cat (phrase-type vp))
                (constituents (?participle))
                --
                (footprints (not perfect))
                (constituents (?aux-have ?participle))
                (syn-cat (phrase-type vp)
                         (agreement ?agr))
                (tam (tense ?tense)
                     (aspect (perfect +)
                             (progressive ?progr))
                     (modality ?mod)))
               (?aux-have
                (HASH meaning ((relation-to-rp simultaneous ?perf-ev ?asp-ev)))
                --
                (parent ?vp)
                (lex-id have)
                (syn-cat (lex-class aux)
                         (agreement ?agr)
                         (verb-form ?verb-form)
                         (finite ?finite)))
               (?participle
                (parent ?vp)
                (referent ?ev-ref)
                (args (?asp-ev ?ev))
                --
                (parent ?vp)
                (syn-cat (verb-form participle)
                         (finite -))))
             :description "A marked TAM-construction that matches on the pattern AUX-HAVE + participle."
             :disable-automatic-footprints t
             :attributes (:label (phrasal marked-phrasal)))
;; (comprehend "I have opened the window")
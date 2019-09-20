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

;;; Deprecated construction.
;;; ------------------------
;;;
(def-fcg-cxn Voice-Active-cxn
             ((?vp
               (referent ?ev-ref)
               (tam ?tam)
               (head ?main-verb))
              <-
              (?vp
               (HASH meaning ((causal-chain-profile basic ?ev ?context)))
               --
               (head ?main-verb)
               (syn-cat (phrase-type vp)
                        (agreement ?agr))
               (constituents (?main-verb)))
              (?main-verb
               (parent ?vp)
               (referent ?ev-ref)
               (args (?ev ?context))
               (sem-frame verb)
               --
               (parent ?vp)
               (sem-frame verb)
               (voice active)))
             :description "Identifies the main verb in its active form."
             :disable-automatic-footprints nil
             :attributes (:label (voice) :score 0.5))
;; (comprehend-and-formulate "the window broke")

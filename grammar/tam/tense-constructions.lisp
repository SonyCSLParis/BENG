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

;;; English Tense only distinguishes between past and non-past.
;;; EV = Event Time
;;; RP = Reference Point
;;; 
;;; They are grounded into speaker's beliefs as being certain.

;;; ---------------------------------------------------------------------------
;;; 0 Auxiliaries: He reads. I read.
;;; 1 Auxiliary: He is reading. He has read. He will read.
;;; 2 Auxiliaries: He has been reading. He will be reading. He will have read.
;;; 3 Auxiliaries: He must have been reading.
;;; ---------------------------------------------------------------------------

(def-fcg-cxn TAM-Present-Tense-cxn
             ((?vp
               (referent ?ev)
               (phrase-type vp)
               (constituents (?finite-verb)))
              <-
              (?VP
               (HASH meaning ((reference-point ?ev present-time-frame)))
               --
               (syn-cat (phrase-type vp)
                        (agreement ?agr))
               (tam (tense past)
                    (aspect ?aspect)
                    (modality indicative))
               (constituents (?finite-verb)))
              (?finite-verb
               (parent ?vp)
               (referent ?ev)
               (sem-frame verb)
               --
               (parent ?vp)
               (syn-cat (agreement ?agr)
                        (verb-form base-form)
                        (finite +))))
             :description "A default construction that applies after the other TAM constructions.
                           Incompatible with modal auxiliaries."
             :disable-automatic-footprints nil
             :attributes (:label (phrasal unmarked-phrasal)))

(def-fcg-cxn TAM-Past-Tense-cxn
             ((?vp
               (referent ?ev)
               (phrase-type vp)
               (constituents (?finite-verb)))
              <-
              (?VP
               (HASH meaning ((reference-point ?ev past-time-frame)))
               --
               (syn-cat (phrase-type vp)
                        (agreement ?agr))
               (tam (tense past)
                    (aspect ?aspect)
                    (modality indicative))
               (constituents (?finite-verb)))
              (?finite-verb
               (parent ?vp)
               (referent ?ev)
               (sem-frame verb)
               --
               (parent ?vp)
               (syn-cat (agreement ?agr)
                        (verb-form participle)
                        (finite +))))
             :description "A default construction that applies after the other TAM constructions.
                           Incompatible with modal auxiliaries."
             :disable-automatic-footprints nil
             :attributes (:label (phrasal unmarked-phrasal)))
;; (comprehend-and-formulate "the window opens")
;; (fcg-show-cxn 'tam-past-tense-cxn)
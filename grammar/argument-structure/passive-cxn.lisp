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

(def-fcg-cxn Passive-Transitive-cxn
  ((?clause
    (constituents (?vp))
    (syn-cat (clause-type transitive)))
   (?vp
    (constituents (?passive-aux ?verb))
    (form ((fields (?modal-aux ?perf-aux ?asp-aux ?passive-aux ?main-verb ?ref-ev))))
    (tam ?tam)
    (referent ?ref-ev))
   (?passive-aux
    (args ?args))
   <-
   (?clause
    --
    (HASH form ((before ?np ?vp ?clause))))
   (?vp
    --
    (parent ?clause)
    (syn-cat (phrase-type VP)
             (agreement ?agr)))
   (?passive-aux
    (HASH meaning ((frame-type event passive-transitive ?ref-ev ?undergoer)))
    --
    (lex-id be)
    (parent ?vp)
    (syn-cat (lex-class aux)
             (is-passive-marker +)
             (verb-form ?vf)
             (agreement ?agr)
             (finite ?fin)))
   (?verb
    (parent ?vp)
    (args ?args)
    (arg-struct ((role ?ref Actor)
                 (role ?ref2 Undergoer)))
    (referent ?ref-ev)
    (sem-frame verb)
    --
    (parent ?vp)
    (voice passive)
    (syn-cat (agreement ?agr))
    (functional-structure
     ((subject ?subject)))
    (dependents (?subject ?passive-aux)))
   (?subject
    (parent ?np)
    (syn-cat
     (lex-class common-noun))
    (referent ?ref2)
    --
    (parent ?np)
    (syn-cat
    (agreement ?agr)
     (lex-class common-noun))))
   :disable-automatic-footprints nil
   :feature-types ((functional-structure sequence))
   :attributes (:label arg-cxn))
;; (comprehend-and-formulate "the ball was kicked")

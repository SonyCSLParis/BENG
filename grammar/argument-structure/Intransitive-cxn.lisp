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

(def-fcg-cxn Topic-Comment-cxn
             (<-
              (?clause
               (information-structure (topic ?np)
                                      (comment ?vp))
               --
               (HASH form ((first ?np ?clause)))
               (constituents (?np ?vp))
               (syn-cat (clause-type intransitive)))
              (?np
               --
               (syn-cat (phrase-type NP))
               (parent ?clause))
              (?vp
               --
               (syn-cat (phrase-type VP))
               (parent ?clause)))
             :disable-automatic-footprints nil
             :attributes (:label arg-cxn))

(def-fcg-cxn Intransitive-cxn
  ((?clause
    (constituents (?vp))
    (syn-cat (clause-type intransitive)))
   <-
   (?vp
    --
    (HASH form ((before ?np ?vp ?clause)))
    (parent ?clause)
    (syn-cat (phrase-type VP)))
   (?verb
    (arg-struct ((role ?ref Emphasized-Role)))
    (parent ?vp)
    (referent ?ev-ref)
    (sem-frame verb)
    --
    (functional-structure (subject ?subject)
                          (not (object ?object)))
    (parent ?vp))
   (?subject
    (parent ?np)
    (syn-cat (lex-class ?lex-class))
    (referent ?ref)
    --
    (parent ?np)))
   :disable-automatic-footprints nil
   ; :feature-types ((functional-structure sequence))
   :attributes (:label arg-cxn))
;; (comprehend "the window broke")


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

(def-fcg-cxn Intransitive-cxn
  ((?clause
    (constituents (?vp))
    (syn-cat (clause-type intransitive)))
   (?vp
    (constituents (?verb))
    (tam ?tam))
   <-
   (?vp
    --
    (HASH form ((before ?np ?vp ?clause)))
    (parent ?clause)
    (syn-cat (phrase-type VP)))
   (?verb
    (HASH meaning ((frame-type event intransitive-frame ?ev-ref ?ref)))
    (arg-struct ((role ?ref Emphasized-Role)))
    (parent ?vp)
    (referent ?ev-ref)
    (sem-frame verb)
    --
    (functional-structure ((subject ?subject)))
    (dependents (?subject))
    (parent ?vp)
    (voice active)
    (syn-cat (agreement ?agr)))
   (?subject
    (parent ?np)
    (syn-cat (lex-class common-noun))
    (referent ?ref)
    --
    (parent ?np)
    (syn-cat (agreement ?agr)
             (lex-class common-noun))))
   :disable-automatic-footprints nil
   :feature-types ((functional-structure sequence))
   :attributes (:label arg-cxn))
;; (comprehend-and-formulate "the window broke")
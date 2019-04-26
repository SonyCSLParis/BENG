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
  (nil
   <-
   (?vp
    --
    (HASH form ((before ?np ?vp ?clause)))
    (parent ?clause)
    (syn-cat (phrase-type VP)))
   (?verb
    (parent ?vp)
    (arg-struct ((role ?ref Emphasized-Role)))
    (referent ?ev)
    (sem-frame verb)
    --
    (parent ?vp)
    (voice active)
    (sem-frame verb)
    (arg-struct ((role ?ref Emphasized-Role)))
    (syn-cat (agreement ?agr))
    (functional-structure
     ((subject ?subject)))
    (dependents (?subject)))
   (?subject
    (parent ?np)
    (syn-cat
     (lex-class common-noun))
    (referent ?ref)
    --
    (parent ?np)
    (syn-cat
    (agreement ?agr)
     (lex-class common-noun))))
   :disable-automatic-footprints nil
   :feature-types ((functional-structure sequence))
   :attributes (:label arg-cxn))
;; (comprehend-and-formulate "the window broke")

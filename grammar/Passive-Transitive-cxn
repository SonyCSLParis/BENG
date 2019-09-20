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
	
    (constituents (?auxiliary ?verb))
    (tam ?tam))
   <-
   (?vp
    --
    (HASH form ((before ?np ?vp ?clause)))
    (parent ?clause)
    (syn-cat (phrase-type VP)))
   (?verb
    (HASH meaning ((frame-type event active-transitive ?ev ?ref ?ref2)))
    (parent ?vp)
    (arg-struct ((role ?ref Actor)
                 (role ?ref2 Undergoer)))
    (referent ?ev)
    (sem-frame verb)
    --
    (parent ?vp)
    (voice active)
    (sem-frame verb)
    (arg-struct ((role ?ref Actor)
                 (role ?ref2 Undergoer)))
    (syn-cat (agreement ?agr))
    (functional-structure
     ((subject ?subject)
      (object ?object)))
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
     (lex-class common-noun)))
   (?object
    (syn-cat
     (lex-class common-noun))
    (referent ?ref2)
    --
    (syn-cat
     (agreement ?agr)
     (lex-class common-noun))))   
   :disable-automatic-footprints nil
   :feature-types ((functional-structure sequence))
   :attributes (:label arg-cxn))
;; (comprehend-and-formulate "the boy broke the window")

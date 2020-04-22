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

(def-fcg-cxn By-Phrase-cxn
             (
              <-
              (?by-phrase
               --
               (referent ?actor)
               (syn-cat (case by-phrase))
               (constituents (?np)))
              (?np
               --
               (syn-cat (phrase-type np))
               (parent ?by-phrase)
               (referent ?actor))                        
              (?clause
               --
               (syn-cat (clause-type ?clause-type))
               (constituents (?vp ?by-phrase)))
              (?vp
               --
               (head ?verb)
               (syn-cat (phrase-type vp)))
              (?verb
               --
               (voice passive)
               (parent ?vp)
               (arg-struct ((role ?actor actor)))))
               :disable-automatic-footprints nil
               :attributes (:label arg-cxn))
;; (comprehend "the ball was kicked")

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

(def-fcg-cxn NP-VedNoun-cxn
  ((?np
    (referent ?ref))
   <-
   (?deverbal-adjective
    (parent ?np)
    (arg-struct ((role ?input undergoer)))
    (referent ?ref)
    --
    (parent ?np)
    (syn-cat (agreement ?agr)
             (finite -)
             (verb-form participle)))
   (?noun
    (syn-cat (lex-class noun))
    (args (?input ?context))
    (referent ?ref)
    (parent ?np)
    --
    (syn-cat
     (agreement ?agr)
     (lex-class noun))
    (dependents (?deverbal-adjective))
    (parent ?np))
   (?np
    --
    (HASH form ((meets ?deverbal-adjective ?noun ?np)))
    (syn-cat
     (phrase-type NP)
     (agreement ?agr))
    (constituents (?deverbal-adjective ?noun))
    (head ?noun)))
 :disable-automatic-footprints nil
 :attributes (:label phrasal))
;; (comprehend "the opened window")

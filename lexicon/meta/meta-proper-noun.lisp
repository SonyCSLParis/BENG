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

(def-fcg-cxn unknown-proper-noun-cxn
             ((?unknown-noun
               (footprints (lex)))
              <-
              (?np
               --
               (referent ?referent)
               (constituents (?unknown-noun))
               (syn-cat (phrase-type np)
                        (agreement ?agr)))
              (?unknown-noun
               (HASH meaning ((domain named-entity ?unknown-entity ?referent ?input ?referent)))
               (lex-id ?unknown-entity)
               (referent ?referent)
               (args (?output ?input))
               --
               (HASH form ((string ?unknown-noun ?the-string)))
               (parent ?np)
               (syn-cat (lex-class proper-noun)
                        (agreement ?agr))))
             :disable-automatic-footprints nil
             :attributes (:label meta-cxn))

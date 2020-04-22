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

(def-fcg-cxn unknown-noun-cxn
             ((?unknown-noun
               (footprints (lex)))
              <-
              (?unknown-noun
               (HASH meaning ((domain object ?unknown-entity ?output ?input ?referent)))
               (lex-id ?unknown-entity)
               (referent ?referent)
               (args (?output ?input))
               --
               (HASH form ((string ?unknown-noun ?the-string)))
               (syn-cat (lex-class noun))))
             :disable-automatic-footprints nil
             :attributes (:label meta-cxn))

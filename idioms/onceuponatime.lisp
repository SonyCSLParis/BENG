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

(def-fcg-cxn onceuponatime-cxn
             (
              <-
              (?idiom
               (footprints (NP))
               (HASH meaning ((frame-type genre-indicator onceuponatime ?setting ?story)))
               --
               (HASH form ((meets ?once ?upon ?idiom)))
               (syn-cat (phrase-type NP)))
              (?once
               (footprints (lex morph))
               --
               (parent ?idiom)
               (HASH form ((string ?once "once")))
               )
             (?upon
               (footprints (lex morph))
               --
               (parent ?idiom)
               (HASH form ((string ?upon "upon")))
               )
              (?atime
               (footprints (NP))
               --
               (HASH form ((meets ?a ?time ?atime)))
               (syn-cat (phrase-type NP))
               (constituents (?a ?time)))
             (?a
               (footprints (lex morph))
               --
               (parent ?atime)
               (HASH form ((string ?a "a")))
               )
              (?time
               (footprints (lex morph))
              --
               (parent ?atime)
               (HASH form ((string ?time "time")))
               ))
             :disable-automatic-footprints t
             :attributes (:label idioms-morph))
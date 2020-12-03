;;; Copyright (C) 2020  Sony Computer Science Laboratories Paris
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

(def-fcg-cxn NP->one-constituent-cxn
             ((?NP-unit
               (footprints (referring-expression)))
               <-
              (?NP-unit
               (referent ?ref)
               --
               (footprints (not referring-expression))
               (constituents (==p ?constituent-1)))
              (?constituent-1
               (referent ?ref)
               (syn-cat (agreement ?agreement))
               --
               (parent ?NP-unit)
               (args (?ref ?x))))
             :disable-automatic-footprints t
             :feature-types ((constituents sequence))
             :attributes (:label unmarked-phrasal :string "np-1"))

(def-fcg-cxn NP->two-constituents-cxn
             ((?NP-unit
               (footprints (referring-expression)))
               <-
              (?NP-unit
               (referent ?ref)
               --
               (HASH form ((meets ?constituent-1 ?constituent-2 ?NP-unit)))
               (constituents (==p ?constituent-1 ?constituent-2)))
              (?constituent-1
               (referent ?ref)
               (syn-cat (agreement ?agreement))
               --
               (parent ?NP-unit)
               (args (?ref ?x)))
              (?constituent-2
               (referent ?ref)
               (syn-cat (agreement ?agreement))
               --
               (parent ?NP-unit)
               (args (?x ?context))))        
             :disable-automatic-footprints t
             :feature-types ((constituents sequence))
             :attributes (:label unmarked-phrasal :string "np-2"))
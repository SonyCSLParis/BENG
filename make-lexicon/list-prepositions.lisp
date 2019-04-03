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

(defparameter *prepositions-spatiotemporal* nil)
(defparameter *prepositions-other* nil)

(setf *prepositions-other* '("of" "like" "with")
      *prepositions-spatiotemporal* (sort-alphabetically (remove-duplicates '("in" "out" "under" "on" "near" "from" "to" "into" "between" "near" "above" "across" "against" "behind" "along" "around" "below" "beneath" "inside" "off" "through" "front" "within" "at" "before" "until" "aboard" "after" "amid" "amidst" "beside" "besides" "beyond" "by" "for" "onto" "outside" "over" "toward" "towards") :test #'string=) :key #'identity))
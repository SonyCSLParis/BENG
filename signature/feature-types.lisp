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

(in-package :fcg)

(export '(;; Features
          alignment
          ; categories ;; Do not export, is already exported in :type-hierarchies.
          dependents
          footprints
          history
          subunits
          boundaries
          agreement
          args
          ev-args
          sem-class
          lexical-alignment
          potential-lex-class
          syn-roles
          form
          meaning
          lex-id

          ;; Construction Sets
          hashed-string
          marked-morph
          zero-morph
          hashed-lex-id
          lex
          referring-expressions
          marked-phrasal
          unmarked-phrasal
          defaults
          functional
          arg-cxn
          unhashed
          lex2

          ))
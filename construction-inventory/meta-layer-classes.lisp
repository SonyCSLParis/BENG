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

;;; ------------------------------------------------------------------------------------------
;;; Unknown Words
;;; ------------------------------------------------------------------------------------------

;;; Diagnosed at event 'fcg::new-node (notified by #'next-cip-solution)

(defclass problem-unknown-BENG-word (problem)
  ((diagnosis :documentation "More information about which kind of unknown word was found."
              :initarg :diagnosis
              :initform nil
              :accessor diagnosis))
  (:documentation "A word not covered by the lexicon has been detected."))

(defclass diagnose-unknown-BENG-word (diagnostic)
  ((trigger :initform 'fcg::new-node)))

(defclass repair-unknown-BENG-word (repair)
  ((trigger :initform 'fcg::new-node)))

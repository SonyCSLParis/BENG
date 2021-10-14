
;;; Copyright (C) 2019-present  Sony Computer Science Laboratories Paris
;;;                             Remi van Trijp (www.remivantrijp.eu)
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

;;;;; -----------------------------------------------------------------------------------------------------------
;;;;; 1. Constructional Network Inventory
;;;;; -----------------------------------------------------------------------------------------------------------
;;;;;
;;;;; Code based on the constructional networks by Pieter Wellens (see babel-core).
;;;;; Wellens, Pieter (2011). Organizing Constructions in Networks. In L. Steels,
;;;;; Design Patterns in Fluid Construction Grammar (pp. 181-202). Amsterdam: John Benjamins.
;;;;; https://ai.vub.ac.be/sites/default/files/wellens-networks.pdf

(export '(constructional-network-inventory
          cxn-vertices cxn-edges))

(defmethod corresponding-processing-cxn-inventory-type ((cxn-inventory (eql 'constructional-network-inventory)))
  'hashed-construction-set)

(defclass constructional-network-inventory (hashed-fcg-construction-set)
  ((constructions-hash-table
    :documentation "Respecifying this slot for having more readable accessors."
    :accessor constructions-hash-table
    :accessor cxn-vertices
    :initarg :constructions-hash-table
    :initarg :cxn-vertices
    :initform (make-hash-table :test #'equalp))
   (cxn-edges
    :documentation "Slot that contains the edges of the network."
    :accessor cxn-edges
    :initarg cxn-edges
    :initform nil))
  (:documentation "A specific hashed-fcg-construction set that uses networks to influence processing and learning."))

(defmethod copy-object-content ((source constructional-network-inventory)
                                (destination constructional-network-inventory))
  (setf (cxn-edges destination)
        (copy-object (cxn-edges source)))
  (setf (constructions destination) (copy-list (constructions source)))
  (setf (trash destination) (copy-list (trash source)))
  (loop for key being the hash-keys of
        (constructions-hash-table source)
        do (setf (gethash key (constructions-hash-table destination))
                 (gethash key (constructions-hash-table source)))))

(defmethod find-cxn ((construction t)
                     (cxn-inventory constructional-network-inventory)
                     &key (key #'name) &allow-other-keys)
  (declare (ignore key))
  (fcg-find-cxn (if (symbolp construction) construction
                  (name construction))
                cxn-inventory))

;;;;; -----------------------------------------------------------------------------------------------------------
;;;;; 2. Compartmentalized Construction Inventory
;;;;; -----------------------------------------------------------------------------------------------------------
;;;;;
;;;;; Inspired by:
;;;;; (a) the discussions with Luc Steels on cellular compartmentalization;
;;;;; (b) the network code by Pieter Wellens and its application priming experiments;
;;;;; (c) research on constructional networks by Holger Diessel, Adele Goldberg, and others;
;;;;; (d) augmented transition networks by William Woods.
;;;;;
;;;;; Even though the basic primitive of linguistic structure is assumed to be a "construction", the linguistic
;;;;; inventory is not one big soup of constructions, but rather a highly structured network. Moreove, here we test
;;;;; the hypothesis that this network may develop compartments that specialize on particular functions.
;;;;;
;;;;; Remi 20/05/2021: These classes are still unused and will be further explored in future updates.

(export '(compartmentalized-construction-inventory
          compartments))

(defclass compartmentalized-construction-inventory (construction-inventory-collection)
  ((compartments
    :documentation "I respecify this slot because I want to give a specific init-form and some extra accessors and initargs."
    :accessor construction-inventories
    :accessor compartments
    :initarg :compartments
    :initarg :construction-inventories
    :initform (make-blackboard)))
  (:documentation "This is a specific collection of two
  construction-networks. One with label 'production, the other 'parsing. It is
  used to investigate how network structures can influence
  constructional processing."))

(defmethod copy-object-content ((source compartmentalized-construction-inventory)
                                (destination compartmentalized-construction-inventory))
  ;; this is done by construction-inventory-collection
  )
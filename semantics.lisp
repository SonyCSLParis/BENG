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

;;;;; --------------------------------------------------------------------------------
;;;;; This file contains a specification of the semantics used by the English grammar.
;;;;; --------------------------------------------------------------------------------

(defparameter *english-grammar-semantics* nil "Slots of semantic views for the English grammar.")
(setf *english-grammar-semantics* '((perspective
                                     (primary referent filler)
                                     (secondary referent filler)
                                     (tertiary referent filler)
                                     (oblique referent filler))
                                    (semantic-operations
                                     (truth-value referent))
                                    (referent-deixis
                                     (referent-status referent context)
                                     (inquired-referent referent context)
                                     (discourse-status discourse-participant referent)
                                     (discourse-function referent context))
                                    (frames
                                     (frame-type frame instance referent) ;; Events
                                     ;; "Domain" comes from Cognitive Grammar (~ Langacker)
                                     (domain instance output input referent)) ;; Non-events
                                    (construal-schema
                                     (semantic-function referent)
                                     (connector referent connected-1 connected-2))
                                    (tamm
                                     (event-deixis instance referent input)
                                     (distance-to-rp referent)
                                     (temporal-relation output input reference-point)
                                     (event-qualification output input))))

;;;;; Some helper functions.
;;;;; ----------------------
(defun english-extract-label (meaning-predicate)
  "Given a meaning predicate, extract the label of the construction."
  (case (first meaning-predicate)
    ('unique-referent (third meaning-predicate))
    ('frame-type (third meaning-predicate))
    ('domain (third meaning-predicate))
    ('referent-status (second meaning-predicate))
    ('inquired-referent (second meaning-predicate))
    ('discourse-function (second meaning-predicate))
    ('discourse-status (second meaning-predicate))
    ('distance-to-rp (second meaning-predicate))
    ('event-deixis (third meaning-predicate))
    ('connector (second meaning-predicate))
    ('temporal-relation (second meaning-predicate))
    ('event-qualification (second meaning-predicate))
    ('truth-value (second meaning-predicate))
    (t
     nil)))
    

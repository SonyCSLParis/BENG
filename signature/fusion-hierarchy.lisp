;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Author: Remi van Trijp (www.remivantrijp.eu)
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

;;; FUSION is a process in constructional language processing in which two categories
;;; are compared in terms of their s(NP (DT A) (NN TAX) (NN ASSESSMENT))emantic compatibility. If so, they can "fuse" with
;;; each other. We operationalize fusion by using an open-ended CATEGORY HIERARCHY as
;;; implemented by Paul Van Eecke.
;;;
;;; For more about fusion, see:
;;; ----------------------------------------------------------------------------
;;; Goldberg, Adele E. (1995). Constructions: A Construction Grammar Approach to Argument
;;;     Structure Constructions. Chicago: Chicago University Press.
;;;
;;; The fusion hierarchy is a severely simplified version of the more general
;;; Category Hierarchy system by Paul Van Eecke. If you need to use its capabilities,
;;; please check out:
;;; ----------------------------------------------------------------------------
;;; Van Eecke, Paul (2018). Generalisation and Specialisation Operators for Computational
;;;     Construction Grammar and their Application for Evolutionary Linguistics Research.
;;;     PhD Thesis. Brussels: Vrije Universiteit Brussel.
;;;     https://cris.vub.be/files/39711319/Paul_Van_Eecke_phd.pdf
;;;

(in-package :beng)

(export '(*fusion-hierarchy*
          get-fusion-hierarchy
          fusion-add-parent))

(defparameter *fusion-hierarchy* nil "Captures the relations for the fusion hierarchy.")
(setf *fusion-hierarchy* (make-hash-table))

(defun get-fusion-hierarchy (cxn-inventory)
  (if cxn-inventory
    (get-data (blackboard cxn-inventory) :fusion-hierarchy)
    *fusion-hierarchy*))

(defun fusion-add-parent (entry parent fusion-hierarchy)
  (setf (gethash entry fusion-hierarchy)
        (pushnew parent (gethash entry fusion-hierarchy))))

(defun fuseable-p (x y fusion-hierarchy)
  "Checks whether x can be fused with y."
  (cond ((null x) nil)
        ((symbolp x) (fuseable-p (gethash x fusion-hierarchy) y fusion-hierarchy))
        ((member y x) t)
        (t
         (fuseable-p (append (rest x) (gethash x fusion-hierarchy)) y fusion-hierarchy))))

;; Now we start filling the fusion hierarchy:
;; ----------------------------------------------------------------------------
;; Argument Roles
(dolist (arg-role '(Actor Undergoer Emphasized-Role))
  (fusion-add-parent arg-role 'Top *fusion-hierarchy*))

;; Grammatical Functions
(dolist (grammatical-function '(Subject Object Dative Oblique))
  (fusion-add-parent grammatical-function 'Top *fusion-hierarchy*))

;; Parts-of-Speech
(progn
  (dolist (pos '(Noun Deverbal-Adjective Determiner))
    (fusion-add-parent pos 'Top *fusion-hierarchy*))
  (fusion-add-parent 'Verb 'Deverbal-Adjective *fusion-hierarchy*)
  (dolist (pos '(Common-Noun Proper-Noun))
    (fusion-add-parent pos 'Noun *fusion-hierarchy*))
  (dolist (pos '(Aux Lex-Verb))
    (fusion-add-parent pos 'Verb *fusion-hierarchy*))
  (dolist (pos '(article))
    (fusion-add-parent pos 'Determiner *fusion-hierarchy*)))
           
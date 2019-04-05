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

;;; FUSION is a process in constructional language processing in which two categories
;;; are compared in terms of their semantic compatibility. If so, they can "fuse" with
;;; each other. We operationalize fusion by using an open-ended CATEGORY HIERARCHY as
;;; implemented by Paul Van Eecke.
;;;
;;; For more about fusion, see:
;;; ----------------------------------------------------------------------------
;;; Goldberg, Adele E. (1995). Constructions: A Construction Grammar Approach to Argument
;;;     Structure Constructions. Chicago: Chicago University Press.
;;;
;;; For more about the Category Hierarchies, see chapter of:
;;; ----------------------------------------------------------------------------
;;; Van Eecke, Paul (2018). Generalisation and Specialisation Operators for Computational
;;;     Construction Grammar and their Application for Evolutionary Linguistics Research.
;;;     PhD Thesis. Brussels: Vrije Universiteit Brussel.
;;;     https://cris.vub.be/files/39711319/Paul_Van_Eecke_phd.pdf
;;;
;;;

(in-package :beng)

(defparameter *fusion-hierarchy* nil "Captures the relations for the fusion hierarchy.")

(setf *fusion-hierarchy* (make-instance 'type-hierarchy))
(add-categories '(Top
                  ;; Argument Roles
                  Actor Undergoer Emphasized-Role
                  ;; Grammatical Functions
                  Subject Object Dative Oblique
                  ;; Parts-of-Speech
                  Noun CommonNoun ProperNoun
                  Verb Aux Lex-Verb
                  ) *fusion-hierarchy*)

;; Argument Roles
(dolist (arg-role '(Actor Undergoer Emphasized-Role))
  (add-link arg-role 'Top *fusion-hierarchy* :weight 1.0))

;; Grammatical Functions
(dolist (grammatical-function '(Subject Object Dative Oblique))
  (add-link grammatical-function 'Top *fusion-hierarchy* :weight 1.0))

;; Parts-of-Speech
(progn
  (dolist (pos '(Noun Verb))
    (add-link pos 'Top *fusion-hierarchy*))
  (dolist (pos '(CommonNoun ProperNoun))
    (add-link pos 'Noun *fusion-hierarchy* :weight 1.0))
  (dolist (pos '(Aux Lex-Verb))
    (add-link pos 'Verb *fusion-hierarchy* :weight 1.0)))
           
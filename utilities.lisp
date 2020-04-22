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

(defun beng-comprehend (utterance)
  (activate-monitor trace-fcg)
  (multiple-value-bind (a solution)
      (comprehend utterance)
    (declare (ignore a))
    (setf *saved-cfs* (fcg-get-transient-structure solution))
    (add-element `((h2) "Named entities:"))
    (nlp-tools::run-displacy-ents utterance)
    ;(add-element `((h2) "Theme-Rheme Structure:"))
    ;(add-element `((p) ,(format nil "Theme: ~a" (beng-get-theme *saved-cfs*))))
    ;(add-element `((p) ,(format nil "Rheme: ~a" (beng-get-rheme *saved-cfs*))))
    (add-element `((h2) "Dependency Structure:"))
    (nlp-tools::run-displacy utterance)
    (add-element `((h2) "Immediate Constituent Structure:"))
    (multiple-value-bind (a b) (nlp-tools::get-beng-sentence-analysis utterance)
      (let* ((units (make-units b))
             (cfs (make-instance 'coupled-feature-structure :left-pole units)))
        (add-element (make-html-fcg-light cfs :feature-types (feature-types *fcg-constructions*)
                                          :configuration (visualization-configuration *fcg-constructions*)
                                          :construction-inventory *fcg-constructions*))))                                          
    solution))

;;;;; Configuration utilities.
;;;;; --------------------------------------------------------------------------------
(defun set-parse-order (order &optional (cxn-inventory *fcg-constructions*))
  (set-configuration cxn-inventory :parse-order order)
  (set-configuration (processing-cxn-inventory cxn-inventory)
                     :parse-order order))
;; (set-parse-order '(hashed-string hashed-lex-id phrasal arg-cxn))


(defun set-production-order (order &optional (cxn-inventory *fcg-constructions*))
  (set-configuration cxn-inventory :production-order order)
  (set-configuration (processing-cxn-inventory cxn-inventory)
                     :production-order order))
;; (set-production-order '(hashed-meaning phrasal arg-cxn hashed-lex-id))

;;;;; English grammar utilities.
;;;;; --------------------------------------------------------------------------------
(defun marked-form? (string)
  (or (unify ".+ing" string)
      (unify ".+s" string)
      (unify ".+ed" string)))

(defparameter *irregular-verb-stems* nil)
(defparameter *anomalous-verbs* nil)

(defun strong-verb-p (verb-lex-id)
  (member verb-lex-id (append *irregular-verb-stems* *anomalous-verbs*)))
;; (strong-verb-p 'shine)

(defun escape-quotes (sentence)
  "Escapes a single quote for compatibility with syntaxnet."
  (if (find #\' sentence :test #'char=)
    (let* ((split-sentence (split-sequence:split-sequence #\' sentence :remove-empty-subseqs t))
           (new-sentence (first split-sentence))
           (escaped-quote (format nil "~a" "\"'\"")))
      (dolist (string (rest split-sentence) new-sentence)
        (setf new-sentence (string-append new-sentence escaped-quote string))))
    sentence))

;;;;; Macros.
;;;;; --------------------------------------------------------------------------------
(defmacro collect-when (var-and-test list)
  `(loop for ,(first var-and-test) in ,list
         when ,(second var-and-test)
         collect ,(first var-and-test)))
;;;; Usage:
;;;; (collect-when (var test) list)
;;;;
;;;; Example:
;;;; FCG 39 > (let ((lst '(1 2 3)))
;;;;            (collect-when (x (oddp x)) lst))
;;;; >> (1 3)

(defmacro return-if (x &optional else)
  (let ((var (gensym)))
  `(let ((,var ,x))
     (if ,var ,var ,else))))
;;;; FCG 44 > (let ((x 'test))
;;;;            (return-if x 'not-true))
;;;; >> TEST
;;;;
;;;; FCG 41 > (let ((x nil))
;;;;            (return-if x 'not-true))
;;;; >> NOT-TRUE
;;;;
;;;; FCG 49 > (return-if (rest '(a b)) 'no-elements-left)
;;;; (B)
;;;;
;;;; FCG 50 > (return-if (rest '(a)) 'no-elements-left)
;;;; NO-ELEMENTS-LEFT

(defmacro return-when (x)
  `(return-if ,x))

;;;;; Miscellaneous functions.
;;;;; --------------------------------------------------------------------------------
(defun single-solution-p (list)
  "Checks whether there is only one solution in a list and returns it."
  (and (null (rest list))
       (first list)))

(defun sort-alphabetically (list-of-symbols &key (key #'symbol-name))
  (sort (copy-list list-of-symbols) #'string< :key key))

(defun find-constant-for-var (var list-of-bindings)
 "Given a variable, find whether there is a constant bound to it."
 (let ((association (rest (assoc var list-of-bindings))))
   (if (variable-p association)
     (find-constant-for-var association list-of-bindings)
     association)))


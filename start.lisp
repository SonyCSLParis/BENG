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

;;; Make sure that you have loaded the file english-grammar.asd (either through
;;; your Lisp init-file or manually). Then load the English grammar by evaluating:

(ql:quickload :beng)

(in-package :beng)

;; First time using the English grammar:
;; -------------------------------------
(build-grammar)
(save-beng *fcg-constructions* :name "beng")

;; Starting from a precompiled model:
;; ----------------------------------
(restore-beng :name "beng")

;; Loading the grammar without writing the files:
;; ----------------------------------------------
(build-grammar :write-files? nil)

;; Customizing what you load:
;; --------------------------
(progn
  (make-beng-cxns)
  (load-lexicon '("nouns")))
;; (size *fcg-constructions*)


;; Start testing:
;; -----------------------------------------------
;; For turning the FCG web interace on and off:
(toggle-monitor trace-fcg)
(comprehend "what did the boy break?")
(comprehend "the boy broke the window")

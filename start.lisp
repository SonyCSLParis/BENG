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

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Loading the Basic English Grammar:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; First time using the English grammar, or when you need to rebuild everything:
;; -----------------------------------------------------------------------------
(build-grammar)

;; If you already built the grammar before, you can skip the writing process:
;; -----------------------------------------------------------------------------
(build-grammar :write-files? nil)

;; If you want to save and restore an image of your grammar (this will be
;; stored in the "models" folder of your BENG repository):
;; -----------------------------------------------------------------------------
(save-beng *fcg-constructions* :name "beng")
(restore-beng :name "beng")

;; If you want to customize what you load:
;; -----------------------------------------------------------------------------
(progn
  (make-beng-cxns) ;; Initializes the construction inventory.
  (load-lexicon '("nouns")) ;; Loads all files in the folder lexicon/nouns
  (load-adjective "happy")) ;; Only loads the adjective happy.
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Developing the Basic English Grammar:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; If you use LispWorks on MacOSx, you can also use the following FCG developer tools:
;; ------------------------------------------------------------------------------------------------------------
;; (dev-construction-browser :mixed)       ; See demo at http://remivantrijp.eu/construction-browser/ 
;; (dev-construction-bank-builder)         ; See demo at http://www.remivantrijp.eu/construction-bank-builder/
;; (dev-construction-tutor :package :beng) ; See demo at http://www.remivantrijp.eu/construction-tutor/

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Start testing:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; For turning the FCG web interace on and off (only use one monitor at the time):
(toggle-monitor trace-fcg)                    ; Recommended
; (toggle-monitor trace-fcg-debugging)        
; (toggle-monitor trace-fcg-search-process)

;; Some examples
(comprehend "what did the boy break?")
(comprehend "the boy broke the window")
(comprehend "The average sentence is usually about ten words long.")                 

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

(import '(nlp-tools::*penelope-host*))

(in-package :beng)

(setf *penelope-host* "http://spacy.fcg-net.org/")


;;; ******************************************************************
;;; *** IMPORTANT: BENG requires an internet connection to work!!! ***
;;; ******************************************************************

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Loading the Basic English Grammar:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; Without loading anything, you can already test the de-render function of the
;;; English grammar. If things run slowly, check your internet connection.
(progn
  (activate-monitor trace-fcg)
  (make-empty-construction-inventory-cxns))
(comprehend "Dr Luc Steels founded the Sony Computer Science Laboratories Paris in 1996, where he assembled a talented team.")
(comprehend "The film executive behind HarperCollins Publishers will rebuild her operation at Sony Pictures after being jettisoned by Walt Disney Studios")

;; You can check the transient structure in your web browser at:
;; http://localhost:8000/
;; Hover above the transient structure to show a pop-up menu, and click
;; "h1" and "h2" to switch between a constituency- and dependency-view on
;; the transient structure.

;; First time using the English grammar, or when you need to rebuild everything.
;; This will write files for each  lexical construction in the folder lexicon,
;; where you can inspect the definitions of the constructions.
;; -----------------------------------------------------------------------------
(build-grammar :write-files t)

;; If you already built the grammar before, you can skip the writing process:
;; -----------------------------------------------------------------------------
(build-grammar :write-files? nil)

;; If you want to save and restore an image of your grammar (this will be
;; stored in the "models" folder of your BENG repository). Building the grammar
;; is as fast as restoring it, but restoring guarantees you start from the
;; same grammar as your last saved session.
;; -----------------------------------------------------------------------------
(save-beng *fcg-constructions* :name "beng")
(restore-beng :name "beng")

;; Typically you want to customize what you load in order to focus only on
;; particular information/challenges. For customizing:
;; -----------------------------------------------------------------------------

;; If you need to rewrite all the files for the lexicon:
;; (write-lexicon)
(with-disabled-monitors ;; Make sure you do not have an active monitor for speed reasons
  (make-beng-cxns) ;; Initializes the construction inventory.
  (load-lexicon '("nouns")) ;; Loads all files in the folder lexicon/nouns
  (load-adjective "happy") ;; Only loads the adjective happy.
  (load-grammatical-constructions "argument-structure")) ;; loads the constructions in the folder
                                                          ;; grammar/argument-structure

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Developing the Basic English Grammar:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; If you use LispWorks on MacOSx, you can also use the following FCG developer tools. You will first need
;; to obtain them from https://github.com/SonyCSLParis/fcg-dev-tools
;; ------------------------------------------------------------------------------------------------------------
;; (ql:quickload :dev-tools)
;; (dev-tools::dev-construction-browser :editor)       ; See demo at http://www.remivantrijp.eu/construction-browser/ 
;; (dev-tools::dev-construction-bank-builder)          ; See demo at http://www.remivantrijp.eu/construction-bank-builder/
;; (dev-tools::dev-construction-tutor :package :beng)  ; See demo at http://www.remivantrijp.eu/construction-tutor/

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Start testing:
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; For turning the FCG web interace on and off (only use one monitor at the time):
(toggle-monitor trace-fcg)
; (toggle-monitor trace-fcg-debugging)
; (toggle-monitor trace-fcg-search-process)

;; Some examples
(comprehend "the window broke")
(comprehend "the scientist presented groundbreaking work")

;; Formulation is currently only possible for a small number of constructions, so
;; it is better to test only comprehension.
(comprehend-and-formulate "the window broke")
(comprehend-and-formulate "the cat")
(comprehend-and-formulate "smelly cat")
(comprehend-and-formulate "the opened windows")

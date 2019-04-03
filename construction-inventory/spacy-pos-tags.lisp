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

;;;;; This FCG-dependency-parser hybrid uses the Spacy dependency parser
;;;;; (see semantic-dependency-parser.lisp in this folder).
;;;;;
;;;;; The Spacy Syntactic Dependency Parser uses the following POS-tags:
;;;;; https://spacy.io/api/annotation#section-pos-tagging
;;;;;

(defparameter *spacy-pos-tag-conversion-table* nil "List of POS Tags and their corresponding categories in the grammar.")

;; Note: this conversion table is far from complete, but contains only relevant conversions.
(setf *spacy-pos-tag-conversion-table*
      '(("NN" noun)
        ("NNS" noun)
        ("NNP" propernoun)
        ("NNPS" propernoun)
        ("VB" verb)
        ("VBD" verb)
        ("VBG" verb)
        ("VBN" verb)
        ("VBP" verb)
        ("VBZ" verb)))


;;; Obsolete: old conversions (might be revived if more distinctions are necessary)
;;;  (defparameter *pos-tag-translation*
;;("AFX" affix)
        ;;("JJ" adjective)
        ;;("JJR" adjective)
        ;;("JJS" adjective)
;;;   '(("CC" "CONJ")
;;;     ("CD" "CD")
;;;     ("DT" "DET")
;;;     ("EX" "EX")
;;;     ("FW" "NNP")
;;;     ("IN" "PREP")
;;;     ("JJ" "ADJ")
;;;     ("JJR" "ADJ")
;;;     ("JJS" "ADJ")
;;;     ("LS" "LS")
;;;     ("MD" "VERB")
;;;     ("PDT" "DET")
;;;     ("POS" "POS")
;;;     ("PRP" "PRONOUN")
;;;     ("PRP$" "PRONOUN")
;;;     ("RB" "ADV")
;;;     ("RBR" "ADV")
;;;     ("RBS" "ADV")
;;;     ("RP" "PARTICLE")
;;;     ("SYM" "SYMBOL")
;;;     ("TO" "TO")
;;;     ("UH" "INTERJECTION")
;;;     ("WDT" "WHDET")
;;;     ("WP" "WHPRON")
;;;     ("WP$" "WHPRON")
;;;     ("WRB" "ADV")))
;;;         


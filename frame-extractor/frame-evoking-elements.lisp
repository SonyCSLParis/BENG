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

;;; Make sure that you have loaded the file english-grammar.asd (either through
;;; your Lisp init-file or manually). Then load the English grammar by evaluating:

(in-package :fcg)

(comprehend "He sought to ridicule growing evidence of drought due to climate change.")
(comprehend  "The drought is due to climate change.")
(comprehend  "What he said is due to climate change.")
(comprehend  "Due  to climate change, there is a drought.")

(def-fcg-cxn due-to-cxn
             (<-
              (?parent-phrase
               (HASH meaning ((frame-type ?frame cause)
                              (frame-evoking-element ?frame due-to)
                              (causer ?frame ?unknown)
                              (effect ?frame ?effect-unit)))
               --
               (HASH form ((meets ?due-unit ?to-unit ?adjectival-phrase)))
               (constituents (?due-unit ?pp-unit)))
              (?due-unit
               (lex-id due-to)
               --
               (HASH form ((string ?due-unit "due")))
               (parent ?parent-phrase))
              (?pp-unit
               --
               (syn-cat (phrase-type prepositional-phrase))
               (constituents (?to-unit ?effect-unit)))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to")))
               (dep-head ?due-unit)
               (parent ?pp-unit)))
             :attributes (:string "due" :meaning due-to :label (hashed-string hashed-meaning)))



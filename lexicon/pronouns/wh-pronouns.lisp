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

;; Who


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; What
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(def-fcg-cxn what-lex
             ((?NP
               (referent ?ref)
               (syn-cat (phrase-type NP)
                        (agreement (- - ?sg ?pl))
                        (function-type WH)))
              (?what
               (footprints (lex NP))
               (parent ?NP)
               (lex-id what)
               (syn-cat (lex-class pronoun)
                        (agreement ?agr))
               (referent ?ref))
              <-
              (?what
               (HASH meaning ((domain object what ?ref ?context ?ref)))
               --
               (HASH form ((string ?what "what")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string hashed-meaning)
                          :apply-fast t :meaning what :lex-id what :string "what" :pos ("PRON")))
;; (comprehend "what")


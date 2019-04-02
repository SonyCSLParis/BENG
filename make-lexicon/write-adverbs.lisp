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

(defun write-negation-adverb (adverb out)
  "Write a different adverb for negation adverbs."
  (format out "~%(def-fcg-cxn ~a-adverb-cxn" adverb)
  (format out "~%             ((?~a-unit" adverb)
  (format out "~%               (parent ?parent)")
  (format out "~%               (footprints (lex morph number))")
  (format out "~%               (referent ?referent)")
  (format out "~%               (args (?x ?ref))")
  (format out "~%               (lex-id ~a)" adverb)
  (format out "~%               (sem-valence (negated-entity ?ref))")
  (format out "~%               (syn-cat (categories (adverb))")
  (format out "~%                        (is-negation-clitic -)")       
  (format out "~%                        (is-negation-adverb +)")       
  (format out "~%                        (syn-function adverbial)))")
  (format out "~%              <-")
  (format out "~%              (?~a-unit" adverb)
  (format out "~%               (HASH meaning ((frame-type semantic-connective ~a ?x)" adverb)
  (format out "~%                              (slot negated-entity ?x ?ref)))" adverb)
  (format out "~%               --")
  (format out "~%               (HASH form ((string ?~a-unit ~s)))))" adverb adverb)
  (format out "~%             :Description \"A negation adverb construction.\"")
  (format out "~%             :disable-automatic-footprints t")
  (format out "~%             :attributes (:label (hashed-string hashed-meaning) :apply-fast t :meaning ~a
                                           :string ~s :pos (~s)))"
          adverb adverb "ADV"))

(defun write-adverbs ()
  (dolist (adverb *adverbs*)
    (with-open-file (out (ensure-directories-exist (beng-pathname :directory '("lexicon" "adverbs")
                                                                  :name adverb
                                                                  :type "lisp"))
                         :direction :output :if-exists :supersede)
      (add-license-and-copyright-header out)
      (format out "~%~%(in-package :fcg)~%")
      (if (find adverb *negation-adverbs* :test #'equalp)
        (write-negation-adverb adverb out)
        (progn (format out "~%(def-fcg-cxn ~a-adverb-cxn" adverb)
          (format out "~%             ((?~a-unit" adverb) 
          (format out "~%               (footprints (lex morph number))")
          (format out "~%               (parent ?parent)")
          (format out "~%               (referent ?referent)")
          (format out "~%               (args (?x ?ref))")
          (format out "~%               (lex-id ~a)" adverb)
          (format out "~%               (sem-valence (trajector ?ref))")
          (format out "~%               (syn-cat (categories (adverb))")
          (format out "~%                        (is-negation-adverb -)")       
          (format out "~%                        (syn-function adverbial)))")
          (format out "~%              <-")
          (format out "~%              (?~a-unit" adverb)
          (format out "~%               (HASH meaning ((frame-type profiling-relationship ~a ?x)" adverb)
          (format out "~%                              (slot trajector ?x ?ref)))" adverb)          
          (format out "~%               --")
          (format out "~%               (HASH form ((string ?~a-unit ~s)))))" adverb adverb)
          (format out "~%             :Description \"A lexical adverb construction.\"")
          (format out "~%             :disable-automatic-footprints t")
          (format out "~%             :attributes (:label (hashed-string hashed-meaning) :apply-fast t :meaning ~a
                                                   :string ~s :pos (~s)))"
                  adverb adverb "ADV"))))))
;; (write-adverbs)

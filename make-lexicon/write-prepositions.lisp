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

(defun write-prepositions ()
  (dolist (preposition (append *prepositions-other* *prepositions-spatiotemporal*))
    (let ((spatiotemporal (if (find preposition *prepositions-other* :test #'string=) '- '+)))
      (with-open-file (out (ensure-directories-exist (beng-pathname :directory '("lexicon" "prepositions")
                                                                    :name preposition
                                                                    :type "lisp"))
                           :direction :output :if-exists :supersede)
        (add-license-and-copyright-header out)
        (format out "~%~%(in-package :beng)~%")
        (format out "~%(def-fcg-cxn ~a-preposition" preposition)
        (format out "~%             ((?~a" preposition)
        (format out "~%               (footprints (lex morph number))")
        (format out "~%               (referent ?rel)")
        (format out "~%               (args (?rel ?a-ref))")
        (format out "~%               (parent ?parent)")
        (format out "~%               (lex-id ~a)" preposition)
        (format out "~%               (sem-valence (trajector ?x)") ;; before: relator-1
        (format out "~%                            (landmark ?y))") ;; before: relator-2
        (format out "~%               (syn-cat (categories (preposition))")
        (format out "~%                        (has-spatiotemporal-potential ~a)))" spatiotemporal)
        (format out "~%              <-")
        (format out "~%              (?~a" preposition)
        (format out "~%               (HASH meaning ((frame-type ~a ?rel ?a-ref)" preposition)
        (format out "~%                              (slot trajector ?rel ?x) (slot landmark ?rel ?y)))")
        (format out "~%               --")
        (format out "~%               (HASH form ((string ?~a ~s)))))" preposition preposition)
        (format out "~%             :disable-automatic-footprints t")
        (format out "~%             :attributes (:label (hashed-string hashed-meaning) :meaning ~a :string ~s :pos (~s)))"
                preposition preposition "PREP")))))
;; (write-prepositions)
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

;;; Do the following if you need to re-generate everything:
;; (list-adjectives) ;; -> only if you do not have the file adjectives-list.lisp
;; (write-nationalities)
;; (write-adjectives)

(defun write-nationalities ()
  (dolist (item *nationalities*)
    (when (and (= 2 (length item))
               (not (find (downcase (second item)) *numerals* :key #'downcase :test #'string=)))
      (let ((string (second item)))
        (with-open-file (out (ensure-directories-exist (beng-pathname :directory '("lexicon" "nationalities")
                                                                       :name string
                                                                       :type "lisp"))
                             :direction :output :if-exists :supersede)
          (add-license-and-copyright-header out)
          (format out "~%~%(in-package :beng)~%")
          (format out "~%(def-fcg-cxn ~a-adj-lex" string)
          (format out "~%             ((?~a-unit" string)
          (format out "~%               (referent ?ref)")
          (format out "~%               (args (?r ?x))")
          (format out "~%               (lex-id ~a)" string)
          (format out "~%               (footprints (lex morph number))")
          (format out "~%               (parent ?parent)")
          (format out "~%               (sem-cat (sem-class predicate))")
          (format out "~%               (syn-cat (categories (adjective predicate))")
          (format out "~%                        (syn-function adjectival)")
          (format out "~%                        (agreement ?agr)))")
          (format out "~%              <-")
          (format out "~%              (?~a-unit" string)
          (format out "~%               (HASH meaning ((domain nationality ~a ?r ?x ?ref)))" string)
          (format out "~%               --")
          (format out "~%               (HASH form ((string ?~a-unit ~s)))))" string (string-capitalize string))
          (format out "~%             :disable-automatic-footprints t")
          (format out "~%             :attributes (:label (hashed-string hashed-meaning) :apply-fast t :meaning ~a :string ~s :pos (~s)))"
                   string (string-capitalize string) "ADJ"))))))


(defun write-adjectives ()
  (dolist (item *adjectives*)
    (when (and (= 2 (length item))
               (not (find (downcase (second item)) *numerals* :key #'downcase :test #'string=)))
      (let ((string (second item)))
        (with-open-file (out (ensure-directories-exist (beng-pathname :directory '("lexicon" "adjectives")
                                             :name string
                                             :type "lisp"))
                             :direction :output :if-exists :supersede)
          (add-license-and-copyright-header out)
          (format out "~%~%(in-package :beng)~%")
          (format out "~%(def-fcg-cxn ~a-adj-lex" string)
          (format out "~%             ((?~a-unit" string)
          (format out "~%               (referent ?ref)")
          (format out "~%               (args (?r ?x))")
          (format out "~%               (lex-id ~a)" string)
          (format out "~%               (footprints (lex morph number))")
          (format out "~%               (parent ?parent)")
          (format out "~%               (sem-cat (sem-class predicate))")
          (format out "~%               (syn-cat (categories (adjective predicate))")
          (format out "~%                        (syn-function adjectival)")
          (format out "~%                        (agreement ?agr)))")
          (format out "~%              <-")
          (format out "~%              (?~a-unit" string)
          (format out "~%               (HASH meaning ((domain category ~a ?r ?x ?ref)))" string)
          (format out "~%               --")
          (format out "~%               (HASH form ((string ?~a-unit ~s)))))" string string)
          (format out "~%             :disable-automatic-footprints t")
          (format out "~%             :attributes (:label (hashed-string hashed-meaning) :apply-fast t :meaning ~a :string ~s :pos (~s)))"
                   string string "ADJ"))))))
;; (write-adjectives)

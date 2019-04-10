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

(defun write-lex-noun-with-string (out base-form &optional (agreement '(- - + -)))
  (format out "~%(def-fcg-cxn ~a-noun-lex" base-form)
  (format out "~%             ((?~a-unit" base-form)
  (format out "~%               (parent ?phrase)")
  (format out "~%               (footprints (lex))")
  (format out "~%               (lex-id ~a)" base-form)
  (format out "~%               (syn-cat (lex-class common-noun)")
  (format out "~%                        (agreement ~a))" agreement)
  (format out "~%               (args (?r ?x))")
  (format out "~%               (referent ?ref)")
  (format out "~%               (sem-cat (sem-class identifier)))")
  (format out "~%              <-")
  (format out "~%              (?~a-unit" base-form)
  (format out "~%               (HASH meaning ((domain object ~a ?r ?x ?ref)))" base-form)
  (format out "~%               --")
  (format out "~%               (HASH form ((string ?~a-unit ~s)))))" base-form base-form)
  (format out "~%             :disable-automatic-footprints t")
  (format out "~%             :attributes (:label (hashed-string hashed-meaning) :apply-fast t :meaning ~a :string ~s :pos (~s)))~%~%"
          base-form base-form "NOUN"))


(defun write-common-noun-definition (base-form plural-form)
  ;; We write the entry.
  (with-open-file (out (ensure-directories-exist
                        (beng-pathname :directory '("lexicon" "nouns")
                                       :name (or base-form plural-form) :type "lisp"))
                       :direction :output :if-exists :supersede)
    (add-license-and-copyright-header out)
    (format out "~%~%(in-package :beng)~%")
    (cond ((string= base-form plural-form) (write-lex-noun-with-string out base-form))
          ((and (null base-form) plural-form) (write-lex-noun-with-string out plural-form '(- - - +)))
          (t
           ;; Write the lexical entry.
           (format out "~%(def-fcg-cxn ~a-noun-lex" base-form)
           (format out "~%             ((?~a-unit" base-form)
           (format out "~%               (parent ?phrase)")
           (format out "~%               (footprints (lex))")
           (format out "~%               (args (?r ?x))")
           (format out "~%               (referent ?ref)")
           (format out "~%               (sem-cat (sem-class identifier)))")
           (format out "~%              <-")
           (format out "~%              (?~a-unit" base-form)
           (format out "~%               (HASH meaning ((domain object ~a ?r ?x ?ref)))" base-form)
           (format out "~%               --")
           (format out "~%               (lex-id ~a)" base-form)
           (format out "~%               (footprints (not lex))")
           (format out "~%               (syn-cat (lex-class common-noun)")
           (format out "~%                        (agreement ?agr))))" )
           (format out "~%             :disable-automatic-footprints t")
           (format out "~%             :attributes (:label (hashed-lex-id hashed-meaning) :apply-fast t :meaning ~a :lex-id ~a :pos (~s)))~%~%"
                   base-form base-form "NOUN")

           ;; Write the singular morph
           (format out "~%(def-fcg-cxn ~a-noun-morph" base-form)
           (format out "~%             ((?~a-unit" base-form)
           (format out "~%               (footprints (morph)))")
           (format out "~%               <-")
           (format out "~%              (?~a-unit" base-form)
           (format out "~%               (lex-id ~a)" base-form)
           (format out "~%               (footprints (not morph))")
           (format out "~%               (syn-cat (lex-class common-noun)")
           (format out "~%                        (agreement (- - + -)))")
           (format out "~%               --")
           (format out "~%               (HASH form ((string ?~a-unit ~s)))))" base-form base-form)
           (format out "~%             :disable-automatic-footprints t")
           (format out "~%             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id ~a :string ~s :pos (~s)))~%~%"
                   base-form base-form "NOUN")
           
           ;; Write the plural morph
           (format out "~%(def-fcg-cxn ~a-noun-morph" plural-form)
           (format out "~%             ((?~a-unit" plural-form)
           (format out "~%               (footprints (morph)))")
           (format out "~%               <-")
           (format out "~%              (?~a-unit" plural-form)
           (format out "~%               (lex-id ~a)" base-form)
           (format out "~%               (footprints (not morph))")
           (format out "~%               (syn-cat (lex-class common-noun)")
           (format out "~%                        (agreement (- - - +)))")
           (format out "~%               --")
           (format out "~%               (HASH form ((string ?~a-unit ~s)))))" plural-form plural-form)
           (format out "~%             :disable-automatic-footprints t")
           (format out "~%             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id ~a :string ~s :pos (~s)))~%~%"
                   base-form plural-form "NOUN")))))

(defun write-deviating-common-noun-definitions ()
  (dolist (entry *irregular-common-nouns*)
    (let ((base-form (downcase (symbol-name (first entry))))
          (plural-form (downcase (symbol-name (second entry)))))
      (write-common-noun-definition base-form plural-form))))
;; (write-deviating-common-noun-definitions)

(defun write-common-noun-definitions ()
  (dolist (lex-id *common-nouns*)
    (let* ((base-form (downcase (symbol-name lex-id)))
           (plural-form (if (unify ".+ics" base-form) base-form
                          (english-make-third-s-form base-form))))

      (write-common-noun-definition base-form plural-form))))
;; (write-common-noun-definitions)

(defun write-always-plural-noun-definitions ()
  (dolist (lex-id *always-plural-nouns*)
    (let ((plural-form (downcase (symbol-name lex-id))))
      (write-common-noun-definition nil plural-form))))
;; (write-always-plural-noun-definitions)

(defun write-nouns ()
  (write-common-noun-definitions)
  (write-deviating-common-noun-definitions)
  (write-always-plural-noun-definitions))

#|

 
;; Old code used for extracting noun lists from COCA bigrams.
 
(defparameter *coca-bigrams* (babel-pathname :directory '(:up "Corpora" "English" "coca")
                                             :name "2-grams"
                                             :type "txt"))

(defun make-noun-lists ()
  (let ((directory '(:up "Corpora" "English" "nouns"))
        (dictionary (loop for pos-tag in *coca-noun-pos-tags*
                          collect (list pos-tag nil))))
    (with-open-file (input-stream *coca-bigrams*)
      (loop for line = (read-line input-stream nil nil)
            while line do (let* ((strings (rest (split-sequence::split-sequence #\Tab line)))
                                 (pair-1 (list (third strings) (first strings)))
                                 (pair-2 (list (fourth strings) (second strings))))
                            (dolist (pair (list pair-1 pair-2))
                              (let ((entry (assoc (first pair) dictionary :test #'string=))
                                    (word (second pair)))
                                (when (and entry (not (find word (second entry) :test #'string=)))
                                  (pushnew word (second entry))))))))
    (loop for entry in dictionary
          do (with-open-file (out (babel-pathname :directory '(:up "Corpora" "English" "nouns") ;;directory
                                                  :name (first entry)
                                                  :type "txt")
                                  :direction :output :if-exists :supersede)
               (dolist (word (sort-alphabetically (second entry) :key #'identity))
                 (format out "~%~a" word))))))
|#

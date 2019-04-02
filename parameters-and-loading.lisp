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

;;;;;; Make and store an English lexicon image
;;;;;; ----------------------------------------------------------------
(defun make-english-lexicon-image ()  
  (fcg::store-grammar *fcg-constructions*
                      :directory '(:up "Corpora" "English")
                      :name "english-lexicon-image"
                      :type "lisp"))
;; (make-english-lexicon-image)

#|

;;;;;; Loading
;;;;;; ----------------------------------------------------------------
(defun load-preposition (name)
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "prepositions")
                        :name name
                        :type "lisp")))

(defun load-all-prepositions ()
  (dolist (preposition (append *prepositions-other* *prepositions-spatiotemporal*))
    (ignore-errors (load-preposition preposition))))
;; (load-all-prepositions)

(defun compile-all-verbs ()
  (dolist (verb *all-main-verbs*)
    (let ((filename (if (eql 'quiz verb) "kwiz" ;; Necessary because of bug in SmartSVN.
                      (downcase (symbol-name verb)))))
      (compile-file (merge-pathnames *verb-pathname*
                                     (babel-pathname :name filename
                                                     :type "lisp"))))))
;; (compile-all-verbs)

(defun load-verb (name)
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "verbs")
                        :name name
                        :type "lisp")))
;; (load-verb "break")

(defun load-all-main-verbs ()
  (dolist (verb *all-main-verbs*)
    (let ((filename (if (eql 'quiz verb) "kwiz"
                      (downcase (symbol-name verb)))))
      (ignore-errors (load-verb filename)))))
;; (load-all-main-verbs)

(defun load-adverb (name)
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "adverbs")
                        :name name
                        :type "lisp")))

(defun load-all-adverbs ()
  (dolist (item *adverbs*)
    (load-adverb (downcase item))))
;; (load-all-adverbs)

;;;;;; Nominal
;;;;;; ----------------------------------------------------------------
(defun load-noun (name)
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "nouns")
                        :name name
                        :type "lisp")))

(defun load-all-nouns ()
  (dolist (noun (append *common-nouns* *deviating-nouns*))
    (ignore-errors (load-noun (downcase noun)))))
;; (load-all-nouns)

(defun load-nationality (name)
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "nationalities")
                        :name name
                        :type "lisp")))

(defun load-all-nationalities ()
  (dolist (name *nationalities*)
    (ignore-errors (load-nationality (last-elt name)))))
;; (load-all-nationalities)

(defun load-all-adjectives ()
  (dolist (adjective *adjectives*)
    (when (and (= 2 (length adjective))
               (not (find (downcase (second adjective)) *numerals* :test #'string= :key #'downcase)))
      (ignore-errors (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "adjectives")
                                           :name (second adjective)
                                           :type "lisp"))))))
;;(load-all-adjectives)

(defun load-adjective (name)
   (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "adjectives")
                         :name name
                         :type "lisp")))

(defun load-auxiliaries ()
  (dolist (filename '("be" "do" "have" "get" "modals"))
    (ignore-errors (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "auxiliaries")
                                         :name filename
                                         :type "lisp")))))
;; (load-auxiliaries)

(defun load-modals ()
  (ignore-errors (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "auxiliaries")
                                       :name "modals"
                                       :type "lisp"))))
;; (load-modals)

(defun load-personal-pronouns ()
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "pronouns")
                        :name "personal-pronouns"
                        :type "lisp")))
;; (load-personal-pronouns)

(defun load-relative-pronouns ()
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "pronouns")
                        :name "relative-pronouns"
                        :type "lisp")))
;; (load-relative-pronouns)

(defun load-possessive-pronouns ()
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "pronouns")
                        :name "possessive-pronouns"
                        :type "lisp")))

(defun load-pronouns ()
  (load-personal-pronouns)
  (load-relative-pronouns)
  (load-possessive-pronouns))

(defun load-clause-connectors ()
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon")
                        :name "clause-connectors"
                        :type "lisp")))

(defun load-predicating-expressions ()
  (load (babel-pathname :directory '(:up "grammars" "English" "grammar")
                        :name "predicating-expressions"
                        :type "lisp")))
;; (load-predicating-expressions)

(defun load-referring-expressions ()
  (load (babel-pathname :directory '(:up "grammars" "English" "grammar")
                        :name "referring-expressions"
                        :type "lisp")))
;; (load-referring-expressions)

(defun load-negation ()
  (load (babel-pathname :directory '(:up "grammars" "English" "grammar")
                        :name "negation"
                        :type "lisp")))
;; (load-negation)

(defun load-word-formation ()
  (load (babel-pathname :directory '(:up "grammars" "English" "grammar")
                        :name "word-formation"
                        :type "lisp")))
;; (load-word-formation)

(defun load-determiners ()
  (load (babel-pathname :directory '(:up "grammars" "English" "lexicon" "function-words")
                        :name "determiners"
                        :type "lisp")))
;; (load-determiners)

(defun load-valence-and-arg-and-inf-structure-cxns ()
  (load (babel-pathname :directory '(:up "grammars" "English" "grammar")
                        :name "valence-arg-and-inf-structure"
                        :type "lisp")))
;; (load-valence-and-arg-and-inf-structure-cxns)

(defun make-files-for-grammar  (&key (reduced nil))
  ""
  (if reduced
    (progn
      ;; Nouns
      (write-common-noun-definitions)
      (write-deviating-common-noun-definitions)
      (write-always-plural-noun-definitions)
      (format t "~%Nouns written...")
      ;; Verbs
      (write-all-verb-definitions)
      (format t "~%Verbs written..."))
    (progn
      (make-words)
      (write-nouns) (format t "~%Nouns written...")
      (write-verbs) (format t "~%Verbs written...")))

  (write-prepositions) (format t "~%Prepositions written...")
  (write-adjectives) (format t "~%Adjectives written...")
  (write-nationalities) (format t "~%Nationalities written...")
  (write-adverbs)  (format t "~%Adverbs written...~% DONE~%"))
;; (make-files-for-grammar)
 
(defun make-english-grammar-image ()
  (fcg::store-grammar *fcg-constructions*
                      :directory '(:up "Corpora" "English")
                      :name "english-grammar-image"
                      :type "lisp"))

;; (make-english-grammar-image)

(defun load-lexicon ()
  (load-all-nouns)
  (load-all-main-verbs)
  (load-auxiliaries)
  (load-modals)
  (load-determiners)
  (load-all-adjectives)
  (load-all-prepositions)
  (load-all-adverbs)
  (load-all-nationalities)
  (load-clause-connectors))

(defun load-all-constructions (&key (reduced t))
  ;; Lexical and morphological constructions
  (load-lexicon :reduced reduced)
   
  ;; Load grammar
  (load-negation)
  (load-word-formation)
  (load-referring-expressions)
  (load-predicating-expressions)
  (load-valence-and-arg-and-inf-structure-cxns))
;; (load-all-constructions)


|#
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

;;;;;; Loading utilities.
;;;;;; ----------------------------------------------------------------
(defun lex-directory (lex-class)
  (remove-if-not #'(lambda(p)
                     (string= "lisp" (pathname-type p)))
                 (directory (beng-pathname :directory (list "lexicon" lex-class)))))

(defun load-lexical-constructions (lex-class)
  (loop for path in (lex-directory lex-class)
        do (load path)))

(defun load-lexicon (&optional (lex-classes '("adjectives"
                                              "adverbs"
                                              "nationalities"
                                              "nouns"
                                              "prepositions"
                                              "verbs")))
  (loop for lex-class in lex-classes
        do (load-lexical-constructions lex-class)))

(defun load-adjective (word)
  (load (beng-pathname :directory '("lexicon" "adjectives")
                       :name word
                       :type "lisp")))

(defun load-adverb (word)
  (load (beng-pathname :directory '("lexicon" "adverbs")
                       :name word
                       :type "lisp")))

(defun load-nationality (word)
  (load (beng-pathname :directory '("lexicon" "nationalities")
                       :name word
                       :type "lisp")))

(defun load-noun (word)
  (load (beng-pathname :directory '("lexicon" "nouns")
                       :name word
                       :type "lisp")))

(defun load-preposition (word)
  (load (beng-pathname :directory '("lexicon" "prepositions")
                       :name word
                       :type "lisp")))

(defun load-verbs (word)
  (load (beng-pathname :directory '("lexicon" "verbs")
                       :name word
                       :type "lisp")))

;;;;;; Writing the lexicon.
;;;;;; ----------------------------------------------------------------
(defun write-lexicon ()
  (write-adjectives)
  (write-adverbs)
  (write-nationalities)
  (write-nouns)
  (write-prepositions)
  (write-verbs))
;; (write-lexicon)


;;;;;; Build a grammar.
;;;;;; ----------------------------------------------------------------
(defun build-grammar (&key (write-files? t))
  "For writing and loading constructions. Only use after instantiating a construction inventory."
  (when write-files?
    (format t "~%Writing constructional definitions (this may take a few seconds)...")
    (write-lexicon))
  (load-lexicon))

;;;;;; Make and store an English lexicon image
;;;;;; ----------------------------------------------------------------
(defun save-beng (grammar &key (directory '("models"))
                               (name "beng")
                               (type "lisp"))
  "Stores the grammar 'grammar' in path 'file-path'"
  (let ((path (beng-pathname :directory directory :name name :type type)))
    (cl-store:store grammar path)))
;; (save-beng *fcg-constructions* :name "beng-only-lexicon")

(defun restore-beng (&key (name "beng") (directory '("models")) (type "lisp"))
  "Loads and returns the grammar 'grammar' in path 'file-path'"
  (let ((path (beng-pathname :directory directory :name name :type type)))
    (cl-store:restore path)))
;; (restore-beng :name "beng")

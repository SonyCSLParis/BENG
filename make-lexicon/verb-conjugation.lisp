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

(defgeneric conjugate-verb (verb-id verb-class))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-1)))
  (let* ((string (downcase verb-id)))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            string
            string)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-2)))
  (let* ((string  (downcase verb-id))
         (strong-form (string-append (string-right-trim '(#\d) string) "t")))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            strong-form strong-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-clas (eql 'class-3)))
  (let* ((string (downcase verb-id))
         (strong-form (string-replace string "come" "came")))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            strong-form strong-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-4)))
  (let* ((string (downcase verb-id))
         (strong-form (cond
                       ((cl-ppcre:scan ".+[ee|ea][dt]" string)
                        (string-append (subseq string 0 (- (length string) 2)) (subseq string (- (length string) 1) (length string))))
                       ((cl-ppcre:scan ".+ind$" string) (string-replace string "ind" "ound"))
                       ((cl-ppcre:scan ".+hold$" string) (string-replace string "hold" "held"))
                       ((cl-ppcre:scan ".+stand$" string) (string-replace string "stand" "stood"))
                       (t
                        (string-replace string "i" "u")))))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            strong-form strong-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-5)))
  (let* ((string (downcase verb-id))
         (past-tense-form (string-replace string "i" "a"))
         (past-participle-form (string-replace string "i" "u")))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            past-tense-form
            past-participle-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-6)))
  (let* ((string (downcase verb-id))
         (strong-form (string-append (string-replace string "ee" "e") "t")))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            strong-form strong-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-7)))
  (let* ((string (downcase verb-id))
         (strong-forms (cond
                        ((cl-ppcre:scan ".+ake$" string) (list (string-replace string "ake" "ook") (string-append string "n")))
                        ((cl-ppcre:scan ".+w$" string) (list (string-append (subseq string 0 (- (length string) 2)) "ew")
                                                             (string-append string "n")))
                        ((cl-ppcre:scan ".+all$" string) (list (string-replace string "all" "ell") (string-append string "en")))
                        ((cl-ppcre:scan ".+id$" string) (list (string-replace string "id" "ade") (string-append string "den")))
                        (t
                         (list (string-replace string "give" "gave") (string-append string "n"))))))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            (first strong-forms) (second strong-forms))))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-8)))
  (let* ((string (downcase verb-id))
         (past-tense-form (string-append (string-replace string "ea" "o") "e"))
         (past-participle-form (if (cl-ppcre:scan ".+ear$" string)
                                 (string-replace string "ear" "orn")
                                 (string-append past-tense-form "n"))))
    (values string 
            (english-make-third-s-form string)
            (english-make-ing-form string)
            past-tense-form past-participle-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-9)))
  (let* ((string (downcase verb-id))
         (past-tense-form (string-replace string "i" "o"))
         (past-participle-form (if (cl-ppcre:scan ".+[dt]e$" string)
                                 (string-append (string-right-trim '(#\e) string) (subseq string (- (length string) 2) (- (length string) 1)) "en")
                                 (string-append string "n"))))
    (values string 
            (english-make-third-s-form string)
            (english-make-ing-form string)
            past-tense-form past-participle-form)))

(defmethod conjugate-verb ((verb-id symbol) (verb-class (eql 'class-10)))
  (let* ((string (downcase verb-id))
         (strong-form (string-replace string "eave" "eft")))
    (values string
            (english-make-third-s-form string)
            (english-make-ing-form string)
            strong-form strong-form)))

(defmethod conjugate-verb ((verb-spec list) (verb-class t))
  (let* ((strings (mapcar #'(lambda(x) (downcase (symbol-name x))) verb-spec))
         (base-form (first strings)))
    (values base-form (english-make-third-s-form base-form) (english-make-ing-form base-form) (second strings) (third strings))))

(defmethod conjugate-verb ((verb-id symbol) (verb-class t))
  (let* ((string (downcase verb-id))
         (ed-form (english-make-ed-form string)))
    (values string (english-make-third-s-form string) (english-make-ing-form string) ed-form ed-form)))
            
(defun english-make-ed-form (string)
  "Infer the ed-form from the base form."
  (cond ((cl-ppcre:scan ".+[y]$" string)
         (string-append (string-right-trim '(#\y) string) "ied"))
        ((and (cl-ppcre:scan ".*[^aeiod][aeiou][^aeiouhjwxy]$" string)
              (not (member string *no-doubling* :test #'string=)))
         (string-append string (subseq (reverse string) 0 1) "ed"))
        (t
         (string-append (string-right-trim '(#\e) string) "ed"))))

(defun english-make-ing-form (string)
  "Infer the ing-form from the base form."
  (cond ((cl-ppcre:scan ".+ie$" string)
         (string-append (subseq string 0 (- (length string) 2)) "ying"))
        ((and (cl-ppcre:scan ".*[^aeiod][aeiou][^aeiouhjwxy]$" string)
              (not (member string *no-doubling* :test #'string=)))
         (string-append string (subseq (reverse string) 0 1) "ing"))
        (t
         (string-append (if (cl-ppcre:scan ".+[^eE]e$" string)
                          (string-right-trim '(#\e) string)
                          string)
                        "ing"))))

(defun english-make-third-s-form (string)
  (cond ((cl-ppcre:scan ".+[^aeou][y]$" string)
         (string-append (string-right-trim '(#\y) string) "ies"))
        ((cl-ppcre:scan ".+(x|sh|h|s)$" string)
         (string-append string "es"))
        (t
         (string-append string "s"))))

(defun already-listed-as-verb-p (name) 
  (probe-file (babel-pathname :directory '("grammars" "English" "lexicon" "verbs")
                              :name name
                              :type "lisp")))

(defun english-infer-base-form-from-ed-form (ed-form)
  (when (>= (length ed-form) 4)
    (find-if #'already-listed-as-verb-p
             (list (string-append (subseq ed-form 0 (- (length ed-form) 3)) "y")
                   (subseq ed-form 0 (- (length ed-form) 3))
                   (subseq ed-form 0 (- (length ed-form) 2))
                   (string-append (subseq ed-form 0 (- (length ed-form) 2)) "e")))))         

(defun english-infer-base-form-from-ing-form (ing-form)
  (when (>= (length ing-form) 5)
    (find-if #'already-listed-as-verb-p
             (list (subseq ing-form 0 (- (length ing-form) 3))
                   (string-append (subseq ing-form 0 (- (length ing-form) 4)) "ie")
                   (subseq ing-form 0 (- (length ing-form) 4))
                   (string-append (subseq ing-form 0 (- (length ing-form) 3)) "e")))))
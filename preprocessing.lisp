;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :nlp-tools)

(defparameter *penelope-host* "http://spacy.fcg-net.org")

(export '(run-displacy run-displacy-ents get-beng-sentence-analysis))

(defun run-beng-parser (sentence &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-dependency-parser> expects a string as input"))
  (send-request "/beng"
                (encode-json-to-string `((:sentence . ,(remove-multiple-spaces sentence))
                                         (:model . ,model)))))

(defun run-displacy (sentence &key (model "en"))
  (unless (stringp sentence)
    (error "The function <run-penelope-dependency-parser> expects a string as input"))
  (let* ((url (string-append *penelope-host* "/displacy"))
         (json (encode-json-to-string `((:sentence . ,(remove-multiple-spaces sentence))
                                        (:model . ,model))))
         (response (exec-and-return "curl" url "-H"
                                    #+lispworks (format nil "~s" "Content-Type: application/json")
                                    #-lispworks (format nil "~a" "Content-Type: application/json")
                                    "-s"   "-d "  json ))) ;; remove single quotes for non lispworks
    (push (string-append "<add-element>"
                         (cl-json:decode-json-from-string (first response))
                         "</add-element>")
          wi::*requests*)
    nil))

(defun run-displacy-ents (sentence &key (model "en"))
  (unless (stringp sentence)
    (error "The function <run-penelope-dependency-parser> expects a string as input"))
  (let* ((url (string-append *penelope-host* "/displacy-ents"))
         (json (encode-json-to-string `((:sentence . ,(remove-multiple-spaces sentence))
                                                  (:model . ,model))))
         (response (exec-and-return "curl" url "-H"
                                    #+lispworks (format nil "~s" "Content-Type: application/json")
                                    #-lispworks (format nil "~a" "Content-Type: application/json")
                                    "-s"   "-d "  json ))) ;; remove single quotes for non lispworks
    (push (string-append "<add-element>"
                         (cl-json:decode-json-from-string (first response))
                         "</add-element>")
          wi::*requests*)
    nil))

(defun convert-ica-string-to-ica-list (string)
  "Given a string from benepar, translate this into a list representation."
  (loop for pair in '(("." "\\.")
                      ("," "\\,")
                      ("''" "PARENTH")
                      ("``" "PARENTH")
                      ("\"" "PARENTH"))
        do (setf string (string-replace string (first pair) (second pair))))
  (read-from-string string))

;;;   
;;;   (read-from-string (apply #'string-append
;;;                            (loop for s in (split-sequence:split-sequence #\Space string)
;;;                                  collect (cond ((ppcre::regex-replace "(.") "(\\. ")
;;;                                                ((string= s "(,") "(\\, ")
;;;                                                ((string= s "(''") "(PARENTH ")
;;;                                                ((string= s "((''") "((PARENTH ")
;;;                                                ((string= s "(``") "(PARENTH ")
;;;                                                ((string= s "(``") "((PARENTH ")
;;;                                                ((string= s "\")") "PARENTH)")
;;;                                                ((string= s "\"))") "PARENTH))")
;;;                                                ((member (subseq s 0 1) '("." ",") :test #'string=)
;;;                                                 (format nil "\\~a" s))
;;;                                                (t
;;;                                                 (format nil "~a " s)))))))

(defun get-beng-sentence-analysis (sentence &key (model "en")) ;; To do: allow sentence ID.
  (let* ((analysis (run-beng-parser (format nil "~a" sentence) :model model))
         (dependency-tree (rest (assoc :tree (first (rest (assoc :beng analysis))))))
         (constituent-tree (convert-ica-string-to-ica-list (second (assoc :ica (second (first analysis)))))))
    (values dependency-tree constituent-tree)))
;; (run-beng-parser "this is a test")
;; (get-beng-sentence-analysis "Once upon a time, there was a cat named  Cotton.")

(in-package :beng)

(defun make-units (lst)
  (if (symbolp (second lst))
    (cons `(,(first lst)
            (constituents ,(rest lst)))
          (loop for e in (rest lst)
                collect `(,e)))     
    (let ((unit-name (first lst))
          (constituents (loop for c in (rest lst)
                              collect (if (listp c)
                                        (cons (make-const (first c)) (rest c))
                                        (make-const c)))))
      (if (listp (first constituents))
        (cons `(,unit-name
                (constituents ,(mapcar #'first constituents)))
              (loop for constituent in constituents
                    append (make-units constituent)))))))


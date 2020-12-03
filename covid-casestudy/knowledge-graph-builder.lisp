

(in-package :beng)

(let ((beng-analyses nil))
  (loop for sentence in (rest (assoc :sentences (nlp-tools::run-penelope-sentence-tokenizer *demo-text*)))
        collect (comprehend sentence)))


(in-package :nlp-tools)


(defparameter *test-text* (rest (assoc :body-text
                                       (rest (assoc :fields (first
                                                             (decode-json-from-source (babel-pathname :directory '(:up "COVID-19" "guardian")
                                                                                                      :name "2020-01-03"
                                                                                                      :type "json"))))))))

(defparameter *transient-structures*
  (loop for sentence in (rest (assoc :sentences (run-penelope-sentence-tokenizer *test-text*)))
        collect (beng::de-render sentence :beng-spacy-benepar)))

(in-package :beng)

(deactivate-monitor trace-fcg)

(add-element (make-html-fcg-light (first nlp-tools::*transient-structures*) :construction-inventory *fcg-constructions*))

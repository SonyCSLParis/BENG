
(ql:quickload :beng)

(in-package :beng)

(export '(categories))
;; (make-beng-cxns)
(defun show-multidimensional-analysis (sentence)
  (multiple-value-bind (meaning resulting-node)
      (comprehend sentence)
    (add-element (make-html-fcg-light resulting-node
                                      :feature-types (feature-types *fcg-constructions*)
                                      :configuration (visualization-configuration *fcg-constructions*)
                                      :cxn-inventory *fcg-constructions*
                                      :expand-initially T))))

(show-multidimensional-analysis "At Sony, we are convinced that language is the key technology for achieving explainable AI.")
(show-multidimensional-analysis "You no longer have to choose between dependency and constituency parsing.")
(show-multidimensional-analysis "A lot of people in Britain regret having voted for leaving the EU, and are hoping for a second referendum.")

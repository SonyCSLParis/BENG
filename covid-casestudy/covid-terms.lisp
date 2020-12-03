
;; (ql:quickload :beng)

(in-package :beng)

;;;; Synonyms for COVID-19
;;;; ------------------------------------------------------------
(def-fcg-cxn COVID-19-morph
             ((?covid-unit
               (footprints (morph)))
              <-
              (?covid-unit
               (lex-id covid-19)
               (footprints (not morph))
               --
               (HASH form ((string ?covid-unit "COVID-19")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-string) :apply-fast t :lex-id covid-10
                          :string "COVID-19" :pos ("NOUN")))

(def-fcg-cxn COVID-19-small-morph
             ((?covid-unit
               (footprints (morph)))
              <-
              (?covid-unit
               (lex-id covid-19)
               (footprints (not morph))
               --
               (HASH form ((string ?covid-unit "covid-19")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-string)
                          :string "covid-19" :pos ("NOUN")))

(def-fcg-cxn coronavirus-morph
             ((?covid-unit
               (footprints (morph)))
              <-
              (?covid-unit
               (lex-id covid-19)
               (footprints (not morph))
               --
               (HASH form ((string ?covid-unit "coronavirus")))))
             :disable-automatic-footprints t
             :attributes (:label (hashed-string)
                          :string "coronavirus" :pos ("NOUN")))

(def-fcg-cxn covid-19-lex
             ((?covid-unit
               (parent ?phrase)
               (footprints (lex))
               (args (?r ?x))
               (referent ?ref)
               (syn-cat (lex-class noun)
                        (agreement (- - + -))))
              <-
              (?covid-unit
               (HASH meaning ((domain virus covid-19 ?r ?x ?ref)))
               (footprints (not lex))
               --
               (lex-id covid-19)))
             :disable-automatic-footprints t
             :attributes (:label (hashed-lex-id hashed-meaning) :apply-fast t :meaning covid-19 :lex-id covid-19 :pos ("NOUN")))

;; (comprehend "If we are in a war, we must act.")
;; (comprehend "a happy cow")
;; (comprehend "a cow")
;; (comprehend "we are in a fight")
;; (comprehend "the young man recently won ATP challenger set")
;; (comprehend "the 22-year-old recently won the tournament")


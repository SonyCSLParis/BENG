
(in-package :beng)


(def-fcg-cxn PrepNP-cxn
             ((?PrepNP-unit
               (footprints (relating-expression))
               (syn-cat (agreement ?agreement)))
              <-
              (?PrepNP-unit
               (referent ?relation)
               --
               (footprints (not relating-expression))
               (HASH form ((meets ?preposition ?NP-consituent ?PrepNP-unit)))
               (constituents (?preposition ?NP-constituent)))
              (?preposition
               (referent ?relation)
               --
               (parent ?PrepNP-unit)
               (sem-valence (trajector ?set-1)
                            (landmark ?set-2)))
              (?NP-constituent
               (parent ?PrepNP-unit)
               (referent ?set-2)
               (syn-cat (phrase-type NP)
                        (agreement ?agreement))))
             :disable-automatic-footprints t
             :attributes (:label unmarked-phrasal))

(def-fcg-cxn NP->NP-PrepNP-cxn
             ((?NP-unit
               (footprints (referring-expression)))
               <-
              (?NP-unit
               (referent ?ref)
               --
               (footprints (not referring-expression))
               (HASH form ((meets ?NP-constituent ?PrepNP-constituent ?NP-unit)))
               (constituents (?NP-constituent ?PrepNP-constituent)))
              (?NP-constituent
               (referent ?set-1)
               (syn-cat (agreement ?agreement))
               --
               (parent ?NP-unit)
               (syn-cat (phrase-type NP)))
              (?PrepNP-constituent
               (referent ?relation)
               (syn-cat (agreement ?agreement))
               --
               (parent ?NP-unit)
               (syn-cat (phrase-type PrepNP))
               (constituents (?preposition)))
              (?preposition
               (referent ?relation)
               --
               (sem-valence (trajector ?set-1)
                            (landmark ?set-2))))               
             :disable-automatic-footprints t
             :attributes (:label unmarked-phrasal :string "np-2"))
(in-package :beng)
                 
(def-fcg-cxn NP->a-b-cxn
  (nil
   <-
   (?determiner-unit
    (syn-cat
     (lex-class determiner))
    (referent ?referent)
    --
    (parent ?np-unit)
    (args (?referent ?input-105)))
   (?head-unit
    (referent ?referent)
    (syn-cat
     (agreement ?agr))
    --
    (parent ?np-unit)
    (args (?output-72 ?input-106)))
   (?np-unit
    --
    (hash form ((meets ?determiner-unit ?head-unit ?unit-6072)))
    (syn-cat
     (phrase-type np))
    (constituents (?determiner-unit ?head-unit))))
 :disable-automatic-footprints t
 :attributes (:label hashed-string))
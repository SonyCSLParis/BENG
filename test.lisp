

(in-package :beng)


(defmacro create-constructions (list-of-strings)
  `(progn
     ,@(loop for string in list-of-strings
             for meaning = (intern string)
             for name = (make-symbol (format "~a-cxn" string))
             collect `(def-fcg-cxn ,name
                                   ((?unit
                                     (referent ?x))
                                    <-
                                    (?unit
                                     (hash meaning ((,meaning ?x)))
                                     --
                                     (hash form ((string ?unit ,string)))))))))

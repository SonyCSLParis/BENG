;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

(in-package :beng)

(defun beng-get-named-entities (utterance-or-structure)
  (if (stringp utterance-or-structure)
    (get-penelope-named-entities utterance-or-structure)
    (ignore-errors (get-data (blackboard utterance-or-structure) :named-entities))))
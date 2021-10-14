;;; Copyright (C) 2019-present  Sony Computer Science Laboratories Paris
;;;                             Remi van Trijp (www.remivantrijp.eu)
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

(in-package :beng)

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; Priority Scores
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun compute-english-base-priority (cip-node)
  "Compute the base priority, which is the score of the parent or 0.0 for the initial node."
  (let ((parent (parent cip-node)))
    (if parent (+ 1 (priority parent)) 0.0)))

(defmethod cip-priority ((node cip-node) (mode (eql :english-grammar-priority)))
  ;; This cip-priority mode ranks hypotheses based on the principles of "Minimize Domains" (Rijkhoff, 1992; also
  ;; see Hawkins, 2004 and Ted Gibson's Dependency Locality Theory) and on semantic coherence. Other potential
  ;; preferences may include iconicity, etc., but are currently not considered.
  ;; For individual words, it uses information from syntaxnet.
  (let ((base-priority (compute-english-base-priority node)))
    ;; In formulation, we do not take a special priority mode into account and we revert to depth-first.
    (if (or (eq (direction (cip node)) '->) (null (all-parents node)))
      base-priority
      ;; In parsing, we check whether there was ambiguity that needs to be resolved.
      (let* ((applied-cxn (car-applied-cxn (cipn-car node)))
             (pos (attr-val applied-cxn :pos)))
        (if pos
          (let ((postagger-tags (get-data (car-source-cfs (cipn-car node)) :postagger-results))
                (bindings (car-second-merge-bindings (cipn-car node))))
            (or (loop for binding in bindings
                      for match = (assoc (rest binding) postagger-tags)
                      when match ;;this is a node with a split in POS tags!
                      return (+ base-priority (if (intersection pos (rest match) :test #'equal)
                                                0.9
                                                0.1)))
                base-priority)) ;; safety net
          (cip-priority node :best-first-minimize-boundary-distance-and-maximize-semantic-coherence))))))

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; Hash methods
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod hash ((construction construction)
                 (mode (eql :hash-english-grammar))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning)
            (attr-val construction :lex-id))
    (remove-duplicates
     (remove nil (list (attr-val construction :string)
                       (attr-val construction :meaning)
                       (attr-val construction :lex-id))))))

(defun hash-lex-id (node)
  "We extract the values of the feature lex-id for each unit."
  (loop for unit in (fcg-get-transient-unit-structure node)
        for lex-id = (or (second (assoc 'sem-frame (unit-body unit) :test #'string=))
                         (second (assoc 'lex-id (unit-body unit) :test #'string=))) ;; lex-id should disappear from construction.
        when lex-id
        collect it))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-english-grammar))
                 &key (label t))
  "Checks the root and returns information for fast processing."
  (cond ((string= label "HASHED-LEX-ID") (hash-lex-id node))
        (t (let ((root (get-root (fcg-get-transient-unit-structure node))))
             (if (eql (fcg-get-direction node) '<-)
               ;; In parsing we are interested in strings
               (append (mapcar #'third (extract-strings (list root)))
                       (loop for unit in (fcg-get-transient-unit-structure node)
                             for accessor = (assoc 'fcg::node-accessor (unit-body unit))
                             when accessor
                             collect (second accessor)))
               ;; In production we are interested in meaninsg
               (loop for meaning in (extract-meaning root)
                     when (beng::english-extract-label meaning)
                     collect it))))))

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; Cxn-suppliers
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defclass cxn-supplier-for-english-grammar
          (cxn-supplier-with-hash+ordered-labels)
  ()
  (:documentation
   "A construction pool that applies constructions of
    different labels by a pre-specified order and supports hashing and ambiguity management"))

(defun english-all-constructions-of-label-hashed (node label)
  "returns all constructions of label 'label' and ordered per string"
  (let* ((hashed-constructions
          (loop for hash in (remove-duplicates (hash node (get-configuration node :hash-mode)
                                                      :label label) :test #'equalp)
                append (gethash hash (constructions-hash-table (construction-inventory node)))))
         (others
          (remove-if #'(lambda(x)
                         (member x hashed-constructions :test #'equal))
                     (gethash nil (constructions-hash-table (construction-inventory node))))))
    (loop for cxn in (append hashed-constructions others)
          for cxn-label = (attr-val cxn :label)
          when (or (and (listp cxn-label) (find label cxn-label :test #'string=))
                   (and (symbolp cxn-label) (string= label cxn-label)))
          collect cxn)))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-english-grammar)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-for-english-grammar
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((the-labels (get-configuration (construction-inventory (cip node))
                                           (if (eq (direction (cip node)) '->)
                                             :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-for-english-grammar
         :current-label (car the-labels)
         :remaining-labels (cdr the-labels)
         :all-constructions-of-current-label
         (english-all-constructions-of-label-hashed node (car the-labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-for-english-grammar)
                     (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. return all the next ones with the same :string or :lex-id
         (let ((first-cxn (pop (remaining-constructions cxn-supplier))))
           (if (eq (direction (cip node)) '<-)
             (let* ((keys (cons-if (attr-val first-cxn :string) (cons-if (attr-val first-cxn :lex-id) nil)))
                    (next-constructions
                     (cons first-cxn
                           (loop for key in keys
                                 append (loop for cxn in (remaining-constructions cxn-supplier)
                                              for cxn-keys = (cons-if (attr-val cxn :string)
                                                                      (cons-if (attr-val cxn :lex-id) nil))
                                              when (find key cxn-keys :test #'equalp)
                                              collect cxn)))))
               ;;now we need to remove the next-constructions from the list of remaining constructions
               (setf (remaining-constructions cxn-supplier)
                     (loop for cxn in (remaining-constructions cxn-supplier)
                           unless (find (name cxn) next-constructions :key #'name :test #'equalp)
                           collect cxn))
               ;;return next constructions:
               (remove-duplicates next-constructions :key #'name :test #'equalp))
             (list first-cxn))))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         
         (setf (current-label cxn-supplier) (first (remaining-labels cxn-supplier))
               (remaining-labels cxn-supplier) (rest (remaining-labels cxn-supplier))
               (all-constructions-of-current-label cxn-supplier)
               (english-all-constructions-of-label-hashed node (current-label cxn-supplier))
               (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; Expand Cip-node
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod expand-cip-node ((node cip-node) (mode (eql :english-expand-cip-node)))
  (loop 
   with nodes-to-queue = nil
   with failed-nodes = nil
   with cxn-inventory = (construction-inventory node)
   for cxns = (next-cxn (cxn-supplier node) node)
   when cxns
   do (let ((succeeded-cars nil)
            (failed-cars nil))
        (dolist (cxn cxns)
          (multiple-value-bind (these-succeeded-cars these-failed-cars)
              (fcg-apply (fcg::safe-cxn cxn (applied-constructions node))
                         (car-resulting-cfs (cipn-car node))
                         (direction (cip node)) :notify nil
                         :configuration (configuration (construction-inventory node))
                         :cxn-inventory cxn-inventory)
            
            (setf succeeded-cars (append succeeded-cars these-succeeded-cars)
                  failed-cars (append failed-cars these-failed-cars))))
            
        (loop for car in succeeded-cars
              do (push (cip-add-child node car)
                       nodes-to-queue))
        
        (loop for car in failed-cars
              do (push (cip-add-child node car :cxn-applied nil)
                       failed-nodes)))
   when nodes-to-queue do (return nodes-to-queue)
   while cxns
   finally (setf (fully-expanded? node) t)))
;; (comprehend "cat mat")

;;;    ;; For meta-layer
;;;    (when (and (get-configuration cip :use-meta-layer)
;;;               (diagnostics node))
;;;      (multiple-value-bind (new-problems new-fixes)
;;;          (notify-learning node :trigger 'new-node)
;;;        (when new-problems
;;;          (loop for problem in new-problems
;;;                do (push (type-of problem) (statuses node)))
;;;          (push 'diagnostic-triggered (statuses node)))
;;;        ;; Loop through the new-fixes (they should have a list of construction-application-results in
;;;        ;; their data-field 'fixed-cars), make nodes of them, add them as children, and enqueue them
;;;        ;; Note: fixes don't need to have this field, they may also directly affect the CIP
;;;        (loop for fix in new-fixes ;;
;;;              when (field? fix 'fixed-cars)
;;;              do (loop for fixed-car in (get-data fix 'fixed-cars)
;;;                       do
;;;                       (let ((fixed-child (cip-add-child node fixed-car)))
;;;                         (push (type-of (issued-by fix)) (statuses fixed-child))
;;;                         (push 'added-by-repair (statuses fixed-child))
;;;                         (cip-enqueue fixed-child cip queue-mode))))))

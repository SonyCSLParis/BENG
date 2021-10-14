;;; Copyright (C) Sony Computer Science Laboratories Paris
;;;               Author: Remi van Trijp (www.remivantrijp.eu)
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

;;; ----------------------------------------------------------------------------
;;; Helper Macros
;;; ----------------------------------------------------------------------------

(defmacro def-frame-supporting-html-method (frame-class frame-elements)

  `(defmethod make-html-for-entity-details ((frame ,frame-class)
                                            &key &allow-other-keys)
     `(((table)
        ((tr)
         ((td) ((b) "Frame Type"))
         ((td) ,(frame-type frame)))
        ((tr)
         ((td) ((b) "Frame Elements"))
         ((td) ,@(loop for element in ',frame-elements
                       for value = (second (assoc element (frame-elements frame)))
                       collect `((tr)
                                 ((td) ((b) ((em) ,(format nil "~@(~a~):" element))))
                                 ((td) ((em) ,value))))))
        ((tr)
         ((td) ((b) "Evoked by"))
         ((td) ,(evoked-by frame)))))))


;;; ----------------------------------------------------------------------------
;;; Semantic Frame Classes and Subclasses
;;; ----------------------------------------------------------------------------

(defclass semantic-frame (irl::entity)
  ((frame-type :initarg :frame-type :accessor frame-type
               :documentation "Contains the FRAME-TYPE predicate (e.g. CAUSE).")
   (frame-elements :initarg :frame-elements :accessor frame-elements
                   :documentation "Contains the roles of a frame (e.g. CAUSER and EFFECT).")
   (evoked-by :initarg :evoked-by :accessor evoked-by))
  (:documentation "A base class for storing semantic frames and visualizing them in the web interface."))

(defmacro def-sem-frame (name frame-elements)
  `(progn
     ;; Define a class for the semantic frame:
     (defclass ,name (semantic-frame) ())
     ;; Define an initialization method:
     (defmethod initialize-instance :after ((sem-frame ,name)
                                            &key &allow-other-keys)
       (setf (frame-type sem-frame) ',name))
     ;; Define web interface methods:
     (def-frame-supporting-html-method ,name ,frame-elements)))

;;; ----------------------------------------------------------------------------
;;; Library of semantic frames
;;; ----------------------------------------------------------------------------

(def-sem-frame cause-frame (causer effect))
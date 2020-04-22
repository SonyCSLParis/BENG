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

(in-package :web-interface)

(export '(add-html))

;;; (defun add-html (html-as-string)
;;;   "If you already have your string formatted as html, add it directly to the web interface."
;;;   (push (string-append "<add-element" html-as-string "</add-element>")
;;;         wi::*requests*)
;;;   nil)

(defun add-html (html-as-string)
  (push (string-append "<add-element" html-as-string "</add-element>")
        wi::*requests*))



  
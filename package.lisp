
(in-package :common-lisp-user)

(defpackage :beng
  (:use :cl-user
	:common-lisp
        :utils
	:test-framework
	:fcg :nlp-tools
        #+:hunchentoot-available-on-this-platform :web-interface
        :monitors
        :meta-layer-learning
        :network
        :type-hierarchies))
(defpackage :opts-asd
  (:use :cl :asdf))

(in-package :opts-asd)

(defsystem :opts
  :name "opts"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CLI options parsing for Common Lisp."
  :serial t
  :components ((:file "opts")))

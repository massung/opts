;;;; CLI options parsing for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :opts
  (:use :cl)
  (:export
   #:parse-opts
   #:next-arg
   #:write-help))

(in-package :opts)

;;; ----------------------------------------------------

(defparameter *argv* nil)

;;; ----------------------------------------------------

(defun next-arg (&key read)
  "Return the next command line argument."
  (let ((arg (pop *argv*)))
    (cond ((null read) arg)

          ;; use the standard reader
          ((eq read t) (let ((*read-eval* nil))
                         (with-input-from-string (s arg)
                           (read s))))

          ;; use a custom read function
          (t (funcall read arg)))))

;;; ----------------------------------------------------

(defun parse-opts (args opts)
  "Parse a command line arguments using an associative list of options."
  (let ((*argv* (coerce args 'list)))
    (loop
       for arg = (next-arg)
       for opt = (apply-arg arg opts)

       ;; if not found, put the argument back
       unless (or (null arg) opt)
       do (push arg *argv*)

       ;; stop at the first unknown argument
       while opt
       collect opt
       into parsed-opts

       ;; parsed results and unparsed arguments
       finally (return (values parsed-opts *argv*)))))

;;; ----------------------------------------------------

(defun apply-arg (arg opts)
  "Lookup an argument and apply the option if found."
  (flet ((match-flag (arg flags)
           (member arg flags :test #'equal)))
    (let ((opt (find arg opts :key #'second :test #'match-flag)))
      (when opt
        (destructuring-bind (name _ &rest args &key action &allow-other-keys)
            opt
          (declare (ignore _))
          (list name (if (null action)
                         t
                       (apply action :allow-other-keys t args))))))))

;;; ----------------------------------------------------

(defun write-help (opts &optional (stream *standard-output*))
  "Output options to a stream."
  (dolist (opt opts)
    (destructuring-bind (_ flags &key help &allow-other-keys)
        opt
      (declare (ignore _))
      (format stream "  ~24@<~{~a~^  ~}~>~@[~a~]~%" flags help))))

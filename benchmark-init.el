;;; benchmark-init.el --- Benchmarks Emacs require and load calls

;; Copyright (C) 2013 Steve Purcell
;; Copyright (C) 2013-2014 David Holm

;; Author: Steve Purcell
;; Maintainer: David Holm <dholmster@gmail.com>
;; Created: 25 Apr 2013
;; Keywords: benchmark

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple benchmark of calls to Emacs require and load functions.
;; It can be used to keep track of where time is being spent during Emacs
;; startup in order to optimize startup times.
;; The code is based on init-benchmarking.el by Steve Purcell.

;;; Installation:

;; Place this file in your load path and add the following code to the
;; beginning of your Emacs initialization script:

;; (require 'benchmark-init)

;; Data collection will begin as soon as benchmark-init is loaded.

;;; Usage:

;; After Emacs has finished loading the following function will bring up
;; the results:
;;
;;  - benchmark-init/show-durations
;;
;; Data collection can be controlled using the following two functions:
;;
;;  - benchmark-init/activate
;;  - benchmark-init/deactivate

;;; Code:

(require 'cl-lib)

(defconst benchmark-init/buffer-name "*Benchmark Init*"
  "Name of benchmark-init list buffer.")

(defconst benchmark-init/list-format
  [("Module" 65 t)
   ("Type" 7 t)
   ("ms" 7 (lambda (a b) (< (string-to-number (aref (cadr a) 2))
                            (string-to-number (aref (cadr b) 2))))
    :right-align t)]
  "Benchmark list format.")

(defconst benchmark-init/list-sort-key
  '("ms" . t)
  "Benchmark list sort key.")

(cl-defstruct benchmark-init/node
  "Tree node structure.

Slots:
`name` Entry name.
`type` Entry type, such as 'require or 'load.
`duration` Duration in milliseconds.
`children` Nodes loaded by this one."
  name type duration children)

(defvar benchmark-init/durations-tree (make-benchmark-init/node
                                       :name 'benchmark-init/root
                                       :type nil
                                       :duration -1
                                       :children nil)
  "Recorded durations stored in a tree.")

(defvar benchmark-init/current-node benchmark-init/durations-tree
  "Current node in durations tree.")

;; Tabulated presentation mode

(define-derived-mode benchmark-init/tabulated-mode tabulated-list-mode
  "Benchmark Init Tabulated"
  "Mode for displaying benchmark-init results in a table."
  (setq tabulated-list-format benchmark-init/list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key benchmark-init/list-sort-key)
  (tabulated-list-init-header))

(defun benchmark-init/list-entries ()
  "Generate benchmark-init list entries from HASH-TABLE."
  (let (entries)
    (mapc
     (lambda (value)
       (let ((name (cdr (assq :name value)))
             (type (symbol-name (cdr (assq :type value))))
             (duration (round (cdr (assq :duration value)))))
         (push (list name `[,name ,type ,(format "%d" duration)]) entries)))
     (cdr (benchmark-init/flatten benchmark-init/durations-tree)))
    entries))

(defun benchmark-init/show-durations-tabulated ()
  "Show the benchmark results in a sorted table."
  (interactive)
  (unless (featurep 'tabulated-list)
    (require 'tabulated-list))
  (with-current-buffer (get-buffer-create benchmark-init/buffer-name)
    (benchmark-init/tabulated-mode)
    (setq tabulated-list-entries 'benchmark-init/list-entries)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

;; Helpers

(defun benchmark-init/time-subtract-millis (b a)
  "Calculate the number of milliseconds that have elapsed between B and A."
  (* 1000.0 (float-time (time-subtract b a))))

(defun benchmark-init/flatten (node)
  "Flatten NODE into an association list."
  (let ((node-alist `((:name . ,(benchmark-init/node-name node))
                      (:type . ,(benchmark-init/node-type node))
                      (:duration . ,(benchmark-init/node-duration node))))
        (children (benchmark-init/node-children node))
        (node-list))
    (cons node-alist
          (dolist (child children node-list)
            (setq node-list
                  (append (benchmark-init/flatten child) node-list))))))

(defun benchmark-init/print-node (node depth)
  "Print NODE at DEPTH."
  (let ((padding (make-string (* 2 depth) ? ))
        (depth (+ depth 1))
        (name (benchmark-init/node-name node))
        (type (symbol-name (benchmark-init/node-type node)))
        (duration (round (benchmark-init/node-duration node)))
        (children (benchmark-init/node-children node)))
    (insert (format "%s[%s %s %dms]\n" padding name type duration))
    (dolist (child children nil)
      (benchmark-init/print-node child depth))))

(defun benchmark-init/begin-measure (name type)
  "Begin measuring NAME of TYPE."
  (let ((parent benchmark-init/current-node)
        (node (make-benchmark-init/node :name name :type type
                                        :duration (current-time)
                                        :children nil)))
    (setq benchmark-init/current-node node)
    parent))

(defun benchmark-init/end-measure (parent should-record-p)
  "Stop measuring and store to PARENT if SHOULD-RECORD-P."
  (let ((node benchmark-init/current-node)
        (duration (benchmark-init/time-subtract-millis
                   (current-time)
                   (benchmark-init/node-duration benchmark-init/current-node))))
    (when (funcall should-record-p)
      (setf (benchmark-init/node-duration node) duration)
      (push node (benchmark-init/node-children parent)))
    (setq benchmark-init/current-node parent)))

(defmacro benchmark-init/measure-around (name type inner should-record-p)
  "Save duration spent in NAME of TYPE around INNER if SHOULD-RECORD-P."
  `(let ((parent (benchmark-init/begin-measure ,name ,type)))
     (prog1
         ,inner
       (benchmark-init/end-measure parent ,should-record-p))))

(defadvice require
  (around build-require-durations (feature &optional filename noerror) activate)
  "Record the time taken to require FEATURE."
  (let ((name (symbol-name feature))
        (already-loaded (memq feature features))
        (should-record-p (lambda ()
                           (and (not already-loaded) (memq feature features)))))
    (benchmark-init/measure-around name 'require ad-do-it should-record-p)))

(defadvice load
  (around build-load-durations (file &optional noerror nomessage nosuffix
                                     must-suffix) activate)
  "Record the time taken to load FILE."
  (let ((name (abbreviate-file-name file))
        (should-record-p (lambda () t)))
    (benchmark-init/measure-around name 'load ad-do-it should-record-p)))

(defun benchmark-init/deactivate ()
  "Deactivate benchmark-init."
  (interactive)
  (ad-deactivate 'require)
  (ad-deactivate 'load))

;;;###autoload
(defun benchmark-init/activate ()
  "Activate benchmark-init and start collecting data."
  (interactive)
  (ad-activate 'require)
  (ad-activate 'load))

;; Obsolete functions

(define-obsolete-function-alias 'benchmark-init/install
  'benchmark-init/activate)

(define-obsolete-function-alias 'benchmark-init/show-durations
  'benchmark-init/show-durations-tabulated)

(provide 'benchmark-init)
;;; benchmark-init.el ends here

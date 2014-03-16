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
;; beginning of your Emacs initialization script.

;; (require 'benchmark-init)
;; (benchmark-init/install)

;;; Usage:

;; After Emacs has finished loading the following two functions can be called
;; in order to display the results.
;;
;;  - benchmark-init/show-durations
;;

;;; Code:

(require 'tabulated-list)

(defun benchmark-init/time-subtract-millis (b a)
  "Calculate the number of milliseconds that have elapsed between B and A."
  (* 1000.0 (float-time (time-subtract b a))))

(defvar benchmark-init/durations (make-hash-table :test 'equal)
  "A hash table of (MODULE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

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

(define-derived-mode benchmark-init/list-mode tabulated-list-mode
  "bencmark-init mode" "Mode for displaying benchmark-init results."
  (setq tabulated-list-format benchmark-init/list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key benchmark-init/list-sort-key)
  (tabulated-list-init-header))

(defun benchmark-init/show-durations ()
  "Show the benchmark for the specified HASH-TABLE with primary column NAME."
  (interactive)
  (with-current-buffer (get-buffer-create benchmark-init/buffer-name)
    (benchmark-init/list-mode)
    (setq tabulated-list-entries 'benchmark-init/list-entries)
    (tabulated-list-print t)
    (switch-to-buffer (current-buffer))))

(defun benchmark-init/list-entries ()
  "Generate benchmark-init list entries from HASH-TABLE."
  (let (entries)
    (maphash
     (lambda (key value)
       (let ((type (first value))
             (duration (round (second value))))
         (push (list key `[,key ,type ,(format "%d" duration)]) entries)))
     benchmark-init/durations)
    entries))

;;;###autoload
(defun benchmark-init/install ()
  "Install benchmark support in Emacs."
  (defadvice require
    (around build-require-durations (feature &optional filename noerror) activate)
    "Note in `benchmark-init/durations' the time taken to require each feature."
    (let* ((already-loaded (memq feature features))
           (require-start-time (and (not already-loaded) (current-time))))
      (prog1
          ad-do-it
        (when (and (not already-loaded) (memq feature features))
          (let ((entry (symbol-name feature))
                (duration (benchmark-init/time-subtract-millis (current-time)
                                                               require-start-time)))
            (puthash entry (list "require" duration)
                     benchmark-init/durations))))))

  (defadvice load
    (around build-load-durations (file &optional noerror nomessage nosuffix must-suffix) activate)
    "Note in `benchmark-init/durations' the time taken to load each file."
    (let* ((load-start-time (current-time)))
      (prog1
          ad-do-it
        (progn
          (let ((entry (abbreviate-file-name file))
                (duration (benchmark-init/time-subtract-millis (current-time)
                                                               load-start-time)))
            (unless (eq (gethash entry benchmark-init/durations) nil)
              (message (format "Loading %s which has already been loaded!" file)))
            (puthash entry (list "load" duration)
                     benchmark-init/durations)))))))

(provide 'benchmark-init)
;;; benchmark-init.el ends here

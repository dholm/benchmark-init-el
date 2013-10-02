;;; benchmark-init.el --- Benchmarks Emacs require and load calls

;; Copyright (C) 2013  David Holm

;; Author: David Holm <dholmster@gmail.com>
;; Keywords: benchmark

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple benchmark of calls to Emacs require and load functions.
;; It can be used to keep track of where time is being spent during Emacs
;; startup in order to optimize startup times.
;; The code was originally based on init-benchmarking.el by Steve Purcell but
;; several modification has gone into it since.

;;; Installation:

;; Place this program in your load path and add the following code to the
;; beginning of your Emacs initialization script.

;; (require 'benchmark-init)
;; (benchmark/install)

;;; Usage:

;; After Emacs has finished loading the following two functions can be called
;; in order to display the results.
;;
;;  - benchmark/show-require-times
;;  - benchmark/show-load-times
;;
;; ctable (https://github.com/kiwanami/emacs-ctable) is used to display the
;; results and must be in your Emacs load path for the functions to work.

;;; Code:


(defun benchmark/time-subtract-millis (b a)
  "Calculate the number of milliseconds that have elapsed between B and A."
  (* 1000.0 (float-time (time-subtract b a))))


(defvar benchmark/require-times (make-hash-table :test 'equal)
  "A hash table of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")


(defvar benchmark/load-times (make-hash-table :test 'equal)
  "A hash-table of (FILE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FILE.")


(defun benchmark/show-require-times ()
  "Show the benchmark for require."
  (interactive)
  (benchmark/show-times benchmark/require-times "Feature"))


(defun benchmark/show-load-times ()
  "Show the benchmark for load."
  (interactive)
  (benchmark/show-times benchmark/load-times "File"))


(defun benchmark/show-times (hash-table name)
  "Show the benchmark for the specified HASH-TABLE with primary column NAME."
  (let* ((column-model
          (list (make-ctbl:cmodel
                 :title name :align 'left
                 :sorter 'ctbl:sort-string-lessp)
                (make-ctbl:cmodel
                 :title "ms" :align 'right :min-width 7
                 :sorter (lambda (a b) (ctbl:sort-number-lessp b a)))))
         (data (let ((entries ()))
                 (maphash (lambda (key value)
                            (add-to-list 'entries (list key (round value)))) hash-table)
                 entries))
         (model
          (make-ctbl:model
           :column-model column-model :data data :sort-state '(2 1)))
         (component
          (ctbl:create-table-component-buffer
           :model model)))
    (pop-to-buffer (ctbl:cp-get-buffer component))))


;;;###autoload
(defun benchmark/install ()
  "Install benchmark support in Emacs."
  (defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
    "Note in `benchmark/require-times' the time taken to require each feature."
    (let* ((already-loaded (memq feature features))
           (require-start-time (and (not already-loaded) (current-time))))
      (prog1
          ad-do-it
        (when (and (not already-loaded) (memq feature features))
          (puthash feature (benchmark/time-subtract-millis (current-time)
                                                           require-start-time)
                   benchmark/require-times)))))

  (defadvice load
    (around build-load-times (file &optional noerror nomessage nosuffix must-suffix) activate)
    "Note in `benchmark/load-times' the time taken to load each file."
    (let* ((load-start-time (current-time)))
      (prog1
          ad-do-it
        (progn
          (unless (eq (gethash file benchmark/load-times) nil)
            (message (format "Loading %s which has already been loaded!" file)))
          (puthash file (benchmark/time-subtract-millis (current-time)
                                                        load-start-time)
                   benchmark/load-times))))))


(provide 'benchmark-init)
;;; benchmark-init.el ends here

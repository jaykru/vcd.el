;;; vcd.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jay Kruer
;;
;; Author: Jay Kruer <j@dank.systems>
;; Maintainer: Jay Kruer <j@dank.systems>
;; Created: August 05, 2023
;; Modified: August 05, 2023
;; Version: 0.0.1
;; Keywords: vcd hardware verilog
;; Homepage: https://github.com/jaykru/vcd.el
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs (yet).
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'cl-lib)
(require 'parsec)

;; from http://xahlee.info/emacs/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defvar sample-vcd (get-string-from-file (read-file-name "Select a VCD file ")))

(cl-defstruct vcd-header
  "VCD header data"
  (date nil)
  (version nil)
  (comment nil)
  (timescale nil))

;; example header
(make-vcd-header
 :date "today"
 :version 1.4
 :timescale '(1.5 "ns"))

(defun vcd-parse-header (vcd-text)
  "Parse VCD-HEADER-TEXT into a vcd-header struct."
  (cl-flet ((extract-field (field)
                           (parsec-with-input vcd-text
                             (parsec-many-till
                              (parsec-any-ch)
                              (parsec-re (format "\\$%s" field)))
                             (string-trim (parsec-many-till-as-string
                                            (parsec-any-ch)
                                            (parsec-re "\\$end"))))))
    (make-vcd-header
     :date nil ;; (extract-field "date")
     :version nil ;; (extract-field "version")
     :timescale (extract-field "timescale")
     :comment nil ;; (extract-field "comment")
     )))

(vcd-parse-header sample-vcd)

;; internal signal trace data structure will be a hashmap H of hashmaps
;;
;; the first level is just signal name -> blah
;; where blah is a hashmap from times in `timescale` incremements to updated values.
;;
;; this allows the wave construction to proceed like:
;; for each signal name:
;;      for each time:
;;              if time \in H[signal_name]:
;;                      cur_val[signal_name] = H[signal_name][time]
;;                      plot "|"+ cur_val[signal_name]
;;              else:
;;                      plot " " + cur_val[signal_name]

(defun vcd-parse-signal-trace (signal-identifier vcd-text)
  nil)

(defun parse-scope ()
  "Parses a scope declaration"
  (list 'push (parsec-and
               (parsec-re "\\$scope")
               (car (last (string-split
                           (string-trim (parsec-many-till-as-string (parsec-any-ch)
                                                                    (parsec-re "\\$end")))))))))

(defun parse-upscope ()
  (let ((res (parsec-and (parsec-re "\\$upscope")
                         (parsec-many-till (parsec-any-ch)
                                           (parsec-re "\\$end")))))
    (list 'pop)))

(parsec-with-input "$scope module blah $end"
  (parse-scope))
(parsec-with-input "$upscope $end"
  (parse-upscope))
(parsec-with-input "$360noscope $end"
  (parse-upscope))

(defun parse-var-decl ()
  (let* ((inner (parsec-and (parsec-str "$var")
                            (split-string (parsec-many-till-as-string
                                           (parsec-any-ch)
                                           (parsec-str "$end")))))
         (ty (first inner))
         (size (second inner))
         (code (third inner))
         (signal-name (fourth inner)))
    (list signal-name code ty size)))


(parsec-with-input "$var logic 1 ) beeg $end"
  (parse-var-decl))

(defun parse-vcd ()
  (parsec-sepby (parsec-or (parse-var-decl)
                           (parse-scope)
                           (parse-upscope))
                (parsec-re ".*")))

(parsec-with-input "$scope module top $end
        $scope module m1 $end
        $var trireg 1 *@ net1 $end
                $var trireg 1 *# net2 $end
        $var trireg 1 *$ net3 $end
        $upscope $end
$scope task t1 $end
$var reg 32 (k accumulator[31:0] $end
$var integer 32 {2 index $end
$upscope $end
$upscope $end" (parse-vcd))

(parsec-with-input (string-truncate-left sample-vcd 2000)
  (parse-vcd))

(defun vcd-parse-all-signal-traces (vcd-text)
  "TODO Dumps trace data for every traced signal in one pass.
   Costly for space when the trace is very large."
  (let (shortname-map (make-hash-table))
    (parsec-with-input vcd-text
      (parsec-many-till
       (parsec-any-ch)
       (parsec-re (format "\\$scope" field)))
      (string-trim (parsec-many-till-as-string
                    (parsec-any-ch)
                    (parsec-re "\\$end"))))))

(provide 'vcd)
;;; vcd.el ends here

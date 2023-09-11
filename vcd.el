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

(defun parse-vcd-header ()
  "Parse VCD-HEADER-TEXT into a vcd-header struct."
  (cl-flet ((extract-field (field)
                           (progn
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

(parsec-with-input sample-vcd (parse-vcd-header))

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
         (ty (car inner))
         (size (cadr inner))
         (code (caddr inner))
         (signal-name (cadddr inner)))
    (list 'declare-signal signal-name code ty size)))


(defun parsec-whitespace ()
  (parsec-one-of ?\n ?\t ?\ ))

(parsec-with-input "$var logic 1 ) beeg $end"
  (parse-var-decl))

(defun parse-var-bureaucracy ()
  (parsec-or
   (parsec-try (parse-var-decl))
   (parsec-try (parse-scope))
   (parsec-try (parse-upscope))))

(defun parse-vcd-name-decls ()
  (parsec-many1 (parsec-return
                   (parse-var-bureaucracy)
                  (parsec-many (parsec-whitespace)))))

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
$upscope $end" (parse-vcd-name-decls))

(parsec-with-input (substring sample-vcd 101 430)
  (parse-vcd-name-decls))

(defun mk-signal-name-table (decls)
  (let ((name-map (make-hash-table :test 'equal))
        (stk '()))
    (progn (dolist (decl decls)
              (pcase decl
                (`(push ,hier-name) (setf stk (cons hier-name stk)))
                (`(pop) (setf stk (cdr stk)))
                (`(declare-signal ,signal-name ,code ,ty ,size)
                 (let ((full-name (string-join (reverse (cons signal-name stk)) "."))
                       (time-map (make-hash-table :test 'equal)))
                   (puthash code (list full-name ty size time-map) name-map)))))
           name-map)))

(mk-signal-name-table (parsec-with-input "$scope module top $end
        $scope module m1 $end
        $var trireg 1 *@ net1 $end
                $var trireg 1 *# net2 $end
        $var trireg 1 *$ net3 $end
        $upscope $end
$scope task t1 $end
$var reg 32 (k accumulator[31:0] $end
$var integer 32 {2 index $end
$upscope $end
$upscope $end" (parse-vcd-name-decls)))

(defun parse-vcd-namemap ()
  (mk-signal-name-table (parse-vcd-name-decls)))

(defun parse-time ()
  (list 'time (parsec-and (parsec-ch ?#)
                          (parsec-return (string-to-number (parsec-many1-as-string (parsec-digit)))
                            (parsec-eol-or-eof)))))

(defun parse-identifier ()
  (string-trim (parsec-many1-as-string (parsec-nonwhitespace))))

(defun parsec-nonwhitespace ()
  (parsec-none-of ?\n ?\t ?\ ))

(defun parse-scalar-value ()
  (parsec-one-of ?0 ?1 ?x ?z))

(defun parse-vector-value ()
  (let ((base (parsec-one-of ?b ?r))
        (digits (parsec-many1-as-string (parse-scalar-value))))
    (concat base digits)))

(defun parse-vector-value-change ()
  (let ((value (parse-vector-value))
        (_ (parsec-many1 (parsec-whitespace)))
        (id (parse-identifier)))
    (list 'chg id value)))

(defun parse-scalar-value-change ()
  (let ((value (parse-scalar-value))
        (id (parse-identifier)))
    (list 'chg id (concat "0b" value))))

(defun parse-value-change ()
  (parsec-or (parsec-try (parse-vector-value-change))
             (parsec-try (parse-scalar-value-change))))

(parsec-with-input "b1000 )k" (parse-value-change))
(parsec-with-input "0#\n0b10000010" (parse-value-change))

(defun parse-value-changes ()
  (parsec-many (parsec-return
                    (parse-value-change)
                  (parsec-many (parsec-whitespace)))))

(defun parse-one-time-signal-dump (name-map)
  (let ((cur-time (cadr (parsec-and (parsec-many (parsec-whitespace))
                                    (parse-time)))))
    (mapc #'(lambda (chg) (pcase chg
                            (`(chg ,id ,value) (let ((time-map (destructuring-bind (_ _ _ table) (gethash id name-map)
                                                                 table)))
                                                 (puthash cur-time value time-map)))))
          (parse-value-changes)))
  nil)

(defun parse-signal-dumps (name-map)
  (progn (parsec-many-till (parsec-any-ch)
                           (parsec-lookahead (parsec-str "#")))
         (parsec-many (parsec-whitespace))
         (parsec-many (parsec-return
                          (parse-one-time-signal-dump name-map)
                        (parsec-many (parsec-whitespace))))
         nil))


(defun parse-enddefinitions ()
  (progn (parsec-str "$enddefinitions")
         (parsec-many1 (parsec-whitespace))
         (parsec-str "$end"))
  nil)

(defun parse-dumpvars (name-map)
  (let (
        ;; (_ (parsec-many-till (parsec-any-ch)
        ;;                      (parsec-lookahead (parsec-str "$dumpvars"))))
        (_ (parsec-str "$dumpvars"))
        (_ (parsec-many1 (parsec-whitespace)))
        (vcs (parse-value-changes))
        (_ (parsec-str "$end"))
        )
    (mapc #'(lambda (chg)
                (pcase chg
                  (`(chg ,id ,value)
                   (let ((time-map (destructuring-bind (_ _ _ table) (gethash id name-map)
                                     table)))
                     (puthash 'init value time-map)))))
            vcs)))

(defun parse-vcd ()
  (let* ((header (parse-vcd-header))
         (_ (parsec-many (parsec-whitespace)))
         (m (parse-vcd-namemap))
         (_ (parsec-many (parsec-whitespace)))
         (_ (parsec-option nil (parse-enddefinitions)))
         (_ (parsec-many (parsec-whitespace)))
         (_ (parsec-option nil (parse-dumpvars m)))
         (_ (parse-signal-dumps m))
         )
    m))

(defvar *namemap* (parsec-with-input sample-vcd (parse-vcd)))

(hash-table-p *namemap*)

(gethash "|s" *namemap*)
(gethash ":g" *namemap*)

(defmacro get-rest (p)
  `(parsec-with-input ,(cadr p)
     (substring ,(cadr p) (1- (parsec-query ,(caddr p) :end)) (1- (length ,(cadr p))))))

(defvar *remainder* (get-rest (parsec-with-input sample-vcd (parse-vcd))))

(parsec-with-input *remainder* (parse-value-changes))

(substring  (get-rest (parse-with-input *remainder* (parse-dumpvars *namemap*))) 0 100)

(defvar *new-rem* (get-rest (parsec-with-input *remainder* (parse-signal-dumps *namemap*))))

(substring *new-rem* 0 14)

(substring (get-rest (parsec-with-input *remainder* (progn  (parse-enddefinitions)
                                                            (parsec-many (parsec-whitespace))
                                                            (parsec-option nil (parse-dumpvars *namemap*))))) 0 100)

(defvar *new-rem* (get-rest (parsec-with-input *remainder*
                              (parsec-and (parsec-until (parsec-lookahead (parsec-str "#")))
                                          (parsec-count 3
                                                        (progn  (parse-time)
                                                                (parsec-many1
                                                                 (parsec-return (parse-value-change)
                                                                   (parsec-or (parsec-try (parsec-many (parsec-whitespace)))
                                                                              (parsec-try (parsec-eof))))))
                                                        )
                                          ))))

(substring *new-rem* 0 100)


(parsec-with-input "b000000000000000000000000 $" (parse-value-change))

(defun vcd-parse-all-signal-traces (vcd-text)
  "TODO Dumps trace data for every traced signal in one pass.
   Costly for space when the trace is very large."
  (let (name-map (make-hash-table :test 'equal))
    (parsec-with-input vcd-text
      (parsec-many-till
       (parsec-any-ch)
       (parsec-re (format "\\$scope" field)))
      (string-trim (parsec-many-till-as-string
                    (parsec-any-ch)
                    (parsec-re "\\$end"))))))

(provide 'vcd)
;;; vcd.el ends here


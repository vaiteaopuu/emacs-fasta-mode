;;; sequence-alignement.el --- a simple alginement program

;; Copyright © 2017, by vaitea OPUU

;; Author: vaitea OPUU (vaiteaopuu@gmail.com)
;; Version: 0.0.0
;; Created: 14 april 2017
;; Keywords: fasta, sequences, dna, protein, viewing

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License version 2.

;;; Commentary:

;; This package contains function to perform sequence alignment. First step is
;; to perform a pair alignment. It's the Needlemann & Wunch algorithm
;; implemented in a python module.

;;; Code:

(defun alignment ()
  "Open a new buffer for display alignment"
  (interactive)
  (switch-to-buffer "*alignment*")
  (alignment-mode)
  (alignment-init))

(defun alignment-init ()
  "Initialize the buffer for alignment: get 2 sequences"
  (let ((seq-a get-sequence)
        (seq-b get-sequence))
    (setq alignment (alignment-align-sequences seq-a seq-b)))
  )

;;; Alignment

(defun alignment-align-sequences (seq-a seq-b)
  "This function perform the alignment of 2 sequences. First we need to create
both matrix, path and scores."
  (message (format "%s\n%s" seq-a seq-b))
  )

(defun create-score-and-path-matrix (seq-a seq-b)
  "Get 2 sequences in order to create the scoring matrix"
  ;; first create empty matrix
  (setq score-matrix (make-vector (* (length seq-a) (length seq-b)) nil))
  (setq path-matrix (make-vector (* (length seq-a) (length seq-b)) nil))
  (mapcar (lambda (x y) (get-score-in-matrix x y)) (zip))
  )

;;; Backtracking

(defun backtracking-sequence-alignment (path-matrix score-matrix)
  "We need to go through the score-matrix from the bottom right to the upper
left corner. The path-matrix store the previous square (-i, j) if we add a gap
on the column sequence (i, -j) if we add a gap on the line sequence."
  )

;;; Displaying the alignement

(defun display-alignment (alignment)
  "This function display the alignment"
  )

;;; Motion in the display alignmnet buffer
;;; Derive

(define-derived-mode alignment-mode special-mode "alignment"
  "alignment-mode is a major mode for viewing sequence alignment file."
  )

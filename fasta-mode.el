;;; fasta-mode.el --- sample major mode for viewing fasta file

;; Copyright © 2015, by vaitea OPUU

;; Author: your name (vaiteaopuu@gmail.com)
;; Version: 0.0.0
;; Created: 14 april 2017
;; Keywords: fasta, sequences, dna, protein

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License version 2.

;;; Commentary:

;; Just syntax highlight for now.

;; full doc on how to use here

;;; Code:

;; * Customization

(defgroup fasta nil
  "Fasta files viewing and comments editing"
  :group 'bindings
  :prefix "fasta-"
  )

;; * Colors schemes

;; ** Header & comments
(defface fasta-header-face
  '((t (:foreground "Red" :weight bold)))
  "red")

(defface fasta-comment-face
  '((t (:foreground "Dodgerblue2" :weight bold)))
  "red")

;; ** Residues faces

(defface ailmfwvc--face
  '((t (:foreground "blue")))
  "A, I, L, M, F, W, V, C residues colors")

(defface rk--face
  '((t (:foreground "red")))
  "R & K residues colors")

(defface nstq--face
  '((t (:foreground "green")))
  "nst residues colors")

(defface c--face
  '((t (:foreground "pink")))
  "C residues colors")

(defface ed--face
  '((t (:foreground "magenta")))
  "E & D residues colors")

(defface g--face
  '((t (:foreground "orange")))
  "G residues colors")

(defface hy--face
  '((t (:foreground "cyan")))
  "H & Y residues colors")

(defface p--face
  '((t (:foreground "yellow")))
  "P residues colors")

;; ** Nucleotides faces

(defface a--face
  '((t (:foreground "green" :weight bold)))
  "green")

(defface t--face
  '((t (:foreground "Chartreuse" :weight bold)))
  "Chartreuse")

(defface c--face
  '((t (:foreground "Blue2" :weight bold)))
  "blue2")

(defface g--face
  '((t (:foreground "DeepSkyBlue" :weight bold)))
  "DeepSkyBlue")

;; * Key element regexp
(defvar fasta-header-symbol "^>")
(defvar fasta-comment-symbol "^ *;")

;; * Displacment function

(defun fasta-down-header ()
  "Go to to the previous header"
  (interactive)
  (if (search-forward-regexp fasta-header-symbol)
      (let ()
        (forward-char)
        (goto-char (search-forward-regexp fasta-header-symbol))
        (beginning-of-line))
    (message "no more sequence"))
  )

(defun fasta-up-header ()
  "Go to to the next header"
  (interactive)
  (goto-char (search-backward-regexp fasta-header-symbol))
  (beginning-of-line)
  )

;; * Other fonctions
;; ** insert date

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y %k:%M:%S %z")))

;; ** retrieve sequence name

(defun fasta-get-seq-name-under-cursor ()
  "This function retrieve the name of the sequence under the
  cursor. From the beginning of the line to the first space."
  ;; start from the beginning of the current line
  (interactive)
  ;; make sure that your at the beginning of header line
  (beginning-of-line)
  (let ((start-name (save-excursion
                      (re-search-forward "[^>]\\|[^ ]")))
        (end-name (save-excursion
                    (re-search-forward " "))))
    ;; If there something looking like > words_1 other words then it will save
    ;; the word_1.
    (when (and start-name end-name)
      (copy-region-as-kill (- start-name 1) (- end-name 1))
      (buffer-substring-no-properties (- start-name 1) (- end-name 1))
      ))
  )

;; ** append a new entry in file

(defun fasta-append-new-note ()
  "Append a new note about a particular fasta sequence"
  (interactive)
  ;; (message (fasta-get-seq-name-under-cursor))

  (let ((new-fasta-entry (fasta-get-seq-name-under-cursor)))
    (append-to-file (concat "* " new-fasta-entry "\n") nil "./test.org")
    (message new-fasta-entry)
    )
  (find-file-other-window "./test.org")
  )

(defun fasta-note-template (fasta-cur-header)
  "When appended, we retrieve other information from fasta header"
  (let (()))
  )

;; * Bindings
(defvar fasta-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-j" 'fasta-down-header)
    (define-key map "\M-k" 'fasta-up-header)
    (define-key map "\M-n" 'fasta-append-new-note)
    map)
  "Keymap for fasta mode")

(defvar fasta-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Lisp")))
    (set-keymap-parent map fasta-mode-shared-map)
    map)
  "TODO: Un mode mineur ")

;; * Derivativ mode part
(define-derived-mode fasta-mode fundamental-mode "fasta"
  "fasta-mode is a major mode for viewing fasta file."

  ;; Syntax highlight
  ;; (font-lock-add-keywords nil '(("A" . 'a--face)))
  ;; (font-lock-add-keywords nil '(("T" . 't--face)))
  ;; (font-lock-add-keywords nil '(("G" . 'g--face)))
  ;; (font-lock-add-keywords nil '(("C" . 'c--face)))
  (font-lock-add-keywords nil '(("A\\|I\\|L\\|M\\|F\\|W\\|V\\|C" . 'ailmfwvc--face)))
  (font-lock-add-keywords nil '(("R\\|K" . 'rk--face)))
  (font-lock-add-keywords nil '(("C" . 'c--face)))
  (font-lock-add-keywords nil '(("N\\|S\\|T\\|Q" . 'nstq--face)))
  (font-lock-add-keywords nil '(("E\\|D" . 'ed--face)))
  (font-lock-add-keywords nil '(("G" . 'g--face)))
  (font-lock-add-keywords nil '(("H\\|Y" . 'hy--face)))
  (font-lock-add-keywords nil '(("P" . 'p--face)))
  (font-lock-add-keywords nil '((">.*" . 'fasta-header-face)))
  (font-lock-add-keywords nil '((".*;.*$" . 'fasta-comment-face)))


  ;; Comment syntax
  (setq comment-start ";")
  (setq comment-end "")

  ;; Use new key map
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fa\\'" . wpdl-mode))

;; add the mode to the `features' list
(provide 'fasta-mode)

;; Local Variables&#58;
;; coding: utf-8
;; End:

;;; fasta-mode.el ends here

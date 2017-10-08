;;; fasta-mode.el --- sample major mode for viewing fasta file

;; Copyright © 2017, by vaitea OPUU

;; Author: vaitea OPUU (vaiteaopuu@gmail.com)
;; Version: 0.0.0
;; Created: 14 april 2017
;; Keywords: fasta, sequences, dna, protein, viewing

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License version 3.

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
  '((t (:foreground "Indianred3" :weight bold)))
  "red")

(defface fasta-comment-face
  '((t (:foreground "Dodgerblue2" :weight bold)))
  "red")

;; ** Residues faces

(defface ailmfwvc--face
  '((t (:foreground "RoyalBlue")))
  "A, I, L, M, F, W, V, C residues colors")

(defface rk--face
  '((t (:foreground "red2")))
  "R & K residues colors")

(defface nstq--face
  '((t (:foreground "LimeGreen")))
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
  '((t (:foreground "cyan3")))
  "H & Y residues colors")

(defface p--face
  '((t (:foreground "yellow3")))
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

;; * motion function

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
  (let (
        (start-name (save-excursion
                      ;; skip > and blank characters
                      (re-search-forward "[^\>[:blank:]]" (line-end-position))))
        (end-name (save-excursion
                    (re-search-forward "> *\\([^[:blank:]]+\\)" (line-end-position)))))
    ;; If there something looking like > words_1 other words then it will save
    ;; the word_1.
    (when (and start-name end-name)
      (copy-region-as-kill (- start-name 1) end-name)
      (buffer-substring-no-properties (- start-name 1) end-name)
      ))
  )

;; ** append a new entry in file

(defun fasta-append-new-note ()
  "Append a new note about a particular fasta sequence"
  (interactive)
  (setq note-file "./notes.org")

  (if (file-exists-p note-file)
      (message (format "adding new entry to: %s" note-file))
    (let ()
      (message (format "creating and adding new entry to: %s" note-file))
      (create-file-buffer note-file)
      (append-to-file "#+STARTUP: showeverything" nil note-file))
    )

  (let ((new-fasta-entry (fasta-get-seq-name-under-cursor))
        (whole-header (fasta-get-whole-header)))
    (message new-fasta-entry)
    (fasta-note-template new-fasta-entry whole-header note-file)
    (find-file-other-window note-file)
    (goto-char (point-max))
    (org-narrow-to-subtree)
    (org-show-block-all)
    )
  )

(defun fasta-get-whole-header ()
  "Get the whole fasta header"
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position))
  )

(defun fasta-note-template (fasta-sequence-id whole-header note-file)
  "When appended, we retrieve other information from fasta header"
  (append-to-file (format "\n* %s\n" fasta-sequence-id) nil note-file)
  (append-to-file (format ":PROPERTIES:\n") nil note-file)
  (append-to-file (format ":IDS: %s\n" (replace-regexp-in-string "\|" " " fasta-sequence-id)) nil note-file)
  (append-to-file (format ":FULL-HEADER: %s\n" (replace-regexp-in-string "^> *"
                                                                         "" whole-header)) nil note-file)
  (append-to-file (format ":END:\n") nil note-file)
  (append-to-file (insert-date))
  )

;; ** Count sequences

(defun fasta-count-sequence ()
  "Count the number of sequences in the current fasta buffer"
  (interactive)
  (let ((count (count-matches "^>.*" 1)))
    (message (format "there are %d sequences" count)))
  )

;; ** Make statistics

;TODO: REMOVE THIS ABSOLUTE PATH
(defun fasta-prcent-aa ()
  "Count number of each residue on each sequences"
  (interactive)
  (with-output-to-temp-buffer "*amino-acids-composition*"
    (shell-command (concat "python " "~/.add/emacs-fasta-mode/python-src/seq_tools.py -f " (buffer-file-name) " -a count")  "*amino-acids-composition*" "Message")
    (pop-to-buffer "*amino-acids-composition*")
    )
  )

;; ** Selection of sequence

(defun fasta-select-sequence ()
  "yank the whole sequence under the cursor"
  (interactive)
  (let ((start-sequence (save-excursion
                          (forward-char)
                          (re-search-backward ">" nil 't)))
        (end-sequence (save-excursion
                        (forward-char)
                        (re-search-forward ">" nil 't))))

    (if (and start-sequence end-sequence)
      (progn
        (message (format "%S to %S" start-sequence (- end-sequence 100)))
        (let ((selected-sequence (buffer-substring-no-properties start-sequence (- end-sequence 1))))
          (message selected-sequence)
          (kill-new selected-sequence)
          )
        )
      (progn
        (when start-sequence
          (message (format "last sequence"))
          (let ((selected-sequence (buffer-substring-no-properties start-sequence (point-max))))
            (message selected-sequence)
            (kill-new selected-sequence)
            )
          )
        (when end-sequence
          (message (format "first sequence"))
          (let ((selected-sequence (buffer-substring-no-properties (point-min) end-sequence)))
            (message selected-sequence)
            (kill-new selected-sequence)
            )
          )
        )
      )
    )
  )

;; * Narrow sequence

(defun fasta-narrow-sequence ()
  "Narrow the current fasta sequence"
  (interactive)
  (let (
        (start (save-excursion
                 (forward-char)
                 (re-search-backward ">" nil t)))
        (end (save-excursion
               (forward-char)
               (re-search-forward ">" nil t))))
    (if end
        (narrow-to-region start (- end 1))
      (narrow-to-region start (point-max)))
    )
  )

(defun fasta-widen-sequence ()
  "Widen to restore all other sequences"
  (interactive)
  (widen))

;; * Bindings
(defvar fasta-mode-shared-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-j" 'fasta-down-header)
    (define-key map "\M-k" 'fasta-up-header)
    (define-key map "\M-n" 'fasta-append-new-note)
    (define-key map "\M-w" 'fasta-narrow-sequence)
    (define-key map "\M-W" 'fasta-widen-sequence)
    (define-key map "\M-c" 'fasta-count-sequence)
    (define-key map "\M-C" 'fasta-prcent-aa)
    (define-key map "\M-s" 'fasta-select-sequence)
    map)
  "Keymap for fasta mode")

(defvar fasta-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "fasta")))
    (set-keymap-parent map fasta-mode-shared-map)
    map))

;; * Derivativ mode part

(define-derived-mode fasta-mode fundamental-mode "fasta"
  "fasta-mode is a major mode for viewing fasta file."

  (font-lock-add-keywords nil '(("a\\|i\\|l\\|m\\|f\\|w\\|v\\|c\\|A\\|I\\|L\\|M\\|F\\|W\\|V\\|C" . 'ailmfwvc--face)))
  (font-lock-add-keywords nil '(("r\\|k\\|R\\|K" . 'rk--face)))
  (font-lock-add-keywords nil '(("c\\|C" . 'c--face)))
  (font-lock-add-keywords nil '(("n\\|s\\|t\\|q\\|N\\|S\\|T\\|Q" . 'nstq--face)))
  (font-lock-add-keywords nil '(("e\\|d\\|E\\|D" . 'ed--face)))
  (font-lock-add-keywords nil '(("g\\|G" . 'g--face)))
  (font-lock-add-keywords nil '(("h\\|y\\|H\\|Y" . 'hy--face)))
  (font-lock-add-keywords nil '(("p\\|P" . 'p--face)))
  (font-lock-add-keywords nil '((".*;.*$" . 'fasta-comment-face)))
  (font-lock-add-keywords nil '((">.*" . 'font-lock-function-name-face)))

  ;; If the size is greater than 1024^2 Emacs switch to read-only-mode in order
  ;; to save performance.

  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo))

  ;; Comment syntax
  (setq comment-start ";")
  (setq comment-end "")

  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fa\\'" . fasta-mode))

;; add the mode to the `features' list
(provide 'fasta-mode)

;; Local Variables&#58;
;; coding: utf-8
;; End:

;;; fasta-mode.el ends here

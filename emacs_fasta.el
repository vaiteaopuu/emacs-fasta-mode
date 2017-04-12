(defvar fasta-header-regexp ">.+$")

(setq fasta-mode-syntax-color
      '((">.+$" . font-lock-function-name-face)
        ("[^>]A" . font-lock-function-name-face)
        ("[^>]G" . font-face-attributes)))

(define-derived-mode fasta-mode fundamental-mode "fasta"
  "major mode for editing mymath language code."
  (setq font-lock-defaults '(fasta-mode-syntax-color)))

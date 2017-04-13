;; Define all colors for each kind of elements

;; Title and comments
(defface fasta-header-face
  '((t (:foreground "Red" :weight bold)))
  "red")

(defface fasta-comment-face
  '((t (:foreground "Dodgerblue2" :weight bold)))
  "red")

;; Nucleotides faces
(defface a--face
  '((t (:foreground "blue" :weight bold)))
  "green")

(defface t--face
  '((t (:foreground "blue" :weight bold)))
  "Chartreuse")

(defface c--face
  '((t (:foreground "Blue2" :weight bold)))
  "blue2")

(defface g--face
  '((t (:foreground "DeepSkyBlue" :weight bold)))
  "BlueViolet")


(define-derived-mode fasta-mode fundamental-mode "fasta"
  "fasta-mode is a major mode for viewing fasta file."

  ;; Syntax highlight
  (font-lock-add-keywords nil '(("G" . 'g--face)))
  (font-lock-add-keywords nil '(("A" . 'a--face)))
  (font-lock-add-keywords nil '(("C" . 'g--face)))
  (font-lock-add-keywords nil '(("T" . 'a--face)))
  (font-lock-add-keywords nil '((">.+$" . 'fasta-header-face)))
  (font-lock-add-keywords nil '((".*;.+$" . 'fasta-comment-face)))

  ;; Comment syntax
  (setq comment-start ";")
  (setq comment-end "")

  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fa\\'" . wpdl-mode))

(provide 'fasta-mode)

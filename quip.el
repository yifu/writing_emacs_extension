
(defalias 'backward-quip 'backward-page)
(defalias 'forward-quip 'forward-page)
(defalias 'narrow-to-quip 'narrow-to-page)
(defalias 'what-quip 'what-page)

;; (defvar quip-mode-syntax-table (make-syntax-table)
;;  "Syntax table for quip major mode.")

;; (defvar quip-mode-abbrev-table (make-abbrev-table))
(require 'derived)

(define-derived-mode quip-mode text-mode "Quip"
  "Run the quip major mode.
Special commands:
\\{quip-mode-map}"
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'page-delimiter)

  (setq paragraph-start "%%\\|[ \t\n\^L]")
  (setq paragraph-separate "%%$\\|[ \t\^L]*$]")
  (setq page-delimiter "^%%$"))

(define-key quip-mode-map (kbd "C-x [") 'backward-quip)
(define-key quip-mode-map (kbd "C-x ]") 'forward-quip)
(define-key quip-mode-map (kbd "C-x n q") 'narrow-to-quip)
(define-key quip-mode-map (kbd "C-c w") 'what-quip)

(defun count-quips ()
  "Count the quips in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (message "%S" (count-matches "^%%$")))))

(provide 'quip)

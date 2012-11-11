
(defvar quip-mode-hook nil
  "*List of functions to call when entering Quip mode.")

(defvar quip-mode-map nil
  "Keymap for quip major mode.")

(if quip-mode-map
    nil
  (setq quip-mode-map (make-sparse-keymap))
  (define-key quip-mode-map (kbd "C-x [") 'backward-quip)
  (define-key quip-mode-map (kbd "C-x ]") 'forward-quip)
  (define-key quip-mode-map (kbd "C-x n q") 'narrow-to-quip)
  (define-key quip-mode-map (kbd "C-c w") 'what-quip))

;; (defvar quip-mode-syntax-table (make-syntax-table)
;;  "Syntax table for quip major mode.")

;; (defvar quip-mode-abbrev-table (make-abbrev-table))

(defun quip-mode ()
  "Run the quip major mode.
Special commands:
\\{quip-mode-map}"
  (interactive "")
  (kill-all-local-variables)

  (setq major-mode 'quip-mode)
  (setq mode-name "Quip")

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'page-delimiter)

  (setq paragraph-start "%%\\|[ \t\n\^L]")
  (setq paragraph-separate "%%$\\|[ \t\^L]*$]")
  (setq page-delimiter "^%%$")

  (use-local-map quip-mode-map)

  (run-hooks 'quip-mode-hook))

(defun count-quips ()
  "Count the quips in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (count-matches "^%%$"))))

(provide 'quip)

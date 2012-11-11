
(defvar quip-mode-hook nil
  "*List of functions to call when entering Quip mode.")

;; (defvar quip-mode-map nil
;;   "Keymap for quip major mode.")

;; (if quip-mode-map
;;     nil
;;   (setq quip-mode-map (make-keymap)))

;; (defvar quip-mode-syntax-table (make-syntax-table)
;;  "Syntax table for quip major mode.")

;; (defvar quip-mode-abbrev-table (make-abbrev-table))

(defun quip-mode ()
  "Run the quip major mode."
  (interactive "")
  (kill-all-local-variables)
  (setq major-mode 'quip-mode)
  (setq mode-name "Quip")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "%%\\|[ \t\n\^L]")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "%%$\\|[ \t\^L]*$]")
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "^%%$")
  ;;  (use-local-map 'quip-mode-map)
  (run-hooks 'quip-mode-hook))

(provide 'quip)

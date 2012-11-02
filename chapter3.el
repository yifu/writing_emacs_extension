
;; Chapter THREE. Cooperating Commands.

(put 'scroll-up-command 'scrolling t)
(put 'scroll-down-command 'scrolling t)
(put 'scroll-left-command 'scrolling t)
(put 'scroll-right-command 'scrolling t)

(defvar unscroll-to-point (make-marker)
  "The text position to revert to when rollbacking 'scroll-*-command'.")

(defvar unscroll-to-window (make-marker)
  "The window position to revert to when rollbacking 'scroll-*-commad'.")
;;(setq unscroll-to-window (make-marker))

(defvar unscroll-to-window-column nil
  "The window horizontal poistion to revert to when rollbackin 'scroll-*-command'.")
;(setq unscroll-to-window-column  nil)

(defun remember-pos-before-scroll ()
  "Change the 'this-command' to 'scrolling' and eventually store the current point, and window position (horizontal scrolling included)"
  (if (not (get last-command 'scrolling))
      (progn
        (set-marker unscroll-to-window (window-start))
        (set-marker unscroll-to-point (point))
        (setq unscroll-to-window-column (window-hscroll))
        (message "%s %s %s." unscroll-to-window unscroll-to-point unscroll-to-window-column))))

(defadvice scroll-up-command (before remember-for-unscroll
                                     activate compile)
  "Remember where we started from."
  (remember-pos-before-scroll))

(defadvice scroll-down-command (before remember-for-unscroll
                                     activate compile)
  "Remember where we started from."
  (remember-pos-before-scroll))

(defun unscroll ()
  "Jump to the location defined in 'unscroll-to'."
  (interactive)
  (if (not (and unscroll-to-point unscroll-to-window unscroll-to-window-column))
      (error "No position to unscroll to.")
    (progn
      (goto-char unscroll-to-point)
      (set-window-start nil unscroll-to-window)
      (set-window-hscroll nil unscroll-to-window-column))))

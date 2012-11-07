
;; Writing GNU Emacs Extensions experiments:
(global-set-key (kbd "C-x C-n") 'other-window)

(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "p")
  (other-window (- (or n 1))))

(global-set-key (kbd "C-x C-p") 'other-window-backward)

;(setq nil 2)

;; Set a finf-file-hook to prevent modifying files when accessed
;; through symlink,
(add-to-list 'find-file-hook 
             '(lambda ()
                (if (file-symlink-p buffer-file-name)
                    (progn
                      (setq buffer-read-only t)
                      (message "The visited file [%s] is a symlink." buffer-file-name)))))

;(remove-hook 'find-file-hook 'read-only-if-symlink)

(defun visit-symlink-target ()
  "Follow symlink when visited."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (find-alternate-file target)
          (error "Not visiting a symlink.")))
    (error "Not visiting a file.")))

(defun clobber-symlink ()
  "Replace symlink with a **copy** of a file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
        (if target
            (if (yer-or-no-p (format "Replace %s with %s? "
                                     buffer-file-name
                                     target))
                (progn
                  (delete-file buffer-file-name)
                  (write-file buffer-file-name)))
          (error "Not visiting a symlink.")))
    (error "Not visiting a file.")))


(defun my-own-test-function (&optional arg)
  "A function for test purpose. Need to be trahed later."
  (interactive "P")
  (message "test %d." (prefix-numeric-value arg)))

(global-set-key (kbd "C-x C-p") 'my-own-test-function)

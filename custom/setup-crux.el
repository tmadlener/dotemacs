;;; setup-crux --- Summary
;;; Commentary:
;; initialization and settings for the crux package

;;; Code:
(use-package crux
  :ensure t
  :init
  (require 'crux)

  :config
  ;; map some of the functions to default kbds
  ;; more or less following https://github.com/bbatsov/crux
  ;; "smart" moving to beginning of line (i.e. toggling between first non-whitespace char
  ;; and actual beginning of line)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)

  ;; move C-k to kill-whole line and s-k to kill line (since the former is probably
  ;; encountered more often during my use of emacs)
  (global-set-key (kbd "C-k") #'crux-kill-whole-line)
  (global-set-key (kbd "s-k") 'kill-line)

  ;; clean up the buffer before saving (indentation and whitespace cleanup)
  (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
  ;; not crux, but still a useful key-binding as
  ;; indenting can be time consuming on large buffers
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)

  ;; duplicating lines (and optionally commenting them)
  (global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c M-d") #'crux-duplicate-and-comment-current-line-or-region)

  ;; rename current buffer (and file if visited)
  (global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)

  ;; remap comment-dwim to comment-or-uncomment-region
  (global-set-key [remap comment-dwim] 'comment-or-uncomment-region)

  ;; use crux advises for some of
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)

  (crux-with-region-or-line comment-or-uncomment-region)

  )

(provide 'setup-crux)
;;; setup-crux.el ends here

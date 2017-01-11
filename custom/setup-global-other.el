;;; setup-global --- Summary
;;; Commentary:
;; setup file for things that are globally used but boil down to basically one-liners
;; on which separate files are somewhat wasted

;;; Code:
;; update changes on file on disk to the current buffer
(global-auto-revert-mode t)

;; also automatically update files accessed via tramp
(setq auto-revert-remote-files t)

;; setup ssh as default mode for tramp
(setq tramp-default-method "ssh")

;; change large file warning threshold to 100 MB
(setq large-file-warning-threshold 100000000)

;; collect all backups of files in one central directory
(defvar backup-directory "~/.backup.emacs")
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
;; backup settings
(setq
 make-backup-files t ; backup a file the first time it is saved
 backup-directory-alist `((".*" . ,backup-directory)) ; save backup files in defined directory
 backup-by-copying t ; copy current file into backup dir
 version-control t ; version number of backup files
 delete-old-versions 6 ; oldest version to keep when a new numbered backup is made
 kept-new-versions 9 ; newest versions to keep when a new numbered backup is made
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 30 ; seconds idle before auto-save
 auto-save-interval 200 ; number of keystrokes between autosaves
 )

;; automatically refresh dired buffer on changes (on disk)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; start garbage collection every 100 s to improve emacs performance
(setq gc-cons-threshold 100000000)

;; saveplace remembers the location in a file when saving
(save-place-mode 1)

;; go-to-address-mode, should make URLs clickable in a buffer
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; enable flyspell in text mode
(add-hook 'text-mode-hook 'flyspell-mode)

(setq global-mark-ring-max 5000 ;; increase mark ring capacity
      mark-ring-max 5000
      mode-require-final-newline t ;; add newline to end of file
      )

;; set variables concerning encoding
;; set variables concerning encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil) ;; prevent indentation from inserting tabs
(delete-selection-mode 1) ;; replace selection when typing instead of inserting at point
(global-set-key (kbd "RET") 'newline-and-indent) ;; indent on newline automatically

(setq
 kill-ring-max 5000 ;; increase kill-ring capacity
 )


;; highlight-symbol package for highlighting symobls at pointn and cycling through them
(use-package highlight-symbol
  :ensure t
  :init
  (require 'highlight-symbol)
  (setq highlight-symbol-mode t)
  (global-set-key (kbd "C-<f3>") 'highlight-symbol)
  (global-set-key (kbd "C-<f4>") 'highlight-symbol-next)
  (global-set-key (kbd "C-<f2>") 'highlight-symbol-prev)
  (global-set-key (kbd "M-s h d") 'highlight-symbol-remove-all)
  )

(use-package magit
  :ensure t
  :init
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  )


;; ibuffer setup; automatic grouping by mode
(setq ibuffer-saved-filter-groups
      '(("default"
         ("c++" (mode . c++-mode))
         ("python" (mode . python-mode))
         ("shell-scripts" (mode . shell-script-mode))
         ("json" (or (mode . javascript-mode)
                     (filename . ".json")))
         ("Helm" (mode . helm-major-mode))
         ("emacs-config" (or (filename . ".emacs.d")))
         ))
      )



(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "default")
             (ibuffer-auto-mode 1)))

;; collapse some of the groups by default via advicing ibuffer
(defvar ibuffer-default-collapsed-groups (list "Helm" "Default"))

(defadvice ibuffer (after collapse-helm)
  (dolist (group ibuffer-default-collapsed-groups)
    (progn
      (goto-char 1)
      (when (search-forward (concat "[ " group " ]") (point-max) t)
        (progn
          (move-beginning-of-line nil)
          (ibuffer-toggle-filter-group)
          )
        )
      )
    )
  (goto-char 1)
  (search-forward "[ " (point-max) t)
  )

(ad-activate 'ibuffer)

(provide 'setup-global-other)
;;; setup-global-other.el ends here

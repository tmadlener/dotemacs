;;; init --- Summary
;;; Commentary:
;;; init.el file called on Emacs startup

;;; Code:
;; mandatory inhibiting of startup screen
(setq inhibit-startup-screen t)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
  (package-initialize)
  )

;; ;; load packages that have been installed via git
;; (let ((default-directory "~/.emacs.d/packages_git/"))
;;   (normal-top-level-add-subdirs-to-load-path))

;; check if use-package is installed (everything after that is handled by
;; it)
(unless package-archive-contents
  (package-refresh-contents)) ;; fetch list of available packages

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; add customization files to load path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; load defined modules
(require 'use-package) ;; get use-package first since it is used in later setup
(require 'setup-async)
(require 'setup-helm)
(require 'setup-company)
(require 'setup-crux)
(require 'setup-programming-development)
(require 'setup-latex)
(require 'setup-faces-ui)
(require 'setup-global-other)
(require 'setup-aliases)
(require 'setup-functions)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (helm-c-yasnippet yasnippet magit use-package crux smartparens smartparens-config monokai-theme color-theme flycheck-tip flycheck auctex company-c-headers helm-descbinds company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)

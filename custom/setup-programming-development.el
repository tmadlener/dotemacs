;;; setup-programming-development --- Summary
;;; Commentary:
;; setup for programming and development

;;; Code:
;; line-numbering in programming mode by default (also show the column numbers)
(add-hook 'prog-mode-hook 'linum-mode)
(column-number-mode 1)

;; cedet and ede setup
;; TODO: check if this is done properly or if this can be enhanced in some way
;; according to official (stand alone) documentation of CEDET thi has to be loaded before
;; any other CEDET component
(require 'cedet)

;; semantic setup (basically copied from old config)
(require 'cc-mode)
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/gcc) ;; automatically load headers found by gcc
(require 'company-semantic)

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode 1) ;; cache done parsing
;; check if buffer is out of date automatically instead of waiting for command
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode 1)
;; enables automatic bookmarking of tags that you edited, so you can return to them later with the semantic-mrub-switch-tags command
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode 1) 
;; activate highlighting of local names that are the same as name of tag under cursor
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode 1)
;; activate displaying of possible name completions in the idle time
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode 1) ;; slowing down emacs

;; (require 'stickyfunc-enhance)
;; (add-hook 'prog-mode-hook 'semantic-stickyfunc-mode)
;; apparently needs mor tweaking

(semantic-mode 1)

;; EDE setup (project management features)
;; TODO: check how projectile might help in this
;; TODO: definitely check out projectile!!!
(require 'ede)
(global-ede-mode 1)
(ede-enable-generic-projects)


;; flycheck and flycheck-tip setup
;; flycheck performs syntax checking on the fly, flycheck-tip provides a pop-up for output
(use-package flycheck
  :init
  (require 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  :ensure t
  )

(use-package flycheck-pos-tip
  :init
  (require 'flycheck-pos-tip)

  :ensure t
  )

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))


;; whitespace setup (trailing whitespaces and max chars per line)
(use-package whitespace
  :init
  (require 'whitespace)
  (setq whitespace-style '(face empty lines-tail trailing))
  ;; this is what fits into half a screen width on the xps13 (with some margin)
  (setq whitespace-line-column 90)
  (global-whitespace-mode 1)

  :ensure t
  )


;; hs-minor-mode for code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c f") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)


;; compile settings
(use-package compile
  :ensure t
  :init (require 'compile)
  :config
  (setq compilation-ask-about-save nil ;; just save before compilation
	compilation-always-kill t ;; just kill old compile process before startin a new one
	compilation-scroll-output 'first-error ;; scroll to first error automatically
	)

  (global-set-key (kbd "<f5>") 'compile)
  )


;; smartparens setup
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  :config
  (sp-with-modes '(c++-mode c-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '(("| " "SPC")
						      ("* ||\n[i]" "RET")))
  )

;; open every .h file in c++-mode (since I mostly deal with c++ this should be a reasonable default)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; set indentation of shell scripts
(setq sh-basic-offset 2
      sh-indentation 2
      sh-indent-for-case-label 0
      sh-indent-for-case-alt '+)


;; yasnippet setup
(use-package yasnippet
  :ensure t
  :pin melpa
  :init
  (require 'yasnippet)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

;; helm-support for yasnippet
(use-package helm-c-yasnippet
  :ensure t
  :init
  (require 'yasnippet)
  (require 'helm-c-yasnippet)
  (global-set-key (kbd "C-c y") 'helm-yas-complete)
  (yas-load-directory "~/.emacs.d/snippets/")
  )

(provide 'setup-programming-development)
;;; setup-programming-development.el ends here

;; company setup
(use-package company
  :init
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t
  )

(use-package company-c-headers
  :init
  (require 'company-c-headers)

  :config
  (add-to-list 'company-backends 'company-c-headers)

  ;; enable c++ header completion
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.4.0")
  
  :ensure t
  )

(provide 'setup-company)

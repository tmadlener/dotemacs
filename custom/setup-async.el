;; async package setup (github package)
;; source: packages_git/async
;; github: https://github.com/jwiegley/emacs-async

(use-package async
  :init
  
  ;; dired-async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)

  (require 'async-bytecomp)
  ;; asynchronous compilation of packages
  (async-bytecomp-package-mode 1)
  (setq async-bytecomp-allowed-packages '(all)) ;; compile all packages asynchronously

  :ensure t
  ;; :pin manual
  )

(provide 'setup-async)

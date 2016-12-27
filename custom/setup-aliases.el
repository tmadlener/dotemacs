;;; setup-aliases --- Summary
;;; Commentary:
;;; file loaded from init.el at startup of Emacs for definition of different aliases

;;; Code:
;; define some aliases
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(setq ibuffer-use-other-window t) ;; display ibuffer in other window
(setq ibuffer-auto-buffers-changed t) ;; change point to ibuffer automatically

(provide 'setup-aliases)
;;; setup-aliases.el ends here

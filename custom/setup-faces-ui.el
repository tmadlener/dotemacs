;;; setup-faces-ui --- Summary
;;; Commentary:
;; setup ui and other stuff mainly concerning appearance

;;; Code:
;; remove unnecessary stuff (scroll bars, tool bars)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn of blinking cursor
(blink-cursor-mode -1)

;; scrolling behaviour
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; more useful frame title that show either file or buffer name
;; taken from prelude-ui.el
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
					    (abbreviate-file-name (buffer-file-name))
					  "%b"))))

;; change font for better looking text
(setq default-frame-alist '((font . "Fira Code-10")))
(set-face-attribute 'italic nil :family "Fira Code-Italic")

(use-package color-theme
  :ensure t
  :init (require 'color-theme)
  )

(use-package monokai-theme
  :ensure t
  :init (load-theme 'monokai t)
  )

(provide 'setup-faces-ui)
;;; setup-faces-ui.el ends here

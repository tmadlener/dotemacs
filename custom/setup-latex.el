;; setup everything concerning latex

(load "auctex.el" nil t t)

(setq-default TeX-PDF-mode t)

(require 'tex)
(defun auctex-hook-function()
  (setq TeX-auto-save t)
  (setq TeX-parse-self t) ;; parse document

  (setq-default TeX-engine 'xetex) ;; use xetex by default
  (setq-default TeX-master nil) ;; make AUCTex ask for a master (main file in which other files are included)
  (TeX-source-correlate-mode t) ;; synchronize evince and source
  (message "auctec-hook-function called")
  )

(add-hook 'LaTeX-mode-hook 'auctex-hook-function)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'yas-minor-mode)

(TeX-global-PDF-mode t)
(setq TeX-save-query nil) ;; auto save on tex-command

(setq reftex-plug-into-AUCTEX t)

;; highlight some user definend commands.
;; using the auctex warning feature for this (NOTE: only keywords, not arguments get highlighted)
;; a more sophisticated approach would be to define a face-lock for this
(setq font-latex-match-warning-keywords
      '(
        ("citeme" "{")
        ("comment" "{")
        ("todo" "{")
        ("refmis" "{")
        )
      )

(provide 'setup-latex)
;;; setup-latex.el ends here

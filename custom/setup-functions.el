;;; setup-functions --- Summary
;;; Commentary:
;;; file contains some functions defined by me

;;; Code:

(defun update-env ()
  "Update the environment variables set to Emacs.

TODO; make this actually take the environment 'at point' and not the one stored in a hardcoded file."
  (interactive)
  (let ((str
         (with-temp-buffer
           (insert-file-contents "/heplx04:/afs/hephy.at/user/t/tmadlener/environment.txt")
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

(provide 'setup-functions)
;;; setup-functions.el ends here

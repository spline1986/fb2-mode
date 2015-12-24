(require 'fb2)

(defvar fb2-mode-hook nil)

(defun fb2-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'wpdl-mode)
  (fb2-read)
  (run-hooks 'fb2-mode-hook))

(add-to-list 'auto-mode-alist '("\\.fb2$" . fb2-mode))

(provide 'fb2-mode)

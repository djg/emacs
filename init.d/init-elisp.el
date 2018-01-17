(require 'req-package)

(req-package eldoc
  :ensure t
  :commands eldoc-mode
  :init (progn
          (add-hook-exec 'emacs-lisp-mode #'eldoc-mode)
          (add-hook-exec 'lisp-interaction-mode #'eldoc-mode)))

(provide 'init-elisp)

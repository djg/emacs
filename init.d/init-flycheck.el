(require 'req-package)

(req-package flycheck
  :ensure t
  :pin melpa-stable
  :config (progn
            (global-flycheck-mode 1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled)
                  flycheck-standard-error-navigation nil)))

(req-package flycheck-pos-tip
  :ensure t
  :pin melpa-stable
  :commands flycheck-pos-tip-error-messages
  :require flycheck
  :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)

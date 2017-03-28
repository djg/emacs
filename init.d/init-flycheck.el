(require 'req-package)

(req-package flycheck
  :config
  (global-flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-standard-error-navigation nil))

(req-package flycheck-pos-tip
  :commands flycheck-pos-tip-error-messages
  :require flycheck
  :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)

(require 'req-package)

(req-package tss
  :init
  (setq tss-popup-help-key "C-:"
        tss-jump-to-definition "C->"
        tss-implement-definition-key "C-c i")
  :config
  (tss-config-default)
  (tss-setup-current-buffer))

(req-package typescript-mode
  :require tss
  :mode ("\\.ts$" . typescript-mode))

(provide 'init-typescript)

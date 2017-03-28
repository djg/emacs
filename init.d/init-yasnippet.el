(require 'req-package)

;; YA snippet
(req-package yasnippet
  :bind
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand))

(provide 'init-yasnippet)

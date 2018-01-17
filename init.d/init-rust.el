(require 'req-package)

(req-package rust-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.rs$"
  :commands rust-enable-format-on-save
  :init
  (add-to-list 'exec-path "~/.cargo/bin")
  :config (progn
            (add-hook-exec 'rust-mode #'racer-mode)
            (add-hook-exec 'rust-mode #'rust-enable-format-on-save)))

(req-package cargo
  :ensure t
  :pin melpa-stable
  :require rust-mode
  :commands cargo-minor-mode
  :config (add-hook-exec 'rust-mode #'cargo-minor-mode))

(req-package racer
  :ensure t
  :pin melpa-stable
  :require rust-mode eldoc company
  :commands (racer-mode)
  :config (progn
            (add-hook-exec 'racer-mode #'eldoc-mode)
            (add-hook-exec 'racer-mode #'company-mode)))

(req-package flycheck-rust
  :ensure t
  :require flycheck
  :config (add-hook-exec 'flycheck-mode #'flycheck-rust-setup))

(req-package toml-mode
  :ensure t
  :mode (("toml\\'" . toml-mode)))

(req-package rust-playground
  :ensure t)

(provide 'init-rust)

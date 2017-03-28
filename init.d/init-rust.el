(require 'req-package)

(req-package rust-mode
  :mode "\\.rs$"
  :commands rustfmt-enable-on-save
  :config
  (add-hook-exec 'rust-mode #'rustfmt-enable-on-save))

(req-package cargo
  :require rust-mode
  :config (add-hook-exec 'rust-mode 'cargo-minor-mode))

(req-package racer
  :require rust-mode eldoc-mode company-mode
  :config
  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path "~/rust/src/")
  (add-hook-exec 'rust-mode 'racer-mode)
  (add-hook-exec 'racer-mode 'eldoc-mode)
  (add-hook-exec 'racer-mode 'company-mode))

(req-package flycheck-rust
  :require flycheck-mode
  :config
  (add-hook-exec 'flycheck-mode 'flycheck-rust-setup))

(provide 'init-rust)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-hide-results t)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(delete-selection-mode t)
 '(fill-column 80)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(flycheck-javascript-eslint-executable "~/Mozilla/gecko/node_modules/.bin/eslint")
 '(flycheck-verilog-verilator-executable "verilator_bin")
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (verilog-mode yasnippet git-messenger yaml-mode web-mode google-c-style clang-format ccls company-lsp lsp-ui lsp-mode magit glsl-mode zerodark-theme flycheck-pos-tip multiple-cursors string-inflection scala-mode auto-package-update cargo change-inner cmake-font-lock edit-server emojify exec-path-from-shell flycheck flycheck-rust gitattributes-mode gitconfig-mode gitignore-mode goto-last-change js2-mode json-mode lua-mode markdown-preview-mode math-symbol-lists projectile projectile-ripgrep python-mode racer rainbow-delimiters rainbow-mode ripgrep rust-playground smart-mode-line smartparens-config toml-mode use-package)))
 '(projectile-cache-file "~/.emacs.d/data/projectile.cache")
 '(projectile-enable-caching t)
 '(projectile-file-exists-local-cache-expire 300)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")))
 '(projectile-keymap-prefix (kbd "C-c p"))
 '(projectile-known-projects-file "~/.emacs.d/data/projectile-bookmarks.eld")
 '(projectile-other-file-alist
   (quote
    (("cpp" "h" "inl")
     ("inl" "h" "cpp")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cpp" "inl" "m" "mm")
     ("vert" "frag")
     ("frag" "vert"))))
 '(projectile-sort-order (quote recentf))
 '(rainbow-delimiters-max-face-count 8)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(split-width-threshold nil)
 '(suggest-key-bindings t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 0)
 '(web-mode-style-padding 0)
 '(whitespace-line-column 90))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Fira Code")))))

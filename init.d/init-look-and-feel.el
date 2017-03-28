(require 'req-package)

;; Highlight numbers, quoted things, escape sequences
(req-package highlight-numbers
  :require prog-mode
  :config
  (add-hook-exec 'prog-mode 'highlight-numbers-mode)
  :diminish highlight-numbers-mode)

(req-package highlight-quoted
  :require prog-mode
  :config
  (add-hook 'prog-mode 'highlight-quoted-mode)
  :diminish highlight-quoted-mode)

(req-package highlight-escape-sequences
  :require prog-mode
  :ensure t
  :config
  (add-hook 'prog-mode 'hes-mode)
  :diminish hes-mode)

(req-package nyan-mode
  :config
  (setq nyan-animation-frame-interval 0.1
        nyan-bar-length 8
        nyan-wavy-trail t)
  (nyan-mode)
  (nyan-start-animation))

(req-package simple
  :bind ("C-z" . undo)
  :config
  (column-number-mode 1))

(req-package tool-bar
  :config
  (tool-bar-mode -1))

(req-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (custom-set-variables '(rainbow-delimiters-max-face-count 8))
  (add-hook-exec 'prog-mode- 'rainbow-delimiters-mode))
                                    
(req-package paren
  :init (show-paren-mode))

(provide 'init-look-and-feel)

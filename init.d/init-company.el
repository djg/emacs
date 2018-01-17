(require 'req-package)

(req-package company
  :ensure t
  :require yasnippet
  :config (progn
            (global-company-mode)
            (delete 'company-semantic company-backends)
            ;; Use dabbrev-code completion for windows
            (cond ((eq system-type 'cygwin)
                   (add-to-list 'company-backends 'company-dabbrev-code)))
            (setq company-tooltip-align-annotations t
                  company-show-numbers t)
            (define-key company-active-map (kbd "M-h") 'company-quickhelp-manual-begin)))


;; Use help with company
(req-package pos-tip
  :require company)

(req-package company-quickhelp
  :ensure t
  :require company
  :config
  (company-quickhelp-mode 1))

(provide 'init-company)

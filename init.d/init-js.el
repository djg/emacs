(require 'req-package)

;;; JS2
(req-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config 
  (setq-default js2-basic-offset 2)
  (font-lock-add-keywords 'js2-mode
                          '(("\\(console\\)\\(\.\\)\\(log\\|trace\\)"
                             (1 font-lock-warning-face t)
                             (3 font-lock-warning-face t)))))

(req-package js2-refactor
  :require js2-mode)

(provide 'init-js)

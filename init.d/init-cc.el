(require 'req-package)

(req-package cc-mode
  :mode (("\\.cpp$" . c++-mode)
         ("\\.hpp$" . c++-mode)
         ("\\.h$" . c++-mode))
  )

;; C++ mode
;; Header completion
(req-package company-c-headers
  :require cc-mode company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-hook-exec 'c-mode-common 'complete-c-headers))

(provide 'init-cc)

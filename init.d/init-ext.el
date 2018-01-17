(require 'req-package)

;;; Edit Server
(req-package edit-server
  :ensure t
  :config (edit-server-start))

;;; Multiple Cursors
(req-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;; Shift Number
(req-package shift-number
  :ensure t
  :bind (("C-M-<up>" . shift-number-up)
         ("C-M-<down>" . shift-number-down)))

;;; String Inversions
(req-package string-inflection
  :ensure t
  :bind (("C-c i" . string-inflection-cycle)
         ;; Force to CamelCase
         ("C-c C" . string-inflection-camelcase)
         ;; Force to lowerCamelCase
         ("C-c L" . string-inflection-lower-camelcase)))

(provide 'init-ext)

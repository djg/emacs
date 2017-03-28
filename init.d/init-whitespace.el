(require 'req-package)

(req-package whitespace
  :config
  (custom-set-faces
   '(whitespace-newline ((t (:foreground "gray20"))))
   '(whitespace-space ((t (:foreground "gray20"))))
   '(whitespace-tab ((t (:background "yellow" :foreground "red")))))
  (custom-set-variables
   '(whitespace-line-column 90)))

(provide 'init-whitespace)

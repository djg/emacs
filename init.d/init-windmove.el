(require 'req-package)

(req-package windmove
  :bind (("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)))

(provide 'init-windmove)

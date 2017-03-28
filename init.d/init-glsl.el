(require 'req-package)

(req-package glsl-mode
  :mode (("\\.glsl$" . glsl-mode)
         ("\\.vert$" . glsl-mode)
         ("\\.frag$" . glsl-mode)
         ("\\.geom$" . glsl-mode)))

(provide 'init-glsl)

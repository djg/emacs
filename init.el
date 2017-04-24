(package-initialize)

(require 'package)

(defconst djg/custom-file (concat user-emacs-directory "custom.el"))

(setq custom-file djg/custom-file
      package-enable-at-startup nil)
(load djg/custom-file t)

(setq inhibit-splash-screen t
      inhibit-startup-message t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(add-hook 'after-init-hook
          (lambda()
            (load (concat user-emacs-directory "init-real.el"))))

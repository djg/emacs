(require 'req-package)

;;; Projectile
(req-package projectile
  :ensure t
  :commands (projectile-global-mode projectile-ignored-projects projectile-compile-project)
  :init (progn
	  (projectile-global-mode)
          (defconst projectile-mode-line-lighter " Proj")))

;;; Grep

(req-package grep
  :ensure t
  :defer 1
  :config (progn
            ;; ignored folders
            (add-to-list 'grep-find-ignored-directories ".release")
            (add-to-list 'grep-find-ignored-directories ".repos")
            (add-to-list 'grep-find-ignored-directories "auto")
            (add-to-list 'grep-find-ignored-directories "elpa")
            (add-to-list 'grep-find-ignored-directories ".git")))

(provide 'init-search)

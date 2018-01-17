(require 'req-package)

(req-package git-timemachine
  :ensure
  :bind ("C-x v t" . git-timemachine))

(req-package magit
  :ensure t
  :bind ("C-x m" . magit-status)
  :commands magit-status
  :config
  (setq magit-auto-revert-mode 1))

(req-package magit-gh-pulls
  :ensure t
  :require magit
  :config
  (add-hook-exec 'magit-mode 'turn-on-magit-gh-pulls))

(req-package git-commit
  :ensure t)

(req-package gitconfig-mode
  :ensure t)

(req-package gitignore-mode
  :ensure t)

(req-package gitattributes-mode
  :ensure t)
   
(provide 'init-git)

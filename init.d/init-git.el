(require 'req-package)

(req-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(req-package magit
  :bind ("C-x m" . magit-status)
  :commands magit-status
  :config
  (setq magit-auto-revert-mode 1))

(req-package magit-gh-pulls
  :require magit
  :config
  (add-hook-exec 'magit-mode 'turn-on-magit-gh-pulls))

(req-package git-commit)
(req-package gitconfig-mode)
(req-package gitignore-mode)
(req-package gitattributes-mode)
   
(provide 'init-git)

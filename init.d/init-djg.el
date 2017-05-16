;;; init-djg.el --- Dan's Emacs initialization

;; Copyright 2015 Daniel Glastonbury

;; Description: Dan's Emacs initialization
;; Author: Dan Glastonbury <dan.glastonbury@gmail.com>
;; Version: 0.0.1
;; Package-Requires: (package)


;;; Commentary:
;; 	No comments

;;; Code:

(defmacro GUI () `(window-system))
(defmacro OSX () `(equal system-type 'darwin))
(defmacro WINNT () `(equal system-type 'windows-nt))

(when (OSX)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/bin") exec-path)
        mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper))

(unless (package-installed-p 'use-package)
  (message "%s" "Bootstrapping use-package...")
  (package-refresh-contents)
  (package-install 'use-package)
  (message "%s" " done."))

(require 'djg)

;; Setup sane default settings
(use-package sane-defaults)

(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; Setup my stuff
(use-package djg
  :bind (("C-x C-<left>" . djg/buf-move-left)
         ("C-x C-<right>" . djg/buf-move-right)
         ("C-x C-<up>" . djg/buf-move-up)
         ("C-x C-<down>" . djg/buf-move-down)
         ("C-<return>" . djg/open-line-below)
         ("C-S-<return>" . djg/open-line-above)
         ("M-o" . djg/find-other-file)
         ("M-O". djg/find-other-file-other-window)))

;;; Ample theme
;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t)
;;                (enable-theme 'ample-light))
;;   :defer 1
;;   :ensure t)

;;; CC
(use-package cc-mode
  :defer t
  :init (progn
          (add-hook 'c-mode-common-hook
                    '(lambda ()
                       (local-set-key (kbd "<f7>") 'recompile)))
          (add-hook 'c-mode-common-hook
                    '(lambda ()
                       (add-hook 'write-contents-hooks 'djg/untabify-buffer nil t)))))

;; Company
;; (use-package company
;;   :commands global-company-mode
;;   :init
;;   (global-company-mode)
;;   :config
;;   (setq company-tooltip-limit 20
;;         company-idle-delay .3
;;         company-echo-delay 0
;;         company-begin-commands '(self-insert-command))
;;   :ensure t)


;; clang-format
(use-package clang-format
  :ensure t
  :bind
(("C-c f" . clang-format-buffer)))

;; CMake
(use-package cmake-mode
  :defer t
  :mode ("\\.cmake$" . cmake-mode)
  :ensure t)

;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; Edit Server
(use-package edit-server
  :if window-system
  :init (add-hook 'after-init-hook 'server-start t))

;; (use-package ergoemacs-mode
;;   :defer t
;;   :init (progn
;;           (setq ergoemacs-theme nil
;;                 ergoemacs-keyboard-layout "us")
;;           (ergoemacs-mode 1))
;;   :ensure t)

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)

;; Helm
;; (use-package helm
;;   :defer t
;;   :diminish helm-mode
;;   :ensure t
;;   :init (progn
;;           (require 'helm-config)
;;           (setq helm-candidate-number-limit 100)
;;           (setq helm-idle-delay 0.0
;;                 helm-input-idle-delay 0.01
;;                 helm-quick-update t
;;                 helm-M-x-requires-pattern nil
;;                 helm-M-x-fuzzy-match t
;;                 helm-ff-skip-boring-files t
;;                 helm-semantic-fuzzy-match t
;;                 helm-imenu-fuzzy-match t)
;;           (global-unset-key (kbd "C-x c"))
;;           (helm-mode))
;;   :config (progn
;;             (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;             (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;;             (define-key helm-map (kbd "C-z") 'helm-select-action))
;;   :bind (("C-c h" . helm-command-prefix)
;;          ("C-c h o" . helm-occur)
;;          ("C-h SPC" . helm-all-mark-rings)
;;          ("M-x" . helm-M-x)
;;          ("M-y" . helm-show-kill-ring)))

;;(use -package irony
;;  :init (progn
;;          (add-hook 'c++-mode-hook 'irony-mode)
;;          (add-hook 'c-mode-hook 'irony-mode)
;;          (add-hook 'irony-mode-hook 'djg/irony-mode-init)
;;          (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
;;  :ensure t)

;;(use-package company-irony
;;  :init
;;  (eval-after-load 'company
;;    '(add-to-list 'company-backends 'company-irony))
;;  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;;  :ensure t)


;;; Mozilla CC styling
(use-package mozilla-c-style
  :config (progn (add-hook 'c-mode-common-hook 'moz/set-c-style)
                 (add-hook 'c-mode-common-hook 'moz/make-newline-indent)))

;;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)

;;; Projectile
(use-package projectile
  :commands (projectile-global-mode projectile-ignored-projects projectile-compile-project)
  :init (progn
	  (projectile-global-mode)
          (defconst projectile-mode-line-lighter " Proj"))
  :bind (("M-S-p" . projectile-find-file))
  :defer t
  :ensure t)

(bind-key "<f8>" 'next-error)

;; (use-package helm-projectile
;;   :defer t
;;   :ensure t)

;;; SMEX
;; (use-package smex
;;   :disabled t
;;   :bind (("M-x" . smex)
;;          ("C-c M-x" . execute-extended-command))
;;   :ensure t)

;; string inversions
(use-package string-inflection
  :defer t
  :bind (("C-c i" . string-inflection-cycle)
         ("C-c C" . string-inflection-camelcase)        ;; Force to CamelCase
         ("C-c L" . string-inflection-lower-camelcase)) ;; Force to lowerCamelCase
  :ensure t)

;; Undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

;; Web
(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config (progn
            (defun web-indirect-this-thing()
              (interactive)
              (let ((beg 0) (end 0))
                (save-excursion
                  (setq beg (progn (web-mode-forward-sexp -1)
                                   (call-interactively 'web-mode-tag-end)
                                   (point)))
                  (setq end (progn (web-mode-forward-sexp 1)
                                   (point))))
                (indirect-region begin end))))
  :ensure t)

;; which-key to give help on keys
(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode)
  :diminish which-key-mode)

;; YAML
(use-package yaml-mode
  :defer t
  :ensure t)


;; Builtins
(use-package custom
  :config (setq custom-file (concat user-emacs-directory "custom.el")))

(use-package files
  :config (setq backup-directory-alist `(("." . "~/.saves"))
                version-control t
                kept-new-versions 10
                kept-old-versions 0
                delete-old-versions t
                backup-by-copying t))

(use-package compile
  :config (progn (setq compilation-scroll-output t)
                 (add-to-list 'compilation-error-regexp-alist
                              '("^\\(/.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|note\\):" 1 2 3))))

(defun dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."

  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)

(provide 'init-djg)
;;; init-djg.el ends here

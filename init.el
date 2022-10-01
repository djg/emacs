(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(add-to-list 'load-path (emacs-path "lisp"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;;(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (message "%s" "Bootstrapping use-package...")
  (package-refresh-contents)
  (package-install 'use-package)
  (message "%s" " done."))

(require 'use-package)

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(eval-and-compile
  (defconst djg/custom-file (emacs-path "custom.el"))
  (setq custom-file djg/custom-file)
  (load djg/custom-file t))

(package-install-selected-packages)

(use-package align
  :bind (("M-[" . djg/align-code)
         ("C-c [" . align-regexp))
  :commands align
  :preface
  (defun djg/align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))

(use-package auto-package-update
  :config
  (auto-package-update-maybe))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;; (use-package ccls
;;   :ensure t
;;   :after lsp-mode
;;   :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode))
  :preface
  (defun djg/c-mode-common-hook()
    (let ((bufname (buffer-file-name)))
      (message "djg/c-mode-common-hook `%s'..." bufname)
      (when bufname
        (cond
         ((string-match "/Volumes/WebKit[^/]*/" bufname)
          (message "Setting webkit style...")
          (webkit-set-c-style))
         ((string-match "/llvm[^/]*/" bufname)
          (message "Setting llvm.org style...")
          (c-set-style "llvm.org"))
         (t
          (message "Leaving default style..."))))))

    :hook (c-mode-common . djg/c-mode-common-hook))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o M-o" . change-outer)))

(use-package clang-format
  :after cc-mode
  :preface
  (defun djg/clang-format-buffer ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (clang-format-buffer)))
  (defun djg/clang-format-on-save ()
    "Add auto-save hook for djg/clang-format-buffer."
    (add-hook 'before-save-hook 'djg/clang-format-on-save))
  :hook (c-mode-common . djg/clang-format-on-save))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

(use-package compile
  :no-require
  :bind (("C-c c" . compile)
         ("M-O"   . djg/show-compilation))
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :preface
  (defun djg/show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun djg/compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook (compilation-filter . djg/compilation-ansi-color-process-output))

(use-package css-mode
  :mode "\\.css\\'")

(use-package djg
  :bind (("C-x C-<left>" . djg/buf-move-left)
         ("C-x C-<right>" . djg/buf-move-right)
         ("C-x C-<up>" . djg/buf-move-up)
         ("C-x C-<down>" . djg/buf-move-down)
         ("C-<return>" . djg/open-line-below)
         ("C-S-<return>" . djg/open-line-above)
         ("C-x |" . djg/toggle-window-split)
         ;;("M-o" . djg/find-other-file)
         ;;("M-O". djg/find-other-file-other-window)
         ))

(use-package eglot
  :commands eglot-ensure
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(use-package ediff
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)))


(use-package edit-server
  :if window-system
  :defer 5
  :config
  (edit-server-start))

;;(use-package emojify
;;  :defer 15
;;  :config
;;  (global-emojify-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (rust-mode-hook       . rust-mode-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)

  (defun djg/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'djg/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

  (defun flycheck-handle-idle-change ()
    "Handle an expired idle time since the last change.
  This is an overwritten version of the original
  flycheck-handle-idle-change, which removes the forced deferred.
  Timers should only trigger inbetween commands in a single
  threaded system and the forced deferred makes errors never show
  up before you execute another command."
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change)))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :commands flycheck-pos-tip-error-messages)

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package gitattributes-mode
  :defer 5)

(use-package gitconfig-mode
  :defer 5)

(use-package gitignore-mode
  :defer 5)

(use-package glsl-mode
  :ensure t
  :mode("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

(use-package google-c-style
  :ensure t)

(use-package webkit-c-style
  :commands webkit-set-c-style)

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package js2-mode
  :mode "\\.js\\'"
  :after flycheck
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

(use-package ligatures
  :commands ligatures/add-to-font-lock-keywords)

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (let* ((path (file-truename
                  (directory-file-name
                   (expand-file-name default-directory))))
           (name (format "*git-monitor: %s*"
                         (file-name-nondirectory path))))
      (unless (and (get-buffer name)
                   (with-current-buffer (get-buffer name)
                     (string= path (directory-file-name default-directory))))
        (with-current-buffer (get-buffer-create name)
          (cd path)
          (ignore-errors
            (start-process "*git-monitor*" (current-buffer)
                           "git-monitor" "-d" path))))))
  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))
  :config
  (use-package magit-commit
    :config
    (use-package git-commit))

  (use-package magit-files
    :config
    (global-magit-file-mode))

  (add-hook 'magit-status-mode-hook
            #'(lambda ()
                (magit-monitor t))))

(use-package magit-popup
  :defer t)

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"	      . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

(use-package math-symbol-lists
  :defer t)

(use-package mule
  :no-require t
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-C C->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-< " . mc/mark-all-this-this)))

(use-package projectile
  :commands (projectile-global-mode projectile-ignored-projects projectile-compile-project)
  :init
  (projectile-global-mode))

(use-package projectile-ripgrep
  :after (projectile ripgrep)
  :bind (:map projectile-command-map
              ("s s" . projectile-ripgrep)))

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :bind (:map python-mode-map
              ("C-c c")
              ("C-c C-z" . python-shell))
  :config
  (defvar python-mode-initialized nil)
  (defun djg/python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t))

    (info-lookup-add-help
     :mode 'python-mode
     :regexp "[a-zA-Z_0-9.]+"
     :doc-spec
     '(("(python)Python Module Index" )
       ("(python)Index"
        (lambda
          (item)
          (cond
           ((string-match
             "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
            (format "%s.%s" (match-string 2 item)
                    (match-string 1 item))))))))

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil))

  (add-hook 'python-mode-hook #'djg/python-mode-hook))

(use-package racer
  :after rust-mode
  :hook (rust-mode . racer-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package ripgrep)

(defconst rust-fira-code-ligatures-list
  '(("[^/]\\(\\*\\*\\)[^/]" #Xe101 2)   ; **
    ("\\(\\*\\*\\*\\)"      #Xe102 3)   ; ***
    ("\\(\\*>\\)"           #Xe103 2)   ; *>
    ("[^*]\\(\\*/\\)"       #Xe104 2)   ; */
    ("\\(!=\\)"             #Xe10e 2)   ; !=
    ("\\(--\\)"             #Xe111 2)   ; --
    ("\\(->\\)"             #Xe114 2)   ; ->
    ("\\(#\\[\\)"           #Xe11a 2)   ; #[
    ("\\(##\\)"             #Xe11c 2)   ; ##
    ("\\(###\\)"            #Xe11d 3)   ; ###
    ("\\(####\\)"           #Xe11e 4)   ; ####
    ("\\(\\.\\.\\)"         #Xe126 2)   ; ..
    ("\\(\\.\\.\\.\\)"      #Xe129 3)   ; ...
    ("\\(/\\*\\)"           #Xe130 2)   ; /*
    ("\\(/=\\)"             #Xe131 2)   ; /=
    ("\\(//\\)"             #Xe134 2)   ; //
    ("\\(///\\)"            #Xe135 3)   ; ///
    ("\\(__\\)"             #Xe137 2)   ; __
    ("\\(&&\\)"             #Xe138 2)   ; &&
    ("\\(||\\)"             #Xe139 2)   ; ||
    ("\\(||=\\)"            #Xe13b 3)   ; ||=
    ("\\(|=\\)"             #Xe140 2)   ; |=
    ("\\(^=\\)"             #Xe142 2)   ; ^=
    ("\\(==\\)"             #Xe149 2)   ; ==
    ("\\(=>\\)"             #Xe14c 2)   ; =>
    ;;("\\(>=\\)"             #Xe152 2)   ; >=
    ("\\(>=\\)"             #X2265 1)   ; >=
    ("\\(>>\\)"             #Xe154 2)   ; >>
    ("\\(>>=\\)"            #Xe156 3)   ; >>=
    ;;("\\(<=\\)"             #Xe168 2)   ; <=
    ("\\(<=\\)"             #X2264 1)   ; <=
    ("\\(<<\\)"             #Xe16d 2)   ; <<
    ("\\(<<=\\)"            #Xe16f 3)   ; <<=
    ("0\\(x\\)[0-9a-fA-F]"  #X00d7 1))) ; Replace x in 0x1234

;(use-package rust-mode
;  :mode "\\.rs\\'"
;  :hook ((rust-mode . flycheck-mode)
;	 (rust-mode . rust-enable-format-on-save)))
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . flycheck-mode)))

(use-package rust-playground
  :defer t)

(use-package sane-defaults)

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'")

(use-package simple
  :bind ("C-z" . undo))

(use-package sleigh-mode
  :mode ("\\.sinc\\'"
         "\\.slaspec\\'"))

(use-package smart-mode-line
  :config
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (sml/setup)
  (sml/apply-theme 'light)
  (remove-hook 'display-time-hook 'sml/propertize-time-string))

(use-package smartparens-config
  :commands smartparens-mode)

(use-package string-inflection
  :ensure t
  :bind (("C-c i" . string-inflection-cycle)
         ("C-c C" . string-inflection-camelcase)
         ("C-c L" . string-inflection-lower-camelcase)))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package web-mode
  :ensure t
  :commands web-mode)

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode
             whitespace-turn-off)
  :preface
  (defvar normalize-hook nil)

  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (run-hook-with-args normalize-hook)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "depending on the file, maybe clean up whitespace."
    (when (and (not (or (memq major-mode '(markdown-mode))
                        (and buffer-file-name
                             (string-match "\\(\\.texi\\|COMMIT_EDITMSG\\)\\'"
                                           buffer-file-name))))
               (locate-dominating-file default-directory ".clean")
               (not (locate-dominating-file default-directory ".noclean")))
      (whitespace-mode 1)
      ;; For some reason, having these in settings.el gets ignored if
      ;; whitespace loads lazily.
      (setq whitespace-auto-cleanup t
            whitespace-line-column 80
            whitespace-rescan-timer-time nil
            whitespace-silent t
            whitespace-style '(face trailing lines space-before-tab empty))
      (add-hook 'write-contents-hooks
                #'(lambda () (ignore (whitespace-cleanup))) nil t)
      (whitespace-cleanup)))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'"
         "\\.yaml\\'")
  :bind (("\C-m" . newline-and-indent)))

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)

(dir-locals-set-class-variables 'huge-git-repository
                                '((magit-status-mode . ((eval
                                                         . (magit-disable-section-inserter
                                                            'magit-insert-tags-header)
                                                         )))))

(dir-locals-set-directory-class "/Volumes/Devel/webkit/Internal" 'huge-git-repository)
(dir-locals-set-directory-class "/Volumes/Devel/webkit/OpenSource" 'huge-git-repository)

;; Sane Emacs Defaults
(setq inhibit-startup-message t)
(setq visible-bell t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(top . 0)) 
(add-to-list 'default-frame-alist '(width . 132))
;(add-to-list 'default-frame-alist '(height . 79))

;; Buffer splitting - I can't stand splitting windows horizontally, so fuck. that. shit
(setq split-width-threshold nil)

;; Place backup files in tmp directory
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Lines should be 80 chars wide
(setq fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate camelCase words
(global-subword-mode 1)

;; Keep cursor away from edges when scrolling up/down
;(require 'smooth-scrolling)

(setq suggest-key-bindings t)

;; Disable VC
(setq vc-handled-backends ())

(provide 'sane-defaults)

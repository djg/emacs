(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-option-modifier 'none
        ns-function-modifier 'hyper)

  (defun mac-open-file ()
    (interactive)
    (let ((file (do-applescript "POSIX path of (choose file)")))
      (if (> (length file) 3)
          (setq file
                (substring file 1 (- (length file) 1))))
      (if (and (not (equal file "")) (file-readable-p file))
          (find-file file))))

  (defun mac-save-file-as ()
    (interactive)
    (let ((file (do-applescript "POSIX path of (choose file name with prompt \"Save As...\")")))
      (if (> (length file) 3)
          (setq file
                (substring file 1 (- (length file) 1))))
      (if (not (equal file ""))
          (write-file file))))

  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-g") 'isearch-repeat-forward)
  (global-set-key (kbd "s-o") 'find-file)
  (global-set-key (kbd "s-O") 'mac-open-file)
  (global-set-key (kbd "s-n") 'find-file)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-S") 'mac-save-file-as)
  (global-set-key (kbd "s-p") 'mac-preview) ; requires mac-preview
  (global-set-key (kbd "s-P") 'execute-extended-command)
  (global-set-key (kbd "s-w") 'kill-buffer)
  (global-set-key (kbd "s-m") 'iconify-frame)
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-.") 'keyboard-quit)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-k") 'kill-buffer)
  (global-set-key (kbd "s-<up>")    'beginning-of-buffer)
  (global-set-key (kbd "s-<down>")  'end-of-buffer)
  (global-set-key (kbd "s-<left>")  'beginning-of-line)
  (global-set-key (kbd "s-<right>") 'end-of-line)
  (global-set-key [(meta down)]     'forward-paragraph)
  (global-set-key [(meta up)]       'backward-paragraph))

(provide 'sane-defaults)

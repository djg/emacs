(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper))

(provide 'sane-defaults)

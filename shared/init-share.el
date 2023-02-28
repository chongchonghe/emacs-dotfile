(setq compile-command "make ")
;; (global-set-key (kbd "C-c r") #'recompile)

(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-K") 'kill-buffer-and-window)
(global-set-key (kbd "s-e") 'eval-region)
(global-set-key (kbd "s-b") 'eval-buffer)
(global-set-key (kbd "s-c") 'compile)
(global-set-key (kbd "s-r") 'recompile)
(global-set-key (kbd "s-,") 'previous-buffer)
(global-set-key (kbd "s-.") 'next-buffer)
;; (global-unset-key (kbd "s-j"))
;; (global-set-key (kbd "s-j") 'jump-to-register)
;; (global-set-key (kbd "M-v") 'evil-paste-after)

(global-set-key (kbd "M-v") 'clipboard-yank)

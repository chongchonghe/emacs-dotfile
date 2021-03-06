;; (use-package latex
(use-package tex
      :defer t
      :mode ("\\.tex\\'" . latex-mode)
      ;; :ensure auctex
      :bind
      (:map LaTeX-mode-map
	("M-n" . outline-next-heading)
	("M-p" . outline-previous-heading))
      :config
      (setq TeX-auto-save t)
      (add-hook 'LaTeX-mode-hook #'visual-line-mode)
      (add-hook 'LaTeX-mode-hook #'no-auto-fill)
      (add-hook 'LaTeX-mode-hook 'hs-minor-mode)
      (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
      (setq TeX-PDF-mode t)	      ;; Compile documents to PDF by default
      (setq-default TeX-master nil) ;; Make emacs aware of multi-file projects
      ;; CDLaTeX
      (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
      ;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
      (autoload 'helm-bibtex "helm-bibtex" "" t)
      (electric-pair-mode)
      (add-hook 'LaTeX-mode-hook
		'(lambda ()
		       (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
		       ))
      (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
      ;; (setq reftex-plug-into-auctex t)
      (setq reftex-plug-into-AUCTeX t)
      (evil-define-key 'normal outline-minor-mode-map (kbd "SPC") 'evil-toggle-fold)

      ;; compile

      ;; (evil-define-key 'normal LaTeX-mode-map (kbd ", l") 'TeX-command-master)
      ;; do not query the user before saving each file with TeX-save-document
      (setq TeX-save-query nil) 
      (evil-define-key 'normal LaTeX-mode-map (kbd ", l") 'TeX-command-run-all)
      (evil-define-key 'normal LaTeX-mode-map (kbd ", v") 'TeX-view)
      (evil-define-key 'normal LaTeX-mode-map (kbd "M-w") 'LaTeX-fill-region)

      ;; sync

      ;; Enable the clicking feature of the sync
      (add-hook 'LaTeX-mode-hook
			(lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view))
			)
      (setq TeX-PDF-mode t)	      ;; Compile documents to PDF by default
      ;; Use Skim as viewer, enable source <-> PDF sync
      ;; make latexmk available via C-c C-c
      ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
      (add-hook 'LaTeX-mode-hook (lambda ()
							       (push
				'("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file")
				TeX-command-list)))
      (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
      ;; use Skim as default pdf viewer
      ;; Skim's displayline is used for forward search (from .tex to .pdf)
      ;; option -b highlights the current line; option -g opens Skim in the background
      (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
      (setq TeX-view-program-list
	'(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

      ;; AucTeX
      ;; (setq reftex-default-bibliography '("~/Dropbox/Bib_bibdesk.bib") )
      ;; (setq helm-bibtex-bibliography '("~/Dropbox/Bib_bibdesk.bib") )
      ;; (setq reftex-default-bibliography '("/Users/chongchonghe/Documents/bib_tmp.bib"))
      ;; (setq helm-bibtex-bibliography '("/Users/chongchonghe/Documents/bib_tmp.bib"))

      ;; ;; (setq TeX-command-force "XeLaTeX")
      ;; (setq TeX-command-force "latexmk")

      (setq TeX-parse-self t)

      (setq TeX-save-query nil)

      (add-hook 'LaTeX-mode-hook
			(lambda()
			      (local-set-key [C-tab] 'TeX-complete-symbol)))

      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

      (require 'smartparens-config)
      (add-hook 'LaTex-mode-hook #'smartparens-mode)

      ;; keybindings
      ;; (define-key outline-mode-map [M-left] 'outline-hide-body)
      ;; (define-key outline-mode-map [M-right] 'outline-show-all)
      ;; (define-key outline-mode-map [M-up] 'outline-previous-heading)
      ;; (define-key outline-mode-map [M-down] 'outline-next-heading)
      ;; (define-key outline-mode-map [C-M-left] 'outline-hide-sublevels)
      ;; (define-key outline-mode-map [C-M-right] 'outline-show-children)
      ;; (define-key outline-mode-map [C-M-up] 'outline-previous-visible-heading)
      ;; (define-key outline-mode-map [C-M-down] 'outline-next-visible-heading)

      (defun turn-on-outline-minor-mode () (outline-minor-mode 1))
      (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
      (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)

      (defun turn-on-flycheck-mode () (flycheck-mode 1))
      (add-hook 'LaTeX-mode-hook 'turn-on-flycheck-mode)
      (add-hook 'LaTeX-mode-hook 'turn-on-flycheck-mode)

      (global-set-key [M-left] 'outline-hide-body)
      (global-set-key [M-right] 'outline-show-all)
      (global-set-key [M-up] 'outline-previous-heading)
      (global-set-key [M-down] 'outline-next-heading)
      (global-set-key [C-M-left] 'outline-hide-sublevels)
      (global-set-key [C-M-right] 'outline-show-children)
      (global-set-key [C-M-up] 'outline-previous-visible-heading)
      (global-set-key [C-M-down] 'outline-next-visible-heading)
      )

(add-to-list
 'display-buffer-alist
 (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
;; not working
(call-process-shell-command "osascript&" nil 0)

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

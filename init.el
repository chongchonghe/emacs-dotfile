  ;; -*- mode: emacs-lisp -*-

  ;; (defun tangle-init ()
  ;;       "If the current buffer is 'init.org' the code-blocks are
  ;; tangled, and the tangled file is compiled."
  ;;       (when (equal (buffer-file-name)
  ;; 			       ;; (expand-file-name (concat user-emacs-directory "init.org")))
  ;; 			       "/Users/chongchonghe/dotfiles/emacs/init.org")
  ;; 	;; Avoid running hooks when tangling.
  ;; 	(let ((prog-mode-hook nil))
  ;; 	      (org-babel-tangle)
  ;; 	      (byte-compile-file (concat user-emacs-directory "init.el")))))
  ;; (add-hook 'after-save-hook 'tangle-init)

  ;; Make startup faster by reducing the frequency of garbage
  ;; collection.  The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold 100000000)

  (package-initialize)
  (add-to-list 'load-path "~/.emacs.d/pkgs")

  ;; (defvar myPackages
  ;;   '(better-defaults
  ;;     ;;ein
  ;;     elpy
  ;;     flycheck
  ;;     ;;spolky   ; theme?
  ;;     ;; py-autopep8
  ;;     ))

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

  ;; This is only needed once, near the top of the file
  (require 'bind-key)
  (eval-when-compile
    ;; Following line is not needed if use-package.el is in ~/.emacs.d
    (add-to-list 'load-path "~/.emacs.d/elpa")
    (require 'use-package))

  ;; (add-to-list 'load-path "~/.emacs.d/elpa")
  ;; (require 'use-package)

  ;; (unless (package-installed-p 'use-package)
  ;;   (package-refresh-contents)
  ;;   (package-install 'use-package))

  (setq use-package-always-ensure t)
  (setq use-package-verbose t)

  ;; (use-package ein)
  ;; (use-package ein-notebook)
  ;; (use-package ein-subpackages)
  ;; (use-package better-defaults)

  ;; remove backup files (e.g. README.md~)
  (setq make-backup-files nil)
  ;; enable line numbers globally
  (global-linum-mode t) 
  ;; Shift-arrow to swith windows
  (windmove-default-keybindings)
  ;; (global-unset-key (kbd "C-x C-c"))
  ;; (global-unset-key (kbd "M-`")) ; not working
  (setq default-fill-column 80)
  ;; Auto revert mode
  (global-auto-revert-mode 1)
  ;; Keep track of loading time
  (defconst emacs-start-time (current-time))
  ;; start server at startup
  (server-start)
  ;; Disable welcome screen
  (setq inhibit-startup-screen t)
  ;; Search only visible 
  (setq search-invisible nil)
  (setq column-number-mode t)
  (setq x-select-enable-clipboard t)
  ;; (desktop-save-mode 1)
  ;; end file with new line ("\n")
  (setq mode-require-final-newline t)

  ;; Use cmd key for meta
  ;; https://superuser.com/questions/297259/set-emacs-meta-key-to-be-the-mac-key
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super)

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

  (setq-default
   ;; Column Marker at 80
   whitespace-line-column 80
   whitespace-style       '(face lines-tail))
  (add-hook 'prog-mode-hook #'whitespace-mode)

  ;; set encoding
  (set-language-environment "utf-8")
  (set-default-coding-systems 'utf-8)
  ;; (set-buffer-file-coding-system 'utf-8-unix)
  ;; (set-clipboard-coding-system 'utf-8-unix)
  ;; (set-file-name-coding-system 'utf-8-unix)
  ;; (set-keyboard-coding-system 'utf-8-unix)
  ;; (set-next-selection-coding-system 'utf-8-unix)
  ;; (set-selection-coding-system 'utf-8-unix)
  ;; (set-terminal-coding-system 'utf-8-unix)
  ;; (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (setq compile-command "make ")
  (global-set-key (kbd "C-c r") #'recompile)

  (defun no-auto-fill ()
    "Turn off auto-fill-mode."
    (auto-fill-mode -1)
    (setq word-wrap t)
    )

(use-package default-text-scale
  :defer 2)

  ;; (add-to-list 'load-path "~/.emacs.d/evil")
  ;; (require 'evil)
  ;; (evil-mode 1)

  ;; (use-package evil
  ;;   :ensure t
  ;;   :defer .1
  ;;   :init
  ;;   (setq evil-want-integration nil) ;; required by evil-collection
  ;;   (setq evil-want-keybinding nil)
  ;;   (setq evil-search-module 'evil-search)
  ;;   (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  ;;   (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  ;;   :config
  ;;   (evil-mode 1)
  ;;   (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  ;;   (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;;   ;; Make horizontal movement cross lines
  ;;   (setq-default evil-cross-lines t)
  ;;   (setq key-chord-two-keys-delay 0.4)
  ;;   (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  ;;   )

  ;; DO NOT PUT EVIL INTO USE-PACKAGE because other part of this dotfile 
  ;; relies on it
  (require 'evil)
  ;; (setq evil-want-integration nil) ;; required by evil-collection
  ;; (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (setq key-chord-two-keys-delay 0.4)
  ;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  ;; (define-key evil-insert-state-map (kbd "M-v") 'yank)

  ;; (use-package evil-collection
  ;;   :after evil
  ;;   :ensure t
  ;;   :config
  ;;   (evil-collection-init))

;; For python
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; julia
(add-hook 'julia-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; ;; For ruby
;; (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(require 'org)
;; inline image size
;; (setq org-image-actual-width nil)
(setq org-image-actual-width 500)
(setq org-hide-emphasis-markers t)
;; startup: showeverything
(setq org-startup-folded nil)

(use-package htmlize
  :load-path "/Users/chongchonghe/dotfiles/emacs/packages/emacs-htmlize/htmlize.el")

  (defun org-toggle-hide-emphasis-markers ()
    "Toggle org-hide-emphasis-markers"
    (interactive)
    (if org-hide-emphasis-markers
        (setq org-hide-emphasis-markers nil)
      (setq org-hide-emphasis-markers t)))

  ;; (setq org-descriptive-links nil)

  (setq org-startup-indented t)

  ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (auto-fill-mode -1)
  (remove-hook 'text-mode-hook #'turn-on-auto-fill)

  (setq outline-blank-line 2)

(defun org-show-two-levels ()
  (interactive)
  (org-content 2))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 2") 'org-show-two-levels))
(defun org-show-three-levels ()
  (interactive)
  (org-content 3))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 3") 'org-show-three-levels))
;; Evaluate it after startup
;; (add-hook 'org-mode-hook #'org-show-two-levels)
(add-hook 'org-view-mode-hook '(text-scale-adjust))

  ;; (use-package org-cliplink
  ;;   :bind ("C-c C-p" . 'org-cliplink))
  (require 'org-cliplink)
  (define-key org-mode-map (kbd "C-c C-p") 'org-cliplink)

  ;; (setq org-modules '(org-tempo))

;; (use-package org-autolist
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
;;   )

  (defun my-org-mode-config ()
		(local-set-key "\M-n" 'outline-next-visible-heading)
		(local-set-key "\M-p" 'outline-previous-visible-heading)
		;; table
		(local-set-key "\C-\M-w" 'org-table-copy-region)
		(local-set-key "\C-\M-y" 'org-table-paste-rectangle)
		(local-set-key "\C-\M-l" 'org-table-sort-lines)
		;; display images
		(local-set-key "\M-I" 'org-toggle-iimage-in-org)
		;; TODOlist
		;; fix tab
		;; (local-set-key "\C-y" 'yank)
		(local-set-key "\M-h" 'windmove-left)
		(local-set-key "\C-cl" 'grg-store-link)
		(local-set-key "\C-cb" 'org-switchb)
		(local-set-key "\C-cp" 'org-display-inline-images)
		(setq-local truncate-lines 'nil)
		;; (org-indent-mode)  ;; not working?
		)
  (add-hook 'org-mode-hook 'my-org-mode-config)

  (with-eval-after-load 'evil-maps
		(define-key evil-normal-state-map (kbd "C-p") 'org-latex-preview)
		(define-key evil-normal-state-map (kbd "C-M-p") 'org-fragtog-mode))
  ;; (define-key org-mode-map (kbd "C-p") 'org-latex-preview)

  ;; (evil-define-key 'normal org-mode-map (kbd ", ,") 'org-insert-structure-template)

  ;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq org-super-agenda-header-map (make-sparse-keymap))
  )

;; 'evil-org-agenda is replaced by the following
;; (define-key org-agenda-mode-map "j" 'evil-next-line)
;; (define-key org-agenda-mode-map "k" 'evil-previous-line)

;; (use-package org-agenda
;;   :bind (:map org-agenda-mode-map
;; 	      ("j" . org-agenda-next-item)
;; 	      ("k" . org-agenda-previous-time)))

;; (use-package org-agenda
;;   :config
;;   (define-key org-agenda-mode-map (kbd "j") #'org-agenda-next-item)
;;   (define-key org-agenda-mode-map (kbd "k") #'org-agenda-previous-item))

;; (use-package org-evil
;;   :ensure t
;;   :after org
;;   )

  ;; (font-lock-add-keywords
  ;;  'org-mode
  ;;  '(("^ *\\([-]\\) "
  ;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; (use-package org-bullets
  ;;       :init
  ;;       (add-hook 'org-mode-hook #'org-bullets-mode)
  ;;       (setq org-bullets-bullet-list '("⚫" "◆" "◉" "▶" "◇"))
  ;;       )
  (setq-default org-list-indent-offset 4)
  (use-package org-superstar              ; supersedes `org-bullets'
				:ensure
				:after org
				:config
				;; Every non-TODO headline now have no bullet
				(setq org-superstar-headline-bullets-list '("\u200b"))
				(setq org-superstar-leading-bullet "\u200b")
				(setq org-superstar-item-bullet-alist
	  '((?+ . ?+)
					(?* . ?➤)
					(?- . ?•)))
				;; Enable custom bullets for TODO items
				(setq org-superstar-special-todo-items t)
				(setq org-superstar-todo-bullet-alist
	  '(("TODO" "☐")
					("NEXT" "✒")
					("HOLD" "✰")
					("WAIT" "☕")
					("CXLD" "✘")
					("DONE" "✔")))
				(org-superstar-restart))
  ;; (setq org-ellipsis "⤵")
  ;; (setq org-ellipsis "▼")

  ;; simple version, only change font size
  ;; (custom-set-faces
  ;;   '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
  ;;   '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  ;;   '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
  ;;   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  ;;   '(org-level-5 ((t (:inherit outline-5))))
  ;;   '(org-level-6 ((t (:inherit outline-6))))
  ;;   '(org-level-7 ((t (:inherit outline-7))))
  ;;   '(org-document-title ((t (:height 2.0 :underline nil))))
  ;; )

  ;; complex version: change font as well
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (with-eval-after-load 'org
    (setq word-wrap t)
    )

  ;; check (custom-theme-set-faces) in the appearance section

  ;; (custom-set-faces
  ;;   ;; '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
  ;;   ;; '(org-document-title ((t (:height 2.0 :underline nil))))
  ;;   '(mu4e-view-face ((t (:inherit default :height 1.2))))
  ;; )

  ;; (setq org-ellipsis "⤵")

  (defvar mv-iframe-format
    ;; You may want to change your width and height.
    (concat "<video"
	    " height=\"500\""
	    " style=\"display:block; margin: 0 auto;\" controls>"
	    " <source"
	    " src=\"%s\""
	    " type=\"video/mp4\">"
	    "</video>"))

  (org-add-link-type
   "mv"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
	      handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format mv-iframe-format
		     path (or desc "")))
       (latex (format "\href{%s}{%s}"
		      path (or desc "video"))))))

  (defvar audio-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe"
	    " width=\"600\""
	    " height=\"60\""
	    " style=\"display:block; margin: 0\""
	    " src=\"%s\">"
	    "</iframe>"))

  (org-add-link-type
   "audio"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
	      handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format audio-iframe-format
		     path (or desc "")))
       (latex (format "\href{%s}{%s}"
		      path (or desc "audio"))))))

  ;; <tab> for 'indent-for-tab-command'
  ;; (evil-define-key 'insert org-mode-map (kbd "C-t") #'indent-for-tab-command)

  ;; load shared .el followed by Emacs specific config
  ;;(load-file "~/.my-elips/org.el")

  ;; (require 'org-mu4e)

;; (require 'ess-site)
(setq  inferior-julia-program-name "/Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (julia . t)
   (shell . t)
   (python . t)
   (ipython . t)
   (abc . t)))

(setq
 org-export-babel-evaluate nil
 org-confirm-python-evaluate nil
 org-confirm-babel-evaluate nil
 org-confirm-C++-evaluate nil
 )

  (defvar org-babel-eval-verbose t
    "A non-nil value makes `org-babel-eval' display")

  (defun org-babel-eval (cmd body)
    "Run CMD on BODY.
  If CMD succeeds then return its results, otherwise display
  STDERR with `org-babel-eval-error-notify'."
    (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
      (with-current-buffer err-buff (erase-buffer))
      (with-temp-buffer
        (insert body)
        (setq exit-code
              (org-babel--shell-command-on-region
               (point-min) (point-max) cmd err-buff))
        (if (or (not (numberp exit-code)) (> exit-code 0)
                (and org-babel-eval-verbose (> (buffer-size err-buff) 0))) ; new condition
            (progn
              (with-current-buffer err-buff
                (org-babel-eval-error-notify exit-code (buffer-string)))
              nil)
          (buffer-string)))))

  (setq org-babel-eval-verbose t)

(setq org-src-preserve-indentation t)

(use-package ob-async)

(use-package ob-ipython)

(evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
(evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo-list)
(evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo-list)
(define-key evil-normal-state-map (kbd "C-a") 'org-agenda)
(setq org-directory "~/Dropbox/orgfiles")

(with-eval-after-load 'org
  ;; (setq org-directory "/Users/chongchonghe/Dropbox/orgfiles")
  ;; (setq org-agenda-files '("~/Dropbox/orgfiles/todos.org"
  ;; 			   "~/Dropbox/orgfiles/notes.org"
  ;; 			   "~/Dropbox/orgfiles/tasks"))
  (setq org-agenda-files '("~/Dropbox/orgfiles"))
  ;; (setq org-agenda-files
  ;; 	"~/Dropbox/orgfiles/agenda.org")
  (setq org-default-notes-file "~/Dropbox/orgfiles/work.org")
  (setq org-agenda-confirm-kill t)
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window)))

  (setq org-agenda-span 14
	org-agenda-start-on-weekday nil
	org-agenda-start-day "-3d")

  ;; ; ;; org-agenda
  ;; (use-package org-projectile
  ;;   :bind (("C-c n p" . org-projectile-project-todo-completing-read)
  ;; 	   ;; ("C-c c" . org-capture)
  ;; 	   ;; ("C-c a" . org-agenda)
  ;; 	   )
  ;;   :config
  ;;   (progn
  ;;     (setq org-projectile-projects-file "~/Dropbox/orgfiles/tasks.org")
  ;;     (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  ;;     (push (org-projectile-project-todo-entry) org-capture-templates))
  ;;   :ensure t)

(setq org-default-priority ?A)
(setq org-highest-priority ?A)
(setq org-lowest-priority ?D)
;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#FF0000" :weight bold))
			   (?B . (:foreground "#FF9815" :weight bold))
			   (?C . (:foreground "#68DF40"))
			   (?D . (:foreground "#11D3FF"))))
;;Different bullets
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "DOIN(o!)" "WAIT(w!)" "FLUP(f!)" "REFI(r!)" "|" "SCHE(s!)" "CXLD(c!)" "DONE(d!)"))
      org-todo-keyword-faces
      '(("TODO" . org-warning)
	("DOIN" . (:foreground "yellow"))
	("FLUP" . (:foreground "magenta"))
	("REFI" . (:foreground "#A52A2A"))
	;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	("CXLD" . (:foreground "gray"))
	("NEXT" . "#008080")
	("DONE" . "#333"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      )

(defun my-org-set-dark-todo-faces ()
  (setq org-todo-keyword-faces
	'(("TODO" . org-warning)
	  ("DOIN" . (:foreground "yellow"))
	  ("FLUP" . (:foreground "magenta"))
	  ("REFI" . (:foreground "#A52A2A"))
	  ;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	  ("CXLD" . (:foreground "gray"))
	  ("NEXT" . "#008080")
	  ("DONE" . "#333"))))

(defun my-org-set-light-todo-faces ()
  (setq org-todo-keyword-faces
	'(("TODO" . org-warning)
	  ("DOIN" . (:foreground "red"))
	  ("FLUP" . (:foreground "magenta"))
	  ("REFI" . (:foreground "#A52A2A"))
	  ;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
	  ("CXLD" . (:foreground "gray"))
	  ("NEXT" . "#008080")
	  ("DONE" . "#333"))))

(with-eval-after-load 'org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "" "Todos")
           "* TODO [#%^{Priority?||A|B|C|D}] %?\n %u\n\n\n" :empty-lines-before 1)
          ("o" "Doing" entry (file+headline "" "Todos")
           "* DOIN [#%^{Priority?||A|B|C|D}] %?\n %u\n\n" :empty-lines-before 1)
          ("a" "Todo with link" entry (file+headline "" "Todos")
           "* TODO [#A] %?\n  %U\n  %a\n\n" :empty-lines-before 1)
          ("n" "Notes" entry (file+headline "" "General Notes")
           "* REFI %? :NOTE:\n%U\n\n" :empty-lines-before 1)
          ("m" "Meeting" entry (file+headline "" "Meetings")
           "* Meeting with %? :MEETING:\n%U\n\n" :empty-lines-before 1)
          ("i" "Ideas" entry (file+headline "" "Ideas")
           "* %?\n  %u\n\n" :empty-lines-before 1)
          ("s" "Scheduled" entry (file+headline "" "Todos")
           "* [#A] %^{Title}\nSCHEDULED: %^t\n%u\n%?\n\n" :empty-lines-before 1)
          ("e" "Event" entry (file+headline "" "Todos")
           "* %^{This is a?||TODO |NEXT |FLUP |DOIN |SCHE |REFI}%^{Title}\nSCHEDULED: %^t\n%t\n%?")
          ("f" "Followup" entry (file+headline "" "Followups")
           "* FLUP [#B] %?\n  %U\n  %a\n\n" :empty-lines-before 1)
          ("l" "Life" entry (file+headline "~/Dropbox/orgfiles/life.org" "Todos")
           "* TODO [#C] %?\n  %U\n  %a\n\n" :empty-lines-before 1)
          ("j" "Journal" entry (file+headline "~/Dropbox/orgfiles/journals.org" "2021")
           "** %? \n\n" :empty-lines-before 1)
          ;; ("w" "Work" entry (file+headline "" "Work")
          ;;  "* DOIN [#A] %? :WORK:\n%U\n\n" :empty-lines-before 1)
          ;; ("g" "General todo" entry (file+headline "/Users/chongchonghe/tasks.org" "Todos")
          ;;  "* TODO [#B] %?\n %a" :empty-lines 1)
          )
        ))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

  (setq org-present-text-scale 2)

  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)
                   ;;(toggle-frame-fullscreen)
                   ))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write)
                   ;;(toggle-frame-fullscreen)
                   ))))

;; (with-eval-after-load 'org
;;   (use-package org-toggl
;;     :init
;;     (setq toggl-auth-token "ce3e8fc3922edda6986a6e729509338f")
;;     (setq org-toggl-inherit-toggl-properties t)
;;     :load-path "/Users/chongchonghe/dotfiles/emacs/packages"
;;     :config
;;     (toggl-get-projects)
;;     (org-toggl-integration-mode)
;;     ;; remove clock-out since it failed at stopping toggl timer
;;     (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
;;     ;; bind C-c i to clock-in then clock-out
;;     (define-key org-mode-map (kbd "C-c i")
;;       (lambda () (interactive) (org-clock-in) (sit-for 2) (org-clock-out)))
;;     )
;;   )

(use-package org-toggl
  :after org
  :defer 3
  :init
  (setq toggl-auth-token "ce3e8fc3922edda6986a6e729509338f")
  (setq org-toggl-inherit-toggl-properties t)
  :load-path "/Users/chongchonghe/dotfiles/emacs/packages"
  :config
  (toggl-get-projects)
  (org-toggl-integration-mode)
  ;; remove clock-out since it failed at stopping toggl timer
  (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
  ;; bind C-c i to clock-in then clock-out
  (define-key org-mode-map (kbd "C-c i")
    (lambda () (interactive) (org-clock-in) (sit-for 2) (org-clock-out))))

  (setq org-file-apps
		'(("\\.docx\\'" . default)
	  ("\\.mm\\'" . default)
	  ("\\.x?html?\\'" . default)
	  ("\\.pdf\\'" . default)
	  ("\\.md\\'" . default)
	  ("\\.png\\'" . default)
	  (auto-mode . emacs)))

  (use-package exec-path-from-shell
	:ensure t
	:config
	(when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; (use-package ox-md)
  (setq org-export-backends '(ascii html md icalendar latex odt))

(defun my-org-html-postamble (plist)
  (concat "<p>Author: Chong-Chong He</p>"
	  (format "<p>Last updated: %s</p>" (format-time-string "%Y-%b-%d"))
	  "<a href='https://www.astro.umd.edu/~chongchong/'>www.astro.umd.edu/~chongchong/</a>"
	  ))
(setq org-html-postamble 'my-org-html-postamble)

;; (setq org-export-html-postamble-format 
;; 	      '(("en" "<p class=\"author\">Author: %a (%e)</p><p class=\"date\">Last Updated %d.</p>")))

;; (setq org-html-postamble-format
;; ;; (setq org-export-html-postamble-format 
;;       '(("en" "<p class=\"author\">Author: %a (%e)</p>
;; Last updated: <p class=\"date\">Date: %d</p>
;; <p class=\"creator\">Generated by %c</p>
;; <p class=\"xhtml-validation\">%v</p>
;; ")))

(setq org-html-validation-link nil)

  (defun my-website-html-postamble (options)
	(concat "<hr>"
			(if (and (plist-get options ':keywords) (not (string= (plist-get options ':keywords) "")))
				(format "<p>Keywords: %s</p>" (plist-get options ':keywords))
				"")
			(format "<p class=\"date\">Modified: %s</p>" (format-time-string "%Y-%m-%d %H:%M:%S"))
			(format "<p>Copyright (c) %s %s</p>"
					(car (split-string (car (plist-get options ':date)) "-")) ;; TODO: get from custom document option
					(car (plist-get options ':author)))
			(format "<p>%s</p>" (plist-get options ':creator))))

(require 'ox-publish)

(global-hl-line-mode 1)
(setq org-src-fontify-natively t)  
(add-to-list 'load-path "/Users/chongchonghe/dotfiles/emacs/packages/emacs-htmlize")
(require 'htmlize)

;; (setq org-html-postamble nil)
(setq org-html-postamble
      (concat "<p>Author: %a</p>"
              "<p>%d</p>"
              "<p><a href='https://www.astro.umd.edu/~chongchong/'>"
              "www.astro.umd.edu/~chongchong/</a></p>"))

;; sitemap function
(defun @-org-publish-org-sitemap (title list)
  "Sitemap generation function."
  (concat (format "#+TITLE: %s\n" title)
          "\n#+SETUPFILE: ../style/default.setup\n"
          "#+OPTIONS: toc:nil\n"
          (org-list-to-subtree list)
          "\n"
          ))

(setq org-publish-project-alist
      '(("body"
         ;; generic
         :base-directory "."
         :base-extension "org"
         :publishing-directory "../public"
         :recursive t
         :language en
         ;; html
         :publishing-function org-html-publish-to-html
         ;; sitemap
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Index"
         ;; :sitemap-sort-files anti-chronologically
         ;; :sitemap-file-entry-format "%d - %t"
         ;; :sitemap-function my-website-sitemap-function
         ;; :sitemap-function org-publish-org-sitemap
         :sitemap-function @-org-publish-org-sitemap
         ;; :html-home/up-format "<div> <a accesskey='h' href='index.html'> UP </a> | <a accesskey='H' href='index.html'> HOME </a> </div>"
         )
        ("css"
         :base-directory "../style/"
         :base-extension "css\\|js"
         :publishing-directory "../public/css"
         :publishing-function org-publish-attachment
         :recursive t)
        ("attach"
         :base-directory "attach/"
         ;; :base-extension "png\\|jpg\\|ico"
         :base-extension "png\\|jpg\\|ico\\|svg"
         :publishing-directory "../public/attach"
         :publishing-function org-publish-attachment
         :recursive t)
        ("all" :components ("body" "css" "attach"))))

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

(setq
 org-ref-bibliography-notes "~/Academics/org-ref-notes.org"
 org-ref-default-bibliography '("~/folders/BIB_HE.bib")
 org-ref-pdf-directory "~/Academics/Papers/"
 )

(use-package plain-org-wiki
  :ensure t
  :load-path "/Users/chongchonghe/dotfiles/emacs/packages/plain-org-wiki/"
  :config
  (setq plain-org-wiki-directory "~/org/wiki/org")
  (setq plain-org-academic-directory "~/org/astronomy/org")
  (setq plain-org-wiki-extra-dirs '("~/org/astronomy/org"))
  (global-set-key (kbd "C-M-w") 'plain-org-wiki))

(org-add-link-type "message"
 (lambda (id)
  (shell-command
   ;; (concat "open -a mail.app message:" id))))
   (concat "open message:" id))))

(use-package org-super-agenda
  :defer 2
  :config
  (org-super-agenda-mode)
  )

(setq org-super-agenda-groups
       '((:auto-category t)))

;; (let ((org-super-agenda-groups
;;        '((:auto-category t))))
;;   (org-agenda-list))

  (setq-default mode-require-final-newline nil)

  (use-package yasnippet
					:diminish yas-minor-mode
					;; :init (yas-global-mode)
					:ensure t
					:config
					(setq yas-snippet-dirs '("~/dotfiles/emacs/snippets/yasnippet-snippets-20210105.1346/snippets" "~/dotfiles/emacs/snippets/personal"))
					(yas-global-mode 1)
					;; (add-to-list #'yas-snippet-dirs "~/dotfiles/emacs/snippets/yasnippet-snippets-20210105.1346/snippets")
					;; (add-to-list #'yas-snippet-dirs "~/dotfiles/emacs/snippets/personal")
					(yas-reload-all)
					;; (progn
					;;   (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
					;;   ;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
					;;   (setq yas-installed-snippets-dir "~/dotfiles/emacs/snippets")
					;;   (setq yas-expand-only-for-last-commands nil))
					)

  (use-package mu4e
    :defer t
    :commands (mu4e)
    :bind (("C-c m" . mu4e)
           )
    :load-path "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e"
    :config

  ;; (require 'mu4e)
  (setq mail-user-agent 'mu4e-user-agent ;; use mu4e for e-mail in emacs
        ;; for mbsync, when move a message to All Mail as archiving
        mu4e-change-filenames-when-moving t
        mu4e-view-show-addresses t
        mu4e-maildir "~/Maildir"
        mu4e-get-mail-command "mbsync umd"
        mu4e-attachment-dir "~/Documents/MailAttachments"
        mu4e-update-interval 180
        ;; mu4e-update-interval nil  ; stop mu4e from updating mails
        ;; disable the message in the minibuffer when index/mails are updated
        mu4e-hide-index-messages nil
        )

  (add-hook 'mu4e-compose-mode-hook #'no-auto-fill)
  (add-hook 'mu4e-view-mode-hook #'no-auto-fill)
  (add-hook 'mu4e-view-mode-hook #'mixed-pitch-mode)

  ;; variable-pitch seems not to apply to mu4e-view-body-face, so I
  ;; need to set it up separately
  ;; Ref: https://github.com/djcb/mu/blob/master/mu4e/mu4e-vars.el
  (set-face-attribute 'mu4e-view-body-face nil :height 1.2)

  (define-key mu4e-view-mode-map (kbd "j") 'next-line)
  (define-key mu4e-view-mode-map (kbd "k") 'previous-line)
  (define-key mu4e-headers-mode-map (kbd "j") 'next-line)
  (define-key mu4e-headers-mode-map (kbd "k") 'previous-line)
  (define-key mu4e-main-mode-map (kbd "U") 'mu4e-update-index)

  (setq mu4e-headers-fields
        '(
          (:human-date    . 12)    ;; alternatively, use :human-date
          (:flags         . 6)
          (:from-or-to    . 21)
          (:subject       . 46)
          )) ;; alternatively, use :thread-subject

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq
   sendmail-program "/usr/local/bin/msmtp"
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")
   ;; send-mail-function 'smtpmail-send-it
   ;; message-send-mail-function 'message-send-mail-with-sendmail
   message-send-mail-function 'smtpmail-send-it
   user-full-name "ChongChong He"
   user-mail-address "che1234@umd.edu"
   smtpmail-smtp-user "che1234@umd.edu"
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   ;; smtpmail-stream-type 'starttls
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   ;; smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
   smtpmail-debug-info t
   mu4e-compose-keep-self-cc t
   mu4e-compose-dont-reply-to-self nil
   ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587 "che1234@umd.edu" nil))
   ;; testing: removing all the folder def since I don't need
   ;; it. Later will set folder shortcuts
   ;; Sent mail??? not sure
   ;; mu4e-sent-folder "/umd/[Gmail].All Mail"
   mu4e-sent-folder "/umd/[Gmail].Sent Mail"
   mu4e-refile-folder "/umd/[Gmail].All Mail"
   mu4e-drafts-folder "/umd/[Gmail].Drafts"
   mu4e-trash-folder "/umd/[Gmail].Trash"
   ;; don't save message to Sent Messages, IMAP takes care of this
   ;; folders
   )

  (setq 
   mu4e-maildir-shortcuts
   '(("/umd/INBOX"               . ?i)
		 ("/umd/[Gmail].Sent Mail"       . ?s)
		 ("/umd/[Gmail].Drafts"          . ?d)
		 ("/umd/[Gmail].Starred"         . ?*)
		 ;; ("/umd/[Gmail].All Mail"        . ?a)
		 ("/umd/high/TODO"               . ?t)
		 ("/umd/high/academic"           . ?a)
		 ("/umd/high/marked"             . ?m)
		 ("/umd/high/followup"           . ?f)
		 ("/umd/t: reference"            . ?r)
		 ("/umd/t: later"                . ?l)
		 ("/umd/t: history"              . ?h)
		 )
   )

  (defun htmlize-and-send ()
    "When in an org-mu4e-compose-org-mode message, htmlize and send it."
    (interactive)
    (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
      (org-mime-htmlize)
      (org-mu4e-compose-org-mode)
      (message-send-and-exit)))

    (add-hook 'mu4e-compose-mode-hook
              (defun my-add-bcc ()
                "Add a cc: header."
                (save-excursion (message-add-header "Cc: che1234@umd.edu\n"))))

    ;; ;; from https://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
    ;; (add-hook 'mu4e-compose-mode-hook
    ;;           (lambda()
    ;;             (let* ((ctx (mu4e-context-current))
    ;;                    (name (if ctx (mu4e-context-name ctx))))
    ;;               (when name
    ;;                 (cond
    ;;                  ((string= name "astro")
    ;;                   (save-excursion (message-add-header "Cc: chongchong@astro.umd.edu\n")))
    ;;                  ((string= name "terpmail")
    ;;                   (save-excursion (message-add-header "Cc: che1234@terpmail.umd.edu\n")))
    ;;                  ((string= name "umd")
    ;;                   (save-excursion (message-add-header "Cc: che1234@umd.edu\n")))
    ;; 		 )))))

  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-format "On %a, %b %d %Y at %R, %f wrote:\n")

  (setq mu4e-enable-notifications t)
  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    ;; (mu4e-alert-set-default-style 'notifications)) ; For Linux.
    ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for Linux
    (mu4e-alert-set-default-style 'notifier))   ; For macOS (through the
                                                ; terminal notifier app).
    ;; (mu4e-alert-set-default-style 'growl))      ; Alternative for macOS.
  ;; Mode Line display of unread emails
  ;; Display of the unread email count in the mode-line
  ;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-disable-mode-line-display)
  ;; adding the following snippet to your init file, will instruct
  ;; mu4e-alert to only display the number of unread emails.
  (setq mu4e-alert-email-notification-types '(subjects))
  ;; (setq mu4e-alert-email-notification-types '(count))

  (setq mu4e-html2text-command
        "textutil -stdin -format html -convert txt -stdout")

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; (setq mu4e-compose-signature
  ;;    "ChongChong He\n")
  (defun my-mu4e-choose-signature ()
    "Insert one of a number of sigs"
    (interactive)
    (let ((message-signature
           (mu4e-read-option "Signature:"
                             '(("formal" .
                                (concat
                                 "Chong-Chong He\n"
                                 "PhD Candidate, Department of Astronomy\n"
                                 "University of Maryland, College Park\n"
                                 "che1234@umd.edu\n"
                                 "http://www.astro.umd.edu/~chongchong"))
                               ("informal" .
                                (concat
                                 "Best,\n"
                                 "ChongChong\n")
                                )
                               ))))
      (message-insert-signature)))
  (add-hook 'mu4e-compose-mode-hook
            (lambda () (local-set-key (kbd "C-c C-w s") #'my-mu4e-choose-signature)))

  (load-file "/Users/chongchonghe/.spacemacs.d/private/mu4e-thread-folding.el")
  (define-key 'mu4e-headers-mode-map (kbd "TAB") 'mu4e-headers-toggle-thread-folding)
  ;;(add-hook 'mu4e-headers-found-hook 'mu4e-headers-fold-all)

  (add-to-list 'mu4e-bookmarks
               '( :name  "Since 2021-1-1"
                         :query "maildir:/umd/INBOX AND date:20210101..now"
                         :key ?r))

  )

  (use-package mu4e-conversation
		:after mu4e
		)

  (use-package helm-flx
    :init (helm-flx-mode +1))

(use-package helm
  ;; :bind
  ;; (("C-M-z" . helm-resume)
  ;;  ("C-x C-f" . helm-find-files)
  ;;  ("C-h b" . helm-descbinds)
  ;;  ("C-x C-r" . helm-mini)
  ;;  ("C-x M-o" . helm-occur)
  ;;  ("M-y" . helm-show-kill-ring)
  ;;  ("C-h a" . helm-apropos)
  ;;  ("C-h m" . helm-man-woman)
  ;;  ("M-g >" . helm-ag-this-file)
  ;;  ("M-g ," . helm-ag-pop-stack)
  ;;  ("M-g ." . helm-do-grep)
  ;;  ("C-x C-i" . helm-semantic-or-imenu)
  ;;  ("M-x" . helm-M-x)
  ;;  ("C-x C-b" . helm-buffers-list)
  ;;  ("C-x C-r" . helm-mini)
  ;;  ("C-x b" . helm-mini)
  ;;  ("C-h t" . helm-world-time))
  :init
  (helm-mode 1)
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  ;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  ;; (global-set-key (kbd "M-C-b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "M-C-o") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB work in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  )

(defun my/turn-on-elpy-mode ()
  (interactive)
  (elpy-mode))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  ;; :hook hs-minor-mode
  :bind (:map python-mode-map
	      ("C-c C-c" . compile)
	      ("s-e" . my/turn-on-elpy-mode)
	      )
  :config
  (setq python-shell-interpreter "/Users/chongchonghe/anaconda3/bin/python3")
  (define-key python-mode-map (kbd "C-c C-z") 'run-python)
  (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)
  (setq python-python-command "/Users/chongchonghe/anaconda3/bin/python")
  (defun my-insert-comments (string)
    "Insert \label{ARG} \index{\nameref{ARG}} at point"
    (interactive "sString for \\label and \\nameref: ")
    (insert "##### "  string  " #####"))
  (define-key python-mode-map (kbd "<f5>") 'my-insert-comments)
  (defun my-insert-comments-block (string)
    "Insert \label{ARG} \index{\nameref{ARG}} at point"
    (interactive "sString for \\label and \\nameref: ")
    (insert "# {{{ "  string  " 
  # }}}"))
  (define-key python-mode-map (kbd "<f6>") 'my-insert-comments-block)
  )
(add-hook 'python-mode-hook 'hs-minor-mode)

  ;; (use-package jedi
  ;; 	      :ensure t)

(use-package flycheck)

(use-package elpy
  :bind
  (:map elpy-mode-map
	("C-M-n" . elpy-nav-forward-block)
	("C-M-p" . elpy-nav-backward-block))
  :hook ((elpy-mode . flycheck-mode))
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
					; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
  (setq elpy-shell-echo-output nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-timeout 30)		; elpy autopep8 call timeout
  ) 
;; (use-package elpy
;;   :ensure t
;;   :commands elpy-enable
;;   :init (with-eval-after-load 'python (elpy-enable))
;;   )

  (defun python-args-to-google-docstring (text &optional make-fields)
	"Return a reST docstring format for the python arguments in yas-text."
	(let* ((indent (concat "\n" (make-string (current-column) 32)))
		   (args (python-split-args text))
	   (nr 0)
		   (formatted-args
		(mapconcat
		 (lambda (x)
		   (concat "    " (nth 0 x)
			   (if make-fields (format ": ${%d:arg%d}" (cl-incf nr) nr))
			   (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
		 args
		 indent)))
	  (unless (string= formatted-args "")
		(concat
		 (mapconcat 'identity
			(list "" "Args:" formatted-args)
			indent)
		 "\n"))))

  (use-package anaconda-mode)

(use-package julia-mode)

  (add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
             (setq tab-width 4)))

  (use-package latex
	:defer t
	:ensure auctex
	:mode ("\\.tex\\'" . LaTeX-mode)
	:bind
	(:map LaTeX-mode-map
	  ("M-n" . outline-next-heading)
	  ("M-p" . outline-previous-heading)
	  ("C-c C-c" . TeX-command-run-all)
	  ("C-c l" . TeX-error-overview)
	  ;; ("C-tab" . TeX-complete-symbol) 
	  ("C-c w" . juanjo:textcount))

	:config
	(setq TeX-auto-save t)
	(setq TeX-auto-save t)
	(setq TeX-PDF-mode t)	      ;; Compile documents to PDF by default
	(setq TeX-parse-self t)
	(setq TeX-save-query nil)
	;; (setq-default TeX-master nil) ;; Make emacs aware of multi-file projects
	(add-hook 'LaTeX-mode-hook #'visual-line-mode)
	(add-hook 'LaTeX-mode-hook #'no-auto-fill)
	(add-hook 'LaTeX-mode-hook 'hs-minor-mode)
	(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
	(add-hook 'LaTeX-mode-hook 'flyspell-mode)
	(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
	(evil-define-key 'normal outline-minor-mode-map (kbd "SPC") 'evil-toggle-fold)
	;; CDLaTeX
	(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
	;; (setq reftex-plug-into-auctex t)
	(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
	(setq reftex-plug-into-AUCTeX t)
	(autoload 'helm-bibtex "helm-bibtex" "" t)
	(electric-pair-mode)
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
	)

  (use-package reftex)
  (setq reftex-default-bibliography
			'("/Users/chongchonghe/Academics/Bib/BIB_HE.bib"))
  ;; (setq reftex-default-bibliography
  ;; 	      '("/Users/chongchonghe/Academics/Bib/BIB_HE.bib",
  ;; 	"/Users/chongchonghe/Academics/Bib/Books.bib",
  ;; 	"/Users/chongchonghe/Academics/Bib/Bib_HE_PhD.bib"))

  (setq reftex-external-file-finders
  '(("tex" . "/path/to/kpsewhich -format=.tex %f")
	("bib" . "/path/to/kpsewhich -format=.bib %f")))

  (use-package windresize
		:defer t
		:bind
		("C-c w" . windresize)
		)

  ;; ;; make swithing windows easier
  (global-set-key (kbd "M-p") (kbd "C-- C-x o"))
  (global-set-key (kbd "M-n") (kbd "C-x o"))
  (global-set-key (kbd "M-j") 'windmove-down)
  (global-set-key (kbd "M-k") 'windmove-up)
  (global-set-key (kbd "M-h") 'windmove-left)
  (global-set-key (kbd "M-l") 'windmove-right)
  ;; (global-set-key (kbd "M-j") 'evil-window-down)
  ;; (global-set-key (kbd "M-k") 'evil-window-up)
  ;; (global-set-key (kbd "M-h") 'evil-window-left)
  ;; (global-set-key (kbd "M-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "M-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "M-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "M-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "M-l") #'evil-window-right)

  (use-package transpose-frame)

  (global-set-key (kbd "<f12>") 'next-buffer)
  (global-set-key (kbd "<f11>") 'previous-buffer)

  (global-set-key (kbd "s-v") 'clipboard-yank)
  (global-set-key (kbd "s-k") 'kill-current-buffer)
  (global-set-key (kbd "s-e") 'eval-region)
  (global-set-key (kbd "s-b") 'eval-buffer)
  (global-set-key (kbd "s-c") 'compile)
  (global-set-key (kbd "s-r") 'recompile)
  (global-set-key (kbd "s-,") 'previous-buffer)
  (global-set-key (kbd "s-.") 'next-buffer)
  (global-set-key (kbd "s-j") 'jump-to-register)

  ;; (global-set-key (kbd "M-v") 'evil-paste-after)

  (global-set-key (kbd "M-v") 'clipboard-yank)

(fset 'my/shrink (kbd "C-u 39 C-x {"))

  ;; (use-package smooth-scroll
  ;;       :config
  ;;       (smooth-scroll-mode 1)
  ;;       (setq smooth-scroll/vscroll-step-size 5)
  ;;       )
  ;; (use-package smooth-scrolling
  ;; 	      :config
  ;; 	      (smooth-scrolling-mode 1))

  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
			(format "%s\\|%s"
					  vc-ignore-dir-regexp
					  tramp-file-name-regexp))
  (setq tramp-verbose 1)

  (add-hook 'hs-minor-mode-hook
		(lambda ()
		  ;;(local-set-key (kbd "C-c p") 'hs-toggle-hiding)
		  ;; (local-set-key (kbd "SPC") 'hs-toggle-hiding)
		  (local-set-key (kbd "C-c h") 'hs-hide-all)
		  (local-set-key (kbd "C-c s") 'hs-show-all)
		  (local-set-key (kbd "C-c l") 'hs-hide-level)))
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (evil-define-key 'normal hs-minor-mode-map (kbd "SPC") 'hs-toggle-hiding)

  ;; (add-hook 'hs-minor-mode-hook 'my-hideshow-config)
  ;; (defun my-hideshow-config ()
  ;;       "For use in 'hs-minor-mode-hook'."
  ;;       ;;(local-set-key (kbd "C-c p") 'hs-toggle-hiding)
  ;;       ;; (local-set-key (kbd "SPC") 'hs-toggle-hiding)
  ;;       (local-set-key (kbd "C-c h") 'hs-hide-all)
  ;;       (local-set-key (kbd "C-c s") 'hs-show-all)
  ;;       (local-set-key (kbd "C-c l") 'hs-hide-level)
  ;;       )
  ;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  ;; (evil-define-key 'normal hs-minor-mode-map (kbd "SPC") 'hs-toggle-hiding)

  (defun taskinit ()
		(interactive)
		(split-window-right)
		((kbd "C-u 10 C-x {"))
		(set-frame-height (selected-frame) 60)
		)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  ;; (setq dashboard-banner-logo-title "your custom text")
  ;; (setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)))
  ;; (dashboard-setup-startup-hook)
  )

  (use-package folding
	:ensure t
	:load-path "/Users/chongchonghe/dotfiles/emacs/packages/project-emacs--folding-mode/folding.el"
	:config
	(folding-mode-add-find-file-hook)
	(add-hook 'folding-mode
		  (lambda () (local-set-key [f9] 'folding-toggle-show-hide)))
	)

  (use-package helm-org)
  (use-package f)

(use-package neotree
  :config
  (defun my-neotree-mode-config ()
    "For use in 'neotree-mode-hook'."
    ;; (local-set-key (kbd "j") 'neotree-next-line)
    ;; (local-set-key (kbd "k") 'neotree-previous-line)
    (local-set-key (kbd "C-j") 'neotree-change-root)
    (local-set-key (kbd "C-k") 'neotree-select-up-node)
    (local-set-key (kbd "<return>") 'neotree-enter)
    ;; (with-eval-after-load 'neotree
    ;;   (define-key neotree-mode-map (kbd "<return>") 'neotree-enter))
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
    (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
    (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
    )
  (add-hook 'neotree-mode-hook 'my-neotree-mode-config))

(add-hook 'neotree-mode-hook
	  (lambda () (define-key evil-motion-state-local-map (kbd "g") 'neotree-refresh)))

;; (use-package matlab-mode)

(defun my-light-theme ()
  (interactive)
  (load-theme 'doom-one-light t)
  (set-background-color "#e6e3df")
  (my-org-set-light-todo-faces)
  )
(defun my-dark-theme ()
  (interactive)
  (load-theme 'doom-one t)
  (my-org-set-dark-todo-faces)
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  (my-light-theme)
  )

  ;; Default font
  (set-face-attribute 'default nil :font "Monaco-16")

  ;; mixed-pitch, https://gitlab.com/jabranham/mixed-pitch
  (use-package mixed-pitch
	:hook
	;; If you want it in all text modes:
	(text-mode . mixed-pitch-mode)
	:config
	(set-face-attribute 'variable-pitch nil :family "Helvetica Neue" :height 1.2)
	(set-face-attribute 'fixed-pitch nil :family "Monaco" :height 1.0)
	;; bigger text for org-mode headings
	(custom-theme-set-faces
	 'user
	 `(org-level-4 ((t (:inherit outline-4 :height 1.0))))
	 `(org-level-3 ((t (:inherit outline-3 :height 1.1))))
	 `(org-level-2 ((t (:inherit outline-2 :height 1.2))))
	 `(org-level-1 ((t (:inherit outline-1 :height 1.3))))
	 `(org-document-title ((t (:family "Helvetica Neue" :height 2.0 :underline nil))))
	 )
	)

  (when window-system (set-frame-size (selected-frame) 130 50))

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "PingFang SC"
                                         :size 15)))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c a") 'org-agenda-list)
;; (define-key yas-minor-mode-map (kbd "C-c t") nil)
;; (global-set-key (kbd "C-c t") 'org-todo-list)
;; ;; (bind-key* "C-c t" 'org-todo-list)
;; (global-set-key (kbd "<f9>") 'org-todo-list)

;; (with-eval-after-load 'org
;;   (define-key org-agenda-mode-map (kbd "j") #'org-agenda-next-item)
;;   (define-key org-agenda-mode-map (kbd "k") #'org-agenda-previous-item))

;; (defun my-shrink ()
;;   (interactive)
;;   ;; (funcall (key-binding (kbd "C-u 39 C-x {")))
;;   ;; (call-interactively (key-binding (kbd "C-u 39 C-x {")))
;;   ;; (/ (loop repeat 39 collect (key-binding (kbd "C-u 39 C-x {"))))
;;   ;; (/ (loop repeat n sum (funcall f arg)) n)
;;   ;; (cl-loop repeat 39 (shrink-window-horizontally))
;;   ;; (r 39 'shrink-window-horizontally 'nil')
;;   'shrink-window-horizontally
;;   'shrink-window-horizontally
;;   'shrink-window-horizontally
;;   'shrink-window-horizontally
;;   )

;; Saving Emacs Sessions
;; (desktop-save-mode 1)

;; (fset 'my-shrink (kbd "C-u 43 C-x {"))
(defun my-shrink ()
  (interactive)
  (shrink-window-horizontally 43))
(defun my-todo ()
  (interactive)
  (find-file "~/Dropbox/orgfiles/tasks.org")
  (delete-other-windows)
  (split-window-right)
  (my-shrink)
  )
;; (my-todo)

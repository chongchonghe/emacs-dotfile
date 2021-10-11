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

  ;; Use cmd key for meta
  ;; https://superuser.com/questions/297259/set-emacs-meta-key-to-be-the-mac-key
  ;; (setq mac-option-key-is-meta nil
  ;;       mac-command-key-is-meta t
  ;;       mac-command-modifier 'meta
  ;;       mac-option-modifier 'super)

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

  (defun my/theme-light ()
		(interactive)
		(load-theme 'doom-one-light t))
  (defun my/theme-dark ()
	(interactive)
		(load-theme 'doom-one t))

  (use-package doom-themes
		:config
		;; Global settings (defaults)
		(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
		(load-theme 'doom-one t)
		;; (load-theme 'doom-one-light t)
		)

  ;; Default font
  (set-face-attribute 'default nil :font "monospace-12")

  ;; mixed-pitch, https://gitlab.com/jabranham/mixed-pitch
  (use-package mixed-pitch
    :load-path "~/dotfiles/emacs/packages-startrek/mixed-pitch.el"
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
  ;; For Javascript
  (add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; ;; For ruby
  ;; (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  (require 'org)
  (setq org-image-actual-width nil)
  (setq org-hide-emphasis-markers t)
  ;; startup: showeverything
  (setq org-startup-folded nil)

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
  ;; Evaluate it after startup
  ;; (add-hook 'org-mode-hook #'org-show-two-levels)
  (add-hook 'org-view-mode-hook '(text-scale-adjust))

  ;; (setq org-modules '(org-tempo))

  (use-package org-autolist
    :config
    (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
    )

(setq org-image-actual-width 500)

  (bind-key "C-c c" 'org-capture)
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c t" 'org-todo)

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

  ;; evil key-bindings
  ;; (define-key org-agenda-mode-map "j" 'evil-next-line)
  ;; (define-key org-agenda-mode-map "k" 'evil-previous-line)

  (use-package evil-org
	:ensure t
	:after org
	:hook (org-mode . (lambda () evil-org-mode))
	:config
	(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

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
  (setq  inferior-julia-program-name "/Applications/Julia-1.4.app/Contents/Resources/julia/bin/julia")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     ;; (julia . t)
     (shell . t)
     (python . t)
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

  (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
  (evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo-list)
  (evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo-list)
  (define-key evil-normal-state-map (kbd "C-a") 'org-agenda-list)

  (with-eval-after-load 'org
					;; (setq org-directory "/Users/chongchonghe/Dropbox/orgfiles")
					(setq org-agenda-files '("~/Dropbox/orgfiles/todos.org"
								 "~/Dropbox/orgfiles/notes.org"
								 "~/Dropbox/orgfiles/tasks"))
					;; (setq org-agenda-files
					;; 	"~/Dropbox/orgfiles/agenda.org")
					(setq org-default-notes-file "~/Dropbox/orgfiles/todos.org")
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

  (with-eval-after-load 'org
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "" "Tasks")
             "* TODO [#A] %?\n %U\n\n" :empty-lines-before 1)
            ("a" "Todo with link" entry (file+headline "" "Tasks")
             "* TODO [#A] %?\n  %U\n  %a\n\n" :empty-lines-before 1 :empty-lines-after 1)
            ("n" "Notes" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
             "* %?\n  %U\n\n" :empty-lines-before 1)
            ("i" "Ideas" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Ideas")
             "* %?\n  %u\n\n" :empty-lines-before 1)
            ("f" "Followup" entry (file+headline "" "Followup")
             "* FLUP [#B] %?\n  %U\n  %a\n\n" :empty-lines-before 1)
            ("l" "Later" entry (file+headline "" "Later (emails or tasks)")
             "* TODO [#D] %?\n  %U\n  %a\n\n" :empty-lines-before 1)
            ;; ("g" "General todo" entry (file+headline "/Users/chongchonghe/tasks.org" "Tasks")
            ;;  "* TODO [#B] %?\n %a" :empty-lines 1)
            )
          ))

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
        '((sequence "TODO(t!)" "NEXT(n!)" "DOIN(o!)" "WAIT(w!)" "FLUP(f!)" "|" "CXLD(c!)" "DONE(d!)"))
        org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("DOIN" . (:foreground "orange"))
          ("FLUP" . (:foreground "magenta"))
          ;; ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("CXLD" . (:foreground "gray"))
          ("NEXT" . "#008080"))
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        )

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

  (defun my-website-sitemap-function (project &optional sitemap-filename)
	"Custom sitemap generator that inserts additional options."
	(let ((buffer (org-publish-org-sitemap project sitemap-filename)))
	  (with-current-buffer buffer
		(insert "\n#+SETUPFILE: ../style/default.setup")
		(save-buffer))))

(setq
 org-ref-bibliography-notes "~/Academics/org-ref-notes.org"
 org-ref-default-bibliography '("~/folders/BIB_HE.bib")
 org-ref-pdf-directory "~/Academics/Papers/"
 )

  (setq mode-require-final-newline nil)
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
	(define-key python-mode-map (kbd "C-c C-z") 'run-python)
	(define-key python-mode-map (kbd "<backtab>") 'python-back-indent)
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
	(setq elpy-rpc-timeout 2))
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

  (setq org-preview-latex-default-process 'dvisvgm)

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
  (dashboard-setup-startup-hook)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(neotree use-package transpose-frame org-superstar org-autolist helm-org helm-flx folding flycheck exec-path-from-shell evil-org elpy doom-themes dashboard auctex anaconda-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq x-meta-keysym 'super)
(setq x-super-keysym 'meta)

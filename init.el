;; -*- mode: emacs-lisp -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               ;; (expand-file-name (concat user-emacs-directory "init.org")))
               "/Users/chongchonghe/system-essentials/emacs/init.org")
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

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

;; Use cmd key for meta
;; https://superuser.com/questions/297259/set-emacs-meta-key-to-be-the-mac-key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(setq-default
 ;; Column Marker at 80
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; set encoding
(set-language-environment "UTF-8")
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

(global-set-key (kbd "C-c r") #'recompile)

(defun no-auto-fill ()
  "Turn off auto-fill-mode."
  (auto-fill-mode -1)
  (setq word-wrap t)
  )

;;customize theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(require 'tsdh-light-theme)
(set-background-color "#d4cec3")

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

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "PingFang SC"
                                       :size 16)))

(when window-system (set-frame-size (selected-frame) 84 50))

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
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

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

;; (load-file "./share.el")

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

;; (use-package org-cliplink
;;   :bind ("C-c C-p" . 'org-cliplink))
(require 'org-cliplink)
(define-key org-mode-map (kbd "C-c C-p") 'org-cliplink)

;; (setq org-modules '(org-tempo))

(use-package org-autolist
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  )

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
  (local-set-key "\C-y" 'yank)
  (local-set-key "\M-h" 'windmove-left)
  (local-set-key "\C-cl" 'grg-store-link)
  (local-set-key "\C-cb" 'org-switchb)
  (setq-local truncate-lines 'nil)
  ;; (org-indent-mode)  ;; not working?
  )
(add-hook 'org-mode-hook 'my-org-mode-config)

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

;; (font-lock-add-keywords
;;  'org-mode
;;  '(("^ *\\([-]\\) "
;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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

(evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)

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
        '(("t" "Todo (without links)" entry (file+headline "" "Tasks")
           "* TODO [#A] %?\n %U\n" :empty-lines-before 1)
          ("a" "Tasks (with links)" entry (file+headline "" "Tasks")
           "* TODO [#A] %?\n  %U\n  %a\n" :empty-lines-before 1 :empty-lines-after 1)
          ("n" "Notes" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
           "* %?\n  %U\n" :empty-lines-before 1)
          ("i" "Ideas" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Ideas")
           "* %?\n  %u\n" :empty-lines-before 1)
          ("f" "Followup" entry (file+headline "" "Followup")
           "* FLUP [#B] %?\n  %U\n  %a\n" :empty-lines-before 1)
          ("l" "Later" entry (file+headline "" "Later (emails or tasks)")
           "* TODO [#D] %?\n  %U\n  %a\n" :empty-lines-before 1)
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
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (setq org-bullets-bullet-list '("⚫" "◆" "◉" "▶" "◇"))
  )
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "DOIN(o!)" "FLUP(f!)" "|" "CXLD(c!)" "DONE(d!)"))
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

(with-eval-after-load 'org
      (use-package org-toggl
	:init
	(setq toggl-auth-token "ce3e8fc3922edda6986a6e729509338f")
	(setq org-toggl-inherit-toggl-properties t)
	:load-path "/Users/chongchonghe/system-essentials/emacs/packages"
	:config
	(toggl-get-projects)
	(org-toggl-integration-mode)
	;; remove clock-out since it failed at stopping toggl timer
	(remove-hook 'org-clock-out-hook #'org-toggl-clock-out)
	;; bind C-c i to clock-in then clock-out
	(define-key org-mode-map (kbd "C-c i")
	      (lambda () (interactive) (org-clock-in) (sit-for 2) (org-clock-out)))
	)
      )

;; (use-package ox-md)
(setq org-export-backends '(ascii html md icalendar latex odt))

(setq org-file-apps
	      '(("\\.docx\\'" . default)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("\\.pdf\\'" . default)
	(auto-mode . emacs)))

(use-package yasnippet
  :diminish yas-minor-mode
  ;; :init (yas-global-mode)
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list #'yas-snippet-dirs "~/system-essentials/emacs/snippets")
  (yas-reload-all)
  ;; (progn
  ;;   (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  ;;   ;; (setq yas-key-syntaxes '("w_" "w_." "^ "))
  ;;   (setq yas-installed-snippets-dir "~/system-essentials/emacs/snippets")
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
  (global-set-key (kbd "M-C-b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  ;; rebind tab to run persistent action
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; make TAB work in terminal
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  ;; list actions using C-z
  (define-key helm-map (kbd "C-z")  'helm-select-action)
)

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  ;; (setq dashboard-banner-logo-title "your custom text")
  ;; (setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)))
  (dashboard-setup-startup-hook))

(use-package elpy)
(use-package flycheck)

(with-eval-after-load 'python
  ;; Enable elpy
  (elpy-enable)
  ;; (elpy-use-ipython)
  (setq elpy-rpc-backend "jedi")
  ;; Disable elpy Vertical Guide Lines for Indentation
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;;(require 'py-autopep8)
  ;; (add-hook 'elpy-mode-hook) ;;'py-autopep8-enable-on-save)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  ;; (define-key ein:notebook-mode-map (kbd "C-c C-x d")
  ;;   'ein:worksheet-delete-cell)
  ;; Autoinsert Python comments
  (global-set-key (kbd "<f5>") 'my-insert-comments)
  (defun my-insert-comments (string)
    "Insert \label{ARG} \index{\nameref{ARG}} at point"
    (interactive "sString for \\label and \\nameref: ")
    (insert "##### "  string  " #####"))
  ;;(global-set-key (kbd "<f6>") 'my-insert-docstring)
  ;;(defun my-insert-docstring (string)
  ;;  "Insert \label{ARG} \index{\nameref{ARG}} at point"
  ;;  (interactive "sString for \\label and \\nameref: ")
  ;;  (insert '""" '  string  ' """'))
  (add-hook 'python-mode-hook 'hs-minor-mode)
  ;; jedi, replaced by (setq elpy-rpc-backend "jedi")
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  ;; (setq jedi:complete-on-dot t)
  (setq elpy-rpc-ignored-buffer-size 204800)
  )

(use-package latex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
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

;; (evil-define-key 'normal LaTeX-mode-map (kbd ", l") 'TeX-command-master)
;; do not query the user before saving each file with TeX-save-document
(setq TeX-save-query nil) 
(evil-define-key 'normal LaTeX-mode-map (kbd ", l") 'TeX-command-run-all)
(evil-define-key 'normal LaTeX-mode-map (kbd ", v") 'TeX-view)
(evil-define-key 'normal LaTeX-mode-map (kbd "M-w") 'LaTeX-fill-region)

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

(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

(add-to-list
 'display-buffer-alist
 (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
;; not working
(call-process-shell-command "osascript&" nil 0)

)

(add-hook 'text-mode-hook
        '(lambda ()
           (setq indent-tabs-mode t)
           (setq tab-width 4)))

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

(global-set-key (kbd "M-v") 'evil-paste-after)

;; (use-package smooth-scroll
;;       :config
;;       (smooth-scroll-mode 1)
;;       (setq smooth-scroll/vscroll-step-size 5)
;;       )
(use-package smooth-scrolling
	      :config
	      (smooth-scrolling-mode 1))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
		      (format "%s\\|%s"
					vc-ignore-dir-regexp
					tramp-file-name-regexp))
(setq tramp-verbose 1)

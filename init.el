;;; init.el --- Main Emacs Inuit File -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; My init file
;;;
;;; Code:

(defvar bootstrap-version)
(setq package-enable-at-startup nil)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(defvar prelude-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq straight-use-package-by-default t)

(use-package f)
(use-package use-package-ensure-system-package
  :ensure t)
;; :bind (([remap async-shell-command] . dtache-shell-command)
;;        :map dtache-shell-mode-map
;;        ("C-c C-q" . dtache-detach-dwim)))

;;; org packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :diminish org-table-header-line-mode)
(setq org-roam-v2-ack t)
(use-package org-roam
  :diminish org-roam-mode)
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
(use-package org-runbook
  :after mustache)
(use-package ob-go)
(use-package visual-fill-column)
(defun my/org-present-start ()
  (setq-local visual-fill-column-width 110
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  ;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.0)
  ;; (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)
  ;;                                    (org-document-title (:height 1.75) org-document-title)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    ; (org-block (:height 1.25) org-block)
  ;;                                    (org-block-begin-line (:height 0.7) org-block)))
  (setq-local org-hide-emphasis-markers t)
  (setq-local header-line-format " ")
  (tab-bar-mode 0))
(add-hook 'image-minor-mode-hook #'auto-revert-mode)
(defun my/org-present-end ()
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (setq-local face-remapping-alist nil)
  (setq-local header-line-format nil)
  (setq-local org-hide-emphasis-markers nil)
  (tab-bar-mode 1))
(use-package org-present
  :init
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Programming ;;;;;
(use-package clojure-mode
  :ensure-system-package
  (clojure-lsp . clojure-lsp/brew/clojure-lsp-native)
  :config
  (setq nrepl-log-messages t)
  (setq lsp-enable-completion-at-point nil)
  (setq prelude-clojure-mode-hook 'prelude-clojure-mode-defaults)
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp))
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (run-hooks 'prelude-clojure-mode-hook))))
(use-package cider
  :config
  (setq nrepl-log-messages t)
  (setq prelude-cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)
  :hook ((cider-mode . eldoc-mode))
  :init
  (add-hook 'cider-repl-mode-hook
            (lambda () (run-hooks 'prelude-cider-repl-mode-hook)))
  ;; (add-hook 'cider-mode-hook
  ;;           #'cider-setup-orderless)
  )
(use-package flycheck-clj-kondo)
(use-package scss-mode)
(use-package rjsx-mode)
(use-package racket-mode)
(use-package terraform-mode)

(use-package typescript-mode
  :mode (rx ".ts" string-end)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
  (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))
(use-package protobuf-mode)
(use-package go-mode
  :hook (before-save . gofmt-before-save))
(use-package gotest)
(use-package lua-mode)
(use-package php-mode)
(use-package geiser
  :config
  (put 'fresh 'scheme-indent-function 1))
(use-package elisp-slime-nav)
;; (use-package tsx-mode
;;   :straight (tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))
(use-package tide)
(defun my/setup-tide-mode ()
  (interactive)
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (lsp)
    ;; (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    ;; (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    ;; (company-mode +1)
    ))
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :init
  (setq-default web-mode-indent-style 2
                web-mode-code-indent-offset 4
                web-mode-attr-indent-offset 2
                web-mode-sql-indent-offset 2
                web-mode-attr-value-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-javascript-indentation 2)
  :hook
  ((web-mode . my/setup-tide-mode)
   (web-mode . prettier-mode)))
(use-package typescript-mode)
(use-package prettier
  :hook ((typescript-tsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (rjsx-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))
(use-package rust-mode)
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))
(use-package jupyter)
(use-package ein)


;;; Text Editing ;;;;;
;; (use-package mustache)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package js2-mode)
(use-package graphql-mode)
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))
(use-package dockerfile-mode)
(use-package csv-mode)
(use-package terraform-mode)
(use-package wat-mode
  :straight (wat-mode :type git :host github :repo "devonsparks/wat-mode"))
(use-package mustache)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :config
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
  (setq avy-dispatch-alist
        '((?x . avy-action-kill-move)
          (?X . avy-action-kill-stay)
          (?p . avy-action-teleport)
          (?m . avy-action-mark)
          (?y . avy-action-yank)
          (?Y . avy-action-yank-line)
          (?l . avy-action-ispell)
          (?z . avy-action-zap-to-char)
          (?. . avy-action-embark)
          (?v . avy-action-eval)))
  (setq avy-background t)
  (setq avy-style 'at-full))
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
(defun avy-action-eval (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (if (looking-at "(")
            (sp-forward-sexp)
          (sp-up-sexp))
        (cond
         ((equal mode-name "EL")
          (eval-last-sexp nil))
         ((equal mode-name "Clojure")
          (cider-eval-last-sexp))))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
(use-package hydra)
(use-package zzz-to-char
  :init
  (global-set-key [remap zap-to-char] 'zzz-to-char))
;; (use-package zop-to-char
;;   )
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode))
(use-package yasnippet-snippets)
(use-package turkish)
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (unbind-key (kbd "M-s") smartparens-mode-map)
  (setq sp-ignore-modes-list nil)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-mode "'" nil :actions nil)
  :init
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode +1)
  :bind
  (("C-M-SPC" . sp-mark-sexp)))
;; (use-package puni
;;   :inti)
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))
(use-package emmet-mode :ensure t :hook typescript-mode)
(use-package hl-todo)
(use-package flycheck
  :diminish flycheck-mode)
(use-package flycheck-package
  :after flycheck
  :init
  (flycheck-package-setup))
(use-package easy-kill
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))
(use-package diff-hl
  :init
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package rainbow-mode)
(use-package origami
  :demand
  :config
  (define-prefix-command 'origami-mode-map)
  (define-key ctl-x-map (kbd "z") 'origami-mode-map)
  (global-origami-mode)
  :bind
  (:map origami-mode-map
   ("o" . origami-open-node)
   ("O" . origami-open-node-recursively)
   ("c" . origami-close-node)
   ("C" . origami-close-node-recursively)
   ("a" . origami-toggle-node)
   ("A" . origami-recursively-toggle-node)
   ("R" . origami-open-all-nodes)
   ("M" . origami-close-all-nodes)
   ("v" . origami-show-only-node)
   ("k" . origami-previous-fold)
   ("j" . origami-forward-fold)
   ("x" . origami-reset)))
(use-package company)
(use-package multiple-cursors)
(use-package lispy)

;; end editing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package code-review)
(use-package ripgrep
  :config
  (setq ripgrep-executable "/usr/local/bin/rg"))
(use-package rg
  :init
  (rg-enable-default-bindings)
  (setq rg-executable "/usr/local/bin/rg"))
(use-package deadgrep)
(use-package crdt)
(use-package magit)
;; (use-package inspector)
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g d" . dtache-consult-sources)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-org-heading)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         :map lsp-mode-map
         ("s-l s" . consult-lsp-symbols)
         ("s-l S" . consult-lsp-file-symbols))

  
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args))))
(use-package ligature
  :config
  (ligature-set-ligatures '(clojure-mode) '("#{" "#(" "#_(" "#_" "#?" "#:"
                                            ";;" "~@"))
  (ligature-set-ligatures '(go-mode) '(":="))
  (ligature-set-ligatures t '("www"))
  (ligature-set-ligatures '(html-mode web-mode rjsx-mode js2-mode)
                          '("</" "<!--" "</>" "-->" "/>" "www"))
  
  ;;(global-ligature-mode)
  )
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
(defun consult-dir--tramp-docker-hosts ()
  "Get a list of hosts from docker."
  (when (require 'docker-tramp nil t)
    (let ((hosts)
          (docker-tramp-use-names t))
      (dolist (cand (docker-tramp--parse-running-containers))
        (let ((user (unless (string-empty-p (car cand))
                        (concat (car cand) "@")))
              (host (car (cdr cand))))
          (push (concat "/docker:" user host ":/") hosts)))
      hosts)))
(defvar consult-dir--source-tramp-docker
  `(:name     "Docker"
    :narrow   ?d
    :category file
    :face     consult-file
    :history  file-name-history
    :items    ,#'consult-dir--tramp-docker-hosts)
  "Docker candiadate source for `consult-dir'.")

(use-package consult-flycheck
  :after lsp-mode)

(use-package consult-lsp
  :config
  (consult-lsp-marginalia-mode 1)
  :after (lsp-mode marginalia))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package wgrep)
(use-package ibuffer-projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))
(use-package html-to-hiccup)
(use-package git-timemachine)
(use-package git-link
  :bind (:map
         projectile-command-map
         ("n h" . git-link-homepage)
         ("n l" . git-link)
         ("n c" . git-link-commit))
  :config
  (setq git-link-open-in-browser t))
(use-package gist)
(use-package forge
  :after magit)
(use-package dwim-shell-command)
(use-package elfeed
  :config
  (setq elfeed-feeds
      '("https://metaredux.com/feed.xml"
        ("https://blog.cleancoder.com/atom.xml" dev)
        ("http://nullprogram.com/feed/" emacs)
        ("https://karthinks.com/index.xml" emacs)
        ("https://sachachua.com/blog/feed/" emacs)
        ("http://ergoemacs.org/emacs/blog.xml" emacs)
        ("https://emacsredux.com/feed.xml" emacs)
        ("https://irreal.org/blog/?feed=rss2" emacs)
        ("http://blog.binchen.org/rss.xml" emacs)
        ("https://200ok.ch/atom.xml" emacs)
        ("https://bzg.fr/index.xml" emacs)
        ("https://updates.orgmode.org/feed/help" org)
        ("https://defn.io/index.xml" racket)
        ("https://emacstil.com/feed.xml" emacs)
        ("https://ag91.github.io/rss.xml" emacs)
        ("https://www.masteringemacs.org/feed" emacs)
        ("http://pragmaticemacs.com/feed/" emacs))))
(use-package anzu
  :diminish anzu-mode
  :init
  (global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))
(use-package engine-mode
	:ensure t
	:config
	(defengine google "https://google.com/search?q=%s" :keybinding "g"
	  :docstring "Applied Google-fu.")
	(defengine google-images "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s" :docstring "Google Images")
	(defengine google-maps "http://maps.google.com/maps?q=%s" :docstring "Mappin' it up.")
	(defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d"
	  :docstring "DDG!")
	(defengine qwant "https://www.qwant.com/?q=%s" :keybinding "q"
	  :docstring "Qwant it.")
	(defengine wikipedia "https://en.wikipedia.org/wiki/Special:Search?search=%s" :keybinding "w"
	  :docstring "Search Wikipedia.")
	(defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y"
	  :docstring "Search YouTube.")
	(defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "h"
	  :docstring "Search GitHub.")
	(defengine melpa "https://melpa.org/#/?q=%s" :keybinding "m"
	  :docstring "Search the Milkypostman's Emacs Lisp Package Archive.")
	(defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s"
	  :docstring "Search Stack Overflow.")
	(defengine wolfram-alpha "http://www.wolframalpha.com/input/?i=%s" :keybinding "a"
	  :docstring "Search Wolfram Alpha.")
	(defengine rfcs "http://pretty-rfc.herokuapp.com/search?q=%s" :keybinding "r"
	  :docstring "Search RFC documents.")
	(defengine ctan "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s" :keybinding "c"
	  :docstring "Search the Comprehensive TeX Archive Network")
	(defengine project-gutenberg "http://www.gutenberg.org/ebooks/search/?query=%s" :keybinding "p"
	  :docstring "Search Project Gutenberg.")
	(engine/set-keymap-prefix (kbd "C-x /"))
    (setq browse-url-firefox-program "/Applications/Firefox\ Developer\ Edition.app/Contents/MacOS/firefox")
	:init
	(engine-mode t))
(use-package code-review)
(use-package awqat
  :straight (awqat :type git :host github :repo "zkry/awqat")
  :config
  ;; istanbul
  ;; (setq calendar-latitude 41.013
  ;;       calendar-longitude 28.946)
  (setq calendar-latitude 33.44
        calendar-longitude -112.07)
  (setq awqat-asr-hanafi nil)
  (awqat-set-preset-diyanet)
  (setq awqat-fajr-angle -20.70)
  (setq awqat-isha-angle -16.3)
  (setq awqat-prayer-safety-offsets
        '(0.0 0.0 7.0 -9.0 0.0 0.0)))
(use-package emms)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Downloads/Music/")
(use-package intentional
  :straight (intentional :type git :host github :repo "zkry/intentional.el")
  :config
  (setq intentional-output-file "~/browse-intentions.json")
  (setq intentional-global-intentions
        '(("Dev Work" always ("http://localhost:3000/*"))
          ("Language Goals" always ("translate.google.com/*" "https://www.deepl.com/*"))
          ("Work on-call" always ("https://*.pagerduty.com/*"))
          ("Work" (between-on-days "08:00" "18:00" (1 2 3 4 5))
           ("https://*.atlassian.net/*"
            "https://github.com/*"
            "https://outlook.office.com/*"
            "https://golang.org/*"
            "https://*circleci.com/*"
            "https://accounts.google.com/*"
            "googleusercontent.com/*"
            "travelaudience.com/*"
            "codeclimate.com/*"))
          ("Relax" (between "19:00" "21:00") ("https://youtube.com/*"))))
  (setq intentional-site-groups
        '(("work" "https://*.atlassian.net/*" "https://github.com/*" "https://outlook.office.com/*" "https://golang.org/*" "travelaudience.com/*" "circleci.com/*")
          ("clojure" "https://clojuredocs.org/*" "https://clojure.org/*" "https://cljdoc.org/*" "https://github.com/*" "https://clojureverse.org/*" "https://stackoverflow.com/*")
          ("golang" "https://golang.org/*" "https://github.com/*" "https://godoc.org/*" "https://stackoverflow.com/*")))
  (setq intentional-tag-intentions
        '(("shopping" ("amazon.com/*" "amazon.de/*"))
          ("deepw" ("mynoise.net/*"))
          ("dev" ("github.com/*"))
          ("clojure" ("clojure"))))
  (setq intentional-save-to-journal nil)
  (setq intentional-extract-clock-body-urls t))
;; end tools


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish
  :init
  (diminish 'projectile-mode
            '(:eval (format " Prj(%s)" (projectile-project-name)))))
(use-package request)
(use-package lsp-mode
  :config
  (setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      lsp-ui-sideline-enable nil))
(use-package dap-mode)
;;(use-package company-lsp)
(use-package lsp-treemacs)
(use-package lsp-ui)
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1))
(use-package super-save
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))
(use-package atomic-chrome)
(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate"))

;; (use-package tsi
;;   :straight (tsi :type git :host github :repo "orzechowskid/tsi.el"))
(use-package tree-edit)
(use-package tree-sitter-langs :ensure t)
(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
  :init
  (projectile-mode +1))
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match 'insert)
  (corfu-scroll-margin 5)
  
  :init
  (global-corfu-mode)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package savehist
  :init
  (savehist-mode))
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize))
(use-package discover-my-major)
(use-package docker-tramp)
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)))
(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (unbind-key (kbd "C-.") flyspell-mode-map))
(use-package pdf-tools)

;;; Elisp Programming
(use-package package-lint)
(use-package restclient)
(use-package f)
(use-package editorconfig
  :diminish editorconfig-mode
  :init
  (editorconfig-mode 1))
(use-package alert)
(use-package ag)
(use-package restclient)
(use-package envrc
  :init
  (envrc-global-mode))
(with-eval-after-load 'envrc
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (tab-bar-mode 1)
  (tab-bar-history-mode)
  (setq enable-recursive-minibuffers t))



;;;;;;;;;;;;;
;;; Themes
;;;;;;;;;;;;;
(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  :config
  (ef-themes-select 'ef-bio))
;; (use-package zenburn-theme
;;   :init
;;   (load-theme 'zenburn t))
;; (use-package modus-themes
;;   :ensure
;;   :init
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-region '(bg-only no-extend))
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-vivendi)
;;   :bind ("<f5" . modus-themes-toggle))
;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample))
;;   :defer t
;;   :ensure t)
;; (use-package poet-theme
;;   :config
;;   (load-theme 'poet))
;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (color-theme-sanityinc-tomorrow-eighties))
;; (use-package tao-theme
;;   :config
;;   (load-theme 'tao-yin))
;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-dark t))
;; (use-package leuven-theme
;;   :init
;;   (load-theme 'leuven t))
;; (use-package subword
;;   :diminish subword-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package youtube
  :straight (youtube :type git :host github :repo "zkry/youtube.el")
  :config
  (setq youtube-storage-dir "~/.youtube/"))
(use-package go-ttest
  :straight (go-ttest :type git :host github :repo "zkry/go-ttest.el"))
(use-package time-table
  :straight (time-table :type git :host github :repo "zkry/time-table"))
(use-package yaml
  :straight (yaml :type git :host github :repo "zkry/yaml.el"))
(use-package ag-plus
  :straight (ag-plus :type git :host github :repo "zkry/ag-plus.el")
  :hook (ag-mode . ag-plus-mode))
;; (use-package prettier-js :ensure t :hook (typescript-mode))







;;; (straight-use-package 'kubernetes)






















;; (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t)








;;; (straight-use-package 'imenu-anywhere)







;; (use-package gitignore-mode)
;; (use-package gitconfig-mode)



 ;; 





















;; (use-package company-go)
;; (use-package company
;;   :diminish company-mode
;;   :config
;;   (setq company-idle-delay 0.5)
;;   (setq company-show-numbers t)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 2)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-tooltip-flip-when-above t))



;;; (use-package clj-refactor)

;;; (use-package browse-kill-ring)






 



;(use-package asm-blox)








;;; Personal packages
















;; (tree-sitter-require 'yaml)




;; (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
;; (add-hook 'typescript-tsx-mode-hook #'tree-sitter-hl-mode)
;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
;;
;; (add-hook
;;  'typescript-mode-hook
;;  (lambda ()
;;    (setq-local font-lock-defaults '(()))
;;    (tree-sitter-hl-mode 1)))
;;
;; (tree-sitter-require 'tsx)
;; (add-to-list
;;  'tree-sitter-major-mode-language-alist
;;  '(typescript-mode . tsx))



;; Experimental

 ;; (use-package dogears
;;   :straight (dogears :type git :host github :repo "alphapapa/dogears.el")
;;   :init
;;   (dogears-mode)
;;   ;; These bindings are optional, of course:
;;   :bind (:map global-map
;;               ("M-g d" . dogears-go)
;;               ("M-g M-b" . dogears-back)
;;               ("M-g M-f" . dogears-forward)
;;               ("M-g M-d" . dogears-list)
;;               ("M-g M-D" . dogears-sidebar)))

;; Personal





(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(defun prelude-clojure-mode-defaults ()
    (subword-mode +1))

(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(defun html-to-hiccup-yank ()
  "Convert the region between START and END from HTML to Hiccup."
  (interactive)
  (let ((hiccup))
    (with-temp-buffer
      (yank)
      (let ((html-sexp (libxml-parse-html-region (point-min) (point-max))))
        (setq hiccup (html-to-hiccup--sexp-to-hiccup html-sexp))))
    (insert hiccup)))

(require 'recentf)
(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list prelude-savefile-dir package-user-dir)))))

(defun prelude-enable-flyspell ()
  "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
  (when (executable-find ispell-program-name)
    (flyspell-mode +1)))

(defun prelude-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
  (whitespace-cleanup))

(defun prelude-enable-whitespace ()
  "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
  ;; keep the whitespace decent all the time (in this buffer)
  (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
  (whitespace-mode +1))


(defvar my/home-directory (getenv "HOME"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editor Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")

(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" prelude-savefile-dir))
(savehist-mode +1)
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" prelude-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)
(recentf-mode +1)
(use-package tramp
  :config
  (setq tramp-default-method "ssh"))
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" prelude-savefile-dir)
        bookmark-save-flag 1)
(require 'dired-x)
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))
(use-package midnight)
(require 'tabify)

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" prelude-savefile-dir))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first error
      )

(repeat-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq require-final-newline t)
(setq blink-matching-paren nil)
(delete-selection-mode t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(global-auto-revert-mode t)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(setq tab-always-indent 'complete)
(setq save-place-file (expand-file-name "saveplace" prelude-savefile-dir))
(menu-bar-mode -1) ;; Does this even do anything?
(toggle-scroll-bar -1)
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)
(tool-bar-mode -1)
(save-place-mode 1)
(set-default 'imenu-auto-rescan t)
(add-hook 'text-mode-hook 'prelude-enable-flyspell)
(add-hook 'text-mode-hook 'prelude-enable-whitespace)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)

(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" prelude-savefile-dir))

;;; Configure Env Vars
(setenv "PATH"
        (concat
         (getenv "PATH")
         (concat ":" my/home-directory "/go/bin")
         (concat ":" my/home-directory "/bin")
         (concat ":" my/home-directory "/.local/bin")
         ":/usr/local/go/bin"
         ":/usr/local/bin"))
(setenv "GOPRIVATE" "github.com/travelaudience/")
(setenv "GOPROXY" "direct")
(setenv "PURE_GIT_PULL" "0")
(setenv "GOSUMDB" "off")
(setenv "GOPATH" "/Users/zromero/go")
(setenv "GOBIN" "/Users/zromero/go/bin")
(setenv "MANPAGER" "cat")
(setenv "PAGER" "cat")



;; frame display config
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;; whitespace mode
(defvar zr/clean-whitespace-on-save t)
(defun zr/cleanup-maybe ()
  (when zr/clean-whitespace-on-save
    (whitespace-cleanup)))

(diminish 'whitespace-mode)
(setq whitespace-line-column 80)
(setq-default tab-width 4)
(setq-default whitespace-style '(face tabs empty trailing lines-tail))
(whitespace-mode 1)
(add-hook 'before-save-hook 'zr/cleanup-maybe nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c p") #'projectile-command-map)
(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-§ i") #'zr/open-init)
(global-set-key (kbd "C-§ o") #'zr/open-organizer)
(global-set-key (kbd "C-§ n") #'zr/open-notes)
(global-set-key (kbd "C-§ r") #'zr/open-refile)
;(global-set-key (kbd "C-g i") #'zr/open-init)
;(global-set-key (kbd "C-g o") #'zr/open-organizer)
;(global-set-key (kbd "C-g n") #'zr/open-notes)
;(global-set-key (kbd "C-g r") #'zr/open-refile)
(global-set-key (kbd "<C-M-backspace>") #'sp-splice-sexp-killing-backward)
(global-set-key (kbd "C-M-]") #'sp-rewrap-sexp)
(global-set-key (kbd "C-M-SPC") #'sp-mark-sexp)
(global-set-key (kbd "s-k") #'crux-kill-whole-line)
(global-set-key (kbd "s-d") #'crux-delete-file-and-buffer)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
(global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)

(global-set-key (kbd "C-c a") #'org-agenda)
;; C-c b org-switchb
;; C-c c org-capture
;; C-c d crux-duplicate-current-line-or-region
;; C-c e envrc
;; C-c f crux-recentf-find-file
;; C-c g prelude-google
;; C-c h
;; C-c i imenu-anywhere
(global-set-key (kbd "C-c i") #'intentional)
;; C-c j counsel-git-grep
;; C-c k crux-kill-other-buffers
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c m") #'mu4e)
;; C-c n crux-cleanup-buffer-or-region
;; C-c o crux-open-with
;; C-c p ... projectile-...
(global-set-key (kbd "C-c q") #'awqat-times-for-day)
;; C-c r crux-rename-buffer-and-file
;; C-c s crux-swap-windows
;; C-c t crux-visit-term
;; C-c u crux-view-url
;; C-c v
(global-set-key (kbd "C-x w") #'elfeed)
;; C-c w prelude-swap-meta-and-super
;; C-c x
;; C-c y prelude-youtube
;; C-c z
;; C-c D crux-delete-file-and-buffer
;; C-c G prelude-github
;; C-c I crux-find-user-init-file
;; C-c S crux-find-shell-init-file
;; C-c U prelude-duckduckgo
;; C-c & ... yas-...
;; C-c ! ... flycheck-
;; C-c <left> winner-undo
;; C-c <right> winner-redo
;; C-c $ flyspell-correct-word-before-point
;; C-c TAB crux-indent-rigidly-and-copy-to-clipboard
;; C-c . ... apply-operation-to-number-at-point

(global-set-key (kbd "C-'") #'avy-goto-char-timer)
(global-set-key (kbd "M-g f") #'avy-goto-line)
(global-set-key (kbd "M-g w") #'avy-goto-word-1)
(global-set-key (kbd "<f5>") #'deadgrep)

(setq auth-sources '("~/.authinfo"))

(setq term-prompt-regexp "[^\n%]*% *")

;; working on tmux
;; (let ((map (if (boundp 'input-decode-map)
;;                input-decode-map
;;              function-key-map)))
;;   (define-key map "\e[1;P27" (kbd "s-s"))
;;   (define-key map "\e[1;P19" (kbd "s-k"))
;;   (define-key map "\e[1;P57" (kbd "C-M-SPC"))
;;   (define-key map "\e[1;P58" (kbd "C-a")))

(global-set-key (kbd "s-u") #'revert-buffer)

;; youtube.el

;; (use-package perspective
;;   :bind (("C-x C-b" . persp-list-buffers)
;;          ("C-x b" . persp-switch-to-buffer*)
;;          ("C-x k" . persp-kill-buffer*))
;;   ;; :hook (kill-emacs . persp-save-state)
;;   :config
;;   (setq persp-sort 'created
;;         persp-state-default-file (concat user-emacs-directory "persp-save-file"))
;;   :init
;;   (persp-mode t))

;; (require 'perspective)
;; (global-set-key (kbd "C-x C-b") #'persp-list-buffers)
;; (global-set-key (kbd "C-x b") #'persp-switch-to-buffer*)
;; (global-set-key (kbd "C-x k") #'persp-kill-buffer*)
;; (add-hook 'kill-emacs-hook #'persp-state-save)

(defun prelude-cider-repl-mode-defaults ()
  (subword-mode +1)
  (run-hooks 'prelude-interactive-lisp-coding-hook))

(setq enable-recursive-minibuffers t)

;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c a") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))




(load "~/.emacs.d/init-org.el")
(load "~/.emacs.d/init-elisp.el")
(load "~/.emacs.d/init-go.el")
(load "~/.emacs.d/init-js.el")
(when (load "~/.emacs.d/init-private.el" t)
  (require 'init-private))
(add-to-list 'load-path (concat my/home-directory "/dev/emacs/mu-1.6.10/mu4e"))
(add-to-list 'load-path (concat my/home-directory "/dev/emacs/yaml"))
(add-to-list 'load-path (concat my/home-directory "/dev/emacs/GIS-200"))
(add-to-list 'load-path (concat my/home-directory "/dev/emacs/dtache"))
(load "~/.emacs.d/init-mu4e.el")

(require 'init-org)
(require 'custom)
(require 'init-go)
(require 'init-js)
(require 'init-mu4e)
;; (require 'init-mu4e)

(provide 'init)

(require 'dtache)
(add-hook 'after-init-hook #'dtache-setup)
(global-set-key [remap async-shell-command] #'dtache-shell-command)
(bind-key (kbd "C-c C-q") #'dtache-detach-dwim 'dtache-shell-mode-map)
(defun my/dtache-state-transition-notification (session)
  "Send an `alert' notification when SESSION becomes inactive."
  (let ((status (dtache--session-status session))
        (title
         (pcase (dtache--session-status session)
           ('success "Dtache finished!")
           ('failure "Dtache failed!"))))
    (alert (dtache--session-command session)
           :title title
           :severity (pcase status
                       ('success 'moderate)
                       ('failure 'high))
           :category 'compile
           :id (pcase status
                 ('success 'dtache-success)
                 ('failure 'dtache-failure)))))
(setq dtache-notification-function #'my/dtache-state-transition-notification)

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:root@147.182.236.138:")
                   "login-args"
                   '(("-l" "%u") ("-p" "%p") ("%c") ("-e" "none") ("-o" "StrictHostKeyChecking=no") ("-o" "UserKnownHostsFile=/dev/null") ("%h"))))

(setq tramp-use-ssh-controlmaster-options nil)

(let ((login-args (assoc 'tramp-login-args (assoc "ssh" tramp-methods)))
      (new-value '((("-l" "%u")
                    ("-p" "%p")
                    ("%c")
                    ("-e" "none")
                    ("-o" "StrictHostKeyChecking=no")
                    ("-o" "UserKnownHostsFile=/dev/null")
                    ("%h")))))
  (setcdr login-args new-value))



(require 'dtache-consult)
(require 'dtache-shell)

(set-face-attribute 'default nil :family "Berkeley Mono")
;; (set-face-attribute 'default nil :family "Hack")
;;(set-face-attribute 'default nil :family "DejaVu Sans Mono")
;;(set-face-attribute 'default nil :family "Iosevka")
;;(set-face-attribute 'default nil :family "JetBrains Mono")
;;(set-face-attribute 'default nil :family "Monocraft")
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.0)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-default-directory-list '("/usr/local/share/info/" "/usr/share/info/"))
 '(alert-default-style 'osx-notifier)
 '(custom-safe-themes
   '("aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "a44bca3ed952cb0fd2d73ff3684bda48304565d3eb9e8b789c6cca5c1d9254d1" "ea5822c1b2fb8bb6194a7ee61af3fe2cc7e2c7bab272cbb498a0234984e1b2d9" "d0fa4334234e97ece3d72d86e39a574f8256b4a8699a1fb5390c402892a1c024" default))
 '(debug-on-error t)
 '(ein:output-area-inlined-images t)
 '(geiser-default-implementation 'mit)
 '(gofmt-command "goimports")
 '(mailcap-prefer-mailcap-viewers nil)
 '(mailcap-user-mime-data nil)
 '(native-comp-async-report-warnings-errors nil)
 '(org-display-remote-inline-images 'cache)
 '(org-duration-units
   '(("min" . 1)
     ("h" . 60)
     ("d" . 1440)
     ("w" . 10080)
     ("m" . 43200)
     ("y" . 525960.0)
     ("pom" . 25)))
 '(org-export-backends '(ascii html icalendar latex odt texinfo))
 '(org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-table-header-line-p t)
 '(reb-re-syntax 'read)
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (require 'package-recipe-mode nil t)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-recipe-mode)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0))
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (web-mode-javascript-indentation . 4)
     (web-mode-javascript-indentation . 2)
     (web-mode-indent-style . 2)
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (cider-shadow-cljs-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")))
 '(time-table-default-time-zones '("America/Phoenix" "America/Chicago"))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-threads 'disabled nil)

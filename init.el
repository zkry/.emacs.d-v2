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

(use-package org
  :diminish org-table-header-line-mode)
(setq org-roam-v2-ack t)
(use-package org-roam
  :diminish org-roam-mode)

(use-package diminish
  :init
  (diminish 'projectile-mode
            '(:eval (format " Prj(%s)" (projectile-project-name)))))

(use-package avy
  :config
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package cider
  :config
  (setq nrepl-log-messages t)
  (setq prelude-cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)
  :hook ((cider-mode . eldoc-mode))
  :init
  (add-hook 'cider-repl-mode-hook
            (lambda () (run-hooks 'prelude-cider-repl-mode-hook))))

(use-package clojure-mode
  :config
  (setq nrepl-log-messages t)
  (setq prelude-clojure-mode-hook 'prelude-clojure-mode-defaults)
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (run-hooks 'prelude-clojure-mode-hook))))

(use-package request)
(use-package markdown-mode)
(use-package magit)
(use-package lsp-mode)
(use-package lsp-treemacs)
(use-package hydra)
(use-package zop-to-char) ;; TODO: configure this
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode))
(use-package yasnippet-snippets)
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1))
(use-package yaml-mode)

(use-package expand-region)
;;; volatile-highlights ?

(use-package turkish)
(use-package swiper
  :bind (("C-s" . swiper)))
(use-package super-save
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  :init
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode +1)
  :bind
  (("C-M-SPC" . sp-mark-sexp)))
(use-package scss-mode)
(use-package rjsx-mode)
(use-package racket-mode)
(use-package protobuf-mode)
(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
  :init
  (projectile-mode +1))
(use-package perspective)
(use-package package-lint)
(use-package operate-on-number) ;; TODO: Learn me
;;; (straight-use-package 'smartrep)
(use-package ob-go)
(use-package lsp-ui)
;;; (straight-use-package 'kubernetes)
(use-package json-mode)
(use-package js2-mode)
(use-package ivy
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :init
  (ivy-mode 1)
  (setq projectile-completion-system 'ivy))
;;; (straight-use-package 'imenu-anywhere)
(use-package ibuffer-projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))
(use-package hyperbole)
(use-package html-to-hiccup)
(use-package hl-todo)
(use-package graphql-mode)
(use-package go-mode)
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package gitignore-mode)
(use-package gitconfig-mode)
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
(use-package geiser
  :config
  (put 'fresh 'scheme-indent-function 1))
(use-package forge)
(use-package flycheck
  :diminish flycheck-mode)
(use-package flycheck-clj-kondo)
(use-package f)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize))
(use-package elisp-slime-nav)
(use-package elfeed
  :config
  (setq elfeed-feeds
      '("https://metaredux.com/feed.xml"
        ("https://blog.cleancoder.com/atom.xml" dev)
        ("https://css-tricks.com/feed/" frontend)
        ("http://nullprogram.com/feed/" blog emacs)
        ("https://www.reddit.com/r/Clojure/.rss" clojure)
        ("https://golangnews.com/index.xml" golang)
        ("https://changelog.com/gotime/feed" golang)
        ("https://www.ardanlabs.com/blog/index.xml" golang)
        ("https://www.cncf.io/feed" k8s)
        ("https://kubernetes.io/feed.xml" k8s)
        ;; ("https://www.reddit.com/r/emacs/.rss" emacs)
        ("https://sachachua.com/blog/feed/" emacs)
        ("http://ergoemacs.org/emacs/blog.xml" emacs)
        ("https://emacsredux.com/feed.xml" emacs)
        ("https://irreal.org/blog/?feed=rss2" emacs)
        ("http://blog.binchen.org/rss.xml" emacs)
        ("https://200ok.ch/atom.xml" emacs)
        ("https://bzg.fr/index.xml" emacs)
        ("https://updates.orgmode.org/feed/help" org)
        ("https://defn.io/index.xml" racket))))
(use-package editorconfig
  :diminish editorconfig-mode
  :init
  (editorconfig-mode 1))
(use-package easy-kill
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))
(use-package dockerfile-mode)
(use-package discover-my-major)

(use-package diff-hl
  :init
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package deft)
(use-package csv-mode)
(use-package crux)
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))
(use-package company-lsp)
(use-package company-go)
(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  :init
  (global-company-mode 1))
(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")))
;;; (use-package clj-refactor)

;;; (use-package browse-kill-ring)
(use-package anzu
  :diminish anzu-mode
  :init
  (global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))
(use-package alert)
(use-package rainbow-mode)
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :init
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))
(use-package ag)
(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))
(use-package subword
  :diminish subword-mode)


;;; Personal packages
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
(use-package intentional
  :straight (intentional :type git :host github :repo "zkry/intentional.el")
  :config
  (setq intentional-output-file "~/browse-intentions.json")
  (setq intentional-global-intentions
        '(("Dev Work" always ("http://localhost:3000/*"))
          ("Language Goals" always ("translate.google.com/*"))
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

;;; Editor Configuration
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

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" prelude-savefile-dir))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first error
      )

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

;; related to lsp performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; frame display config
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 80)
  :init
  (whitespace-mode 1)
  (setq-default tab-width 4)
  (setq-default whitespace-style '(face tabs empty trailing lines-tail)))

;; (diminish 'intentional-minor-mode)
;; TODO - Install Intentional

;; keybindings
(global-set-key (kbd "C-c p") #'projectile-command-map)

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

(global-set-key (kbd "C-c a") #'org-agenda)
;; C-c b org-switchb
;; C-c c org-capture
;; C-c d crux-duplicate-current-line-or-region
;; C-c e crux-eval-and-replace
;; C-c f crux-recentf-find-file
;; C-c g prelude-google
;; C-c h
;; C-c i imenu-anywhere
;; C-c j counsel-git-grep
;; C-c k crux-kill-other-buffers
;; C-c l org-store-link
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


;; (require 'perspective)
;; (persp-mode)
;; (global-set-key (kbd "C-x C-b") #'persp-list-buffers)
;; (global-set-key (kbd "C-x b") #'persp-switch-to-buffer*)
;; (global-set-key (kbd "C-x k") #'persp-kill-buffer*)
;; (setq persp-sort 'created)
;; (add-hook 'kill-emacs-hook #'persp-state-save)

(defun prelude-cider-repl-mode-defaults ()
  (subword-mode +1)
  (run-hooks 'prelude-interactive-lisp-coding-hook))

(setq enable-recursive-minibuffers t)

;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c a") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)

;;; Advice 
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
(add-to-list 'load-path "/Users/zromero/dev/emacs/mu-1.6.1/mu4e/")
(load "~/.emacs.d/init-mu4e.el")

(require 'init-org)
(require 'custom)
(require 'init-go)
(require 'init-js)
(require 'init-mu4e)
;; (require 'init-mu4e)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'osx-notifier)
 '(custom-safe-themes
   '("ea5822c1b2fb8bb6194a7ee61af3fe2cc7e2c7bab272cbb498a0234984e1b2d9" "d0fa4334234e97ece3d72d86e39a574f8256b4a8699a1fb5390c402892a1c024" default))
 '(debug-on-error t)
 '(geiser-default-implementation 'mit)
 '(ivy-youtube-key "AIzaSyAU3TcMs0Hm28E3SAzcUHVEJBDmRpFFfpo")
 '(org-display-remote-inline-images 'cache)
 '(org-duration-units
   '(("min" . 1)
     ("h" . 60)
     ("d" . 1440)
     ("w" . 10080)
     ("m" . 43200)
     ("y" . 525960.0)
     ("pom" . 25)))
 '(org-export-backends '(ascii beamer html icalendar latex md odt confluence))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-table-header-line-p t)
 '(package-selected-packages
   '(clj-refactor perspective html-to-hiccup graphql package-lint racket-mode graphql-mode lsp-java f org-tree-slide deft org-journal org-download 2048-game ob-go ivy-youtube hyperbole alert ibuffer-projectile gnuplot gnuplot-mode forge kubernetes hy-mode csv-mode turkish elfeed lua-mode ample-theme ag scss-mode protobuf-mode flycheck-clj-kondo rjsx-mode groovy-mode org-pomodoro dockerfile-mode yasnippet yasnippet-snippets git-link yaml-mode geiser lsp-ui company-lsp json-mode js2-mode gotest go-projectile go-eldoc company-go go-mode rainbow-mode elisp-slime-nav cider clojure-mode rainbow-delimiters company counsel swiper ivy exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring beacon anzu ace-window))
 '(safe-local-variable-values
   '((eval when
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
 '(time-table-default-time-zones '("America/Phoenix" "America/Chicago")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
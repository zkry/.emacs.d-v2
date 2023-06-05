;;; init.el --- V3 of my initialization -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file contains my new Emacs configuration

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           ,
;;          ~)
;;           (_---;
;;            /|~|\
;;           / / /|

(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)

;;; Use Package Setup

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(use-package org
  :diminish org-table-header-line-mode
  :bind (:map org-mode-map
              ("C-c C-<return>" . org-pomodoro)))

(elpaca-wait)


;;; Emacs Setup

(defvar zr/clean-whitespace-on-save t)
(defun zr/cleanup-maybe ()
  (when zr/clean-whitespace-on-save
    (whitespace-cleanup)))

(use-package ht)

(use-package emacs
  :elpaca nil
  :init
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
  (setq enable-recursive-minibuffers t)

  :config
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
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (save-place-mode 1)
  (set-default 'imenu-auto-rescan t)
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  ;; dired
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)

  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
  (setq whitespace-line-column 80)
  (setq-default tab-width 4)
  (setq-default whitespace-style '(face tabs empty trailing lines-tail))
  (whitespace-mode 1)
  (setq auth-sources '("~/.authinfo"))

  (setq term-prompt-regexp "[^\n%]*% *")
  (setq enable-recursive-minibuffers t)

  ;;; Hooks
  :hook ((text-mode . abbrev-mode)
         (after-save . executable-make-buffer-file-executable-if-script-p)
	 (before-save . zr/cleanup-maybe))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Instalation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org Packages ;;;;;;;;;;;;;;;;;;

(use-package ob-go)

(use-package verb
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))
(use-package org-pomodoro
  :config
  (setq org-pomodoro-start-sound (file-truename "~/Documents/alert2.wav"))
  (setq org-pomodoro-finished-sound (file-truename "~/Documents/alert1.wav"))
  (setq org-pomodoro-overtime-sound (file-truename "~/Documents/alert1.wav"))
  (setq org-pomodoro-long-break-sound (file-truename "~/Documents/alert1.wav")))
(use-package org-journal)
(use-package org-download)

(require 'org-capture)
(require 'org-agenda)
(require 'org-clock)
(require 'org-table)
(require 'org-habit)
(require 'org-duration)
(require 'org-protocol)

;;; Programming Modes ;;;;;;;;;;;;;;;;;;

;; Clojure
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
            (lambda () (run-hooks 'prelude-cider-repl-mode-hook))))

(use-package flycheck-clj-kondo)
(use-package scss-mode)
(use-package racket-mode)
(use-package terraform-mode)
(use-package feature-mode)
(use-package protobuf-mode)
(use-package go-mode
  :hook (before-save . gofmt-before-save))
(use-package go-sea
  :elpaca
  (:host github :repo "zkry/go-sea.el" :files ("dist" "*.el")))
(use-package gotest)
(use-package lua-mode)
(use-package php-mode)
(use-package geiser
  :config
  (put 'fresh 'scheme-indent-function 1))
(use-package elisp-slime-nav)
(use-package web-mode
  :mode (("\\.html\\'" . web-mode))
  :init
  (setq-default web-mode-indent-style 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2
                web-mode-sql-indent-offset 2
                web-mode-attr-value-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-javascript-indentation 2))
;; TODO
;; (use-package edraw
;;   :straight (edraw-org :type git :host github :repo "misohena/el-easydraw")
;;   :config
;;   (require 'edraw-org)
;;   (edraw-org-setup-default))
(use-package prettier-js
  ;; TODO: Configure all of the hooks properly here
  :hook ((typescript-tsx-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))
(use-package rust-mode)
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))
(use-package markdown-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package graphql-mode)
(use-package dockerfile-mode)
(use-package csv-mode)
(use-package mustache)

;; JS Setup
(setq js-indent-level 2)

;; Go Setup
(setq go-ts-mode-indent-offset 4)

;;; Editing ;;;;;;;;;;;;;;;;;;;;;;;
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
          ;; (?. . avy-action-embark)
          (?v . avy-action-eval)))
  (setq avy-background t)
  (setq avy-style 'at-full))
;; (defun avy-action-embark (pt)
;;   (unwind-protect
;;       (save-excursion
;;         (goto-char pt)
;;         (embark-act))
;;     (select-window
;;      (cdr (ring-ref avy-ring 0))))
;;   t)
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
(use-package zzz-to-char
  :init
  (global-set-key [remap zap-to-char] 'zzz-to-char))

;; TODO: Configure tempel as desired
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )
(use-package dap-mode)

(use-package tempel-collection)

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

(use-package flycheck
  :diminish flycheck-mode)

(use-package flycheck-package
  :after flycheck
  :init
  (flycheck-package-setup))

(use-package multiple-cursors)


;;; Tools ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))))
(use-package eglot
  :hook ((go-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (js-mode . eglot-ensure)))
(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu4e")
(use-package copilot
  :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
(use-package ripgrep
  :config
  (setq ripgrep-executable "/usr/local/bin/rg"))
(use-package rg
  :init
  (rg-enable-default-bindings)
  (setq rg-executable "/opt/homebrew/bin/rg"))
(use-package magit)
(use-package projectile)
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; TODO: review consult commands
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
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args))))
;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))
;; (use-package embark-consult
;;   :ensure t :after (consult)
;;   :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package wgrep)
(use-package git-timemachine)
(use-package git-link
  :bind (:map
         project-prefix-map
         ("n h" . git-link-homepage)
         ("n l" . git-link)
         ("n c" . git-link-commit))
  :config
  (setq git-link-open-in-browser t))
(use-package forge
  :after magit)
(use-package dwim-shell-command)
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

;; TODO
;; (use-package awqat
;;   :straight (awqat :type git :host github :repo "zkry/awqat")
;;   :config
;;   ;; istanbul
;;   ;; (setq calendar-latitude 41.013
;;   ;;       calendar-longitude 28.946)
;;   (setq calendar-latitude 33.44
;;         calendar-longitude -112.07)
;;   (setq awqat-asr-hanafi nil)
;;   (awqat-set-preset-diyanet)
;;   (setq awqat-fajr-angle -20.70)
;;   (setq awqat-isha-angle -16.3)
;;   (setq awqat-prayer-safety-offsets
;;         '(0.0 0.0 7.0 -9.0 0.0 0.0)))

(use-package emms
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Downloads/Music/"))


;;; Utilities ;;;;;;;;;;;;;;;;;;
(use-package system-packages)
(use-package diminish)
(use-package request)
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode 1))
(use-package super-save
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))
(use-package atomic-chrome)
;; TODO
;; (use-package combobulate
;;   :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
;;   :hook ((js-ts-mode . combobulate-mode)
;;          (typescript-ts-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode)))
(use-package tree-sitter-langs :ensure t)
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

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-initialize))
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)))
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))
(use-package flyspell
  :elpaca nil
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (unbind-key (kbd "C-.") flyspell-mode-map))
(use-package pdf-tools)
(use-package package-lint)
(use-package restclient)
(use-package f)
(use-package editorconfig
  :diminish editorconfig-mode
  :init
  (editorconfig-mode 1))
(use-package alert)
(use-package ag)
(use-package envrc
  :init
  (envrc-global-mode))
(with-eval-after-load 'envrc
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map)) ;; TODO: what is bound here?
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode +1)

(require 'bookmark)
(setq bookmark-save-flag 1)

(require 'dired-x)

(use-package ediff
  :elpaca nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(require 'tabify)

(require 'em-smart)
(require 'eshell)

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'compile)
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

(repeat-mode)

;;; Themes ;;;;;;;;;;;;;;;;;;;;;
(use-package ef-themes
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  :config
  (ef-themes-select 'ef-bio))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c m") #'mu4e)
(global-set-key (kbd "C-c q") #'awqat-times-for-day)
(global-set-key (kbd "C-x w") #'elfeed)

(global-set-key (kbd "C-h C-f") #'find-function)

(global-set-key (kbd "s-u") #'revert-buffer)

;; to be added:
;; (global-set-key (kbd "<C-M-backspace>") #'sp-splice-sexp-killing-backward)
;; (global-set-key (kbd "C-M-]") #'sp-rewrap-sexp)
;; (global-set-key (kbd "C-M-SPC") #'sp-mark-sexp)
;; (global-set-key (kbd "s-k") #'crux-kill-whole-line)
;; (global-set-key (kbd "s-d") #'crux-delete-file-and-buffer)
;; (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
;; (global-set-key (kbd "C->") #'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
;; (global-set-key (kbd "C-'") #'avy-goto-char-timer)
;; (global-set-key (kbd "M-g f") #'avy-goto-line)
;; (global-set-key (kbd "M-g w") #'avy-goto-word-1)
;; (global-set-key (kbd "<f5>") #'deadgrep)

;; binding notes:
;; C-c b org-switchb
;; C-c c org-capture
;; C-c d crux-duplicate-current-line-or-region
;; C-c e envrc
;; C-c f crux-recentf-find-file
;; C-c g prelude-google
;; C-c h
;; C-c i imenu-anywhere
;; (global-set-key (kbd "C-c i") nil)
;; C-c j counsel-git-grep
;; C-c k crux-kill-other-buffers
;; C-c n crux-cleanup-buffer-or-region
;; C-c o crux-open-with
;; C-c p ... projectile-...
;; C-c r crux-rename-buffer-and-file
;; C-c s crux-swap-windows
;; C-c t crux-visit-term
;; C-c u crux-view-url
;; C-c v
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc. Configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Path
(defconst my/home-directory "/Users/zachary.romero")
(setenv "PATH"
        (concat
         (getenv "PATH")
         (concat ":" my/home-directory "/go/bin")
         (concat ":" my/home-directory "/bin")
         (concat ":" my/home-directory "/.local/bin")
         ":/usr/local/go/bin"
         ":/usr/local/bin"
         ":/Users/zachary.romero/Library/Python/3.9/bin"))
(setenv "GOPRIVATE" "github.com/travelaudience/")
(setenv "GOPROXY" "direct")
(setenv "PURE_GIT_PULL" "0")
(setenv "GOSUMDB" "off")
(setenv "GOPATH" "/Users/zachary.romero/go")
(setenv "GOBIN" "/Users/zachary.romero/go/bin")
(setenv "MANPAGER" "cat")
(setenv "PAGER" "cat")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defvar zr/clean-whitespace-on-save t)
(defun zr/cleanup-maybe ()
  (when zr/clean-whitespace-on-save
    (whitespace-cleanup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(elpaca-wait)
(add-to-list 'load-path "~/dev/emacs/bp")
(require 'bp)
(load-file "~/.emacs.d/init-org.el")
(load-file "~/.emacs.d/init-js.el")
(load-file "~/.emacs.d/init-go.el")
(load-file "~/.emacs.d/init-mu4e.el")
(load-file "~/.emacs.d/colemak-russian.el")

(set-face-attribute 'default nil :family "Berkeley Mono")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-float 1 1 3 "Martin Luther King Day")
     (holiday-fixed 2 2 "Groundhog Day")
     (holiday-fixed 2 14 "Valentine's Day")
     (holiday-float 2 1 3 "President's Day")
     (holiday-fixed 3 17 "St. Patrick's Day")
     (holiday-fixed 4 1 "April Fools' Day")
     (holiday-float 5 0 2 "Mother's Day")
     (holiday-float 5 1 -1 "Memorial Day")
     (holiday-fixed 6 14 "Flag Day")
     (holiday-float 6 0 3 "Father's Day")
     (holiday-fixed 7 4 "Independence Day")
     (holiday-float 9 1 1 "Labor Day")
     (holiday-float 10 1 2 "Columbus Day")
     (holiday-fixed 10 31 "Halloween")
     (holiday-fixed 11 11 "Veteran's Day")
     (holiday-float 11 4 4 "Thanksgiving")
     (holiday-easter-etc)
     (holiday-fixed 12 25 "Christmas")
     (if calendar-christian-all-holidays-flag
         (append
          (holiday-fixed 1 6 "Epiphany")
          (holiday-julian 12 25 "Christmas (Julian calendar)")
          (holiday-greek-orthodox-easter)
          (holiday-fixed 8 15 "Assumption")
          (holiday-advent 0 "Advent")))
     (if calendar-hebrew-all-holidays-flag
         (append
          (holiday-hebrew-tisha-b-av)
          (holiday-hebrew-misc)))
     (holiday-islamic-new-year)
     (holiday-islamic 9 1 "Ramadan Begins")
     (if calendar-islamic-all-holidays-flag
         (append
          (holiday-islamic 1 10 "Ashura")
          (holiday-islamic 3 12 "Mulad-al-Nabi")
          (holiday-islamic 7 26 "Shab-e-Mi'raj")
          (holiday-islamic 8 15 "Shab-e-Bara't")
          (holiday-islamic 9 27 "Shab-e Qadr")
          (holiday-islamic 10 1 "Id-al-Fitr")
          (holiday-islamic 12 10 "Id-al-Adha")))
     (if calendar-bahai-all-holidays-flag
         (append
          (holiday-fixed 11 26 "Day of the Covenant")
          (holiday-fixed 11 28 "Ascension of `Abdu’l-Bahá")))
     (holiday-chinese-new-year)
     (if calendar-chinese-all-holidays-flag
         (append
          (holiday-chinese 1 15 "Lantern Festival")
          (holiday-chinese-qingming)
          (holiday-chinese 5 5 "Dragon Boat Festival")
          (holiday-chinese 7 7 "Double Seventh Festival")
          (holiday-chinese 8 15 "Mid-Autumn Festival")
          (holiday-chinese 9 9 "Double Ninth Festival")
          (holiday-chinese-winter-solstice)))
     (solar-equinoxes-solstices)
     (holiday-sexp calendar-daylight-savings-starts
                   (format "Daylight Saving Time Begins %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-starts-time
                               (float 60))
                            calendar-standard-time-zone-name)))
     (holiday-sexp calendar-daylight-savings-ends
                   (format "Daylight Saving Time Ends %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-ends-time
                               (float 60))
                            calendar-daylight-time-zone-name)))))
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "MacBook-Pro.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(holiday-bahai-holidays nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; automatically set the mode to rjsx-mode
(require 'js)
(require 'sgml-mode)
(require 'smartparens)
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

(setq js-indent-level 4
      sgml-basic-offset 2)

(add-hook 'rjsx-mode-hook 'lsp)
(add-hook 'rjsx-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-(") #'sp-slurp-hybrid-sexp)
            (local-set-key (kbd "C-M-t") #'sp-transpose-hybrid-sexp)
            (local-set-key (kbd "C-M-T") #'sp-push-hybrid-sexp)
            (local-set-key (kbd "<C-tab>") #'sp-indent-adjust-sexp)
            (local-set-key (kbd "<C-M-tab>") #'sp-dedent-adjust-sexp)))
(provide 'init-js)

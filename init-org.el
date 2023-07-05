;;; init-org.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package org-pomodoro)
(use-package org-tree-slide
  :bind
  (("<f8>" . org-tree-slide-mode)
   ("S-<f8>" . org-tree-slide-skip-done-toggle)))
(global-set-key (kbd "<f7>") #'org-timer-start)

(use-package org-pomodoro)
(use-package org-journal)
(use-package org-download)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/path/to/org-files/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-directory "/Users/zromero/Dropbox/org/roam")
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(elpaca-wait)
(setq calendar-islamic-all-holidays-flag t)

(require 'org-capture)
(require 'org-agenda)
(require 'org-clock)
(require 'org-table)
(require 'org-habit)
(require 'org-duration)

(require 'org-protocol)

(setq org-roam-v2-ack t)

(defvar zr/org-directory "/Users/zachary.romero/org/")

(defun zr/org-file (path)
  (concat zr/org-directory path))
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-roam-mode-map
         ("C-c n j" . org-journal-new-entry)
         ("C-c n a t" . org-roam-dailies-capture-today)
         ("C-c n a y" . org-roam-dailies-capture-yesterday)
         ("C-c n a m" . org-roam-dailies-capture-tomorrow)
         ("C-c n a w" . org-roam-dailies-capture-this-week)
         ("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-graph))
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-directory "~/org/roam")
  (org-roam-db-autosync-mode)
  (org-roam-setup))


(defvar zr/refile-file (zr/org-file "refile.org"))
(defvar zr/notes-file (zr/org-file "notes.org"))
(defvar zr/organizer-file (zr/org-file "organizer.org"))
(defvar zr/tickler-file (zr/org-file "tickler.org"))

(defun zr/open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun zr/open-refile ()
  (interactive)
  (find-file zr/refile-file))

(defun zr/open-organizer ()
  (interactive)
  (find-file zr/organizer-file))

(defun zr/open-notes ()
  (interactive)
  (find-file zr/notes-file))

(defun zr/til ()
  (interactive)
  (find-file zr/notes-file)
  (goto-char (point-min))
  (search-forward "* TIL")
  (insert (format "\n** %s " (format-time-string "%d-%m-%Y"))))

(require 'org-pomodoro)
(defconst zr/org-pomodoro-bitbar-file-name
  "~/dev/bitbar/org-pomodoro.10s.sh"
  "The name of the org-pomodoro file for bitbar to see.")
(defvar zr/org-pomodoro-ticker
  0
  "Ticker to keep track of how many ticks called.")

(defun zr/org-pomodoro-color (time)
  "Calculate the color to display TIME in bitbar."
  (if (equal org-pomodoro-state ':short-break)
      "blue"
    (let* ((parts (split-string time ":"))
           (mins (string-to-number (car parts)))
           (secs (string-to-number (cadr parts))))
      (if (< mins 5) "red" "black"))))

(defun zr/org-pomodoro-update-bitbar ()
  "Update bitbar with pomodoro time."
  (let* ((time-str (org-pomodoro-format-seconds))
         (color-str (zr/org-pomodoro-color time-str ))
         (file-name (format "~/pomodoro.txt")))
    (if (equal "00:00" time-str)
        (shell-command-to-string (format "echo '0' > %s" file-name))
      (shell-command-to-string (format "echo '🍅%s | color=\"%s\"' > %s" (org-pomodoro-format-seconds) color-str file-name)))))


(defvar zr/org-clock--timer nil "Timer to write to file.")

(defun zr/org-clock-update-bitbar ()
  (let* ((time-secs (float-time (time-subtract (current-time) org-clock-start-time)))
         (mins (/ time-secs 60))
         (secs (mod time-secs 60))
         (file-name "~/pomodoro.txt")
         (time-str (format "%02dm%02ds" mins secs)))
    (shell-command-to-string (format "echo '%s' > %s" time-str file-name))))

(defun zr/org-clock-start-timer ()
  (message "starting org-clock timer")
  (when (> (float-time (time-subtract (current-time) org-pomodoro-last-clock-in)) 1)
    ;; you can start org timer printer
    (when zr/org-clock--timer
      (cancel-timer zr/org-clock--timer))
    (setq zr/org-clock--timer (run-with-timer 5 5 #'zr/org-clock-update-bitbar))))
(defun zr/org-clock-stop-timer ()
  (message "stopping org-clock timer")
  (when zr/org-clock--timer
    (zr/org-pomodoro-delete-bitbar-file)
    (cancel-timer zr/org-clock--timer)
    (setq zr/org-clock--timer nil)))


(defun zr/org-pomodoro-bitbar-tick ()
  (setq zr/org-pomodoro-ticker (1+ zr/org-pomodoro-ticker))
  (when (= (mod zr/org-pomodoro-ticker 10) 0)
    (zr/org-pomodoro-update-bitbar)))

(defun zr/org-pomodoro-start-bitbar-file ()
  "Create bitbar file."
  ;; (shell-command-to-string
  ;;  (concat (format "echo '#!/bin/bash' > %s" zr/org-pomodoro-bitbar-file-name)
  ;;          ";"
  ;;          (format "echo 'cat /Users/zromero/pomodoro.txt' >> %s" zr/org-pomodoro-bitbar-file-name)
  ;;          ";"
  ;;          (format "chmod +x %s" zr/org-pomodoro-bitbar-file-name)))
  (zr/org-pomodoro-update-bitbar))

(defun zr/org-pomodoro-delete-bitbar-file ()
  "Delete the org Pomodoro bitbar file."
  (shell-command-to-string
   "echo '0' > ~/pomodoro.txt"))

(add-hook 'org-pomodoro-tick-hook 'zr/org-pomodoro-bitbar-tick)
(add-hook 'org-pomodoro-started-hook 'zr/org-pomodoro-start-bitbar-file)
(add-hook 'org-pomodoro-killed-hook 'zr/org-pomodoro-delete-bitbar-file)
(add-hook 'org-pomodoro-finished-hook 'zr/org-pomodoro-delete-bitbar-file)
(add-hook 'org-pomodoro-break-finished-hook 'zr/org-pomodoro-delete-bitbar-file)

(add-hook 'org-clock-in-hook 'zr/org-clock-start-timer)
(add-hook 'org-clock-out-hook 'zr/org-clock-stop-timer)

(defun org-roam-dailies-capture-this-week (n &optional goto)
  (interactive "p")
  (let ((year (string-to-number (format-time-string "%Y" (current-time))))
        (month (string-to-number (format-time-string "%m" (current-time))))
        (day (string-to-number (format-time-string "%d" (current-time)))))
    (org-roam-dailies--capture (time-add (* (org-day-of-week day month year) -86400) (current-time)) t)
    (goto-char (point-min))
    (when (not (search-forward "plan.txt" nil t))
      (goto-char (point-max))
      (insert "\n* plan.txt\n\n** Monday\n\n** Tuesday\n\n** Wednesday\n\n** Thursday\n\n** Friday\n\n** Saturday\n\n** Sunday"))))

(defun zr/org-write-schedule ()
  "Write a schedule."
  (interactive)
  (pcase-let* ((start-pos (point))
               (now (decode-time))
               (`(_ _ ,hour _ _ _ _ _ _) now))
    (while (< hour 23)
      (insert (format " - %d:00 : " hour))
      (insert "\n")
      (setq hour (1+ hour)))
    (org-indent-region start-pos (point))))

(global-set-key (kbd "C-c c") 'org-capture)

(define-key org-mode-map (kbd "C-c C-x C-m") #'org-pomodoro)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "|" "DONE(d)" "CANCELLED(c)")))

;; Show the daily agenda by default
(setq org-agenda-span 'day)

;; Hide tasks that are scheduled in the future
(setq org-agenda-todo-ignore-scheduled 'future)

;; Use "second" instead of "day" for time comparision
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;; TODO: fixme
(setq org-capture-templates
      '(("t" "Todo" entry (file zr/refile-file)
         "* TODO %?\n   %U\n   %l" :empty-lines 1)
        ("T" "Todo with Clipboard" entry (file zr/refile-file)
         "* TODO %?\n   %U\n   %c" :empty-lines 1)
        ("n" "Note" entry (file zr/refile-file)
         "* %?\n   %U\n   %l" :empty-lines 1)
        ("N" "Note with Clipboard" entry (file zr/refile-file)
         "* %?\n   %U\n   %c" :empty-lines 1)
        ("r" "Reading List" entry (file+headline zr/organizer-file "Reading List")
         "* TODO %?\n   %U\n   %l" :empty-lines 1)))

(setq org-adapt-indentation t)
(setq org-columns-default-format "%80ITEM(Task) %11TODO %10Effort(Effort){:} %10CLOCKSUM")
(setq org-agenda-todo-ignore-deadlines nil)
(setq org-agenda-files (list zr/organizer-file zr/tickler-file "~/org/calendar.org"))
(setq org-default-notes-file zr/refile-file)
(setq org-adapt-indentation t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-log-into-drawer t)
(setq org-agenda-include-diary t)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-<up>") #'org-move-subtree-up)
            (local-set-key (kbd "s-<down>") #'org-move-subtree-down)
            (local-set-key (kbd "<M-S-return>") #'org-insert-todo-heading)
            (local-set-key (kbd "<C-S-return>") #'org-insert-todo-heading-respect-content)))


(require 'org-faces)

(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.15)
                (org-level-6 . 1.15)
                (org-level-7 . 1.15)
                (org-level-8 . 1.15)))
  (set-face-attribute (car face) nil :font "Hack" :weight 'medium :height (cdr face)))
(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.8)
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun org-clear-checklists-items-in-subtree ()
  (interactive)
  (let ((pt (point)))
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (while (search-forward "- [X]" nil t)
      (backward-char 2)
      (delete-char 1)
      (insert " "))
    (widen)
    (goto-char pt)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (go . t)
   (java . t)
   (shell . t)
   (ruby . t)
   (latex . t)
   (scheme . t)
   (lilypond . t)
   (R . t)
   (latex . t))) ; this line activates dot
(setq org-confirm-babel-evaluate nil)

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    ;; also skip if future scheduled
    (let* ((scheduled-time (org-get-scheduled-time (point)))
           (future-scheduled (and scheduled-time (time-less-p (current-time) scheduled-time))))
      (when future-scheduled (setq should-skip-entry t)))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun zac/org-skip-future-scheduled ()
  (let* ((skip (save-excursion (org-entry-end-position)))
         (scheduled-time (org-get-scheduled-time (point)))
         (future-scheduled (and scheduled-time (time-less-p (current-time) scheduled-time))))
    (if future-scheduled skip nil)))

(setq org-agenda-custom-commands
      '(("n" "Agenda / INTR / PROG / NEXT"
         ((agenda "" nil)
          (todo "INTR" nil)
          (todo "PROG" nil)
          (todo "NEXT" nil)
          (tags "+assorted+TODO=\"NEXT\""
                ((org-agenda-overriding-header "Next Assorted"))))
         nil)))

;; Old org agenda setup
;; (setq org-agenda-custom-commands
;;       (quote (("g" "GTD"
;;                ((agenda "" nil)
;;                 (tags-todo "gtd+assorted-@market-frozen"
;;                            ((org-agenda-overriding-header "Tasks Backlog")
;;                             (org-agenda-skip-function #'zac/org-skip-future-scheduled)))
;;                                         ;(tags-todo "gtd-assorted/NEXT"
;;                                         ;           ((org-agenda-overriding-header "Projects NEXT Tasks")
;;                                         ;            (org-agenda-todo-ignore-scheduled t)))
;;                 (tags-todo "gtd-assorted-@market-frozen/TODO"
;;                            ((org-agenda-overriding-header "Project TODO Tasks")
;;                             (org-agenda-todo-ignore-scheduled t)
;;                             (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;                 (tags-todo "gtd+deepw-@market-frozen/TODO"
;;                            ((org-agenda-overriding-header "Deep Work Candidates")
;;                             (org-agenda-todo-ignore-scheduled t)
;;                             (org-agenda-skip-function (lambda () (or (zac/org-skip-future-scheduled) (my-org-agenda-skip-all-siblings-but-first))))))))
;;               ("w" "Work"
;;                ((agenda "" nil)
;;                 (tags-todo "+@office/TODO"
;;                            ((org-agenda-overriding-header "TODO Tasks")
;;                             (org-agenda-todo-ignore-scheduled t)
;;                             (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first))))
;;                ((org-agenda-tag-filter-preset '("+@office")))))))

;; TODO Find a better way to manage stuck projects. It seems like the 2nd level
;;      header shouldn't have a TODO tag.
(setq org-stuck-projects
      '("+LEVEL=2+proj/-DONE-CANCELLED"
        ("TODO" "NEXT")
        nil
        ""))

(setq org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml-1.2023.1.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(require 'ox-latex)
(require 'ox-texinfo)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)
(setq org-latex-listing t)

(global-set-key (kbd "C-c l") #'org-store-link)

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "./media")

(setq org-id-link-to-org-use-id t)

(add-to-list 'display-buffer-alist
                  '("\\*org-roam\\*"
                    (display-buffer-in-direction)
                    (direction . right)
                    (window-width . 0.33)
                    (window-height . fit-window-to-buffer)))
(setq org-roam-directory (zr/org-file "roam"))

(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n g") #'org-roam-graph)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n j") #'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n t") #'org-roam-dailies-find-today)
(global-set-key (kbd "C-c n d") #'deft)
(define-key org-mode-map (kbd "C-'") nil)


(require 'org-journal)

(setq org-journal-date-prefix "#+title: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-dir (zr/org-file "roam/daily/")
      org-journal-date-format "%A, %d %B %Y"
      org-journal-time-prefix  "** ")

(use-package deft
  :bind
  (:map org-roam-mode-map
   ("C-c n d" . deft))
  :config
  (setq deft-directory (zr/org-file "roam"))
  (setq deft-use-filename-as-title t)
  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n#\\+title:")
  :custom
       (deft-recursive t)
       (deft-use-filter-string-for-filename t)
       (deft-default-extension "org")
       (deft-directory org-roam-directory))

(setq org-show-notification-handler #'message)
(use-package org-anki
  :bind
  (:map org-mode-map
        ("C-c f" . org-anki-sync-entry))
  :init
  (customize-set-variable 'org-anki-default-deck "org"))

(provide 'init-org)

;;; init-org.el ends here

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

(elpaca-wait)
(setq calendar-islamic-all-holidays-flag t)

(require 'org-capture)
(require 'org-agenda)
(require 'org-clock)
(require 'org-table)
(require 'org-habit)
(require 'org-duration)

(server-start)
(require 'org-protocol)

(setq org-roam-v2-ack t)

(defvar zr/org-directory "~/org-workspace/")

(defun zr/org-file (path)
  (concat zr/org-directory path))

;; (use-package org-roam
;;   :ensure t
;;   :custom
;;   (org-roam-directory (file-truename "~/org-workspace/roam"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c n j" . org-roam-dailies-capture-today)
;;          :map org-roam-mode-map
;;          ("C-c n j" . org-journal-new-entry)
;;          ("C-c n a t" . org-roam-dailies-capture-today)
;;          ("C-c n a y" . org-roam-dailies-capture-yesterday)
;;          ("C-c n a m" . org-roam-dailies-capture-tomorrow)
;;          ("C-c n a w" . org-roam-dailies-capture-this-week)
;;          ("C-c n l" . org-roam)
;;          ("C-c n f" . org-roam-find-file)
;;          ("C-c n g" . org-roam-graph))
;;   :config
;;   (setq org-roam-v2-ack t)
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   (org-roam-setup))

(use-package org-present)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(defconst zr/refile-file (zr/org-file "capture2.org"))
(defconst zr/notes-file (zr/org-file "notes.org"))
(defconst zr/organizer-file (zr/org-file "gtd.org"))
(defconst zr/calendar-file (zr/org-file "calendar.org"))
(defconst zr/tickler-file (zr/org-file "tickler.org"))

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
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; Show the daily agenda by default
(setq org-agenda-span 'day)

;; List of tags that should be attached to the project but are irrelevant for task
(setq org-tags-exclude-from-inheritance '("aof" "operational"))

;; Hide tasks that are scheduled in the future
(setq org-agenda-todo-ignore-scheduled 'future)

;; Use "second" instead of "day" for time comparision
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

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
         "* TODO %?\n   %U\n   %l" :empty-lines 1)
        ("c" "Contacts" entry (file+headline "~/org-workspace/apps/contacts.org" "Inbox")
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")))

(setq org-adapt-indentation t)
(setq org-columns-default-format "%80ITEM(Task) %11TODO %10Effort(Effort){:} %10CLOCKSUM")
(setq org-agenda-todo-ignore-deadlines nil)
(setq org-agenda-files (list zr/organizer-file zr/tickler-file zr/calendar-file))
(setq org-default-notes-file zr/refile-file)
(setq org-adapt-indentation t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-log-into-drawer t)
(setq org-log-done 'time)
(setq org-agenda-include-diary t)
(setq org-log-into-drawer t)

;;; Babel


(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (scheme . t)
   (gnuplot . t)))
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))


;;; Exporting

(setq org-src-fontify-natively t)
(setq org-latex-listing t)


;;; Agenda

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

(defun org-agenda-skip-future-scheduled ()
  (let* ((scheduled-time (org-get-scheduled-time (point)))
         (current-date (org-today)))
    (if (and scheduled-time
             (> (time-to-days scheduled-time) current-date))
        (save-excursion (or (outline-next-heading) (point-max)))
      nil)))

(setq org-agenda-skip-function 'org-agenda-skip-future-scheduled)

(setq org-stuck-projects
      '("+LEVEL=2-TODO=\"DONE\"-maybe-operational"
        ("NEXT")
        nil
        ""))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s") (todo . " %i %-12:c") (tags . " %i %-12:c") (search . " %i %-12:c")))

(setq org-agenda-custom-commands
      '(("r" "Reading"
         ((tags-todo "+reading"()))
         nil)
        ("n" "Agenda / INTR / PROG / NEXT"
         ((agenda "" nil)
          (tags-todo "-maybe+stratp+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Strategic Plan")))
          (tags-todo "-maybe+@focus+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Focus Work Candidates")))
          (tags-todo "-maybe+@light+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Light Work")
                      (org-agenda-prefix-format " %i %-12:c%?-12t% s")))
          (tags-todo "-maybe+@home+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "At Home")))
          (tags-todo "-maybe+@anytime+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Anytime")))
          (tags-todo "-maybe+@errands+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "Errands Outside")))
          (stuck)
          (todo "WAIT" nil
                ((org-agenda-overriding-header "Waiting Tasks")))
          (tags-todo "-maybe-@anytime-@home-@light-@errands-@focus+TODO=\"NEXT\"" nil))
         nil)))


;;; Org Roam
(add-to-list 'display-buffer-alist
                  '("\\*org-roam\\*"
                    (display-buffer-in-direction)
                    (direction . right)
                    (window-width . 0.33)
                    (window-height . fit-window-to-buffer)))


;;; Org Journal
(require 'org-journal)

(setq org-journal-date-prefix "#+title: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-dir (zr/org-file "roam/daily/")
      org-journal-date-format "%A, %d %B %Y"
      org-journal-time-prefix  "** ")


;;; Keybindings
(global-set-key (kbd "C-c c") 'org-capture)
(define-key org-mode-map (kbd "C-c C-x C-m") #'org-pomodoro)
(define-key org-mode-map (kbd "<s-down>") #'org-move-subtree-down)
(define-key org-mode-map (kbd "<s-up>") #'org-move-subtree-up)
(global-set-key (kbd "C-c l") #'org-store-link)
;; (global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
;; (global-set-key (kbd "C-c n f") #'org-roam-node-find)
;; (global-set-key (kbd "C-c n g") #'org-roam-graph)
;; (global-set-key (kbd "C-c n i") #'org-roam-node-insert)
;; (global-set-key (kbd "C-c n c") #'org-roam-capture)
;; (global-set-key (kbd "C-c n j") #'org-roam-dailies-capture-today)
;; (global-set-key (kbd "C-c n t") #'org-roam-dailies-find-today)

(define-key org-mode-map (kbd "C-'") nil)

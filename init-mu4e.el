(require 'mu4e)
(require 'mu4e-icalendar)
(mu4e-icalendar-setup)
(setq
 mue4e-headers-skip-duplicates t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"

 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/[Gmail].All Mail"
 mu4e-sent-folder   "/[Gmail].Sent Mail"
 mu4e-drafts-folder "/[Gmail].Drafts"
 mu4e-trash-folder  "/[Gmail].Trash")

(setq mu4e-get-mail-command  "offlineimap")
(setq mu4e-attachment-dir "~/Downloads")

;; (require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it)
(setq user-mail-address "zachary.romero@blueprintprep.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(provide 'init-mu4e)

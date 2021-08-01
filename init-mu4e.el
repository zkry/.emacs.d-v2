(require 'mu4e)
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
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash")

(setq mu4e-get-mail-command  "mbsync -a")
(setq mu4e-attachment-dir "~/Downloads")

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq user-mail-address "zacromero@posteo.net"
      smtpmail-smtp-server "posteo.de"
      smtpmail-default-smtp-server "posteo.de"
      smtpmail-smtp-service 587)
(provide 'init-mu4e)

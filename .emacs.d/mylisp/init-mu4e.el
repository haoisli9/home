
(use-package mu4e
  :defer 3
  :init
  ;; (require 'cygwin-mount)
  :config
  (setq mu4e-mu-binary "D:/Users/emacs-build-main/msys64/usr/local/bin/mu.exe")
  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  ;; set home mail directory 
  (setq mu4e-mu-home "~/Maildir/")

  ;; the next are relative to the root maildir
  ;; (see `mu info`).
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (setq mu4e-attachment-dir "~/Maildir/attachment")
  (setq
        mu4e-drafts-folder "/drafts"
        mu4e-sent-folder "/sent"
        )
  (setq mu4e-trash-folder
        (lambda (msg)
          ;; the 'and msg' is to handle the case where msg is nil
          (cond ((and msg (mu4e-message-contact-field-matches msg :to "13991819104@139.com"))
                 "/139/trash")
                ((and msg (mu4e-message-contact-field-matches msg :to "haoisli9@hotmail.com"))
                 "/hotmail/Junk")
                (t "/trash"))))
  ;; (setq mu4e-sent-folder
  ;;      (lambda (msg)
  ;;        ;; the 'and msg' is to handle the case where msg is nil
  ;;        (cond ((and msg (mu4e-message-contact-field-matches msg :to "13991819104@139.com"))
  ;;               "/139/sent")
  ;;              ((and msg (mu4e-message-contact-field-matches msg :to "haoisli9@hotmail.com"))
  ;;               "/hotmail/sent")
  ;;              "/sent")))
  
 (setq mu4e-refile-folder
        (lambda (msg)
          (cond
           ;; messages sent by me go to the sent folder
           ((mu4e-message-sent-by-me msg (mu4e-personal-addresses))
            mu4e-sent-folder)
           ;; everything else goes to /archive
           ;; important to have a catch-all at the end!
           (t  "/archive"))))
  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq   mu4e-maildir-shortcuts
          '((:maildir "/archive" :key ?a)
            (:maildir "/139/inbox"   :key ?1)
            (:maildir "/hotmail/inbox"   :key ?2)
            (:maildir "/drafts"  :key ?d)
            (:maildir "/sent"    :key ?s)))

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '( (:date    .  20)    ;; alternatively, use :human-date
           (:flags         .   6)
           (:from          .  25)
           (:subject       .  nil))) ;; alternatively, use :thread-subject

  ;; program to get mail; alternatives are 'fetchmail', 'getmail'
  ;; isync or your own shellscript. called when 'U' is pressed in
  ;; main view.

  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  (setq mu4e-get-mail-command "D:/Users/emacs-build-main/msys64/usr/bin/mbsync -a")
  (setq mu4e-view-auto-mark-as-read nil)
  ;;Fixing duplicate UID errors when using mbsync and mu4e
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-compose-format-flowed t)
  (setq mu4e-view-prefer-html t)
  ;; (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-results-limit 1000)

  (setq mu4e-view-use-gnus t)
  ;; use this do not show external images.
  (setq gnus-blocked-images nil)

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  (setq message-confirm-send t)
  ;; flag face
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-precise-alignment t)
  (setq mu4e-headers-show-threads nil)
  
  ;; all the icons后有时对不齐，先停用；
  ;; (setq mu4e-headers-unread-mark `("u" . ,(propertize
  ;;                                          "○"
  ;;                                          'face '(:foreground "red") 
  ;;                                          'display (all-the-icons-faicon "diamond"))))
  (setq mu4e-headers-unread-mark `("u" . ,(propertize
                                           "✿"
                                           'face '(:foreground "green") 
                                           )))
  (setq mu4e-headers-related-label `("R" . "♥"))

  (add-hook 'mu4e-view-mode-hook 'olivetti-mode)

  ;; general emacs mail settings; used when composing e-mail
  ;; the non-mu4e-* stuff is inherited from emacs/message-mode
  (setq mu4e-compose-reply-to-address "13991819104@139.com"
        mu4e-compose-dont-reply-to-self t
        user-mail-address "13991819104@139.com"
        user-full-name  "Li Hao")
  ;; (setq mu4e-compose-signature "Foo X. Bar\nhttp://www.example.com\n")

  ;;设置发送邮件的服务器地址
  (setq smtpmail-smtp-server "smtp.139.com"
        ;; smtpmail-default-smtp-server "smtp.139.com"
        smtpmail-smtp-service 25
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        ;; smtpmail-local-domain "example.com"
        smtpmail-auth-credentials "~/.authinfo"
        ;; if you need offline mode, set these -- and create the queue dir
        ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
        ;; smtpmail-queue-mail  nil
        ;; smtpmail-queue-dir  "~/Maildir/queue/cur"
        )
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
)

;; (use-package mu4e-maildirs-extension
;;   :ensure t
;;   :after mu4e
;;   :config
;;   ;; use mu4e-maildirs-extension package.
;;   (mu4e-maildirs-extension)
;;   )

(message "mu4e configuration loaded.")

(provide 'init-mu4e)

(require 'gnus)

;; 设置自己的默认email地址，和用户名
(setq user-full-name	"LIHAO")
(setq user-mail-address "13991819104@139.com")

;;设置获取邮件的服务器地址

;; (setq gnus-select-method '(nnml ""))
;; set pop server
;; (setq mail-sources
;;       '((pop :server "xamail.datangmobile.cn"   ;; 在这里设置 pop3 服务器
;;              :leave 14                         ;; leave the article on server.
;;              :port "pop3"
;;              )))
;; set smtp
;; (setq smtpmail-auth-credentials
;;    '(("xamail.datangmobile.cn"                ;; SMTP 服务器
;;       25                                   ;; 端口号
;;       "lihao@datangmobile.cn"                 ;; 用户名
;;       "")))                    ;; 密码

(setq gnus-select-method '(nnimap "139"
                                  (nnimap-address "imap.139.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-authinfo-file "~/.authinfo")
                                  ))

;; (setq gnus-select-method '(nnmaildir "139"
;;                                      (directory "~/Maildir/139")
;;                                      (expire-age never)))

(add-to-list 'gnus-secondary-select-methods '(nnimap "hotmail"
                                                     (nnimap-address "imap-mail.outlook.com")
                                                     (nnimap-server-port 993)
                                                     (nnimap-stream ssl)
                                                     (nnimap-authinfo-file "~/.authinfo")
                                                     ))
;;语言环境设定
;; (set-language-environment 'Chinese-GB)
;; (setq gnus-group-name-charset-group-alist
;;       '(("\\.com\\.cn:" . gbk)
;;         ("news\\.newsfan\\.net" . gbk)))
;; (setq gnus-group-name-charset-method-alist
;;       '(((nntp "news.newsfan.net") . gbk)))

(setq gnus-default-charset 'chinese-iso-8bit
      gnus-group-name-charset-group-alist '((".*" . utf-8))
      gnus-summary-show-article-charset-alist
      '((1 . cn-gb-2312)
        (2 . utf-8)
        (3 . chinese-iso-8bit)
        (4 . gbk)
        (5 . big5)
        (6 . gb18030))
      gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown iso-8859-1))
;;解决gb18030乱码
(setq gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown gb18030))

;;设定要显示的头信息
;; (setq gnus-visible-headers
;;       '("^From:\\|^Newsgroups:\\|^Subject:\\|^Date:"))
(setq gnus-boring-article-headers
      '(empty many-to followup-to reply-to))
(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        gnus-thread-sort-by-most-recent-date
        ))

;; (remove-hook 'gnus-mark-article-hook
;;              'gnus-summary-mark-read-and-unread-as-read)
;; (add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(eval-after-load 'gnus-topic
  '(progn
     ;; (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("139" visible nil nil))
                                 (("hotmail" visible nil nil))
                                 ))

     ;; ;; ;; key of topic is specified in my sample ".gnus.el"
     (setq gnus-topic-alist '(
                              ("139" ; the key of topic
                               "INBOX")
                              ("hotmail"
                               "nnimap+hotmail:Inbox")
                              ("Gnus")))
     ;; ;; see latest 200 mails in topic "gmail" when press Enter on any group inside topic
     (gnus-topic-set-parameters "Gnus" '((display . 200)))
     (gnus-topic-set-parameters "139" '((display . 100) (auto-expire . t) (expiry-wait . 15) (visible . t)))
     (gnus-topic-set-parameters "hotmail" '((display . 100) (auto-expire . t) (expiry-wait . 30) (visible . t)))
     ))

;; (setq gnus-group-line-format "%P%M%S[%5t]%5y : %B%(%g%)\n")
;; (setq gnus-summary-line-format "%4:%U%R%z |%B%s %-60=|%-20,20f |%&user-date; \n")
(defun gnus-my-set-format ()
  (setq
   ;; gnus-topic-line-format (concat "%i[ "
   ;;                                " %(%{%n -- %A%}%) ]%v\n")
   ;; gnus-group-line-format (concat "%1M%1S%5y "
   ;;                                " : %(%-50,50G%)\n")
   ;; gnus-summary-line-format (concat "%1{%U%R%z: %}%[%2{%&user-date;%}%] "
   ;;                                  " %4{%-20,20n%} %3{"
   ;;                                  " %}%(%1{%B%}%s%)\n")
   ;; gnus-user-date-format-alist (list (cons t (concat
   ;;                                            " %Y-%m-%d %H:%M")))
   gnus-topic-line-format (concat "%i[ "
                                  (propertize ;(all-the-icons-faicon "folder-open")
                                   (all-the-icons-material "folder")
                                   'display '(raise 0.0))
                                  " %(%{%n -- %A%}%) ]%v\n")

   gnus-group-line-format (concat "%1M%1S%5y "
                                  (propertize ;(all-the-icons-faicon "envelope-o")
                                   (all-the-icons-material "mail")
                                   'display '(raise 0.0))
                                  " : %(%-50,50G%)\n")

   gnus-summary-line-format (concat "%1{%U%R%z: %}%[%2{%&user-date;%}%] "
                                    (propertize ;(all-the-icons-faicon "male")
                                     (all-the-icons-material "person")
                                     'display '(raise 0.0))
                                    " %4{%-34,34n%} %3{"
                                    (propertize ;(all-the-icons-faicon "terminal")
                                     (all-the-icons-material "send")
                                     'display '(raise 0.0))
                                    " %}%(%1{%B%}%s%)\n")

   ;; gnus-user-date-format-alist '((t . " %Y-%m-%d %H:%M"))
   gnus-user-date-format-alist (list (cons t (concat
                                              (propertize ;(all-the-icons-faicon "calendar" :v-adjust -0.01)
                                               (all-the-icons-material "date_range")
                                               'display '(raise 0.0))
                                              " %Y-%m-%d %H:%M")))
   
   )
  
  (set-fontset-font t '(#xe2c7 . #xe2c7) (font-spec :family "Material Icons"))
  (set-fontset-font t '(#xe158 . #xe163) (font-spec :family "Material Icons"))
  (set-fontset-font t '(#xe7fd . #xe7fd) (font-spec :family "Material Icons"))
  (set-fontset-font t '(#xe916 . #xe916) (font-spec :family "Material Icons"))

  (setq gnus-sum-thread-tree-root "\u2564 "
          gnus-sum-thread-tree-false-root "\u2564 "
          gnus-sum-thread-tree-leaf-with-other "\u251c\u2500\u252c\25b9 "
          gnus-sum-thread-tree-vertical "\u2502 "
          gnus-sum-thread-tree-single-leaf "\u2570\u2500\u25b9 ")
  ;; (setq gnus-sum-thread-tree-root "> "
  ;;         gnus-sum-thread-tree-false-root "+ "
  ;;         gnus-sum-thread-tree-leaf-with-other "+-> "
  ;;         gnus-sum-thread-tree-vertical "| "
  ;;         gnus-sum-thread-tree-single-leaf "|-> ")
   )

(gnus-my-set-format)

(cond (window-system
       (setq custom-background-mode 'light)
       (defface my-group-face-1
         '((t (:foreground "red1" :bold t))) "First group face")
       (defface my-group-face-2
         '((t (:foreground "DarkSeaGreen4" :bold t))) "Second group face")
       (defface my-group-face-3
         '((t (:foreground "Green" :bold t))) "Third group face")
       (defface my-group-face-4
         '((t (:foreground "SteelBlue" :bold t))) "Fourth group face")
       (defface my-group-face-5
         '((t (:foreground "Cyan" :bold t))) "Fifth group face")))

(setq gnus-group-highlight
      '(((> unread 200) . my-group-face-1)
        ((and (< level 3) (zerop unread)) . my-group-face-2)
        ((< level 3) . my-group-face-3)
        ((zerop unread) . my-group-face-4)
        (t . my-group-face-5)))

;; 设定屏幕的分割比例
;; 两屏设置
(gnus-add-configuration '(article (vertical 1.0
                                            (summary .30 point) (article 1.0))))
;; 三屏设置
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;; 	       (vertical 25
;; 			 (group 1.0))
;; 	       (vertical 1.0
;; 			 (summary 0.25 point)
;; 			 (article 1.0)))))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            ;; (setq fill-column 100)
            (setq left-margin-width 10)
            (setq right-margin-width 10)
            ;; (gnus-article-fill-long-lines)
            ))

;;自动显示图片
(auto-image-file-mode)
;; (add-to-list 'mm-attachment-override-types "image/*")
;; gnus 默认不显示html邮件中的图片，只好手工hook一下；
(add-hook 'gnus-article-prepare-hook 'gnus-article-show-images)

;;杂 项
(setq gnus-confirm-mail-reply-to-news t
      message-kill-buffer-on-exit t
      message-elide-ellipsis "[...]\n"
)

;; * 键，帖子被拷贝到本地的 cache 中保存起来，再次 Meta-* 取消
(setq gnus-use-cache 'passive)
;; 可以保留同主体中已读邮件，把 'some 改为t可以下载所有文章
(setq gnus-fetch-old-headers 'some)

;;------------------------------------------------------------------------------------------
(setq gnus-posting-styles
      '((".*"
	 (name "LH")
	 (signature "sent from Emacs+Gnus\n")
         )
        ;; Reply to a message from the same subaddress the message
        ;; was sent to.
        ((header "to" "13991819104@139.com")
         (address "13991819104@139.com"))
        ((header "to" "haoisli9@hotmail.com")
         (address "haoisli9@hotmail.com"))
        ))

;;设置发送邮件的服务器地址
(setq smtpmail-default-smtp-server "smtp.139.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 25
      smtpmail-auth-credentials "~/.authinfo"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defvar smtp-accounts
  '(;; (plain    "me@foo.com" "smtp.foo.com" 25  "user-foo" "pass-foo")
    (login    "13991819104@139.com" "smtp.139.com" 25 "13991819104@139.com" nil)
    (login    "haoisli9@hotmail.com" "smtp-mail.outlook.com" 25 "haoisli9@hotmail.com" nil)
    ;; (cram-md5 "me@foo.com" "smtp.hoo.com" 25  "user-hoo" nil)
    ;; (ssl      "me@bar.com" "smtp.bar.com" 587 "user-bar" "pass-bar" "key" "cert")
    ;; (ssl      "me@baz.com" "smtp.baz.com" 587 "user-baz" "pass-baz" "key" "cert")
    ))

(defun set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-login (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-supported (list mech))
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        ;; starttls-gnutls-program "gnutls-cli"
        ;; If you set starttls-use-gnutls as t, It means use gnutls-cli but
        ;; starttls command tool. Of course ,you need install gnutls first on
        ;; your OS.
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (cl-loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (auth-mech address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((memq auth-mech '(cram-md5 plain))
               (cl-return (apply 'set-smtp (cons auth-mech auth-spec))))
              ((eql auth-mech 'login)
               (cl-return (apply 'set-smtp-login (cons auth-mech auth-spec))))
              ((eql auth-mech 'ssl)
               (cl-return (apply 'set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
          finally (error "Cannot infer SMTP information."))))

(defadvice smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (change-smtp)))

(ad-activate 'smtpmail-via-smtp)

;;-------------------------------------------------------------------------------
;; face definations
(with-eval-after-load 'gnus
  (defun gnus-buffer-face-mode-variable ()
    (interactive)
    ;; (hl-line-mode)
    ;; (face-remap-add-relative 'hl-line '(:background "#6272a4"))
    (face-remap-add-relative 'gnus-header-from '(:foreground "Green"))
    (face-remap-add-relative 'gnus-summary-selected '(:background "#6272a4" :foreground "yellow"))
    ;; (make-face 'width-font-face)
    ;; (set-face-attribute 'width-font-face nil :family "Material Icons")   ;; FiraCode NF 16
    ;; (setq buffer-face-mode-face 'width-font-face)
    ;; (buffer-face-mode)
    )
    (add-hook 'gnus-mode-hook 'gnus-buffer-face-mode-variable))

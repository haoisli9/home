(require 'gnus)

;; 设置自己的默认email地址，和用户名
;; (setq user-mail-address	"lihao@datangmobile.cn"
;;       user-full-name	"LIHAO")
(setq user-mail-address	"13991819104@139.com"
      user-full-name	"LIHAO")

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

;; (setq gnus-select-method '(nnimap "imap.139.com"))
(setq gnus-select-method '(nnimap "139"
                                  (nnimap-address "imap.139.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-authinfo-file "~/.authinfo")
                                  ))

;; (add-to-list 'gnus-secondary-select-methods '(nnimap "imap.139.com"))
;; (add-to-list 'gnus-secondary-select-methods '(nnimap "139"
;;                                                      (nnimap-address "imap.139.com")
;;                                                      (nnimap-server-port 993)
;;                                                      (nnimap-stream ssl)
;;                                                      (nnimap-authinfo-file "~/.authinfo")
;;                                                      ))

;;设置发送邮件的服务器地址
;; (setq smtpmail-default-smtp-server "xamail.datangmobile.cn")
(setq smtpmail-default-smtp-server "smtp.139.com"
      ;; smtpmail-smtp-server "smtp.139.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      ;; smtpmail-smtp-service 25
      smtpmail-auth-credentials "~/.authinfo")

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
(setq gnus-visible-headers
      '("^From:\\|^Newsgroups:\\|^Subject:\\|^Date:"))
;; (setq gnus-boring-article-headers
;;       '(empty many-to followup-to reply-to))
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
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("misc" visible))
                                 ;; (("datangmobile" visible nil nil))
                                 (("139" visible nil nil))))

     ;; ;; ;; key of topic is specified in my sample ".gnus.el"
     (setq gnus-topic-alist '(
                              ;; ("datangmobile" ; the key of topic
                              ;;  "mail.misc")
                              ("139" ; the key of topic
                               "INBOX"
                               "nnfolder+archive:sent.2020")
                              ("misc" ; the key of topic
                               "dummy.group"
                               "nndraft:drafts")
                              ("Gnus")))
     ;; ;; see latest 200 mails in topic "gmail" when press Enter on any group inside topic
     (gnus-topic-set-parameters "Gnus" '((display . 200)))
     (gnus-topic-set-parameters "misc" '((display . 200)))
     ;; ;; (gnus-topic-set-parameters "datangmobile" '((display . 200) (total-expire . t) (expiry-wait . 7) (visible . nil)))
     (gnus-topic-set-parameters "139" '((display . 200) (auto-expire . t) (expiry-wait . 15) (visible . t)))
     ))

(setq gnus-group-line-format "%P%M%S[%5t]%5y : %B%(%g%)\n")
(setq gnus-summary-line-format "%4:%U%R%z |%B%s %-60=|%-20,20f |%&user-date; \n")

;; (setq gnus-sum-thread-tree-root "\u2564 "
;;         gnus-sum-thread-tree-false-root "\u2564 "
;;         gnus-sum-thread-tree-leaf-with-other "\u251c\u2500\u252c\25b9 "
;;         gnus-sum-thread-tree-vertical "\u2502 "
;;         gnus-sum-thread-tree-single-leaf "\u2570\u2500\u25b9 ")
(setq gnus-sum-thread-tree-root "> "
        gnus-sum-thread-tree-false-root "+ "
        gnus-sum-thread-tree-leaf-with-other "+-> "
        gnus-sum-thread-tree-vertical "| "
        gnus-sum-thread-tree-single-leaf "|-> ")

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
      '(((> unread 10) . my-group-face-1)
        ((and (< level 3) (zerop unread)) . my-group-face-2)
        ((< level 3) . my-group-face-3)
        ((zerop unread) . my-group-face-4)
        (t . my-group-face-5)))

;; 设定屏幕的分割比例
;; 两屏设置
(gnus-add-configuration '(article (vertical 1.0
                                            (summary .25 point) (article 1.0))))
;; 三屏设置
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;; 	       (vertical 25
;; 			 (group 1.0))
;; 	       (vertical 1.0
;; 			 (summary 0.25 point)
;; 			 (article 1.0)))))
;; (gnus-add-configuration
;;  '(summary
;;    (horizontal 1.0
;; 	       (vertical 25
;; 			 (group 1.0))
;; 	       (vertical 1.0
;; 			 (summary 1.0 point)))))

;; (add-hook 'gnus-article-prepare-hook
;;           (lambda ()
;;             ;; (setq fill-column 100)
;;             (setq left-margin 10)
;;             ;; (gnus-article-fill-long-lines)
;;             ))

;;自动显示图片
(auto-image-file-mode)
(add-to-list 'mm-attachment-override-types "image/*")
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

;; (setq gnus-posting-styles
;;       '((".*"
;; 	 (name "LH")
;; 	 (address "abc@dt.com")
;; 	 (signature "sent from Emacs+Gnus\n")
;; 	 ))
;; )

;; (with-eval-after-load 'gnus
;;   (defun gnus-buffer-face-mode-variable ()
;;     (interactive)
;;     ;; (hl-line-mode)
;;     ;; (face-remap-add-relative 'hl-line '(:background "#6272a4" :foreground "white"))
;;     ;; (face-remap-add-relative 'default '(:background "black"))
;;     ;; (face-remap-add-relative 'variable-pitch '(:background unspecified))
;;     (face-remap-add-relative 'bold '(:foreground "DarkSeaGreen4"))
;;     (face-remap-add-relative 'gnus-header-from '(:foreground "Green"))
;;     (face-remap-add-relative 'gnus-summary-selected '(:background "#6272a4" :foreground "yellow"))
;;     (make-face 'width-font-face)
;;     (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 16")   ;; FiraCode NF 16
;;     (setq buffer-face-mode-face 'width-font-face)
;;     (buffer-face-mode))
;;     (add-hook 'gnus-mode-hook 'gnus-buffer-face-mode-variable))

;; -*- coding: utf-8; lexical-binding: t; -*-

;; Author: lihao

;; (require 'benchmark-init)
;; (benchmark-init/activate)

;; start server mode
(if (display-graphic-p)  (server-start))

;; 在注册表中建如下键值：
;; HKEY_CLASSES_ROOT\*\shell\Edit with Emacs\command
;; 并将其default的值设为：
;; "path\to\emacsclientw.exe" --no-wait --alternate-editor="path\to\runemacs.exe" "%1"

;; measurement startup timer.
(defvar my-init-time 'nil)
(defun my-display-benchmark()
  (message "Loaded %s packages in %.03fs, init time %s up time %s"
           (length package-activated-list)
           (or my-init-time
               (setq my-init-time (float-time (time-subtract (current-time) before-init-time)))
               )
           (emacs-init-time)
           (emacs-uptime)))
(add-hook 'emacs-startup-hook #'my-display-benchmark)

;; Emacs配置文件内容写到下面.
;;------------------------------------------------------------
;; enverioment configuration.
;;------------------------------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;; (add-to-list 'package-archives '("org"  . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu"  . "https://elpa.gnu.org/packages/"))

(package-initialize)

(message "package initialize.")

;; 将lisp目录放到加载路径的前面以加快启动速度
(add-to-list 'load-path "~/.emacs.d/mylisp/")
;; (let ((dir (locate-user-emacs-file "lisp")))
;;   (add-to-list 'load-path (file-name-as-directory dir)))
;; install all the sub-directories in the beginnging of load-path.
(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "~/.emacs.d/mylisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path)

;; (setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
;; (setq exec-path (append exec-path '("/sw/bin")))

;;------------------------------------------------------------------
;;{{{ basic configuration.
;;------------------------------------------------------------------
(if (display-graphic-p)
    (progn
      ;;设置窗口位置为屏库左上角(0,0)
      (set-frame-position (selected-frame) 180 20)
      ;;设置宽和高
      (set-frame-width (selected-frame) 98)
      (set-frame-height (selected-frame) 22)
      ;; 最大化窗口
      ;; (setq initial-frame-alist (quote ((fullscreen . maximized))))
      ))

(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      ;; (format   "%s:pixelsize=%d"  english english-size) :weight 'semi-bold)
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size :weight 'medium))))

;; Setting English Font
(set-face-attribute 'default nil :font "Iosevka Term 16")
;; (set-face-attribute 'default nil :font "Sarasa Mono SC Nerd 16")

;; Chinese Font
;; 当有字体已支持中文时，不用再次设置
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "微软雅黑" :size 30)))  ;; 微软雅黑，24

;; Emoji
(set-fontset-font "fontset-default" 'unicode "Sarasa Mono SC Nerd 12")
;; (set-fontset-font "fontset-default" 'unicode "FiraCode NF 12")
;; (set-fontset-font t 'unicode-bmp (font-spec :family "all-the-icons"))

;; use for fanyi Emoji.
(progn
  ;; set font for emoji (if before emacs 28, should come after setting symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    )))

;; (set-fontset-font t 'emoji (font-spec :family "Symbola") nil 'prepend)
;; (set-fontset-font t 'emoji "Segoe UI Emoji" nil 'prepend)

;; 支持字体缓存
(setq inhibit-compacting-font-caches t)

;; 让鼠标滚动更好用
(setq scroll-margin 1)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(modify-frame-parameters nil '((inhibit-double-buffering . t)))

;;--------------------------------------------------------------------
;; basic settings.
(setq inhibit-startup-screen t)
;; set scratch buffer message to nil.
(setq initial-scratch-message nil)
;; set scratch buffer to org-mode.
(setq initial-major-mode 'org-mode)

(setq warning-suppress-log-types '((comp)))
;; 关闭出错时的提示声
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)

(auto-image-file-mode t)
;; original is "auto"
;; (setq image-scaling-factor 1.0)

;;  Emacs handles most of the common image formats (SVG, JPEG, PNG, GIF and some others) internally, but images that
;; don't have native support in Emacs can still be displayed if an external conversion program (like ImageMagick "convert",
;; GraphicsMagick "gm" or "ffmpeg") is installed. 
(setq image-use-external-converter t)

(setq column-number-mode t)
(setq line-number-mode t)
;; used for auto-fill-mode
(setq fill-column 100)
;; wrap after characters of a certain category 
(setq word-wrap-by-category t)

;; linum style
(setq linum-format "%4d")
(display-line-numbers-mode t)
(toggle-indicate-empty-lines nil)
(global-visual-line-mode t)

(tool-bar-mode -1)
;;取消滚动栏
(if (display-graphic-p) (set-scroll-bar-mode nil))

;; 关闭tooltip
;; (tooltip-mode 0)

;; disable cursor blink.
(blink-cursor-mode -1)

(setq show-paren-mode t)
(setq show-paren-style 'parenthesis)
;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
(add-hook 'minibuffer-setup-hook
          (lambda () (electric-pair-local-mode -1)))

;; unique buffer name. Show path if names are same
(setq uniquify-buffer-name-style 'forward)

;; 高亮显示选中的区域
(transient-mark-mode t)
(delete-selection-mode)
(setq track-eol t)

;; backup policies
(save-place-mode)

(setq auto-save-default nil)
(setq create-lockfiles nil)
;; do not backup.
(setq make-backup-files nil)

;; 递归使用minibuffer
(setq enable-recursive-minibuffers t)

(setq-default indent-tabs-mode nil)     ;默认用空格替代TAB
(setq default-tab-width 4)              ;设置TAB默认的宽度

;; 中文段落支持
(setq sentence-end "\\([。？！…]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; 把这些缺省禁用的功能打开
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; hippie complete
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol
        try-complete-lisp-symbol-partially
        try-complete-file-name
        try-complete-file-name-partially
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-list
        try-expand-line
        try-expand-list-all-buffers
        try-expand-line-all-buffers
        ))
(global-set-key [(meta ?/)] 'hippie-expand)

;; 打开最近文件
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 100
      recentf-exclude '(
			"/ssh:"
			"/sudo:"
			"recentf$"
			"company-statistics-cache\\.el$"
			;; ctags
			"/TAGS$"
			;; global
			"/GTAGS$"
			"/GRAGS$"
			"/GPATH$"
			;; binary
			"\\.mkv$"
			"\\.mp[34]$"
			"\\.avi$"
			"\\.wav$"
			"\\.docx?$"
			"\\.xlsx?$"
			;; ~/.emacs.d/**/*.el included
                        "\\.elc$"
			;; configuration file should not be excluded
			;; "/home/[a-z]\+/\\.[a-df-z]"
                        "index$"
                        "bookmarks$"
                        "\\.cache"
                        "\\.newsrc"
			))
;; recentf展示时，可以把HOME目录替换成~
;; (setq recentf-filename-handlers 'abbreviate-file-name)
(recentf-mode)

;;------------------------------------------------------------
;; ignore grep files.
(eval-after-load 'grep
  '(progn
     ;; eacl and other general grep (rgrep, grep ...) setup
     (dolist (v '("auto"
		  "target"
		  "node_modules"
		  "*dist"
		  ".cache"
		  ".npm"
		  "elpa"))
       (add-to-list 'grep-find-ignored-directories v))
     (dolist (v '("tags"
		  "TAGS"
		  "GTAGS"
		  "GRTAGS"
		  "GPATH"
		  "cscope.files"
		  "*.log"))
       (add-to-list 'grep-find-ignored-files v))))
;;

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; 设置部分应用的编码
(if (eq system-type 'windows-nt)
    (progn
      (add-to-list 'process-coding-system-alist '("fd" utf-8 . gbk)) 
      (add-to-list 'process-coding-system-alist '("rg" utf-8 . gbk)) 
      (add-to-list 'process-coding-system-alist '("es" gbk . gbk))
      ))

;; so long mode to get shell output.
(global-so-long-mode 1)

;------------------------------------------------------------
;; shell configuration
;; 在环境变量中增加prompt, 值$E[1;32;40m$t $E[1;36;40m$p$E[1;36;40m $g$E[1;37;40m ，可以在prompt中显示时间
(setq shell-file-name (executable-find "cmdproxy.exe"))
;; (setq shell-file-name "C:/Git/bin/bash.exe")
;; (setenv "ESHELL" "bash")

;; set maximum-buffer size for shell-mode
(setq comint-buffer-maximum-size 1024)

;; don't allow shell prompt to be erased
(setq comint-prompt-read-only "y")

;; Actually display colors when programs output colored text.  Without
;; this command, emacs prints the actual control characters.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))))))))

;; 退出term的时候关闭term对应的buffer
(add-hook 'shell-mode-hook 'kill-buffer-when-shell-command-exit)

;; 为了让shell正确显示中文，可以在执行 M-x : shell 之前可以先执行 C-x RET c chinese-gbk-dos

;;------------------------------------------------------------
;; woman setting
(setq woman-manpath (list "d:/Unix/man/" "d:/Unix/share/man/" "d:/cygwin64/usr/share/man/"))
(defun woman-helpful ()
  (interactive)
  (let ((woman-use-topic-at-point t))
    (woman)))

;;-----------------------------------------------------------
;; ediff mode config.
;; Whitespace insensitivity – Include ‘-w’ in ‘ediff-diff-options’.
;; Case insensitivity – Include ‘-i’ in ‘ediff-diff-options’.

;; To make ‘ediff’ operate on selected-frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; ignore differences in whitespace
;; (setq ediff-diff-options "-c -w")
(setq-default ediff-ignore-similar-regions t)
(setq-default ediff-ignore-case nil)

(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

(add-hook 'ediff-startup-hook 'scroll-all-mode)
(add-hook 'ediff-quit-hook
          #'(lambda ()
              (scroll-all-mode -1)))

;;----------------------------------------------------------------
;; browser configure.
(setq browse-url-generic-program (executable-find "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"))
;; (setq browse-url-generic-program (executable-find "C:/Program Files/Mozilla Firefox/firefox.exe"))
(setq browse-url-browser-function 'browse-url-generic)

;;-----------------------------------------------------------
;; enhance isearch mode
(setq-default isearch-regexp-lax-whitespace t
;; If non-nil, the four motion commands M-<, M->, C-v and M-v, when invoked during
;; Isearch, move respectively to the first occurrence of the current search string
;; in the buffer, the last one, the first one after the current window, and the
;; last one before the current window. 
              isearch-allow-motion t
              isearch-motion-changes-direction t
              search-ring-max 10
              regexp-search-ring-max 10)

(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-e" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-i" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-j" 'isearch-edit-string)
             )))

(eval-after-load "isearch"
  '(progn
     (require 'isearch-dabbrev)
     (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)))

;;;###autoload
(defun occur-from-isearch ()
  "Invoke `occur' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (let (search-nonincremental-instead)
      (isearch-exit))
    (occur query)
    (pop-to-buffer "*Occur*")))

(define-key isearch-mode-map (kbd "C-c o") 'occur-from-isearch)

(message "isearch mode loaded.")

;;}}} base configuration loaded.

;;------------------------------------------------------------
;;{{{ packages configuration.
;;------------------------------------------------------------
;; 配置 `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
)

;; 安装 `quelpa'
(use-package quelpa
  :ensure t
  :commands quelpa
  :config
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil)
  (quelpa-checkout-melpa-p nil))

;;--------------------------------------------------------------------
;; anzu configuration.
;; Show number of matches while searching
(use-package anzu
  :ensure t
  :hook (after-init . global-anzu-mode)
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace-regexp)))

;; htmlize configuration.
(setq htmlize-output-type 'inline-css)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "r"))

(use-package fanyi
  :ensure t
  :defer t
  :config
  (setq fanyi-sound-player-support-https t)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     ;; fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     ;; fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider))
  )

;; bookmarks configuration.
(use-package bm
  :ensure t
  :bind
  ("<C-f2>" . bm-toggle)
  ("<f2>" . bm-next)
  ("<S-f2>" . bm-previous)
  )

;; folding style.
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defun hideshow-folded-overlay-fn (ov)
  ;; (message "overlay is %s" (overlay-get ov 'hs))
  ;; (when (eq 'code (overlay-get ov 'hs))
  (when (memq (overlay-get ov 'hs) '(code comment))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
           (info (format " ... #%d " nlines)))
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

;; like writeroom but buffer local.
;; (require 'olivetti)
;; (setq-default olivetti-body-width 0.5)

;;------------------------------------------------------------
;; elfeed configuration
(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-feeds
        '(
          "https://remacs.cc/index.xml"
          ;; "http://www.chinanews.com/rss/importnews.xml"
          ;; "https://planet.emacslife.com/atom.xml"
          "http://rss.zol.com.cn/news.xml"
          "http://www.geekpark.net/rss"
          "https://sspai.com/feed"
          "https://www.huxiu.com/rss/0.xml"
          "https://www.zhihu.com/rss"
          ))
  (setq-default elfeed-search-filter "@1-months-ago +unread")
  ;; (setq-default elfeed-search-filter "@1-months-ago +unread ")
  ;; (add-hook 'elfeed-new-entry-hook
  ;;           (elfeed-make-tagger :before "2 weeks ago"
  ;;                               :remove 'unread))
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              ;; (olivetti-mode)
              ;; (setq fill-column 100)
              (setq left-margin-width 20)
              (setq right-margin-width 20)
              ))
  )

;;------------------------------------------------------------
;; dired-single configuration.
(require 'init-dired)

;;------------------------------------------------------------
;; evil configuration
(require 'init-evil)

;;------------------------------------------------------------
;; avy mode.
(use-package avy
  :ensure t
  :config
 )

(use-package ace-window
  :ensure t)

;;------------------------------------------------------------
;; pyim
(use-package pyim
  :config	     
;; avy pinyin support.
  (with-eval-after-load 'avy
    (defun my-avy--regex-candidates (fun regex &optional beg end pred group)
      (let ((regex (pyim-cregexp-build regex)))
        (funcall fun regex beg end pred group)))
    (advice-add 'avy--regex-candidates :around #'my-avy--regex-candidates))
  
  ;; isearch pinyin support.
  ;; 注意：这个功能有一些限制，搜索字符串中只能出现 “a-z” 和 “’”，如果有其他字符（比 如 regexp 操作符），则自动关闭拼音搜索功能。
  (defun toggle-pyim-isearch-mode ()
    (interactive)
    (if (eq pyim-isearch-mode t)
	(pyim-isearch-mode -1)
      (pyim-isearch-mode 1)
      ))
  (define-key isearch-mode-map "\M-c" 'toggle-pyim-isearch-mode)
)

;;---------------------------------------------------------------------------
(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

;; (use-package all-the-icons-ibuffer
;;   :ensure t
;;   :init
;;   (all-the-icons-ibuffer-mode 1)
;;   :config
;;   (setq all-the-icons-ibuffer-human-readable-size t))

;; (require 'all-the-icons-completion)
;; (all-the-icons-completion-mode)

;;----------------------------------------------------------------------------
(message "loading seperate init files .... ")

;; (require 'init-ivy)
(require 'init-vertico)

;;------------------------------------------------------------------
;; company-mode configuration
;; ctags configuration in init-company;
(require 'init-company)
;; (require 'init-corfu)

;;----------------------------------------------------------
;; ctags configuration.
;; (require 'init-citre)
(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (defun citre-jump-xref ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (let* ((xref-prompt-for-identifier nil))
               (call-interactively #'xref-find-definitions)))))

  (global-set-key (kbd "C-x c j") 'citre-jump-xref)
  (global-set-key (kbd "C-x c k") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (global-set-key (kbd "C-x c r") 'citre-jump-to-reference)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek-reference)
  (global-set-key (kbd "C-x c U") 'citre-global-update-database)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set these if gtags/global is not in your PATH (and you want to use the
   ;; global backend)
   ;; citre-gtags-program "/path/to/gtags"
   ;; citre-global-program "/path/to/global"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   ;; citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-tags-file-global-cache-dir "~/.cache/tags/"
   citre-default-create-tags-file-location 'global-cache
   citre-update-tags-file-when-no-definitions t
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)))

;;------------------------------------------------------------
;; dumb configuration.
(use-package dumb-jump
  :config
  ;; use a completion framework instead of default popup.
  (setq dumb-jump-selector 'completing-read)
  ;; (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-prefer-searcher 'rg)
  ;; set xref backend to dumb-jump at the end.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 95)
  )

(setq xref-search-program 'ripgrep)
(define-key evil-normal-state-map (kbd "M-.") nil)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(defun zjy/xref-find-backends ()
  "find local backends function."
  (let (backends
        backend)
    (dolist (f xref-backend-functions)
      (when (functionp f)
        (setq backend (funcall f))
        (when backend
          (cl-pushnew (funcall f) backends))))
    (reverse backends)))

(defun zjy/xref-find-all-backends ()
  "find all backends including global backends."
  (let (backends
        backend)
    (dolist (f (append xref-backend-functions (default-value 'xref-backend-functions)))
      (when (functionp f)
        (setq backend (funcall f))
        (when backend
          (cl-pushnew backend backends))))
    (reverse (delete-dups backends))))

(defun zjy/xref--create-fetcher (input kind arg)
  "Return an xref list fetcher function.

It revisits the saved position and delegates the finding logic to
the xref backend method indicated by KIND and passes ARG to it."
  (let* ((orig-buffer (current-buffer))
         (orig-position (point))
         (backends (zjy/xref-find-backends))
         (method (intern (format "xref-backend-%s" kind))))
    (lambda ()
      (save-excursion
        ;; Xref methods are generally allowed to depend on the text
        ;; around point, not just on their explicit arguments.
        ;;
        ;; There is only so much we can do, however, to recreate that
        ;; context, given that the user is free to change the buffer
        ;; contents freely in the meantime.
        (when (buffer-live-p orig-buffer)
          (set-buffer orig-buffer)
          (ignore-errors (goto-char orig-position)))
        (let (xrefs)
          (cl-dolist (backend backends)
            (ignore-errors
              (message "try xref-backend-%s" backend)
              (setq xrefs (funcall method backend arg))
              (when xrefs
                (cl-return))))
          (unless xrefs
            (xref--not-found-error kind input))
          xrefs)))))

(advice-add #'xref--create-fetcher :override #'zjy/xref--create-fetcher)

;;----------------------------------------------------------------
;; lsp configuration.
;; (require 'init-lsp-bridge)
(require 'init-lsp)
;; (require 'init-eglot)

;;----------------------------------------------------------------------------
(use-package helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-symbol)
         ([remap describe-key] . helpful-key))
  :config
  (with-eval-after-load 'counsel
    (progn ()
           (setq counsel-describe-function-function #'helpful-callable)
           (setq counsel-describe-variable-function #'helpful-variable)
           )))

;; add remove button behind describe-function. use <f1>f to active describe-function.
(defun function-advices (function)
  "Return FUNCTION's advices.

URL https://emacs-china.org/t/advice/7566/16
Version 2022-06-29 00.01.07 +8000"
  (let ((flist (indirect-function function)) advices)
    (when (and (consp flist)
               (or (eq 'macro (car flist))
                   (and (autoloadp flist) (memq (nth 4 flist) '(macro t)))))
      (setq flist (cdr flist)))
    (while (advice--p flist)
      (setq advices `(,@advices ,(advice--car flist)))
      (setq flist (advice--cdr flist)))
    advices))

(define-advice describe-function-1 (:after (function) advice-remove-button)
  "Add a button to remove advice.

URL https://emacs-china.org/t/advice/7566/16
Version 2022-06-29 00.01.07 +8000"
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        (let ((ad-list (function-advices function)))
          (while (re-search-forward "^\\(?:This \\(?:function\\|macro\\) has \\)?:[-a-z]+ advice: \\(.+\\)\\.?$" nil t)
            (let* ((name (string-trim (match-string 1) "[‘'`]" "[’']"))
                   (symbol (intern-soft name))
                   (advice (or symbol (car ad-list))))
              (when advice
                (when symbol
                  (cl-assert (eq symbol (car ad-list))))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'cursor-sensor-functions `((lambda (&rest _) (message "%s" ',advice)))
                   'help-echo (format "%s" advice)
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (when (yes-or-no-p (format "Remove %s ? " ',advice))
                        (message "Removing %s of advice from %s" ',function ',advice)
                        (advice-remove ',function ',advice)
                        (revert-buffer nil t)))
                   'follow-link t))))
            (setq ad-list (cdr ad-list))))))))

;;----------------------------------------------------------------
;; tree-sitter
;;----------------------------------------------------------------
;; (require 'init-treesitter)
(when (treesit-available-p)
  (add-to-list 'treesit-extra-load-path "d:/Users/home/.emacs.d/mylisp/tree-sitter-langs-extra-bin")
  (use-package treesit-auto
  :demand t
  :config
  ;; (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode))
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode)))

;;----------------------------------------------------------------
;; find-file-in-project
;; (use-package find-file-in-project
;;   :init
;;   (defalias 'ffip 'find-file-in-project-by-selected)
;;   :config
;;   (setq ffip-use-rust-fd t)
;;   )

;;----------------------------------------------------------------
;; rainbow mode.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;----------------------------------------------------------------
(require 'which-key)
(which-key-mode)

(window-numbering-mode)

(global-hungry-delete-mode)

;;----------------------------------------------------------------
;; markdown mode.
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  ;; `pandoc' is better than obsolete `markdown'
  ;; iconv -f gbk -t gbk README.md | pandoc -o 1.html
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown")))

;; pdf tools.
(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install)

  :mode
  ("\\.pdf\\'" . pdf-view-mode)

  :hook
  (pdf-view-mode . (lambda () (blink-cursor-mode -1)))
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1))))


(add-to-list 'auto-mode-alist '("\\.\\(asn\\|exp\\)\\'" . asn1-mode))

;;------------------------------------------------------------
(require 'init-calendar)

(require 'init-org)

(require 'init-shrface)

;; some eshell functions
(require 'init-eshell)

(require 'init-treemacs)

;; (require 'init-w3m)

;;---------------------------------------------------------------------------------
;; automatically set input method.
(use-package sis
  :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  (((text-mode prog-mode org-mode) . sis-context-mode)
   ((text-mode prog-mode org-mode) . sis-inline-mode))

  :config
  ;; enable the /cursor color/ mode
  ;; (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  ;; (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  ;; (sis-global-inline-mode t)
  )

(message "packages configuration loaded.")

;;}}} package configuration loaded.

;;------------------------------------------------------------
;;{{{ lisp configuration.
;;-------------------------------------------------------------
(require 'irfc-mode)
(setq irfc-directory (expand-file-name "d:/Protocol/RFC-all/"))
(setq irfc-buffer-name-includes-title nil)
(setq irfc-download-base-url "https://www.rfc-editor.org/rfc/")
(add-to-list 'auto-mode-alist '("/rfc[0-9]+\\.txt\\'" . irfc-mode))

(setq irfc-imenu-generic-expression
      '(
        ("Contents" "^\\([A-Z]?[0-9\\.]+[ ]+[^\\.\n]+\\)$" 1)
        ("Table" "^\\(Table of Contents\\)$" 1)
        ))
(add-hook 'irfc-mode-hook
        (lambda ()
          (setq imenu-generic-expression irfc-imenu-generic-expression)
          (define-key irfc-mode-map (kbd "M-]") 'irfc-table-jump)
          ))

;;; In jsonian-mode
(quelpa '(jsonian :fetcher git :url "https://github.com/iwahbe/jsonian.git"))
(use-package jsonian
  :ensure nil
  :after so-long
  :custom
  (jsonian-no-so-long-mode))

;;---------------------------------------------------------------
(quelpa '(sdcv :fetcher git :url "https://github.com/manateelazycat/sdcv.git"))
(require 'sdcv)
(setq sdcv-say-word-p nil)               ;; say word after translation
(setq sdcv-dictionary-data-dir "d:/unix/dic/") ;; setup directory of stardict dictionary
(setq sdcv-env-lang "zh_CN.UTF-8")
(setq sdcv-dictionary-simple-list    ;; setup dictionary list for simple search
      '(
        "懒虫简明英汉词典"
        ))
(setq sdcv-dictionary-complete-list     ;; setup dictionary list for complete search
      '(
        "CDICT5英汉辞典"
        "简明英汉字典增强版"
        "牛津英汉双解美化版"
        "朗道英汉字典5.0"
        ))

(defun sdcv-search-input-fast ()
  "Translate current WORD at point.
And show information using tooltip."
  (interactive)
  ;; Display simple translate result.
  (let* ((word (sdcv-region-or-word)))
    (if word (sdcv-search-simple word))))

(global-set-key [(f6)] 'sdcv-search-input-fast)

(global-set-key [(f5)] 'avy-goto-char)

;; dictionary-overlay
;; git clone --depth=1 -b main https://github.com/ginqi7/websocket-bridge ~/.emacs.d/lisp/websocket-bridge/
(quelpa '(websocket-bridge :fetcher git :url "https://github.com/ginqi7/websocket-bridge.git"))
(require 'websocket-bridge)
;; git clone --depth=1 -b main https://github.com/ginqi7/dictionary-overlay ~/.emacs.d/lisp/dictionary-overlay/
(quelpa '(dictionary-overlay :fetcher git :url "https://github.com/ginqi7/dictionary-overlay.git"))
(require 'dictionary-overlay)
(setq dictionary-overlay-user-data-directory
      (expand-file-name "~/.cache/dictionary-overlay-data"))
(set-face-attribute 'dictionary-overlay-unknownword nil
                    :foreground "#8386C5")
(set-face-attribute 'dictionary-overlay-translation nil
                    :underline "#C77577" :background "#7A696B")

(setq dictionary-overlay-auto-jump-after
   '(mark-word-known         ; recommended
     ;; mark-word-unknown    ; not recommended
     render-buffer           ; opinionated, but turn it on, why not
     ))

(dictionary-overlay-start)

;;---------------------------------------------------------------
(require 'keep-buffers)

;;------------------------------------------------------------
;; insert-translated-name
;; (quelpa '(insert-translated-name :fetcher git :url "https://github.com/manateelazycat/insert-translated-name.git"))
;; https://github.com/manateelazycat/insert-translated-name
;; (use-package insert-translated-name
;;   :defer 3
;;   :config
;;   (setq insert-translated-name-translate-engine "google"))

;;------------------------------------------------------------
;; color-rg
;; https://github.com/manateelazycat/color-rg
(require 'color-rg)
(setq color-rg-extra-arguments "")
(setq color-rg-search-ignore-rules "")
(setq color-rg-search-no-ignore-file t)
(setq color-rg-recenter-match-line t)
(define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)

;;------------------------------------------------------------
;; https://github.com/manateelazycat/thing-edit
(quelpa '(thing-edit :fetcher git :url "https://github.com/manateelazycat/thing-edit.git"))
(require 'thing-edit)

;; https://github.com/lyjdwh/avy-thing-edit
(quelpa '(avy-thing-edit :fetcher git :url "https://github.com/lyjdwh/avy-thing-edit.git"))
(require 'avy-thing-edit)

;; select the total word including - inside.
(defun select-total-word(&optional arg)
  "select the total word including - inside."
  (interactive "P")
  (backward-sexp)
  (set-mark (point))
  (forward-sexp))

;; select the total word including - inside.
(defun select-total-part(&optional arg)
  (interactive "sCharacter: ")
  ;; if arg not nil, search backword for arg, then put the mark.
  (if (eq t (compare-strings "" 0 (length "") arg 0 (length arg))) (search-backward "\"")
    (search-backward arg))
  (set-mark (point))
  (forward-sexp)
  (backward-char))

(defun thing-copy-part(&optional arg)
  (interactive "sCharacter: ")
  ;; if arg not nil, search backword for arg, then put the mark.
  (if (eq t (compare-strings "" 0 (length "") arg 0 (length arg))) (search-backward "\"")
    (search-backward arg))
  (save-excursion
    (let ((beg (+ 1 (point))))
      (forward-sexp)
      (thing-edit-internal beg (- (point) 1)))))

(defun thing-cut-part(&optional arg)
  (interactive "sCharacter: ")
  ;; if arg not nil, search backword for arg, then put the mark.
  (if (eq t (compare-strings "" 0 (length "") arg 0 (length arg))) (search-backward "\"")
    (search-backward arg))
  (save-excursion
    (let ((beg (+ 1 (point))))
      (forward-sexp)
      (thing-edit-internal beg (- (point) 1) t))))

(require 'swbuff-x)
(setq swbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*" "TAGS$"))
(setq swbuff-exclude-mode-regexp "{help|info}-mode")
(setq swbuff-special-buffers-re "^\\..*")
(setq swbuff-separator " | ")
(setq swbuff-clear-delay 1)
(set-face-attribute 'swbuff-default-face nil
                    :foreground "default" :underline nil)
(set-face-attribute 'swbuff-special-buffers-face nil
                    :foreground "red" :underline nil)
(set-face-attribute 'swbuff-current-buffer-face nil
                    :foreground "yellow" :bold t :underline t)
(set-face-attribute 'swbuff-separator-face nil
                    :foreground "green")
(global-set-key (kbd "<C-tab>") 'swbuff-switch-to-next-buffer)
(global-set-key (kbd "<C-S-kp-tab>") 'swbuff-switch-to-previous-buffer)

;;}}} lisp configuration loaded.

(require 'init-misc)

;;------------------------------------------------------------
;;{{{ program mode.
;;------------------------------------------------------------
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook #'(lambda ()
                    (c-set-style "stroustrup")
                    )))

(dolist (hook (list
               'make-mode-hook
               ))
  (add-hook hook #'(lambda ()
                    (setq-local indent-tabs-mode t)
                    )))

(use-package yaml-mode
  :ensure t
  :defer t
  :bind (:map yaml-mode-map
              ("M-." . yaml-find-ref))
  :config
  (defun yaml-find-ref ()
    "find yaml reference."
    (interactive)
    (let ((str (buffer-substring-no-properties (search-backward-regexp "^") (search-forward-regexp "$")))
          (case-fold-search nil))
      (if (not (string-search "$ref:" str))
          (message "Not in reference line.")
        (progn 
          (let* ((beg (string-search "'" str))
                 (end (string-search "'" str (+ 1 beg)))
                 (str (substring str (+ 1 beg) end))
                 (slist (split-string str "/"))
                 (filename ""))
            (while slist
              (setq stag (car slist)
                    slist  (cdr slist))
              (cond 
               ((string-search "#" stag)
                (setq filename (substring stag 0 (string-search "#" stag)))
                (if (not (string-equal filename ""))
                    (progn
                      (find-file-existing filename)
                      (beginning-of-buffer))))
               ((string-search "components" stag)
                (progn
                  (beginning-of-buffer)
                  (search-forward-regexp "^components:" nil t)))
               (t (if (eq 0 (string-to-number stag))
                      (search-forward-regexp (concat "^[ \t]*" stag ":") nil t)
                    (search-forward-regexp (concat "'" stag "'") nil t))
                  )
               ))))))))


;; PDB command line
(defun my-python-debug-buffer ()
  "Run python debugger on current buffer."
  (interactive)
  (setq command (format "python -u -m pdb %s " (file-name-nondirectory buffer-file-name)))
  (let ((command-with-args (read-string "Debug command: " command nil nil nil)))
    (pdb command-with-args)))

;; python shell configuration.
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

;; highlight the marker line in gud mode.
(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;;(move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer))))))

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))
(add-hook 'kill-buffer-hook 'gud-kill-buffer)

;;}}} program configuration loaded.

(defun my--push-point-to-xref-marker-stack (&rest r)
  (xref-push-marker-stack (point-marker)))
(dolist (func '(find-function
                yaml-find-ref
                irfc-table-jump
                ))
  (advice-add func :before 'my--push-point-to-xref-marker-stack))

;;------------------------------------------------------------
;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
;; Use H for Hyper, s for Super.
;; (setq w32-pass-rwindow-to-system nil)
;; (setq w32-rwindow-modifier 'super) ; Right Windows key

;; (setq w32-pass-apps-to-system nil)
;; (setq w32-apps-modifier 'hyper) ; Menu/App key
;; (setq w32-pass-lwindow-to-system nil)
;; (setq w32-lwindow-modifier 'super)
;; (w32-register-hot-key [s-])

;;------------------------------------------------------------
;; Use puni-mode globally and disable it for term-mode.
(use-package puni
  :ensure t
  :defer nil
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  ;; (puni-global-mode)
  ;; (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  :config
  (global-set-key (kbd "C-k") 'puni-kill-line)
  (global-set-key (kbd "C-S-k") 'puni-backward-kill-line)
  (global-set-key (kbd "C-M-f") 'puni-forward-sexp)
  (global-set-key (kbd "C-M-b") 'puni-backward-sexp)
  )

;;------------------------------------------------------------
;;{{{ hydra configuration.

(require 'linum-relative)
(defhydra hydra-toggle (:color pink)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_i_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_l_ display-line-numbers-mode:    %`display-line-numbers-mode
_r_ linum-relative-mode:    %`linum-relative-mode
_p_ toggle-word-wrap:   %`word-wrap
_v_ visual-line-mode:   %`visual-line-mode
_w_ writeroom-mode:     %`writeroom-mode
"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("i" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("l" display-line-numbers-mode nil)
  ("r" linum-relative-mode nil)
  ("p" toggle-word-wrap nil)
  ("v" visual-line-mode nil)
  ("w" writeroom-mode nil)
  ("q" nil "quit"))

(defhydra hydra-avy (:color blue :hint nil)
  "
_c_har    ch_a_r2    wi_n_dow   _r_g     r_g_dir
_w_ord    se_x_p     _f_ile     _u_rl    _p_art   e_m_ail
_W_ord    se_X_p     _F_ile     _L_ine   s_d_cv
  "
  ("c"  avy-goto-char)
  ("a"  avy-goto-char-2)
  ("n"  ace-window)
  ("W"  avy-thing-copy-word)
  ("X"  avy-thing-copy-sexp)
  ("F"  avy-thing-copy-filename)
  ("L"  avy-thing-copy-line)
  ("w"  thing-copy-word)
  ("x"  thing-copy-sexp)
  ("f"  thing-copy-filename)
  ("u"  thing-copy-url)
  ("m"  thing-copy-email)
  ("p"  thing-copy-part)
  ("d"  sdcv-search-input+)
  ("r"  color-rg-search-input)
  ("g"  color-rg-search-dir-with-type)
 )

(defhydra hydra-common (:color blue :hint nil)
  "
_b_url    _c_omment      s_d_cv     _r_g
r_g_dir   multi_e_dit    _h_elp 
  "
  ("b"  browse-url)
  ("c"  evilnc-comment-or-uncomment-lines)
  ("d"  sdcv-search-input+)
  ("e"  evil-multiedit-match-all)
  ("r"  color-rg-search-input)
  ("g"  color-rg-search-dir-with-type)
  ("h"  helpful-at-point)
  )

(defhydra hydra-folding (:color blue :hint nil)
  "
fo_l_ding       _f_old-hs          _o_utline
  "
  ("f"  hs-toggle-hiding)
  ("l"  folding-toggle-show-hide)
  ("o"  outline-toggle-children)
  )

(defhydra hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

;; Recommended binding:
(defhydra hydra-all (:color blue :hint nil)
  "
_t_oggle  _a_vy  _c_ommon   _f_olding    dumb-_j_ump
  "
  ("t" hydra-toggle/body)
  ("c" hydra-common/body)
  ("a" hydra-avy/body)
  ("f" hydra-folding/body)
  ("j" hydra-dumb-jump/body)
)

(define-key evil-normal-state-map (kbd "<SPC>") 'hydra-all/body)
(define-key evil-visual-state-map (kbd "<SPC>") 'hydra-all/body)
(global-set-key (kbd "C-c <SPC>") 'hydra-all/body)
(global-set-key (kbd "M-s a") 'hydra-avy/body)

;;}}} hyudra configuration loaded.

;;------------------------------------------------------------
;; global key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [(f3)] 'query-replace-regexp)
;; (global-set-key [(f5)] 'loop-alpha)
(global-set-key [(f8)] 'org-agenda-list)
;; switch between current buffer and previous one.
;; (global-set-key (kbd "<C-tab>") #'(lambda () (interactive) (switch-to-buffer nil)))

(message "key binding loaded.")

;;----------------------------------------------------------------
;;{{{ UI settings.
;;----------------------------------------------------------------
;;设置标题栏为buffer的内容
(setq frame-title-format
      '(
        (" [%@%*] ")
        ;; (:eval (if (buffer-modified-p)
        ;;            " ! "))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " @ Emacs" emacs-version
        ;; " - " minor-mode-alist
      ))

;;-----------------------------------------------------------------
;; theme set

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
(load-theme 'doom-dracula t)

;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;;------------------------------------------------------------
;; mode-line configuration.
;;------------------------------------------------------------
(require 'diminish)
(diminish 'anzu-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(diminish 'which-key-mode)
(diminish 'visual-line-mode)
(diminish 'org-indent-mode)
(diminish 'tree-sitter-mode " ❀")
(diminish 'hungry-delete-mode " ✜")
;; company-fuzzy-mode can show the backends.
(diminish 'company-mode)

;; (require 'init-modeline)
(use-package doom-modeline
  :custom
  (doom-modeline-modal-icon nil)
  (doom-modeline-icon t)
  (doom-modeline-minor-modes t)
  :hook (after-init . doom-modeline-mode))

;; (use-package keycast
;;   :after doom-modeline
;;   :commands keycast-mode
;;   :config
;;   (define-minor-mode keycast-mode
;;     "Show current command and its key binding in the mode line."
;;     :global t
;;     (if keycast-mode
;;         (progn
;;           (add-hook 'pre-command-hook 'keycast--update t)
;;           (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
;;       (remove-hook 'pre-command-hook 'keycast--update)
;;       (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))
;;   (keycast-mode -1)
;;   )

(use-package ligature
  ;; :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(org-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;-----------------------------------------------------------------------
(require 'init-face)

;; which function mode.
(setq which-func-imenu-joiner-function
      (lambda (x)
        (let* ((otype (car x))
               (loc-sym (car (split-string (car (last x)) " ")))
               (path (mapcan
                      (lambda (pel)
                        (split-string pel "::"))
                      (seq-map
                       (lambda (pel)
                         (car (split-string pel " ")))
                       (butlast (cdr x)))))
               (result (seq-filter
                        (lambda (pel)
                          (and (<= (length (split-string pel "::")) 1) ;;(<= (length (split-string pel "\\.")) 1)
                               (seq-reduce (lambda (o v)
                                             (and o (not (string= pel v)))) (list "callback") t)))
                        path)))
          (when (seq-reduce (lambda (o v) (or o (string= v otype))) (list "Method" "Constructor" "Function") nil)
            (setq result (append result (list loc-sym))))
          ;;(message "<< %s %s" x (list (list loc-sym ":" otype) (list (seq-map #'list path))))
          ;;(message ">>> %s" result)
          (setq header-line-format (mapconcat #'identity result " » "))
          ;; return
          "")))

;; Fall back font for glyph missing in Roboto
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'font-lock-comment-face))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'font-lock-comment-face))

;; jit-lock，提升font lock性能，但可能影响font lock速度
;; (use-package jit-lock
;;   :ensure nil
;;   :custom
;;   (jit-lock-defer-time 0.25)
;;   (jit-lock-stealth-time 16))

;;}}} UI configuration loaded.

;;------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   '(treesit-auto avy-thing-edit company-english-helper dictionary-overlay elispfl flx-rs inherit-org jsonian sdcv thing-edit websocket-bridge ligature quelpa fanyi websocket dumb-jump company-fuzzy citre ahk-mode realgud ob-mermaid lsp-pyright lsp-mode diminish sis hungry-delete consult-company company-ctags company company-tabnine embark-consult embark consult marginalia vertico orderless asn1-mode org-modern shrface devdocs-browser pdf-tools cmake-mode evil-multiedit which-key yaml-mode eshell-syntax-highlighting eshell-up eshell-z org-download treemacs-evil treemacs helpful ace-window avy evil-pinyin evil dired-single elfeed bm wgrep use-package folding web-mode puni writeroom-mode linum-relative vimrc-mode go-mode evil-anzu nov w32-browser markdown-mode htmlize anzu cal-china-x evil-nerd-commenter evil-surround evil-matchit isearch-dabbrev rainbow-delimiters rainbow-mode window-numbering doom-modeline doom-themes all-the-icons expand-region pyim))
 '(tool-bar-mode nil)
 '(warning-suppress-types '((mule))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -*- coding: utf-8; lexical-binding: t; -*-

;; Author: lihao

;;; Code:
;; This file is designed to be re-evaled; use the variable first-time

(defvar first-time t
  "Flag signifying this is the first time that .emacs has been evaled")

;;------------------------------------------------------------

;;{{{ basic configuration.
;; start server mode
(if (display-graphic-p)  (server-start))

;; 在注册表中建如下键值：
;; HKEY_CLASSES_ROOT\*\shell\Edit with Emacs\command
;; 并将其default的值设为：
;; "path\to\emacsclientw.exe" --no-wait --alternate-editor="path\to\runemacs.exe" "%1"

;; 将内存回收阈值增大，加快启动速度；启动完成后更新为初始值
;; (defvar default-file-name-handler-alist file-name-handler-alist)
;; (defun my|pre-init()
;;   (setq gc-cons-threshold most-positive-fixnum
;;         gc-cons-percentage 1.0
;;         file-name-handler-alist nil
;;         ))
;; (defun my|post-init ()
;;   (setq gc-cons-threshold 20000000
;;         gc-cons-percentage 0.1
;;         file-name-handler-alist default-file-name-handler-alist)
;;   ;; GC automatically while unfocusing the frame
;;   ;; `focus-out-hook' is obsolete since 27.1
;;   ;; (if (boundp 'after-focus-change-function)
;;   ;;     (add-function :after after-focus-change-function
;;   ;;                   (lambda ()
;;   ;;                     (unless (frame-focus-state)
;;   ;;                       (garbage-collect))))
;;   ;;   (add-hook 'focus-out-hook 'garbage-collect))
;;   )
;;(add-hook 'before-init-hook #'my|pre-init)
;;(add-hook 'emacs-startup-hook #'my|post-init)

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

;;----------------------------------------------------------------
(require 'package)
;; (setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")))
;; (add-to-list 'package-archives '("melpa_stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")))
                         ;; ("gnu"   . "http://elpa.emacs-china.org/gnu/")))
(package-initialize)
(message "package initialize.")

;; add load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; install all the sub-directories in the beginnging of load-path.
(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "~/.emacs.d/lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path)

;;------------------------------------------------------------------
;; ignore cl warnings.
(setq byte-compile-warnings '(cl-functions))

;; show emacs gc message
;; (when (eq system-type 'windows-nt)
;;   (setq gc-cons-threshold (* 512 1024 1024))
;;   (setq gc-cons-percentage 0.1)
;;   (run-with-idle-timer 60 t #'garbage-collect)
;;   ;; 显示垃圾回收信息，这个可以作为调试用 ;;
;;   (setq garbage-collection-messages t))
(gcmh-mode 1)

;;关闭出错时的提示声
(setq visible-bell t)

(if (display-graphic-p)
    (progn
      ;;设置窗口位置为屏库左上角(0,0)
      (set-frame-position (selected-frame) 10 10)
      ;;设置宽和高
      (set-frame-width (selected-frame) 115)
      (set-frame-height (selected-frame) 22)
      ;; 最大化窗口
      ;; (setq initial-frame-alist (quote ((fullscreen . maximized))))
      ))
;; 支持字体缓存
(setq inhibit-compacting-font-caches t)

;;-----------------------------------------------------------------
;; theme set
;; (load-theme 'spacemacs-light t)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;;-----------------------------------------------------------------
;; font set
;; lihao: 发现现在的emacs对中文支持非常好，配置如下内容反而会导致问题，默认配置没问题;
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'cp936)
;; (set-keyboard-coding-system 'utf-8)
;; (setq-default pathname-coding-system 'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8))
;; (setq file-name-coding-system 'utf-8)

;; buffer新建和读取都默认是utf-8
;; (prefer-coding-system 'utf-8)

;; Setting English Font
;; (set-face-attribute 'default nil :font "FiraCode NF 14")
(set-face-attribute 'default nil :font "Sarasa Mono SC 16")
;; (set-face-attribute 'default nil :font "Roboto Mono Light 14")

;; Chinese Font
;; 当有字体已支持中文时，不用再次设置
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "微软雅黑" :size 28)))  ;; 微软雅黑，24
;; Emoji
(set-fontset-font t 'unicode (font-spec :family "Segoe UI Emoji" :size 14))
(set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Sarasa Mono Slab SC" :size 16 :weight 'bold))

;;-----------------------------------------------------------------
(setq show-paren-style 'parenthesis)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)
(add-hook 'minibuffer-setup-hook
          (lambda () (electric-pair-local-mode -1)))

(fset 'yes-or-no-p 'y-or-n-p)
;;(mouse-avoidance-mode 'animate)

;; 防止页面滚动时跳动,scroll-margin 3可以在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
(setq scroll-margin 2
      scroll-conservatively 10000)

;; 把这些缺省禁用的功能打开
;;(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; 中文段落支持
(setq sentence-end "\\([。？！…]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;取消滚动栏
(if (display-graphic-p) (set-scroll-bar-mode nil))

(setq column-number-mode t)
(setq line-number-mode t)

;; used for auto-fill-mode
(setq fill-column 100)

;; linum style
(setq linum-format "%4d")
(display-line-numbers-mode t)
(toggle-indicate-empty-lines nil)

;; 高亮显示选中的区域
(transient-mark-mode t)
(delete-selection-mode)
(setq track-eol t)
;; 递归使用minibuffer
(setq enable-recursive-minibuffers t)

(auto-image-file-mode t)
;; original is "auto"
(setq image-scaling-factor 1.0)
(blink-cursor-mode 0)

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(setq sort-fold-case t)
(setq search-invisible t)

;; set scratch buffer message to nil.
(setq initial-scratch-message nil)

;; unique buffer name.
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same

;; something in windows. cmdproxy will show chinese error, but command like fd can handle.
;; not required.
;; (if (eq system-type 'windows-nt)
;;     (set-default 'process-coding-system-alist
;;                  '(
;;                    ;; 不能设置cmdproxy，对导致很多乱码；
;;                    ;; ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)
;;                    ;; ("[rR][gG]" utf-8 . gbk)
;;                    ;; ("[fF][dD]" utf-8 . gbk)
                   ;;  ("sdcv" utf-8-dos . chinese-iso-8bit-dos)
                   ;; )))

;;------------------------------------------
;; backup policies
;; (setq auto-save-default nil)
;; (setq create-lockfiles nil)

(setq make-backup-files t)
(setq version-control t)
(setq delete-old-versions t)
(setq backup-directory-alist '((".*" . "~/auto-save-list/")))
(setq auto-save-list-file-prefix "~/auto-save-list/.saves-")
(message "backup policy loaded.")
;; backup end

;; hippie complete
(setq dabbrev-always-check-other-buffers t)
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-line
        try-expand-line-all-buffers
        try-expand-list
        try-expand-list-all-buffers
        try-complete-file-name
        try-complete-file-name-partially
        try-complete-lisp-symbol
        try-complete-lisp-symbol-partially
        try-expand-whole-kill
        ))
(global-set-key [(meta ?/)] 'hippie-expand)
;;(global-set-key [(meta ?/)] 'semantic-ia-complete-symbol-menu)

;;}}}

;;------------------------------------------------------------
;;{{{ dired configuration.
(require 'dired-x)

(setq dired-recursive-copies t)
(setq dired-recursive-deletes t)
(setq dired-recursive-deletes 'always)        ;; don't ask when deleting recursively
(setq dired-recursive-copies 'always)         ;; don't ask when copying recursively
(setq dired-details-hidden-string "[ ... ] ") ;; display string for hiden details
(setq dired-listing-switches "-aBhgGl")       ;; arguments passed to "ls"
(setq directory-free-space-args "-Ph")        ;; arguments passed to free space
;; if there is a Dired buffer displayed in the next
;; window, use its current directory, instead of this Dired buffer’s
;; current directory.if there is a Dired buffer displayed in the next
;; window, use its current directory, instead of this Dired buffer’s
;; current directory.
(setq dired-dwim-target t)

;; switch --group-directories-first do not work.
(defun directory-dired-sort ()
  "dired-mode中让目录显示在文件前"
  (save-excursion
    (let
      (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and
   (featurep 'xemacs)
   (fboundp 'dired-insert-set-properties)
   (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'directory-dired-sort)
(add-hook 'dired-lood-hook 'directory-dired-sort)

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (setq dired-omit-files "^#\\|^\\..*\\|semantic.cache")
            (setq dired-omit-extensions '("CVS/" "semantic.cache" ".git/" ".desktop" "~" ".log" ".bak" ".elc" ".el~" ".obj"))
            (setq dired-omit-mode nil)
            ))

(defun dired-open-web-file ()
  "open file with web browser."
  (interactive)
  ;; use arg passed in or find name of current line
  (let ((name (dired-get-filename nil t)))
	;; lihao add
    (if (eq name nil)
        (error (format "No file exist here.")))
    ;; add end
    (w3m-find-file name))
)

(add-hook 'dired-mode-hook (lambda ()
  (interactive)
  (make-local-variable  'dired-sort-map)
  (setq dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  ;; (define-key dired-mode-map (kbd "<M-up>") 'dired-up-directory)
  (define-key dired-sort-map "s"
              '(lambda () "sort by Size"
                (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
  (define-key dired-sort-map "x"
              '(lambda () "sort by eXtension"
                 (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
  (define-key dired-sort-map "t"
              '(lambda () "sort by Time"
                 (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
  (define-key dired-sort-map "n"
              '(lambda () "sort by Name"
                 (interactive) (dired-sort-other (concat dired-listing-switches ""))))

  (define-key dired-mode-map "W" 'dired-open-web-file)
))
;;}}}

;;------------------------------------------------------------
;;{{{ shell
;; (setq shell-file-name (executable-find "zsh.exe"))

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

;;}}}

;;------------------------------------------------------------
;; 修改GDB many window的布局
(defadvice gdb-setup-windows (after my-setup-gdb-windows activate)
  "my gdb UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left))      ;code and output
        (win2 (split-window-below (/ (* (window-height) 2) 3)))     ;stack
        )
    (select-window win2)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4)))) ;io
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
    (select-window win0)
    ))

;; 退出gdb的时候关闭gdb对应的buffer
(add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)

;;------------------------------------------------------------
;;woman Setting
(setq woman-manpath (list "d:/Unix/man/" "d:/Unix/share/man/"))
(defun woman-helpful ()
  (interactive)
  (let ((woman-use-topic-at-point t))
    (woman)))

;;------------------------------------------------------------
(setq-default indent-tabs-mode nil)     ;默认用空格替代TAB
(setq default-tab-width 4)              ;设置TAB默认的宽度

(global-set-key (kbd "RET") 'newline-and-indent)

(global-visual-line-mode t)

;;------------------------------------------------------------
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-default-buffer-method 'selected-window)
;; (ido-everywhere t)

;;------------------------------------------------------------
;;{{{ ignore grep files.
(eval-after-load 'grep
  '(progn
     ;; eacl and other general grep (rgrep, grep ...) setup
     (dolist (v '("auto"
		  "target"
		  "node_modules"
		  "bower_components"
		  "*dist"
		  ".sass_cache"
		  ".cache"
		  ".npm"
		  "elpa"))
       (add-to-list 'grep-find-ignored-directories v))
     (dolist (v '("*.min.js"
		  "*.map"
		  "*.bundle.js"
		  "*.min.css"
		  "tags"
		  "TAGS"
		  "GTAGS"
		  "GRTAGS"
		  "GPATH"
		  "cscope.files"
		  "*.json"
		  "*.log"))
       (add-to-list 'grep-find-ignored-files v))))
;;}}}

;;{{{ recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 50
      recentf-exclude '("/tmp/"
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
			"\\.pdf$"
			"\\.docx?$"
			"\\.xlsx?$"
			;; sub-titles
			"\\.sub$"
			"\\.srt$"
			"\\.ass$"
			;; ~/.emacs.d/**/*.el included
                        "\\.elc$"
			;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
			))
;; recentf展示时，可以把HOME目录替换成~
;; (add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
;;}}}

;;{{{ ediff mode config.
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
;; ediff from EmacsClient, it used to be not selected by default.
;; (add-hook 'ediff-startup-hook
;; 	  (lambda ()
;; 	    (progn
;; 	      (select-frame-by-name "Ediff")
;; 	      (set-frame-size(selected-frame) 40 10))))

(message "ediff loaded.")
;;}}}

;;-----------------------------------------------------------
;; enhance isearch mode
(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-i" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-j" 'isearch-edit-string)
             (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
             (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

             (define-key isearch-mode-map (kbd "<ctrl left>") 'isearch-repeat-backward)
             (define-key isearch-mode-map (kbd "<ctrl right>") 'isearch-repeat-forward)
             ;; (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
             ;; (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
             )))

(message "isearch mode loaded.")

(setq doc-view-cache-directory "~/.cache/docview/")

;;----------------------------------------------------------------
;; python shell configuration.
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(message "ipython shell configuration loaded.")


;;------------------------------------------------------------
(global-anzu-mode)
(message "anzu loaded.")

;;------------------------------------------------------------
;;{{{ ace mode.
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back"
  t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

 ;; uncomment if you want to use `ace-jump-mode'
(setq ace-pinyin-use-avy nil)

(message "ace loaded.")
;;}}}

;;-----------------------------------------------------------
;; bookmarks configuration.
(use-package bm
  :ensure t
  :bind
  ("<C-f2>" . bm-toggle)
  ("<f2>" . bm-next)
  ("<S-f2>" . bm-previous)
  )

(message "bm loaded.")
;;------------------------------------------------------------
;; smex configuration.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(message "smex loaded.")
;;------------------------------------------------------------
;; htmlize configuration.
(setq htmlize-output-type 'inline-css)

;;------------------------------------------------------------
;;{{{ dired-single mode.
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
	loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  ;; original is dired-mode-map "^"
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(require 'w32-browser)
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "X" 'dired-w32-browser)
     (define-key dired-mode-map "O" 'dired-w32explore)))

;; open app in wsl.

;;;###autoload
(defmacro wsl--open-with (id &optional app dir)
  `(defun ,(intern (format "wsl/%s" id)) ()
     (interactive)
     (wsl-open-with ,app ,dir)))

(defun wsl-open-with (&optional app-name path)
  "Send PATH to APP-NAME on WSL."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "%s `wslpath -w %s`" 
(shell-quote-argument app-name) path)))
    (shell-command-to-string command)))
(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)

(message "dired single loaded.")
;;}}}


;;------------------------------------------------------------
;;{{{ evil-mode configuration.
;; If you want to use emacs keybindings instead of the ones that evil makes in insert state, you can change the evil-insert-state-bindings variable to your liking or set evil-disable-insert-state-bindings to t before loading evil (or use customize to set it afterwards).
;; https://github.com/noctuid/evil-guide#use-some-emacs-keybindings
(setq evil-disable-insert-state-bindings t)
(require 'evil)
(evil-mode 1)

(defun my-evil-mode-hook ()
  (setq evil-symbol-word-search t)
  (setq evil-want-fine-undo t)
  ;; 当v 选择到行尾时是否包含换行符
  (setq-default evil-want-visual-char-semi-exclusive t)
  ;; C-e ,到行尾时,光标的位置是在最后一个字符后,还是在字符上
  (setq evil-move-cursor-back t)
  ;; (setq evil-highlight-closing-paren-at-point-states nil)
  )
(add-hook 'evil-normal-state-entry-hook 'my-evil-mode-hook)

;; specify major mode uses Evil (vim) NORMAL state or EMACS original state.
;; You may delete this setup to use Evil NORMAL state always.
(dolist (p '((minibuffer-inactive-mode . emacs)
             (calendar-mode . emacs)
	     (calc-mode . emacs)
	     (calculator-mode . emacs)
             (special-mode . emacs)
             (grep-mode . emacs)
             (Info-mode . emacs)
             (term-mode . emacs)
             (sdcv-mode . emacs)
             (anaconda-nav-mode . emacs)
             (log-edit-mode . emacs)
             (vc-log-edit-mode . emacs)
             (magit-log-edit-mode . emacs)
             (erc-mode . emacs)
             (neotree-mode . emacs)
             (w3m-mode . emacs)
             (gud-mode . emacs)
             (help-mode . emacs)
             (eshell-mode . emacs)
             (shell-mode . emacs)
             (xref--xref-buffer-mode . emacs)
             (gtags-select-mode . emacs)
             ;; (message-mode . emacs)
             (epa-key-list-mode . emacs)
             ;; (fundamental-mode . emacs)
             (woman-mode . normal)
             (profiler-report-mode . emacs)
             (dired-mode . normal)
             (compilation-mode . emacs)
             (speedbar-mode . emacs)
             (ffip-file-mode . emacs)
             (helpful-mode . emacs)
             (ivy-occur-mode . normal)
             (ivy-occur-grep-mode . normal)
             (color-rg-mode . emacs)
             (elfeed-search-mode . emacs)
             (elfeed-show-mode . emacs)
             (messages-buffer-mode . normal)))
  (evil-set-initial-state (car p) (cdr p)))

(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
;; (define-key evil-insert-state-map (kbd "C-k") 'previous-line)
;; (define-key evil-insert-state-map (kbd "C-j") 'next-line)
;; (define-key evil-insert-state-map (kbd "C-h") 'left-char)
;; (define-key evil-insert-state-map (kbd "C-l") 'right-char)
;;------------------------------------------------------------
(define-key evil-normal-state-map (kbd "<up>") 'previous-line)
(define-key evil-normal-state-map (kbd "<down>") 'next-line)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-visual-line)
(define-key evil-normal-state-map (kbd "q") 'quit-window)

;;------------------------------------------------------------
;; better way to set insert mapping to emacs. set evil-disable-insert-state-bindings to t.
;; (defalias 'evil-insert-state 'evil-emacs-state)
;; (fset 'evil-insert-state 'evil-emacs-state)
;; remove all keybindings from insert-state keymap, use emacs-state when editing
(setcdr evil-insert-state-map nil)
;;;把emacs模式下的按键绑定到Insert模式下
(define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
;; ESC to switch back normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; TAB to indent in normal-state
(define-key evil-visual-state-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
;; enable cua-mode when entering insert state. and disable cua-mode
;; when exiting insert state.
(add-hook 'evil-insert-state-entry-hook (lambda ()
                                          (cua-mode 1)))
(add-hook 'evil-insert-state-exit-hook (lambda ()
                                         (cua-mode -1)))

;; disable some minor mode in evil.
;; solution 1, reset keymaps.
;; (eval-after-load 'view-mode
;;   '(progn
;;      (evil-make-overriding-map view-mode-map 'normal)
;;      ;; force update evil keymaps after xxx-mode loaded
;;      (add-hook 'view-mode-hook #'evil-normalize-keymaps)))
;; solution 2, disable evil local mode.
;; (defadvice view-mode (after toggle-evil activate)
;;   "Turn off `evil-local-mode' when enabling
;; `view-mode', and turn it back on when disabling `view-mode'."
;;   (evil-local-mode (if view-mode -1 1)))

(global-evil-matchit-mode 1)
;; (evil-find-char-pinyin-mode 1)
(global-evil-visualstar-mode t)
(global-evil-surround-mode t)

(use-package evil-pinyin
  :init
  ;;(setq evil-pinyin-scheme 'simplified-xiaohe-all)
  (setq evil-pinyin-with-search-rule 'exclam)     ;; 感叹号开启

  :config
  ;;; set evil-search to evil-ex-search.
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-search-persistent-highlight nil)
  (global-evil-pinyin-mode))

(with-eval-after-load 'evil
  (require 'evil-anzu))

;; 搜索选中的内容
(defun evil-ex-start-search-with-region-string ()
 (let ((selection (with-current-buffer (other-buffer (current-buffer) 1)
                   (when (evil-visual-state-p)
                    (let ((selection (buffer-substring-no-properties (region-beginning)
                                      (1+ (region-end)))))
                     (evil-normal-state)
                     selection)))))
  (when selection
   (evil-ex-remove-default)
   (insert selection)
   (evil-ex-search-activate-highlight (list selection
                                       evil-ex-search-count
                                       evil-ex-search-direction)))))

(advice-add 'evil-ex-search-setup :after 'evil-ex-start-search-with-region-string)

(message "evil mode loaded.")
;;}}}

;;----------------------------------------------------------------
;;{{{ nearby file path as text object,
;;      - "vif" to select base name
;;      - "vaf" to select full path
;;
;;  example:
;;    "/hello/world"
;;    "/test/back.exe"
;;    "C:hello\\hello\\world\\test.exe"
;;    "D:blah\\hello\\world\\base.exe"
(defun evil-filepath-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let* (rlt prefix-ch postfix-ch)
    (when (and (> (point) (point-min)) (< (point) (point-max)))
      (save-excursion
        (backward-char)
        (setq prefix-ch (following-char)))
      (save-excursion
        (forward-char)
        (setq postfix-ch (following-char))))
    (if (and (not (or (= prefix-ch 32) (= postfix-ch 32)))
             (or (= ch 47) (= ch 92)) )
        (setq rlt t))
    rlt))


(defun evil-filepath-not-path-char (ch)
  "Check ascii table for charctater."
  (or (and (<= 0 ch) (<= ch 32))
      (memq ch
            '(34 ; double quotes
              ?'
              40 ; (
              41 ; )
              ?<
              ?>
              91 ; [
              93 ; ]
              ?`
              ?{
              ?}
              127))))


(defun evil-filepath-calculate-path (b e)
  (let* (rlt f)
    (when (and b e)
      (setq b (+ 1 b))
      (when (save-excursion
              (goto-char e)
              (setq f (evil-filepath-search-forward-char 'evil-filepath-is-separator-char t))
              (and f (>= f b)))
        (setq rlt (list b (+ 1 f) (- e 1)))))
    rlt))


(defun evil-filepath-get-path-already-inside ()
  (let* (b e)
    (save-excursion
      (setq b (evil-filepath-search-forward-char 'evil-filepath-not-path-char t)))
    (save-excursion
      (when (setq e (evil-filepath-search-forward-char 'evil-filepath-not-path-char))
        (goto-char (- e 1))
        ;; example: hello/world,
        (if (memq (following-char) '(?, ?.))
            (setq e (- e 1)))))
    (evil-filepath-calculate-path b e)))


(defun evil-filepath-search-forward-char (fn &optional backward)
  (let* (found
         rlt
         (limit (if backward (point-min) (point-max)))
         out-of-loop)
    (save-excursion
      (while (not out-of-loop)
        ;; for the char, exit
        (if (setq found (apply fn (list (following-char))))
            (setq out-of-loop t)
          ;; reach the limit, exit
          (if (= (point) limit)
              (setq out-of-loop t)
            ;; keep moving
            (if backward (backward-char) (forward-char)))))
      (if found (setq rlt (point))))
    rlt))


(defun evil-filepath-extract-region ()
  "Find the closest file path"
  (let* (rlt b f1 f2)
    (if (and (not (evil-filepath-not-path-char (following-char)))
             (setq rlt (evil-filepath-get-path-already-inside)))
        ;; maybe (point) is in the middle of the path
        t
      ;; need search forward AND backward to find the right path
      (save-excursion
        ;; path in backward direction
        (when (setq b (evil-filepath-search-forward-char 'evil-filepath-is-separator-char t))
          (goto-char b)
          (setq f1 (evil-filepath-get-path-already-inside))))
      (save-excursion
        ;; path in forward direction
        (when (setq b (evil-filepath-search-forward-char 'evil-filepath-is-separator-char))
          (goto-char b)
          (setq f2 (evil-filepath-get-path-already-inside))))
      ;; pick one path as the final result
      (cond
       ((and f1 f2)
        (if (> (- (point) (nth 2 f1)) (- (nth 0 f2) (point)))
            (setq rlt f2)
          (setq rlt f1)))
       (f1
        (setq rlt f1))
       (f2
        (setq rlt f2))))
    rlt))


(evil-define-text-object evil-filepath-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (let* ((selected-region (evil-filepath-extract-region)))
    (if selected-region
        (evil-range (nth 1 selected-region) (nth 2 selected-region) :expanded t))))


(evil-define-text-object evil-filepath-outer-text-object (&optional NUM begin end type)
  "Nearby path."
  (let* ((selected-region (evil-filepath-extract-region)))
    (if selected-region
        (evil-range (car selected-region) (+ 1 (nth 2 selected-region)) type :expanded t))))


(define-key evil-inner-text-objects-map "f" 'evil-filepath-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'evil-filepath-outer-text-object)

;;}}}


;;------------------------------------------------------------
;;{{{ helm mode configuration.
(use-package helm-config
  :config
  ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-completion-in-region-fuzzy-match t
        helm-mode-fuzzy-match t)
  :bind
  ;; (global-set-key (kbd "M-x") 'helm-M-x)
  ;; ("C-x C-b" . 'helm-mini)
  ;; (:map isearch-mode-map
  ;;       ("C-o" . 'helm-occur-from-isearch))
  )

(message "helm loaded.")

;;}}}

;;------------------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;------------------------------------------------------------
;; (setq markdown-command "pandoc")
(eval-after-load 'markdown-mode
  '(progn
     ;; `pandoc' is better than obsolete `markdown'
     (when (executable-find "pandoc")
       (setq markdown-command "pandoc -f markdown"))))

;;------------------------------------------------------------
(use-package fold-dwim
  :defer 3)

;;------------------------------------------------------------
;;{{{ multi-cursors.
;; (defun mc/my-quit ()
;;   "Quit from mark mode."
;;   (interactive)
;;   (mc/keyboard-quit)
;;   (multiple-cursors-mode 0))

;; (use-package multiple-cursors
;;   ;; :bind (("C->"           . mc/mark-next-like-this)
;;   ;;        ("C-<"           . mc/mark-previous-like-this)
;;   ;;        ("C-M->"         . mc/skip-to-next-like-this)
;;   ;;        ("C-M-<"         . mc/skip-to-previous-like-this)
;;   ;;        ("C-;"           . mc/mark-all-like-this)
;;   ;;        ("C-M-<mouse-1>" . mc/add-cursor-on-click)
;;   ;;        ("C-:"           . mc/my-quit)
;;   ;;        ("M-n"           . mc/cycle-forward)
;;   ;;        ("M-p"           . mc/cycle-backward))
;;   :config
;;   (setq mc/insert-numbers-default 1)
;;   (setq mc/cmds-to-run-once
;;         '(
;;           swiper-mc
;;           ))
;;   )

;;}}}

;------------------------------------------------------------
;;{{{ company-mode configuration
(require 'company)
;; NOT to load company-mode for certain major modes.
;; https://github.com/company-mode/company-mode/issues/29
(setq company-global-modes
      '(not
	eshell-mode shell-mode comint-mode erc-mode gud-mode rcirc-mode
	minibuffer-inactive-mode))
(add-hook 'after-init-hook 'global-company-mode)

;; Trigger completion immediately.
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(setq company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "M-n") 'company-other-backend)

;; (setq company-clang-arguments '(
;;                                 "--target=i686-w64-mingw64"
;;                                 ;; "-IC:/mingw-w64/i686-8.1.0/mingw32/i686-w64-mingw32/include/"
;; 				))

;; company-tabnine
(use-package company-tabnine
  :ensure t)

(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))
(add-to-list 'company-transformers 'company//sort-by-tabnine t)
;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
	       (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
	ad-do-it))))

;; `:separate`  使得不同 backend 分开排序
(add-to-list 'company-backends #'company-tabnine)
;; (push '(company-capf :with company-tabnine :separate) company-backends)

;;}}}

;;----------------------------------------------------------
;; citre configure.
;;----------------------------------------------------------
(defun citre-jump+ ()
  (interactive)
  (condition-case _
      (citre-jump)
    (error (call-interactively #'lsp-find-definition))))

(defun my--push-point-to-xref-marker-stack (&rest r)
  (xref-push-marker-stack (point-marker)))
(dolist (func '(find-function
                counsel-imenu
                helm-imenu
                projectile-grep
                helm-grep-ag
                counsel-rg
                lsp-ivy-workspace-symbol
                citre-jump))
  (advice-add func :before 'my--push-point-to-xref-marker-stack))

(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.
  (global-set-key (kbd "C-M-]") 'citre-jump)
  (global-set-key (kbd "C-M-[") 'citre-jump-back)
  (global-set-key (kbd "M-]") 'citre-jump+)
  (global-set-key (kbd "M-[") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-M-p") 'citre-ace-peek)
  :config
  (setq
   ;; Set this if you want to always use one location to create a tags file.
   citre-tags-file-global-cache-dir "~/.cache/tags/"
   citre-default-create-tags-file-location 'global-cache
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; Set this if readtags is not in your path.
   ;; citre-readtags-program "/path/to/readtags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   ;; citre-project-root-function #'projectile-project-root
   ))

(defun company-citre (-command &optional -arg &rest _ignored)
  "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'company-citre))
    (prefix (and (bound-and-true-p citre-mode)
                 (or (citre-get-symbol) 'stop)))
    (meta (citre-get-property 'signature -arg))
    (annotation (citre-capf--get-annotation -arg))
    (candidates (all-completions -arg (citre-capf--get-collection -arg)))
    (ignore-case (not citre-completion-case-sensitive))))

;; use company-capf to auto-complete
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'python-mode-hook
               'go-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (push '(company-capf company-citre :with company-tabnine :separate) company-backends)
                    )))

;;------------------------------------------------------------
;;{{{ outline-mode configuration.
;; change the ellipsis.
(set-display-table-slot
     standard-display-table
     'selective-display
     (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
       (vconcat (mapcar (lambda (c) (+ face-offset c)) " [...] "))))

(defun whitespace-change-ellipsis ()
    "Change ellipsis when used with `whitespace-mode'."
    (when buffer-display-table
      (set-display-table-slot buffer-display-table
                              'selective-display
                              (string-to-vector " [...]"))))
(add-hook 'whitespace-mode-hook #'whitespace-change-ellipsis)

;;}}}

;;------------------------------------------------------------
(require 'calendar-init)

(require 'org-init)

;; some eshell functions
(require 'eshell-init)

;;------------------------------------------------------------
;;{{{ ivy and counsel configuration.
;; Add recent files and bookmarks to the ivy-switch-buffer
(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

(ivy-set-actions
 'counsel-find-file '(
                      ("j" find-file-other-frame "other frame")
                      ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
                      ("x" counsel-find-file-extern "open externally")
                      ("d" delete-file "delete")))

(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (unless (featurep 'counsel) (require 'counsel))
    (counsel-imenu)))

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
;; can not set to nil because some package will use this value.
;; (setq large-file-warning-threshold nil)

(use-package counsel-etags
  :ensure t
  :bind (("C-x c ]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-tags-program "ctags -e -L")
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(eval-after-load 'counsel-etags
  '(progn
     ;; counsel-etags-ignore-directories does NOT support wildcast
     (push "build_clang" counsel-etags-ignore-directories)
     ;; counsel-etags-ignore-filenames supports wildcast
     (push "TAGS" counsel-etags-ignore-filenames)
     (push "*.json" counsel-etags-ignore-filenames)))

(unless (featurep 'pinyinlib) (require 'pinyinlib))
(setq counsel-etags-convert-grep-keyword
  (lambda (keyword)
    (if (and keyword (> (length keyword) 0))
        (pinyinlib-build-regexp-string keyword t)
      keyword)))

;; ‘counsel-yank-pop’ in visual mode does not replace the region, instead it just inserts the text. advise ‘counsel-yank-pop’ with a function that kills the region.
(defun moon-override-yank-pop (&optional arg)
  "Delete the region before inserting poped string."
  (when (and evil-mode (eq 'visual evil-state))
    (kill-region (region-beginning) (region-end))))
(advice-add 'counsel-yank-pop :before #'moon-override-yank-pop)

;; 显示具体的搜索命令
;; (defun counsel-before-counsel--async-command (cmd &rest _)
;;   (unless (stringp cmd)
;;     (setq cmd (string-join cmd " ")))
;;   (lv-message "Command: (@%s) %s"
;;               (propertize default-directory 'face font-lock-constant-face)
;; 有乱码，改编码没有用。。。
;;               (propertize (encode-coding-string cmd locale-coding-system) 'face font-lock-doc-face)))
              ;; (propertize cmd 'face font-lock-doc-face)))
;; (propertize (encode-coding-string cmd locale-coding-system) 'face font-lock-doc-face)))

;; (advice-add 'counsel--async-command :before
;;             #'counsel-before-counsel--async-command)

;; find-file-in-project
(use-package find-file-in-project
  :ensure t
  :config
  (defalias 'ffip 'find-file-in-project-by-selected)
  (setq ffip-use-rust-fd t)
  )

;;}}}
;;------------------------------------------------------------

;;{{{ pyim configuration with ivy.
(use-package pyim
  :after ivy
  :config
  ;; (defun eh-ivy-cregexp (str)
  ;;   (concat
  ;;    (ivy--regex-plus str)
  ;;    "\\|"
  ;;    (pyim-cregexp-build str)))

  (defun eh-ivy-cregexp (str)
    (let ((x (ivy--regex-plus str))
    ;; (let ((x (ivy--regex-ignore-ordre str))
          (case-fold-search nil))
      (if (listp x)
          (mapcar (lambda (y)
                    (if (cdr y)
                        (list (if (equal (car y) "")
                                  ""
                                (pyim-cregexp-build (car y)))
                              (cdr y))
                      (list (pyim-cregexp-build (car y)))))
                  x)
        (pyim-cregexp-build x))))

  (setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp))))

(message "ivy configuration loaded.")
;;}}}

;;------------------------------------------------------------
;;{{{ w3m configuration.
(setq w3m-use-cookies t
      w3m-coding-system 'utf-8
      ;; w3m-file-coding-system 'utf-8
      ;; w3m-file-name-coding-system 'utf-8
      ;; w3m-input-coding-system 'utf-8
      ;; w3m-output-coding-system 'utf-8
      ;; w3m-terminal-coding-system 'utf-8
      w3m-cookie-accept-bad-cookies t
      w3m-home-page "www.emacs-china.org"
      ;; 设定w3m运行的参数，分别为使用cookie和使用框架
      w3m-command-arguments       '("-F" "-cookie")
      w3m-mailto-url-function     'compose-mail
      w3m-use-toolbar t
      w3m-fill-column 100
      w3m-use-tab     nil
      w3m-confirm-leaving-secure-page nil
      )

;;设置显示图片
(setq w3m-default-display-inline-images t)
(setq w3m-default-toggle-inline-images t)
;;显示图标
(setq w3m-show-graphic-icons-in-header-line t)
(setq w3m-show-graphic-icons-in-mode-line t)

(defun w3m-browse-buffer (&optional buffer)
  "Use w3m browser buffer BUFFER."
  (interactive "bBuffer to browse use w3m: ")
  (setq buffer (get-buffer buffer))
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (save-excursion
      (w3m-buffer)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq w3m-current-title (buffer-name)))))

(defun dired-w3m-find-file ()
  (interactive)
  (require 'w3m)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
        (w3m-find-file file))))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "W" 'dired-w3m-find-file)))

;; (setq w3m-browse-buffer-use-current-dir t)
;; (defun w3m-browse-current-buffer ()
;;   (interactive)
;;   (let (filename)
;;     (if w3m-browse-buffer-use-current-dir
;;         (setq filename (if buffer-file-name
;;                            (expand-file-name
;;                             (concat (file-name-nondirectory (buffer-name)) ".htm")
;;                             default-directory)
;;                          (expand-file-name "w3mtmpfile.htm" default-directory)))
;;       (setq filename (concat (make-temp-file "w3m-") ".html")))
;;     (unwind-protect
;;         (progn
;; 	  (set-buffer-file-coding-system 'utf-8 t t)
;;           (write-region (point-min) (point-max) filename)
;;           (w3m-find-file filename))
;;       (delete-file filename))))

(defun w3m-print-current-url ()
  "Display current url."
  (interactive)
  (w3m-message "%s" (w3m-url-readable-string w3m-current-url)))

(defun w3m-copy-current-url ()
  "Display the current url in the echo area and put it into `kill-ring'."
  (interactive)
  (when w3m-current-url
    (let ((deactivate-mark nil))
      (kill-new w3m-current-url)
      (w3m-print-current-url))))

(defun w3m-copy-url-at-point ()
  (interactive)
  (let ((url (w3m-anchor)))
    (if (w3m-url-valid url)
        (kill-new (w3m-anchor))
      (message "No URL at point!"))))

(add-hook 'w3m-mode-hook
          (lambda ()
            (local-set-key "\C-y" 'w3m-copy-url-at-point)))

(message "w3m configuration loaded.")
;;}}}

;;----------------------------------------------------------------
;; browser configure.

;; (setq browse-url-browser-function 'w3m)
;; use shr to view html mail which is dependent on libxml
;; I prefer w3m. That's emacs 24.3+ default setup.
;; If you prefer colored mail body and other advanced features,
;; you can either comment out below line and let Emacs decide the
;; best html mail rendering engine, or "(setq mm-text-html-renderer 'shr)"
;; in "~/.gnus.el"
;; (setq mm-text-html-renderer 'w3m)

;; (setq browse-url-generic-program (executable-find "/cygdrive/c//Program Files/Mozilla Firefox/firefox.exe"))
(setq browse-url-generic-program (executable-find "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"))
;; (setq browse-url-generic-program (executable-find "C:/Program Files/Mozilla Firefox/firefox.exe"))
(setq browse-url-browser-function 'browse-url-generic)

;;-----------------------------------------------------------
;; smart-compile.
;;(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)

;;------------------------------------------------------------
;; helpful
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

;;------------------------------------------------------------
;; for gtags;
;; (setenv "GTAGSLABLE" "native-pygments")

;;------------------------------------------------------------
;; novel mode. for epub.
;;------------------------------------------------------------
;;{{{ nov.
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; (setq nov-text-width 80)
;; set it to t to inhibit text filling
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)

(with-eval-after-load 'nov
  (defun novel-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "YaHei Consolas Hybrid 16")   ;; FiraCode NF 16
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
    (add-hook 'nov-mode-hook 'novel-buffer-face-mode-variable))

(with-eval-after-load "nov"
  (when (string-equal system-type "windows-nt")
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist))))

;; FIXME: errors while opening `nov' files with Unicode characters
(with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

;;}}}

;;------------------------------------------------------------
;; imenu list
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)

;;---------------------------------------------------
;; magit
;; (use-package magit
;;   :ensure t
;;   :config
;;   (progn
;;     (setq magit-git-executable "d:/msys64/usr/lib/git-core/git.exe"))
;;   )

;; 默认全局使用
(projectile-global-mode)
;; 默认打开缓存
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;------------------------------------------------------------
;;{{{ treemacs
(use-package treemacs
  :ensure t
  :defer  t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-python-executable "python.exe"
          treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-filewatch-mode                t
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name "~/.cache/treemacs-persist")
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-mode               t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t b"   . treemacs-bookmark)
        ("C-x t f"   . treemacs-find-file)
        ("C-x t g"   . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-icons-dired
  :after dired
  :ensure t
  :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;; (use-package treemacs-persp
;;   :after treemacs persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;;}}}

;;------------------------------------------------------------
;;{{{ all-the-icons.
(require 'all-the-icons-init)

;;}}}

;;------------------------------------------------------------
;; neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-width 50)
(setq neo-autorefresh t)

;;------------------------------------------------------------
;; youdao-dictionary
;; Enable Cache
(setq url-automatic-caching t)

;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
;; (push "*Youdao Dictionary*" popwin:special-display-config)

;; Set file path for saving search history
(setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")

;; Enable Chinese word segmentation support (支持中文分词)
(setq youdao-dictionary-use-chinese-word-segmentation t)

(require 'ivy-xref) ; unless installed from a package
(setq xref-show-xrefs-function 'ivy-xref-show-xrefs)

;;------------------------------------------------------------
(message "package configuration loaded.")

;;------------------------------------------------------------
;;{{{ lisp configuration.

;;-------------------------------------------------------------
(require 'irfc)
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
           (setq imenu-generic-expression irfc-imenu-generic-expression)))

;;------------------------------------------------------------
;; sdcv
;; (use-package sdcv-mode
;;   :defer 2)
;; (with-eval-after-load 'sdcv-mode
;;   (defun sdcv-buffer-face-mode-variable ()
;;     (interactive)
;;     (set-buffer-file-coding-system 'utf-8)
;;     (make-face 'width-font-face)
;;     (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 16")   ;; FiraCode NF 16
;;     (setq buffer-face-mode-face 'width-font-face)
;;     (buffer-face-mode))
;;     (add-hook 'sdcv-mode-hook 'sdcv-buffer-face-mode-variable))

(require 'sdcv)
(setq sdcv-say-word-p nil)               ;say word after translation

(setq sdcv-dictionary-data-dir "/cygdrive/d/unix/dic/") ;setup directory of stardict dictionary
(setq sdcv-env-lang "zh_CN.UTF-8")
(setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '(
        "懒虫简明英汉词典"
        ))

(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '(
        "CDICT5英汉辞典"
        "牛津英汉双解美化版"
        "朗道英汉字典5.0"
        ))

;;------------------------------------------------------------
(require 'keep-buffers)

(require 'swbuff-x)
(setq swbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*" "TAGS$"))
(setq swbuff-exclude-mode-regexp "dired-mode")
(setq swbuff-separator "|")
(setq swbuff-clear-delay 1)
(global-set-key (kbd "<C-tab>") 'swbuff-switch-to-next-buffer)
(global-set-key (kbd "<C-S-kp-tab>") 'swbuff-switch-to-previous-buffer)
;; (global-set-key (kbd "<C-right>") 'swbuff-switch-to-next-buffer)
;; (global-set-key (kbd "<C-left>") 'swbuff-switch-to-previous-buffer)

(use-package hexview-mode
  :defer 2)

(require 'iscroll-all)
(add-hook 'ediff-startup-hook 'scroll-all-mode)
(add-hook 'ediff-quit-hook
          '(lambda ()
             (scroll-all-mode -1)))

;;------------------------------------------------------------
;; company-english-helper
;; https://github.com/manateelazycat/company-english-helper
(use-package company-english-helper
  :defer 3)
;; insert-translated-name-insert
;; https://github.com/manateelazycat/insert-translated-name
(use-package insert-translated-name
  :defer 3
  :config
  (setq insert-translated-name-translate-engine "google"))

;;------------------------------------------------------------
;; elfeed configuration
(require 'elfeed)
(setq elfeed-feeds
      '(
        "http://www.chinanews.com/rss/importnews.xml"
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

;; 设置elfeed使用独立的字体
;; (with-eval-after-load 'elfeed
;;   (defun elfeed-buffer-face-mode-variable ()
;;     (interactive)
;;     (make-face 'width-font-face)
;;     (set-face-attribute 'width-font-face nil :font "FiraCode NF 16")   ;; FiraCode NF 16
;;     (setq buffer-face-mode-face 'width-font-face)
;;     (buffer-face-mode))
;;     (add-hook 'elfeed-show-mode-hook 'elfeed-buffer-face-mode-variable))

;;------------------------------------------------------------
;; color-rg
;; https://github.com/manateelazycat/color-rg
(require 'color-rg)

;; https://github.com/manateelazycat/thing-edit
(require 'thing-edit)

;; https://github.com/lyjdwh/avy-thing-edit/blob/master/avy-thing-edit.el
(require 'avy-thing-edit)

;;}}}

;;------------------------------------------------------------

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;;------------------------------------------------------------
;; flymake disabled.
(setq lsp-diagnostic-package :none)
;; yasnappet disabled.
(setq lsp-enable-snippet nil)
(setq lsp-enable-symbol-highlighting nil)

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (c-set-style "stroustrup")
                    (lsp)
                    )))
;; C20 syntax support.
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(dolist (hook (list
               'python-mode-hook
               'make-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (setq-local indent-tabs-mode t)
                    (setq-local tab-width 4)
                    )))

(add-hook 'go-mode-hook 'lsp-deferred)

(add-hook 'yaml-mode-hook 'lsp-deferred)

;;----------------------------------------------------------------
(window-numbering-mode)

;;----------------------------------------------------------------
;;设置标题栏为buffer的内容
(setq frame-title-format
      '(
        (:eval (if (buffer-modified-p)
                   " ! "))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " @ Emacs" emacs-version
        " - " minor-mode-alist)
      )
;;------------------------------------------------------------
;; mode-line configuration.
;;------------------------------------------------------------
;; (require 'modeline-init)
(use-package doom-modeline
  :custom
  (doom-modeline-modal-icon nil)
  (doom-modeline-icon t)
  :hook (after-init . doom-modeline-mode))

;;----------------------------------------------------------------
;;{{{ self face reconfigure
;; (set-face-attribute 'font-lock-comment-face nil
;;                     :foreground "grey50")   ;; #292e34
(set-face-attribute 'fringe nil
                    :foreground "green")
(set-face-attribute 'show-paren-match nil
                    :foreground "green"
                    :bold t
                    :underline t)
(set-face-attribute 'show-paren-mismatch nil
                    :foreground "red"
                    :bold t
                    :underline t)
;; (set-face-attribute 'evil-ex-lazy-highlight nil
;;                     :background "grey50")
;; virtual files color for ivy-switch-buffer.
;; (set-face-attribute 'ivy-virtual nil
;;                     :foreground "grey50")

(setq evil-normal-state-cursor '("#E02C6D" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-emacs-state-cursor '("gold" box))

;; Fall back font for glyph missing in Roboto
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'font-lock-comment-face))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'font-lock-comment-face))

;;}}}

;;------------------------------------------------------------
;;{{{ self-defin function
;; copy region or whole line
(global-set-key "\M-w"
		(lambda ()
		  (interactive)
		  (if mark-active
		      (kill-ring-save (region-beginning)
				      (region-end))
		    (progn
		      (kill-ring-save (line-beginning-position)
				      (line-end-position))
		      (message "copied line")))))

;; kill region or whole line
;; (global-set-key "\C-k"
;; 		(lambda ()
;; 		  (interactive)
;; 		  (if mark-active
;; 		      (kill-region (region-beginning)
;; 				   (region-end))
;; 		    (progn
;; 		      (kill-region (line-beginning-position)
;; 				   (line-end-position))
;; 		      (message "killed line")))))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(defun my-add-new-line()
  "jump to new line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; C-M-, 在另一窗口处查看光标处的 tag
(global-set-key (kbd "C-M-,") '(lambda () (interactive) (lev/find-tag t)))
(defun lev/find-tag (&optional show-only)
  "Show tag in other window with no prompt in minibuf."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
			      (get major-mode 'find-tag-default-function)
			      'find-tag-default))))
    (if show-only
	(progn (find-tag-other-window default)
	       (shrink-window (- (window-height) 12)) ;; 限制为 12 行
	       (recenter 1)
	       (other-window 1))
      (find-tag default))))

;; select the total word including - inside.
(defun select-total-word(&optional arg)
  "select the total word including - inside."
  (interactive "P")
  (backward-sexp)
  (set-mark (point))
  (forward-sexp))
;; quotes

;; select the total word including - inside.
(defun select-total-part(&optional arg)
  (interactive "sCharacter: ")
  ;; if arg not nil, search backword for arg, then put the mark.
  (if (eq t (compare-strings "" 0 (length "") arg 0 (length arg))) (search-backward "\"")
    (search-backward arg))
  (set-mark (point))
  (forward-sexp)
  (backward-char))

(defun dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;set transparent effect
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
 (defun loop-alpha ()
   (interactive)
   (let ((h (car alpha-list)))                ;; head value will set to
     ((lambda (a ab)
        (set-frame-parameter (selected-frame) 'alpha (list a ab))
        (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
        ) (car h) (car (cdr h)))
     (setq alpha-list (cdr (append alpha-list (list h))))
     )
 )

(defun view-indirect-buffer-other-window ()
  (interactive)
  (clone-indirect-buffer-other-window (buffer-name) t)
  (view-buffer (current-buffer)))

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unlessbuffer-display-table
   (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push
   (if(region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (let((sym (thing-at-point 'symbol)))
       (when (stringp sym) (regexp-quote sym)))) regexp-history)
  (call-interactively 'occur))

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

;;;###autoload
(defun occur-from-evil-ex ()
  "Invoke `occur' from isearch."
  (interactive)
  (occur (car evil-ex-search-pattern))
  (pop-to-buffer "*Occur*"))

(define-key isearch-mode-map (kbd "C-c o") 'occur-from-isearch)
(define-key evil-motion-state-map (kbd "C-c o") 'occur-from-evil-ex)

;;------------------------------------------------------------
;; PDB command line
(defun my-python-debug-buffer ()
  "Run python debugger on current buffer."
  (interactive)
  (setq command (format "python -u -m pdb %s " (file-name-nondirectory buffer-file-name)))
  (let ((command-with-args (read-string "Debug command: " command nil nil nil)))
    (pdb command-with-args)))

(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

;; highlight the marker line in gud mode.
(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;;(move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer)))))

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))
(add-hook 'kill-buffer-hook 'gud-kill-buffer)

;; move line up down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; jump out.
(global-set-key (kbd "M-<left>") 'backward-up-list)
(global-set-key (kbd "M-<right>") 'up-list)

;; 在括号内时就高亮包含内容的两个括号
;; (define-advice show-paren-function (:around (fn) fix-show-paren-function)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn)
;;         (t (save-excursion
;;              (ignore-errors (backward-up-list))
;;              (funcall fn)))))

;;}}}


(defcustom my-tmp-register 8
  "Register used to store the cursor position."
  :type 'integer)

(defcustom my-original-register 9
  "Register used to store the cursor position."
  :type 'integer)

(defun my-tmp-mark ()
  "Store cursor position fast in a register.

Use `my-tmp-back` to jump back to the stored position."
  (interactive)
  (point-to-register my-tmp-register)
  (point-to-register my-original-register)
  (message "tmp marked in %s." (point-marker)))

(defun my-tmp-back-original ()
  "Jumps between current and stored cursor position."
  (interactive)
  (set-register my-tmp-register (point-marker))
  (jump-to-register my-original-register)
  (message "tmp marked in %s" (point-marker)))

(defun my-tmp-back ()
  "Jumps between current and stored cursor position."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register my-tmp-register)
    (set-register my-tmp-register tmp)
    (message "tmp marked in %s" tmp)))

;;----------------------------------------------------------------
;; emacs buildin mode.
;;----------------------------------------------------------------
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(use-package ediff
  :ensure nil
  :hook (ediff-quit . winner-undo))

(use-package hideshow
  :ensure nil
  :bind (:map prog-mode-map
         ("C-c TAB" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
           '((c-mode "{" "}" "/[*/]" nil nil)
             (c++-mode "{" "}" "/[*/]" nil nil)
             (rust-mode "{" "}" "/[*/]" nil nil)))))

;; 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

(global-origami-mode 1)

(use-package whitespace
  :ensure nil
  ;; :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))


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
;;{{{ hydra configuration.
(defhydra hydra-jump (:color blue :hint nil)
  "
wo_m_an  _t_ranslation  _y_oudao  _e_nglish
_c_apture  _o_rg-open
  "
  ("y" youdao-dictionary-search-at-point)
  ("e" toggle-company-english-helper)
  ("t" insert-translated-name-insert-original-translation)
  ("o" open-org-note-file)
  ("c" org-capture)
  ("m" woman-helpful)
  )

(defhydra hydra-fold (:color blue :hint nil)
  "
_f_old-toggle  fold-dwim-_h_ide-all  fold-dwim-_s_how-all
hide-_l_ines-matching   hide-lines-_n_ot-matching   hide-lines-show-_a_ll
_z_origami-toggle-node  origami-show-only-nod_e_
  "
  ("l" hide-lines-matching)
  ("n" hide-lines-not-matching)
  ("a" hide-lines-show-all)
  ("f" fold-dwim-toggle)
  ("h" fold-dwim-hide-all)
  ("s" fold-dwim-show-all)
  ("z" origami-toggle-node)
  ("e" origami-show-only-node)
  )

(require 'linum-relative)
(defhydra hydra-toggle (:color pink)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_i_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
_l_ display-line-numbers-mode:    %`display-line-numbers-mode
_r_ linum-relative-mode:    %`linum-relative-mode
_p_ toggle-word-wrap:   %`word-wrap
_v_ evil-local-mode:    %`evil-local-mode
_o_ origami-mode:       %`origami-mode
_h_ hs-minor-mode:      %`hs-minor-mode
_j_ visual-line-mode:   %`visual-line-mode
_c_ cua-mode:           %`cua-mode
_y_ evil-pinyin-mode:   %`evil-pinyin-mode
"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("i" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("l" display-line-numbers-mode nil)
  ("r" linum-relative-mode nil)
  ("p" toggle-word-wrap nil)
  ("v" evil-local-mode nil)
  ("o" origami-mode nil)
  ("h" hs-minor-mode nil)
  ("j" visual-line-mode nil)
  ("c" cua-mode nil)
  ("y" evil-pinyin-mode nil)
  ("q" nil "quit"))

(defhydra hydra-avy-copy (:color blue :hint nil)
  "
_w_ord    _s_ymbol     _f_ile   _l_ine   _u_rl    e_m_ail
  "
  ("w"  avy-thing-copy-word)
  ("s"  avy-thing-copy-symbol)
  ("f"  avy-thing-copy-filename)
  ("l"  avy-thing-copy-line)
  ("u"  avy-thing-copy-url)
  ("m"  avy-thing-copy-email)
  )

(defhydra hydra-copy (:color blue :hint nil)
  "
_w_ord    _s_ymbol   _f_ile   _l_ine   _u_rl    e_m_ail
  "
  ("w"  thing-copy-word)
  ("s"  thing-copy-symbol)
  ("f"  thing-copy-filename)
  ("l"  thing-copy-line)
  ("u"  thing-copy-url)
  ("m"  thing-copy-email)
  )

;; Recommended binding:
(defhydra hydra-all (global-map "C-c h" :color blue :hint nil)
  "
_j_ump    _t_oggle    _f_old     a_v_y-copy  cop_y_
  "
  ("j" hydra-jump/body)
  ("t" hydra-toggle/body)
  ("f" hydra-fold/body)
  ("y" hydra-copy/body)
  ("v" hydra-avy-copy/body)
)

(message "hydra key binding.")
;;}}}

;;------------------------------------------------------------
(use-package general
  :config (general-evil-setup)

  ;; use `,` as leader key
  (general-create-definer my-comma-leader-def
    :prefix "<SPC>"
    ;; :prefix ","
    :states '(normal visual))

  (my-comma-leader-def
    ",,"  'my-tmp-mark
    ".."  'my-tmp-back
    ",."  'my-tmp-back-original
    "aa"  'ace-pinyin-dwim
    "al"  'ace-jump-line-mode
    "aw"  'ace-jump-word-mode
    ;; "bb"  '((lambda () (interactive) (switch-to-buffer nil)) :which-key "prev-buffer")
    "cc"  'evilnc-comment-or-uncomment-lines
    "cp"  'evilnc-comment-or-uncomment-paragraph
    "ct"  'evilnc-comment-or-uncomment-html-tag
    "dd"  'sdcv-search-input+
    "dj"  'dired-jump
    "nt"  'neotree-toggle
    "ss"  'swiper-thing-at-point
    "rg"  'color-rg-search-input
    "rp"  'color-rg-search-input-in-project
    "ff"  'counsel-fd-file-jump
    "fg"  'counsel-grep-or-swiper
    "fl"  'counsel-locate
    "fm"  'counsel-imenu
    "hh"  'helpful-at-point
    "ul"  'browse-url
    "yt"  'select-total-part
    ;; hydra binding
    "<SPC>" 'hydra-all/body
   )

  (general-create-definer my-leader-def
    :prefix "C-c")

  ;; ** Global Keybindings
  (my-leader-def
    ",,"  'my-tmp-mark
    ".."  'my-tmp-back
    ",."  'my-tmp-back-original
    "aa"  'ace-pinyin-dwim
    "al"  'ace-jump-line-mode
    "aw"  'ace-jump-word-mode
    ;; "bb"  '((lambda () (interactive) (switch-to-buffer nil)) :which-key "prev-buffer")
    "cc"  'evilnc-comment-or-uncomment-lines
    "cp"  'evilnc-comment-or-uncomment-paragraph
    "ct"  'evilnc-comment-or-uncomment-html-tag
    "dd"  'sdcv-search-input
    "dj"  'dired-jump
    "nt"  'neotree-toggle
    "ss"  'swiper-thing-at-point
    "rg"  'color-rg-search-input
    "rp"  'color-rg-search-input-in-project
    "ff"  'counsel-fd-file-jump
    "fg"  'counsel-grep-or-swiper
    "fl"  'counsel-locate
    "fm"  'counsel-imenu
    "hh"  'helpful-at-point
    "ul"  'browse-url
    "yt"  'select-total-part
    ;; hydra binding
    "<SPC>" 'hydra-all/body
    )
  )

(message "general key binding end.")

;;------------------------------------------------------------
;; key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [(f3)] 'color-rg-search-input)
(global-set-key [(f4)] 'ace-pinyin-dwim)
(global-set-key [(f5)] 'loop-alpha)
(global-set-key [(f6)] 'sdcv-search-input+)
;; switch between current buffer and previous one.
(global-set-key [(f12)] '(lambda () (interactive) (switch-to-buffer nil)))

;;------------------------------------------------------------
;;{{{ desktop.

;;如果你想保存上次打开的文件记录，那么可以使用 desktop。这是 Emacs 自带的。你只需要加入以上设置，然
;;后 M-x desktop-save。以后 Emacs 启动时就会打开你上次离开时的所有 buffer.
;;M-x desktop-clear
;; Restore the "desktop" - do this as late as possible!!
;; (if first-time
;;     (progn
;;       (load "desktop")
;;       ;; save a bunch of variables to the desktop file
;;       ;; for lists specify the len of the maximal saved data also
;;       (setq desktop-globals-to-save
;; 	    (append '((extended-command-history . 10)
;; 		      (file-name-history        . 30)
;; 		      (ido-last-directory-list  . 10)
;;                       (ido-work-directory-list  . 10)
;;                       (ido-work-file-list       . 10)
;;                       (grep-history             . 3)
;;                       (compile-history          . 3)
;;                       (minibuffer-history       . 5)
;;                       (query-replace-history    . 5)
;;                       (read-expression-history  . 5)
;;                       (regexp-history           . 5)
;;                       (regexp-search-ring       . 3)
;;                       (search-ring              . 3)
;;                       (comint-input-ring        . 5)
;;                       (shell-command-history    . 5)
;;                       desktop-missing-file-warning
;;                       tags-file-name
;;                       register-alist)))
;;       (desktop-save-mode)
;;       (message "reading desktop done.")))

;;}}}

;; Indicate that this file has been read at least once
(setq first-time nil)

;; end of init.el
;;------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(ripgrep origami gcmh projectile citre imenu-extra realgud iscroll tree-sitter csharp-mode fsharp-mode doom-modeline doom-themes neotree go-mode 0blayout elfeed counsel-fd find-file-in-project fd-dired lsp-pyright ivy-xref lsp-ivy lsp-mode spinner powerline treemacs-icons-dired treemacs-evil treemacs org-download centered-cursor-mode general evil-anzu youdao-dictionary evil-pinyin format-all ahk-mode eshell-z eshell-up all-the-icons-ivy all-the-icons-ivy-rich org-superstar all-the-icons-ibuffer all-the-icons imenu-list nov powershell spacemacs-theme smart-compile helpful wgrep modern-cpp-font-lock counsel-etags ace-window quickrun posframe js2-mode evil-textobj-anyblock vimrc-mode dired-single web-mode evil-nerd-commenter hydra evil-surround which-key htmlize hide-lines linum-relative rainbow-mode w32-browser json-mode yaml-mode evil-visualstar anzu ace-pinyin markdown-mode fold-dwim avy evil-matchit window-numbering use-package rainbow-delimiters pyim counsel semi swiper ace-jump-mode smex expand-region cal-china-x bm company-tabnine company w3m helm evil))
 '(recentf-mode t)
 '(save-place-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

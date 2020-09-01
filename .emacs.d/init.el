;; -*- coding: utf-8; lexical-binding: t; -*-

;; Author: lihao

;;; Code:
;; This file is designed to be re-evaled; use the variable first-time

(defvar first-time t
  "Flag signifying this is the first time that .emacs has been evaled")

;;------------------------------------------------------------

;;{{{ basic configuration.
;; start server mode
;; (require 'server)
;; (if (not (server-running-p)) (server-start))
(if (display-graphic-p) (server-start))

;; 在注册表中建如下键值：
;; HKEY_CLASSES_ROOT\*\shell\Edit with Emacs\command
;; 并将其default的值设为：
;; "path\to\emacsclientw.exe" --no-wait --alternate-editor="path\to\runemacs.exe" "%1"

;; 将内存回收阈值增大，加快启动速度；启动完成后更新为初始值
(defvar default-file-name-handler-alist file-name-handler-alist)
(defun my|pre-init()
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0
        file-name-handler-alist nil
        ))
(defun my|post-init ()
  (setq gc-cons-threshold 20000000
        gc-cons-percentage 0.1
        file-name-handler-alist default-file-name-handler-alist)
  ;; GC automatically while unfocusing the frame
  ;; `focus-out-hook' is obsolete since 27.1
  ;; (if (boundp 'after-focus-change-function)
  ;;     (add-function :after after-focus-change-function
  ;;                   (lambda ()
  ;;                     (unless (frame-focus-state)
  ;;                       (garbage-collect))))
  ;;   (add-hook 'focus-out-hook 'garbage-collect))
  )
(add-hook 'before-init-hook #'my|pre-init)
(add-hook 'emacs-startup-hook #'my|post-init)

(defvar my-init-time 'nil)
(defun my-display-benchmark()
  (message "Loaded %s packages in %.03fs"
           (length package-activated-list)
           (or my-init-time
               (setq my-init-time (float-time (time-subtract (current-time) before-init-time)))
               )))
(add-hook 'emacs-startup-hook #'my-display-benchmark)

;; Emacs配置文件内容写到下面.
;;------------------------------------------------------------
;; enverioment configuration.
;; (setenv "GTAGSCONF" "~/.globalrc")

;;关闭出错时的提示声
(setq visible-bell t)

;; (if (display-graphic-p)
;;     (progn
;;       ;;设置窗口位置为屏库左上角(0,0)
;;       (set-frame-position (selected-frame) 20 10)
;;       ;;设置宽和高
;;       (set-frame-width (selected-frame) 60)
;;       (set-frame-height (selected-frame) 15)
;;       ;; 最大化窗口
;;       ;; (setq initial-frame-alist (quote ((fullscreen . maximized))))
;;       ))
;; 支持字体缓存
(setq inhibit-compacting-font-caches t)

;; font set
;; lihao: 发现现在的emacs对中文支持非常好，配置如下内容反而会导致问题，默认配置没问题;
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (setq-default pathname-coding-system 'utf-8)
;; (setq default-process-coding-system '(utf-8 . utf-8))
;; (setq file-name-coding-system 'utf-8)

;; buffer新建和读取都默认是utf-8
;; (prefer-coding-system 'utf-8)

;; Setting English Font
;; (set-face-attribute 'default nil :font "Fira Code 16")
;; (set-face-attribute 'default nil :font "Fira Mono 16")
;; Yahei Consolas 字体自带中文，所以可以不用设定中文
(set-face-attribute 'default nil :font "Sarasa Mono SC 18")
;; (set-face-attribute 'default nil :font "YaHei Consolas Hybrid 18")
;; (set-face-attribute 'default nil :font "Courier New 16")

;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "微软雅黑" :size 26)))  ;; 微软雅黑，24

;;-----------------------------------------------------------------
(setq dracula-alternate-mode-line-and-minibuffer t)
(load-theme 'dracula t)
;; (load-theme 'wombat t)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'monokai-pro t)

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

(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(setq sort-fold-case t)
(setq search-invisible t)

;; set scratch buffer message to nil.
(setq initial-scratch-message nil)

;; unique buffer name.
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same

;;------------------------------------------
;; backup policies
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 3)
(setq kept-new-versions 10)
(setq dired-kept-versions 2)
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
(setq woman-manpath (list "d:/Unix/man/" "d:/Unix/share/man/" "c:/cygwin64/usr/man/" "c:/cygwin64/usr/share/man/"))
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
(setq recentf-max-saved-items 100
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
			;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
			))
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

;;----------------------------------------------------------------
(require 'package)
;; (setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")))
;; (add-to-list 'package-archives '("melpa_stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")))
                         ;; ("gnu"   . "http://elpa.emacs-china.org/gnu/")))
(package-initialize)
(message "package initialize.")

(add-to-list 'load-path "~/.emacs.d/lisp/")
;; install all the sub-directories in the beginnging of load-path.
(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "~/.emacs.d/lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path)

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

(setq evil-normal-state-cursor '("#E02C6D" box))
(setq evil-insert-state-cursor '("gold" bar))
(setq evil-emacs-state-cursor '("white" box))

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
;; (require 'helm-config)
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
(require 'fold-dwim)

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

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :config (setq company-box-doc-enable nil))

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "M-n") 'company-other-backend)

(setq company-clang-arguments '(
                                "--target=i686-w64-mingw32"
                                ;; "-IC:/mingw-w64/i686-8.1.0/mingw32/i686-w64-mingw32/include/"
				))

;; company-tabnine
(require 'company-tabnine)
;; `:separate`  使得不同 backend 分开排序
(add-to-list 'company-backends #'company-tabnine)

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

(eval-after-load 'company
  '(progn
     (company-ctags-auto-setup)
     (setq company-ctags-support-etag t)))

;;}}}

;;------------------------------------------------------------
;;{{{ chinese calendar.
(require 'cal-china-x)
(setq calendar-latitude +34.16)
(setq calendar-longitude +108.54)
(setq calendar-location-name "西安")

(setq cal-china-x-important-holidays '(
				       (holiday-fixed 3 13 "生日")
				       (holiday-fixed 7 27 "老婆生日")
				       ))
(setq cal-china-x-general-holidays '(
			       ;;公历节日
			       (holiday-fixed 1 1 "元旦")
			       (holiday-fixed 2 14 "情人节")
			       (holiday-fixed 3 8 "妇女节")
			       (holiday-fixed 4 1 "愚人节")
			       (holiday-fixed 5 1 "劳动节")
			       (holiday-fixed 5 4 "青年节")
			       (holiday-float 5 0 2 "母亲节")
			       (holiday-fixed 6 1 "儿童节")
			       (holiday-float 6 0 3 "父亲节")
			       (holiday-fixed 7 1 "建党节")
			       (holiday-fixed 8 1 "建军节")
			       (holiday-fixed 9 10 "教师节")
			       (holiday-fixed 10 1 "国庆节")
			       (holiday-fixed 12 25 "圣诞节")
			       ;; 农历节日
			       (holiday-lunar 12 30 "除夕" 0)
			       (holiday-lunar 1 1 "春节" 0)
			       (holiday-lunar 1 15 "元宵节" 0)
			       (holiday-solar-term "清明" "清明节")
			       (holiday-lunar 5 5 "端午节" 0)
			       (holiday-lunar 7 7 "七夕节" 0)
			       (holiday-lunar 8 15 "中秋节" 0)
			       (holiday-lunar 9 9 "重阳节" 0)
			       ))
(setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays))
;; 设置Calendar的显示
(setq calendar-remove-frame-by-deleting t)
;; (setq mark-diary-entries-in-calendar t)       ; 标记有记录的日子
(setq calendar-mark-holidays-flag t)          ; 标记节假日
;; (setq calendar-mark-diary-entries-flag t)     ; 让calendar自动标记出记有待办事项的日期
(setq calendar-week-start-day 0) ; 设置星期一为每周的第一天，否则星期数有些对不上

;;除去基督徒的节日、希伯来人的节日和伊斯兰教的节日。
(setq christian-holidays nil
      hebrew-holidays nil
      islamic-holidays nil
      solar-holidays nil
      bahai-holidays nil)
;; 设置颜色
(set-face-attribute 'calendar-weekend-header nil
                    :foreground "green")
(set-face-attribute 'calendar-today nil
		    :box '(:line-width 1 :color "green")
		    :background "DarkGreen"
		    :foreground "yellow")
(set-face-attribute 'cal-china-x-general-holiday-face nil
		    :background "SkyBlue"
		    :foreground "black")
(set-face-attribute 'diary nil
		    :background "yellow"
		    :foreground "black")

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; 保存日记的文件
(setq diary-file "~/diary")
;; appointment
(setq appt-issue-message t)
;; 在mode-line上倒计时
(setq appt-display-mode-line t)

;; (setq diary-date-forms '((year "/" month "/" day "[^/0-9]"))
;;       calendar-date-display-form '(year "/" month "/" day)
;;       calendar-time-display-form
;;       '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

;; add ISO week number.
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (if (eq 1 calendar-week-start-day) (calendar-absolute-from-gregorian (list month day year))
                    (calendar-absolute-from-gregorian (list month (+ 1 day) year)))
                  )))
        'font-lock-face 'calendar-iso-week-face))
(copy-face 'font-lock-keyword-face 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
		    :foreground "yellow" :height 1.0)
(setq calendar-intermonth-header
      (propertize "Wk"
                  'font-lock-face 'calendar-iso-week-header-face))
(set-face-attribute 'calendar-iso-week-face nil
		    :height 1.0 :foreground "salmon")

;;------------------------------------------------------------
;; 计算伏天和数九
;;------------------------------------------------------------
(defconst cal-china-x-nine-characters  ; 数九天 array
  ["一九" "二九" "三九" "四九" "五九" "六九" "七九" "八九" "九九"])

(defun cal-china-x-winter-solstice-date (date)  ; 计算冬至日期(1-11月为去年，12月为今年)
  "Return winter solstice(冬至) date in Gregorian form.

If MONTH = 12, return current year's date
Else return last year's date"
  (let* ((cyear (if (= (calendar-extract-month date) 12)
                    (calendar-extract-year date)
                  (1- (calendar-extract-year date)))))
    (car (rassoc '"冬至" (cal-china-x-solar-term-alist-new cyear)))))

(defun winter-solstice-day-diff (date)  ; 计算与指定冬至的天数差
  (cal-china-x-days-diff date (cal-china-x-winter-solstice-date date)))

(defun cal-china-x-get-several-nines-string (date)  ; 生成数九天的 string
  (let ((daygap (winter-solstice-day-diff date)))
    (if (or (< daygap 0) (> daygap 80))
        ""
      (concat (aref cal-china-x-nine-characters (/ daygap 9))
              "("
              (number-to-string (1+ (% daygap 9)))
              ")"
              ))))

(defun cal-china-x-solar-term-date (date solar-term)
  "Return solar-term date in Gregorian form."
  (let* ((cyear (calendar-extract-year date)))
    (car (rassoc solar-term (cal-china-x-solar-term-alist-new cyear)))))

(defun cal-china-x-chinese-day-celestial-stem-number (date)
  "String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian date)))
    (% (+ a-date 15) 10)))

(defun cal-china-x-solar-term-celestical-stem (date solar-term)
  (cal-china-x-chinese-day-celestial-stem-number
   (cal-china-x-solar-term-date date solar-term)))

(defun cal-china-x-day-diff-from-solar-term (date solar-term) ; 庚日
  (let ((ss-stem (- 7 (cal-china-x-solar-term-celestical-stem date solar-term))))
    (if (< ss-stem 0) (+ ss-stem 10)
      ss-stem)))

(defun cal-china-x-chufu-date (date)
  (let* ((ss-date (cal-china-x-solar-term-date date "夏至"))
         (ss-year (calendar-extract-year ss-date))
         (ss-day (calendar-extract-day ss-date))
         (day-diff (+ 20 (cal-china-x-day-diff-from-solar-term date "夏至")))
         (chufu-day (- day-diff (- 30 ss-day))))
    (list 7 chufu-day ss-year)))

(defun cal-china-x-zhongfu-date (date)
  (list 7 (+ (calendar-extract-day (cal-china-x-chufu-date date)) 10)
        (calendar-extract-year date)))

(defun cal-china-x-mofu-date (date)
  (let* ((ss-date (cal-china-x-solar-term-date date "立秋"))
         (ss-year (calendar-extract-year ss-date))
         (ss-day (calendar-extract-day ss-date))
         (day-diff (cal-china-x-day-diff-from-solar-term date "立秋"))
         (mofu-day (+ day-diff ss-day)))
    (list 8 mofu-day ss-year)))

(defun cal-china-x-get-futian-string (date)
  (let* ((chufu (cal-china-x-chufu-date date))
         (zhongfu (cal-china-x-zhongfu-date date))
         (mofu (cal-china-x-mofu-date date))
         (chufu-gap (cal-china-x-days-diff date chufu))
         (zhongfu-gap (cal-china-x-days-diff date zhongfu))
         (mofu-gap (cal-china-x-days-diff date mofu))
         )
    (if (or (< chufu-gap 0) (> mofu-gap 9))
        ""
      (if (and (>= chufu-gap 0) (< zhongfu-gap 0))
          (concat "初伏("
                  (number-to-string (1+ chufu-gap))
                  ")")
        (if (and (>= zhongfu-gap 0) (< mofu-gap 0))
            (concat "中伏("
                    (number-to-string (1+ zhongfu-gap))
                    ")")
          (concat "末伏("
                  (number-to-string (1+ mofu-gap))
                  ")"))))))

(defun calendar-fu-jiu-date ()
  "Day of Futian and JiuTian for date under cursor."
  (interactive)
  (let ((date (calendar-cursor-to-date)))
    (message "%s: %s%s"
             (calendar-date-string date t t)
             (cal-china-x-get-several-nines-string date)
             (cal-china-x-get-futian-string date)
             )))

(defun calendar-get-fu-jiu-string (date)
  "Day of Futian and JiuTian for date under cursor."
  (format "%s%s"
          (cal-china-x-get-several-nines-string date)
          (cal-china-x-get-futian-string date)
          ))

;;------------------------------------------------------------

(message "chinese calendar loaded.")
;;}}}

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
 'counsel-find-file '(("d" delete-file "delete")))

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
  :bind (("M-]" . counsel-etags-find-tag-at-point))
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
;;               (propertize cmd 'face font-lock-doc-face)))
;; 有乱码，改编码没有用。。。
;; (propertize (encode-coding-string cmd locale-coding-system) 'face font-lock-doc-face)))

;; (advice-add 'counsel--async-command :before
;;             #'counsel-before-counsel--async-command)

(require 'counsel-fd)

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
(window-numbering-mode)

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

(with-eval-after-load 'nov
  (defun novel-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "YaHei Consolas Hybrid 16")   ;; FiraCode NF 16
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
    (add-hook 'nov-mode-hook 'novel-buffer-face-mode-variable))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; (setq nov-text-width 80)
;; set it to t to inhibit text filling
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)

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

(use-package treemacs-projectile
  :after treemacs projectile
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
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; (setq neo-window-width 25)
;; (setq neo-autorefresh t)

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
(require 'sdcv-mode)

(with-eval-after-load 'sdcv-mode
  (defun sdcv-buffer-face-mode-variable ()
    (interactive)
    (set-buffer-file-coding-system 'utf-8)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 16")   ;; FiraCode NF 16
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
    (add-hook 'sdcv-mode-hook 'sdcv-buffer-face-mode-variable))

(setq sdcv-dictionary-data-dir "d:/unix/dic/")   ;; set local sdcv dict to search word

;;------------------------------------------------------------
(require 'keep-buffers)

;; (require 'swbuff)
;; (setq swbuff-exclude-buffer-regexps '("^ .*" "^\\*.*\\*" "TAGS$"))
;; (setq swbuff-separator "|")
;; (setq swbuff-clear-delay 1)
;; ;; (global-set-key (kbd "<C-tab>") 'swbuff-switch-to-next-buffer)
;; ;; (global-set-key (kbd "<C-S-tab>") 'swbuff-switch-to-previous-buffer)
;; (global-set-key (kbd "<C-right>") 'swbuff-switch-to-next-buffer)
;; (global-set-key (kbd "<C-left>") 'swbuff-switch-to-previous-buffer)

(require 'hexview-mode)

(require 'scroll-all+)
(add-hook 'ediff-startup-hook 'scroll-all-mode)
(add-hook 'ediff-quit-hook
          '(lambda ()
             (scroll-all-mode -1)))

;;------------------------------------------------------------
;; company-english-helper
;; https://github.com/manateelazycat/company-english-helper
(require 'company-english-helper)
;; insert-translated-name-insert
;; https://github.com/manateelazycat/insert-translated-name
(require 'insert-translated-name)
(setq insert-translated-name-translate-engine "google")

;;------------------------------------------------------------
;; color-rg
;; https://github.com/manateelazycat/color-rg
(require 'color-rg)

;; https://github.com/manateelazycat/thing-edit
(require 'thing-edit)

;; https://github.com/junegunn/fzf-bin/releases

;;}}}

;;------------------------------------------------------------

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
                    ;; (push '(company-capf :with company-tabnine :separate) company-backends)
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

;;----------------------------------------------------------------
;;{{{ face reconfigure
;; (set-face-attribute 'font-lock-comment-face nil
;;                     :foreground "grey50")   ;; #292e34
(set-face-attribute 'show-paren-match nil
                    :foreground "green"
                    :bold t
                    :underline t)
(set-face-attribute 'show-paren-mismatch nil
                    :foreground "red"
                    :bold t
                    :underline t)
(set-face-attribute 'evil-ex-lazy-highlight nil
                    :background "grey50")
;; virtual files color for ivy-switch-buffer.
;; (set-face-attribute 'ivy-virtual nil
;;                     :foreground "grey50")

;;}}}
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
;;{{{ mode-line-format.
(setq-default
 mode-line-format
 (list
   " ["
   '(:eval (when (window-numbering-get-number)
             (propertize (int-to-string (window-numbering-get-number)) 'face 'font-lock-keyword-face
		       'help-echo "window number")))
   "]"
   ;; evil state
   '(:eval
     (propertize (evil-generate-mode-line-tag evil-state) 'face
		 (cond ((evil-normal-state-p) 'font-lock-function-name-face)
		       ((evil-insert-state-p) 'font-lock-keyword-face)
		       ((evil-visual-state-p) '(:foreground "orange" :weight bold))
                       ((evil-motion-state-p) '(:foreground "yellow" :weight bold))
		       ((evil-emacs-state-p) '(:foreground "green" :weight bold))
		       (t 'font-lock-constant-face))))
   mode-line-front-space
   mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
   mode-line-frame-identification mode-line-buffer-identification
   ;; '(:eval (when (projectile-project-p)
   ;;          (propertize (format " P[%s] " (projectile-project-name))
   ;;                      'face 'font-lock-variable-name-face)))
   " "
   mode-line-position
   "("
   '(:eval (concat (format "%s" (int-to-string (count-lines (point-min) (point-max))))))
   ") "
   ;; the current major mode for the buffer.
   "["
   '(:eval (propertize (format "%s" major-mode)  ;; "%m"
		       'face nil
		       'help-echo (format "Coding: %s" buffer-file-coding-system)))
   ;; list of minor modes
   ;; minor-mode-alist
   "] "
   ;; mode-line-modes
   ;; mode-line-misc-info
   '(:eval (when vc-mode (concat "[" (propertize (format "%s" vc-mode) 'face 'font-lock-variable-name-face) "] " ))) ; vc-mode vc-mode
   ;; global-mode-string, org-timer-set-timer in org-mode need this
   '(:eval (propertize "%M " 'face nil))
   "[" ;; insert vs overwrite mode, input-method in a tooltip
   '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
		       'face 'font-lock-preprocessor-face
		       'help-echo (concat "Buffer is in "
					  (if overwrite-mode "overwrite" "insert") " mode")))
   ;; was this buffer modified since the last save?
   '(:eval (when (buffer-modified-p)
	     (concat "/"  (propertize "MD"
				      'face '((:foreground "red" :weight bold))
				      'help-echo "Buffer has been modified"))))
   ;; is this buffer read-only?
   '(:eval (when buffer-read-only
	     (concat "/"  (propertize "RO"
				      'face 'font-lock-type-face
				      'help-echo "Buffer is read-only"))))
   "] "
   "["
   '(:eval (propertize
	   (concat (pcase (coding-system-eol-type buffer-file-coding-system)
		     (0 "LF ")
		     (1 "CRLF ")
		     (2 "CR "))
		   (let ((sys (coding-system-plist buffer-file-coding-system)))
		     (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
			 "UTF-8"
		       (upcase (symbol-name (plist-get sys :name)))))
		   )))
   "] "
   ;;"%-" ;; fill with '-'
   mode-line-end-spaces))

;; change mode-line color by evil state
;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                    (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let* ((color (cond ((minibufferp) default-color)
;;                                    ;; ((evil-insert-state-p) '("yellow" . "#ffffff"))
;;                                   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                                   ;; ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                                   (t default-color))))
;;                 (set-face-background 'mode-line (car color))
;;                 (set-face-foreground 'mode-line (cdr color))))))

(set-face-attribute 'mode-line-inactive nil
                    :background "#34475a"    ;; #44475a
                    :box '(:color "#5d4d7a" :line-width 1))   ;; #5d4d7a

;;}}}
(require 'airline-themes)
;; (load-theme 'airline-powerlineish t)
(load-theme 'airline-fork-railscasts t)

;; Hide Evil and buffer state on inactive buffers.
;; Valid Values: t (hidden), nil (shown)
(setq airline-hide-state-on-inactive-buffers t)

;; "Hide eyebrowse indicator on inactive buffers.
;; Valid Values: t (hidden), nil (shown)"
(setq airline-hide-eyebrowse-on-inactive-buffers t)

;; Hide vc branch on inactive buffers:
;; Valid Values: t (hidden), nil (shown)
(setq airline-hide-vc-branch-on-inactive-buffers nil)

;; Set eshell prompt colors to match the airline theme.
;; Valid Values: t (enabled), nil (disabled)
(setq airline-eshell-colors t)

;; Set helm colors to match the airline theme.
;; Valid Values: t (enabled), nil (disabled)
(setq airline-helm-colors t)

;; Set the cursor color based on the current evil state.
;; Valid Values: t (enabled), nil (disabled)
(setq airline-cursor-colors t)

;; Display the currend directory along with the filename.
;; Valid Values: 'airline-directory-full
;;               'airline-directory-shortened
;;               nil (disabled)
(setq airline-display-directory nil)

;; Max directory length to display when using 'airline-directory-shortened
(setq airline-shortened-directory-length 30)

;; Unicode character choices
(setq airline-utf-glyph-separator-left #xe0b0
      airline-utf-glyph-separator-right #xe0b2
      airline-utf-glyph-subseparator-left #xe0b1
      airline-utf-glyph-subseparator-right #xe0b3
      airline-utf-glyph-branch #xe0a0
      airline-utf-glyph-readonly #xe0a2
      airline-utf-glyph-linenumber #x2630)

;; You may also wish to force powerline to use utf8 character separators
(setq powerline-default-separator 'utf-8)
(setq powerline-utf-8-separator-left  #xe0b0
      powerline-utf-8-separator-right #xe0b2)

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

(global-set-key [(meta return)] 'my-add-new-line)

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
(global-set-key [(f5)] 'loop-alpha)
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

(define-key isearch-mode-map (kbd "M-s o") 'occur-from-isearch)
(define-key evil-motion-state-map (kbd "M-s o") 'occur-from-evil-ex)

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

(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
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
_a_ce    wo_m_an     _h_elpful   _t_ranslation   i_m_enu
s_d_cv   _s_wiper    _f_grep    _e_nglish   r_g_ _n_treemacs
_l_fd    _y_oudao    _c_apture
  "
  ("a" ace-pinyin-dwim)
  ("d" sdcv-search)
  ("y" youdao-dictionary-search-at-point)
  ("s" swiper-thing-at-point)
  ("f" counsel-grep-or-swiper)
  ("e" toggle-company-english-helper)
  ("t" insert-translated-name-insert-original-translation)
  ("g" color-rg-search-input)
  ("l" fd-dired)
  ("n" treemacs)
  ("m" counsel-imenu)
  ("c" org-capture)
  ("w" woman-helpful)
  ("h" helpful-at-point)
  )

(defhydra hydra-fold (:color blue :hint nil)
  "
_f_old-toggle  fold-dwim-_h_ide-all  fold-dwim-_s_how-all
hide-_l_ines-matching   hide-lines-_n_ot-matching   hide-lines-show-_a_ll
  "
  ("l" hide-lines-matching)
  ("n" hide-lines-not-matching)
  ("a" hide-lines-show-all)
  ("f" fold-dwim-toggle)
  ("h" fold-dwim-hide-all)
  ("s" fold-dwim-show-all))

(require 'linum-relative)
;; (require 'evil-pinyin)
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
_o_ outline-minor-mode: %`outline-minor-mode
_h_ hs-minor-mode:      %`hs-minor-mode
_f_ folding-mode:       %`folding-mode
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
  ("o" outline-minor-mode nil)
  ("h" hs-minor-mode nil)
  ("f" folding-mode nil)
  ("j" visual-line-mode nil)
  ("c" cua-mode nil)
  ("y" evil-pinyin-mode nil)
  ("q" nil "quit"))

;; Recommended binding:
(defhydra hydra-all (global-map "C-c" :color blue :hint nil)
  ""
  ("SPC" hydra-jump/body)
  ("tt" hydra-toggle/body)
  ("ff" hydra-fold/body)
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
    "ac"  'ace-pinyin-dwim
    "al"  'ace-jump-line-mode
    "ao"  'ace-window
    "aw"  'ace-jump-word-mode
    "as"  'ace-swap-window
    "bb"  '((lambda () (interactive) (switch-to-buffer nil)) :which-key "prev-buffer")
    "cc"  'evilnc-comment-or-uncomment-lines
    "cp"  'evilnc-comment-or-uncomment-paragraph
    "ct"  'evilnc-comment-or-uncomment-html-tag
    "dw"  'thing-cut-word
    "dW"  'thing-cut-sexp
    "ds"  'thing-cut-symbol
    "dl"  'thing-cut-line
    "df"  'thing-cut-filename
    "du"  'thing-cut-url
    "dm"  'thing-cut-email
    "dS"  'thing-cut-sentence
    "dp"  'thing-cut-parentheses
    "dj"  'dired-jump
    "bf"  'beginning-of-defun
    "ef"  'end-of-defun
    "ss"  'swiper-thing-at-point
    "rg"  'color-rg-search-input
    "fd"  'counsel-fd-file-jump
    "fl"  'counsel-locate
    "ul"  'browse-url
    "yw"  'thing-copy-word
    "yW"  'thing-copy-sexp
    "ys"  'thing-copy-symbol
    "yl"  'thing-copy-line
    "yf"  'thing-copy-filename
    "yu"  'thing-copy-url
    "ym"  'thing-copy-email
    "yS"  'thing-copy-sentence
    "yp"  'thing-copy-parentheses
    "yP"  'thing-copy-paragraph
    "yt"  'select-total-part
    ;; hydra binding
    "<SPC>" 'hydra-jump/body
    "tt" 'hydra-toggle/body
    "fo" 'hydra-fold/body
   )

  (general-create-definer my-leader-def
    :prefix "C-c")

  ;; ** Global Keybindings
  (my-leader-def
    ",,"  'my-tmp-mark
    ".."  'my-tmp-back
    ",."  'my-tmp-back-original
    "ac"  'ace-pinyin-dwim
    "al"  'ace-jump-line-mode
    "ao"  'ace-window
    "aw"  'ace-jump-word-mode
    "as"  'ace-swap-window
    "bb"  '((lambda () (interactive) (switch-to-buffer nil)) :which-key "prev-buffer")
    "cc"  'evilnc-comment-or-uncomment-lines
    "cp"  'evilnc-comment-or-uncomment-paragraph
    "ct"  'evilnc-comment-or-uncomment-html-tag
    "dw"  'thing-cut-word
    "dW"  'thing-cut-sexp
    "ds"  'thing-cut-symbol
    "dl"  'thing-cut-line
    "df"  'thing-cut-filename
    "du"  'thing-cut-url
    "dm"  'thing-cut-email
    "dS"  'thing-cut-sentence
    "dp"  'thing-cut-parentheses
    "dj"  'dired-jump
    "bf"  'beginning-of-defun
    "ef"  'end-of-defun
    "ss"  'swiper-thing-at-point
    "rg"  'color-rg-search-input
    "fd"  'counsel-fd-file-jump
    "fl"  'counsel-locate
    "ul"  'browse-url
    "yw"  'thing-copy-word
    "yW"  'thing-copy-sexp
    "ys"  'thing-copy-symbol
    "yl"  'thing-copy-line
    "yf"  'thing-copy-filename
    "yu"  'thing-copy-url
    "ym"  'thing-copy-email
    "yS"  'thing-copy-sentence
    "yp"  'thing-copy-parentheses
    "yP"  'thing-copy-paragraph
    "yt"  'select-total-part
    ;; hydra binding
    "<SPC>" 'hydra-jump/body
    "tt" 'hydra-toggle/body
    "fo" 'hydra-fold/body
   )
  )

(message "general key binding end.")

;;------------------------------------------------------------
;; key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (require 'gnu-elpa-keyring-update)

(global-set-key [(f3)] 'color-rg-search-input)
(global-set-key [(f4)] 'my-tmp-back)
(global-set-key [(C-f4)] 'my-tmp-mark)
(global-set-key [(M-f4)] 'my-tmp-back-original)

;;------------------------------------------------------------
;;{{{ desktop.

;;如果你想保存上次打开的文件记录，那么可以使用 desktop。这是 Emacs 自带的。你只需要加入以上设置，然
;;后 M-x desktop-save。以后 Emacs 启动时就会打开你上次离开时的所有 buffer.
;;M-x desktop-clear
;; Restore the "desktop" - do this as late as possible!!
(if first-time
    (progn
      (load "desktop")
      ;; save a bunch of variables to the desktop file
      ;; for lists specify the len of the maximal saved data also
      (setq desktop-globals-to-save
	    (append '((extended-command-history . 10)
		      (file-name-history        . 30)
		      (ido-last-directory-list  . 10)
                      (ido-work-directory-list  . 10)
                      (ido-work-file-list       . 10)
                      (grep-history             . 3)
                      (compile-history          . 3)
                      (minibuffer-history       . 5)
                      (query-replace-history    . 5)
                      (read-expression-history  . 5)
                      (regexp-history           . 5)
                      (regexp-search-ring       . 3)
                      (search-ring              . 3)
                      (comint-input-ring        . 5)
                      (shell-command-history    . 5)
                      desktop-missing-file-warning
                      tags-file-name
                      register-alist)))
      (desktop-save-mode)
      (message "reading desktop done.")))

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
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(calendar-mode-line-format
   '(#("<" 0 1
       (keymap
        (keymap
         (mode-line keymap
                    (mouse-1 . calendar-scroll-right)))
        mouse-face mode-line-highlight help-echo "mouse-1: previous month"))
     "Calendar"
     (cal-china-x-get-holiday date)
     (concat " "
             (calendar-date-string date t)
             (format " 第%d周"
                     (funcall
                      (if cal-china-x-custom-week-start-date 'cal-china-x-custom-week-of-date 'cal-china-x-week-of-date)
                      date)))
     (cal-china-x-chinese-date-string date)
     (calendar-get-fu-jiu-string date)
     #(">" 0 1
       (keymap
        (keymap
         (mode-line keymap
                    (mouse-1 . calendar-scroll-left)))
        mouse-face mode-line-highlight help-echo "mouse-1: next month"))))
 '(column-number-mode t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(inhibit-startup-screen t)
 '(initial-major-mode 'org-mode)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(counsel-fd fd-dired lsp-pyright ivy-xref lsp-ivy lsp-mode spinner powerline treemacs-icons-dired treemacs-projectile treemacs-evil treemacs dracula-theme org-download centered-cursor-mode general evil-anzu youdao-dictionary monokai-pro-theme evil-pinyin format-all ahk-mode eshell-z eshell-up all-the-icons-ivy counsel-projectile all-the-icons-ivy-rich srcery-theme org-superstar all-the-icons-ibuffer all-the-icons imenu-list nov powershell spacemacs-theme smart-compile helpful wgrep modern-cpp-font-lock company-ctags counsel-etags ace-window quickrun posframe js2-mode evil-textobj-anyblock vimrc-mode dired-single web-mode evil-nerd-commenter hydra evil-surround which-key htmlize hide-lines linum-relative rainbow-mode w32-browser json-mode yaml-mode evil-visualstar anzu ace-pinyin markdown-mode fold-dwim folding avy evil-matchit window-numbering use-package rainbow-delimiters pyim projectile counsel semi swiper ace-jump-mode smex expand-region cal-china-x bm company-tabnine company w3m helm evil))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
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

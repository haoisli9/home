;; -*- Emacs-Lisp -*-
;; org mode settings.
;; lihao

;;{{{ org-mode-setting
(use-package org
  :init
  (setq org-src-fontify-natively t)
  :defer t
  :config
  (setq org-agenda-files '("~/../lihao/org/agenda/task.org"
                           "~/../lihao/org/agenda/done.org"
                           "~/../lihao/org/agenda/diary.org"
                           ))
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-include-all-todo nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-custom-commands nil)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-to-appt t)

  ;; Agenda styling
  (setq 
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (600 800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "▶ now ────────────────────────────────────")

  (setq org-agenda-include-diary t)
  (setq diary-file "~/diary")

  ;; Various preferences
  (setq org-log-done 'time
        ;; 文本内语法高亮
        org-src-fontify-natively t
        ;; Edit settings
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-support-shift-select t
        )
  ;; Org styling, hide markup etc.
  (setq org-startup-folded 'show2levels
        org-pretty-entities t
        org-startup-indented t
        ;; 隐藏标题栏里的一堆星号
        org-hide-leading-stars t
        ;; 显示隐藏标记
        org-ellipsis "  "   ;; "    ⭍ ..."
        ;; 直接显示语法样式
        org-hide-emphasis-markers t
        org-image-actual-width '(800)
        org-startup-with-inline-images t
        )
  
  ;; 上下标，如果去掉，使用^和_来解释上下标，否则使用{}来触发；
  ;; a_{1} 下标
  ;; a_1   显示成 a_1
  ;; a^{1} 上标
  ;; a^1   显示成 a^1
  ;; 一个是用于 export 的，一个是用于本地显示的，最好配置成一样保证一致性
  (setq org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{})

  ;; 把<>符号从分隔符（delimiter）改成标点（punctuation），避免误匹配
  ;; 参考Emacs Lisp: Syntax Table [http://xahlee.info/emacs/emacs/elisp_syntax_table.html]
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table)
  
  (setq
   ;; Hide html built-in style and script.
   org-html-htmlize-output-type 'inline-css ;; 保留代码块高亮
   org-html-doctype "html5"
   org-html-validation-link nil
   org-export-with-toc t
   org-export-headline-levels 3
   org-html-postamble nil
   ;; turn off default style
   ;; org-html-head "<link rel='stylesheet' type='text/css' href='css/org.css'/>"
   org-html-head-include-default-style nil
   org-html-head-include-scripts nil)

   ;;------------------------------------------------------------
   ;; (setq org-html-checkbox-type 'unicode)

   ;; 显示数字章节号
   ;; (setq org-startup-numerated t)

   (defun org-mode-my-init ()
     (define-key org-mode-map (kbd "TAB") 'org-cycle)
     (evil-define-key '(normal emacs) org-mode-map (kbd "TAB") 'org-cycle)
     ;; use unicode font "FireCode NF"
     (setq-local prettify-symbols-alist
                  '(("lambda"  . ?λ)
                    ;; (":PROPERTIES:" . ?)
                    ;; (":ID:" . ?)
                    ;; (":END:" . ?)
                    ;; ("#+TITLE:" . ?)
                    ;; ("#+AUTHOR" . ?)
                    ;; ("#+BEGIN_QUOTE" . ?)
                    ;; ("#+END_QUOTE" . ?)
                    ;; ("#+RESULTS:" . ?)
                    ("[ ]" . ?)
                    ("[-]" . ?)
                    ("[X]" . ?)
                    ("[#A]" . ?🅐)
                    ("[#B]" . ?🅑)
                    ("[#C]" . ?🅒)))
     (prettify-symbols-mode t)
     )
   (add-hook 'org-mode-hook 'org-mode-my-init)
)

;;----------------------------------------------------------------------------
;; jditaa fix chinese problem but is' version is 0.9
(setq org-ditaa-jar-path "~/.emacs.d/plugins/jditaa.jar")
;; (setq org-ditaa-jar-path "~/.emacs.d/plugins/ditaa-0.11.0-standalone.jar")
;; in windows-system, file always encoded as gbk. so -Dfile utf-8 is not required.
(setq org-babel-default-header-args:ditaa
      '((:results . "file")
        (:exports . "results")
        (:java . "")))

(setq org-plantuml-jar-path "~/.emacs.d/plugins/plantuml-1.2022.6.jar")
; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")
(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; sdot - Graphviz DOT block
;; to use chinese, put fontname="Microsoft YaHei" or other font(SimHei/FangSong) near node/edge/label etc.
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")
(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cmdline -r\n"
  _ - \n
  "#+end_src\n")
(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; splantuml - PlantUML Source block
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png\n"
  _ - \n
  "#+end_src\n")
(define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

;;----------------------------------------------------------------------------
;; mermaid
(setq ob-mermaid-cli-path "c:/Users/lihao.BJ/AppData/Roaming/npm/mmdc.cmd")
(org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t)))

(defun org-export-docx ()
    "Export current buffer to docx file with the template.docx."
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (expand-file-name "template.docx" "d:/Users/lihao/org/publish/")))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name) docx-file template-file))
      (message "Convert finish: %s" docx-file)))

;;----------------------------------------------------------------------------
;;Sunrise and Sunset
;;日出而作, 日落而息
(defun diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

(setq org-agenda-format-date 'my/org-agenda-format-date-aligned)
(defun my/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
      This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (aref cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date))
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "(闰月)")))
         (cn-day-string (aref cal-china-x-day-name
                              (1- cn-day))))
    (format "%04d-%02d-%02d 周%s %s%s" year month
            day dayname cn-month-string cn-day-string)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "TODAY(n)" "WAIT(w@/!)" "|" "DONE(d)" "QUIT(q@/!)")))
(setq org-todo-keyword-faces
      '(
        ("QUIT"    .   (:background "white" :foreground "black" :weight bold))
        ("WAIT"    .   (:background "#FBF0CC" :foreground "black" :weight bold))
        ("TODO"    .   (:background "#CF9293" :foreground "black" :weight bold))
        ("TODAY"   .   (:background "pink" :foreground "black" :weight bold))
        ("DOING"   .   (:background "orange" :foreground "black" :weight bold))
        ("DONE"    .   (:background "grey50" :foreground "black" :weight bold))
        ))
(setq org-tag-alist '(("work" . ?w) ("home" . ?h) ("personal" . ?p)))

;; 任务完成自动更新上级任务状态
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-default-capture-file "~/../lihao/org/agenda/captures.org")
(setq org-agenda-diary-file "~/../lihao/org/agenda/diary.org")
(setq org-capture-templates
      '(("n" "Notes" entry (file+headline org-default-capture-file "Notes")
         "** TODO %?%i\n- Time: %T\n")
        ("o" "Others" entry (file+headline org-default-capture-file "Others")
         "** %?%i\n- Time: %T\n")
        ("j" "Journal" entry (file+olp+datetree org-agenda-diary-file "Diary")
         "** %?%i\n")))

;; refiles
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Refile settings
(defun my/org-buffer-files ()
  "Return list of opened orgmode buffer files"
  (mapcar (function buffer-file-name)
	  (org-agenda-files 'files)))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets '(
                           ;; (my/org-buffer-files :maxlevel . 1)
                           (nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 1)
                           ))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
;; 显示网络图片
(setq org-display-remote-inline-images 'cache)

;;------------------------------------------------------------
;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
;; (setq org-time-clocksum-format
;;       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; 优先级范围和默认任务的优先级
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?C)
(setq org-default-priority ?B)
;; 优先级醒目外观
(setq org-priority-faces
  '((?A . (:foreground "#FF5554" :weight bold))
    (?B . (:foreground "orange" :weight bold))
    (?C . (:foreground "#50F576" :weight bold))
    ))

;;------------------------------------------------------------
;; "◉" "○" "▷" "✸"
;; ● ◇ ✚ ✜ ☯ ◆ ♥ ♠ ♣ ♦ ☢ ❀ ✿ ◆ ◖ ▶
;; ► • ★ ▸ ▼ ◈ ⬧ ⬨ "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
;; A nice collection of unicode bullets:
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters

;; Enable org-modern-mode
(use-package org-modern
  :ensure t
  :after org
  :config
  (setq org-modern-todo-faces
        '(
          ("QUIT"    .   (:background "white" :foreground "black" :weight bold))
          ("WAIT"    .   (:background "#FBF0CC" :foreground "black" :weight bold))
          ("TODO"    .   (:background "#CF9293" :foreground "black" :weight bold))
          ("TODAY"   .   (:background "pink" :foreground "black" :weight bold))
          ("DOING"   .   (:background "orange" :foreground "black" :weight bold))
          ("DONE"    .   (:background "grey50" :foreground "black" :weight bold))
          ))
  (set-face-attribute 'org-modern-done nil
                      :background "gray50" :foreground "black" :bold t)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  :custom
  (org-modern-star ["◉""●""○""◆""◇"])
  (org-modern-list
        '((?+ . "◦")
          (?* . "•")
          (?- . "▸")
          ))
  (org-modern-block nil)
  (org-modern-table nil)
  (org-modern-priority nil)
  (org-modern-checkbox nil)
)

;;------------------------------------------------------------
;; 增加文件头
(defun insert-org-meta-info (dir)
  (interactive "fchoose a directory: ")
  (let (real-files)
    (dolist (file (directory-files dir))
      (unless (or (string= "." (substring file 0 1))
		          (string= "#" (substring file 0 1))
		          (string= "~" (substring file -1))))
	      (push file real-files))
    (dolist (filename real-files)
      (let* ((title (file-name-base filename))
	         (date (format-time-string "%F %T" (current-time)))
	         (meta-info (format "#+TITLE: %s\n#+DATE: %s\n#+LANGUAGE: zh-CN\n#+OPTIONS: toc:t H:4 html-postamble:nil\n#+HTML_HEAD: %s\n\n" title date "<link rel='stylesheet' type='text/css' href='css/org.css'/>"))
	         (file (concat dir filename)))
	    (with-temp-buffer
	      (insert-file-contents file)
	      (goto-char (point-min))
	      (insert meta-info)
	      (write-file file))))))

(defun insert-org-header-info (arg)
  (interactive "sTitle: ")
  (let* ((title (if (string= arg "") (buffer-name) arg))
         (date (format-time-string "%F %T" (current-time)))
         (meta-info (format "#+TITLE: %s\n#+DATE: %s\n#+LANGUAGE: zh-CN\n#+OPTIONS: toc:t H:4 html-postamble:nil\n#+HTML_HEAD: %s\n\n" title date "<link rel='stylesheet' type='text/css' href='css/org.css'/>"))
         (buffer (buffer-name)))
    (insert-buffer buffer)
    (goto-char (point-min))
    (insert meta-info)))

(defvar org-open-note-directory "d:/Users/lihao/org/note/")
(defun org-open-note-file (arg)
  (interactive "sTitle: ")
  (let* ((file (format "%s%s.org" org-open-note-directory (format-time-string "%F" (current-time))))
         (title (if (string= arg "") "" arg))
         (date (format-time-string "%F %T" (current-time)))
         (meta-info (format "#+TITLE: %s\n#+CREATED: %s\n#+LANGUAGE: zh-CN\n#+OPTIONS: toc:t H:4 html-postamble:nil\n#+HTML_HEAD: %s\n\n" title date "<link rel='stylesheet' type='text/css' href='css/org.css'/>")))
    (if (file-readable-p file)
        (find-file file)
      (progn 
        (find-file file)
        (goto-char (point-min))
        (insert meta-info)))
    ))

(use-package org-download
  :ensure t
  :after org
  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
  :bind (
         ("C-S-p" . org-download-clipboard))
  :config
  (setq-default org-download-image-dir "./images/")
  ;; Drag and drop to Dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-screenshot-method "convert clipboard: %s")
  )

;; self defined color
(eval-after-load 'org
  '(progn
     (set-face-attribute 'org-level-1 nil
                         :bold t
                         :foreground "#FF79C6"
                         :height 1.0)
     (set-face-attribute 'org-level-2 nil
                         :foreground "#D3B8FB"
                         :height 1.0)
     (set-face-attribute 'org-level-3 nil
                         :foreground "#B7DEF8"
                         :height 1.0)
     (set-face-attribute 'org-level-4 nil
                         :foreground "#BFEBE0"
                         :height 1.0)
     (set-face-attribute 'org-level-5 nil
                         :foreground "#f8f8f2"
                         :height 1.0)
;;      (set-face-attribute 'org-level-6 nil
;;                          :foreground "#ffb86c"
;;                          :height 1.0)
;;      (set-face-attribute 'org-level-7 nil
;;                          :foreground "#0189cc"
;;                          :height 1.0)
;;      (set-face-attribute 'org-level-8 nil
;;                          :foreground "#50fa7b"
;;                          :height 1.0)
     (set-face-attribute 'org-ellipsis nil
                         :foreground "#6272a4")
     (set-face-attribute 'org-agenda-current-time nil
                         :foreground "yellow")
     (set-face-attribute 'org-agenda-date-today nil
                         :foreground "green")
     (set-face-attribute 'org-agenda-date-weekend nil
                         :foreground "light blue")
     (set-face-attribute 'org-scheduled-previously nil
                         :foreground "light yellow")
     (set-face-attribute 'org-scheduled-today nil
                         :foreground "light green") 
     ))

;; 设置org使用独立的字体
;; (with-eval-after-load 'org
;;   (defun org-buffer-face-mode-variable ()
;;     (interactive)
;;     (make-face 'width-font-face)
;;     (set-face-attribute 'width-font-face nil :font "FiraCode NF 14")
;;     (setq buffer-face-mode-face 'width-font-face)
;;     (buffer-face-mode))
;;   (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (hl-line-mode)
;;             (face-remap-add-relative 'hl-line :box '(:color "deep pink" :line-width 2))
;;             (face-remap-add-relative 'variable-pitch '(:foreground "blue" :background "white"))))

(message "org-mode configuration loaded.")

(use-package inherit-org
  :config
  (require 'org)
  (with-eval-after-load 'info
    (add-hook 'Info-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'helpful
    (add-hook 'helpful-mode-hook 'inherit-org-mode))

  (with-eval-after-load 'w3m
    (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify) ;only one level is supported
    (add-hook 'w3m-fontify-after-hook 'inherit-org-mode))
  
  :custom
  (inherit-org-bullets-bullet-list '("◉""●""○""◆""◇"))
  )

(provide 'init-org)
;;}}}

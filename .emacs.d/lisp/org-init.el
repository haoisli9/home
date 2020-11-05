;; -*- Emacs-Lisp -*-
;; org mode settings.
;; lihao

;;{{{ org-mode-setting

;; Various preferences
(setq org-log-done 'time
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      ;; 文本内语法高亮
      org-src-fontify-natively t
      org-tags-column -80)

(setq org-agenda-files '("~/../org/task.org"
                         "~/../org/done.org"
                         "~/../org/captures.org"
                         ))
(setq org-agenda-use-time-grid t)
(setq org-agenda-include-all-todo nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-include-diary t)
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-agenda-custom-commands nil)
(setq org-agenda-default-appointment-duration 60)
(setq org-agenda-mouse-1-follows-link t)
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-to-appt t)

(setq org-agenda-include-diary t)
(setq org-agenda-diary-file "~/../org/diary.org")
(setq diary-file "~/diary")

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
      '((sequence "TODO(t)" "DOING(i!)" "TBD(p!)" "TODAY(n!)" "|" "DONE(d!)" "ABORT(q@/!)")))
(setq org-todo-keyword-faces
      '(
	("ABORT"   .   (:background "grey50" :foreground "red" :weight bold))
	("TBD"     .   (:foreground "gray" :weight bold))
	("TODO"    .   (:foreground "#CF9293" :weight bold))
	("TODAY"   .   (:foreground "pink" :weight bold))
	("DOING"   .   (:foreground "#FBF0CC" :weight bold))
	("DONE"    .   (:foreground "light green" :weight bold))
	))
(setq org-tag-alist '(("work" . ?w) ("home" . ?h) ("personal" . ?p)))

;; 优先级范围和默认任务的优先级
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?E)
(setq org-default-priority ?E)
;; 优先级醒目外观
(setq org-priority-faces
  '((?A . (:background "red" :foreground "white" :weight bold))
    (?B . (:background "DarkOrange" :foreground "white" :weight bold))
    (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
    (?D . (:background "DodgerBlue" :foreground "yellow" :weight bold))
    (?E . (:background "SkyBlue" :foreground "black" :weight bold))
    ))
(setq org-startup-indented t)

;; 任务完成自动更新上级任务状态
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-default-capture-file "~/../org/captures.org")

(setq org-capture-templates
      '(("n" "Note" entry (file+headline org-default-capture-file "Note")
         "** TODO %?%i\n   - Added: %T")
        ("r" "Reminder" entry (file+headline org-default-capture-file "Reminder")
         "** %T  %?%i\n")
        ("j" "Journal" entry (file+datetree org-agenda-diary-file "Journal")
         "** Entered on %U\n + %?%i\n")))

;; refiles
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Refile settings
(defun my/org-buffer-files ()
  "Return list of opened orgmode buffer files"
  (mapcar (function buffer-file-name)
	  (org-buffer-list 'files)))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 1)
                                 ;; (my/org-buffer-files :maxlevel . 1)
                                 (org-agenda-files :maxlevel . 1))))

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

;;------------------------------------------------------------
;; "◉" "○" "▷" "✸"
;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ✿ ◆ ◖ ▶
;; ► • ★ ▸
;; A nice collection of unicode bullets:
;; http://nadeausoftware.com/articles/2007/11/latency_friendly_customized_bullets_using_unicode_characters
(setq org-superstar-headline-bullets-list
      '(
        "◉"
        "●"
        "○"
        "•"
        ))
(setq org-superstar-item-bullet-alist
      '(
        (?- . ?▶)
        (?+ . ?▷)
        ;; (?+ . 9671)
        ))
(setq org-superstar-special-todo-items nil)
(setq org-ellipsis " ▼ ")

(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)
            (visual-line-mode -1)
            ;; (define-key org-mode-map (kbd "C-'") 'nil)
))

(use-package org
  :init
  (setq org-src-fontify-natively t)
  :config
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
   (setq org-html-checkbox-type 'unicode)
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

(defun org-insert-clipboard (&optional captionp)
  (interactive "P")
  (let* ((image-dir
          (if (not (buffer-file-name))
              (cond ((string-prefix-p "CAPTURE-[0-9]" (buffer-name))
                     (let ((buffer-name (replace-regexp-in-string "CAPTURE-[0-9-]*" "" (buffer-name))))
                       (concat (file-name-directory (buffer-file-name (get-file-buffer buffer-name))) "images")))
                    (t (yank) (error "")))
            "images"))
         (fname (concat (make-temp-name "image-") (format-time-string "%Y%m%d-%H%M%S")))
         (image-file (concat image-dir "/" fname ".png"))
         (exit-status
          (call-process "convert.exe" nil nil nil "clipboard:" image-file))
          ;; (shell-command (format "convert.exe clipboard: %s" image-file)))
         )
    (if (zerop exit-status)
        (progn
          (unless (file-exists-p image-dir) (make-directory image-dir))
          (if captionp
              (let ((rename (read-string "Filename to rename the temp images: ")))
                (rename-file image-file (concat image-dir "/" rename ".png") t)
                (insert (format "#+CAPTION: %s label:fig:%s\n" (read-string "Caption: ") rename))
                (kill-new (format "Fig. ref:fig:%s " rename)))
            (insert (format "[[file:%s]]" image-file))
            (org-display-inline-images)))
      (when captionp (user-error "No images in clipboard."))
      (yank))))

(use-package org-download
  :ensure t
  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
  :bind ("C-S-y" . org-insert-clipboard)
  :config
  (require 'org-download)
  ;; Drag and drop to Dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  ;; (setq org-download-screenshot-method "convert.exe clipboard: %s")
  )


(eval-after-load 'org
  '(progn
     (set-face-attribute 'org-level-1 nil
                         :foreground "#ff79c6"
                         :height 1.1)
     (set-face-attribute 'org-level-2 nil
                         :foreground "#8be9fd"
                         :height 1.0)
     (set-face-attribute 'org-level-3 nil
                         :foreground "#bd93f9"
                         :height 1.0)
     (set-face-attribute 'org-level-4 nil
                         :foreground "#f1fa8c"
                         :height 1.0)
     (set-face-attribute 'org-level-5 nil
                         :foreground "#50fa7b"
                         :height 1.0)
     (set-face-attribute 'org-level-6 nil
                         :foreground "#ffb86c"
                         :height 1.0)
     (set-face-attribute 'org-level-7 nil
                         :foreground "#0189cc"
                         :height 1.0)
     (set-face-attribute 'org-level-8 nil
                         :foreground "#f8f8f2"
                         :height 1.0)
     (set-face-attribute 'org-agenda-current-time nil
                         :foreground "green")
     (set-face-attribute 'org-agenda-date-today nil
                         :foreground "green")
     (set-face-attribute 'org-agenda-date-weekend nil
                         :foreground "light blue")

     ))

;; 设置org使用独立的字体
(with-eval-after-load 'org
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "FiraCode NF 16")   ;; FiraCode NF 16
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
    (add-hook 'org-mode-hook 'org-buffer-face-mode-variable))

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (hl-line-mode)
;;             (face-remap-add-relative 'hl-line :box '(:color "deep pink" :line-width 2))
;;             (face-remap-add-relative 'variable-pitch '(:foreground "blue" :background "white"))))

(message "org-mode configuration loaded.")

(provide 'org-init)
;;}}}

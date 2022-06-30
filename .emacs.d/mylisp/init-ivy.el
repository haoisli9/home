;; -*- lexical-binding: t -*-

(use-package ivy :demand
  :init
  ;; ivy pinyin support.
  (defun eh-ivy-cregexp (str)
    ;; (let ((x (ivy--regex-plus str))
    (let ((x (ivy--regex-ignore-order str))
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
  (setq ivy-re-builders-alist '(
                                (counsel-rg . ivy--regex-plus)
                                (counsel-locate . ivy--regex-plus)
                                (counsel-grep . ivy--regex-plus)
                                (t . eh-ivy-cregexp)))
  :config
  ;; Add recent files and bookmarks to the ivy-switch-buffer
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'abbreviate
        ivy-use-selectable-prompt t
        ivy-count-format "%d/%d "
        ivy-read-action-function 'ivy-hydra-read-action
        ;; 把所有completing改成ivy风格
        completing-read-function #'ivy-completing-read)
  :bind
  (("C-x b" . ivy-switch-buffer))
)

(defun counsel-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
	   (fboundp 'w32-shell-execute))
      (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
	    (format "explorer.exe %s" (file-name-directory (expand-file-name file)))) 'gbk))
    (call-process (pcase system-type
		    ('darwin "open")
		    ('cygwin "cygstart")
		    (_ "xdg-open"))
		  nil 0 nil
		  (file-name-directory (expand-file-name file)))))

(use-package counsel
  :ensure t
  :after ivy
  :bind (
         ("M-x" . #'counsel-M-x)
         ("C-x C-f" . #'counsel-find-file)
         ("C-h f" . #'counsel-describe-function)
         ("C-h v" . #'counsel-describe-variable)
         ("M-i l" . #'counsel-locate)
         ("M-i m" . #'counsel-imenu)
         ("M-i s" . #'counsel-grep-or-swiper)
         )
  :config
  ;; (setq counsel-grep-base-command
  ;;       "rg -i -M 120 --no-heading --line-number --color never %s %s")
  (ivy-set-actions
   'counsel-find-file '(
                        ("j" find-file-other-frame "other frame")
                        ;; ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
                        ("x" counsel-find-file-extern "open file externally")
                        ("e" counsel-directory-externally "open directory")
                        ("d" delete-file "delete")))
  (ivy-set-actions
   'counsel-locate '(
                     ("j" find-file-other-frame "other frame")
                     ("x" counsel-find-file-extern "open file externally")
                     ("e" counsel-directory-externally "open directory")
                     ("d" delete-file "delete")))
  )

(defvar ivy-occur-filter-prefix ">>> ")

;;;###autoload
(defun ivy-occur/filter-lines ()
  (interactive)
  (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
    (user-error "Current buffer is not in ivy-occur mode"))

  (let ((inhibit-read-only t)
        (regexp (read-regexp "Regexp(! for flush)"))
        (start (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "[0-9]+ candidates:"))))
    (if (string-prefix-p "!" regexp)
        (flush-lines (substring regexp 1) start (point-max))
      (keep-lines regexp start (point-max)))
    (save-excursion
      (goto-char (point-min))
      (let ((item (propertize (format "[%s]" regexp) 'face 'ivy-current-match)))
        (if (looking-at ivy-occur-filter-prefix)
            (progn
              (goto-char (line-end-position))
              (insert item))
          (insert ivy-occur-filter-prefix item "\n"))))))

;;;###autoload
(defun ivy-occur/undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (save-excursion
          (goto-char (point-min))
          (looking-at ivy-occur-filter-prefix))
        (undo)
      (user-error "Filter stack is empty"))))

(defun ivy|occur-mode-setup ()
  (local-set-key "/" #'ivy-occur/filter-lines)
  (local-set-key (kbd "C-/") #'ivy-occur/undo))

(add-hook 'ivy-occur-mode-hook 'ivy|occur-mode-setup)
(add-hook 'ivy-occur-grep-mode-hook 'ivy|occur-mode-setup)

(use-package ivy-xref
      :ensure
      :init
      (setq xref-show-definitions-function #'ivy-xref-show-defs
            xref-show-xrefs-function #'ivy-xref-show-xrefs))

(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (unless (featurep 'counsel) (require 'counsel))
    (counsel-imenu)))

;;----------------------------------------------------------------
(use-package counsel-etags
  :ensure t
  :bind (("M-]" . counsel-etags-find-tag-at-point))
  :init
  ;; auto update TAGS file.
  ;; (add-hook 'prog-mode-hook
  ;;       (lambda ()
  ;;         (add-hook 'after-save-hook
  ;;           'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  ;; (setq counsel-etags-extra-tags-files '("/usr/include/TAGS" "/usr/local/include/TAGS"))
  ;; (setq counsel-etags-update-interval 60)
  (setq counsel-etags-use-ripgrep-force t)
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (push "build" counsel-etags-ignore-directories)
  (push "build_clang" counsel-etags-ignore-directories)
  ;; counsel-etags-ignore-filenames supports wildcast
  (push "TAGS" counsel-etags-ignore-filenames)
  (push "*.json" counsel-etags-ignore-filenames)

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Do case-sensitive tag searches
  (setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
  ;; Don't warn when TAGS files are large
  ;; can not set to nil because some package will use this value.
  (setq large-file-warning-threshold nil)

  (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)

  ;; (unless (featurep 'pinyinlib) (require 'pinyinlib))
  ;; (setq counsel-etags-convert-grep-keyword
  ;;   (lambda (keyword)
  ;;     (if (and keyword (> (length keyword) 0))
  ;;         (pinyinlib-build-regexp-string keyword t)
  ;;       keyword)))

  ;; (unless (featurep 'pyim) (require 'pyim))
  ;; (setq counsel-etags-convert-grep-keyword
  ;;   (lambda (keyword)
  ;;     (if (and keyword (> (length keyword) 0))
  ;;         (pyim-cregexp-build keyword)
  ;;       keyword)))
  )

;; ‘counsel-yank-pop’ in visual mode does not replace the region, instead it just inserts the text. advise ‘counsel-yank-pop’ with a function that kills the region.
(defun moon-override-yank-pop (&optional arg)
  "Delete the region before inserting poped string."
  (when (and evil-mode (eq 'visual evil-state))
    (kill-region (region-beginning) (region-end))))
(advice-add 'counsel-yank-pop :before #'moon-override-yank-pop)

(use-package counsel-fd
  :ensure t
  :defer t
  :after counsel
  :config
  (setq counsel-fd-command "fd -a --hidden --color never ")
  (ivy-set-actions
   'counsel-fd-file-jump '(
                        ("j" find-file-other-frame "other frame")
                        ("x" counsel-find-file-extern "open externally")
                        ("d" delete-file "delete")))
  )

(define-key isearch-mode-map (kbd "C-c o") 'swiper-from-isearch)

(use-package all-the-icons-ivy
  :ensure t
  :after all-the-icons ivy
  :config
  (set-face-attribute 'all-the-icons-ivy-dir-face nil
                      :foreground "gold")
  (setq all-the-icons-ivy-file-commands nil)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy
  :init
  (setq all-the-icons-ivy-rich-display-transformers-list
        '(
          counsel-buffer-or-recentf
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (counsel-buffer-or-recentf-transformer
             (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face)))
           :delimiter "	")
          ;; ivy-switch-buffer
          ;; (:columns
          ;;  ((all-the-icons-ivy-rich-buffer-icon
          ;;    (:width 2))
          ;;   (ivy-rich-candidate
          ;;    (:width 30))
          ;;   (ivy-rich-switch-buffer-size
          ;;    (:width 7))
          ;;   (ivy-rich-switch-buffer-indicators
          ;;    (:width 4 :face error :align right))
          ;;   (ivy-rich-switch-buffer-major-mode
          ;;    (:width 20 :face warning))
          ;;   (ivy-rich-switch-buffer-project
          ;;    (:width 15 :face success))
          ;;   (ivy-rich-switch-buffer-path
          ;;    (:width
          ;;     (lambda
          ;;       (x)
          ;;       (ivy-rich-switch-buffer-shorten-path x
            ;;                                            (ivy-rich-minibuffer-width 0.3)))))
           ;;  )
           ;; :predicate
           ;; (lambda
           ;;   (cand)
           ;;   (get-buffer cand))
           ;; :delimiter "  ")
          counsel-M-x
          (:columns
           ((all-the-icons-ivy-rich-function-icon)
            (counsel-M-x-transformer
             (:width 50))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((all-the-icons-ivy-rich-function-icon)
            (counsel-describe-function-transformer
             (:width 50))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((all-the-icons-ivy-rich-variable-icon)
            (counsel-describe-variable-transformer
             (:width 50))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-set-variable
          (:columns
           ((all-the-icons-ivy-rich-variable-icon)
            (counsel-describe-variable-transformer
             (:width 50))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-apropos
          (:columns
           ((all-the-icons-ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-info-lookup-symbol
          (:columns
           ((all-the-icons-ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-descbinds
          (:columns
           ((all-the-icons-ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-find-file
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (all-the-icons-ivy-rich-file-name (:width 0.4))
            (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
            (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
            (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
            (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (all-the-icons-ivy-rich-file-name (:width 0.7))
            (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
            (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (all-the-icons-ivy-rich-file-name (:width 0.4))
            (all-the-icons-ivy-rich-file-modes (:width 12 :face all-the-icons-ivy-rich-file-modes-face))
            (all-the-icons-ivy-rich-file-id (:width 12 :face all-the-icons-ivy-rich-file-owner-face))
            (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
            (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (all-the-icons-ivy-rich-file-name (:width 0.7))
            (all-the-icons-ivy-rich-file-size (:width 7 :face all-the-icons-ivy-rich-size-face))
            (all-the-icons-ivy-rich-file-modification-time (:face all-the-icons-ivy-rich-time-face)))
           :delimiter "\t")
          counsel-git
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-recentf
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (ivy-rich-candidate
             (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face)))
           :delimiter "	")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (all-the-icons-ivy-rich-bookmark-name
             (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "	")
          counsel-bookmarked-directory
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-package
          (:columns
           ((all-the-icons-ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-fonts
          (:columns
           ((all-the-icons-ivy-rich-font-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-major
          (:columns
           ((all-the-icons-ivy-rich-function-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-find-library
          (:columns
           ((all-the-icons-ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-load-library
          (:columns
           ((all-the-icons-ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-load-theme
          (:columns
           ((all-the-icons-ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-world-clock
          (:columns
           ((all-the-icons-ivy-rich-world-clock-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-tramp
          (:columns
           ((all-the-icons-ivy-rich-tramp-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-git-checkout
          (:columns
           ((all-the-icons-ivy-rich-git-branch-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-list-processes
          (:columns
           ((all-the-icons-ivy-rich-process-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-projectile-switch-project
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-projectile-find-file
          (:columns
           ((all-the-icons-ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "	")
          counsel-projectile-find-dir
          (:columns
           ((all-the-icons-ivy-rich-project-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "	")
          counsel-minor
          (:columns
           ((all-the-icons-ivy-rich-mode-icon)
            (ivy-rich-candidate))
           :delimiter "	")
          counsel-imenu
          (:columns
           ((all-the-icons-ivy-rich-imenu-icon)
            (ivy-rich-candidate))
           :delimiter "	")
	  counsel-fd-file-jump
	  (:columns
	   ((all-the-icons-ivy-rich-file-icon)
	    (ivy-rich-candidate))
	   :delimiter " ")
	  counsel-fd-dired-jump
	  (:columns
	   ((all-the-icons-ivy-rich-file-icon)
	    (ivy-rich-candidate))
	   :delimiter " ")
	  project-find-file
	  (:columns
	   ((all-the-icons-ivy-rich-file-icon)
	    (ivy-rich-candidate))
	   :delimiter " ")
	  project-find-dir
	  (:columns
	   ((all-the-icons-ivy-rich-file-icon)
	    (ivy-rich-candidate))
	   :delimiter " ")
          ))
  (all-the-icons-ivy-rich-mode 1))

;; do not remap kill-buffer function in all-the-icons-ivy-rich.el
(global-unset-key [remap kill-buffer])

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(message "ivy configuration loaded.")

(provide 'init-ivy)

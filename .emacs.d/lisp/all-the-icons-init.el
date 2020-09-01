;; -*- Emacs-Lisp -*-
;; all-the-icons settings.
;; lihao

(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 1.0))

;; (require 'all-the-icons-dired)
;; (set-face-attribute 'all-the-icons-dired-dir-face nil
;;                     :foreground "gold")
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1)
  :config (setq all-the-icons-ibuffer-human-readable-size t))

;; (require 'all-the-icons-ivy)
;; (set-face-attribute 'all-the-icons-ivy-dir-face nil
;;                     :foreground "gold")
;; (all-the-icons-ivy-setup)
;; (with-eval-after-load 'counsel
;;   (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (set-face-attribute 'all-the-icons-ivy-dir-face nil
                      :foreground "gold")
  (setq all-the-icons-ivy-file-commands nil)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (setq all-the-icons-ivy-rich-display-transformers-list
   '(counsel-switch-buffer
     (:columns
      ((all-the-icons-ivy-rich-buffer-icon)
       (ivy-rich-candidate
        (:width 30))
       (ivy-rich-switch-buffer-size
        (:width 7))
       (ivy-rich-switch-buffer-indicators
        (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode
        (:width 12 :face warning))
       (ivy-rich-switch-buffer-project
        (:width 15 :face success))
       (ivy-rich-switch-buffer-path
        (:width
         (lambda
           (x)
           (ivy-rich-switch-buffer-shorten-path x
                                                (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda
        (cand)
        (get-buffer cand))
      :delimiter "	")
     counsel-switch-buffer-other-window
     (:columns
      ((all-the-icons-ivy-rich-buffer-icon)
       (ivy-rich-candidate
        (:width 30))
       (ivy-rich-switch-buffer-size
        (:width 7))
       (ivy-rich-switch-buffer-indicators
        (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode
        (:width 12 :face warning))
       (ivy-rich-switch-buffer-project
        (:width 15 :face success))
       (ivy-rich-switch-buffer-path
        (:width
         (lambda
           (x)
           (ivy-rich-switch-buffer-shorten-path x
                                                (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda
        (cand)
        (get-buffer cand))
      :delimiter "	")
     persp-switch-to-buffer
     (:columns
      ((all-the-icons-ivy-rich-buffer-icon)
       (ivy-rich-candidate
        (:width 30))
       (ivy-rich-switch-buffer-size
        (:width 7))
       (ivy-rich-switch-buffer-indicators
        (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode
        (:width 12 :face warning))
       (ivy-rich-switch-buffer-project
        (:width 15 :face success))
       (ivy-rich-switch-buffer-path
        (:width
         (lambda
           (x)
           (ivy-rich-switch-buffer-shorten-path x
                                                (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda
        (cand)
        (get-buffer cand))
      :delimiter "	")
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
       (ivy-read-file-transformer))
      :delimiter "	")
     counsel-file-jump
     (:columns
      ((all-the-icons-ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "	")
     counsel-dired
     (:columns
      ((all-the-icons-ivy-rich-file-icon)
       (ivy-read-file-transformer))
      :delimiter "	")
     counsel-dired-jump
     (:columns
      ((all-the-icons-ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "	")
     counsel-el
     (:columns
      ((all-the-icons-ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "	")
     counsel-fzf
     (:columns
      ((all-the-icons-ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "	")
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
     counsel-buffer-or-recentf
     (:columns
      ((all-the-icons-ivy-rich-file-icon)
       (counsel-buffer-or-recentf-transformer
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
     treemacs-projectile
     (:columns
      ((all-the-icons-ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "	")))
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(provide 'all-the-icons-init)

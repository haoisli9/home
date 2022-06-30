
;;---------------------------------------------------------------------------
;; shrface package extends shr / eww with org features and analysis capability. It can be used
;; in dash-docs, eww, nov.el, mu/mu4e, anki.el, etc. It is also able to export HTML buffer to
;; Org buffer/file, or download/archive web pages.
;;---------------------------------------------------------------------------

(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)

  ;; (shrface-default-keybindings) ; setup default keybindings
  (define-key shrface-mode-map (kbd "TAB") 'shrface-outline-cycle)
  (define-key shrface-mode-map (kbd "<backtab>") 'shrface-outline-cycle-buffer)
  (define-key shrface-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key shrface-mode-map (kbd "C-k") 'shrface-previous-headline)
  
  (setq shrface-href-versatile t)

  :custom
  (shrface-bullets-bullet-list '("◉""○""●""❀""✿"))
  )

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(use-package nov
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  ;;{{{ nov.
  ;; (setq nov-text-width 80)
  ;; set it to t to inhibit text filling
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)

  (with-eval-after-load 'olivetti
    (add-hook 'nov-mode-hook 'olivetti-mode))

  ;; (with-eval-after-load 'nov
  ;;   (defun novel-buffer-face-mode-variable ()
  ;;     (interactive)
  ;;     (make-face 'width-font-face)
  ;;     ;; (set-face-attribute 'width-font-face nil :font "YaHei Consolas Hybrid 16")   ;; FiraCode NF 16
  ;;     (set-face-attribute 'width-font-face nil :font "微软雅黑 30")
  ;;     (setq buffer-face-mode-face 'width-font-face)
  ;;     (buffer-face-mode))
  ;;     (add-hook 'nov-mode-hook 'novel-buffer-face-mode-variable))

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
  ;; shrface
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

(provide 'init-shrface)


(use-package lsp-mode
  :ensure t
  :defer t

  ;; :custom-face
  ;; (lsp-headerline-breadcrumb-path-error-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'error))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-path-warning-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'warning))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-path-info-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'success))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-path-hint-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'success))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-symbols-error-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'error)))))
  ;; (lsp-headerline-breadcrumb-symbols-warning-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'warning)))))
  ;; (lsp-headerline-breadcrumb-symbols-info-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'success)))))
  ;; (lsp-headerline-breadcrumb-symbols-hint-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'success)))))
     
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode) . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration))))

  :commands lsp-deferred
  :config
  ;; performance enhancement.
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit

  ;; (with-no-warnings
  ;;   ;; Disable `lsp-mode' in `git-timemachine-mode'
  ;;   (defun my-lsp--init-if-visible (fn &rest args)
  ;;     (unless (bound-and-true-p git-timemachine-mode)
  ;;       (apply fn args)))
  ;;   (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

  ;;   ;; Enable `lsp-mode' in sh/bash/zsh
  ;;   (defun my-lsp-bash-check-sh-shell (&rest _)
  ;;     (and (eq major-mode 'sh-mode)
  ;;          (memq sh-shell '(sh bash zsh))))
  ;;   (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell))
       
  :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
  
  :init
  ;; flymake disabled.
  (setq lsp-diagnostics-provider :none)

  ;; disable signature help.
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil) 

  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  
  (setq 
        ;; yasnappet disabled.
        lsp-enable-snippet nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil)

  ;; For `lsp-clients'
  ;; lsp-clients-python-library-directories '("/usr/local/" "/usr/"))

  ;; when use confu
  (with-eval-after-load 'confu

    (lsp-completion-provider :none) ;; we use Corfu!
    
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    (defun my/orderless-dispatch-flex-first (_pattern index _total)
      (and (eq index 0) 'orderless-flex))

    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
    (add-hook 'lsp-completion-mode 'my/lsp-mode-setup-completion)
    )
)

;; Python
(use-package lsp-pyright
  :after lsp
  ;; :preface
  ;; ;; Use yapf to format
  ;; (defun lsp-pyright-format-buffer ()
  ;;   (interactive)
  ;;   (when (and (executable-find "yapf") buffer-file-name)
  ;;     (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  ;; :hook (python-mode . (lambda ()
  ;;                        (require 'lsp-pyright)
  ;;                        (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
  :init
  (setq lsp-pyright-python-executable-cmd "c:/Python/Python39/python.exe"))
   
;; lsp-mode lsp-pyright

(message "lsp configuration loaded.")

(provide 'init-lsp)

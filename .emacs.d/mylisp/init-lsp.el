
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (c-mode . lsp-deferred)
         ;; (c++-mode . lsp-deferred)
         ;; (go-mode . lsp-deferred)
         ;; (python-mode . lsp-deferred)
         ;; (yaml-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred
  :config
  ;; performance enhancement.
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  
  ;; flymake disabled.
  (setq lsp-diagnostic-package :none)
  (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  ;; yasnappet disabled.
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)

  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

  :init
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

(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-pyright-python-executable-cmd "c:/python/Python39/python.exe")
  ;; :hook (python-mode . (lambda ()
  ;;                         (require 'lsp-pyright)
  ;;                         (lsp-deferred))))  ; or lsp
  )

;; lsp-mode lsp-pyright

(message "lsp configuration loaded.")

(provide 'init-lsp)

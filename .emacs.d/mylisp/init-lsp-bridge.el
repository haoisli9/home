
;; https://github.com/manateelazycat/lsp-bridge
;; git clone -b master https://github.com/manateelazycat/lsp-bridge.git  ~/.emacs.d/lisp/lsp-bridge-master/
(quelpa '(lsp-bridge :fetcher git :url "https://github.com/manateelazycat/lsp-bridge.git"))

;; (require 'yasnippet)
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)       ;; provide Java third-party library jump and -data directory support, optional

(setq lsp-bridge-python-command "c:/Python/Python39/python.exe")

;; (yas-global-mode 1)
(global-lsp-bridge-mode)
(define-key lsp-bridge-mode-map (kbd "M-;") 'acm-toggle-english-helper)

(setq acm-candidate-match-function 'orderless-flex)
(setq acm-enable-tabnine-helper t)
;; (setq acm-enable-citre t)

;; (define-key acm-mode-map (kbd "<tab>") 'acm-select-next)
;; (define-key acm-mode-map (kbd "<s-tab>") 'acm-select-prev)
;; (define-key acm-mode-map (kbd "<backtab>") 'acm-select-prev)
;; (setq lsp-bridge-enable-log t)

(message "lsp-bridge loaded.")

(provide 'init-lsp-bridge)


(require 'eglot)
(setq eglot-stay-out-of '(flymake))
(setq eglot-events-buffer-size 0)

;; setting yaml server. 
(setf (car (alist-get 'yaml-mode eglot-server-programs)) "~/.emacs.d/.cache/lsp/npm/yaml-language-server/yaml-language-server")

;; Option 1: Specify explicitly to use Orderless for Eglot
;; (with-eval-after-load 'eglot
;;   (setq completion-category-overrides '((eglot (styles orderless)))))

;; Option 2: Undo the Eglot modification of completion-category-defaults, use common completion category.
(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(provide 'init-eglot)

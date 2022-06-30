
(require 'eglot)
(setq eglot-stay-out-of '(flymake))
(setq eglot-events-buffer-size 0)

;; Option 1: Specify explicitly to use Orderless for Eglot
;; (with-eval-after-load 'eglot
;;   (setq completion-category-overrides '((eglot (styles orderless)))))

;; Option 2: Undo the Eglot modification of completion-category-defaults, use common completion category.
(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(provide 'init-eglot)

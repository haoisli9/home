
;; face reconfigure

(set-face-attribute 'default nil
                    :foreground "#E0E1F7")   ;; "#A0B1C7"

;; (set-face-attribute 'font-lock-comment-face nil
;;                     :foreground "grey50" :background "default")   ;; #292e34
(set-face-attribute 'fringe nil
                    :foreground "green")
(set-face-attribute 'vertical-border nil
                    :foreground "grey50")

(set-face-attribute 'show-paren-match nil
                    :foreground "yellow"
                    :background "darkgreen"
                    :bold t)
(set-face-attribute 'show-paren-mismatch nil
                    :foreground "yellow"
                    :background "red"
                    :bold t)

(require 'hl-line)
(set-face-attribute 'hl-line nil
                    :background "grey30")

;; avy like ace-jump-mode face.
(setq avy-background t)
(set-face-attribute 'avy-lead-face nil
                    :foreground "red"
                    :background "default")
(set-face-attribute 'avy-lead-face-0 nil
                    :foreground "orange"
                    :background "default")

;; (set-face-attribute 'evil-ex-lazy-highlight nil
;;                     :background "grey50")

;; set company face.
(eval-after-load 'company
  '(progn
     ;; (set-face-attribute 'company-echo-common nil
     ;;                     :background "#b2b2b2" :foreground "#292b2e")
     ;; (set-face-attribute 'company-preview nil
     ;;                     :background "#34323e" :foreground "#9a9aba")
     (set-face-attribute 'company-preview-common nil
                         :background "#34323e" :foreground "#b2b2b2")
     (set-face-attribute 'company-preview-search nil
                         :background "#444155" :foreground "#86dc2f")
     ;; (set-face-attribute 'company-scrollbar-bg nil
     ;;                     :background "#212026")
     ;; (set-face-attribute 'company-scrollbar-fg nil
     ;;                     :background "#5d4d7a")
     ;; (set-face-attribute 'company-template-field nil
     ;;                     :background "#444155" :extend t)
     ;; (set-face-attribute 'company-tooltip nil
     ;;                     :background "#34323e" :foreground "#9a9aba")
     (set-face-attribute 'company-tooltip-annotation nil
                         :foreground "#ce537a")
     ;; (set-face-attribute 'company-tooltip-common nil
     ;;                     :background "#34323e" :foreground "#4f97d7")
     ;; (set-face-attribute 'company-tooltip-common-selection nil
     ;;                     :foreground "#4f97d7")
     ;; (set-face-attribute 'company-tooltip-mouse nil
     ;;                     :foreground "#b2b2b2" :background "#444155")
     (set-face-attribute 'company-tooltip-search nil
                         :background "#444155" :foreground "#86dc2f")
     (set-face-attribute 'company-tooltip-search-selection nil
                         :foreground "#86dc2f" :bold t)
     ))

;; set modeline face.
;; '(mode-line ((t (:foreground "#bbc2cf" :background "#222226" :box (:color "#5d4d7a" :line-width 1)))))
(set-face-attribute 'mode-line nil
                    :foreground "#bbc2cf" :background "#222226"
                    :box '(:color "#5d4d7a" :line-width 1))
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#b2b2b2" :background "#292b2e"
                    :box '(:color "#5d4d7a" :line-width 1))

;; elisp enhance face.
(quelpa '(elispfl :fetcher git :url "https://github.com/cireu/elispfl.git"))
(with-eval-after-load 'elisp-mode
  (require 'elispfl)
  (elispfl-mode))

(provide 'init-face)

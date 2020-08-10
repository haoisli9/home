
;;; Code:

(deftheme airline-fork-railscasts
  "source: https://github.com/vim-airline/vim-airline-themes")

(let ((normal-outer-foreground   "#272935") (normal-outer-background   "#a5c261")
      (normal-inner-foreground   "#f4f1ed") (normal-inner-background   "#3a4055")
      (normal-center-foreground  "#ffffff") (normal-center-background  "grey30")

      (insert-outer-foreground   "#005f5f") (insert-outer-background   "#ffffff")
      (insert-inner-foreground   "#3a4055") (insert-inner-background   "#0087af")
      (insert-center-foreground  "#87d7ff") (insert-center-background  "#005f87")

      ;; (visual-outer-foreground   "#272935") (visual-outer-background   "#b6b3eb")
      ;; (visual-inner-foreground   "#f4f1ed") (visual-inner-background   "#3a4055")
      ;; (visual-center-foreground  "#ffffff") (visual-center-background  "grey30")
      (visual-outer-foreground   "#080808") (visual-outer-background   "#ffaf00")
      (visual-inner-foreground   "#9e9e9e") (visual-inner-background   "#303030")
      (visual-center-foreground  "#ffffff") (visual-center-background  "#121212")

      (replace-outer-foreground  "#272935") (replace-outer-background  "#da4939")
      (replace-inner-foreground  "#f4f1ed") (replace-inner-background  "#3a4055")
      (replace-center-foreground "#ffffff") (replace-center-background "grey30")

      ;; (emacs-outer-foreground   "#272935") (emacs-outer-background   "#6d9cbe")
      ;; (emacs-inner-foreground   "#f4f1ed") (emacs-inner-background   "#3a4055")
      ;; (emacs-center-foreground  "#ffffff") (emacs-center-background  "grey30")
      (emacs-outer-foreground   "#272935") (emacs-outer-background   "#b6b3eb")
      (emacs-inner-foreground   "#f4f1ed") (emacs-inner-background   "#3a4055")
      (emacs-center-foreground  "#ffffff") (emacs-center-background  "grey30")

      (inactive1-foreground      "#e6e1dc") (inactive1-background      "grey30")
      (inactive2-foreground      "#e6e1dc") (inactive2-background      "grey30")
      (inactive3-foreground      "#e6e1dc") (inactive3-background      "grey30"))

  (airline-themes-set-deftheme 'airline-fork-railscasts)

  (when airline-cursor-colors
    (setq evil-emacs-state-cursor   emacs-outer-background
          evil-normal-state-cursor  normal-outer-background
          evil-insert-state-cursor  `(bar ,insert-outer-background)
          evil-replace-state-cursor replace-outer-background
          evil-visual-state-cursor  visual-outer-background))
)

(airline-themes-set-modeline)

(provide-theme 'airline-fork-railscasts)
;;; airline-base16_railscasts-theme.el ends here

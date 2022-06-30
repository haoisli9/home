;; for modeline-configuration.

;;------------------------------------------------------------
(window-numbering-mode)

;;------------------------------------------------------------
;;{{{ mode-line-format.
;; (setq-default
;;  mode-line-format
;;  (list
;;    " ["
;;    '(:eval (when (window-numbering-get-number)
;;              (propertize (int-to-string (window-numbering-get-number)) 'face 'font-lock-keyword-face
;;         	       'help-echo "window number")))
;;    "]"
;;    ;; evil state
;;    '(:eval
;;      (propertize (evil-generate-mode-line-tag evil-state) 'face
;; 		 (cond ((evil-normal-state-p) 'font-lock-function-name-face)
;; 		       ((evil-insert-state-p) 'font-lock-keyword-face)
;; 		       ((evil-visual-state-p) '(:foreground "orange" :weight bold))
;;                        ((evil-motion-state-p) '(:foreground "yellow" :weight bold))
;; 		       ((evil-emacs-state-p) '(:foreground "green" :weight bold))
;; 		       (t 'font-lock-constant-face))))
;;    mode-line-front-space
;;    mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
;;    mode-line-frame-identification mode-line-buffer-identification
;;    ;; '(:eval (when (projectile-project-p)
;;    ;;          (propertize (format " P[%s] " (projectile-project-name))
;;    ;;                      'face 'font-lock-variable-name-face)))
;;    " "
;;    mode-line-position
;;    "("
;;    '(:eval (concat (format "%s" (int-to-string (count-lines (point-min) (point-max))))))
;;    ") "
;;    ;; the current major mode for the buffer.
;;    "["
;;    '(:eval (propertize (format "%s" major-mode)  ;; "%m"
;; 		       'face nil
;; 		       'help-echo (format "Coding: %s" buffer-file-coding-system)))
;;    ;; list of minor modes
;;    ;; minor-mode-alist
;;    "] "
;;    ;; mode-line-modes
;;    ;; mode-line-misc-info
;;    '(:eval (when vc-mode (concat "[" (propertize (format "%s" vc-mode) 'face 'font-lock-variable-name-face) "] " ))) ; vc-mode vc-mode
;;    ;; global-mode-string, org-timer-set-timer in org-mode need this
;;    '(:eval (propertize "%M " 'face nil))
;;    "[" ;; insert vs overwrite mode, input-method in a tooltip
;;    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;; 		       'face 'font-lock-preprocessor-face
;; 		       'help-echo (concat "Buffer is in "
;; 					  (if overwrite-mode "overwrite" "insert") " mode")))
;;    ;; was this buffer modified since the last save?
;;    '(:eval (when (buffer-modified-p)
;; 	     (concat "/"  (propertize "MD"
;; 				      'face '((:foreground "red" :weight bold))
;; 				      'help-echo "Buffer has been modified"))))
;;    ;; is this buffer read-only?
;;    '(:eval (when buffer-read-only
;; 	     (concat "/"  (propertize "RO"
;; 				      'face 'font-lock-type-face
;; 				      'help-echo "Buffer is read-only"))))
;;    "] "
;;    "["
;;    '(:eval (propertize
;; 	   (concat (pcase (coding-system-eol-type buffer-file-coding-system)
;; 		     (0 "LF ")
;; 		     (1 "CRLF ")
;; 		     (2 "CR "))
;; 		   (let ((sys (coding-system-plist buffer-file-coding-system)))
;; 		     (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
;; 			 "UTF-8"
;; 		       (upcase (symbol-name (plist-get sys :name)))))
;; 		   )))
;;    "] "
;;    ;;"%-" ;; fill with '-'
;;    mode-line-end-spaces))

;; change mode-line color by evil state
;; (lexical-let ((default-color (cons (face-background 'mode-line)
;;                                    (face-foreground 'mode-line))))
;;   (add-hook 'post-command-hook
;;             (lambda ()
;;               (let* ((color (cond ((minibufferp) default-color)
;;                                    ;; ((evil-insert-state-p) '("yellow" . "#ffffff"))
;;                                   ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                                   ;; ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                                   (t default-color))))
;;                 (set-face-background 'mode-line (car color))
;;                 (set-face-foreground 'mode-line (cdr color))))))

(set-face-attribute 'mode-line-inactive nil
                    :background "#34475a"    ;; #44475a
                    :box '(:color "#5d4d7a" :line-width 1))   ;; #5d4d7a

;;}}}
(require 'airline-themes)
;; (load-theme 'airline-powerlineish t)
(load-theme 'airline-fork-railscasts t)

;; Hide Evil and buffer state on inactive buffers.
;; Valid Values: t (hidden), nil (shown)
(setq airline-hide-state-on-inactive-buffers t)

;; "Hide eyebrowse indicator on inactive buffers.
;; Valid Values: t (hidden), nil (shown)"
(setq airline-hide-eyebrowse-on-inactive-buffers t)

;; Hide vc branch on inactive buffers:
;; Valid Values: t (hidden), nil (shown)
(setq airline-hide-vc-branch-on-inactive-buffers nil)

;; Set eshell prompt colors to match the airline theme.
;; Valid Values: t (enabled), nil (disabled)
(setq airline-eshell-colors t)

;; Set helm colors to match the airline theme.
;; Valid Values: t (enabled), nil (disabled)
(setq airline-helm-colors t)

;; Set the cursor color based on the current evil state.
;; Valid Values: t (enabled), nil (disabled)
(setq airline-cursor-colors t)

;; Display the currend directory along with the filename.
;; Valid Values: 'airline-directory-full
;;               'airline-directory-shortened
;;               nil (disabled)
(setq airline-display-directory nil)

;; Max directory length to display when using 'airline-directory-shortened
(setq airline-shortened-directory-length 30)

;; Unicode character choices
(setq airline-utf-glyph-separator-left #xe0b0
      airline-utf-glyph-separator-right #xe0b2
      airline-utf-glyph-subseparator-left #xe0b1
      airline-utf-glyph-subseparator-right #xe0b3
      airline-utf-glyph-branch #xe0a0
      airline-utf-glyph-readonly #xe0a2
      airline-utf-glyph-linenumber #x2630)

;; You may also wish to force powerline to use utf8 character separators
(setq powerline-default-separator 'utf-8)
(setq powerline-utf-8-separator-left  #xe0b0
      powerline-utf-8-separator-right #xe0b2)

(provide 'init-modeline)

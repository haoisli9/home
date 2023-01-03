;; -*- lexical-binding: t -*-

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-redo)
  (evil-mode 1)
  ;; specify major mode uses Evil (vim) NORMAL state or EMACS original state.
  ;; You may delete this setup to use Evil NORMAL state always.
  (dolist (p '((minibuffer-inactive-mode . emacs)
	       (calendar-mode . emacs)
	       (calc-mode . emacs)
	       (calculator-mode . emacs)
	       (special-mode . emacs)
	       (grep-mode . emacs)
	       (Info-mode . emacs)
	       (term-mode . emacs)
	       (sdcv-mode . emacs)
	       (anaconda-nav-mode . emacs)
	       (log-edit-mode . emacs)
	       (vc-log-edit-mode . emacs)
	       (magit-log-edit-mode . emacs)
	       (erc-mode . emacs)
	       (neotree-mode . emacs)
	       (w3m-mode . emacs)
	       (gud-mode . emacs)
	       (help-mode . emacs)
	       (eshell-mode . emacs)
	       (shell-mode . emacs)
	       (xref--xref-buffer-mode . emacs)
	       (gtags-select-mode . emacs)
	       ;; (message-mode . emacs)
	       ;; (fundamental-mode . emacs)
	       (woman-mode . normal)
	       (profiler-report-mode . emacs)
	       (dired-mode . normal)
	       (compilation-mode . emacs)
	       (speedbar-mode . emacs)
	       (ffip-file-mode . emacs)
	       (helpful-mode . emacs)
	       (color-rg-mode . emacs)
	       (elfeed-search-mode . emacs)
	       (elfeed-show-mode . emacs)
	       ))
    (evil-set-initial-state (car p) (cdr p)))

  :config
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-visual-line)
  (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "q") 'quit-window)

  ;; change evil-search to isearch
  (define-key evil-normal-state-map (kbd "/") 'isearch-forward)
  (define-key evil-normal-state-map (kbd "?") 'isearch-backward)
  (define-key evil-normal-state-map (kbd "n") 'isearch-repeat-forward)
  (define-key evil-normal-state-map (kbd "N") 'isearch-repeat-backward)
  (define-key evil-motion-state-map (kbd "C-c o") 'occur-from-isearch)
  (define-key evil-motion-state-map (kbd "M-s v") 'consult-from-isearch)

  ;; unbind some evil-insert key.
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-v") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  
  (add-hook 'evil-insert-state-entry-hook (lambda () (cua-mode 1)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (cua-mode -1)))

  (setq evil-normal-state-cursor '("#D02C6D" box))
  ;; (setq evil-normal-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-emacs-state-cursor '("gold" box))

  ;; add newline in insert state. 
  (defun my-add-newline ()
    (interactive)
    (end-of-visual-line)
    (newline-and-indent)
    )
  (define-key evil-normal-state-map (kbd "M-RET") 'my-add-newline)

  (defun occur-from-evil-ex ()
    "Invoke `occur' from evil-search"
    (interactive)
    (evil-ex-nohighlight)
    (occur (car evil-ex-search-pattern))
    (pop-to-buffer "*Occur*")
    )

  (defun swiper-from-evil-ex ()
    "Invoke `swiper' from isearch."
    (interactive)
    (evil-ex-nohighlight)
    (swiper (car evil-ex-search-pattern)))

  (defun consult-from-evil-ex ()
    "Invoke `consult-line from isearch."
    (interactive)
    (evil-ex-nohighlight)
    (consult-line (car evil-ex-search-pattern)))

  ;; set (evil-select-search-module 'evil-search-module 'evil-search)
  ;; so here set evil-search occur to isearch
  ;; (define-key evil-motion-state-map (kbd "C-c o") 'occur-from-evil-ex)
  ;; (define-key evil-motion-state-map (kbd "M-s v") 'consult-from-evil-ex)
  
  ;; 搜索选中的内容
  (defun evil-ex-start-search-with-region-string ()
    (let ((selection (with-current-buffer (other-buffer (current-buffer) 1)
                       (when (evil-visual-state-p)
                         (let ((selection (buffer-substring-no-properties (region-beginning)
                                                                          (1+ (region-end)))))
                           (evil-normal-state)
                           selection)))))
      (when selection
        (evil-ex-remove-default)
        (insert selection)
        (evil-ex-search-activate-highlight (list selection
                                                 evil-ex-search-count
                                                 evil-ex-search-direction))
        )))

  (advice-add 'evil-ex-search-setup :after 'evil-ex-start-search-with-region-string)

  (setq-default
   evil-disable-insert-state-bindings t
   evil-symbol-word-search t
   evil-want-fine-undo t
   ;; 当v选择到行尾时是否包含换行符
   evil-want-visual-char-semi-exclusive t
   ;; C-e, 到行尾时,光标的位置是在最后一个字符后,还是在字符上
   evil-move-cursor-back t)
)

(use-package evil-pinyin
  :ensure t
  :init
  ;;(setq-default evil-pinyin-scheme 'simplified-xiaohe-all)
  ;;(setq-default evil-pinyin-with-search-rule 'always)

  :config
  ;; set evil-search to isearch.
  ;; (evil-select-search-module 'evil-search-module 'isearch)
  ;; set evil-search to evil-ex-search.
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-search-persistent-highlight nil)
  (global-evil-pinyin-mode)
  )

(with-eval-after-load 'evil
  (require 'evil-anzu)
  ;; misc evil plugins.
  (global-evil-matchit-mode 1)
  (global-evil-surround-mode t)
  )

(provide 'init-evil)

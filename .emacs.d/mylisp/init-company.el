;; -*- lexical-binding: t -*-

(use-package company
  :init
  (setq company-require-match nil            ; Don't require match, so you can still move your cursor as expected.
        company-tooltip-align-annotations t  ; Align annotation to the right side.
        company-dabbrev-downcase nil         ; No downcase when completion.
        company-dabbrev-ignore-buffers "\\`[ *]||TAGS$"
        company-dabbrev-code-other-buffers t
        company-dabbrev-code-everywhere t
        company-dabbrev-code-ignore-case t
        )

  ;; Trigger completion immediately.
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; NOT to load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not
          comint-mode erc-mode gud-mode rcirc-mode
          minibuffer-inactive-mode
          ))
  (add-hook 'after-init-hook 'global-company-mode)
  
  :config
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            ;; cause erro in eshell mode.
                            ;; company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-common-frontend
                            company-echo-metadata-frontend))

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  ;; (company-tng-configure-default)
  
  ;; (define-key company-active-map [tab] 'company-select-next)
  ;; (define-key company-active-map (kbd "TAB") 'company-select-next)
  ;; (define-key company-active-map [backtab] 'company-select-previous)
  ;; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  ;; (define-key company-active-map (kbd "RET") #'company-complete)
  ;; (define-key company-active-map (kbd "TAB") #'company-complete-common)
  (define-key company-active-map (kbd "M-m") #'company-other-backend)
  (define-key company-active-map (kbd "M-.") #'company-show-location)

  ;; (setq company-clang-arguments '(
  ;;                                 ;; "--target=i686-w64-mingw64"
  ;;                                 "-Id:/cygwin64/usr/include/"
  ;;                                 ))

  (setq company-transformers '(delete-dups company-sort-by-occurrence))

  ;; Enable downcase only when completing the completion.
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)
  
  )

;; company-tabnine
(use-package company-tabnine
  :after company
  :custom
  (company-tabnine-max-num-results 9))

(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))
(add-to-list 'company-transformers 'company//sort-by-tabnine t)

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
	       (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
	ad-do-it))))

;; set completion-styles to basic to get word completion first.
;; (setq completion-styles '(basic substring orderless))
(setq company-search-regexp-function 'company-search-words-in-any-order-regexp)

;; `:separate` 使得不同 backend 分开排序
;; (add-to-list 'company-backends '(company-capf company-dabbrev company-files :with  company-tabnine :separate))
;; (setq company-backends '((company-capf company-etags company-dabbrev-code company-files :with company-tabnine :separate)))
(setq company-backends '((company-capf company-etags company-dabbrev-code company-files :separate)))
;; set locally.
(dolist (hook (list
               'org-mode-hook
               'text-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq-local company-backends '((company-dabbrev company-files company-tabnine :separate)))
                     ;; (setq-local company-backends '((company-dabbrev company-files)))
                     )))

(use-package company-fuzzy
 :ensure t
 :hook (prog-mode . company-fuzzy-mode)
 :init
 (quelpa '(flx-rs :fetcher git :url "https://github.com/jcs-elpa/flx-rs.git"))
 (require 'flx-rs)
 (flx-rs-load-dyn)
 (setq company-fuzzy-sorting-backend 'flx-rs
       ;;  If you set company-fuzzy-sorting-backend to 'flx then you probably don't need this to be on because the flx scoring engine already take care of that!
       company-fuzzy-prefix-on-top t
       ;; company-fuzzy-show-annotation nil
       ;; company-fuzzy-history-backends '(company-yasnippet)
       company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@"))
 )

;; company-english-helper
(quelpa '(company-english-helper :fetcher git :url "https://github.com/manateelazycat/company-english-helper.git"))
(use-package company-english-helper
  :after company
  :defer 3
  :bind
  (:map company-mode-map
        ("M-c c" . my/toggle-company-english-helper)
        ("M-c ;" . my/company-english-helper-search))
  :config
  ;;; Make sure add company-english-helper backend to end.
  ;; (if company-english-helper-active-p (toggle-company-english-helper))
  ;; (add-to-list 'company-backends 'company-english-helper-search t)
  ;; (setq company-english-helper-active-p t)
  (defun my/company-english-helper-search ()
    (interactive)
    (company-cancel)
    (command-execute 'company-english-helper-search))

  (defun my/toggle-company-english-helper ()
    (interactive)
    ;; (company-mode t)
    (defvar-local my/company-english-helper-backends nil)
    (defvar-local my/company-fuzzy-mode-p nil)
    (let ((backends (buffer-local-value 'my/company-english-helper-backends (current-buffer))))
      (if backends
          (progn
            (message "company english helper off")
            (setq-local company-backends backends)
            (setq-local company-backends (delete 'company-english-helper-search company-backends))
            (if my/company-fuzzy-mode-p (company-fuzzy-turn-on-company-fuzzy-mode))
            (setq-local my/company-english-helper-backends nil))
        (message "company english helper on")
        ;; english helper search can not used in fuzzy mode.
        (if company-fuzzy-mode
            (progn
              (setq-local my/company-fuzzy-mode-p company-fuzzy-mode)
              (company-fuzzy-mode 0)))
        (setq-local my/company-english-helper-backends company-backends)
        ;; (setq-local company-backends '(company-english-helper-search))
        (setq-local company-backends
                    (append
                     '(company-english-helper-search)
                     (delete 'company-english-helper-search company-backends)))
        ))))

;; company-anywhere.
;; Enable company in middle of symbols.
(defun company-anywhere-after-finish (completion)
  (when (and (stringp completion)
             (looking-at "\\(?:\\sw\\|\\s_\\)+")
             (save-match-data
               (string-match (regexp-quote (match-string 0)) completion)))
    (delete-region (match-beginning 0) (match-end 0))))
(add-hook 'company-after-completion-hook 'company-anywhere-after-finish)

(defun company-anywhere-grab-word (_)
  (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
(advice-add 'company-grab-word :around 'company-anywhere-grab-word)

(defun company-anywhere-grab-symbol (_)
  (buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))
(advice-add 'company-grab-symbol :around 'company-anywhere-grab-symbol)

(defun company-anywhere-dabbrev-prefix (_)
  (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)" company-dabbrev-char-regexp) 1))
(advice-add 'company-dabbrev--prefix :around 'company-anywhere-dabbrev-prefix)

(defun company-anywhere-capf (fn command &rest args)
  (if (eq command 'prefix)
      (let ((res (company--capf-data)))
        (when res
          (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
                (prefix (buffer-substring-no-properties (nth 1 res) (point))))
            (cond
             (length (cons prefix length))
             (t prefix)))))
    (apply fn command args)))
(advice-add 'company-capf :around 'company-anywhere-capf)

(defun company-anywhere-preview-show-at-point (pos completion)
  (when (and (save-excursion
               (goto-char pos)
               (looking-at "\\(?:\\sw\\|\\s_\\)+"))
             (save-match-data
               (string-match (regexp-quote (match-string 0)) completion)))
    (move-overlay company-preview-overlay (overlay-start company-preview-overlay) (match-end 0))
    (let ((after-string (overlay-get company-preview-overlay 'after-string)))
      (when after-string
        (overlay-put company-preview-overlay 'display after-string)
        (overlay-put company-preview-overlay 'after-string nil)))))
(advice-add 'company-preview-show-at-point :after 'company-anywhere-preview-show-at-point)

(with-eval-after-load "company-dwim"
  (defun company-anywhere-dwim-overlay-show-at-point (pos prefix completion)
    (when (and (looking-at "\\(?:\\sw\\|\\s_\\)+")
               (save-match-data
                 (string-match (regexp-quote (match-string 0)) completion)))
      (move-overlay company-dwim-overlay (overlay-start company-dwim-overlay) (match-end 0))))
  (advice-add 'company-dwim-overlay-show-at-point :after
              'company-anywhere-dwim-overlay-show-at-point))

(defun company-anywhere-tng-frontend (command)
  (when (and (eq command 'update)
             company-selection
             (looking-at "\\(?:\\sw\\|\\s_\\)+")
             (save-match-data
               (string-match (regexp-quote (match-string 0))
                             (nth company-selection company-candidates))))
    (move-overlay company-tng--overlay (overlay-start company-tng--overlay) (match-end 0))))
(advice-add 'company-tng-frontend :after 'company-anywhere-tng-frontend)

;; (require 'init-ctags)

(use-package consult-company
  :after consult company
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company))

(defun my-eshell-mode-company ()
  (setq-local company-idle-delay nil))

  ;; set company-complete-common;
(dolist (hook (list
               'eshell-mode-hook
               'shell-mode-hook
               ))
  (add-hook hook 'my-eshell-mode-company))

(message "company configuration loaded.")

(provide 'init-company)

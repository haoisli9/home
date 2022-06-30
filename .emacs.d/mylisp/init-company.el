;; -*- lexical-binding: t -*-

(use-package company
  :init
  (setq company-require-match nil            ; Don't require match, so you can still move your cursor as expected.
        company-tooltip-align-annotations t  ; Align annotation to the right side.
        company-eclim-auto-save nil          ; Stop eclim auto save.
        company-dabbrev-downcase nil         ; No downcase when completion.
        company-dabbrev-ignore-buffers "\\`[ *]||TAGS$"
        )

  ;; Trigger completion immediately.
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; NOT to load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not
          shell-mode comint-mode erc-mode gud-mode rcirc-mode
          minibuffer-inactive-mode
          ))
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
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
  )

;; too slow so disable it.
;; (use-package company-fuzzy
;;  :hook (company-mode . company-fuzzy-mode)
;;  :init
;;  (setq company-fuzzy-sorting-backend 'flx
;;        ;;  If you set company-fuzzy-sorting-backend to 'flx then you probably don't need this to be on because the flx scoring engine already take care of that!
;;        company-fuzzy-prefix-on-top t
;;        company-fuzzy-prefix-on-top nil
;;        ;; company-fuzzy-history-backends '(company-yasnippet)
;;        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

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

;; `:separate`  使得不同 backend 分开排序
;; (add-to-list 'company-backends '(company-capf company-dabbrev company-files :with  company-tabnine :separate))
;; (setq company-backends '((company-capf company-dabbrev company-files :with company-tabnine)))
(setq company-backends '((company-capf company-dabbrev company-etags company-files :with company-tabnine)))

;; company-english-helper
;; https://github.com/manateelazycat/company-english-helper
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
    (company-mode t)
    (defvar-local my/company-english-helper-backends nil)
    (let ((backends (buffer-local-value 'my/company-english-helper-backends (current-buffer))))
      (if backends
          (progn
            (message "company english helper off")
            (setq-local company-backends backends)
            (setq-local my/company-english-helper-backends nil))
        (message "company english helper on")
        (setq-local my/company-english-helper-backends company-backends)
        (setq-local company-backends
                    (append
                     '(company-english-helper-search)
                     (delete 'company-english-helper-search company-backends)))))))

;; Enable company in middle of symbols.
(require 'company-anywhere)

;; find . -name "*.[ch]" | ctags -e -L -
(use-package company-ctags
  :after company
  :config
  (company-ctags-auto-setup)
  (setq company-ctags-ignore-case t)
  ;; (setq company-ctags-extra-tags-files '("$HOME/TAGS" "/usr/include/TAGS"))

  (defun my-consult-company-ctags ()
    "Input code from company backend using fuzzy matching."
    (interactive)
    (company-abort)
    (let* ((company-backends '(company-ctags))
           (company-ctags-fuzzy-match-p t))
      (consult-company)))
  ;; (define-key company-mode-map (kbd "M-]") 'my-consult-company-ctags)
  )

(use-package consult-company
  :after consult company
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company))


(message "company configuration loaded.")

(provide 'init-company)

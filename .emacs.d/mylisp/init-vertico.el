;;------------------------------------------------------------------------
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("<tab>" . vertico-insert)    ; Choose selected candidate
        ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
        ("?" . #'minibuffer-completion-help)
        ("M-RET" .  #'minibuffer-force-complete-and-exit)
        ("M-TAB" .  #'minibuffer-complete)
        ;; NOTE 2022-02-05: Cycle through candidate groups
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group))
  :custom
  (vertico-count 15)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
 )

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :init
  (vertico-multiform-mode)
  :config
  ;; (setq vertico-multiform-categories
  ;;       '((symbol (vertico-sort-function . vertico-sort-alpha))
  ;;         (file (vertico-sort-function . sort-directories-first))))

  (defun sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; (setq vertico-multiform-commands
  ;;       '((find-file (vertico-sort-function . sort-directories-first))))
  (setq vertico-multiform-commands
        '((irfc-browser (vertico-sort-function . nil))))
  )

(advice-add #'ffap-menu-ask :around (lambda (&rest args)
                                 (cl-letf (((symbol-function #'minibuffer-completion-help)
                                            #'ignore))
                                   (apply args))))

;; orderless
(use-package orderless
  :ensure t
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(basic substring orderless)  ;; basic does not sort.
        completion-category-defaults nil
        completion-ignore-case t
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        completion-category-overrides '((file (styles orderless partial-completion)))) ;; orderless is tried first
        ;; completion-category-overrides '((file (styles basic-remote ;; For `tramp' hostname completion with `vertico'
        ;;                                               orderless))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch))

;; marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map ("M-a" . marginalia-cycle))
  )

;; (use-package all-the-icons-completion
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))
(require 'all-the-icons-completion)
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

;; consult
;; https://github.com/minad/consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s s" . consult-ripgrep-or-line)
         ("M-s l" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; consult-locate配置使用everything
  (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  ;; (add-to-list 'process-coding-system-alist '("es" gbk . gbk)) 

  (add-to-list 'consult-buffer-filter "\\`\\*TAGS\\'")

  ;; consult-outline support for eshell prompts
  (add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))
  
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (progn
      (setq
	consult-narrow-key "<"
	consult-line-numbers-widen t
	consult-async-min-input 2
	consult-async-refresh-delay  0.15
	consult-async-input-throttle 0.2
	consult-async-input-debounce 0.1
      ))

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Narrowing which-key help without delay
  (defun immediate-which-key-for-narrow (fun &rest args)
  (let* ((refresh t)
         (timer (and consult-narrow-key
                     (memq :narrow args)
                     (run-at-time 0.05 0.05
                                  (lambda ()
                                    (if (eq last-input-event (elt consult-narrow-key 0))
                                        (when refresh
                                          (setq refresh nil)
                                          (which-key--update))
                                      (setq refresh t)))))))
    (unwind-protect
        (apply fun args)
      (when timer
        (cancel-timer timer)))))
  (advice-add #'consult--read :around #'immediate-which-key-for-narrow)
  
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; misc functions.
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defcustom consult-ripgrep-or-line-limit 300000
    "Buffer size threshold for `my-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
    :type 'integer)

  (defun consult-ripgrep-or-line ()
    "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
    (interactive)
    (if (or (not buffer-file-name)
            (buffer-narrowed-p)
            (ignore-errors
              (file-remote-p buffer-file-name))
            (jka-compr-get-compression-info buffer-file-name)
            (<= (buffer-size)
		(/ consult-ripgrep-or-line-limit
                   (if (eq major-mode 'org-mode) 4 1))))
	(consult-line)
      (when (file-writable-p buffer-file-name)
	(save-buffer))
      (let ((consult-ripgrep-command
             (concat "rg "
                     "--null "
                     "--line-buffered "
                     "--color=ansi "
                     "--max-columns=250 "
                     "--no-heading "
                     "--line-number "
                     ;; adding these to default
                     "--smart-case "
                     "--hidden "
                     "--max-columns-preview "
                     ;; add back filename to get parsing to work
                     "--with-filename "
                     ;; defaults
                     "-e ARG OPTS "
                     (shell-quote-argument buffer-file-name))))
	(consult-ripgrep))))

  
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)
   ;; consult-line consult-line-multi consult-ripgrep-or-line consult-line-symbol-at-point
   consult-buffer consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref consult-imenu
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '("M-."
                  :debounce 1 "<up>" "<down>")
  ))

(defvar-local consult-toggle-preview-orig nil)
(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)))

;; Bind to `vertico-map' or `selectrum-minibuffer-map'
(define-key vertico-map (kbd "M-P") #'consult-toggle-preview)

;; If you don’t want the group titles for modes configured in consult-imenu-config to be searchable,
;; it is possible to advise orderless, such that it ignores the top most menu names/group titles when searching candidates.
;; The following implements this, while also allowing embark collect to visit a single imenu entry from the collect buffer.
(defun my/consult-imenu-around-advice (ci-orig &rest r)
  "Patch orderless to inhibit matching group categories in consult-imenu."
  (if-let* ((config (cdr (seq-find (lambda (x) (derived-mode-p (car x)))
                                   consult-imenu-config)))
            (types (plist-get config :types))
            (types-regex (rx-to-string
                          `(and line-start (or ,@(mapcar #'cadr types)) ? ))))
      (cl-letf* ((of-orig (symbol-function 'orderless-filter))
                 ((symbol-function 'orderless-filter) ;patch pattern compiler within filter
                  (lambda (&rest r)
                    (cl-letf* ((opc-orig (symbol-function 'orderless-pattern-compiler))
                               ((symbol-function 'orderless-pattern-compiler)
                                (lambda (&rest r)
                                  (if (and (eq (length r) 1) ;single match string starts
                                           (string-match-p types-regex (car r)))
                                      (apply opc-orig r)
                                    (mapcar (lambda (x) ;replace beginning-of-string
                                              (if (string-match (regexp-quote "\\`" ) x)
                                                  (concat types-regex
                                                          (replace-match "\\b" nil t x))
                                                (concat types-regex ".*?" x)))
                                            (apply opc-orig r))))))
                      (apply of-orig r))))
                 (oh-orig (symbol-function 'orderless--highlight))
                 ((symbol-function 'orderless--highlight) ; patch highlighter to skip type
                  (lambda (regexps string)
                    (if-let* ((pref
                               (next-single-property-change 0 'consult--type string)))
                        (cl-letf* ((sm-orig (symbol-function 'string-match))
                                   ((symbol-function 'string-match)
                                    (lambda (re str)
                                      (funcall sm-orig re str (1+ pref)))))
                          (funcall oh-orig regexps string))
                      (funcall oh-orig regexps string)))))
        (apply ci-orig r))
    (apply ci-orig r)))

(advice-add #'consult-imenu :around #'my/consult-imenu-around-advice)

(defvar consult--fd-command "fd")
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended t)))
    (when re
      (cons (append
             (list consult--fd-command
                   "-a" "--hidden" "--color=never" "--full-path"
                   (consult--join-regexps re 'extended))
             opts)
            hl))))

(defun consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    ;; (call-interactively #'find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))
    (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))
    ))

;; embark
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   :map embark-file-map
   ("X" . consult-directory-externally)
   ) 

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :hook (;; 令 `*Embark Collect Completion*` 的窗口最大不超过当前 frame 的 40%。
         ;; 不然一个补全窗口占据太多视野功能就有点过了.
         (embark-collect-post-revert . resize-embark-collect-completions))
  
  :config

  (defun resize-embark-collect-completions ()
    (fit-window-to-buffer (get-buffer-window)
                          (floor (* 0.4 (frame-height))) 1))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 (display-buffer-at-bottom)
                 ;; nil
                 (window-parameters ((no-other-window . t)
                                     (mode-line-format . none))))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; 打开特定目录
(defun consult-directory-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (let ((file-or-dir (if (file-directory-p file) file
                      (file-name-directory (expand-file-name file)))))
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (shell-command-to-string (encode-coding-string (replace-regexp-in-string "/" "\\\\"
                                                                                 (format "explorer.exe %s" file-or-dir)) 'gbk))
      (call-process (pcase system-type
                      ('darwin "open")
                      ('cygwin "cygstart")
                      (_ "xdg-open"))
                    nil 0 nil
                    file-or-dir))))

;;打开当前文件的目录
(defun my-open-current-directory ()
  (interactive)
  (consult-directory-externally default-directory))

(with-eval-after-load 'pyim
  (defun eh-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))

  (defun consult-toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'eh-orderless-regexp 'orderless-regexp))
        (progn ()
               (advice-add 'orderless-regexp :around #'eh-orderless-regexp)
               (message "chinese match enabled."))
      (progn ()
             (advice-remove 'orderless-regexp #'eh-orderless-regexp)
             (message "chinese match disabled."))
      ))

  ;; Bind to `vertico-map' or `selectrum-minibuffer-map'
  (define-key vertico-map (kbd "M-;") #'consult-toggle-chinese-search)

  (defun disable-py-search (&optional args)
    (if (advice-member-p #'eh-orderless-regexp 'orderless-regexp)
	(advice-remove 'orderless-regexp #'eh-orderless-regexp)))

  ;; (advice-add 'exit-minibuffer :after #'disable-py-search)
  (add-hook 'minibuffer-exit-hook 'disable-py-search)
  )

;; 更通用的xxx-thing-at-point
(defvar mcfly-commands
  '(consult-line consult-ripgrep consult-git-grep consult-grep consult-ripgrep-or-line))

(defvar mcfly-back-commands
  '(self-insert-command
    ;; 更多其他设置,可以参考: https://github.com/lynnux/.emacs.d/blob/ac552c1/settings/package_extra.el#L1225-L1299
    ))

(defun mcfly-back-to-present ()
  "Self-explained."
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region
	        (goto-char (minibuffer-prompt-end))
          (point-max)))))

(defun mcfly-time-travel ()
  "Insert `thing-at-point'."
  (when (memq this-command mcfly-commands)
    (insert (propertize
             (save-excursion
			         (set-buffer (window-buffer (minibuffer-selected-window)))
			         (or (seq-some
                    (lambda (thing) (thing-at-point thing t))
					          '(region url symbol sexp))
			             "No thing at point"))
             'face 'shadow))
    (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)
    ;; 如果喜欢光标停留在最后一行, 删掉下一行
    (goto-char (minibuffer-prompt-end))
    ))

;; setup code
(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)

;; vertico consult consult-dir marginalia embark embark-consult orderless all-the-icons-completion
(message "vertico configuration loaded.")

(provide 'init-vertico)

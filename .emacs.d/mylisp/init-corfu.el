
;; Auto completion example
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)          ;; Enable auto completion
  ;; bond to "M-SPC" by default.
  ;; (corfu-separator ?\s)    ;; Set to orderless separator
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("M-q" . corfu-quick-complete)
        )
  ;; Enable Corfu only for certain modes. See also `corfu-excluded-modes'.
  ;; :hook (
  ;;        (text-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :init
  ;; (setq corfu-excluded-modes (list #'prog-mode #'org-mode))
  (global-corfu-mode)
)

;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
;;   (add-hook 'eshell-mode-hook
;;    	    (lambda ()
;;    	      (setq completion-in-region-function
;;    		    (kind-icon-enhance-completion
;;    		     completion-in-region-function))))
;;   :custom
;;   (kind-icon-default-style
;;    '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0)))

(dolist (hook (list
               'eshell-mode-hook
               'shell-mode-hook
               ))
  (add-hook hook #'(lambda ()
                    (setq-local corfu-auto nil)
                    (corfu-mode))))

(with-eval-after-load 'corfu
  (require 'kind-all-the-icons)
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
  )

;; https://github.com/minad/cape
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  :bind (
         ;; ("C-c p p" . completion-at-point) ;; capf
         ("M-m" . corfu-other-backend)
         ;; ("C-c p t" . complete-tag)        ;; etags
         ;; ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ;; ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ;;        ("C-c p k" . cape-keyword)
         ;;        ("C-c p s" . cape-symbol)
         ;;        ("C-c p a" . cape-abbrev)
         ;;        ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ;;        ("C-c p w" . cape-dict)
         ;;        ("C-c p \\" . cape-tex)
         ;;        ("C-c p _" . cape-tex)
         ;;        ("C-c p ^" . cape-tex)
         ;;        ("C-c p &" . cape-sgml)
         ;;        ("C-c p r" . cape-rfc1345)
         )
  :init
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
    
  ;; 默认用这三个补全后端
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  ;; https://github.com/50ways2sayhard/tabnine-capf/
  (require 'tabnine-capf)
  ;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)

  ;; Merge the dabbrev, dict and keyword capfs, display candidates together.
  (setq completion-at-point-functions
               (list (cape-super-capf #'cape-dabbrev #'tabnine-completion-at-point) #'cape-file))
 )

(defvar corfu-backend-list
  (list #'cape-symbol #'cape-dabbrev #'cape-keyword #'completion-at-point))
(defun corfu-other-backend ()
  (interactive)
  (let* ((backend (car corfu-backend-list)))
    (ignore-errors (command-execute backend))
    (message "current backend: %s" backend)
    (setq corfu-backend-list (append (cdr corfu-backend-list) (list backend)))))

(defun my/setup-elisp ()
  (setq-local completion-at-point-functions
              `(,(cape-super-capf
                   #'elisp-completion-at-point
                   #'cape-symbol
                   #'cape-dabbrev)
                  cape-file)
                cape-dabbrev-min-length 4))
(add-hook 'emacs-lisp-mode-hook #'my/setup-elisp)

  ;; git clone --depth=1 -b master https://github.com/50ways2sayhard/tabnine-capf.git ~/.emacs.d/lisp/tabnine-capf-master/
  ;; Run M-x corfu-tabnine-install-binary to install the TabNine binary for your system.
;; (use-package tabnine-capf
;;   :hook (kill-emacs . tabnine-capf-kill-process)
;;   :config
;;   (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

;; Optionally use the `orderless' completion style.
;; orderless has already been implemented at init-vertico.
;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; git clone --depth=1 -b master https://github.com/manateelazycat/corfu-english-helper.git ~/.emacs.d/lisp/corfu-english-helper/
(use-package corfu-english-helper
  :after corfu
  :bind
  ("C-c p e" . toggle-corfu-english-helper)
  (:map corfu-map ("M-;" . toggle-corfu-english-helper)))

(message "Corfu configuration loaded.")

;; corfu cape corfu-english-helper corfu-tabnine kind-all-the-icons.

(provide 'init-corfu)

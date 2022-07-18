
;;------------------------------------------------------------
;; novel mode. for epub.
;;------------------------------------------------------------

;;{{{ nov.
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; (setq nov-text-width 80)
;; set it to t to inhibit text filling
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)

(with-eval-after-load 'olivetti
  (add-hook 'nov-mode-hook 'olivetti-mode))

;; (with-eval-after-load 'nov
;;   (defun novel-buffer-face-mode-variable ()
;;     (interactive)
;;     (make-face 'width-font-face)
;;     ;; (set-face-attribute 'width-font-face nil :font "YaHei Consolas Hybrid 16")   ;; FiraCode NF 16
;;     (set-face-attribute 'width-font-face nil :font "微软雅黑 30")
;;     (setq buffer-face-mode-face 'width-font-face)
;;     (buffer-face-mode))
;;     (add-hook 'nov-mode-hook 'novel-buffer-face-mode-variable))

(with-eval-after-load 'nov
  (when (string-equal system-type "windows-nt")
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist))))

;; FIXME: errors while opening `nov' files with Unicode characters
(with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

;;}}}

;; (use-package nov-xwidget
;;   :demand t
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
;;   (setq nov-xwidget-browser-function 'browse-url)
;;   )

(provide 'init-nov)

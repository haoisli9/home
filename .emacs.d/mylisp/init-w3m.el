
;;------------------------------------------------------------
;;{{{ w3m configuration.
(use-package w3m
  :ensure t
  :defer t
  :config
  (setq w3m-use-cookies t
        w3m-coding-system 'utf-8
        ;; w3m-file-coding-system 'utf-8
        ;; w3m-file-name-coding-system 'utf-8
        ;; w3m-input-coding-system 'utf-8
        ;; w3m-output-coding-system 'utf-8
        ;; w3m-terminal-coding-system 'utf-8
        w3m-cookie-accept-bad-cookies t
        w3m-home-page "www.emacs-china.org"
        ;; 设定w3m运行的参数，分别为使用cookie和使用框架
        w3m-command-arguments       '("-F" "-cookie")
        w3m-mailto-url-function     'compose-mail
        w3m-use-toolbar t
        ;; w3m-fill-column 100
        w3m-use-tab     nil
        w3m-confirm-leaving-secure-page nil
        )

  ;;设置显示图片
  (setq w3m-default-display-inline-images t)
  ;;显示图标
  (setq w3m-show-graphic-icons-in-header-line t)
  (setq w3m-show-graphic-icons-in-mode-line t)

  (defun w3m-browse-buffer (&optional buffer)
    "Use w3m browser buffer BUFFER."
    (interactive "bBuffer to browse use w3m: ")
    (setq buffer (get-buffer buffer))
    (unless buffer (setq buffer (current-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (w3m-buffer)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq w3m-current-title (buffer-name)))))

  (defun dired-w3m-find-file ()
    (interactive)
    ;; (require 'w3m)
    (let ((file (dired-get-filename)))
      (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
          (w3m-find-file file))))

  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "W" 'dired-w3m-find-file)))

  ;; (setq w3m-browse-buffer-use-current-dir t)
  ;; (defun w3m-browse-current-buffer ()
  ;;   (interactive)
  ;;   (let (filename)
  ;;     (if w3m-browse-buffer-use-current-dir
  ;;         (setq filename (if buffer-file-name
  ;;                            (expand-file-name
  ;;                             (concat (file-name-nondirectory (buffer-name)) ".htm")
  ;;                             default-directory)
  ;;                          (expand-file-name "w3mtmpfile.htm" default-directory)))
  ;;       (setq filename (concat (make-temp-file "w3m-") ".html")))
  ;;     (unwind-protect
  ;;         (progn
  ;; 	  (set-buffer-file-coding-system 'utf-8 t t)
  ;;           (write-region (point-min) (point-max) filename)
  ;;           (w3m-find-file filename))
  ;;       (delete-file filename))))

  (defun w3m-print-current-url ()
    "Display current url."
    (interactive)
    (w3m-message "%s" (w3m-url-readable-string w3m-current-url)))

  (defun w3m-copy-current-url ()
    "Display the current url in the echo area and put it into `kill-ring'."
    (interactive)
    (when w3m-current-url
      (let ((deactivate-mark nil))
        (kill-new w3m-current-url)
        (w3m-print-current-url))))

  (defun w3m-copy-url-at-point ()
    (interactive)
    (let ((url (w3m-anchor)))
      (if (w3m-url-valid url)
          (kill-new (w3m-anchor))
        (message "No URL at point!"))))

  (add-hook 'w3m-mode-hook
            (lambda ()
              (local-set-key "\C-y" 'w3m-copy-url-at-point)))
  )

(message "w3m configuration loaded.")

;;}}}
(provide 'init-w3m)

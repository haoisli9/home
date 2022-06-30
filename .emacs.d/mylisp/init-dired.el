;; -*- lexical-binding: t -*-

(require 'dired-x)

(setq dired-recursive-copies t)
(setq dired-recursive-deletes t)
(setq dired-recursive-deletes 'always)        ;; don't ask when deleting recursively
(setq dired-recursive-copies 'always)         ;; don't ask when copying recursively
(setq dired-details-hidden-string "[ ... ] ") ;; display string for hiden details
(setq dired-listing-switches "-aBhgGl")       ;; arguments passed to "ls"
(setq directory-free-space-args "-Ph")        ;; arguments passed to free space
;; if there is a Dired buffer displayed in the next
;; window, use its current directory, instead of this Dired buffer’s
;; current directory.
(setq dired-dwim-target t)

;; switch --group-directories-first do not work.
(defun directory-dired-sort ()
  "dired-mode中让目录显示在文件前"
  (save-excursion
    (let
        (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and
   (featurep 'xemacs)
   (fboundp 'dired-insert-set-properties)
   (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'directory-dired-sort)
(add-hook 'dired-lood-hook 'directory-dired-sort)

(defun dired-open-web-file ()
  "open file with web browser."
  (interactive)
  ;; use arg passed in or find name of current line
  (let ((name (dired-get-filename nil t)))
    ;; lihao add
    (if (eq name nil)
        (error (format "No file exist here.")))
    ;; add end
    (eww-open-file name))
  )

(defun dired-open-and-play-gif-image ()
  "Open and play GIF image `FILE' in Emacs buffer."
  (interactive)
  (let ((gif-image (create-image (dired-get-filename nil t)))
        (tmp-buf (get-buffer-create "*GIF image animation*")))
    (switch-to-buffer tmp-buf)
    (erase-buffer)
    (insert-image gif-image)
    (image-animate gif-image nil t)
    (local-set-key (kbd "q") 'bury-buffer)
    ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (setq dired-omit-files "^#\\|^\\..*\\|semantic.cache")
            (setq dired-omit-extensions '("CVS/" "semantic.cache" ".git/" ".desktop" "~" ".log" ".bak" ".elc" ".el~" ".obj"))
            (setq dired-omit-mode nil)

            (make-local-variable  'dired-sort-map)
            (setq dired-sort-map (make-sparse-keymap))
            (define-key dired-mode-map "s" dired-sort-map)
            ;; (define-key dired-mode-map (kbd "<M-up>") 'dired-up-directory)
            (define-key dired-sort-map "s"
                        #'(lambda () "sort by Size"
                            (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
            (define-key dired-sort-map "x"
                        #'(lambda () "sort by eXtension"
                            (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
            (define-key dired-sort-map "t"
                        #'(lambda () "sort by Time"
                            (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
            (define-key dired-sort-map "n"
                        #'(lambda () "sort by Name"
                            (interactive) (dired-sort-other (concat dired-listing-switches ""))))
            (define-key dired-mode-map "W" 'dired-open-web-file)           
            ))

(use-package dired-single
  :ensure t
  :config
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
    ;; <add other stuff here>
    (define-key dired-mode-map [return] 'dired-single-buffer)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
    ;; original is dired-mode-map "^"
    (define-key dired-mode-map "^"
                (function
                 (lambda nil (interactive) (dired-single-buffer "..")))))

  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))

  (eval-after-load "dired"
    '(progn
       (define-key dired-mode-map "X" 'dired-w32-browser)
       (define-key dired-mode-map "O" 'dired-w32explore)))
  )

;; open external app use Powershell.
(defun open-with-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in Emacs Lisp, if @FNAME is given, open that."
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'"
                    (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))
         $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath)
           (let ((process-connection-type nil))
             (start-process "" nil "xdg-open" $fpath)))
         $file-list))))))

;; open app in wsl.
;;;###autoload
(defmacro wsl--open-with (id &optional app dir)
  `(defun ,(intern (format "wsl/%s" id)) ()
     (interactive)
     (wsl-open-with ,app ,dir)))

(defun wsl-open-with (&optional app-name path)
  "Send PATH to APP-NAME on WSL."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "%s `wslpath -w %s`" 
                          (shell-quote-argument app-name) path)))
    (shell-command-to-string command)))

(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)

(message "dired single loaded.")

(provide 'init-dired)

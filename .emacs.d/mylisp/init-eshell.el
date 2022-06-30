;; -*- Emacs-Lisp -*-
;; eshell mode utilitys
;; lihao

(use-package eshell-z
  :ensure t
  :defer t)

(use-package eshell-up
  :ensure t
  :defer t)

(use-package eshell-syntax-highlighting
  :ensure t
  :defer t)

(defun eshell/eshell-view-file (file)
    "A version of `view-file' which properly respects the eshell prompt."
    (interactive "fView file: ")
    (unless (file-exists-p file) (error "%s does not exist" file))
    (let ((had-a-buf (get-file-buffer file))
          (buffer (find-file-noselect file)))
      (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
              'special)
          (progn
            (switch-to-buffer buffer)
            (message "Not using View mode because the major mode is special"))
        (let ((undo-window (list (window-buffer) (window-start)
                                 (+ (window-point)
                                    (length (funcall eshell-prompt-function))))))
          (switch-to-buffer buffer)
          (view-mode-enter (cons (selected-window) (cons nil undo-window))
                           'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
    the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (tyler-eshell-view-file file)
          (goto-line line))
      (eshell/eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

(defun eshell/info (&optional subject)
  "Invoke `info', optionally opening the Info system to SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (if (not (null subject))
        (let ((node-exists (ignore-errors (Info-menu subject))))
          (if (not node-exists)
              (format "No menu item `%s' in node `(dir)Top'." subject))))))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(add-hook 'eshell-mode-hook
          #'(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))


(defun eshell/ff (&rest args)
  "Invoke `find-file' on the file.
    \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defun eshell/fop (FILE)
  "Invoke (w32-shell-execute \"Open\" FILE) and substitute slashes for backslashes"
  (w32-shell-execute "Open" (substitute ?\\ ?/ (expand-file-name FILE))))

(defun eshell/cls()
  "to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (run-with-idle-timer
             1 nil
             #'(lambda ()
                 ;; (require 'eshell-did-you-mean)
                 ;; (eshell-did-you-mean-setup)
                 (require 'eshell-up)
                 (defalias 'eshell/up 'eshell-up)
                 (defalias 'eshell/up-peek 'eshell-up-peek)
                 (require 'eshell-z)
                 (eshell-syntax-highlighting-global-mode +1)
                 ))))

(defun eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell-sudo-toggle ()
  "Toggle sudo with current command."
  (interactive)
  (save-excursion
    (let ((commands (buffer-substring-no-properties
                     (progn (eshell-bol) (point)) (point-max))))
      (if (string-match-p "^sudo " commands)
          (progn
            (eshell-bol)
            (while (re-search-forward "sudo " nil t)
              (replace-match "" t nil)))
        (progn
          (eshell-bol)
          (insert "sudo ")
          )))))

(defun eshell-reload-shell-history ()
  (with-temp-message ""
    (cond ((string-equal shell-file-name "/bin/bash")
           (shell-command "history -r"))
          ((string-equal shell-file-name "/bin/zsh")
           (shell-command "fc -W; fc -R")))))

(defun eshell-parse-bash-history ()
  "Parse the bash history."
  (if (file-exists-p "~/.bash_history")
      (let (collection bash_history)
        (eshell-reload-shell-history)
        (setq collection
              (nreverse
               (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                               (buffer-string))
                             "\n"
                             t)))
        (when (and collection (> (length collection) 0)
                   (setq bash_history collection))
          bash_history))
    nil))

(defun eshell-parse-zsh-history ()
  "Parse the bash history."
  (if (file-exists-p "~/.zsh_history")
      (let (collection zsh_history)
        (eshell-reload-shell-history)
        (setq collection
              (nreverse
               (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                               (replace-regexp-in-string "^:[^;]*;" "" (buffer-string)))
                             "\n"
                             t)))
        (when (and collection (> (length collection) 0)
                   (setq zsh_history collection))
          zsh_history))
    nil))

(defun eshell-parse-shell-history ()
  "Parse history from eshell/bash/zsh/ ."
  (delete-dups
   (mapcar
    (lambda (str)
      (string-trim (substring-no-properties str)))
    (append
     (ring-elements eshell-history-ring)
     (eshell-parse-bash-history)
     (eshell-parse-zsh-history)))))

(defun eshell-search-history ()
  "Interactive search eshell history."
  (interactive)
  (save-excursion
    (let* ((start-pos (eshell-beginning-of-input))
           (input (eshell-get-old-input))
           (all-shell-history (eshell-parse-shell-history)))
      (let* ((command (ido-completing-read "Search history: " all-shell-history)))
        (eshell-kill-input)
        (insert command)
        )))
  ;; move cursor to eol
  (end-of-line))

(defcustom eshell-clear-buffer-key "C-l"
  "The keystroke for clear buffer."
  :type 'string
  :group 'eshell)

(defcustom eshell-sudo-toggle-key "C-S-l"
  "The keystroke for toggle sudo"
  :type 'string
  :group 'eshell)

(defcustom eshell-search-history-key "M-'"
  "The keystroke for search history"
  :type 'string
  :group 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd eshell-clear-buffer-key) 'eshell-clear-buffer)
            (define-key eshell-mode-map (kbd eshell-sudo-toggle-key) 'eshell-sudo-toggle)
            (define-key eshell-mode-map (kbd eshell-search-history-key) 'eshell-search-history)
            ))

(defun eshell-emacs (&rest args)
  "Open a file in Emacs with ARGS, Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defalias 'eshell/e 'eshell-emacs)

(defun eshell-unpack (file &rest args)
  "Unpack FILE with ARGS."
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command(concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))

(defalias 'eshell/unpack 'eshell-unpack)

;;---------------------------------------------------------------------
;; get from eshell-prompt-extras.
(defcustom epe-fish-path-max-len 30
  "Default maximum length for path in `epe-fish-path'."
  :group 'epe
  :type 'number)

;; https://www.emacswiki.org/emacs/EshellPrompt
(defun epe-fish-path (path &optional max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (max-len (or max-len epe-fish-path-max-len))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

;; Synchronal buffer name by directory change.
(defun eshell-sync-dir-buffer-name ()
  "Change aweshell buffer name by directory change."
  (when (equal major-mode 'eshell-mode)
    (rename-buffer (format "eshell: %s" (epe-fish-path default-directory))
                   t)))

(add-hook 'eshell-directory-change-hook #'eshell-sync-dir-buffer-name)
(add-hook 'eshell-mode-hook #'eshell-sync-dir-buffer-name)


;; Make cat with syntax highlight.
(defun eshell-cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(advice-add 'eshell/cat :override #'eshell-cat-with-syntax-highlight)

;; Alert user when background process finished or aborted.
(defface eshell-alert-buffer-face
  '((t (:foreground "#ff2d55" :bold t)))
  "Alert buffer face."
  :group 'eshell)

(defface eshell-alert-command-face
  '((t (:foreground "#ff9500" :bold t)))
  "Alert command face."
  :group 'eshell)

(defun eshell-command-alert (process status)
  "Send `alert' with severity based on STATUS when PROCESS finished."
  (let* ((cmd (process-command process))
         (buffer (process-buffer process))
         (msg (replace-regexp-in-string "\n" " " (string-trim (format "%s: %s" (mapconcat 'identity cmd " ")  status))))
         (buffer-visible (member buffer (mapcar #'window-buffer (window-list)))))
    (unless buffer-visible
      (message "%s %s"
               (propertize (format "[eshell Alert] %s" (string-remove-prefix "eshell: " (buffer-name buffer))) 'face 'eshell-alert-buffer-face)
               (propertize msg 'face 'eshell-alert-command-face)))))

(add-hook 'eshell-kill-hook #'eshell-command-alert)

(defun my/comint-font-lock-off-if-long-line (string)
  (when (bound-and-true-p font-lock-mode)
    (let ((long-line-found nil))
      (mapc #'(lambda (line)
                (if (> (length line) so-long-threshold)
                    (setq long-line-found t)))
            (split-string string "\n"))
      (when long-line-found
        (font-lock-mode -1)
        (message "disable `font-lock-mode' because of long line found in buffer '%s'" (buffer-name))))))

(defun my/shell--ask-if-multiline-input (input)
  "Avoid accidentally INPUT too many commands to shell."
  (let ((p1 (search "\n" input))
        (p2 (search "\n" input :from-end t))
        (buffer nil))
    (when (and (not (eq nil p1)) (not (eq nil p2)) (not (eq p1 p2)))
      (setq buffer (get-buffer-create "*Multiple Line Shell Input*"))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert input)
        (read-only-mode t)
        (let ((o (make-overlay (point-min) (point-max) buffer nil t)))
          (overlay-put o 'face `(:background "#000" :foreground "#FFF")))
        (display-buffer buffer))
      (unless (yes-or-no-p "Input multiple line to shell:")
        (kill-buffer buffer)
        (error "Input multiple line to shell aborted"))
      (kill-buffer buffer))))

(add-to-list 'comint-output-filter-functions 'my/comint-font-lock-off-if-long-line)
(add-to-list 'comint-input-filter-functions 'my/shell--ask-if-multiline-input)

;; ====================
;; insert date and time

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-date-format "%Y %b %d %A"
  "Format of date to insert with `insert-current-date' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-date ()
  "insert the current date and time into current buffer.
Uses `current-date-format' for the formatting the date/time."
       (interactive)
       ;; (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

(message "eshell configuration loaded.")

(provide 'init-eshell)

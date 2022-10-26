
(use-package citre
  :ensure t
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-t") 'xref-pop-marker-stack)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set this if you want to always use one location to create a tags file.
   citre-tags-file-global-cache-dir "~/.cache/tags/"
   citre-default-create-tags-file-location 'global-cache
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   citre-auto-enable-citre-mode-modes '(prog-mode)
   citre-update-tags-file-when-no-definitions t
   ;; Set this if readtags is not in your path.
   ;; citre-readtags-program "/path/to/readtags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   ;; citre-project-root-function #'projectile-project-root
   ))

;;------------------------------------------------------------
;; citre的ivy后端，不过用(setq completing-read-function #'ivy-completing-read)
;; 可以把所有的completing动作按ivy执行；
;; (cl-defun citre-jump-ivy-read (definitions symbol)
;;   "Select an element ivy-read' interface.  See
;; `citre-jump-select-definition-function' for the use of this function."
;;   (pcase (length definitions)
;;     (1 (car definitions))
;;     (_ (ivy-read (format "%s: " symbol) definitions
;;                  :caller 'citre-jump-ivy-read))))
;; (setq citre-jump-select-definition-function  #'citre-jump-ivy-read)

(if 0
    (progn
;;----------------------------------------------------------------
(use-package citre-global
  :ensure nil
  :defer 3
  :init
  (add-hook 'citre-mode-hook
            (defun require-citre-global ()
              (require 'citre-global)
              (remove-hook 'citre-mode-hook #'require-citre-global)))

  (setq citre-gtags-args '("--compact" "--objdir" "--gtagslabel" "native-pygments" "--gtagsconf" "~/.globalrc"))
  
  (global-set-key (kbd "C-x c d") 'citre-jump-to-definition)
  (global-set-key (kbd "C-x c r") 'citre-jump-to-reference)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek-references)
  (global-set-key (kbd "C-x c U") 'citre-global-update-database)
  (with-eval-after-load 'citre-peek
    (define-key citre-peek-keymap (kbd "M-l r") 'citre-peek-through-references))

  :config
  
  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (let* ((xref-prompt-for-identifier nil))
               (call-interactively #'xref-find-definitions)))))
  ;; (error (call-interactively #'lsp-find-definition))))
  ;; (global-set-key (kbd "M-]") 'citre-jump+)
  
;;;###autoload
  (defun citre-jump-to-definition ()
    "Jump to the definition of the symbol at point. This uses the `citre-jump' UI."
    (interactive)
    (let* ((marker (point-marker))
           (symbol (citre-tags-get-symbol-default))
           (definitions
            (citre-global-get-definitions symbol))
           (root (funcall citre-project-root-function)))
      (when (null definitions) 
        (user-error "Can't find definitions for %s" symbol))
      (citre-jump-show symbol definitions marker root)))

  (defun citre-global-get-definitions (&optional name case-fold start-file)
    "Get definition tags using global.
When NAME is non-nil, get definitions of NAME, otherwise get
definitions of the symbol under point.

When CASE-FOLD is non-nil, do case-insensitive matching.

By default, the result is sort by nearness (see the `--nearness'
option in global) start from the current file or directory.
START-FILE can be nil to keep this behavior, be a string to
specify the start file, or be a symbol (like `alpha') to use the
default alphabetical sort.

Global program is run under current `default-directory'."
    (let ((name (or name (citre-tags-get-symbol-default)))
          (start-file
           (pcase start-file
             ('nil (with-selected-window (or (minibuffer-selected-window)
                                             (selected-window))
                     (or (buffer-file-name) default-directory)))
             ((pred stringp) start-file)
             ((pred symbolp) nil))))
      (mapcar (lambda (line)
                (citre-global--parse-definition-line line default-directory name))
              (citre-global--get-definition-lines name case-fold start-file))))

  (defun citre-global--parse-definition-line (line rootdir &optional name)
    "Parse a LINE in the output of global.
ROOTDIR is the working directory when running the global command.
The return value is a tag contains `ext-abspath', `line', and
`extras' field.  If NAME is given, is used as the `name' field.
The value of `extras' field is \"definition\"."
    (if (string-match (rx line-start
                          (group-n 1 (+ (not (any ":"))))
                          ":"
                          (group-n 2 (+ num))
                          ":")
                      line)
        (let ((path (match-string 1 line))
              (linum (match-string 2 line))
              (tag (make-hash-table :test #'eq)))
          ;; We don't record the pattern field since it's generate in real time,
          ;; so it can't be used to deal with file updates.
          (setq path (expand-file-name (citre-global--read-path path) rootdir))
          (when name (puthash 'name (substring-no-properties name) tag))
          (puthash 'ext-abspath path tag)
          (puthash 'line linum tag)
          (puthash 'extras "definition" tag)
          tag)
      (error "Invalid LINE")))

  (defvar citre-global--find-definitions-args
    '("--color=never"
      "--encode-path= :"
      "--result=grep"
      "--literal"
      "--definition"
      "--symbol")
    "Arguments used for finding definitions using global.
`citre-global--get-definition-lines' may add more arguments on
these.")

  (defun citre-global--get-definition-lines (name &optional case-fold start-file)
    "Find definitions to NAME using global and return the outputed lines.
When CASE-FOLD is non-nil, do case-insensitive matching.  When
START-FILE is non-nil, sort the result by nearness (see the help
message of global) start from START-FILE."
    (let* ((name (when name (substring-no-properties name)))
           inhibit-message
           args)
      (when case-fold (push "--ignore-case" args))
      ;; Global doesn't know how to expand "~", so we need to expand START-FILE.
      (when start-file (push (concat "--nearness=" (expand-file-name start-file))
                             args))
      (setq args (append args citre-global--find-definitions-args
                         (list "--" name)))
      (citre-global--get-output-lines args )))

  (defun citre-global--peek-get-symbol-and-definitions ()
    "Return the symbol under point and definitions of it.
This is similar to `citre-peek--get-symbol-and-definitions'."
    (citre-peek--hack-buffer-file-name
     (let* ((symbol (or (citre-tags-get-symbol-default)
                        (user-error "No symbol at point")))
            (definitions (or (citre-global-get-definitions symbol)
                             (user-error "Can't find definitions for %s"
                                         symbol))))
       (cons symbol definitions))))

;;;;; Commands

;;;###autoload
  (defun citre-peek-definitions (&optional buf point)
    "Peek the definitions of the symbol in BUF and POINT.
When BUF or POINT is nil, it's set to the current buffer and
point."
    (interactive)
    (let* ((buf (or buf (current-buffer)))
           (point (or point (point)))
           (symbol-refs (save-excursion
                          (with-current-buffer buf
                            (goto-char point)
                            (citre-global--peek-get-symbol-and-definitions))))
           (marker (if (buffer-file-name) (point-marker))))
      (citre-peek-show (car symbol-refs) (cdr symbol-refs) marker)))

;;;###autoload
  (defun citre-ace-peek-definitions ()
    "Peek the definitions of a symbol on screen using ace jump.
This is similar to `citre-ace-peek'."
    (interactive)
    (when-let ((pt (citre-ace-pick-point)))
      (citre-peek-definitions (current-buffer) pt)))

;;;###autoload
  (defun citre-peek-through-definitions ()
    "Peek through a symbol in current peek window for its definitions."
    (interactive)
    (when-let* ((buffer-point (citre-ace-pick-point-in-peek-window))
                (symbol-defs
                 (save-excursion
                   (with-current-buffer (car buffer-point)
                     (goto-char (cdr buffer-point))
                     (citre-global--peek-get-symbol-and-defintions)))))
      (citre-peek-make-current-def-first)
      (citre-peek--make-branch (car symbol-defs) (cdr symbol-defs))))

;;;; `xref-find-definitions' integration

  ;; (defun citre-xref--global-find-definition (symbol)
  ;;   "Return the xref object of definitions of SYMBOL."
  ;;   (mapcar #'citre-xref--make-object
  ;;           (citre-global-get-definitions symbol)))

  ;; (cl-defmethod xref-backend-definitions ((_backend (eql citre)) symbol)
  ;;   "Define method for xref to find definition of SYMBOL."
  ;;   (citre-xref--global-find-definition symbol))
  )
))

(provide 'init-citre)

;;; -*- lexical-binding: t -*-
;;
;; Tips,
;; - Use `pop-tag-mark' to jump back.
;;
;; - The grep program path on Native Windows Emacs uses either forward slash or
;;   backward slash.  Like "C:/rg.exe" or "C:\\\\rg.exe".
;;   If grep program path is added to environment variable PATH, you don't need
;;   worry about slash problem.
;;
;; - Add below code into "~/.emacs" to AUTOMATICALLY update tags file:
;;
;;   ;; Don't ask before reloading updated tags files
;;   (setq tags-revert-without-query t)
;;   ;; NO warning when loading large tag files
;;   (setq large-file-warning-threshold nil)
;;   (add-hook 'prog-mode-hook
;;     (lambda ()
;;       (add-hook 'after-save-hook
;;                 'counsel-etags-virtual-update-tags 'append 'local)))
;;
;; - `counsel-etags-extra-tags-files' contains extra tags files to parse.
;;   Set it like,
;;     (setq counsel-etags-extra-tags-files
;;           '("./TAGS" "/usr/include/TAGS" "$PROJ1/include/TAGS"))
;;
;;   Files in `counsel-etags-extra-tags-files' should have symbols with absolute path only.
;;
;; - You can set up `counsel-etags-ignore-directories' and `counsel-etags-ignore-filenames',
;;   (with-eval-after-load 'counsel-etags
;;      ;; counsel-etags-ignore-directories does NOT support wildcast
;;      (push "build_clang" counsel-etags-ignore-directories)
;;      (push "build_clang" counsel-etags-ignore-directories)
;;      ;; counsel-etags-ignore-filenames supports wildcast
;;      (push "TAGS" counsel-etags-ignore-filenames)
;;      (push "*.json" counsel-etags-ignore-filenames))
;;
;;  - Rust programming language is supported.
;;    The easiest setup is to use ".dir-locals.el".
;;   in root directory.  The content of .dir-locals.el" is as below,
;;
;;   ((nil . ((counsel-etags-update-tags-backend . (lambda (src-dir) (shell-command "rusty-tags Emacs")))
;;            (counsel-etags-tags-file-name . "rusty-tags.emacs"))))
;;
;;  - The ignore files (.gitignore, etc) are automatically detected and append to ctags
;;    cli options as "--exclude="@/ignore/file/path".
;;    Set `counsel-etags-ignore-config-files' to nil to turn off this feature.
;;
;;  - If base configuration file "~/.ctags.exuberant" exists, it's used to
;;    generate "~/.ctags" automatically.
;;    "~/.ctags.exuberant" is Exuberant Ctags format, but the "~/.ctags" could be
;;    Universal Ctags format if Universal Ctags is used.
;;    You can customize `counsel-etags-ctags-options-base' to change the path of
;;    base configuration file.

;;  - You can also use native imenu with below setup,
;;      (setq imenu-create-index-function
;;            'counsel-etags-imenu-default-create-index-function)
;;
;; See https://github.com/redguardtoo/counsel-etags/ for more tips.

;;; Code:

(require 'etags)
(require 'cl-lib)
(require 'find-file)
(require 'tramp nil t)
(require 'browse-url)

(defgroup counsel-etags nil
  "Complete solution to use ctags."
  :group 'tools)

(defcustom counsel-etags-browse-url-function 'browse-url-generic
  "The function to open url in tags file."
  :group 'counsel-etags
  :type 'function)

(defcustom counsel-etags-ignore-config-files
  '(".gitignore"
    ".hgignore"
    "~/.ignore")
  "Path of configuration file which specifies files that should ignore.
Path is either absolute path or relative to the tags file."
  :group 'counsel-etags
  :type '(repeat string))

(defcustom counsel-etags-command-to-scan-single-code-file nil
  "Shell Command to scan single file.
If it's nil, a command using ctags is automatically created."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-extra-tags-files nil
  "List of extra tags files to load.  They are not updated automatically.

A typical format is

    (\"./TAGS\" \"/usr/include/TAGS\" \"$PROJECT/*/include/TAGS\")

Environment variables can be inserted between slashes (`/').
They will be replaced by their definition.  If a variable does
not exist, it is replaced (silently) with an empty string.

Symbol location inside tags file should use absolute path.
A CLI to create tags file:

  find /usr/include | ctags -e -L -"
  :group 'counsel-etags
  :type '(repeat 'string))

(defcustom counsel-etags-stop-auto-update-tags nil
  "If t, tags will not be updated automatically."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-use-ripgrep-force nil
  "Force use ripgrep as grep program.
If rg is not in $PATH, then it need be defined in `counsel-etags-grep-program'."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-ripgrep-default-options
  ;; @see https://github.com/BurntSushi/ripgrep/issues/501
  ;; some shell will expand "/" to a complete file path.
  ;; so try to avoid "/" in shell
  (format "-n -M 1024 --no-heading --color never -s %s"
          (if (eq system-type 'windows-nt) "--path-separator \"\x2f\"" ""))
  "Default options passed to ripgrep command line program."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-grep-extra-arguments ""
  "Extra arguments passed to grep program."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-convert-grep-keyword 'identity
  "Convert keyword to grep to new regex to feed into grep program.

Here is code to enable grepping Chinese using pinyinlib,

  (unless (featurep 'pinyinlib) (require 'pinyinlib))
  (setq counsel-etags-convert-grep-keyword
         (lambda (keyword)
           (if (and keyword (> (length keyword) 0))
               (pinyinlib-build-regexp-string keyword t)
             keyword)))"
  :group 'counsel-etags
  :type 'function)

(defcustom counsel-etags-fallback-grep-function #'counsel-etags-grep
  "The fallback grep function if tag can't be found at first.
Hope grep can find something.

Below parameters is passed to the function.
The parameter \"keyword\" is the search keyword.
The parameter \"hint\" is the hint for grep ui.
The parameter \"root\" is the project root directory."
  :group 'counsel-etags
  :type 'function)

(defcustom counsel-etags-can-skip-project-root nil
  "If t, scanning project root is optional."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-find-tag-name-function 'counsel-etags-find-tag-name-default
  "The function to use to find tag name at point.
It should be a function that takes no arguments and returns an string.
If it returns nil, the `find-tag-default' is used.

The function `counsel-etags-word-at-point' could be used find word at point.
The definition of word is customized by the user."
  :group 'counsel-etags
  :type 'function)

(defcustom counsel-etags-major-modes-to-strip-default-tag-name
  '(org-mode
    markdown-mode)
  "Major mode where default tag name need be stripped.
It's used by `counsel-etags-find-tag-name-default'."
  :group 'counsel-etags
  :type '(repeat 'sexp))

(defcustom counsel-etags-ignore-directories
  '(;; VCS
    ".git"
    ".svn"
    ".cvs"
    ".bzr"
    ".hg"
    ;; project misc
    "bin"
    "dist"
    "fonts"
    "images"
    ;; Mac
    ".DS_Store"
    ;; html/javascript/css
    ".npm"
    ".tmp" ; TypeScript
    ".sass-cache" ; SCSS/SASS
    ".idea"
    "node_modules"
    "bower_components"
    ;; python
    ".tox"
    ;; vscode
    ".vscode"
    ;; emacs
    ".cask")
  "Ignore directory names."
  :group 'counsel-etags
  :type '(repeat 'string))

(defcustom counsel-etags-ignore-filenames
  '(;; VCS
    ;; project misc
    "*.log"
    ;; rusty-tags
    "rusty-tags.vim"
    "rusty-tags.emacs"
    ;; Ctags
    "tags"
    "TAGS"
    ;; compressed
    "*.tgz"
    "*.gz"
    "*.xz"
    "*.zip"
    "*.tar"
    "*.rar"
    ;; Global/Cscope
    "GTAGS"
    "GPATH"
    "GRTAGS"
    "cscope.files"
    ;; html/javascript/css
    "*bundle.js"
    "*min.js"
    "*min.css"
    ;; Images
    "*.png"
    "*.jpg"
    "*.jpeg"
    "*.gif"
    "*.bmp"
    "*.tiff"
    "*.ico"
    ;; documents
    "*.doc"
    "*.docx"
    "*.xls"
    "*.ppt"
    "*.pdf"
    "*.odt"
    ;; C/C++
    ".clang-format"
    "*.obj"
    "*.so"
    "*.o"
    "*.a"
    "*.ifso"
    "*.tbd"
    "*.dylib"
    "*.lib"
    "*.d"
    "*.dll"
    "*.exe"
    ;; Java
    ".metadata*"
    "*.class"
    "*.war"
    "*.jar"
    ;; Emacs/Vim
    "*flymake"
    "#*#"
    ".#*"
    "*.swp"
    "*~"
    "*.elc"
    ;; Python
    "*.pyc")
  "Ignore file names.  Wildcast is supported."
  :group 'counsel-etags
  :type '(repeat 'string))

(defcustom counsel-etags-project-file '("TAGS" "tags" ".svn" ".hg" ".git")
  "The file/directory used to locate project root directory.
You can set up it in \".dir-locals.el\"."
  :group 'counsel-etags
  :type '(repeat 'string))

(defcustom counsel-etags-project-root nil
  "Project root directory.  The directory automatically detects if it's nil."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-tags-file-name "TAGS"
  "Tags file name."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-ctags-options-file "~/.ctags"
  "File to read options from, like \"~/.ctags\".
Universal Ctags won't read options from \"~/.ctags\" by default.
So we force Universal Ctags to load \"~/.ctags\".

Exuberant Ctags can NOT read option file \".ctags\" through cli option.

So we use Emacs Lisp to load \"~.ctags\".

Use file name \"ctags.cnf\" instead \".ctags\" if it needs change.

Universal Ctags does NOT have this bug.

Please do NOT exclude system temporary folder in ctags configuration
because imenu functions need create and scan files in this folder."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-ctags-options-base "~/.ctags.exuberant"
  "Ctags configuration base use by all Ctags implementations.
Universal Ctags converts it to `counsel-etags-ctags-options-file'.
If it's nil, nothing happens."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-imenu-excluded-names
  '("this"
    "if"
    "unless"
    "import"
    "const"
    "public"
    "static"
    "private"
    "for"
    "while"
    "export"
    "declare"
    "let")
  "Some imenu items should be excluded by name."
  :group 'counsel-etags
  :type '(repeat 'string))

(defcustom counsel-etags-imenu-excluded-types
  '("variable"
    "constant")
  "Some imenu items should be excluded by type.
Run 'ctags -x some-file' to see the type in second column of output."
  :group 'counsel-etags
  :type '(repeat 'string))

(defcustom counsel-etags-candidates-optimize-limit 256
  "Sort candidates if its size is less than this variable's value.
Candidates whose file path has Levenshtein distance to current file/directory.
You may set it to nil to disable re-ordering for performance reason.
If `string-distance' exists, sorting happens and this variable is ignored."
  :group 'counsel-etags
  :type 'integer)

(defcustom counsel-etags-sort-grep-result-p t
  "Sort grep result by string distance."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-max-file-size 512
  "Ignore files bigger than `counsel-etags-max-file-size' kilobytes.
This option is ignored if GNU find is not installed."
  :group 'counsel-etags
  :type 'integer)

(defcustom counsel-etags-after-update-tags-hook nil
  "Hook after tags file is actually updated.
The parameter of hook is full path of the tags file."
  :group 'counsel-etags
  :type 'hook)

(defcustom counsel-etags-org-property-name-for-grepping
  "GREP_PROJECT_ROOT"
  "Org node property name for get grepping project root."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-org-extract-project-root-from-node-p
  t
  "Extract project root directory from org node."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-update-interval 300
  "The interval (seconds) to update tags file.
Used by `counsel-etags-virtual-update-tags'.
Default value is 300 seconds."
  :group 'counsel-etags
  :type 'integer)

(defcustom counsel-etags-ctags-program nil
  "Ctags Program.  Ctags is automatically detected if it's nil.
You can set it to the full path of the executable."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-grep-program nil
  "Grep program.  Program is automatically detected if it's nil.
You can set it to the full path of the executable."
  :group 'counsel-etags
  :type 'string)

(defcustom counsel-etags-quiet-when-updating-tags t
  "Be quiet when updating tags."
  :group 'counsel-etags
  :type 'boolean)

(defcustom counsel-etags-update-tags-backend
  'counsel-etags-scan-dir-internal
  "A user-defined function to update tags file during auto-updating.
The function has same parameters as `counsel-etags-scan-dir-internal'."
  :group 'counsel-etags
  :type 'sexp)

(defconst counsel-etags-no-project-msg
  "No project found.  You can create tags file using `counsel-etags-scan-code'.
So we don't need the project root at all.
Or you can set up `counsel-etags-project-root'."
  "Message to display when no project is found.")

(defvar counsel-etags-debug nil "Enable debug mode.")

;; Timer to run auto-update tags file
(defvar counsel-etags-timer nil "Internal timer.")

(defvar counsel-etags-tags-file-history nil
  "Tags files history.  Recently accessed file is at the top of history.
The file is also used by tags file auto-update process.")

(defvar counsel-etags-cache nil "Cache of multiple tags files.")

(declare-function outline-up-heading "outline")
(declare-function org-entry-get "outline")

(defun counsel-etags-org-entry-get-project-root ()
  "Get org property from current node or parent node recursively."
  (when (and (derived-mode-p 'org-mode)
             counsel-etags-org-extract-project-root-from-node-p)
    (unless (featurep 'org) (require 'org))
    (unless (featurep 'outline) (require 'outline))
    (let* ((pos (point))
           (prop-name counsel-etags-org-property-name-for-grepping)
           (rlt (org-entry-get pos prop-name))
           (loop t)
           old-pos)

      (save-excursion
        (unless rlt
          (setq old-pos (point))
          (condition-case nil (outline-up-heading 1) (error nil))
          (while loop
            (cond
             ((or (setq rlt (org-entry-get (point) prop-name))
                  (eq (point) old-pos))
              (setq loop nil))
             (t
              (setq old-pos (point))
              (condition-case nil (outline-up-heading 1) (error nil)))))
          (goto-char pos))
        rlt))))

(defun counsel-etags-win-path (executable-name drive)
  "Guess EXECUTABLE-NAME's full path in Cygwin on DRIVE."
  (let* ((path (concat drive ":\\\\cygwin64\\\\bin\\\\" executable-name ".exe")))
    (if (file-exists-p path) path)))

;;;###autoload
(defun counsel-etags-guess-program (executable-name)
  "Guess path from its EXECUTABLE-NAME on Windows.
Return nil if it's not found."
  (cond
   ((file-remote-p default-directory)
    ;; Assume remote server has already added EXE into $PATH!
    executable-name)
   ((eq system-type 'windows-nt)
    (or (counsel-etags-win-path executable-name "c")
        (counsel-etags-win-path executable-name "d")
        (counsel-etags-win-path executable-name "e")
        (counsel-etags-win-path executable-name "f")
        (counsel-etags-win-path executable-name "g")
        (counsel-etags-win-path executable-name "h")
        executable-name))
   (t
    (if (executable-find executable-name) (executable-find executable-name)))))

;;;###autoload
(defun counsel-etags-version ()
  "Return version."
  (message "1.10.1"))

;;;###autoload
(defun counsel-etags-get-hostname ()
  "Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified"
  (with-temp-buffer
    (shell-command "hostname" t)
    (goto-char (point-max))
    (delete-char -1)
    (buffer-string)))

(defun counsel-etags-get-tags-file-path (dir)
  "Get full path of tags file from DIR."
  (and dir (expand-file-name (concat (file-name-as-directory dir)
                                  counsel-etags-tags-file-name))))

(defun counsel-etags-locate-tags-file ()
  "Find tags file: Search `counsel-etags-tags-file-history' and parent directories."
  (counsel-etags-get-tags-file-path (locate-dominating-file default-directory
                                                            counsel-etags-tags-file-name)))

(defun counsel-etags-tags-file-directory ()
  "Directory of tags file."
  (let* ((f (counsel-etags-locate-tags-file)))
    (if f (file-name-directory (expand-file-name f)))))

(defun counsel-etags-locate-project ()
  "Return the root of the project."
  (let* ((tags-dir (if (listp counsel-etags-project-file)
                       (cl-some (apply-partially 'locate-dominating-file
                                                 default-directory)
                                counsel-etags-project-file)
                     (locate-dominating-file default-directory
                                             counsel-etags-project-file)))
         (project-root (or counsel-etags-project-root
                           (and tags-dir (file-name-as-directory tags-dir)))))
    (or project-root
        (progn (message counsel-etags-no-project-msg)
               nil))))

(defun counsel-etags-add-tags-file-to-history (tags-file)
  "Add TAGS-FILE to the top of `counsel-etags-tags-file-history'."
  (let* ((file (expand-file-name tags-file)))
    (setq counsel-etags-tags-file-history
          (delq nil (mapcar
                     (lambda (s)
                       (unless (string= file (expand-file-name s)) s))
                     counsel-etags-tags-file-history)))
    (push tags-file counsel-etags-tags-file-history)))

;;;###autoload
(defun counsel-etags-async-shell-command (command tags-file)
  "Execute string COMMAND and create TAGS-FILE asynchronously."
  (let* ((proc (start-file-process "Shell" nil shell-file-name shell-command-switch command)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        (let* ((status (process-status process)))
          (when (memq status '(exit signal))
            (cond
             ((string= (substring signal 0 -1) "finished")
              (let* ((cmd (car (cdr (cdr (process-command process))))))
                (if counsel-etags-debug (message "`%s` executed." cmd))
                ;; If tramp exists and file is remote, clear file cache
                (when (and (fboundp 'tramp-cleanup-this-connection)
                           ,tags-file
                           (file-remote-p ,tags-file))
                  (tramp-cleanup-this-connection))
                ;; reload tags-file
                (when (and ,tags-file (file-exists-p ,tags-file))
                  (run-hook-with-args 'counsel-etags-after-update-tags-hook ,tags-file)
                  (message "Tags file %s was created." ,tags-file))))
             (t
              (message "Failed to create tags file. Error=%s CLI=%s"
                       signal
                       ,command)))))))))

(defun counsel-etags-dir-pattern (dir)
  "Trim * from DIR."
  (setq dir (replace-regexp-in-string "[*/]*\\'" "" dir))
  (setq dir (replace-regexp-in-string "\\`[*]*" "" dir))
  dir)


(defun counsel-etags-emacs-bin-path ()
  "Get Emacs binary path."
  (let* ((emacs-executable (file-name-directory (expand-file-name invocation-name
                                                                  invocation-directory))))
    (replace-regexp-in-string "/" "\\\\" emacs-executable)))

(defun counsel-etags--ctags--info (ctags-program)
  "Get CTAGS-PROGRAM information."
  (shell-command-to-string (concat ctags-program " --version")))

;;;###autoload
(defun counsel-etags-exuberant-ctags-p (ctags-program)
  "If CTAGS-PROGRAM is Exuberant Ctags."
  (let* ((cmd-output (counsel-etags--ctags--info ctags-program)))
    (and (not (string-match-p "Universal Ctags" cmd-output))
         (string-match-p "Exuberant Ctags" cmd-output))))

;;;###autoload
(defun counsel-etags-universal-ctags-p (ctags-program)
  "If CTAGS-PROGRAM is Universal Ctags."
  (and (executable-find ctags-program)
       (not (counsel-etags-exuberant-ctags-p ctags-program))))

(defun counsel-etags-valid-ctags (ctags-program)
  "If CTAGS-PROGRAM is Ctags return the program.
If it's Emacs etags return nil."
  (when ctags-program
    (let* ((cmd-output (counsel-etags--ctags--info ctags-program)))
      (unless (string-match-p " ETAGS.README" cmd-output)
        ctags-program))))

(defun counsel-etags-languages (ctags-program)
  "List languages CTAGS-PROGRAM supports."
  (let* ((cmd (concat ctags-program " --list-languages")))
    (split-string (shell-command-to-string cmd) "\n")))

(defun counsel-etags-universal-ctags-opt ()
  "Generate option for Universal ctags."
  (format "--options=\"%s\""
          (expand-file-name counsel-etags-ctags-options-file)))

(defun counsel-etags-convert-config (config program)
  "Convert CONFIG of PROGRAM into Universal Ctags format."
  (let* ((rlt config)
         (langs (counsel-etags-languages program))
         ch
         regex)
    (dolist (lang langs)
      (when (not (string= "" lang))
        (setq ch (substring-no-properties lang 0 1))
        (setq regex (format "--langdef=[%s%s]%s *$"
                            ch
                            (downcase ch)
                            (substring-no-properties lang 1)))
        (setq rlt (replace-regexp-in-string regex "" rlt))))
    rlt))

(defun counsel-etags-ctags-options-file-cli (program)
  "Use PROGRAM to create cli for `counsel-etags-ctags-options-file'."
  (let* (str
         (exuberant-ctags-p (counsel-etags-exuberant-ctags-p program)))
    (cond
     ;; Don't use any configuration file at all
     ((or (not counsel-etags-ctags-options-file)
          (string= counsel-etags-ctags-options-file ""))
      "")

     ;; ~/.ctags.exuberant => ~/.ctags
     ((file-exists-p counsel-etags-ctags-options-base)
      (setq str
            (counsel-etags-read-internal counsel-etags-ctags-options-base))
      (unless exuberant-ctags-p
        ;; Universal Ctags
        (setq str (counsel-etags-convert-config str program)))
      ;; Make sure ~/.ctags exist
      (counsel-etags-write-internal str counsel-etags-ctags-options-file)
      ;; OK, no we can pass option to cli
      (if exuberant-ctags-p "" (counsel-etags-universal-ctags-opt)))

     ;; ~/.ctags is missing
     ((not (file-exists-p counsel-etags-ctags-options-file))
      "")

     ;; If options file is "~/.ctags" and Exuberant Ctags is used
     ;; "~/.ctags" won't be loaded.
     ;; But if options file is empty, "~/.ctags" will be loaded.
     ;; It's a bug of Exuberant Ctags, work around here.
     (exuberant-ctags-p
      ;; For Exuberant Ctags, I only accept ~/.ctags
      "")

     ;; Universal Ctags
     (t
      (counsel-etags-universal-ctags-opt)))))

(defun counsel-etags-ctags-ignore-config ()
  "Specify ignore configuration file (.gitignore, for example) for Ctags."
  (let* (rlt configs filename)
    (dolist (f counsel-etags-ignore-config-files)
      (when (file-exists-p (setq filename (expand-file-name f)))
        (push (file-local-name filename) configs)))
    (setq rlt (mapconcat (lambda (c) (format "--exclude=\"@%s\"" c)) configs " "))
    (when counsel-etags-debug
        (message "counsel-etags-ctags-ignore-config returns %s" rlt))
    rlt))

(defun counsel-etags-get-scan-command (ctags-program &optional code-file)
  "Create command for CTAGS-PROGRAM.
If CODE-FILE is a real file, the command scans it and output to stdout."
  (let* ((cmd ""))
    (cond
     ;; Use ctags only
     (ctags-program
      (setq cmd
            (format "%s %s %s -e %s %s %s -R %s"
                    ctags-program
                    (mapconcat (lambda (p)
                                 (format "--exclude=\"*/%s/*\" --exclude=\"%s/*\""
                                         (counsel-etags-dir-pattern p)
                                         (counsel-etags-dir-pattern p)))
                               counsel-etags-ignore-directories " ")
                    (mapconcat (lambda (p)
                                 (format "--exclude=\"%s\"" p))
                               counsel-etags-ignore-filenames " ")
                    (counsel-etags-ctags-options-file-cli ctags-program)
                    (counsel-etags-ctags-ignore-config)
                    ;; print a tabular, human-readable cross reference
                    ;; --<my-lang>-kinds=f still accept all user defined regex
                    ;; so we have to filter in Emacs Lisp
                    (if code-file "-x -w" "")
                    (if code-file (format "\"%s\"" code-file) ""))))

     (t
      (message "You need install Ctags at first.  Universal Ctags is highly recommended.")))
    (when counsel-etags-debug
      (message "counsel-etags-get-scan-command called => ctags-program=%s cmd=%s"
               ctags-program cmd))
    cmd))

;;;###autoload
(defun counsel-etags-scan-dir-internal (src-dir)
  "Create tags file from SRC-DIR."
  ;; TODO save the ctags-opts into hash
  (let* ((ctags-program (or counsel-etags-ctags-program
                            (counsel-etags-valid-ctags
                             (counsel-etags-guess-program "ctags"))))
         (default-directory src-dir)
         ;; if both find and ctags exist, use both
         ;; if only ctags exists, use ctags
         ;; run find&ctags to create TAGS, `-print` is important option to filter correctly
         (cmd (counsel-etags-get-scan-command ctags-program))
         (tags-file (counsel-etags-get-tags-file-path src-dir)))
    (unless ctags-program
      (error "Please install Exuberant Ctags or Universal Ctags before running this program!"))
    (when counsel-etags-debug
      (message "counsel-etags-scan-dir-internal called => src-dir=%s" src-dir)
      (message "default-directory=%s cmd=%s" default-directory cmd))
    ;; always update cli options
    (message "%s at %s" (if counsel-etags-debug cmd "Scan") default-directory)
    (counsel-etags-async-shell-command cmd tags-file)))

(defun counsel-etags-toggle-auto-update-tags ()
  "Stop/Start tags auto update."
  (interactive)
  (if (setq counsel-etags-stop-auto-update-tags
            (not counsel-etags-stop-auto-update-tags))
      (message "Tags is NOT automatically updated any more.")
    (message "Tags will be automatically updated.")))

(defun counsel-etags-scan-dir (src-dir)
  "Create tags file from SRC-DIR."
  (if counsel-etags-debug (message "counsel-etags-scan-dir called => %s" src-dir))
  (cond
   (counsel-etags-stop-auto-update-tags
    ;; do nothing
    )
   (t
    (funcall counsel-etags-update-tags-backend src-dir))))

;;;###autoload
(defun counsel-etags-directory-p (regex)
  "Does directory of current file match REGEX?"
  (let* ((case-fold-search nil)
         (dir (or (when buffer-file-name
                    (file-name-directory buffer-file-name))
                  ;; buffer is created in real time
                  default-directory
                  "")))
    (string-match-p regex dir)))

;;;###autoload
(defun counsel-etags-filename-p (regex)
  "Does current file match REGEX?"
  (let* ((case-fold-search nil)
         (file (or buffer-file-name default-directory "")))
    (string-match-p regex file)))

(defun counsel-etags-read-internal (file)
  "Read content of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun counsel-etags-write-internal (content file)
  "Write CONTENT into FILE."
  (write-region content nil file))

(defun counsel-etags-read-file (file)
  "Return FILE content with child files included."
  (let* ((raw-content (counsel-etags-read-internal file))
         (start 0)
         (re "^\\([^,]+\\),include$")
         included
         (extra-content ""))
    (while (setq start (string-match re raw-content start))
      (when (file-exists-p (setq included (match-string 1 raw-content)))
        (setq extra-content (concat extra-content
                                    "\n"
                                    (counsel-etags-read-internal included))))
      (setq start (+ start (length included))))
    (concat raw-content extra-content)))

(defmacro counsel-etags--tset (table x y val row-width)
  "Set TABLE cell at position (X, Y) with VAL and ROW-WIDTH."
  `(aset ,table (+ ,x (* ,row-width ,y)) ,val))

(defmacro counsel-etags--tref (table x y row-width)
  "Get TABLE cell at position (X, Y) with ROW-WIDTH."
  `(aref ,table (+ ,x (* ,row-width ,y))))

(defun counsel-etags-levenshtein-distance (str1 str2 hash)
  "Return the edit distance between strings STR1 and STR2.
HASH store the previous distance."
  (let* ((val (gethash str1 hash)))
    (unless val
      (let* ((length-str1 (length str1))
             (length-str2 (length str2))
             ;; it's impossible files name has more than 512 characters
             (d (make-vector (* (1+ length-str1) (1+ length-str2)) 0))
             ;; d is a table with lenStr2+1 rows and lenStr1+1 columns
             (row-width (1+ length-str1))
             (i 0)
             (j 0))
        ;; i and j are used to iterate over str1 and str2
        (while (<= i length-str1) ;; for i from 0 to lenStr1
          (counsel-etags--tset d i 0 i row-width) ;; d[i, 0] := i
          (setq i (1+ i)))
        (while (<= j length-str2) ;; for j from 0 to lenStr2
          (counsel-etags--tset d 0 j j row-width) ;; d[0, j] := j
          (setq j (1+ j)))
        (setq i 1)
        (while (<= i length-str1) ;; for i from 1 to lenStr1
          (setq j 1)
          (while (<= j length-str2) ;; for j from 1 to lenStr2
            (let* ((cost
                    ;; if str[i] = str[j] then cost:= 0 else cost := 1
                    (if (equal (aref str1 (1- i)) (aref str2 (1- j))) 0 1))
                   ;; d[i-1, j] + 1     // deletion
                   (deletion (1+ (counsel-etags--tref d (1- i) j row-width)))
                   ;; d[i, j-1] + 1     // insertion
                   (insertion (1+ (counsel-etags--tref d i (1- j) row-width)))
                   ;; d[i-j,j-1] + cost // substitution
                   (substitution (+ (counsel-etags--tref d (1- i) (1- j) row-width) cost))
                   (distance (min insertion deletion substitution)))
              (counsel-etags--tset d i j distance row-width)
              (setq j (1+ j))))
          (setq i (1+ i))) ;; i++
        ;; return d[lenStr1, lenStr2] or the max distance
        (setq val (counsel-etags--tref d length-str1 length-str2 row-width))
        (puthash str1 val hash)))
    val))

(defun counsel-etags-cache-invalidate (tags-file)
  "Invalidate the cache of TAGS-FILE."
  (plist-put counsel-etags-cache (intern tags-file) nil))

(defun counsel-etags--time-cost (start-time)
  "Show time cost since START-TIME."
  (let* ((time-passed (float-time (time-since start-time))))
    (format "%.01f second%s"
            time-passed
            (if (<= time-passed 2) "" "s"))))

(defun counsel-etags-tags-file-must-exist ()
  "Make sure tags file does exist."
  (let* ((tags-file (counsel-etags-locate-tags-file))
         src-dir)
    (when (and (not tags-file)
               ;; No need to hint after user set `counsel-etags-extra-tags-files'
               (not counsel-etags-extra-tags-files)
               (not counsel-etags-can-skip-project-root))
      (setq src-dir (read-directory-name "Ctags will scan code at: "
                                         (counsel-etags-locate-project)))
      (cond
       (src-dir
        (counsel-etags-scan-dir src-dir)
        (setq tags-file (counsel-etags-get-tags-file-path src-dir)))
       (t
        (error "Can't find TAGS.  Please run `counsel-etags-scan-code'!"))))
    ;; the tags file IS touched
    (when tags-file
      (counsel-etags-add-tags-file-to-history tags-file))))

;;;###autoload
(defun counsel-etags-find-tag-name-default ()
  "Find tag at point."
  (let ((tag-name (find-tag-default)))
    (when (and (memq major-mode
                     counsel-etags-major-modes-to-strip-default-tag-name)
           (string-match "^\\(`.*`\\|=.*=\\|~.*~\\|\".*\"\\|'.*'\\)$" tag-name))
      (setq tag-name (substring tag-name 1 (1- (length tag-name)))))
    tag-name))


;;;###autoload
(defun counsel-etags-scan-code (&optional dir)
  "Use Ctags to scan code at DIR."
  (interactive)
  (let* ((src-dir (or dir
                      (read-directory-name "Ctags will scan code at: "
                                           (or (counsel-etags-locate-project)
                                               default-directory)))))
    (when src-dir
      (counsel-etags-scan-dir src-dir))))

(defun counsel-etags-imenu-scan-string (output)
  "Extract imenu items from OUTPUT."
  (let* (cands
         (lines (split-string output "\n")))
    (dolist (l lines)
      (let* ((items (split-string l " +"))
             (tag-name (nth 0 items))
             (tag-type (nth 1 items))
             (tag-line-num (nth 2 items)))
        (when (and (>= (length items) 4)
                   ;; tag name is not excluded
                   (not (member tag-name counsel-etags-imenu-excluded-names))

                   ;; tags type is not excluded
                   (not (member tag-type counsel-etags-imenu-excluded-types))
                   (string-match "[0-9]+" tag-line-num))
          (push (cons tag-name tag-line-num) cands))))
    cands))


;;;###autoload
(defun counsel-etags-imenu-default-create-index-function ()
  "Create an index alist for the definitions in the current buffer."
  (let* ((ctags-program (or counsel-etags-ctags-program
                            (counsel-etags-guess-program "ctags")))
         (ext (if buffer-file-name (file-name-extension buffer-file-name) ""))
         ;; ctags needs file extension
         (code-file (make-temp-file "coet" nil (concat "." ext)))
         cmd
         imenu-items
         cands)

    (when (and code-file (file-exists-p code-file))
      ;; write current buffer into code file
      (write-region (point-min) (point-max) code-file)
      (setq cmd
            (cond
             (counsel-etags-command-to-scan-single-code-file
              (concat counsel-etags-command-to-scan-single-code-file
                      "\""
                      code-file
                      "\""))
             (t
              (counsel-etags-get-scan-command ctags-program code-file))))

      ;; create one item for imenu list
      ;; (cons name (if imenu-use-markers (point-marker) (point)))
      (setq cands (counsel-etags-imenu-scan-string (shell-command-to-string cmd)))

      ;; cands contains list of name and line number
      ;; Example of cands:
      ;;  (setq cands (list (cons "hello" "5")))
      ;; we need convert it into imenu items (name . marker)
      (save-excursion
        (dolist (c cands)
          (let* ((name (car c)))
            (goto-char (point-min))
            (counsel-etags-forward-line (cdr c))
            (when (search-forward name (point-at-eol) t)
              (forward-char (- (length name))))
            (push (cons name (point-marker)) imenu-items))))

      ;; clean up tmp file
      (unless counsel-etags-debug (delete-file code-file)))

    imenu-items))

;;;###autoload
(defun counsel-etags-virtual-update-tags()
  "Scan code and create tags file again.
It's the interface used by other hooks or commands.
The tags updating might not happen."
  (interactive)
  (let* ((dir (and buffer-file-name
                   (file-name-directory buffer-file-name)))
         (tags-file (and counsel-etags-tags-file-history
                         (car counsel-etags-tags-file-history))))

    (when counsel-etags-debug
      (message "counsel-etags-virtual-update-tags called. dir=%s tags-file=%s" dir tags-file))

    (when (and dir
               tags-file
               (string-match-p (file-name-directory (expand-file-name tags-file))
                               (expand-file-name dir)))
      (cond
       ((or (not counsel-etags-timer)
            (> (- (float-time (current-time)) (float-time counsel-etags-timer))
               counsel-etags-update-interval))

        ;; start timer if not started yet
        (setq counsel-etags-timer (current-time))

        ;; start updating
        (if counsel-etags-debug (message "counsel-etags-virtual-update-tags actually happened."))

        (let* ((dir (file-name-directory (expand-file-name (counsel-etags-locate-tags-file)))))
          (if counsel-etags-debug (message "update tags in %s" dir))
          (funcall counsel-etags-update-tags-backend dir)))

       (t
        ;; do nothing, can't run ctags too often
        (if counsel-etags-debug (message "counsel-etags-virtual-update-tags is actually skipped.")))))))

;;;###autoload
(defun counsel-etags-update-tags-force (&optional forced-tags-file)
  "Update current tags file using default implementation.
If FORCED-TAGS-FILE is nil, the updating process might now happen."
  (interactive)
  (let* ((tags-file (or forced-tags-file
                        (counsel-etags-locate-tags-file))))
    (when tags-file
      ;; @see https://github.com/redguardtoo/counsel-etags/issues/82
      ;; If code file is moved and TAGS is updated, invalidate the cache.
      (counsel-etags-cache-invalidate tags-file)
      ;; scan the code now
      (counsel-etags-scan-dir (file-name-directory (expand-file-name tags-file)))
      (unless counsel-etags-quiet-when-updating-tags
        (message "%s is updated!" tags-file)))))

;;;###autoload
(defun counsel-etags-append-to-tags-file (sections tags-file)
  "Append SECTIONS into TAGS-FILE.
Each section is a pair of file and tags content in that file.
File can be url template like \"https//myproj.com/docs/API/%s\".
The `counsel-etags-browse-url-function' is used to open the url."
  (when (and tags-file
             (file-exists-p tags-file)
             (file-readable-p tags-file)
             (file-writable-p tags-file)
             sections
             (> (length sections) 0))

    (with-temp-buffer
      (insert-file-contents tags-file)
      (goto-char (point-max))
      (dolist (s sections)
        (when (and (car s) (cdr s))
          (insert (format "\n\014\n%s,%d\n%s" (car s) 0 (cdr s)))))
      (write-region (point-min) (point-max) tags-file nil :silent))))

(provide 'init-etags)
;;; counsel-etags.el ends here

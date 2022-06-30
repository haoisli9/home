;; -*- lexical-binding: t -*-

;; copy region or whole line
(global-set-key "\M-w"
		(lambda ()
		  (interactive)
		  (if mark-active
		      (kill-ring-save (region-beginning)
				      (region-end))
		    (progn
		      (kill-ring-save (line-beginning-position)
				      (line-end-position))
		      (message "copied line")))))

;; kill region or whole line
;; (global-set-key "\M-k"
;;                 (lambda ()
;;                   (interactive)
;;                   (if mark-active
;;                       (kill-region (region-beginning)
;;                                    (region-end))
;;                     (progn
;;                       (kill-region (line-beginning-position)
;;                                    (line-end-position))
;;                       (message "killed line")))))

(defun dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unlessbuffer-display-table
   (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

;;set transparent effect
(setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    ))

;; move line up down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

(defcustom my-tmp-register 8
  "Register used to store the cursor position."
  :type 'integer)

(defcustom my-original-register 9
  "Register used to store the cursor position."
  :type 'integer)

(defun my-tmp-mark ()
  "Store cursor position fast in a register.

Use `my-tmp-back` to jump back to the stored position."
  (interactive)
  (point-to-register my-tmp-register)
  (point-to-register my-original-register)
  (message "tmp marked in %s." (point-marker)))

(defun my-tmp-back-original ()
  "Jumps between current and stored cursor position."
  (interactive)
  (set-register my-tmp-register (point-marker))
  (jump-to-register my-original-register)
  (message "tmp marked in %s" (point-marker)))

(defun my-tmp-back ()
  "Jumps between current and stored cursor position."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register my-tmp-register)
    (set-register my-tmp-register tmp)
    (message "tmp marked in %s" tmp)))

;; -------------------------------------------------------------------------
;;; Section: Rar Archives
;; modified by unrar in windows OS.
(defun archive-unrar-summarize (&optional file)
  ;; File is used internally for `archive-rar-exe-summarize'.
  (unless file (setq file buffer-file-name))
  (let* ((copy (file-local-copy file))
         (files ()))
    (with-temp-buffer
      (unwind-protect
          (call-process "unrar" nil t nil "v -c-" (or file copy))
        (if copy (delete-file copy)))
      (goto-char (point-min))
      (re-search-forward "^\\(-+\s+-+\\)+\s*$")
      (goto-char (+ 1 (point)))
      (while (looking-at (concat "^\s+[A-Z.]+\s+"          ; Attributes
                                 "\\([0-9]+\\)\s+"         ; Size
                                 "\\([0-9]+\\)\s+"         ; Packed
                                 "\\([0-9\.*]+\\)%\s+"      ; Ratio
                                 "\\([0-9-]+\\)\s+"        ; Date
                                 "\\([0-9:]+\\)\s+"        ; Time
                                 "\\([0-9A-Z]+\\)\s+"      ; Checksum
                                 "\\(.*\\)$"               ; Name
                                 ))
        (goto-char (match-end 0))
        (let ((name (match-string 7))
              (size (match-string 1)))
          (push (archive--file-desc name name nil
                                    ;; Size
                                    (string-to-number size)
                                    ;; Date&Time.
                                    (concat (match-string 4) " " (match-string 5))
                                    :ratio (match-string 3))
                files))
        (goto-char (+ 1 (point)))))
    (archive--summarize-descs (nreverse files))))

(advice-add 'archive-rar-summarize :override #'archive-unrar-summarize)

(defun archive-unrar-extract (archive name)
  ;; unrar-free seems to have no way to extract to stdout or even to a file.
  (if (file-name-absolute-p name)
      ;; The code below assumes the name is relative and may do undesirable
      ;; things otherwise.
      (error "Can't extract files with non-relative names")
    ;; unrar用x带目录解压会产生盘符目录，应该是msys2处理windows盘符的bug，暂时用e替代；
    (archive-extract-rar-by-file archive name '("unrar" "e") "All OK")))

(defun archive-extract-rar-by-file (archive name command &optional stdout-test)
  (let ((dest (make-temp-file "arc-dir" 'dir))
	(stdout-file (make-temp-file "arc-stdout")))
    (unwind-protect
	(prog1
	    (apply #'call-process
		   (car command)
		   nil
		   `(:file ,stdout-file)
		   nil
                   `(,@(cdr command) ,archive ,name ,dest))
	  (with-temp-buffer
	    (insert-file-contents stdout-file)
	    (goto-char (point-min))
	    (when (if (stringp stdout-test)
		      (not (re-search-forward stdout-test nil t))
		    (> (buffer-size) 0))
	      (message "%s" (buffer-string))))
          
          ;; 由于上面是用e解压，所以这里需要把文件名中的目录去掉
          (setq name (file-name-nondirectory name))
          
	  (if (file-exists-p (expand-file-name name dest))
	      (insert-file-contents-literally (expand-file-name name dest))))
      (if (file-exists-p stdout-file)
	  (delete-file stdout-file))
      (if (file-exists-p (expand-file-name name dest))
	  (delete-file (expand-file-name name dest)))
      (while (file-name-directory name)
	(setq name (directory-file-name (file-name-directory name)))
	(when (file-directory-p (expand-file-name name dest))
	  (delete-directory (expand-file-name name dest))))
      (when (file-directory-p dest)
	(delete-directory dest)))))

(advice-add 'archive-rar-extract :override #'archive-unrar-extract)

;;--------------------------------------------------------------------------------------
(provide 'init-misc)


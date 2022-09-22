
;;; init-ctags.el --- Use ctags & find to create TAGS on Winows/Linux/OSX
;; add by lihao at 2022 9月 09 星期五 

;;--------------------------------------------------------------------------------------
;; (compile "find . -type f -not -name 'TAGS' -print | etags -")
;; (compile "find . -name \"*.[chCH]\" -print | etags -")
;; (compile "find . -name \"*.[chCH]\" -print | ctags -e -L -")
;; (compile "ctags -o TAGS --languages=C,C++ --kinds-all=* --fields=* --extras=* -eR")
;; (compile "ctags -o TAGS --kinds-all=* --fields=+aiKSz --extras=+q -eR")
(defvar ctags-tags-command-alist
  ;; special TAGS rule to generate TAGS files.
  (list '("e:/hello/ctags-master/TAGS" "ctags -o TAGS --languages=C,C++ --kinds-all=* --fields=* --extras=* -eR")
        '("e:/hello/linux-2.6.39.4/TAGS" "ctags -o TAGS --languages=C,C++ --kinds-all=* --fields=* --extras=* -eR")))

;;;###autoload
(defun ctags-compile-tags ()
 "compile etags for the current project"
  (interactive)
  (let ((src-dir (expand-file-name (read-directory-name "Gen TAGS in Dir: ")))
        (cmd "find . -type f -not -name 'TAGS' -print | etags -")
        build-cmd)
    (cd src-dir)
    (setq build-cmd (assoc (concat src-dir "TAGS") ctags-tags-command-alist))
    (if build-cmd
        (setq build-cmd (format "%s" (cdr build-cmd)))
      (setq build-cmd cmd))
    ;; (message "%s" build-cmd)
    (compile build-cmd)
  ))

;;;###autoload
(defun ctags-async-shell-command (command tags-file)
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
                ;; If tramp exists and file is remote, clear file cache
                (when (and (fboundp 'tramp-cleanup-this-connection)
                           ,tags-file
                           (file-remote-p ,tags-file))
                  (tramp-cleanup-this-connection))
                ;; reload tags-file
                (when (and ,tags-file (file-exists-p ,tags-file))
                  (message "Tags file %s was (re)Generated." ,tags-file))))
             (t
              (message "Failed to create tags file. Error=%s CLI=%s"
                       signal
                       ,command)))))))))

;;;###autoload
(defun ctags-update-tag (tag)
 "compile etags for the current project"
  (let* ((src-dir (file-name-directory tag))
        (cmd "find . -type f -not -name 'TAGS' -print | etags -")
        build-cmd)
    (cd src-dir)
    (setq build-cmd (assoc tag ctags-tags-command-alist))
    (if build-cmd
        (setq build-cmd (format "%s" (cdr build-cmd)))
      (setq build-cmd cmd))
    ;; (message "%s called in %s" build-cmd src-dir)
    (ctags-async-shell-command build-cmd tag)  
  ))

;;;###autoload
(defun ctags-update-current-tag (&optional is-used-as-api)
  "Update current TAGS."
  (interactive)
  (if tags-file-name
      (ctags-update-tag tags-file-name)
    (unless is-used-as-api
      (message "no tags file loaded."))))

;;;###autoload
(defun ctags-update-all-tags ()
 "Automatically update TAGS files"
 (interactive)
  (let* ((tags-list tags-table-list))
    (if tags-file-name
        (setq tags-list (add-to-list 'tags-list tags-file-name)))
    (dolist (tag tags-list)
      (unless (string-match-p "TAGS$" tag)
        (setq tag (concat (file-name-absolute-p tag) "TAGS")))
      (ctags-update-tag tag))
    (unless is-used-as-api
      (message "Tags in `tags-file-name' and `tags-table-list' are updated!"))))

;;--------------------------------------------------------------------------------------
;; auto update tags.
;; Timer to run auto-update TAGS.
(defvar ctags-updated-timer nil)
(defvar ctags-auto-update-tags-interval 600
  "The interval to update TAGS.
It's used by `ctags-auto-update-tags' and in seconds format.
Default value is 600 which equals 5 minutes.")

;;;###autoload
(defun ctags-auto-update-tags()
  ;; (interactive)
  (cond
   ((or (not ctags-updated-timer)
        (> (- (float-time (current-time)) (float-time counsel-etags-timer))
           ctags-auto-update-tags-interval))
    ;; run updage tags function.
    (setq ctags-updated-timer (current-time))
    (ctags-update-all-tags t)
    (message "All tag files have been updated after %d seconds!"
             (- (float-time (current-time)) (float-time ctags-updated-timer))))
   (t
    ;; timer not reach, do nothing.
    )
   ))

;; (add-hook 'after-save-hook 'ctags-auto-update-tags)

(provide 'init-ctags)

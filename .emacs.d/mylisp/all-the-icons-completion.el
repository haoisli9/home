;;; all-the-icons-completion.el --- Add icons to completion candidates -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Itai Y. Efrat
;;
;; Author: Itai Y. Efrat <https://github.com/iyefrat>
;; Maintainer: Itai Y. Efrat <itai3397@gmail.com>
;; Created: June 06, 2021
;; Modified: June 06, 2021
;; Version: 0.0.1
;; Package-Version: 20220409.1204
;; Package-Commit: 286e2c064a1298be0d8d4100dc91d7a7a554d04a
;; Keywords: convenient, lisp
;; Homepage: https://github.com/iyefrat/all-the-icons-completion
;; Package-Requires: ((emacs "26.1") (all-the-icons "5.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licence:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Add icons to completion candidates.
;;
;;; Code:

(require 'all-the-icons)

(defgroup all-the-icons-completion nil
  "Add icons to completion candidates."
  :version "26.1"
  :group 'appearance
  :group 'convenience
  :prefix "all-the-icons-completion")

(defface all-the-icons-completion-dir-face
  '((t nil))
  "Face for the directory icon."
  :group 'all-the-icons-faces)

(defun all-the-icons-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  ;; (message "cat : %s" cat)
  (cl-case cat
    (file (all-the-icons-completion-get-file-icon cand))
    (project-file (all-the-icons-completion-get-file-icon cand))
    (buffer (all-the-icons-completion-get-buffer-icon cand))
    (theme (all-the-icons-completion-get-theme-icon))
    (imenu (all-the-icons-completion-get-imenu-icon cand))
    (command (all-the-icons-completion-get-function-icon cand))
    (function (all-the-icons-completion-get-function-icon cand))
    (variable (all-the-icons-completion-get-variable-icon cand))
    (package (all-the-icons-completion-get-package-icon cand))
    (symbol (all-the-icons-completion-get-symbol-icon cand))
    (t "")))

(defun all-the-icons-completion-get-file-icon (cand)
  "Return the icon for the candidate CAND of completion category file."
  (cond ((string-match-p "\\/$" cand)
         (concat
          (all-the-icons-icon-for-dir cand :face 'all-the-icons-completion-dir-face)
          " "))
        (t (concat (all-the-icons-icon-for-file cand) " "))))

(defun all-the-icons-completion-get-buffer-icon (cand)
  "Return the icon for the candidate CAND of completion category buffer."
  (let* ((mode (buffer-local-value 'major-mode (get-buffer cand)))
         (icon (all-the-icons-icon-for-mode mode))
         (parent-icon (all-the-icons-icon-for-mode
                       (get mode 'derived-mode-parent))))
    (concat
     (if (symbolp icon)
         (if (symbolp parent-icon)
             (all-the-icons-faicon "sticky-note-o")
           parent-icon)
       icon)
     " ")))

(defun all-the-icons-completion-get-project-icon (_candidate)
  "Display project icon."
  (concat
   (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01 :face 'all-the-icons-silver)
   " "))

(defun all-the-icons-completion-get-mode-icon (_candidate)
  "Display mode icon."
  (concat
   (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)
   " "))

(defun all-the-icons-completion-get-function-icon (_candidate)
  "Display function icon."
  (concat
   (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)
   " "))

(defun all-the-icons-completion-get-variable-icon (_candidate)
  "Display the variable icon in."
  (concat
   (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)
   " "))

(defun all-the-icons-completion-get-symbol-icon (candidate)
  "Display the symbol icon."
  (let ((sym (intern candidate)))
    (cond ((functionp sym)
           (all-the-icons-completion-get-function-icon candidate))
          ((symbolp sym)
           (all-the-icons-completion-get-variable-icon candidate))
          (t (concat
              (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver) " ")))))

(defun all-the-icons-completion-get-keybinding-icon (_candidate)
  "Display the keybindings icon."
  (concat
   (all-the-icons-faicon "keyboard-o" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lsilver)
   " "))

(defun all-the-icons-completion-get-library-icon (_candidate)
  "Display the library icon."
  (concat
   (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue)
   " "))

(defun all-the-icons-completion-get-package-icon (_candidate)
  "Display the package icon."
  (concat
   (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)
   " "))

(defun all-the-icons-completion-get-font-icon (_candidate)
  "Display the font icon."
  (concat
   (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)
   " "))

(defun all-the-icons-completion-get-world-clock-icon (_candidate)
  "Display the world clock icon."
  (concat
   (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)
   " "))

(defun all-the-icons-completion-get-tramp-icon (_candidate)
  "Display the tramp icon."
  (concat
   (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01)
   " "))

(defun all-the-icons-completion-get-git-branch-icon (_candidate)
  "Display the git branch icon."
  (concat
   (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)
   " "))

(defun all-the-icons-completion-get-process-icon (_candidate)
  "Display the process icon."
  (concat
   (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)
   " "))

(defun all-the-icons-completion-get-imenu-icon (cand)
  "Return the icon for the candidate CAND of completion category imenu."
  (concat
   (let ((case-fold-search nil))
     (cond
      ((string-match-p "Type Parameters?[:)]" cand)
       (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
      ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" cand)
       (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
      ((string-match-p "Constants?[:)]" cand)
       (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
      ((string-match-p "Enum\\(erations?\\)?[:)]" cand)
       (all-the-icons-material "storage" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-orange))
      ((string-match-p "References?[:)]" cand)
       (all-the-icons-material "collections_bookmark" :height 0.95 :v-adjust -0.2))
      ((string-match-p "\\(Types?\\)\\|\\(Property\\)[:)]" cand)
       (all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
      ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)\\|\\(Constructors?\\)[:)]" cand)
       (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
      ((string-match-p "\\(Class\\)\\|\\(Structs?\\)[:)]" cand)
       (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
      ((string-match-p "Interfaces?[:)]" cand)
       (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
      ((string-match-p "Modules?[:)]" cand)
       (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
      ((string-match-p "Packages?[:)]" cand)
       (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))
      (t (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-blue))))
   " "))

(defun all-the-icons-completion-get-bookmark-type-icon (candidate)
  "Return bookmark type from CANDIDATE."
  (concat
   (let ((filename (bookmark-get-filename candidate)))
     (cond ((null filename)
            (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
           ((file-remote-p filename)
            (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
           ((not (file-exists-p filename))
            (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
           ((file-directory-p filename)
            (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
           (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :v-adjust 0.0))))
   " "))

(defun all-the-icons-completion-get-theme-icon ()
  "Return the icon for completion category theme."
  (concat (all-the-icons-material "palette" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lcyan) " "))

(defun all-the-icons-completion-completion-metadata-get (orig metadata prop)
  "Meant as :around advice for `completion-metadata-get', Add icons as prefix.
ORIG should be `completion-metadata-get'
METADATA is the metadata.
PROP is the property which is looked up."
  (if (eq prop 'affixation-function)
      (let ((cat (funcall orig metadata 'category))
            (aff (or (funcall orig metadata 'affixation-function)
                     (when-let ((ann (funcall orig metadata 'annotation-function)))
                       (lambda (cands)
                         (mapcar (lambda (x) (list x "" (funcall ann x))) cands))))))
        (cond
         ((and (eq cat 'multi-category) aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (let ((orig (get-text-property 0 'multi-category cand)))
                           (list cand
                                 (concat (all-the-icons-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (list cand
                               (concat (all-the-icons-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (all-the-icons-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (all-the-icons-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun all-the-icons-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `all-the-icons-completion-mode' to it."
  (all-the-icons-completion-mode (if marginalia-mode 1 -1)))

;;;###autoload
(define-minor-mode all-the-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if all-the-icons-completion-mode
      (advice-add #'completion-metadata-get :around #'all-the-icons-completion-completion-metadata-get)
    (advice-remove #'completion-metadata-get #'all-the-icons-completion-completion-metadata-get)))

(provide 'all-the-icons-completion)
;;; all-the-icons-completion.el ends here

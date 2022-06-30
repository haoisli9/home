;;; iscroll-all.el --- scroll all buffers together minor mode

;; Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Gary D. Foster <Gary.Foster@corp.sun.com>
;; Keywords: scroll crisp brief lock

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Update by lihao to support mouse wheel and scroll bar click      ;;
;;                                                                   ;;
;;  When scrolling in selected window, all buffers will scroll       ;;
;;  together. When scrolling in not selected window, only window     ;;
;;  under the cursor will scroll. I do like this because I want to   ;;
;;  scroll some special windows (not all) in scroll-all-mode.        ;;
;;  Of course you can change it.                                     ;;
;;                                                                   ;; 
;;  any bug, please let me know <9307420654@163.com>                 ;;
;;  2005-04-23                                                       ;;
;;  for NT emacs GNU Emacs 21.3.50.1 (i386-mingw-nt5.1.2600)         ;;
;;  of 2005-01-31 on NONIQPC                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;    This mode allows multiple buffers to be 'locked' so that scrolling
;;    up or down lines in any buffer causes all the buffers to mirror
;;    the scrolling.  It hooks into the post-command-hook to check for
;;    potential scrolling commands and if we're locked, mirrors them in all
;;    windows.  This allows us to grab line-at-a-time scrolling as well as
;;    screen-at-a-time scrolling, and doesn't remap any of the keyboard
;;    commands to do it.

;; You can enable and disable this mode with the 'scroll-all-mode' command.

;; Suggestions/ideas from:
;;    Rick Macdonald <rickm@vsl.com>
;;    Anders Lindgren <andersl@csd.uu.se>

;;; Code:

(require 'scroll-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add by lihao to support mouse wheel
(defun scroll-all-event-mouse-all (event)
  "Scroll up or down according to the EVENT."
  (interactive "e")
  (let* ((curwin (if mouse-wheel-follow-mouse
                     (prog1
                         (selected-window)
                       (select-window (mwheel-event-window event)))))
         (mods
	  (delq 'click (delq 'double (delq 'triple (event-modifiers event)))))
         (amt (assoc mods mouse-wheel-scroll-amount)))
    ;; Extract the actual amount or find the element that has no modifiers.
    (if amt (setq amt (cdr amt))
      (let ((list-elt mouse-wheel-scroll-amount))
	(while (consp (setq amt (pop list-elt))))))
    (if (floatp amt) (setq amt (1+ (truncate (* amt (window-height))))))
    (when (and mouse-wheel-progressive-speed (numberp amt))
      ;; When the double-mouse-N comes in, a mouse-N has been executed already,
      ;; So by adding things up we get a squaring up (1, 3, 6, 10, 15, ...).
      (setq amt (* amt (event-click-count event))))
    (unwind-protect
	(let ((button (mwheel-event-button event)))
	  (cond ((eq button mouse-wheel-down-event) (scroll-all-function-all 'scroll-down amt))
		((eq button mouse-wheel-up-event) (scroll-all-function-all 'scroll-up amt))
		(t (error "Bad binding in mwheel-scroll"))))
      (if curwin (select-window curwin))))
  (when (and mouse-wheel-click-event mouse-wheel-inhibit-click-time)
    (if mwheel-inhibit-click-event-timer
	(cancel-timer mwheel-inhibit-click-event-timer)
      (add-hook 'pre-command-hook 'mwheel-filter-click-events))
    (setq mwheel-inhibit-click-event-timer
	  (run-with-timer mouse-wheel-inhibit-click-time nil
			  'mwheel-inhibit-click-timeout))))

(defun scroll-all-event-mouse (event)
  (interactive (list last-input-event))
  (scroll-all-function-all 'scroll-all-event-mouse-all event))

(defun scroll-all-bar-drag-1 (event)
  (let* ((start-position (event-start event))
	 (window (nth 0 start-position))
	 (portion-whole (nth 2 start-position)))
      ;; Calculate position relative to the accessible part of the buffer.
      (goto-char (+ (point-min)
		    (scroll-bar-scale portion-whole
				      (- (point-max) (point-min)))))
      (vertical-motion 0 window)
      (set-window-start window (point))))

(defun scroll-all-bar-scroll-all (event)
  (interactive "e")
  (let* ((end-position (event-end event))
	 (window (nth 0 end-position))
	 (part (nth 4 end-position))
	 before-scroll)
    (cond ((eq part 'end-scroll))
	  (t
       (setq before-scroll (or before-scroll (point)))
       (cond ((eq part 'above-handle)
              (scroll-up '-))
             ((eq part 'below-handle)
              (scroll-up nil))
             ((eq part 'ratio)
              (let* ((portion-whole (nth 2 end-position))
                     (lines (scroll-bar-scale portion-whole
                                              (1- (window-height)))))
                (scroll-up (cond ((not (zerop lines)) lines)
                                 ((< (car portion-whole) 0) -1)
                                 (t 1)))))
             ((eq part 'up)
              (scroll-up -1))
             ((eq part 'down)
              (scroll-up 1))
             ((eq part 'top)
              (set-window-start window (point-min)))
             ((eq part 'bottom)
              (goto-char (point-max))
              (recenter))
             ((eq part 'handle)
              (scroll-all-bar-drag-1 event)))
	   (sit-for 0)))))

(defun scroll-all-event-scrollbar (event)
  (interactive (list last-input-event))
  (scroll-all-function-all 'scroll-all-bar-scroll-all event))

(defun scroll-all-check-to-scroll ()
  "Check `this-command' to see if a scroll is to be done."
  (cond ((eq this-command 'next-line)
	 (call-interactively 'scroll-all-scroll-down-all))
	((eq this-command 'previous-line)
	 (call-interactively 'scroll-all-scroll-up-all))
	((memq this-command '(scroll-up scroll-up-command))
	 (call-interactively 'scroll-all-page-down-all))
	((memq this-command '(scroll-down scroll-down-command))
	 (call-interactively 'scroll-all-page-up-all))
	((eq this-command 'beginning-of-buffer)
	 (call-interactively 'scroll-all-beginning-of-buffer-all))
	((eq this-command 'end-of-buffer)
	 (call-interactively 'scroll-all-end-of-buffer-all))
    ;; add by lihao to support pc-select.el
    ((eq this-command 'next-line-nomark)
	 (call-interactively 'scroll-all-scroll-down-nomark-all))
	((eq this-command 'previous-line-nomark)
	 (call-interactively 'scroll-all-scroll-up-nomark-all))
	((eq this-command 'scroll-up-nomark)
	 (call-interactively 'scroll-all-page-down-nomark-all))
	((eq this-command 'scroll-down-nomark)
	 (call-interactively 'scroll-all-page-up-nomark-all))
    ((eq this-command 'beginning-of-buffer-nomark)
	 (call-interactively 'scroll-all-beginning-of-buffer-nomark-all))
	((eq this-command 'end-of-buffer-nomark)
	 (call-interactively 'scroll-all-end-of-buffer-nomark-all))
    ;; add by lihao to support mouse wheel
    ((eq this-command 'mwheel-scroll)
	 (call-interactively 'scroll-all-event-mouse))
    ((eq this-command 'scroll-bar-toolkit-scroll)
     (call-interactively 'scroll-all-event-scrollbar))))

;;;###autoload
(define-minor-mode scroll-all-mode
  "Toggle Scroll-All minor mode.
With ARG, turn Scroll-All minor mode on if ARG is positive, off otherwise.
When Scroll-All mode is on, scrolling commands entered in one window
apply to all visible windows in the same frame."
  nil " *SL*" nil
  :global t
  :group 'windows
  :group 'scrolling
  (if scroll-all-mode
      (add-hook 'post-command-hook 'scroll-all-check-to-scroll)
    (remove-hook 'post-command-hook 'scroll-all-check-to-scroll)))

(provide 'iscroll-all)

;;; arch-tag: db20089a-b157-45df-b5d4-2430e60acdd8
;;; scroll-all.el ends here

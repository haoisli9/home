;; -*- lexical-binding: t -*-

;; 将内存回收阈值增大，加快启动速度；启动完成后更新为初始值
(defvar default-file-name-handler-alist file-name-handler-alist)

(defun my|pre-init()
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 1.0
        file-name-handler-alist nil
        ))
(defun my|post-init ()
  (setq gc-cons-threshold (* 256 1024 1024)
        gc-cons-percentage 0.1
        file-name-handler-alist default-file-name-handler-alist)
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; GC automatically while unfocusing the frame
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (unless (frame-focus-state)
                        (garbage-collect)))))
    ;; `focus-out-hook' is obsolete since 27.1
    ;; (add-hook 'focus-out-hook 'garbage-collect))
  )
(add-hook 'before-init-hook #'my|pre-init)
(add-hook 'after-init-hook #'my|post-init)



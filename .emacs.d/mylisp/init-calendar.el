;; -*- Emacs-Lisp -*-
;; lihao

;;------------------------------------------------------------
;;{{{ chinese calendar.
(require 'cal-china-x)
(setq calendar-latitude +34.16)
(setq calendar-longitude +108.54)
(setq calendar-location-name "西安")

(setq cal-china-x-important-holidays '(
				       ;; (holiday-fixed 3 13 "生日")
				       ;; (holiday-fixed 7 27 "老婆生日")
				       ))
(setq cal-china-x-general-holidays '(
			       ;;公历节日
			       (holiday-fixed 1 1 "元旦")
			       (holiday-fixed 2 14 "情人节")
			       (holiday-fixed 3 8 "妇女节")
			       (holiday-fixed 4 1 "愚人节")
			       (holiday-fixed 5 1 "劳动节")
			       (holiday-fixed 5 4 "青年节")
			       (holiday-float 5 0 2 "母亲节")
			       (holiday-fixed 6 1 "儿童节")
			       (holiday-float 6 0 3 "父亲节")
			       (holiday-fixed 7 1 "建党节")
			       (holiday-fixed 8 1 "建军节")
			       (holiday-fixed 9 10 "教师节")
			       (holiday-fixed 10 1 "国庆节")
			       (holiday-fixed 12 25 "圣诞节")
			       ;; 农历节日
			       (holiday-lunar 12 30 "除夕" 0)
			       (holiday-lunar 1 1 "春节" 0)
			       (holiday-lunar 1 15 "元宵节" 0)
			       (holiday-solar-term "清明" "清明节")
			       (holiday-lunar 5 5 "端午节" 0)
			       (holiday-lunar 7 7 "七夕节" 0)
			       (holiday-lunar 8 15 "中秋节" 0)
			       (holiday-lunar 9 9 "重阳节" 0)
                               ;; 节气，清明按节日算
			       (holiday-solar-term "小寒" "** 小寒 **")
			       (holiday-solar-term "大寒" "** 大寒 **")
			       (holiday-solar-term "立春" "** 立春 **")
			       (holiday-solar-term "雨水" "** 雨水 **")
			       (holiday-solar-term "惊蛰" "** 惊蛰 **")
			       (holiday-solar-term "春分" "** 春分 **")
			       (holiday-solar-term "谷雨" "** 谷雨 **")
			       (holiday-solar-term "立夏" "** 立夏 **")
			       (holiday-solar-term "小满" "** 小满 **")
			       (holiday-solar-term "芒种" "** 芒种 **")
			       (holiday-solar-term "夏至" "** 夏至 **")
			       (holiday-solar-term "小暑" "** 小暑 **")
			       (holiday-solar-term "大暑" "** 大暑 **")
			       (holiday-solar-term "立秋" "** 立秋 **")
                               (holiday-solar-term "处暑" "** 处暑 **")
                               (holiday-solar-term "白露" "** 白露 **")
			       (holiday-solar-term "秋分" "** 秋分 **")
                               (holiday-solar-term "寒露" "** 寒露 **")
                               (holiday-solar-term "霜降" "** 霜降 **")
			       (holiday-solar-term "立冬" "** 立冬 **")
			       (holiday-solar-term "小雪" "** 小雪 **")
                               (holiday-solar-term "大雪" "** 大雪 **")
                               (holiday-solar-term "冬至" "** 冬至 **")
	       ))

(setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays))
;; 设置Calendar的显示
(setq calendar-remove-frame-by-deleting t)
;; (setq mark-diary-entries-in-calendar t)       ; 标记有记录的日子
(setq calendar-mark-holidays-flag t)          ; 标记节假日
;; (setq calendar-mark-diary-entries-flag t)     ; 让calendar自动标记出记有待办事项的日期
(setq calendar-week-start-day 0) ; 设置星期一为每周的第一天，否则星期数有些对不上

;;除去基督徒的节日、希伯来人的节日和伊斯兰教的节日。
(setq christian-holidays nil
      hebrew-holidays nil
      islamic-holidays nil
      solar-holidays nil
      bahai-holidays nil)
;; 设置颜色
(set-face-attribute 'calendar-weekend-header nil
                    :foreground "green")
(set-face-attribute 'calendar-today nil
		    :box '(:line-width 1 :color "green")
		    :background "DarkGreen"
		    :foreground "yellow")
(set-face-attribute 'cal-china-x-general-holiday-face nil
		    :background "SkyBlue"
		    :foreground "black")
(set-face-attribute 'diary nil
		    :background "yellow"
		    :foreground "black")

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; 保存日记的文件
(setq diary-file "~/diary")
;; appointment
(setq appt-issue-message t)
;; 在mode-line上倒计时
(setq appt-display-mode-line t)

;; (setq diary-date-forms '((year "/" month "/" day "[^/0-9]"))
;;       calendar-date-display-form '(year "/" month "/" day)
;;       calendar-time-display-form
;;       '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

;; add ISO week number.
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (if (eq 1 calendar-week-start-day) (calendar-absolute-from-gregorian (list month day year))
                    (calendar-absolute-from-gregorian (list month (+ 1 day) year)))
                  )))
        'font-lock-face 'calendar-iso-week-face))
(copy-face 'font-lock-keyword-face 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
		    :foreground "DarkGoldenrod" :height 1.0)
(setq calendar-intermonth-header
      (propertize "Wk"
                  'font-lock-face 'calendar-iso-week-header-face))
(set-face-attribute 'calendar-iso-week-face nil
		    :height 1.0 :foreground "salmon")

;; diary for chinese birthday, modify diary-chinese-anniversary function.
(defun my-diary-chinese-anniversary-1 (month day &optional year mark)
  "Like `diary-anniversary' (which see) but accepts Chinese date."
  (pcase-let* ((ddate (diary-make-date month day year))
               (`(,dc ,dy ,dm ,dd)      ;diary chinese date
                (if year
                    (calendar-chinese-from-absolute
                     (calendar-chinese-to-absolute-for-diary ddate))
                  (list nil nil (calendar-extract-month ddate)
                        (calendar-extract-day ddate))))
               (`(,cc ,cy ,cm ,cd)      ;current chinese date
                (calendar-chinese-from-absolute
                 (calendar-absolute-from-gregorian (calendar-current-date))))
               (diff (if (and dc dy)
                         (+ (* 60 (- cc dc)) (- cy dy))
                       100)))
    (and (> diff 0)
         ;; The Chinese month can differ by 0.5 in a leap month.
         (or (= dm cm) (= (+ 0.5 dm) cm))
         (= dd cd)
         (cons mark (format entry diff (diary-ordinal-suffix diff))))))

;; %%(my–diary-chinese-anniversary 9 23 1993) 这是农历 1993 年 9 月 23 日生人的第 %d%s 个生日
(defun my-diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* (
             ;; (calendar-date-style 'american)
             (d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (date a-date)
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (my-diary-chinese-anniversary-1 lunar-month lunar-day y mark))
    (my-diary-chinese-anniversary-1 lunar-month lunar-day year mark)))

;;------------------------------------------------------------
;; 计算伏天和数九
;;------------------------------------------------------------
(defconst cal-china-x-nine-characters  ; 数九天 array
  ["一九" "二九" "三九" "四九" "五九" "六九" "七九" "八九" "九九"])

(defun cal-china-x-winter-solstice-date (date)  ; 计算冬至日期(1-11月为去年，12月为今年)
  "Return winter solstice(冬至) date in Gregorian form.

If MONTH = 12, return current year's date
Else return last year's date"
  (let* ((cyear (if (= (calendar-extract-month date) 12)
                    (calendar-extract-year date)
                  (1- (calendar-extract-year date)))))
    (car (rassoc '"冬至" (cal-china-x-solar-term-alist-new cyear)))))

(defun winter-solstice-day-diff (date)  ; 计算与指定冬至的天数差
  (cal-china-x-days-diff date (cal-china-x-winter-solstice-date date)))

(defun cal-china-x-get-several-nines-string (date)  ; 生成数九天的 string
  (let ((daygap (winter-solstice-day-diff date)))
    (if (or (< daygap 0) (> daygap 80))
        ""
      (concat (aref cal-china-x-nine-characters (/ daygap 9))
              "("
              (number-to-string (1+ (% daygap 9)))
              ")"
              ))))

(defun cal-china-x-solar-term-date (date solar-term)
  "Return solar-term date in Gregorian form."
  (let* ((cyear (calendar-extract-year date)))
    (car (rassoc solar-term (cal-china-x-solar-term-alist-new cyear)))))

(defun cal-china-x-chinese-day-celestial-stem-number (date)
  "String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian date)))
    (% (+ a-date 15) 10)))

(defun cal-china-x-solar-term-celestical-stem (date solar-term)
  (cal-china-x-chinese-day-celestial-stem-number
   (cal-china-x-solar-term-date date solar-term)))

(defun cal-china-x-day-diff-from-solar-term (date solar-term) ; 庚日
  (let ((ss-stem (- 7 (cal-china-x-solar-term-celestical-stem date solar-term))))
    (if (< ss-stem 0) (+ ss-stem 10)
      ss-stem)))

(defun cal-china-x-chufu-date (date)
  (let* ((ss-date (cal-china-x-solar-term-date date "夏至"))
         (ss-year (calendar-extract-year ss-date))
         (ss-day (calendar-extract-day ss-date))
         (day-diff (+ 20 (cal-china-x-day-diff-from-solar-term date "夏至")))
         (chufu-day (- day-diff (- 30 ss-day))))
    (list 7 chufu-day ss-year)))

(defun cal-china-x-zhongfu-date (date)
  (list 7 (+ (calendar-extract-day (cal-china-x-chufu-date date)) 10)
        (calendar-extract-year date)))

(defun cal-china-x-mofu-date (date)
  (let* ((ss-date (cal-china-x-solar-term-date date "立秋"))
         (ss-year (calendar-extract-year ss-date))
         (ss-day (calendar-extract-day ss-date))
         (day-diff (cal-china-x-day-diff-from-solar-term date "立秋"))
         (mofu-day (+ day-diff ss-day)))
    (list 8 mofu-day ss-year)))

(defun cal-china-x-get-futian-string (date)
  (let* ((chufu (cal-china-x-chufu-date date))
         (zhongfu (cal-china-x-zhongfu-date date))
         (mofu (cal-china-x-mofu-date date))
         (chufu-gap (cal-china-x-days-diff date chufu))
         (zhongfu-gap (cal-china-x-days-diff date zhongfu))
         (mofu-gap (cal-china-x-days-diff date mofu))
         )
    (if (or (< chufu-gap 0) (> mofu-gap 9))
        ""
      (if (and (>= chufu-gap 0) (< zhongfu-gap 0))
          (concat "初伏("
                  (number-to-string (1+ chufu-gap))
                  ")")
        (if (and (>= zhongfu-gap 0) (< mofu-gap 0))
            (concat "中伏("
                    (number-to-string (1+ zhongfu-gap))
                    ")")
          (concat "末伏("
                  (number-to-string (1+ mofu-gap))
                  ")"))))))

(defun calendar-fu-jiu-date ()
  "Day of Futian and JiuTian for date under cursor."
  (interactive)
  (let ((date (calendar-cursor-to-date)))
    (message "%s: %s%s"
             (calendar-date-string date t t)
             (cal-china-x-get-several-nines-string date)
             (cal-china-x-get-futian-string date)
             )))

(defun calendar-get-fu-jiu-string (date)
  "Day of Futian and JiuTian for date under cursor."
  (format "%s%s"
          (cal-china-x-get-several-nines-string date)
          (cal-china-x-get-futian-string date)
          ))

;; diary for chinese birthday, can be used by %%my--diary-chinese-anniversary%% in diary.
(defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (date a-date)
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

(setq calendar-mode-line-format
   '(#("<" 0 1
       (keymap
        (keymap
         (mode-line keymap
                    (mouse-1 . calendar-scroll-right)))
        mouse-face mode-line-highlight help-echo "mouse-1: previous month"))
     "Calendar"
     (cal-china-x-get-holiday date)
     (concat " "
             (calendar-date-string date t)
             (format " 第%d周"
                     (funcall
                      (if cal-china-x-custom-week-start-date 'cal-china-x-custom-week-of-date 'cal-china-x-week-of-date)
                      date)))
     (cal-china-x-chinese-date-string date)
     (calendar-get-fu-jiu-string date)
     #(">" 0 1
       (keymap
        (keymap
         (mode-line keymap
                    (mouse-1 . calendar-scroll-left)))
        mouse-face mode-line-highlight help-echo "mouse-1: next month"))))

;;------------------------------------------------------------

(message "chinese calendar loaded.")

(provide 'init-calendar)

;;}}}

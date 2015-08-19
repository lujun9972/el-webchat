;;-*- lexical-binding:t -*-
;; 以下是client端代码
(require 'url)
(require 'thingatpt)
(require 'webchat-mode)
(defvar webchat-client--total-lines 0
  "webchat客户端已经收到多少行聊天记录")
(defun webchat-client--get-content(host port)
  (let ((buf (url-retrieve-synchronously (format "http://%s:%s/?start=%s" host port webchat-client--total-lines) t))
		content)
	(unwind-protect 
		(with-current-buffer buf
		  (goto-char (point-min))
		  (search-forward-regexp "^$")
		  (setq content (read-from-whole-string (buffer-substring-no-properties (+ (point ) 1) (point-max)))))
	  (kill-buffer buf))
	(setq webchat-client--total-lines (car content))
	(decode-coding-string (cdr content) 'utf-8-dos)))

(defun webchat-client--say(host port who content)
  (let ((url (format "http://%s:%s/update/"
					 host
					 port))
		(url-request-method "POST")
		(url-request-extra-headers
		 '(("Content-Type" . "application/x-www-form-urlencoded")))
		(url-request-data (format "who=%s&content=%s" (url-hexify-string who) (url-hexify-string content))))
	(url-retrieve url
				  (lambda (status)
					(kill-buffer (current-buffer)))
				  nil
				  t)))
(defvar webchat-client-content-buffer "*webchat-content*"
  "显示聊天内容的buffer")
(defun webchat-client--display-content(host port)
  "在buffer内显示聊天内容"
  (let ((content (webchat-client--get-content host port))
		(webchat-client-content-window (get-buffer-window (get-buffer-create webchat-client-content-buffer))))
	(save-selected-window
	  (save-excursion 
		(when webchat-client-content-window
		  (select-window webchat-client-content-window))
		(with-current-buffer webchat-client-content-buffer
		  (goto-char (point-max))
		  (insert content))))))


(defun webchat-client--talk (host port who)
  (interactive)
  "Function called when return is pressed in interactive mode to talk"
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (webchat-client--say host port who content)
	(erase-buffer)))

(defvar webchat-client--timer nil)
(defvar webchat-client-talk-buffer "*webchat-talk*"
  "输入聊天内容的buffer")
(defun webchat-talk (host port who)
  (interactive (list (read-string "请输入服务器地址: " "localhost")
					 (read-number "请输入服务端口: " 8000)
					 (read-string "请输入你的名称: " user-login-name)))
  (switch-to-buffer (get-buffer-create webchat-client-content-buffer))
  (select-window (split-window-below -4))
  (switch-to-buffer (get-buffer-create webchat-client-talk-buffer))
  (webchat-mode)
  (local-set-key (kbd "<RET>") (lambda ()
								 (interactive)
								 (webchat-client--talk host port who)))
  ;; (setq webchat-client--timer (run-with-idle-timer 1 0.3 (lambda ()
  ;; 														   (webchat-client--display-content host port))))
  (setq webchat-client--timer (run-with-timer 1 0.3 (lambda ()
													  (webchat-client--display-content host port)))))

(defun webchat-quit ()
  (interactive)
  (cancel-timer webchat-client--timer)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))

(provide 'webchat-client)

;; 以下是server端代码
(require 'elnode)
(defvar webchat-server--total-lines 0
  "聊天室中总用有多少行内容")
(defvar webchat-server-max-hold-lines 1000
  "服务端最多保存多少行聊天内容")
(defvar webchat-server--content-ring (make-ring webchat-server-max-hold-lines)
  "聊天内容")
(defun webchat-server--get-content-start-from (start)
  "根据请求的起始行,产生实际应该返回的内容"
  (let* ((new-content-lines (- webchat-server--total-lines start))
		 (new-content-list (cond ((= new-content-lines 0)
								  nil)
								 (t (subseq (ring-elements webchat-server--content-ring) (- new-content-lines))))))
	(apply #'concat new-content-list)))

(defun webchat-server--get-content-handler (httpcon)
  (let* ((start (string-to-number (or (elnode-http-param httpcon "start") "0")))
		 (new-content (webchat-server--get-content-start-from start)))
	(elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
	(elnode-http-return httpcon (prin1-to-string (cons webchat-server--total-lines new-content)))))

(defun webchat-server--say-handler (httpcon)
  (let ((who (elnode-http-param httpcon "who"))
		(content (elnode-http-param httpcon "content")))
	(when (stringp content)
	  (ring-insert-at-beginning webchat-server--content-ring (format "%s:\n\t%s\n" who content))
	  (incf webchat-server--total-lines)))
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  ;; (elnode-http-start httpcon 302 '("Location" . "/"))
  (elnode-http-return httpcon))

(defconst webchat-urls
  `(("^/$" . webchat-server--get-content-handler)
	("^/update/.*$" . webchat-server--say-handler)))


(defun webchat-server--dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon webchat-urls))

(defun webchat-server()
  (interactive)
  (let ((port (read-number "请输入监听端口: ")))
	(elnode-start 'webchat-server--dispatcher-handler :port port)))

;; 以下是client端代码
(require 'url)
(defvar webchat-client-server-host "localhost"
  "webchat的服务器地址")
(defvar webchat-client-server-port 8000
  "webchat的服务器监听端口")
(defvar webchat-client--total-lines 0
  "webchat客户端已经收到多少行聊天记录")
(defun webchat-client--get-content(&optional host port)
  (setq host (or host webchat-client-server-host))
  (setq port (or port webchat-client-server-port))
  (let ((buf (url-retrieve-synchronously (format "http://%s:%s/?start=%s" host port webchat-client--total-lines) t))
		content)
	(with-current-buffer buf
	  (goto-char (point-min))
	  (search-forward-regexp "^$")
	  (setq content (read-from-whole-string (buffer-substring-no-properties (+ (point )1) (point-max)))))
	(kill-buffer buf)
	(setq webchat-client--total-lines (car content))
	(decode-coding-string (cdr content) 'utf-8-dos)))

(defun webchat-client--say(who content &optional host port)
  (setq host (or host webchat-client-server-host))
  (setq port (or port webchat-client-server-port))
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
(defun webchat-client--display-content()
  "在buffer内显示聊天内容"
  (let ((content (webchat-client--get-content))
		(webchat-client-content-window (get-buffer-window (get-buffer-create webchat-client-content-buffer))))
	(save-selected-window
	  (save-excursion 
		(when webchat-client-content-window
		  (select-window webchat-client-content-window))
		(with-current-buffer webchat-client-content-buffer
		  (goto-char (point-max))
		  (insert content))))))


(define-derived-mode webchat-mode text-mode "WebChat"
  "Major mode for running webchat"
  (make-local-variable 'scroll-step)
  (setq scroll-step 2)
  (local-set-key (kbd "<RET>") #'webchat-client--talk))

(defvar webchat-client-who user-login-name
  "webchat客户名")
(defun webchat-client--talk ()
  (interactive)
  "Function called when return is pressed in interactive mode to talk"
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (webchat-client--say webchat-client-who content)
	(erase-buffer)))

(defvar webchat-client--timer nil)
(defvar webchat-client-talk-buffer "*webchat-talk*"
  "输入聊天内容的buffer")
(defun webchat-talk ()
  (interactive)
  (setq webchat-client-server-host (read-string "请输入服务器地址: " webchat-client-server-host))
  (setq webchat-client-server-port (read-number "请输入服务端口: " webchat-client-server-port))
  (setq webchat-client-who (read-string "请输入你的名称: " webchat-client-who))
  (switch-to-buffer (get-buffer-create webchat-client-content-buffer))
  (select-window (split-window-below -4))
  (switch-to-buffer (get-buffer-create webchat-client-talk-buffer))
  (webchat-mode)
  ;; (setq webchat-client--timer (run-with-idle-timer 1 0.3 #'webchat-client--display-content))
  (setq webchat-client--timer (run-with-timer 1 0.3 #'webchat-client--display-content)))

(defun webchat-quit ()
  (interactive)
  (cancel-timer webchat-client--timer)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))

(provide 'webChat)

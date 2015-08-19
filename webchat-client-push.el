;;-*- lexical-binding:t -*-
;; 以下是client端代码
(require 'url)
(require 'thingatpt)
(add-to-list 'load-path default-directory)
(require 'webchat-mode)
(defvar webchat-client--total-lines 0
  "webchat客户端已经收到多少行聊天记录")
(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
		(url-request-extra-headers
		 '(("Content-Type" . "application/x-www-form-urlencoded")))
		(url-request-data
		 (mapconcat (lambda (arg)
					  (concat (url-hexify-string (format "%s" (car arg)))
							  "="
							  (url-hexify-string (format "%s" (cdr arg)))))
					args
					"&")))
	(url-retrieve url (lambda (status)
						(kill-buffer (current-buffer))) nil t)))

(defun webchat-client--say(host port who content)
  (let ((url (format "http://%s:%s/update/" host port)))
	(url-http-post url `((who . ,who) (content . ,content)))))

(defvar webchat-client-content-buffer "*webchat-content*"
  "显示聊天内容的buffer")

(defun webchat-client--display-content(proc content)
  "在buffer内显示聊天内容"
  (let ((webchat-client-content-window (get-buffer-window (get-buffer-create webchat-client-content-buffer))))
	(save-selected-window
	  (save-excursion 
		(when webchat-client-content-window
		  (select-window webchat-client-content-window))
		(with-current-buffer webchat-client-content-buffer
		  (goto-char (point-max))
		  (insert content))))))

(defvar webchat-client-talk-buffer "*webchat-talk*"
  "输入聊天内容的buffer")

(defun webchat-client--register-as-push-client (host port listener)
  (let ((url (format "http://%s:%s/register-as-push/" host port)))
	(url-http-post url `(("port" . ,listener)))))

(defvar webchat-client--process nil)
(defun webchat-talk (host port listener who)
  (interactive (list (read-string "请输入服务器地址: " "localhost")
					 (read-number "请输入服务端口: " 8000)
					 (read-number "请输入客户端的监听端口: " 9000)
					 (read-string "请输入你的名称: " user-login-name)))
  (switch-to-buffer (get-buffer-create webchat-client-content-buffer))
  (select-window (split-window-below -4))
  (switch-to-buffer (get-buffer-create webchat-client-talk-buffer))
  (webchat-mode)
  (local-set-key (kbd "<RET>") (lambda  ()
								 (interactive)
								 "Function called when return is pressed in interactive mode to talk"
								 (let ((content (buffer-substring-no-properties (point-min) (point-max))))
								   (webchat-client--say host port who content)
								   (erase-buffer))))
  (webchat-client--register-as-push-client host port listener)
  (setq webchat-client--process
		(make-network-process :name "webchat-client-content"
							  :server t
							  :service listener
							  :buffer webchat-client-content-buffer
							  :filter #'webchat-client--display-content)))

(defun webchat-quit ()
  (interactive)
  (kill-process webchat-client--process)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))

(provide 'webchat-client)

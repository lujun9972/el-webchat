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
		  (let ((inhibit-read-only t)
				(pos (point-max)))
			(insert content)
			(webchat-display-inline-images-async nil t pos (point-max))))))))

(defvar webchat-client-talk-buffer "*webchat-talk*"
  "输入聊天内容的buffer")

(defun webchat-client--register-as-push-client (host port listener)
  (let ((url (format "http://%s:%s/register-as-push/" host port)))
	(url-http-post url `(("port" . ,listener)))))

(defvar webchat-client--process nil)
(defun webchat-talk (host port listener who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
					 (read-number "请输入客户端的监听端口: " 9000)
					 (read-string "请输入你的名称: " user-login-name)))
  (webchat-build-window webchat-client-content-buffer webchat-client-talk-buffer)
  (local-set-key (kbd "<C-return>") (lambda  ()
								 (interactive)
								 "Function called when return is pressed in interactive mode to talk"
								 (let ((content (buffer-substring-no-properties (point-min) (point-max))))
								   (webchat-client--say host port who content)
								   (erase-buffer))))
  (webchat-client--register-as-push-client host port listener)
  (setq webchat-client--process
		(make-network-process :name "webchat-client-content"
							  :family 'ipv4
							  :server t
							  :service listener
							  :buffer webchat-client-content-buffer
							  :filter #'webchat-client--display-content))
  (set-process-coding-system webchat-client--process 'utf-8 'utf-8))

(defun webchat-quit ()
  (interactive)
  (delete-process webchat-client--process)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))

(defun webchat-client--select-from-alist (alist &optional prompt)
  ""
  (unless prompt
	(setq prompt ""))
  (let* ((keys (mapcar #'car alist))
		 ;; (ido-separator "\n")
		 (key (ido-completing-read prompt keys))
		 (value (cdr (assoc key alist))))
	(cond ((atom value)
		   value)
		  (t (cdr value)))))


(defun webchat-client(host port listener who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
					 (read-number "请输入客户端的监听端口: " 9000)
					 (read-string "请输入你的名称: " user-login-name)))
  (let ((p1 (make-network-process :name "webchat-dispatcher"
								  :buffer "*webchat-dispatcher*"
								  :family 'ipv4
								  :host host
								  :service port)))
	(while (not (eq (process-status p1) 'closed))
	  (accept-process-output p1 0.1))
	(with-current-buffer (process-buffer p1)
	  (setq port (webchat-client--select-from-alist  (car (read-from-string (buffer-substring-no-properties (point-min) (point-max)))) "channel: "))
	  (delete-process p1)
	  (kill-buffer))
	(webchat-talk host port listener who)))
(provide 'webchat-client)

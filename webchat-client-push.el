;;-*- lexical-binding:t -*-
;; 以下是client端代码
(require 'url)
(require 'thingatpt)
(add-to-list 'load-path default-directory)
(require 'webchat-misc)
(require 'webchat-mode)

(defgroup webchat-client nil
  "webchat客户端配置")

(defcustom webchat-client-content-buffer "*webchat-content*"
  "显示聊天内容的buffer name"
  :type 'string)

(defcustom webchat-client-talk-buffer "*webchat-talk*"
  "输入聊天内容的buffer name"
  :type 'string)

(defcustom webchat-client-display-image t
  "是否显示聊天内容中图片链接所指向的图片"
  :type 'boolean)

(defcustom webchat-client-notification-by-sound t
  "收到消息时,是否发声通知"
  :type 'boolean)

(defcustom webchat-client-notification-sound-file nil
  "收到消息时,通知的声音文件(wav,或au格式). 若为nil则直接调用beep函数通知"
  :type '(choice (file :must-match t)
				 (const nil)))

(when (featurep 'dbusbind)
  (require 'notifications)
  (defcustom webchat-client-desktop-notification t
	"收到消息时,是否使用desktop-notification通知")
  (defvar webchat-client-last-desktop-notification-id nil
	"上一次通知的notification id"))

(defun webchat-client--SAY-RESPONSE (proc content)
  "在buffer内显示聊天内容"
  (let ((webchat-client-content-window (get-buffer-window (get-buffer-create webchat-client-content-buffer)))
		title body)
	(string-match "\\(^[^\r\n]+\\)[\r\n]\\(.+\\)$" content)
	(setq title (match-string 1 content))
	(setq body (match-string 2 content))
	(save-selected-window
	  (save-excursion 
		(when webchat-client-content-window
		  (select-window webchat-client-content-window))
		(with-current-buffer webchat-client-content-buffer
		  (goto-char (point-max))
		  (let ((inhibit-read-only t)
				(pos (point-max)))
			(insert (decode-coding-string  content 'utf-8))
			(when webchat-client-display-image
			  (webchat-display-inline-images-async nil t pos (point-max)))
			(when webchat-client-notification-by-sound
			  (if webchat-client-notification-sound-file
				  (play-sound-file webchat-client-notification-sound-file)
				(ding 1)))
			(when (and (boundp 'webchat-client-desktop-notification)
					   webchat-client-desktop-notification)
			  (setq webchat-client-last-desktop-notification-id
					(notifications-notify :title title
										  :body body
										  :replaces-id webchat-client-last-desktop-notification-id)))))))))

(defun webchat-client--UPLOAD-RESPONSE (proc http-port upload-file-path)
  "在buffer中插入upload file url"
  (let ((host (process-contact proc :host)))
	(with-current-buffer webchat-client-talk-buffer
	  (goto-char (point-max))
	  (insert (format "[[http://%s:%s/%s]]" host http-port upload-file-path)))))

(defvar webchat-client--process nil)

(defun webchat-client-upload-file (&optional file)
  (let* ((file (or file (read-file-name "upload file:")))
		 (file-data (with-temp-buffer
					  (insert-file-contents-literally file)
					  (buffer-string))))
	(lispy-process-send webchat-client--process 'UPLOAD file file-data)))

(defun webchat-client-screenshot-upload (&optional file)
  "`file'为截屏产生的临时文件名称(不带后缀名,因为后缀只能为png格式的文件). 默认为myscreen"
  (setq file (or file "myscreen"))
  (call-process "jp.sh" nil nil nil file)
  (webchat-client-upload-file (format "%s.png" file)))

(defun webchat-client-toggle-image ()
  (setq webchat-client-display-image (not webchat-client-display-image)))

(defun webchat-talk (host port who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
					 (read-string "请输入你的名称: " user-login-name)))

  (setq webchat-client--process
		(make-lispy-network-process :name "webchat-client-content"
									:family 'ipv4
									:host host
									:service port
									;; :coding 'raw-text
									;; :buffer webchat-client-content-buffer
									:filter (lambda (proc &rest objs)
											  (let* ((cmd (car objs))
													 (cmd-fn (intern (format "webchat-client--%s" cmd)))
													 (args (cdr objs)))
												(apply cmd-fn proc args)))))

  (webchat-build-window webchat-client-content-buffer webchat-client-talk-buffer
						(list "上传文件"
							  (lambda (btn)
								(webchat-client-upload-file)))
						(list (if webchat-client-display-image
								  "不再显示图片"
								"显示图片")
							  (lambda (btn)
								(let ((inhibit-read-only t))
								  (webchat-client-toggle-image)
								  (set-button-label btn (if webchat-client-display-image
															"不再显示图片"
														  "显示图片")))))
						(list "截屏"
							  (lambda (btn)
								(webchat-client-screenshot-upload))))
  
  (local-set-key (kbd "<C-return>") (lambda ()
									  (interactive)
									  "Function called when return is pressed in interactive mode to talk"
									  (goto-char (point-min))
									  (forward-line)
									  (let ((content (encode-coding-string  (delete-and-extract-region (point) (point-max)) 'utf-8)))
										(lispy-process-send webchat-client--process 'SAY who content)))))

(defun webchat-quit ()
  (interactive)
  (delete-process webchat-client--process)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))


(defun webchat-client(host port who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入webchat服务端口: " 8000)
					 (read-string "请输入你的名称: " user-login-name)))
  (let* ((p1 (make-network-process :name "webchat-dispatcher"
								   :buffer "*webchat-dispatcher*"
								   :family 'ipv4
								   :host host
								   :service port))
		 (channel-list (read-from-process-wait p1))
		 (channel (ido-completing-read "channel: " channel-list)))
	(write-to-process p1 'REQUEST-CHANNEL-PORT channel)
	(setq port (caar (read-from-process-wait p1)))
	(with-current-buffer (process-buffer p1)
	  (delete-process p1)
	  (kill-buffer))
	(webchat-talk host port who)))

(provide 'webchat-client)

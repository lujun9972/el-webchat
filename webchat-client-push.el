;;-*- lexical-binding:t -*-
;; 以下是client端代码
(require 'url)
(require 'thingatpt)
(add-to-list 'load-path default-directory)
(require 'webchat-misc)
(require 'webchat-mode)

(defun webchat-client--say(host port who content)
  (let ((url (format "http://%s:%s/update/" host port)))
	(url-http-post url `((who . ,who) (content . ,content)))))

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
			(when webchat-client-display-image
			  (webchat-display-inline-images-async nil t pos (point-max)))))))))

(defvar webchat-client--process nil)

(defun webchat-talk (host port listener who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
					 (read-number "请输入客户端的监听端口: " 9000)
					 (read-string "请输入你的名称: " user-login-name)))

  (defun webchat-client-upload-file (&optional file)
	(interactive)
	(setq file (or file (read-file-name "upload file:")))
	(let* ((upload-url (format "http://%s:%s/upload/" host port))
		   (upload-file-path (car  (url-upload-file upload-url file))))
	  (with-current-buffer webchat-client-talk-buffer
		(goto-char (point-max))
		(insert (format "[[http://%s:%s/%s]]" host port upload-file-path)))))

  (webchat-build-window webchat-client-content-buffer webchat-client-talk-buffer)
  (with-current-buffer webchat-client-talk-buffer
	(insert-function-button "upload file" (lambda (btn)
											(webchat-client-upload-file)))
	(insert "\t")
	(insert-function-button "show images" (lambda (btn)
											(setq webchat-client-display-image t)))
	(insert "\t")
	(insert-function-button "no images" (lambda (btn)
											(setq webchat-client-display-image nil)))

	(newline)
	(add-text-properties (point-min) (point) '(read-only t rear-nonsticky t)))
  (local-set-key (kbd "<C-return>") (lambda ()
									  (interactive)
									  "Function called when return is pressed in interactive mode to talk"
									  (goto-char (point-min))
									  (forward-line)
									  (let ((content (delete-and-extract-region (point) (point-max))))
										(webchat-client--say host port who content))))
  (setq webchat-client--process
		(make-network-process :name "webchat-client-content"
							  :family 'ipv4
							  :server nil
							  :service listener
							  :buffer webchat-client-content-buffer
							  :filter #'webchat-client--display-content))
  (set-process-coding-system webchat-client--process 'utf-8 'utf-8))

(defun webchat-quit ()
  (interactive)
  (delete-process webchat-client--process)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))


(defun webchat-client(host port listener who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
					 (read-number "请输入客户端的监听端口: " 9000)
					 (read-string "请输入你的名称: " user-login-name)))
  (let* ((p1 (make-network-process :name "webchat-dispatcher"
								   :buffer "*webchat-dispatcher*"
								   :family 'ipv4
								   :host host
								   :service port))
		 (channel-list (read-from-process-wait p1))
		 (channel (ido-completing-read "channel: " channel-list)))
	(write-to-process p1 'REQUEST-CHANNEL-PORT channel)
	(setq port (car (read-from-process-wait p1)))
	(with-current-buffer (process-buffer p1)
	  (delete-process p1)
	  (kill-buffer))
	(webchat-talk host port listener who)))

(provide 'webchat-client)

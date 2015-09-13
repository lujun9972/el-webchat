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

(defun webchat-client--SAY-RESPONSE (proc content)
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

(defun webchat-client--UPLOAD-RESPONSE (proc http-port upload-file-path)
  "在buffer中插入upload file url"
  (let ((host (process-contact proc :host)))
	(with-current-buffer webchat-client-talk-buffer
	  (goto-char (point-max))
	  (insert (format "[[http://%s:%s/%s]]" host http-port upload-file-path)))))

(defvar webchat-client--process nil)

(defun webchat-client-upload-file (&optional file)
  (let* ((file (read-file-name "upload file:"))
		 (file-data (with-temp-buffer
					  (insert-file-contents-literally file)
					  (buffer-string))))
	(lispy-process-send webchat-client--process 'UPLOAD file file-data)))

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

  (webchat-build-window webchat-client-content-buffer webchat-client-talk-buffer)
  
  (local-set-key (kbd "<C-return>") (lambda ()
									  (interactive)
									  "Function called when return is pressed in interactive mode to talk"
									  (goto-char (point-min))
									  (forward-line)
									  (let ((content (delete-and-extract-region (point) (point-max))))
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

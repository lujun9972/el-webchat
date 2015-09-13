;;-*- lexical-binding:t -*-
;; 以下是client端代码
(require 'url)
(require 'thingatpt)
(add-to-list 'load-path default-directory)
(require 'webchat-misc)
(require 'webchat-mode)

(defvar webchat-client--total-lines 0
  "webchat客户端已经收到多少行聊天记录")

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
  (let ((url (format "http://%s:%s/update/" host port)))
	(url-http-post url `((who . ,who) (content . ,content)))))

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
		  (let ((inhibit-read-only t)
				(pos (point)))
			(insert content)
			(when webchat-client-display-image
			  (webchat-display-inline-images-async nil t pos (point-max)))))))))


(defun webchat-client--talk (host port who)
  (interactive)
  "Function called when return is pressed in interactive mode to talk"
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (webchat-client--say host port who content)
	(erase-buffer)))

(defun webchat-client-toggle-image ()
  (setq webchat-client-display-image (not webchat-client-display-image)))

(defvar webchat-client--timer nil)

(defun webchat-talk (host port who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
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
  
  (local-set-key (kbd "<C-return>") (lambda ()
									  (interactive)
									  "Function called when return is pressed in interactive mode to talk"
									  (goto-char (point-min))
									  (forward-line)
									  (let ((content (delete-and-extract-region (point) (point-max))))
										(webchat-client--say host port who content))))
  (setq webchat-client--timer (run-with-idle-timer 1 0.3 (lambda ()
   														   (webchat-client--display-content host port))))
  ;; (setq webchat-client--timer (run-with-timer 1 0.3 (lambda ()
  ;; 													  (webchat-client--display-content host port))))
  )

(defun webchat-quit ()
  (interactive)
  (cancel-timer webchat-client--timer)
  (select-window (get-buffer-window webchat-client-content-buffer))
  (kill-buffer-and-window))


(defun webchat-client(host port who)
  (interactive (list (read-string "请输入服务器地址: " "127.0.0.1")
					 (read-number "请输入服务端口: " 8000)
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
	(webchat-talk host port who)))

(provide 'webchat-client)

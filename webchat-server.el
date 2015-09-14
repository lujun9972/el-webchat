#! /usr/bin/emacs --script
;; 以下是server端代码
(unless (and (boundp 'package--initialized)
			 package--initialized)
  (package-initialize))
(require 'simple-httpd)
(require 'subr-x)
(require 'cl)
(add-to-list 'load-path default-directory)
(require 'webchat-misc)

(defvar webchat-server--push-client-connections nil)
(defvar webchat-server--content-sender-process nil
  "发送聊天内容的network process")
(defun webchat-server--create-content-sender-process (port)
  "创建用于发送聊天内容的network process"
  (when webchat-server--content-sender-process
	(delete-process webchat-server--content-sender-process))
  (setq webchat-server--content-sender-process
		(make-lispy-network-process :name "webchat-server-content"
							  :family 'ipv4
							  :server t
							  :service port
							  ;; :coding 'raw-text
							  :log (lambda (server connection msg)
									 "将新建的链接,存入`webchat-server--push-client-connection'中"
									 (message "log:%s,%s,%s" server connection msg)
									 (add-to-list 'webchat-server--push-client-connections connection))
							  :filter (lambda (connection &rest objs)
										"转发收到的聊天内容"
										(ignore-errors
										  (let* ((cmd (car objs))
												 (cmd-fn (intern (format "webchat-server--%s" cmd)))
												 (args (cdr objs)))
											(apply cmd-fn connection args))))
							  :sentinel (lambda (proc event)
										  "从`webchat-server--push-client-connection'中删除关闭的链接"
										  (message "sentinel:%s" event)
										  (when  (cl-some (lambda (reg)
															(string-match-p reg event))
														  '("finished" "exited" "connection broken"))
											(setq webchat-server--push-client-connections (remove proc webchat-server--push-client-connections))
											(delete-process proc))))))

(defun webchat-server--format-message (who content)
  "格式化聊天内容"
  (format "* %s-<%s>:\n%s\n" who (current-time-string)  content))

(defun webchat-server--SAY (proc who content)
  (mapc (lambda (proc)
		  (lispy-process-send proc 'SAY-RESPONSE (webchat-server--format-message who content)))
		webchat-server--push-client-connections))

(defun webchat-server--UPLOAD (proc upload-file-name upload-file-data)
  (let ((upload-file-path (format "upload-files/%s.%s" (md5 upload-file-data) (file-name-extension  upload-file-name))))
	(when (stringp upload-file-data)
	  (with-temp-file upload-file-path
		(insert  upload-file-data)))
	(lispy-process-send proc 'UPLOAD-RESPONSE httpd-port upload-file-path)))

;; (defun webchat-server--REQUIRE-FILE (proc require-file-path start end)
;;   (let ((file-data (with-temp-buffer
;; 					 (insert-file-literally require-file-path)
;; 					 (buffer-string))))
;; 	(lispy-process-send proc 'REQUIRE-FILE-RESPONSE file-data start end)))


(defun webchat-server(port &optional http-port)
  (interactive `(,(read-number "请输入监听端口" 8000)))
  (webchat-server--create-content-sender-process port)
  (setq httpd-port (or http-port (next-unused-port (+ 1 port))))
  (httpd-serve-directory default-directory))


(provide 'webchat-server)

;; 以下操作是为了兼容#!emacs --script方式
(when (member "-scriptload" command-line-args)
  (let ((port (string-to-number (car command-line-args-left)))
		(http-port (ignore-errors (string-to-number (cadr command-line-args-left)))))
	(webchat-server port http-port)
	(while t
	  (sit-for 1))
	(setq command-line-args-left nil)))


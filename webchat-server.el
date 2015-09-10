#! /usr/bin/emacs --script
;; 以下是server端代码
(unless (and (boundp 'package--initialized)
			 package--initialized)
  (package-initialize))
(require 'elnode)
(require 'subr-x)
(require 'cl)
(add-to-list 'load-path default-directory)
(require 'elnode-fix)

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

(defvar webchat-server--push-client-connections nil)
(defvar webchat-server--content-sender-process nil
  "发送聊天内容的network process")
(defun webchat-server--create-content-sender-process (port)
  "创建用于发送聊天内容的network process"
  (when webchat-server--content-sender-process
	(delete-process webchat-server--content-sender-process))
  (setq webchat-server--content-sender-process
		(make-network-process :name "webchat-client-content"
							  :family 'ipv4
							  :server t
							  :service port
							  ;; :coding 'utf-8-dos
							  :log (lambda (server connection msg)
									 "将新建的链接,存入`webchat-server--push-client-connection'中"
									 (message "log:%s,%s,%s" server connection msg)
									 (add-to-list 'webchat-server--push-client-connections connection))
							  :filter (lambda (connection msg)
										"转发收到的聊天内容"
										(mapc (lambda (conn)
												(process-send-string conn msg))
											  webchat-server--push-client-connections))
							  :sentinel (lambda (proc event)
										  "从`webchat-server--push-client-connection'中删除关闭的链接"
										  (message "sentinel:%s" event)
										  (when  (cl-some (lambda (reg)
															(string-match-p reg event))
														  '("finished" "exited" "connection broken"))
											(setq webchat-server--push-client-connections (remove proc webchat-server--push-client-connections)))))))

(defun webchat-server--format-message (who content)
  "格式化聊天内容"
  (format "* %s-<%s>:\n%s\n" who (current-time-string)  content))

(defun webchat-server--say-handler (httpcon)
  (let ((who (elnode-http-param httpcon "who"))
		(content (elnode-http-param httpcon "content")))
	(when (stringp content)
	  (ring-insert-at-beginning webchat-server--content-ring (webchat-server--format-message who content))
	  (setq webchat-server--total-lines (1+ webchat-server--total-lines))
	  (mapc (lambda (proc)
			  (process-send-string proc (webchat-server--format-message who content)))
			 webchat-server--push-client-connections)))
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  (elnode-http-return httpcon))

(defun webchat-server--upload-handler (httpcon)
  (let* ((upload-file (elnode-http-param httpcon "uploadfile"))
		 (upload-file-name (get-text-property 0 :elnode-filename upload-file))
		 (upload-file-path (format "upload-files/%s.%s" (md5 upload-file) (file-name-extension  upload-file-name))))
	(when (stringp upload-file)
	  (with-temp-file upload-file-path
		(insert (string-as-multibyte upload-file))))
	(elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
	(elnode-http-return httpcon upload-file-path)))

(fset 'webchat-server--upload-files-handler (elnode-webserver-handler-maker default-directory)) ;此处doc-root貌似只能用default-directory不能用"./",不要问我为什么,我想静静......

(defconst webchat-urls
  `(("^/$" . webchat-server--get-content-handler)
	("^/upload/.*$" . webchat-server--upload-handler)
	("^/upload-files/.*$" . webchat-server--upload-files-handler)
	("^/update/.*$" . webchat-server--say-handler)))


(defun webchat-server--dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon webchat-urls))

(defun webchat-server(port)
  (interactive `(,(read-number "请输入监听端口" 8000)))
  (webchat-server--create-content-sender-process 9000)
  (elnode-start 'webchat-server--dispatcher-handler :port port))


(provide 'webchat-server)

;; 以下操作是为了兼容#!emacs --script方式
(when (member "-scriptload" command-line-args)
  (let ((port (string-to-number (car command-line-args-left))))
  (webchat-server port)
  (while t
	(sit-for 1))
  (setq command-line-args-left nil)))

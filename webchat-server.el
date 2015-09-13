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
(require 'webchat-misc)


;; 以下为pull模式的代码
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


(defun webchat-server--format-message (who content)
  "格式化聊天内容"
  (format "* %s-<%s>:\n%s\n" who (current-time-string)  content))

(defun webchat-server--say-handler (httpcon)
  (let ((who (elnode-http-param httpcon "who"))
		(content (elnode-http-param httpcon "content")))
	(when (stringp content)
	  (webchat-server--SAY-internal who content)	;兼容push模式
	  (webchat-server--say-handler-internal who content)))
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  (elnode-http-return httpcon))

(defun webchat-server--say-handler-internal (who content)
  (ring-insert-at-beginning webchat-server--content-ring (webchat-server--format-message who content))
  (setq webchat-server--total-lines (1+ webchat-server--total-lines)))

(defun webchat-server--upload-handler (httpcon)
  (let* ((upload-file (elnode-http-param httpcon "uploadfile"))
		 (upload-file-name (get-text-property 0 :elnode-filename upload-file))
		 (upload-file-path (format "upload-files/%s.%s" (md5 upload-file) (file-name-extension  upload-file-name))))
	(when (stringp upload-file)
	  (with-temp-file (format "%s/%s" default-directory upload-file-path)
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

;; 以下为push模式的server代码
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
										(let* ((cmd (car objs))
											   (cmd-fn (intern (format "webchat-server--%s" cmd)))
											   (args (cdr objs)))
										  (apply cmd-fn connection args)))
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
  (webchat-server--SAY-internal who content)
  (webchat-server--say-handler-internal who content))

(defun webchat-server--SAY-internal (who content)
  (mapc (lambda (proc)
		  (lispy-process-send proc 'SAY-RESPONSE (webchat-server--format-message who content)))
		webchat-server--push-client-connections))

(defun webchat-server--UPLOAD (proc upload-file-name upload-file-data)
  (let ((upload-file-path (format "upload-files/%s.%s" (md5 upload-file-data) (file-name-extension  upload-file-name))))
	(when (stringp upload-file-data)
	  (with-temp-file upload-file-path
		(insert  upload-file-data)))
	(lispy-process-send proc 'UPLOAD-RESPONSE webchat-server--http-port upload-file-path)))

;; (defun webchat-server--REQUIRE-FILE (proc require-file-path start end)
;;   (let ((file-data (with-temp-buffer
;; 					 (insert-file-literally require-file-path)
;; 					 (buffer-string))))
;; 	(lispy-process-send proc 'REQUIRE-FILE-RESPONSE file-data start end)))


(defvar webchat-server--http-port nil)

(defun webchat-server(port &optional http-port)
  (interactive `(,(read-number "请输入监听端口" 8000)))
  (webchat-server--create-content-sender-process port)
  (setq webchat-server--http-port (or http-port (next-unused-port (+ 1 port))))
  (elnode-start 'webchat-server--dispatcher-handler :port webchat-server--http-port))


(provide 'webchat-server)

;; 以下操作是为了兼容#!emacs --script方式
(when (member "-scriptload" command-line-args)
  (let ((port (string-to-number (car command-line-args-left)))
		(http-port (ignore-errors (string-to-number (cadr command-line-args-left)))))
	(webchat-server port http-port)
	(while t
	  (sit-for 1))
	(setq command-line-args-left nil)))


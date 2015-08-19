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

(defvar webchat-server--push-client-connections nil)
(defun webchat-server--register-as-push-handler (httpcon)
  "客户端注册为服务器主动推送消息"
  (let ((port (string-to-number (or (elnode-http-param httpcon "port") "9000")))
		(host (process-contact httpcon :host)))
	(add-to-list 'webchat-server--push-client-connections (open-network-stream "push-client" "push-client" host port))))

(defun webchat-server--say-handler (httpcon)
  (let ((who (elnode-http-param httpcon "who"))
		(content (elnode-http-param httpcon "content")))
	(when (stringp content)
	  (ring-insert-at-beginning webchat-server--content-ring (format "%s:\n\t%s\n" who content))
	  (incf webchat-server--total-lines)
	  (mapc (lambda (proc)
			  (process-send-string proc (format "%s:\n\t%s\n" who content))) webchat-server--push-client-connections)))
  (elnode-http-start httpcon 200 '("Content-Type" . "text/plain"))
  ;; (elnode-http-start httpcon 302 '("Location" . "/"))
  (elnode-http-return httpcon))

(defconst webchat-urls
  `(("^/$" . webchat-server--get-content-handler)
	("^/register-as-push/.*$" . webchat-server--register-as-push-handler)
	("^/update/.*$" . webchat-server--say-handler)))


(defun webchat-server--dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon webchat-urls))

(defun webchat-server()
  (interactive)
  (let ((port (read-number "请输入监听端口: ")))
	(elnode-start 'webchat-server--dispatcher-handler :port port)))

(provide 'webchat-server)

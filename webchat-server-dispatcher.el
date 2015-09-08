(defvar webchat-server-topic-port-alist '(("cartoon" . 8001) ("emacs" . 8002))
  "主题与端口的对应表")
(defvar webchat-server-dispatcher-process nil)

(defun webchat-server--build-server-process (port)
  "创建webchat-server进程"
  (let ((emacs-bin-path (concat invocation-directory invocation-name)))
	(start-process "webchat-server" "webchat-server" emacs-bin-path "--script" "webchat-server.el" (number-to-string port))))


(defun webchat-server-dispatcher (port)
  (interactive `(,(read-number "请输入服务端口: " 8000)))
  (when webchat-server-dispatcher-process
	(delete-process webchat-server-dispatcher-process))
  (setq webchat-server-dispatcher-process
		(make-network-process :name "webchat-server-dispatcher"
							  :family 'ipv4
							  :server t
							  :service port
							  :log #'webchat-server-dispatch-topics
							  :filter #'webchat-server-dispatch-response))
  (mapcar #'webchat-server--build-server-process (mapcar #'cdr webchat-server-topic-port-alist)))

(defun webchat-server-dispatch-topics (server connection msg)
  (set-process-buffer connection (get-buffer-create (process-name connection)))
  (process-send-string connection (prin1-to-string (mapcar #'car webchat-server-topic-port-alist))))

(defun webchat-server-dispatch-response (process msg)
  (with-current-buffer (process-buffer process)
	(goto-char (point-max))
	(insert msg))
  (let ((request (read-from-process process)))
	(when request
	  (let* ((cmd (car request))
			 (data (cdr request))
			 (cmd-fn (intern (format "webchat-server-dispatch-%s" cmd))))
		(write-to-process process (apply cmd-fn data))))))

(defun webchat-server-dispatch-REQUEST-CHANNEL-PORT (channel)
  (cdr (assoc-string channel webchat-server-topic-port-alist)))

;; 以下操作是为了兼容#!emacs --script方式
(when (member "-scriptload" command-line-args)
  (let ((port (string-to-number (car command-line-args-left))))
  (webchat-server-dispatcher port)
  (message "webchat-server-dispatcher loaded")
  (while t
	(sit-for 1))
  (setq command-line-args-left nil)))


;; (webchat-server-dispatcher 8000)

(provide 'webchat-server-dispatcher)

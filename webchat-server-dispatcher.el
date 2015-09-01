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
							  :log #'webchat-server-dispatch-topics))
  (mapcar #'webchat-server--build-server-process (mapcar #'cdr webchat-server-topic-port-alist)))

(defun webchat-server-dispatch-topics (server connection msg)
  (process-send-string connection (prin1-to-string webchat-server-topic-port-alist))
  (delete-process connection))

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

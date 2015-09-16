(add-to-list 'load-path default-directory)
(require 'webchat-misc)
(defvar webchat-server-topic-port-alist '(("emacs"  8002 8003))
  "主题与端口的对应表")
(defvar webchat-server-dispatcher-process nil)

(defun webchat-server--build-service-process (port http-port)
  "创建webchat-server进程"
  (let ((emacs-bin-path (concat invocation-directory invocation-name)))
	(if http-port
		(start-process "webchat-server" "webchat-server" emacs-bin-path "--script" "webchat-server.el" (number-to-string port) (number-to-string http-port))
	  (start-process "webchat-server" "webchat-server" emacs-bin-path "--script" "webchat-server.el" (number-to-string port)))))


(defun webchat-server-dispatcher (port)
  (interactive `(,(read-number "请输入服务端口: " 8000)))
  (when webchat-server-dispatcher-process
	(delete-process webchat-server-dispatcher-process))
  (setq webchat-server-dispatcher-process
		(make-lispy-network-process :name "webchat-server-dispatcher"
							  :family 'ipv4
							  :server t
							  :service port
							  :log #'webchat-server-dispatch-topics
							  :filter #'webchat-server-dispatch-response))
  (mapcar (lambda (x)
			(apply  #'webchat-server--build-service-process x))
		  (mapcar #'cdr webchat-server-topic-port-alist)))

(defun webchat-server-dispatch-topics (server connection msg)
  (set-process-buffer connection (get-buffer-create (process-name connection)))
  (process-send-string connection (prin1-to-string (mapcar #'car webchat-server-topic-port-alist))))

(defun webchat-server-dispatch-response (process &rest request)
  (ignore-errors
	(when request
	  (message "DEBUG: get request[%s]" request)
	  (let* ((cmd (car request))
			 (data (cdr request))
			 (cmd-fn (intern (format "webchat-server-dispatch-%s" cmd))))
		(if (functionp cmd-fn)
			(lispy-process-send process (apply cmd-fn data))
		  (delete-process process))))))


(defun webchat-server-dispatch-REQUEST-CHANNEL-PORT (channel)
  (let ((port (cadr (assoc-string channel webchat-server-topic-port-alist)))
		(http-port (caddr (assoc-string channel webchat-server-topic-port-alist)))
		(max-service-port (apply #'max (mapcar #'caddr webchat-server-topic-port-alist)))) ;暂时默认http-port一定会比port大
	(unless port
	  (setq port (next-unused-port (+ 1 max-service-port)))
	  (setq http-port (next-unused-port (+ 1 port))))
	(webchat-server--build-service-process port http-port)
	(while (not (local-port-used-p port))
	  (sit-for 1))
	(add-to-list 'webchat-server-topic-port-alist (list channel port http-port) t)
	(list  port http-port)))

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

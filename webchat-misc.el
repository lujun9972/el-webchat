(package-initialize)
(require 'cl)
(defun read-from-process (process)
  "从process buffer中读取lisp object. 若成功则返回object,并将读取的内容从process buffer中移除,否则返回nil并保持process buffer内容不变"
  (let ((buf (process-buffer process))
		obj)
	(with-current-buffer buf
	  (goto-char (point-min))
	  (setq obj (ignore-errors  (read (current-buffer))))
	  (if obj
		  (delete-region (point-min) (point))
		(goto-char (point-min))))
	obj))

(defun read-from-process-wait (process)
  "从process buffer中读取lisp object. 若成功则返回object,并将读取的内容从process buffer中移除,否则阻塞一直到成功为止"
  (let (obj)
	(while (not (or (eq 'closed (process-status process))
					(setq obj (read-from-process process))))
	  (accept-process-output process 0.1))
	obj))

  (defun write-to-process (process &rest objs)
	(process-send-string process (prin1-to-string objs)))

(defun local-port-used-p (port)
  "探测本地指定端口是否已被占用"
  (let ((p1 (ignore-errors (make-network-process :name "connection test"
												 :service port
												 ;; :family 'ipv4
												 :host 'local))))
	(when p1
	  (delete-process p1)
	  t)))

(defun next-unused-port (base)
  "返回>=`base'的未占用端口"
  (while (local-port-used-p base)
	(setq base (+ 1 base)))
  base)

(defun lispy-process-default-filter (proc &rest objs)
  "lispy-process的默认filter"
  (when (buffer-live-p (process-buffer proc))
	(with-current-buffer (process-buffer proc)
	  (let ((moving (= (point) (process-mark proc))))
		(save-excursion
		  ;; Insert the text, advancing the process marker.
		  (goto-char (process-mark proc))
		  (insert (prin1-to-string objs))
		  (set-marker (process-mark proc) (point)))
		(if moving (goto-char (process-mark proc)))))))

(defun make-lispy-network-process (&rest args)
  "类似`make-network-process'但使用lisp object作为传输对象

filter function的函数签名应该为(process &rest objs)
该函数会使用process的'output property临时存放收到的字符串,可以通过参数:store-msg-property来设置存储在哪个property"
  (lexical-let* ((ori-filter-fn (or  (plist-get args :filter)
									 #'lispy-process-default-filter))
				 (store-msg-property (or  (plist-get args :store-msg-property)
										  'output)))
	(plist-put args :filter
			   (lambda (process msg)
				 (let ((content (process-get process store-msg-property))
					   result obj)
				   (setq content (concat content msg))
				   (while (setq result (ignore-errors (read-from-string content)))
					 (setq content (substring content (cdr result)))
					 (setq obj (car result))
					 (apply ori-filter-fn process obj))
				   (process-put process store-msg-property content))))
	(apply #'make-network-process args)))

(defun lispy-process-send (process &rest objs)
  "类似`process-send-string' 但发送的是lisp object"
  (process-send-string process (prin1-to-string objs)))

(defun lispy-process-send-wait (process wait-flag &rest objs)
  "类似`lispy-process-send' 但会等待回应
其中会设置process的'WAIT属性为`wait-flag. 并等待回应函数将'WAIT属性清为nil"
  (process-put process 'WAIT 'wait-flag)
  (process-send-string process (prin1-to-string objs))
  (while (process-get process 'WAIT)
	(accept-process-output process 0.05)))

(defun insert-function-button (label fn)
  "插入名为`label'的按钮,当按下时会出发fn函数"
  (insert-button label 'follow-link t 'face 'mode-line-inactive 'action fn))

(defun set-button-label (btn label)
  ""
  (replace-regexp ".+" label nil (button-start btn) (button-end btn)))
(provide 'webchat-misc)

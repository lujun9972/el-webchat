(package-initialize)
(require 'http-post-simple)
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

(defun url-upload-file (url file)
  "upload `file' to `url'. return the responese"
  (let ((content-type (if (string-match-p (org-image-file-name-regexp) file)
						  (format "image/%s" (file-name-extension file))
						(format "multipart/mixed"))) 
		(file-data (with-temp-buffer
					 (insert-file-contents-literally file)
					 (buffer-string))))
	(http-post-simple-multipart url nil `(("uploadfile" ,file ,content-type ,file-data)))))

(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
		(url-request-extra-headers
		 '(("Content-Type" . "application/x-www-form-urlencoded")))
		(url-request-data
		 (mapconcat (lambda (arg)
					  (concat (url-hexify-string (format "%s" (car arg)))
							  "="
							  (url-hexify-string (format "%s" (cdr arg)))))
					args
					"&")))
	(url-retrieve url (lambda (status)
						(kill-buffer (current-buffer))) nil t)))

(defun make-lispy-network-process (&rest args)
  "类似`make-network-process'但使用lisp object作为传输对象

filter function的函数签名应该为(process &rest objs)
该函数会使用process的'output property临时存放收到的字符串,可以通过参数:store-msg-property来设置存储在哪个property"
  (lexical-let* ((ori-filter-fn (plist-get args :filter))
				 (store-msg-property (or  (plist-get args :store-msg-property)
										  'output)))
	(when ori-filter-fn
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
	  (apply #'make-network-process args))))

(defun lispy-process-send (process &rest objs)
  "类似`process-send-string' 但发送的是lisp object"
  (process-send-string process (prin1-to-string objs)))

(defun set-lispy-process-filter (process filter &optional store-msg-property)
  "类似`set-filter-filter' 但是`filter'的函数参数应该为(process &rest objs)
该函数会使用process的'output property临时存放收到的字符串,可以通过参数store-msg-property来设置存储在哪个property"
  (lexical-let* ((ori-filter-fn filter)
				 (store-msg-property (or  store-msg-property
										  'output)))
	(set-process-filter process
						(lambda (process msg)
						  (let ((content (process-get process store-msg-property))
								result obj)
							(setq content (concat content msg))
							(while (setq result (ignore-errors (read-from-string content)))
							  (setq content (substring content (cdr result)))
							  (setq obj (car result))
							  (apply ori-filter-fn process obj))
							(process-put process store-msg-property content))))))
(defun insert-function-button (label fn)
  "插入名为`label'的按钮,当按下时会出发fn函数"
  (insert-button label 'follow-link t 'face 'mode-line-inactive 'action fn))

(defun set-button-label (btn label)
  ""
  (replace-regexp ".+" label nil (button-start btn) (button-end btn)))
(provide 'webchat-misc)

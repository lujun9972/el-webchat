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

(provide 'webchat-misc)

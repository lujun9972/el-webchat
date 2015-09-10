;; elnode fix
(defun elnode--http-mp-decode (buffer header-end-pt boundary)
  "Decode a multipart/form-data upload with BOUNDARY in BUFFER."
  (with-current-buffer buffer
    (goto-char (- header-end-pt 2)) ; moves back over the \r\n
    (loop while (eq (car next-boundary) :continue)
       with next-boundary = (elnode--http-mp-find-boundary boundary)
       collect
         (destructuring-bind (leader alist)
             (elnode--http-parse-header (current-buffer) (point) t)
           (let* ((cde
                   (mail-header-parse-content-disposition
                    (kva "Content-Disposition" alist)))
                  (name (aget (cdr cde) 'name))
                  (filename (aget (cdr cde) 'filename))
                  (pt (point)))
             ;; Find the next end point
             (setq next-boundary
                   (elnode--http-mp-find-boundary boundary))
             (let* ((lbp (line-beginning-position))
                    (content (buffer-substring pt (cadr next-boundary)))
                    (content-data
                     (cond
                       ((not filename) content)
                       (t (propertize content :elnode-filename filename)))))
               (cons name content-data)))))))

(provide 'elnode-fix)

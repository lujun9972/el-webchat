(require 'org)
(unless (and (boundp 'package--initialized)
			 package--initialized)
  (package-initialize))

(define-derived-mode webchat-mode org-mode "WebChat"
  "Major mode for running webchat"
  (make-local-variable 'scroll-step)
  (setq scroll-step 2)
  (org-indent-mode 1)
  (org-display-inline-images)
  (if (require 'htmlize nil t)
	  (progn 
		(make-local-variable 'org-src-fontify-natively)
		(setq org-src-fontify-natively t))
	(warn "无法加载htmlize,无法使用本地代码高亮")))

(defun webchat-build-window (content-buffer talk-buffer)
  "构建webchat窗口模式"
  (switch-to-buffer (get-buffer-create content-buffer))
  (webchat-mode)
  (read-only-mode)
  (select-window (split-window-below -4))
  (switch-to-buffer (get-buffer-create talk-buffer))
  (webchat-mode))

(defun url-get-content-from-url(url)
  "获取 `url'的内容"
  (require 'url)
  (let ((url-buffer (url-retrieve-synchronously url))
		url-content)
	(with-current-buffer url-buffer
	  (goto-char (point-min))
	  (search-forward-regexp "^$")
	  (setq url-content (buffer-substring-no-properties (+ (point) 1) (point-max))))
	(kill-buffer url-buffer)
	url-content))




(defun webchat-display-inline-images-callback (status start end type old width ori-buffer)
  (unwind-protect 
	  (let (file-data)
		(goto-char (point-min))
		(search-forward-regexp "^$")
		(setq file-data (buffer-substring-no-properties (+ (point) 1) (point-max)))
		(when file-data
		  (with-current-buffer ori-buffer
			(if (and (car-safe old) refresh)
				(image-refresh (overlay-get (cdr old) 'display))
			  (setq img (create-image file-data type t :width width))
			  (when img
				(setq ov (make-overlay start end))
				(overlay-put ov 'display img)
				(overlay-put ov 'face 'default)
				(overlay-put ov 'org-image-overlay t)
				(overlay-put ov 'modification-hooks
							 (list 'org-display-inline-remove-overlay))
				(push ov org-inline-image-overlays))))))
	(kill-buffer)))

(defun webchat-display-inline-images-async (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (if (fboundp 'clear-image-cache) (clear-image-cache)))
    (save-excursion
      (save-restriction
		(widen)
		(setq beg (or beg (point-min)) end (or end (point-max)))
		(goto-char beg)
		(let ((re (concat "\\[\\[\\(\\(file:\\|http:\\|https:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
						  (substring (org-image-file-name-regexp) 0 -2)
						  "\\)\\]" (if include-linked "" "\\]")))
			  (case-fold-search t)
			  old file ov img type attrwidth width)
		  (while (re-search-forward re end t)
			(setq old (get-char-property-and-overlay (match-beginning 1)
													 'org-image-overlay)
				  file (substring-no-properties (match-string 0) 2 -2))
			(when (image-type-available-p 'imagemagick)
			  (setq attrwidth (if (or (listp org-image-actual-width)
									  (null org-image-actual-width))
								  (save-excursion
									(save-match-data
									  (when (re-search-backward
											 "#\\+attr.*:width[ \t]+\\([^ ]+\\)"
											 (save-excursion
											   (re-search-backward "^[ \t]*$\\|\\`" nil t)) t)
										(string-to-number (match-string 1))))))
					width (cond ((eq org-image-actual-width t) nil)
								((null org-image-actual-width) attrwidth)
								((numberp org-image-actual-width)
								 org-image-actual-width)
								((listp org-image-actual-width)
								 (or attrwidth (car org-image-actual-width))))
					type (if width 'imagemagick)))
			(require 'url)
			(url-retrieve file #'webchat-display-inline-images-callback `(,(match-beginning 0) ,(match-end 0) ,type ,old ,width ,(current-buffer)))))))))

(defun webchat-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (if (fboundp 'clear-image-cache) (clear-image-cache)))
    (save-excursion
      (save-restriction
		(widen)
		(setq beg (or beg (point-min)) end (or end (point-max)))
		(goto-char beg)
		(let ((re (concat "\\[\\[\\(\\(file:\\|http:\\|https:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
						  (substring (org-image-file-name-regexp) 0 -2)
						  "\\)\\]" (if include-linked "" "\\]")))
			  (case-fold-search t)
			  old file file-data ov img type attrwidth width)
		  (while (re-search-forward re end t)
			(setq old (get-char-property-and-overlay (match-beginning 1)
													 'org-image-overlay)
				  file (substring-no-properties (match-string 0) 2 -2))
			(when (image-type-available-p 'imagemagick)
			  (setq attrwidth (if (or (listp org-image-actual-width)
									  (null org-image-actual-width))
								  (save-excursion
									(save-match-data
									  (when (re-search-backward
											 "#\\+attr.*:width[ \t]+\\([^ ]+\\)"
											 (save-excursion
											   (re-search-backward "^[ \t]*$\\|\\`" nil t)) t)
										(string-to-number (match-string 1))))))
					width (cond ((eq org-image-actual-width t) nil)
								((null org-image-actual-width) attrwidth)
								((numberp org-image-actual-width)
								 org-image-actual-width)
								((listp org-image-actual-width)
								 (or attrwidth (car org-image-actual-width))))
					type (if width 'imagemagick)
					file-data (save-match-data (url-get-content-from-url file))))
			(when file-data
			  (if (and (car-safe old) refresh)
				  (image-refresh (overlay-get (cdr old) 'display))
				(setq img (save-match-data (create-image file-data type t :width width)))
				(when img
				  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
				  (overlay-put ov 'display img)
				  (overlay-put ov 'face 'default)
				  (overlay-put ov 'org-image-overlay t)
				  (overlay-put ov 'modification-hooks
							   (list 'org-display-inline-remove-overlay))
				  (push ov org-inline-image-overlays))))))))))

(defun webchat-show-url-img (start end)
  (interactive "r")
  (let* ((url (string-trim (buffer-substring-no-properties start end)))
		 (file (url-get-content-from-url url))
		 (img (create-image file 'imagemagick t))
		 ov)
	(when img
	  (setq ov (make-overlay start end))
	  (overlay-put ov 'display img)
	  (overlay-put ov 'face 'default))))


(provide 'webchat-mode)

(define-derived-mode webchat-mode org-mode "WebChat"
  "Major mode for running webchat"
  (make-local-variable 'scroll-step)
  (setq scroll-step 2))

(provide 'webchat-mode)

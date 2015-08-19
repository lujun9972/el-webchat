(define-derived-mode webchat-mode text-mode "WebChat"
  "Major mode for running webchat"
  (make-local-variable 'scroll-step)
  (setq scroll-step 2))

(provide 'webchat-mode)

(defun fringes-alert-modified (&rest args)
  (setq res (null (string-match-p "^\\*.*\\*$" (buffer-name (current-buffer)))))
  (if res (message "t %s" (buffer-name (current-buffer))) (message "nil %s" (buffer-name (current-buffer)))))
  ;; (if 
  ;;     (set-face-background 'fringe "red")
  ;;     ;;(progn (set-face-background 'fringe "red") (message "?"))
  ;;   (message "NO")))
    ;; (if (not (buffer-modified-p))
    ;;     (set-face-background 'fringe "black"))
  ;;   )
  ;; )
(defun fringes-alert-saved (&rest args)
  (set-face-background 'fringe "black"))

(add-hook 'after-change-functions #'fringes-alert-modified)
;; (setq-default after-change-functions nil)
(add-hook 'after-save-hook #'fringes-alert-saved)

(provide 'fringes-alert)

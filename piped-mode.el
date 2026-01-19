(define-derived-mode piped-mode fundamental-mode "piped"
  :after-hook
  (save-excursion
    (end-of-buffer)
    (re-search-backward "^\\[[^\n]*\\(?:\n[^\n]+\\)*" nil t 2)
    (kill-new (match-string-no-properties 0)))
  (message "Last piped command copied"))

(provide 'piped-mode)

;; desperation
(defun ido-preview-cond()
  "Function using lot of dynamic variables inside.
Return string.
The general rule: (car ido-matches) - item we are watching here."
  (cond
    ((and file
       (file-exists-p file)
       (not (file-directory-p file))
       ido-current-directory)
      (save-excursion
        (save-window-excursion
          (when (find-file file)
            (prog1 (buffer-substring (point-min) (point-max)) (kill-buffer))))))
    ((consp (car ido-matches))
      (cond
        ((and (cdar ido-matches) (stringp (cadar ido-matches))) ; interaction with kill-ring-ido.el
          (cadar ido-matches))
        (t (ido-name ido-matches))))
    ((and (stringp (car ido-matches)) (fboundp (intern (car ido-matches)))) ; functionp → fboundp
     (noflet ((message(&rest args))) (save-window-excursion (describe-function (intern (car ido-matches))))))
    ;; for describe-variable:
    ((and (stringp (car ido-matches)) (boundp (intern (car ido-matches))))
      (noflet ((message(&rest args))) (save-window-excursion (describe-variable (intern (car ido-matches))))))
    ((and (stringp (car ido-matches)) (bufferp (get-buffer (car ido-matches))))
      (save-excursion (set-buffer (get-buffer (car ido-matches))) (buffer-substring (point-min) (point-max))))
    ((stringp (car ido-matches)) (car ido-matches))
    (t "No matches, sir.")))

(defun ido-preview-forward(&optional arg)
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive "P")
  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (unless (or (equal 'ido-preview-forward last-command) (equal 'ido-preview-backward last-command))
    (setq minibuffer-scroll-window nil))

  (let ((window minibuffer-scroll-window))
    ;; If there's a fresh completion window with a live buffer,
    ;; and this command is repeated, scroll that window.
    (if (window-live-p window)
      (with-current-buffer (window-buffer window)
        (scroll-other-window arg)
        nil)
      (progn
        (let ((enable-recursive-minibuffers t)
               (file (ido-name (car ido-matches))))
          (if file
            (setq file (concat ido-current-directory file)))
          (get-buffer-create " *preview-ido*")
          ;; using the fact we are in (save-window-excursion) while browsing files.
          (other-window 1) ; currently we are at minibuffer-window, so jump at real buffer

	  ;; plus: fix for ido-preview when ilist-menu is open
	  (if (string-equal (buffer-name) "*Ilist*")
	      (windmove-right))

          (delete-other-windows) ; AHAHAHA~!
          (setq minibuffer-scroll-window (get-buffer-window (switch-to-buffer " *preview-ido*"))) ;set current window the thing we preview, and setting minibuffer-scroll-window to scroll.
          (other-window -1)
          (with-current-buffer " *preview-ido*"
            (text-mode)
            (erase-buffer)
            (insert (ido-preview-cond))
            (ignore-errors (set-auto-mode))))))))

(defun ido-preview-backward(arg)
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive "P")

  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (unless (or (equal 'ido-preview-forward last-command) (equal 'ido-preview-backward last-command))
    (setq minibuffer-scroll-window nil))

  (let ((window minibuffer-scroll-window))
    ;; If there's a fresh completion window with a live buffer,
    ;; and this command is repeated, scroll that window.
    (if (window-live-p window)
      (with-current-buffer (window-buffer window)
        (scroll-other-window
          ;; reversing argument
          (cond
            ((not arg) '-)
            (t (- arg))))
        nil)
      (progn
        (let ((enable-recursive-minibuffers t)
               (file (ido-name (car ido-matches))))
          (if file
            (setq file (concat ido-current-directory file)))
          (get-buffer-create " *preview-ido*")
          ;; using the fact we are in (save-window-excursion) while browsing files.
          (other-window 1) ; currently we are at minibuffer-window, so jump at real buffer
          (delete-other-windows) ; AHAHAHA~!
          (setq minibuffer-scroll-window (get-buffer-window (switch-to-buffer " *preview-ido*"))) ;set current window the thing we preview, and setting minibuffer-scroll-window to scroll.
          (other-window -1)
          (with-current-buffer " *preview-ido*"
            (text-mode)
            (erase-buffer)
            (insert (ido-preview-cond))
            (ignore-errors (set-auto-mode))))))))

(provide 'ido-preview)

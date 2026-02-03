;;; jekyll-gen.el
;; Generate jekyll post
;; 2026-02-03 09:15
;;
;; Dependencies:
;; - requires jekyll-posts-dir to be defined

(setq jekyll-template "---
layout: post
title:
date: {date}
modified_date: {date}
categories:
lang:
---

##

{% include fin.html %}
")

(defun jekyll-date ()
  (format-time-string "%F"))

(defun jekyll-datetime ()
  (format-time-string "%F %H:%M"))

(defun jekyll-populated-template ()
  (string-replace "{date}" (jekyll-datetime) jekyll-template))

(defun create-jekyll-post ()
  ;; requires jekyll-posts-dir to be defined
  (interactive)
  (let ((date (jekyll-date))
        (name (subst-char-in-string
               ? ?-                     ; spaces to hyphen
               (read-string "Jekyll post filename (without date): "))))
    (find-file (expand-file-name (concat date "-" name ".md") jekyll-posts-dir))
    (insert (jekyll-populated-template))
    (search-backward "title:")
    (end-of-line)
  ))

(provide 'jekyll-gen)

;;; jekyll-gen ends here

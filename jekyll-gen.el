;;; jekyll-gen.el
;; Generate jekyll post
;; 2026-02-03 09:15
;;
;; Dependencies:
;; - emacs >28.1 (because of file-name-concat)
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
  (let ((name (subst-char-in-string
               ? ?-                     ; spaces to hyphen
               (string-replace
                "{date}" (jekyll-date)
                (read-string "Jekyll post name: " "{date} "))))
        (location (read-string "Location: " "_posts")))
    (find-file (file-name-concat jekyll-posts-dir ".." location (concat name ".md")))
    (insert (jekyll-populated-template))
    (search-backward "title:")
    (end-of-line)
  ))

(provide 'jekyll-gen)

;;; jekyll-gen ends here

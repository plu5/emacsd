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

(setq devlog-template "---
layout: post
title: {num} — 
date: {date}
modified_date: {date}
categories:
lang: en
redirect_from: /devlog/{num}
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

(defun last-file-lexicographically (path &optional match nameonly)
  (let ((full (if nameonly nil 'full)))
    (car (sort (directory-files path full match t) #'string-greaterp))))

(defun devlog-n ()
  "Return next devlog number"
  ;; requires jekyll-posts-dir to be defined and for there to be _devlog in it
  ;; and for all the names of devlogs to start with a number followed by hypen
  (1+
   (string-to-number
    (car (split-string
          (last-file-lexicographically
           (file-name-concat jekyll-posts-dir "../_devlog") ".md" t) "-")))))

(defun devlog-populated-template (num)
  (string-replace
   "{date}" (jekyll-datetime)
   (string-replace "{num}" num devlog-template)))

(defun create-devlog ()
  ;; requires jekyll-posts-dir to be defined
  (interactive)
  (let ((num (number-to-string (devlog-n))))
    (find-file (file-name-concat jekyll-posts-dir "../_devlog" (concat num ".md")))
    (insert (devlog-populated-template num))
    (search-backward "title:")
    (end-of-line)))

(provide 'jekyll-gen)

;;; jekyll-gen ends here

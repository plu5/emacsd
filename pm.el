;;; stripped-down configuration @ 2024-12-08 plu5

;;; HOW TO USE THIS FILE:
;; add to the top of init.el:
;; (add-to-list 'load-path "path/to/folder")
;; (require 'pm)

;;; HOW TO INSTALL PACKAGES:
;; M-x package-list-packages / M-x package-refresh-contents
;; then they'll be listed in M-x package-install
;;; HOW TO RELOAD PATH:
;; quit ahk and restart. not sufficient to reload script.
;; M-x getenv PATH to verify.
;;; NEED IN PATH:
;; * mingw rgrep find.exe *ahead* of windows find.exe
;; * python installation directory *ahead* of WindowsApps. may need to make a copy of your python.exe with the name python3.exe. for proper linting also pip install flake8.

;;; LIST OF ALL PACKAGES INSTALLED FROM MELPA/ELPA/GNU:
;; [important:] use-package magit ido-grid-mode smex ido-completing-read+ move-text imenu-list ws-butler undo-tree
;; [optional:] flycheck yasnippet multiple-cursors anzu avy expand-region diminish ahk-mode helm helm-swoop
;; [dependencies:] noflet (dependency for ido-preview)

;;; LIST OF LOCAL PACKAGES IN LOAD PATH:
;; [optional:] ido-preview grab-and-drag org-simple-expiry doentry-gen doentry-mode piped-mode jekyll-gen

;;; LIST OF LOCAL FILES IN LOAD PATH (NOT INCLUDING LOCAL PACKAGES LISTED ABOVE):
;; pm.el (you are here), fsffdark-theme.el (in p-custom-theme-load-path), setup-hippie.el

;;; FONTS:
(setq p-frame-font "Source Code Pro 14")
(setq p-imenu-list-font "Inconsolata-10")

;;; LOCAL PATHS:
(setq p-custom-theme-load-path "w:/C/home/.emacs.d/_/reps/emacs-fsff-theme")
(setq p-pnotes-path (if (eq system-type 'windows-nt)
                        "W:/B/Dropbox/Apps" "/media/pnotes"))
(setq p-reps-dir "/media/Windows/Users/pm/dev/reps")

(setq p-verdicts-path (expand-file-name "verdicts.txt" p-pnotes-path))
(setq p-yas-snippet-dir (expand-file-name "emacs/yasnippet-snippets" p-pnotes-path))
(setq p-undo-tree-history "~/.emacs.d/undo")
(setq jekyll-posts-dir (expand-file-name "plu5.github.io/_posts" p-reps-dir))

;;; UNIQUE BUFFERS/FILES:
(setq p-main-file (expand-file-name "PlainText 2/pers/gtd/2026.org" p-pnotes-path))
(setq p-inbox-file (expand-file-name "PlainText 2/pers/gtd/inbox.org" p-pnotes-path))
(setq p-inbox2-file (expand-file-name "PlainText 2/pers/gtd/getthisshitoutofmysight.org" p-pnotes-path))
(setq p-fr-file (expand-file-name "PlainText 2/s/l/fr-phrases.org" p-pnotes-path))
(setq p-logs-dir (expand-file-name "Day One/Journal.dayone/entries/" p-pnotes-path))
(setq p-logs-regexp "*.doentry")

;;; EXTREMELY IMPORTANT DEFAULTS
(defun ---EXTREMELY-IMPORTANT-DEFAULTS () ()) ; (this is just here for imenu)
;; please for the love of god don't wipe my creation dates
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq backup-by-copying t)
(setq backup-by-copying-when-linked t)
;;; LESS IMPORTANT DEFAULTS
(setq auto-save-default nil) ; unsure about this but annoyed by junk files
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1) ; scrollbars don't work well
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq column-number-mode t)
(global-visual-line-mode t) ; word wrap everywhere
(setq visual-line-fringe-indicators t) ; word wrap indicators
(setq shift-select-mode nil)
(delete-selection-mode 1)
(windmove-default-keybindings) ; move around emacs windows with shift + arrows
(global-set-key [mouse-2] nil) ; avoid yank on middle mouse
(global-set-key [mouse-3] nil) ; avoid mouse-save-then-kill on right mouse
(setq scroll-conservatively 10000) ; scroll one line at a time instead of a large portion of the screen you go down/up with text cursor
;;; utf-8 (avoids being asked what coding system to use when you save files)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;; FONT
(set-frame-font p-frame-font nil t)

;;; THEMES
(defun ---THEMES () ())
(add-to-list 'custom-theme-load-path p-custom-theme-load-path)
(load-theme 'fsffdark t)

;;; SOME KEYBINDINGS
(defun ---SOME-KEYBINDINGS () ())
(global-unset-key (kbd "C-z")) ; i use it as prefix for a bunch of shit
(global-unset-key (kbd "<pinch>")) ; prevent accidental pinching when scrolling on thinkpad
(global-set-key (kbd "C-M-:") 'eval-region)
(global-set-key (kbd "C-z :") 'eval-buffer)
(global-set-key (kbd "C-z r") 'revert-buffer)
(global-set-key [next] 'next-buffer)
(global-set-key [prior] 'previous-buffer)
(global-set-key (kbd "C-z RET") (lambda () (interactive) (start-process "" "z" "urxvt")))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-=") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "<XF86Paste>") 'yank)

;;; WINDOW GEOMETRY
(defun ---WINDOW-GEOMETRY () ())
(setq initial-frame-alist '((top . 0) (left . -1913) (width . 95) (height . 42))) ; with minimap the width was 112 ;; alternative height: 35
(defun p-frame-resize-r ()
  "snap to right but still enough room for 80-char lines https://pragmaticemacs.wordpress.com/2015/10/26/resize-your-emacs-frame-with-keyboard-shortcuts/ had to hardcode in my case bc couldn't get it working otherwise. e.g. frame pixel size results in too high even when resizing pixelwise. with minimap the numbers are 1160 1028"
  (interactive)
  (set-frame-size (selected-frame) 1045 1028 t)
  (set-frame-position (selected-frame) -1913 0))
(global-set-key (kbd "C-z <right>") 'p-frame-resize-r)
(defun p-enlarge-window-max ()
  (interactive)
  (enlarge-window 99))
(global-set-key (kbd "C-z ^") 'p-enlarge-window-max)

;;; EDITING-DEFUNS
(defun ---EDITING-DEFUNS () ())
;;; copying (magnars)
;; copy region if active
;; otherwise copy to end of current line
;;   * with prefix, copy N whole lines
(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))
(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))
(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))
(global-set-key (kbd "M-w") 'save-region-or-current-line)
;;; newlines (magnars)
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
;;; + unicode C-S-u insertion hack necessary for linux and plover
;; by Post Self https://emacs.stackexchange.com/a/80061/15886
(setq lexical-binding t)
(defun my/insert-unicode-char ()
  (interactive)
  (let* ((digits (named-let gather-hex-digits ((digits nil))
                   (let ((digit (read-char)))
                     (if (not (or (<= ?0 digit ?9)
                                  (<= ?a digit ?f)
                                  (<= ?A digit ?F)))
                         digits
                       (gather-hex-digits (cons digit digits))))))
         (code (string-to-number
                (apply 'string (reverse digits))
                16)))
    (insert-char code)))
(define-key global-map (kbd "C-S-u") 'my/insert-unicode-char)
;;; + rectangle yank push lines
;; by jue https://emacs.stackexchange.com/a/46352/15886
(defun my-insert-rectangle-push-lines ()
  "Yank a rectangle as if it was an ordinary kill."
  (interactive "*")
  (when (and (use-region-p) (delete-selection-mode))
    (delete-region (region-beginning) (region-end)))
  (save-restriction
    (narrow-to-region (point) (mark))
    (yank-rectangle)))
(global-set-key (kbd "C-x r C-y") #'my-insert-rectangle-push-lines)
;;; + verdicts
;; inserts verdicts into buffer and adds &gt; to the beginning of each para
(defun p-verdicts ()
  (interactive)
  (insert-file-contents p-verdicts-path)
  (beginning-of-buffer)
  (insert "&gt; ")
  (while (= (forward-paragraph) 0)
    (forward-line)
    (if (< (point) (point-max)) (insert "&gt; "))))
(defun p-verdicts-clipboard ()
  (interactive)
  (yank)
  (beginning-of-buffer)
  (insert "&gt; ")
  (while (= (forward-paragraph) 0)
    (forward-line)
    (if (< (point) (point-max)) (insert "&gt; "))))
(global-set-key (kbd "C-z t v") #'p-verdicts-clipboard)
;;; + p/clean-ocr-yanked-thing
(defun p/clean-ocr-yanked-thing ($string &optional $from $to)
  "Made by modifying xah’s remove-vowel:
http://ergoemacs.org/emacs/elisp_command_working_on_string_or_region.html
replaces common OCR issues and also replaces breaklines with spaces"
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))
  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if $string t nil))
    (setq inputStr (if workOnStringP $string (buffer-substring-no-properties $from $to)))
    (setq outputStr
          (replace-regexp-in-string "\\bTh " "Th" inputStr))
    (setq outputStr
          (replace-regexp-in-string "ﬁ " "fi" outputStr))
    (setq outputStr
          (replace-regexp-in-string "fi " "fi" outputStr))
    (setq outputStr
          (replace-regexp-in-string "fl " "fl" outputStr))
    (setq outputStr
          (replace-regexp-in-string "ﬁ" "fi" outputStr))
    (setq outputStr
          (replace-regexp-in-string "ﬀ " "ff" outputStr))
    (setq outputStr
          (replace-regexp-in-string "ﬂ " "fl" outputStr))
    (setq outputStr
          (replace-regexp-in-string "ﬃ " "ffi" outputStr))
    (setq outputStr
          (replace-regexp-in-string "ﬄ " "ffl" outputStr))
    (setq outputStr
          (replace-regexp-in-string "-
" "" outputStr))
    (setq outputStr
          (replace-regexp-in-string "
" " " outputStr))
    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region $from $to)
        (goto-char $from)
        (insert outputStr)))))
(global-set-key (kbd "C-z c") 'p/clean-ocr-yanked-thing)
;; + p-xml-escape-region
(defun p-xml-escape-region ($string &optional $from $to)
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))
  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if $string t nil))
    (setq inputStr (if workOnStringP $string (buffer-substring-no-properties $from $to)))
    (setq outputStr (xml-escape-string inputStr))
    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region $from $to)
        (goto-char $from)
        (insert outputStr)))))
(global-set-key (kbd "C-z x") 'p-xml-escape-region)
;; + p-hhmm-date
(defun p-hhmm-date () (interactive)
       (insert (format-time-string "[%H:%M] ")))
(global-set-key (kbd "H-4") 'p-hhmm-date)
(defun p-hhmm-date-z () (interactive)
       (insert (format-time-string "%H:%M ")))
(global-set-key (kbd "C-z C-z") 'p-hhmm-date-z)
(defun p-full-date () (interactive)
       (insert (format-time-string "%Y-%m-%d %H:%M")))
(global-set-key (kbd "H-5") 'p-full-date)
;; + sommeil
;; (defun p-sommeil ()
;;   (interactive)
;;   (insert-file-contents "~/sommeil.log"))
;; (global-set-key (kbd "H-]") 'p-sommeil)
;; + dates pour yasnippet gtd
(setq p-taches-predefinies
      '(("11-23" . "[ ] 00:00 souhaite à ls bon anniversaire")
        ("05-10" . "[ ] 00:00 souhaite à os bon anniversaire")
        ("04-01" . "[ ] 00:00 souhaite à f bon anniversaire")
        ("02-20" . "[ ] @tat awav")
        ("jeudi" . "[ ] @red bmqs")
        ("jeudi" . "[ ] @tat epic")
        ))
(defvar jours-de-la-semaine
  '(("dimanche" . 0) ("lundi" . 1) ("mardi" . 2) ("mercredi" . 3) ("jeudi" . 4)
    ("vendredi" . 5) ("samedi" . 6)))
(defun jour-de-la-semaine-dune-date (date)
  "Convertit une date de format YYYY-MM-DD au jour de la semaine correpondant"
  (let ((n 
            (nth 6 (decode-time (org-time-string-to-time date)))))
    (dolist (j jours-de-la-semaine)
      (when (= n (cdr j))
        (return (car j))))))
(defun p-str-taches-predefinies-pour-date (date)
  "Renvoie string liste des tâche(s) prédéfinies pour DATE (format MM-DD)."
  (let ((res-liste nil))
    (message date)
    (dolist (item p-taches-predefinies)
      (when (or (string= date (car item))
                (and (string-match-p "[d-v]" (car item))
                     (string= (jour-de-la-semaine-dune-date
                          (concat (format-time-string "%Y") "-" date))
                          (car item))))
        (push (cdr item) res-liste)))
    (if res-liste
        (concat "\n    + " (mapconcat 'identity res-liste "\n    + "))
      "")))
;; github.com/alexott/emacs-configs/blob/91fb0a1/rc/emacs-rc-muse.el#L306
(defun re-replace-text (from to)
  (save-excursion
    ;; had to add this to make it work regardless of point position
    (beginning-of-buffer)
    (while (re-search-forward from nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match to t)))))
;; mise à jour du champ date de modification
(defun update-modified-date-field (from-disk)
  (interactive
   (list (y-or-n-p "Date from disk? (n will put current date) ")))
  (let ((date (if from-disk (copy-file-last-date-to-clipboard nil t)
                (format-time-string "%F %H:%M"))))
    (re-replace-text "^modified_date: .*$" (concat "modified_date: " date))))
(defvar-local always-update-modified-date-field t
  "Don't ask and always update modified date field if it exists
(maybe-update-modified-date-field)")
(defun maybe-update-modified-date-field ()
  (interactive)
  ;; yibe https://stackoverflow.com/a/6886731/18396947
  (when (and (stringp buffer-file-name)
             (string-match "\\.md\\'" buffer-file-name))
    (save-excursion
      (beginning-of-buffer)
      (when (and (re-search-forward "^modified_date:")
                 (or always-update-modified-date-field
                     (y-or-n-p "Update modified_date? ")))
        (update-modified-date-field nil)))))
(add-hook 'before-save-hook #'maybe-update-modified-date-field)

;;; GIT DEFUNS
;; site md files (notes / articles)
;; this puts literal '' in the commit message which was an accident
;; but I decided to leave it like that because it communicates that it
;; was an automatic commit
(defun magit-commit-texts ()
  (interactive)
  (magit-with-toplevel
    (let ((date (format-time-string "%F %H:%M"))
          (magit-git-global-arguments
           (remove "--literal-pathspecs" magit-git-global-arguments)))
      (magit-run-git "add" "*.md")
      (magit-run-git "commit" (concat "--message='Update texts " date "'")
                     "--"))))

;;; FILE DEFUNS (not bound to anything atm, but useful functions to be able to run with M-x)
;; magnars
(defun ---FILE-DEFUNS () ())
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
;;
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
	(message "File '%s' successfully removed" filename)))))
;;
;; by ThatThinkingFeeling https://www.reddit.com/r/emacs/comments/cwnd57/-/eyeazix/
(defun copy-path-to-clipboard (&optional bufname noclipboard)
  "Copy the current buffer file name (path) to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (if (null bufname) (buffer-file-name)
                      (buffer-file-name (get-buffer bufname))))))
    (when filename
      (if noclipboard
          filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename)))))
;; ref Tyler https://emacs.stackexchange.com/a/22674/15886
(defun copy-file-last-date-to-clipboard (&optional bufname noclipboard)
  (interactive)
  (let* ((path (if (null bufname) (buffer-file-name)
                 (buffer-file-name (get-buffer bufname))))
         (data (file-attributes path))
         (modified (format-time-string "%Y-%m-%d %H:%M" (nth 5 data))))
    (when modified
      (if noclipboard
          modified
        (kill-new modified)
        (message "Copied file modification date '%s' to the clipboard. (file '%s')" modified path)))))
;; get file creation date, which i use in yasnippets
(defun file-creation-date (&optional bufname)
  (interactive)
  (let* ((path (if (null bufname) (buffer-file-name)
                 (buffer-file-name (get-buffer bufname))))
         (output-buffer "*date*")
         ;; to get the whole date use "Birth: \\(.*\\)" instead
         ;; this one gets just Y-m-d H:M
         (date-regexp
          "Birth: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)"))
    (call-process "stat" nil output-buffer nil path)
    (save-excursion
      (with-current-buffer output-buffer
        (end-of-buffer)
        (re-search-backward date-regexp nil t 2)
        (setq output (match-string-no-properties 1))))
    (kill-buffer output-buffer)
    (message output)))
;;
(defun p-switch-to-main-file ()
  (interactive)
  (find-file p-main-file))
(global-set-key (kbd "C-z b a") #'p-switch-to-main-file)
(defun p-switch-to-inbox-file ()
  (interactive)
  (find-file p-inbox-file))
(global-set-key (kbd "C-z b i") #'p-switch-to-inbox-file)
(defun p-switch-to-inbox2-file ()
  (interactive)
  (find-file p-inbox2-file))
(global-set-key (kbd "C-z b o") #'p-switch-to-inbox2-file)
(defun p-switch-to-fr-file ()
  (interactive)
  (find-file p-fr-file))
(global-set-key (kbd "C-z b f") #'p-switch-to-fr-file)
(defun p-cpp-header-toggle ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (string-match ".cpp" filename)
        (find-file (replace-regexp-in-string ".cpp$" ".h" filename))
      (if (string-match ".h" filename)
          (find-file (replace-regexp-in-string ".h$" ".cpp" filename))))))
(global-set-key (kbd "C-z b h") #'p-cpp-header-toggle)
(defun p-switch-to-messages-buf ()
  (interactive)
  (switch-to-buffer "*Messages*"))
(global-set-key (kbd "C-z b m") #'p-switch-to-messages-buf)
(defun p-switch-to-scratch-buf ()
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "C-z b s") #'p-switch-to-scratch-buf)
(defun p-switch-to-piped-buf ()
  (interactive)
  (switch-to-buffer "*piped*"))
(global-set-key (kbd "C-z b p") #'p-switch-to-piped-buf)
(defun p-rgrep-logs ()
  (interactive)
  (rgrep (read-regexp "Logs rgrep regex: ") p-logs-regexp p-logs-dir))
(global-set-key (kbd "C-z s") #'p-rgrep-logs)

;;; PROGRAMMING
(defun ---PROGRAMMING () ())
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)
(setq css-indent-offset 2)
(setq-default c-basic-offset 4)
(setq-default show-trailing-whitespace t)

;;; BASIC IDO (more stuff below in ido-related packages)
(defun ---BASIC-IDO () ())
(setq ido-use-virtual-buffers t) ; buffers from recentf
(setq recentf-max-saved-items nil) ; no recentf limit
(setq ido-default-buffer-method 'selected-window) ; always allow me to switch to a buffer, even if it's already open in another frame
(ido-mode 1)
(ido-everywhere 1)

;;; WINDOWS
(defun ---WINDOWS () ())
(defun open-in-explorer ()
  (interactive)
  (cond
   ;; In buffers with file name
   ((buffer-file-name)
    (shell-command (concat "start explorer /e,/select,\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
   ;; In dired mode
   ((eq major-mode 'dired-mode)
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
   ;; In eshell mode
   ((eq major-mode 'eshell-mode)
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
   ;; Use default-directory as last resource
   (t
    (shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))))
(global-set-key [M-f2] 'open-in-explorer)

;;; PACKAGE
(defun ---PACKAGE () ())
(setq use-package-enable-imenu-support t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;;; PYTHON
(defun ---PYTHON () ())
(use-package python
  :bind
  (
   :map python-mode-map
   ("C-c C-c" . (lambda () (interactive) (start-process-shell-command "" "z" (concat "urxvt -e $SHELL --rcfile <(echo \"bspc node -f last; while inotifywait -e close_write .; do python " (buffer-file-name) "; done\")"))))
   ))
;;; ORG MODE
(defun ---ORG-MODE () ())
(use-package org
  :bind
  (("C-z t t" . (lambda () (interactive) (org-time-stamp '(4))))
   :map org-mode-map
   ("C-c o" . org-goto)
   ("C-c n" . org-next-item)
   ("C-c p" . org-previous-item)
   ;; ("<C-return>" . open-line-below) ; 2024-12-10 17:48 but it seems like just normal enter does the same sort of thing nowadays
   ("<return>" . org-return-and-maybe-indent) ; most of the time doesn't indent, which is what i want
   ("<C-return>" . org-return))
  :config
  (setq org-cycle-include-plain-lists 'integrate)
  (setq org-fontify-whole-heading-line t)
  (setq org-list-indent-offset 2) ; total indent is 4, this seems to just +2 to existing 2. i have this in order to make it consistent with my existing notes, as seems maybe later versions of org-mode changed the indentations of subitems to be just 2 instead of 4. i have this line in my old config too though.
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (setq org-hide-emphasis-markers t) ; e.g. _test_ underlines and hides the _
  (setq org-startup-folded t)
  (require 'org-mouse) ; ability to click on headings (builtin)
  (setq org-mouse-features '(context-menu activate-stars activate-bullets))
  ;; org-goto configuration (Kyle Meyer https://emacs.stackexchange.com/a/20762/15886)
  (setq org-goto-interface 'outline-path-completionp)
  (setq org-outline-path-complete-in-steps nil)
  ;; + org-simple-expiry (creation dates for headings)
  (use-package org-simple-expiry
    :ensure nil
    :bind
    (:map org-mode-map
	  ("C-c c" . org-expiry-insert-created))
    :config
    (setq org-expiry-created-property-name "CREATED")
    (setq org-expiry-inactive-timestamps nil)
    )
  )

;;; ONLINE INSTALLED PACKAGES
;; + ido-grid-mode
(use-package ido-grid-mode
  :config
  (ido-grid-mode 1)
  (setq ido-grid-mode-prefix-scrolls t))
;; + smex
(use-package smex :bind (("M-x" . smex)))
;; + ido-completing-read+
(use-package ido-completing-read+ :config (ido-ubiquitous-mode 1))
;; + move-text (move lines with M-<up>, M-<down>)
(use-package move-text :bind
  (("C-S-<up>" . move-text-up)
   ("C-S-<down>" . move-text-down)
   :map org-mode-map
   (("C-S-<up>" . move-text-up)
    ("C-S-<down>" . move-text-down)))
  )
  ;; :config (move-text-default-bindings)) ; the default bindings are M-<up> and M-<down> which interferes with org-mode bindings. and also in notepad++ it is C-S up and down, so best to stay consistent with it
;; + imenu-list
(use-package imenu-list
  :bind
  ("C-z t i" . p/imenu-list-toggle)
  ("C-<tab>" . p/imenu-list-toggle)
  :config
  (make-face 'speedbar-face)
  (set-face-font 'speedbar-face p-imenu-list-font)
  ;;
  (defun imenu-list-major-mode-hook-function ()
    (visual-line-mode -1)
    (setq truncate-lines t)
    (buffer-face-set 'speedbar-face))
  (add-hook 'imenu-list-major-mode-hook #'imenu-list-major-mode-hook-function)
  ;;
  (setq imenu-list-size 15)
  (setq imenu-list-position 'left)
  (setq imenu-list-mode-line-format nil)
  ;; the toggle function
  (defun p/imenu-list-toggle ()
    "Toggle imenu-list."
    (interactive)
    (if (get-buffer "*Ilist*")
	(if (equal (buffer-name (window-buffer (frame-first-window))) "*Ilist*")
	    (delete-window (frame-first-window))
	  (imenu-list-show-noselect)
	  (with-current-buffer (imenu-list-get-buffer-create)
	    (setq window-size-fixed 'width)))
      (imenu-list-minor-mode 1)
      (with-current-buffer (imenu-list-get-buffer-create)
	(setq window-size-fixed 'width))))
  ;; make hl-line go over existing fontification (fixes issue where the foreground colour of hl-line is not respected in imenu-list)
  (setq hl-line-overlay-priority 10)
  )
;; start imenu list at launch. i know it's not good here but i can't get it to work in any other way and i did try
;; (p/imenu-list-toggle)
;; a long time later (2025-12-01 15:07) je l'ai enlevé car j'ai l'habitude d'utiliser plusieurs fenêtres séparés dans belial bspwm et ça ne marche qu'avec la fenêtre où il est attaché. alors je l'ouvre si besoin de savoir où je suis dans le buffer mais pas besoin de l'avoir au lancement
;; + helm
(use-package helm :bind ("C-z i" . helm-imenu)
  :config
  ;; workaround to an issue with helm-imenu enter not working due to this not being defined
  (setq helm-buffers-maybe-switch-to-tab nil)
  )
;; + helm-swoop
(use-package helm-swoop
  :bind
  (("M-s z" . helm-swoop)
   ("M-s /" . helm-multi-swoop)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch))
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  )
;; + ws-butler (cleans trailing whitespace in edited lines)
(use-package ws-butler :config (add-hook 'prog-mode-hook #'ws-butler-mode))
;; + undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :demand
  :bind
  ("C-_" . undo-tree-undo)
  ("M-_" . undo-tree-redo)
  :config
  (defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
    "Split undo-tree side-by-side" ; by meqif
    (let ((split-height-threshold nil)
	  (split-width-threshold 0))
      ad-do-it))
  (setq undo-tree-history-directory-alist `(("." . ,p-undo-tree-history))) ; avoid ~undo-tree~ files all over the place
  ; (setq undo-tree-visualizer-diff t) ; press d to hide the diff when moving between states quickly for better performance [actually too laggy so commenting this out and remember you can press d to show the diff]
  (setq message-truncate-lines t) ; stops echo area jumping up every time you save a file due to both the file and its ~undo-tree~ saving, which annoys me
  ;; a long time later (2025-12-01 15:26) en fait je vais juste le désactiver, ça lag et ça plante emacs quand le undo tree est trop grand, en plus d'usurer le ssd en doublant chaque sauvegarde. je n'ai jamais aimé qu'ils ont introduit ça
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))
;; + yasnippet
(use-package yasnippet
  :defer 5
  :init
  (setq yas-snippet-dirs '(p-yas-snippet-dir))
  :config
  (yas-global-mode))
;; + multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  :config
  (defun prompt-for-insert ()           ; Squidly, magnars
    "Insert a different thing in cursor using mc"
    (interactive)
    (insert (read-string "Insert: ")))
  (define-key mc/keymap (kbd "C-i") 'prompt-for-insert))
;; + anzu (isearch with number of matches)
(use-package anzu
  :demand
  :diminish anzu-mode
  :bind
  ("C-s" . isearch-forward)
  ("C-r" . isearch-backward)
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp)
  :config
  (global-anzu-mode 1))
;; + avy
(use-package avy
  :bind
  ("M-g w" . avy-goto-word-or-subword-1)
  ("M-g M-c" . avy-copy-region) ; copies a particular start-end lines to point
  ("M-g M-l" . avy-move-line) ; moves a particular line to point
  :config
  (avy-setup-default)
  (setq avy-all-windows nil))
;; + diminish (hide modeline things you don't want to have showing)
(use-package diminish
  :diminish visual-line-mode
  )
;; + expand-region (increases selected region by semantic units)
(use-package expand-region
  :bind
  ("M-@" . er/expand-region))
;; + flycheck (errors and warnings in programming modes)
(use-package flycheck
  :after (:any python js c c++)
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; désactiver mypy pour le moment (2025-12-06 10:58)
  (setq flycheck-python-mypy-config "")
  (setq flycheck-python-mypy-executable "dontrunmypyplease")
  ;; (add-hook 'emacs-lisp-mode-hook 'flycheck-mode) ; not a good idea to enable by default in elisp files due to macros vulnerability
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))

;;; LOCAL PACKAGES
(defun ---LOCAL-PACKAGES () ())
;; + ido-preview which is in dropbox documents (so has to be here before anything that uses load-path in "W:/C/home/.emacs.d/_/packages" otherwise it will load the wrong one) [actually even if stuff that use that are after it breaks it. so don't use that path]
(use-package ido-preview
  :ensure nil
  :bind (
	 :map ido-common-completion-map
         ("C-a" . beginning-of-visual-line)
         ("C-z" . ido-preview-forward))
  :config
  (define-key ido-completion-map (kbd "C-z") 'ido-preview-forward)
  :init
  ;; hack to get it working (see log 2024-12-09 03:14)
  (run-with-idle-timer 1 nil (lambda () (require 'noflet)))
  ;; ^ `Error running timer: (error "Eager macro-expansion failure: (void-function -map)")` output is normal, and it should still work despite that
  )
;; + grab-and-drag (enables drag with middle mouse hold) (not been able to find anything as good as this on gnu/elpa/melpa. v old thing from emacswiki)
(use-package grab-and-drag
  :diminish grab-and-drag-mode
  :ensure nil
  :init
  (setq grab-and-drag-button 2)
  (setq grab-and-drag-enable-inertia nil)
  (setq grab-and-drag-gesture-time 0)
  :config
  (grab-and-drag-mode))
;; + hippie-expand (builtin)
(use-package setup-hippie :ensure nil)
;; + doentry-gen
(use-package doentry-gen :ensure nil)
;; + doentry-mode
(use-package doentry-mode :ensure nil)
;; + diredtza
;; (use-package diredtza :ensure nil
;;   :after dired)
;; + piped-mode
(use-package piped-mode :ensure nil)
;; + jekyll-gen
(use-package jekyll-gen :ensure nil)


;;; NOTES
(defun ---NOTES () ())
;; * 2024-12-08 17:22 this file's creation date is wrong, it was created on or before 2024-12-08 13:42:30. the creation date got fucked up because of emacs default insane backup settings that wipe your creation date. a mistake i have been bitten by several times.
;; * 2024-12-08 20:58 ^ that continued to be an issue even after. and i was losing my head over it. i think it was because of custom automatically modifying this file when you set something in it or install a package so the solution is (setq custom-file (concat user-emacs-directory "custom.el")) [in addition to (setq backup-by-copying t) or disabling backups]
;; * 2024-12-08 22:13 setting that made custom not work properly. so i am going to just put all this crap in a new file and let init.el be just autogenerated and require my file that actually has my settings in it
;; Warning (initialization): Your ‘load-path’ seems to contain
;; your ‘user-emacs-directory’: w:/B/dev/pc/gccemacs/_/
;; This is likely to cause problems...
;; Consider using a subdirectory instead, e.g.: w:/B/dev/pc/gccemacs/_/lisp
;; so it has to be in a different path

;;; KNOWN ISSUES:
;; * ?? in line number for large buffers. this is a performance thing so don't want to disable it. to check your position in buffer you can do M-x what-line. it's not bound to anything by default sadly so i'll bind it here (2024-12-09 06:48). and to calculate percentage: C-x = (what-cursor-position)
(global-set-key (kbd "C-z l") 'what-line)

(provide 'pm)

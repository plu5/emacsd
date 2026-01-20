;;; fsffdark-theme.el --- Dark Emacs color theme windows

;; Copyright (C) 2003-2014 Free Software Foundation, Inc.

;; URL: https://github.com/plu5/emacs-fsff-theme
;; Version: 20170220.20:59
;; Keywords: color theme

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Dark variant of fsff-theme for Emacs.
;;
;; Requirements: Emacs 24.

;;; Code:

;; Docstring stolen from alect-themes
(defvar fsffdark-display-class  '((type graphic))
  "Class of terminals (DISPLAY) for which fsffdark theme faces are applied.
For other terminals, faces stay unthemed.
See Info node `(elisp) Defining Faces' for the possibilities for
DISPLAY.

Example values
  t
  '((type graphic))
  '((class color) (min-colors N)) terminals with min N number of colours
  '((type x w32 mac))")


(deftheme fsffdark
  "Dark Emacs color theme")

(let ((c fsffdark-display-class))
  (custom-theme-set-faces
   'fsffdark

   `(default ((,c (:foreground "white" :background "grey5"))))
   `(fringe ((,c (:inherit default :foreground "grey20" :background "black"))))
   `(custom-button ((,c (:background "grey85" :foreground "black" :box (:line-width 1 :style released-button)))))
   `(tool-bar ((,c (:foreground "black" :background "grey92" :box (:line-width 1 :style released-button)))))
   `(vertical-border ((,c (:foreground "black"))))

   `(highlight ((,c (:background "darkolivegreen"))))
   `(region ((,c (:background "#404000"))))
   `(secondary-selection ((,c (:background "#555500"))))
   `(trailing-whitespace ((,c (:background "grey15"))))

   `(mode-line ((,c (:foreground "white" :background "black" :box nil))))
   `(mode-line-inactive ((,c :foreground "grey50" :background "grey10")))
   `(mode-line-highlight ((,c (:foreground "black" :background "snow" :inherit (highlight)))))
   ;; ↓ or just (:foreground "white" :weight bold) for more minimal. i think minimal looks better but harder to notice vertical split between windows.
   `(mode-line-buffer-id ((,c (:foreground "black" :background "gold4"))))
   `(mode-line-emphasis ((,c (:foreground "grey30" :weight bold))))

   `(powerline-active1 ((,c (:inherit mode-line :background "gold3"))))
   `(powerline-active2 ((,c (:inherit mode-line :background "gold4"))))
   `(powerline-inactive1 ((,c (:inherit mode-line-inactive :background "grey85" :foreground "grey20"))))
   `(powerline-inactive2 ((,c (:inherit mode-line-inactive :background "white" :foreground "grey20"))))

   `(show-paren-mismatch  ((,c (:foreground "#e9e9e9" :background "#ea3838"))))
   `(show-paren-match     ((,c (:foreground "#e9e9e9" :background "#3cb370"))))
   
   `(erc-notice-face ((,c (:foreground "gray65"))))
   `(erc-timestamp-face ((,c (:foreground "white" :weight bold))))

   `(font-lock-comment-face ((,c (:foreground "red" :slant italic))))
   `(font-lock-string-face ((,c (:foreground "mediumseagreen"))))
   `(font-lock-doc-face ((,c (:foreground "red3"))))
   `(font-lock-keyword-face ((,c (:foreground "white" :weight bold))))
   `(font-lock-function-name-face ((,c (:foreground "steelblue"))))
   `(font-lock-variable-name-face ((,c (:foreground "#91b"))))
   `(font-lock-constant-face ((,c (:foreground "lightslateblue"))))
   `(font-lock-type-face ((,c (:foreground "khaki"))))
   
   `(ein:cell-input-area ((,c (:background "black"))) t)
   `(calendar-today ((,c (:background "yellow1" :underline t))))
   
   `(diff-added ((,c (:inherit diff-changed :foreground "green3"))))
   `(diff-file-header ((,c (:background "grey80" :box (:line-width 2 :color "grey80") :weight bold))))
   `(diff-header ((,c (:background "grey80" :box (:line-width 2 :color "grey80")))))
   `(diff-hunk-header ((,c (:inherit diff-header :box (:line-width 2 :color "grey80")))))
   `(diff-removed ((,c (:foreground "red"))))
   
   `(dired-directory ((,c (:inherit font-lock-keyword-face))))
   `(dired-filetype-compress ((,c (:foreground "Orchid"))))
   `(dired-filetype-execute ((,c (:foreground "green4" :weight bold))))
   `(dired-filetype-omit ((,c (:foreground "gray50"))))
   `(dired-filetype-plain ((,c (:foreground "SeaGreen"))))
   `(dired-filetype-source ((,c (:foreground "red3" :weight bold))))
   `(dired-filetype-video ((,c (:foreground "brown"))))
   `(flymake-warnline ((,c (:background "LightBlue3"))))
   `(highlight-indentation-face ((,c (:inherit fringe :background "black"))))
   `(jabber-activity-face ((,c (:background "green1"))))
   `(jabber-chat-error ((,c (:background "pink"))))
   `(jabber-chat-prompt-system ((,c (:foreground "green3" :weight bold))))
   `(jabber-chat-text-local ((,c (:foreground "red3"))))
   `(jabber-title-large ((,c (:weight bold :height 2.0 :width expanded))))
   `(jabber-title-medium ((,c (:background "#ffb" :box (:line-width 2 :color "grey75" :style released-button) :weight bold :height 1.2 :width expanded))))
   
   `(helm-M-x-key ((,c (:foreground "orange4" :underline t))))
   `(helm-ff-executable ((,c (:foreground "darkgreen" :weight bold))))
   `(helm-ff-symlink ((,c (:foreground "orange4"))))
   `(helm-grep-lineno ((,c (:foreground "orange4"))))
   `(helm-source-header ((,c (:background "black" :foreground "white" :box (:line-width 1 :color "grey75" :style pressed-button) :weight normal :height 1.0 :family "Sans Serif"))))
   `(helm-selection ((,c (:foreground "black" :background "gold4"))))
   `(helm-selection-line ((,c (:background "#e3e300"))))
   `(helm-ff-dotted-directory ((,c :foreground "blue4" :underline t)))
   
   `(magit-branch ((,c (:inherit magit-header :background "yellow" :box (:line-width 1 :color "grey75" :style released-button)))))
   `(magit-branch-current ((,c (:inherit magit-branch-local :background "green1" :box 1))))
   `(magit-branch-local ((,c (:background "black" :box (:line-width 1 :color "*")))))
   `(magit-branch-remote ((,c (:background "black" :foreground "DarkOliveGreen4" :box (:line-width 1 :color "gray25")))))
   `(magit-diff-add ((,c (:inherit diff-added))))
   `(magit-diff-del ((,c (:inherit diff-removed))))
   `(magit-diff-none ((,c (:inherit diff-context))))
   `(magit-header ((,c (:inherit header-line :background "white"))))
   `(magit-item-highlight ((,c (:inherit highlight))))
   `(magit-refname ((,c (:foreground "grey30" :box (:line-width 2 :color "grey75")))))
   `(magit-section-heading ((,c (:background "black" :box (:line-width 1 :color "grey75" :style released-button) :weight bold))))
   `(magit-section-title ((,c (:inherit magit-header :box (:line-width 1 :color "grey75" :style released-button)))))
   `(magit-tag ((,c (:background "black" :foreground "Goldenrod4" :box (:line-width 1 :color "gray25")))))
   
   `(org-agenda-structure ((,c (:foreground "#5F7F5F"))))
   `(org-agenda-date ((,c (:background "grey90" :foreground "grey30"))))
   `(org-agenda-date-weekend ((,c (:inherit org-agenda-date :slant italic))))
   `(org-agenda-date-today ((,c (:inherit org-agenda-date :weight bold))))
   `(org-agenda-calendar-event ((,c :foreground "#785607")))
   `(org-agenda-dimmed-todo-face ((,c (:background "yellow3" :foreground "black"))))
   `(org-agenda-done ((,c (:foreground "grey50"))))
   `(org-block-begin-line ((,c (:foreground "#BDBDAE"))))
   `(org-block-end-line ((,c (:foreground "#BDBDAE"))))
   `(org-block ((,c (:foreground "grey90"))))
   `(org-meta-line ((,c (:inherit shadow))))
   `(org-checkbox ((,c (:weight ultrabold :foreground "white"))))
   `(org-hide ((,c (:background "#c8c8bb" :foreground "#c8c8bb"))))
   `(org-code ((,c (:foreground "#00ff40"))))
   `(org-latex-and-related ((,c (:foreground "#766458"))))
   `(org-special-keyword ((,c (:inherit shadow))))
   `(outline-1 ((,c (:slant italic :background "grey20"))))
   `(outline-2 ((,c (:slant italic :foreground "#e6cb1e"))))
   `(outline-3 ((,c (:slant italic :foreground "#e67c02"))))
   `(outline-4 ((,c (:slant italic :foreground "#e60219"))))
   `(outline-5 ((,c (:slant italic :foreground "#e6008e"))))
   `(outline-6 ((,c (:slant italic :foreground "#cb00e6"))))
   `(outline-7 ((,c (:slant italic :foreground "#8133ff"))))
   `(outline-8 ((,c (:slant italic :foreground "#001eff"))))

   `(speedbar-separator-face ((,c (:background "white"))))
   `(speedbar-button-face ((,c (:foreground "white" :weight ultrabold))))
   `(speedbar-highlight-face ((,c (:background "gold4"))))
   `(speedbar-file-face ((,c (:foreground "grey10"))))
   `(speedbar-selected-face ((,c (:foreground "black" :background "gold4" :weight bold))))
   `(Pe/directory-face ((,c (:foreground "white" :weight ultrabold))))
   `(pe/file-face ((,c (:foreground "grey10"))))
   `(sh-heredoc ((,c (:foreground "tan4"))))
   `(warning ((,c (:foreground "DarkOrange3" :weight bold))))

   `(visible-mark-active ((,c (:background "gold"))))
   `(visible-mark-face1 ((,c (:background "grey70"))))
   `(visible-mark-face2 ((,c (:background "grey85"))))

   `(flycheck-error-list-warning ((,c (:foreground "grey60"))))
   `(flycheck-fringe-warning ((,c (:foreground "grey60"))))
   `(flycheck-warning ((,c (:underline (:color "grey60" :style wave)))))
   `(form-feed-line ((,c (:strike-through "white"))))
   `(company-scrollbar-bg ((,c (:background "snow2"))))
   `(company-scrollbar-fg ((,c (:background "grey"))))
   `(company-tooltip ((,c (:foreground "black" :background "grey95"))))
   `(company-tooltip-selection ((,c (:background "gold4"))))
   `(yascroll:thumb-fringe ((,c (:background "gold4" :foreground "gold4"))))

   `(anzu-mode-line ((,c (:foreground "darkgreen" :weight bold))))
   `(avy-background-face ((,c (:foreground "#cfd8dc"))))
   `(avy-lead-face ((,c (:inherit avy-lead-face-0))))
   `(avy-lead-face-0 ((,c (:foreground "white" :background "#558b2f" :weight bold))))
   `(aw-leading-char-face ((,c (:foreground "white" :background "#558b2f" :weight bold :height 1.7))))
   ;; requires (setq hl-line-overlay-priority 10) to work on top of other things
   `(hl-line ((,c (:foreground "black" :background "gold4"))))

   `(imenu-list-entry-face-0 ((,c (:foreground "white"))))
   `(imenu-list-entry-face-1 ((,c (:foreground "#e6cb1e"))))
   `(imenu-list-entry-face-2 ((,c (:foreground "#e67c02"))))
   `(imenu-list-entry-face-3 ((,c (:foreground "#e60219"))))
   
   `(rainbow-delimiters-depth-1-face ((,c (:foreground "#e91e63"))))
   `(rainbow-delimiters-depth-2-face ((,c (:foreground "#1565c0"))))
   `(rainbow-delimiters-depth-3-face ((,c (:foreground "#ef6c00"))))
   `(rainbow-delimiters-depth-4-face ((,c (:foreground "purple"))))
   `(rainbow-delimiters-depth-5-face ((,c (:foreground "darkolivegreen"))))
   `(rainbow-delimiters-depth-6-face ((,c (:foreground "#26A69A"))))
   `(rainbow-delimiters-depth-7-face ((,c (:foreground "#B71C1C"))))
   `(rainbow-delimiters-depth-8-face ((,c (:foreground "brown"))))
   `(rainbow-delimiters-depth-9-face ((,c (:foreground "navy"))))
   `(rainbow-delimiters-unmatched-face ((,c (:foreground "#212121" :background "#EF6C00"))))
   
   `(yascroll:thumb-fringe ((,c (:foreground "grey70" :background "grey70"))))
   `(eros-result-overlay-face ((,c (:foreground "grey70" :background "grey20"))))

   `(ido-only-match ((,c (:inherit ido-first-match))))
   `(ido-subdir ((,c (:foreground "darkred"))))
   
   `(minimap-current-line-face ((,c (:background "gold4"))))
   `(minimap-active-region-background ((,c (:foreground "black" :background "grey20"))))

   `(git-timemachine-commit ((,c (:foreground "#DC322F" :weight bold))))
   `(git-timemachine-minibuffer-author-face ((,c (:foreground "#8CB5C6" :weight bold))))
   `(git-timemachine-minibuffer-detail-face ((,c (:foreground "#586E75" :weight bold))))

   `(term-color-black ((,c (:foreground "#073642" :background "#073642"))))
   `(term-color-blue ((,c (:foreground "#268bd2" :background "#268bd2"))))
   `(term-color-cyan ((,c (:foreground "#2aa198" :background "#2aa198"))))
   `(term-color-green ((,c (:foreground "#588d75" :background "#588d75"))))
   `(term-color-magenta ((,c (:foreground "#d33682" :background "#d33682"))))
   `(term-color-red ((,c (:foreground "#dc322f" :background "#dc322f"))))
   `(term-color-white ((,c (:foreground "#8c8c8c" :background "#8c8c8c"))))
   `(term-color-yellow ((,c (:foreground "#b58900" :background "#b58900"))))))


(provide-theme 'fsffdark)
;;; fsffdark-theme.el ends here

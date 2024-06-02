;;; Josh's Pretty Cool Emacs Config
 
;; Personal Info
(setq user-full-name "Joshua Markle"
      user-mail-address "joshuamarkle25@gmail.com")

;; Fonts
(setq doom-font (font-spec :family "CaskaydiaMono Nerd Font" :size 13))

;; General
(setq display-line-numbers-type t)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night t))

;; Org
(setq org-directory "~/Sync/Notes/")
(setq org-agenda-files (list "~/Sync/Notes/personal.org"
                             "~/Sync/Notes/School/math.org"))

;; Modeline
(setq doom-modeline-height 35 ;; General Modeline config
      doom-modeline-mode-icon nil
      size-indication-mode nil
      column-number-mode nil
      line-number-mode nil
      doom-modeline-buffer-encoding nil)
(after! doom-modeline ;; Create custom VIM indicators
  (doom-modeline-def-segment modals
    "Displays modal editing states."
    (let* ((evil (when (bound-and-true-p evil-local-mode)
                   (let ((tag (cond
                               ((evil-normal-state-p) (propertize " NORMAL " 'face `(:background "#7aa2f7" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               ((evil-insert-state-p) (propertize " INSERT " 'face `(:background "#9ece6a" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               ((evil-visual-state-p) (propertize " VISUAL " 'face `(:background "#bb9af7" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               ((evil-replace-state-p) (propertize " REPLACE " 'face `(:background "#f7768e" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               ((evil-motion-state-p) (propertize " MOTION " 'face `(:background "#ff9e64" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               ((evil-operator-state-p) (propertize " OPERATOR " 'face `(:background "#0db9d7" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               ((evil-emacs-state-p) (propertize " EMACS " 'face `(:background "#9d7cd8" :foreground "#16161e" :box (:line-width (0 . 8) :color "#16161e" :style nil))))
                               (t (evil-state-property evil-state :tag t))))) ; Catch custom or undefined states
                     tag)))
           (ow (doom-modeline--overwrite))
           (god (doom-modeline--god))
           (ryo (doom-modeline--ryo))
           (xf (doom-modeline--xah-fly-keys))
           (vsep (doom-modeline-vspc))
           (sep (and (or evil ow god ryo xf) (doom-modeline-spc))))
      (concat sep
              (and evil (concat evil (and (or ow god ryo xf) vsep)))
              (and ow (concat ow (and (or god ryo xf) vsep)))
              (and god (concat god (and (or ryo xf) vsep)))
              (and ryo (concat ryo (and xf vsep)))
              xf
              sep))))

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

; Dashboard
(setq fancy-splash-image (concat doom-user-dir "emacs.png"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

;; Org
(setq org-directory "~/sync/notes/")
(setq org-agenda-files (list "~/sync/notes/personal.org"
                             "~/sync/notes/school/math.org"))

;; Modeline
(setq doom-modeline-height 35 ;; General Modeline config
      doom-modeline-mode-icon nil
      size-indication-mode nil
      column-number-mode nil
      line-number-mode nil
      doom-modeline-buffer-encoding nil)
(after! '(doom-modeline doom-theme)
  (set-face-attribute 'doom-modeline-bar nil :background (doom-color 'blue) :foreground 'fg)
  (set-face-attribute 'doom-modeline-highlight nil :background (doom-color 'blue) :foreground 'fg)
  (set-face-attribute 'doom-modeline-panel nil :background (doom-color 'blue) :foreground 'fg)
  (setq auto-revert-check-vc-info t
        doom-modeline-major-mode-icon nil
        ;; doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-github nil
        doom-modeline-vcs-max-length 60)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-modeline-def-modeline 'main ; Custom minimal modeline
    '(matches bar modals workspace-name window-number persp-name selection-info buffer-info remote-host debug matches)
    '(vcs github mu4e grip gnus check misc-info repl lsp " "))

  (doom-modeline-def-segment modals ;; Create custom VIM indicators
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

; Competative Programming
(map! :leader
      (:prefix ("e" . "execute")
        :desc "Run code in term"
        "c" #'run-code-in-term))

(defun run-code-in-term ()
  "Run code in vterm"
  (interactive)
  (let ((file-name (buffer-file-name))
        (target-vterm-buffer (or (get-buffer "*doom:vterm-popup:main*")
                                 (get-buffer "*vterm*"))))
    (unless target-vterm-buffer
      (vterm)
      (setq target-vterm-buffer (current-buffer))
      (rename-buffer "*vterm*")) ; Rename to standard vterm
    (switch-to-buffer-other-window target-vterm-buffer)
    (cond
     ((string-suffix-p ".py" file-name) ; Python
      (vterm-send-string (format "python %s\n" file-name)))
     ((string-suffix-p ".cpp" file-name) ; C++
      (let ((output-file (file-name-sans-extension file-name)))
        (vterm-send-string (format "g++ -o %s %s && %s\n" output-file file-name output-file))))
     (t
      (message "File type not supported for execution in vterm.")))))

;; Vterm
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 100000
        vterm-kill-buffer-on-exit t
        vterm-shell "/bin/bash")
  (evil-collection-define-key 'normal 'vterm-mode-map "c" #'vterm-clear)
  (defun my/vterm-auto-insert ()
    (when (eq major-mode 'vterm-mode)
      (evil-insert 1)))
  (advice-add 'vterm-send-return :after #'my/vterm-auto-insert))

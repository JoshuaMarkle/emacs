;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.

(setq user-full-name "Joshua Markle"
      user-mail-address "joshuamarkle25@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font (font-spec :family "CaskaydiaMono Nerd Font" :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/Notes/")
(setq org-agenda-files (list "~/Notes/personal.org"
                             "~/Notes/School/math.org"
                             "~/Notes/work.org"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Modeline
;; Change the default VIM mode identifier
;; Create a small box with NORMAL/INSERT/VISUAL etc
(setq doom-modeline-height 35
        doom-modeline-mode-icon nil
        size-indication-mode nil
        column-number-mode nil
        line-number-mode nil
        doom-modeline-buffer-encoding nil)

(after! doom-modeline
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

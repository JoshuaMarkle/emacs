(setq user-full-name "Joshua Markle"
    user-mail-address "joshuamarkle25@gmail.com")

(setq doom-font (font-spec :family "CaskaydiaMono Nerd Font" :size 13))

; Tabs
(setq-default tab-width 4
    indent-tabs-mode nil
    c-basic-offset 4
    lisp-body-indent 4
    lisp-indent-offset 4)

; Add line numbers only in programming mode
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))
(setq display-line-numbers-type 'absolute)

(use-package doom-themes
    :ensure t
    :config
    (load-theme 'tokyo-night t))

(setq fancy-splash-image (concat doom-user-dir "emacs.png"))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

; Hide modeline on dashboard
(defun hide-modeline-in-dashboard ()
  (when (string-equal (buffer-name) "*doom*")
    (setq mode-line-format nil)))
(add-hook 'buffer-list-update-hook 'hide-modeline-in-dashboard)

(setq org-directory "~/sync/notes/")
(setq org-agenda-files (list "~/sync/notes/personal.org"
                           "~/sync/notes/school/math.org"))

; Extra org preferences
(setq org-startup-with-inline-images t)

(use-package org
  :config
  (setq org-latex-classes
        '(("notes"
           "\\documentclass{notes}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (setq org-latex-compiler "xelatex") ; For custom fonts
  (setq org-export-headline-levels 4) ; Ensure correct headings
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings 'minted) ; Fancy code blocks
  (add-to-list 'org-latex-packages-alist '("" "minted")))

;; Mapping to export note as pdf
(map! :leader
      (:prefix ("n" . "notes")
        :desc "Export note as PDF" "e" #'org-latex-export-to-pdf))

(setq doom-modeline-height 35
    doom-modeline-mode-icon nil
    size-indication-mode nil
    column-number-mode nil
    line-number-mode nil
    doom-modeline-buffer-encoding nil)

(after! doom-modeline
    (setq auto-revert-check-vc-info t
        doom-modeline-major-mode-icon nil
        doom-modeline-github nil
        doom-modeline-vcs-max-length 60)
    (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)

    ; Custom minimal modeline
    (doom-modeline-def-modeline 'main
        '(matches bar modals workspace-name window-number persp-name selection-info buffer-info remote-host debug)
        '(vcs github mu4e grip gnus check misc-info repl lsp " "))

    ; Create custom VIM indicators
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

(defun bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings"
    (when (and (eq major-mode 'comint-mode)
              (string-match "finished" string)
              (not
                  (with-current-buffer buffer
                      (search-forward "warning" nil t))))
        (run-with-timer 1 nil
            (lambda (buf)
                (let ((window (get-buffer-window buf)))
                    (when (and (window-live-p window)
                              (eq buf (window-buffer window)))
                        (delete-window window))))
            buffer)))

(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)

(map! :leader
      (:prefix ("e" . "execute")
        :desc "Run code in term" "c" #'run-code-in-term
        :desc "Run code in term using scratch" "s" (lambda () (interactive) (run-code-in-term t))))

(defun run-code-in-term (use-scratch)
    (interactive "P") ; Prompt prefix (for scratch buffer grabbing)

    ; Detect any open terminals
    (let ((file-name (buffer-file-name))
             (target-vterm-buffer (or (get-buffer "*doom:vterm-popup:main*")
                                      (get-buffer "*vterm*")))
             (scratch-content (when use-scratch
                                  (with-current-buffer "*doom:scratch*"
                                      (buffer-string))))) ; Get scratch content only if use-scratch is true
        (unless target-vterm-buffer
            (vterm)
            (setq target-vterm-buffer (current-buffer))
            (rename-buffer "*vterm*")) ; Rename to standard vterm
        (switch-to-buffer-other-window target-vterm-buffer)
        (cond

            ; Automatically run singular file
            ((string-suffix-p ".py" file-name) ; Python
                (vterm-send-string (format "python %s\n" file-name))
                (when use-scratch (vterm-send-string scratch-content)))
            ((string-suffix-p ".cpp" file-name) ; C++
                (let ((output-file (file-name-sans-extension file-name)))
                    (vterm-send-string (format "g++ -o %s %s && %s\n" output-file file-name output-file))
                    (when use-scratch (vterm-send-string scratch-content))))
            (t
                (message "File type not supported")))))

(use-package vterm
    :ensure t
    :config
    (setq vterm-max-scrollback 100000
        vterm-kill-buffer-on-exit t
        vterm-shell "/bin/bash"
        vterm-prompt-regexp "  .* ") ; My prompts typically look like this

    ; Clear term mapping
    (evil-collection-define-key 'normal 'vterm-mode-map "c" #'vterm-clear)

    ; Paste content from scratch buffer (used for large code inputs)
    (evil-collection-define-key 'normal 'vterm-mode-map "s" #'paste-from-scratch)
    (defun paste-from-scratch ()
        (interactive)
        (let ((scratch-content (with-current-buffer "*scratch*"
                                   (buffer-string))))
            (vterm-send-string scratch-content)))

    ; Enter insert mode after exec
    (defun my/vterm-auto-insert ()
        (when (eq major-mode 'vterm-mode)
            (evil-insert 1)))
    (advice-add 'vterm-send-return :after #'my/vterm-auto-insert))

(after! evil
    (general-define-key
    :states 'normal
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right))

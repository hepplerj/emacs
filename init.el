;;; -*- lexical-binding: t -*-

(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 20 1024 1024))
  (gcmh-mode 1))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(eval-when-compile
  (dolist (sym '(cl-flet lisp-complete-symbol))
    (setplist sym (use-package-plist-delete
                   (symbol-plist sym) 'byte-obsolete-info))))

(setq which-func-update-delay 1.0)

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq native-comp-eln-load-path '("~/.config/emacs/eln-cache/"))

;; House keeping 
(setq user-emacs-directory "~/.config/emacs/")
(setq default-directory "~/")

(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(setq large-file-warning-threshold (* 50 1024 1024))

;; Package manager 
(require 'package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 20)
        ("MELPA"        . 15)
        ("ORG"          . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))
(package-initialize)

;; Local files 
(defvar local-lisp (concat user-emacs-directory "local-lisp/"))
(add-to-list 'load-path  local-lisp)
(let ((default-directory local-lisp))
  (normal-top-level-add-subdirs-to-load-path))

(setq org-agenda-files '())
(setq register-alist '())
(setq projectile-project-search-path '())

(add-hook
 'after-init-hook
 (lambda ()
   (let ((init-file (concat user-emacs-directory "init.el"))
         (private-file (concat user-emacs-directory "private.el"))
         (custom-file (concat user-emacs-directory "custom.el")))
     (when (file-exists-p private-file)
       (load-file private-file))
     (when (file-exists-p custom-file)
       (load-file custom-file))
     (server-start))))

;; support for org dblocks
(add-to-list 'load-path "~/.config/emacs/nursery/lisp")

;; Terminal setup 
(dirtrack-mode t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq vterm-shell "/opt/homebrew/bin/fish"))

(use-package dwim-shell-command
  :defer t
  :init (require 'dwim-shell-commands))

;; Keymappings 
(defvar custom-bindings-map (make-keymap)
  "A keymap for custom keybindings.")

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings-map."
  :init-value t
  :global t
  :keymap custom-bindings-map)

(custom-bindings-mode 1)

(setq mac-command-modifier       'meta
      mac-right-command-modifier 'meta
      mac-option-modifier        nil
      mac-right-option-modifier  nil)

(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; Visuals 
(dolist (mode
         '(tool-bar-mode       ;; Remove toolbar
           scroll-bar-mode     ;; Remove scollbars
           menu-bar-mode       ;; Remove menu bar
           blink-cursor-mode)) ;; Solid cursor, not blinking
  (funcall mode 0))

(setq inhibit-startup-message           t       ;; No startup message
      inhibit-startup-echo-area-message t       ;; No startup message in echo area
      inhibit-startup-screen            t       ;; No default startup screen
      initial-buffer-choice             t       ;; *scratch* is default startup buffer
      initial-major-mode                'fundamental-mode
      ring-bell-function                'ignore ;; No bell
      display-time-default-load-average nil     ;; Don't show me load time
      scroll-margin                     0       ;; Space between top/bottom
      use-dialog-box                    nil)    ;; Disable dialog

;; Frames and windows
(add-to-list 'default-frame-alist     '(fullscreen . maximized))
;; (add-hook 'window-setup-hook          'toggle-frame-fullscreen t)  ;; F11

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(90 . 55) '(100 . 100)))))
(global-set-key (kbd "C-c h t") 'toggle-transparency)

(add-to-list 'default-frame-alist '(internal-border-width . 16))

(set-fringe-mode 10)                          ;; Set fringe width to 10

(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil)       ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'header-line t :inherit 'default)

;; emacs plus macos 
(when (eq system-type 'darwin)
  ; no title bar
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  ; don't use proxy icon
  (setq ns-use-proxy-icon nil)
  ; don't show buffer name in title bar
  (setq frame-title-format ""))

;; Programming 
(setq-default cursor-type 'bar)

(use-package beacon
  :defer t
  :init  (beacon-mode 1)
  :bind (:map custom-bindings-map ("C-:" . beacon-blink))
  :config
  (setq beacon-blink-when-window-scrolls nil))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(show-paren-mode t) ;; Highlight matching parentheses

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq my-whitespace-style '(face tabs lines-tail)
      whitespace-style my-whitespace-style
      whitespace-line-column 120
      fill-column 120
      whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [36 10])
        (tab-mark 9 [9655 9] [92 9])))

;; in e.g. clojure-mode-hook
;; (whitespace-mode 1)
;; or globally
;; (global-whitespace-mode 1)
(add-hook 'prog-mode 'whitespace-mode)

;; Fonts 
(defvar jah/font-height 115)

(when (eq system-type 'darwin)
  (setq jah/font-height 115))

(when (member "Fragment Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Fragment Mono" :height jah/font-height)
  (set-face-attribute 'fixed-pitch nil :family "Fragment Mono"))

(when (member "Roboto Mono" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Roboto Mono"))

(use-package mixed-pitch
  :defer t
  :hook ((org-mode   . mixed-pitch-mode)
         (LaTeX-mode . mixed-pitch-mode)))

(defvar ligature-def '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                       "\\\\" "://"))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode ligature-def)
  (global-ligature-mode t))

(setq text-scale-mode-step 1.1)

(use-package default-text-scale
  :defer t
  :bind (:map custom-bindings-map
              ("C-x C-+" . default-text-scale-increase)
              ("C-x C--" . default-text-scale-decrease)
              ("C-x C-0" . default-text-scale-reset)))

;; Icons 
(use-package nerd-icons)

(use-package emojify
  :config
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font
      t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))

;; Themes 
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(use-package south-theme
  :vc (:url "https://github.com/SophieBosio/south"
       :rev :newest
       :branch "main"))

(use-package nano-theme
  :vc (:url "https://github.com/rougier/nano-theme"
            :rev :newest
            :branch "main"))

(setq custom-safe-themes t)

(defvar jah/default-dark-theme  'doom-nord)
(defvar jah/default-light-theme 'south)

(defvar jah/default-dark-accent-colour  "SkyBlue4")
(defvar jah/default-light-accent-colour "#D9EDFC")

(load-theme jah/default-light-theme t)

(use-package autothemer
  :defer t)

(use-package auto-dark
  :ensure t
  :hook ((auto-dark-dark-mode
          .
          (lambda ()
            (interactive)
            (progn
              (custom-set-faces
               `(eval-sexp-fu-flash
                 ((t (:background
                      ,jah/default-dark-accent-colour)))))
              `(load-theme ,jah/default-dark-theme t))))
         (auto-dark-light-mode
          .
          (lambda ()
            (interactive)
            (progn
              (custom-set-faces
               `(eval-sexp-fu-flash
                 ((t (:background
                      ,jah/default-light-accent-colour)))))
              `(load-theme ,jah/default-light-theme t)))))
  :custom
  (auto-dark-themes                   `((,jah/default-dark-theme) (,jah/default-light-theme)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript          t)
  :init (auto-dark-mode t))

(when (eq system-type 'darwin)
  (define-key custom-bindings-map (kbd "M-T") 'dwim-shell-commands-macos-toggle-dark-mode))

(defun disable-custom-themes (theme &optional no-confirm no-enable)
  (mapc 'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'disable-custom-themes)

;; Mode line
(column-number-mode t) ;; Show current column number in mode line

(defvar lsp-modeline--code-actions-string nil)

(setq-default mode-line-format
  '("%e"
	(:propertize " " display (raise +0.4)) ;; Top padding
	(:propertize " " display (raise -0.4)) ;; Bottom padding

	(:propertize "λ " face font-lock-comment-face)
	mode-line-frame-identification
	mode-line-buffer-identification

	;; Version control info
	(:eval (when-let* ((vc vc-mode))
			 ;; Use a pretty branch symbol in front of the branch name
			 (list (propertize "   " 'face 'font-lock-comment-face)
                   ;; Truncate branch name to 50 characters
				   (propertize (truncate-string-to-width
                                (substring vc 5) 50)
							   'face 'font-lock-comment-face))))

	;; Add space to align to the right
	(:eval (propertize
			 " " 'display
			 `((space :align-to
					  (-  (+ right right-fringe right-margin)
						 ,(+ 3
                             (string-width (or lsp-modeline--code-actions-string ""))
                             (string-width "%4l:3%c")))))))

    ;; LSP code actions
    (:eval (or lsp-modeline--code-actions-string ""))
	
	;; Line and column numbers
	(:propertize "%4l:%c" face mode-line-buffer-id)))

(use-package hide-mode-line
  :defer t
  :bind (:map custom-bindings-map ("C-c h m" . hide-mode-line-mode)))

;; Text display 
(use-package olivetti
  :defer t
  :config
  (setq olivetti-style t)
  :hook (olivetti-mode . (lambda () 
                           (if olivetti-mode
                               (text-scale-set 2)
                             (text-scale-set 0)))))

(use-package adaptive-wrap
  :defer t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package writeroom-mode
  :defer t)

(use-package focus
  :defer t)

;; General editing 
(delete-selection-mode   t) ;; Replace selected text when yanking
(global-so-long-mode     t) ;; Mitigate performance for long lines
(global-visual-line-mode t) ;; Break lines instead of truncating them
(global-auto-revert-mode t) ;; Revert buffers automatically when they change
(recentf-mode            t) ;; Remember recently opened files
(savehist-mode           t) ;; Remember minibuffer prompt history
(save-place-mode         t) ;; Remember last cursor location in file

(setq auto-revert-interval         1         ;; Refresh buffers fast
      auto-revert-verbose          nil       ;; Don't notify me about reverts
      echo-keystrokes              0.1       ;; Show keystrokes fast
      frame-inhibit-implied-resize 1         ;; Don't resize frame implicitly
      sentence-end-double-space    nil       ;; No double spaces
      recentf-max-saved-items      1000      ;; Show more recent files
      use-short-answers            t         ;; 'y'/'n' instead of 'yes'/'no' etc.
      save-interprogram-paste-before-kill t  ;; Save copies between programs
      history-length               25        ;; Only save the last 25 minibuffer prompts
      global-auto-revert-non-file-buffers t) ;; Revert Dired and other buffers

(setq-default tab-width 4)  ;; Smaller tabs

;; Scrolling
(setq scroll-conservatively            101
      mouse-wheel-follow-mouse         't
      mouse-wheel-progressive-speed    nil
      ;; Scroll 1 line at a time, instead of default 5
      ;; Hold shift to scroll faster and meta to scroll very fast
      mouse-wheel-scroll-amount        '(1 ((shift) . 3) ((meta) . 6)))

;; (Native) smooooooth scrolling
(pixel-scroll-mode)
(pixel-scroll-precision-mode)

(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll    nil)

;; Tabs
(defun infer-indentation-style ()
  "Default to no tabs, but use tabs if already in project"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count   (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq-default indent-tabs-mode nil))
    (if (> tab-count space-count) (setq-default indent-tabs-mode t))))

(setq-default indent-tabs-mode nil)
(infer-indentation-style)

(define-key custom-bindings-map (kbd "<backtab>") 'indent-rigidly-left)

(setq backward-delete-char-untabify-method 'hungry)

(defun jah/delete-dont-kill (arg)
  "Delete characters backward until encountering the beginning of a word.
   With argument ARG, do this that many times. Don't add to kill ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun jah/backward-delete ()
  "Delete a word, a character, or whitespace."
  (interactive)
  (cond
   ;; If you see a word, delete all of it
   ((looking-back (rx (char word)) 1)
    (jah/delete-dont-kill 1))
   ;; If you see a single whitespace and a word, delete both together
   ((looking-back (rx (seq (char word) (= 1 blank))) 1)
	(jah/delete-dont-kill 1))
   ;; If you see several whitespaces, delete them until the next word
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   ;; If you see a single non-word character, delete that
   (t
    (backward-delete-char-untabify 1))))

(define-key custom-bindings-map [C-backspace] 'jah/backward-delete)

;; Browse killring
(use-package browse-kill-ring
  :defer t)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(use-package auto-save-buffers-enhanced
  :ensure t
  :config
  (auto-save-buffers-enhanced t)
  (setq auto-save-buffers-enhanced-exclude-regexps
        '("init.el"
          "\\(\\.js\\|\\.ts\\|\\.jsx\\|\\.tsx\\|\\.html\\)$")))

;; Move where I mean 
(use-package mwim
  :ensure t
  :bind (:map custom-bindings-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)))

;; Text editing functions 
(use-package expand-region
  :defer t
  :bind (:map custom-bindings-map
              ("M-q" . er/expand-region)
              ("M-'" . er/contract-region)))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
		;; This would override `fill-column' if it's an integer.
		(emacs-lisp-docstring-fill-column t))
	(fill-paragraph nil region)))
;; Handy key definition
(define-key custom-bindings-map (kbd "C-c n q") 'unfill-paragraph)

(use-package multiple-cursors
  :defer t
  :functions
  mc/remove-fake-cursors
  mc/save-excursion
  mc/create-fake-cursor-at-point
  mc/maybe-multiple-cursors-mode
  :bind (:map custom-bindings-map
              ("M-n" . mc/mark-next-like-this)
              ("M-p" . mc/mark-previous-like-this))
  :config
  (setq mc/always-run-for-all t))

(use-package symbol-overlay
  :defer t
  :functions
  symbol-overlay-put
  symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map custom-bindings-map
              ("C-;" . symbol-overlay-put)
              ("M-N" . symbol-overlay-jump-next)
              ("M-P" . symbol-overlay-jump-previous)))

(defun ar/mc-mark-all-symbol-overlays ()
  "Mark all symbol overlays using multiple cursors."
  (interactive)
  (mc/remove-fake-cursors)
  (when-let* ((overlays (symbol-overlay-get-list 0))
              (point (point))
              (point-overlay (seq-find
                              (lambda (overlay)
                                (and (<= (overlay-start overlay) point)
                                     (<= point (overlay-end overlay))))
                              overlays))
              (offset (- point (overlay-start point-overlay))))
    (setq deactivate-mark t)
    (mapc (lambda (overlay)
            (unless (eq overlay point-overlay)
              (mc/save-excursion
               (goto-char (+ (overlay-start overlay) offset))
               (mc/create-fake-cursor-at-point))))
          overlays)
    (mc/maybe-multiple-cursors-mode)))

(define-key custom-bindings-map (kbd "C-M-;") 'ar/mc-mark-all-symbol-overlays)

(use-package undo-fu
  :defer t
  :bind (:map custom-bindings-map
              ("C-_" . undo-fu-only-undo)
              ("M-_" . undo-fu-only-redo)))

;; Undo Tree

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind
  (("C-c w u" . undo-tree-visualize)))

(use-package move-dup
  :bind (:map custom-bindings-map
              (("C-M-<up>"    . move-dup-move-lines-up)
               ("C-M-<down>"  . move-dup-move-lines-down)
               ("C-M-<left>"  . move-dup-move-lines-up)
               ("C-M-<right>" . move-dup-move-lines-down)
               ("M-W"         . move-dup-duplicate-down))))

(define-key custom-bindings-map (kbd "C-j") 'join-line)

(define-key custom-bindings-map
            (kbd "M-j")
            (lambda ()
              (interactive)
              (join-line -1)))

(define-key custom-bindings-map (kbd "C-S-k") 'kill-whole-line)

(use-package crux
  :defer t
  :bind (:map custom-bindings-map
         ("C-S-<return>" . crux-smart-open-line-above)
         ("M-S-<down>"   . crux-duplicate-current-line-or-region)))

;; Buffers and navigation 
(defun jah/kill-buffer (&optional arg)
"When called with a prefix argument -- i.e., C-u -- kill all interesting
buffers -- i.e., all buffers without a leading space in the buffer-name.
When called without a prefix argument, kill just the current buffer
-- i.e., interesting or uninteresting."
(interactive "P")
  (cond
    ((and (consp arg) (equal arg '(4)))
      (mapc
        (lambda (x)
          (let ((name (buffer-name x)))
            (unless (eq ?\s (aref name 0))
              (kill-buffer x))))
        (buffer-list)))
    (t
      (kill-buffer (current-buffer)))))

(define-key custom-bindings-map (kbd "C-c k") 'jah/kill-buffer)

(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on `split-window-sensibly', but prefers to split WINDOW side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
               (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq split-window-preferred-function 'split-window-really-sensibly)

(setq-default split-height-threshold nil
              split-width-threshold  nil
              fill-column            80) ;; Maximum line width
              ;; window-min-width       80) ;; No smaller windows than this

(use-package switch-window
  :bind (:map custom-bindings-map
              ("C-x o" . 'switch-window)
              ("C-x 1" . 'switch-window-then-maximize)
              ("C-x 2" . 'switch-window-then-split-below)
              ("C-x 3" . 'switch-window-then-split-right)
              ("C-x 0" . 'switch-window-then-delete)))

(use-package transpose-frame
  :bind (:map custom-bindings-map
              ("C-c f" . 'flop-frame)))

;; Project management 
(use-package projectile
  :defer t
  :bind (:map custom-bindings-map
              ("C-c w p" . projectile-command-map))
  :config
  (add-to-list 'projectile-project-search-path "~/Dropbox/projects/")
  (add-to-list 'projectile-project-search-path "~/Dropbox/playground/")
  (add-to-list 'projectile-project-search-path "~/Documents/writing/")
  :init
  (projectile-mode))

;; File explorer sidebar
(use-package treemacs
  :defer t
  :bind (:map custom-bindings-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-width 30
        treemacs-follow-mode t
        treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile))

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Programming"   (predicate . (derived-mode-p 'prog-mode)))
         ("Clojure/CIDER" (or (name  . "clojure")
                              (name  . "cider")
                              (name  . "nrepl")))
         ("Org"           (mode . org-mode))
         ("Dired"         (mode . dired-mode))
         ("Magit"         (name . "magit")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-never-show-predicates
      '(;; System buffers
        "^\\*Messages\\*$"
        "^\\*scratch\\*$"
        "^\\*Completions\\*$"
        "^\\*Help\\*$"
        "^\\*Apropos\\*$"
        "^\\*info\\*$"
        "^\\*Async-native-compile-log\\*$"

        ;; LSP Buffers
        "^\\*lsp-log\\*$"
        "^\\*clojure-lsp\\*$"
        "^\\*clojure-lsp::stderr\\*$"
        "^\\*ts-ls\\*$"
        "^\\*ts-ls::stderr\\*$"))

(setq ibuffer-formats
      '((mark " " (name 60 -1 :left))))

;; Dired 
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("C-<right>" . dired-find-alternate-file)
              ("C-<left>"  . dired-up-directory)
              ("C-<down>"  . dired-find-alternate-file)
              ("C-<up>"    . dired-up-directory)
              ("c"         . dired-create-empty-file))
  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq dired-use-ls-dired nil)))

(put 'dired-find-alternate-file 'disabled nil) ; disables warning
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Completion 
(defun jah/take-me-home ()
  (interactive)
  (if (looking-back "/" nil)
      (progn (call-interactively 'delete-minibuffer-contents) (insert "~/"))
    (call-interactively 'self-insert-command)))

(use-package vertico
  :defer t
  :bind (:map vertico-map ("~" . jah/take-me-home))
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (setq read-extended-command-predicate       'command-completion-default-include-p
        vertico-count                         32  ; Show more candidates
        read-file-name-completion-ignore-case t   ; Ignore case of file names
        read-buffer-completion-ignore-case    t   ; Ignore case in buffer completion
        completion-ignore-case                t)) ; Ignore case in completion

(use-package vertico-posframe
  :init
  (setq vertico-posframe-parameters   '((left-fringe  . 12)    ;; Fringes
                                        (right-fringe . 12)
                                        (undecorated  . nil))) ;; Rounded frame
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width        96                       ;; Narrow frame
        vertico-posframe-height       vertico-count            ;; Default height
        ;; Don't create posframe for these commands
        vertico-multiform-commands    '((consult-line    (:not posframe))
                                        (consult-ripgrep (:not posframe)))))

(use-package corfu
  :defer t
  :custom
  (corfu-auto          t)
  (corfu-auto-delay    0.1)
  (corfu-auto-prefix   1)
  (corfu-cycle         t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :hook (org-mode . (lambda () (corfu-mode -1))))

(setq tab-always-indent 'complete)

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package eat :ensure t)

;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main")
;;   ; :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("C-<tab>" . copilot-accept-completion))
;;   :config (setq copilot-indent-offset-warning-disable t))

;; Search 
(use-package ripgrep
  :defer t)

(use-package rg
  :defer t)

(use-package ag
  :defer t)

(use-package wgrep
  :defer t)

(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

;; Consult 
(use-package consult
  :bind (:map custom-bindings-map
              ("C-s"     . consult-line)
              ("C-M-s"   . consult-ripgrep)
              ("C-x b"   . consult-buffer)
              ("C-x C-b" . consult-buffer)
              ("M-g g"   . consult-goto-line)
              ("M-g t"   . consult-imenu)
              ("M-g a"   . consult-imenu-multi)))

;; Imenu 
(use-package imenu-list
  :defer t
  :bind (:map custom-bindings-map
              ("M-g i" . imenu-list-smart-toggle)))

;; Marginalia
(use-package marginalia
  :init 
  (marginalia-mode 1))

;; embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Misc 
(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package magit
  :defer t
  :bind (:map magit-mode-map
              ("C-M-f" . magit-section-forward)
              ("C-M-b" . magit-section-backward))
  :bind (:map custom-bindings-map
              ("M-g b" . magit-blame-addition))
  :hook
  ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq magit-mode-quit-window 'magit-restore-window-configuration
		magit-auto-revert-mode t)
  ; Remove tags from status buffer headings to speed up refresh slightly
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))

(use-package forge
  :after magit)

(use-package blamer
  :after magit
  :bind (("C-c g i" . blamer-show-commit-info)
         ("C-c g b" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time                 0.3)
  (blamer-min-offset                4)
  (blamer-max-commit-message-length 100)
  (blamer-datetime-formatter        "[%s]")
  (blamer-commit-formatter          " ● %s")
  :custom-face
  (blamer-face ((t :foreground "#7aa2cf"
                    :background nil
                    :height 1
                    :italic nil))))

(use-package git-link
  :defer t
  :init
  (setq git-link-use-commit t
        git-link-open-in-browser t))

(use-package git-timemachine
  :defer t)

;; Try packages 
(use-package try)

;; Snippets 
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  (yas-global-mode 1)) ;; or M-x yas-reload-all if you've started YASnippet already.

;; Silences the warning when running a snippet with backticks (runs a command in the snippet)
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change)) 

;; Better buffers 
(use-package helpful
  :bind (:map custom-bindings-map
			  ("C-h f" . #'helpful-function)
			  ("C-h v" . #'helpful-variable)
			  ("C-h k" . #'helpful-key)
			  ("C-h x" . #'helpful-command)
			  ("C-h d" . #'helpful-at-point)
			  ("C-h c" . #'helpful-callable)))

(use-package which-key
  :config
  (which-key-mode))

;; Spellcheck 
;; (use-package jinx
;;   :hook (emacs-startup . global-jinx-mode)
;;   :bind (("M-$"   . jinx-correct)
;;          ("C-M-$" . jinx-languages))
;;   :config
;;   (setq jinx-languages "en_GB"))

;; LaTeX 
(use-package auctex
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode)
  (LaTeX-mode . reftex-mode)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . olivetti-mode))

(setq reftex-toc-split-windows-horizontally t
	  reftex-toc-split-windows-fraction     0.2)

;; PDF tools 
(use-package pdf-tools
  :defer t
  :init (pdf-loader-install)
  :hook ((pdf-view-mode . (lambda () (auto-revert-mode -1)))
         (pdf-view-mode . (lambda () (company-mode -1))))
  :bind (:map pdf-view-mode-map
              ("C-s"   . isearch-forward)
              ("C-M-s" . pdf-occur)))

(use-package doc-view
  :hook (doc-view-mode . (lambda ()
                           (display-warning
                            emacs
                            "Oops, using DocView instead of PDF Tools!"
                            :warning))))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;; epub reading 
(use-package nov
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Editor config
(use-package editorconfig
  :defer t)

;; Feed reading 
(use-package elfeed
  :bind (:map custom-bindings-map ("C-x w" . elfeed))
  :config
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"
        "https://deniskyashif.com/index.xml"
        "https://sarahendren.com/feed.xml"
        "https://robinrendle.com/feed.xml"
        "https://www.citationneeded.news/rss/"
        "https://rss.beehiiv.com/feeds/UTUG4cosDb.xml"
        "https://placing.technology/feed.rss"
        "https://jasonsantamaria.com/feed.atom"
        "https://aworkinglibrary.com/feed/index.xml"
        "https://www.miriamsuzanne.com/feed.xml"
        "https://www.wrecka.ge/rss/"
        "https://chriscoyier.net/feed/"
        "https://jenschuetz.com/feed.xml"
        "https://henry.codes/rss/writing.xml"
        "https://adactio.com/journal/rss"
        "https://brilliantcrank.com/rss/"
        "https://rachsmith.com/feed.xml"
        "https://ericwbailey.website/feed/feed.xml"
        "https://lucybellwood.com/feed/"
        "https://www.anildash.com/feed.xml"
        "https://markllobrera.com/feed/feed.xml"
        "https://nazhamid.com/feed.xml"
        "https://simplebits.com/notebook/rss/"
        "https://maya.land/feed.xml"
        "https://chrisglass.com/links/feed/"
        "https://wordridden.com/rss"
        "https://feeds.feedburner.com/robweychert"
        "https://melanie-richards.com/feed.xml"
        "https://lynnandtonic.com/feed.xml"
        "https://daverupert.com/atom.xml"
        "https://phirephoenix.com/feed.xml"
        "https://www.xorph.com/nfd/feed/"
        "https://www.susanjeanrobertson.com/feed-journal.xml"
        "https://www.rizwanakhan.com/prose"
        "https://ethanmarcotte.com/wrote/feed.xml")))

;; Config profiling 
(use-package esup
  :defer t
  :config
  (setq esup-depth 0))

;; Org 
(use-package org
  :defer t
  :init
  (setq org-archive-location "~/Documents/org/archive.org::datetree/")
  (setq org-clock-persist t)
  (setq org-clock-auto-clock-resolution t)
  (setq org-startup-folded 'nofold)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-idle-time 5)
  (setq org-clock-in-resume t)
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t) ; hide spam

  ;; Visuals 
  ;;:hook (org-mode . olivetti-mode) -- not sure I like this yet

  :config
  ;; Resize Org headings
  (custom-set-faces
   '(org-document-title ((t (:height 1.2))))
   '(outline-1          ((t (:height 1.2))))
   '(outline-2          ((t (:height 1.2))))
   '(outline-3          ((t (:height 1.2))))
   '(outline-4          ((t (:height 1.2))))
   '(outline-5          ((t (:height 1.2))))
   '(outline-6          ((t (:height 1.2))))
   '(outline-7          ((t (:height 1.2))))
   '(outline-8          ((t (:height 1.2))))
   '(outline-9          ((t (:height 1.2)))))
  
(org-indent-mode -1)

(setq org-startup-with-latex-preview t)

(plist-put org-format-latex-options :scale 1.35)

(let ((png (cdr (assoc 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter '("dvipng -D %D -T tight -bg Transparent -o %O %F")))

(setq org-startup-folded 'content)

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-pretty-entities t
      org-ellipsis "  ·")

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(setq org-log-done                       t
      org-auto-align-tags                t
      org-tags-column                    -80
      org-fold-catch-invisible-edits     'show-and-error
      org-special-ctrl-a/e               t
      org-insert-heading-respect-content t)

)

(use-package org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config 
  (setq org-hide-emphasis-markers t)  ;; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ;; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t))   ;; Show links

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("◆" "◇" "•" "⚬" "●" "○"))
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO"     . 9744)
                                          ("PROG"     . 9744)
                                          ("NEXT"     . 9744)
                                          ("WAIT"     . 9744)
                                          ("DROP"     . 9744)
                                          ("QUESTION" . 9744)
                                          ("DONE"     . 9745)))
  :hook (org-mode . org-superstar-mode))

;; General 
(add-hook 'org-mode-hook #'(lambda () (electric-indent-local-mode -1)))

(setq org-link-frame-setup
      '((vm      . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus    . org-gnus-no-new-news)
        (file    . find-file)
        (wl      . wl-other-frame)))

(setq org-return-follows-link t)

(setq org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil)))

;; Agenda 
(setq org-agenda-start-on-weekday nil
      org-agenda-block-separator  nil
      org-agenda-remove-tags      t)

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-prefix "\n❯ ")
  ;; Hide the thin width char glyph
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))
  (org-super-agenda-mode))

(use-package org-ql
  :after org
  :config
  (when (boundp 'org-ql-views)
    (add-to-list 'org-ql-views
                 '("Questions" :buffers-files org-agenda-files :query
                   (and
                    (not
                     (done))
                    (todo "QUESTION"))
                   :sort
                   (todo priority date)
                   :super-groups org-super-agenda-groups :title "Agenda-like"))))

;; Delete default agenda commands
(setq org-agenda-custom-commands nil)

(defvar regular-view-groups
  '((:name "Scheduled"
     :scheduled t
     :order 1)
	(:name "Deadlines"
     :deadline t
     :order 2)))

(add-to-list 'org-agenda-custom-commands
	  '("d" "Day View"
		 ((agenda "" ((org-agenda-overriding-header "Day View")
                      (org-agenda-span 'day)
                      (org-super-agenda-groups regular-view-groups)))
		  (org-ql-block '(todo "PROG") ((org-ql-block-header "\n❯ In Progress")))
		  (org-ql-block '(todo "NEXT") ((org-ql-block-header "\n❯ Next Up")))
          (org-ql-block '(todo "WAIT") ((org-ql-block-header "\n❯ Backlog")))
		  (org-ql-block '(priority "A") ((org-ql-block-header "\n❯ Important"))))))


(add-to-list 'org-agenda-custom-commands
		'("e" "Three-Day View"
               ((agenda "" ((org-agenda-span 3)
                            (org-agenda-start-on-weekday nil)
                            (org-deadline-warning-days 0))))))

(setq org-agenda-skip-deadline-if-done  t
	  org-agenda-skip-scheduled-if-done t)

(setq org-agenda-deadline-leaders '("Deadline:  " "In %2d d.: " "%2d d. ago: "))

;; Tasks 
(setq org-lowest-priority  ?F) ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "#BF616A")
        (66 . "#EBCB8B")
        (67 . "#B48EAD")
        (68 . "#81A1C1")
        (69 . "#5E81AC")
        (70 . "#4C566A")))

(setq org-todo-keywords
      '((sequence
         ;; Needs further action
		 "TODO(t)" "PROG(p)" "NEXT(n)" "WAIT(w)" "QUESTION(q) DROP(x)"
		 "|"
         ;; Needs no action currently
		 "DONE(d)")))

(defun org-mark-as-done ()
  (interactive)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
                            ;; below target heading
    (cond ((looking-at "\*+ TODO")
           (org-todo "DONE"))
		  ((looking-at "\*+ NEXT")
           (org-todo "DONE"))
          ((looking-at "\*+ WAIT")
           (org-todo "DONE"))
		  ((looking-at "\*+ PROG")
           (org-todo "DONE"))
		  ((looking-at "\*+ DROP")
           (org-todo "DONE"))
		  ((looking-at "\*+ DROP")
           (org-todo "QUESTION"))
		  ((looking-at "\*+ DONE")
           (org-todo "DONE"))
          (t (message "Undefined TODO state.")))))

(define-key custom-bindings-map (kbd "C-c d") 'org-mark-as-done)

(use-package hide-lines
  :vc (:url "https://github.com/vapniks/hide-lines.git"
       :branch "master"
       :rev :newest))

(defun hide-done-tasks ()
  (interactive)
  (hide-lines-matching "* DONE"))

(defun show-done-tasks ()
  (interactive)
  (hide-lines-show-all))

(setq org-directory "~/Documents/org/")
(setq org-agenda-files (list org-directory))

(setq org-archive-location (concat org-directory "archive.org::"))

;;(setq org-capture-templates
;;       `(("i" "Inbox" entry  (file "inbox.org")
;;        ,(concat "* TODO %?\n"
 ;;                "/Entered on/ %U"))))
(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(define-key custom-bindings-map (kbd "C-c l") 'org-store-link)
(define-key custom-bindings-map (kbd "C-c a") 'org-agenda)
(define-key custom-bindings-map (kbd "C-c c") 'org-capture)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c t") 'org-todo)
  (define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in))

(set-register ?i (cons 'file (concat org-directory "inbox.org")))
(set-register ?w (cons 'file (concat org-directory "work.org")))

(define-key custom-bindings-map (kbd "C-r") 'jump-to-register)

;; Babel 
(setq org-export-use-babel       nil
      org-confirm-babel-evaluate nil)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python     . t)
;;    (haskell    . t)
;;    (clojure    . t)))

(use-package ob-python
  :ensure nil
  :after (ob python)
  :config
  (setq org-babel-python-command python-shell-interpreter))

;; TESTING: org-node
(use-package org-mem
  :defer
  :config
  (setq org-mem-do-sync-with-org-id t)
  (setq org-mem-watch-dirs
        (list "~/Documents/notes/"))
  (org-mem-updater-mode))

(use-package org-node
  :after org
  :init
  ;; Optional key bindings
  (keymap-set global-map "M-o n" org-node-global-prefix-map)
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-o n" org-node-org-prefix-map))
  :config
  (org-node-cache-mode)
  (org-node-backlink-mode)
  (setq org-node-backlink-aggressive t)
  (org-node-complete-at-point-mode)
  (add-hook 'after-save-hook 'org-node-rename-file-by-title)
  ;; Use regular capture instead of roam capture since we're migrating away from roam
  (setq org-node-creation-fn #'org-capture)
  (setq org-node-file-slug-fn #'org-node-slugify-like-roam-default)
  (setq org-node-file-timestamp-format "%Y%m%d%H%M%S-")
  (setq org-node-directory "~/Documents/notes"))

;; Org Capture Templates
(setq org-capture-templates
      '(("t" "Tasks" entry
     (file+headline "" "Inbox")
     "* TODO %?\n %U")
        ("g" "General Source" plain
         (function org-node-capture-target)
         "#+title: %^{Source Title}
#+filetags: :project/sagebrush:primary-source:
:PROPERTIES:
:ID:       %(org-id-new)
:DATE_CREATED: %U
:DATE:     %^{Document Date}
:SOURCE_TYPE: %^{Source Type|newspaper|book|report|article|interview|speech|memo|minutes}
:PUBLICATION: %^{Publication/Publisher}
:CREATOR:  %^{Author/Creator}
:ARCHIVE:  %^{Archive}
:COLLECTION: %^{Collection}
:BOX:      %^{Box}
:FOLDER:   %^{Folder}
:END:

* Source Information
- Source: %\\1, %\\2, %\\3
- Tropy: %^{Tropy Link (optional)}

* Content Summary
%^{Brief summary of what this is (who, what, where, when)}

* Key Excerpts


* Themes


* People


* Organizations


* Places


* Analysis


* Cross-references


* Notes
%^{Initial thoughts/questions}

* Transcription
** TODO Needs transcription"
         :empty-lines 1)

        ("p" "Person" plain
         (function org-node-capture-target)
         "#+title: %^{Full Name}
#+filetags: :project/sagebrush:biography:
:PROPERTIES:
:ID:       %(org-id-new)
:DATE_CREATED: %U
:BIRTH_YEAR: %^{Birth Year (if known)}
:DEATH_YEAR: %^{Death Year (if known)}
:OCCUPATION: %^{Primary Occupation}
:LOCATION:  %^{Primary Location/Region}
:END:

* Biographical Information
%^{Basic biographical details}

* Role in Research
%^{Why this person matters to your project}

* Key Relationships
** Family


** Professional


** Political/Social


* Timeline
** Major Events


* Sources Mentioning This Person
%^{Initial sources - more will be added via backlinks}

* Analysis
%^{Your interpretation of this person's significance}

* Research Questions
%^{What you want to know more about}"
         :empty-lines 1)

        ("l" "Letter" plain
         (function org-node-capture-target)
         "#+title: Letter: %^{Sender} to %^{Recipient} - %^{Date}
#+filetags: :project/sagebrush:primary-source:
:PROPERTIES:
:ID:       %(org-id-new)
:DATE_CREATED: %U
:DATE:     %\\3
:SOURCE_TYPE: letter
:LETTER_TYPE: %^{Type|personal|business|official}
:SENDER:   %\\1
:RECIPIENT: %\\2
:ARCHIVE:  %^{Archive}
:COLLECTION: %^{Collection}
:BOX:      %^{Box}
:FOLDER:   %^{Folder}
:END:

* Source Information
- Source: Letter from %\\1 to %\\2, %\\3
- Tropy: %^{Tropy Link (optional)}

* Content Summary
%^{Brief summary of letter content}

* Key Excerpts


* Themes


* People
- [[%\\1]] (sender)
- [[%\\2]] (recipient)

* Organizations


* Places


* Analysis


* Cross-references


* Notes
%^{Initial thoughts/questions}

* Transcription
** TODO Needs transcription"
         :empty-lines 1)

        ("o" "Organization" plain
         (function org-node-capture-target)
         "#+title: %^{Organization Name}
#+filetags: :project/sagebrush:organization:
:PROPERTIES:
:ID:       %(org-id-new)
:DATE_CREATED: %U
:FOUNDED:  %^{Founded Year (if known)}
:DISSOLVED: %^{Dissolved Year (if applicable)}
:ORG_TYPE: %^{Type|government|business|advocacy|professional|religious|social}
:LOCATION: %^{Headquarters/Primary Location}
:END:

* Overview
%^{Brief description of the organization}

* Role in Research
%^{Why this organization matters to your project}

* Key Leadership
** Founders


** Important Leaders/Members


* Organizational Structure
%^{How was it organized? Departments, branches, etc.}

* Goals and Activities
** Stated Mission


** Primary Activities


** Political/Social Positions


* Timeline
** Major Events/Milestones


* Sources Mentioning This Organization
%^{Initial sources - more will be added via backlinks}

* Analysis
%^{Your interpretation of this organization's significance}

* Research Questions
%^{What you want to know more about}"
         :empty-lines 1)))


;; Roam - DISABLED FOR ORG-NODE MIGRATION
;; (use-package org-roam
;;   :after org
;;   :hook (org-roam-mode . org-roam-db-autosync-mode)
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/Documents/notes")
;;   (org-roam-completion-everywhere t)
;;   CAPTURE TEMPLATES MOVED TO SEPARATE ORG-CAPTURE-TEMPLATES SECTION ABOVE
;;   (org-roam-capture-templates - REMOVED
;;       TEMPLATES REMOVED - SEE ORG-CAPTURE-TEMPLATES ABOVE)
;;          :unnarrowed t))
;;    )
;;   :config
;;   ;; Sync my Org Roam database automatically
;;   (org-roam-db-autosync-enable)
;;   (org-roam-db-autosync-mode)
;;   ;; Open Org files in same window
;;   (add-to-list 'org-link-frame-setup '(file . find-file)))

;; DISABLED FOR ORG-NODE MIGRATION
;; (use-package consult-org-roam
;;    :ensure t
;;    :after org-roam
;;    :init
;;    (require 'consult-org-roam)
;;    ;; Activate the minor mode
;;    (consult-org-roam-mode 1)
;;    :custom
;;    ;; Use `ripgrep' for searching with `consult-org-roam-search'
;;    (consult-org-roam-grep-func #'consult-ripgrep)
;;    ;; Configure a custom narrow key for `consult-buffer'
;;    (consult-org-roam-buffer-narrow-key ?r)
;;    ;; Display org-roam buffers right after non-org-roam buffers
;;    ;; in consult-buffer (and not down at the bottom)
;;    (consult-org-roam-buffer-after-buffers t)
;;    :config
;;    ;; Eventually suppress previewing for certain functions
;;    (consult-customize
;;     consult-org-roam-forward-links
;;     :preview-key "M-.")
;; )

;; (setq org-roam-node-display-template
;;       (concat "${title:*} "
;;         (propertize "${tags:10}" 'face 'org-tag)))

;; (use-package org-roam-ui
;;     :after org-roam
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; org conveniences 
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-t" . org-download-screenshot)
         ("s-y" . org-download-clipboard))))

;; org-transclusion
(use-package org-transclusion
  :after org
  :config
  (set-face-background 'org-transclusion "#222"))
(define-key global-map (kbd "C-c t a") #'org-transclusion-add)
(define-key global-map (kbd "C-c t m") #'org-transclusion-mode)

(use-package toc-org
  :after org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode))

;; Citar to access bibliographies
(use-package citar
   :custom
   (citar-bibliography '("~/Documents/Academia/library.bib"))
   (citar-notes-paths '("~/Documents/notes/"))
   (citar-open-always-create-notes t)
   (org-cite-insert-processor 'citar)
   (org-cite-follow-processor 'citar)
   (org-cite-activate-processor 'citar)
   :bind (:map org-mode-map
          ("C-c w b k" . org-cite-insert)))

;; org-roam-bibtex - DISABLED FOR ORG-NODE MIGRATION
;; (use-package org-roam-bibtex
;;   :after (org-roam citar)
;;   :config (org-roam-bibtex-mode 1))

;; Programming 
(use-package evil-nerd-commenter
  :defer t
  :bind (:map custom-bindings-map ("C-0" . evilnc-comment-or-uncomment-lines)))

(add-hook 'prog-mode-hook 'subword-mode)

(electric-pair-mode 1)

(use-package hl-todo
    :hook (prog-mode . hl-todo-mode)
    :config
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       font-lock-keyword-face  bold)
            ("FIXME"      error                   bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face  bold)
            ("NOTE"       success                 bold)
            ("DEPRECATED" font-lock-doc-face      bold))))

(use-package markdown-mode
  :defer t)

;; Flycheck 
(use-package flycheck
  :defer t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c ! !" . flycheck-explain-error-at-point))
  :config (setq flycheck-display-errors-function #'ignore))

;; Code formatting 
(use-package apheleia
  :defer t
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :hook ((typescript-mode    . apheleia-mode)
         (javascript-mode    . apheleia-mode)
         (typescript-ts-mode . apheleia-mode)
         (tide-mode          . apheleia-mode))
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  ;; (apheleia-global-mode +1)
)

;; Eldoc 
(use-package eldoc
  :defer t
  :config
  (global-eldoc-mode))

;; xref 
(use-package consult-xref-stack
  :vc
  (:url "https://github.com/brett-lempereur/consult-xref-stack" :branch "main")
  :bind (:map custom-bindings-map
              ("C-," . consult-xref-stack-backward)
              ("C-." . consult-xref-stack-forward)))

;; http requests
(use-package verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; tree-sitter 
;; (use-package treesit
;;   :ensure nil
;;   :mode (("\\.mjs\\'" . typescript-ts-mode)
;;          ("\\.mts\\'" . typescript-ts-mode)
;;          ("\\.cjs\\'" . typescript-ts-mode)
;;          ("\\.json\\'" .  json-ts-mode)
;;          ("\\.Dockerfile\\'" . dockerfile-ts-mode)
;;          ("\\.prisma\\'" . prisma-ts-mode))
;;   :preface
;;   (defun os/setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
;;                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;                (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
;;                (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;                (make "https://github.com/alemuller/tree-sitter-make")
;;                (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;                (cmake "https://github.com/uyha/tree-sitter-cmake")
;;                (c "https://github.com/tree-sitter/tree-sitter-c")
;;                (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;                (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
;;                (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))
;;
;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
;;   ;; distinct from their ordinary counterparts.
;;   ;;
;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   (dolist (mapping
;;            '((Css-mode . css-ts-mode)
;;              (typescript-mode . typescript-ts-mode)
;;              (js-mode . typescript-ts-mode)
;;              (js2-mode . typescript-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (json-mode . json-ts-mode)
;;              (js-json-mode . json-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))
;;   :config
;;   (os/setup-install-grammars))

;; Strucutral editing 
(use-package paredit
  :defer t
  :commands
  paredit-forward
  paredit-backward
  paredit-forward-up
  paredit-forward-down
  paredit-backward-up
  paredit-backward-down)

(use-package lispy
  :after paredit
  :hook (lispy-mode . (lambda () (electric-pair-local-mode -1)))
  :config
  (setcdr lispy-mode-map nil)
  :bind (:map lispy-mode-map

         ;; Basic editing
         ("("  . lispy-parens)
         (")"  . lispy-right-nostring)
         ("["  . lispy-brackets)
         ("]"  . lispy-close-square)
         ("{"  . lispy-braces)
         ("}"  . lispy-close-curly)
         (";"  . lispy-comment)
         ("\"" . paredit-doublequote)

         ;; Slurping & barfing
         ("C-<right>"   . paredit-forward-slurp-sexp)
         ("C-<left>"    . paredit-forward-barf-sexp)
         ("C-<right>"   . paredit-forward-slurp-sexp)
         ("C-<left>"    . paredit-forward-barf-sexp)

         ;; LISP-friendly Emacs commands
         ("C-k"        . lispy-kill)
         ("DEL"        . paredit-backward-delete)

         ;; Navigating sexpressions
         ("C-f"   . paredit-forward)
         ("C-b"   . paredit-backward)
         ("C-M-f" . paredit-forward-up)
         ("C-M-b" . paredit-backward-up)

         ("C-M-n" . paredit-forward-down)
         ("C-M-p" . paredit-backward-down)
         ("C-M-d" . paredit-forward-down)
         ("C-M-u" . paredit-backward-up)

         ("M-a"   . lispy-beginning-of-defun)
         ("M-d"   . lispy-different) ; Toggle between beginning and start of sexp

         ;; Moving sexpressions
         ("C-M-<up>"   . lispy-move-up)
         ("C-M-<down>" . lispy-move-down)
         ("M-c"        . lispy-clone)

         ;; Manipulating sexpressions
         ("M-<up>"   . lispy-splice-sexp-killing-backward)
         ("M-<down>" . lispy-splice-sexp-killing-forward)
         ("M-s"      . lispy-splice)
         ("M-r"      . lispy-raise-sexp)
         ("M-S"      . lispy-split)
         ("M-J"      . lispy-join)
         ("M-?"      . lispy-convolute)

         ;; Misc.
         ("M-\"" . lispy-meta-doublequote)
         ("M-)"  . lispy-close-round-and-newline)
         ("M-("  . lispy-wrap-round)))

;; Combobulate
(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate"
            :rev :newest
            :branch "main")
  :custom (combobulate-key-prefix "C-c o")
  :bind (:map combobulate-key-map
              ("M-a" . combobulate-navigate-beginning-of-defun)
              ("M-e" . combobulate-navigate-end-of-defun)))

;; LSP 
(use-package lsp-mode
  :defer t
  :init (setq lsp-use-plists t)
  :hook ((lsp-mode              . lsp-enable-which-key-integration)
         (typescript-mode       . lsp)
         (tsx-ts-mode           . lsp)
         (typescript-ts-mode    . lsp)
         (web-mode              . lsp))
  :bind (:map lsp-mode-map
              ("M-<return>" . lsp-execute-code-action)
              ("C-M-."      . lsp-find-references)
              ("C-c r"      . lsp-rename))
  :config
  (setq lsp-diagnostics-provider :flycheck
        lsp-completion-provider  :none)       ;; I use corfu
  ;; Disable visual features
  (setq lsp-headerline-breadcrumb-enable nil  ;; No breadcrumbs
        lsp-lens-enable                  nil  ;; No lenses

        ;; Enable code actions in the mode line
        lsp-modeline-code-actions-enable t
        lsp-modeline-code-action-fallback-icon "✦"

        ;; Limit raising of the echo area to show docs
        lsp-signature-doc-lines 3)
  (setq lsp-file-watch-threshold  1500)
  (setq lsp-format-buffer-on-save nil)

  (with-eval-after-load 'lsp-modeline
    (set-face-attribute 'lsp-modeline-code-actions-preferred-face nil
                        :inherit font-lock-comment-face)
    (set-face-attribute 'lsp-modeline-code-actions-face nil
                        :inherit font-lock-comment-face)))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable      nil))

(use-package origami
  :defer t
  :hook (prog-mode . origami-mode)
  :bind (:map origami-mode-map
              ("C-<" . origami-toggle-node)))

(use-package lsp-origami
  :after lsp
  :hook (lsp-mode . lsp-origami-try-enable))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; Languages 
(use-package clojure-mode
  :defer t
  :bind (:map clojure-mode-map
              ("C-("     . clojure-convert-collection-to-list)
              ("C-'"     . clojure-convert-collection-to-quoted-list)
              ("<C-lsb>" . clojure-convert-collection-to-vector)
              ("C-{"     . clojure-convert-collection-to-map)
              ("C-#"     . clojure-convert-collection-to-set))
  :config
  (require 'flycheck-clj-kondo))

(defun jah/cider-eval-and-test-ns ()
  "Evaluate the current namespace, then run all tests associated with it."
  (interactive)
  (cider-eval-buffer)
  ;; Get the current namespace
  (let ((ns (cider-current-ns)))
    (when ns
      (cider-test-run-ns-tests ns))))

(use-package cider
  :defer t
  :hook ((cider-mode      . lispy-mode)
         (cider-repl-mode . lispy-mode)
         (clojure-mode    . lispy-mode)
         (clojure-mode    . whitespace-mode))
  :bind (:map cider-repl-mode-map
              ("C-l"   . cider-repl-clear-buffer))
  :bind (:map cider-mode-map
              ("C-c s" . cider-selector)
              ("C-c t" . jah/cider-eval-and-test-ns))
  :config
  (setq cider-repl-display-help-banner       nil
        clojure-toplevel-inside-comment-form t
        cider-download-java-sources          t)
  ; (def-cider-selector-method ?e
  ;   "CIDER result buffer."
  ;   cider-result-buffer)
  )

(use-package flycheck-clj-kondo
  :ensure t)

(use-package anakondo
  :ensure t
  :commands anakondo-minor-mode)

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :bind (:map cider-mode-map
              ("C-M-c" . cljr-clean-ns))
  :config
  (setq cljr-add-ns-to-blank-clj-files nil)
  (cljr-add-keybindings-with-prefix "C-<return>"))

(use-package eval-sexp-fu
  :after cider)

(use-package cider-eval-sexp-fu
  :after cider)

(defun cider-jack-in-with-profile ()
  (interactive)
  (letrec ((profile (read-string "Enter profile names (,separated): "))
           (lein-params (concat "with-profile +" profile " repl :headless")))
    (message "lein-params set to: %s" lein-params)
    (set-variable 'cider-lein-parameters lein-params)
    (cider-jack-in '())))

(defun cider-jack-in-with-portal-profile ()
  (interactive)
  (set-variable 'cider-lein-parameters "with-profile +portal repl :headless")
  (cider-jack-in '()))

(use-package dash
  :defer t)

(use-package s
  :defer t)

(use-package f
  :defer t)

(use-package eros
  :defer t
  :functions
  eros-mode
  eros-eval-defun
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eros-eval-defun)
              ("C-c C-k" . eval-buffer))
  :config
  (eros-mode 1))

(use-package python
  :interpreter ("python3" . python-mode)
  :defer t
  :config
  (setq python-shell-interpreter "python3.11")
  (add-hook 'python-mode
			 (lambda () (setq forward-sexp-function nil))))

(use-package hide-mode-line
  :defer t
  :hook (inferior-python-mode . hide-mode-line-mode))

(use-package nix-mode
  :defer t)

(use-package typescript-mode
  :defer t
  :mode (("\\.js\\'"   . typescript-mode)
         ("\\.ts\\'"   . typescript-mode)
         ("\\.jsx\\'"  . tsx-ts-mode)
         ("\\.tsx\\'"  . tsx-ts-mode))
  :hook ((typescript-mode . combobulate-mode)
         (tsx-ts-mode     . combobulate-mode))
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :defer t
  :mode "components\\/.*\\.js\\'")

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq js2-basic-offset 2))

(use-package xref-js2
  :after js2-mode
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (setq xref-js2-search-program 'rg)
  (define-key js2-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key js2-mode-map (kbd "M-,") 'xref-pop-marker-stack))

;; Go development
(use-package go-mode
  :defer t
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save))
  :config
  (setq gofmt-command "gofmt")
  (setq tab-width 4)
  (setq go-tab-width 4))

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode))

(use-package gorepl-mode
  :defer t
  :hook (go-mode . gorepl-mode))

;; Go testing support
(defun jah/go-run-tests ()
  "Run go tests for current package."
  (interactive)
  (if (string-match "go" (file-name-extension (buffer-file-name)))
      (save-buffer))
  (shell-command "cd . && go test"))

(defun jah/go-run-package ()
  "Run current Go package."
  (interactive)
  (shell-command (concat "cd " (file-name-directory (buffer-file-name)) " && go run .")))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-t") 'jah/go-run-tests)
  (define-key go-mode-map (kbd "C-c C-r") 'jah/go-run-package))

;; Writing tools
(defun jah/org-count-words ()
    "Add word count to each heading property drawer in an Org mode buffer."
    (interactive)
    (org-map-entries
     (lambda ()
       (let* ((start (point))
              (end (save-excursion (org-end-of-subtree)))
              (word-count (count-words start end)))
         (org-set-property "wordcount" (number-to-string word-count))
         (unless (org-entry-get nil "target")
           (org-set-property "target" "0"))))))

(defun ews-org-insert-notes-drawer ()
    "Generate or open a NOTES drawer under the current heading."
    (interactive)
    (push-mark)
    (org-previous-visible-heading 1)
    (next-line)
    (org-beginning-of-line)
    (if (looking-at-p "^[ \t]*:NOTES:")
        (progn
          (org-fold-hide-drawer-toggle 'off)
          (re-search-forward "^[ \t]*:END:" nil t)
          (previous-line)
          (org-end-of-line)
          (org-return))
      (org-insert-drawer nil "NOTES"))
    (org-unlogged-message "Press <C-u C-SPACE> to return to the previous position."))

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c C-x n") #'ews-org-insert-notes-drawer))

;; Custom keybinds 
(define-key custom-bindings-map (kbd "M-o") 'other-window)
(define-key custom-bindings-map (kbd "C-c b") 'ibuffer)
(define-key custom-bindings-map (kbd "C-f") 'forward-sexp)
(define-key custom-bindings-map (kbd "C-b") 'backward-sexp)
(define-key custom-bindings-map (kbd "C-c u") 'revert-buffer)

;; Copy/paste with macOS bindings
(define-key custom-bindings-map (kbd "M-c") 'clipboard-kill-ring-save)

;; Hyperlink insertion function for org-mode
(defun jah/org-insert-link-as-title ()
  "Insert an org link where both the link and title are the same URL."
  (interactive)
  (let ((url (read-string "URL: ")))
    (when (and url (not (string-empty-p url)))
      (insert (format "[[%s][%s]]" url url)))))

(define-key custom-bindings-map (kbd "C-c h l") 'jah/org-insert-link-as-title)

;; Writing workflow keybindings under C-c w - MIGRATED TO ORG-NODE
;; Note: Some consult-org-roam functions don't have direct org-node equivalents
;; Using basic alternatives for now
(define-key custom-bindings-map (kbd "C-c w d f") 'org-node-find)
;; (define-key custom-bindings-map (kbd "C-c w d s") 'consult-org-roam-search)  ; No direct equivalent yet
;; (define-key custom-bindings-map (kbd "C-c w d b") 'consult-org-roam-backlinks)  ; No direct equivalent yet
;; (define-key custom-bindings-map (kbd "C-c w d l") 'consult-org-roam-forward-links)  ; No direct equivalent yet
;; (define-key custom-bindings-map (kbd "C-c w d t") 'org-roam-buffer-toggle)  ; Use org-node-backlink-mode instead

;; Citar bibliography keybindings
(define-key custom-bindings-map (kbd "C-c w b c") 'citar-create-note)
(define-key custom-bindings-map (kbd "C-c w b n") 'citar-open-notes)
(define-key custom-bindings-map (kbd "C-c w b x") 'citar-insert-citation)
(define-key custom-bindings-map (kbd "C-c w b d") 'citar-dwim)
(define-key custom-bindings-map (kbd "C-c w b e") 'citar-open)

;; Org node operations - MIGRATED FROM ORG-ROAM
(define-key custom-bindings-map (kbd "C-c w r f") 'org-node-find)
(define-key custom-bindings-map (kbd "C-c w r i") 'org-node-insert-link)
;; (define-key custom-bindings-map (kbd "C-c w r t") 'org-roam-tag-add)  ; No direct equivalent in org-node

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eval-sexp-fu-flash ((t (:background "#D9EDFC"))))
 '(org-document-title ((t (:height 1.2))))
 '(outline-1 ((t (:height 1.2))))
 '(outline-2 ((t (:height 1.2))))
 '(outline-3 ((t (:height 1.2))))
 '(outline-4 ((t (:height 1.2))))
 '(outline-5 ((t (:height 1.2))))
 '(outline-6 ((t (:height 1.2))))
 '(outline-7 ((t (:height 1.2))))
 '(outline-8 ((t (:height 1.2))))
 '(outline-9 ((t (:height 1.2)))))

;;; Backups
(setopt
 ;; Put them in the unusual path /home/backups/ to avoid cluttering rg output.
 backup-directory-alist `(("." . "/Users/jheppler/backups"))
 delete-old-versions t ;; nil led to Emacs looking broken for newbie-me
 vc-make-backup-files t ;; I don't commit regularly in every project
 version-control t)

;; Graceful degradation
(unless (file-writable-p "/Users/jheppler/backups/")
  (error "Disabling backups because can't write to: /Users/jheppler/backups/")
  (setq backup-directory-alist nil)
  (setq make-backup-files nil))

;; Lesson learned
(add-hook 'after-save-hook #'my-fix-invalid-backup-settings)

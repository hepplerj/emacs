;;; init.el

;; Author: Jason Heppler <jason@jasonheppler.org>
;;
;; Init file originally based off the Emacs Writing
;; Studio init file: https://lucidmanager.org/tags/emacs

;; Emacs 29 available?

(when (< emacs-major-version 29)
  (error "Requires version 29 or later"))

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name
			     "custom.el"
			     user-emacs-directory))

(load custom-file :no-error-if-file-is-missing)

;; Nano theme configuration
(use-package nano-theme
  :config
  ;; Set fonts before loading nano-theme
  (set-face-attribute 'default nil :font "Berkeley Mono" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "Berkeley Mono")
  (set-face-attribute 'variable-pitch nil :font "Berkeley Mono")
  ;; Enable nano-theme features
  (nano-light))


;; Bind key for customising variables

(keymap-global-set "C-c w v" 'customize-variable)

;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure nil)
  (package-native-compile t)
  (warning-minimum-level :emergency))

;; Load EWS functions -- specific to Emacs writing studio

(load-file (concat (file-name-as-directory user-emacs-directory)
		   "ews.el"))

;; Check for missing external software

(ews-missing-executables
 '(("gs" "mutool")
   "pdftotext"
   "soffice"
   "zip"
   "ddjvu"
   "curl"
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc") 
   ("grep" "ripgrep")
   ("convert" "gm")
   "dvipng"
   "latex"
   "aspell"
   "git"))

;;; LOOK AND FEEL

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Short answers only please

(setq-default use-short-answers t)

;; Scratch buffer settings

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+title: Time to Work\n#+subtitle: Scratch Buffer\nThe text in this buffer is not saved when exiting Emacs!\n\n"
      inhibit-startup-screen t)

;; Spacious padding

(use-package spacious-padding
  :custom
  (line-spacing 3)
  (spacious-padding-mode 1))

;; Mixed-pitch mode

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; Window management
;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; MINIBUFFER COMPLETION

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

(use-package savehist
  :init
  (savehist-mode))

;; Search for partial matches in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :init
  (marginalia-mode))

;; Helm -- mainly for buffer nav
;;(use-package 'helm)

;; Improve keyboard shortcut discoverability

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

;; Contextual menu with right mouse button

(when (display-graphic-p)
  (context-menu-mode))

;; Improved help buffers

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

;;; Text mode settings

(use-package text-mode
  :ensure
  nil
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

;; Check spelling with flyspell and aspell

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary ews-aspell-dictionaries)
  (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :config
  (ispell-set-spellchecker-params)
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

;;; Org

(use-package org
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'error)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-id-link-to-org-use-id t)
  (org-fold-catch-invisible-edits 'show))

(setq org-directory (concat (getenv "HOME") "/Documents/org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-log-done 'time)

;; Advanced queries with org-ql 
(use-package org-ql
  :after org)

;; org-transclude
(use-package org-transclude
  :after org)
(define-key global-map (kbd "C-c t a") #'org-transclusion-add)
(define-key global-map (kbd "C-c t m") #'org-transclusion-mode)

;; Show hidden emphasis markers
(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews
(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Org modern: Most features are disabled for beginning users

(use-package org-modern
  :hook
  (org-mode . org-modern-mode))
;;  :custom
;;  (org-modern-table nil)
;;  (org-modern-keyword nil)
;;  (org-modern-timestamp nil)
;;  (org-modern-priority nil)
;;  (org-modern-checkbox nil)
;;  (org-modern-tag nil)
;;  (org-modern-block-name nil)
;;  (org-modern-keyword nil)
;;  (org-modern-footnote nil)
;;  (org-modern-internal-target nil)
;;  (org-modern-radio-target nil)
;;  (org-modern-statistics nil)
;;  (org-modern-progress nil))

;; INSPIRATION

;; Doc-View

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Managing Bibliographies

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Citar to access bibliographies

;; (use-package citar
;;   :defer t
;;   :custom
;;   (citar-bibliography ews-bibtex-files)
;;   :bind
;;   (("C-c w b o" . citar-open)))

;; Read RSS feeds with Elfeed

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory (getenv "HOME"))
		 "elfeed.org"))))

;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Emacs Multimedia System

(use-package emms
  :config
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("C-c w m b" . emms-browser)
   ("C-c w m e" . emms)
   ("C-c w m p" . emms-play-playlist )
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

;; Open files with external applications

(use-package openwith
  :config
  (openwith-mode t)
  :custom
  (openwith-associations nil))

;; Fleeting notes

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-goto-interface 'outline-path-completion)
  (org-capture-templates
   '(("f" "Fleeting note"
      item
      (file+headline org-default-notes-file "Notes")
      "- %?\n  - Captured: %U")
     ("F" "Fleeting note with context"
      item
      (file+headline org-default-notes-file "Notes")
      "- %?\n  - Context: %a\n  - Captured: %U")
     ("p" "Permanent note (org-roam)" plain
      (function org-roam-node-find)
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
      :empty-lines 1
      :unnarrowed t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?\n  - Added: %U")
     ("j" "Journal entry" entry
      (file+olp+datetree org-default-notes-file "Journal")
      "* %?\n  - Captured: %U")
     ("r" "Reading note" item
      (file+headline org-default-notes-file "Reading")
      "- %?\n  - Source: %^{Source}\n  - Captured: %U"))))

;; Org-Roam 
(use-package org-roam
  :defer t
  :custom
  (org-roam-directory "~/Documents/notes/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
      :unnarrowed t)

     ("a" "archival-item" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+FILETAGS: :project/sagebrush:source:\n:PROPERTIES:\n:CREATOR: \n:PUBLISHER: \n:DATE: \n:ARCHIVE_NAME: \n:COLLECTION: \n:SERIES: \n:BOX: \n:FOLDER: \n:CALL_NUMBER: \n:ITEM_NUMBER: \n:PEOPLE: \n:ORGANIZATIONS: \n:LOCATIONS: \n:TOPICS: \n:SOURCE_URL: \n:END:\n\n* Abstract / Overview\n\n\n* Images\n\n\n* Transcription\n\n\n* Notes\n\n")
      :unnarrowed t)

    ("p" "person" plain
        "%?"
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+TITLE: ${title}\n#+FILETAGS: :project/sagebrush:person:biography:\n:PROPERTIES:\n:NAME: \n:BIRTH_DATE: \n:DEATH_DATE: \n:OCCUPATIONS: \n:STATES_LIVED: \n:FAITH: \n:THEMES: \n:END:\n\n* Biography\n\n\n* Connections\n\n\n* Key Events\n\n\n* Relevant Documents\n\n\n* Notes\n\n")
        :unnarrowed t)

    ("e" "event" plain
        "%?"
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+TITLE: ${title}\n#+FILETAGS: :event:\n:PROPERTIES:\n:START_DATE: \n:END_DATE: \n:LOCATION: \n:PEOPLE_INVOLVED: \n:ORGANIZATIONS_INVOLVED: \n:TOPICS: \n:END:\n\n* Description\n\n\n* Context\n\n\n* Significance\n\n\n* Related Notes\n\n")
        :unnarrowed t)

    ("s" "analysis-synthesis" plain
        "%?"
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+TITLE: ${title}\n#+FILETAGS: :project/sagebrush:bridge:\n:PROPERTIES:\n:DATE: %U\n:END:\n\n* Topic Summary\n\n\n* /Argument\n\n\n* Evidence\n\n\n* Connections\n\n\n* Remaining Questions/Next Steps\n\n")
        :unnarrowed t))
   )
  :config
  (org-roam-db-autosync-mode)
  ;; :bind
  (("C-c w d n" . org-roam-node-find)          ; equivalent to denote
   ("C-c w d i" . org-roam-node-insert)        ; equivalent to denote-link-or-create
   ("C-c w d f" . org-roam-node-find)          ; equivalent to consult-notes
   ("C-c w d l" . org-roam-buffer-toggle)      ; show links/backlinks
   ("C-c w d b" . org-roam-buffer-toggle)      ; equivalent to denote-find-backlink
   ("C-c w d r" . org-roam-node-rename)        ; equivalent to denote-rename-file
   ("C-c w d k" . org-roam-tag-add)            ; equivalent to denote-rename-file-keywords
   ("C-c w d d" . org-roam-dailies-goto-today))
  )
(setq org-adapt-indentation t)

;; Org-roam UI enhancements
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Org-roam queries
(use-package org-roam-ql
  :after (org-roam)
  :bind ((:map org-roam-mode-map
               ;; Have org-roam-ql's transient available in org-roam-mode buffers
               ("v" . org-roam-ql-buffer-dispatch)
               :map minibuffer-mode-map
               ;; Be able to add titles in queries while in minibuffer.
               ;; This is similar to `org-roam-node-insert', but adds
               ;; only title as a string.
               ("C-c n i" . org-roam-ql-insert-node-title))))

;; Consult integration for org-roam
(use-package consult-org-roam
  :after (consult org-roam)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :bind
  (("C-c w d g" . consult-org-roam-search)
   ("C-c w g" . consult-grep)
   ("C-c w h" . consult-org-heading))
  :config
  (consult-org-roam-mode 1))

;; Org-roam dailies for date-based notes
(use-package org-roam-dailies
  :after org-roam
  :bind
  (("C-c w d t" . org-roam-dailies-goto-today)
   ("C-c w d y" . org-roam-dailies-goto-yesterday)
   ("C-c w d T" . org-roam-dailies-goto-tomorrow)
   ("C-c w d c" . org-roam-dailies-capture-today)))

;; Citar to access bibliographies
(use-package citar
   :custom
   (citar-bibliography '("~/Documents/Academia/library.bib"))
   (citar-notes-paths '("~/Documents/notes/"))
   (citar-open-always-create-notes t)
   (org-cite-insert-processor 'citar)
   (org-cite-follow-processor 'citar)
   (org-cite-activate-processor 'citar)
   :bind
   (("C-c w b c" . citar-create-note)
    ("C-c w b n" . citar-open-notes)
    ("C-c w b x" . citar-insert-citation)
    ("C-c w b d" . citar-dwim)
    ("C-c w b e" . citar-open)
    :map org-mode-map
    ("C-c w b k" . org-cite-insert)))

;; Optional: org-roam export for publishing
(use-package org-roam-export
  :after org-roam)

;; Custom functions to replicate some Denote-specific behavior
(defun ews-org-roam-link-description-title-case (node)
  "Convert org-roam NODE title to title case for links."
  (let ((title (org-roam-node-title node)))
    (s-titleize title)))

;; Set custom link description function
(setq org-roam-node-display-template
      (concat "${title:60} "
              (propertize "${tags:20}" 'face 'org-tag)))

;; Custom face for org-roam links (equivalent to denote-faces-link)
(custom-set-faces
 '(org-roam-link ((t (:slant italic)))))

;; Additional bindings for extracted functionality
(bind-key "C-c w d s" #'my/org-roam-extract-subtree)
(bind-key "C-c w d h" #'org-roam-extract-subtree) ; heading extraction

;; Custom functions for fleeting/permanent notes integration
(defun ews-fleeting-to-permanent ()
  "Convert current fleeting note item to a permanent org-roam note."
  (interactive)
  (let ((content (thing-at-point 'line t)))
    (when content
      (kill-whole-line)
      (org-roam-capture- :node (org-roam-node-create :title (read-string "Title: "))
                        :templates '(("d" "default" plain "%?"
                                     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                       "#+title: ${title}\n#+date: %U\n#+filetags: fleeting\n\n")
                                     :unnarrowed t))
                        :info (list :body content)))))

(defun ews-review-fleeting-notes ()
  "Open fleeting notes file for review and processing."
  (interactive)
  (find-file org-default-notes-file)
  (goto-char (point-min))
  (when (search-forward "* Notes" nil t)
    (org-show-subtree)))

(bind-key "C-c w n f" #'ews-fleeting-to-permanent)
(bind-key "C-c w n r" #'ews-review-fleeting-notes)

;; Denote

;; (use-package denote
;;   :defer t
;;   :custom
;;   (denote-sort-keywords t)
;;   (denote-link-description-function #'ews-denote-link-description-title-case)
;;   :hook
;;   (dired-mode . denote-dired-mode)
;;   :custom-face
;;   (denote-faces-link ((t (:slant italic))))
;;   :bind
;;   (("C-c w d b" . denote-find-backlink)
;;    ("C-c w d d" . denote-date)
;;    ("C-c w d l" . denote-find-link)
;;    ("C-c w d i" . denote-link-or-create)
;;    ("C-c w d k" . denote-rename-file-keywords)
;;    ("C-c w d n" . denote)
;;    ("C-c w d r" . denote-rename-file)
;;    ("C-c w d R" . denote-rename-file-using-front-matter)))
;;
;; (use-package denote-org
;;   :bind
;;   (("C-c w d h" . denote-org-link-to-heading)
;;    ("C-c w d s" . denote-org-extract-org-subtree)))


;; Consult convenience functions

(use-package consult
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-c w g" . consult-grep))
  :config
  (add-to-list 'consult-preview-allowed-hooks 'visual-line-mode))

;; Consult-Notes for easy access to notes
(use-package consult-notes
  :bind (("C-c w d f" . consult-notes)
         ("C-c w d g" . consult-notes-search-in-all-notes))
  :config
  (consult-notes-org-roam-mode 1))
;; (use-package consult-notes
;;   :commands (consult-notes
;;              consult-notes-search-in-all-notes
;;              consult-notes-org-roam-find-node
;;              consult-notes-org-roam-find-node-relation)
;;   ;; :custom
;;   ;; (consult-notes-denote-display-keywords-indicator "_")
;;   :bind
;;   (("C-c w d f" . consult-notes)
;;    ("C-c w d g" . consult-notes-search-in-all-notes))
;;   ;;:init
;;   ;;(consult-notes-denote-mode)
;;   )


(use-package org-roam-bibtex
  :after (org-roam citar)
  :config (org-roam-bibtex-mode 1))

;; Citar-Denote to manage literature notes

;; (use-package citar-denote
;;   :custom
;;   (citar-open-always-create-notes t)
;;   :init
;;   (citar-denote-mode)
;;   :bind
;;   (("C-c w b c" . citar-create-note)
;;    ("C-c w b n" . citar-denote-open-note)
;;    ("C-c w b x" . citar-denote-nocite)
;;    :map org-mode-map
;;    ("C-c w b k" . citar-denote-add-citekey)
;;    ("C-c w b K" . citar-denote-remove-citekey)
;;    ("C-c w b d" . citar-denote-dwim)
;;    ("C-c w b e" . citar-denote-open-reference-entry)))

;; Explore and manage your Denote collection

;; (use-package denote-explore
;;   :bind
;;   (;; Statistics
;;    ("C-c w x c" . denote-explore-count-notes)
;;    ("C-c w x C" . denote-explore-count-keywords)
;;    ("C-c w x b" . denote-explore-barchart-keywords)
;;    ("C-c w x e" . denote-explore-barchart-filetypes)
;;    ;; Random walks
;;    ("C-c w x r" . denote-explore-random-note)
;;    ("C-c w x l" . denote-explore-random-link)
;;    ("C-c w x k" . denote-explore-random-keyword)
;;    ("C-c w x x" . denote-explore-random-regex)
;;    ;; Denote Janitor
;;    ("C-c w x d" . denote-explore-identify-duplicate-notes)
;;    ("C-c w x z" . denote-explore-zero-keywords)
;;    ("C-c w x s" . denote-explore-single-keywords)
;;    ("C-c w x o" . denote-explore-sort-keywords)
;;    ("C-c w x w" . denote-explore-rename-keyword)
;;    ;; Visualise denote
;;    ("C-c w x n" . denote-explore-network)
;;    ("C-c w x v" . denote-explore-network-regenerate)
;;    ("C-c w x D" . denote-explore-barchart-degree)))

;; Set some Org mode shortcuts

(use-package org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

;; Distraction-free writing

(use-package olivetti
  :demand t
  :bind
  (("C-c w o" . ews-olivetti)))

;; Focus mode - dims everything except current paragraph
(use-package focus
  :bind
  (("C-c w f" . focus-mode)))

;; Undo Tree

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind
  (("C-c w u" . undo-tree-visualise)))

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-global-bibliography '("~/Documents/Academia/library.bib")
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

;; Lookup words in online dictionaries

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  (("C-c w s d" . dictionary-lookup-definition)))

(use-package powerthesaurus
  :bind
  (("C-c w s p" . powerthesaurus-transient)))

;; Writegood-Mode for weasel words, passive writing and repeated word detection

(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  :hook
  (text-mode . writegood-mode))

;; Titlecasing

(use-package titlecase
  :bind
  (("C-c w s t" . titlecase-dwim)
   ("C-c w s c" . ews-org-headings-titlecase)))

;; Abbreviations

(add-hook 'text-mode-hook 'abbrev-mode)

;; Lorem Ipsum generator

(use-package lorem-ipsum
  :custom
  (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
  :init
  (setq lorem-ipsum-sentence-separator
        (if sentence-end-double-space "  " " "))
  :bind
  (("C-c w s i" . lorem-ipsum-insert-paragraphs)))

;; ediff

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Enable Other text modes

;; Fountain mode for writing scripts

(use-package fountain-mode)

;; Markdown mode

(use-package markdown-mode)

;; PUBLICATION

;; Generic Org Export Settings

(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

;; epub export

(use-package ox-epub
  :demand t
  :init
  (require 'ox-org))

;; LaTeX PDF Export settings

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; EWS paperback configuration

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("ews"
     "\\documentclass[11pt, twoside, hidelinks]{memoir}
      \\setstocksize{9.25in}{7.5in}
      \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
      \\setlrmarginsandblock{1.5in}{1in}{*} 
      \\setulmarginsandblock{1in}{1.5in}{*}
      \\checkandfixthelayout
      \\layout
      \\setcounter{tocdepth}{0}
      \\setsecnumdepth{subsection}
      \\renewcommand{\\baselinestretch}{1.2}
      \\setheadfoot{0.5in}{0.75in}
      \\setlength{\\footskip}{0.8in}
      \\chapterstyle{bianchi}
      \\renewcommand{\\beforechapskip}{-30pt}
      \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
      \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
      \\setsubsubsecheadstyle{\\normalfont\\centering}
      \\pagestyle{myheadings}
      \\usepackage[font={small, it}]{caption}
      \\usepackage{ccicons}
      \\usepackage{ebgaramond}
      \\usepackage[authoryear]{natbib}
      \\bibliographystyle{apalike}
      \\usepackage{svg}
      \\hyphenation{mini-buffer}
      \\renewcommand{\\LaTeX}{LaTeX}
      \\renewcommand{\\TeX}{TeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; Bind org agenda command and custom agenda
(use-package org
    :custom
    (org-agenda-files '("~/Documents/notes/" "~/Documents/org/"))
    (org-agenda-custom-commands
     '(("e" "Agenda, next actions and waiting"
        ((agenda "" ((org-agenda-overriding-header "Next three days:")
                     (org-agenda-span 3)
                     (org-agenda-start-on-weekday nil)))
         (todo "NEXT" ((org-agenda-overriding-header "Next Actions:")))
         (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))))
       ("r" . "Research Notes")
       ("rpa" "People alphabetically"
        tags "+biography"
        ((org-agenda-overriding-header "People A-Z:")
         (org-agenda-sorting-strategy '(alpha-up))
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottop))))
       ("rt" "Themes and synthesis"
        tags "+bridge"
        ((org-agenda-overriding-header "Analysis & Synthesis Notes:")
         (org-agenda-sorting-strategy '(time-up))
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottop))))))
    :bind
    (("C-c a" . org-agenda)))

;; Set your Org-roam directory
(setq org-roam-directory "~/Documents/notes")

(defvar jh/org-roam-source-notes-cache nil)
(defvar jh/org-roam-source-notes-cache-time nil)
(defvar jh/org-roam-source-notes-cache-ttl 300) ;; cache for 5 minutes
(defvar jh/org-roam-source-notes-current-filter "")

(defun jh/refresh-org-roam-source-notes-cache ()
  "Refresh the cached list of org-roam source notes."
  (interactive)
  (message "Refreshing org-roam source notes cache...")
  (let* ((notes-dir org-roam-directory)
         (files (directory-files-recursively notes-dir "\\.org$"))
         (results '()))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((title (progn
                        (goto-char (point-min))
                        (when (re-search-forward "^#\\+TITLE: *\\(.*\\)" nil t)
                          (match-string 1))))
               (date (progn
                       (goto-char (point-min))
                       (if (re-search-forward "^:DATE: *\\(.*\\)" nil t)
                           (match-string 1)
                         "No date")))
               (filetags (progn
                           (goto-char (point-min))
                           (when (re-search-forward "^#\\+FILETAGS: *\\(.*\\)" nil t)
                             (match-string 1))))
               (source (progn
                         (when filetags
                           (when (string-match "source/\\([^:]+\\)" filetags)
                             (match-string 1 filetags))))))
          (when (and filetags
                     (string-match-p "source" filetags))
            (push (list date file title filetags source) results)))))
    (setq jh/org-roam-source-notes-cache results)
    (setq jh/org-roam-source-notes-cache-time (current-time))))

(defun jh/list-org-roam-source-notes-by-date (&optional filter)
  "List cached org-roam source notes, optionally FILTERed by subtype like 'letter' or 'newspaper'."
  (interactive
   (let* ((tags
           (delete-dups
            (cl-loop for entry in jh/org-roam-source-notes-cache
                     for filetags = (nth 3 entry)
                     when (and filetags (string-match "source/\\([^:]+\\)" filetags))
                     collect (match-string 1 filetags)))))
     (list
      (completing-read
       "Filter by source subtype (leave blank for all): "
       tags nil t))))


  ;; Use cache if stale
  (when (or (not jh/org-roam-source-notes-cache)
            (not jh/org-roam-source-notes-cache-time)
            (> (float-time (time-subtract (current-time) jh/org-roam-source-notes-cache-time))
               jh/org-roam-source-notes-cache-ttl))
    (jh/refresh-org-roam-source-notes-cache))

  (let* ((filtered
          (cl-remove-if-not
           (lambda (entry)
             (let ((tags (nth 3 entry)))
               (or (string= filter "")
                   (and tags (string-match-p (concat "source/" (regexp-quote filter)) tags)))))
           jh/org-roam-source-notes-cache))
         (sorted
          (sort filtered
                (lambda (a b)
                  (let ((d1 (car a))
                        (d2 (car b)))
                    (cond
                     ((string= d1 "No date") nil)
                     ((string= d2 "No date") t)
                     (t (string< d1 d2))))))))

    ;; Output buffer
    (with-current-buffer (get-buffer-create "*Chronological Source Notes*")
      (read-only-mode -1)
      (erase-buffer)
      (insert "* Chronological Source Notes\n\n")
      (insert (format "%-10s   %-15s   %s\n" "DATE" "SOURCE" "TITLE"))
      (insert (make-string 80 ?-) "\n")
      (dolist (entry sorted)
        (let ((date (nth 0 entry))
              (path (nth 1 entry))
              (title (nth 2 entry))
              (source (or (nth 4 entry) "unknown")))
          (insert (format "%-10s   %-15s   [[file:%s][%s]]\n" date source path title))))
      (goto-char (point-min))
      (org-mode)
      (read-only-mode 1)
      (switch-to-buffer (current-buffer)))))




(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

;; FILE MANAGEMENT

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil))

;; Hide or display hidden files

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ( "."     . dired-omit-mode))
  :custom (dired-omit-files "^\\.[a-zA-Z0-9]+"))

;; Backup files

(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              version-control t
              delete-old-versions t
              create-lockfiles nil)

;; Recent files

(use-package recentf
  :config
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 50)
  :bind
  (("C-c w r" . recentf-open)))

;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r d" . bookmark-delete))

;; Work with Obsidian 

;; (require 'obsidian)
;; ;; Location of obsidian vault
;; (setopt obsidian-directory "/Users/jheppler/research/Research")
;; ;; Default location for new notes from `obsidian-capture'
;; (setopt obsidian-inbox-directory "00. Inbox")
;; ;; Useful if you're going to be using wiki links
;; (setopt markdown-enable-wiki-links t)
;;
;; ;; Create note
;; (define-key obsidian-mode-map (kbd "C-c C-n") 'obsidian-capture)
;; ;; If you prefer you can use `obsidian-insert-wikilink'
;; (define-key obsidian-mode-map (kbd "C-c C-l") 'obsidian-insert-link)
;; ;; Open file pointed to by link at point
;; (define-key obsidian-mode-map (kbd "C-c C-o") 'obsidian-follow-link-at-point)
;; ;; Open a note from vault
;; (define-key obsidian-mode-map (kbd "C-c C-p") 'obsidian-jump)
;; ;; Follow a backlink for the current file
;; (define-key obsidian-mode-map (kbd "C-c C-b") 'obsidian-backlink-jump)
;;
;; ;; Activate obsidian mode and backlinks mode
;; (global-obsidian-mode t)
;; (obsidian-backlinks-mode t)

;; Custom key commands

;; Mimic vim's join line
(fset 'jah/join-lines
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item (quote ([14 1 backspace 32 2] 0 "%d")) arg)))

(global-set-key (kbd "C-c j") 'jah/join-lines)

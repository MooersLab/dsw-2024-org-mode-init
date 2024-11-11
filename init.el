(setq user-emacs-directory "~/e29fewpackages/")
;; alias e29f='/Applications/Emacs29.4.app/Contents/MacOS/Emacs \
;; --init-directory ~/e29fewpackages --debug-init'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org)
(setq package-enable-at-startup nil)
(use-package use-package)
(setq straight-use-package-by-default t)

(message "Finished straight package manger configuration.") 


(message "Start global configuration.") 

;; find file and got to line number
(defun find-file-at-line (file line)
  "Open FILE on LINE."
  (interactive "fFile: \nNLine: \n")
  (find-file file)
  (goto-line line))
  


;;;# Minibuffer history keybindings
;; The calling up of a previously issued command in the minibuffer with ~M-p~ saves times.
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)

;;;# switch-to-minibuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-cm" 'switch-to-minibuffer) ;; Bind to `C-c m' for minibuffer.

;;;# Bibtex configuration
(defconst blaine/bib-libraries (list "/Users/blaine/Documents/global.bib"))

;;;# Combined with emacs-mac, this gives good PDF quality for [[https://www.aidanscannell.com/post/setting-up-an-emacs-playground-on-mac/][retina display]].
(setq pdf-view-use-scaling t)


;;;# PDF default page width behavior
(setq-default pdf-view-display-size 'fit-page)

;;;# Custom key sequences.
;; (global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)


;;;# display line numbers. Need with s-l.
(global-display-line-numbers-mode)

;;;# highlight current line
(global-hl-line-mode +1)
(set-face-background hl-line-face "wheat1")
(set-face-attribute 'mode-line nil  :height 180)

;;# Shell configuration
(use-package exec-path-from-shell
  :straight t
  :init
  (setenv "SHELL" "/opt/local/bin/bash")
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))



;;;# Size of the starting Window
(setq initial-frame-alist '((top . 1)
                (left . 450)
                (width . 101)
                (height . 90)))



;; ==> adjust here
;; See this [[http://ergoemacs.org/emacs/emacs_hyper_super_keys.html][ for more information.]]
;; set keys for Apple keyboard, for emacs in OS X
;; Source http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make option key do Super.
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-function-modifier 'hyper)  ; make Fn key do Hyper. Only works on Apple produced keyboards.
(setq mac-right-command-modifier 'hyper)


(defun reload-init-e29f ()
  "Reload the init.el file for e29org. Edit the path to suite your needs."
  (interactive)
  (load-file "~/e29fewpackages/init.el"))
  
;** create-org-table-with-caption
;This interactive function prompts the user for the number of rows. columns, and the caption of the table.

(defun create-org-table-with-caption ()
"This interactive function prompts the user for the number of rows. columns, and the caption of the table."
  (interactive)
  (let ((rows (read-number "Enter the number of rows: "))
        (cols (read-number "Enter the number of columns: "))
        (label (read-string "Enter the table label: "))
        (caption (read-string "Enter the table's caption: ")))
    (insert (format "#+CAPTION: %s \\label{%s}\n" caption label))
    (insert (format "#+NAME: %s\n" label))
    (insert "|")
    (dotimes (_ cols)
      (insert "----+"))
    (insert "\n|")
    ;;(insert "|")
    (dotimes (col cols)
      (insert (format " %c |" (+ ?A col))))
    (insert "\n|")
    (dotimes (_ cols)
      (insert "----+"))
    (insert "\n")
    (dotimes (_ rows)
      (insert "|")
      (dotimes (_ cols)
        (insert "     |"))
      (insert "\n"))
    (insert "|")
    (dotimes (_ cols)
      (insert "----+"))))

; *** insert-org-captioned-figure
;
; The function prompts the user for the image file path and name, the label, and the caption.

(defun insert-org-captioned-figure ()
  "Insert a captioned figure in Org-mode."
  (interactive)
  (let ((image-name (read-string "Enter the image file path: "))
        (label (read-string "Enter the figure label: "))
        (caption (read-string "Enter the figure caption: ")))
    (insert (format "#+CAPTION: %s \\label{%s}\n" caption label))
    (insert (format "#+NAME: %s\n" label))
    (insert (format "[[file:%s]]\n" image-name))))


;; *** latex-to-org-list-region

;; Select a list of items in LaTex and convert to list of items in org-mode

;; To use this function, select the region containing the LaTeX list and run:
;; M-x latex-to-org-list-region

(defun latex-to-org-list-region (start end)
  "Convert a LaTeX itemize list in the region to an Org-mode list."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\\\item" end t)
      (replace-match "-"))))


; https://unix.stackexchange.com/questions/691444/how-do-i-open-a-file-at-specific-line-in-a-running-emacs
; 
; have ffap pick up line number and goto-line
; found on emacswiki : https://www.emacswiki.org/emacs/FindFileAtPoint#h5o-6
; 

(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and
    save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string 
          (and (string-match ":[0-9]+" name)
               (substring name (1+ (match-beginning 0)) (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
    (if (and line-number (> line-number 0)) 
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (goto-line ffap-file-at-point-line-number)
    (setq ffap-file-at-point-line-number nil)))

(message "Finished global configuration.") 


(message "Start package configurations A")

;;;# A
(use-package auctex
  :straight t
  :defer t)
  
  (message "Start package configurations C")

;;;# C

(use-package citar
  :straight t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
    (citar-bibliography '("/Users/blaine/Documents/global.bib"))
    (citar-library-paths '("/Users/blaine/0papersLabeled") '("/Users/blaine/0booksUnlabeled"))
    (citar-library-file-extensions '("pdf" "epub"))
  :hook
  ;; enable autocompletion in buffer of citekeys
    (LaTeX-mode . citar-capf-setup)
    (org-mode . citar-capf-setup))


(use-package company-box
    :straight t
    :config
    (setq company-box-max-candidates 50
          company-frontends '(company-tng-frontend company-box-frontend)
          company-box-icons-alist 'company-box-icons-all-the-icons))

(with-eval-after-load 'company
  (define-key company-active-map
              (kbd "TAB")
              #'company-complete-common-or-cycle)
  (define-key company-active-map
              (kbd "<backtab>")
              (lambda ()
                (interactive)
                (company-complete-common-or-cycle -1))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-.") #'company-show-location)
  (define-key company-active-map (kbd "RET") nil))


;;;## Company Configuration
;; Source: https://github.com/Exafunction/codeium.el
(use-package company
  :straight t    
  :defer 0.1
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode))

  :config
  (global-company-mode t)
  (company-tng-configure-default) ; restore old tab behavior
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 1
   ;; get only preview
   ;; company-frontends '(company-preview-frontend)
   ;; also get a drop down
   company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
   ))


(message "Started L packages configurations")



;; Install and configure lsp-mode
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action))
  :bind-keymap ("C-c l" . lsp-command-map)
  :hook ((latex-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil))

;; Install and configure lsp-latex
(use-package lsp-latex
  :straight t
  :after lsp-mode
  :hook (latex-mode . lsp-latex-enable))

;; Install and configure lsp-ltex
(use-package lsp-ltex
  :straight t
  :after lsp-mode
  :hook ((text-mode . lsp)
         (latex-mode . lsp)
         (org-mode . lsp))
  :config
  (setq lsp-ltex-language "en-US")
  :init
  (setq lsp-ltex-version "16.0.0"))
   
(use-package lsp-ui
    :straight t
    :commands lsp-ui-mode)
 
(use-package lsp-treemacs 
       :straight t
       :commands lsp-treemacs-errors-list)



(message "Start package configurations M")
;;;# M
;;;## Marginalia Configuration
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :config
  (marginalia-mode))
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)


;;;## BEGINNING of org-agenda
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; follow links
(setq org-return-follows-link t)

;; Treat *.org files as org-mode files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Hide the markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;; Stoup's fonts for org
(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.6 :underline nil))))))






(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; org-capture
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

(setq org-agenda-files '("/Users/blaine/gtd/tasks/JournalArticles.org"
                         "/Users/blaine/gtd/tasks/potentialWriting.org"
                         "/Users/blaine/gtd/tasks/Proposals.org"
                         ; "/Users/blaine/gtd/tasks/Books.org"
                          "/Users/blaine/gtd/tasks/Talks.org"
                         ; "/Users/blaine/gtd/tasks/Posters.org"
                         ; "/Users/blaine/gtd/tasks/ManuscriptReviews.org"
                          "/Users/blaine/gtd/tasks/Private.org"
                         ; "/Users/blaine/gtd/tasks/Service.org"
                         ; "/Users/blaine/gtd/tasks/Teaching.org"
                         ; "/Users/blaine/gtd/tasks/Workshops.org"
                         ; "/Users/blaine/gtd/tasks/springsem24.org"
                         ; "/Users/blaine/gtd/tasks/summersem24.org"
                         "/Users/blaine/gtd/tasks/fallsem24.org"))
(message "Finished org-agenda configuration. Line 5139.")


;; Cycle through these keywords with shift right or left arrows.
(setq org-todo-keywords
        '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)"  "WAITING(w!)" "CAL(a)"  "PROJ(j)" "|" "DONE(d!)" "SOMEDAY(s!)" "CANCELLED(c!)"  )))

;; TODO colors OBE (Overcome by events)
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "GoldenRod" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("WAITING" . (:foreground "DarkOrange" :weight bold))
        ("CAL" . (:foreground "Red" :weight bold))
        ("PROJ" . (:foreground "LimeGreen" :weight bold))        
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ("CANCELLED" . (:foreground "LightGray" :weight bold))
        ))

(setq org-refile-targets '(("/Users/blaine/gtd/tasks/JournalArticles.org" :maxlevel . 2)
   ("/Users/blaine/gtd/tasks/Proposals.org" :maxlevel . 2)
   ))
(setq org-refile-use-outline-path 'file)
(message "Finished refile target configuration. Line 5162.")

;; ***** customized agenda views
;;
;; These are my customized agenda views by project.
;; The letter is the last parameter.
;; For example, enter ~C-c a b~ and then enter 402 at the prompt to list all active tasks related to 402 tasks.
;;
;; I learned about this approach [[https://tlestang.github.io/blog/keeping-track-of-tasks-and-projects-using-emacs-and-org-mode.html][here]].
;;
;; The CATEGORY keyword resides inside of a Properties drawer.
;; The drawers are usually closed.
;; I am having trouble opening my drawers in may org files.
;; In addition, I do not want to have to add a drawer to each TODO.
;;
;; I am loving Tags now.
;; I may switch to using Tags because they are visible in org files.
;; I tried and they are not leading to the expect list of TODOs in org-agenda.
;; I am stumped.
;;
;; In the meantime, enter ~C-c \~ inside JournalArticles.org to narrow the focus to the list of TODOs or enter ~C-c i b~ to get an indirect buffer.
;;

(setq org-agenda-custom-commands
      '(
    ("b"
             "List of all active 1019 tasks."
             tags-todo
             "1019\"/TODO|INITIATED|WAITING")
    ("c"
             "List of all active 523 RNA-drug crystallization review paper tasks."
             tags-todo
             "CATEGORY=\"523\"/TODO|INITIATED|WAITING")
    ("d"
             "List of all active 0527CrystalDetectionByAI tasks."
             tags-todo
             "CATEGORY=\"527\"/TODO|INITIATED|WAITING") 
    ("e"
            "List of all active 0032RNA32merEditingSite tasks."
            tags-todo
            "CATEGORY=\"32\"/TODO|INITIATED|WAITING") 
              
    ("l"
             "List of all active 0598tenRulesWritingProjectLog tasks."
             tags-todo
            "CATEGORY=\"598\"/TODO|INITIATED|WAITING")            
    ("e"
            "List of all active 0495emofat4mx tasks."
            tags-todo
            "CATEGORY=\"495\"/TODO|INITIATED|WAITING")            
    ("r"
            "List of all active 0466ReproDSD tasks."
            tags-todo
            "CATEGORY=\"515\"/TODO|INITIATED|WAITING")            
    ("s"
            "List of all active 0515CrystallizationSupports tasks."
            tags-todo
            "CATEGORY=\"515\"/TODO|INITIATED|WAITING")            
    ("P"
         "List of all projects"
         tags
         "LEVEL=2/PROJ")))

(message "Finished org-agenda custum command configuration.")


(message "Start package configurations P")
;# P
(use-package pdf-tools
 :straight t    
 :pin manual ;; manually update
 :config
 ;; initialise
 (pdf-tools-install)
 ;; open pdfs scaled to fit width
 (setq-default pdf-view-display-size 'fit-width)
 ;; use normal isearch
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
 :custom
 (pdf-annot-activate-created-annotations t "automatically annotate highlights"))


 (message "Start package configurations T")

 ;; C-x t t to launch treemacs
 ;; Support dragging files from the treemacs directory to a buffer to open them.
 ;; Default configuration for treemacs minus the treemacs-evil pacakge.
 ;; 
 (use-package treemacs
   :straight t
   :defer t
   :init
   (with-eval-after-load 'winum
     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
   :config
   (progn
     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
           treemacs-deferred-git-apply-delay        0.5
           treemacs-directory-name-transformer      #'identity
           treemacs-display-in-side-window          t
           treemacs-eldoc-display                   'simple
           treemacs-file-event-delay                2000
           treemacs-file-extension-regex            treemacs-last-period-regex-value
           treemacs-file-follow-delay               0.2
           treemacs-file-name-transformer           #'identity
           treemacs-follow-after-init               t
           treemacs-expand-after-init               t
           treemacs-find-workspace-method           'find-for-file-or-pick-first
           treemacs-git-command-pipe                ""
           treemacs-goto-tag-strategy               'refetch-index
           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
           treemacs-hide-dot-git-directory          t
           treemacs-indentation                     2
           treemacs-indentation-string              " "
           treemacs-is-never-other-window           nil
           treemacs-max-git-entries                 5000
           treemacs-missing-project-action          'ask
           treemacs-move-files-by-mouse-dragging    t
           treemacs-move-forward-on-expand          nil
           treemacs-no-png-images                   nil
           treemacs-no-delete-other-windows         t
           treemacs-project-follow-cleanup          nil
           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
           treemacs-position                        'left
           treemacs-read-string-input               'from-child-frame
           treemacs-recenter-distance               0.1
           treemacs-recenter-after-file-follow      nil
           treemacs-recenter-after-tag-follow       nil
           treemacs-recenter-after-project-jump     'always
           treemacs-recenter-after-project-expand   'on-distance
           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
           treemacs-project-follow-into-home        nil
           treemacs-show-cursor                     nil
           treemacs-show-hidden-files               t
           treemacs-silent-filewatch                nil
           treemacs-silent-refresh                  nil
           treemacs-sorting                         'alphabetic-asc
           treemacs-select-when-already-in-treemacs 'move-back
           treemacs-space-between-root-nodes        t
           treemacs-tag-follow-cleanup              t
           treemacs-tag-follow-delay                1.5
           treemacs-text-scale                      nil
           treemacs-user-mode-line-format           nil
           treemacs-user-header-line-format         nil
           treemacs-wide-toggle-width               70
           treemacs-width                           35
           treemacs-width-increment                 1
           treemacs-width-is-initially-locked       t
           treemacs-workspace-switch-cleanup        nil)

     ;; The default width and height of the icons is 22 pixels. If you are
     ;; using a Hi-DPI display, uncomment this to double the icon size.
     ;;(treemacs-resize-icons 44)

     (treemacs-follow-mode t)
     (treemacs-filewatch-mode t)
     (treemacs-fringe-indicator-mode 'always)
     (when treemacs-python-executable
       (treemacs-git-commit-diff-mode t))

     (pcase (cons (not (null (executable-find "git")))
                  (not (null treemacs-python-executable)))
       (`(t . t)
        (treemacs-git-mode 'deferred))
       (`(t . _)
        (treemacs-git-mode 'simple)))

     (treemacs-hide-gitignored-files-mode nil))
   :bind
   (:map global-map
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t d"   . treemacs-select-directory)
         ("C-x t B"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)))

 (use-package treemacs-projectile
   :after (treemacs projectile)
   :straight t)

 (use-package treemacs-icons-dired
   :hook (dired-mode . treemacs-icons-dired-enable-once)
   :straight t)

 ; (use-package treemacs-magit
 ;   :after (treemacs magit)
 ;   :straight t)

 (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
   :straight t
   :config (treemacs-set-scope-type 'Perspectives))

 ; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
 ;   :after (treemacs)
 ;   :straight t
 ;   :config (treemacs-set-scope-type 'Tabs))

 (treemacs-start-on-boot)
(message "End package configurations T")


(message "Start package configurations U")
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))


(message "Start package configurations V")
;;;# V
;;;## Vertico Configuration
(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )


(message "Start package configurations Y")

(use-package yasnippet
  :straight t    
  :config
  (yas-global-mode 1))
(global-set-key "\C-o" 'yas-expand)
(global-set-key "\C-c y i" 'yas-insert-snippet)
(global-set-key "\C-c y n" 'yas-new-snippet)



;; A cool hydra for finding snippets at point. Invoke with C-c y.
(use-package hydra
  :straight t     
  :defer 2
  :bind ("C-c y" . hydra-yasnippet/body))

(use-package math-preview
    :straight t
    :custom (math-preview-command "/Users/blaine/.nvm/versions/node/v22.4.0/lib/node_modules/math-preview/math-preview.js"))



(use-package popup
      :straight t )
;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))

    
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pdf-view-incompatible-modes
   '(linum-mode linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

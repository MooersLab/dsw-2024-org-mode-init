; The org-mode verson of the ~few packages~ init.el configuration available at https://github.com/MooersLab/dsw-2024-org-mode-init.
;%  - The number of semicolons indicates the headline level.
;%  - The combintatoin of a semicolon and a precent sign denotes a comment to be converted to prose in the org file.
;%  - The lines without semicolons in the left margin wind up in code blocks.
;%  
;%  - I use the bash command  alias e29f='/Applications/Emacs29.4.app/Contents/MacOS/Emacs --init-directory ~/e29fewpackages --debug-init'
;%  
;; The top section sets up the package management software and must come first.
(setq user-emacs-directory "~/e29fewpackages/")
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
;%  Messages printed to the messages buffer aid in locating bugs in the config file because the bug is somewhere beyond the last message.
(message "Finished straight package manager configuration.") 

;; Globalsettings
;%  The order of the remaining content of the intial.org does not matter but I like to divide it into settings, user-defined functions, and packages.
(message "Start global configuration.") 
;;; Enable mouse in terminal Emacs (emacs -nw)
(when (eq window-system nil)
      (xterm-mouse-mode t))
;;; Minibuffer history keybindings
;%  The calling up of a previously issued command in the minibuffer with ~M-p~ saves times.
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)
;;; Bibtex configuration
(defconst blaine/bib-libraries (list "/Users/blaine/Documents/global.bib"))

;;; For retina displays on Macs 
;%  TCombined with emacs-mac, this gives good PDF quality for [[https://www.aidanscannell.com/post/setting-up-an-emacs-playground-on-mac/][retina display]].
(setq pdf-view-use-scaling t)
;;; PDF default page width behavior
(setq-default pdf-view-display-size 'fit-page)
;;; Custom key sequences.
;%  Delete trailing whitespaces on lines.
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
;;; display line numbers. Need with s-l.
(global-display-line-numbers-mode)
;;; highlight current line
;%  This feature aids finding the cursor's current position.
(global-hl-line-mode +1)
(set-face-background hl-line-face "wheat1")
(set-face-attribute 'mode-line nil  :height 180)
;;; Shell configuration
;%  Let Emacs know which shell that you are using.
(use-package exec-path-from-shell
  :straight t
  :init
  (setenv "SHELL" "/opt/local/bin/zsh")
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  (exec-path-from-shell-initialize))
;;; Set size of the starting Window
;%  I like a wider window. Setting it initially saves an operation.
(setq initial-frame-alist '((top . 1)
                (left . 450)
                (width . 101)
                (height . 90)))
;;; Setting modifier keys
;%   ==> adjust here for operating system
;%  See this [[http://ergoemacs.org/emacs/emacs_hyper_super_keys.html][ for more information.]]
;%  set keys for Apple keyboard, for emacs in OS X
;%  Source http://xahlee.info/emacs/emacs/emacs_hyper_super_keys.html
;%  Seems to only work in the GUI mode.
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make option key do Super.
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-function-modifier 'hyper)  ; make Fn key do Hyper. Only works on Apple produced keyboards.
(setq mac-right-command-modifier 'hyper)
;;; Move cursor to place at last save
;%  Enable save-place-mode
(save-place-mode 1)
;%  Optionally specify the file to save cursor positions
(setq save-place-file "~/e29fewpackages/places")
;;; Setting section is finished
(message "Finished settings configuration.") 
;%  ***************************** User-defined functions section ***********************************************  
;; Start user-defined functions section
(message "User defined functions in alphabetical order.") 

;;; create-org-table-with-caption
;%  This interactive function prompts the user for the number of rows, columns, and caption of the table.
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
;;; find file and go to line number
;%  interactively enter the file name and line number in the minibuffer
(defun find-file-at-line (file line)
  "Open FILE on LINE."
  (interactive "fFile: \nNLine: \n")
  (find-file file)
  (goto-line line))

;;; ffap: find file at point
;%  https://unix.stackexchange.com/questions/691444/how-do-i-open-a-file-at-specific-line-in-a-running-emacs
;%  have ffap pick up line number and goto-line
;%  found on emacswiki : https://www.emacswiki.org/emacs/FindFileAtPoint#h5o-6
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
;;; insert-org-captioned-figure
;%  The function prompts the user for the image file path and name, the label, and the caption.
(defun insert-org-captioned-figure ()
  "Insert a captioned figure in Org-mode."
  (interactive)
  (let ((image-name (read-string "Enter the image file path: "))
        (label (read-string "Enter the figure label: "))
        (caption (read-string "Enter the figure caption: ")))
    (insert (format "#+CAPTION: %s \\label{%s}\n" caption label))
    (insert (format "#+NAME: %s\n" label))
    (insert (format "[[file:%s]]\n" image-name))))
;;; Convert a selected latex list of items to a org-mode list
;%  To use this function, select the region containing the LaTeX list and run:
;%  M-x latex-to-org-list-region
(defun latex-to-org-list-region (start end)
  "Convert a LaTeX itemize list in the region to an Org-mode list."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\\\item" end t)
      (replace-match "-"))))
;%  ;;; Open the Daily Log headline while keeping the other headline closed.
;%  (defun org-open-specific-headline (headline)
;%    "Open the specific top-level HEADLINE while keeping others closed."
;%    (goto-char (point-min))
;%    (org-overview)
;%    (if (re-search-forward (format "^\\* %s" (regexp-quote headline)) nil t)
;%        (org-show-children 3)
;%      (message "Headline not found")))
;% 
;%  (defun my-org-mode-setup ()
;%    "Custom setup for Org mode."
;%    (org-open-specific-headline "Daily Log"))
;%  (add-hook 'org-mode-hook 'my-org-mode-setup)
;% 
;% 
;%  ;;; Moves cursor to headline with the restart tag.
;%  ;%  Waits for the  my-org-mode-setup function to run first.
;%  (defun my-open-log-file-and-move-above-tag ()
;%    "If the current buffer's file name starts with 'log', move the cursor above a second-level headline with a specific tag."
;%    (let ((tag "restart-here"))
;%      (when (and buffer-file-name
;%                 (string-prefix-p "log" (file-name-nondirectory buffer-file-name)))
;%        (goto-char (point-min))
;%        (if (re-search-forward (format "^\\*\\* .*:%s:" tag) nil t)
;%           (forward-line -1)
;%          (message "Tag not found")))))
;% 
;%  ;defun org-open-specific-headline (headline)
;%  ; "Open the specific top-level HEADLINE while keeping others closed.
;%  ;hen move the cursor to the line above the second-level headline with the tag."
;%  ; (goto-char (point-min))
;%  ; (org-overview)
;%  ; (if (re-search-forward (format "^\\* %s" (regexp-quote headline)) nil t)
;%  ;     (progn
;%  ;       (org-show-children 3)
;%  ;       (when (re-search-forward "^\\*\\* .*:restart-here:" nil t)
;%  ;         (beginning-of-line)
;%  ;         (forward-line -1)))
;%  ;   (message "Headline not found")))
;% 
;% 
;% 
;%  (defun my-delayed-org-setup ()
;%    "Run my-org-mode-setup and then my-open-log-file-and-move-above-tag."
;%    (my-org-mode-setup)
;%    (run-at-time "0.1 sec" nil 'my-open-log-file-and-move-above-tag))
;% 
;%  (add-hook 'org-mode-hook 'my-delayed-org-setup)
;;; Open a file and move to a headline with a specific tag
;%  The default tag is restart-here.
;%  Example usage:
;%  (open-org-file-and-move-to-tag "~/path/to/your/file.org" "your-tag")
(defun open-org-file-and-move-to-tag (file &optional tag)
  "Open an Org file and move the cursor below a headline with a specific TAG.
If TAG is not provided, use a hardcoded default tag.
You have have to adjust the headline level in the funciton.
The regular expression ^\\*\\* .*:%s: is used to search for second-level headlines (those starting with **) with the specified tag."
  (interactive "fOrg file: \nsTag (leave empty for default): ")
  (let ((tag (if (string= tag "") "restart-here" tag)))
    (find-file file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\* .*:%s:" tag) nil t)
        (org-end-of-subtree)
      (message "Tag not found"))))
;;; Play a YouTube video with mpv
;%  You insert the YouTube url in the minibufffer.
;%  You have to install mpv with a package manager and another binary package.
;%  sudo curl -L https://yt-dl.org/downloads/latest/youtube-dl -o /usr/local/bin/youtube-dl
;%  sudo chmod a+rx /usr/local/bin/youtube-dl
(defun play-youtube-video (url)
  "Play a YouTube video with mpv."
  (interactive "sYouTube URL: ")
  (start-process "mpv" nil "mpv" URL))  
;;; Protesilous Starvou functions
;;;; Functions to open popup menu when Emacs server is running
;%  Source: https://www.youtube.com/watch?v=vbWxT8tht9A&list=PL8Bwba5vnQK14z96Gil86pLMDO2GnOhQ6
;%  This function is used to capture notes when Emacs is not open.
;%  This idea could be harness for other purposes.
;%  You will need to map the 
(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))
(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))
(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)
(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

;%  (declare-function tmr "tmr" (time &optional description acknowledgep))
;%  (defvar tmr-timer-created-functions)
;% 
;%  ;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
;%  (prot-window-define-with-popup-frame tmr)
;% 
;%  (add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame)

;;;;; The emacsclient calls that need ot be bound to system-wide keys
;%  The emacsclient is an Emac version agnostic.
;%  
;%  alias oc="emacslient -e '(prot-window-popup-org-capture)'"
;%  emacsclient -e '(prot-window-popup-tmr)'

;;; Reload the initialization file after editing it in Emacs
(defun reload-init-e29f ()
  "Reload the init.el file for e29org. Edit the path to suite your needs."
  (interactive)
  (load-file "~/e29fewpackages/init.el"))
  

;;; Spawn a new shell with the supplied title 
(defun spawn-shell (name)
  "Invoke shell test"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer))
  (process-send-string nil "echo 'test1'\n")
  (process-send-string nil "echo 'test2'\n"))

  
;;; Move the cursor to the minibuffer without using the mouse
;%  From video https://www.youtube.com/watch?v=X8c_TrGfYcM&t=15s using Emacs as a multiplexer."
;%  Derived from http://stackoverflow.com/a/4116113/446256.
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key "\C-cm" 'switch-to-minibuffer) ;; Bind to `C-c m' for minibuffer.
(message "End of user-defined functions.")


;; Package configuration section
(message "Start package configurations in alphabetical order by package name.")
(message "Start A package configurations")
;;; A
;;;; AUCTeX
;%  This is the more advanced LaTeX. 
;%  Emacs has native LaTeX support that AUCTeX is an alternative to.
(use-package auctex
  :straight t
  :defer t)
(message "Start C package configurations")
;;; C
;;;; Citar
;%  Citar is a bibliographic management system that is an alternative to org-ref.
;%  Citar can operate in tex and org file whereas org-ref is limited to org-mode files.
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
;;;; company-box
;;;;; formats the options delivered by the company autocompletion system
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
;;;; Company Configuration
;%  Source: https://github.com/Exafunction/codeium.el
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
(message "Finished C package configurations")
(message "Started L packages configurations")
;;; L
;;;; lsp-mode
;%  This mode is required to be able to use other lsp modes.
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
;;;; lsp-latex
(use-package lsp-latex
  :straight t
  :after lsp-mode
  :hook (latex-mode . lsp-latex-enable))
;;;; lsp-ltex (the languagetool lsp server)
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
;;;; lsp-ui   
(use-package lsp-ui
    :straight t
    :commands lsp-ui-mode)
;;;; lsp-treemacs 
(use-package lsp-treemacs 
       :straight t
       :commands lsp-treemacs-errors-list)       
(message "Finished L package configurations")
(message "Start M package configurations")
;;; M
;;;; Marginalia Configuration
;%  This package produces a transient list of optional commands.
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :config
  (marginalia-mode))
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;;;; Math-preview that is mode agnostic
;%  Uses MathJaX.
;%  You have to download and install the bindary
(use-package math-preview
  :straight t
  :custom (math-preview-command "/Users/blaine/.nvm/versions/node/v22.4.0/lib/node_modules/math-preview/math-preview.js"))s
(message "Finished M packages configurations")



(message "Start O package configurations")
;;; O
;;;; Org-agenda
;%  This can be complex to configure.

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;;;;; Follow links
(setq org-return-follows-link t)

;;;;; Treat *.org files as org-mode files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;;;; Hide the emphasis markers so you just see bold text as BOLD-TEXT and not *BOLD-TEXT*
(setq org-hide-emphasis-markers t)

;;;;; Stoup's fonts for org
;%  These settings have been added to the customization section.
;%  They can only be called once in an init.el file.
;% #+BEGIN-COMMENT
;% (let* ((variable-tuple
;%         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;%               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;%               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;%               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;%               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;%               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;%        (base-font-color     (face-foreground 'default nil 'default))
;%        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;% 
;%   (custom-theme-set-faces
;%    'user
;%    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;%    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;%    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;%    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;%    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;%    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
;%    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
;%    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
;%    `(org-document-title ((t (,@headline ,@variable-tuple :height 1.6 :underline nil))))))
;% #+END_COMMENT

;;;;; Org-agenda key-bindings
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(global-set-key (kbd "C-c v") 'org-refile)
;%  Note that I have more capture commands below.
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
;;;;; Source of TODO keywords read into org-agenda
(setq org-agenda-files '("/Users/blaine/.notes"
                         "/Users/blaine/gtd/tasks/JournalArticles.org"
                         "/Users/blaine/0573CrystalDetectionMeasurement/log0573.org"
                         "/Users/blaine/0598tenRulesWritingLog/cb/log0598tsrWritingLog.org"
                         "/Users/blaine/gtd/tasks/Proposals.org"
                         "/Users/blaine/1019NIHemofat/cb/log1019.org"
                         "/Users/blaine/gtd/tasks/Books.org"
                         "/Users/blaine/gtd/tasks/Talks.org"
                         "/Users/blaine/gtd/tasks/Posters.org"
                         "/Users/blaine/gtd/tasks/ManuscriptReviews.org"
                         "/Users/blaine/gtd/tasks/Private.org"
                         "/Users/blaine/gtd/tasks/Service.org"
                         "/Users/blaine/gtd/tasks/Teaching.org"
                         "/Users/blaine/gtd/tasks/Workshops.org"
                         "/Users/blaine/gtd/tasks/springsem24.org"
                         "/Users/blaine/gtd/tasks/summersem24.org"
                         "/Users/blaine/gtd/tasks/fallsem24.org"))
;;;;; Order of TODO keywords
;%  Cycle through these keywords with shift right or left arrows.
(setq org-todo-keywords
        '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)"  "WAITING(w!)" "CAL(a)"  "PROJ(j)" "|" "DONE(d!)" "SOMEDAY(s!)" "CANCELLED(c!)"  )))
;;;;; TODO keyword colors
(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "Red" :weight bold))
        ("PLANNING" . (:foreground "DeepPink" :weight bold))
        ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
        ("WAITING" . (:foreground "DarkOrange" :weight bold))
        ("CAL" . (:foreground "GoldenOrange" :weight bold))
        ("PROJ" . (:foreground "LimeGreen" :weight bold))
        ("DONE" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ("CANCELLED" . (:foreground "LightGray" :weight bold))
        ))
;;;;; Press C-c r to select the log file from the list of log files for insert todos with C-c t
;%  The function will then search for the heading with the specified tag and append the TODO item at the bottom of the TODO list under that heading.
;% (setq my-org-refile-directories '(
;%    ("/Users/blaine/gtd/tasks/JournalArticles.org" :maxlevel . 3)
;%    ("/Users/blaine/gtd/tasks/Proposals.org" :maxlevel . 3)
;%    ("/Users/blaine/0598tenRulesWritingLog/cb/log0598tsrWritingLog.org" :maxlevel . 3)
;%    ))

;;;;;; Writing project log files as refile targets
;%  The TODOs are at the 3rd headline level.

(setq my-org-refile-targets '(
    ("/Users/blaine/0573CrystalDetectionMeasurement/log0573.org" :maxlevel . 3)
    ("/Users/blaine/0598tenRulesWritingLog/cb/log0598tsrWritingLog.org" :maxlevel . 3)
    ("/Users/blaine/1019NIHemofat/cb/log1019.org" :maxlevel . 3)
   ))

(defun my-select-org-refile-target ()
     "Prompt for an org refile target from a list of directories."
     (interactive)
     (let ((target (completing-read "Select refile target: " my-org-refile-targets)))
       (setq my-org-refile-targets `((,target :maxlevel . 3)))))

(setq org-refile-use-outline-path 'file)
(global-set-key (kbd "C-c r") 'my-select-org-refile-target)


;;;;;; Append TODO item at bottom of TODO list in writing project log file.
;%  I like to use project ID number and 'do' at the tag.
(defun my-append-todo-to-heading (tag todo-text)
     "Append TODO-TEXT to the bottom of a TODO list under a heading with TAG. Enter the tag without the flanking colons."
         (interactive "sTag: \nsTODO: ")
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward (format "^\*+ .* :%s:" tag) nil t)
               (progn
                 (org-end-of-subtree t t)
                 (insert (format "*** TODO %s \n" todo-text)))
             (message "Heading with tag %s not found" tag))))
(global-set-key (kbd "C-c t") 'my-append-todo-to-heading)

(message "Finished refile target configuration.")

;;;;; Customized agenda views
;% 
;% These are my customized agenda views by project.
;% The letter is the last parameter.
;% For example, enter ~C-c a b~ and then enter 402 at the prompt to list all active tasks related to 402 tasks.
;% 
;% I learned about this approach [[https://tlestang.github.io/blog/keeping-track-of-tasks-and-projects-using-emacs-and-org-mode.html][here]].
;% 
;% The CATEGORY keyword resides inside of a Properties drawer.
;% The drawers are usually closed.
;% I am having trouble opening my drawers in may org files.
;% In addition, I do not want to have to add a drawer to each TODO.
;% 
;% I am loving Tags now.
;% I may switch to using Tags because they are visible in org files.
;% I tried and they are not leading to the expect list of TODOs in org-agenda.
;% I am stumped.
;% 
;% In the meantime, enter ~C-c \~ inside JournalArticles.org to narrow the focus to the list of TODOs or enter ~C-c i b~ to get an indirect buffer.
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
(message "Fnished O package configurations")

(message "Start P package configurations")
;;; P
;;;; pdf-tools
;%  Emacs has a built-in DocView package that can display PDFs but it is not as versatile as pdf-tools.
;%  You will likely by prompted to run pdf-tools-install to compile it.
;%  It is needed to be able to annotate PDF from inside Emacs.
;%  I have not gotten that far.
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
(message "Finished P package configurations.")
 
(message "Start S package configurations.")
;;; S
;;;; show-key 
;%  Conflicts with tmux-pane
;% #+BEGIN_COMMENT
;% (straight-use-package
;%   '(showkey :type git :local-repo "~/e29fewpackages/manual-install/showkey"))
;% (set-face-attribute 'default nil :height 240)
;% (require 'showkey)
;% #+END_COMMENT
;;;; The emacsclient call depends on the daemon or `server-mode'
;%  The emacsclient depends either running the server or a daemon.
;%  The server is easier to manage than the daemon because you will
;%  find to find the daemon and kill it after updating the init.el file.
;%  This package is built-in, so it does not need to be installed by straight.
(use-package server  
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))
(message "Finished S package configurations.")


(message "Start T package configurations.")
;;; T
;;;; tmux-pane
(use-package tmux-pane
   :straight (tmux-pane :type git :host github :repo "laishulu/emacs-tmux-pane"))

;;;; treemacs
;%  Provides sidebar access to contents of the treemacs project directory 
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

;;;; treemacs-protjectile         
(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)
;;;; treemacs-icons-dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

;;;; treemacs-perspective 
;%  treemacs-perspective if you use perspective.el vs. persp-mode
(use-package treemacs-persp 
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :straight t
  :config (treemacs-set-scope-type 'Perspectives))

;;;; treemacs start on boot
(treemacs-start-on-boot)
(message "Finished T package configurations.")

(message "Start U package configurations.")
;;; U
;;;; undo-tree
;%  The displayed undo tree is very helpful.
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1)) 
(message "Finished U package configurations.")   


(message "Start V package configurations.")
;;; V
;;;; vterm
;%  One of five terminal type available in Emacs.
;%  See https://github.com/akermu/emacs-libvterm for configuration of init.el and .zshrc
(use-package vterm
  :straight t)
(define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)

;;;; vertico
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
  (setq vertico-cycle t))
(message "Finished V package configurations.") 


(message "Start package configurations W")
(message "Finished W package configurations.") 


;;; Y
(message "Start package configurations Y")
;;;; yasnippet
;%  The popular snippet creator and manager package,
(use-package yasnippet
  :straight t    
  :config
  (yas-global-mode 1))
(global-set-key "\C-o" 'yas-expand)
(global-set-key "\C-c y i" 'yas-insert-snippet)
(global-set-key "\C-c y n" 'yas-new-snippet)

;;;; A cool hydra for finding snippets at point. Invoke with C-c y.
(use-package hydra
  :straight t     
  :defer 2
  :bind ("C-c y" . hydra-yasnippet/body))

;;;; A popup menu for snippet selection
(use-package popup
  :straight t)
;; add some shortcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
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
     :isearch t)))
(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))
(message "Finished Y package configurations.") 

; Customizations
;%  Leave these alone.
(message "Start custom set-variables")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pdf-view-incompatible-modes
   '(linum-mode linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode))
 '(showkey-log-mode t)
 '(weatherline-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande" :height 1.6 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande" :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande" :height 1.2))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "Black" :font "Lucida Grande")))))
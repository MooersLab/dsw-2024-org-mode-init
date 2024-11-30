; The org-mode verson of the ~few packages~ init.el configuration available at https://github.com/MooersLab/dsw-2024-org-mode-init.
;%  - The number of semicolons indicates the headline level.
;%  - The combintatoin of a semicolon and a precent sign denotes a comment to be converted to prose in the org file.
;%  - The lines without semicolons in the left margin wind up in code blocks.
;%  
;%  - I use the bash command  alias e29f='/Applications/Emacs29.4.app/Contents/MacOS/Emacs --init-directory ~/e29fewpackages --debug-init'
;%  - Note that you can toogle a fullscreen of Emacs on the Mac with M-F10.
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
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (setq xterm-extra-capabilities '(modifyOtherKeys)))
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
;;; Set the path to the new Node.js binary
;% The default Node.js is installed by macports in /opt/local/bin and is outdated.
;% The newer version is installed by home brew in /usr/local/bin.
;% Check version of Node found by runing (shell-command-to-string "node -v")
(let ((new-node-path "/usr/local/bin/node"))
  (setq exec-path (cons new-node-path exec-path))
  (setenv "PATH" (concat new-node-path ":" (getenv "PATH"))))
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
;% (defun append-to-sqlite-table (db-path table-name pairs)
;%   "Append PAIRS of terms to a two-column TABLE-NAME in the SQLite database at DB-PATH.
;% PAIRS should be a list of cons cells, where each cons cell is a pair of terms."
;%   (let ((db (emacsql-sqlite db-path)))
;%     (dolist (pair pairs)
;%       (emacsql db
;%                [:insert :into $i1 :values $v2]
;%                table-name
;%                (vector (car pair) (cdr pair))))
;%     (emacsql-close db)))
;% ;; Example usage:
;% (let ((db-path "/path/to/your/database.sqlite")
;%       (table-name 'your_table)
;%       (pairs '((term1 . term2) (term3 . term4) (term5 . term6))))
;%   (append-to-sqlite-table db-path table-name pairs))
;;; convert-init-el-to-org    
(defun convert-init-el-to-org (input-file output-file)
      "Convert an Emacs init.el file to an Org-mode file."
      (with-temp-buffer
        (insert-file-contents input-file)
        (let ((in-src-block nil))
          (with-temp-file output-file
            (insert "#+TITLE: Emacs Configuration\n")
            (insert "#+AUTHOR: Blaine Mooers\n")
            (insert "#+OPTIONS: toc:nil\n\n")
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (cond
                 ;; Match comments and convert them to org-mode headings or prose
                 ((string-match-p "^\\s-*;%+" line)
                  (let ((prose (string-trim (replace-regexp-in-string "^\\s-*;%+" "" line))))
                    (when in-src-block
                      (insert "#+END_SRC\n\n")
                      (setq in-src-block nil))
                    (insert (format "%s\n\n" prose))))
                 ((string-match-p "^\\s-*;" line)
                  (let* ((level (length (match-string 0 line)))
                         (heading (string-trim (replace-regexp-in-string "^\\s-*;+" "" line))))
                    (when in-src-block
                      (insert "#+END_SRC\n\n")
                      (setq in-src-block nil))
                    (insert (format "%s %s\n" (make-string level ?*) heading))))
                 (t
                  (unless in-src-block
                    (insert "#+BEGIN_SRC emacs-lisp\n")
                    (setq in-src-block t))
                  (insert (format "%s\n" line))))
                (forward-line 1)))
            (when in-src-block
              (insert "#+END_SRC\n"))))))
;% Example usage:
;% (convert-init-el-to-org "~/path/to/init.el" "~/path/to/init.org")
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
;;; launch-ithoughtsx
;% This is the best mindmapping software that I have encountered.
(defun launch-ithoughtsx ()
  "Launch iThoughtsX application."
  (interactive)
  (shell-command "open -a iThoughtsX"))
(global-set-key (kbd "C-c i") 'launch-ithoughtsx) 
;;; launch-jabref
;% I favored the simplicity and power of JabRef for mamanging BibTeX entries.
(defun launch-jabref ()
      "Launch jabRef application."
      (interactive)
      (shell-command "open -a JabRef"))
    (global-set-key (kbd "C-c j") 'launch-jabref) 
;;; launch-timesspent
;% This is a sqlite database where I track my effort.
(defun launch-timesspent ()
      "Launch timesspent database."
      (interactive)
      (shell-command "open /Users/blaine/6003TimeTracking/cb/mytime.db"))
    (global-set-key (kbd "C-c e") 'launch-timesspent) 
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

;;; open-template-with-citekey
;% Open template file renmaed with the citekey under the point.
;% This file is for use with an annotated bibliography.
;% Developed with the help of CoPilot.
(defun open-new-abibnote-on-citekey ()
  "Open a template file in Org-mode, rename it to the citekey under the cursor, 
  and save it to '~/abibNotes/'. Citar has a function that will insert the citekey."
  (interactive)
  (let* ((citekey (thing-at-point 'word t))
         (template-file "~/abibNotes/templates/abib-template.org")
         (output-dir "~/abibNotes/")
         (output-file (concat output-dir citekey ".org")))
    (unless (file-exists-p output-dir)
      (make-directory output-dir t))
    (if (and citekey (file-exists-p template-file))
        (progn
          (copy-file template-file output-file t)
          (find-file output-file)
          (message "Template file saved as %s" output-file))
      (message "Citekey or template file not found"))))
(global-set-key (kbd "C-c z") 'open-new-abibnote-on-citekey)
;;;; play-youtube-video 
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
(prot-window-define-with-popup-frame org-capture)
(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

;%#+BEGIN_COMMENT 
;%  (declare-function tmr "tmr" (time &optional description acknowledgep))
;%  (defvar tmr-timer-created-functions)
;%  (prot-window-define-with-popup-frame tmr)
;% 
;%  (add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame)
;%#+END_COMMENT

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
(setq exec-path (append '("/usr/local/texlive/2024/bin/universal-darwin") exec-path))
(setenv "PATH" (concat "/usr/local/texlive/2024/bin/universal-darwin:" (getenv "PATH")))
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
;;;; citar-org-roam
;% Use to generate literature notes for bib entries accessed with citar.
(use-package citar-org-roam
  :straight t
  :after (citar org-roam)
  :config (citar-org-roam-mode))
(setq citar-org-roam-note-title-template "${author} - ${title}")
(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :target
         (file+head
          "%<%Y%m%d%H%M%S>-${slug}.org"
          "#+title: ${note-title}\n")
         :unnarrowed t)
        ("n" "literature note" plain
         "%?"
         :target
         (file+head
          "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
          "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
         :unnarrowed t)))
(setq citar-org-roam-capture-template-key "n")

; (citar-register-notes-source
;  'orb-citar-source (list :name "Org-Roam Notes"
;         :category 'org-roam-node
;         :items #'citar-org-roam--get-candidates
;         :hasitems #'citar-org-roam-has-notes
;         :open #'citar-org-roam-open-note
;         :create #'orb-citar-edit-note
;         :annotate #'citar-org-roam--annotate))

(setq citar-notes-source 'orb-citar-source)


(defvar bibliographic-entry-template
  "#+title: %s
#+subtitle: Bibliographic Notes
#+author: %s
#+email: %s
#+property: header-args+ :comments link
#+cite_export: csl apa.csl

* Notes

|

* References

#+begin_src bibtex :tangle %s :exports none
%s
#+end_src

#+print_bibliography:")

(defun my-citar-org-open-notes (key entry)
  (let* ((bib (concat my/bibtex-directory key ".bib"))
         (org (concat my/bibtex-directory key ".org"))
         (new (not (file-exists-p org))))
    (funcall citar-file-open-function org)
    (when (and new (eq (buffer-size) 0))
      (insert (format bibliographic-entry-template
                      (assoc-default "title" entry)
                      user-full-name
                      user-mail-address
                      bib
                      (with-temp-buffer
                        (insert-file-contents bib)
                        (buffer-string))))
      (search-backward "|")
      (delete-char 1))))


; Define the Template:
;
; The bibliographic-entry-template variable holds the template string for the bibliographic entry.
; Function to Open Notes:
;
; The my-citar-org-open-notes function constructs the paths for the .bib and .org files, opens the .org file, and inserts the formatted template if the file is new.
; Set the Default Note Function:
;
; The setq-default line sets my-citar-org-open-notes as the default function for opening notes with citar.
; Usage:
; Add the above code to your init.el file.
; Ensure that my/bibtex-directory is defined and points to the directory where your BibTeX files are stored.
; When you open a note with citar, it will use the specified template and function



(setq-default citar-open-note-function 'my-citar-org-open-notes)
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
;%  This is a popular autocompletion package.
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
;;;; copilot
;% see https://github.com/copilot-emacs/copilot.el?tab=readme-ov-file for install protocol.
;% Requires a Github Copilot subscription.
;% I had to configure the copilot-node-executable variable to point to the correct executable.
;% Configure by entering
;% See for fancy functions to enhance experience: 
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t) 
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)  
(message "Finished C package configurations")

;;; E
(message "Started E packages configurations")
(use-package emacsql
    :straight t)
; (package-use emacsql-sqlite
;     :straight t)
(message "Finished E packages configurations")



;;; F
(message "Started F packages configurations")
;;;; frame-tabs
;% This package is fanstastic! I love the tiny tabs.
;% They ease navigating between buffers, especially when using emacsclient.
(frame-tabs-mode)
(message "Finished F packages configurations")



(message "Started J packages configurations")
;;;; Jinx
;% The enhancted spell checker.
;% Uses fewer resources than flyspell and is faster.
;% https://github.com/minad/jinx/wiki
(use-package jinx
  :straight (jinx :type git :host github :repo "minad/jinx")
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;% Enable Jinx globally
(add-hook 'emacs-startup-hook #'global-jinx-mode)

(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-M-$" #'jinx-languages)
(message "Finished J packages configurations")
(message "Started L packages configurations")
;;; L
;;;; listen
;% MP3 player
;% No configuration required.
;% package-install listen
;% Requires vlc installed via home brew.
;% Configure via customize the file path to your mp3 files.
;% Adding mp3 files without metadata to queues is tricky.
;% Free mp3's of classical music can be found here: https://archive.org/ and here 
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
;%  This package adds the first line of the docstring for the commands returned by vertico.
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :config
  (marginalia-mode))
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)
;;;; Markdown
;% Sometimes we have to edit the README.md files for our secret manuscirpt repositroies on GitHub or Codeberg.
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown"))
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "M-<up>") 'markdown-move-item-up)
    (define-key markdown-mode-map (kbd "M-<down>") 'markdown-move-item-down))
;% Functions to move list items up and down
  (defun markdown-move-item-up ()
    "Move the current markdown list item up."
    (interactive)
    (let ((col (current-column)))
      (markdown-beginning-of-item)
      (transpose-lines 1)
      (forward-line -2)
      (forward-char col)))
  (defun markdown-move-item-down ()
    "Move the current markdown list item down."
    (interactive)
    (let ((col (current-column)))
      (markdown-beginning-of-item)
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (forward-char col)))
;;;; Math-preview that is mode agnostic
;%  Uses MathJaX.
;%  You have to download and install the binary
(use-package math-preview
  :straight t
  :custom (math-preview-command "/Users/blaine/.nvm/versions/node/v22.4.0/lib/node_modules/math-preview/math-preview.js"))


;% ;;;; Mingus
;% ;% For Linux users.
;% ;% A MPD client for Emacs that provides a user-friendly interface for managing your music library and playlists.
;% (use-package mingus
;%     :straight t
;%     :config
;%     (setq mingus-mpd-host "localhost"
;%           mingus-mpd-port "6600"))
;% ;% Mingus can exceed the buffer limits.
;% ;% Keep playists short.
;% (setq max-specpdl-size 13000)  ;; Default is 1300
;% (setq max-lisp-eval-depth 8000)  ;; Default is 800
;% ;;;; MPC
;% ;% A lighter MPD client
;% ;% Must install mpc outside of Emacs. I used home brew.
;% ;% Check connection to MPD with `mpc -h localhost -p 6600 status`
;% (use-package mpc
;%   :straight t
;%   :config
;%   (setq mpc-host "localhost"
;%         mpc-port "6600"))
;% (setenv "MPD_HOST" "localhost")
;% (setenv "MPD_PORT" "6600")
;% ;;;; MPD
;% ;% Runs as a daemon.
;% ;% Can install with home brew on the Mac.
;% (use-package mpdel
;%     :straight (mpdel :type git :host github :repo "mpdel/mpdel")
;%     :config
;%     (setq mpdel-prefix-key (kbd "C-c m"))
;% (mpdel-mode))
(message "Finished M packages configurations")
(message "Start O package configurations")
;;; O
;;;; orderless
;% This package returns search terms without regard to their order in the name of the returned function or command.
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;;;; LaTeX config for org-mode
(with-eval-after-load 'ox-latex
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode --shell-escape -synctex=1 -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode --shell-escape -synctex=1 -output-directory %o %f"
          "pdflatex -interaction nonstopmode --shell-escape -synctex=1 -output-directory %o %f")))


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

;;;;;; Include the writing project log files as refile targets
;% A list can be stored in a private file
;%  The TODOs are at the 3rd headline level.
;% Initialize private refile targets as an empty list.
(setq private-refile-targets '())
;; Load private refile targets from an external file if it exists.
(let ((private-file (expand-file-name "private-refile-targets.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load private-file)))
;% Public refile target:
(setq public-refile-target
      '(( "/Users/blaine/1019NIHemofat/cb/log1019.org" :maxlevel . 3)))
;% Combine the private and public refile targets. The private version could be the above empty list.
(setq my-org-refile-targets (append private-refile-targets public-refile-target))
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
;;;; Remap the change priority keys to use the UP or DOWN key
;; (define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
;; (define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
(message "Finished org-agenda custum command configuration.")
(message "Started org-cc.")
;% ;;;; org-cc
;% ;% Context clues
;% ;% source  https://github.com/durableOne/org-cc
;% (use-package org-cc
;%   :straight (org-cc :type git :host github :repo "durableOne/org-cc")
;%   :ensure nil
;%   :after org
;%   :custom
;%   (org-cc-directory (concat org-directory "org-cc")) ;; subdirectory of the heading's attachment directory
;%   (org-cc-days 14)
;%   :init
;%   (add-hook 'org-clock-in-hook #'org-cc-display-notes)
;% )
;% (global-set-key (kbd "C-c k") 'org-cc-edit-cc-file)
;% (global-set-key (kbd "C-c x") 'org-cc-display-notes)
;% (message "Finished org-cc.")
(message "Started org-noter configuration.")
;;;; org-noter
;% This package synchronizes notes on documents, such as PDFs, while keeping the notes themselves in an Org-mode file. 
;% The external notes are in a org document with all the features that Org has.
;% Enter `i` in the document buffer to save the location of the note.
;% Workds best with pdf-tools and org-ref.
;% Integrated here with citar.
;%
;% - Open a new reference note with (citar0-open-notes)
;% Open the document in Emacs.
;% M-x org-noter
(use-package org-noter
             :straight)
;% Create folder for storing the org-noter-notes.
                    (setq org-noter-notes-search-path '("~/org-noter-notes/"))

;% (message "Started org-noter configuration.")
;% (use-package org-noter
;%   :straight (org-noter :type git :host github :repo "weirdNox/org-noter")
;%   :config
;%   ;; Your org-noter config ........
;%   (require 'org-noter-pdftools))
;%
;% (use-package org-pdftools
;%   :straight (org-pdftools :type git :host github :repo "fuxialexander/org-pdftools")
;%   :hook (org-mode . org-pdftools-setup-link))
;%
;% (use-package org-noter-pdftools
;%   :straight (org-noter-pdftools :type git :host github :repo "fuxialexander/org-pdftools")
;%   :after org-noter
;%   :config
;%   ;; Add a function to ensure precise note is inserted
;%   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;%     (interactive "P")
;%     (org-noter--with-valid-session
;%      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;%                                                    (not org-noter-insert-note-no-questions)
;%                                                  org-noter-insert-note-no-questions))
;%            (org-pdftools-use-isearch-link t)
;%            (org-pdftools-use-freepointer-annot t))
;%        (org-noter-insert-note (org-noter--get-precise-info)))))
;%
;%   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;%   (defun org-noter-set-start-location (&optional arg)
;%     "When opening a session with this document, go to the current location.
;% With a prefix ARG, remove start location."
;%     (interactive "P")
;%     (org-noter--with-valid-session
;%      (let ((inhibit-read-only t)
;%            (ast (org-noter--parse-root))
;%            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;%        (with-current-buffer (org-noter--session-notes-buffer session)
;%          (org-with-wide-buffer
;%           (goto-char (org-element-property :begin ast))
;%           (if arg
;%               (org-entry-delete nil org-noter-property-note-location)
;%             (org-entry-put nil org-noter-property-note-location
;%                            (org-noter--pretty-print-location location))))))))
;%   (with-eval-after-load 'pdf-annot
;%     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;% (message "Finished org-noter configuration.")


;%(use-package org-noter
;%  :straight (org-noter :type git :host github :repo "weirdNox/org-noter")      
;%  :after org
;%  :config
;%  ;% Your org-noter config ........
;%  :config
;%  (setq
;%    org_notes (concat (getenv "HOME") "/org-roam/")
;%    zot_bib (concat (getenv "HOME") "/Documents/global.bib")
;%    org-directory org_notes
;%    deft-directory org_notes
;%    org-roam-directory org_notes
;%    ;% keep an empty line between headings and content in Org file
;%    org-noter-separate-notes-from-heading t)
;%  (require 'org-noter-pdftools))

;%(use-package org-pdftools
;%  :straight (org-pdftools :type git :host github :repo "fuxialexander/org-pdftools") 
;%  :hook (org-mode . org-pdftools-setup-link))
;%
;%(use-package org-noter-pdftools
;%  :after org-noter
;%  :config
;%  ;% Add a function to ensure precise note is inserted
;%  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;%    (interactive "P")
;%    (org-noter--with-valid-session
;%     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;%                                                   (not org-noter-insert-note-no-questions)
;%                                                 org-noter-insert-note-no-questions))
;%           (org-pdftools-use-isearch-link t)
;%           (org-pdftools-use-freepointer-annot t))
;%       (org-noter-insert-note (org-noter--get-precise-info)))))
;%
;%  ;% fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;%  (defun org-noter-set-start-location (&optional arg)
;%    "When opening a session with this document, go to the current location.
;%With a prefix ARG, remove start location."
;%    (interactive "P")
;%    (org-noter--with-valid-session
;%     (let ((inhibit-read-only t)
;%           (ast (org-noter--parse-root))
;%           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;%       (with-current-buffer (org-noter--session-notes-buffer session)
;%         (org-with-wide-buffer
;%          (goto-char (org-element-property :begin ast))
;%          (if arg
;%              (org-entry-delete nil org-noter-property-note-location)
;%            (org-entry-put nil org-noter-property-note-location
;%                           (org-noter--pretty-print-location location))))))))
;%  (with-eval-after-load 'pdf-annot
;%    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;%;;;; pdf-tools-org-noter-helpers
;%(use-package pdf-tools-org-noter-helpers
;%  :straight (pdf-tools-org-noter-helpers :type git :host github :repo "analyticd/pdf-tools-org-noter-helpers"))
(message "Finished org-noter configuration.")
(message "Start org-pomodoro configuration.")
;;;; org-pomodoro
(use-package org-pomodoro
    :straight (org-pomodoro :type git :host github :repo "marcinkoziej/org-pomodoro")
    :commands  (org-pomodoro)
    :config
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))
;% add hook to enable automated start of the next pom after a break.
;% Source: https://github.com/marcinkoziej/org-pomodoro/issues/32
;% (add-hook 'org-pomodoro-break-finished-hook
;%           (lambda ()
;%             (interactive)
;%             (point-to-register 1)
;%             (org-clock-goto)
;%             (org-pomodoro '(25))
;%             (register-to-point 1)
;%             (shell-command-to-string "open -a tomighty.app")
;%             ))
(use-package sound-wav)
(setq org-pomodoro-ticking-sound-p nil)
(setq org-pomodoro-ticking-sound-states '(:pomodoro :short-break :long-break))
(setq org-pomodoro-ticking-sound-states '(:pomodoro))
(setq org-pomodoro-ticking-frequency 1)
(setq org-pomodoro-audio-player "mplayer")
(setq org-pomodoro-finished-sound-args "-volume 0.9")
(setq org-pomodoro-long-break-sound-args "-volume 0.9")
(setq org-pomodoro-short-break-sound-args "-volume 0.9")
(setq org-pomodoro-ticking-sound-args "-volume 0.3")
(global-set-key (kbd "C-c o") 'org-pomodoro)
(message "Finished org-pomodoros configuration.")
(message "Start configuration of org-ref.")
;;;; org-ref
;% Set the case of the Author and Title to Capitalize with customize.
(use-package org-ref
     :straight (org-ref :type git :host github :repo "jkitchin/org-ref")
     :init
    (use-package bibtex)
    (setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator ""
          bibtex-autokey-titleword-separator ""
          bibtex-autokey-titlewords 9
          bibtex-autokey-titlewords-stretch 9
          bibtex-autokey-titleword-length 15)
    ;% H is the hyper key. I have bound H to Fn. For the MacAlly keyboard, it is bound to right-command.
    (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
    ;% (use-package org-ref-ivy)
    (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
                org-ref-insert-cite-function 'org-ref-cite-insert-ivy
                org-ref-insert-label-function 'org-ref-insert-label-link
                org-ref-insert-ref-function 'org-ref-insert-ref-link
                org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
    ; (use-package org-ref-arxiv)
    ; (use-package org-ref-pubmed)
    ; (use-package org-ref-wos)
)
(message "Finished configuration of org-ref.")
(message "Start bibtex-completion-bibliography configuration.")
(setq bibtex-completion-bibliography '("/Users/blaine/Documents/global.bib")
    bibtex-completion-library-path '("/Users/blaine/0papersLabeled/" "/Users/blaine/0booksLabeled/")
    bibtex-completion-notes-path "/Users/blaine/org-roam/references/notes/"
    bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
    bibtex-completion-additional-search-fields '(keywords)
    bibtex-completion-display-formats
    '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
      (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
      (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
    bibtex-completion-pdf-open-function
    (lambda (fpath)
      (call-process "open" nil 0 nil fpath)))

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)
(message "Finished bibtex-completion-bibliography configuration.")
(message "Start org-roam configurations")
;;;; Basic org-roam config
(use-package org-roam
  :straight (org-roam 
             :type git 
             :host github 
             :repo "org-roam/org-roam")
  :init
  (setq org-roam-v2-ack t)  ;; If you're using Org-roam v2             
  :custom
  (org-roam-directory (file-truename "/Users/blaine/org-roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-id-get-create)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Ensure C-c n is a prefix key
  (define-prefix-command 'org-roam-prefix-map)
  (global-set-key (kbd "C-c n") 'org-roam-prefix-map)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (org-roam-ui-mode))
   ;% If using org-roam-protocol
   ;% (use-package org-roam-protocol
   ;%      :straight t))
;% Following https://jethrokuan.github.io/org-roam-guide/
(message "Finished org-roam configurations")
(message "Start org-roam-capture template configurations.")
; (setq org-roam-capture-templates
;       '(("p" "permanent" plain
;          "%?"
;          :if-new (file+head "main/${slug}.org" "#+title: ${title}\n\n* Note type: permanent\n\n* References\n\n* Backlinks\n\n#+created_at: %U\n#+last_modified: %U\n")
;          :immediate-finish t
;          :unnarrowed t)
;          ;; citar literature note
;         ;% ("n" "literature note" plain
;         ;%  "%?"
;         ;%  :target (file+head "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
;         ;%             "#+title: ${citar-citekey}.\n Article title: ${note-title}.\n Year: ${citar-year} \n  Keywords: ${citar-keywords} \n Note type: literature\n\n\n#+created: %U\n#+last_modified: %U\n\n")
;         ;%           :unnarrowed t)
;         ("r" "reference" plain "%?"
;          :if-new
;          (file+head "reference/${title}.org" "#+title: ${title}\n\n\n\n\n* References\n\n* Backlinks\n\n#+created_at: %U\n#+last_modified: %U\n")
;          :immediate-finish t
;          :unnarrowed t)
;          ("l" "clipboard" plain #'org-roam-capture--get-point "%i%a"
;          :file-name "%<%Y%m%d%H%M%S>-${slug}"
;          :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: %?"
;          :unnarrowed t
;          :prepend t
;          :jump-to-captured t)
;          ;% Vidianos G's config with ivy-bibtex
;          ("v" "bibliography reference" plain
;              "%?"
;              : if-new
;              (file+head "ref/${citekey}.org" "#+title: ${title}\n
;               ,#+filetags: ${entry-type}
;          - keywords :: ${keywords}
;          - tags ::
;
;          ,* Analysis of ${entry-type} by ${author}
;
;
;
;          * References\n\n* Backlinks\n\n#+created_at: %U\n#+last_modified: %U\n
;          :PROPERTIES:
;          :URL: ${Url}
;          :NOTER_DOCUMENT: ${file}
;          :NOTER_PAGE:
;          :END:")
;              :unnarrowed t
;              :jump-to-captured t)
;         ("b" "bibliography notes" plain             ; Org-noter integration
;           (file "~/org-roam/references/notes/notes-template.org")
;                  :target (file+head "references/notes/${citekey}.org"
;                  "#+title: ${title}\n :article:\n\n\n\n\n* References\n\n* Backlinks\n\n#+created_at: %U\n#+last_modified: %U\n")
;                   :empty-lines 1)
;         ("a" "article" plain "%?"
;          :if-new
;          (file+head "articles/${title}.org" "#+title: ${title}\n :article:\n\n\n\n\n* References\n\n* Backlinks\n\n#+created_at: %U\n#+last_modified: %U\n")
;          :immediate-finish t
;          :unnarrowed t)))
; (setq org-roam-node-display-template
;     (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;% Writing technical documents requires us to write in paragraphs,
;% whereas org mode by default is intended to be used as an outliner,
;% to get around this problem, setting up org-export to preserve line breaks is useful
;% (setq org-export-preserve-breaks t)
(message "Finished org-roam configurations.")
(message "Start org-roam-bibtex.")
; (use-package org-roam-bibtex
;   :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex")
;   :after org-roam
;   :config
;   (use-package org-ref)) ; optional: if using Org-ref v2 or v3 citation links
(use-package org-roam-bibtex
  :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex")    
  :after org-roam
  :config
  (use-package org-ref)) 
(message "Finished org-roam-bibtex.")
(message "Start toggle-state-at-point for use with images and equations.")
;% Place point on link to image. Left-click to display image in another buffer. Enter C-c t to display the code of the link for easy editing.
;% Place point on equation. Enter C-c t to render it with MathJax. Left click on the rendered equation to switch back to the code.
;% Put multiline code from mathpix between double dollar signs and treat as being on one line.
;% This trick does not work with the equation environment compressed to one line. You have to use M-x math-preview-region.
;% I modified this from https://emacs.stackexchange.com/questions/59151/how-can-i-switch-a-preview-image-in-an-org-mode-buffer-to-its-source-block
;% 
;% I ran out of time to determine how to render an active region. I need to find the analog of the latex-fragment:
;% ('latex-???? (math-preview-region))
;% ???? has to be some kind of an org-element-type. org-latex-section does not work.
;% This would enable using this application of the math-preview-region to render equation environments.
                                                          (defun bhmm/toggle-state-at-point ()
  (interactive)
  (let ((ctx (org-element-context)))
    (pcase (org-element-type ctx)
      ('link           (org-toggle-link-display))
      ('latex-fragment (math-preview-at-point)))))

;% (define-key org-mode-map (kbd "C-c t") #'bhmm/toggle-state-at-point)
(message "End toggle-state-at-point for use with images and equations.")
(message "Start ox-typst.")
(use-package ox-typst
  :straight (ox-typst :type git :host github :repo "jmpunkt/ox-typst")
  :after org)
(message "Finished ox-typst.")
(message "Finished O package configurations")

(message "Start P package configurations")
;;; P
;;;; pdf-tools
;%  Emacs has a built-in DocView package that can display PDFs but it is not as versatile as pdf-tools.
;%  You will likely by prompted to run pdf-tools-install to compile it.
;%  It is needed to be able to annotate PDF from inside Emacs.
;%  I have not gotten that far.
;% (use-package pdf-tools
;% :straight t
;% :pin manual ;; manually update
;% :config
;% ;; initialise
;% (pdf-tools-install)
;% ;; open pdfs scaled to fit width
;% (setq-default pdf-view-display-size 'fit-width)
;% ;; use normal isearch
;% (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;% :custom
;% (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;% source https://github.com/malb/emacs.d/blob/master/malb.org#pdf-viewer
;% Learned about this from Julien during Berln Emacs Meetup Nev. 27, 2024.                        
(use-package pdf-tools
   :straight         
   :magic ("%PDF" . pdf-view-mode)
   :after (org tex)
   :config (progn
             (pdf-tools-install)
             (setq-default pdf-view-display-size 'fit-page)

             (require 'pdf-annot)

             (setq pdf-annot-default-annotation-properties
                   `((t (label . "M"))
                     (text (icon . "Note")
                           (color . "#dc322f"))
                     (highlight (color . "#fef3d5"))
                     (squiggly (color . "#dc322f"))
                     (strike-out(color . "#dc322f"))
                     (underline (color . "#268bd2"))))

             (setq pdf-view-resize-factor 1.1
                   pdf-annot-activate-created-annotations t
                   pdf-view-midnight-invert nil
                   pdf-misc-print-programm "gtklp")

             (add-hook 'pdf-annot-list-mode-hook #'pdf-annot-list-follow-minor-mode)

             (defun malb/pdf-extract-table (&optional format)
               (let* ((format- (upcase (or format "CSV")))
                      (pdf-filename (buffer-file-name))
                      (txt-filename (make-temp-name "/tmp/tabula-"))
                      (buffer (generate-new-buffer
                               (generate-new-buffer-name (format "*tabular<%s>*"
                                                                 (file-name-base pdf-filename))))))
                 (shell-command (format "java -jar %s -f %s -p %d -o \"%s\" \"%s\""
                                        malb/tabular-jar
                                        format-
                                        (pdf-view-current-page)
                                        txt-filename
                                        pdf-filename)
                                nil)
                 (pop-to-buffer buffer)
                 (insert-file-contents txt-filename)
                 (cond
                  ((eq format nil) (progn
                                     (org-mode)
                                     (call-interactively 'mark-whole-buffer)
                                     (call-interactively 'org-table-convert-region)))
                  ((string-equal format "JSON") (progn
                                                  (json-mode)
                                                  (json-pretty-print-buffer))))
                 (delete-file txt-filename)))

             (defun malb/pdf-extract-text ()
               (let* ((pdf-filename (buffer-file-name))
                      (txt-filename (make-temp-name "/tmp/tabula-"))
                      (buffer (generate-new-buffer
                               (generate-new-buffer-name (format "*pdftotext<%s>*"
                                                                 (file-name-base pdf-filename))))))
                 (shell-command (format "pdftotext -layout -nopgbrk \"%s\" \"%s\""
                                        pdf-filename txt-filename)
                                nil)
                 (pop-to-buffer buffer)
                 (insert-file-contents txt-filename)
                 (delete-file txt-filename)))

             (defun malb/pdf-view-llncs-from-bounding-box (arg &optional window)
               "Set the height from the page's bounding-box."
               (interactive "P")
               (let* ((bb (pdf-cache-boundingbox (pdf-view-current-page window)))
                      (h-margin (max (if arg 0.35 0.28) (or pdf-view-bounding-box-margin 0.0)))
                      (w-margin (max 0.05 (or pdf-view-bounding-box-margin 0.0)))
                      (slice (list (- (nth 0 bb)
                                      (/ h-margin 2.0))
                                   (- (nth 1 bb)
                                      (/ w-margin 2.0))
                                   (+ (- (nth 2 bb) (nth 0 bb))
                                      h-margin)
                                   (+ (- (nth 3 bb) (nth 1 bb))
                                      w-margin))))
                 (apply 'pdf-view-set-slice
                        (append slice (and window (list window))))))

             (defun pdf-tools-org-edges-to-region (edges)
               "Attempt to get 4-entry region \(LEFT TOP RIGHT BOTTOM\) from several EDGES.
 We need this to import annotations and to get marked-up text, because
 annotations are referenced by its edges, but functions for these tasks
 need region."
               (let ((left0 (nth 0 (car edges)))
                     (top0 (nth 1 (car edges)))
                     (bottom0 (nth 3 (car edges)))
                     (top1 (nth 1 (car (last edges))))
                     (right1 (nth 2 (car (last edges))))
                     (bottom1 (nth 3 (car (last edges))))
                     (n (safe-length edges)))
                 ;; we try to guess the line height to move
                 ;; the region away from the boundary and
                 ;; avoid double lines
                 (list left0
                       (+ top0 (/ (- bottom0 top0) 3))
                       right1
                       (- bottom1 (/ (- bottom1 top1) 3)))))

             (defun malb/pdf-annot-export-as-org (compact)
               "Export annotations to Org Buffer."
               (interactive "P")
               (let* ((annots (sort (pdf-annot-getannots) 'pdf-annot-compare-annotations))
                      (source-buffer (current-buffer))
                      (source-buffer-name (file-name-sans-extension (buffer-name)))
                      (source-file-name (buffer-file-name source-buffer))
                      (target-buffer-name (format "*Notes for %s*" source-buffer-name))
                      (target-buffer (get-buffer-create target-buffer-name)))

                 (with-current-buffer target-buffer
                   (org-mode)
                   (erase-buffer)
                   (visual-fill-column-mode)

                   (insert (format "#+title: Notes for %s\n" source-buffer-name))
                   (insert (format "#+startup: indent\n\n"))
                   (insert (format "source: [[%s][%s]]\n\n" source-file-name source-buffer))

                   (mapc (lambda (annot) ;; traverse all annotations
                           (progn
                             (let ((page (cdr (assoc 'page annot)))
                                   (highlighted-text
                                    (if (pdf-annot-get annot 'markup-edges)
                                        (let ((highlighted-text
                                               (with-current-buffer source-buffer
                                                 (pdf-info-gettext (pdf-annot-get annot 'page)
                                                                   (pdf-tools-org-edges-to-region
                                                                    (pdf-annot-get annot 'markup-edges))))))
                                          (replace-regexp-in-string "\n" " " highlighted-text))
                                      nil))
                                   (note (pdf-annot-get annot 'contents)))

                               (when (or highlighted-text (> (length note) 0))
                                 (insert (if compact "- " "* "))
                                 (insert (format "page %s" page))

                                 (when highlighted-text
                                   (insert (if compact (format ": “%s” " highlighted-text)
                                             (concat "\n\n#+begin_quote\n"
                                                     highlighted-text
                                                     "\n#+end_quote"))))
                                 (if (> (length note) 0)
                                     (insert (if compact (format " %s\n" note)
                                               (format "\n\n%s\n\n" note)))
                                   (insert (if compact "\n" "\n\n")))))))
                         (cl-remove-if
                          (lambda (annot) (member (pdf-annot-get-type annot) (list 'link)))
                          annots))
                   )
                 (pop-to-buffer target-buffer '(display-buffer-pop-up-window))))

             (defun malb/pdf-annot-export-as-md (compact)
               "Export annotations to Makrdown buffer."
               (interactive "P")
               (let* ((annots (sort (pdf-annot-getannots) 'pdf-annot-compare-annotations))
                      (source-buffer (current-buffer))
                      (source-buffer-name (file-name-sans-extension (buffer-name)))
                      (source-file-name (buffer-file-name source-buffer))
                      (target-buffer-name (format "*Notes for %s*" source-buffer-name))
                      (target-buffer (get-buffer-create target-buffer-name)))
                 (with-current-buffer target-buffer
                   (markdown-mode)
                   (erase-buffer)
                   (visual-fill-column-mode)

                   (insert (format "---\ntitle: Notes for %s\n---\n\n" source-buffer-name))
                   (insert (format "source: [%s](%s)\n\n" source-buffer source-file-name))
                   (mapc (lambda (annot) ;; traverse all annotations
                           (progn
                             (let ((page (cdr (assoc 'page annot)))
                                   (highlighted-text
                                    (if (pdf-annot-get annot 'markup-edges)
                                        (let ((highlighted-text
                                               (with-current-buffer source-buffer
                                                 (pdf-info-gettext (pdf-annot-get annot 'page)
                                                                   (pdf-tools-org-edges-to-region
                                                                    (pdf-annot-get annot 'markup-edges))))))
                                          (replace-regexp-in-string "\n" " " highlighted-text))
                                      nil))
                                   (note (pdf-annot-get annot 'contents)))

                               (when (or highlighted-text (> (length note) 0))
                                 (insert (if compact "- " "On "))
                                 (insert (format "page %s" page))

                                 (when highlighted-text
                                   (insert (if compact (format ": “%s” " highlighted-text)
                                             (concat ":  \n> "
                                                     (replace-regexp-in-string "\n" "\n> " highlighted-text)
                                                     "\n"))))
                                 (if (> (length note) 0)
                                     (insert (if compact (format " %s\n" note)
                                               (format "\n\n%s\n\n" note)))
                                   (insert (if compact "\n" "\n\n")))))))
                         (cl-remove-if
                          (lambda (annot) (member (pdf-annot-get-type annot) (list 'link)))
                          annots)))
                 (pop-to-buffer target-buffer '(display-buffer-pop-up-window))))

             (defhydra malb/hydra-pdf-extract (:color blue)
               "
 Org:       _o_ compact  _O_ normal     _t_ table
 Markdown:  _m_ compact  _M_ normal
 Other:     _p_ plain    _c_ csv table  _j_ json table _x_ ocr
 "
               ("o" (lambda () (interactive) (malb/pdf-annot-export-as-org 1)))
               ("O" malb/pdf-annot-export-as-org)
               ("m" (lambda () (interactive) (malb/pdf-annot-export-as-md  1)))
               ("M" malb/pdf-annot-export-as-md)
               ("c" (lambda () (interactive) (malb/pdf-extract-table "CSV")))
               ("j" (lambda () (interactive) (malb/pdf-extract-table "JSON")))
               ("t" (lambda () (interactive) (malb/pdf-extract-table)))
               ("p" (lambda () (interactive) (malb/pdf-extract-text)))
               ("x" (lambda () (interactive) (start-process (format "ocr %s" buffer-file-name)
                                                            nil "ocrmypdf" buffer-file-name buffer-file-name)))
               ("q" nil "cancel"))

             (bind-key "s h" #'malb/pdf-view-llncs-from-bounding-box pdf-view-mode-map)
             (bind-key "D" #'dedicated-mode pdf-view-mode-map)
             (bind-key "x" #'malb/hydra-pdf-extract/body pdf-view-mode-map)

             (defun malb/pdf-annot-move (forward)
               (let ((annot-list (with-current-buffer
                                     (pdf-annot-get-buffer pdf-annot-edit-contents--annotation)
                                   pdf-annot-list-buffer)))
                 (if annot-list
                     (progn
                       (pdf-annot-edit-contents-commit)
                       (if forward
                           (call-interactively 'tablist-next-line)
                         (call-interactively 'tablist-previous-line))
                       (call-interactively 'tablist-find-entry))
                   (let ((this nil)
                         (next nil)
                         (annotations
                          (sort (pdf-annot-getannots
                                 nil nil
                                 (cdar pdf-annot-edit-contents--annotation))
                                'pdf-annot-compare-annotations)))
                     (dolist (annot (if forward annotations (reverse annotations)))
                       (when (equal this t)
                         (setq next annot)
                         (setq this nil))
                       (when (equal (pdf-annot-get-id annot)
                                    (pdf-annot-get-id pdf-annot-edit-contents--annotation))
                         (setq this t)))
                     (pdf-annot-edit-contents-finalize t)
                     (when next
                       (pdf-view-goto-page (pdf-annot-get next 'page))
                       (pdf-annot-edit-contents next))))))

             (defun malb/pdf-annot-next ()
               (interactive)
               (malb/pdf-annot-move t))

             (defun malb/pdf-annot-prev ()
               (interactive)
               (malb/pdf-annot-move nil))

             (defun malb/pdf-annot-yank-highlight ()
               (interactive)
               (let* ((a pdf-annot-edit-contents--annotation)
                      (text
                       (replace-regexp-in-string
                        "\n" " "
                        (pdf-info-gettext
                         (pdf-annot-get a 'page)
                         (pdf-tools-org-edges-to-region (pdf-annot-get a 'markup-edges))
                         0
                         (pdf-annot-get a 'buffer)))))
                 (insert (format "\"%s\"" text))))

             (bind-key "C-c C-n" 'malb/pdf-annot-next pdf-annot-edit-contents-minor-mode-map)
             (bind-key "C-c C-p" 'malb/pdf-annot-prev pdf-annot-edit-contents-minor-mode-map)
             (bind-key "C-c C-y" 'malb/pdf-annot-yank-highlight pdf-annot-edit-contents-minor-mode-map)

             (defun malb/do-to-pdf-view-buffer (fn)
               (let ((cur-window (get-buffer-window)))
                 (dolist (window (window-list))
                   (let ((buffer (window-buffer window)))
                     (with-current-buffer buffer
                       (when (eq major-mode 'pdf-view-mode)
                         (select-window window)
                         (call-interactively fn)
                         (select-window cur-window)
                         ))))))

             (defun malb/other-pdf-view-next-page ()
               (interactive)
               (malb/do-to-pdf-view-buffer #'pdf-view-next-page))

             (defun malb/other-pdf-view-prev-page ()
               (interactive)
               (malb/do-to-pdf-view-buffer #'pdf-view-previous-page))

             (bind-key "C->" #'malb/other-pdf-view-next-page org-mode-map)
             (bind-key "C-<" #'malb/other-pdf-view-prev-page org-mode-map)
             (bind-key "C->" #'malb/other-pdf-view-next-page LaTeX-mode-map)
             (bind-key "C-<" #'malb/other-pdf-view-prev-page LaTeX-mode-map)))
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
   
(use-package sqlite3
      :straight t)
      
(defun export-csv-to-qiterm (csv-file db-file table-name)
  "Export selected rows from a CSV file to an SQLite database."
  (interactive "fCSV file: \nfSQLite DB file: \nsTable name: ")
  (let ((db (sqlite3-open db-file))
        (rows (with-temp-buffer
                (insert-file-contents csv-file)
                (split-string (buffer-string) "\n" t))))
    (dolist (row rows)
      (let ((values (split-string row ",")))
        (sqlite3-exec db
                      (format "INSERT INTO %s VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');"
                              table-name
                              (nth 0 values)
                              (nth 1 values)
                              (nth 2 values)
                              (nth 3 values)
                              (nth 4 values)
                              (nth 5 values)
                              (nth 6 values)                              
                              (nth 7 values)))))
    (sqlite3-close db)
    (message "Data successfully appended to %s" table-name)))

;; Usage example:
;; (export-csv-to-qiterm "~/6233iterm/qiterm.csv" "~/6233iterm/qiterm.db" "qiterm")   
    
(message "Finished S package configurations.")


(message "Start T package configurations.")
;;; T
;;;; tmux-pane
(use-package tmux-pane
   :straight (tmux-pane :type git :host github :repo "laishulu/emacs-tmux-pane"))
   
   
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
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key))

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


(message "Start W package configurations")
;;;; wc-mode
;% Add the path to the repo
(straight-use-package
   '(wc-mode :type git :local-repo "~/e29fewpackages/manual-install/wc-mode"))
(global-set-key "\C-cw" 'wc-mode)
(wc-mode 1)
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
 '(copilot-network-proxy '(:host "\"127.0.0.1\"" :port 7890))
 '(copilot-node-executable "/usr/local/bin/node")
(message "Start custom set-variables")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-battery-mode t)
 '(global-display-line-numbers-mode t)
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
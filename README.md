![Version](https://img.shields.io/static/v1?label=dsw-2024-org-mode-init&message=0.5&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# Emacs configuration file for writing research papers in Org-mode 

This configuration file accompanies the 2024 November 22 Oklahoma Data Science Workshop meeting.
The `init.org` version of the `init.el` file is rendered nicely by GitHub.

This configuration file, `init.el`, focuses on writing research papers in org-mode in `GNU Emacs`.
It was meant to use default settings, but I included packages I enjoy using (e.g., treemacs, undo-tree, vertico, marginalia, citar, yasnippets, pdf-tools).
It includes `company` for autocompletion.
It uses `lsp-mode` instead of eglot.
Uses James Stoup's fonts for org-mode.
It includes custom functions to make it easier to work in Org Mode.
For example, `C-c t` captures a thought and adds it as a todo item to the bottom of the TODO list in the selected logfile. The keybinding `C-c r` is used to first select the target log file . 
The configuration uses straight and use-package for package management.

After my talk on November 22, I made the following enhancements:

- Added copilot.el. You will need a GitHub token to use it. It can get in the way when writing prose and make the cursor jump to unintended places so use with caution.
- Added org-roam, org-ref, citar-org-roam, org-roam-bibtex, and org-roam-bibtex - citar integration. (Org-ref is used to add bibtex entries to the bibliography by pasting the DOI into the buffer. Citar is used to insert the citations and generate the bibliography. Org-roam-bibtex is used to search for bibtex entries in the bibliography. Org-roam-bibtex - citar is used to insert the citations and generate the bibliography in org-roam.)
- Moved the handling of org-roam capture notes to citar.
- Added the faster `jinx` package for spell checking (use right mouse button to select a correction or to add a term to your personal library).
- Added a function to open a bibNotes.org file in ~/bibNotes that is renamed after the citekey that the cursor is sitting on for the annotated bibliography. The entries in the main org file of the annotated bibliography
- Added a function to open the log file in the current directory and place the cursor at line above the headline with restart-here tag. This is where you make the next daily entry.
- Added wc-count-mode for displaying the new words added on the modeline.
- Added the markdown package to support the editing of README.md files that you might edit for private GitHub repositories or Codeberg repositories where you store your manuscript file. Added functions to support the moving of items in a list up and down with M-<up> abd M-<down>.
- Added the overlooked package orderless for enhanced searching of command in the mini-buffer.


It should give the new user a rich enough experience to suppress their urge to add new packages and break their configuration.

## Prerequisite
GNU Emacs only. I am using version 29.4 compiled from source code. It should work with versions 30 and 31. It may work with older versions.

## Installation
Download and move the init.el file to your Emacs configuration folder (e.g., ~/e29fewpackages).

## Usage example
Start emacs with the init-directory flag pointing to your Emacs configuration folder.

```bash
/Applications/Emacs29.4.app/Contents/MacOS/Emacs --init-directory ~/e29fewpackages --debug-init
```

## Notes

- Give the Emacs several minutes to install about 60 packages during startup.
- Startup time will be about 10 seconds in future startups.
- If commented out three packages that require customized file paths. Edit these to suit your workflow.
- I map the above command to the zsh alias `e29f`. 
```bash
alias ef='/Applications/Emacs29.4.app/Contents/MacOS/Emacs --init-directory ~/e29fewpackages --debug-init'
```

## Improved usage with alias

I use `e29f +500 logXXXX.org` to open `logXXXX.org` in the current directory on startup and to place the cursor at line 500.


## Enhance your editing experience by using the Emacs server

Enter `M-x server-start' in a Emacs GUI session to start the Emacs server.
Then you can open instantly another instance of the same session in another terminal window by using the following alias.

```bash
alias efmxc='/Applications/Emacs29.4.app/Contents/MacOS/bin/emacsclient29 -c'
```

Use it by entering `efmxc filename.org`.

You may into trouble if another Emacs server is running.
Kill it with the following command.
The `-s` option is for the name of the server.

```bash
alias efk="/Applications/Emacs29.4.app/Contents/MacOS/bin/emacsclient29 -s ef29server --eval '(kill-emacs)'"
```

The same opened buffers will be present.
Enter `C-x #` to close a file and close that instance of the server.
The emacslient makee Emacs more useful for short edits where Neovim or Textmate might otherwise be used due to their fast startup times.



  
## Update history

|Version      | Changes                                                                                                                                 | Date              |
|:------------|:----------------------------------------------------------------------------------------------------------------------------------------|:------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                             | 2024 November 10  |
| Version 0.2 |   Minor edits                                                                                                                                                      | 2024 November 22  |
| Version 0.3 |   Add init file in org-mode format for rendering by GitHub.                                                                               | 2024 November 25  |
| Version 0.4 |   Updated the init.el file with org-roam, org-ref, citar, org-roam-bibtex, and org-roam-bibtex - citar integration. Added a function to open bibNotes.org in ~/bibNotes for the annotated bibliography. This should complete the tool chain. | 2024 November 26  |
| Version 0.5 |   Updated init.el file to specify path to texlive.                                                                                                   | 2024 November 30  |

## Acknowledgments
### Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)

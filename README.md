![Version](https://img.shields.io/static/v1?label=dsw-2024-org-mode-init&message=0.1&color=brightcolor)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

# Emacs configuration file for writing research papers in Org-mode 

This presentation is for the 2024 November 22 Oklahoma Data Science Workshop meeting.

This configuration file, `init.el`, focuses on writing research papers in org-mode in `GNU Emacs`.
It was meant to use default settings, but I included packages I enjoy using (e.g., treemacs, undo-tree, vertico, marginalia, citar, yasnippets, pdf-tools).
It includes `company` for autocompletion.
It uses `lsp-mode` instead of eglot.
Uses James Stoup's fonts for org-mode.
It includes custom functions to make it easier to work in Org Mode.
The uses straight for package management.
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

  
## Update history

|Version      | Changes                                                                                                                                 | Date              |
|:------------|:----------------------------------------------------------------------------------------------------------------------------------------|:------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                             | 2024 November 10  |
| Version 0.2 |   Minor edits                                                                                                                           | 2024 November 22  |

## Acknowledgments
### Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)

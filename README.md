## Introduction

My personal Emacs configuration. It has the above notable features:

- *Fast*: Bootstraps in less than a minute and starts in less than 0.2s on my apple silicon.
- *Small*: Its core is in `init.el` which has currently less than 300 lines of code and installs less than 20 packages.
- *Simple*: No nasty hacks. Close to vanilla.
- *Advanced*: Autoload, CJK, environment variables, fonts, and more.

An Emacs 30 with treesit support is required.

## Install

```sh
git clone git@github.com:RadioNoiseE/emacs.git $HOME/.emacs.d
```

To install all language grammar for treesitter, evaluate:

```elisp
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```

## Note

You may want to modify `package-archive` as it uses mirror sites from Tsinghua University.

## Introduction

An Emacs 30 compiled with Treesit and XWidget support is assumed. The latest Emacs from IGC/MPS branch is recommended.
If you are on UNIX platform, the script `ebuild.sh` can be used to build Emacs with IGC/MPS features.

This is personal Emacs configuration, without higher-level abstraction.

## Install

```sh
git clone --depth 1 https://github.com/RadioNoiseE/emacs $HOME/.emacs.d
```

To install all language grammar for treesitter, evaluate:

```elisp
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
```

## Misc

It comes with additional Plan9 mk, TECO and RelaxNG Compact support by LdBeth, as well as WEB support by Don Knuth and major mode for J.
The support for OCaML, Swift, TeX, CommonLisp, Coq won't be activated unless you have their compiler installed.

EWW is patched in a way so that it can render XML with XSL stylesheets. WanderLust is modified so that it renders HTML mail using XWidget.

Some XML related stuff are under `schema` which you can safely remove with the nXML package.

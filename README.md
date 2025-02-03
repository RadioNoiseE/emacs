## Introduction

An Emacs 30 compiled with treesit support is required. The latest Emacs from IGC/MPS branch is recommended.

If you are on UNIX platform, there is a script `ebuild.sh` which you can use to make Emacs.

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

Some XML related stuff are under `schema` which you can safely remove.

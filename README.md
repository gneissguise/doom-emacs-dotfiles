# doom-emacs-dotfiles

---

## Intro

This repository contains the `.el` files for my [DooM Emacs](https://github.com/hlissner/doom-emacs) configuration.  My current workflow is for learning Clojure, so everything is configured with that in mind.. Although I would love to branch and learn Org Mode one day.    

To see more advanced configurations, consider:
- [zzamboni/dot-doom](https://github.com/zzamboni/dot-doom)
- [tecosaur/emacs-config](https://github.com/tecosaur/emacs-config)

**Notice**

>I'm mainly using the packages provided by the Doom installation with the exception of: [iedit](https://github.com/victorhge/iedit).

packages.el
```elisp
(package! iedit)
```

config.el
```elisp
(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))
```

**Notice**

>I installed `clojure-lsp` separately as suggested by the following blog post: https://www.ianjones.us/clojure-development-in-emacs

## Contents

```sh
$ tree
.
├── config.el
├── init.el
├── LICENSE
├── packages.el
└── README.md

0 directories, 5 files
```

## Prereqs
* Emacs
* Doom Emacs
* [clojure-lsp](https://github.com/clojure-lsp/clojure-lsp)

## Installation
```sh
$ wget https://raw.githubusercontent.com/gneissguise/doom-emacs-dotfiles/main/{config,init,packages}.el -P ~/.doom.d
```

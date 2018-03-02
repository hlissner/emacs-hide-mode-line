[![MELPA](http://melpa.org/packages/hide-mode-line-badge.svg?style=flat-square)](http://melpa.org/#/hide-mode-line)
[![MIT](https://img.shields.io/badge/license-MIT-green.svg?style=flat-square)](./LICENSE)

# hide-mode-line

Provides `hide-mode-line-mode`. A minor mode that hides (or masks) the mode-line
in your current buffer. It can be used to toggle an alternative mode-line,
toggle its visibility, or simply disable the mode-line in buffers where it isn't
very useful otherwise.

> Extracted from `doom-hide-modeline-mode` in [Doom Emacs][doom].

## Install

`hide-mode-line` is available on MELPA.

`M-x package-install hide-mode-line`

```emacs-lisp
(require 'hide-mode-line)
```

Add `hide-mode-line-mode` to hooks where you don't want a mode-line, like the
completion-list or org todo/agenda-popup windows, or neotree.

```emacs-lisp
(add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
(add-hook 'neotree-mode-hook #'hide-mode-line-mode)
```

Or replace the mode-line in specific windows:

```emacs-lisp
(let ((hide-mode-line-format '("%b")))
  (hide-mode-line-mode +1))

(setq-local hide-mode-line-format '("%b"))
(hide-mode-line-mode +1)
```


[doom]: https://github.com/hlissner/doom-emacs

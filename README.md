liquid-tip.el
=============

Interactive popup to show liquid types in Emacs Haskell-mode.

Fallback on hdevtools, if no liquid type data is found.


### Debugging

Make sure `hdevtools` is in the search path `exec-path`:

`(setq exec-path (append exec-path '("<path-to-hdevtools>")))`

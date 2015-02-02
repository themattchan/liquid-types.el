liquid-tip.el
=============

Interactive popup to inspect types in Emacs Haskell-mode.

Make sure `hdevtools` is in the search path `exec-path`:

`(setq exec-path (append exec-path '("<path-to-hdevtools>")))`


TODO:

0. make hdevtools/type-info-just-str take a pos like everything else,
might require significant rewrite

1. add an auto-clean function for the .liquid directory so it doesn't
   leave garbage all over the place

2. customize.el settings for liquid-tip

3. refactor/rewrite hdevtools.el
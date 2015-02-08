TODO:

0. make hdevtools/type-info-just-str take a pos like everything else,
might require significant rewrite

0. ~~Add customize.el settings for liquid-tip. Most importantly, allow user to
select the button lock binding.~~

0. One click install from MELPA, at most adding `(require 'liquid-tip)` to init.el.

0. ~~refactor/rewrite hdevtools.el~~ leave as liquid-hdevtools.el for now

0. Add check for `liquid` and `hdevtools` executable.

0. Refactor overloaded variable MODE -- refers to the popup style and also
   syntax checker `'flycheck`. Relabel the latter to a defcustom of
   `liquid-checker` or something instead, and stop threading it around.

0. Refine error reporting, if liquid gets stuck the very helpful message "No
   information for \<identifier\>" is printed. Instead it should display the
   location of the error/failure.

0. Does not show the same type info for multiple patterns in function defs --
   the first pattern gets a type, subsequent patterns get "not found".
   Don't even know if this is possible given the format of the json file

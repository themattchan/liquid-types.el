liquid-types.el
===============

Error reporting (via flycheck) and type display (via pos-tip) for 

+ [liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell)

Requirements
------------

+ `flycheck`
+ `haskell-mode`
+ `pos-tip`
+ `thing-at-pt`
+ `button-lock`
+ `flycheck-color-mode-line`

Install
-------

Suppose that your emacs `'load-path` includes `~/.emacs.d/`

~~~~~
(add-to-list 'load-path "~/.emacs.d/")
~~~~~

*Step 1* Grab the various mode files:

~~~~~
cd ~/.emacs.d
wget https://raw.githubusercontent.com/ucsd-progsys/liquid-tip/master/flycheck-liquid.el
wget https://raw.githubusercontent.com/ucsd-progsys/liquid-tip/master/liquid-tip.el
~~~~~

*Step 2* Add the following to your `init.el` or equivalent:

~~~~~
;; ----------------------- Configure Flycheck ------------------

(require 'flycheck)

;; Global Flycheck
(global-flycheck-mode)

;; Rerun check on idle and save
(setq flycheck-check-syntax-automatically 
      '(mode-enabled idle-change save))

;; ----------------------- Configure LiquidHaskell -------------

(require 'haskell-mode)
(require 'flycheck-liquid)
(require 'liquid-tip)

(add-hook 'haskell-mode-hook 
	  '(lambda () (flycheck-select-checker 'haskell-liquid)))
(add-hook 'haskell-mode-hook  
	  (lambda () (liquid-tip-init 'ascii)))
(add-hook 'literate-haskell-mode-hook 
	  '(lambda () (flycheck-select-checker 'haskell-liquid)))
(add-hook 'literate-haskell-mode-hook  
	  (lambda () (liquid-tip-init 'ascii)))
~~~~~

*Optionally* You can configure flycheck to highlight errors in red:

~~~~~
;; ------------------- Flycheck Customization ----------------------

(require 'flycheck-color-mode-line)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(set-face-attribute 'flycheck-error nil
                    :foreground "red"
	         	    :background "pink")
~~~~~



Disable
-------

To disable flycheck-liquid on a particular file, add:

    -- Local Variables:
    -- flycheck-disabled-checkers: (haskell-liquid)
    -- End:

at the end of the file.




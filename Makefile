# dont need this either if installing with Emacsn
.PHONY: install
install:
	ln -s .emacs $(HOME)/.emacs
	ln -s elisp $(HOME)/elisp

# no longer needed really
# At least Arch installs it properly now.
.PHONY: mu4e
mu4e:
	cp -r ~/.cache/yay/mu-git/src/mu/mu4e $(HOME)/elisp/extensions/

# get an example mbsync rc
.PHONY: mbsync
mbsync:
	ln -s mbsyncrc ~/.mbsyncrc

all: install mu4e mbsync

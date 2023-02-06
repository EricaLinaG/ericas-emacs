
# My emacs setup. 

An old school emacs setup that uses packages. 
no _use-package_, just elpa, some config files and some extensions
outside of elpa.

## Installation

I use Emacsn now with Chemacs2. I wouldn't want to manage my emacs the hard way again.

### Emacsn - Chemacs2

Go to the [Emacsn](https://github.com/ericalinag/Emacsn) repo and follow the directions there.

Using [Chemacs2](https://github.com/plexus/chemacs2) as an emacs bootloader gives lots of possibilities.

Chemacs allows Emacsn to create dev, stable, test, for a default configuration 
and also have other Emacs configurations installed simultaneously.
With multiple profiles to execute the emacs installs in different ways.

For manintenance, Emacsn provides these commands. 
  - `make <profile-name>-update` etc.
    - `make stable-update` in ~/Emacsn will update your stable install.
    - `make test-update` in ~/Emacsn will update your test install.
    - `make doom-update` in ~/Emacsn will update your doom install.

  - `make test-install`  in ~/Emacsn will create a fresh test install of whatever the default 
      configuration has in github.

### Clone -> ~/.emacs.d   The hard way ???

You can just clone it into your *.emacs.d* directory.

      `git clone https://github.com/erica-linag/ericas-emacs.git ~/.emacs.d`

The following scripts can be run to either install or update all of the packages.
The only difference is that install ignores packages that are already installed.
I think it makes very little difference in load time. The update script adds
a `git pull origin main` before looking for updates.

The nice thing about these
is they run from the command line and are done. They will also work just fine
with a fresh vanilla install. The caveat is they need --chdir, or to be in their
proper folder upon execution.

      `emacs --script install.el --chdir ~/.emacs.d`
      or
      `emacs --script update.el  --chdir ~/.emacs.d`
      
or you can just run it the first time and wait for the packages to load.

## Emacs packages, some explanation of what is here.

There are configurations and also extensions which I wrote or 
borrowed from someone else that aren't available
as packages. Initialization happens in init.el_

The package list is in _early-packages/mypackages.el_.

## The big things; 

- [Evil-mode](https://github.com/emacs-evil/evil)(which can be easily switched off in _elisp/init.el_).
- [Modus themes.](https://github.com/protesilaos/modus-themes)
- [Lots of Hydras.](https://github.com/abo-abo/hydra)
- [Org](https://orgmode.org/)
- [Mu4e](https://www.emacswiki.org/emacs/mu4e)
- [Vertico](https://github.com/minad/vertico)
- [PosFrame](https://github.com/tumashu/posframe)
- [Dashboard](https://github.com/emacs-dashboard/emacs-dashboard)
- [undo-tree](https://www.emacswiki.org/emacs/UndoTree)

I've considered [multiple cursors](https://github.com/magnars/multiple-cursors.el) 
but have not yet tried them out.

I program lots of languages, these are the main ones. They may or may not
have extended configurations from default. 

- Languages:
  - C, C++
  - Clojure
  - Clojure script
  - Python
  - Lua
  - Haskell
  - Lisps - Scheme, Guile, Rackett, etc.
  - markdown
  - shell
  - org
  
   
#### Ivy, Ido, Helm, Vertico, etc.

I've used pretty much all the helpers over the years, ivy, ido, smex, helm, vertico.
Currently using vertico. Configurations are still there for the others.

## Key Files

Look in elisp/:
  - early-packages - Folder for early loading. lists all packages to load

  - config/ - Folder where all the real setup goes.
  - extensions/ - Folder where non-elpa custom code goes.

  - config/vars.el - miscellaneous variable setting.
  - config/keys.el - key bindings, mostly F keys.
  - config/evil-leader.el - more key bindings, vi style.

Look in _packages.el_ if I happen to add a package through _package-install_ 
I then go add it to _packages.el_ so I won't forget and configuration is repeatable. 
If I forget, the next fresh install will likely fail with package not installed.

## Natural Languages

I have been studying French for the last few years.
Now studying Italian. I am working on replacing Anki with Org drill in my routine.

I can also see that I'll probably want to add another language or two in the 
future. So I've been working to tap into the language capabilities of
emacs. I have a nice function to switch between input methods and dictionaries.
Ispell, flyspell, and hunspell are all working together for spell checking.
Google translate is there for highlighted text, current word, or sentence at point
and Language Tool is there to check grammar. Take a look at the language sub-menu
in _evil-leader-conf.el_ even if you are going to turn off evil-mode. Check out
_elisp/extensions/language.el_ and _elisp/config/lang-config.el_ and 
_google-translate-conf.el_

## Mu4e - Mail

_Mu4e_ I use mu4e for email. I can't imagine a better email client. There is a
reasonably basic mu4e configuration with multiple contexts. There is a sample mbsyncrc
file that can be used to configure _isync/mbsync_.  

This is a bit easier now than it used to be. Arch Linux seems to install it properly
when _mu_ is installed with pacman.

I cannot speak about other distributions or OS'.

## Key mappings

I didn't mess with key mappings except for F keys. 
My <leader> key is currently __,__ but I am looking to change to __SPC__ and also to
do something similar to doom-emacs and spacemacs so that everything is available 
outside of evil-mode.

I have an extensive menu system on Evil-leader which allows for __,w__ for write, 
__,q__ delete-buffer, etc. the entire Hydra subsystem is available at <leader> h.

I use _which-key-posframe_ which is almost like hydra with all the submenus. 

Mostly, the key mappings I added are non-intrusive.  It is definitely a
good idea to go read _config/evil-leader-conf.el_ whether you want _Evil_ key
bindings or not. It will give you a good idea of functionality to look for or map
to your own keys in _keys.el_

# Additional packages needed

See my [arch-pkgs repo](http://github.com/ericalinag/arch-pkgs) for an easy way to install
everything you need.

 * For email
    * mu-git - on Arch linux
      * (install mu/mu-git/mu4e)[https://www.djcbsoftware.nl/code/mu/mu4e/Installation.html#Installation]
        or maybe just do a `yay -S mu-git`.
    * isync (mbsync) 
    * `make mbsync` to copy a sample _.mbsyncrc_ for use with _isync_ to your home directory. 
                 Additional isync/mbsync/mu4e resources 
                 [are here:](http://www.ict4g.net/adolfo/notes/2014/12/27/EmacsIMAP.html)

 * for Spelling and grammar.
    * languagetool 
    * hunspell -- add dictionaries as needed. 
    * hunspell dictionaries  [get them here!](https://github.com/EricGebhart/Hunspell-dictionaries)
    Arch Linux has a lot of them.  Just do `pacman -Ss hunspell` to see what arch has.

 - Fonts
  *Iosevka Fonts* [are here!](https://github.com/be5invis/Iosevka)
  Or just install the Arch Linux packages. One is community, the other AUR. 
      `yay -S ttf-iosevka ttc-iosevka`


Evil Mode
=========
I've been using emacs in some sort of Vi emulation since 1995. Evil-mode is, IMHO the best vi emulator so far. Although neovim is doing a really good job. vimscript is an unfortunate language.
You can easily turn it off in _setup.el_ . The Evil mode setup includes a few but not all of the Evil-mode extensions. For more information check out the [Evil-mode documentation.](http://www.emacswiki.org/emacs/Evil)

Included along with evil mode are: 
* [evil-leader](https://github.com/cofi/evil-leader)
* [evil-paredit](https://github.com/roman/evil-paredit)
* [evil-nerd-commenter](https://github.com/redguardtoo/evil-nerd-commenter)
* [evil-surround](https://github.com/timcharper/evil-surround)
* evil-org
 
CycleBufs
==============
I don't use this anymore. I'm currently switching to perspective which
works well with projectile and Exwm.

Cyclebufs is now built on top of BS - Buffer Selection. There are several bs-configurations,
and extra functionality which makes switching buffers more contextual. 
Reusing windows for different mode groups shell, dired, and bs-show if they are visible.

Also cycling of buffers based on groups.

As an example, the shell group contains shell, eshell, ansi-term, cider, and inferior python modes.
Once a buffer has one of the modes in the group, cycling will stay within that group.
There is also contextual cycling based on the mode group of the current buffer, 
cycling through shells, *buffers, or file buffers accordingly. 

Cyclebufs will open a shell buffer of your choice based on the value of cb-shell-command. The default is
eshell. See *vars.el*.


## Themes
I am using Modus Themes now. There are lots of other themes here,
but I'm tempted to remove all but my custom palette-theme extensions.
They frequently are deleted from elpa and cause trouble during install
with package not found errors. The name must then be removed from packages.el.

Lots of themes from packages. Additionally my own personal theme
strange-deeper-blue. As well as a couple of variations on solarized.
There is also a palette-themes.el which is a more general library
adapted from the solarized-theme. Palette-themes allow for the creation
of themes simply by defining a palette of colors. There are four
different variations of the solarized themes included.

;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------- lang
     ;; coq
     (elm :variables
          elm-interactive-command '("elm" "repl")
          elm-reactor-command '("elm" "reactor")
          elm-reactor-arguments '("--port" "8000")
          elm-compile-command '("elm" "make")
          ;; elm-compile-arguments '("--output=elm.js" "--debug")
          elm-compile-arguments '("--output=elm.js")
          elm-package-command '("elm" "package")
          elm-package-json "elm.json")
     emacs-lisp
     html
     haskell
     idris
     javascript
     lua
     org
     racket
     (rust :variables
           rust-rls-cmd '("rustup" "run" "stable" "rls"))
     ;; scala
     yaml
     shell-scripts
     ;; ----------------------------------------------------------- conf
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior nil
                      auto-completion-idle-delay nil ;; no automatic suggestion
                      ;; auto-completion-private-snippets-directory nil
                      ;; auto-completion-enable-snippets-in-popup nil
                      ;; auto-completion-enable-help-tooltip nil
                      ;; auto-completion-enable-sort-by-usage nil
                      )
     (ivy :variables
          ;; Add recent files to buffer completion menu
          ivy-use-virtual-buffers t)
     spell-checking
     syntax-checking
     ;; ----------------------------------------------------------- tool
     bibtex
     erc
     lsp
     nixos
     pdf
     ranger
     restclient
     ;; TODO:
     ;; ANSI rather than eshell, shell, ...
     ;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-shell 'ansi-term
     ;;        ; shell-default-term-shell "/run/current-system/sw/bin/zsh"
     ;;        shell-default-position 'bottom)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     rainbow-mode
     zenburn-theme
     nord-theme
     ;; `:location' documentation
     ;; https://github.com/syl20bnr/spacemacs/blob/f94fea920019feb18900b8f0aa4807d8da145a75/doc/LAYERS.org#packagesel
     (ospl :location "~/.emacs.d/private/local/")
     ;; `recipe' documentation
     ;; https://github.com/melpa/melpa/tree/5f197baa4452e0bd3bac3fa6dc033a2c8a1ae228#recipe-format
     (lsp-haskell :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell"))
     (ob-racket :location (recipe :fetcher github :repo "DEADB17/ob-racket"))
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ;; Do not highlight the surrounding parentheses: Show Paren Mode
     ;; and Rainbow Delimiters are sufficient for me
     highlight-parentheses
     ;; The `font-lock-add-keywords' feature does the job. I don't
     ;; need a minor mode to do the same
     hl-todo
     ;; Do not show symbol as bullet for headers in org
     org-superstar
     ;; I don't care of image and YouTube videos thumbnails support in
     ;; erc
     erc-image erc-yt
     ;; Do not print a ~ to indicate the end of file
     vi-tilde-fringe
     ;; Do not mix company with auto-complete
     auto-complete
     ;; No!
     lsp-ui lsp-treemacs
     treemacs treemacs-icons-dired treemacs-evil treemacs-persp
     treemacs-projectile
     yasnippet auto-yasnippet ivy-yasnippet yasnippet-snippets
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-remap-Y-to-y$ t)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (agenda . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(nord
                         ;; zenburn
                         spacemacs-light)

   ;; TODO: Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator arrow :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  ;; Note: Spacemacs 0.3 removes the usage of `exec-path-from-shell'
  ;; and replaces it by a built-in function
  ;; `spacemacs/load-spacemacs-env' which reads variables from
  ;; `~/.spacemacs.env'. Variables from `~/.spacemacs.env' are
  ;; generated by `spacemacs//init-spacemacs-env'. See,
  ;; https://github.com/syl20bnr/spacemacs/pull/10884/files
  ;;
  ;; `exec-path-from-shell' was a problem with my usage of Nix sandbox
  ;; and nix-shell (i.e., launch Emacs from a specific sandbox shell).
  ;; The solution was to put `exec-path-from-shell' in the excluded
  ;; package. See,
  ;; https://github.com/rcherrueau/.dotfiles/blob/37f6047cbd5a108c8a6f7b418b6adafd5aaed219/spacemacs/.spacemacs#L95-L100
  ;;
  ;; The new solution isn't satisfactory either. The
  ;; `spacemacs//init-spacemacs-env' initialization function is called
  ;; only once. Thus, the `~/.spacemacs.env' doesn't contains my PATH
  ;; generated by nix-shell. One solution consists of running
  ;; `spacemacs/force-init-spacemacs-env' to regenerate the file. Or
  ;; simpler, do not rely on `spacemacs/load-spacemacs-env'.
  ;;
  ;; (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; ----------------------------------------------------------- Customize
  ;; Set custome variables in a specific file
  (setq custom-file (locate-user-emacs-file "private/mine-pref.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file)

  ;; Fallback on Noto Emoji for symbol
  (set-fontset-font t 'symbol (font-spec :family "Noto Emoji" :size 16)))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; ------------------------------------------------------------- Utils
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (string-match (rx-to-string `(: bos ,prefix) t)
                       string)
         t))

  (defun string/ends-with (string suffix)
    "Return t if STRING ends with SUFFIX."
    (and (string-match (rx-to-string `(: ,suffix eos) t)
                       string)
         t))

  ;; Try (macroexpand '(rcherr/inoremap "jk" company-complete-common))
  (defmacro rcherr/inoremap (key-seq fun)
    "Mimic vim 'inoremap KEY-SEQ FUN' in insert mode.

Trigger FUN if KEY-SEQ is pressed rapidly in evil insert mode.
For instance, \(rcherr/inoremap \"jk\" company-complete-common)
calls the completion function `company-complete-common' when
rapidly pressing 'jk'.

Note: Do not quote FUN with macro, (see URL
https://stackoverflow.com/a/14001190)."
    (let ((defun-name (intern (format "recherr/inoremap-%s" key-seq)))
          ;; FIXME: Generalize to more than a seq of 2 chars
          (fst-key (aref key-seq 0))
          (snd-key (aref key-seq 1))
          (timeout 0.1) ;; Delay between fst-and snd-key to trigger
                        ;; FUN
          )
      ;; If the name of the defun exists, then KEY-SEQ is already in
      ;; use somewhere else.
      (when (fboundp defun-name)
        (error "The key sequence '%s' is already used" key-seq))

      ;; The macro
      `(progn
         ;; The function that insert in the buffer `fst-key', then
         ;; waits for `snd-key' to trigger FUN. See,
         ;; https://emacs.stackexchange.com/a/20024
         (defun ,defun-name ()
           (interactive)
           (let ((modified (buffer-modified-p)))
             (insert ,fst-key)
             (let ((evt (read-event nil nil ,timeout)))
               (cond
                ;; `snd-key' match ⇒ remove `fst-key' and exec FUN.
                ((and (characterp evt) (= evt ,snd-key))
                 (delete-char -1)
                 (set-buffer-modified-p modified)
                 (,fun))
                ;; timeout or not `snd-key' ⇒ `evt' should be
                ;; considered as normal char.
                (t (push evt unread-command-events))))))

         ;; The maping into evil-insert-state-map
         (define-key evil-insert-state-map
                     [,fst-key]
                     (quote ,defun-name)))))

  ;; -------------------------------------------------------- Appearance
  ;; -- Fringeline
  ;; Display `-' in the fringe line for EOF
  (setq-default indicate-empty-lines t)

  ;; -- Line behavior
  (setq-default default-fill-column 70)
  (setq-default fill-column default-fill-column)

  ;; Set sentence delimiter to two spaces
  (setq-default sentence-end-double-space t)

  ;; Stop truncate lines
  (add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

  ;; Never insert tabs
  (setq-default indent-tabs-mode nil)

  ;; Add a new line at the end of file
  (setq-default require-final-newline t)

  ;; Change colors a bit
  (when (custom-theme-p 'zenburn)
    (custom-theme-set-faces
     'zenburn
     ;; Highlight current line
     '(hl-line ((t (:background "#4A2F59" :extend t)) (t :weight bold)) t)
     '(region ((t (:background "#6B3654" :extend t)) (t :inverse-video t)) t)
     ;; Change background color of block in org
     '(org-block ((t (:background "zenburn-bg" :extend t))) t)))

  (when (custom-theme-p 'nord)
    (custom-theme-set-faces
     'nord
     ;; Helm
     '(helm-source-header ((t (;; :foreground "#88C0D0"
                               ;; :background "#434C5E"
                               :underline nil
                               ;; :weight bold
                               :box (:line-width -1 :style released-button)
                               :extend t))) t)
     ))

  ;; Highlight the following words in comments
  (defun add-watchwords ()
    (font-lock-add-keywords
     nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|XXX\\|BUG\\|Note:\\)"
            1 font-lock-warning-face t))))

  (add-hook 'prog-mode-hook #'add-watchwords)
  (add-hook 'org-mode-hook #'add-watchwords)

  ;; -- Display formfeed `C-q C-l' as horizontal line.
  (global-page-break-lines-mode t)
  (add-to-list 'page-break-lines-modes 'prog-mode)
  (add-to-list 'page-break-lines-modes 'text-mode)

  ;; Selecting text put it in the x-buffer for x-copy
  (setq select-enable-primary t)

  (setq ispell-dictionary "en_US")

  (spacemacs/toggle-camel-case-motion-globally-on)

  ;; Manage bibLaTeX references
  (defun rcherr/org-ref-get-pdf-filename (key)
    "Return the pdf filename associated with a bibtex KEY.
This searches for the pattern *KEY.pdf. If one result is found it
is returned, but if multiple results are found, e.g. there are
related files to the KEY you are prompted for which one you
want."
    (let* ((cmd  (format "find /home/rfish/Sync/Papers/ -iname '*%s.pdf' -print0" key))
           (pdfs (s-split " " (shell-command-to-string cmd) t)))
      (cond
       ((= (length pdfs) 0)
        (message "There is no articles with the key %s" key))
       ((= (length pdfs) 1)
        (car pdfs))
       ((> (length pdfs) 1)
        (completing-read "Choose an article: " pdfs)))))

  (setq org-ref-default-bibliography '("~/prog/inria-perso/Bibliography.bib")
        org-ref-bibliography-notes "~/Sync/Papers/notes.org"
        org-ref-get-pdf-filename-function 'rcherr/org-ref-get-pdf-filename)

  ;; ------------------------------------------------------------- Modes

  ;; -- company
  (with-eval-after-load 'company
    ;; Start completion candidate on "jk". Note:
    ;; `auto-completion-complete-with-key-sequence' complete the
    ;; selected candidate once completion is already initiated. It is
    ;; not what I am looking for.
    (rcherr/inoremap "jk" company-complete-common))

  ;; -- Coq
  (with-eval-after-load 'coq-mode
    ;; Fix pressing <ESC> to enter normal mode triggers Proof General
    ;; autocomplete.
    ;; - https://github.com/syl20bnr/spacemacs/issues/8853
    ;; - https://github.com/syl20bnr/spacemacs/issues/11539
    (setq coq-mode-abbrev-table '()))

  ;; -- erc
  (with-eval-after-load 'erc
    ;; Flyspell
    ;; https://www.emacswiki.org/emacs/ErcSpelling
    (erc-spelling-mode t))

  ;; -- haskell
  (with-eval-after-load 'haskell-mode
    (require 'lsp-haskell)
    (add-hook 'haskell-mode-hook #'lsp))

  ;; -- helm
  (with-eval-after-load 'helm-files
    ;; Don't show files matching regexp in
    ;; `helm-boring-file-regexp-list' after a =SPC f f=
    (setq helm-ff-skip-boring-files t)

    ;; Adds Idris compiled files to the list of boring files
    (add-to-list 'helm-boring-file-regexp-list "\\.ibc$")

    ;; Removes the 2 first dot files "current/path/." and
    ;; "current/path/.."
    ;; https://www.reddit.com/r/emacs/comments/3f55nm/how_to_remove_2_first_dot_files_from_helmfindfiles/
    (advice-add 'helm-ff-filter-candidate-one-by-one
                :around (lambda (fcn file)
                          (unless
                              (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'"
                                            file)
                            (funcall fcn file)))))

  ;; -- Idris
  (with-eval-after-load 'idris-mode
    ;; Fix spacemacs integration
    (setq idris-stay-in-current-window-on-compiler-error t)
    (dolist (x '("*idris-notes*" "*idris-holes*" "*idris-info*"))
      (plist-put (cdr (assoc x popwin:special-display-config)) :noselect t))
    ;; Go with the Elab Monad
    (setq idris-enable-elab-prover t)
    (setq idris-simple-indent-mode nil))

  ;; -- LaTeX
  ;;
  ;; emacs automatically detects the mode of a ".tex" file from the
  ;; macro into it. Sometimes it guesses the TeX-mode and other times
  ;; LaTeX-mode. So, It's safer to put stuff in TeX-mode since
  ;; LaTeX-mode inherits form it, and everything here will be loaded
  ;; by both mode.
  (with-eval-after-load 'tex-mode
    ;; (require 'ospl)
    ;; (add-hook 'tex-mode-hook #'turn-on-ospl)
    )

  ;; -- Org
  (with-eval-after-load 'org
    ;; Highlight LaTeX related syntax
    (setf org-highlight-latex-and-related '(latex))
    ;; Do not auto-indent lines depending on the depth on the node.
    (setq org-adapt-indentation nil)
    (setq org-startup-indented nil)
    ;; Syntax highlighting of source block
    (setq org-src-fontify-natively t)
    ;; Set TAB of source block as if it were used in the appropriate
    ;; major mode
    (setq org-src-tab-acts-natively t)
    ;; Preserve my indentation during source block export
    (setq org-src-preserve-indentation t)
    ;; Activate smartparens mode
    (smartparens-mode t)

    ;; Todo Keywords
    (setq org-todo-keywords
          '((sequence "TODO" "FEEDBACK" "FAIR"
                      "|" "DONE" "INVALID")
          ))

    ;; Todo Special Colors
    ;; http://orgmode.org/manual/Faces-for-TODO-keywords.html
    (setq org-todo-keyword-faces
          '(("FEEDBACK" . "orange")
            ("FAIR" . "orange")))

    ;; Set the external pdf application to zathura
    (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")

    ;; -- Easy templates
    ;;
    ;; easy-template changed in org-mode 9.2, the following reverts
    ;; the good old `<s TAB'.
    ;;
    ;; See,
    ;; https://www.reddit.com/r/emacs/comments/ad68zk/get_easytemplates_back_in_orgmode_92/
    ;; https://emacs.stackexchange.com/a/46992
    (add-to-list 'org-modules 'org-tempo)
    (add-to-list 'org-structure-template-alist
                 '("M" . "MAILQUOTE"))

    ;; Racket support inside babel
    (add-to-list 'org-babel-load-languages '(racket . t))
    )

  (with-eval-after-load 'toc-org
    ;; Make :TOC: headline use org link instead of github
    (setq-default toc-org-hrefify-default "org"))

  (with-eval-after-load 'ox-ascii
    ;; -- ASCII Special Export for mail
    ;;
    ;; See [file:~/.emacs.d/elpa/org-plus-contrib-20180716/ox-ascii.el::(org-export-define-backend%20'ascii]
    ;;
    ;; Offers new kind of blocks with specific formating related
    ;; - MAILQUOTE: display content as it is prepend with a ">".
    (defun mail-ascii/special-block (_special-block contents _info)
      "Transcode a special block element from Org to ACII.
CONTENTS holds the contents of the block. INFO is a plist holding
contextual information. This function offers new kind of
formating depending on the type of _SPECIAL_BLOCK."
      (let ((blockname (org-element-property :type _special-block)))
        (cond
         ;; MAILQUOTE: display CONTENTS with a ">".
         ((string= blockname "MAILQUOTE")
          (let* ((contents-begin (org-element-property :contents-begin _special-block))
                 (contents-end (org-element-property :contents-end _special-block))
                 ;; Get content of the block without `fill-paragraph' application.
                 (orig-contents (s-lines (buffer-substring-no-properties contents-begin contents-end))))

            (defun mail-ascii/quoted-line? (line)
              "Tests if a line is a mail-quoted one."
              (s-starts-with? ">" line))

            (defun mail-ascii/quote-line (line)
              "Quote the current line."
              (if (not (mail-ascii/quoted-line? line))
                  (s-prepend "> " line)
                (s-prepend ">" line)))

            (s-join "\n" (-map 'mail-ascii/quote-line orig-contents))))
         ;; Fallback on the original org-ascii-special-block
         (t (org-ascii-special-block (_special-block contents _info))))))

    (org-export-define-derived-backend 'mail-ascii 'ascii
      :menu-entry '(?t "Export to Plain Text"
                       ((?M "As ASCII for Mail"
                           (lambda (a s v b)
                             (org-export-to-buffer 'mail-ascii "*Org MAIL-ASCII Export*")))))
      :translate-alist '((special-block . mail-ascii/special-block))))

  ;; -- racket
  (with-eval-after-load 'racket-mode
    ;; Do not display overlay information à la DrRacket.
    (setq racket-show-functions '(racket-show-echo-area))
    (add-hook 'racket-xp-mode-hook
              (lambda ()
                (remove-hook 'pre-redisplay-functions
                             #'racket-xp-pre-redisplay
                             t)))

    ;; I have to disable this right now.  It is too greedy.  It eats
    ;; all my RAM after 20 minutes
    (remove-hook 'racket-mode-hook 'racket-xp-mode)

    ;; Terminate the Racket process if memory use exceeds 2Go.
    (setq-default racket-memory-limit 2048))

  ;; -- tramp
  (with-eval-after-load 'tramp
    ;; Add my G5K bin directory to the list of remote paths TRAMP
    ;; searches for programs (so TRAMP can call `ag' on for instance).
    ;; [[info:tramp#Remote%20programs][info:tramp#Remote programs]]
    (add-to-list 'tramp-remote-path "/home/rcherrueau/bin/usr/bin"))

  ;; -- vc-mode
  ;; Visit symbolic link to a file. This bypass the emacs version
  ;; control system, but I don't use it!
  (setq vc-follow-symlinks nil)
  ;; Fully disable vc-mode
  (setq vc-handled-backends ())
  )

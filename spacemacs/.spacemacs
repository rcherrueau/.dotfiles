;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   dotspacemacs-ask-for-lazy-installation nil
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------- chat
     erc
     ;; ----------------------------------------------------------- lang
     emacs-lisp
     (haskell :variables haskell-completion-backend 'ghc-mod)
     html
     idris
     javascript
     python
     racket
     rust
     ;; ----------------------------------------------------------- tool
     auto-completion
     ansible
     helm
     vagrant
     ;; ANSI rather than eshell, shell, ...
     ;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
            ; shell-default-term-shell "/run/current-system/sw/bin/bash")
     ;; --------------------------------------------------------- others
     nixos
     org
     spell-checking
     syntax-checking
     ;; --------------------------------------------------------- perso
     mine-java
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(tao-theme page-break-lines
     rainbow-mode realgud)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
     ;; Remove this so Spacemacs use PATH from nix-shell, see:
     ;; https://github.com/syl20bnr/spacemacs/issues/2294 Also
     ;; cherry-pick PR #8543 (commit: 48c12d4) to make rust and go
     ;; layer behave correctly without `exec-path-from-shell'
     ;; https://github.com/syl20bnr/spacemacs/issues/8543
     exec-path-from-shell
     ;; Do not print a ~ to indicate the end of file
     vi-tilde-fringe
     ;; Do not show symbol as bullet for headers in org
     org-bullets
     ;; Do not highlight the surrounding parentheses: Show Paren Mode
     ;; and Rainbow Delimiters are sufficient for me
     highlight-parentheses
     ;;
     anzu
     ;; The `font-lock-add-keywords' feature does the job. I don't
     ;; need a minor mode to do the same
     hl-todo
     ;; YouTube videos thumbnails inside erc doesn't work well
     erc-yt
     )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; dotspacemacs-startup-lists '((recents . 10)
   ;;                              (todo . nil))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive nil
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn misterioso tao-yang)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 97
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 97
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; -- Customize
  ;; Set custome variables in a specific file
  (setq custom-file (locate-user-emacs-file "private/mine-pref.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file)

  ;; Fallback on Noto Emoji for symbol
  (set-fontset-font t 'symbol (font-spec :family "Noto Emoji" :size 15))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
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

  ;; -------------------------------------------------------- Appearance
  ;; -- Fringeline
  ;; Display `-' in the fringe line for EOF
  (setq-default indicate-empty-lines t)

  ;; -- Line behavior
  (setq-default default-fill-column 70)
  ;; Stop truncate lines
  (add-hook 'text-mode-hook (lambda ()
                              (setq truncate-lines nil)))
  ;; Never insert tabs
  (set-default 'indent-tabs-mode nil)

  ;; Highlight current line
  (set-face-attribute 'hl-line nil :background "#4A2F59")
  (set-face-attribute 'region nil :background "#6B3654")

  ;; Highlight the following words in comments
  (defun add-watchwords ()
    (font-lock-add-keywords
     nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|XXX\\|BUG\\|Note:\\)"
            1 font-lock-warning-face t))))

  (add-hook 'prog-mode-hook #'add-watchwords)
  (add-hook 'org-mode-hook #'add-watchwords)

  ;; -- Powerline
  ;; Display the name of the function in the powerline
  (which-function-mode t)

  ;; Powerline with arrow
  (setq powerline-default-separator 'arrow)
  (setq powerline-default-separator-dir '(right . right))

  ;; Custom power line
  ;; (spaceline-compile "main"
  ;;       '(((workspace-number window-number)
  ;;          :fallback evil-state
  ;;          :separator "|"
  ;;          :face highlight-face)
  ;;         (buffer-modified buffer-encoding-abbrev line-column
  ;;          "⚓" buffer-id remote-host)
  ;;         major-mode
  ;;         ((minor-modes process)
  ;;          :when active)
  ;;         (erc-track :when active)
  ;;         ((flycheck-errors flycheck-warnings flycheck-infos)
  ;;          :when active)
  ;;         (org-pomodoro :when active)
  ;;         (org-clock :when active)
  ;;         which-function)

  ;;       '(selection-info
  ;;         ((global-mode new-version)
  ;;          :when active)
  ;;         buffer-position hud))

  ;; -- Dispalye formfeed `C-q C-l' as horizontal line.
  (global-page-break-lines-mode t)
  (add-to-list 'page-break-lines-modes 'css-mode)
  (add-to-list 'page-break-lines-modes 'js-mode)
  (add-to-list 'page-break-lines-modes 'haskell-mode)
  (add-to-list 'page-break-lines-modes 'idris-mode)
  (add-to-list 'page-break-lines-modes 'proverif-pv-mode)
  (add-to-list 'page-break-lines-modes 'racket-mode)
  (add-to-list 'page-break-lines-modes 'sh-mode)
  (add-to-list 'page-break-lines-modes 'web-mode)

  ;; ------------------------------------------------------------- Other
  (setq ispell-dictionary "en_GB")
  (setq select-enable-primary t) ;; Selecting text put it in the
                                 ;; x-buffer for x-copy
  (spacemacs/toggle-camel-case-motion-globally-on)

  ;; ------------------------------------------------------------- Modes

  ;; -- erc
  (with-eval-after-load 'erc
    ;; Flyspell
    ;; https://www.emacswiki.org/emacs/ErcSpelling
    (erc-spelling-mode t))

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
    (setq idris-stay-in-current-window-on-compiler-error t)
    (setq idris-enable-elab-prover t)
    (setq idris-simple-indent-mode nil))

  ;; -- Org
  (with-eval-after-load 'org
    ;; Do not auto-indent lines depending on the depth on the node.
    (setq org-startup-indented nil)
    ;; Syntax highlighting of source block
    (setq org-src-fontify-natively t)
    ;; Preserve my indentation during source block export
    (setq org-src-preserve-indentation t)
    ;; Activate smartparens mode
    (smartparens-mode t)

    ;; Todo Keywords
    (setq org-todo-keywords
          '((sequence "TODO" "FEEDBACK" "FAIR"
                      "|" "DONE" "DELEGATED" "INVALID" "FIXED")
          ))

    ;; Todo Special Colors
    ;; http://orgmode.org/manual/Faces-for-TODO-keywords.html
    (setq org-todo-keyword-faces
          '(("FEEDBACK" . "orange")
            ("FAIR" . "orange")))

    ;; Note: Seems fix with the 0.105 v of spacemacs
    ;; ;; evil-org binds "J" to `org-shiftdown'. Let's bind "J" to
    ;; ;; `evil-join' instead.
    ;; (evil-define-key 'normal evil-org-mode-map
    ;;   "J" 'evil-join)

    ;; Set the external pdf application to zathura
    (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")

    ;; Highlight LaTeX related syntax
    (setf org-highlight-latex-and-related '(latex))

    ;; Easy templates
    (add-to-list 'org-structure-template-alist
                 '("M" "#+BEGIN_MAILQUOTE\n?\n#+END_MAILQUOTE"))

    ;; -- Agnostic cite hyperlink; support in LaTeX
    ;;
    ;; A link of the form `cite:mykey' is transformed as a
    ;; \cite{mykey} in LaTeX.
    ;; see org-ascii-link and org-latex-link for inspiration
    (defun org-cite-export (key desc format)
      "Create the export version of a cite link."
      (cond
       ((eq format 'latex)
        (if (null desc)
            (format "\\cite{%s}" key)
          ;; If you provide a description, then use that description
          ;; (defcitealias is available with natbib)
          (format "\\defcitealias{%s}{%s}\\citetalias{%s}" key desc key)))
       (t (format "[%s]" key))))

    ;; Follows a bib key and visits the bibfile. The bibfile path must
    ;; be defined in `org-bibref-file'.
    (defun org-cite-open (key)
      "Visit the reference on KEY.
  KEY shoulb be a citation key available in the `org-bibref-file'
  file"
      (let ((path org-bibref-file)
            (arg 'emacs)
            (search key))
        ;; org-open-file is a function defines in org.el. It is used in
        ;; `org-open-at-point' arround line 10548.
        (org-open-file path arg nil search)))

    (org-add-link-type "cite" 'org-cite-open 'org-cite-export)

    ;; Same as `cite' but year only This feature is available for
    ;; instance with natbib and biblatex with the author-year scheme.
    ;;
    ;; A link of the form `citey:mykey' is transformed as a
    ;; \citeyearpar{mykey} in LaTeX.
    (defun org-citey-export (key desc format)
      "Create the export version of a year cite link."
      (cond
       ((eq format 'latex) (format "~\\citeyear{%s}" key))
       (t (format "[%s]" key))))

    (org-add-link-type "citey" 'org-cite-open 'org-citey-export)

    ;; (defun org-cite-store-link ()
    ;;   "Store a link to a citation."
    ;;   (let* ((key (org-cite-get-key))
    ;;          (link (concat "cite:" key))
    ;;          (description (fomat "Citation for %s in %s" key org-bibref-file)))
    ;;     (org-store-link-props
    ;;      :type "cite"
    ;;      :link link
    ;;      :decription description)))
    )

  (with-eval-after-load 'ox
    ;; Transforms many following cites into one multiple cite.
    (defun org/latex-filter-cites (final-output backend info)
      "Makes a multiple cite of adjacent cites in LaTeX export"
      ;; http://www.emacswiki.org/emacs/ElispCookbook#toc2
      (when (org-export-derived-backend-p backend 'latex)
        (replace-regexp-in-string
         "\\\\cite{[a-zA-Z0-9+]+}\\(?:[\s\n]*\\\\cite{[a-zA-Z0-9+]+}\\)+"
         (lambda (cites)
           (save-match-data
             (concat "\\\\cite{"
                     (string-join
                      (mapcar (lambda (cite)
                                (replace-regexp-in-string
                                 "\\\\cite{\\([a-zA-Z0-9+]+\\)}"
                                 "\\1"
                                 cite))
                              (split-string cites))
                      ",")
                     "}")))
         final-output)))
    (add-to-list 'org-export-filter-final-output-functions
                 'org/latex-filter-cites)

    ;; -- Do not launch error on unresolved link
    ;;
    ;; The good way:
    ;; `org-export-resolve-fuzzy-link' `org-export-before-processing-hook'
    ;; http://comments.gmane.org/gmane.emacs.orgmode/100754
    ;; http://emacs.stackexchange.com/a/16914
    ;; http://emacs.stackexchange.com/a/9494
    ;; http://kdr2.com/tech/emacs/orgmode-export-process.html
    ;; (defun org-drop-unliked-link (backend)
    ;;   "Drop links which don't point to a target"
    ;;   (org-element-map (org-element-parse-buffer) 'link
    ;;     (lambda (link)
    ;;       (when (string= (org-element-property :type link) "file")
    ;;         (org-element-property :path link)))))
    ;; (add-hook 'org-export-before-processing-hook #'org-drop-unliked-link)
    ;;
    ;; The easy way ; AOP!:
    (defun org/drop-unlinked-link (orig-fun link info)
      "Do not launch error on unresolved link"
      (condition-case err
          (funcall orig-fun link info)
        ;; The function that uses the link resolver has two flows:
        ;; - The link resolver returns an org-element, then the
        ;;   exporter produces one reference to that link.
        ;; - The link resolver returns a string, then the exporter
        ;;   produces an url link.
        ;; Because we consider custom-id on healines, we are
        ;; interested in the first flow. Hence, if the custom-id is
        ;; not reachable, we have to construct a fake headline
        ;; org element.
        ;; http://orgmode.org/worg/dev/org-element-api.html
        (user-error
         (progn
           (message
            "Unresolvable link %s; fallback using `org-drop-unlinked-link'"
            (org-element-property :path link))
           (org-element-create
            'headline
            ;; Sets reference of the fake headline with the
            ;; path of our link
            `(:CUSTOM_ID ,(org-element-property :path link)
            ;; Sets the level to prevent error while
            ;; `org-export-numbered-headline-p'
              :level     0))))))

    (advice-add 'org-export-resolve-id-link :around #'org/drop-unlinked-link)
    )

  (with-eval-after-load 'ox-ascii
    ;; -- ASCII Special Export for mail
    ;;
    ;;
    ;; See http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=lisp/ox-ascii.el;hb=HEAD
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
      :translate-alist '((special-block . mail-ascii/special-block)))
    )

  ;; -- python
  (with-eval-after-load 'python
    (setq python-indent-offset 2)
    (setq python-fill-column default-fill-column)

    ;; https://github.com/google/yapf/blob/master/yapf/yapflib/style.py#L220
    ;;   style = CreateGoogleStyle()
    ;; style['INDENT_DICTIONARY_VALUE'] = True
    ;; style['INDENT_WIDTH'] = 2
    ;; style['JOIN_MULTIPLE_LINES'] = False
    ;; style['SPLIT_BEFORE_BITWISE_OPERATOR'] = True
    ;; style['SPLIT_PENALTY_FOR_ADDED_LINE_SPLIT'] = 0
    (setq py-yapf-options
          (format "--style='{based_on_style:chromium,column-limit:%s}'"
                  default-fill-column)))

  ;; -- shell
  (with-eval-after-load 'shell
    (setq sh-basic-offset 2))

  ;; -- tramp
  (with-eval-after-load 'tramp
    ;; Add my G5K bin directory to the list of remote paths TRAMP can
    ;; search for programs (so TRAMP can call `ag' on for instance).
    ;; [[info:tramp#Remote%20programs][info:tramp#Remote programs]]
    (add-to-list 'tramp-remote-path "/home/rcherrueau/bin/usr/bin"))

  ;; -- web-mode
  (with-eval-after-load 'web-mode
    (defvar-local fmdkdd/browser-window-list nil
      "List of the browser windows that will receive a F5 key
event when `fmdkdd/reload-browser-windows' is called.

Elements of the list are cons cells (ID . FOCUS) where ID is a X
window id obtained from running 'xdotool selectwindow' (a string
representing a number), and FOCUS is a boolean indicating whether
we must give the focus to that window before sending the key
event. See the documentation of
`fmdkdd/browser-window-no-focus-regexp' for why this field is
needed.")

    (defvar fmdkdd/last-browser-window-list nil
      "List of the browser windows that were last selected by the
user through `fmdkdd/select-browser-windows'.

This value is used by `fmdkdd/reload-browser-windows' when the
buffer-local variable `fmdkdd/browser-window-list' is nil.")

    (defvar fmdkdd/browser-window-no-focus-regexp "Firefox"
      "This regexp is used by `fmdkdd/select-browser-windows' on
the names of each selected window to determine if they need
windowactivate.

Google Chrome and Chromium do not listen to the key sent by
xdotool when the window does not have focus (see
https://code.google.com/p/chromium/issues/detail?id=393145).
Using windowfocus is not enough to receive the key either (it
fails randomly). But using the windowactivate command of xdotool
shifts the focus away from the Emacs window, so we'd rather avoid
it if we can.")

    (defun fmdkdd/save-and-reload-browser-windows (&optional reselect)
      "Save current buffer and call `fmdkdd/reload-browser-windows'."
      (interactive "P")
      (save-buffer)
      (fmdkdd/reload-browser-windows reselect))

    (defun fmdkdd/reload-browser-windows (&optional reselect)
      "Send F5 to reload the browser windows.

The windows to reload are found by looking up the buffer-local
variable `fmdkdd/browser-window-list', then the global
`fmdkdd/last-browser-window-list' if the former is nil. If both
are nil, call `fmdkdd/select-browser-windows' to ask the user to
select the browser window with the mouse cursor and sets the
forementionned variables for future calls.

With a prefix argument, it bypasses the variables and forces the
reselection of the window. Multiple windows can be selectionned
this way with a prefix argument greater than 1."
      (interactive "P")
      (fmdkdd//do-reload-browser-windows
       (if reselect (fmdkdd/select-browser-windows reselect)
         (cond
          ;; I know!  I know!
          (fmdkdd/browser-window-list)
          ;; Hmm, maybe the window you specified last time?
          (fmdkdd/last-browser-window-list)
          ;; I give up!  Please tell me.
          (t (fmdkdd/select-browser-windows)))))
      (message "Reloaded browser windows"))

    (defun fmdkdd//do-reload-browser-windows (window-list)
      "Send F5 to each window of WINDOW-LISP using xdotool.

Elements of the list are cons cells (ID . FOCUS) where ID is a X
window id obtained from running 'xdotool selectwindow' (a string
representing a number), and FOCUS is a boolean indicating whether
we must give the focus to that window before sending the key
event."
      (let ((cmd)        ; xdotool command to reload each window
            (wactivate)) ; must we switch the focus away from the Emacs window?
        (setq cmd
              (mapconcat
               (lambda (w)
                 (let ((id (car w))
                       (focus (cdr w)))
                   (if focus
                       (progn
                         (setq wactivate t)
                         (format "windowactivate --sync %s key --window %s 'F5'"
                                 id id))
                     (format "key --window %s 'F5'" id))))
               window-list " "))
        (shell-command
         ;; If one window needed the focus, we need to save the
         ;; current Emacs window and get focus back to it once we are
         ;; done. We leverage the WINDOW_STACK of xdotool to do that.
         (if wactivate
             (format "xdotool getactivewindow %s windowactivate" cmd)
           (format "xdotool %s" cmd)))))

    (defun fmdkdd/select-browser-windows (&optional times)
      "Use 'xdotool selectwindow' to select TIMES windows interactively,
and save the values for future calls to
`fmdkdd/reload-browser-windows'."
      (when (not (numberp times)) (setq times 1)) ; Default value for TIMES
      (let ((windows))
        (dotimes (n times)
          (message
           (format "Select the browser window using the mouse cursor (%d/%d)"
                   (1+ n) times))
          (let* ((id (string-trim
                      (shell-command-to-string "xdotool selectwindow")))
                 (focus (fmdkdd//must-focus-window-to-reload-p id)))
            (setq windows (cons (cons id focus) windows))))
        ;; Oh, /these/ windows.  I'll remember next time.
        (setq fmdkdd/browser-window-list windows
              fmdkdd/last-browser-window-list windows)
        windows))

    (defun fmdkdd//must-focus-window-to-reload-p (window)
      "Check if the window needs to get the focus to receive a
key sent by xdotool."
      (not (string-match-p
            fmdkdd/browser-window-no-focus-regexp
            (string-trim (shell-command-to-string
                          (format "xdotool getwindowname %s" window))))))

    (spacemacs/set-leader-keys
      "or" 'fmdkdd/save-and-reload-browser-windows)
;;     (defun firefox-reload ()
;;       "Saves the current-buffer and reloads the firefox window
;; that lastly got the focus"
;;       (interactive)
;;       (save-buffer)
;;       (if (spacemacs/system-is-mac)
;;           (shell-command-to-string
;;            "osascript ~/.dotfiles/system/apple-scripts/reload-firefox.scpt")
;;         ;; TODO: shell script for linux
;;         ""))

;;     (spacemacs/set-leader-keys-for-major-mode 'web-mode
;;       "r" 'firefox-reload
;;       ;; rebind old , r * to , w *
;;       "wc" 'web-mode-element-clone
;;       "wd" 'web-mode-element-vanish
;;       "wk" 'web-mode-element-kill
;;       "wr" 'web-mode-element-rename
;;       "ww" 'web-mode-element-wrap
;;       )
    )
  ;; -- vc-mode
  ;; Visit symbolic link to a file. This bypass the emacs version
  ;; control system, but I don't use it!
  (setq vc-follow-symlinks nil)
  ;; Fully disable vc-mode
  (setq vc-handled-backends ())
  )

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
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------- lang
     emacs-lisp
     html
     idris
     javascript
     racket
     ;; --------------------------------------------------------- others
     ;; erc
     ;; nixos
     org
     ;; ANSI rather than eshell, shell, ...
     ;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
            ; shell-default-term-shell "/run/current-system/sw/bin/bash")
     spell-checking
     ;; --------------------------------------------------------- perso
     mine-java
     proverif
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(tao-theme page-break-lines)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(
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
     )
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

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
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
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
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn misterioso tao-yang)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Mono"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
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
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
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
  ;; Are we using a mac-keyboard
  (setq mac-keyboard t)
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

  ;; Gets the healdlines path of your current position in org-mode
  ;; https://github.com/fmdkdd/dotfiles/blob/master/spacemacs/.emacs.d/private/fmdkdd/packages.el#L172
  (defun fmdkdd/org-full-outline-path ()
    "Concatenate the results of `org-get-outline-path' and
`org-get-heading' to get the full outline path to the heading we
are currently in."
    (unless (org-before-first-heading-p)
      (let* ((path (append (org-get-outline-path)
                           (cons (org-get-heading t t) nil))))
        (org-format-outline-path path 10)))) ; XXX: not sure if the width
                                             ; argument works right

  ;; ---------------------------------------------------- Hardware setup
  ;; -- Key bindings
  ;; Mac-os key
  (when mac-keyboard
   (setq x-meta-keysym 'super)
   (setq x-super-keysym 'meta))

  ;; Toggle axe-window split
  ;; http://emacswiki.org/emacs/ToggleWindowSplit
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (spacemacs/set-leader-keys "w|" 'toggle-window-split)

  ;; -- Customize
  ;; Set custome variables in a specific file
  (setq custom-file (locate-user-emacs-file "mine-pref.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file)

  ;; -------------------------------------------------------- Appearance
  ;; -- Fringeline
  ;; Display - in the fringe line for EOF
  (setq-default indicate-empty-lines t)
  ;; Set the fringe bitmaps as emacs default values
  (setq-default fringe-indicator-alist
                '((truncation left-arrow right-arrow)
                  (continuation left-curly-arrow right-curly-arrow)
                  (overlay-arrow . right-triangle)
                  (up . up-arrow)
                  (down . down-arrow)
                  (top top-left-angle top-right-angle)
                  (bottom bottom-left-angle
                          bottom-right-angle
                          top-right-angle
                          top-left-angle)
                  (top-bottom left-bracket
                              right-bracket
                              top-right-angle
                              top-left-angle)
                  (empty-line . empty-line)
                  (unknown . question-mark)))

  ;; Line behavior
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
     nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|XXX\\|BUG\\|Note\\):"
            1 font-lock-warning-face t))))

  (add-hook 'prog-mode-hook #'add-watchwords)
  (add-hook 'org-mode-hook #'add-watchwords)

  ;; -- Powerline
  (setq powerline-default-separator 'arrow)
  (setq powerline-default-separator-dir '(right . right))

  ;; FIXME: https://github.com/TheBB/spaceline
  ;; Powerline segment for orgmode
  ;; (spacemacs|define-mode-line-segment org-path
  ;;   (fmdkdd/org-full-outline-path)
  ;;   ;; Displays in org-mode when window is focused
  ;;   :when (eq major-mode 'org-mode))

  ;; (setq spacemacs-mode-line-left
  ;;       '(((workspace-number window-number)
  ;;          :fallback state-tag
  ;;          :separator "|"
  ;;          :face state-face)
  ;;         (buffer-modified buffer-encoding-abbrev line-column
  ;;          "⚓" buffer-id remote-host)
  ;;         major-mode
  ;;         ((flycheck-errors flycheck-warnings flycheck-infos)
  ;;          :when active)
  ;;         ((minor-modes process)
  ;;          :when active)
  ;;         (erc-track :when active)
  ;;         (org-pomodoro :when active)
  ;;         (org-clock :when active)
  ;;         (org-path :when active)))

  ;; (setq spacemacs-mode-line-right
  ;;       '(selection-info
  ;;        ((global-mode new-version)
  ;;         :when active)
  ;;        buffer-position hud))

  ;; -- Dispalye formfeed `C-q C-l' as horizontal line.
  (global-page-break-lines-mode t)
  (add-to-list 'page-break-lines-modes 'web-mode)
  (add-to-list 'page-break-lines-modes 'js-mode)
  (add-to-list 'page-break-lines-modes 'css-mode)

  ;; ------------------------------------------------------------- Modes
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
  (with-eval-after-load 'idris
    (turn-off-idris-simple-indent))

  ;; -- Org
  (with-eval-after-load 'org
    ;; Do not auto-indent lines depending on the depth on the node.
    (setq org-startup-indented nil)
    ;; Syntax highlighting of source block
    (setq org-src-fontify-natively t)
    ;; Preserve my indentation during source block export
    (setq org-src-preserve-indentation t)

    ;; Note: Seems fix with the 0.105 v of spacemacs
    ;; ;; evil-org binds "J" to `org-shiftdown'. Let's bind "J" to
    ;; ;; `evil-join' instead.
    ;; (evil-define-key 'normal evil-org-mode-map
    ;;   "J" 'evil-join)

    ;; Set the external pdf application to zathura on my nixos
    (when (spacemacs/system-is-linux)
      (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s"))

    ;; Highlight LaTeX related syntax
    (setf org-highlight-latex-and-related '(latex))

    ;; -- Easy template
    ;;
    ;; Adds abbreviation for the todo special block (if nothing is
    ;; already bind on "T"). See Easy template in the org manual.
    (add-to-list 'org-structure-template-alist
                 '("T" "#+BEGIN_TODO\nTODO: ?\n#+END_TODO"))
    (add-to-list
     'org-structure-template-alist
      '("TL"
        "#+ATTR_LATEX: :options [inline]\n#+BEGIN_TODO\nTODO: ?\n#+END_TODO"))


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
    ;; -- Supports marginpar in LaTeX
    ;;
    ;; A footnote reference starting with `:margin:' is transformed as
    ;; a \marginpar in LaTeX. The `:margin:' key word is deleted in
    ;; other backend.
    (defun org/latex-filter-ref-margin (fnote backend info)
      (if (org-export-derived-backend-p backend 'latex)
          ;; If LaTeX backend
          (cond
           ((string/starts-with fnote "\\footnote{:margin: ")
            (concat "\\marginpar{"
                    (substring fnote (length "\\footnote{:margin: "))))
           ((string/starts-with fnote "\\footnote{:margin:")
            (concat "\\marginpar{"
                    (substring fnote (length "\\footnote{:margin:")))))
        ;; Other backend
        (progn
          ;; DEBUG:
          ;; (message "fnoteref: %s" fnote)
          (replace-regexp-in-string ":margin: " "" fnote))))

    (add-to-list 'org-export-filter-footnote-reference-functions
                 'org/latex-filter-ref-margin)

    ;; Transforming footnote into margin puts a mess in the footnote
    ;; numbering. This function provides the good numbering.
    (defun org-export-get-footnote-number/margin
        (orig-fun footnote info &optional data body-first)
      (let ((count 0)
            (seen)
            (label (org-element-property :label footnote)))
        (catch 'exit
          (org-export--footnote-reference-map
           (lambda (f)
             (let* ((l (org-element-property :label f))
                   ;; Gets the defintion of the ref
                    (d (org-trim
                          (org-export-data
                           (org-export-get-footnote-definition f info)
                           info)))
                    ;; Test if this is a margin or not
                    (is-margin (string-match ":margin:" d)))
               ;; DEBUG:
               ;; (message "match %s %s" is-margin d)
               (cond
                ;; Anonymous footnote match: return number.
                ((and (not l) (not label) (eq footnote f))
                 (throw 'exit
                        ;; If this is a margin, then don't count
                        (if is-margin count (1+ count))))
                ;; Labels match: return number.
                ((and label l (string= label l))
                 (throw 'exit
                        ;; If this is a margin, then don't count
                        (if is-margin count (1+ count))))
                ;; Otherwise store label and increase counter if label
                ;; wasn't encountered yet.
                ((not l) (if is-margin count (incf count)))
                ((not (member l seen)) (push l seen)
                 (if is-margin count (incf count))))))
           (or data (plist-get info :parse-tree)) info body-first))))

    ;; The new numbering only works when we are during a latex export.
    ;; So we make it available only during latex export.
    (defun cflow-org-export-latex (orig-fun backend &rest args)
      (when (org-export-derived-backend-p backend 'latex)
        (advice-add 'org-export-get-footnote-number :around
                    #'org-export-get-footnote-number/margin))

      (let ((res (apply orig-fun backend args)))

        (when (org-export-derived-backend-p backend 'latex)
          (advice-remove 'org-export-get-footnote-number
                         #'org-export-get-footnote-number/margin))
        res))

    (advice-add 'org-export-as :around #'cflow-org-export-latex)


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

    ;; -- Export listings in a figure environment
    ;;
    ;; Redefines `org-latex-src-block' with an advice to override the
    ;; way BEGIN_SRC blocks are exported. This wraps listings in a
    ;; figure environment for captions in the margin to work. The user
    ;; should give a caption to make this feature available. The
    ;; placement of the figure is controlled by the value of the LaTeX
    ;; specific attribute `:float'.
    ;;
    ;; See
    ;; https://github.com/fmdkdd/phd-thesis/blob/master/tex/export-setup.el#L34
    (defun org/wraps-lstlisting-in-figure (orig-fun src-block contents info)
      (when (org-string-nw-p (org-element-property :value src-block))
        (let* ((lang (org-element-property :language src-block))
               (caption (org-element-property :caption src-block))
               (caption-above-p (org-latex--caption-above-p src-block info))
               (label (org-element-property :name src-block))
               (custom-env (and lang
                                (cadr (assq (intern lang)
                                            org-latex-custom-lang-environments))))
               (num-start (case (org-element-property :number-lines src-block)
                            (continued (org-export-get-loc src-block info))
                            (new 0)))
               (retain-labels (org-element-property :retain-labels src-block))
               (attributes (org-export-read-attribute :attr_latex src-block))
               (float (plist-get attributes :float))
               (listings (plist-get info :latex-listings)))
          (cond ((and listings                    ;; Not case 1
                      (not custom-env)            ;; Not case 2
                      (not (eq listings 'minted)) ;; Not case 3
                      caption)                    ;; caption is set.
                 ;; Wraps lstlisting in a figure
                 (let ((lst-lang
                        (or (cadr (assq (intern lang)
                                        (plist-get info :latex-listings-langs)))
                            lang))
                       (caption-str
                        (when caption
                          (let ((main (org-export-get-caption src-block))
                                (secondary (org-export-get-caption src-block t)))
                            (if (not secondary)
                                (format "{%s}" (org-export-data main info))
                              (format "{[%s]%s}"
                                      (org-export-data secondary info)
                                      (org-export-data main info))))))
                       (lst-opt (plist-get info :latex-listings-options)))
                   (concat
                    "\\SetListingFigureName\n"
                    (format "\\begin{figure}[%s]\n" (or float "htb"))
                    ;; Options.
                    (format
                     "\\lstset{%s}\n"
                     (concat
                      (org-latex--make-option-string
                       (append
                        lst-opt
                        (cond
                         ((and (not float) (plist-member attributes :float)) nil)
                         ((string= "multicolumn" float) '(("float" "*")))
                         ((and float (not (assoc "float" lst-opt)))
                          `(("float" ,(plist-get info :latex-default-figure-position)))))
                        `(("language" ,lst-lang))
                        ;; (if label `(("label" ,label)) '(("label" " ")))
                        ;; (if caption-str `(("caption" ,caption-str)) '(("caption" " ")))
                        ;; `(("captionpos" ,(if caption-above-p "t" "b")))
                        (cond ((assoc "numbers" lst-opt) nil)
                              ((not num-start) '(("numbers" "none")))
                              ((zerop num-start) '(("numbers" "left")))
                              (t `(("firstnumber" ,(number-to-string (1+ num-start)))
                                   ("numbers" "left"))))))
                      (let ((local-options (plist-get attributes :options)))
                        (and local-options (concat "," local-options)))))
                    ;; Source code.
                    (format
                     "\\begin{lstlisting}\n%s\\end{lstlisting}"
                     (let* ((code-info (org-export-unravel-code src-block))
                            (max-width
                             (apply 'max
                                    (mapcar 'length
                                            (org-split-string (car code-info) "\n")))))
                       (org-export-format-code
                        (car code-info)
                        (lambda (loc num ref)
                          (concat
                           loc
                           (when ref
                             ;; Ensure references are flushed to the right,
                             ;; separated with 6 spaces from the widest line of
                             ;; code
                             (concat (make-string (+ (- max-width (length loc)) 6) ? )
                                     (format "(%s)" ref)))))
                        nil (and retain-labels (cdr code-info)))))
                    (if caption-str (format "\n\\caption{%s}" caption-str) "")
                    (if label (format "\n\\label{%s}" label) "")
                    "\n\\end{figure}"
                    "\n\\UnsetListingFigureName")))
                ;; Else, proceed with normal export
                (t
                 (funcall orig-fun src-block contents info))))))

    (advice-add 'org-latex-src-block :around #'org/wraps-lstlisting-in-figure)
    )

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

  ;; -------------------------------------------------------------- Misc
  ;; Revert gc threshold to the default value after initalization.
  ;; I've got lag on key-up in big org files such as my timeline
  ;; https://github.com/syl20bnr/spacemacs/issues/3011
  (setq gc-cons-threshold 800000)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

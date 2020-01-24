# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, options, lib, pkgs, ... }:

{
  imports = [./cachix.nix ./specific.nix];

  # Minimum swappiness without disabling it entirely (preserve ssd)
  boot.kernel.sysctl = {
    "vm.swappiness" = lib.mkDefault 1;
  };

  # Tmp on tmpfs
  boot.tmpOnTmpfs = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Virtual console options
  console = {
    font = "";            # Use font of the system
    useXkbConfig = true;  # Configure KeyMap from the xserver keyboard settings
  };

  # Set time zone and location
  time.timeZone = "CET";

  location = {
    provider = "manual";
    latitude = 47.216542; longitude = 1.553005; # Nantes
  };

  # Garbage collection
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Font: find local font with `fc-list|grep <font>`
  fonts = {
    # Basic set of font w/ reasonable coverage of Unicode (this
    # includes deja vu and noto-font-emoji)
    enableDefaultFonts = true;
    # Extra font
    fonts = with pkgs; [
      crimson             # Oldstyle typefaces (pair it with cochineal
                          # in LaTex)
      iosevka             # Font for programming
      fira                # Font of Firefox OS
      fira-code           # Fira Mono + programming ligatures
      # -- Misc
      font-awesome        # Social logo
    ];

    fontconfig.defaultFonts = {
      monospace = [ "Iosevka" "Fira Code" "DejaVu Sans Mono" ];
      sansSerif = [ "Fira Sans" "DejaVu Sans" ];
      serif = [ "Crimson" "DejaVu Serif" ];
      emoji = [ "Noto Emoji" ];
    };
  };

  # My account. Don't forget to set a password with ‘passwd’.
  users.users.rfish = {
    createHome = true;
    home = "/home/rfish";
    extraGroups = [ "wheel" "video" ];
    useDefaultShell = true;
    uid = 1000;
  };
  users.defaultUserShell = pkgs.zsh;

  # Environment variables
  # environment.variables.EDITOR = "emacsclient -c -a \"\"";
  environment.variables.EDITOR = "${pkgs.vim}/bin/vim -n -u /home/rfish/.vimrc --noplugin";

  #------------------------------------------------------------------- App
  nixpkgs.config.allowUnfree = true;

  # Overlays for package overriding. See,
  # - https://nixos.wiki/wiki/Overlays
  # - https://blog.flyingcircus.io/2017/11/07/nixos-the-dos-and-donts-of-nixpkgs-overlays/
  nixpkgs.overlays = [ (import ./overlays.nix) ];

  # My tools of the trade.
  #
  # Packages installed in system profile. To search by name, run:
  # ~$ nix search -u <<name>>
  # To test it:
  # ~$ nix run nixpkgs.<<name>> -c <<name>>
  #
  # See, http://beyermatthias.de/blog/2015/11/27/nixos-on-unstable---how-to-fix-a-broken-nixos-rebuild/
  environment.systemPackages = with pkgs; [
    # system
    dnsutils mtr ncat traceroute tcpdump # dig
    nixUnstable # nix repl, nix run, ...
    aria stow htop unzip unrar tree
    ranger w3m xsel # w3m to display images, xsel to copy file name with `yd`
    usbutils # lsusb
    vagrant

    # windowing
    xlibs.libXft

    dunst # The lightweight notification-daemon

    # soft
    firefox chromium
    imagemagick # import -window root screenshot.jpg
    inkscape gimp
    kpcli xclip # xclip is used by kpcli to copy pass to clipboard
    mpv # replacement of mplayer
    qtox
    rxvt_unicode
    syncthing
    xorg.xbacklight # Decrease screen brightness
    xcape # Escape and Control on a single key
    xdotool # Script your mouse
    zathura xournal pdfgrep # Pdf viewer + notetaking

    # development tools
    git
    emacs # (emacsWithPackages (epkgs: [epkgs.pdf-tools]))
    vim aspell aspellDicts.en aspellDicts.fr
    silver-searcher
    zeal sqlite

   # # Put here the list of needed tex packages. scheme-* collection-*
   # # are predefined sets of tex packages. You can find theme using
   # # nix-repl
   # # nix-repl> :l <nixpkgs>
   # # nix-repl> pkgs.texlive.coll<tab>
   # # You can also put the name of a package directly, e.g.,
   # # `moderncv`, find them with `nix-repl>
   # # pkgs.textlive.<the-package>`.
   # # see https://nixos.org/wiki/TexLive_HOWTO#New_TexLive_Infrastructure
   # # Use parentheses to force the evaluation of the `combine`
   # # function and compute the new texlive derivation
   # (texlive.combine {
   #   inherit (texlive)
   #   # some tests: collection-basic ⊂ scheme-basic
   #   # nix-repl> :a lib
   #   # let scheme-basic = pkgs.texlive.scheme-basic.pkgs; in
   #   # let coll-basic = pkgs.texlive.collection-basic.pkgs; in
   #   # lib.filter (c: !lib.elem c scheme-basic) coll-basic
   #   # > [ ]
   #   #
   #   # This function test if a certain pakcage is contains inside a
   #   # collection, based on it's name. Find the doc of lib under
   #   # https://github.com/NixOS/nixpkgs/tree/master/lib
   #   #
   #   # let hasPackage = pkgName: pkgDerivation:
   #   #     let pkgNames = map (v: if (pkgs.lib.hasAttr "name" v)
   #   #                            then (pkgs.lib.getAttr "name" v)
   #   #                            else "") pkgDerivation;
   #   #     in pkgs.lib.lists.elem pkgName pkgNames;
   #   # in (hasPackage "texlive-ulem-2015" pkgs.texlive.ulem.pkgs)
   #   # > true
   #   # and to find the name
   #   # > pkgs.lib.getAttr "name" (pkgs.lib.head pkgs.texlive.bera.pkgs)
   #   scheme-small
   #   collection-latex collection-latexextra collection-langfrench
   #   # bera is for my thesis only, see if I cannot remove
   #   # it and put-it inside an nix-shell
   #   luatex collection-luatex bera
   #   # courier font is required by IEEETran (pcrr8t)
   #   courier
   #   # cm-super required by [T1]{fontspec}
   #   cm-super
   #   # dvipng required by org-mode to compute preview images of latex
   #   # formulas
   #   dvipng
   #   # provides \includesvg
   #   svg
   #   # Required by IEEEtran.cls
   #   # collection-fontsrecommended
   #   # Required by adt-inria-hub
   #   rsfs
   #   latexmk ;
   # }) bibtex2html

    # language
    racket
  ];

  #---------------------------------------------------------------- Daemon
  # I have to investigate this a little bit more latter. I've got few
  # problem with environment variable
  #
  # # Emacs as daemom:
  # # https://nixos.org/wiki/Emacs_configuration#Emacs_Systemd_Daemon
  # #
  # # With emacsclient, if Tramp doesn't prompt for password, make sure
  # # that you haven't an incorrectly configured `~/.authinfo(.gpg)'
  # # that causes Tramp to automatically send a default password. In
  # # this case, delete it.
  # systemd.user.services.emacs = {
  #   description = "Emacs Daemon";
  #
  #   environment = {
  #     SSH_AUTH_SOCK = "%t/ssh-agent";
  #     NIX_PATH = "${pkgs.lib.concatStringsSep ":" config.environment.profiles}";
  #     TERMINFO_DIRS = "/run/current-system/sw/share/terminfo";
  #     ASPELL_CONF = "/run/current-system/sw/lib/aspell";
  #   };
  #
  #   serviceConfig = {
  #     Type = "forking";
  #     ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
  #     ExecStop = "${pkgs.emacs}/bin/emacsclient --eval (kill-emacs)";
  #     Restart = "always";
  #   };
  #
  #    # Start emacs daemon with the rest of the user services
  #    wantedBy = [ "default.target" ];
  #  };
  # # Start emacs at login
  # systemd.services.emacs.enable = true;

  #-------------------------------------------------- App/Service Settings
  # Programs settings
  programs = {
    # Fix for emacs ssh https://github.com/NixOS/nix/issues/644
    # bash.promptInit = "PS1=\"# \"";
    bash.enableCompletion = true;

    gnupg.agent = {
      enable = true;
      pinentryFlavor = "curses";
    };

    tmux = {
      enable = true;
      clock24 = true;
      historyLimit = 65535;
      keyMode = "vi";
      terminal = "screen-256color";
      newSession = true;
    };

    zsh = {
      enable = true;
      # # Find the generated zprofile in /etc/static/zprofile
      # loginShellInit = ''
      # '';

      # Find the generated zshrc in /etc/static/zshrc
      interactiveShellInit = ''
        # Some utils functions
        . /etc/nixos/functions.sh

        # ctrl-p/n to move up/down a line in the buffer, or if already at
        # the top/bottom line, search back/forward in the history for a line
        # beginning with the first word in the buffer.
        #
        # > man zshzle  # seek for `up-line`
        #
        # Do a `cat -v` and press the key (e.g., ctrl-p) to find the
        # key value (e.g., ^P).
        bindkey -e '^P' up-line-or-search
        bindkey -e '^N' down-line-or-search

        # # TMUX
        # # https://wiki.archlinux.org/index.php/Tmux#Start_tmux_on_every_shell_login
        # if which tmux >/dev/null 2>&1; then
        #   #if not inside a tmux session, and if no session is started, start a new session
        #   test -z "$TMUX" && (tmux attach || tmux new-session)
        # fi
      '';

      shellAliases = {
        l = "ls -alh"; ll = "ls -l"; ls = "ls --color=tty";
        fu  = "sudo $(fc -ln -1 -1)";
        encrypt = "openssl enc -aes-256-cbc -salt";
        decrypt = "openssl enc -aes-256-cbc -salt -d";
        p = "xdg-open-background"; r = "ranger"; ns = "nix-shell";
        # emacs="emacsclient -c -a \"\""; # Start daemon or connect to it
      };

      autosuggestions = {
        enable = true;
      };

      ohMyZsh = {
        enable = true;
        theme = "norm";
        plugins = [ "git" "gitignore" ];
      };
    };
  };

  # Xorg settings
  hardware.opengl.driSupport32Bit = true;
  services.xserver = {
    # Enable the X11 windowing system.
    autorun = true;
    enable = true;

    displayManager = {
      # Session to pre-select in the session chooser (`none`
      # desktopManager + `i3` windowManger)
      defaultSession = "none+i3";


      # LightDM as login manager
      lightdm = {
        enable = true;
        autoLogin.user = "rfish";
      };

      # -- bash_profile for login shell
      # Shell commands executed just before the window or desktop
      # manager is started. An equivalent to the .bash_profile file
      # https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789
      #
      # http://unix.stackexchange.com/a/50667 ; man bash
      # Gentle reminder about bash:
      # - *Login* :: Means that the shell is run as part of the login of the
      #     user to the system. Typically used to do any configuration that
      #     a user needs/wants to establish his work-environment.
      # - `/etc/profile` :: The systemwide initialization file, executed for
      #     login shells.
      # - `~/.bash_profile` (or old `~/.profile`) :: The personal
      #     initialization file, executed for login shells.
      #
      # - *Interactive* :: As the term implies: Interactive means that the
      #     commands are run with user-interaction from keyboard. E.g. the
      #     shell can prompt the user to enter input.
      # - `~/.bashrc` :: The individual per-interactive-shell startup file
      #
      # The `mkBefore` ensures that this sessionCommands is read
      # before the one redefined in specific.nix.
      # https://nixos.org/nixos/manual/index.html#sec-modularity
      #
      # I can checks this by loading and reading the `config` argument
      # that contains the complete, merged, system configuration.
      #
      # nix-repl> :l <nixpkgs/nixos> # this (1) loads the specific.nix
      #                              # and current configuration.nix,
      #                              # (2) merges value to form the
      #                              # result of combining configuration
      #                              # returned by every module
      # nix-repl> :p config.services.xserver.displayManager.sessionCommands
      sessionCommands = lib.mkBefore ''
        ${pkgs.xcape}/bin/xcape -e 'Shift_L=Escape'
        ${pkgs.xcape}/bin/xcape -e 'Control_L=Escape'
      '';
    };

    desktopManager.xterm.enable = false;

    windowManager = {
      i3.enable = true;
      i3.extraPackages = with pkgs; [
        i3lock i3status-rust rofi
        # Disable standby (DPMS) and screensaver when a window is
        # fullscreen.
        # https://faq.i3wm.org/question/4217/how-to-disable-screensavermonitor-standby-when-fullscreen.1.html
        # https://github.com/altdesktop/i3ipc-python/blob/master/examples/disable-standby-fs.py
        # https://github.com/NixOS/nixpkgs/blob/c34ba30888a60065f3de99037625019a5a7d1227/pkgs/applications/window-managers/i3/wk-switch.nix
        (let i3-disable-dpms = {stdenv, fetchFromGitHub, python3Packages, xset}:
               python3Packages.buildPythonApplication rec {
                 pname = "i3-disable-dpms";
                 version = "2019-12-16";

                 src = fetchFromGitHub {
                   owner = "altdesktop";
                   repo = "i3ipc-python";
                   rev = "13d6e3c8f190381cce5ed38545ac0fd4c365f00c";
                   sha256 = "1g56faigwhmx00v3wya0npg53b6pcivl7cfyijmwzlx55arikqk7";
                 };

                 propagatedBuildInputs = with python3Packages; [ i3ipc ];
                 dontBuild = true;
                 doCheck = false;

                 installPhase = ''
                   mkdir -p "$out/bin"
                   cp examples/disable-standby-fs.py "$out/bin/${pname}"
                   chmod a+x "$out/bin/${pname}"
                 '';

                 meta = with stdenv.lib; {
                   homepage = https://github.com/altdesktop/i3ipc-python;
                   description = "Script to disable DPMS from the i3ipc-python library";
                   license = licenses.bsd3;
                 };
               };
         in i3-disable-dpms {
           stdenv = pkgs.stdenv;
           fetchFromGitHub = pkgs.fetchFromGitHub;
           python3Packages = pkgs.python3Packages;
           xset = pkgs.xset;
         })
      ];
    };

    # Symlink the X server configuration under /etx/X11/xorg.conf
    exportConfiguration = true;

    # US international so that I can easily type accented letter with a qwerty keyboard
    layout = "us";
    xkbVariant = "intl";
    xkbOptions = "ctrl:nocaps,terminate:ctrl_alt_bksp";

    # Mouse settings
    inputClassSections = [
      # Set mouse speed as `xset m 3/2 0`, see xset man page.
      ''
      Identifier "My Mouse"
      MatchIsPointer "yes"
      Option "AccelerationNumerator" "3"
      Option "AccelerationDenominator" "2"
      Option "AccelerationThreshold" "0"
      ''
    ];
  };

  # Audio settings (alsa)
  sound = {
    enable = true;
    mediaKeys.enable = false; # Managed by i3
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  # Enable redshift daemon.
  # $ systemctl --user status redshift.service
  services.redshift.enable = true;

  # Enable Unbound DNS and set it as DNS in resolv.conf. For
  # resolv.conf/nameservers see also nix
  # networking.networkmanager.appendNameservers and
  # networking.networkmanager.insertNameservers
  services.unbound = {
    # Enabling ubound automatically set:
    # networking.nameservers to [ "127.0.0.1" ];
    enable = true;
    # forward remaining requests to https://dns.watch/
    forwardAddresses = [
      "84.200.69.80" "84.200.70.40" # dns.watch
      "2001:1608:10:25::1c04:b12f" "2001:1608:10:25::9249:d69b"
      "9.9.9.9" "149.112.112.112"   # quad9.net
      "2620:fe::fe" "2620:fe::9"
    ];

  };
  # This is not take into account if `unbound.enable = true;`
  networking.nameservers = [
      "84.200.69.80" "84.200.70.40" # dns.watch
      "2001:1608:10:25::1c04:b12f" "2001:1608:10:25::9249:d69b"
      "9.9.9.9" "149.112.112.112"   # quad9.net
      "2620:fe::fe" "2620:fe::9" ];

  # Enable virtualization
  virtualisation.docker = {
    enable = true;
    # Only load docker service, but make it inactive. A command on the
    # docker cli (e.g., `docker ps` or `docker run`) will activate it.
    enableOnBoot = false;
    # Do `docker system prune -f` periodically.
    autoPrune.enable = true;
  };
  users.extraGroups.docker.members = [ "rfish" ];

  system.stateVersion = "19.03";
}

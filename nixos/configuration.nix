# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [./specific.nix];

  # Linux kernel 4.8
  boot.kernelPackages = pkgs.linuxPackages_4_8;

  # Tmp on tmpfs
  boot.tmpOnTmpfs = true;

  # Select internationalisation properties.
  i18n = {
    consoleUseXkbConfig = true;
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "CET";

  # Garbage collection
  nix.gc.automatic = true;
  nix.gc.dates = "00:01";

  # Font
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      fira                # Font of Firefox OS
      fira-mono           # Font of Firefox OS
      fira-code           # Fira Mono + programming ligatures
    ];

    fontconfig = {
      defaultFonts.monospace = [ "FiraCode" ];
    };
  };

  # My account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.rfish = {
    createHome = true;
    home = "/home/rfish";
    extraGroups = [ "wheel" "networkmanager" ];
    useDefaultShell = true;
    uid = 1000;
    shell = "/run/current-system/sw/bin/zsh";
  };

  # Environment variables
  environment.variables.EDITOR = "emacsclient -c";

  #------------------------------------------------------------------- App
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP|fgrep wget
  environment.systemPackages = with pkgs; [
    # system
    nix-repl ncat
    wget openssh stow htop unzip unrar gnupg tree
    ranger w3m xsel # w3m to print images, xsel to copy file name with `yd`
    usbutils # lsusb
    vagrant

    # windowing
    xlibs.libXft
    i3status rofi

    # soft
    firefox chromium
    imagemagick # import -window root screenshot.jpg
    inkscape
    kpcli xclip # xclip is used by kpcli to copy pass to clipboard
    qtox
    rxvt_unicode
    syncthing
    vlc
    xorg.xbacklight # Decrease screen brightness
    xcape # Escape and Control on a single key
    xdotool # Script your mouse
    zathura xournal # Pdf viewer + notetaking

    # development tools
    git
    emacs vim aspell aspellDicts.en aspellDicts.fr
    silver-searcher
    # Put here the list of needed tex packages. scheme-* collection-*
    # are predefined sets of tex packages. You can find theme using
    # nix-repl
    # nix-repl> :l <nixpkgs>
    # nix-repl> pkgs.texlive.coll<tab>
    # You can also put the name of a package directly, e.g.,
    # `moderncv`, find them with `nix-repl>
    # pkgs.textlive.<the-package>`.
    # see https://nixos.org/wiki/TexLive_HOWTO#New_TexLive_Infrastructure
    # Use parentheses to force the evaluation of the `combine`
    # function and compute the new texlive derivation
   (texlive.combine {
     inherit (texlive)
     # some tests: collection-basic ⊂ scheme-basic
     # nix-repl> :a lib
     # let scheme-basic = pkgs.texlive.scheme-basic.pkgs; in
     # let coll-basic = pkgs.texlive.collection-basic.pkgs; in
     # lib.filter (c: !lib.elem c scheme-basic) coll-basic
     # > [ ]
     #
     # This function test if a certain pakcage is contains inside a
     # collection, based on it's name. Find the doc of lib under
     # https://github.com/NixOS/nixpkgs/tree/master/lib
     #
     # let hasPackage = pkgName: pkgDerivation:
     #     let pkgNames = map (v: if (pkgs.lib.hasAttr "name" v)
     #                            then (pkgs.lib.getAttr "name" v)
     #                            else "") pkgDerivation;
     #     in pkgs.lib.lists.elem pkgName pkgNames;
     # in (hasPackage "texlive-ulem-2015" pkgs.texlive.ulem.pkgs)
     # > true
     # and to find the name
     # > pkgs.lib.getAttr "name" (pkgs.lib.head pkgs.texlive.bera.pkgs)
     scheme-small
     collection-latex collection-latexextra collection-langfrench
     # bera is for my thesis only, see if I cannot remove
     # it and put-it inside an nix-shell
     luatex collection-luatex bera
     # courier font is required by IEEETran (pcrr8t)
     courier
     # cm-super required by [T1]{fontspec}
     cm-super
     latexmk;
   })

    # language
    pltScheme
    openjdk sbt pmd
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
    bash.enableCompletion = true;

    tmux = {
      enable = true;
      clock24 = true;
      historyLimit = 65535;
      keyMode = "vi";
      terminal = "screen-256color";
    };

    zsh = {
      enable = true;
      interactiveShellInit = ". /etc/nixos/functions.sh";
      shellAliases = {
        l = "ls -alh"; ll = "ls -l"; ls = "ls --color=tty";
        pleaze  = "sudo $(history -p !!)";
        encrypt = "openssl enc -aes-256-cbc -salt";
        decrypt = "openssl enc -aes-256-cbc -salt -d";
        p = "xdg-open"; r = "ranger";
      };
    };
  };

  # Xorg settings
  hardware.opengl.driSupport32Bit = true;
  services.xserver = {
    # Enable the X11 windowing system.
    autorun = true;
    enable = true;
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    windowManager.i3.enable = true;
    windowManager.default = "i3";
    displayManager = {
      slim = {
        enable = true;
        defaultUser = "rfish";
        # theme = pkgs.fetchurl {
        #   url    = "https://github.com/jagajaga/nixos-slim-theme/archive/Final.tar.gz";
        #   sha256 = "4cab5987a7f1ad3cc463780d9f1ee3fbf43603105e6a6e538e4c2147bde3ee6b";
        # };
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
        ${pkgs.xcape}/bin/xcape -e 'Shift_L=Escape;Control_L=Escape'
        # ${pkgs.emacs}/bin/emacs --daemon
      '';
    };

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

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable virtualization
  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  nixpkgs.config.virtualbox.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = [ "rfish" ];

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
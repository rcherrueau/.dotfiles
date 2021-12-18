# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, options, lib, pkgs, ... }:

{
  imports = [
    ./cachix.nix
    ./modules/syncthing.nix
    ./modules/vim.nix
    ./modules/email.nix
  ];

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

  nix = {
    # Garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    # Workaround to `nix run ...`: "experimental Nix feature
    # 'nix-command' is disabled; use '--extra-experimental-features
    # nix-command' to override".
    #
    # See https://github.com/NixOS/nixpkgs/issues/80332
    extraOptions = ''
     experimental-features = nix-command flakes
    '';
  };

  nix.trustedUsers = [ "root" "rfish" ];

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
    isNormalUser = true;
  };
  users.defaultUserShell = pkgs.zsh;

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
    xclip 
    # TODO: vagrant

    # Circumvent the default /run/current-system/sw/share/applications/mimeinfo.cache
    # that is not overwritable in nixos and make my xdg-open always open pdf in
    # chromium or inkscape.
    # https://discourse.nixos.org/t/configure-how-xdg-open-opens-html-files/6419/17
    mimeo

    # windowing
    xlibs.libXft
    dunst # The lightweight notification-daemon

    # soft
    firefox chromium
    imagemagick # import -window root screenshot.jpg
    inkscape gimp
    mpv # replacement of mplayer
    (pass.withExtensions (passpkgs: with passpkgs; [ pass-otp pass-update ]))
    qtox
    rxvt-unicode-unwrapped
    xorg.xbacklight # Decrease screen brightness
    xcape # Escape and Control on a single key
    xdotool # Script your mouse
    zathura xournal # Pdf viewer + notetaking
    # mattermost CLI
    matterhorn
    # Notification and token fetcher for authentication
    #
    # I could handle the notification in an overlay, as following, but
    # this would rebuild matterhorn which takes forever. So, I prefer
    # the next solution that copy the script in
    # /run/current-system/sw/share/matterhorn/.
    #
    # > matterhorn = self.haskell.lib.overrideCabal super.matterhorn (old:
    # >   {
    # >     # buildInput = old.matterhorn.buildInput ++ [ self.libnotify];
    # >     postInstall = (old.postInstall or "") + ''
    # >       cp notification-scripts/notify $out/matterhorn-notify
    # >     '';
    # >   });
    #
    # See,
    # https://nixos.org/nixpkgs/manual/#chap-trivial-builders
    # https://nixos.org/nixpkgs/manual/#ssec-stdenv-functions
    (let
      matterhorn-notify-src = pkgs.matterhorn.src;
      mattermost-ffox-token = pkgs.fetchFromGitHub {
        owner = "ftilde";
        repo = "mattermost-session-cookie-firefox";
        rev = "606c3d8728bb532bedab03e648151beec52b98f8";
        sha256 = "10bcia9ghxlhcan0f77gr8gzq83b5ix9hgyjb0wz3ylmlds99lrb";
      };
    in (runCommand "matterhorn-extra" {
      buildInputs = with pkgs; [
        libnotify            # Deps for notification script
        curl firefox sqlite  # Deps for token handling
      ];
    } ''
      # Find scripts in /run/current-system/sw/share/matterhorn/
      OUTPUT="$out/share/matterhorn"
      mkdir -p "$OUTPUT"

      # Extract notification script from matterhorn sources
      tar -xvzf ${matterhorn-notify-src} ${pkgs.matterhorn.name}/notification-scripts/notify

      # Substitute dependencies in notification and token scripts
      substitute ${pkgs.matterhorn.name}/notification-scripts/notify "$OUTPUT/matterhorn-notify" \
           --replace 'notify-send' '${libnotify}/bin/notify-send'
      substitute ${mattermost-ffox-token}/mattermost-session-cookie-firefox \
           "$OUTPUT/matterhorn-token" \
           --replace 'sqlite3' '${sqlite}/bin/sqlite3' \
           --replace 'curl' '${curl}/bin/curl'

      # Make notification and token scripts executable
      chmod +x "$OUTPUT/matterhorn-notify"
      chmod +x "$OUTPUT/matterhorn-token"
    ''))

    # development tools
    direnv
    git
    emacs # (emacsWithPackages (epkgs: [epkgs.pdf-tools]))
    aspell aspellDicts.en aspellDicts.fr
    manix
    ripgrep ripgrep-all bat
    devdocs-desktop # zeal
    sqlite

    # language
    racket
  ];
  environment.pathsToLink = [ "/share/matterhorn" ];

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

    # Favor id_ed25519 over id_rsa when doing a ssh connexion
    ssh.hostKeyAlgorithms = [ "ssh-ed25519" "ssh-rsa" ];

    # Use gnupg agent for ssh connections, git sign, pass, ...
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
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

        # Trigger direnv at prompt
        eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
      '';

      shellAliases = {
        l = "ls -alh"; ll = "ls -l"; ls = "ls --color=tty";
        cat = "${pkgs.bat}/bin/bat --theme=Nord --style=plain";
        fgrep = "${pkgs.ripgrep}/bin/rg --fixed-strings";
        pdfgrep = "${pkgs.ripgrep-all}/bin/rga --rga-adapters=poppler";
        fu = "sudo $(fc -ln -1 -1)";
        encrypt = "openssl enc -aes-256-cbc -salt";
        decrypt = "openssl enc -aes-256-cbc -salt -d";
        p = "${pkgs.mimeo}/bin/mimeo"; r = "ranger"; ns = "nix-shell";
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
      lightdm.enable = true;

      autoLogin.enable = false;
      autoLogin.user = "rfish";

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
      sessionCommands = ''
        ${pkgs.xcape}/bin/xcape -e 'Shift_L=Escape;Control_L=Escape'
        ${pkgs.dunst}/bin/dunst &
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
        (let i3-disable-dpms = {stdenv, python3Packages, xset}:
               python3Packages.buildPythonApplication rec {
                 pname = "i3-disable-dpms";
                 version = python3Packages.i3ipc.version;
                 src = python3Packages.i3ipc.src;

                 propagatedBuildInputs = with python3Packages; [ i3ipc ];
                 dontBuild = true;
                 doCheck = false;

                 installPhase = ''
                   mkdir -p "$out/bin"
                   substitute examples/disable-standby-fs.py "$out/bin/${pname}" \
                       --replace 'xset' '${xset}/bin/xset'
                   chmod a+x "$out/bin/${pname}"
                 '';

                 meta = python3Packages.i3ipc.meta // {
                   homepage = https://github.com/altdesktop/i3ipc-python;
                   description = "Script to disable DPMS from the i3ipc-python library";
                 };
               };
         in i3-disable-dpms {
           stdenv = pkgs.stdenv;
           python3Packages = pkgs.python3Packages;
           xset = pkgs.xorg.xset;
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
      # Set mouse speed as `xset m 5/2 0`, see xset man page.
      # Mouse speed is 2.5 times as fast
      ''
      Identifier "My Mouse"
      MatchIsPointer "yes"
      Option "AccelerationNumerator" "5"
      Option "AccelerationDenominator" "2"
      Option "AccelerationThreshold" "0"
      ''
    ];
  };

  # Audio settings (alsa + pulseaudio)
  # https://nixos.wiki/wiki/PulseAudio
  # https://github.com/NixOS/nixpkgs/issues/79310
  sound = {
    enable = true;
    mediaKeys.enable = false; # Managed by i3
  };
  hardware.pulseaudio.enable = true;
  users.extraGroups.audio.members = [ "rfish" ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # OpenVPN client:
  services.openvpn.servers = {
    # Access to Grid5000. Activate it with `sudo systemctl start
    # openvpn-g5k`
    g5k = {
      autoStart = false;
      config = ''
        client
        remote vpn.grid5000.fr 1194 udp
        dev tun

        auth-retry interact
        ca /home/rfish/openvpn/g5k/cavpn.crt
        cert /home/rfish/openvpn/g5k/rcherrueau.crt
        key /home/rfish/openvpn/g5k/rcherrueau.key
        tls-auth /home/rfish/openvpn/g5k/ta.key
      '';
      # Update /etc/resolv.conf with g5k DNS
      updateResolvConf = true;
    };
  };

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

    # See `man unbound.conf`
    settings.forward-zone = [{
      name = ".";
      # forward DNS not in cache to:
      forward-addr = [
        "84.200.69.80" "84.200.70.40" # dns.watch
        "2001:1608:10:25::1c04:b12f" "2001:1608:10:25::9249:d69b"
        "9.9.9.9" "149.112.112.112"   # quad9.net
        "2620:fe::fe" "2620:fe::9"
      ];
    }];
  };

  # This is not taken into account if `unbound.enable = true;`
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

  # Environment variables used in the global /etc/profile.
  environment.variables = {
    XDG_CONFIG_HOME="$HOME/.config";

    # --RAW-CONTROL-CHARS to interpret ANSI colors
    # --LONG-PROMPT to display percent of the file,
    # --ignore-case to smartly ignore case during search
    LESS="--RAW-CONTROL-CHARS --LONG-PROMPT --ignore-case --quit-if-one-screen";
    PAGER="less $LESS";
  };
}

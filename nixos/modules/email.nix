{ config, pkgs, lib, ... }:

let
  # --------------------------------------------------------------------
  # Email accounts definition:

  accounts = [
    { # GMail
      default = true;
      name = "GMail";
      email = "RonanCherrueau@gmail.com";
      imap.host = "imap.gmail.com";
      smtp.host = "smtp.gmail.com";
      keepass = "/root/perso/GMail";
      signature = pkgs.writeText "gmail-sign" ''
        Ronan-Alexandre Cherrueau
      '';
    }
    { # Inria
      name = "Inria";
      email = "Ronan-Alexandre.Cherrueau@inria.fr";
      imap.host = "zimbra.inria.fr";
      smtp.host = "smtp.inria.fr";
      smtp.user = "rcherrue";
      keepass = "/root/Inria/inria";
      signature = pkgs.writeText "inria-sign" ''
        Ronan-A. Cherrueau
        https://rcherrueau.github.io
      '';
    }
    { # IMT
      # https://intranet.imt-atlantique.fr/assistance-support/informatique/didacticiels/
      name = "IMT";
      email = "Ronan-Alexandre.Cherrueau@imt-atlantique.fr";
      imap.host = "z.imt.fr";
      smtp.host = "z.imt.fr";
      keepass = "/root/IMT/rcherr12";
      signature = pkgs.writeText "inria-sign" ''
        Ronan-A. Cherrueau
        https://rcherrueau.github.io
      '';
    }
  ];

  # --------------------------------------------------------------------
  # Notmuch tagging script
  notmuchTags = pkgs.writers.writeDash "notmuch-tag-mails" ''
    # Index new emails
    ${notmuchWp}/bin/notmuch new

    # Tag new mails according to their folder path
    # > notmuch tag +inria -- new path:inria/**
    ${lib.concatMapStrings (name: ''
      ${notmuchWp}/bin/notmuch tag +${name} -- path:${name}/**
    '') (map (lib.getAttr "name") accounts)}

    # GMail specific configurations
  '';

  # --------------------------------------------------------------------
  # Configurations files

  # mbsync configuration file (man mbsync)
  mbsyncrc = pkgs.writeText "mbsyncrc" ''
    CopyArrivalDate yes  # Keeps the time stamp based message sorting intact

    # Declare accounts
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      IMAPAccount ${acc.name}
      User        ${acc.email}
      Host        ${acc.imap.host}
      SSLType     IMAPS
      PassCmd     "${getPasswd} ${acc.keepass}"
    '') accounts}

    # A Store defines a collection of mailboxes; basically a folder,
    # either local or remote. Here we specify the remote and local Store:

    ## - The remote store (IMAPStore) is where we get the email from
    ##   (e.g., the specification of an imap account)
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      IMAPStore ${acc.name}-remote
      Account   ${acc.name}
    '') accounts}

    ## - The local store (MaildirStore) is where we store the email on
    ##   our computer.
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      MaildirStore ${acc.name}-local
      Path         ${mailDir}/${acc.name}/
      Inbox        ${mailDir}/${acc.name}/inbox
      SubFolders   Verbatim
    '') accounts}

    # A Channel connects two Stores, describing the way the two are
    # synchronized.
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      Channel  ${acc.name}-inbox
      Master   :${acc.name}-remote:
      Slave    :${acc.name}-local:
      Patterns "*" "!Trash" "![Gmail]*" # Will copy all remote email accounts as is except Trash
      Create   Slave  # Automatically create missing mailboxes on the Slave.
      Expunge  Slave  # Only delete on Slave
      Sync     All
      MaxSize  100m # Don't download any email greater than this
    '') accounts}
  '';

  # msmtp configuration file (man msmtp)
  msmtprc = pkgs.writeText "msmtprc" ''
    # Set default values for all following accounts
    defaults
    auth            on
    tls             on
    syslog          on

    # Accounts
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      account       ${acc.name}
      host          ${acc.smtp.host}
      port          587
      from          ${acc.email}
      user          ${if acc.smtp ? "user" then acc.smtp.user else acc.email}
      passwordeval  ${getPasswd} ${acc.keepass}
    '') accounts}

    # Set a default account
    account default : ${(findDefaultAccount accounts).name}
  '';

  # Configuration file for notmuch (man notmuch-config)
  notmuchConfig = pkgs.writeText "notmuch-config" ''
    [database]
    path=${mailDir}

    [user]
    name=Ronan-Alexandre Cherrueau
    primary_email=${(findDefaultAccount accounts).email}
    other_email=${lib.concatMapStringsSep ";" (lib.getAttr "email") accounts}

    # Configuration for "notmuch new"
    # - tags:	A list (separated by ';') of the tags that will be
    #   added to all messages incorporated by "notmuch new".
    # - ignore: A list (separated by ';') of file and directory names
    #   that will not be searched for messages by "notmuch new".
    [new]
    tags=inbox;unread;
    ignore=

    # Search configuration
    #
    # - exclude_tags: A separated list of tags that will be excluded
    #   from search results by default. Using an excluded tag in a
    #   query will override that exclusion.
    [search]
    exclude_tags=deleted;spam;

    # Maildir flags have precedence over the `new` tagging. Thus an
    # already read mail gets its initial `unread` tag correctly
    # removed.
    [maildir]
    synchronize_flags=true
  '';

  # Astroid GUI config
  # https://github.com/astroidmail/astroid/wiki/Configuration-Reference
  astroidConfig = pkgs.runCommand "astroid-config" {
    buildInputs = [ pkgs.jq ];
  } ''
    # Merge default config with account information
    echo ${lib.escapeShellArg astroidDefaultConfig} | \
    jq -s '.[] as $in | $in * {
       "astroid" : {
         "notmuch_config": "${notmuchConfig}",
         "debug": { "dryrun_sending": true },
         "hints": { "level": -1 },
         "log": { "syslog": true },
       },
       "startup": {
         "queries": {
           ${lib.concatMapStringsSep "," (name: ''
           "${name}" : "tag:${name} and tag:inbox"
           '') (map (lib.getAttr "name") accounts)}
         }
       },
       "editor": {
         "cmd": "emacs --parent-id %3 %1",
         "external_editor": false,
         "attachment_words": ["attach", "jointe", "p.-j."],
       },
       "thread_index": { "cell": {
           "line_spacing": 3,
           "message_count_length": 5,
           "authors_length": 33,
           "tags_alpha": 1,
       }},
       "general": { "time": {
         "diff_year": "%F",
       }},
       "mail": {
         "send_delay": 20,
       },
       "accounts": {
         ${lib.concatMapStringsSep "," (acc: ''
         "${acc.name}": {
            "name": "Ronan-Alexandre Cherrueau",
            "email": "${acc.email}",
            "sendmail": "${msmtpWp} -i --read-recipents --account=${acc.name}",
            "always_gpg_sign": false,
            "save_sent": true,
            "save_sent_to": "${mailDir}/${acc.name}/sent/cur/",
            "save_draft_to": "${mailDir}/${acc.name}/drafts/",
            "signature_file": "${acc.signature}",
            "signature_separate": true,
            ${lib.optionalString (acc ? default) ''
              "default": ${(lib.boolToString acc.default)}
            ''}
         }
         '') accounts}
       },
    }' > $out
  '';

  # --------------------------------------------------------------------
  # Utils

  # Directory to store emails
  mailDir = config.users.users.rfish.home + "/.mail";

  # Default account
  findDefaultAccount = accounts:
    let errorMsg = "cannot find a default email account";
    in lib.lists.findFirst
        # Get the acc.default key or false
        (acc: if acc ? default then acc.default else false)
        (abort errorMsg) accounts;

  # Read password from KeePassX
  getPasswd = pkgs.writers.writeDash "kpcli-read-passwd" ''
    # Get the password
    PASSWD=$(TERM=xterm ${pkgs.kpcli}/bin/kpcli \
                 --kdb ${config.users.users.rfish.home}/secret/passwd.kdbx \
                 --pwfile /etc/nixos/secret/keepass \
                 --readonly \
                 --command "show -f $*" \
             | grep Pass: \
             | sed -Er 's/ Pass: (.+)/\1/g')

    # Display it
    echo "$PASSWD"
  '';

  # Create the directory for local email stores
  mkLocalStores = pkgs.writers.writeDash "msbync-local-stores" ''
    ${lib.concatMapStrings (name: ''
       mkdir -p ${mailDir}/${name}/
       chown ${config.users.users.rfish.name}:${config.users.users.rfish.group} ${mailDir}/${name}/
    '') (map (lib.getAttr "name") accounts)}
  '';

  # Create the default astroid config
  #
  # XXX: astroid cannot open DISPLAY `:` and so segfault before
  # generating the configuration.  I workaround it with `xvfb` to get
  # a dummy DISPLAY.
  #
  # Debug:
  # - Build locally in the nix REPL with `:b astroidDefaultConfig`
  # - Pop a shell in the nix REPL with `:s astroidDefaultConfig`
  # - See the log of the build with `nix log <<path-of-derivation.drv>>`
  #
  # https://github.com/astroidmail/astroid/issues/579
  # https://github.com/astroidmail/astroid/issues/516
  astroidDefaultConfig = builtins.fromJSON (
    pkgs.runCommand "astroid-default-config" {buildInputs = [ pkgs.astroid pkgs.xvfb_run ];} ''
      export HOME=nixos/tmphome
      ${pkgs.xvfb_run}/bin/xvfb-run -d \
        ${pkgs.astroid}/bin/astroid --disable-log --new-config --config $out
    '');

  # Wraps astroid to call the custom config.
  #
  # We want astroid to be called with our custom config. I can make a
  # shell script that does so:
  # > astroidWithConf = pkgs.writers.writeDashBin "astroid" ''
  # >   ${pkgs.astroid}/bin/astroid --config ${astroidConfig} $*
  # > '';
  # However, such a shell script does not propagate man pages and
  # other paths from the old pkgs.astroid derivation. A better
  # solution is to go with wrapProgram.
  #
  # wrapProgram is done most of the time in the `postFixup` phase.  We
  # can `override` the astroid derivation to add the wrapping of the
  # program, but this will require the recompilation of astroid. A
  # better solution is the following
  #
  # See https://nixos.wiki/wiki/Nix_Cookbook
  # https://github.com/ahmedtd/nixpkgs/blob/49aa2483e0a6ced59b46655c523e1399e27220d1/pkgs/build-support/setup-hooks/make-wrapper.sh
  astroidWp = pkgs.symlinkJoin {
    name = "astroid";
    paths = [ pkgs.astroid ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/astroid \
        --set NOTMUCH_CONFIG "${notmuchConfig}" \
        --add-flags "--config ${astroidConfig}"
    '';
  };

  # Wraps notmuch to call the custom config
  notmuchWp = pkgs.symlinkJoin {
    name = "notmuch";
    paths = [ pkgs.notmuch ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/notmuch \
        --set NOTMUCH_CONFIG "${notmuchConfig}"
    '';
  };

  # Wraps msmtp to call the custom config
  msmtpWp = pkgs.symlinkJoin {
    name = "msmtp";
    paths = [ pkgs.msmtp ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/msmtp \
        --add-flags "--file=${msmtprc}"
    '';
  };

in {
  environment.systemPackages = with pkgs; [
    isync       # to fetch email (mbsync)
    notmuchWp   # to index and search email
    msmtpWp     # to send email
    astroidWp   # GUI
  ];

  # Configure mbsync + notmuch
  # See https://wiki.archlinux.org/index.php?title=Isync&oldid=627584#Automatic_synchronization
  systemd.user.services.mbsync =
    let HOME = config.users.users.rfish.home;
    in {
      description = "Mailbox synchronization service";
      startAt = [ "*:00/5" ];  # Pull every 5 minutes
      environment.NOTMUCH_CONFIG=notmuchConfig;  # notmuch configuration file
      serviceConfig = {
        Type = "oneshot";
        # Synchronize emails
        ExecStart = [''
          ${pkgs.isync}/bin/mbsync --config ${builtins.trace ''
            msmtp ${msmtprc}
            notmuch ${notmuchConfig}
            astroid ${astroidConfig}''
            mbsyncrc} -Va
        ''];
        ExecStartPre =  [ mkLocalStores ];
        ExecStartPost = [ notmuchTags ];
      };
    };
}

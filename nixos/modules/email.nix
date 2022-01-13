{ config, pkgs, lib, ... }:

# Debug:
# - Build locally in the nix REPL with `:b notmuchWp`
# - Pop a shell in the nix REPL with `:s notmuchWp`
# - See the log of the build with `nix log <<path-of-derivation.drv>>`

let
  # --------------------------------------------------------------------
  # Email accounts:
  accounts = (import ./email-accounts.nix);

  # --------------------------------------------------------------------
  # Configurations files

  # mbsync configuration file (man mbsync)
  mbsyncrc = pkgs.writeText "mbsyncrc" ''
    CopyArrivalDate yes  # Keeps the time stamp based message sorting intact

    # Declare accounts
    ${lib.concatMapStringsSep "\n" (acc: with acc; ''
      IMAPAccount ${name}
      User        ${email}
      Host        ${imap.host}
      SSLType     IMAPS
      PassCmd     "${pkgs.pass}/bin/pass ${lib.escapeShellArg pass} | head -n1"
    '') accounts}

    # A Store defines a collection of mailboxes; basically a folder,
    # either local or remote. Here we specify the remote and local Store:

    ## - The remote store (IMAPStore) is where we get the email from
    ##   (e.g., the specification of an imap account)
    ${lib.concatMapStringsSep "\n" (acc: with acc; ''
      IMAPStore ${name}-remote
      Account   ${name}
    '') accounts}

    ## - The local store (MaildirStore) is where we store the email on
    ##   our computer.
    ${lib.concatMapStringsSep "\n" (acc: with acc; ''
      MaildirStore ${name}-local
      Path         ${mailDir}/${name}/
      Inbox        ${mailDir}/${name}/${boxes.inbox}
      SubFolders   Verbatim
    '') accounts}

    # A Channel connects two Stores, describing the way the two are
    # synchronized.
    ${lib.concatMapStringsSep "\n" (acc: with acc; ''
      Channel  ${name}-inbox
      Far      :${name}-remote:  # Master
      Near     :${name}-local:   # Slave
      Patterns "${boxes.inbox}" "${boxes.trash}" "${boxes.drafts}" "${boxes.sent}"
      Sync     All   # Propagate read, deletion ...
      Create   Near  # Automatically create missing mailboxes on the Slave.
      Expunge  Near  # Only delete on Slave (do `mbysnc --expunge-far ${name}-inbox` to delete)
      MaxSize  100m  # Don't download any email greater than this
    '') accounts}
  '';

  # notmuch configuration file (man notmuch-config)
  notmuchConfig = pkgs.writeText "notmuch-config" ''
    [database]
    path=${mailDir}

    [user]
    name=Ronan-Alexandre Cherrueau
    primary_email=${defaultAccount.email}
    other_email=${lib.concatMapStringsSep ";" (acc: acc.email) accounts}

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

    # Extra search keys, may require `notmuch reindex '*'` to use them
    [index]
    header.List=List-Id
    header.DeliveredTo=Delivered-To
  '';

  # --------------------------------------------------------------------
  # Software

  # Wraps mbsync to call the custom config
  #
  # We want mbsync to be called with our custom config. I can make a
  # shell script that does so:
  # > mbsyncWithConf = pkgs.writers.writeDashBin "mbsync" ''
  # >   ${pkgs.isync}/bin/mbsync --config ${mbsyncrc} $*
  # > '';
  # However, such a shell script does not propagate man pages and
  # other paths from the old pkgs.isync derivation.  A general
  # solution is to go with `wrapProgram`.  wrapProgram is done most of
  # the time in the `postFixup` phase.  We can `override` the isync
  # derivation to add the wrapping of the program in the postFixup
  # phase, but this will require the recompilation of isync.  As a
  # better solution, use `wrapProgram` with `pkgs.symlinkJoin`.
  #
  # See https://nixos.wiki/wiki/Nix_Cookbook
  # https://github.com/ahmedtd/nixpkgs/blob/49aa2483e0a6ced59b46655c523e1399e27220d1/pkgs/build-support/setup-hooks/make-wrapper.sh
  mbsyncWp = pkgs.symlinkJoin {
    name = "mbsync";
    paths = [ pkgs.isync ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/mbsync \
        --add-flags "--config ${mbsyncrc}"
    '';
  };

  # Wraps notmuch to call the custom config
  #
  # I have to symlink man, info, emacs files manually because package
  # notmuch defines separate outputs for these and are not part of the
  # default "$out".  I have to access them with
  # `notmuch.(man|info|emacs)`.
  notmuchWp = pkgs.runCommand "notmuch" {
    buildInputs = [ pkgs.makeWrapper ];
  } ''
    mkdir -p $out/bin $out/include $out/lib $out/share

    # Link every top-level folder from pkgs.notmuch to our new
    # target ....
    ln -s ${pkgs.notmuch}/bin/* $out/bin/
    ln -s ${pkgs.notmuch}/include/* $out/include/
    ln -s ${pkgs.notmuch}/lib/* $out/lib/
    ln -s ${pkgs.notmuch.man}/share/man $out/share/
    ln -s ${pkgs.notmuch.info}/share/info $out/share/
    ln -s ${pkgs.notmuch.emacs}/bin/* $out/bin
    ln -s ${pkgs.notmuch.emacs}/share/emacs $out/share/

    # ... except the notmuch binary ...
    rm $out/bin/notmuch

    # ... because we create this ourself, by creating a wrapper
    makeWrapper ${pkgs.notmuch}/bin/notmuch $out/bin/notmuch \
      --set NOTMUCH_CONFIG "${notmuchConfig}"
  '';

  # MUA application
  muaApp =
    let muaPath = config.users.users.rfish.home +
          "/prog/APE/mail/scala/target/scala-3.1.0/" +
          "mail-user-agent-assembly-0.1.0.jar";
        taggingScriptPath = config.users.users.rfish.home +
          "/prog/APE/mail/scala/taggingScript";
        muaWP = pkgs.writeShellScriptBin "mua" ''
          export MBSYNC_BIN='${mbsyncWp}/bin/mbsync'
          export NOTMUCH_BIN='${notmuchWp}/bin/notmuch'
          export TAGGING_SCRIPT='${taggingScriptPath}'
          export MAILDIR=${lib.escapeShellArg mailDir}
          export ACCOUNTS=${lib.escapeShellArg (builtins.toJSON accounts)}
          exec "${pkgs.jdk}/bin/java" "-jar" "${muaPath}" "$@"
        '';
    in pkgs.runCommand "mua" {
      buildInputs = [mbsyncWp notmuchWp pkgs.jdk muaWP];
    } ''
      mkdir -p $out/bin
      ln -s ${muaWP}/bin/mua $out/bin/mua
    '';

  # --------------------------------------------------------------------
  # Utils

  # The default account
  defaultAccount =
    let errorMsg = "cannot find a default email account";
        # Test if an account has the `default` key set to `true`
        isDefault = acc: acc ? default && acc.default;
        # Find the first default account
    in lib.lists.findFirst isDefault (abort errorMsg) accounts;

  # Directory to store emails
  mailDir = config.users.users.rfish.home + "/.mail";

  # Create the directory for local email stores
  mkLocalStores = pkgs.writers.writeDash "msbync-local-stores" ''
    ${lib.concatMapStrings (acc: ''
       mkdir -p ${mailDir}/${acc.name}/
       chown ${config.users.users.rfish.name}:${config.users.users.rfish.group} ${mailDir}/${acc.name}/
    '') accounts}
  '';

in {
  environment.systemPackages = with pkgs; [
    mbsyncWp    # to fetch email (mbsync)
    notmuchWp   # to index and search email
    muaApp      # Scala MUA app
  ];

  # Configure mbsync + notmuch
  # See https://wiki.archlinux.org/index.php?title=Isync&oldid=627584#Automatic_synchronization
  systemd.user.services.polling-email = {
      description = "Mail User Agent";
      startAt = [ "*:00/5" ];  # Pull every 5 minutes
      # Start mua service after the user logging.
      # XXX: I would like to use "network-online.target" but systemd
      # --user runs as a separate process from the systemd --system
      # process.  User units can not reference or depend on system
      # unit.
      requires = [ "default.target" ];
      after = [ "default.target" ];
      restartIfChanged = true;
      environment.NOTMUCH_CONFIG=notmuchConfig;  # notmuch configuration file
      serviceConfig = {
        Type = "oneshot";
        ExecStartPre =  [
          # Build the local store if any
          mkLocalStores
          # Deleted emails may not work if `notmuch new` has not been
          # executed first.  I prefixed the command with a dash `-` to
          # tell systemd to ignore the result.
          "-${muaApp}/bin/mua delete"
        ];
        # Synchronize emails
        ExecStart = "${muaApp}/bin/mua pull";
        ExecStartPost = [];
      };
    };
}

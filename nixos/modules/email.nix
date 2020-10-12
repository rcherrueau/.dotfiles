{ config, pkgs, lib, ... }:

let
  # --------------------------------------------------------------------
  # Email accounts:
  accounts = rec {
    Gmail = {
      default = true;
      email = "RonanCherrueau@gmail.com";
      imap.host = "imap.gmail.com";
      smtp.host = "smtp.gmail.com";
      keepass = "/root/perso/GMail";
      trash = "[Gmail]/Bin";
      signature = pkgs.writeText "gmail-sign" ''
        Ronan-Alexandre Cherrueau
      '';
    };
    Inria = {
      email = "Ronan-Alexandre.Cherrueau@inria.fr";
      imap.host = "zimbra.inria.fr";
      smtp.host = "smtp.inria.fr";
      smtp.user = "rcherrue";
      keepass = "/root/Inria/inria";
      trash = "Trash";
      signature = pkgs.writeText "inria-sign" ''
        Ronan-A. Cherrueau
        https://rcherrueau.github.io
      '';
    };
    IMT = {
      # https://intranet.imt-atlantique.fr/assistance-support/informatique/didacticiels/
      inherit (Inria) signature trash;
      email = "Ronan-Alexandre.Cherrueau@imt-atlantique.fr";
      imap.host = "z.imt.fr";
      smtp.host = "z.imt.fr";
      keepass = "/root/IMT/rcherr12";
    };
  };

  # --------------------------------------------------------------------
  # Notmuch tagging script
  notmuchTags = pkgs.writeText "notmuch-tag-mails" ''
    # Tag new mails according to their folder path
    # > notmuch tag +inria -- path:inria/**
    ${lib.concatMapStrings (name: ''
    +${name} -- path:${name}/**
    '') accountNames}

    +ISP -- from:freetelecom.fr or from:free-mobile.fr or from:assistance.free.fr
    +Banque -- from:ca-atlantique-vendee.fr or from:ing.com
    +List +G5k -- list:*.lists.grid5000.fr or from:grid5000.fr
    # Require `notmuch config set index.header.Listid List-ID` and `notmuch reindex '*'`
    +List +Nix -- Listid:24d1741146b951f90adf436fdmc
    +List +Racket -- Listid:2d4bcd7724e2a351c8e594233mc
    # +deleted -- subject:[SPAM]
  '';

  # --------------------------------------------------------------------
  # Configurations files

  # mbsync configuration file (man mbsync)
  mbsyncrc = pkgs.writeText "mbsyncrc" ''
    CopyArrivalDate yes  # Keeps the time stamp based message sorting intact

    # Declare accounts
    ${lib.concatMapStringsSep "\n" (name: with accounts.${name}; ''
      IMAPAccount ${name}
      User        ${email}
      Host        ${imap.host}
      SSLType     IMAPS
      PassCmd     "${getPasswd} ${keepass}"
    '') accountNames}

    # A Store defines a collection of mailboxes; basically a folder,
    # either local or remote. Here we specify the remote and local Store:

    ## - The remote store (IMAPStore) is where we get the email from
    ##   (e.g., the specification of an imap account)
    ${lib.concatMapStringsSep "\n" (name: ''
      IMAPStore ${name}-remote
      Account   ${name}
    '') accountNames}

    ## - The local store (MaildirStore) is where we store the email on
    ##   our computer.
    ${lib.concatMapStringsSep "\n" (name: ''
      MaildirStore ${name}-local
      Path         ${mailDir}/${name}/
      Inbox        ${mailDir}/${name}/inbox
      SubFolders   Verbatim
    '') accountNames}

    # A Channel connects two Stores, describing the way the two are
    # synchronized.
    ${lib.concatMapStringsSep "\n" (name: with accounts.${name}; ''
      Channel  ${name}-inbox
      Master   :${name}-remote:
      Slave    :${name}-local:
      Patterns "*" "![Gmail]*" "${trash}" # Copy all remote email box as is except [Gmail]
      Sync     All    # Propagate read, deletion ...
      Create   Slave  # Automatically create missing mailboxes on the Slave.
      Expunge  Slave  # Only delete on Slave
      MaxSize  100m   # Don't download any email greater than this
    '') accountNames}
  '';

  # msmtp configuration file (man msmtp)
  msmtprc = pkgs.writeText "msmtprc" ''
    # Set default values for all following accounts
    defaults
    auth            on
    tls             on
    syslog          on

    # Accounts
    ${lib.concatMapStringsSep "\n" (name: with accounts.${name}; ''
      account       ${name}
      host          ${smtp.host}
      port          587
      from          ${email}
      user          ${if smtp ? user then smtp.user else email}
      passwordeval  ${getPasswd} ${keepass}
    '') accountNames}

    # Set a default account
    account default : ${defaultAccount.name}
  '';

  # notmuch configuration file (man notmuch-config)
  notmuchConfig = pkgs.writeText "notmuch-config" ''
    [database]
    path=${mailDir}

    [user]
    name=Ronan-Alexandre Cherrueau
    primary_email=${defaultAccount.value.email}
    other_email=${builtins.concatStringsSep ";"
      (lib.catAttrs "email" (builtins.attrValues accounts))}

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

  # Astroid configuration
  # See https://github.com/astroidmail/astroid/wiki/Configuration-Reference
  astroidConfig = (pkgs.formats.json {}).generate "astroid-config.json" (astroidDefaultConfig // {
    accounts = builtins.mapAttrs (name: acc: with acc; {
      name = "Ronan-Alexandre Cherrueau";
      email = "${email}";
      sendmail = "${msmtpWp}/bin/msmtpq --read-envelope-from --read-recipients --account=${name}";
      always_gpg_sign = false;
      save_sent = true;
      save_sent_to = "${mailDir}/${name}/sent/cur/";
      save_draft_to = "${mailDir}/${name}/Drafts/";
      signature_file = "${signature}";
      signature_separate = true;
      default = if acc ? default then default else false;
    }) accounts;
    startup.queries =
      # { Inria = "tag:inbox and tag:Inria"; Gmail = "tag:inbox and tag:Gmail" }
      lib.genAttrs accountNames (name: "tag:inbox and tag:${name}");
    poll.interval = 0; # Disable automatic polling from astroid (managed by systemd)
    astroid = {
      notmuch_config = notmuchConfig;
      debug.dryrun_sending = true;
      hints.level = -1;
      log.syslog = true;
    };
    editor = {
      cmd = ''
        emacs --parent-id %3 --eval '(progn (find-file "%1")
                                            (setq mail-setup-with-from nil)
                                            (mail-mode))'
      '';
      external_editor = false;
      attachement_words = lib.concatStringsSep ","
        [ "attach" "attachement"
          "p.-j." "pièce jointe" "pièce-jointe" "ci-joint"];
    };
    # Thread index is the "list of emails" view
    thread_index.cell = {
      line_spacing = 3;
      message_count_length = 5;
      authors_length = 33;
      tags_alpha = 1;
      # Don't show these tags
      hidden_tags = lib.concatStringsSep ","
        [ "attachment" "flagged" "unread" "replied" "inbox" "List" ];
    };
    general.time.diff_year = "%F";
    mail.send_delay = 20;  # Wait 20 seconds before sending email
  });

  # Move deleted emails to trash box and delete old emails.
  deleteMails =
    let getTrashBox = name: with accounts.${name}; "${name}/${trash}";
        trashBoxNames = map getTrashBox accountNames;
        trashBoxesQuery = lib.concatMapStringsSep " OR " (box: "folder:${box}") trashBoxNames;
    in pkgs.writers.writeBash "delete-emails" ''
      # Move emails of a notmuch QUERY into BOX
      #
      # Note: mbsync adds a unique identifier to file names (e.g.,
      # `/path/to/mail,U=<UID>:2,SR` -- with `2` stands for the version of
      # UID generation if I am right).  Moving files causes UID conflicts
      # and prevent mbsync from syncing with "Maildir error: UID 9610 is
      # beyond highest assigned UID 86."  The sed command in the following
      # removes the UID to force mbsync to regenerate one and avoid UID
      # conflicts.
      function moveToBox {
        local QUERY="$1"
        local BOX="$2"

        for EMAIL_PATH in $(${notmuchWp}/bin/notmuch search --output=files "$QUERY")
        do
          # Strip UID from email name
          EMAIL_BASENAME=$(basename "$EMAIL_PATH")
          EMAIL_NO_UID=$(echo "$EMAIL_BASENAME" | sed -r 's/U=[0-9]+:2/U=:2/g')
          EMAIL_IN_BOX="${mailDir}/$BOX/cur/$EMAIL_NO_UID"
          # Move email to $BOX
          echo "Move email from $EMAIL_PATH to $EMAIL_IN_BOX"
          mv "$EMAIL_PATH" "$EMAIL_IN_BOX"
        done
      }

      # # Delete emails older than 30 days
      # ${notmuchWp}/bin/notmuch search --output=files --format=text0 \
      #   tag:deleted and date:..30days \
      #   | xargs -0 --no-run-if-empty rm

      # Move emails marked as deleted into the Trash box
      ${lib.concatMapStrings (name: ''
         moveToBox "tag:${name} AND tag:deleted NOT (${trashBoxesQuery})" \
                   ${getTrashBox name}
      '') accountNames}

      # Remove the inbox tag for emails in trash box (this also
      # systematically tags emails as +deleted in case some emails have
      # been deleted from the webmail interface)
      ${notmuchWp}/bin/notmuch tag -inbox +deleted -- ${trashBoxesQuery}
    '';

  # --------------------------------------------------------------------
  # Utils

  # List of account names
  accountNames = builtins.attrNames accounts;

  # The default account -> {name = defaultAccountName; value = defaultAccountValues;}
  defaultAccount =
    let errorMsg = "cannot find a default email account";
        # Test if an account has the `default` key set to `true`
        isDefault = acc: acc ? default && acc.default;
        # List of account values with a special attribute name
        accountWithNames = lib.mapAttrsToList (name: acc: acc // {name = name;} ) accounts;
        # Find the first default account
        acc = lib.lists.findFirst isDefault (abort errorMsg) accountWithNames;
    in { name = acc.name; value = acc; };

  # Directory to store emails
  mailDir = config.users.users.rfish.home + "/.mail";

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
  astroidDefaultConfig = builtins.fromJSON (lib.readFile (
    pkgs.runCommand "astroid-default-config" {buildInputs = [ pkgs.astroid pkgs.xvfb_run ];} ''
      export HOME=nixos/tmphome
      ${pkgs.xvfb_run}/bin/xvfb-run -d \
        ${pkgs.astroid}/bin/astroid --disable-log --new-config --config $out
    ''));

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
        --add-flags "--config=${astroidConfig}"
    '';
  };

  # Wraps mbsync to call the custom config
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
    mbsyncWp    # to fetch email (mbsync)
    notmuchWp   # to index and search email
    msmtpWp     # to send email
    astroidWp   # GUI
  ];

  # Configure mbsync + notmuch
  # See https://wiki.archlinux.org/index.php?title=Isync&oldid=627584#Automatic_synchronization
  systemd.user.services.polling-email =
    let HOME = config.users.users.rfish.home;
    in {
      description = "Mailbox synchronization service";
      startAt = [ "*:00/10" ];  # Pull every 10 minutes
      environment.NOTMUCH_CONFIG=notmuchConfig;  # notmuch configuration file
      serviceConfig = {
        Type = "oneshot";
        ExecStartPre =  [
          # Build the local store if any
          mkLocalStores
          # Notify astroid about new polling
          #
          # XXX: `astroid --start-polling` ends with a coredump but
          # seems to work!  I prefixed the command with a dash `-` to
          # tell systemd to ignore the result.
          "-${astroidWp}/bin/astroid --start-polling"
          # Deleted emails may not work if `notmuch new` has not been
          # executed first.  I prefixed the command with a dash `-` to
          # tell systemd to ignore the result.
          "-${deleteMails}"
        ];
        # Synchronize emails
        ExecStart = "${builtins.trace ''
                       mbsync ${mbsyncrc}
                       msmtp ${msmtprc}
                       notmuch ${notmuchConfig}
                       astroid ${astroidConfig}
                       deleteEmail ${deleteMails}
                       notmuchTags ${notmuchTags} ''
                       mbsyncWp}/bin/mbsync -Va";
        ExecStartPost = [
          # Index new emails
          "${notmuchWp}/bin/notmuch new"
          # Tag emails
          "${notmuchWp}/bin/notmuch tag --batch --input=${notmuchTags}"
          # Stop notifying astoid
          "-${astroidWp}/bin/astroid --stop-polling"
        ];
      };
    };
}

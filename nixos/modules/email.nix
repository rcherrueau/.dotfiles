{ config, pkgs, lib, ... }:

let
  # Email accounts
  accounts = [
    # Inria
    {
      name = "inria";
      mail = "Ronan-Alexandre.Cherrueau@inria.fr";
      imap = {
        host = "zimbra.inria.fr";
        ssl  = "IMAPS";
      };
      passcmd = "cat /etc/nixos/secret/mbsync-inria";
    }
    # # Gmail
    # {
    #   name = "gmail";
    #   mail = "RonanCherrueau@gmail.com";
    #   imap = {
    #     host = "imap.gmail.com";
    #     ssl  = "IMAPS";
    #   };
    #   passcmd = "";
    # }
  ];
  account-names = map (lib.getAttr "name") accounts;
  # Directory to store emails
  mail-dir = config.users.users.rfish.home + "/.mail";
  # Notmuch tagging script
  notmuch-hook = pkgs.writers.writeDash "notmuch-tag-mails" ''
    # Index new emails
    ${pkgs.notmuch}/bin/notmuch new

    # Tag new mails according to their folder path
    # > notmuch tab +inria -- -new path:inria/**
    ${lib.concatMapString (name: ''
      ${pkgs.notmuch}/bin/notmuch tag +${name} -- -new path:${name}/**
    '') account-names}

    # GMail specific configurations
  '';
  mbsyncrc = pkgs.writeText "mbsyncrc" ''
    # Keeps the time stamp based message sorting intact
    CopyArrivalDate yes

    # Declare accounts
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      IMAPAccount ${acc.name}
      User        ${acc.mail}
      Host        ${acc.imap.host}
      SSLType     ${acc.imap.ssl}
      PassCmd     "${acc.passcmd}"
    '') accounts}

    # A Store defines a collection of mailboxes; basically a folder,
    # either local or remote. Here we specify the remote and local Store:

    # - The remote store (IMAPStore) is where we get the mail from
    #   (e.g., the specification of an imap account)
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      IMAPStore ${acc.name}-remote
      Account   ${acc.name}
    '') accounts}

    # - The local store (MaildirStore) is where we store the email on
    #   our computer.
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      MaildirStore ${acc.name}-local
      Path         ${mail-dir}/${acc.name}/
      Inbox        ${mail-dir}/${acc.name}/inbox
      SubFolders   Verbatim
    '') accounts}

    # A Channel connects two Stores, describing the way the two are
    # synchronized. They are specified using patterns, which match remote
    # mail folders. Some commonly used patters include:
    # - "*" to match everything
    # - "!dir" to exclude "dir"
    # - "dir" to match dir
    ${lib.concatMapStringsSep "\n\n" (acc: ''
      Channel  ${acc.name}-inbox
      Master   :${acc.name}-remote:
      Slave    :${acc.name}-local:
      Patterns "INBOX" "Sent" "Drafts" "Trash" # Will copy the remote email account as is
      # Create   Both   # Automatically create missing mailboxes on the Master/Slave.
      Create   Slave  # Automatically create missing mailboxes on the Slave.
      Expunge  Slave  # Only delete on Slave
      Sync     Pull
      MaxSize  100m # Don't download any email greater than this
    '') accounts}
  '';
  # notmuch = pkgs.writeText "notmuch-config" ''
  # '';
  # astroid = pkgs.writeText "astroid-config" ''
  # '';
in {
  environment.systemPackages = with pkgs; [
    isync     # to fetch email (mbsync)
    notmuch   # to index and search email
    msmtp     # to send email
    astroid   # GUI
  ];

  # Configure mbsync + notmuch
  # See https://wiki.archlinux.org/index.php?title=Isync&oldid=627584#Automatic_synchronization
  systemd.user.services.mbsync =
    let HOME = config.users.users.rfish.home;
    in {
      description = "Mailbox synchronization service";
      startAt = [ "*:00/5" ];  # Pull every 5 minutes
      environment.NOTMUCH_CONFIG="${HOME}/.notmuch-config";  # notmuch configuration file
      serviceConfig = {
        Type = "oneshot";
        # Synchronize emails
        ExecStart = [''
          ${pkgs.isync}/bin/mbsync --config ${mbsyncrc} -Va
        ''];
        ExecStartPost = [
          # Index new emails
          "${pkgs.notmuch}/bin/notmuch new"
          # Tag new mails according to their folder path
          "${pkgs.notmuch}/bin/notmuch tag +inria -- -new path:inria/**"
        ];
      };
    };
}

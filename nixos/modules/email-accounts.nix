# Email accounts

# Here is a small python snippet to list all mailboxes of an account:
#
# $ nix shell nixpkgs\#python3Full -c python
# >>> import email, imaplib
# >>> mail = imaplib.IMAP4_SSL("imap.gmail.com")
# >>> mail.login("<email>", "<password>")
# >>> mail.list()

let
  Gmail = {
    default = true;
    name = "Gmail";
    sync = true;
    email = "RonanCherrueau@gmail.com";
    imap.host = "imap.gmail.com";
    smtp.host = "smtp.gmail.com";
    pass = "perso/GMail";
    boxes = {
      inbox = "INBOX";
      drafts = "[Gmail]/Drafts";
      sent = "[Gmail]/Sent Mail";
      trash = "[Gmail]/Bin";
    };
  };
  Inria = {
    name = "Inria";
    sync = false;
    email = "Ronan-Alexandre.Cherrueau@inria.fr";
    imap.host = "zimbra.inria.fr";
    smtp.host = "smtp.inria.fr";
    smtp.user = "rcherrue";
    pass = "job/Inria/inria";
    boxes = {
      inbox = "inbox";
      drafts = "Drafts";
      sent = "Sent";
      trash = "Trash";
    };
  };
  IMT = {
    # https://intranet.imt-atlantique.fr/assistance-support/informatique/didacticiels/
    inherit (Inria) boxes;
    name = "IMT";
    sync = false;
    email = "Ronan-Alexandre.Cherrueau@imt-atlantique.fr";
    imap.host = "z.imt.fr";
    smtp.host = "z.imt.fr";
    pass = "job/IMT/rcherr12";
  };
  OLibre = {
    default = true;
    name = "OLibre";
    sync = true;
    email = "Ronan-Alexandre.Cherrueau@objectif-libre.com";
    imap.host = "imap.gmail.com";
    smtp.host = "smtp.gmail.com";
    pass = "job/objectif-libre/notmuch-mua";
    boxes = {
      # Do a `mbsync --list OLibre-inbox` to ensure each box exists
      inbox = "INBOX";
      drafts = "[Gmail]/Drafts";
      sent = "[Gmail]/Sent Mail";
      trash = "[Gmail]/Trash";
    };
  };

in [ Gmail OLibre Inria IMT ]

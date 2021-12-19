# --------------------------------------------------------------------
# Email accounts:

rec {
    Gmail = {
      store = "Gmail";
      sync = true;
      default = true;
      email = "RonanCherrueau@gmail.com";
      imap.host = "imap.gmail.com";
      smtp.host = "smtp.gmail.com";
      pass = "perso/GMail";
      boxes = {
      # Do a `mbsync --list Gmail-inbox` to list boxes
        inbox = ["INBOX" "![Gmail]*"];
        drafts = "[Gmail]/Drafts";
        sent = "[Gmail]/Sent Mail";
        trash = "[Gmail]/Bin";
      };
    };
    Inria = {
      store = "Inria";
      sync = false;
      email = "Ronan-Alexandre.Cherrueau@inria.fr";
      imap.host = "zimbra.inria.fr";
      smtp.host = "smtp.inria.fr";
      smtp.user = "rcherrue";
      pass = "job/Inria/inria";
      # Do a `mbsync --list Inria-inbox` to list boxes
      boxes = {
        inbox = ["*" "!Junk"];
        drafts = "Drafts";
        sent = "Sent";
        trash = "Trash";
      };
    };
    IMT = {
      # https://intranet.imt-atlantique.fr/assistance-support/informatique/didacticiels/
      inherit (Inria) boxes;
      store = "IMT";
      sync = false;
      email = "Ronan-Alexandre.Cherrueau@imt-atlantique.fr";
      imap.host = "z.imt.fr";
      smtp.host = "z.imt.fr";
      pass = "job/IMT/rcherr12";
    };
}

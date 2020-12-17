# --------------------------------------------------------------------
# Email accounts:

rec {
    Gmail = {
      default = true;
      email = "RonanCherrueau@gmail.com";
      imap.host = "imap.gmail.com";
      smtp.host = "smtp.gmail.com";
      keepass = "/root/perso/GMail";
      box = {
        # Copy all remote email box as is except [Gmail]
        # XXX: I currently open 166 boxes.  That takes way too long.
        # Narrow this to speed up downloading
        inbox = ["*" "![Gmail]*"];
        drafts = "[Gmail]/Drafts";
        sent = "[Gmail]/Sent Mail";
        trash = "[Gmail]/Bin";
      };
    };
    Inria = {
      email = "Ronan-Alexandre.Cherrueau@inria.fr";
      imap.host = "zimbra.inria.fr";
      smtp.host = "smtp.inria.fr";
      smtp.user = "rcherrue";
      keepass = "/root/Inria/inria";
      # Do a `mbsync --list Inria-inbox` to list boxes
      box = {
        inbox = ["*" "!Junk"];
        drafts = "Drafts";
        sent = "Sent";
        trash = "Trash";
      };
    };
    IMT = {
      # https://intranet.imt-atlantique.fr/assistance-support/informatique/didacticiels/
      inherit (Inria) box;
      email = "Ronan-Alexandre.Cherrueau@imt-atlantique.fr";
      imap.host = "z.imt.fr";
      smtp.host = "z.imt.fr";
      keepass = "/root/IMT/rcherr12";
    };
}

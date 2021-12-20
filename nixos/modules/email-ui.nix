{ config, pkgs, lib, ... }:

let
  # --------------------------------------------------------------------
  # Email accounts:
  accounts = (import ./email-accounts.nix);

  # --------------------------------------------------------------------
  # Configurations files

  # msmtp configuration file (man msmtp)
  msmtprc = pkgs.writeText "msmtprc" ''
    # Set default values for all following accounts
    defaults
    auth          on
    tls           on
    syslog        on

    # Accounts
    ${lib.concatMapStringsSep "\n" (acc: with acc; ''
    account       ${name}
    host          ${smtp.host}
    port          ${if smtp ? port then builtins.toString(smtp.port) else "587"}
    from          ${email}
    user          ${if smtp ? user then smtp.user else email}
    passwordeval  ${pkgs.pass}/bin/pass ${lib.escapeShellArg pass} | head -n1
    '') accounts}

    # Set DEBUG account, start it with:
    #   msmtpd --port=2500 --command 'tee $(mktemp)'
    # And send mail to:
    #   From: debug@127.0.0.1
    #   to: debug@127.0.0.1
    account       debug
    auth          off
    tls           off
    tls_starttls  off
    host          127.0.0.1
    port          2500
    from          debug@127.0.0.1
    user          debug@127.0.0.1

    # Set a default account
    account default : ${defaultAccount.name}
  '';

  # Emacs configuration
  #
  # See https://notmuchmail.org/notmuch-emacs/
  # and https://notmuchmail.org/emacstips/
  mua-el =
    let file-el = pkgs.writeText "rcherr-mua.el" ''
      ;; Display emails with the newest first
      (setq-default notmuch-search-oldest-first nil)

      ;; Apply the following tags when an email is deleted
      (setq notmuch-message-deleted-tags '("-inbox" "+deleted"))

      ;; Mailing topics to display
      (setq notmuch-saved-searches
            '(${lib.concatMapStrings (acc: with acc; ''
              (:name "${name}" :query "tag:${name} AND tag:inbox" :search-order newest-first)
              '') accounts}
              (:name "unread" :query "tag:unread" :key ,(kbd "u"))
              (:name "drafts" :query "tag:draft" :key ,(kbd "d"))))

      ;; msmtp with multiple accounts
      ;; See https://notmuchmail.org/emacstips/#index11h2
      (setq sendmail-program "${msmtpWp}/bin/msmtp")
      (setq mail-specify-envelope-from t)
      (setq message-sendmail-envelope-from 'header)
      (setq mail-envelope-from 'header)
      (setq message-send-mail-function #'message-send-mail-with-sendmail)

      ;; Check for attachment
      (add-hook 'notmuch-mua-send-hook 'notmuch-mua-attachment-check)
      (setq notmuch-mua-attachment-regexp
            "\\b\\(attache\?ment\\|attached\\|attach\\|pi[èe]ce[-\s]+jointe?\\|p\.-j\.\\|ci-joint\\)\\b")

      (provide 'rcherr-mua)
    '';
    in  pkgs.runCommand "mua-config-dir" {} ''
      # Copy mua.el into emacs dir
      mkdir -p $out/share/emacs/site-lisp/
      ln -s "${file-el}" $out/share/emacs/site-lisp/rcherr-mua.el
    '';

  # --------------------------------------------------------------------
  # Software

  # Wraps msmtp to call the custom config
  msmtpWp = pkgs.symlinkJoin {
    name = "msmtp";
    paths = [ pkgs.msmtp ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/msmtp \
        --add-flags "--file=${msmtprc}"

      wrapProgram $out/bin/msmtpq \
        --add-flags "--file=${msmtprc}"
    '';
  };

  # --------------------------------------------------------------------
  # Utils

  # The default account
  defaultAccount =
    let errorMsg = "cannot find a default email account";
        # Test if an account has the `default` key set to `true`
        isDefault = acc: acc ? default && acc.default;
        # Find the first default account
    in lib.lists.findFirst isDefault (abort errorMsg) accounts;

  # --------------------------------------------------------------------
  # Utils

in {
  environment.systemPackages = with pkgs; [
    # To send email
    (builtins.trace ">> msmtprc: ${msmtprc}" msmtpWp)
    mua-el
  ];
}

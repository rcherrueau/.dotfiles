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
    #   To: 42@42.42
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
    let
      # Emacs file
      rcherr-mua-el = ./rcherr-mua.el;
      # Custom searches (one per `account`)
      account-searches = lib.concatMapStrings (acc: with acc; ''
          (:name "${name}"
           :query "tag:${name} AND tag:inbox"
           :search-order newest-first)
        '') accounts;
      # Save sent emails into `account.boxes.sent`
      account-sentdirs = lib.concatMapStrings (acc: with acc; ''
          ("${lib.strings.toLower email}" . "\"${name}/${boxes.sent}\" +${name} +sent")
        '') accounts;

    # Copy rcherr-mua.el into emacs dir
    # FIXME: msmtp*q* failed even if the output is successful
    in pkgs.runCommand "mua-config-dir" {} ''
      mkdir -p $out/share/emacs/site-lisp/
      substitute "${rcherr-mua-el}" $out/share/emacs/site-lisp/rcherr-mua.el \
        --replace "/usr/bin/msmtp" "${msmtpWp}/bin/msmtp" \
        --replace ";;@account-searches@" ${lib.escapeShellArg account-searches} \
        --replace ";;@account-sentdirs@" ${lib.escapeShellArg account-sentdirs}
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
    msmtpWp  # To send email
    mua-el   # To view them in emacs
  ];
}

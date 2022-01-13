;; ---------------------------------------------------------------------
;; General
(require 'notmuch)

(defun rcherr-mua/poll-and-refresh-this-buffer ()
  (interactive)
  (message "Polling emails...")
  (call-process-shell-command "systemctl --user start polling-email")
  (message "Polling emails...done")
  (notmuch-refresh-this-buffer))

;; ---------------------------------------------------------------------
;; Hello mode

;; Set the cursor position to the first saved search
(defun move-to-first-saved-search ()
  ;; If I am at the beginning of the buffer and can moved to
  ;; "Saved searches", then moved to the next line > next button.
  (when (and (eq (point) (point-min))
             (search-forward "Saved searches:" nil t))
    (forward-line)
    (widget-forward 1)))
(add-hook 'notmuch-hello-refresh-hook 'move-to-first-saved-search)

;; List "Saved searches" to display
(setq notmuch-saved-searches
      '((:name "unread" :query "tag:unread" :key ,(kbd "u"))
        (:name "drafts" :query "tag:draft" :key ,(kbd "d"))
        ;;@account-searches@
        ))

;; Poll new emails and refresh buffer with P
(evilified-state-evilify-map notmuch-hello-mode-map
  :mode notmuch-search-mode
  :bindings
  (kbd "P") 'rcherr-mua/poll-and-refresh-this-buffer)

;; ---------------------------------------------------------------------
;; Search mode

;; Display emails with the newest first
(setq-default notmuch-search-oldest-first nil)

;; Poll new emails and refresh buffer with P
(evilified-state-evilify-map notmuch-search-mode-map
  :mode notmuch-search-mode
  :bindings
  (kbd "P") 'rcherr-mua/poll-and-refresh-this-buffer)

;; ---------------------------------------------------------------------
;; Show mode

(defun notmuch-show-delete-message-then-exit ()
  (interactive)
  (notmuch-show-tag '("-inbox" "+deleted"))
  (notmuch-show-next-thread nil))

(defun notmuch-show-delete-thread-then-exit ()
  (interactive)
  (notmuch-show-tag-all '("-inbox" "+deleted"))
  (notmuch-show-next-thread nil))

;; Keybinds
(evilified-state-evilify-map notmuch-show-mode-map
  :mode notmuch-show-mode
  :bindings
  (kbd "d") 'notmuch-show-delete-message-then-exit
  (kbd "D") 'notmuch-show-delete-thread-then-exit)

;; Press "d" to delete message in notmuch-show mode
;; (define-key notmuch-show-mode-map "d" 'notmuch-show-delete-message-then-exit)
;; (define-key notmuch-show-mode-map "D" 'notmuch-show-delete-thread-then-exit)

;; ---------------------------------------------------------------------
;; Message mode

;; msmtp with multiple accounts
;; See https://notmuchmail.org/emacstips/#index11h2
(setq sendmail-program "/usr/bin/msmtp")
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq message-send-mail-function #'message-send-mail-with-sendmail)
(setq message-kill-buffer-on-exit t)  ;; Close & kill buffer on C-c C-c

;; Check for attachment before sending the email
(setq notmuch-mua-attachment-regexp
      "\\b\\(attache\?ment\\|attached\\|attach\\|pi[èe]ce[-\s]+jointe?\\|p\.-j\.\\|ci-joint\\)\\b")
(add-hook 'notmuch-mua-send-hook 'notmuch-mua-attachment-check)

;; Where to save sent emails
(setq notmuch-fcc-dirs
      '(;;@account-sentdirs@
        (".*" . "sent")))

(provide 'rcherr-mua)

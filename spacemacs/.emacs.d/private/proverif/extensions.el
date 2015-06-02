;;; extensions.el --- NAME Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq proverif-pre-extensions
  '(
    ;; pre extension names go here
    proverif
    ))

(setq proverif-post-extensions
  '(
    ;; post extension names go here
    ))

;; For each extension, define a function NAME/init-<extension-name>
;;
;; (defun NAME/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun proverif/init-proverif ()
  "Initialize my extension"
  (use-package proverif
    :mode
    ("\\.pv$" . proverif-pv-mode)
    ("\\.pi$" . proverif-pi-mode)
    ("\\.horn$" . proverif-horn-mode)
    ("\\.horntype$" . proverif-horntype-mode))
  )


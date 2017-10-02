(defconst tla+-packages
  '(
    (tla-mode
     :location (recipe :fetcher github :repo "ratish-punnoose/tla-mode"))
    flycheck
    (flycheck-tla
     :toogle (configuration-layer/layer-usedp 'syntax-checking)
     :location local
     ;; FIXME: Use this once version number is fixed
     ;; (recipe :fetcher github :repo "iamarcel/flycheck-tla")
     )
    ))


(defun tla+/init-tla-mode ()
  (use-package tla-mode
    :defer t))

(defun tla+/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'tla-mode))

(defun tla+/init-flycheck-tla ()
  (use-package flycheck-tla))

(setq mine-java-packages
      '(cc-mode
        sbt-mode
        ;; (pmd :location local)
        ))


(defun mine-java/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (setq c-basic-offset 2)
    (spacemacs/set-leader-keys-for-major-mode 'java-mode
      "c" 'sbt-command
      "r" 'sbt-run-previous-command
      "h" 'sbt-find-definitions)))

(defun mine-java/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    ;; compilation-skip-threshold tells the compilation minor-mode
    ;; which type of compiler output can be skipped. 1 = skip info
    ;; 2 = skip info and warnings.
    (setq compilation-skip-threshold 1)

    ;; On 'sbt-run-previous-command skip the question to save buffers and
    ;; have buffers saved automatically instead.
    (setq compilation-ask-about-save nil)))

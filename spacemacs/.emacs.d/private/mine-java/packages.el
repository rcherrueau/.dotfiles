(setq mine-java-packages
      '(cc-mode
        sbt-mode
        (pmd :location local)))

(defun pmd-current-sbt-project ()
  (pmd-file-or-dir (sbt:find-root)))

(defun mine-java/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (setq c-basic-offset 2)

    (spacemacs/set-leader-keys-for-major-mode 'java-mode
      ;; SBT
      ;; I have to put this here since sbt is lazy load
      "c" 'sbt-command
      "e" 'next-error
      "r" 'sbt-run-previous-command
      "h" 'sbt-find-definitions

      ;; PMD
      ;; I have to put this here since pmd is lazy load
      "pb" 'pmd-current-buffer
      "pp" 'pmd-current-sbt-project


      )))

(defun mine-java/post-init-mine-java-mode ()
  (spacemacs|add-company-hook java-mode))

(defun mine-java/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init
    (push 'sbt-mode company-backends-java-mode)
    :config
    ;; compilation-skip-threshold tells the compilation minor-mode
    ;; which type of compiler output can be skipped. 1 = skip info
    ;; 2 = skip info and warnings.
    (setq compilation-skip-threshold 1)

    ;; On 'sbt-run-previous-command skip the question to save buffers and
    ;; have buffers saved automatically instead.
    (setq compilation-ask-about-save nil)

    ;; (spacemacs/declare-prefix-for-mode 'java-mode "ms" "sbt")
    ))

(defun mine-java/init-pmd ()
  (use-package pmd
    ;; FIXME
    ;; :defer t
    :init
    (push 'pmd company-backends-java-mode)))

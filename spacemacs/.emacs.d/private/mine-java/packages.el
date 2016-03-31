(setq mine-java-packages
      '(cc-mode
        sbt-mode
        (pmd :location local)))

(defun mine-java/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    ;; Indentation level to two
    (setq c-basic-offset 2)

    ;; C-mode by default align parameter with the end of the method,
    ;; e.g:
    ;; > public void foo(
    ;; >                 String a,
    ;; >                 String b) { }
    ;;
    ;; Aligning the second parameter on the first one is really good.
    ;; However, I'm not a big fan of the alignment of the first
    ;; parameter with the method. I rather prefer something like that:
    ;; > public void foo(
    ;; >   String a,
    ;; >   String b) { }
    ;;
    ;; The following fixes this issue.
    ;; See http://stackoverflow.com/a/1365821
    (setq c-offsets-alist '((arglist-intro +)))

    ;; List of specific rules for pmd
    (setq pmd-specific-rules '(("prU" . "unusedcode")
                               ("prE" . "empty")
                               ("prN" . "unnecessary")
                               ("prB" . "basic")
                               ("prI" . "imports")))

    ;; Generates spacemacs binding to call each specific rule
    ;; - first, generates interactive function
    (mapc (lambda (r) (eval `(mk-pmd-rule-function ,(cdr r))))
          pmd-specific-rules)
    ;; - then, creates spacemacs shortcut to this call.
    (mapc (lambda (r)
            (let ((plabel (car r))
                  (pfun (intern (concat "pmd-project-" (cdr r))))
                  (blabel (downcase (car r)))
                  (bfun (intern (concat "pmd-buffer-" (cdr r)))))
              (spacemacs/set-leader-keys-for-major-mode
                'java-mode plabel pfun blabel bfun)))
          pmd-specific-rules)

    (spacemacs/set-leader-keys-for-major-mode 'java-mode
      ;; SBT
      ;; I have to put this here since sbt is lazy load
      "c" 'sbt-command
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
    :commands (pmd-current-buffer pmd-file-or-dir)
    ;; :init
    ;; (push 'pmd company-backends-java-mode)
    :config
    ;; Point to the correct directories for java and pmd-home
    (setq pmd-java-home "/usr/bin/env java")
    (setq pmd-home (if (spacemacs/system-is-linux)
                       "~/.nix-profile/"
                     "/usr/local/pmd/"))))

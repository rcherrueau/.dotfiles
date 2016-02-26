(defun pmd-current-project ()
  "Run pmd with this sbt project as a root"
  (interactive)
  (pmd-file-or-dir (sbt:find-root)))

;; run pmd with one specific rule
(defun pmd-specific-rule (rule target)
  "Run pmd with one specific java `rule'. The value of `target'
can be either 'buffer that runs pmd on the current buffer, or
'project that runs pmd on the current project."
  ;; rebind the dufcustom `pmd-ruleset-list' lexically
  (let ((pmd-ruleset-list (list (concat "java-"rule))))
    (cond
     ((eq 'buffer target) (pmd-current-buffer))
     ((eq 'project target) (pmd-current-project)))))

;; macro that generate the call function for a specific pmd java rule
(defmacro mk-pmd-rule-function (rule)
  "Macro that generates the interactive function for a specific
pmd java rule. The `rule' argument should avoid the 'java-'
prefix. This generates one function for current buffer and
another one for the current project. Names are
`pmd-buffer-<rule>' and `pmd-project-<rule>'."
  (let* ((bfun-symbol (intern (format "pmd-buffer-%s" rule)))
         (pfun-symbol (intern (format "pmd-project-%s" rule)))
         (bdoc (concat "Run pmd on the current buffer with the "
                       rule " rule."))
         (pdoc (concat "Run pmd on the current project with the "
                       rule " rule.")))
    `(progn
       (defun ,bfun-symbol ()
         ,bdoc
         (interactive)
         (pmd-specific-rule ,rule 'buffer))
       (defun ,pfun-symbol ()
         ,pdoc
         (interactive)
         (pmd-specific-rule ,rule 'project)))))

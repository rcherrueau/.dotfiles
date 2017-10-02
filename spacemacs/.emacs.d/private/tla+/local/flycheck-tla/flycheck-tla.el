;;; flycheck-tla.el --- Flycheck integration for TLA+ files

;; Copyright (C) 2016 Marcel Samyn

;; Author: Marcel Samyn <dev@mrcl.io>
;; Version: 0.1
;; Keywords: convenience languages tools
;; Package-Required: ((flycheck "0.25"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This flychecker uses the `SANY Syntactic Analyzer' (included in the TLA+
;; Tools) to find errors in your TLA files.

;;; Setup:

;; (eval-after-load 'flycheck '(require 'flycheck-tla))

;;; Code:

(require 'flycheck)

;;;###autoload
(flycheck-define-checker tla
                         "A syntax checker for the TLA+ language."
                         :command ("java" "tla2sany.SANY" source-inplace)
                         :error-patterns
                         ((info line-start (message) "starting at line " line ", column " column)
                          (error line-start (message) " at line " line ", column " column))
                         :modes tla-mode)

;;;###autoload
(add-to-list 'flycheck-checkers 'tla)

(provide 'flycheck-tla)

;;; flycheck-tla.el ends here

;;; flycheck-hdl-questasim.el --- Support questasim's compile tools in flycheck

;; Copyright (C) 2018 Manuel Eggimann <meggimann@iis.ee.ethz.ch>
;;
;; Author: Manuel Eggimann <meggimann@iis.ee.ethz.ch>
;; Created: 21.10.2018
;; Version: 0.1
;; Package-Requires: ((flycheck "0.25") projectile verilog-mode vhdl-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides additional flycheck checkers for HDL using the Mentor
;; Graphics Qustasim comiple tools.

;;; Code:

(flycheck-define-checker hdl-questasim-vlog
  "A Verilog/SystemVerilog syntax checker using the vlog compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vlog" "-sv" "-work" (eval (concat (projectile-project-root) ".flycheck-work/"))  source)
  :error-patterns
  ((error line-start "** Error"  (opt " (suppressible)") ": " (file-name) "(" line "): (" (id (and "vlog-" (one-or-more digit))) ") " (message) line-end)
   (error line-start "** Error"  (opt " (suppressible)") ": " "(" (id (and "vlog-" (one-or-more digit))) ") " (file-name) "(" line "): " (message)))
  :enabled (lambda() (projectile-project-root))
  :modes verilog-mode
  )

(flycheck-define-checker hdl-questasim-vcom
  "A VHDL syntax checker using the vcom compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vcom" "-work" (eval (concat (projectile-project-root) ".flycheck-work/"))  source)
  :error-patterns
  ((error line-start "** Error: (" (id (and "vcom-" (one-or-more digit) ") " (message))))
   (error line-start "** Error" (opt " (suppressible)") ": " (file-name) "(" line "): " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (message) line-end))
  :enabled (lambda() (projectile-project-root))
  :modes vhdl-mode
  )


(defun flycheck-hdl-questasim-clear-workdir ()
  "Clears the questasim working directory used for flycheck if not nill."
  (interactive)
  (if (file-directory-p (concat (projectile-project-root) ".flycheck-work/"))
      (progn
        (delete-directory (concat (projectile-project-root) ".flycheck-work/") t)
        (message "Successfully deleted working directory"))
    (message "No working directory found to delete. Is the current file within a projectile project?")))

(add-to-list 'flycheck-checkers 'hdl-questasim-vlog)
(add-to-list 'flycheck-checkers 'hdl-questasim-vcom)

(provide 'flycheck-hdl-questasim)

;;; flycheck-hdl-questasim.el ends here

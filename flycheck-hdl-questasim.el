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

(defvar flycheck-hdl-questasim-use-global-workdir nil "It non-nil a work library in the projectile-root will be used. This makes it possible resolve symbols defined in multiple files in different directories since the workdir will accumulate the compiled files with each parsed buffer.")

(flycheck-define-checker hdl-questasim-vlog
  "A Verilog/SystemVerilog syntax checker using the vlog compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vlog" "-sv" "-work" (eval (flycheck-hdl-questasim-workdir))  source)
  :error-patterns
  (;(error line-start "** Error"  (opt " (suppressible)") ": " (file-name) "(" line "): (" (id (and "vlog-" (one-or-more digit))) ") " (message) line-end)
   (error line-start "** Error"  (opt " (suppressible)") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (file-name) "(" line "): " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (message) line-end))
  :modes verilog-mode
  )

(flycheck-define-checker hdl-questasim-vcom
  "A VHDL syntax checker using the vcom compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vcom" "-work" (eval (flycheck-hdl-questasim-workdir))  source)
  :error-patterns
  ((error line-start "** Error: (" (id (and "vcom-" (one-or-more digit) ") " (message))))
   (error line-start "** Error" (opt " (suppressible)") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (file-name) "(" line "): " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (message) line-end))
  :modes vhdl-mode
  )


(defun flycheck-hdl-questasim-clear-workdir ()
  "Clears the questasim working directory used for flycheck if not nill."
  (interactive)
  (if (file-directory-p (flycheck-hdl-questasim-workdir))
      (progn
        (delete-directory (flycheck-hdl-questasim-workdir) t)
        (message "Successfully deleted working directory"))
    (message "No working directory found to delete. Is the current file within a projectile project?")))

(defun flycheck-hdl-questasim-workdir ()
  (if (and (projectile-project-p) flycheck-hdl-questasim-use-global-workdir)
      (concat (projectile-project-root) ".flycheck-work/")
    ".flycheck-work"))

(defun flycheck-hdl-questasim-toggle-workdir ()
  "Toggles between the usage of a global and a local working directory for compilation. A hidden directory in the projectile project root is used for the global working directory."
  (interactive)
  (if flycheck-hdl-questasim-use-global-workdir
      (progn
        (message "Switching to local workdir")
        (setq flycheck-hdl-questasim-use-global-workdir nil))
    (if (not flycheck-hdl-questasim-use-global-workdir)
        (if (projectile-project-p)
            (progn
              (message "Switching to global workdir at %s" (projectile-project-root))
              (setq flycheck-hdl-questasim-use-global-workdir t))
          (message "No projectile project was found. Cannot switch to global workdir")))))

(add-to-list 'flycheck-checkers 'hdl-questasim-vlog)
(add-to-list 'flycheck-checkers 'hdl-questasim-vcom)

(provide 'flycheck-hdl-questasim)

;;; flycheck-hdl-questasim.el ends here

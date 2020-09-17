;;; flycheck-hdl-questasim.el --- Support questasim's compile tools in flycheck

;; Copyright (C) 2018 Manuel Eggimann <meggimann@iis.ee.ethz.ch>
;;
;; Author: Manuel Eggimann <meggimann@iis.ee.ethz.ch>
;; Created: 21.10.2018
;; Version: 0.2
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

(require 'flycheck)

(defgroup flycheck-hdl-questasim nil "Customization for the questasim flycheck syntax checker")

(defcustom flycheck-hdl-questasim-use-global-workdir t "If non-nil a work library in the projectile-root will be used. This makes it possible resolve symbols defined in multiple files in different directories since the workdir will accumulate the compiled files with each parsed buffer. By default (nil) a working directory in the buffers current directory will be used."
  :type '(boolean)
  :group 'flycheck-hdl-questasim)

(defun flycheck-hdl-questasim-get-toplevels (source-original)
  "Reads and returns the toplevel modules from a previously generated toplevels.txt"
  (split-string (string-trim (shell-command-to-string (concat "cat " (flycheck-hdl-questasim-workdir) "/" source-original ".toplevels.txt"))))
  )

(defun flycheck-hdl-questasim-run-vopt-p ()
  "Only run vopt checker if the buffer is not modified and toplevel modules where detected."
  (let ((path (concat (flycheck-hdl-questasim-workdir) "/" (file-name-nondirectory (buffer-file-name)) ".toplevels.txt")))
    (and (not (buffer-modified-p))  (file-exists-p path) (not (string= "" (shell-command-to-string (concat "cat " path))))))
  )

(defun flycheck-hdl-questasim-get-vlog-args ()
  "If a .flycheck-work/vlog_args.txt exists its content will be returned. This function is used to pass additional arguments to vlog (e.g. include directives etc.)"
  (when (file-exists-p (concat (flycheck-hdl-questasim-workdir) "/vlog_args.txt")) (split-string (shell-command-to-string (concat "cat " (flycheck-hdl-questasim-workdir) "/vlog_args.txt"))))
  )

(defun flycheck-hdl-questasim-get-vcom-args ()
  "If a .flycheck-work/vcom_args.txt exists its content will be returned. This function is used to pass additional arguments to vcom (e.g. include directives etc.)"
  (when (file-exists-p (concat (flycheck-hdl-questasim-workdir) "/vcom_args.txt")) (split-string (shell-command-to-string (concat "cat " (flycheck-hdl-questasim-workdir) "/vcom_args.txt"))))
  )

(defun flycheck-hdl-questasim-get-vopt-args ()
  "If a .flycheck-work/vopt_args.txt exists its content will be returned. This function is used to pass additional arguments to vopt (e.g. include directives etc.)"
  (when (file-exists-p (concat (flycheck-hdl-questasim-workdir) "/vopt_args.txt")) (split-string (shell-command-to-string (concat "cat " (flycheck-hdl-questasim-workdir) "/vopt_args.txt"))))
  )

(defun flycheck-hdl-questasim-clear-workdir ()
  "Clears the questasim working directory used for flycheck if not nill."
  (interactive)
  (if (file-directory-p (flycheck-hdl-questasim-workdir))
      (progn
        (delete-directory (flycheck-hdl-questasim-workdir) t)
        (message "Successfully deleted working directory"))
    (message "No working directory found to delete. Is the current file within a projectile project?")))

(defun flycheck-hdl-questasim-analyze-workdir (dir)
  "Compiles every *.sv, *.v and *.vhd file found in the directory
specified into your current working directory (see
`flycheck-hdl-questasim-toggle-workdir' for more info about the
working dir)."
  (interactive "DSelect the directory to scan for files: ")
  (call-process-shell-command (concat (string-join (apply 'list (if (not flycheck-hdl-questasim-vlog-executable) "vlog" flycheck-hdl-questasim-vlog-executable)
                                                          "-sv"
                                                          "-noincr"
                                                          ;; (flycheck-hdl-questasim-get-vlog-args)
                                                          "-work " (flycheck-hdl-questasim-workdir)
                                                          "-modelsimini" (flycheck-hdl-questasim-modelsimini)
                                                          (directory-files-recursively dir "[[:alnum:]]*\\.sv")) " ")
                                      ) nil "*Flycheck Questasim Analyze Workdir Output*" t)
  (flycheck-buffer))

(defun flycheck-hdl-questasim-workdir ()
  (if (and (projectile-project-p) flycheck-hdl-questasim-use-global-workdir)
      (concat (projectile-project-root) ".flycheck-work")
    ".flycheck-work"))

(defun flycheck-hdl-questasim-modelsimini ()
  "Searches for a global modelsimini file and returns its path if found."
  (when (and flycheck-hdl-questasim-use-global-workdir (file-exists-p (concat (flycheck-hdl-questasim-workdir) "/modelsim.ini")))
    (concat (flycheck-hdl-questasim-workdir) "/modelsim.ini")))

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

(defun flycheck-hdl-remove-modelsimini ()
  "Deletes the modelsimini in the current directory if one of the questasim checkers is enabled."
  (when (or (flycheck-may-use-checker 'hdl-questasim-vcom-live) (flycheck-may-use-checker 'hdl-questasim-vlog-live))
    (delete-file "modelsim.ini")))

(flycheck-define-checker hdl-questasim-vlog
  "A Verilog/SystemVerilog syntax checker using the vlog compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vlog" "-sv" "-noincr" "-lint" (eval (flycheck-hdl-questasim-get-vlog-args)) "-work" (eval (flycheck-hdl-questasim-workdir)) "-writetoplevels" (eval (concat (flycheck-hdl-questasim-workdir) "/" (file-name-nondirectory (buffer-file-name)) ".toplevels.txt")) (eval (when (flycheck-hdl-questasim-modelsimini) (list "-modelsimini" (flycheck-hdl-questasim-modelsimini))))  source-original)
  :predicate (lambda () (not (buffer-modified-p)))
  :error-patterns
  ((warning line-start "** Warning"  (opt " (suppressible)") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (message) line-end)
   (error line-start "** Error"  (opt " (suppressible)") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (message) line-end))
  :modes verilog-mode
  :working-directory (lambda (checker) (when flycheck-hdl-questasim-use-global-workdir (projectile-project-root)))
  :error-filter (lambda (list) nil) ;; We do not care about the output of this
  ;; checker. We are only interested in the sid-effects
  :next-checkers ((warning . hdl-questasim-vopt))
  )

(flycheck-define-checker hdl-questasim-vlog-live
  "A Verilog/SystemVerilog syntax checker using the vlog compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vlog" "-sv" "-noincr" "-lint" (eval (flycheck-hdl-questasim-get-vlog-args)) "-work" (eval (flycheck-hdl-questasim-workdir)) "-writetoplevels" (eval (concat (flycheck-hdl-questasim-workdir) "/" (file-name-nondirectory (buffer-file-name)) ".toplevels.txt")) (eval (when (flycheck-hdl-questasim-modelsimini) (list "-modelsimini" (flycheck-hdl-questasim-modelsimini))))  source)
  :error-patterns
  ((warning line-start "** Warning"  (opt " (suppressible)") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (message) line-end)
   (error line-start "** Error"  (opt " (suppressible)") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vlog-" (one-or-more digit)) ") ")) (message) line-end))
  :modes verilog-mode
  :working-directory (lambda (checker) (when flycheck-hdl-questasim-use-global-workdir (projectile-project-root)))
  :next-checkers ((warning . hdl-questasim-vlog))
  )

(flycheck-define-checker hdl-questasim-vopt
  "Shows elaboration time errors and warnings for desing preveously checked with hdl-questasim-vlog or hdl-questasim-vcom using vopt."

  :command ("vopt" "-lint" "-pedanticerrors" "-check_synthesis" "-noincr" (eval (flycheck-hdl-questasim-get-vopt-args)) "-work" (eval (flycheck-hdl-questasim-workdir)) (eval (flycheck-hdl-questasim-get-toplevels (file-name-nondirectory (buffer-file-name)))) "-o" (eval (s-replace "." "_" (concat (file-name-nondirectory (buffer-file-name)) "_opt"))) (eval (when (flycheck-hdl-questasim-modelsimini) (list "-modelsimini" (flycheck-hdl-questasim-modelsimini)))))
  :predicate flycheck-hdl-questasim-run-vopt-p
  :error-patterns
  ((warning line-start "** Warning"  (opt " (suppressible)") ": " (opt "(" (id (and "vopt-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vopt-" (one-or-more digit)) ") ")) (message) line-end)
   (error line-start "** Error"  (opt " (suppressible)") ": " (opt "(" (id (and "vopt-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vopt-" (one-or-more digit)) ") ")) (message) line-end)
   (error line-start "** Error" (message) (* space) " Scope:" (* anything) "File: " (file-name) " Line: " line line-end)) ; word " File: " (file-name) " Line: " line))
  :modes verilog-mode
  :working-directory (lambda (checker) (when flycheck-hdl-questasim-use-global-workdir (projectile-project-root)))
  )


(flycheck-define-checker hdl-questasim-vcom-live
  "A VHDL syntax checker using the vcom compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vcom" "-noincr" (eval (flycheck-hdl-questasim-get-vcom-args)) "-work" (eval (flycheck-hdl-questasim-workdir)) "-writetoplevels" (eval (concat (flycheck-hdl-questasim-workdir) "/" (file-name-nondirectory (buffer-file-name)) ".toplevels.txt")) (eval (when (flycheck-hdl-questasim-modelsimini) (list "-modelsimini" (flycheck-hdl-questasim-modelsimini)))) source)
  :error-patterns
  ((warning line-start "** Warning" (opt " (suppressible)") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (message) line-end)
   (error line-start "** Error" (opt " (suppressible)") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (message) line-end))
  :modes vhdl-mode
  :working-directory (lambda (checker) (when flycheck-hdl-questasim-use-global-workdir (projectile-project-root)))
  :next-checkers ((warning . hdl-questasim-vcom))
  )


(flycheck-define-checker hdl-questasim-vcom
  "A VHDL syntax checker using the vcom compiler of Mentor Graphics Questasim.

See URL `https://www.mentor.com/products/fv/questa/'."

  :command ("vcom" "-noincr" (eval (flycheck-hdl-questasim-get-vcom-args)) "-work" (eval (flycheck-hdl-questasim-workdir)) "-writetoplevels" (eval (concat (flycheck-hdl-questasim-workdir) "/" (file-name-nondirectory (buffer-file-name)) ".toplevels.txt")) (eval (when (flycheck-hdl-questasim-modelsimini) (list "-modelsimini" (flycheck-hdl-questasim-modelsimini)))) source-original)
  :predicate (lambda () (not (buffer-modified-p)))
  :error-patterns
  ((warning line-start "** Warning" (opt " (suppressible)") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (message) line-end)
   (error line-start "** Error" (opt " (suppressible)") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (file-name) (opt "(" line (opt "." column) ")") ": " (opt "(" (id (and "vcom-" (one-or-more digit)) ") ")) (message) line-end))
  :error-filter (lambda (list) nil) ;; We do not care about the output of this
  ;; checker. We are only interested in the sid-effects
  :modes vhdl-mode
  :working-directory (lambda (checker) (when flycheck-hdl-questasim-use-global-workdir (projectile-project-root)))
  :next-checkers ((warning . hdl-questasim-vopt))
  )

;;Cleanup modelsim.ini files after each syntax check
(add-hook 'flycheck-after-syntax-check-hook 'flycheck-hdl-remove-modelsimini)

(add-to-list 'flycheck-checkers 'hdl-questasim-vlog-live)
(add-to-list 'flycheck-checkers 'hdl-questasim-vcom-live)
(add-to-list 'flycheck-checkers 'hdl-questasim-vcom 'append)
(add-to-list 'flycheck-checkers 'hdl-questasim-vlog 'append)
(add-to-list 'flycheck-checkers 'hdl-questasim-vopt 'append)


(provide 'flycheck-hdl-questasim)

;;; flycheck-hdl-questasim.el ends here

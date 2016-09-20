;;; vagrant-macros --- Macros to create interactive vagrant commands. -*- no-byte-compile: t -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; URL: https://github.com/nverno/vagrant
;; Created: 18 September 2016

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Macros to create vagrant commands.

;;; Code:
(require 'cl-lib)

(defmacro vagrant-cmds (cmds &rest args)
  (macroexp-progn
   (cl-loop for c in (eval cmds)
      collect `(vagrant-cmd ,c ,@args))))

;; Create interactive vagrant commands
(defmacro vagrant-cmd (cmd &optional vagrant-default-args nosearch)
  "Create interactive function `vagrant-'CMD, with optional default
arguments VAGRANT-DEFAULT-ARGS. If NOSEARCH is non-nil, a vagrantfile
is not searched for."
  (let* ((cmd (eval cmd))
         (fn (intern (concat "vagrant-" cmd)))
         (vargs (if vagrant-default-args
                    (or (and (consp vagrant-default-args)
                             (mapconcat 'identity
                                        vagrant-default-args " "))
                        vagrant-default-args)
                  ""))
         (fn-doc (concat "Call 'vagrant " cmd "'"
                         (when (not (string= vargs ""))
                           (format " (with default arguments: %s)" vargs))
                         ".")))
    `(defun ,fn (&optional dir box args)
       ,fn-doc
       (interactive)
       (,(if nosearch 'let 'let*)
        (cons
         ,@(when (not nosearch)
             '((default-directory
                 (or dir 
                     (file-name-directory (vagrant-locate-vagrantfile))))
               (name (or box
                         (and (equal current-prefix-arg '(4))
                              (vagrant-completing-read
                               "Vagrant box: " (vagrant-list-boxes)))
                         (car (vagrant-list-boxes))))))
         (args (or args
                   (if (equal current-prefix-arg '(16))
                       (read-string "Args: ")
                     ,vargs)))
         (buff (prog1 (get-buffer-create "*Vagrant*")
                 (with-current-buffer "*Vagrant*"
                   (let ((inhibit-read-only t))
                     (erase-buffer))))))
        ,@(when (not nosearch)
            '((unless name
                (user-error "Unable to find vagrant boxes"))))
        (let ((command (concat "vagrant "
                               ,cmd " "
                               ,@(when (not nosearch) '((when name (concat name " "))))
                               args)))
          (message "Running %s" command)
          (async-shell-command command buff)
          (switch-to-buffer buff)
          (vagrant-mode))))))

;; ------------------------------------------------------------
;;* vagrant-machine commands, for tabulated list interface

(defmacro vagrant-machine-cmd (cmd)
  "Create commands to call on boxes in tabulated list."
  (let* ((cmd (eval cmd))
         (fn (intern (concat "vagrant-machine-" cmd)))
         (fn1 (intern (concat "vagrant-" cmd)))
         (fn-doc (concat "Call 'vagrant " cmd "' on box in tabulated list.")))
    `(defun ,fn ()
       ,fn-doc
       (interactive)
       (,fn1 (vagrant--machine-read 4)
             (vagrant--machine-read 1)))))

(defmacro vagrant-machine-cmds (cmds &rest args)
  (macroexp-progn
   (cl-loop for c in (eval cmds)
      collect `(vagrant-machine-cmd ,c ,@args))))

(defmacro vagrant-machine-make-menu (cmds &rest other)
  "Expand interactive commands into menu"
  `(quote
    ,(nconc
      (cons "Vagrant Machines" nil)
      other
      (cl-loop for c in (eval cmds)
         collect (vector c (intern (concat "vagrant-machine-" c)) t)))))

(provide 'vagrant-macros)

;;; vagrant-macros.el ends here

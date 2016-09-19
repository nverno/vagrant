;;; vagrant --- Utilities to manage vagrant from emacs.

;;; Author: Noah Peart <noah.v.peart@gmail.com>
;;; Copyright (C) 2016, Noah Peart, all rights reserved.
;;; URL: https://github.com/nverno/vagrant
;;; Keywords: vagrant docker
;;; Created: 18 September 2016

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

;; Manage vagrant boxes / Vagrantfiles from emacs

;;; Code:

(eval-when-compile
  (require 'vagrant-macros)
  (require 'subr-x))

(defgroup vagrant nil
  "Vagrant box management tools for emacs."
  :group 'external)

(eval-and-compile
  (defcustom vagrant-commands
    '("box" "connect" "destroy" "docker-logs" "docker-run"
      "halt" "init" "list-commands" "login" "package" "plugin"
      "provision" "rdp" "reload" "resume" "rsync" "rsync-auto" "snapshot"
      "share" "ssh" "ssh-config" "status" "suspend" "up")
    "List of vagrant commands to call interactively."
    :type 'sexp
    :group 'vagrant)

  (defcustom vagrant-global-commands
    '("global-status" "version" "help")
    "List of vagrant commands to call interactively that don't require 
locating vagrant root directory for project."
    :type 'sexp
    :group 'vagrant))

(defcustom vagrant-completing-read 'ido-completing-read
  "Completion engine to use."
  :type 'function
  :group 'vagrant)

;; ------------------------------------------------------------
;;* Internal

(defalias 'vagrant-completing-read vagrant-completing-read)

(defvar-local vagrant-root nil
  "Default path to vagrantfile.")

(defun vagrant-locate-vagrantfile (&optional dir)
  "Find Vagrantfile for in parent directories, or prompt."
  (or vagrant-root
      (setq vagrant-root (locate-dominating-file
                          (or dir default-directory) "Vagrantfile"))
      (let ((msg (if dir
                     (format "Vagrantfile not found in %s, new directory: " dir)
                   "Directory to search for boxes: ")))
        (vagrant-locate-vagrantfile (read-directory-name msg)))))

;; boxes

(defun vagrant-list-boxes ()
  "List of vagrant boxes in `vagrant-root'."
  (when-let ((root (vagrant-locate-vagrantfile)))
   (let ((dir (expand-file-name ".vagrant/machines/" root)))
     (delq nil (directory-files dir nil "^[^.]")))))

;; ------------------------------------------------------------
;;* Build interactive commmands

(vagrant-cmds vagrant-global-commands nil t)
(vagrant-cmds vagrant-commands)

;; ------------------------------------------------------------
;;* shell

(declare-function comint-send-input "shell")

(defun vagrant-comint-sender (proc string)
  (comint-send-string
   proc
   (concat (replace-regexp-in-string "[\n\r]+$" "\n" string) "\n")))

(defun vagrant-comint-filter (string)
  (replace-regexp-in-string "[\n\r]+$" "\n" string))

(defun vagrant-send-input ()
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
         (str (comint-get-old-input-default)))
    (unless (string= str "")
      (process-send-string proc (concat str "\n")))))

(defvar vagrant-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "RET") 'vagrant-send-input)
    km))

(define-minor-mode vagrant-mode
  "Vagrant minor mode."
  nil
  :keymap vagrant-mode-map
  :lighter "Vagrant"
  (setq-local comint-input-sender-no-newline t)
  (setq-local comint-input-sender (function vagrant-comint-sender))
  (add-hook 'comint-input-filter-functions 'vagrant-comint-filter nil t))

(provide 'vagrant)

;;; vagrant.el ends here

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
      "halt" "login" "package" "plugin"
      "provision" "rdp" "reload" "resume" "rsync" "rsync-auto" "snapshot"
      "share" "ssh" "ssh-config" "status" "suspend" "up")
    "List of vagrant commands to call interactively."
    :type 'sexp
    :group 'vagrant)

  (defcustom vagrant-global-commands
    '("global-status" "version" "help" "list-commands" "init")
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
;;* Note: windows needs process-coding utf-8-unix otherwise there is a
;;  trailing '\r' in ssh

(defvar vagrant-menu
  '("Vagrant"
    ["Global status" vagrant-global-status t]
    ["SSH" vagrant-ssh t]
    ["Help" vagrant-help t]
    ["Up" vagrant-up t]
    ["Halt" vagrant-halt t]
    ["Reload" vagrant-reload t]
    ["Provision" vagrant-provision t]
    ["Box" vagrant-box t]
    ["List command" vagrant-list-commands t]))

(defvar vagrant-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil vagrant-menu)
    (define-key km (kbd "<f2> m g") 'vagrant-global-status)
    (define-key km (kbd "<f2> m s") 'vagrant-ssh)
    (define-key km (kbd "<f2> m ?") 'vagrant-help)
    (define-key km (kbd "<f2> m u") 'vagrant-up)
    (define-key km (kbd "<f2> m h") 'vagrant-halt)
    (define-key km (kbd "<f2> m r") 'vagrant-reload)
    (define-key km (kbd "<f2> m b") 'vagrant-box)
    (define-key km (kbd "<f2> m p") 'vagrant-provision)
    (define-key km (kbd "<f2> m l") 'vagrant-list-commands)
    km))

(defun vagrant-output-filter (string)
  "Add a prompt indicator"
  (concat string "> "))

(define-minor-mode vagrant-mode
  "Vagrant minor mode.
Commands:\n
\\{vagrant-mode-map}"
  nil
  :keymap vagrant-mode-map
  :lighter "Vagrant"
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
  (remove-hook 'comint-input-filter-functions 'shell-directory-tracker t)
  (add-hook 'comint-preoutput-filter-functions 'vagrant-output-filter nil t)
  (setq-local comint-process-echoes nil)
  (shell-dirtrack-mode -1))

;; not using
(defun vagrant-proc-sentinel (p s)
  (message "%s finished with status: '%s'" p s)
  (pop-to-buffer "*Vagrant*")
  (when (get-buffer-process (current-buffer))
    (shell-mode)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (vagrant-mode)
  (goto-char (point-max)))

(provide 'vagrant)

;;; vagrant.el ends here

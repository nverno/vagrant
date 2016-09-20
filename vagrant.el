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
  (or (and dir (setq vagrant-root (locate-dominating-file dir "Vagrantfile")))
      vagrant-root
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

(defun vagrant-open-vagrantfile (&optional dir)
  (interactive)
  (let ((dir (or (and dir (vagrant-locate-vagrantfile dir))
                 (and current-prefix-arg
                      (vagrant-locate-vagrantfile
                       (read-directory-name "Directory to search for Vagrantfile: ")))
                 (vagrant-locate-vagrantfile))))
    (when dir
      (find-file (expand-file-name "Vagrantfile" dir)))))

;;;###autoload
(defun vagrant-vagrant ()
  "Launch vagrant mode"
  (interactive)
  (vagrant-global-status))

;; ------------------------------------------------------------
;;* Note: windows needs process-coding utf-8-unix otherwise there is a
;;  trailing '\r' in ssh

(defvar vagrant-menu
  '("Vagrant"
    ["Global status" vagrant-global-status t]
    ["SSH" vagrant-ssh t]
    ["Vagrantfile" vagrant-open-vagrantfile t]
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
    (define-key km (kbd "<f2> m v") 'vagrant-open-vagrantfile)
    km))

(defun vagrant-output-filter (string)
  "Add a prompt indicator"
  (concat string "> "))

(declare-function shell-dirtrack-mode "shell")
(declare-function shell-mode "shell")
(defvar comint-process-echoes)

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

;; ------------------------------------------------------------
;;* List machines
;;  adapted from https://github.com/emacs-pe/vagrant.el

(cl-defstruct (vagrant-machine (:constructor vagrant-machine--create))
  id name provider state directory)

(defconst vagrant-global-re
  "^\\([[:xdigit:]]+\\)\\s-*\\([[:alnum:]_]+\\)\\s-*\\(\\w+\\)\\s-*\\(\\w+\\)\\s-*\\(.*\\)"
  "Regexp to match `vagrant global-status' output.")

(defvar vagrant-machines nil)
(defun vagrant--machines (&optional refetch)
  "Fetch the vagrant machines."
  (unless (or refetch vagrant-machines)
    (let* ((str (shell-command-to-string "vagrant global-status"))
           (start (+ 2 (string-match "-\n" str)))
           (lines (split-string (substring str start) "\n" nil "\\s-*")))
      (setq vagrant-machines
            (cl-loop for line in lines
               while (not (string= line ""))
               for value = (split-string line)
               collect (cl-multiple-value-bind (id name provider state dir) value
                         (cons id
                               (vagrant-machine--create
                                :id id
                                :name name
                                :provider provider
                                :state state
                                :directory dir)))))))
  vagrant-machines)

(defun vagrant--generate-table-entry (item)
  "Generate a tabulate mode entry from an ITEM."
  (cl-destructuring-bind (id . machine) item
    (list id (vector (vagrant-machine-id machine)
                     (vagrant-machine-name machine)
                     (vagrant-machine-provider machine)
                     (vagrant-machine-state machine)
                     (vagrant-machine-directory machine)))))

(defun vagrant--generate-table-entries ()
  (mapcar #'vagrant--generate-table-entry (vagrant--machines)))

(defun vagrant--machine-read (n)
  "Read Nth entry of vagrant machine."
  (if (eq major-mode 'vagrant-machine-mode)
      (aref (tabulated-list-get-entry) n)
    (vagrant-completing-read "Vagrant machine id: "
                             (vagrant--machines)
                             nil nil nil nil
                             (tabulated-list-get-id))))

;; ------------------------------------------------------------
;;* Machine list interactive commands

(vagrant-machine-cmds vagrant-commands)

(defun vagrant-machine-reload-index ()
  "Reload vagrant machines index."
  (interactive)
  (vagrant--machines t))

;;;###autoload
(defun vagrant-list ()
  "List vagrant boxes in `vagrant-machine-mode'."
  (interactive)
  (with-current-buffer (get-buffer-create "*Vagrant Machines*")
    (vagrant-machine-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;; ------------------------------------------------------------
;;* List mode

(defvar vagrant-machine-menu
  (vagrant-machine-make-menu
    vagrant-commands
    ["Reload index" vagrant-machine-reload-index t]))

(defvar vagrant-machine-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil vagrant-machine-menu)
    (define-key km (kbd "u") 'vagrant-machine-up)
    (define-key km (kbd "s") 'vagrant-machine-ssh)
    (define-key km (kbd "r") 'vagrant-machine-reload-index)
    km)
  "Vagrant machine mode map")

(define-derived-mode vagrant-machine-mode tabulated-list-mode "Vagrant Boxes"
  "List of vagrant machines.\n
Commands: \n
\\{vagrant-machine-map}"
  (setq tabulated-list-format [("id" 7 nil)
                               ("name" 10 nil)
                               ("provider" 10 nil)
                               ("state" 10 nil)
                               ("directory" 60 nil)])
  (add-hook 'tabulated-list-revert-hook 'vagrant-machines-reload nil t)
  (setq tabulated-list-entries 'vagrant--generate-table-entries)
  (tabulated-list-init-header))

(provide 'vagrant)

;;; vagrant.el ends here

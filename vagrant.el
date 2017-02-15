;;; vagrant --- Utilities to manage vagrant from emacs.

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/vagrant
;; Keywords: vagrant docker
;; Created: 18 September 2016

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

;; [![Build Status](https://travis-ci.org/nverno/vagrant.svg?branch=master)](https://travis-ci.org/nverno/vagrant)

;; Manages vagrant boxes from emacs.

;; Commands are defined for all vagrant command-line commmands through the corresponding
;; macros in `vagrant-macros.el'.  There are methods for TRAMP and vagrant ssh.

;;  Also defines two modes,
;; - vagrant-mode:
;;   For vagrant-ssh as well as other vagrant commands.
;; - vagrant-machine-mode:
;;   List interface to available boxes adapted from [emacs-pe](https://github.com/emacs-pe/vagrant.el)

;;; Code:

(eval-when-compile
  (require 'vagrant-macros)
  (require 'subr-x))

(defgroup vagrant nil
  "Vagrant box management tools for emacs."
  :group 'external)

(eval-and-compile
  (defcustom vagrant-commands
    '("connect" "destroy" "docker-logs" "docker-run"
      "halt" "login" "package" "plugin"
      "provision" "rdp" "reload" "resume" "rsync" "rsync-auto" "snapshot"
      "share" "ssh" "ssh-config" "status" "suspend" "up")
    "List of vagrant commands to call interactively."
    :type 'sexp
    :group 'vagrant)

  (defcustom vagrant-global-commands
    '("box" "global-status" "help" "init" "list-commands" "version")
    "List of vagrant commands to call interactively that don't require 
locating vagrant root directory for project."
    :type 'sexp
    :group 'vagrant))

(defcustom vagrant-completing-read 'ido-completing-read
  "Completion engine to use."
  :type 'function
  :group 'vagrant)

;; ------------------------------------------------------------
;;; Internal

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

(defsubst vagrant-list-boxes ()
  "List of vagrant boxes in `vagrant-root'."
  (when-let ((root (vagrant-locate-vagrantfile)))
   (let ((dir (expand-file-name ".vagrant/machines/" root)))
     (delq nil (directory-files dir nil "^[^.]")))))

;; vagrantfile

(defun vagrant-vagrantfile-pn (file)
  "Retrieve private network ip defined in vagrantfile."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "private_network.*?\\([0-9.]+\\)" nil t)
      (match-string-no-properties 1))))

;; ------------------------------------------------------------
;;; Build interactive commmands

(vagrant-cmds vagrant-global-commands nil t)
(vagrant-cmds vagrant-commands)

(defun vagrant-open-vagrantfile (&optional dir)
  "Open associated vagrantfile."
  (interactive)
  (let ((dir (or (and dir (vagrant-locate-vagrantfile dir))
                 (and current-prefix-arg
                      (vagrant-locate-vagrantfile
                       (read-directory-name "Directory to search for Vagrantfile: ")))
                 (vagrant-locate-vagrantfile))))
    (when dir
      (find-file (expand-file-name "Vagrantfile" dir)))))

(defun vagrant-tramp-shell ()
  "Tramp into private network."
  (interactive)
  (let* ((dir (or (and (eq major-mode 'vagrant-machine-mode)
                       (vagrant--machine-read 4))
                  (directory-file-name (vagrant-locate-vagrantfile))))
         (vfile (expand-file-name "Vagrantfile" dir))
         (name (vagrant-machine-get dir "directory" "name"))
         (ip (vagrant-vagrantfile-pn vfile)))
    (when ip
      (let ((default-directory
              (format "/%s:vagrant@%s:"
                      (if (eq system-type 'windows-nt) "plink" "ssh") ip))
            (explicit-shell-file-name "/bin/bash")
            (buff (concat "*vagrant:" name "*")))
        (if (eq system-type 'windows-nt)
            (shell buff)
          (ansi-term "/bin/bash" buff))))))

;;;###autoload
(defun vagrant-vagrant ()
  "Launch `vagrant-mode'"
  (interactive)
  (vagrant-global-status))

;; ------------------------------------------------------------
;;; Note: windows needs process-coding utf-8-unix otherwise there is a
;;  trailing '\r' in ssh

(defvar vagrant-menu
  (vagrant-make-menu vagrant-commands "Vagrant" "vagrant-"
    vagrant-global-commands
    ["Vagrantfile" vagrant-open-vagrantfile t]
    ["Tramp shell" vagrant-tramp-shell t]))

(defvar vagrant-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil vagrant-menu)
    (define-key km (kbd "C-z")      'vagrant-tramp-shell)
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
;;; List machines
;; see https://github.com/emacs-pe/vagrant.el

(cl-defstruct (vagrant-machine (:constructor vagrant-machine--create))
  id name provider state directory)

(defconst vagrant-global-re
  "^\\([[:xdigit:]]+\\)\\s-*\\([[:alnum:]_]+\\)\\s-*\\(\\w+\\)\\s-*\\(\\w+\\)\\s-*\\(.*\\)"
  "Regexp to match `vagrant global-status' output.")

(defvar vagrant-machines nil)
(defun vagrant--machines (&optional refetch)
  "Fetch the vagrant machines."
  (when (or refetch (not vagrant-machines))
    (let* ((str (shell-command-to-string "vagrant global-status"))
           (lines (nthcdr 2 (split-string str "\n" nil " *"))))
      (unless (string-prefix-p "There are no" (car lines))
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
                                  :directory dir))))))))
  vagrant-machines)

(defun vagrant--generate-table-entry (item)
  "Generate a tabulate mode entry from an ITEM."
  (cl-destructuring-bind (id . machine) item
    (list id (vector (vagrant-machine-id machine)
                     (vagrant-machine-name machine)
                     (vagrant-machine-provider machine)
                     (vagrant-machine-state machine)
                     (vagrant-machine-directory machine)))))

(defsubst vagrant--generate-table-entries ()
  (mapcar #'vagrant--generate-table-entry (vagrant--machines)))

(defun vagrant--machine-read (n)
  "Read Nth entry of vagrant machine."
  (if (eq major-mode 'vagrant-machine-mode)
      (aref (tabulated-list-get-entry) n)
    (vagrant-completing-read "Vagrant machine id: "
                             (vagrant--machines)
                             nil nil nil nil
                             (tabulated-list-get-id))))

(defun vagrant-machine-get-id (value fun)
  "Retrieve vagrant machine id using retrieval FUN, that matched VALUE."
  (cl-flet ((fn (intern (concat "vagrant-machine-" fun))))
   (cl-loop for (id . dat) in vagrant-machines
      when (string= (downcase value) (downcase (fn dat))) return id)))

(defun vagrant-machine-get (value match-fun ret-fun)
  "Retrieve vagrant machine value using RET-FUN where there is a match for VALUE
using retrieval RET-FUN."
  (cl-flet ((fn (intern (concat "vagrant-machine-" match-fun)))
            (fn2 (intern (concat "vagrant-machine-" ret-fun))))
   (cl-loop for (id . dat) in vagrant-machines
      when (string= (downcase value) (downcase (fn dat)))
      return (fn2 dat))))

(defun vagrant-machine-invert-index (item)
  "Return alist keyed by ITEM to id."
  (cl-flet ((fn (intern (concat "vagrant-machine-" item))))
    (cl-loop for (id . dat) in vagrant-machines
       collect (cons (fn dat) id))))

;; ------------------------------------------------------------
;;; Machine list interactive commands

(vagrant-machine-cmds vagrant-commands)

(defun vagrant-machine-vagrantfile (dir)
  (interactive (list (vagrant--machine-read 4)))
  (vagrant-open-vagrantfile dir))

(defun vagrant-machine-reload-index ()
  "Reload vagrant machines index."
  (interactive)
  (message "Reloading vagrant machine index...")
  (vagrant--machines t))

;;;###autoload
(defun vagrant-list ()
  "List vagrant boxes in `vagrant-machine-mode'."
  (interactive)
  (with-current-buffer (get-buffer-create "*Vagrant Machines*")
    (vagrant--machines)
    (vagrant-machine-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;; ------------------------------------------------------------
;;; List mode

(defvar vagrant-machine-menu
  (vagrant-make-menu vagrant-commands "Vagrant Machines" "vagrant-machine-"
    nil
    ["Reload index" vagrant-machine-reload-index t]
    ["Vagrantfile" vagrant-machine-vagrantfile t]
    ["Tramp shell" vagrant-tramp-shell t]
    "--"))

(defvar vagrant-machine-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil vagrant-machine-menu)
    (define-key km (kbd "C-z")  'vagrant-tramp-shell)
    (define-key km (kbd "u")    'vagrant-machine-up)
    (define-key km (kbd "s")    'vagrant-machine-ssh)
    (define-key km (kbd "v")    'vagrant-machine-vagrantfile)
    (define-key km (kbd "h")    'vagrant-machine-halt)
    km)
  "Vagrant machine mode map")

(define-derived-mode vagrant-machine-mode tabulated-list-mode "Vagrant Boxes"
  "List of vagrant machines.\n
Commands: \n
\\{vagrant-machine-mode-map}"
  (setq tabulated-list-format [("id" 7 nil)
                               ("name" 10 nil)
                               ("provider" 10 nil)
                               ("state" 10 nil)
                               ("directory" 60 nil)])
  (add-hook 'tabulated-list-revert-hook 'vagrant-machine-reload-index nil t)
  (setq tabulated-list-entries 'vagrant--generate-table-entries)
  (tabulated-list-init-header))

;; ------------------------------------------------------------
;;; Tramp methods

(autoload 'tramp-dump-connection-properties "tramp-cache")
(defvar tramp-cache-data)
(defvar tramp-cache-data-changed)
(defvar tramp-methods)
(declare-function tramp-file-name-method "tramp")

(defvar vagrant-tramp-method "vagrant")
(defvar vagrant-tramp-config-file
  (expand-file-name "etc/config/vagrant-tramp-config" "~/.emacs.d"))

(defun vagrant-tramp-add-ssh-config (id)
  "Add ssh config for machine ID to `vagrant-tramp-config-file'."
  (interactive 
   (list
    (or (and (eq major-mode 'vagrant-machine-mode)
             (vagrant--machine-read 0))
        (and vagrant-root
             (vagrant-machine-get-id (directory-file-name vagrant-root)
                                     "directory"))
        (let* ((inv (vagrant-machine-invert-index "name"))
               (name (vagrant-completing-read "Add ssh-config for machine: " inv)))
          (cdr (assoc-string name inv))))))
  (with-temp-buffer
    (let ((exit-status (call-process
                       "vagrant" nil (current-buffer) nil "ssh-config" id)))
     (if (zerop exit-status)
         (write-region (buffer-string) nil vagrant-tramp-config-file 'append)
       (error (buffer-string))))))

(defun vagrant-tramp-blank-config (&optional reset)
  (when (or reset
            (not (file-exists-p vagrant-tramp-config-file)))
    (with-temp-file vagrant-tramp-config-file (insert ""))))

(defun vagrant-tramp-cleanup ()
  "Cleanup vagrant tramp connection info."
  (interactive)
  (maphash #'(lambda (key _)
               (and (vectorp key)
                    (string= vagrant-tramp-method (tramp-file-name-method key))
                    (remhash key tramp-cache-data)))
           tramp-cache-data)
  (setq tramp-cache-data-changed t)
  (when (eq current-prefix-arg '(4))
    (vagrant-tramp-blank-config t))
  (tramp-dump-connection-properties))

(defun vagrant-tramp-add-plink-method ()
  (interactive)
  (add-to-list 'tramp-methods
               `(,vagrant-tramp-method
                 (tramp-login-program "plink")
                 (tramp-login-args
                  (("-l" "%u")
                   ("-P" "%p")
                   ("-ssh")
                   ("-t")
                   ("%h")
                   ("\"")
                   ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '")
                   ("/bin/bash")
                   ("\"")))
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-login
                  ("-l"))
                 (tramp-remote-shell-args
                  ("-c"))
                 (tramp-default-port 22))))

(defun vagrant-tramp-add-ssh-method ()
  "Add vagrant method for tramp."
  (interactive)
  (add-to-list 'tramp-methods
               `(,vagrant-tramp-method
                 (tramp-login-program       "ssh")
                 (tramp-login-args          (("-l" "%u")
                                             ("-p" "%p")
                                             ("%c")
                                             ("-e" "none")
                                             ("%h")
                                             ("-F" ,vagrant-tramp-config-file)))
                 (tramp-async-args           (("-q")))
                 (tramp-remote-shell         "/bin/bash")
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-gw-args              (("-o" "GlobalKnownhostsfile=/dev/null")
                                              ("-o" "UserKnownHostsFile=/dev/null")
                                              ("-o" "StrictHostKeyChecking=no")))
                 (tramp-default-port         22))))

(defconst vagrant-tramp-completion-function-alist
  `((vagrant-tramp-blank-config ,vagrant-tramp-config-file)
    (tramp-parse-sconfig        ,vagrant-tramp-config-file))
  "Alist of (FUNCTION FILE) for vagrant method.")

;; (eval-after-load 'tramp
;;   '(progn
;;      (vagrant-tramp-add-plink-method)
;;      (tramp-set-completion-function vagrant-tramp-method
;;                                     vagrant-tramp-completion-function-alist)))

(provide 'vagrant)

;;; vagrant.el ends here

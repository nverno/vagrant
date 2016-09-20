;;; vagrant-tramp-shell --- vagrant ssh -*- no-byte-compile: t; -*-

;; Mixing vagrant-tramp.el and vagrant.el

;;; Code:

(eval-when-compile
  (require 'comint)
  (require 'tramp)
  (require 'tramp-cache)
  (require 'cl-lib))
(autoload 'comint-exec "comint")

(defgroup vagrant nil
  "Vagrant box management."
  :prefix "vagrant-"
  :group 'applications)

(defcustom vagrant-executable "vagrant"
  "Vagrant exectuble path"
  :type 'string
  :group 'vagrant)

(defcustom vagrant-machines-buffer-name "*vagrant-machines*"
  "Vagrant buffer name for list of machines."
  :type 'string
  :group 'vagrant)

(defcustom vagrant-ssh-config-file
  (expand-file-name "vagrant-ssh-config" user-emacs-directory)
  "Vagrant ssh_config(5) filename."
  :type 'string
  :group 'vagrant)

(defcustom vagrant-disable-asking nil
  "Disable asking before destructive operations."
  :type 'boolean
  :group 'vagrant)

(defconst vagrant-tramp-method "vagrant"
  "Method to connect vagrant machines.")

(cl-defstruct (vagrant-machine (:constructor vagrant-machine--create))
  "A structure holding all the information of a vagrant machine."
  id name provider state dir)

(defvar vagrant-machines nil
  "An alist containing available vagrant machines.")

(defvar vagrant-machines-already-fetched nil)

(define-error 'vagrant-error "Unknown vagrant error")
(define-error 'vagrant-machine-notfound "Vagrant machine not found"
  'vagrant-error)
(define-error 'vagrant-command-error "Vagrant command exited abnormally"
  'vagrant-error)

(defvar vagrant-global-re
  "^\\([[:xdigit:]]+\\)\\s-*\\([[:alnum:]_]+\\)\\s-*\\(\\w+\\)\\s-*\\(\\w+\\)\\s-*\\(.*\\)"
  "Regexp to match `vagrant global-status' output.")

(defun vagrant--machines ()
  "Fetch the vagrant machines."
  (unless vagrant-machines-already-fetched
    (setq vagrant-machines-already-fetched t)
    (let* ((str (shell-command-to-string
                 (concat vagrant-executable " global-status")))
           (start (+ 2 (string-match "-\n" str)))
           (lines (split-string (substring str start) "\n" nil "\\s-*")))
      (setq vagrant-machines
            (cl-loop for line in lines
               while (not (string= line ""))
               for value = (split-string line)
               collect (cl-multiple-value-bind
                           (id name provider state dir) value
                         (cons id
                               (vagrant-machine--create
                                :id id
                                :name name
                                :provider provider
                                :state state
                                :dir dir)))))))
  vagrant-machines)

(defun vagrant--run-subcommand (&rest args)
  "Run vagrant subcommand with ARGS."
  (let ((command (mapconcat 'identity (cons vagrant-executable args) " ")))
    (compilation-start command nil
                       (lambda (_) (format "Vagrant: %s" command)))))

(defconst vagrant-tramp-ssh
  (shell-quote-argument
   (concat (file-name-directory
            (or load-file-name
                buffer-file-name))
           "bin/vagrant-tramp-ssh")))

(defun vagrant-tramp--all-boxes ()
  "Alist of vagrant box info."
  (save-match-data
    (let* ((str (shell-command-to-string "vagrant global-status"))
           (start (+ 2 (string-match "-\n" str)))
           (lines (split-string (substring str start) "\n" nil "\\s-*")))
      (cl-loop for line in lines
         while (not (string= line ""))
         do (string-match vagrant-global-re line)
         collect (cl-loop for x in '(id name provider state dir)
                    and y upfrom 1
                    collect (cons x (match-string y line)))))))

(defun vagrant-tramp--box-running-p (box)
  "True if `BOX' is reported as running."
  (string= (cdr (assoc 'state box)) "running"))

(defun vagrant-tramp--box-name (box)
  (let ((name (cdr (assoc 'name box))))
    (concat (file-name-base (cdr (assoc 'dir box)))
            (unless (string= name "default")
              (concat "--" name)))))

(defun vagrant-tramp--running-boxes ()
  (seq-filter #'vagrant-tramp--box-running-p
              (vagrant-tramp--all-boxes)))

(defun vagrant-tramp--completions (&optional file)
  "List for vagrant tramp completion.  FILE argument is ignored."
  (mapcar #'(lambda (box)
              (list "" (vagrant-tramp--box-name box)))
         (vagrant-tramp--running-boxes)))

(defun vagrant-tramp-output-filter (str)
  "Add prompt to output string from subprocess."
  (concat str "\n$ "))

(defun vagrant-tramp-shell (&optional box-name)
  (interactive)
  (let* ((boxes (vagrant-tramp--running-boxes))
         (names (mapcar #'vagrant-tramp--box-name boxes))
         (box-name (or box-name
                       (if (listp names)
                           (ido-completing-read "vagrant ssh to: " names)
                         (car names))))
         (name (concat "vagrant terminal:" box-name))
         (buffer (get-buffer-create (format "*%s*" name)))
         (box (car (cl-member-if
                    #'(lambda (bs) (string= box-name (vagrant-tramp--box-name bs)))
                    boxes)))
         (box-id (cdr (assoc 'id box)))
         (default-directory (cdr (assoc 'dir box))))
    (comint-exec buffer name "vagrant" nil (list "ssh" box-id))
    (switch-to-buffer buffer)
    (comint-mode)
    (add-hook 'comint-preoutput-filter-functions
              #'vagrant-tramp-output-filter nil t)))

(defun vagrant-tramp-add-method ()
  "Add `vagrant-tramp-method' to `tramp-methods'."
  (add-to-list 'tramp-methods
               `(,vagrant-tramp-method
                 (tramp-login-program ,vagrant-tramp-ssh)
                 (tramp-login-args (("%h")
                                    ("\"")
                                    ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '")
                                    ("/bin/sh")
                                    ("\"")))
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-login ("-l"))
                 (tramp-remote-shell-args ("-c")))))

(defconst vagrant-tramp-completion-function-alist
  '((vagrant-tramp--completions "")))

(eval-after-load 'tramp
  '(progn
     (vagrant-tramp-add-method)
     (tramp-set-completion-function
      vagrant-tramp-method
      '((vagrant-tramp--completions "")))))

(provide 'vagrant-tramp-shell)

;;; vagrant-tramp-shell.el ends here

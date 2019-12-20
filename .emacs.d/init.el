(require 'package)

;; ------------------------------------------------------------------------------------
;; STRAIGHT SETUP
;; This section was copied from github.com/raxod502/striaght.el/README.md

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; CUSTOM COMMANDS & ALIASES

(defun load-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defmacro leaf-all (packages)
  (cons 'progn (mapcar (lambda (pkg) `(leaf ,pkg :straight t)) packages)))

(defalias 'lcfg 'load-config)

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; PACKAGE CONFIGS

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf-keywords-init)

; This function was partially copied from hankail02/dotfiles/.emacs.d/config/base/init-evil.el
(leaf evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
  :bind ((:evil-insert-state-map ("C-k" . nil)) ; conflict with other ^k bindings
	 (:minibuffer-local-map  ("C-j" . next-line-or-history-element)
				 ("C-k" . previous-line-or-history-element)))) 

(leaf magit
  :straight t)

(leaf which-key
  :straight t)

;; ------------------------------------------------------------------------------------

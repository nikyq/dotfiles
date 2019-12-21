(require 'package)

;; ------------------------------------------------------------------------------------
;; STRAIGHT SETUP
;; This section was copied from github.com/raxod502/striaght.el/README.md

(setq package-enable-at-startup nil)

(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; CUSTOM COMMANDS & ALIASES

(defmacro leaf-all (&rest packages)
  (cons 'progn (mapcar (lambda (pkg) `(leaf ,pkg :straight t)) packages)))

(defun load-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun magit-stage-current-buffer-and-commit ()
  (interactive)
  (magit-stage-file (buffer-file-name (window-buffer (minibuffer-selected-window))))
  (magit-commit-create))

(defalias 'lcfg 'load-config)
(defalias 'mscbc 'magit-stage-current-buffer-and-commit)
(defalias 'pes 'pp-eval-last-sexp)

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; INITIALIZATION

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf-keywords-init)
(leaf-all counsel swiper)

;; ------------------------------------------------------------------------------------
;; GLOBAL CONFIGS

(setq find-file-visit-truename t)

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; PACKAGE CONFIGS

; This part was partially copied from hankail05/dotfiles/.emacs.d/config/base/init-evil.el
(leaf evil
  :straight t
  :leaf-defer nil
  :config (evil-mode 1)
  :bind ((:evil-insert-state-map ("C-k" . nil)) ; conflict with other ^k bindings
         (:minibuffer-local-map  ("C-j" . next-line-or-history-element)
                                 ("C-k" . previous-line-or-history-element))))

(leaf god-mode
  :straight t
  :require t
  :leaf-defer nil
  :bind* ("ESC" . nil))

(leaf ivy
  :straight t
  :config (ivy-mode 1)
  :setq (ivy-use-virtual-buffers . t)
        (enable-recursive-minibuffers . t))

(leaf which-key
  :straight t
  :config (which-key-mode 1))

(leaf magit
  :straight t
  :require t
  :leaf-defer nil
  :bind (:magit-file-mode-map ("C-c C-c" . magit-stage-current-buffer-and-commit)))

;; ------------------------------------------------------------------------------------

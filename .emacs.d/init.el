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

(defun indent-relative-back ()
  (interactive)
  (if (and abbrev-mode
	   (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
	  (let ((end (save-excursion (forward-line 1) (point))))
	    (move-to-column start-column)
	    ;; Is start-column inside a tab on this line?
	    (if (> (current-column) start-column)
		(backward-char 1))
	    (or (looking-at "[ \t]")
		(skip-chars-backward " \t" end))
	    (skip-chars-backward "^ \t" end)
	    (or (= (point) end) (setq indent (current-column))))))
    (cond (indent
           (let ((opoint (point-marker)))
             (print indent)
             (indent-to indent 0)
             (if (> opoint (point))
                 (goto-char opoint))
             (move-marker opoint nil)))
          (t (tab-to-tab-stop)))))
            
(defalias 'lcfg 'load-config)
(defalias 'mscbc 'magit-stage-current-buffer-and-commit)
(defalias 'pes 'pp-eval-last-sexp)

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; INITIALIZATION

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf-keywords-init)
(leaf-all counsel swiper god-mode evil-god-state)

;; ------------------------------------------------------------------------------------
;; GLOBAL CONFIGS

(setq find-file-visit-truename t)
(define-key global-map (kbd "s-c") 'evil-execute-in-god-state)
(setq-default indent-tabs-mode nil)

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; PACKAGE CONFIGS

(leaf evil
  :straight t
  :leaf-defer nil
  :config (evil-mode 1))

(leaf evil-god-state
  :straight t
  :config (evil-define-key 'normal global-map (kbd "SPC") 'evil-execute-in-god-state)
          (evil-define-key 'visual global-map (kbd "SPC") 'evil-execute-in-god-state)
          (evil-define-key 'god global-map (kbd "ESC") 'evil-god-state-bail))

(leaf ivy
  :straight t
  :config (ivy-mode 1)
  :setq (ivy-use-virtual-buffers . t)
        (enable-recursive-minibuffers . t))

(leaf which-key
  :straight t
  :config (which-key-mode 1)
          (which-key-enable-god-mode-support))

(leaf magit
  :straight t
  :require t
  :leaf-defer nil
  :bind (:magit-file-mode-map ("C-c C-c" . magit-stage-current-buffer-and-commit)))

;; ------------------------------------------------------------------------------------

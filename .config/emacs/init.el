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
  (load-file "~/.config/emacs/init.el"))

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

(defun avy-goto-open-paren ()
  (interactive)
  (avy--generic-jump "(" nil))

(defun avy-goto-closed-paren ()
  (interactive)
  (avy--generic-jump ")" nil))

(defalias 'lcfg 'load-config)
(defalias 'mscbc 'magit-stage-current-buffer-and-commit)
(defalias 'pes 'pp-eval-last-sexp)

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; INITIALIZATION

(straight-use-package 'exec-path-from-shell)
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

(exec-path-from-shell-initialize)
(leaf-keywords-init)
(leaf-all counsel swiper hydra expand-region yasnippet vterm flycheck maude-mode) 

;; ------------------------------------------------------------------------------------
;; GLOBAL CONFIGS

(defun config-global () 
  (setq find-file-visit-truename t)
  (global-set-key (kbd "<Hangul>") 'toggle-input-method)

  (set-face-attribute 'default nil :family "Iosevka Term Extended")

  (set-face-attribute 'default nil :height 100)

  (setq face-font-rescale-alist
        '(("NanumGothicCoding" . 1.1)))

                                        ;| 가 | 나 | 다 | 라 | 마 | 바 | 사 | 아 | 자 | 차 |
                                        ;| aa | bb | cc | dd | ee | ff | gg | hh | ii | jj |

                                        ; (define-key global-map (kbd "s-c") 'evil-execute-in-god-state)
  
  (set-input-method "korean-hangul")

  (setq c-basic-offset 4)
  (c-set-offset 'brace-list-intro '+)
  (c-set-offset 'brace-list-entry 'c-lineup-under-anchor)

  (setq-default indent-tabs-mode nil)
  (global-display-line-numbers-mode)
  (add-hook 'after-init-hook 'global-company-mode)

  (tool-bar-mode 0)
  (centaur-tabs-mode 1)

  (with-eval-after-load "quail"
    (push
     (cons "colemak"
           (concat
            "                              "
            "`~1!2@3#4$5%6^7&8*9(0)-_=+    "   ; numbers
            "  qQwWfFpPgGjJlLuUyY;:[{]}\|\|  " ; qwerty
            "  aArRsStTdDhHnNeEiIoO'\"      "   ; asdf
            "  zZxXcCvVbBkKmM,<.>/?        "   ; zxcv
            "                              "))
     quail-keyboard-layout-alist)

    (quail-set-keyboard-layout "colemak"))

  (with-eval-after-load "quail/hangul"
    (defun hangul2-input-method (key)
      "2-Bulsik input method."
      (setq key (quail-keyboard-translate key))
      (if (or buffer-read-only (not (alphabetp key)))
          (list key)
        (quail-setup-overlays nil)
        (let ((input-method-function nil)
              (echo-keystrokes 0)
              (help-char nil))
          (setq hangul-queue (make-vector 6 0))
          (hangul2-input-method-internal key)
          (unwind-protect
              (catch 'exit-input-loop
                (while t
                  (let* ((seq (read-key-sequence nil))
                         (cmd (lookup-key hangul-im-keymap seq))
                         key)
                    (cond
                     ((and (stringp seq)
                           (= 1 (length seq))
                           (setq key (quail-keyboard-translate (aref seq 0)))
                           (alphabetp key))
                      (hangul2-input-method-internal key))
                     ((commandp cmd)
                      (call-interactively cmd))
                     (t
                      (setq unread-command-events
                            (nconc (listify-key-sequence seq)
                                   unread-command-events))
                      (throw 'exit-input-loop nil))))))
            (quail-delete-overlays))))))

  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))

;; ------------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------------
;; PACKAGE CONFIGS

(leaf evil
  :straight t
  :leaf-defer nil
  :pre-setq
  (evil-want-C-u-scroll . t)
  :config
  (evil-mode 1))

;; (leaf evil-god-state
;;   :straight t
;;   :config (evil-define-key 'normal global-map (kbd "SPC") 'evil-execute-in-god-state)
;;           (evil-define-key 'visual global-map (kbd "SPC") 'evil-execute-in-god-state)
;;           (evil-define-key 'god global-map (kbd "ESC") 'evil-god-state-bail))

;; (leaf aggressive-indent
;;   :straight t
;;   :config (global-aggressive-indent-mode 1))

(leaf undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
  :bind
  (:undo-tree-map ("C-/". nil)))

(straight-use-package
  '(selectrum :host github :repo "raxod502/selectrum")) ; Look! This library is not even on MELPA! Amazing!

(straight-use-package
  '(selectrum-prescient :host github :repo "raxod502/prescient.el"
                        :files ("selectrum-prescient.el")))

(leaf selectrum
  :straight t                           ; This won't have much effects...
  :config (selectrum-mode 1)
          (selectrum-prescient-mode 1)
          (prescient-persist-mode 1))

(leaf prescient
  :straight t
  :config (add-to-list 'prescient-filter-method 'fuzzy))

(leaf lsp-mode
  :straight t

  :custom
  (lsp-rust-analyzer-cargo-watch-command . "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints . t)

  :config
  (setq lsp-enable-indentation nil)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)))

(leaf lsp-ui
  :straight t
  :custom (lsp-ui-doc-position . 'bottom))

(leaf eglot
  :preface
  (defun project-root (project)
    (car (project-roots project))))

(leaf company
  :straight t
  :require t
  :bind ("M-<tab>" . company-complete))

(leaf yasnippet
  :straight t
  :config (yas-reload-all)
  :hook (prog-mode-hook . yas-minor-mode))

(leaf rtags
  :straight t
  :require t)

(leaf which-key
  :straight t
  :config (which-key-mode 1))
;; (which-key-enable-god-mode-support)

(leaf symex
  :straight t
  :config
  (setq symex--user-evil-keyspec
        '(("j" . symex-go-up)
          ("k" . symex-go-down)
          ("C-j" . symex-climb-branch)
          ("C-k" . symex-descend-branch)
          ("M-j" . symex-goto-highest)
          ("M-k" . symex-goto-lowest)))
  (symex-initialize)
  (evil-define-key 'normal 'global (kbd ",") 'symex-mode-interface))

(leaf magit
  :straight t
  :require t
  :leaf-defer nil
  :config (define-key global-map (kbd "C-c C-c") 'magit-stage-current-buffer-and-commit))

(leaf treemacs
  :straight t
  :config (evil-define-key 'treemacs treemacs-mode-map (kbd "B") #'treemacs-bookmark)
  :bind ("C-<SPC>" . treemacs))

(leaf treemacs-evil
  :straight t
  :require t)

(leaf centaur-tabs
  :straight t
  :require t
  :config (centaur-tabs-mode 1)
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward)))

(leaf org
  :require t)

(leaf org-mind-map
  :straight t
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot"))

(leaf verb
  :straight (verb :type git
                  :host github
                  :repo "federicotdn/verb")
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(leaf key-chord
  :straight t
  :custom (key-chord-two-keys-delay . 0.01)
  :config (key-chord-mode 1))

(leaf avy
  :straight t
  :require t
  :require
  :config (define-key global-map (kbd "C-/") 'avy-goto-word-0))

(leaf rainbow-delimiters
  :straight t
  :hook
  (lisp-mode-hook . #'rainbow-delimiters-mode)
  (emacs-lisp-mode-hook . #'rainbow-delimiters-mode)
  (clojure-mode-hook . #'rainbow-delimiters-mode))

(leaf windmove
  :straight t
  :config (evil-define-key 'normal 'global-map (kbd "M-h") 'windmove-left)
          (evil-define-key 'normal 'global-map (kbd "M-l") 'windmove-right)
          (evil-define-key 'normal 'global-map (kbd "M-k") 'windmove-up)
          (evil-define-key 'normal 'global-map (kbd "M-j") 'windmove-down))

(leaf cider
  :straight t)

(leaf haskell-mode
  :straight t
  :hook (haskell-mode-hook . interactive-haskell-mode)
  :custom
  (haskell-indentation-layout-offset . 4)
  (haskell-indentation-starter-offset . 4)
  (haskell-indentation-left-offset . 4)
  (haskell-indentation-where-pre-offset . 2)
  (haskell-indentation-where-post-offset . 4))

(leaf rustic
  :straight t
  :hook (rustic-mode-hook . (lambda () (setq-local buffer-save-without-query t)))
  :bind (:rustic-mode-map
         ("M-?" . lsp-find-references)
         ("C-c C-c l" . flycheck-list-errors)
         ("C-c C-c a" . lsp-execute-code-action)
         ("C-c C-c r" . lsp-rename)
         ("C-c C-c q" . lsp-workspace-restart)
         ("C-c C-c Q" . lsp-workspace-shutdown)
         ("C-c C-c s" . lsp-rust-analzyer-status))
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil))


(leaf elcord
  :straight t
  :require t
  :config (elcord-mode))

;; ------------------------------------------------------------------------------------

(config-global)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-background-color "black")
 '(centaur-tabs-mode t nil (centaur-tabs))
 '(haskell-indentation-layout-offset 4 t)
 '(haskell-indentation-starter-offset 4 t)
 '(haskell-indentation-where-post-offset 4 t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "grey55"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "SkyBlue1"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "SeaGreen1"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "orchid"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "khaki1"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "lime green"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "maroon1"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "DarkGoldenrod1")))))

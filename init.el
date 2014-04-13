(setq inhibit-startup-message t)

(add-to-list 'load-path "~/.emacs.d")

;; initialize packages
(require 'package)
(setq package-enable-at-startup nil)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(load "ensure-packages")
(ensure-packages-install-missing)

(setq initial-frame-alist '((top . 58)
                            (left . 10)
                            (width . 170)
                            (height . 40)))

; Ubuntu Mono at 10.5pt
(set-face-font 'default "Ubuntu Mono-10.5")
;(set-face-font `default "Consolas-11")
; Meiryo 11 for Japanese
;(set-fontset-font t 'japanese-jisx0208 "Meiryo-11")

; Add to load path
(setq load-path (cons "~/.emacs.d/elisp-files" load-path))

; Load the adwaita theme
(load-theme 'adwaita)

; Disable backups
(setq make-backup-files nil)

; js2-mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))
;; (setq js2-language-version 180)

; line numbers
(global-linum-mode t)

; get rid of the stupid toolbar
(tool-bar-mode 0)

; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

; disable scrollbar
(scroll-bar-mode -1)

; set column width to 80
(set-fill-column 80)

; set column mode on
(column-number-mode 1)

; save emacs buffers across sessions
(desktop-save-mode 1)

; use Alt-UDLR to move across split viewports
(windmove-default-keybindings 'meta)

; redo via undo-tree
(global-undo-tree-mode)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

; everywhere is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; set frame title
(setq frame-title-format "%f — Emacs")

; move buffers around
(global-set-key (kbd "<S-M-up>")    'buf-move-up)
(global-set-key (kbd "<S-M-down>")  'buf-move-down)
(global-set-key (kbd "<S-M-left>")  'buf-move-left)
(global-set-key (kbd "<S-M-right>") 'buf-move-right)

; require the final newline
(setq require-final-newline t)

; BSD style 4 life
(setq c-default-style "bsd")
(dtrt-indent-mode 1)

; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-async-byte-compile-exclude-files-regexp "init.el")
 '(c-basic-offset 2)
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default)))
 '(desktop-path (quote ("~/.emacs.d" "." "~")))
 '(desktop-save (quote ask-if-new))
 '(desktop-save-mode t)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(oz-indent-chars 2)
 '(safe-local-variable-values (quote ((js2-basic-offset . 2))))
 '(scroll-preserve-screen-position 1)
 '(vc-handled-backends nil))

; ido mode!
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x f") 'ido-find-file)

; CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

; Don't jump around while scrolling
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

; unix line endings
(setq default-buffer-file-coding-system 'utf-8-unix)

; we want to delete text on select
(delete-selection-mode 1)

; don't copy on select
(setq mouse-drag-copy-region nil)

(setq TeX-PDF-mode t)

; 80 chars wide
(setq default-fill-column 80)

; 1.18 line spacing, for maximum readability
(setq-default line-spacing 0.18)

; Emacs server
(server-start)

; Change the cursor dynamically (depending on the mode)
(require 'cursor-chg)
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode

; Do not blink the cursor
(blink-cursor-mode -1)

(put 'narrow-to-region 'disabled nil)

;set xetex mode in tex/latex
(add-hook 'LaTeX-mode-hook (lambda()
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
(setq TeX-command-default "XeLaTeX")
(setq TeX-save-query nil)
(setq TeX-show-compilation t)
))

; Lusty explorer
(require 'lusty-explorer)
(global-set-key (kbd "C-x C-b") 'lusty-buffer-explorer)
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)

; fix for scroll-conservatively jerkiness
(setq auto-window-vscroll nil)

; automatic asynchronous byte compilation
(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Make tramp handle 2-factor auth
(setq
 tramp-password-prompt-regexp
 (concat
  "^.*"
  (regexp-opt
   '("passcode" "Passcode"
     "passphrase" "Passphrase"
     "password" "Password") t)
  ".*:\0? *"))

(setq auto-save-default nil)

; Org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

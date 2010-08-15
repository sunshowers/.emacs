(setq inhibit-startup-message t)
(setq initial-frame-alist '((top . 58)
                            (left . 10)
                            (width . 170)
                            (height . 40)))

; Consolas 11pt
(set-face-font `default "Consolas-11")

; Add to load path
(setq load-path (cons "~/.emacs.d/elisp-files" load-path))

; Twilight colour theme
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/elisp-files/themes/color-theme-twilight.el")
(color-theme-twilight)

; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))
(setq js2-language-version 180)

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

; redo support, yay
(require 'redo)
(global-set-key (kbd "C-S-z") 'redo)

; everywhere is utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; set frame title
(setq frame-title-format "%f — Emacs")

; move buffers around
(require 'buffer-move)
(global-set-key (kbd "<S-M-up>")    'buf-move-up)
(global-set-key (kbd "<S-M-down>")  'buf-move-down)
(global-set-key (kbd "<S-M-left>")  'buf-move-left)
(global-set-key (kbd "<S-M-right>") 'buf-move-right)

; require the final newline
(setq require-final-newline t)

; BSD style 4 life
(setq c-default-style "bsd" c-basic-offset 2)

; NO TABS. EVER.
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

; auto indent
(add-hook 'c-mode-common-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'js2-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

; pastie!
(require 'pastie)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(desktop-path (quote ("~/.emacs.d" "." "~")))
 '(desktop-save (quote ask-if-new))
 '(desktop-save-mode t)
 '(oz-indent-chars 2)

 '(rcirc-default-full-name "Siddharth Agarwal")
 '(rcirc-default-nick "sid0")
 '(rcirc-default-user-name "sid0")
 '(rcirc-dim-nicks (quote ("firebot")))
 '(rcirc-fill-column (quote frame-width))
 '(rcirc-log-flag t)
 '(rcirc-prompt "%t:%n> ")
 '(rcirc-response-formats (quote (("PRIVMSG" . "<%N> %m") ("NOTICE" . "-%N- %m") ("ACTION" . "* %N %m") ("COMMAND" . "%m") ("ERROR" . "%fw!!! %m") ("JOIN" . "--> %N %fshas joined this channel") ("PART" . "%fp<-- %N has left %m") ("NICK" . "%fp<-> %N is now known as %fs%m") ("MODE" . "%fp*** %N has set mode %m") ("QUIT" . "%fp<-- %N has quit: %m") ("TOPIC" . "%fp*** %N has set the topic to %m") (t . "%fp*** %fs%n %r %m"))))
 '(rcirc-server-alist (quote (("irc.oftc.net" :channels ("#hackers-india")) ("75.125.121.93" :port 7778) ("irc.rizon.net" :channels ("#anon32"))
                              ("irc.freenode.net" :channels ("#reddit" "#iitm-linux" "#kit-bot")) ("im.bitlbee.org"))))
 '(rcirc-time-format "[%H:%M:%S] ")
 '(rcirc-track-minor-mode t)
 '(scroll-preserve-screen-position 1)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Git Arch MCVS))))

; RCIRC
(require 'rcirc)
(require 'rcirc-late-fix)

; Authentication info
(load "~/.emacs.d/irc-passwords.el")

(setq rcirc-authinfo `(("oftc" nickserv "sid0" ,irc-oftc-password)
                       ("bitlbee" bitlbee "sid0" ,irc-bitlbee-password)
                       ("75.125.121.93" bitlbee "sid0" ,irc-moz-znc-password)
                       ("Rizon" nickserv "sid0" ,irc-rizon-password)
                       ("Freenode" nickserv "sid0" ,irc-freenode-password)))

; Keep input line at the bottom
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'scroll-conservatively)
		 8192)))
(defface rcirc-late-fix-face '((t (:underline t)))
  "Face for showing fixed words on the channel buffer.")

;; Windows doesn't like "*foo*" filenames
(eval-after-load 'rcirc
  '(defun rcirc-generate-new-buffer-name (process target)
     "Return a buffer name based on PROCESS and TARGET.
This is used for the initial name given to IRC buffers."
     (substring-no-properties
      (if target
	  (concat (replace-regexp-in-string "\*" "" target) "@"
                  (process-name process))
	(process-name process)))))

(eval-after-load 'rcirc
  '(defun-rcirc-command op (nicks)
     "Send OP for `nicks'.
    Limitation: in its interactive form, you can only op one nick."
     (interactive (list (completing-read "Op nick: "
                                         (with-rcirc-server-buffer rcirc-nick-table))))
     (dolist (nick (split-string nicks " "))
       (rcirc-send-string process
                          (format "ChanServ OP %s %s" target nick)))))

(eval-after-load 'rcirc
  '(defalias 'rcirc-cmd-opme
     '(lambda (&optional args process target)
        (interactive)
        (rcirc-cmd-op (rcirc-nick (rcirc-buffer-process))))
     "Request a ChanServ OP on my current nick in the current channel."))

(eval-after-load 'rcirc
  '(defun-rcirc-command deop (nicks)
     "Send DEOP for `nicks'.
    Limitation: in its interactive form, you can only de-op one nick."
     (interactive (list (completing-read "Op nick: "
                                         (with-rcirc-server-buffer rcirc-nick-table))))
     (dolist (nick (split-string nicks " "))
       (rcirc-send-string process
                          (format "ChanServ DEOP %s %s" target nick)))))

(eval-after-load 'rcirc
  (defalias 'rcirc-cmd-deopme
    '(lambda (&optional args process target)
       (interactive)
       (rcirc-cmd-deop (rcirc-nick (rcirc-buffer-process))))))

(define-key rcirc-mode-map (kbd "C-c C-O") 'rcirc-cmd-op)
(define-key rcirc-mode-map (kbd "C-c o") 'rcirc-cmd-opme)
(define-key rcirc-mode-map (kbd "C-c d") 'rcirc-cmd-deopme)
(define-key rcirc-mode-map (kbd "C-c C-D") 'rcirc-cmd-deop)

; browse kill ring
(require 'browse-kill-ring)

; ido mode!
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x f") 'ido-find-file)

; Proper mercurial support!
; (require 'mercurial)

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

; SML mode
(load "sml-mode-startup")

; 1.15 line spacing, for maximum readability
(setq-default line-spacing 0.15)

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
(load "~/.emacs.d/elisp-files/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'cl)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar ensure-packages
  '(buffer-move
    cursor-chg
    dtrt-indent
    go-mode
    haskell-mode
    js2-mode
    lusty-explorer
    markdown-mode
    php-mode
    sml-mode
    tuareg
    caml
    twilight-theme
    undo-tree)
  "A list of packages to ensure are installed at launch.")

(defun ensure-packages-package-installed-p (p)
  (cond ((package-installed-p p) t)
	(t nil)))
  
(defun ensure-packages-installed-p ()
  (mapcar 'ensure-packages-package-installed-p ensure-packages))
  
(defun ensure-packages-install-missing ()
  (interactive)
  (unless (every 'identity (ensure-packages-installed-p))
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p ensure-packages)
    (when (not (package-installed-p p))
      (package-install p)))))

(provide 'ensure-packages)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)
(add-to-list 'default-frame-alist '(font . "Hack-10"))
(global-visual-line-mode 1)
(global-hl-line-mode 1)

(setq make-backup-files nil)
(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.."
  (if buffer-file-name
    (concat
      (file-name-directory buffer-file-name)
      ".cache/autosave/"
      (file-name-nondirectory buffer-file-name))
    (expand-file-name
      (concat "#%" (buffer-name) "#"))))

(defun kill-invisible-buffers ()
  (dolist (buf  (buffer-list))
    (unless (get-buffer-window buf t) (kill-buffer buf))))
;; kill invisible buffers every 10 minutes
(run-at-time t 600 kill-invisible-buffers)

(load "~/.emacs.d/org-indent.el")
(global-org-indent-mode 1)
;(define-key global-map (kbd "RET") 'newline-and-indent)

;(when (not (package-installed-p ...))
;  (package-install ...))
;(require ...)

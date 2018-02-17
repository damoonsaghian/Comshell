;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t inhibit-startup-echo-area-message t)
(add-to-list 'default-frame-alist '(font . "Hack-10"))
(global-visual-line-mode 1)
(global-hl-line-mode 1)

(desktop-save-mode 1)
;(setq-default desktop-restore-eager 5)
;; https://www.emacswiki.org/emacs/DesktopMultipleSaveFiles

(defun kill-invisible-buffers ()
  (dolist (buf  (buffer-list))
    (unless (get-buffer-window buf t) (kill-buffer buf))))
;; kill invisible buffers every 10 minutes
(run-at-time t 600 kill-invisible-buffers)

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


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Invocation.html

;; http://ergoemacs.org/emacs/file_management.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Files.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell-Commands-in-Dired.html
;; https://github.com/thamer/runner
;; in dired-mode pressing "enter":
;; , */projects/* -> swaymsg move scratchpad, move to workspace named "nameofproject", if there is no window named "nameofproject", swaymsg [workspace=focused] kill, load the saved emacs desktop, swaymsg move down
;; , *.m -> open image-dired/movie in a new emacs window
;; , else -> open file in a new emacs window
;; https://github.com/Fuco1/dired-hacks
;; https://emacs.stackexchange.com/questions/12153/does-some-command-exist-which-goes-to-the-next-file-of-the-current-directory
;; next file:
;; , go to tree view
;; , next file
;; , open (in the window at right, go to the first line)

;; split-window-horizontally
;; follow-mode
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Display.html
;; https://github.com/larstvei/Focus
;; https://www.emacswiki.org/emacs/HideShow
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Pages.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Words.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hard-and-Soft-Newlines.html
;; https://github.com/joostkremers/visual-fill-column

;; https://github.com/mrkkrp/modalka
;; https://github.com/jyp/boon
;; http://retroj.net/modal-mode
;; https://github.com/justbur/emacs-which-key

;; https://jblevins.org/projects/markdown-mode/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
;; https://orgmode.org/manual/Tables.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Based-Tables.html

;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
;; https://www.gnu.org/software/auctex
;; https://github.com/aaptel/preview-latex
;; https://github.com/josteink/wsd-mode
;; http://ergoemacs.org/emacs/emacs_view_image_thumbnails.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html
;; https://www.emacswiki.org/emacs/ThumbsMode
;; https://www.gnu.org/software/emms/screenshots.html
;; http://wikemacs.org/wiki/Media_player
;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
;; https://github.com/larsmagne/movie.el
;; https://github.com/dbrock/bongo
;; http://www.mplayerhq.hu/DOCS/tech/slave.txt

;; https://www.emacswiki.org/emacs/CategoryWebBrowser
;; https://en.wikipedia.org/wiki/Eww_(web_browser)
;; https://www.gnu.org/software/emacs/manual/html_node/eww/index.html
;; emacs eww "url-configuration-directory
;; http://ergoemacs.org/emacs/emacs_eww_web_browser.html
;; http://www.wilkesley.org/~ian/xah/emacs/emacs_eww_web_browser.html
;; https://www.gnu.org/software/emacs/manual/html_node/eww/Basics.html
;; cookies and cache location: ~/.emacs.d/url/
;; the location is controlled by the following variables:
;;   url-cookie-file -> default value is "~/.emacs.d/url/cookies"
;;   url-configuration-directory -> default value is "~/.emacs.d/url/"
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/EWW.html
;; make emacs always use its own browser for opening URL links
(setq browse-url-browser-function 'eww-browse-url)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hyperlinking.html
;; swaymsg splitv; chromium --new-window "link_address"
;; swaymsg focus right

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Document-View.html

;; https://www.emacswiki.org/emacs/CategoryShell
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html
;; https://www.gnu.org/software/emacs/manual/html_node/eshell/index.html
;; http://ergoemacs.org/emacs/emacs_unix.html

;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/flycheck/flycheck-rust

(load "~/.emacs.d/org-indent.el")
(global-org-indent-mode 1)
;(define-key global-map (kbd "RET") 'newline-and-indent)

;(when (not (package-installed-p ...))
;  (package-install ...))
;(require ...)

;; (current-time-string nil "Iran")

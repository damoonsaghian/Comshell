(server-start)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq insert-default-directory nil) ;; or use double slash mechanism;
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq window-sides-vertical t)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(cua-mode 1)

;; header line instead of modeline;
(setq-default header-line-format
              '(" "
                mode-line-buffer-identification
                (:propertize (buffer-modified-p "*" " ") 'face '(:foreground "red"))
                " [" mode-line-position "] "
                (vc-mode vc-mode) "  "
                mode-line-modes "  "
                mode-line-misc-info))
(setq-default mode-line-format nil)
;; https://www.emacswiki.org/emacs/HeaderLine#toc2

(defun minibuffer-line-update ()
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer)
    (insert (propertize (format-time-string "%F %a %I:%M%P")
                        'face '(:foreground "#777777")))))
(run-with-timer t 2 #'minibuffer-line-update)
(global-eldoc-mode -1)
;; https://blog.idorobots.org/entries/system-monitor-in-emacs-mode-line.html
;; https://github.com/zk-phi/symon/blob/master/symon.el

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(left-fringe . 2))
(add-to-list 'default-frame-alist '(right-fringe . 2))
(set-face-attribute 'fringe nil :background nil)

(setq scroll-conservatively 200) ;; never recenter point
;; move point to top/bottom of buffer before signaling a scrolling error;
(setq scroll-error-top-bottom t)

(setq blink-cursor-blinks 0)
(setq-default cursor-in-non-selected-windows nil)

(add-to-list 'default-frame-alist '(foreground-color . "#222222"))
(set-face-attribute 'highlight nil :background "#dddddd")
(set-face-attribute 'region nil :background "#dddddd")
(set-face-attribute 'default nil :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; paragraphs
(setq paragraph-start "\n" paragraph-separate "\n")
(defun next-paragraph ()
  (interactive)
  (unless (bobp) (left-char))
  (forward-paragraph)
  (unless (eobp) (progn (forward-paragraph)
                        (redisplay t)
                        (backward-paragraph)
                        (right-char))))
(global-set-key (kbd "C-<down>") 'next-paragraph)
(defun previous-paragraph ()
  (interactive)
  (left-char)
  (backward-paragraph)
  (unless (bobp) (progn (forward-paragraph)
                        (redisplay t)
                        (backward-paragraph)
                        (right-char))))
(global-set-key (kbd "C-<up>") 'previous-paragraph)

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-l -I \"#*#\" -I \"*.lock\" -I \"target\"")
;; auto-save-file-name-transforms
;; "^\\(\\.*\\)\\'\\|^target\\'|\\.lock\\'\\|^\\(\\#*\\)\\#\\'"
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
;; remove first line in dired;
(add-hook 'dired-after-readin-hook
          (lambda () (let ((buffer-read-only))
                       (save-excursion
                         (delete-region (progn (goto-char (point-min)) (point))
                                        (progn (forward-line 1) (point)))))))
;; https://www.emacswiki.org/emacs/DiredView
;; async file operations in dired
;; for copy_paste mechanism:
;;   https://github.com/Fuco1/dired-hacks/blob/master/dired-ranger.el

;; to remember point's places in buffers between sessions,
;;   i'm going to use a solution based on save-place,
;;   but with individual save-place-file for any project;
;; list of buffer groups
;; buffer groups + save and restore
;; to-alist, alist-to-file + timer

(defun show-projects ()
  (let* ((buffer (dired-noselect "~/projects/1"))
         (window (display-buffer-use-some-window buffer nil)))
    (set-window-dedicated-p window t)
    (select-window window)
    (hl-line-highlight))

  ;; to do: automatically mount storage devices when available,
  ;;   and show their "projects" directories in seperate panes (Emacs windows);
  ;; use display-buffer instead of display-buffer-use-some-window for other;
  ;; https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor
  )
(show-projects)

(defun my-find-file ()
  (interactive)
  (let ((file-name (dired-get-filename)))
    (cond
     ((string-match-p "/projects/[^/]*/[^/]*/?\\'" file-name)
      (when (file-directory-p file-name)
        ;; go to the workspace named "file-name";
        ;; then if there is no Emacs frame in the workspace:
        ;; , first close all windows in current workspace,
        ;;   and all workspaces named like this: "1:project_name /*";
        ;; , then run a new instance of Emacs for this project;
        (call-process-shell-command
         (concat
          "i3-msg workspace '\"" file-name "\"'; "
          "if [[ \"$(i3-msg [workspace=__focused__ class=Emacs tiling] mark a)\" "
          "  =~ \"false\" ]]; "
          "then "
          "  i3-msg [workspace=__focused__] kill; "
          "  i3-msg workspace '\"" file-name "\"'; "
          "  emacsclient -c --eval '(goto-project \"" file-name "\")' & fi"))))
     
     ((file-directory-p file-name)
      (cond
       ((string-match-p "\\.m/?\\'" file-name)
        ;; open image-dired/movie in the right window
        ;; http://ergoemacs.org/emacs/emacs_view_image_thumbnails.html
        ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html
        ;; https://www.emacswiki.org/emacs/ThumbsMode
        ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
        ;; https://github.com/larsmagne/movie.el
        ;; http://www.mplayerhq.hu/DOCS/tech/slave.txt
        ;; https://www.gnu.org/software/emms/
        ;; http://wikemacs.org/wiki/Media_player
        ;; https://github.com/dbrock/bongo
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        )
       (t
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        )))

     (t (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))))))
(define-key dired-mode-map [remap dired-find-file] 'my-find-file)
(define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file)

(defun goto-project (project-path)
  (let* ((buffer (dired-noselect project-path))
         (window (display-buffer-in-side-window
                  buffer
                  '((side . left) (window-width . 0.2)))))
    (set-window-dedicated-p window t)
    (select-window window)
    (hl-line-highlight)
    (my-find-file)))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(defun goto-link-at-point ()
  "open the project with the URL under cursor; 
  if the path starts with “http://”, copy it and go to Firefox workspace;"
  (interactive)
  (let (($path (ffap-file-at-point)))
    (if (string-match-p "\\`git://" $path)
        (progn
          (
           ))
      (if (string-match-p "\\`https?://" $path)
          (progn
            (
             ))
        (message "file doesn't exist: '%s';" $path)))))

(require 'package)
(defun require-package (package)
  (unless (require package nil 'noerror)
    (progn
      (unless (assoc package package-archive-contents)
	(package-refresh-contents))
      (package-install package)
      (require package))))
(defun install-package (package)
  (unless (package-installed-p package nil 'noerror)
    (progn
      (unless (assoc package package-archive-contents)
	(package-refresh-contents))
      (package-install package))))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; https://github.com/rranelli/auto-package-update.el/blob/master/auto-package-update.el

;; modal key_bindings
;; https://github.com/mrkkrp/modalka
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
;; in insert mode: Esc -> send key, normal mode;
;; in normal mode:
;;   one key (only received by i3) -> go to projects, send Esc to Emacs
;;     (all Emacs through xdotool);
;;   i-> send key, go to insert mode;
;; firefox-normal-mode:
;;   block all letter keys;
;;   i -> insert mode
;;   workspace back and forth
;;   projects
;; firefox-insert-mode: Esc -> normal mode

;; https://orgmode.org/manual/Tables.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Based-Tables.html
;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
;; https://www.gnu.org/software/auctex
;; https://github.com/aaptel/preview-latex
;; https://github.com/josteink/wsd-mode
;; https://jblevins.org/projects/markdown-mode/

;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/flycheck/flycheck-rust
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm, icicles, icomplete

;; https://github.com/dengste/minimap
;; https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Side-Windows.html
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; https://www.emacswiki.org/emacs/DiredSync
;; http://ergoemacs.org/emacs/emacs_magit-mode_tutorial.html
;;   https://magit.vc/
;;   https://github.com/vermiculus/magithub
;; https://github.com/DarthFennec/highlight-indent-guides
;;   https://github.com/zk-phi/indent-guide
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/index.html
;;   https://www.gnu.org/software/emacs/manual/html_node/message/index.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Gnus.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Sending-Mail.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Rmail.html
;;   https://www.gnu.org/software/emacs/manual/html_node/mh-e/index.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
;; https://www.emacswiki.org/emacs/DictMode
;;   https://github.com/gromnitsky/wordnut
;;   https://www.emacswiki.org/emacs/ThesauriAndSynonyms
;;   https://github.com/atykhonov/google-translate
;; https://www.gnu.org/software/emacs/manual/html_node/calc/index.html
;; https://github.com/domtronn/all-the-icons.el
;; https://www.gnu.org/software/emacs-muse/manual/html_node/Extending-Muse.html#Extending-Muse
;; https://github.com/Fuco1/smartparens
;; http://company-mode.github.io/

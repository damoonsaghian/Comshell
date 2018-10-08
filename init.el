(server-start)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq insert-default-directory nil) ;; alternatively we can use double slash mechanism;
(setq make-backup-files nil)
(setq create-lockfiles nil) ;; because we only use one Emacs instance;
(setq window-sides-vertical t)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(cua-mode 1)

(defun minibuffer-line-update ()
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer)
    (insert (propertize (format-time-string "%F %a %I:%M%P")
                        'face '(:foreground "#777777")))))
(run-with-timer 2 2 #'minibuffer-line-update)
(global-eldoc-mode -1)
;; https://blog.idorobots.org/entries/system-monitor-in-emacs-mode-line.html
;; https://github.com/zk-phi/symon/blob/master/symon.el

;; use double space (instead of tab) for completion;
(defun my-complete ()
  (interactive)
  (if (equal (char-before (point)) ?\s)
      (progn (delete-backward-char 1)
             (minibuffer-complete))
    (insert " ")))
(define-key minibuffer-local-must-match-map (kbd "SPC") 'my-complete)
(define-key minibuffer-local-completion-map (kbd "SPC") 'my-complete)
(define-key minibuffer-local-filename-completion-map (kbd "SPC") 'my-complete)

;; header line instead of modeline;
(setq-default
 header-line-format
 '((:eval (propertize " " 'display '((space :align-to 0))))
   "%b"
   (:eval (if (buffer-modified-p)
              (propertize "* " 'face '(:foreground "red"))
            "  "))
   (:propertize "%30q" face (:foreground "dark cyan"))
   (:propertize ("line:%l  " (vc-mode vc-mode) "  (%m)  " mode-line-misc-info)
                face (:foreground "#777777"))))
(setq-default mode-line-format nil)
(set-face-attribute 'header-line nil
                    :foreground "#222222"
                    :background "#dddddd"
                    :box '(:line-width -1 :color "#aaaaaa"))
(setq Info-use-header-line nil)

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(set-face-attribute 'window-divider nil :foreground "#222222")

(add-to-list 'default-frame-alist '(left-fringe . 3))
(add-to-list 'default-frame-alist '(right-fringe . 3))
(set-face-attribute 'fringe nil :background 'unspecified :foreground "red")
(define-fringe-bitmap 'right-arrow
  [#b01100000
   #b01100000
   #b00000000
   #b01100000
   #b01100000
   #b00000000
   #b01100000
   #b01100000])
(define-fringe-bitmap 'left-arrow
  [#b01100000
   #b01100000
   #b00000000
   #b01100000
   #b01100000
   #b00000000
   #b01100000
   #b01100000])

(scroll-bar-mode -1)
(setq scroll-conservatively 200) ;; never recenter point
;; move point to top/bottom of buffer before signaling a scrolling error;
(setq scroll-error-top-bottom t)
pixel-scroll-mode

(setq blink-cursor-blinks 0)
(setq-default cursor-in-non-selected-windows nil)

(add-to-list 'default-frame-alist '(foreground-color . "#222222"))
(set-face-attribute 'highlight nil :background "LightBlue1")
(set-face-attribute 'region nil :background "LightBlue1")
(set-face-attribute 'default nil :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

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

(save-place-mode 1)
(run-with-idle-timer 30 30 #'save-place-alist-to-file)
;; todo: separate save-place-file for each project;
;; list of buffer groups
;; save-place-to-alist, save-place-alist-to-file

;; show projects
(let* ((buffer (dired-noselect "~/projects/1"))
       (window (display-buffer-use-some-window buffer nil)))
  (set-window-dedicated-p window t)
  (select-window window)
  (hl-line-highlight))
;; to do: automatically mount storage devices when available,
;;   and show their "projects/*" directories in seperate panes (Emacs windows);
;; note: use display-buffer instead of display-buffer-use-some-window for these new panes;
;; https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor

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
(require-package 'modalka)
(define-minor-mode my-modalka-mode
  nil nil nil nil
  (if my-modalka-mode (modalka-mode 1) (modalka-mode -1))
  (setq-local cursor-type (if my-modalka-mode 'box 'hollow)))

;; changed the definition of "modalka--maybe-activate" such that
;;   "my-modalka-global-mode" activates modalka, only in "shell-mode",
;;   and modes derived from "text-mode" or "prog-mode";
(defun my-modalka--maybe-activate ()
  (unless (or (minibufferp)
              (and
               (not (derived-mode-p 'text-mode 'prog-mode))
               (not (equal major-mode 'shell-mode))))
    (my-modalka-mode 1)))
(define-globalized-minor-mode my-modalka-global-mode
  my-modalka-mode
  my-modalka--maybe-activate)

(define-key modalka-mode-map (kbd "RET")
  (lambda ()
    (interactive)
    (my-modalka-global-mode -1)))
;;(modalka-define-kbd "f" "C-f")
;;(modalka-define-kbd "b" "C-b")
;;(modalka-define-kbd "n" "C-n")
;;(modalka-define-kbd "p" "C-p")
;;(modalka-define-kbd "a" "C-a")
;;(modalka-define-kbd "e" "C-e")
;;(modalka-define-kbd "m" "C-SPC")
;;(modalka-define-kbd "<escape>" "C-g")
;;(modalka-define-kbd "<tab>" "C-g")
(my-modalka-global-mode 1)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm, icicles, icomplete

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

;; https://github.com/dengste/minimap
;; https://github.com/Fuco1/smartparens
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
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
;; https://www.emacswiki.org/emacs/DictMode
;;   https://github.com/gromnitsky/wordnut
;;   https://www.emacswiki.org/emacs/ThesauriAndSynonyms
;;   https://github.com/atykhonov/google-translate
;; https://www.gnu.org/software/emacs/manual/html_node/calc/index.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html
;; https://www.gnu.org/software/emacs-muse/manual/html_node/Extending-Muse.html#Extending-Muse
;; http://company-mode.github.io/
;; https://github.com/domtronn/all-the-icons.el

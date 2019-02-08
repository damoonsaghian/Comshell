(menu-bar-mode -1)
(tool-bar-mode -1)
;; to introduce this frame as the main Emacs frame, to the window manager,
;;   so it doesn't make it floating, like other frames;
(set-frame-name "comshell")
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq insert-default-directory nil) ;; alternatively we can use double slash mechanism;
(setq make-backup-files nil)
;; (setq create-lockfiles nil) ;; because we only use one Emacs instance;
(setq window-sides-vertical t)
(setq blink-cursor-blinks 0)
(setq-default cursor-in-non-selected-windows nil)
(setq-default mode-line-format nil)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(cua-mode 1)
;; automatically recover unsaved files;

(defun sleep ()
  (interactive)
  (call-process-shell-command "sleep 0.1; systemctl suspend"))

(defun minibuffer-line-update ()
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer)
    (insert (propertize (format-time-string "%F %a %I:%M%P")
                        'face '(:foreground "#777777")))))
(run-with-timer 2 2 #'minibuffer-line-update)
(global-eldoc-mode -1)
;; https://github.com/manateelazycat/awesome-tray
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Misc.html
;; https://emacs.stackexchange.com/questions/27767/center-text-in-minibuffer-echo-area
;; insert text in minibuffer using overlays:
;;   (let ((ol (make-overlay (point-min) (point-min))))
;;     (overlay-put ol 'before-string (format "%s\n" myresults)))
;; https://blog.idorobots.org/entries/system-monitor-in-emacs-mode-line.html
;; https://github.com/zk-phi/symon/blob/master/symon.el

;; use double space (instead of tab) for completion;
(defun my-complete ()
  (interactive)
  (if (equal (char-before (point)) ?\s)
      (progn (delete-backward-char 1)
             (minibuffer-complete))
    (insert " ")))
(define-key minibuffer-local-filename-must-match-map (kbd "SPC") 'my-complete)
(define-key minibuffer-local-filename-completion-map (kbd "SPC") 'my-complete)
(require 'shell)
(defun my-shell-complete ()
  (interactive)
  (if (equal (char-before (point)) ?\s)
      (progn (delete-backward-char 1)
             (completion-at-point))
    (insert " ")))
(define-key minibuffer-local-shell-command-map (kbd "SPC") 'my-shell-complete)
(define-key shell-mode-map (kbd "SPC") 'my-shell-complete)
;; todo: code completion;

(setq-default
 header-line-format
 '((:eval (propertize " " 'display '((space :align-to 0))))
   (:eval (replace-regexp-in-string "<.*>" "" (buffer-name)))
   (:eval (if (buffer-modified-p)
              (propertize "* " 'face '(:foreground "red"))
            "  "))
   (:eval (if (and (equal (window-start) (point-min)) (equal (window-end) (point-max)))
              nil
            (propertize "%q" 'face '(:foreground "dark cyan"))))))
(set-face-attribute 'header-line nil :foreground "#222222" :background "#dddddd")
  ;; :box '(:line-width -1 :color "#aaaaaa"))
(setq Info-use-header-line nil)
(setq-default mode-line-format nil)

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(set-face-attribute 'window-divider nil :foreground "#555555")

(scroll-bar-mode -1)
(setq scroll-conservatively 200) ;; never recenter point
;; move point to top/bottom of buffer before signaling a scrolling error;
(setq scroll-error-top-bottom t)
;; pixel-scroll-mode

(add-to-list 'default-frame-alist '(left-fringe . 3))
(add-to-list 'default-frame-alist '(right-fringe . 3))
(set-face-attribute 'fringe nil :background 'unspecified :foreground "red")
(define-fringe-bitmap 'right-arrow
  [#b01100000 #b01100000 #b00000000 #b01100000 #b01100000 #b00000000 #b01100000 #b01100000])
(define-fringe-bitmap 'left-arrow
  [#b11000000 #b11000000 #b00000000 #b11000000 #b11000000 #b00000000 #b11000000 #b11000000])

(add-to-list 'default-frame-alist '(foreground-color . "#222222"))
(set-face-attribute 'highlight nil :background "#CCFFFF")
(set-face-attribute 'region nil :background "#CCFFFF")
(set-face-attribute 'default nil :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

;; move between paragraphs;
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

(save-place-mode 1)
(run-with-idle-timer
 30 30 (lambda ()
         (let ((inhibit-message t))
           (save-places-to-alist)
           (if save-place-loaded
               (save-place-alist-to-file)))))
;; to_do: separate save-place-file for each project;
;; list of buffer groups
;; save-place-to-alist, save-place-alist-to-file

(require 'dired)
(require 'hl-line)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-l -I \"#*#\" -I \"*.lock\" -I \"target\"")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook (lambda () (setq hl-line-mode t)))
;; https://www.emacswiki.org/emacs/DiredView
;; async file operations in dired
;; for copy_paste mechanism:
;;   https://github.com/Fuco1/dired-hacks/blob/master/dired-ranger.el
;; sort numbers (10 after 9)

;; to_do: implement "dir-tree", and use that instead of "dired";

;; when moving between windows, send point to highlighted line (if there is any);
(defun my-other-window ()
  (interactive)
  (if hl-line-overlay (goto-char (overlay-start hl-line-overlay)))
  (other-window 1))
(global-set-key (kbd "M-SPC") #'my-other-window)

(defvar projects-window)
(defun show-projects ()
  (interactive)
  (let ((buffer (dired-noselect "~/projects/")))
    (setq projects-window (display-buffer-in-side-window
                           buffer
                           '((side . left) (slot . 1) (window-width . 0.2))))
    (set-window-parameter projects-window 'no-delete-other-windows t)
    (set-window-dedicated-p projects-window t) ;; not sure if this is necessary;
    (select-window projects-window)
    (hl-line-highlight)))
(global-set-key (kbd "M-RET") #'show-projects)

(defun my-find-file ()
  (interactive)
  (let ((file-name (dired-get-filename)))
    (cond
     ((string-match-p "/projects/[^/]*/?\\'" file-name)
      (when (file-directory-p file-name)
          (let* ((buffer (dired-noselect file-name))
                 (window (display-buffer-in-side-window
                          buffer
                          '((side . left) (slot . 0) (window-width . 0.2)))))
            (set-window-parameter window 'no-delete-other-windows t)
            (set-window-dedicated-p window t)
            (select-window window)
            (hl-line-highlight)
            (ignore-errors (delete-window projects-window))
            (my-find-file))))

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
        (hl-line-highlight)
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        (delete-other-windows)
        )
       (t
        (hl-line-highlight)
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        (delete-other-windows)
        )))

     (t
      (hl-line-highlight)
      (select-window
       (display-buffer-use-some-window (find-file-noselect file-name) nil))
      (delete-other-windows)))))
(define-key dired-mode-map [remap dired-find-file] 'my-find-file)
(define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file)

(add-hook 'emacs-startup-hook (lambda () (show-projects) (my-find-file) (show-projects)))

(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
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

(require 'server)
(unless (server-running-p) (server-start))

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

(require-package 'rust-mode)
(setq rust-indent-offset 2)

;; modal key_bindings
;; https://github.com/mrkkrp/modalka
;(require-package 'modalka)
;(defun modal-buffer-p ()
;  (or (derived-mode-p 'text-mode 'prog-mode 'conf-mode)
;      (equal major-mode 'shell-mode)))
;(defun modalka--maybe-activate ()
;  (if (modal-buffer-p) (modalka-mode 1)))
;(add-hook 'modalka-mode-hook (lambda () (set-cursor-color "black")))
;(add-hook 'buffer-list-update-hook
;          (lambda ()
;            (if (with-current-buffer (window-buffer (selected-window))
;                  (and (not modalka-mode) (modal-buffer-p)))
;                (set-cursor-color "red")
;              (set-cursor-color "black"))))
; 
;(global-set-key (kbd "<escape>") (lambda () (interactive) (modalka--maybe-activate)))
;(global-set-key (kbd "<tab>") (lambda () (interactive) (modalka--maybe-activate)))
;(define-key modalka-mode-map (kbd "RET")
;  (lambda ()
;    (interactive)
;    (modalka-mode -1)
;    (set-cursor-color "red")))

;; https://stackoverflow.com/questions/19757612/how-to-redefine-a-key-inside-a-minibuffer-mode-map
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequences.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Sequence-Input.html
;; https://www.emacswiki.org/emacs/KeySequence

;; modalka-define-kbd is only for global keybindings
;; local keybindings must be defined for each mode separately;
;(modalka-define-kbd "a" "C-a") ;; move-beginning-of-line
;(modalka-define-kbd "b" "C-b") ;; backward-char
;(modalka-define-kbd "c" "C-c") ;;
;(modalka-define-kbd "e" "C-e") ;; move-end-of-line
;(modalka-define-kbd "f" "C-f") ;; forward-char
;(modalka-define-kbd "g" "C-g") ;;
;(modalka-define-kbd "h" "C-h") ;;
;(modalka-define-kbd "i" "C-i") ;; * indent-for-tab-command
;(modalka-define-kbd "j" "C-j") ;; * electric-newline-and-maybe-indent
;(modalka-define-kbd "k" "C-k") ;; * kill-line
;(modalka-define-kbd "l" "C-l") ;; * recenter-top-bottom
;(modalka-define-kbd "m" "C-SPC") ;; cua-set-mark
;(modalka-define-kbd "n" "C-n") ;; next-line
;(modalka-define-kbd "o" "C-o") ;; * open-line
;(modalka-define-kbd "p" "C-p") ;; previous-line
;(modalka-define-kbd "q" "C-q") ;; * quoted-insert
;(modalka-define-kbd "r" "C-r") ;; isearch-repeat-backward
;(modalka-define-kbd "s" "C-s") ;; isearch-forward
;(modalka-define-kbd "t" "C-t") ;; * transpose-char
;(modalka-define-kbd "u" "C-u") ;; universal-argument
;(modalka-define-kbd "v" "C-v") ;; cua-paste
;(modalka-define-kbd "w" "C-w") ;; * kill-region
;(modalka-define-kbd "x" "C-x") ;; 
;(modalka-define-kbd "y" "C-y") ;; * cua-paste
;(modalka-define-kbd "z" "C-z") ;; undo
;(modalka-define-kbd "1" "C-1") ;; (digit-argument 1)
;(modalka-define-kbd "2" "C-2")
;(modalka-define-kbd "3" "C-3")
;(modalka-define-kbd "4" "C-4")
;(modalka-define-kbd "5" "C-5")
;(modalka-define-kbd "6" "C-6")
;(modalka-define-kbd "7" "C-7")
;(modalka-define-kbd "8" "C-8")
;(modalka-define-kbd "9" "C-9")
;(modalka-define-kbd "0" "C-0")
;(modalka-define-kbd "," "C-,")
;(modalka-define-kbd "_" "C-_") ;; * undo
;(modalka-global-mode 1)

;; /sudo::/...
;; /root@localhost:/...

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

;; automatically mount storage devices when available,
;;   https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor
;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/flycheck/flycheck-rust
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/

;; https://iqss.github.io/IQSS.emacs/init.html
;; https://github.com/dengste/minimap
;; https://github.com/Fuco1/smartparens
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; https://www.emacswiki.org/emacs/DiredSync
;; managing window configurations (probably not a good idea):
;;   https://github.com/wasamasa/eyebrowse
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
;; https://github.com/Malabarba/beacon

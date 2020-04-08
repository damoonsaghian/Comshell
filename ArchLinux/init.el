(menu-bar-mode -1)
(tool-bar-mode -1)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
;;(setq insert-default-directory nil) ;; alternatively we can use double slash mechanism;
(setq-default major-mode 'text-mode)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(cua-mode 1)
(setq window-sides-vertical t)
;; after deleting a window kill its buffer if it doesn't have any other window;

(setq make-backup-files nil)
;; automatically recover unsaved files;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recover.html
;; https://www.emacswiki.org/emacs/AutoSave#toc1
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Files.html
;; auto-save-file-name-transforms
;; "^\\(\\.*\\)\\'\\|^target\\'|\\.lock\\'\\|^\\(\\#*\\)\\#\\'"
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html
;; http://www.rexegg.com/

;; header line instead of modeline;
(setq-default mode-line-format nil)
(setq Info-use-header-line nil)
(setq-default
 header-line-format
 '((:eval (propertize " " 'display '((space :align-to 0))))
   (:eval (replace-regexp-in-string "<.*>" "" (buffer-name)))
   (:eval (if (buffer-modified-p)
              (propertize "* " 'face '(:foreground "red"))
            "  "))
   ;;(:propertize "%30q" face (:foreground "dark cyan"))
   (:eval (if (and (equal (window-start) (point-min)) (equal (window-end) (point-max)))
              nil
            (propertize "%q" 'face '(:foreground "dark cyan"))))))
   "                                           "
   mode-line-misc-info))
(set-face-attribute 'header-line nil :foreground "#222222" :background "#dddddd")

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(set-face-attribute 'window-divider nil :foreground "#555555")
(scroll-bar-mode -1)

;; never recenter point
(setq scroll-conservatively 101)
;; move point to top/bottom of buffer before signaling a scrolling error;
(setq scroll-error-top-bottom t)

;; paragraphs
(setq paragraph-start "\n" paragraph-separate "\n")
(defun next-paragraph ()
  (interactive)
  (unless (bobp) (left-char))
  (forward-paragraph)
  (unless (eobp)
    (forward-paragraph)
    (redisplay t)
    (backward-paragraph)
    (right-char)))
(global-set-key (kbd "C-<down>") 'next-paragraph)
(defun previous-paragraph ()
  (interactive)
  (left-char)
  (backward-paragraph)
  (unless (bobp)
    (forward-paragraph)
    (redisplay t)
    (backward-paragraph)
    (right-char)))
(global-set-key (kbd "C-<up>") 'previous-paragraph)

(add-to-list 'default-frame-alist '(foreground-color . "#222222"))
(set-face-attribute 'highlight nil :background "LightBlue1") ;;"#CCFFFF"
(set-face-attribute 'region nil :background "LightBlue1")
(set-face-attribute 'default nil :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")

(setq blink-cursor-blinks 0)
(setq-default cursor-in-non-selected-windows nil)
;; https://github.com/Malabarba/beacon

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(defun goto-link-at-point ()
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

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-l -I \".#*\" -I \"#*#\" -I \"*.lock\" -I \"target\"")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; remove first line in dired;
(add-hook 'dired-after-readin-hook
          (lambda () (let ((buffer-read-only))
                       (save-excursion
                         (delete-region (progn (goto-char (point-min)) (point))
                                        (progn (forward-line 1) (point)))))))
;; make the first line of dired, invisible;
;;(add-hook
;; 'dired-after-readin-hook
;; (lambda ()
;;   (let ((buffer-read-only))
;;     (save-excursion
;;       (set-text-properties 1 (progn (goto-char 1) (forward-line 1) (point))
;;                            '(invisible t))))))
;; auto refresh dired, but be quiet about it;
(add-hook 'dired-mode-hook 'auto-revert-mode) ;;(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; https://github.com/Fuco1/dired-hacks#dired-open
;; https://www.emacswiki.org/emacs/DiredView
;; async file operations in dired
;; https://github.com/jwiegley/emacs-async
;; https://truongtx.me/tmtxt-dired-async.html
;; https://github.com/jwiegley/emacs-async/blob/master/dired-async.el
;; https://oremacs.com/2016/02/24/dired-rsync/
;; for copy_paste mechanism:
;;   https://github.com/Fuco1/dired-hacks/blob/master/dired-ranger.el
;; sort numbers (10 after 9)
;;   https://emacs.stackexchange.com/questions/5649/sort-file-names-numbered-in-dired
;; show state of files (modified or not, git) in dired using marks;
;;   https://github.com/syohex/emacs-dired-k
;;   https://github.com/dgutov/diff-hl
;;   https://emacs.stackexchange.com/questions/9503/how-can-i-visualize-vcs-status-in-dired
;;   show branch in the header of side window;

(require 'hl-line)
(add-hook 'dired-mode-hook 'hl-line-mode)
;;(add-hook 'dired-mode-hook (lambda () (setq hl-line-mode t)))

;; when moving between windows, send point to highlighted line (if there is any);
(defun my-other-window ()
  (interactive)
  (if hl-line-overlay (goto-char (overlay-start hl-line-overlay)))
  (other-window 1))
(global-set-key (kbd "C-TAB") #'my-other-window)

(defun project-side-window-open ()
  (let* ((buffer (dired-noselect project-path))
         (window (display-buffer-in-side-window
                  buffer
                  '((side . left) (window-width . 0.2) (no-delete-other-windows . t)))))
    (set-window-dedicated-p window t)
    (select-window window)
    (hl-line-highlight)
    ;; send a message to all servers except "/", to hide their frame;
    ))

(defun project-initial-view (project-path)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  )

(defun project-open (project-dir)
  (setq-default server-name project-dir)
  (server-start)

  (require 'desktop)
  (let ((desktop-file-dir-path (expand-file-name ".cache/" project-dir))
        (desktop-file-path (expand-file-name ".cache/.emacs.desktop" project-dir)))
    (unless (file-exists-p desktop-file-dir-path)
      (make-directory desktop-file-dir-path t))
    (unless (file-exists-p desktop-file-path)
      (desktop-save desktop-file-dir-path))
    (setq desktop-path (list desktop-file-dir-path)
          desktop-restore-frames nil
          desktop-load-locked-desktop t)
    (desktop-save-mode 1)
    (desktop-read desktop-file-dir-path))
  (setq desktop-restore-eager 5)
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
  ;; https://stackoverflow.com/questions/4477376/some-emacs-desktop-save-questions-how-to-change-it-to-save-in-emacs-d-emacs
  (setq-default desktop-base-file-name "emacs.desktop")
  (setq-default desktop-path (expand-file ".cache/" project-dir))
  (setq-default desktop-restore-eager 5)
  (setq-default desktop-load-locked-desktop t)
  (desktop-save-mode 1)

  (when nil ;; first window's buffer is not project-dir
    (project-initial-view)))

(defun project-activate (project-dir)
  (call-process-shell-command
   (concat
    "emacsclient --socket-name \""
    project-dir
    "\" --eval '(select-frame-set-input-focus (selected-frame))'"
    " || "
    "emacs --eval '(project-open \"" project-dir "\")'"))

  ;; send point to highlighted line (if there is any);
  (if hl-line-overlay (goto-char (overlay-start hl-line-overlay))))

(defun my-find-file ()
  (interactive)
  (let ((file-name (dired-get-filename)))
    (cond
     ((string-match-p "/projects/[^/]*/?\\'" file-name)
      (when (file-directory-p file-name)
        (project-activate file-name)))

     ((file-directory-p file-name)
      (cond
       ((file-exists-p (expand-file file-name ".gallery"))
        ;; open media browser in the right window
        ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
        ;; https://github.com/larsmagne/movie.el
        ;; http://ergoemacs.org/emacs/emacs_view_image_thumbnails.html
        ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html
        ;; mpv
        ;; https://www.gnu.org/software/emms/
        ;; http://wikemacs.org/wiki/Media_player
        ;; https://github.com/dbrock/bongo
        )
       ((file-exists-p (expand-file file-name ".media"))
        ;; view in overlay;
        )
       (t
        (hl-line-highlight)
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        (delete-other-windows))))

     ((string-match-p "\\.jpg/?\\'" file-name)
      ;; view image;
      )

     (t (hl-line-highlight)
      (select-window
       (display-buffer-use-some-window (find-file-noselect file-name) nil))
      (delete-other-windows))
     )))

(defun projects-list-find-file ()
  (interactive)
  (when (file-directory-p (dired-get-filename))
    (project-activate file-name)))

(if (eq command-line-args '("emacs"))
    (progn
      (define-key dired-mode-map [remap dired-find-file] 'projects-list-find-file)
      (define-key dired-mode-map [remap dired-find-file-other-window] 'projects-list-find-file))
  (define-key dired-mode-map [remap dired-find-file] 'my-find-file)
  (define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file))

(defun projects-list-create ()
  (let* ((buffer (dired-noselect "~/projects"))
         (window (display-buffer-use-some-window buffer nil)))
    (set-window-dedicated-p window t)
    (select-window window))

    (local-set-key (kbd "<escape>") #'my-other-window)

  ;; to do: automatically find all "projects/*" directories in connected storage devices,
  ;;   and create an eyebrowse window for each;
  )
(defun projects-list-activate ()
  (interactive)
  (call-process-shell-command
    (concat
      "emacsclient --socket-name / --eval '(select-frame-set-input-focus (selected-frame))'"
      " || emacs")))
(global-set-key (kbd "M-RET") 'projects-list-activate)
(when (eq command-line-args '("emacs"))
  (add-hook 'emacs-startup-hook (lambda () (projects-list-create)))
  (setq server-name "/")
  (server-start))

(require 'package)
(package-initialize)
(defun require-package (package)
  (unless (require package nil 'noerror)
    (unless (assoc package package-archive-contents)
	    (package-refresh-contents))
    (package-install package)
    (require package)))
(defun install-package (package)
  (unless (package-installed-p package nil 'noerror)
    (unless (assoc package package-archive-contents)
	     (package-refresh-contents))
    (package-install package)))
(add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/") t)
;; https://github.com/rranelli/auto-package-update.el/blob/master/auto-package-update.el
;; https://emacs.stackexchange.com/questions/38206/upgrading-packages-automatically

;; https://github.com/jwiegley/use-package
;; https://github.com/SidharthArya/emacsit
;; https://www.reddit.com/r/emacs/comments/bwptua/a_simple_package_manager_for_emacs/
;; https://www.reddit.com/r/emacs/comments/5v8n87/emacs_is_often_touted_for_its_concurrency_over/
;; https://www.reddit.com/r/emacs/comments/9drkhm/is_it_able_to_manage_emacs_packages_using_cli/
;; https://www.reddit.com/r/emacs/comments/acvn2l/elisp_script_to_install_all_packages_very_fast/
;; https://emacs.stackexchange.com/questions/34180/how-can-i-script-emacs-to-install-packages-from-list

;; https://github.com/wasamasa/eyebrowse

(defun project-tree-show ()
  ()
  (treemacs-find-file )
  )
(with-eval-after-load 'treemacs
  (project-tree-show)
  (on-window-buffer-focus (lambda (buffer)
    (buffer-file-name buffer)
    (project-tree-show))))

;; modal key_bindings
;; https://github.com/mrkkrp/modalka
(require-package 'modalka)
(add-hook 'modalka-mode-hook (lambda ()
                               (set-cursor-color "black")
                               (call-process-shell-command "i3-msg mode default")))
(define-key modalka-mode-map (kbd "RET")
  (lambda ()
    (interactive)
    (modalka-global-mode -1)
    (set-cursor-color "red")
    (call-process-shell-command "i3-msg mode insert")))

;;(modalka-define-kbd "f" "C-f")
;;(modalka-define-kbd "b" "C-b")
;;(modalka-define-kbd "n" "C-n")
;;(modalka-define-kbd "p" "C-p")
;;(modalka-define-kbd "a" "C-a")
;;(modalka-define-kbd "e" "C-e")
;;(modalka-define-kbd "m" "C-SPC")
;;(modalka-define-kbd "<escape>" "C-g")
;;(modalka-define-kbd "<tab>" "C-g")
;;(define-key modalka-mode-map (kbd "x f")
;;  (lambda () (interactive) (call-process-shell-command "i3-msg workspace /Firefox/;
;;    if [[ \"$(i3-msg [workspace=__focused__ class=Firefox] focus)\" =~ \"false\" ]];
;;    then i3-msg 'workspace /Firefox/; exec firefox'; fi")))

(add-to-list 'modalka-excluded-modes 'dired-mode)
(add-to-list 'modalka-excluded-modes 'help-mode)
(add-to-list 'modalka-excluded-modes 'Info-mode)
;;(modalka-global-mode 1)

;; https://explog.in/dot/emacs/config.html
;; https://iqss.github.io/IQSS.emacs/init.html
;; https://www.spacemacs.org/layers/LAYERS.html
;; https://www.reddit.com/r/rust/comments/a3da5g/my_entire_emacs_config_for_rust_in_fewer_than_20/

;; https://www.emacswiki.org/emacs/Dedicated_Minibuffer_Frame
;; https://stackoverflow.com/questions/3050011/is-it-possible-to-move-the-emacs-minibuffer-to-the-top-of-the-screen
;; https://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
;; https://emacs.stackexchange.com/questions/1074/how-to-display-the-content-of-minibuffer-in-the-middle-of-the-emacs-frame
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffers-and-Frames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Visibility-of-Frames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Child-Frames.html#Child-Frames

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html

;; https://orgmode.org/manual/Tables.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Based-Tables.html
;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
;; https://www.gnu.org/software/auctex
;; https://github.com/aaptel/preview-latex
;; https://github.com/josteink/wsd-mode
;; https://jblevins.org/projects/markdown-mode/

;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/flycheck/flycheck-rust
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm, icicles, icomplete

;; /sudo::/...
;; /root@localhost:/

;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
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

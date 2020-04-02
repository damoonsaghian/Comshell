(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)
;;(setq insert-default-directory nil) ;; or use double slash mechanism;
(setq make-backup-files nil)
;;(setq create-lockfiles nil)
(setq window-sides-vertical t)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(cua-mode 1)

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
(scroll-bar-mode -1)

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

;; never recenter point
(setq scroll-conservatively 200)
;; move point to top/bottom of buffer before signaling a scrolling error;
(setq scroll-error-top-bottom t)

(setq blink-cursor-blinks 0)
;;(setq-default cursor-in-non-selected-windows nil)

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

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-l -I \"#*#\" -I \"*.lock\" -I \"target\"")
;; auto-save-file-name-transforms
;; "^\\(\\.*\\)\\'\\|^target\\'|\\.lock\\'\\|^\\(\\#*\\)\\#\\'"
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;;(add-hook 'dired-mode-hook 'hl-line-mode)
;; remove first line in dired;
(add-hook 'dired-after-readin-hook
          (lambda () (let ((buffer-read-only))
                       (save-excursion
                         (delete-region (progn (goto-char (point-min)) (point))
                                        (progn (forward-line 1) (point)))))))
;; https://www.emacswiki.org/emacs/DiredView
;; async file operations in dired
;; https://github.com/jwiegley/emacs-async
;; https://truongtx.me/tmtxt-dired-async.html
;; https://oremacs.com/2016/02/24/dired-rsync/
;; for copy_paste mechanism:
;;   https://github.com/Fuco1/dired-hacks/blob/master/dired-ranger.el

(defun project-tree ()
  ;; load treemacs asynchronously;
  )

(defun open-project (project-dir)
  (setq-default server-name project-dir)
  (server-start)

  (setq-default desktop-base-file-name "emacs.desktop")
  (setq-default desktop-path (expand-file ".cache/" project-dir))
  (setq-default desktop-restore-eager 5)
  (setq-default desktop-load-locked-desktop t)
  (desktop-save-mode 1)

  (project-tree))

(defun goto-project (project-dir)
  (call-process-shell-command
   (concat
    "emacsclient --socket-name \""
    project-dir
    "\" --eval '(select-frame-set-input-focus (selected-frame))'"
    " || "
    "emacs --eval '(open-project \"" project-dir "\")'")))

(defun my-find-file ()
  (interactive)
  (let ((file-name (dired-get-filename)))
    (cond
     ((string-match-p "/projects/[^/]*/[^/]*/?\\'" file-name)
      (when (file-directory-p file-name)
        (goto-project file-name)))

     ((string-match-p "\\.i/?\\'" file-name)
      (when (file-directory-p file-name)
        ;; view images;
        (find-file))
      )

     (t (find-file)))))
(define-key dired-mode-map [remap dired-find-file] 'my-find-file)
;;(define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file)

(defun create-projects-window ()
  (let* ((buffer (dired-noselect "~/projects"))
         (window (display-buffer-use-some-window buffer nil)))
    (set-window-dedicated-p window t)
    (select-window window))

  ;; to do: automatically find all "projects/*" directories in connected storage devices,
  ;;   and create an eyebrowse window for each;
  )
(defun show-projects ()
  (interactive)
  (call-process-shell-command
    (concat
      "emacsclient --socket-name / --eval '(select-frame-set-input-focus (selected-frame))'"
      " || emacs")))
(global-set-key (kbd "M-RET") 'show-projects)
(when (eq command-line-args '("emacs"))
  (create-projects-window)
  (setq server-name "/")
  (server-start))

(require 'package)
(package-initialize)
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
;; https://github.com/rranelli/auto-package-update.el/blob/master/auto-package-update.el
;; https://emacs.stackexchange.com/questions/38206/upgrading-packages-automatically

;; https://github.com/jwiegley/use-package
;; https://github.com/SidharthArya/emacsit
;; https://www.reddit.com/r/emacs/comments/bwptua/a_simple_package_manager_for_emacs/
;; https://www.reddit.com/r/emacs/comments/5v8n87/emacs_is_often_touted_for_its_concurrency_over/
;; https://www.reddit.com/r/emacs/comments/9drkhm/is_it_able_to_manage_emacs_packages_using_cli/
;; https://www.reddit.com/r/emacs/comments/acvn2l/elisp_script_to_install_all_packages_very_fast/
;; https://emacs.stackexchange.com/questions/34180/how-can-i-script-emacs-to-install-packages-from-list

(require-package 'eyebrowse)
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
;;((file-directory-p file-name)
;; (cond
;;  ((file-exists-p (expand-file file-name ".gallery"))
;;   ;; open media browser in the right window
;;   ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
;;   ;; https://github.com/larsmagne/movie.el
;;   ;; http://ergoemacs.org/emacs/emacs_view_image_thumbnails.html
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html
;;   ;; mpv
;;   ;; https://www.gnu.org/software/emms/
;;   ;; http://wikemacs.org/wiki/Media_player
;;   ;; https://github.com/dbrock/bongo
;;   )
;;  (t
;;   ;; unfold
;;   )))

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

;; https://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Visibility-of-Frames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Child-Frames.html#Child-Frames

;; https://www.spacemacs.org/layers/LAYERS.html

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
;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/flycheck/flycheck-rust
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm, icicles, icomplete

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

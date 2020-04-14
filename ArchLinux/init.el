(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)
(setq visible-bell t)
;(setq insert-default-directory nil) ;; alternatively we can use double slash mechanism;
(setq-default major-mode 'text-mode)
(cua-mode 1)
(setq window-sides-vertical t)
(global-set-key (kbd "C-x k") (lambda () (interactive) (quit-window t)))
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (other-window -1)))
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

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(set-face-attribute 'window-divider nil :foreground "#555555")

(scroll-bar-mode -1)
(setq-default indicate-buffer-boundaries '((up . left) (down . left)))

(setq-default mode-line-format nil)

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
(set-face-attribute 'highlight nil :background "#CCFFFF")
(set-face-attribute 'region nil :background "#CCFFFF")
(set-face-attribute 'default nil :family "Monospace" :height 105)
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
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-keep-marker-rename nil
      dired-keep-marker-copy nil)
(setq dired-listing-switches "-lv -I \"#*#\" -I \"*.lock\" -I \"target\"")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; only move between lines containing a file;
(define-key dired-mode-map [remap next-line] 'dired-next-line)
(define-key dired-mode-map [remap previous-line] 'dired-previous-line)
;; make the first line invisible;
(add-hook 'dired-after-readin-hook (lambda ()
  (let ((inhibit-read-only t))
    (save-excursion
      (set-text-properties
        1
        (progn (goto-char 1) (forward-line 1) (point))
        '(invisible t))))))

(require 'hl-line)
(add-hook 'dired-mode-hook (lambda ()
  (setq hl-line-mode t)
  (when (string-match-p "/projects/[^/]*/?\\'" default-directory) ;; alt: dired-directory
    (setq header-line-format
          '((:eval (propertize " " 'display '((space :align-to 0))))
            (:eval (replace-regexp-in-string "<.*>" "" (buffer-name)))
            " "
            mode-line-misc-info)))))
(add-hook 'window-configuration-change-hook 'hl-line-highlight)

;; https://www.emacswiki.org/emacs/DiredView
;; for copy_paste mechanism:
;;   https://github.com/Fuco1/dired-hacks/blob/master/dired-ranger.el
;; async file operations in dired
;;   https://github.com/jwiegley/emacs-async
;;   https://truongtx.me/tmtxt-dired-async.html
;;   https://github.com/jwiegley/emacs-async/blob/master/dired-async.el
;; https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-icons-dired.el
;;   https://github.com/domtronn/all-the-icons.el
;;   https://github.com/sebastiencs/icons-in-terminal
;; show state of files (modified or not, git) in dired using marks;
;;   https://github.com/syohex/emacs-dired-k
;;   https://github.com/dgutov/diff-hl
;;   https://emacs.stackexchange.com/questions/9503/how-can-i-visualize-vcs-status-in-dired
;; https://oremacs.com/2016/02/24/dired-rsync/
;; https://github.com/Fuco1/dired-hacks#dired-open

;; to store/restore side windows;
(add-to-list 'window-persistent-parameters '(window-side . writable))
(add-to-list 'window-persistent-parameters '(window-slot . writable))

(defun project-directory-side-window (project-dir)
  (interactive)
  (let* ((buffer (dired-noselect project-dir))
         (window (display-buffer-in-side-window
                  buffer
                  '((side . left) (slot . 0)))))
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-parameter window 'window-width 0.2)
    (select-window window)))

(defun project-open (project-dir)
  (let ((project-cache-dir (expand-file-name ".cache/" project-dir)))
    (unless (file-exists-p project-cache-dir)
      (make-directory project-cache-dir t))
    (setq server-name (expand-file-name ".cache/emacs.socket" project-dir))
    (server-start)

    (require 'desktop)
    (setq desktop-path (list project-cache-dir)
          desktop-base-file-name "emacs.desktop"
          desktop-restore-eager 5
          desktop-load-locked-desktop t)
    (if (file-exists-p (expand-file-name "emacs.desktop" project-cache-dir))
        (desktop-read project-cache-dir)
      (project-directory-side-window project-dir)
      (desktop-save project-cache-dir))
    (desktop-save-mode 1)))

(defun delete-following-windows ()
  (let ((window (next-window)))
    (unless (eq (window-parameter window 'window-slot) 0)
      (condition-case nil
          (progn (delete-window window)
                 (delete-following-windows))
        (error (set-window-buffer window "*scratch*"))))))

(defun my-find-file ()
  (interactive)
  (hl-line-highlight)
  (delete-following-windows)
  (let ((file-name (dired-get-filename)))
    (cond
     ((file-directory-p file-name)
      (cond
       ((file-exists-p (expand-file-name ".gallery" file-name))
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
        ;; https://github.com/larsmagne/movie.el
        ;; https://www.gnu.org/software/emms/
        ;; http://wikemacs.org/wiki/Media_player
        ;; https://github.com/dbrock/bongo
        )

       ((eq (window-parameter nil 'window-side) 'left)
        (let* ((buffer (dired-noselect file-name))
               (slot (+ 1 (window-parameter nil 'window-slot)))
               (window (display-buffer-in-side-window
                        buffer
                        `((side . left) (slot . ,slot)))))
          (set-window-parameter window 'window-width 0.2)
          (set-window-parameter window 'no-delete-other-windows t)
          (select-window window)))

       ;((file-exists-p (expand-file file-name ".media"))
        ;; view in overlay;
        ;)

       (t
        ;(select-window
         ;(display-buffer-use-some-window (find-file-noselect file-name) nil))
        )))

     ((string-match-p "\\.jpg/?\\'" file-name)
      ;; view in overlay;
      )

     (t
      (select-window
       (display-buffer-use-some-window (find-file-noselect file-name) nil)))
     )))

;; projects list: an Emacs instance with a floating frame, showing the list of projects;

(defun projects-list-find-file ()
  (interactive)
  (let ((file-name (dired-get-filename)))
    (when (file-directory-p file-name)
      (hl-line-highlight)
      ;; send a message to all servers except "projects-list", to hide their frame;
      (call-process-shell-command
       (concat
        "emacsclient --socket-name \""
        (expand-file-name ".cache/emacs.socket" file-name)
        "\" --eval '(select-frame-set-input-focus (selected-frame))'"
        " || "
        "emacs --maximized --eval '(project-open \"" file-name "\")' &")))))

(defun projects-list-create ()
  (let* ((buffer (dired-noselect "~/projects"))
         (window (display-buffer-use-some-window buffer nil)))
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t)
    (select-window window))

  (global-set-key (kbd "C-<backspace>") 'lower-frame)

  ;; eyebrowse views status in the header line;
  (setq header-line-format
        '((:eval (propertize " " 'display '((space :align-to 0))))
          mode-line-misc-info))

  ;; to do: automatically find all "projects/*" directories in connected storage devices,
  ;;   and create an eyebrowse view for each;
  )

(defun projects-list-activate ()
  (interactive)
  (call-process-shell-command
    (concat
      "emacsclient --socket-name projects-list"
      " --eval '(select-frame-set-input-focus (selected-frame))'"
      " || emacs &")))
(global-set-key (kbd "C-p") 'projects-list-activate)

(if (equal command-line-args '("emacs"))
    (progn
      (define-key dired-mode-map [remap dired-find-file] 'projects-list-find-file)
      (define-key dired-mode-map [remap dired-find-file-other-window] 'projects-list-find-file)
      (add-hook 'emacs-startup-hook 'projects-list-create)
      (setq server-name "projects-list")
      (server-start))
  (define-key dired-mode-map [remap dired-find-file] 'my-find-file)
  (define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file))

;; otherwise "select-frame-set-input-focus" above doesn't work properly;
(add-hook 'focus-in-hook (lambda () (raise-frame)))

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	           '("melpa" . "https://stable.melpa.org/packages/") t)
(defun require-package (package)
  (unless (require package nil 'noerror)
    (package-refresh-contents)
    (package-install package)
    (require package)))
(defun install-package (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))
;; https://emacs.stackexchange.com/questions/38206/upgrading-packages-automatically
;; https://www.reddit.com/r/emacs/comments/acvn2l/elisp_script_to_install_all_packages_very_fast/
;; https://www.reddit.com/r/emacs/comments/a4n6iw/how_to_easily_update_one_elpa_package/
;; https://emacs.stackexchange.com/questions/4045/automatically-update-packages-and-delete-old-versions
;; https://github.com/rranelli/auto-package-update.el/blob/master/auto-package-update.el#L251
;; https://github.com/mola-T/SPU

(require-package 'eyebrowse)
(setq eyebrowse-keymap-prefix (kbd "C-w"))
(setq eyebrowse-mode-line-separator " ")
(setq eyebrowse-wrap-around t)
(define-key eyebrowse-mode-map (kbd "j") 'eyebrowse-prev-window-config)
(define-key eyebrowse-mode-map (kbd "k") 'eyebrowse-next-window-config)
(define-key eyebrowse-mode-map (kbd "h") 'eyebrowse-last-window-config)
(define-key eyebrowse-mode-map (kbd "q") 'eyebrowse-close-window-config)
(unless (equal command-line-args '("emacs"))
  (define-key eyebrowse-mode-map (kbd "w") (lambda ()
    (eyebrowse-create-window-config)
    (project-directory-side-window))))

;; modal key_bindings
;; https://github.com/mrkkrp/modalka
;; https://github.com/emacsorphanage/god-mode
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
;(global-set-key (kbd "<escape>") (lambda () (interactive) (modalka--maybe-activate)))
;(global-set-key (kbd "<tab>") (lambda () (interactive) (modalka--maybe-activate)))
;(define-key modalka-mode-map (kbd "RET")
;  (lambda ()
;    (interactive)
;    (modalka-mode -1)
;    (set-cursor-color "red")))

;(add-to-list 'modalka-excluded-modes 'dired-mode)
;(add-to-list 'modalka-excluded-modes 'help-mode)
;(add-to-list 'modalka-excluded-modes 'Info-mode)

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

(defun double-space-to-tab ()
  (interactive)
  (if (equal (char-before (point)) ?\s)
      (progn (delete-backward-char 1)
             (execute-kbd-macro (kbd "<tab>")))
             ;; (call-interactively (key-binding "<tab>"))
    (insert " ")))
(define-key minibuffer-local-map (kbd "SPC") 'double-space-to-tab)
(require 'shell)
(define-key minibuffer-local-shell-command-map (kbd "SPC") 'double-space-to-tab)
(define-key shell-mode-map (kbd "SPC") 'double-space-to-tab)

(defun sleep ()
  (interactive)
  (call-process-shell-command "sleep 0.1; systemctl suspend"))

;; https://explog.in/dot/emacs/config.html
;; https://iqss.github.io/IQSS.emacs/init.html
;; https://www.spacemacs.org/layers/LAYERS.html
;; https://www.reddit.com/r/rust/comments/a3da5g/my_entire_emacs_config_for_rust_in_fewer_than_20/
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;(setq minibuffer-auto-raise t)
;; https://www.emacswiki.org/emacs/Dedicated_Minibuffer_Frame
;; https://stackoverflow.com/questions/3050011/is-it-possible-to-move-the-emacs-minibuffer-to-the-top-of-the-screen
;; https://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
;; https://emacs.stackexchange.com/questions/1074/how-to-display-the-content-of-minibuffer-in-the-middle-of-the-emacs-frame
;; https://github.com/muffinmad/emacs-mini-frame
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Initial-Parameters.html#Initial-Parameters
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffers-and-Frames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Visibility-of-Frames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Child-Frames.html#Child-Frames

;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/flycheck/flycheck-rust
;; https://github.com/brotzeit/rustic
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;(setq rust-indent-offset 2)

;; http://company-mode.github.io/
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm, icomplete

;; http://ergoemacs.org/emacs/emacs_magit-mode_tutorial.html
;;   https://magit.vc/
;;   https://github.com/vermiculus/magithub
;; https://github.com/DarthFennec/highlight-indent-guides
;;   https://github.com/zk-phi/indent-guide
;; https://orgmode.org/manual/Tables.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Based-Tables.html
;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
  ;; https://www.gnu.org/software/auctex
  ;; https://github.com/aaptel/preview-latex
  ;; https://github.com/josteink/wsd-mode
  ;; https://jblevins.org/projects/markdown-mode/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
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
;; https://github.com/Fuco1/smartparens

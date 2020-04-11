(menu-bar-mode -1)
(tool-bar-mode -1)
(setq use-dialog-box nil)
(setq visible-bell t)
;(setq insert-default-directory nil) ;; alternatively we can use double slash mechanism;
(setq-default major-mode 'text-mode)
(cua-mode 1)
(setq window-sides-vertical t)
(global-set-key (kbd "C-x k") (lambda () (quit-window t)))
(global-set-key (kbd "C-TAB") (lambda () (other-window -1)))
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

;; dired first line:
(add-hook 'dired-after-readin-hook (lambda ()
  (let ((inhibit-read-only t)
        (window (get-buffer-window)))
    (save-excursion
      ;; replace the first line with the name of the directory;
      (delete-region (progn (goto-char (point-min)) (point))
                     (progn (move-end-of-line) (point)))
      (insert (file-name-directory (directory-file-name default-directory)))
      ;; in the case of a project directory window, make the first line invisible;
      ;; alternative: (eq window (frame-first-window))
      (unless (and (eq (window-parameter window 'window-side) 'left)
                   (eq (window-parameter window 'window-slot) 0))
        (set-text-properties 1 (progn (goto-char 1) (forward-line 1) (point))
                             '(invisible t)))))))

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

(require 'hl-line)
(add-hook 'dired-mode-hook (lambda () (setq hl-line-mode t)))
;; when focus out of a frame, send point to highlighted line (if there is any);
(add-hook
 'focus-out-hook
 (lambda () (if hl-line-overlay (goto-char (overlay-start hl-line-overlay)))))
;; when moving between windows, send point to highlighted line (if there is any);
(add-hook
 'buffer-list-update-hook
 (lambda () (if hl-line-overlay (goto-char (overlay-start hl-line-overlay)))))
;; this may cause inconviniece when a temporary buffer is created or killed;
;; alternative: advise select-window;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Named-Functions.html
;; https://www.emacswiki.org/emacs/AdvisingFunctions

(defun project-new-view (project-dir)
  ;; new eyebrowse

  ;; project directory window, a side window which shows:
  ;; , the content of project directory;
  ;; , the name of the project in the first line;
  ;; , eyebrowse views status in the header line;
  (let* ((buffer (dired-noselect project-dir))
         (window (display-buffer-in-side-window
                  buffer
                  '((side . left) (slot . 0) (window-width . 0.2)))))
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t) ;; not sure if this is necessary;

    (select-window window)
    (setq header-line-format
          '((:eval (propertize " " 'display '((space :align-to 0))))
            mode-line-misc-info))))

(defun project-open (project-dir)
  (setq-default server-name project-dir)
  (server-start)

  (require 'desktop)
  (let ((desktop-file-dir-path (expand-file-name ".cache/" project-dir))
        (desktop-file-path (expand-file-name ".cache/.emacs.desktop" project-dir)))
    (unless (file-exists-p desktop-file-dir-path)
      (make-directory desktop-file-dir-path t))
    (unless (file-exists-p desktop-file-path)
      (project-new-view)
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
  (desktop-save-mode 1))

(defun delete-following-windows ()
  (let ((window (next-window)))
    (unless (eq (window-parameter nil 'window-slot) 0)
      (delete-window window)
      (delete-following-windows))))

(defun my-find-file ()
  (interactive)
  (hl-line-highlight)
  (let ((file-name (dired-get-filename)))
    (cond
     ((file-directory-p file-name)
      (cond
       ((file-exists-p (expand-file file-name ".gallery"))
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        (delete-following-windows)
        ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
        ;; https://github.com/larsmagne/movie.el
        ;; https://www.gnu.org/software/emms/
        ;; http://wikemacs.org/wiki/Media_player
        ;; https://github.com/dbrock/bongo
        )

       ((eq (window-parameter nil 'window-side) 'left)
        (let* ((buffer (dired-noselect project-dir))
               (slot (+ 1 (window-parameter nil 'window-slot)))
               (window (display-buffer-in-side-window
                        buffer
                        '((side . left) (slot . ,slot) (window-width . 0.2)))))
          (set-window-parameter window 'no-delete-other-windows t)
          (set-window-dedicated-p window t) ;; not sure if this is necessary;
          (select-window window)
          (delete-following-windows)))

       ;((file-exists-p (expand-file file-name ".media"))
        ;; view in overlay;
        ;)

       (t
        (select-window
         (display-buffer-use-some-window (find-file-noselect file-name) nil))
        (delete-following-windows))))

     ((string-match-p "\\.jpg/?\\'" file-name)
      ;; view in overlay;
      )

     (t
      (select-window
       (display-buffer-use-some-window (find-file-noselect file-name) nil))
      (delete-following-windows))
     )))

;; projects list: an Emacs instance with a floating frame, showing the list of projects;

(defun projects-list-find-file ()
  (interactive)
  (when (file-directory-p file-name)
    (hl-line-highlight)
    ;; send a message to all servers except "/", to hide their frame;
    (call-process-shell-command
     (concat
      "emacsclient --socket-name \""
      file-name
      "\" --eval '(select-frame-set-input-focus (selected-frame))'"
      " || "
      "emacs --maximized --eval '(project-open \"" file-name "\")'"))))

(defun projects-list-create ()
  (let* ((buffer (dired-noselect "~/projects"))
         (window (display-buffer-use-some-window buffer nil)))
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t)
    (select-window window))

  (global-set-key (kbd "C-TAB") #'lower-frame)

  ;; eyebrowse views status in the header line;
  (setq header-line-format
        '((:eval (propertize " " 'display '((space :align-to 0))))
          mode-line-misc-info))

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

(if (eq command-line-args '("emacs"))
    (progn
      (define-key dired-mode-map [remap dired-find-file] 'projects-list-find-file)
      (define-key dired-mode-map [remap dired-find-file-other-window] 'projects-list-find-file)
      (add-hook 'emacs-startup-hook 'projects-list-create)
      (setq server-name "/")
      (server-start))
  (define-key dired-mode-map [remap dired-find-file] 'my-find-file)
  (define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file))

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
(define-key minibuffer-map (kbd "SPC") 'double-space-to-tab)
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

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq insert-default-directory nil) ;; alternatively we can use double slash mechanism;
;; (setq window-sides-vertical t)
(setq blink-cursor-blinks 0)
(setq-default cursor-in-non-selected-windows nil)
(setq-default mode-line-format nil)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(cua-mode 1)
(setq-default major-mode 'text-mode)
;; automatically recover unsaved files;

;; auto-save-file-name-transforms
;;(when (not (file-exists-p ".cache"))
;;  (make-directory ".cache"))
;;(setq auto-save-file-name-transforms
;;      '((".*" ".cache/" t)))
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recover.html
;; https://www.emacswiki.org/emacs/AutoSave#toc1
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Files.html
(setq make-backup-files nil)
;; (setq create-lockfiles nil)

(defun sleep ()
  (interactive)
  (call-process-shell-command "sleep 0.1; systemctl suspend"))

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm, icicles, icomplete

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
;; code completion;
(setq-default tab-width 4)

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(set-face-attribute 'window-divider nil :foreground "#555555")

(setq scroll-bar-adjust-thumb-portion nil)
(add-to-list 'default-frame-alist '(scroll-bar-width . 1))
(scroll-bar-mode 'left)
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
(set-face-attribute 'default nil :family "Monospace" :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")

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

;; "if [[ \\"$(i3-msg [workspace=__focused__ class=Qutebrowser] focus)\\" =~ \\"false\\" ]]; \
;;  then \
;;    if [[ \\"$(i3-msg [class=Qutebrowser] focus)\\" =~ \\"false\\" ]]; \
;;    then qutebrowser& fi \
;;  else i3-msg [class=Qutebrowser] move scratchpad; fi"

;;(require 'server)
;;(unless (server-running-p) (server-start))

(load "./projects-list")

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

(load "./project-tree")

(load "./modalka")

(require-package 'adaptive-wrap)
(setq-default adaptive-wrap-extra-indent tab-width)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
;; https://github.com/joostkremers/visual-fill-column
(require-package 'visual-fill-column)
(setq-default fill-column 100)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(global-visual-line-mode +1)

(require-package 'rust-mode)
;;(setq rust-indent-offset 2)
;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/flycheck/flycheck-rust
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;; https://github.com/brotzeit/rustic

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html

;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
;; https://www.gnu.org/software/auctex
;; https://github.com/aaptel/preview-latex
;; https://github.com/josteink/wsd-mode
;; https://jblevins.org/projects/markdown-mode/

;; automatically mount storage devices when available,
;;   https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor

;; /sudo::/...
;; /root@localhost:/...

;(add-hook 'emacs-startup-hook (lambda () ...))

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

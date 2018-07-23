(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(global-set-key (kbd "C-x k") #'kill-this-buffer)

(setq window-divider-default-places t
      window-divider-default-right-width 2
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(setq scroll-bar-adjust-thumb-portion nil)
(add-to-list 'default-frame-alist '(scroll-bar-width . 13))
(add-to-list 'default-frame-alist '(left-fringe . 2))
(add-to-list 'default-frame-alist '(right-fringe . 0))

;; (setq-default mode-line-format nil)
(setq insert-default-directory nil)
(global-eldoc-mode -1)

(setq scroll-conservatively 200) ;; never recenter point
;; move point to top/bottom of buffer before signaling a scrolling error;
(setq scroll-error-top-bottom t)

(setq blink-cursor-blinks 0)
(add-to-list 'default-frame-alist '(foreground-color . "#222222"))
(set-face-attribute 'highlight nil :background "lemon chiffon")
(set-face-attribute 'region nil :background "LightSkyBlue1")
(set-face-attribute 'default nil :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

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

(setq-default indent-tabs-mode nil)

;; adaptive wrap (this is taken from adaptive-wrap package);
(defun adaptive-wrap-fill-context-prefix (beg en)
  "like `fill-context-prefix', but with length 2;"
  ;; note: fill-context-prefix may return nil; see: http://article.gmane.org/gmane.emacs.devel/156285
  (let* ((fcp (or (fill-context-prefix beg en) ""))
         (fcp-len (string-width fcp))
         (fill-char (if (< 0 fcp-len)
                        (string-to-char (substring fcp -1))
                      ?\ )))
    (concat fcp
            (make-string 2 fill-char))))

(defun adaptive-wrap-prefix-function (beg end)
  "indent the region between BEG and END with adaptive filling;"
  ;; any change at the beginning of a line might change its wrap prefix, which affects the whole line;
  ;; so we need to "round-up" `end' to the nearest end of line;
  ;; we do the same with `beg' although it's probably not needed;
  (goto-char end)
  (unless (bolp) (forward-line 1))
  (setq end (point))
  (goto-char beg)
  (forward-line 0)
  (setq beg (point))
  (while (< (point) end)
    (let ((lbp (point)))
      (put-text-property (point)
                         (progn (search-forward "\n" end 'move) (point))
                         'wrap-prefix
                         (let ((pfx (adaptive-wrap-fill-context-prefix
                                     lbp (point))))
                           ;; remove any `wrap-prefix' property that might have been added earlier;
                           ;; otherwise, we end up with a string containing a `wrap-prefix' string, containing a `wrap-prefix' string ...
                           (remove-text-properties 0 (length pfx) '(wrap-prefix) pfx)
                           pfx))))
  `(jit-lock-bounds ,beg . ,end))

(define-minor-mode adaptive-wrap-prefix-mode
  "wrap the buffer text with adaptive filling;"
  :lighter ""
  :group 'visual-line
  (if adaptive-wrap-prefix-mode
      (progn
        ;; HACK ATTACK! we want to run after font-lock (so our wrap-prefix includes the faces applied by font-lock),
        ;; but  jit-lock-register doesn't accept an `append' argument,
        ;; so we add ourselves beforehand, to make sure we're at the end of the hook (bug#15155);
        (add-hook 'jit-lock-functions
                  #'adaptive-wrap-prefix-function 'append t)
        (jit-lock-register #'adaptive-wrap-prefix-function))
    (jit-lock-unregister #'adaptive-wrap-prefix-function)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(defun go-to-link-at-point ()
  "open the file path under cursor; if the path starts with “http://”, open the URL in browser; input path can be relative, full path, URL;"
  (interactive)
  (let (($path (ffap-file-at-point)))
    (if (string-match-p "\\`https?://" $path)
        (progn
          (
           ;; if the web_browser with the profile corresponding to this project is not open, open it; then if there is a web_browser window named "project-name, $path", raise it; otherwise create it;
           ))
      (if (file-exists-p $path)
          (progn
            (
             ;; if there is an emacs frame named "project-name, $path", raise it; otherwise create it;
             ))
        (message "file doesn't exist: '%s';" $path)))))

;; https://www.emacswiki.org/emacs/BrowseUrl
;; https://www.chromium.org/user-experience/multi-profiles

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffers-and-Frames.html
;; https://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
;; https://emacs.stackexchange.com/questions/1074/how-to-display-the-content-of-minibuffer-in-the-middle-of-the-emacs-frame
;; https://www.emacswiki.org/emacs/Dedicated_Minibuffer_Frame

;; view-mode
;; https://github.com/emacs-evil/evil
;; https://github.com/emacs-evil/evil-collection
;; https://www.gnu.org/software/emacs/manual/html_mono/viper.html
;; https://github.com/mrkkrp/modalka
;; https://github.com/jyp/boon
;; http://retroj.net/modal-mode
;; https://github.com/abo-abo/hydra
;; https://github.com/chrisdone/god-mode
;; https://github.com/xahlee/xah-fly-keys
;; https://github.com/ergoemacs/ergoemacs-mode
;; https://github.com/justbur/emacs-which-key
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html

(require 'dired)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(add-hook 'dired-mode-hook (lambda () (setq cursor-type nil)))
(require 'hl-line)
(setq hl-line-sticky-flag nil)
(add-hook 'dired-mode-hook 'hl-line-mode)

(add-to-list
 'command-switch-alist
 (cons "projects"
       #'(lambda (projects-path)
           (setq dired-listing-switches "-l")
           (add-hook 'dired-mode-hook 'dired-hide-details-mode)
           (find-file projects-path)
           (defun dired-find-project ()
             (interactive)
             (let ((file-name (dired-get-filename nil t)))
               (if (file-directory-p file-name)
               ;; first move all windows in the main workspace into the hidden workspace, and rename the main workspace to "project_name";
               ;; then if there is an Emacs frame named "project_name*", if a window named "project_name" exist, move it to the main workspace, otherwise close all windows named "project_name*"; then do the next line;
               ;; if there is no Emacs frame named "project_name*", load the saved Emacs desktop in the project directory, and open its windows in the hidden workspace; then if there is a frame named "project_name", move it to the main workspace, otherwise create it;
               )))
           (define-key dired-mode-map [remap dired-find-file] 'dired-find-project))))

(add-to-list 'command-switch-alist
             (cons "project"
                   #'(lambda (project-path)
                       (desktop-save-mode 1)
                       (desktop-change-dir project-path))))

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

;; https://www.emacswiki.org/emacs/SrSpeedbar
;; https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Side-Windows.html
;; http://mads-hartmann.com/2016/05/12/emacs-tree-view.html
;; next file:
;; , go to tree view
;; , next file
;; , open (in the window at right, go to the first line)
;; https://www.emacswiki.org/emacs/DiredView

;; (require-package 'sr-speedbar)
;; in speedbar show all files
;; hide "\".*\" \"target\" \"*.lock\" \"#*#\""
;; (defun sr-speedbar-open-file ()
;;   (interactive)
;;   (let ((file-name (speedbar-line-file nil t)))
;;     (if (and (file-directory-p file-name) (string-match-p "\\.m\\'" file-name))
;;         (find-file file-name)
        ;; open image-dired/movie in the right window
        ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
        ;; https://github.com/larsmagne/movie.el
        ;; http://www.mplayerhq.hu/DOCS/tech/slave.txt
;;       (speedbar-find-file))))
;; (define-key speedbar-mode-map [remap speedbar-find-file] speedbar-open-file))

;; https://orgmode.org/manual/Tables.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Text-Based-Tables.html
;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html
;; https://www.gnu.org/software/auctex
;; https://github.com/aaptel/preview-latex
;; https://github.com/josteink/wsd-mode
;; https://jblevins.org/projects/markdown-mode/
;; http://ergoemacs.org/emacs/emacs_view_image_thumbnails.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html
;; https://www.emacswiki.org/emacs/ThumbsMode
;; https://www.gnu.org/software/emms/screenshots.html
;; http://wikemacs.org/wiki/Media_player
;; https://github.com/dbrock/bongo

;; https://github.com/dengste/minimap

;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el
;; https://github.com/racer-rust/emacs-racer
;; https://github.com/flycheck/flycheck-rust
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/

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
;;   https://www.emacswiki.org/emacs/DictEm
;;   https://www.emacswiki.org/emacs/wordnik.el
;;   https://github.com/gromnitsky/wordnut
;;   https://www.emacswiki.org/emacs/ThesauriAndSynonyms
;;   https://github.com/atykhonov/google-translate
;; https://www.gnu.org/software/emacs/manual/html_node/calc/index.html
;; https://github.com/domtronn/all-the-icons.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Document-View.html

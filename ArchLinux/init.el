(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)
(setq visible-bell t)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(require 'seq)

(defun delete-following-windows ()
  (let ((window (next-window)))
    (unless (or (equal window (frame-first-window))
                (eq (window-parameter window 'window-slot) 0))
      (condition-case nil
          (progn (delete-window window)
                 (delete-following-windows))
        (error (progn (set-window-dedicated-p window nil)
                      (save-selected-window
                        (select-window window)
                        (display-about-screen)
                        (set-window-prev-buffers window nil))))))))
(global-set-key (kbd "C-x 0")
                (lambda () (interactive)
                  (delete-following-windows)
                  (condition-case nil
                      (delete-window)
                    (error (progn (set-window-dedicated-p nil nil)
                                  (display-about-screen)
                                  (set-window-prev-buffers nil nil))))
                  (other-window -1)))

(setq window-divider-default-places t
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(window-divider-mode 1)
(set-face-attribute 'window-divider nil :foreground "#555555")

(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w j")
                (lambda () (interactive)
                  (if hl-line-overlay
                      (goto-char (overlay-start hl-line-overlay)))
                  (other-window -1)))
(global-set-key (kbd "C-w k")
                (lambda () (interactive)
                  (if hl-line-overlay
                      (goto-char (overlay-start hl-line-overlay)))
                  (other-window 1)))

(setq window-combination-limit nil)
(setq window-combination-resize t)

(setq window-sides-vertical t)
(setq display-buffer-alist
      `(("\\*Completions\\*" display-buffer-pop-up-window)
        ("\\*.*\\*" display-buffer-in-side-window
         (side . bottom) (slot . 0) (window-height . 0.3))))
(global-set-key (kbd "C-x C-f")
                (lambda (filename &optional _wildcards)
                  (interactive
                   (find-file-read-args "Find file: "
                                        (confirm-nonexistent-file-or-buffer)))
                  (let ((buffer (find-file-noselect filename)))
                    (select-window
                     (display-buffer-in-side-window
                      buffer `((side . bottom) (slot . 0) (window-height . 0.3)))))))

;; when an empty side window remains from last session,
;;   it will be used to show buffers (and i don't know why);
;; to avoid that:
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (mapcar
             (lambda (window)
               (if (or (eq (window-parameter window 'window-side) 'bottom)
                       (eq (window-parameter window 'window-side) 'top))
                   (delete-window window)))
             (window-list))
            t))

(add-to-list 'window-persistent-parameters '(window-side . writable))
(add-to-list 'window-persistent-parameters '(window-slot . writable))
(add-to-list 'window-persistent-parameters '(no-delete-other-windows . writable))
(add-to-list 'window-persistent-parameters '(header-line-format . writable))

(setq-default mode-line-format nil)
(set-face-attribute 'header-line nil :foreground "#333333" :background "#dddddd")
(setq-default header-line-format
              '((:eval (if (and buffer-file-name (buffer-modified-p))
                           (propertize "▊" 'face '(:foreground "red"))))
                (:eval (propertize " " 'display '((space :align-to 0))))
                (:eval (or buffer-file-truename dired-directory (buffer-name)))))

(scroll-bar-mode -1)
;(if (and (equal (window-start) (point-min)) (equal (window-end) (point-max)))
;    nil)

;; never recenter point;
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

(setq blink-cursor-blinks 0)
(setq-default cursor-in-non-selected-windows nil)
;; https://github.com/Malabarba/beacon

(add-to-list 'default-frame-alist '(foreground-color . "#333333"))
(set-face-attribute 'fringe nil :background 'unspecified)
(set-face-attribute 'highlight nil :background "#CCFFFF")
(set-face-attribute 'region nil :background "#CCFFFF")
(set-face-attribute 'default nil :family "Monospace" :height 105)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")

(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(defun goto-link-at-point ()
  (interactive)
  (let ((path (ffap-file-at-point)))
    (cond
     ((string-match-p "\\`git://" path)
      )
     ((string-match-p "\\`https?://" path)
      )
     (t
      (message "file doesn't exist: '%s';" path))
     )))

;; =====================================================
;; dired

(require 'dired)
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-keep-marker-rename nil
      dired-keep-marker-copy nil
      dired-keep-marker-hardlink nil
      dired-keep-marker-symlink nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq dired-listing-switches "-v")
;; unfortunately "ls -v" sorting is case sensitive, even when "LC_COLLATE=en_US.UTF-8";
;; so i had to use Emacs' own "ls";
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-ignore-case t)
(require 'dired-x)
(setq dired-omit-verbose nil)
(setq dired-omit-files "^target$\\|\\.lock$")
(add-hook 'dired-mode-hook 'dired-omit-mode)

(define-key dired-mode-map [remap end-of-buffer]
  (lambda () (interactive)
    (end-of-buffer)
    (if (eq (point) (point-max))
        (forward-line -1))))
(define-key dired-mode-map [remap next-line]
  (lambda () (interactive)
    (forward-line 1)
    (if (eq (point) (point-max))
        (forward-line -1))))
(define-key dired-mode-map [remap previous-line]
  (lambda () (interactive)
    (forward-line -1)))

(defvar video-file-suffix (concat "\\.avif$\\|\\.jpg$\\|\\.png$\\|\\.gif$\\|\\.webp$\\|"
                                  "\\.mp4$\\|\\.mkv$\\|\\.webm$\\|\\.mpg$\\|\\.flv$\\|\\.g$"))
(defvar audio-file-suffix "\\.opus$\\|\\.ogg$\\|\\.mka$\\|\\.mp3$")
(defvar media-file-suffix (concat video-file-suffix "\\|" audio-file-suffix))
(defvar known-file-suffix (concat video-file-suffix "\\|" audio-file-suffix "\\|\\.txt$"))

;; https://github.com/purcell/diredfl
(nconc dired-font-lock-keywords
       (list
        ;; suffixes
        '("[^ .]\\(\\.[^. /]+\\)$" 1 dired-ignored-face)
        ;; media files
        `(,(concat "\\([^\n]*\\)\\(" media-file-suffix "\\)") 1 dired-mark-face prepend)
        ;; marked files
        `(,(concat "^\\([^\n " (char-to-string dired-del-marker) "].*$\\)")
          1 dired-marked-face prepend)
        `(,(concat "^\\([" (char-to-string dired-del-marker) "].*$\\)")
          1 dired-flagged-face prepend)
        ))

(add-hook
 'dired-after-readin-hook
 (lambda ()
   (let ((inhibit-read-only t))
     (save-excursion
       ;; hide the first line in dired;
       (goto-char 1)
       (forward-line 2)
       (narrow-to-region (point) (point-max))

       (while (not (eobp))
         (let ((filename (dired-get-filename nil t)))
           (when filename
             ;; hide the two spaces at the begining of each line in dired;
             (let ((ov (make-overlay (point) (1+ (point)))))
               (overlay-put ov 'invisible t))
             (dired-goto-file filename)
             (let ((ov (make-overlay (1- (point)) (point))))
               (overlay-put ov 'invisible t))

             ;; if the corresponding buffer is modified, add a modified indicator;
             ;; for directories this must be done for each modified buffer whose file's path
             ;;   contains the directory;
             (let ((buffer (get-file-buffer filename)))
               (if (and buffer
                        (not (file-directory-p filename))
                        (buffer-modified-p buffer))
                   (modified-indicator-add-overlay)
                 (if (file-directory-p filename)
                     (dolist (_buffer
                              (seq-filter (lambda (buffer)
                                            (let ((f (buffer-file-name buffer)))
                                              (and (buffer-modified-p buffer)
                                                   f
                                                   (not (file-directory-p f))
                                                   (string-prefix-p
                                                    (file-name-as-directory filename)
                                                    (expand-file-name f)))))
                                          (buffer-list)))
                       (modified-indicator-add-overlay)))))

             ;; hide known file suffixes;
             (if (search-forward-regexp (concat "[^ .]\\(" known-file-suffix "\\)")
                                        (line-end-position) t)
                 (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
                   (overlay-put ov 'invisible t)))
             ))
         (forward-line 1))))))

;; hide markers
(advice-add 'dired-mark :after
            (lambda (_arg &optional _interactive)
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((filename (dired-get-filename nil t)))
                    (when filename
                      (let ((ov (make-overlay (point) (1+ (point)))))
                        (overlay-put ov 'invisible t))))
                  (forward-line 1)))))

(require 'hl-line)
(add-hook 'dired-mode-hook (lambda () (setq hl-line-mode t)))
;; before leaving a window, send the cursor back to the highlighted line (if there is any);
(add-hook 'mouse-leave-buffer-hook (lambda ()
  (if hl-line-overlay
      (goto-char (overlay-start hl-line-overlay)))))
(add-hook 'pre-command-hook (lambda ()
                              (if (and (eq this-command 'other-window)
                                       hl-line-overlay)
                                  (goto-char (overlay-start hl-line-overlay)))))

;; for copy_paste mechanism:
;;   https://emacs.stackexchange.com/questions/39116/simple-ways-to-copy-paste-files-and-directories-between-dired-buffers
;;   https://emacs.stackexchange.com/questions/17599/current-path-in-dired-or-dired-to-clipboard
;; async file operations in dired
;;   https://github.com/jwiegley/emacs-async
;;   https://truongtx.me/tmtxt-dired-async.html
;;   https://github.com/jwiegley/emacs-async/blob/master/dired-async.el
;; https://oremacs.com/2016/02/24/dired-rsync/

(defun my-find-file ()
  (interactive)
  (hl-line-highlight)
  (delete-following-windows)
  (let ((file-name (dired-get-filename)))
    (cond
     ((file-directory-p file-name)

      (cond
       ((eq (window-parameter nil 'window-side) 'left)
        (if (string-match-p "\\.g/?$" file-name)
            (let* ((buffer (dired-noselect file-name))
                   (window (display-buffer-use-some-window buffer nil)))
              (set-window-parameter window 'no-delete-other-windows t)
              (set-window-dedicated-p window t)
              (select-window window)
              (set-window-parameter window 'header-line-format 'none)
              ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
              ;; https://github.com/larsmagne/movie.el
              )
          (let* ((buffer (dired-noselect file-name))
                 (slot (+ 1 (window-parameter nil 'window-slot)))
                 (window (display-buffer-in-side-window
                          buffer
                          `((side . left) (slot . ,slot) (window-width . 0.2)))))
            (set-window-parameter window 'no-delete-other-windows t)
            (select-window window)
            (set-window-parameter window 'header-line-format 'none))))

       ;; ((string-match-p "\\.mp4/?$" file-name)
       ;;  ;; view the files in overlay;
       ;; )
       ;; ((string-match-p "\\.mp3/?$" file-name)
       ;;  ;; play audio files;
       ;; )

       (t
        (let* ((buffer (dired-noselect file-name))
               (window (or (display-buffer-use-some-window buffer nil)
                           (display-buffer-below-selected buffer nil))))
          (set-window-parameter window 'no-delete-other-windows t)
          (set-window-dedicated-p window t)
          (select-window window)
          (set-window-parameter window 'header-line-format 'none)))))

     ((string-match-p video-file-suffix file-name)
      ;; view in overlay;

      (call-process "xdg-open" nil 0 nil file-name)
      )

     ((string-match-p audio-file-suffix file-name)
      ;; play audio file;
      ;; https://www.gnu.org/software/emms/
      ;; https://github.com/mihaiolteanu/vuiet
      ;; https://github.com/jorenvo/simple-mpc
      ;; https://github.com/pft/mingus
      ;; https://github.com/mpdel/mpdel

      (call-process "xdg-open" nil 0 nil file-name)
      )

     (t
      (let* ((buffer (find-file-noselect file-name))
             (window (or (display-buffer-use-some-window buffer nil)
                         (display-buffer-below-selected buffer nil))))
        (set-window-parameter window 'no-delete-other-windows t)
        (set-window-dedicated-p window t)
        (select-window window)
        (set-window-parameter window 'header-line-format 'none))
      ))))

(defun parent-directories (file-name)
  (let (parent-directories)
    ;; find parent directories in the project;
    (let ((file-name (file-name-directory (directory-file-name file-name))))
      (if (and file-name
               (string-match-p "/projects/" file-name))
          (while (or (not (string-match-p "/projects/$" file-name)))
            (push file-name parent-directories)
            (setq file-name
                  (file-name-directory (directory-file-name file-name)))))
      parent-directories)))

(defun parent-directories-update ()
  (let ((main-file ""))
    (mapcar
     (lambda (window)
       (let* ((buffer (window-buffer window))
              (file-name (or (buffer-file-name buffer)
                             (let ((dir (with-current-buffer buffer dired-directory)))
                               (if dir (expand-file-name dir))))))
         (when (and file-name
                    (eq 'none (window-parameter window 'header-line-format))
                    (string-lessp main-file file-name))
           (setq main-file file-name))))
     (window-list))

    (let ((buffer (window-buffer (selected-window)))
          (file-name (dired-get-filename nil t)))
      (project-directory-side-window)
      (dolist (directory (cdr (parent-directories main-file)))
        (dired-goto-file directory)
        (my-find-file))
      (when (string-lessp "" main-file)
        (dired-goto-file main-file)
        (my-find-file))
      (let ((window (get-buffer-window buffer)))
        (if (window-live-p window)
            (select-window window)))
      (if file-name (dired-goto-file file-name)))))

(advice-add 'dired-revert :after (lambda (&rest _) (parent-directories-update)))

(defun modified-indicator-add-overlay ()
  (let ((s "x")
        (ov (make-overlay (point) (1+ (point)))))
    (put-text-property 0 1 'display '(left-fringe filled-rectangle error) s)
    (overlay-put ov 'modified-indicator t)
    (overlay-put ov 'before-string s)))

;; indicate modified state of a file in dired;
(defun modified-indicator (file-name &optional remove)
  (when (and file-name
             (eq 'none (window-parameter nil 'header-line-format)))
    (dolist (directory (parent-directories file-name))
      (with-current-buffer (dired-noselect directory)
        (save-excursion
          (dired-goto-file file-name)
          (if remove
              (let ((overlay (elt (seq-filter
                                   (lambda (ov) (overlay-get ov 'modified-indicator))
                                   (overlays-at (point)))
                                  0)))
                (if (overlayp overlay) (delete-overlay overlay)))
            (modified-indicator-add-overlay)))))))

(add-hook 'first-change-hook (lambda () (modified-indicator buffer-file-name)))
(add-hook 'after-save-hook (lambda () (modified-indicator buffer-file-name 'remove)))
;(advice-add 'undo :after (lambda (&optional _arg)))
(add-hook 'post-command-hook (lambda ()
                               (when (and (eq this-command 'undo)
                                          (not (buffer-modified-p)))
                                 (modified-indicator buffer-file-name 'remove))))

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
        "emacs --eval '(project-open \"" file-name "\")' &")))))

;; otherwise "select-frame-set-input-focus" above doesn't work properly;
(add-hook 'focus-in-hook (lambda () (raise-frame)))

(if (equal command-line-args '("emacs"))
    (progn
      (define-key dired-mode-map [remap dired-find-file] 'projects-list-find-file)
      (define-key dired-mode-map [remap dired-find-file-other-window] 'projects-list-find-file)
      (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'projects-list-find-file))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (define-key dired-mode-map [remap dired-find-file] 'my-find-file)
  (define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'my-find-file))

;; ==========================================================
;; project

(defvar project-directory nil)

(defun project-directory-side-window ()
  (interactive)
  (let* ((buffer (dired-noselect project-directory))
         (window (display-buffer-in-side-window
                  buffer
                  '((side . left) (slot . 0) (window-width . 0.2)))))
    (set-window-parameter window 'no-delete-other-windows t)
    (select-window window)

    ;; show project's views, and project's name, in the header line;
    (set-window-parameter
     window
     'header-line-format
     '((:eval (propertize " " 'display '((space :align-to 0))))
       (:eval (let ((views-num (length (eyebrowse--get 'window-configs))))
                (if (< 1 views-num)
                    (propertize (format "%d/%d "
                                        (eyebrowse--get 'current-slot)
                                        views-num)
                                'font-lock-face '(:foreground "forest green")))))
       (:eval (replace-regexp-in-string
               "^[[:digit:]]+, " ""
               (file-name-nondirectory (directory-file-name default-directory))))
       ))))

(defun project-open (project-dir)
  (setq project-directory project-dir)
  (let* ((project-cache-dir (expand-file-name ".cache/" project-directory)))
    (unless (file-exists-p project-cache-dir)
      (make-directory project-cache-dir t))

    (setq server-name (expand-file-name "emacs.socket" project-cache-dir))
    (server-start)

    (undo-system project-cache-dir)

    (push '(foreground-color . :never) frameset-filter-alist)
    (push '(background-color . :never) frameset-filter-alist)
    (push '(background-mode . :never) frameset-filter-alist)
    (push '(font . :never) frameset-filter-alist)
    (push '(font-backend . :never) frameset-filter-alist)
    (push '(font-parameter . :never) frameset-filter-alist)
    (push '(cursor-type . :never) frameset-filter-alist)
    (push '(cursor-color . :never) frameset-filter-alist)
    (push '(mouse-color . :never) frameset-filter-alist)
    (push '(border-width . :never) frameset-filter-alist)
    (push '(internal-border-width . :never) frameset-filter-alist)
    (push '(right-divider-width . :never) frameset-filter-alist)
    (push '(bottom-divider-width . :never) frameset-filter-alist)
    (push '(vertical-scroll-bars . :never) frameset-filter-alist)
    (push '(screen-gamma . :never) frameset-filter-alist)
    (push '(alpha . :never) frameset-filter-alist)
    (push '(line-spacing . :never) frameset-filter-alist)
    (push '(left-fringe . :never) frameset-filter-alist)
    (push '(right-fringe . :never) frameset-filter-alist)
    (push '(no-special-glyphs . :never) frameset-filter-alist)
    (push '(scroll-bar-foreground . :never) frameset-filter-alist)
    (push '(scroll-bar-background . :never) frameset-filter-alist)
    (push '(scroll-bar-width . :never) frameset-filter-alist)
    (push '(scroll-bar-height . :never) frameset-filter-alist)
    (push '(menu-bar-lines . :never) frameset-filter-alist)
    (push '(tool-bar-lines . :never) frameset-filter-alist)
    (push '(tool-bar-position . :never) frameset-filter-alist)
    (push '(title . :never) frameset-filter-alist)
    (push '(wait-for-wm . :never) frameset-filter-alist)
    (push '(inhibit-double-buffering . :never) frameset-filter-alist)
    (push '(icon-type . :never) frameset-filter-alist)
    (push '(auto-raise . :never) frameset-filter-alist)
    (push '(auto-lower . :never) frameset-filter-alist)
    (push '(display-type . :never) frameset-filter-alist)
    (push '(environment . :never) frameset-filter-alist)

    (require 'desktop)
    (setq desktop-path (list project-cache-dir)
          desktop-base-file-name "emacs.desktop"
          desktop-restore-eager 5
          desktop-load-locked-desktop t)
    (if (file-exists-p (expand-file-name "emacs.desktop" project-cache-dir))
        (desktop-read project-cache-dir)
      (display-about-screen)
      (project-directory-side-window)
      (desktop-save project-cache-dir))
    (desktop-save-mode 1)
    (parent-directories-update)))

;; projects list: an Emacs instance with a floating frame, showing the list of projects;
(defun projects-list-create ()
  (let* ((buffer (dired-noselect "~/projects"))
         (window (display-buffer-use-some-window buffer nil)))
    (set-window-parameter window 'no-delete-other-windows t)
    (set-window-dedicated-p window t)
    (select-window window)

    (set-window-parameter window 'header-line-format
                          '((:eval (propertize " " 'display '((space :align-to 0))))
                            (:eval (let ((views-num (length (eyebrowse--get 'window-configs))))
                                     (if (< 1 views-num)
                                         (propertize (format "%d/%d "
                                                             (eyebrowse--get 'current-slot)
                                                             views-num)
                                                     'font-lock-face '(:foreground "forest green")))))
                            "projects")))

  ;; to do: automatically find all "projects/*" directories in connected storage devices,
  ;;   and create an eyebrowse view for each;
  )

(when (equal command-line-args '("emacs"))
  (add-hook 'emacs-startup-hook 'projects-list-create)
  (setq server-name "projects-list")
  (server-start))

(defun projects-list-activate ()
  (interactive)
  (call-process-shell-command
    (concat
      "emacsclient --socket-name projects-list"
      " --eval '(select-frame-set-input-focus (selected-frame))'"
      " || emacs &")))

(define-prefix-command 'project-map)
(global-set-key (kbd "C-p") 'project-map)
(global-set-key (kbd "C-p p") 'projects-list-activate)

;; =========================================================
;; package management

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
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

;; ==========================================================
;; undo system which also is used to recover unsaved files;
(install-package 'undohist)

(defun undo-system (project-cache-dir)
  (require 'undohist)
  (setq undohist-directory (expand-file-name "emacs-undo" project-cache-dir))
  (if (not (file-directory-p undohist-directory))
      (make-directory undohist-directory t))
  (defvar-local saved-undo-list nil)

  ;; clear undo history, after saving the buffer (even if buffer is unmodified);
  (advice-add 'save-buffer :after (lambda (&optional _arg)
                                        (delete-file (make-undohist-file-name buffer-file-name))
                                        (setq saved-undo-list nil)
                                        (setq buffer-undo-list nil)))

  (defvar-local undo-save-timer nil)
  (defun undo-save-set-timer (buffer)
    (when buffer
      (with-current-buffer buffer
        (unless (or (null buffer-file-name)
                    (eq t buffer-undo-list)
                    (equal buffer-undo-list saved-undo-list))
          (let ((buffer-undo-list buffer-undo-list)
                (inhibit-modification-hooks t)
                last-command)
            (save-excursion
              (primitive-undo (length buffer-undo-list) buffer-undo-list)
              (setq last-command 'ignore)
              (undohist-save-safe)
              (primitive-undo 1 buffer-undo-list)))
          (setq saved-undo-list buffer-undo-list)))))

  ;; after 10 seconds, if buffer-undo-list is modified, while keeping buffer-undo-list:
  ;; , undo all the way back to previously saved;
  ;; , save undo history;
  ;; , undo the previous undo all;
  (add-hook 'after-change-functions (lambda (_beg _end _length)
                                      (unless (null buffer-file-name)
                                        ;; cancel any previous timer;
                                        (when undo-save-timer
                                          (cancel-timer undo-save-timer)
                                          (setq undo-save-timer nil))
                                        (setq undo-save-timer
                                              (run-at-time 10 nil
                                                           'undo-save-set-timer
                                                           (current-buffer))))))

  ;; recover file from its saved undo history;
  (add-hook 'find-file-hook (lambda ()
                              (undohist-recover-safe)
                              (primitive-undo 1 buffer-undo-list)
                              (setq last-command 'ignore)
                              (setq saved-undo-list buffer-undo-list))))

;; ===========================================================
;; eyebrowse

(require-package 'eyebrowse)
(eyebrowse-mode t)
(setq eyebrowse-wrap-around t)
(global-set-key (kbd "C-p j") 'eyebrowse-prev-window-config)
(global-set-key (kbd "C-p k") 'eyebrowse-next-window-config)
(global-set-key (kbd "C-p h") 'eyebrowse-last-window-config)
(global-set-key (kbd "C-p q") 'eyebrowse-close-window-config)
(unless (equal command-line-args '("emacs"))
  (global-set-key (kbd "C-p n") (lambda ()
    (interactive)
    (eyebrowse-create-window-config)
    (project-directory-side-window))))

(add-hook 'eyebrowse-pre-window-switch-hook
          (lambda ()
            (if hl-line-overlay
                (goto-char (overlay-start hl-line-overlay)))))

(add-hook 'eyebrowse-post-window-switch-hook 'parent-directories-update)

(run-with-idle-timer
 20 t
 (lambda ()
   ;; delete the buffer if it's not in any other eyebrowse window;
   (dolist (buffer (buffer-list))
     (with-current-buffer buffer
       (unless (or (get-buffer-window)
                   (and (null buffer-file-name) (null dired-directory)))
         (let ((buffer-has-no-window t))
           (dolist (window-config (eyebrowse--get 'window-configs))
             (eyebrowse--walk-window-config (cadr window-config)
                                            (lambda (item)
                                              (when (eq (car item) 'buffer)
                                                (let ((buffer-name (cadr item)))
                                                  (if (equal (buffer-name buffer) buffer-name)
                                                      (setq buffer-has-no-window nil)))))))
           (if buffer-has-no-window (kill-buffer buffer))))))))

;; =============================================================
;;modalka
(cua-mode 1)

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
;; https://emacs-leuven.readthedocs.io/en/latest/#description
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

;; https://github.com/bmag/imenu-list
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html

;; lsp-rust, lsp-flycheck
;; https://christian.kellner.me/2017/05/31/language-server-protocol-lsp-rust-and-emacs/
;; https://github.com/flycheck/flycheck-rust
;; https://github.com/brotzeit/rustic
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;(setq rust-indent-offset 2)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)
;; ido-ubiquitous, helm

;; highlight-changes-mode
;; instead of highlighting create fringes;
;; https://github.com/emacs-evil/goto-chg/blob/master/goto-chg.el

;; http://ergoemacs.org/emacs/emacs_magit-mode_tutorial.html
;;   https://magit.vc/
;;   https://github.com/vermiculus/magithub
;;   https://github.com/dgutov/diff-hl
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
;; https://github.com/fniessen/emacs-leuven-theme
;; https://github.com/jackkamm/undo-propose-el
;; https://github.com/jgarvin/mandimus

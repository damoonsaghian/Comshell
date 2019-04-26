(require 'dired)
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-keep-marker-rename nil
      dired-keep-marker-copy nil)
(setq dired-listing-switches "-l -I \"#*#\" -I \"*.lock\" -I \"target\"")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; auto refresh dired, but be quiet about it;
;;(add-hook 'dired-mode-hook 'auto-revert-mode) ;;(setq global-auto-revert-non-file-buffers t)
;;(setq auto-revert-verbose nil)

(define-key dired-mode-map [remap next-line] 'dired-next-line)
(define-key dired-mode-map [remap previous-line] 'dired-previous-line)

(require 'hl-line)
(add-hook 'dired-mode-hook (lambda () (setq hl-line-mode t)))

(defun hide-projects-list ()
  (interactive)
  ;; send point to highlighted line (if there is any);
  (if hl-line-overlay (goto-char (overlay-start hl-line-overlay)))
  ;; hide projects-list window
  )

;; make the first line of dired, invisible;
(add-hook
 'dired-after-readin-hook
 (lambda ()
   (let ((inhibit-read-only t))
     (save-excursion
       (set-text-properties 1 (progn (goto-char 1) (forward-line 1) (point))
                            '(invisible t))))))

;; https://github.com/Fuco1/dired-hacks#dired-open
;; show state of files (modified or not, git) in dired using marks;
;;   https://github.com/syohex/emacs-dired-k
;;   https://github.com/dgutov/diff-hl
;;   https://emacs.stackexchange.com/questions/9503/how-can-i-visualize-vcs-status-in-dired
;;   show branch in the header of side window;
;; https://www.emacswiki.org/emacs/DiredView
;; async file operations in dired
;;   https://github.com/jwiegley/emacs-async/blob/master/dired-async.el
;; for copy_paste mechanism:
;;   https://github.com/Fuco1/dired-hacks/blob/master/dired-ranger.el
;; sort numbers (10 after 9)
;;   https://emacs.stackexchange.com/questions/5649/sort-file-names-numbered-in-dired
;; https://github.com/sebastiencs/sidebar.el

(defun show-projects ()
  (interactive)
  (select-frame (make-frame '((name . "projects") (minibuffer . nil))))
  (dired "~/projects/")
  (local-set-key (kbd "<escape>") #'my-other-window)
  (hl-line-highlight))
(global-set-key (kbd "M-RET") 'show-projects)

(defun my-find-file ()
  (interactive)
  (let ((file-name (dired-get-filename)))
    (cond
     ((string-match-p "/projects/[^/]*/?\\'" file-name)
      (when (file-directory-p file-name)
        (delete-frame)
        (let* ((buffer (dired-noselect file-name))
               (window (display-buffer-in-side-window
                        buffer
                        '((side . left) (slot . 0) (window-width . 0.2)))))
          (set-window-parameter window 'no-delete-other-windows t)
          (set-window-dedicated-p window t) ;; not sure if this is necessary;
          (select-window window)
          (hl-line-highlight)
          (my-find-file))))
     (t
      (hl-line-highlight)
      (select-window
       (display-buffer-use-some-window (find-file-noselect file-name) nil))
      (delete-other-windows)))))
(define-key dired-mode-map [remap dired-find-file] 'my-find-file)
(define-key dired-mode-map [remap dired-find-file-other-window] 'my-find-file)

(call-process-shell-command
 (concat
  "i3-msg workspace '\"" project-name "\"'; "
  "if [[ \"$(i3-msg [workspace=__focused__ class=Emacs] mark a)\" "
  "  =~ \"false\" ]]; "
  "then "
  "  i3-msg [workspace=__focused__] kill; "
  "  emacs --title='' --eval '(goto-project \"" file-name "\")' & fi"))
;; --name=''
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html

(defun goto-project (project-path)
  (let* ((buffer (dired-noselect project-path))
         (window (display-buffer-in-side-window
                  buffer
                  '((side . left) (window-width . 0.2) (no-delete-other-windows . t)))))
    (set-window-dedicated-p window t)
    (select-window window)
    (hl-line-highlight)
    (my-find-file)))

(defun open-project (project-path)
  (require 'desktop)
  (let ((desktop-file-dir-path (expand-file-name ".cache/" project-path))
        (desktop-file-path (expand-file-name ".cache/.emacs.desktop" project-path)))
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
)


;; https://www.emacswiki.org/emacs/Dedicated_Minibuffer_Frame
;; https://stackoverflow.com/questions/3050011/is-it-possible-to-move-the-emacs-minibuffer-to-the-top-of-the-screen
;; https://stackoverflow.com/questions/5079466/hide-emacs-echo-area-during-inactivity
;; https://emacs.stackexchange.com/questions/1074/how-to-display-the-content-of-minibuffer-in-the-middle-of-the-emacs-frame
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffers-and-Frames.html

;; after quiting, show projects list;
;; (add-hook 'kill-emacs-hook (lambda () ...))
;; "if [[ \\"$(i3-msg [class=Emacs title=projects] focus)\\" =~ \\"false\\" ]]; \
;; then emacs & fi"
;; [workspace=__focused__ floating] move scratchpad

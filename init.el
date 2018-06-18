(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      visible-bell t
      scroll-bar-adjust-thumb-portion nil
      scroll-conservatively 200
      paragraph-start "\n"
      paragraph-separate "\n"
      make-backup-files nil)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-eldoc-mode -1)
;(setq-default mode-line-format nil)
(setq-default indent-tabs-mode nil)
(set-face-attribute 'region nil :background "sky blue")
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

(setq default-frame-alist
      '((scroll-bar-width . 13)
        (left-fringe . 2)
        (right-fringe . 0)
        (foreground-color . "#222222")
        ))

(setq-default cursor-type 'bar)
(setq blink-cursor-blinks 0)
(set-face-attribute 'cursor nil :background "red")
(global-hl-line-mode 1)
(set-face-attribute 'highlight nil :background "lemon chiffon")
(show-paren-mode 1)

;; following code is taken from minibuffer-line package
(defvar minibuffer-line-format
  '((:eval (format-time-string "%I:%M%p %a %F")))
  "specification of the contents of the minibuffer-line; uses the same format as `mode-line-format'.")
(defface minibuffer-line--face
  '((t :foreground "#777777"))
  "minibuffer-line face")
(defvar minibuffer-line--buffer " *Minibuf-0*")
(defvar minibuffer-line--timer nil)
(defun minibuffer-line--update ()
  (with-current-buffer minibuffer-line--buffer
    (erase-buffer)
    (insert (format-mode-line minibuffer-line-format 'minibuffer-line--face))))

(define-minor-mode minibuffer-line-mode
  "display status info in the minibuffer window;"
  :global t
  (with-current-buffer minibuffer-line--buffer
    (erase-buffer))
  (when minibuffer-line--timer
    (cancel-timer minibuffer-line--timer)
    (setq minibuffer-line--timer nil))
  (when minibuffer-line-mode
    (setq minibuffer-line--timer (run-with-timer t 10 #'minibuffer-line--update))
    (minibuffer-line--update)))
(minibuffer-line-mode 1)

(setq insert-default-directory nil)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(setq dired-listing-switches "-l -I \"target\" -I \"*.lock\" -I \"#*#\"")
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(setq-default proced-auto-update-flag t)
(setq-default proced-auto-update-interval 2)

(defun next-paragraph ()
  (interactive)
  (unless (bobp) (left-char))
  (forward-paragraph)
  (unless (eobp)
    (progn
      (forward-paragraph)
      (redisplay t)
      (backward-paragraph)
      (right-char))))
(global-set-key (kbd "C-<down>") 'next-paragraph)
(defun previous-paragraph ()
  (interactive)
  (left-char)
  (backward-paragraph)
  (unless (bobp)
    (progn
      (forward-paragraph)
      (redisplay t)
      (backward-paragraph)
      (right-char))))
(global-set-key (kbd "C-<up>") 'previous-paragraph)

;; following code is taken from adaptive-wrap package;
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
;(package-initialize)
;(require-package 'package-name)

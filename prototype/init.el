(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      scroll-bar-adjust-thumb-portion nil
      scroll-conservatively 200
      paragraph-start "\n"
      paragraph-separate "\n"
      make-backup-files nil)
(setq-default scroll-bar-width 8)
(add-to-list 'default-frame-alist '(left-fringe . 2))
(add-to-list 'default-frame-alist '(right-fringe . 2))
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
(set-face-attribute 'fixed-pitch-serif nil :font "Monospace")

;; when tree view is ready, and the modified files are marked there, there is no need for the mode line;
;(setq-default mode-line-format nil)

(desktop-save-mode 1)

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
;(require-package 'package_name)

(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      scroll-conservatively 200)
;; always load newest byte code;
(setq load-prefer-newer t)
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)
;; automatically refresh dired buffer on changes;
(add-hook 'dired-mode-hook 'auto-revert-mode)

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
(global-set-key (kbd "<next>") 'next-paragraph)
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
(global-set-key (kbd "<prior>") 'previous-paragraph)

;; following code is taken from adaptive-wrap package;
(defun adaptive-wrap-fill-context-prefix (beg en)
  "Like `fill-context-prefix', but with length 2."
  ;; Note: fill-context-prefix may return nil; See:
  ;; http://article.gmane.org/gmane.emacs.devel/156285
  (let* ((fcp (or (fill-context-prefix beg en) ""))
         (fcp-len (string-width fcp))
         (fill-char (if (< 0 fcp-len)
                        (string-to-char (substring fcp -1))
                      ?\ )))
    (concat fcp
            (make-string 2 fill-char))))

(defun adaptive-wrap-prefix-function (beg end)
  "Indent the region between BEG and END with adaptive filling."
  ;; Any change at the beginning of a line might change its wrap prefix, which
  ;; affects the whole line.  So we need to "round-up" `end' to the nearest end
  ;; of line.  We do the same with `beg' although it's probably not needed.
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
			   ;; Remove any `wrap-prefix' property that
			   ;; might have been added earlier.
			   ;; Otherwise, we end up with a string
			   ;; containing a `wrap-prefix' string
			   ;; containing a `wrap-prefix' string ...
			   (remove-text-properties
			    0 (length pfx) '(wrap-prefix) pfx)
			   pfx))))
  `(jit-lock-bounds ,beg . ,end))

;;;###autoload
(define-minor-mode adaptive-wrap-prefix-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  :group 'visual-line
  (if adaptive-wrap-prefix-mode
      (progn
        ;; HACK ATTACK!  We want to run after font-lock (so our
        ;; wrap-prefix includes the faces applied by font-lock), but
        ;; jit-lock-register doesn't accept an `append' argument, so
        ;; we add ourselves beforehand, to make sure we're at the end
        ;; of the hook (bug#15155).
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
(package-initialize)
;(require-package 'package-name)

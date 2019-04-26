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
;
;(global-set-key (kbd "<escape>") (lambda () (interactive) (modalka--maybe-activate)))
;(global-set-key (kbd "<tab>") (lambda () (interactive) (modalka--maybe-activate)))
;(define-key modalka-mode-map (kbd "RET")
;  (lambda ()
;    (interactive)
;    (modalka-mode -1)
;    (set-cursor-color "red")))

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

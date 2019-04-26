(require-package 'treemacs)
;; "^\\(\\.*\\)\\'\\|^target\\'|\\.lock\\'\\|^\\(\\#*\\)\\#\\'"
;; http://www.rexegg.com/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html

;;((file-directory-p file-name)
;; (cond
;;  (() ;; see if the directory contains a ".gallery" file;
;;   ;; open image-dired/movie in the right window
;;   ;; http://ergoemacs.org/emacs/emacs_view_image_thumbnails.html
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Image_002dDired.html
;;   ;; https://www.emacswiki.org/emacs/ThumbsMode
;;   ;; https://lars.ingebrigtsen.no/2011/04/12/emacs-movie-browser/
;;   ;; https://github.com/larsmagne/movie.el
;;   ;; http://www.mplayerhq.hu/DOCS/tech/slave.txt
;;   ;; https://www.gnu.org/software/emms/
;;   ;; http://wikemacs.org/wiki/Media_player
;;   ;; https://github.com/dbrock/bongo
;;   (hl-line-highlight)
;;   (select-window
;;    (display-buffer-use-some-window (find-file-noselect file-name) nil))
;;   (delete-other-windows)
;;   )
;;  (t
;;   ;; expand node in treemacs;
;;   )))

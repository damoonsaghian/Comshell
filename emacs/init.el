;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(tool-bar-mode -1)
;(menu-bar-mode -1)
;(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(font . "Hack-10"))
(setq-default inhibit-startup-screen 1)

(desktop-save-mode 1)
(setq-default desktop-restore-eager 5)
(global-visual-line-mode 1)
(global-hl-line-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "org-indent")
(global-org-indent-mode 1)


;;
;workspace = ""
;sway get_workspaces > $workspaces;
;if [[ $workspaces =~ '"name": $project_name' ]]
;then
;  sway workspace $project_name
;else
;  emacs -session... $project_name;
;  sway [workspace="$project_name"] kill;
;  sway get_workspaces > $workspaces;
;  if [[ $workspaces =~ '"name": $project_name' ]]
;  then
;    sway workspace prev
;  fi
;fi

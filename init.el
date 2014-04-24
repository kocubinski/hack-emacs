;; Now install el-get at the very first
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync '(cl-lib))
(add-to-list 'load-path "~/.emacs.d/el-get/cl-lib")
     
(setq el-get-git-shallow-clone t) 

(setq my-packages
    (append '(cl-lib evil color-theme solarized-theme yascroll auto-complete pos-tip helm)))

(el-get 'sync my-packages)
(evil-mode 1)

(color-theme-solarized-light)

(when (display-graphic-p)
   (if (eq system-type 'windows-nt) 
       (set-face-attribute 'default nil :font "Consolas-11")
     (set-face-attribute 'default nil :font "Inconsolata-15")))

(fringe-mode 4)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(yascroll-bar-mode 1)

(defun ome-auto-complete-setup ()
  (require 'auto-complete-config)

  (define-key ac-mode-map (kbd "M-/") 'ac-fuzzy-complete)
  (dolist (ac-mode '(text-mode org-mode))
    (add-to-list 'ac-modes ac-mode))
  (dolist (ac-mode-hook '(text-mode-hook org-mode-hook prog-mode-hook))
    (add-hook ac-mode-hook
              (lambda ()
                (setq ac-fuzzy-enable t)
                (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
                (add-to-list 'ac-sources 'ac-source-filename))))

  (ac-config-default))

(ome-auto-complete-setup)

(defun ome-helm-setup ()
  (require 'helm-config)
  (setq helm-input-idle-delay 0.2)
  (helm-mode t)
  (global-set-key (kbd "C-x c g") 'helm-do-grep)
  (global-set-key (kbd "C-x c o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(ome-helm-setup)

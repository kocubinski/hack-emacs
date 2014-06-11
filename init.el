;; Now install el-get at the very first
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; emacs 24.3 clib hack (from OME)
(el-get 'sync '(cl-lib))
(add-to-list 'load-path "~/.emacs.d/el-get/cl-lib")
     
;; quicker git clones in el-get
(setq el-get-git-shallow-clone t) 

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

;; load packages
(load-file "~/.emacs.d/init-packages.el")

(el-get 'sync my-packages)

;; of course we're evil.
(evil-mode 1)

;; gui ;;
;;;;;;;;;

;; set theme
(color-theme-solarized-dark)

;; set font
(when (display-graphic-p)
   (if (eq system-type 'windows-nt) 
       (set-face-attribute 'default nil :font "Consolas-11")
     (set-face-attribute 'default nil :font "Inconsolata-12")
     ;(set-face-attribute 'default nil :font "Monaco-12")
     ;(set-face-attribute 'default nil :font "Dejavu Sans Mono-11")
     ))

;; scroll bar
(fringe-mode 4)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(yascroll-bar-mode 1)

;; show parenthesis match
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; better duplicate buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(require 'uniquify)

;; just some powerline eye candy
(defun ome-powerline-setup ()
  (powerline-default-theme))

(ome-powerline-setup)

(blink-cursor-mode -1)

(setq backup-directory-alist `(("." . "~/.saves")))

(require 'org)

;; keybinds ;;
;;;;;;;;;;;;;;

(global-set-key [(f10)] 'ido-switch-buffer)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)

;; regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-,") 'other-window)

;; default is C-j for newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; core ;;
;;;;;;;;;;

;; TODO: look through additional "defaults" at:
;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-basic.org

;; flycheck (aka "Flymake done right")

(require 'flycheck)
(defun ome-flycheck-setup ()
  (eval-after-load 'flycheck
    '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
  (add-hook 'prog-mode-hook 'flycheck-mode))
(ome-flycheck-setup)

;; upgrade from 80, geez!
(setq-default fill-column 100)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; save place in open files
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".saved-places"))
(require 'saveplace)

;; completion ;;
;;;;;;;;;;;;;;;;

;; better hippie expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; auto-complete
(defun ome-auto-complete-setup ()
  (require 'auto-complete-config)

  (define-key ac-mode-map (kbd "M-/") 'ac-fuzzy-complete)
  (dolist (ac-mode '(text-mode org-mode))
    (add-to-list 'ac-modes ac-mode))
  (dolist (ac-mode-hook '(text-mode-hook org-mode-hook prog-mode-hook web-mode-hook))
    (add-hook ac-mode-hook
              (lambda ()
                (setq ac-fuzzy-enable t)
                (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
                (add-to-list 'ac-sources 'ac-source-filename))))

  (ac-config-default))
(ome-auto-complete-setup)

;; helm!!
(defun ome-helm-setup ()
  (require 'helm-config)
  (setq helm-input-idle-delay 0.2)
  (helm-mode t)
  (global-set-key (kbd "C-x c g") 'helm-do-grep)
  (global-set-key (kbd "C-x c o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf))
(ome-helm-setup)

;; yasnippet
(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t)))

(defun ome-yasnippet-setup ()
  (setq yas-prompt-functions
        '(yas-popup-isearch-prompt
          yas-no-prompt))
  (yas-global-mode)
  (add-to-list 'yas/root-directory "~/.emacs.d/el-get/yasnippet-snippets/"))
(ome-yasnippet-setup)

;; misc ;;
;;;;;;;;;;

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-log-done 'time)

(require 'rainbow-delimiters) ; shouldn't need this.. ?
(global-rainbow-delimiters-mode)

(auto-compression-mode t)
(auto-image-file-mode t)
(global-visual-line-mode t)

;; projectile project management
(defun ome-projectile-setup ()
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (global-set-key (kbd "C-x c h") 'helm-projectile))

(ome-projectile-setup)

;; TODO lots more at: (parens, CSS hacks)
;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-miscs.org

;; XML

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; javascript ;;
;;;;;;;;;;;;;;;;
(require 'tern)
(require 'skewer-mode)
(require 'skewer-repl)

(defun ome-js2-mode-setup ()
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2)))
  (setq js2-bounce-indent-p t)
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
(ome-js2-mode-setup)

(defun ome-tern-setup ()
  (when (el-get-package-installed-p 'js2-mode)
    (add-hook 'js2-mode-hook (lambda () (tern-mode t))))
  (when (el-get-package-installed-p 'js3-mode)
    (add-hook 'js3-mode-hook (lambda () (tern-mode t))))
  (setq tern-command (cons (executable-find "tern") '()))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup))))
(ome-tern-setup)

(defun ome-skewer-mode-setup ()
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))
(ome-skewer-mode-setup)

;; HTML/CSS ;;
;;;;;;;;;;;;;

(defun ome-web-mode-setup ()
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun ome-web-mode-hook ()
    ;; indentation
    ;; HTML offset indentation
    (setq web-mode-markup-indent-offset 2)
    ;; CSS offset indentation
    (setq web-mode-code-indent-offset 2)
    ;; Script offset indentation (for JavaScript, Java, PHP, etc.)
    (setq web-mode-css-indent-offset 2)
    ;; HTML content indentation
    (setq web-mode-indent-style 2)

    ;; padding
    ;; For <style> parts
    (setq web-mode-style-padding 1)
    ;; For <script> parts
    (setq web-mode-script-padding 1)
    ;; For multi-line blocks
    (setq web-mode-block-padding 0)
    (auto-complete-mode)
    (add-to-list 'ac-sources ac-css-classes-source))

  (add-hook 'web-mode-hook 'ome-web-mode-hook))

(ome-web-mode-setup)

(load-file "~/.emacs.d/web-mode-snippets.el")
(load-file "~/.emacs.d/ac-web.el")
(require 'ac-web)

(require 'rainbow-mode)

(defun ome-rainbow-mode-setup ()
  (add-hook 'css-mode-hook 'rainbow-mode))

(ome-rainbow-mode-setup)

;; clojure/lisp ;;
;;;;;;;;;;;;;;;;;;

(require 'paredit)
(require 'cider)
(require 'cider-macroexpansion)

(defun ome-cider-setup ()
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (setq cider-repl-tab-command 'indent-for-tab-command)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-popup-stacktraces t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-select-error-buffer t)
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-buffer-name-separator "-")
  (setq nrepl-buffer-name-show-port t))
(ome-cider-setup)

(defun ome-ac-nrepl-setup ()
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-repl-mode)))
(ome-ac-nrepl-setup)

;; try to speed up autocomplete
;; (defun clojure-auto-complete ()
;;   (interactive)
;;   (let ((ac-sources
;;          `(ac-source-nrepl-ns
;;            ac-source-nrepl-vars
;;            ac-source-nrepl-ns-classes
;;            ac-source-nrepl-all-classes
;;            ac-source-nrepl-java-methods
;;            ac-source-nrepl-static-methods
;;            ,@ac-sources)))
;;   (auto-complete)))

(add-hook 'clojure-mode-hook 
	  (lambda ()
	    (paredit-mode)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'cider-repl-mode-hook 'remove-dos-eol) ;Remove ^M from clojure repl in windows

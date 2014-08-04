(require 'cider)

(defvar dev-dir "C:/dev")

(defun legitipet-start-website ()
  (cider-switch-to-repl-buffer)
  (insert "(start-website)")
  (cider-repl-return)
  (remove-hook 'nrepl-connected-hook 'legitipet-start-website))

(defun legitipet-open ()
  (interactive)
  ;(add-hook 'nrepl-connected-hook 'legitipet-start-website)
  (find-file (concat dev-dir "/legitipet/project.clj"))
  (cider-jack-in))

(defun legitipet-start-cljs ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(user/cljs-browser-repl)")
  (cider-repl-return))

(require 'find-file-in-project)

(defvar css-regexp "\\.\\(-?[_a-zA-Z]+[_a-zA-Z0-9-]*\\)\\s-*{")

(defun parse-css-class-names ()
  (make-local-variable 'ffip-patterns)
  (setq ffip-patterns '("*.css"))

  (let (matches)
    (dolist (f (mapcar 'cdr (ffip-project-files)))
      (with-temp-buffer
	(insert-file-contents f)
	(save-match-data
	  (while (re-search-forward css-regexp nil t)
	    (setq matches (cons (match-string 1) matches))))))
    (remove-duplicates matches)))

(defvar ac-css-classes-cache nil)

(defun ac-css-classes-init ()
  (setq ac-css-classes-cache (parse-css-class-names)))

(defvar ac-css-classes-source
  '(;;(init . ac-css-classes-init)
    (candidates . ac-css-classes-cache)
    (cache)
    (limit . 20)
    ))

(provide 'ac-web)

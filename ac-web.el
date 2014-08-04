(require 'find-file-in-project)

(defvar acw-css-regexp "\\.\\(-?[_a-zA-Z]+[_a-zA-Z0-9-]*\\)\\s-*{")

(defun acw-parse-css-class-names ()
  (make-local-variable 'ffip-patterns)
  (setq ffip-patterns '("*.css"))

  (let (matches)
    (dolist (f (mapcar 'cdr (ffip-project-files)))
      (with-temp-buffer
	(insert-file-contents f)
	(save-match-data
	  (while (re-search-forward acw-css-regexp nil t)
	    (setq matches (cons (match-string 1) matches))))))
    (remove-duplicates matches)))

(defvar ac-css-classes-cache nil)

(defun ac-css-classes-init ()
  (setq ac-css-classes-cache (acw-parse-css-class-names)))

(defvar ac-css-classes-source
  '(;;(init . ac-css-classes-init)
    (candidates . acw-parse-css-class-names)
    (cache)
    (limit . 20)
    ))

(provide 'ac-web)

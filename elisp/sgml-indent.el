;;
;; sgml-indent.el
;;
;; Defines a simple indentation routine for editing sgml or other
;; derived file types.

(require 'sgml-mode)

(defun sgml-indent-setup () 
  (interactive) 
  (setq indent-tabs-mode nil)
  (setq indent-line-function 'sgml-indent)
  (local-set-key "\t" 'sgml-indent)
  (local-set-key "\^M" 'newline-and-indent)
  (local-set-key ">" 'sgml-electric-bracket)
  )
(add-hook 'sgml-mode-hook 'sgml-indent-setup)

(defvar sgml-indent-level 3
  "Number of spaces to increase indentation for nested sgml tags")

(defun sgml-electric-bracket ()
  (interactive)
  (save-excursion
    (insert ">")
    (indent-according-to-mode)
  )
  (forward-char 1))

(defun sgml-indent () 
  (interactive)
  (back-to-indentation)
  (let ((indent-point (point))
	(indent-column 0))
    ;; Find the previous tag
    (when (re-search-backward "^\\([ \t]*\\)<" nil t)
      (let ((prev-indent-point (match-end 1))
	    (prev-indent-column nil)
	    )
	(goto-char prev-indent-point)
	(setq prev-indent-column (current-column))

	;; debug
	(save-excursion
	  (setq beginning (point))
	  (end-of-line)
	  (message "checking %s" (buffer-substring beginning (point))))
	
	;; match the tag and any attributes
	(cond
	 ((re-search-forward "<[^/][^> \t]*[ \t]*\\([a-zA-Z_-]*=\".*\"[ \t\n\r]*\\)*"
			    indent-point t)
	  (message (format "tag matched \"%s\"" (match-string 0)))
	  (message (format "looking-at returned %s" (looking-at ">.*-->")))
	  (goto-char (match-end 0))
	  (cond
	   ((or
	     (looking-at "/>")
	     (looking-at ">.*</[^ \t]*>")
	     (and (string-equal (match-string 0) "<!-- ")
		  (looking-at ".*-->")))
	    
	    (message "Found self-closed tag")
	    ;; self-closed tag or comment - indent to same level as previous tag
	    (setq indent-column prev-indent-column)
	    )
	   ((looking-at ">")
	    (message "Found open tag")
	    ;; opened tag: increase indent
	    (setq indent-column (+ prev-indent-column sgml-indent-level))
	    )
	   (t
	    ;; incomplete tag - indent to same level as first attribute
	    (message "Found incomplete tag")
	    (goto-char prev-indent-point)
	    (re-search-forward "<[^ \t]*[ \t]*")
	    (goto-char (match-end 0))
	    (setq indent-column (current-column))
	    )))

	 ((looking-at "</[^ \t]*>")
	  ;; close tag - indent to same level as the tag
	  (message "Found close tag")
	  (setq indent-column prev-indent-column)
	  )))
      )
      
    ;; now need to check if the current line is a close tag
    (goto-char indent-point)
    (if (looking-at "</[^ \t]*>")
	(setq indent-column (- indent-column sgml-indent-level)))
      
    (message (format "indenting to column %d" indent-column))
    (setq indent-column (max indent-column 0))
    (if (< indent-column (current-column))
	(delete-char (- indent-column (current-column)))
      (indent-to-column (max indent-column 0))
      )))

(provide 'sgml-indent)

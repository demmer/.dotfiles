;;
;; sgml-indent.el
;;
;; Defines a simple indentation routine for editing sgml or other
;; derived file types.
;; 
;; KNOWN BUGS:
;;
;; - Doesn't correctly deal with multiple close tags on the same line:
;;   <mytag1/> </myenclosingtag> is interpreted as a self-closing tag
;;   and so the next line will keep the same indent level instead of
;;   popping up one level.


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
  "Function to attempt to indent sgml (or any derived form) in a
reasonable way. Increases the indentation level by sgml-indent-level
spaces at each open tag."
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
	  (sgml-indent-debug-message "checking %s" (buffer-substring beginning (point))))
	
	;; match the tag and any attributes
	(cond
	 ((re-search-forward "<[^/> \t]*[ \t]*\\([a-zA-Z_-]*=\".*\"[ \t\n\r]*\\)*"
			    indent-point t)
	  (sgml-indent-debug-message (format "tag matched \"%s\"" (match-string 0)))
	  (sgml-indent-debug-message (format "looking-at returned %s" (looking-at ">.*-->")))
	  (goto-char (match-end 0))
	  (cond
	   ((or
	     (looking-at "/>")
	     (looking-at ">.*</[^ \t]*>")
	     (and (string-equal (match-string 0) "<!-- ")
		  (looking-at ".*-->")))
	    
	    (sgml-indent-debug-message "Found self-closed tag")
	    ;; self-closed tag or comment - indent to same level as previous tag
	    (setq indent-column prev-indent-column)
	    )
	   ((looking-at ">")
	    (sgml-indent-debug-message "Found open tag")
	    ;; opened tag: increase indent
	    (setq indent-column (+ prev-indent-column sgml-indent-level))
	    )
	   (t
	    ;; incomplete tag - indent to same level as first attribute
	    (sgml-indent-debug-message "Found incomplete tag")
	    (goto-char prev-indent-point)
	    (re-search-forward "<[^ \t]*[ \t]*")
	    (goto-char (match-end 0))
	    (setq indent-column (current-column))
	    )))

	 ((looking-at "</[^ \t]*>")
	  ;; close tag - indent to same level as the tag
	  (sgml-indent-debug-message "Found close tag")
	  (setq indent-column prev-indent-column)
	  )))
      )
      
    ;; now need to check if the current line is a close tag
    (goto-char indent-point)
    (if (looking-at "</[^ \t]*>")
	(setq indent-column (- indent-column sgml-indent-level)))
      
    (setq indent-column (max indent-column 0))
    (sgml-indent-debug-message (format "indenting to column %d" indent-column))
    (if (< indent-column (current-column))
	(save-excursion
	  (beginning-of-line)
	  (delete-region (point) indent-point)))
    (indent-to-column indent-column)
    ))

;; Debugging support
(defvar sgml-indent-debug nil)

(defun sgml-indent-debug ()
  "Toggles debug printing"
  (interactive)
  (if (eq sgml-indent-debug nil)
      (setq sgml-indent-debug t)
    (setq sgml-indent-debug nil)))

(defun sgml-indent-debug-message (&rest args)
  (if (not (eq sgml-indent-debug nil))
      (eval (cons 'message args))
    ))

      

(provide 'sgml-indent)

(provide 'tex-stuff)

(defconst Latex-closer (concat (regexp-quote "\\end") "{[^}]*}"))
(defconst Latex-opener (concat (regexp-quote "\\begin") "{[^}]*}"))
(defconst Latex-blocker
  (concat "\\\(" Latex-opener "\\\)\\\|\\\(" Latex-closer "\\\)"))
(modify-syntax-entry  ?{ "(}" latex-mode-syntax-table )
(modify-syntax-entry  ?} "){" latex-mode-syntax-table )

(defun latex-close-block () (interactive) 
    (let ((block (save-excursion (latex-find-block 0))) )
        (if (or (not block) (string-equal block "document"))
	    (error "No matching block found."))
        (if (not (bolp)) (newline))
        (insert-string (concat "\\end{" block "}"))
        (if (not (eolp)) (newline)))
)
 
(defun latex-find-block (depth)
      (if (re-search-backward Latex-blocker 0 t) 
	  (progn
	    (while (looking-at Latex-closer)
	      (latex-find-block (+ depth 1))
	      (re-search-backward Latex-blocker 0 t))
	    (if (and (= depth 0) (looking-at Latex-opener))
		(progn 
		  (search-forward "{")
		  (let ((first (point)))
		    (search-forward "}")
		    (backward-char)
		    (buffer-substring first (point))))
	        nil))
	  nil))

(defun current-tex-error () (interactive)
       (let ((buffer (current-buffer))
	     temp line)
           (switch-to-buffer-other-window "*TeX-shell*")
	   (end-of-buffer)
	   (if (re-search-backward "\nl\.[0-9]+" 0 t)
	       (progn (forward-char 3)
		      (setq temp (point))
		      (re-search-forward "[^0-9]") (backward-char)
		      (setq line (string-to-int
				  (buffer-substring temp (point))))
		      (if (re-search-backward "\n! " 0 t)
			  (progn
			    (forward-char))))
	       (error "No error found"))
	   (switch-to-buffer-other-window buffer)
	   (goto-line line)))
	   
(defvar latex-block-list '( ("enumerate" . nil)
			    ("list" . t)
			    ("abstract" . nil)
			    ("equation*" . nil)
			    ("eqnarray" . nil)
			    ("quote" . nil)
			    ("quotation" . nil)
			    ("center" . nil)
			    ("verse" . nil)
			    ("verbatim" .nil)
			    ("itemize" . nil)
			    ("description" . nil)
			    ("titlepage" . nil )
			    ("thebibliography" . nil)
			    ("figure" . nil)
			    ("tabular" .nil)
			    ("fig" . t )
			    ("Fig" . nil )
			    ("minipage" . t)
			    ("array" . t)
			    ("sblist" . nil)
			    ("blist" . nil)
			    ("dlist" . nil)
			    ("clist" . nil)
			    ("table" . t)) "List of known LaTeX blocks")
			    
(defun latex-begin-block () "Doc"  (interactive)
	(let ((block (completing-read "block: " latex-block-list nil 1)))
	  (if (not (eolp)) (progn (end-of-line) (newline)))
	  (insert "\\begin{" block "}")
	  (if (cdr (assoc block latex-block-list))
	      (progn (insert "[]") (backward-char)))))

(defun latex-declare-block (args block) "Add a block to latex-block-list"
  (interactive "p
sblock: ")
  (setq latex-block-list (cons (cons block
				     (= args 4)) latex-block-list)))

(defun number-slides () (interactive)
  (save-excursion
    (let ((count 1))
      (goto-char (point-min))
      (while (re-search-forward "\\\\begin{slide} *{\\([^}]*\\)}" nil t)
	(if (looking-at ".*% [0-9]*$")
	    (progn
	      (re-search-forward ".*%")
	      (kill-line))
	  (progn (end-of-line) (insert " %")))
	(insert " " (int-to-string count))
	(setq count (+ count 1))))))

(defun number-slides () (interactive)
  (save-excursion
    (let ((count 1))
      (goto-char (point-min))
      (while (re-search-forward "^ *\\\\begin{slide} *{\\([^}]*\\)}" nil t)
	(if (looking-at ".*% *[0-9]* *$")
	    (progn
	      (re-search-forward ".*%")
	      (kill-line))
	  (progn (end-of-line) (insert " %")))
	(insert " " (int-to-string count))
	(setq count (+ count 1))))))

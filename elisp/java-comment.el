;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  java-comment.el        msh     2/96
;;;
;;;  big thanks here to bmm and amd
;;;
;;;  defines java-comment routines for cs016 java emacs...
;;;  All java comment routines are compatible with javadoc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Some setup variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *java-comment-length* 60)
(defvar *java-functab-length* 4)
(defvar *java-commenting* 0)




;;;  Draws a header line 
(defun java-header-line ()
  (insert "/*")
  (insert-char ?* (- *java-comment-length* 4))
  (insert "*/")
  (newline))



;;;  A new comment line
(defun java-body-line ()
  (insert " * ")
  (newline))



;;;  Increases a comment box by one line
(defun java-box-increase ()
  (interactive)
;;  (java-box-enter)
  (next-line 1)
  (beginning-of-line)
  (java-body-line)
  (previous-line 1)
  (beginning-of-line)
  (forward-char 3))
  



;;;  Exits comment mode
(defun java-box-exit ()
  (interactive)
  (let ((prev))
    (progn
      (setq *java-commenting* nil)
      (local-set-key "\015" 'newline-and-indent)
      (beginning-of-line)
      (setq prev (point))
      (forward-char 3)
      (let ((test (buffer-substring prev (point))))
	   (cond
	    ((string-equal test " * ")
		(search-forward "*/")
		(end-of-line)
		(forward-char 1))))
      (message "Out of comment mode...")
	 )))


;;;   Enters comment mode
(defun java-box-enter ()
  (interactive)
  (setq *java-commenting* 1)
  (local-set-key "\015" 'java-box-increase)
  (message "Commenting...: M-9 to toggle."))

(defun java-box-enterexit ()
  (interactive)
  (if *java-commenting* (java-box-exit) (java-box-enter)))

;;;  Finding the next parameter in a parameter list
(defun java-next-param ()
  (interactive)
  (let ((prev) (next))
    (progn
      (re-search-forward "[^ ^,^\n^\t^\r]")
      (backward-char 1)
      (setq prev (point))
      
      (re-search-forward "[,)\t\n\r]")
      (backward-char 1)
      (setq next (point))
      (buffer-substring prev next)
)))
  

;;;  Finding the next word
(defun java-next-word ()
  (interactive)
  (re-search-forward "[^ ^\t^\n^\r]")
  (backward-char 1)
  (let ((prev (point)))
    (re-search-forward "[ (,)\t\n\r]")
    (backward-char 1)
    (let ((next (point)))
      (buffer-substring prev next))))



(defun java-more-param ()
  (re-search-forward "[,)]")
  (backward-char 1)
  (let ((test-start (point)))
    (forward-char 1)
    (let ((test-end (point)))
      (let ((test (buffer-substring test-start test-end)))
	(re-search-backward "[,)]")
	(if (equal test ")") nil t)))))



  
;;;  Adds parameters to a function comment
(defun java-add-params ()
  (interactive)
  (let ((param) (mark))
    (progn
      (setq param (java-next-param))
      (setq mark (point))
      (cond
       ((eq (length param) 0)
	(search-backward " * ")
	(forward-line 1)
	(beginning-of-line))
       (t
	(search-backward "*")
	(forward-line 1)
	(insert (concat " * @param " param))
	(beginning-of-line)
	(forward-char 10)
	(let ((prev (point)) (type))
	  (re-search-forward "[ ]")
	  (setq type (- (point) prev))
	  (delete-region prev (point))
	  (java-next-word)
	  (newline)
	  (goto-char (- (+ mark (+ 11 (length param))) type)))
	(java-add-params)
	))
)))      



(defun java-fill-in-return ()
  (let ((mark) (prev) (funcname))
    (setq mark (point))
    (setq prev (point))
    (forward-word 1)
    (setq funcname (buffer-substring prev (point)))
    (search-backward "class")
    (forward-word 1)
    (forward-char 1)
    (setq prev (point))
    (forward-word 1)
    
    (message (concat "funcname = " funcname))
    (cond
     ((string-equal funcname (buffer-substring prev (point)))
      (goto-char mark)
      (java-blank-return))
    (t
     (goto-char mark)
     (search-backward "*")
     (end-of-line)
     (newline)
     (beginning-of-line)
     (insert " * @return ")))

))



;;;  Sorry return is void
(defun java-blank-return ()
  (search-backward "*")
  (end-of-line))



;;;  Adds a return line to a java function comment box.
(defun java-add-return ()
  (let ((test-return (java-next-word)))
    (cond
     ((string-equal test-return "void")
      (java-blank-return))
     ((or (string-equal test-return "private")
	  (or (string-equal test-return "protected")
	      (or (string-equal test-return "public")
			(or (string-equal test-return "final")
			    (or (string-equal test-return "abstract")
				   (or (string-equal test-return "static")
					  (or (string-equal test-return "native"))))))))
      (java-add-return))
     (t
      (backward-word 1)
      (java-fill-in-return)))))
	  


;;;   Creates a function comment box in the javadoc style
(defun java-function-comment ()
  (interactive)
  (java-header-line)
  (beginning-of-line)
  (insert "/** ")
  (newline)
  (beginning-of-line)
  (insert " * ")
  (newline)
  (insert " * ")
  (newline)
  (re-search-forward "[(,]")
  (java-add-params)
  (java-add-return)
  (newline)
  (insert " */")
  (forward-line 1)
  (insert " ")
  (delete-backward-char 1)
  (search-backward "/** ")
  (next-line 1)
  (forward-char 3)
  (java-box-enter))




;;   Creates an instance comment box.
(defun java-instance-comment ()
  (interactive)
  (beginning-of-line)
  (insert "/** ")
  (newline)
  (beginning-of-line)
  (insert " *  ")
  (newline)
  (insert " *  ")
  (newline)
  (insert " */")
  (newline)
  (search-backward "/**")
  (next-line 1)
  (beginning-of-line)
  (forward-char 3)
  (java-box-enter))


;;;   Creates a java header comment
(defun java-header-comment ()
  (interactive)
  (beginning-of-buffer)
  (insert "/*")
  (newline)

  (insert " * @(#)")
  (insert (file-name-nondirectory (expand-file-name
				   (buffer-file-name))))
  (insert "  ")
  (insert (current-time-string))
  (newline)
  (insert " */")
  (newline))
  



;;; Creates a java class comment box. 
(defun java-class-comment ()
  (interactive)
  (search-forward "class")
  (beginning-of-line)
  (previous-line 1)
  (newline)
  (java-header-line)
  (insert "/**")
  (newline)
  (insert " *   ")
  (newline)
  (insert " *   ")
  (newline)
  (insert " * @version   ")
  (insert (current-time-string))
  (newline)
  (insert " * @author ")
  (newline)
  (insert " */")
  (search-backward "/**")
  (next-line 1)
  (forward-char 3)
  (java-box-enter))

(defun java-insert-header ()
  "Inserts the header into a java file when created. [mjd]"
  (interactive)
  (let ((name 
	    (file-name-sans-versions buffer-file-name)))
    (if (string-match "\\.java$" name)	   (java-header-comment))))

(append-no-dup 'java-insert-header find-file-not-found-hooks)

;;;  Load the menu
;;(load "java-comment-menu")
(provide 'java-comment)





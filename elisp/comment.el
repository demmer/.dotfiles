;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Laurion Burchall (ldb@cs.brown.edu)
;; Modifed by amd for aesthetic reasons.
;; And more by mjd for time wasting reasons
;;

(provide 'c-file-header)

; amd: toss out a java comment
(defun java-comment ()
  (interactive)
  (beginning-of-line)
  (insert "/**")
  (newline)
  (insert " * ")
  (newline)
  (insert " * ")
  (newline)
  (insert " */")
  (newline)
  (previous-line 3)
  (forward-char 3))


(defun comment-member-class ()
  (interactive)
  (let ((prev) (next) (mark))
    (progn
      (beginning-of-line)
      (search-forward "class")
      (forward-char)
      (setq prev (point))
      (search-forward-regexp "[ \{\:\n]")
      (backward-char)
      (setq next (point))
      (let ((classname (buffer-substring prev next)))
	(beginning-of-line)
	(box-class-header)
	(insert classname)
	(delete-region (point) (+ (point) (length classname)))
	(forward-line 2)
	(body-line)
	(previous-line 1)
	(forward-char 3)
	)
)))      

(defun comment-any-function ()
  (interactive)
  (beginning-of-line)
  (let ((beg) (end))
    (setq beg (point))
    (search-forward "(")
    (setq end (point))
    (goto-char beg)
    (if (search-forward "::" end t)
	   (comment-member-function)
	 (comment-nonmember-function)
	 )))
		

(defun comment-nonmember-function ()
  (interactive)
  (let ((prev) (next))
    (progn
      (beginning-of-line)
      (search-forward "(")
	 (backward-char 2)
	 (while (not (current-word t))
		(backward-char 1))
      (search-backward-regexp "[ \n]")
	 (forward-char 1)
      (setq prev (point))
      (search-forward-regexp "[ (\n]")
      (setq next (- (point) 1))
      (let ((funcname (buffer-substring prev next)))
	   (beginning-of-line)
	   (backward-line)
	   (if (not (current-word t))
		  (forward-line))
	   (box-func-header)
	   (insert funcname)
	   (delete-region (point) (+ (point) (length funcname)))
	   (search-forward funcname)
	   (insert "::" );; temp to get the others to work right
	   )
      (comment-member-parameters)
	 (comment-member-return)
	 (search-forward "::")
	 (backward-char 2)
	 (delete-char 2)
	 (search-backward "/**")
	 (backward-line 2)
	 (forward-char 3)
	 
)))      
  

; amd: comments a member function, with auto insert of name
; mjd: modified to include parameter and return list
(defun comment-member-function ()
  "Assumes the cursor is placed on the first line of the
function declaration. Creates a comment block with the
given function name. [AMD]"
  (interactive)
  (let ((prev) (next))
    (progn
      (beginning-of-line)
      (search-forward "::")
      (search-backward-regexp "[ \n]")
      (goto-char (+ (point) 1))
      (setq prev (point))
      (search-forward-regexp "[ (\n]")
      (setq next (- (point) 1))
      (let ((funcname (buffer-substring prev next)))
	(beginning-of-line)
	(backward-line)
	(if (not (current-word t))
	    (forward-line))
	(box-func-header)
	(insert funcname)
	(delete-region (point) (+ (point) (length funcname)))
	(search-forward "::")
	)
      (comment-member-parameters)
      (comment-member-decide-type)

)))      

;;tkl: changes the Function Name field to Constructor or Destructor,
;;     depeding on which it is.
(defun comment-member-decide-type ()
  "Assumes the cursor is placed on the first line of the function
declaration. Changes the comment block to reflect if the function
is a constructor or a destructor. Also calls comment-member-return
to comment the return type if there is one. [TKL]"
  (interactive)
  (let ((classtart) (classend) (funstart) (funend) (type) (id))
    (progn
      (beginning-of-line)
      (setq classtart (point))
      (search-forward "::")
      (goto-char (- (point) 2))
      (setq classend (point))
      (let ((classname (buffer-substring classtart classend)))
	(goto-char (+ (point) 2))
	(setq funstart (point))
	(search-forward "(")
	(re-search-backward "[a-zA-Z0-9^\n^\t^\r]")
	(forward-char 1)
	(setq funend (point))
	(let ((funname (buffer-substring funstart funend)))
	  (cond
	   ((string-equal classname funname) 
	    (setq type "Constructor"))
	   ((string= "~" (substring funname 0 1))
	    (setq type "Destructor"))
	   (t
	    (setq type "neither")))
	(cond
	 ((not (string= type "neither"))
	    (search-backward "Name:")
	    (beginning-of-line)
	    (forward-char 16)
	    (insert type)
	    (beginning-of-line)
	    (goto-char (+ (point) 3))
	    (delete-region (point) (+ (point) 13))
	    (correct-comment-line)
	    (search-forward "Returns:")
	    (beginning-of-line)
	    (kill-line)
	    (kill-line)
	    (box-increase)
	    ) 
	   (t
	    (beginning-of-line)
	    (comment-member-return))) ;;comments return type
)))))	     




;mjd: lists parameters in comment block
(defun comment-member-parameters ()
  "Assumes the cursor is placed on the first line of the
function declaration. Adds the parameters to the comment
block. [MJD]"
      (beginning-of-line)
      (re-search-forward "[(,]")
      (comment-member-add-params)
)

 
(defun comment-member-add-params ()
  (let ((param) (mark))
    (progn
      (setq param (comment-member-next-param))
      (setq mark (point))
      (search-backward "Returns: ")
      (beginning-of-line)
      (previous-line 1)
      (cond
       ((eq (length param) 0)
	(kill-line)
	(kill-line)
	(search-forward "::")
	(beginning-of-line)
	)
       (t
	(forward-char 15)
	(insert param)
	(delete-region (point) (+ (point) (length param)))
	(box-increase)
	(search-forward "::")
	(beginning-of-line)
	(goto-char (+ mark *comment-length*))
	(forward-char)
	(comment-member-add-params)
	
	))
)))
    
;mjd: this gets the next parameter

(defun comment-member-next-param ()
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

;mjd: this adds the return variable to the comment

(defun comment-member-return ()
  "Assumes the cursor is placed on the line containing the function
  definition. Adds the return type to the comment block. [MJD]"
  (interactive)
  (let ((prev) (next) (return))
    (progn
	 (search-forward "::")
;	 (backward-word 2)
	 (re-search-backward "[ \t\r\n]")
	 (re-search-backward "[ \t\r\n]")
	 (forward-char 1)
;	 (backward-word)
      (setq prev (point))
      (re-search-forward "[ \t\n\r]")
      (backward-char 1)
      (setq next (point))
      (setq return (buffer-substring prev next))
	 (message return)
	 (cond ((string-equal return "Returns:")
		   (setq return "int (default)")
		   (backward-word 1)
		   )
		  (t (search-backward "Returns: ")))
	 (forward-char 9)
	 (insert return)
	 (delete-region (point) (+ (point) (length return)))
	 (forward-line 1)
	 (box-increase)
	 )
    ))  

;; amd: This is how long the comment lines should be
(defvar *comment-length* 75)

;; amd: fixes the end of one comment line, which can get screwed
;;      up by insert/delete
(defun correct-comment-line ()
  "Fixes the comment end marker on the current line. [amd]"
  (interactive)
  (end-of-line)
  (search-backward "*/")
  (let ((movement (- (- *comment-length* 2) (current-column))))
    (if (> movement 0)
	(insert (format (format "%%-%ds" movement) ""))
      (backward-delete-char (* -1 movement)))))

;; amd: inserts an empty comment box
(defun box-empty ()
  (interactive)
  (newline)
  (header-line)
  (body-line) 
  (body-line) 
  (body-line) 
  (body-line) 
  (header-line)
  (box-enter)
  (previous-line 5)
  (forward-char 3)
  (message "Press M-9 to toggle overwrite-mode.."))
   
;; insert a solid comment line
(defun header-line ()
  (insert "/*")
  (insert-char ?* (- *comment-length* 4))
  (insert "*/")
  (newline))

;; insert a comment line filled with spaces
(defun body-line ()
  (insert "/*")
  (insert-char ?\040 (- *comment-length* 4))
  (insert "*/")
  (newline))

(defun box-done ()
  "[cs032] Finish up the box comment."
  (interactive)
  (search-forward "--- */")
  (end-of-line)
  (forward-char 1)
  (box-exit))

(defun box-delete ()
  "[cs032] Delete key in a box comment."
  (interactive)
  (backward-char 1)
  (delete-char 1)
  (insert " ")
  (backward-char 1))

(defvar box-in-or-out nil)

(defun box-enterexit ()
  "[cs032] Either enter or exit a box comment (toggle)"
  (interactive)
  (if box-in-or-out (box-exit) (box-enter)))

(defun box-increase ()
  "[cs032] Increase the size of a box comment."
  (interactive)
  (correct-comment-line)
  (box-enter)
  (overwrite-mode 0)
  (next-line 1)
  (beginning-of-line)
  (body-line)
  (previous-line 1)
  (beginning-of-line)
  (forward-char 3)
  (overwrite-mode 1))

(defun insert-comment-string (prefix string)
  "[ldb] inserts prefix - then the string and finishes the comment"
  (let ((prefix-length (length prefix))
	(string-length (length string)))
    (insert prefix)
    (if (not (null string))
	(insert string))
    (insert-char ? (- *comment-length* (+ prefix-length string-length 2)))
    (insert "*/")))

(defun box-enter ()
  "[cs032] Enter a box comment"
  (interactive)
  (auto-fill-mode t)
  (setq box-in-or-out t)
  (local-set-key "" 'newline)
  (set-cursor-color "medium spring green")
  (set-background-color "gray20")
  (setq fill-paragraph-function nil)
  (cond
  (t
    (define-key global-map [f1] 'box-done)
  (message "In the comment...: M-9 to toggle."))))

(defun box-exit ()
  "[cs032] Exit a box comment"
  (interactive)
  (auto-fill-mode nil)
  (setq box-in-or-out nil)
  (local-set-key "" 'newline-and-indent)
  (set-cursor-color "yellow")
  (set-background-color "black")
  (setq fill-paragraph-function 'c-fill-paragraph)
  (cond
   (t
    (define-key global-map [f1] 'box-comment-1)
  (message "Out of the comment..."))))

(defun box-func-header ()
  "[cs032] Make my function header just the way I like it."
  (interactive)
  (header-line)
  (insert-comment-string "/* Function Name:" nil)
  (newline)
  (insert-comment-string "/* Parameters:" nil)
  (newline)
  (insert-comment-string "/* Returns:" nil)
  (newline)
  (body-line)
  (body-line)
  (header-line)
  (box-enter)
  (previous-line 6)
  (beginning-of-line)
  (forward-char 18)
  (message "Press M-9 to toggle overwrite-mode.."))


(defun box-class-header ()
  "[cs032] Make my class header just the way I like it."
  (interactive)
  (newline)
  (header-line)
  (insert-comment-string "/* Class Name : " nil)
  (newline)
  (body-line) 
  (body-line)
  (header-line)
  (box-enter)
  (previous-line 4)
  (forward-char 17)
  (message "Press M-9 to toggle overwrite-mode.."))


(defun box-myname-header ()
  "[ldb] Make my file header comment just the way I like it."
  (interactive)
  (header-line)
  (insert-comment-string "/*    NAME: " (getenv "NAME"))
  (newline)
  (insert-comment-string  "/*    ACCT: " (getenv "USER"))
  (newline)
  ;; I guess the nodirectory-expand-buffername method gives
     ;; the best chance of getting the filename
	(insert-comment-string "/*    FILE: " (file-name-nondirectory
					       (expand-file-name 
						(buffer-file-name))))
	(newline)
	(if (getenv "ASSIGNMENT")
	    (progn
		 (insert-comment-string "/*    ASGN: " (getenv "ASSIGNMENT"))
		 (newline)
		 ))
	(insert-comment-string "/*    DATE: " (current-time-string))
	(newline)
	(header-line)
	(newline)
	(newline))


;; c-file-header inserts a header into our .C  files
;; the header file associated with the .C is included automatically
(defun c-header-stuff ()
  "[ldb] Insert a header into a .C file"
  (box-myname-header)
  (newline)
  (let ((def-name (concat 
		   (substring (file-name-nondirectory buffer-file-name) 0
			      (- 
			       (length 
				(file-name-nondirectory buffer-file-name)) 
			       2))
		   ".H\"\n\n")))
    (insert (concat "#include \"" def-name))))

;; h-file-header inserts a header into our .H files
;; the appropriate #ifdefs are put in automatically
(defun h-header-stuff ()
  "[ldb] Insert a header into a .H file"
  (box-myname-header)
  (let ((def-name (concat (upcase 
			   (substring (file-name-nondirectory buffer-file-name) 0
				      (- (length (file-name-nondirectory buffer-file-name)) 2)))
			  "_HEADER")))
    (insert (concat "#ifndef " def-name))
    (insert (concat "\n#define " def-name))
    (insert "\n\n\n\n#endif\n")
    (forward-line -3)))

;; This inserts the appropriate header into a .C or .H file
(defun c-file-header ()
  "[ldb] Inserts a header into a C file containing the program name,
user and a sccs header"
  (interactive)
  (let ((user (getenv "USER"))
	(file buffer-file-name))
    ;; deal with inserting into buffers with no files associated with them
       (if (= (length file) 0)
	   (setq file "UNKNOWNfile.C")
	 (setq file (file-name-nondirectory file)))
       ;; enter c-mode if we're not there already
	  (if (not (string= major-mode "c++-mode"))
	      (c++-mode))
	  ;; now we decide if we want to insert a .h header or a .c header
	     (if (or (string-match "\\.h$" file)
		     (string-match "\\.H$" file))
		 (h-header-stuff)
	       (c-header-stuff))))

;; auto-insert-header checks to see if the file it is visiting is a .C or .H 
;; file. If it is it calls c-file-header.
(defun auto-insert-header ()
  "[ldb] Inserts the header into a C file (.C or .H) whenever one is created"
  (let ((name 
	 (file-name-sans-versions buffer-file-name)))
    (if (or (string-match "\\.c$" name)
	    (string-match "\\.C$" name)
	    (string-match "\\.CPP$" name)
	    (string-match "\\.cpp$" name)
	    (string-match "\\.H$" name)
	    (string-match "\\.h$" name))
	(c-file-header))))


 ;; this sets auto-insert-header to be called whenever a new file is created
(add-hook 'auto-insert-header find-file-not-found-hooks)

(defun uncomment-region ()
  "Stupid uncommenting procedure"
  (interactive)
  (comment-region (region-beginning) (region-end) -1))

(defvar CH-buffer-pairs)

(setq CH-buffer-pairs '((".cc" ".h")
			(".c" ".h")
			(".C" ".H")
			(".tcc" ".h")
			(".tcl" ".cc")
			("hpp" "cpp")
			("M.nc" ".nc")
 			("M.nc" "C.nc")
			(".cpp" ".hpp")
			(".cpp" ".h")
			))
(require 'cl)
(defun CH-buffer-match (name)
  "Matches from CH-buffer pairs, and returns a list of modified buffer
names. e.g. if passed buffer.C, will return buffer.H and vice versa.
Uses CH-buffer-pairs to get the pairs of filename extensions. Matches
buffer copy number as well, so buffer.c<2> will return buffer.h<2>"

  (let ((ver) (pt) (names))
    ;; strip out the version first, either in <2> or , <dir> style
    (setq pt (string-match "[<,]" name))
    (cond
     (pt
      (setq ver (substring name pt (length name)))
      (setq name (substring name 0 pt))
      )
     (t
      (setq ver nil)
      ))

    (setq names (mapcar
		 (lambda (pair) "" nil
		   (let ((regexp1
			  (concat "^\\(.*\\)" (regexp-quote (car pair)) "$"))
			 (regexp2
			  (concat "^\\(.*\\)" (regexp-quote (cadr pair)) "$")))
		     (cond
		      ((string-match regexp1 name)
		       (concat (match-string 1 name) (cadr pair)))
		      ((string-match regexp2 name)
		       (concat (match-string 1 name) (car pair)))
		      (t nil))
		     ))
		 CH-buffer-pairs))
    
    (setq names (remove-if #'null names))
    
    (if (and names ver)
	(mapcar (lambda (name) "" nil 
		  (concat name ver)) 
		names))
    names
    )
  )

(defun switch-CH-buffer ()
  "Switches to an open buffer or finds the file according to the settings
in CH-buffer-pairs. Originally written by amd, rewritten by mjd to add the
pair table functionality. If the buffer is of the form buffer.C<2>, first
looks in the buffer list for the corresponding buffer.H<2> or looks in the
current directory for buffer.H.[mjd]"
  (interactive)
  (let ((targetlist (CH-buffer-match (buffer-name)))
	(bufferlist (buffer-list))
	(matches) (match) (pos) (file) (files))
    
    ;; Try to find an open buffer that matches a name from the list
    (setq matches
	  (mapcar 
	   (lambda (targetname) "" nil
	     (mapcar 
	      (lambda (buffer) "" nil
		(if (string= targetname (buffer-name buffer))
		    buffer
		  nil)
		) bufferlist)
	     ) targetlist)
	  )

    ;; grab the first non-null match
    ;; XXX this is lame but i can't for the life of me figure out how
    ;; to join a list of lists together
    (setq match (car (car
		      (remove-if #'null 
				 (mapcar (lambda (list) "" nil
					   (remove-if #'null list))
					 matches)))))
    (cond
     (match (switch-to-buffer match))
     (t
      ;; Otherwise build a list of candidate files
      (setq files
	    (mapcar
	     (lambda (targetname) "" nil
	       (setq pos (string-match "\<" targetname))
	       (if pos (setq targetname (substring targetname 0 pos)))
	       (concat (file-name-directory (buffer-file-name)) targetname))
	     targetlist))
      
      ;; See if one exists
      (setq file (car (remove-if #'null (mapcar (lambda (filename) "" nil
						  (if (file-exists-p filename)
						      filename)
						  )
						files))))

      ;; If one exists, or the user wants one, open it, if not, see if
      ;; we want it to be created
      (if file (find-file file)
	(if (and (car matches) (y-or-n-p "Create new file? "))
	    (find-file (read-from-minibuffer "Filename: " (car files)))
	  (message "Not a switchable buffer")))
      )
    )))

(defun open-include ()
  "Searches forward and backward for quotes, and then tries to
open the file which matches the enclosed text. [AMD]"
  (interactive)
  (let ((prev) (next))
    (progn
      (beginning-of-line)
      (search-forward "\"")
      (setq prev (point))
      (search-forward "\"")
      (setq next (- (point) 1))
      (find-file (buffer-substring prev next)))))

; amd: override function-menu
(defun mouse-buffer-sort-by-name (item1 item2)
  (string-lessp (upcase (car item1))
		(upcase (car item2))))

(defun mouse-buffer-menu (event)
  "Pop up a menu of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((menu
	 (list "Buffer Menu"
	       (cons "Select Buffer"
		     (sort
		      (let ((tail (buffer-list))
			    (maxbuf 0)
			    head)
			(while tail
			  (or (eq ?\ (aref (buffer-name (car tail)) 0))
			      (setq maxbuf
				    (max maxbuf
					 (length (buffer-name (car tail))))))
			  (setq tail (cdr tail)))
			(setq tail (buffer-list))
			(while tail
			  (let ((elt (car tail)))
			    (if (not (string-match "^ "
						   (buffer-name elt)))
				(setq head (cons
					    (cons
					     (format
					      (format "%%-%ds  %%s%%s  %%s"
						      maxbuf)
					      (buffer-name elt)
					      (if (buffer-modified-p elt)
						  "*" " ")
					      (save-excursion
						(set-buffer elt)
						(if buffer-read-only "%" " "))
					      (or (buffer-file-name elt) ""))
					     elt)
					    head))))
			  (setq tail (cdr tail)))
		      (reverse head))
			'mouse-buffer-sort-by-name)))))
    (let ((buf (x-popup-menu event menu))
          (window (posn-window (event-start event))))
      (if buf
          (progn
            (or (framep window) (select-window window))
            (switch-to-buffer buf))))))

; amd: set a tab every N chars
(defun set-tab-stops (tab-size)
  "Set tab stops every ARG characters. [AMD]"
  (interactive "nTab size: ")
  (setq tab-stop-list (list))
  (let ((loop tab-size))
    (while (< loop 100)
      (setq tab-stop-list
	    (append tab-stop-list (list loop)))
      (setq loop (+ loop tab-size)))))

; amd: all of this crap extends regions, Macintosh style.
;      for example, select some text and then use shift-arrows to
;      make the selection bigger.
;
;      recommended keys:
;          shift-left/right char
;          shift-up/down line
;          meta-shift-left/right word
;          meta-shift-up/down para

(defun shift-region (from to)
  (goto-char from)
  (transient-mark-mode t)
  (push-mark)
  (set-mark (point))
  (goto-char to)
  (let (this-command)
      (copy-region-as-kill (mark) (point))))

(defun extend-region-left (from to)
  (interactive "r")
  (shift-region (- from 1) to))

(defun extend-region-right (from to)
  (interactive "r")
  (shift-region from (+ to 1)))

(defun extend-region-up (from to)
  (interactive "r")
  (goto-char from)
  (forward-line -1)
  (shift-region (point) to))

(defun extend-region-down (from to)
  (interactive "r")
  (goto-char to)
  (forward-line 1)
  (shift-region from (point)))

(defun extend-region-word-left (from to)
  (interactive "r")
  (goto-char from)
  (forward-word -1)
  (shift-region (point) to))

(defun extend-region-word-right (from to)
  (interactive "r")
  (goto-char to)
  (forward-word 1)
  (shift-region from (point)))

(defun extend-region-para-up (from to)
  (interactive "r")
  (goto-char from)
  (forward-paragraph -1)
  (shift-region (point) to))

(defun extend-region-para-down (from to)
  (interactive "r")
  (goto-char to)
  (forward-paragraph 1)
  (shift-region from (point)))

(defun backward-line (&optional num)
  (interactive)
  (forward-line (if num (- num) -1)))

(defun revert-buffer-at-point ()
  "Revert buffer, without prompting, and return the cursor to the
   current position in the file. [mjd]"
  (interactive)
  (let ((old-pt (point)))
    (revert-buffer t t)
    (goto-char old-pt)
    ))

(setq sentence-end-double-space nil)
(setq garbage-collection-messages t)

(defun toggle-and-chmod-read-only () 
  (interactive)
  (let ((file (buffer-file-name)))
    (let ((wr (file-writable-p file)))
      (if wr
	  (shell-command (format "/bin/chmod -w %s" file))
	(shell-command (format "/bin/chmod +w %s" file)))
      (toggle-read-only wr)
      )
    ))

(defun rm-ctrl-ms ()
  (interactive)
  (let ((old-pt (point)))
    (goto-char (point-min))
    (while (search-forward "" nil t)
      (replace-match "" nil t))
    (goto-char old-pt)
    ))

(defun tcl-grep () 
  (interactive)
  (let ((str) (grepargs))
    (setq str (read-from-minibuffer "grep in ~/work/am-1/tcl/*/*.tcl: "))
    (setq grepargs (concat "grep -n -e " str " ~/work/am-1/tcl/*/*.tcl"))
    (grep grepargs)
))

(defun other-window-only ()
  (interactive)
  (other-window 1)
  (delete-other-windows)
  )

;; pulled from tera-added.el
(defun line-to-top-of-window nil
  "Move the line the cursor is on to the top of the current window"
  (interactive)
  (recenter 0))

(if (not (boundp 'xxx-do-next-line))
    (progn 
      (fset 'old-next-line (symbol-function 'next-line))
      (defvar xxx-do-next-line nil)))

(defun next-line (&optional arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one, nothing happens.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically."
  (interactive "p")
  (if (save-excursion (end-of-line) (eobp))
      (end-of-line)
    (old-next-line (if arg arg 1))))

(require 'vc)
(defun vc-annotate-goto-line (prompt-version)
"Wrapper around vc-annotate to move the point in the annotation window
to match the current line in the source file."
  (interactive "P")
  (let ((opoint (point)) start linenum)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(beginning-of-line)
	(setq start (point))
	(goto-char opoint)
	(beginning-of-line)
	(setq linenum (+ 1 (count-lines 1 (point))))
	))
    (let ((def (vc-workfile-version buffer-file-name)))
      (vc-annotate (buffer-file-name)
		   (if (null prompt-version) def
		     (read-string
		      (format "Annotate from version (default %s): " def)
		      nil nil def))))
      
    (other-window 1)
    (goto-line linenum)
    (other-window -1)
    ))

(defun vc-print-status (verbose)
  "Show the current checkout status of a file."
  (interactive "P")
  (vc-ensure-vc-buffer)
  (let ((file buffer-file-name))
    (if (not (eq (vc-backend file) 'CVS))
	(error "Sorrt, vc-print-status is only implemented for CVS"))
    (let ((temp-buffer-name (concat "*cvs status " (buffer-name) "*")))
      (with-output-to-temp-buffer temp-buffer-name
	(call-process "cvs" nil (get-buffer temp-buffer-name) nil
		      "status"
		      (if verbose "-v" "-l") ;; -l is meaningless anyway
		      (file-name-nondirectory (buffer-file-name))))))
  )

(require 'man)
(defun perldoc (man-args)
  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
		(input (read-string
			(format "Perldoc%s: "
				(if (string= default-entry "")
				    ""
				  (format " (default %s)" default-entry))))))
	   (if (string= input "")
	       (if (string= default-entry "")
		   (error "No man args given")
		 default-entry)
	     input))))

  (let ((my-manual-program manual-program))
    (setq manual-program "perldoc")
    (condition-case nil
	(Man-getpage-in-background man-args))
    (setq manual-program my-manual-program)))

(defun current-line ()
  "Return the text of the current line"
  (interactive)
  (save-excursion
    (let ((beg) (end))
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      (buffer-substring-no-properties beg end))
    ))

;; don't scatter backup files all over the filesystem
(require 'dired)
(defun my-make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name`."
  (let ((dir "~/.emacs.d/backups/"))
    (if (not (file-exists-p dir))
	(make-directory dir t)))
  (concat (expand-file-name "~/.emacs.d/backups/")
	  (dired-replace-in-string "/" "|" file-name)
	  "~"))

(setq make-backup-file-name-function 'my-make-backup-file-name)

;; silly little hook for sending cs264 summaries
(defun cs264-summary ()
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name)))
	(buf  (buffer-substring (point-min) (point-max))))
    (string-match "\\(.*\\).txt" name)
    (setq name (substring name (match-beginning 1) (match-end 1)))
    (mail-other-window)
    (insert "cs264@imail.eecs.berkeley.edu")
    (insert "\nBcc: demmer@cs.berkeley.edu")
    (next-line)
    (end-of-line)
    (insert (format "Paper Summary: %s" name))
    (goto-char (point-max))
    (insert buf)
    (goto-char (point-min))
    ))

;; i do replace-string a lot
(defalias 'rs 'replace-string)

;; conversion function from // style comments to /* style
(defun c-convert-block-comments (from to)
  (interactive "r")

  ;; do the end comment first since point will move
  (newline-and-indent)
  (previous-line 1)
  (goto-char to)
  (indent-according-to-mode)
  (insert " */")

  ;; now replace all the //
  (while (search-forward "//" nil t)
    (replace-match "*" nil t))
  
  ;; finally the start comment
  (goto-char from)
  (indent-according-to-mode)
  (insert "/*")
  (newline)
  (insert " ") ;; indent does strange things in comment mode
  (indent-according-to-mode)
  )

;; from bowei
(defun c-align-by-last-char (from to char offset)
  "Align all of the columns by the rightmost character, e.g. line up a
bunch of = signs or variable declarations. Inserts space + offset."
  (save-excursion
    (goto-char from)
    (let ((column 0)
	  (endmark (make-marker)))
      (move-marker endmark to)
      ;; Find smallest column
      (let ((find-smallest-column
	     #'(lambda (col b e)
		 (end-of-line)
		 (let ((pos (search-backward char b)))
		   (if (< col (- pos b 1))
		       (- pos b 1)
		     col))
	       ) ; lambda
	     ))
	(setq column (fold-by-line-on-region from to find-smallest-column 0)))
      ;; Put in the spaces
      (let ((put-spaces
	     #'(lambda (notused b e)
		 (end-of-line)
		 (let* ((pos (search-backward char b)))
		   (insert-char 32 (+ (- column (- pos b)) offset))
		 ) ; let*
	       ) ; lambda
	   ))
	(fold-by-line-on-region from to put-spaces nil)
      ) ; let
    ) ; let
  ) ; save-excursion
)

;; from bowei
(defun c-align-space-in-region (from to)
  "Do the correct formatting on a region, heuristically by looking at
the characters on the first line."
  (interactive "*r")
  (whitespace-cleanup)
  (untabify from to)
  (save-excursion
    (goto-char from)
    (beginning-of-line)
    (let ((use-equal (search-forward "=" (point-at-eol) t))
	  (use-colon (search-forward ":" (point-at-eol) t)))
      (if use-equal
	  (c-align-by-last-char from to "=" 1)
	(if use-colon
	    (c-align-by-last-char from to ":" 1)
	  (c-align-by-last-char from to " " 1))))
  ) ; save excursion
)

(defun c-align-space-in-region-by-space (from to)
  "Align columns based on space characters in the region"
  (interactive "*r")
  (whitespace-cleanup)
  (untabify from to)
  (save-excursion
    (goto-char from)
    (beginning-of-line)
    (c-align-by-last-char from to " " 1)))

(defun c-align-space-in-region-by-equal (from to)
  "Align columns by equal characers in the region"
  (interactive "*r")
  (whitespace-cleanup)
  (untabify from to)
  (save-excursion
    (goto-char from)
    (beginning-of-line)
    (c-align-by-last-char from to "=" 1)))

;; from bowei
(defun c-create-flag-constants (from to type)
  "Add constants to a C/C++ enumeration/define in the specified
region, e.g.

FLAG,
ANOTHER_FLAG,
YET_ANOTHER_FLAG,

becomes:

FLAG		 = 1<<0,
ANOTHER_FLAG	 = 1<<1,
YET_ANOTHER_FLAG = 1<<2,

If the prefix is non-nil, then a numbered sequence from 0,1,... will
be used instead of the bit positions."
  (interactive "*r\nP")
  (save-excursion
    (goto-char from)
    (let ((column 0)
	  (endmark (make-marker)))
      (move-marker endmark to)
      ;; Find the smallest column, deleting what follows = and ,
      (let ((find-smallest-column
	     #'(lambda (col b e)
		 (let ((pos (or (search-forward-regexp "=\\|," e t) (+ e 2))))
		   (if (< col (- pos b 1))
		       (- pos b 1)
		     col)))))
	(setq column (fold-by-line-on-region from to find-smallest-column 0))
       ) ; let
      ;; Generate the flag statements
      (let ((make-flags
	     #'(lambda (shift b e)		 
		 (beginning-of-line)
		 ;; skip empty lines
		 (if (eolp) shift
		   (progn
		     ;; zap rid of crap after ,=
		     (save-excursion
		       (let ((pos (search-forward-regexp "=\\|," e t)))
			 (if pos (delete-region (- pos 1) e))))
		     ;; indent and put the 1<<X, on the line
		     (end-of-line)
		     (indent-to column)
		     (if type
			 (insert (format "= %d," shift))
		       (insert (format "= 1<<%d," shift)))
		     (+ shift 1)))
		 ) ; lambda
	   ))
	(fold-by-line-on-region from endmark make-flags 0)
      ) ; let
    ) ; let
  ) ; save-excursion
)

(defun fold-by-line-on-region (from to fcn initial-value)
  "Fold fcn on region (from, to), starting with the first line. fcn
will have a signature (init-val beginning-of-line
end-of-line). Returns the last folded value. Set ws to true if
trailing whitespace should be deleted and ignored in terms of the end
of line."
  (save-excursion
    (let ((cur-val initial-value))
      (while (< (point) to)
	(beginning-of-line)
	(setq cur-val 
	      (apply fcn (list cur-val (point) (point-at-eol))))
	(forward-line 1)
      ) ; while
      cur-val
    ) ; let
  ) ; save-excursion
)

(defun scroll-up-ctrl-l ()
  "Refinement to scroll-up that respects ^L characters"
  (interactive)
  (let (pt1 pt2)
    (save-excursion
      (scroll-up)
      (setq pt1 (point)))

    (save-excursion
      (if (re-search-forward "^$" nil t)
	  (next-line 1)
	(goto-char pt1))
      (setq pt2 (point)))

    (if (< pt1 pt2)
	(goto-char pt1)
      (goto-char pt2))
    )
  
  (recenter 0))

(global-set-key "\C-v" 'scroll-up-ctrl-l)

(defun c++-add-tag-types ()
  "Interactive function prompts for filename for the types to be added
to the font lock list"
  (interactive)
  (let ((filename (read-file-name "tags location: " 
				  default-directory
				  "types.tag"
				  t
				  "")))
    (c++-add-tag-types-1 filename)))

(defun c++-add-tag-types-1 (filename)
  "Add filename which is a list of the types to the C++ types"
  (let ((type-buffer (or (get-buffer "*c++-types-list*")
			 (generate-new-buffer "*c++-types-list*"))))
    (with-current-buffer type-buffer
      (erase-buffer)
      (insert-file-contents filename)
      (goto-char 0)
      (while (not (= (point) (point-max)))
	(let ((typename (buffer-substring (point) (point-at-eol))))
	  (if (not (member typename c++-font-lock-extra-types))
	      (setq c++-font-lock-extra-types 
		    (append c++-font-lock-extra-types (list typename))))
	  (goto-char (point-at-eol))
	  (forward-char))))))


;; Copied from lcvs
(defun my-region-active-p ()
  (if (fboundp 'region-active-p)
      (region-active-p)
    (and transient-mark-mode
	 (condition-case ()
	     (mark)
	   (error nil)))))

(require 'longlines)
(defun add-c++-function-separator ()
  "Add a comment-line of //---- to extend across the file."
  (interactive)
  (if (my-region-active-p)
      (kill-region (mark) (point)))
  (insert comment-start)
  (if (eq (char-before (point)) 32)
      (delete-region (- (point) 1) (point)))
  (insert "----------------------------------")
  (insert "------------------------------------")
  (if longlines-mode (use-hard-newlines t))
  (newline)
  )

(defun compile-no-local (command &optional comint)
  "Just like 'compile' but forcing compile-command to be a global
   variable."
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (read-from-minibuffer "Compile command: "
				command nil nil
				(if (equal (car compile-history) command)
				    '(compile-history . 1)
				  'compile-history))
	command))
    (consp current-prefix-arg)))
  (kill-local-variable 'compile-command)
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (setq compilation-directory default-directory)
  (compilation-start command comint))

(defun my-fill-paragraph ()
  "Just like fill-paragraph only in longlines-mode it forces
   the paragraph to be one long line."
  (interactive)
  (if longlines-mode
      ; first delete all newlines in paragraph
      (let ((cur-adaptive-fill adaptive-fill-mode))
	(save-excursion
	  (move-to-left-margin)
	  (if (zerop (forward-paragraph))
	      (let ((to (- (point) 1))
		    (from (progn (backward-paragraph) (point))))

		;; make sure from > to
		(goto-char (min from to))
		(setq to   (max from to))
		(setq from (point))
	      
		; skip blanks at beginning
		(skip-chars-forward " \t\n")

		;; delete all newlines in region
		(while (< (point) to)
		  (if (eq ?\n (char-after (point)))
		      (progn (delete-char 1) (insert " "))
		    (forward-char 1)
		    ))
		)))

	(setq adaptive-fill-mode nil)
	(fill-paragraph nil)
	(setq adaptive-fill-mode cur-adaptive-fill)
	)
    (fill-paragraph nil)
    ))

(defun ediff-merge3-unique-bufname (file-A
				    file-B
				    file-ancestor
				    &optional
				    startup-hooks
				    merge-buffer-file)
  (interactive)
  (let ((buf (ediff-merge-files-with-ancestor file-A file-B file-ancestor
					     startup-hooks merge-buffer-file)))
    (set-buffer buf)
    (rename-buffer (format "* Ediff %s *" merge-buffer-file))))
  
(defun byte-compile-whole-directory (directory)
  "Forcably byte compile everything in a directory"
  (interactive "DByte compile whole directory: \n")
  (message (format "compiling %s" directory))
  (byte-recompile-directory directory t t))


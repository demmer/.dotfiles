(defun requote (start end)
  "Requotes the selected text, mail style. [AMD]"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (replace-regexp "^> " "")
      (fill-region start (point-max))
      (goto-char start)
      (replace-regexp "^" "> ")
      )))


(defvar CH-buffer-pairs)

(setq CH-buffer-pairs '((".C" ".H")
			(".cc" ".h")
			(".c" ".h")
			(".tcl" ".cc")
			("M.nc" ".nc")
 			("M.nc" "C.nc")
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
      ;; Otherwise try to open a file in the current directory
      (setq files 
	    (mapcar
	     (lambda (targetname) "" nil
	       (setq pos (string-match "\<" targetname))
	       (if pos (setq targetname (substring targetname 0 pos)))
	       (let ((filename (concat (file-name-directory (buffer-file-name))
				       targetname)))
		 (if (file-exists-p filename) filename)
		 )
	       ) targetlist))
      (setq file (car (remove-if #'null files)))
      (if file
	  (find-file file)
	(message "Not a switchable buffer"))
      ))
    ))

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
    (beginning-of-buffer)
    (replace-string "" "")
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

(require 'compile)
(defun grope (sym)
  (interactive (list (read-string "Grope for: " (current-word))))
  (compile-internal (concat "grope \"" sym "\"") "No more grope hits" "grope"
                    nil grep-regexp-alist))

;; pulled from comment.el
(defun uncomment-region ()
  "Stupid uncommenting procedure"
  (interactive)
  (comment-region (region-beginning) (region-end) -1))

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
    (vc-annotate prompt-version)
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

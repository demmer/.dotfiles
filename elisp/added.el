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
			))

(defun CH-buffer-match (name)
  "Matches from CH-buffer pairs, and returns a list of modified buffer
names. e.g. if passed buffer.C, will return buffer.H and vice versa.
Uses CH-buffer-pairs to get the pairs of filename extensions. Matches
buffer copy number as well, so buffer.C<2> will return
buffer.H<2>.[mjd]"
  
  (let (
	(root (file-name-sans-extension name))
	(ext (file-name-extension name))
	(ver) (pt) (test))

    (setq pt (string-match "<" ext))
    (cond
     (pt
      (setq ver (substring ext pt (length ext)))
      (setq ext (substring ext 0 pt))
      )
     (t
      (setq ver nil)
      ))
    (setq ext (concat "." ext))
    (setq test (mapcar
		(lambda (pair) "" nil
		  (cond
		   ((string-equal (car pair) ext)
		    (cadr pair))
		   ((string-equal (cadr pair) ext)
		    (car pair))
		   (t nil))
		  )
		CH-buffer-pairs))
    (setq exts (remove-if #'null test))
    (if exts 
	(mapcar (lambda (ext) "" nil 
		  (concat root ext ver)) 
		exts)
      nil)
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
	(bufferlist (buffer-list)))
    
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
    (setq match (car (remove-if #'null (car matches))))
    (cond
     (match (switch-to-buffer match))
     (t
      ;; Otherwise try to open a file in the current directory
      (setq files 
	    (mapcar
	     (lambda (targetname) "" nil
	       (setq pos (string-match "\<" targetname))
	       (if pos (setq targetname (substring targetname 0 pos)))
	       (let ((filename (concat (file-name-directory (buffer-file-name)) targetname)))
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


(defun printf-dprintf ()
  "Dumb elisp function that searches through a document replacing printf
calls with dprintf macro calls. [mjd]"
  (interactive)
  (let ((pos (point)) (count 0) (total (buffer-size)))
    (goto-line 1)
    (message "Converting printf to dprintf (%3d%%) done."
		   (fume--relative-position))
    (while (search-forward " printf" nil t)
	 (progn
	   (backward-word 1)
	   (insert "d")
	   (search-forward "(")
	   (insert "(")
	   (search-forward ";")
	   (backward-char 1)
	   (insert ")")
	   (setq count (+ count 1))
	   ))
	 (message "Done . . . converted %d instances." count)
	 (goto-char pos)
	 )
  
  )

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

(defun vc-annotate-goto-line ()
  (interactive)
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
    (message (format "linenum is %s" linenum))
    (vc-annotate nil)
    (other-window 1)
    (goto-line linenum)
    (other-window -1)
    ))

(load "zenirc")
(defun make-zenirc-frame () 
  (interactive)
  (let ((ctl-frame 
	 (make-frame (list '(width . 112) '(height . 14) '(minibuffer . nil))))
	)
    (modify-frame-parameters ctl-frame (list '(left . 1) 
					     '(top . 1015) 
					     '(menu-bar-lines . 0)
					     '(title . "Zenirc")
					     ))
    (other-frame 1)
    (zenirc)
    (switch-to-buffer "*zenirc*")
    (insert "/join #ff")
    (zenirc-send-line)
    (delete-other-windows)
    (other-frame -1)
    ))

(defun other-window-only ()
  (interactive)
  (other-window 1)
  (delete-other-windows)
  )

(defun convert-xservice ()
  (interactive)
  (if (not (re-search-forward "\\$[-a-zA-Z0-9_]*_" nil t))
      (message "done")
    (progn
      (set-mark (match-beginning 0))
      (goto-char (match-end 0))
      (let ((prompt (format "Convert %s? " (buffer-substring (match-beginning 0) (match-end 0)))))
	(let ((input (read-string prompt nil "y")))
;	(let ((input (read-char prompt)))
	  (if (not (string-equal input "y"))
	      (convert-xservice)
	    (progn
	      (goto-char (match-beginning 0))
	      (delete-char 1)
	      (insert "[$xservice_ ")
	      (goto-char (match-end 0))
	      (forward-char 11)
	      (insert "]")
	      (convert-xservice)
	      )
       ))))))


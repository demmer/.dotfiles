(if (not (boundp 'xxx-do-next-line))
    (progn 
      (fset 'old-next-line (symbol-function 'next-line))
      (defvar xxx-do-next-line nil)))

; (fset 'find-file (symbol-function 'find-file-other-window))

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

(defun higher-window (&optional arg)
  "Go to window above (or left of) this one on the screen"
  (interactive "p")
  (other-window (- arg)))
  
(defun line-to-top-of-window nil
  "Move the line the cursor is on to the top of the current window"
  (interactive)
  (recenter 0))

(defun top-of-window ()
  "Move point to top of window."
  (interactive)
  (move-to-window-line 0))

(defun bottom-of-window ()
  "Move point to botom of window."
  (interactive)
  (move-to-window-line -1))

(defun lower-case-word (&optional arg)
  "Like downcase word but, with arg negated."
  (interactive "*p")
  (save-excursion
    (or (eobp) (forward-char 1))
    (backward-word 1)
    (downcase-word arg)))

(defun upper-case-word (&optional arg)
  "Like upcase word but, with arg negated."
  (interactive "*p")
  (save-excursion
    (or (eobp) (forward-char 1))
    (backward-word 1)
    (upcase-word arg)))

(defun cap-case-word (&optional arg)
  "Like capitalize word but, with arg negated."
  (interactive "*p")
  (save-excursion
    (or (eobp) (forward-char 1))
    (backward-word 1)
    (capitalize-word arg)))

(defun new-scroll-up (&optional arg)
  "Gosling emacs like scroll function."
  (interactive "p")
  (scroll-up arg))

(defun new-scroll-down (&optional arg)
  "Gosling emacs like scroll function."
  (interactive "p")
  (scroll-down arg))

(defun new-page-up (&optional arg)
  "Gosling emacs-like ^V function."
  (interactive "P")
  (let ((i 0) (limit (prefix-numeric-value arg)))
    (while (< i limit)
      (scroll-up)
      (setq i (1+ i)))))

(defun new-page-down (&optional arg)
  "Gosling emacs-like ^V function."
  (interactive "P")
  (let ((i 0) (limit (prefix-numeric-value arg)))
    (while (< i limit)
      (scroll-down)
      (setq i (1+ i)))))

(fset 'window-bigger 'enlarge-window)
(defun window-smaller (&optional arg)
  "Make current window ARG lines smaller."
  (interactive "p")
  (if (< (window-height) (+ arg window-min-height))
      (setq arg (- (window-height) window-min-height)))
  (enlarge-window (- arg)))

(fset 'skip-white-space 'skip-blanks)
(defun skip-blanks nil
  "go forward over blanks and tabs."
  (interactive)
  (while (and (not (eobp))
	      (or (equal (following-char) ?\ )
		  (equal (following-char) ?\t)))
    (forward-char)))
		     
(defun re-display-one-line nil
  "Re-display the current line, to correct for minor transmission glitches."
  (interactive)
  (let ((obim (buffer-modified-p))
	(oro buffer-read-only)
	(odot (point)))
    (setq buffer-read-only nil)
    (beginning-of-line)
    (cond
     ((eolp) nil)
     (t
      (kill-line)
      (sit-for 0)
      (yank)))
    (goto-char odot)
    (setq buffer-read-only oro)
    (set-buffer-modified-p obim)))
  
(defun end-of-window nil
  "Move point to the end of the window"
  (interactive)
  (move-to-window-line -1)
  (end-of-line))

(defun beginning-of-window nil
  "Move point to the beginning of the window"
  (interactive)
  (move-to-window-line 0)
  (beginning-of-line))

(defun eliminate-blanks-forward nil
  "Delete all following blanks, tabs, and newlines."
  (interactive)
  (while 
      (and (not (eobp)) (or (eq (following-char) ?\ )
			      (eq (following-char) ?\t)
			      (eq (following-char) ?\n)))
    (delete-char 1)))

(defun forward-paren nil
  "Go forward to the next unbalanced close paren, or to the end of the
next balanced parenthesized expression, which ever is closer."
  (interactive)
  (while (and
	  (not (eobp))
	  (not (memq (char-syntax (following-char)) '(?\( ?\)))))
    (forward-char))
  (cond ((eobp) (error "No matching paren"))
	((eq (char-syntax (following-char)) ?\()
	 (goto-char (scan-sexps (point) 1)))
	(t
	 (forward-char))))

(defun backward-paren nil
  "Go backward to the next unbalanced close paren, or to the beginning of the
next balanced parenthesized expression, which ever is closer."
  (interactive)
  (while (and
	  (not (bobp))
	  (not (memq (char-syntax (preceding-char)) '(?\( ?\)))))
    (backward-char))
  (cond ((bobp) (error "No matching paren"))
	((eq (char-syntax (preceding-char)) ?\))
	 (goto-char (scan-sexps (point) -1)))
	(t
	 (backward-char))))

(defun close-paren nil
  "If following char is the same as the one inserted, just go forward, else
insert the char.  Blink the matching paren."
  (interactive)
  (if (= last-command-char (following-char))
      (forward-char)
    (insert last-command-char))
  (blink-matching-open))

(defun open-paren nil
  (interactive)
  (if (or (eolp) (= (char-syntax (following-char)) ?\)))
      (progn
	(cond
	 ((= last-command-char ?\()
	  (insert "()"))
	 ((= last-command-char ?\[)
	  (insert "[]"))
	 ((= last-command-char ?\{)
	  (insert-string "{}")))
	(backward-char))
    (insert last-command-char)))

(defun dedent-line (&optional arg)
  "Indent this line by indent-amount characters."
  (interactive "*P")
  (let ((i 0)
	(n-arg (prefix-numeric-value arg)))
    (if arg
	(while (< i n-arg)
	  (beginning-of-line)
	  (let ((ci (current-indentation)))
	    (delete-horizontal-space)
	    (indent-to (- ci indent-amount))
	    (next-line 1)
	    (setq i (1+ i))))
      (save-excursion 
	(beginning-of-line)
	(let ((ci (current-indentation)))
	  (delete-horizontal-space)
	  (indent-to (- ci indent-amount))))
      (if (< (current-column) (current-indentation)) (skip-white-space)))))
    
(defun indent-line (&optional arg)
  "Indent this line by indent-amount characters."
  (interactive "*P")
  (let ((i 0)
	(n-arg (prefix-numeric-value arg)))
    (if arg
	(while (< i n-arg)
	  (beginning-of-line)
	  (let ((ci (current-indentation)))
	    (delete-horizontal-space)
	    (indent-to (+ ci indent-amount))
	    (next-line 1)
	    (setq i (1+ i))))
      (save-excursion 
	(beginning-of-line)
	(let ((ci (current-indentation)))
	  (delete-horizontal-space)
	  (indent-to (+ ci indent-amount))))
      (if (< (current-column) (current-indentation)) (skip-white-space)))))
    
(fset 'fetch-current-word 'fetch-this-word)
(defun fetch-this-word (&optional syntax-table)
  "Return the current word as a string.  Optional syntax-table gives
a syntax-table to use temporarily."
  (let ((old-syntax-table (syntax-table)) answer)
    (if syntax-table (set-syntax-table syntax-table))
    (save-excursion
      (if (bolp) (skip-blanks))
      (or (eobp) (forward-char))
      (forward-word -1)
      (setq answer (buffer-substring 
		    (point)
		    (progn (forward-word 1) (point)))))
    (set-syntax-table old-syntax-table)
    answer))
    
(defun change-indent (old-ind new-ind)
  "Change the indentation from old to new, and set new as the local
value of indent-amount."
  (interactive "nOld indent: \nnNew indent: \n")
  (setq indent-amount new-ind)
  (save-excursion
    (let (cur-col n-tabs left-over)
    (goto-char (point-min))
    (while (not (eobp))
      (setq cur-col (current-indentation))
      (delete-horizontal-space)
      (setq n-tabs (/ cur-col old-ind))
      (setq left-over (% cur-col old-ind))
      (if (>= left-over new-ind) (setq left-over (- new-ind 1)))
      (indent-to (+ (* n-tabs new-ind) left-over))
      (next-line 1)
      (end-of-line)
      (if (not (eobp)) (beginning-of-line))))))
    
(defun position nil
  "Report current position in buffer."
  (interactive)
  (let ((this-line (count-lines (point-min) (point)))
	(after-lines (count-lines (point) (point-max))))
    (if (bolp) (setq this-line (1+ this-line)))
    (message "Line %d of %d, Char %d"
	     this-line
	     (+ this-line after-lines -1)
	     (current-column))))

(defun newline-and-indent-eliminating-blanks nil
  (newline)
  (insert ".")
  (delete-blank-lines)
  (delete-backward-char 1)
  (indent-like-last-line))

(defun eliminate-blanks nil
  (delete-blank-lines)
  (delete-blank-lines))

(defun newline-and-indent ()
  "Insert a newline, then indent according to mode, like Tab."
  (interactive "*")
  (insert ?\n)
  (indent-according-to-mode))

;(defun find-backup-file-name (s)
;    "Return a list containing the name of the backup name for s"
;    (list (concat s ".bak")))

(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider auto-save-visited-file-name; that is checked
before calling this function.
This is a separate function so your .emacs file or site-init.el can redefine it.
See also auto-save-file-name-p."
  (if buffer-file-name
      (concat buffer-file-name ".ckp")
    (expand-file-name (concat (buffer-name) ".ckp"))))

(defun auto-save-file-name-p (filename)
  "Return t if FILENAME can be yielded by make-auto-save-file-name.
FILENAME should lack slashes.
This is a separate function so your .emacs file or site-init.el can redefine it."
  (string-match filename "^.*\\.ckp"))

(defun kill-cwd nil
  "Kill the current default directory."
  (interactive)
  (let ((cwd default-directory))
    (set-buffer (get-buffer-create " kill-cwd "))
    (erase-buffer)
    (insert-string cwd)
    (kill-region (point-min) (point-max))
    (setq this-command 'kill-cwd)))

(defun do-an-error (filename linenumber &optional old-position)
  "Display file FILENAME in another window, positioning at line LINENUMBER.
Make current line visible in this window.  Optional OLDPOSITION gives place to
position point in current buffer."
  (beginning-of-line)
  (line-to-top-of-window)
  (if (and old-position (pos-visible-in-window-p old-position))
      (goto-char old-position))
  (find-file-other-window filename)
  (goto-line (if (numberp linenumber) linenumber (string-to-int linenumber))))

(if (boundp 'lisp-interaction-mode-map)
    (progn 
      (define-key lisp-interaction-mode-map "\n" 'newline-and-indent)
      (define-key lisp-interaction-mode-map "\r" 'eval-print-last-sexp)))

(defvar Remote-command-process nil
  "The process used to communicate commands from shell to emacs.")

(defun pause-emacs nil
  "Pause emacs.  Process process input before displaying the screen."
  (interactive)
  (if (not Remote-command-process) (Remote-command-start))
  (suspend-emacs)
  (accept-process-output))

(defun read-password ()
  (let ((answ "") tem)
    (while (not(or  (= (setq tem (read-char)) ?\^m)
		    (= tem ?\n)))
      (setq answ (concat answ (char-to-string tem))))
    answ))

(defun my-spell-word (arg)
  "If prefix arg, then spell-word, else spell-string."
  (interactive "P")
  (if arg
      (spell-word)
    (call-interactively 'spell-string)))

(defun scan-buffer (start howmany character)
  "Do it by hand."
  (save-excursion 
    (goto-char start)
    (if (search-forward (char-to-string character) nil 1 howmany)
	(point)
      (point))))

(defun occur-this-word nil
  "Find all occurances of the word at the cursor."
  (interactive)
  (occur (concat "\\b" (fetch-this-word) "\\b")))

(defun kill-this-line nil
  "Kill the line that point is on."
  (beginning-of-line)
  (kill-line 1))

(defun repeat (count)
  "Repeat last command with no query.
Repeats the command as many times as indicated by numeric argument COUNT.
If the last command had a numeric argument, it is ignored; if it had an
argument list (usually happens only for functions that are not invoked by
interactive keypath), the list is omitted; and if it was a self-inserted
character, the current keybinding is inserted instead."
  (interactive "p")
  (let (str pos)
    (if (eq last-command 'repeat)
        ()
      (setq pos (- (- (length (recent-keys)) 2)
                   (length (this-command-keys))))
      (setq str (substring (recent-keys) pos (+ pos 2)))
      (setq repeat-complex-flag nil)
      (setq repeat-kill-flag nil)
      (cond
       ((or (equal str "e") (equal str "E"))
        (setq repeat-command 'call-last-kbd-macro))
       ((or (equal str "d") (equal str "D"))
        (setq repeat-command 'kill-word)
        (setq repeat-kill-flag t))
       ((or (equal str "") (equal str ""))
        (setq repeat-command 'backward-kill-word)
        (setq repeat-kill-flag t))
       ((eq last-command 'exit-minibuffer)
        (setq repeat-complex-flag t)
        (setq repeat-command (car command-history)))
       (t
        (setq repeat-command last-command))))
    ;(princ repeat-command)
    (while (> count 0)
      (if repeat-kill-flag
          (append-next-kill))
      (if repeat-complex-flag
          (eval repeat-command)
        (command-execute repeat-command))
      (setq this-command 'repeat)
      (setq count (1- count)))
    (setq last-command 'repeat)))
(global-set-key "\^\\" 'repeat)

(defun norm-compile (prompt)
  (interactive "P")
  (require 'compile)
  (save-some-buffers t)
  (if prompt
      (progn 
	(setq new (read-string (concat "Compile command ("  compile-command "): ") nil)
	      compile-command (if (string= new "") compile-command new))
	(compile1 compile-command "No more errors"))
    (compile1 "gmake -k" "No more errors")))
      
(defun ffinger-user (arg)
  (interactive "P")
  (l-finger-user arg t))

(defun finger-user (arg)
  (interactive "P")
  (l-finger-user arg nil))

(defun l-finger-user (arg fast)
  (let (name)
    (setq name 
	  (if arg 
	      (let (beg end)
		(save-excursion
		 (if (not (looking-at "\\<"))
		     (forward-word -1))
		 (setq beg (point))
		 (forward-word 1)
		 (setq end (point)))
		(buffer-substring beg end))
	    (read-string "Finger user: ")))
    (if fast
	(start-process "finger" "*finger*"
		       "sh" "-c" (concat 
				  "fing "
				  name
				  "|egrep 'In real life'|uniq"))
      (start-process "finger" "*finger*"
		     "fing" name))
    (with-output-to-temp-buffer "*finger*"
      (princ "fing ")
      (princ name)
      (terpri))))
      
(defun copy-current-line nil
  (interactive)
  (save-excursion
	(beginning-of-line)
	(set-mark (point))
	(next-line)
	(copy-region-as-kill (point) (mark)))
)

(defun scroll-half-screen-up nil
  "Scroll up screen half of current window"
  (interactive)
    (move-to-window-line nil)
    (recenter 0)
    (move-to-window-line nil)
)

(defun scroll-half-screen-down nil
  "Scroll down screen half of current window"
  (interactive)
    (move-to-window-line nil)
    (recenter -1)
    (move-to-window-line nil)
  )


(defun scroll-up-one-line nil
  "Scroll up screen one line"
  (interactive)
  (scroll-up 1)
)

(defun scroll-down-one-line nil
  "Scroll down screen one line"
  (interactive)
  (scroll-down 1)
  )

(defun delete-char-or-list () "" (interactive)
  (if (eolp)
      (minibuffer-completion-help)
    (delete-char 1)))

(defun backward-kill-line () (interactive)
      (let ((beg (point)))
	(beginning-of-line)
	(delete-region (point) beg)))

(defun my-exit-from-emacs ()
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))

(defun prev-switch-screen ()
  (interactive)
  (other-frame -1)
)

(defun switch-screen ()
  (interactive)
  (other-frame 1)
)




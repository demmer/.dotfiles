;;; lsvn.el --- a little SVN mode, or lomew's SVN mode
;;; lvc-svn.el -- demmer renamed

;; Copyright (C) 2006 Bart Robinson <lomew@pobox.com>

;; Author: Bart Robinson <lomew@pobox.com>
;; Created: Sep 2006
;; Version: trunk ($Revision: 1.6 $)
(defconst lvc-svn-version "trunk")
;; Date: the-date
;; Keywords: svn

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; blah blah blah
;; (setq lvc-svn-commit-template "BugID: \nCC: \nApproved-by: \nReviewed-by: \n")

(require 'ediff)
(require 'ediff-vers)

;; User vars.

(defvar lvc-svn-command "svn"
 "*How to call svn.")

;; Internal Vars.

;; Specifies what to search for when looking for the filename
;; in "svn status" output.  Also takes into account the characters
;; we add for marked files.
;; The parens are assumed to enclose the state information.
(defconst lvc-svn-status-linepat-0 "[ ACDIMRX?!~][ CM][ L][ +][ S]")
(defconst lvc-svn-status-linepat (concat "^\\("
				      lvc-svn-status-linepat-0 "[ K]"
				      "\\)"
				      "[ *]"))
;; For "svn status --show-updates" output.
(defconst lvc-svn-ustatus-linepat (concat "^\\("
				      lvc-svn-status-linepat-0 "[ KOTB]"
				      " [ *] +[-0-9]* +"
				      "\\)"
				      "[ *]"))

;; Line pattern appropriate for this buffer.
(defvar lvc-svn-linepat nil)
(make-variable-buffer-local 'lvc-svn-linepat)

(defvar lvc-svn-font-lock-keywords
  '(("^[ ]+\\*[ ]+[^ ].+"    . lvc-needs-update-face)
    ("^[M][ *].*" . lvc-local-changes-face)
    ("^[AR][ *].*" . lvc-addremove-face)
    ("^C[ *].*"    . lvc-conflict-face)
    ))

(defvar lvc-svn-debug nil
 "If non-nil, put lvc-svn into debug mode.")

(defconst lvc-svn-mode-map
 (let ((map (make-sparse-keymap 'lvc-svn-mode-map)))
   (define-key map "?" 'lvc-svn-explain-this-line)
   (define-key map "n" 'lvc-svn-next-line)
   (define-key map "p" 'lvc-svn-prev-line)
   (define-key map "m" 'lvc-svn-mark-file)
   (define-key map "u" 'lvc-svn-unmark-file)
   (define-key map "U" 'lvc-svn-update-some-files)
   (define-key map "R" 'lvc-svn-revert)
   (define-key map "r" 'lvc-svn-remove)
   (define-key map "C" 'lvc-svn-commit)
   (define-key map "d" 'lvc-svn-diff-base)
   (define-key map "D" 'lvc-svn-diff-head)
   (define-key map "e" 'lvc-svn-ediff)
   (define-key map "l" 'lvc-svn-log-base)
   (define-key map "L" 'lvc-svn-log-head)
;    (define-key map "s" 'lcvs-show-status)
   (define-key map "S" 'lvc-svn-sort)
;    (define-key map "a" 'lcvs-annotate)
   (define-key map "g" 'lvc-svn-refresh-buffer)
   (define-key map "G" 'lvc-svn-status)
   (define-key map "f" 'lvc-svn-find-file)
   (define-key map "o" 'lvc-svn-find-file-other-window)
   (define-key map "q" 'lvc-svn-quit-just-bury)
   (define-key map "+" 'lvc-svn-add)
;    (define-key map "-" 'lcvs-remove-crap)
;    (define-key map "\C-k" 'lcvs-kill-region-or-line)
;    (define-key map "\C-w" 'lcvs-kill-region)
;    (define-key map "\C-xu" 'lcvs-undo)
;    (define-key map "\C-xc" 'lcvs-clean)
;    (condition-case ()
;	 ;; This is for XEmacs, will error in Emacs.
;	 (define-key map '(control /) 'lcvs-undo)
;      (error nil))
   (define-key map "\C-c\C-k" 'lvc-svn-kill-process)
   map)
 "Keymap for `lvc-svn-mode'")

(defvar lvc-svn-lots-of-dashes (make-string 72 ?-)
 "A long string of dashes.")

(defvar lvc-svn-inserted-final-dashes nil
 "If we inserted the final dashes.")
(make-variable-buffer-local 'lvc-svn-inserted-final-dashes)

(defvar lvc-svn-marked-files nil
 "Alist of marked files.  It is in the form `\(file . status)'.")
(make-variable-buffer-local 'lvc-svn-marked-files)

(defvar lvc-svn-submode nil
 "The submode for `lvc-svn-mode' for this buffer.")
(make-variable-buffer-local 'lvc-svn-submode)

(defvar lvc-svn-head-revision-from-last-ustatus "HEAD"
 "The HEAD revision as reported by `status -u'.")
(make-variable-buffer-local 'lvc-svn-head-revision-from-last-ustatus)


;; User functions.

(defun lvc-svn-status (dir &optional show-updates want-fresh-buf)
 "Call \"svn status\" in DIR and then call `lvc-svn-mode' (which see).
Optional arg SHOW-UPDATES (interactive prefix arg) means to
pass \"-u/--show-updates\" and show updatable files.
Optional WANT-FRESH-BUF means don't reuse an existing buffer visiting
the same directory."
 (interactive (list (expand-file-name
		      (file-name-as-directory
		       (lvc-read-directory-name (concat "SVN status"
							 (if current-prefix-arg
							     " -u")
							 " for directory: ")
						 lvc-last-dir
						 lvc-last-dir t)))
		     current-prefix-arg))
 (setq lvc-last-dir dir)
 (let* ((basename (file-name-nondirectory (directory-file-name dir)))
	 (bufname (format "*SVN-status-%s*" basename))
	 (procname (format "svn-status-%s" basename))
	 (buf (get-buffer bufname))
	 (cmd (if show-updates
		  (list lvc-svn-command "status" "--show-updates")
		(list lvc-svn-command "status")))
	 proc)
   ;; Use an existing buffer if it is "visiting" the same dir.
   (if (and (not want-fresh-buf)
	     buf
	     (string-equal (save-excursion
			     (set-buffer buf)
			     default-directory)
			   dir))
	(pop-to-buffer buf)
     ;; Else make one.
     (setq buf (get-buffer-create bufname))
     (save-excursion
	;; Check for existing process.
	(if (and (get-buffer-process buf)
		 (eq (process-status (get-buffer-process buf)) 'run))
	    (error "%s process already running" procname))

	;; Prepare the buffer.
	;; lvc-svn-mode makes the buffer read-only, so we have to take that
	;; into account here (and we might be called from lvc-svn-refresh-buffer).
	(set-buffer buf)
	(setq default-directory dir)
	(unwind-protect
	    (let ((info (lvc-svn-info))
		  (buffer-read-only nil))
	      (buffer-disable-undo (current-buffer))
	      (erase-buffer)
	      (insert "In " dir "\n")
	      (insert "BASE is " (lvc-svn-info-get info "Revision") "\n")
	      (insert "URL is " (lvc-svn-info-get info "URL") "\n")
	      (insert "\n")
	      (insert "$ " (mapconcat 'identity cmd " ") "\n")
	      (insert lvc-svn-lots-of-dashes "\n")
	      ;; Insert a dummy line for the cwd they can run 'log' on.
	      (if show-updates
		  (insert "       *            .\n")
		(insert "       .\n")))
	  (buffer-enable-undo (current-buffer)))

	;; Set up keybindings etc.
	(lvc-svn-mode (if show-updates 'ustatus 'status))

	;; Make the buffer visible and start the process.
	(pop-to-buffer buf)
	(goto-char (point-min))
	(setq proc (apply 'start-process procname buf cmd))
	(set-process-filter proc (function lvc-svn-filter))
	(set-process-sentinel proc (function lvc-svn-sentinel))))))

(defun lvc-svn-mode (submode)
 "Major mode for interacting with SVN.
The normal entry point is `lvc-svn-status'.

The hook `lvc-svn-mode-hook', if set, is run upon entry.

The following keys have meaning in an `lvc-svn-mode' buffer:
\\{lvc-svn-mode-map}
Some of the commands can work on marked files via a \\[universal-argument]
prefix; consult the individual documentation for each command
via `describe-key' on \\[describe-key]"
 ;; XXX/lomew completions would be nice, but are hassle since I have to
 ;; define a keymap
 (interactive "SSubmode (status or ustatus): ")
 (if (and (not (eq submode 'status))
	   (not (eq submode 'ustatus)))
     (error "Submode should be status or ustatus"))
 (kill-all-local-variables)
 (setq lvc-svn-marked-files nil)
 (cond ((eq submode 'status)
	 (setq lvc-svn-linepat lvc-svn-status-linepat))
	(t
	 (setq lvc-svn-linepat lvc-svn-ustatus-linepat)))
 (setq lvc-svn-submode submode)
 (use-local-map lvc-svn-mode-map)
 (setq major-mode 'lvc-svn-mode
	mode-name "LVC-SVN")
 (setq modeline-process '(":%s"))
 (setq buffer-read-only t)
 (make-variable-buffer-local 'font-lock-defaults)
  (if lvc-font-lock-enabled
      (setq font-lock-defaults '(lvc-svn-font-lock-keywords)))
 (run-hooks 'lvc-svn-mode-hook))

(defun lvc-svn-explain-this-line ()
 "Explain what this line means.  If repeated then offer brief help.
Translates stuff like \"M foo/bar.c\" into something like \"this file has
been locally modified\"."
 (interactive)
 (if (eq last-command 'lvc-svn-explain-this-line)
     (message "m/u mark | d/D(l/L) diff(log) base/head | C commit | R revert | U update | + add")
   (let* ((state (lvc-svn-current-file-state))
	   (res (mapconcat 'symbol-name state ", ")))
     (if (memq 'out-of-date state)
	  (setq res (concat res (format ", BASE is %d"
					(lvc-svn-current-file-base)))))
     (message res))))

(defun lvc-svn-next-line ()
 "Move cursor to the next file."
 (interactive)
 (if (re-search-forward lvc-svn-linepat nil t)
     (if lvc-explain-each-line
	  (lvc-svn-explain-this-line))
   (error "No more files")))

(defun lvc-svn-prev-line ()
 "Move cursor to the previous file."
 (interactive)
 (let ((pt (point)))
   (beginning-of-line)
   (if (re-search-backward lvc-svn-linepat nil t)
	(progn 
	  (goto-char (match-end 0))
	  (if lvc-explain-each-line
	      (lvc-svn-explain-this-line)))
     (goto-char pt)
     (error "No more files"))))

(defun lvc-svn-refresh-buffer (arg)
 "Re-get the status for this dir.
Prefix arg toggles showing updatable files."
 (interactive "P")
 (let ((want-status (if arg
			 (eq lvc-svn-submode 'status)
		       (eq lvc-svn-submode 'ustatus))))
   (lvc-svn-status default-directory want-status 'fresh)))

(defun lvc-svn-quit-just-bury ()
 "\"Quit\" lvc-svn-mode by burying the buffer."
 (interactive)
 (bury-buffer))

(defun lvc-svn-kill-process ()
 "Kill the svn process, if there is one.
We assume the current buffer is the one that is supposed to be running
a svn process."
 (interactive)
 (if (get-buffer-process (current-buffer))
     (interrupt-process (get-buffer-process (current-buffer)))
   (error "No svn process running")))

(defun lvc-svn-mark-file ()
 "Mark the file on this line for later processing."
 (interactive)
 (lvc-svn-bitch-if-commit-in-progress)
 (let ((file (lvc-svn-current-file))
	(state (lvc-svn-current-file-state)))
   (if (assoc file lvc-svn-marked-files)
	nil
     (lvc-svn-set-mark-state t)
     (setq lvc-svn-marked-files (cons (cons file state) lvc-svn-marked-files))))
 ;; `lvc-svn-next-line' can error if at the end of files.
 (condition-case nil
     (lvc-svn-next-line)
   (error nil)))

(defun lvc-svn-unmark-file ()
 "Remove the file on this line from the list of to-be-processed files.
See also `lvc-svn-mark-file'."
 (interactive)
 (lvc-svn-bitch-if-commit-in-progress)
 (let ((file (lvc-svn-current-file)))
   (if (not (assoc file lvc-svn-marked-files))
	nil
     (lvc-svn-set-mark-state nil)
     (setq lvc-svn-marked-files (lvc-svn-remassoc file lvc-svn-marked-files))))
 ;; `lvc-svn-next-line' can error if at the end of files.
 (condition-case nil
     (lvc-svn-next-line)
   (error nil)))

(defun lvc-svn-diff-base (arg)
 "Diff some files against the BASE revision (what you originally checked out).
Use this when you have locally modified files and want to see what
you have done.  See also `lvc-svn-diff-head'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
 (interactive "P")
 (message "Diffing...")
 (lvc-svn-do-command "diff"
		   "No differences with the BASE"
		   nil
		   (mapcar 'car (lvc-svn-get-relevant-files arg)))
 (message "Diffing...done"))

(defun lvc-svn-diff-head (arg)
 "Diff some files against the HEAD revision.
Use this when files have been checked in by someone else and you want
to see what has changed before you update your copies.  See also
`lvc-svn-diff-base'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
 (interactive "P")
 (if (not (memq lvc-svn-submode '(status ustatus)))
     ;; XXX/lomew - don't bind this in update mode
     (error "Diffing BASE against HEAD is nonsensical in update mode"))
 (message "Diffing...")
 (lvc-svn-do-command "diff"
		   "No differences with the HEAD"
		   nil
		   (cons (format "-rBASE:%s"
				 lvc-svn-head-revision-from-last-ustatus)
			 (mapcar 'car (lvc-svn-get-relevant-files arg))))
 (message "Diffing...done"))


;; This doesn't drop you back to the examine/update buffer and is hard
;; to do (would have to set the ediff hook (maybe the cleanup hook) to
;; some function to put us back in the examine buffer then remove the
;; function from the hook.  this would prevent multiple ediffs running
;; from dsvn since the hook is global)
(defun lvc-svn-ediff (arg)
  "Ediff a file with the HEAD revision.
This will compare the contents of the current-file with the tip of
the current branch."
  (interactive "P")
  (find-file (lvc-svn-current-file))
  (ediff-vc-internal "" "" nil))

;; XXX/lomew work in -v and --stop-on-copy
;; XXX/lomew work in the repositioning stuff?
;;   this would assume we are logging from beyond BASE
(defun lvc-svn-log-base ()
 "Show log for the current file.  Logs from BASE to the earliest revision."
 (interactive)
 (message "Logging...")
 (lvc-svn-do-command "log"
		   "No output"
		   nil
		   (list "--limit=100" "-v" (lvc-svn-current-file)))
 (message "Logging...done"))

(defun lvc-svn-log-head ()
 "Shows the log for revisions you would get if you updated this file."
 (interactive)
 (let ((base (lvc-svn-current-file-base)))
   ;; If BASE == HEAD then say no changes.
   (if (and (numberp base)
	     (numberp lvc-svn-head-revision-from-last-ustatus)
	     (= base lvc-svn-head-revision-from-last-ustatus))
	(with-output-to-temp-buffer "*SVN-log*"
	  (princ (format "No changes from %d to %d"
			 base lvc-svn-head-revision-from-last-ustatus)))
     ;; Otherwise run log.
     (message "Logging...")
     (lvc-svn-do-command "log"
		       "No output"
		       nil
		       (list "--limit=100" "-v"
			     (format "-r%s:%s"
				     lvc-svn-head-revision-from-last-ustatus
				     ;; Don't include BASE itself
				     ;; since we already have that and
				     ;; don't want to show it (in case
				     ;; this file was changed in
				     ;; BASE).
				     (if (numberp base) (1+ base) base))
			     (lvc-svn-current-file)))
     (message "Logging...done"))))

(defun lvc-svn-sort ()
 "Sort the SVN output in this buffer.
This is useful to get files with similar status together."
 (interactive)
 (save-excursion
   (goto-char (point-min))
   ;; Go thru each section bracketed by lvc-svn-lots-of-dashes
   (while (re-search-forward "^----" nil t)
     (let (beg end)
	(forward-line)
	(setq beg (point))
	(if (re-search-forward "^----" nil t)
	    (progn
	      (setq end (match-beginning 0))
	      (forward-line))
	  (setq end (point-max)))
	(let ((buffer-read-only nil))
	  (sort-lines nil beg end))))))

(defun lvc-svn-commit (arg)
 "Commit files.
If given a prefix argument, commit the marked files.  Otherwise commit
the file on this line."
 (interactive "P")
 (lvc-svn-bitch-if-commit-in-progress)
 (let ((files (lvc-svn-get-relevant-files arg))
	(this-buffer (current-buffer))
	cur state)
   ;; Don't let them do something dumb.
   ;; This should match what we display in the "SVN:..." lines.
   (setq cur files)
   (while cur
     (setq state (cdr (car cur)))
     (setq cur (cdr cur))
     (if (or (memq 'replaced state)
	      (memq 'added state)
	      (memq 'deleted state)
	      (memq 'modified state)
	      (memq 'pmodified state))
	  nil
	(error "Can only commit replaced, added, deleted, or modified files")))
   ;; Checks ok, give them the edit buffer.
   (pop-to-buffer (get-buffer-create "*SVN-commit-message*"))
   (lvc-svn-commit-mode this-buffer files)))

(defun lvc-svn-find-file (&optional arg)
 "Find the file on this line, or the marked files if given a prefix arg."
 (interactive "P")
 (mapcar (function (lambda (pair)
		      (if (file-exists-p (car pair))
			  (save-excursion
			    (find-file (car pair))))))
	  (lvc-svn-get-relevant-files arg)))

(defun lvc-svn-find-file-other-window ()
 "Find the file on this line."
 (interactive)
 (let ((file (lvc-svn-current-file)))
   (if (file-exists-p file)
	(find-file-other-window file)
     (error (format "%s not in working copy, probably a new file" file)))))

(defun lvc-svn-add (arg)
 "Schedule unversioned files for addition.
If given a prefix argument, add the marked files.  Otherwise add
the file on this line.
The files won't be actually added to the repository until they are
formally committed."
 (interactive "P")
 (let ((files (lvc-svn-get-relevant-files arg))
	state status cur)
   ;; Check thru the files for addable ones.  These are only ?/unversioned ones
   (setq cur files)
   (while cur
     (setq state (cdr (car cur)))
     (setq cur (cdr cur))
     (cond
      ((memq 'unversioned state)
	nil)
      ((memq 'deleted state)
	;; XXX/lomew it would be nice to collect these and then do a svn revert
	;; for them in addition to the add
	(error (substitute-command-keys
		"Deleted files can't be added, use revert, on \\[lvc-svn-revert]")))
      (t
	(error "Can only add unversioned files"))))
   (message "Adding...")
   (setq status (lvc-svn-do-command-quietly "add" nil (mapcar 'car files)))
   (message "Adding...done")
   (if (zerop status)
	;; Update the diplayed state of the files to "A" from "?"
	(let ((cur files)
	      pair)
	  (while cur
	    (setq pair (car cur))
	    (setq cur (cdr cur))
	    (lvc-svn-change-file-state (car pair) 'added)))
     ;; Otherwise an error happened, bitch appropriately
     (pop-to-buffer "*SVN-add*")
     (goto-char (point-min))
     (insert "\n"
	      "*** The add was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "\n")
     (error "Add failed, see *SVN-add* buffer for details."))))

(defun lvc-svn-update-some-files (arg)
 "Update some files.
If given a prefix arg, update the working copy to HEAD,
otherwise just this file."
 (interactive "P")
 (let ((filename (if arg "." (lvc-svn-current-file)))
	(nconflicts 0)
	update-msg status)
   (setq update-msg (format "Updating %s to %s"
			     (if (string-equal filename ".")
				 "working copy"
			       filename)
			     (if (numberp lvc-svn-head-revision-from-last-ustatus)
				 (format "HEAD (%d)"
					 lvc-svn-head-revision-from-last-ustatus)
			       "HEAD")))
   (message (format "%s..." update-msg))
   (setq status (lvc-svn-do-command-quietly
		  "update"
		  nil
		  (list (format "-r%s" lvc-svn-head-revision-from-last-ustatus)
			filename)))
   (message (format "%s...done" update-msg))

;-    (if (zerop status)
;-	;; Go thru each line from the update output and update the
;-	;; status buffer.  
;-	(let ((cur (lvc-svn-parse-update-buffer "*SVN-update*"))
;-	      item file newstate)
;-	  (while cur
;-	    (setq item (car cur))
;-	    (setq file (car item))
;-	    (setq newstate (cdr item))
;-	    (setq cur (cdr cur))
;-	    (if (or (memq 'added newstate)
;-		    (memq 'deleted newstate)
;-		    (memq 'updated newstate))
;-		     props...
;-		(lvc-svn-remove-file-line file)
	      
   (pop-to-buffer "*SVN-update*")
   (goto-char (point-min))
;;    (insert "\n"
;; 	    "*** XXX/lomew deal with this buffer\n"
;; 	    "\n")
   ))

(defun lvc-svn-revert (arg)
 "Revert some files, discarding local changes.
By default reverts the file on this line.
If supplied with a prefix argument, revert the marked files.
By default this command requires confirmation to remove the files.  To
disable the confirmation, you can set `lvc-revert-confirm' to nil."
 (interactive "P")
 (let* ((files (lvc-svn-get-relevant-files arg))
	 (multiple-p (cdr files))
	 status)
   (if (and lvc-revert-confirm
	     (not (yes-or-no-p (format "Revert %s? "
				       (if multiple-p
					   "the marked files"
					 (car (car files)))))))
	(message "Revert cancelled")
     (message "Reverting...")
     (setq status (lvc-svn-do-command-quietly "revert" nil (mapcar 'car files)))
     (message "Reverting...done")
     (if (zerop status)
	  ;; Revert some buffers and remove the file lines from the status buf
	  (let ((cur files)
		pair file)
	    (while cur
	      (setq pair (car cur))
	      (setq cur (cdr cur))
	      (setq file (car pair))
	      (lvc-svn-revert-buffers-visiting-file file)
	      (lvc-svn-remove-file-line file)))
     ;; Otherwise an error happened, bitch appropriately
     (pop-to-buffer "*SVN-revert*")
     (goto-char (point-min))
     (insert "\n"
	      "*** The revert was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "\n")
     (error "Revert failed, see *SVN-revert* buffer for details.")))))


(defun lvc-svn-remove (arg)
  "Remove files, (obviously) discarding local changes."
  (interactive "P")
  (let* ((files (lvc-svn-get-relevant-files arg))
	 (multiple-p (cdr files)))
    (if (and lvc-remove-confirm
	     (not (yes-or-no-p (format "Remove %s? "
				       (if multiple-p
					   "the marked files"
					 (car (car files)))))))
	(message "Remove cancelled")
      ;; Otherwise remove the files
      (mapcar (function (lambda (e)
			  (let ((file (car e)))
			    (shell-command (format "rm -rf %s" file))
			    (lvc-svn-remove-file-line file)
			    )))
	      files))))

;; The committing major mode

(defvar lvc-svn-commit-msgs (make-ring 10)
 "Stores last few commit messages.")
(defvar lvc-svn-commit-msgs-index nil)
(make-variable-buffer-local 'lvc-svn-commit-msgs-index)

(defvar lvc-svn-commit-initial-buffer-contents ""
"Contents of the commit buffer when we initially prepare it.
Used for the commit message ring.")
(make-variable-buffer-local 'lvc-svn-commit-initial-buffer-contents)

(defconst lvc-svn-commit-delimiter
 "-- This line, and those below, will be ignored --")

(defvar lvc-svn-commit-parent-buffer nil
 "The examine/update mode buffer.
For commit-mode buffers.")
(make-variable-buffer-local 'lvc-svn-commit-parent-buffer)

(defvar lvc-svn-commit-files nil
 "Alist of the files to be committed.
It has the same form as `lvc-svn-marked-files'.
For commit-mode buffers.")
(make-variable-buffer-local 'lvc-svn-commit-files)

(defvar lvc-svn-commit-mode-map
 (let ((map (make-sparse-keymap 'lvc-svn-commit-mode-map)))
   (define-key map "\C-c\C-c" 'lvc-svn-commit-finish)
   (define-key map "\M-p" 'lvc-svn-commit-insert-prev-commit-msg)
   (define-key map "\M-n" 'lvc-svn-commit-insert-next-commit-msg)
   map)
 "Keymap for `lvc-svn-commit-mode'")

(defun lvc-svn-commit-mode (parent files)
 "Major mode for providing a commit log message and committing files.
This mode is not meant to be user invoked."
 (interactive)

 (setq lvc-svn-commit-parent-buffer parent)
 (setq lvc-svn-commit-files (sort files (lambda (a b)
					(string-lessp (car a) (car b)))))

 (use-local-map lvc-svn-commit-mode-map)
 (setq major-mode 'lvc-svn-commit-mode)
 (setq mode-name "SVN-Commit")

 (setq lvc-svn-commit-msgs-index nil)

 (lvc-svn-prepare-commit-buffer files)
 (setq lvc-svn-commit-initial-buffer-contents (buffer-string))
 (goto-char (point-min))
 (if lvc-commit-template
     (insert lvc-commit-template))
 (set-buffer-modified-p nil)

 (message (substitute-command-keys "Type \\[lvc-svn-commit-finish] when done."))
 (run-hooks 'text-mode-hook))

;; Insert stuff to show them what files they're affecting.
;; Imitate logic from svn commit.
;; XXX/lomew I think the affected files output could be more readable.
(defun lvc-svn-prepare-commit-buffer (files)
 (insert "\n\n")
 (insert lvc-svn-commit-delimiter)
 (insert "\n")
 (insert (substitute-command-keys
	   "-- Type \\[lvc-svn-commit-finish] when done --\n"))
 (insert "-- Committing in " default-directory " --\n")
 (insert "\n")
 (while files
   (let ((txtmod ?_)
	  (propmod ?\ )
	  (unlock ?\ )
	  (file (car (car files)))
	  (state (cdr (car files))))
     ;; text
     (cond ((memq 'replaced state)	(setq txtmod ?R))
	    ((memq 'added state)	(setq txtmod ?A))
	    ((memq 'deleted state)	(setq txtmod ?D))
	    ((memq 'modified state)	(setq txtmod ?M)))
     ;; props
     (if (memq 'pmodified state)	(setq propmod ?M))
     ;; locks, XXX/lomew assuming sans --no-unlock
     (if (memq 'lock-have state)	(setq unlock ?U))
     (insert (format "%c%c%c  %s\n" txtmod propmod unlock file))
     (setq files (cdr files)))))

(defun lvc-svn-commit-finish ()
 ;; Finish up the commit by grabbing the commit string and calling svn commit.
 ;; If the commit worked, clear out the affected files from the parent buffer.
 ;; Otherwise complain loudly and pop up the commit output.
 ;;
 ;; This is tricky since several buffers are involved, each with their own
 ;; local variables and such.  Watch out.
 (interactive)
 (let ((logbuf (get-buffer "*SVN-commit-message*"))
	(commit-bufname "*SVN-commit*")
	(files lvc-svn-commit-files)
	(parent lvc-svn-commit-parent-buffer)
	msg justfiles files-file message-file status)
   ;; Make sure they specified some message.
   (if (string-equal (buffer-string) lvc-svn-commit-initial-buffer-contents)
	(error "Please specify a commit message"))
   (setq justfiles (mapcar 'car files))
   ;; Remove any crap from the buffer, extracting the commit message.
   (lvc-svn-commit-tidy-up-buffer)
   (setq msg (buffer-string))
   ;; Check again for deadbeat messages, reinitialize the buffer if needed.
   (if (string-equal msg "")
	(progn
	  (goto-char (point-min))
	  (insert lvc-svn-commit-initial-buffer-contents)
	  (error "Please specify a non-empty commit message")))
   ;; Make sure any buffers visiting those files aren't dirty.
   (lvc-svn-ensure-saved justfiles)
   ;; Store the list of files and commit message in temp files to
   ;; avoid any limits on argv size.
   (setq files-file (make-temp-name (concat (file-name-as-directory
					      lvc-temp-file-directory)
					     "lvc-svnC"))
	  message-file (make-temp-name (concat (file-name-as-directory
						lvc-temp-file-directory)
					       "lvc-svnM")))
   (unwind-protect
	(progn
	  (with-temp-file files-file
	    (insert (mapconcat 'identity justfiles "\n")))
	  (with-temp-file message-file
	    (insert msg))
	  ;; Do the commit.  We make sure to do it in the parent buffer so
	  ;; CWD, etc is correct.
	  (pop-to-buffer parent)
	  (message "Committing...")
	  (ring-insert lvc-svn-commit-msgs msg)
	  (setq status (lvc-svn-do-command-quietly
			"commit" nil
			(list "--file" message-file
			      "--targets" files-file)))
	  (message "Committing...done"))
     ;; Always clean up.
     (delete-file files-file)
     (delete-file message-file))
   ;; Remove lines in parent buffer for files we successfully committed.
   ;; Complain loudly if the commit failed.
   (if (zerop status)
	(let ((cur justfiles) file)
	  (while cur
	    (setq file (car cur))
	    (lvc-svn-revert-buffers-visiting-file file)
	    (lvc-svn-remove-file-line file)
	    (setq cur (cdr cur)))
	  ;; Only chuck buffer when all is good.
	  (kill-buffer logbuf))
     ;; Commit failed.
     (pop-to-buffer commit-bufname)
     (goto-char (point-min))
     (insert "\n"
	      "*** The commit was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "*** The commit message is in " (buffer-name logbuf) ".\n"
	      "\n")
     (error "Commit failed, see %s buffer for details." commit-bufname))))

(defun lvc-svn-commit-tidy-up-buffer ()
 (save-excursion
   ;; Remove leading blank lines.
   (goto-char (point-min))
   (if (and (re-search-forward "\\S-" nil t)
	     (/= (point) (point-min)))
	(progn
	  (forward-char -1)
	  (delete-region (point-min) (point))))
   ;; Remove the instructions and list of files.
   (if (re-search-forward (concat "^" lvc-svn-commit-delimiter "$") nil t)
	(progn
	  (beginning-of-line)
	  (delete-region (point) (point-max))))
   ;; Trim trailing blank lines
   (goto-char (point-max))
   (if (and (re-search-backward "\\S-" nil t)
	     (/= (point) (point-max)))
	(progn
	  (forward-char 1)
	  (delete-region (point) (point-max))))))

(defun lvc-svn-commit-insert-prev-commit-msg (arg)
 "Cycle backwards thru commit message history."
 (interactive "*p")
 (let ((len (ring-length lvc-svn-commit-msgs)))
   (if (= len 0)
	(error "Empty commit message string")
     (erase-buffer)
     ;; Initialize the index on the first use of this command
     ;; so that the first M-p gets index 0, and the first M-n gets
     ;; index -1.
     (if (null lvc-svn-commit-msgs-index)
	  (setq lvc-svn-commit-msgs-index
		(if (> arg 0) -1
		  (if (< arg 0) 1 0))))
     (setq lvc-svn-commit-msgs-index
	    (mod (+ lvc-svn-commit-msgs-index arg) len))
     (message "Commit Msg %d" (1+ lvc-svn-commit-msgs-index))
     (insert (ring-ref lvc-svn-commit-msgs lvc-svn-commit-msgs-index))
     (insert lvc-svn-commit-initial-buffer-contents))))

(defun lvc-svn-commit-insert-next-commit-msg (arg)
 "Cycle forwards thru commit message history."
 (interactive "*p")
 (lvc-svn-commit-insert-prev-commit-msg (- arg)))


;; Internal functions.

(defun lvc-svn-current-file ()
 (save-excursion
   (beginning-of-line)
   (if (looking-at lvc-svn-linepat)
	(buffer-substring (match-end 0)
			  (progn (end-of-line) (point)))
     (error "No file on this line"))))

(defun lvc-svn-current-file-state ()
 (save-excursion
   (beginning-of-line)
   (if (looking-at lvc-svn-linepat)
	(lvc-svn-parse-state-string (match-string 1))
     (error "No file on this line"))))

(defun lvc-svn-current-file-base ()
 ;; Parse the current line for the base revision info.
 ;; Only makes sense in ustatus mode.  If we can't parse it out
 ;; return "BASE"
 (save-excursion
   (beginning-of-line)
   (if (looking-at lvc-svn-linepat)
	(progn
	  (forward-char 8)
	  (if (looking-at "[ \t]*\\([0-9]+\\)")
	      (string-to-number (match-string 1))
	    "BASE"))
     (error "No file on this line"))))

(defun lvc-svn-set-mark-state (on)
 (save-excursion
   (beginning-of-line)
   (if (not (looking-at lvc-svn-linepat))
	(error "No file on this line")
     (let ((buffer-read-only nil))
	(replace-match (concat (match-string 1) (if on "*" " ")))))))

(defun lvc-svn-parse-state-string (str)
 ;; Parse the state portion of status output and return a list of
 ;; symbols
 (let (state)
   ;; 1st column - general
   (if (string-match "^A" str) (setq state (cons 'added state)))
   (if (string-match "^C" str) (setq state (cons 'conflicted state)))
   (if (string-match "^D" str) (setq state (cons 'deleted state)))
   (if (string-match "^I" str) (setq state (cons 'ignored state)))
   (if (string-match "^M" str) (setq state (cons 'modified state)))
   (if (string-match "^R" str) (setq state (cons 'replaced state)))
   (if (string-match "^X" str) (setq state (cons 'external state)))
   (if (string-match "^\\?" str) (setq state (cons 'unversioned state)))
   (if (string-match "^!" str) (setq state (cons 'missing state)))
   (if (string-match "^~" str) (setq state (cons 'obstructed state)))
   ;; 2nd column - props
   (if (string-match "^.C" str) (setq state (cons 'pconflicted state)))
   (if (string-match "^.M" str) (setq state (cons 'pmodified state)))
   ;; 3rd column - locks
   (if (string-match "^..L" str) (setq state (cons 'locked state)))
   ;; 4th column - adds w/history
   (if (string-match "^...\\+" str) (setq state (cons 'add-history state)))
   ;; 5th column - switched
   (if (string-match "^....S" str) (setq state (cons 'switched state)))
   ;; 6th column - lock token
   (if (string-match "^.....K" str) (setq state (cons 'lock-have state)))
   (if (string-match "^.....O" str) (setq state (cons 'lock-other state)))
   (if (string-match "^.....T" str) (setq state (cons 'lock-stolen state)))
   (if (string-match "^.....B" str) (setq state (cons 'lock-broken state)))
   ;; 7th col blank
   ;; 8th col - out of date info (with -u, otherwise start of file name)
   (if (string-match "^.......\\*" str) (setq state (cons 'out-of-date state)))
   state))

;; commitable-p
;; updated-p
;; conflicted-p

(defun lvc-svn-redraw-modeline (&optional all)
 ;; Emacs doesn't have this XEmacsism.
 (if (fboundp 'redraw-modeline)
     (redraw-modeline all)
   nil))

(defun lvc-svn-bitch-if-commit-in-progress ()
 ;; If a commit is in progress, go to the commit-message buffer and
 ;; tell them what to do.
 ;; XXX/lomew should tie the commit buffer to the status buffer
 ;; and only enforce one commit per status
 (let ((buf (get-buffer "*SVN-commit-message*")))
   (if buf
	(progn
	  (pop-to-buffer buf)
	  (error "Please finish or abort this commit first")))))

;; Too bad Emacs doesn't have this XEmacs feature.
(defun lvc-svn-remassoc (key list)
 "Delete by side effect any elements of LIST whose car is `equal' to KEY.
The modified LIST is returned.  If the first member of LIST has a car
that is `equal' to KEY, there is no way to remove it by side effect;
therefore, write `(setq foo (remassoc key foo))' to be sure of changing
the value of `foo'."
 (if (fboundp 'remassoc)
     (remassoc key list)
   ;; Roll our own...
   (let ((prev list)
	  (cur (cdr list)))
     ;; Check elems 2...end
     (while cur
	(if (equal (car (car cur)) key)
	    (setcdr prev (cdr cur))
	  (setq prev cur))
	(setq cur (cdr cur)))
     ;; Check the head
     (if (equal (car (car list)) key)
	  (cdr list)
	list))))

(defun lvc-svn-get-relevant-files (use-marks &optional noerror)
 ;; Return a list of files in the form of `lvc-svn-marked-files'
 ;; If USE-MARKS is non-nil then use the marked file list,
 ;; otherwise use the current file.
 ;; Optionaly NOERROR means return nil instead of throwing an error
 ;; when no files are marked.
 (if use-marks
     (if lvc-svn-marked-files
	  ;; sort modifies
	  (sort (copy-sequence lvc-svn-marked-files)
		(function (lambda (a b)
			    (string-lessp (car a) (car b)))))
	(if noerror
	    nil
	  (error "No marked files")))
   (list (cons (lvc-svn-current-file)
		(lvc-svn-current-file-state)))))

;; XXX/lomew svn doesn't distinguish between args before the command
;; and args after, clean this up later.
(defun lvc-svn-do-command (cmd default-output svnopts &optional cmdopts)
 ;; Do the svn command `cmd' and print the result in buffer *SVN-`cmd'*.
 ;; If there is no output, insert some default text.
 ;; Returns the command exit status.
 (let ((args (append svnopts (list cmd) cmdopts))
	(bufname (concat "*SVN-" cmd "*"))
	(cwd default-directory)
	status)
   ;; We override `temp-buffer-show-function' so we can insert some
   ;; default text if the command had no output.
   (let ((temp-buffer-show-function
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (if (and lvc-use-diff-mode
			(string-equal cmd "diff")
			(fboundp 'diff-mode))
		   (diff-mode))
	       (setq default-directory cwd)
	       (if (zerop (lvc-svn-buffer-size buf))
		   (insert default-output))
	       (if lvc-svn-debug
		   (progn
		     (goto-char (point-min))
		     (insert (concat "DEBUG: "
				     lvc-svn-command " "
				     (mapconcat 'identity args " ")
				     "\n")))))
	     (let ((win (display-buffer buf)))
	       (if (member cmd lvc-view-mode-commands)
		   (lvc-set-view-mode win buf))))))
     (with-output-to-temp-buffer bufname
	(setq status (apply 'call-process lvc-svn-command
			    nil standard-output
			    nil args))))
   status))

(defun lvc-svn-do-command-quietly (cmd svnopts &optional cmdopts)
 ;; Do the svn command `cmd' and print the result in buffer *SVN-`cmd'*.
 ;; Returns the command exit status.
 (let ((args (append svnopts (list cmd) cmdopts))
	(bufname (concat "*SVN-" cmd "*"))
	(cwd default-directory)
	status buf)
   (setq buf (get-buffer-create bufname))
   (save-excursion
     (set-buffer buf)
     (setq default-directory cwd)
     (setq buffer-read-only nil)
     (erase-buffer))
   (setq status (apply 'call-process lvc-svn-command
			nil buf nil
			args))
   status))

(defun lvc-svn-buffer-size (&optional buffer)
 ;; `buffer-size' in Emacs doesn't take an arg like XEmacs.
 (condition-case nil
     (buffer-size buffer)
   (error (save-excursion
	     (if buffer
		 (set-buffer buffer))
	     (buffer-size)))))

(defun lvc-svn-ensure-saved (files)
 ;; Check for any buffers visiting any of the FILES and offer to save
 ;; them.
 (save-excursion
   (map-y-or-n-p
    (function (lambda (buf)
		 (if (and buf (buffer-modified-p buf))
		     (format "Save file %s? " (buffer-file-name buf)))))
    (function (lambda (buf)
		 (set-buffer buf)
		 (condition-case ()
		     (save-buffer)
		   (error nil))))
    (mapcar 'get-file-buffer files)
    '("file" "files" "save"))))

(defun lvc-svn-revert-buffers-visiting-file (file)
 ;; Revert any buffers visiting FILE.
 ;; FILE can be relative if the current directory of the caller is correct.
 (let ((buf (get-file-buffer file)))
   (if (and buf
	     (file-exists-p file)
	     (not (verify-visited-file-modtime buf))
	     (not (buffer-modified-p buf)))
	(save-excursion
	  (set-buffer buf)
	  (revert-buffer nil t)))))

(defun lvc-svn-remove-file-line (file)
 ;; Delete lines in the (readonly) status buffer that match a filename.
 ;; Removes from marked files too.
 (save-excursion
   (goto-char (point-min))
   ;; Remove the line.
   (let ((buffer-read-only nil))
     (delete-matching-lines (concat lvc-svn-linepat (regexp-quote file) "$")))
   ;; Update marked files.
   (if (assoc file lvc-svn-marked-files)
	(setq lvc-svn-marked-files (lvc-svn-remassoc file lvc-svn-marked-files)))))

(defun lvc-svn-change-this-file-state (newstate)
 ;; Change the displayed state of the file on this line.
 ;; If the file in in the marked list, update that too.
 ;; Only deals with the file state, first column
 (let ((newstatechar (cond ((eq newstate 'added)       ?A)
			    ((eq newstate 'conflicted)  ?C)
			    ((eq newstate 'deleted)     ?D)
			    ((eq newstate 'ignored)     ?I)
			    ((eq newstate 'modified)    ?M)
			    ((eq newstate 'replaced)    ?R)
			    ((eq newstate 'external)    ?X)
			    ((eq newstate 'unversioned) ??)
			    ((eq newstate 'missing)     ?!)
			    ((eq newstate 'obstructed)  ?~)
			    (t (error "Illegal new file state")))))
   ;; Rewrite the first column of the line.
   (beginning-of-line)
   (let ((buffer-read-only nil))
     (subst-char-in-region (point) (1+ (point)) (char-after)
			    newstatechar 'noundo))

   ;; Update the marked files
   (let ((file (lvc-svn-current-file))
	  (curstate (lvc-svn-current-file-state)))
     (if (assoc file lvc-svn-marked-files)
	  (setq lvc-svn-marked-files
		(cons (cons file curstate)
		      (lvc-svn-remassoc file lvc-svn-marked-files)))))))

(defun lvc-svn-change-file-state (file newstate)
 ;; Change the displayed state of a file.
 (save-excursion
   (goto-char (point-min))
   (if (re-search-forward (concat lvc-svn-linepat (regexp-quote file) "$") nil t)
	(lvc-svn-change-this-file-state newstate))))

(defun lvc-svn-info ()
 ;; Call svn info and parse the result into an alist suitable for use with
 ;; the lvc-svn-info-get function.
 (let ((bufname "*SVN-info*")
	status)
   (setq status (lvc-svn-do-command-quietly "info" nil))
   (if (zerop status)
	(lvc-svn-parse-info-buffer bufname)
     (pop-to-buffer bufname)
     (error "svn \"info\" failed, see %s buffer for details" bufname))))

(defun lvc-svn-parse-info-buffer (buf)
 ;; Parse svn info output into an alist.
 (let (result)
   (save-excursion
     (set-buffer buf)
     (goto-char (point-min))
     (while (re-search-forward "^\\([^:]+\\)\\s-*:\\s-*\\(.*\\)" nil t)
	(setq result (cons (cons (buffer-substring (match-beginning 1)
						   (match-end 1))
				 (buffer-substring (match-beginning 2)
						   (match-end 2)))
			   result))))
   result))

(defun lvc-svn-info-get (info str)
 ;; Retrieve the value, if any, of the field corresponding to STR in
 ;; alist INFO, which is presumed to have come from `lvc-svn-info'.
 (cdr (assoc str info)))

(defun lvc-svn-parse-update-buffer (buf)
 ;; Parse svn update output in BUF into an
 ;; alist like ((file state ...) (file state ...) ...)
 (let (result)
   (save-excursion
     (set-buffer buf)
     (goto-char (point-min))
     (while (re-search-forward "^\\([ADUCG ][ADUCG ][B ]  \\)\\(.*\\)" nil t)
	(let ((str (match-string 1))
	      (file (match-string 2))
	      state)
	  (if (string-match "^A" str) (setq state (cons 'added state)))
	  (if (string-match "^D" str) (setq state (cons 'deleted state)))
	  (if (string-match "^U" str) (setq state (cons 'updated state)))
	  (if (string-match "^C" str) (setq state (cons 'conflicted state)))
	  (if (string-match "^G" str) (setq state (cons 'merged state)))

	  (if (string-match "^.A" str) (setq state (cons 'padded state)))
	  (if (string-match "^.D" str) (setq state (cons 'pdeleted state)))
	  (if (string-match "^.U" str) (setq state (cons 'pupdated state)))
	  (if (string-match "^.C" str) (setq state (cons 'pconflicted state)))
	  (if (string-match "^.G" str) (setq state (cons 'pmerged state)))

	  (setq result (cons (cons file state) result)))))
   result))


;; Process-related stuff.

(defun lvc-svn-filter (proc string)
 ;; This is called when there is new input.  The input can be
 ;; any size and may have partial lines.  See the Elisp manual
 ;; where it describes process filters for an explanation of the
 ;; marker magic here.
 (with-current-buffer (process-buffer proc)
   (let ((moving (= (point) (process-mark proc)))
	  (buffer-read-only nil)
	  beg)
     (save-excursion
	;; Insert the text, advancing the process marker and fixing lines.
	(goto-char (process-mark proc))
	(setq beg (point))
	(insert string)
	(set-marker (process-mark proc) (point))
	(lvc-svn-parse-output beg))
     (if moving (goto-char (process-mark proc))))))

(defun lvc-svn-parse-output (beg)
 ;; Scan the output, noting and maybe rewriting some things.
 (goto-char beg)
 (beginning-of-line)			; in case last insert had partial line
 (let ((stuff-to-do t)
	prev-point dont-move)
   (while stuff-to-do
     (setq dont-move nil)

     (cond
      ;; Note the reported HEAD revision.
      ;; Be aware that svn:externals will print their info afterwards
      ;; and we don't want their HEAD, so set this once.
      ;; XXX/lomew this is broken for externals
      ((looking-at "^Status against revision:[ \t]+\\([0-9]+\\)\n")
	(if (equal lvc-svn-head-revision-from-last-ustatus
		   (default-value 'lvc-svn-head-revision-from-last-ustatus))
	    (setq lvc-svn-head-revision-from-last-ustatus
		  (string-to-number (match-string 1))))
	;; Put in the ending dashes
	(beginning-of-line)
	(insert lvc-svn-lots-of-dashes "\n")
	(setq lvc-svn-inserted-final-dashes t))
      ((looking-at "^Performing status on external item at '.*'")
	(end-of-line)
	(insert "\n" lvc-svn-lots-of-dashes)))

     ;; Move forward.  If point changes we have more stuff to do.
     (if dont-move
	  nil
	(setq prev-point (point))
	(forward-line)
	(setq stuff-to-do (> (point) prev-point))))))

(defun lvc-svn-sentinel (proc msg)
 ;; Tell the user the process is done.
 (let* ((buf (process-buffer proc))
	 (buffer-is-visible (get-buffer-window buf 'visible))
	 (msg-long (format "%s process %s" (process-name proc) msg)))
   (if (memq (process-status proc) '(signal exit))
	;; Process is dead.
	(progn
	  ;; Don't tell them when the buffer is visible.
	  (or buffer-is-visible
	      (message msg-long))
	  (if (null (buffer-name buf))
	      ;; Buffer was killed.
	      (set-process-buffer proc nil)
	    ;; Else process died some other way.
	    (set-buffer buf)

	    ;; Hack the modeline.
	    (setq modeline-process
		  (concat ":"
			  (symbol-name (process-status proc))
			  (if (zerop (process-exit-status proc))
			      " OK"
			    (format " [exit-status %d]"
				    (process-exit-status proc)))))
	    (lvc-svn-redraw-modeline)

	    ;; Indicate status in buffer too.  Remember that lcvs-mode
	    ;; makes the buffer read-only.
	    (save-excursion
	      (goto-char (point-max))
	      (setq buffer-read-only nil)
	      (if (not lvc-svn-inserted-final-dashes)
		  (insert lvc-svn-lots-of-dashes "\n"))
	      (insert "\n" msg-long)
	      (forward-char -1)		;back up over the \n to insert the time
	      (insert " at " (substring (current-time-string) 0 19))
	      (forward-char 1)		;and step over it
	      (setq buffer-read-only t))

	    ;; Go to the first file, if there is one, unless the user
	    ;; has already moved.  lvc-svn-next-line will print stuff
	    ;; unless lvc-explain-each-line is nil.  We make it nil
	    ;; if BUF is not visible.  Also, lvc-svn-next-line will error
	    ;; if no files.
	     (if (= (point) (point-min))
		 (let ((lvc-explain-each-line
			(and lvc-explain-each-line
			     (get-buffer-window buf 'visible))))
		   (condition-case nil
		       (lvc-svn-next-line)
		     (error nil)))))))))

(provide 'lvc-svn)

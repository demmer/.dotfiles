;;; lvc-hg.el --- little version control mercurial mode

;; Copyright (C) 2007 Michael Demmer <demmer@cs.berkeley.edu>

(defconst lvc-hg-version "0.1")

(defvar lvc-hg-command "hg"
  "*How to run mercurial")

(defvar lvc-hg-commit-template nil
  "*If non nil, default text to put in commit buffers")

(defvar lvc-hg-explain-each-line t
  "*If non-nil lvc-hg-mode will print a message in the echo area
describing the current line.  This info is always available with
the \\[lvc-explain-this-line] command.")

;
; Plan:
;
; hg status output with ability to add/commit/etc like lcvs does.
;
; hg ???? to find diffs between working dir and local repo
;
; then have keybindings for pull, push, update, merge


(defvar lvc-hg-temp-file-directory
  (or (getenv "TMPDIR")
      (and (file-directory-p "/tmp") "/tmp")
      (and (file-directory-p "C:\\TEMP") "C:\\TEMP")
      (and (file-directory-p "C:\\") "C:\\")
      "/tmp")
  "*Name of a directory where lvc-hg can put temporary files.")

(defvar lvc-hg-revert-confirm t
  "*If non-nil, reverting files will require confirmation.")


;; Internal Vars.

;; Specifies what to search for when looking for the filename
;; in "hg status" output.  Also takes into account the characters
;; we add for marked files.
;; The parens are assumed to enclose the state information.
(defconst lvc-hg-linepat "^\\([ MARC!?I]\\)[ *]")

(defvar lvc-hg-debug nil
  "If non-nil, put lvc-hg into debug mode.")

(defconst lvc-hg-mode-map
  (let ((map (make-sparse-keymap 'lvc-hg-mode-map)))
    (define-key map "?" 'lvc-hg-explain-this-line)
    (define-key map "n" 'lvc-hg-next-line)
    (define-key map "p" 'lvc-hg-prev-line)
    (define-key map "m" 'lvc-hg-mark-file)
    (define-key map "u" 'lvc-hg-unmark-file)
;    (define-key map "U" 'lvc-hg-update-some-files)
    (define-key map "R" 'lvc-hg-revert)
    (define-key map "C" 'lvc-hg-commit)
    (define-key map "d" 'lvc-hg-diff-cur)
    (define-key map "D" 'lvc-hg-diff-tip)
;    (define-key map "e" 'lcvs-ediff)
;    (define-key map "l" 'lvc-hg-log-base)
;    (define-key map "L" 'lvc-hg-log-tip)
;    (define-key map "s" 'lcvs-show-status)
    (define-key map "S" 'lvc-hg-sort)
;    (define-key map "a" 'lcvs-annotate)
    (define-key map "g" 'lvc-hg-refresh-buffer)
    (define-key map "G" 'lvc-hg-status)
    (define-key map "I" 'lvc-hg-incoming)
    (define-key map "O" 'lvc-hg-outgoing)
    (define-key map "f" 'lvc-hg-find-file)
    (define-key map "o" 'lvc-hg-find-file-other-window)
    (define-key map "q" 'lvc-hg-quit-just-bury)
    (define-key map "+" 'lvc-hg-add)
;    (define-key map "-" 'lcvs-remove-crap)
;    (define-key map "\C-k" 'lcvs-kill-region-or-line)
;    (define-key map "\C-w" 'lcvs-kill-region)
;    (define-key map "\C-xu" 'lcvs-undo)
;    (define-key map "\C-xc" 'lcvs-clean)
;    (condition-case ()
;	 ;; This is for XEmacs, will error in Emacs.
;	 (define-key map '(control /) 'lcvs-undo)
;      (error nil))
    (define-key map "\C-c\C-k" 'lvc-hg-kill-process)
    map)
  "Keymap for `lvc-hg-mode'")

(defvar lvc-hg-view-mode-commands
  '("annotate" "log")
  "List of hg commands that get their output put into view-mode.")

(defvar lvc-hg-marked-files nil
  "Alist of marked files.  It is in the form `\(file . status)'.")
(make-variable-buffer-local 'lvc-hg-marked-files)


;; User functions.

(defun lvc-hg-prepare-buffer (op)
  "Prepare the buffer to accept output from operation 'op' by
clearing old results and inserting the header for new ones.
Returns the correct location for the process output."
  (interactive)
  ;; lvc-hg-mode makes the buffer read-only, so we have to take that
  ;; into account here.
  (save-excursion
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    ;; First strip out the old incoming values (if any) and insert the marker
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (if (re-search-forward (format "%s:\n" op) nil t)
	(let ((beg (match-end 0)))
	  (goto-char beg)
	  (while (not (looking-at "---"))
	    (forward-line))
	  (forward-line)
	  (delete-region beg (point)))
      (goto-char (point-max))
      (insert (format "%s:\n\n" op))
      (backward-line 1)
     )
    (buffer-enable-undo (current-buffer))
    (setq buffer-read-only t)
    (point)
    ))

(defun lvc-hg-status (dir &optional want-fresh-buf)
  "Call \"hg status\" in DIR and then call `lvc-hg-mode' (which see).
Optional WANT-FRESH-BUF means don't reuse an existing buffer visiting
the same directory."
  (interactive (lvc-status-get-args))
  (setq lvc-last-dir dir)
  
  (let* ((basename (file-name-nondirectory (directory-file-name dir)))
	 (bufname (format "*HG-status-%s*" basename))
	 (procname (format "hg-status-%s" basename))
	 (buf (get-buffer bufname))
	 (cmd (list lvc-hg-command "status"))
	 proc insert-pt)
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
	(set-buffer buf)
	(setq default-directory dir)
	
	(setq insert-pt (lvc-hg-prepare-buffer "Status"))
	
	;; Set up keybindings etc.
	(lvc-hg-mode)

	;; Make the buffer visible and start the first process in the
	;; sequence.
	(pop-to-buffer buf)
	(setq lvc-current-directory (directory-file-name dir))
	(setq proc (apply 'start-process procname buf cmd))
	(set-marker (process-mark proc) insert-pt)
	(set-process-filter proc (function lvc-hg-filter))
	(set-process-sentinel proc (function lvc-hg-sentinel))))))

(defun lvc-hg-mode ()
  "Major mode for interacting with HG.
The normal entry point is `lvc-hg-status'.

The hook `lvc-hg-mode-hook', if set, is run upon entry.

The following keys have meaning in an `lvc-hg-mode' buffer:
\\{lvc-hg-mode-map}
Some of the commands can work on marked files via a \\[universal-argument]
prefix; consult the individual documentation for each command
via `describe-key' on \\[describe-key]"
  ;; XXX/lomew completions would be nice, but are hassle since I have to
  ;; define a keymap
  (interactive)
  (kill-all-local-variables)
  (setq lvc-hg-marked-files nil)
  (use-local-map lvc-hg-mode-map)
  (setq major-mode 'lvc-hg-mode
	mode-name "LVC-HG")
  (setq modeline-process '(":%s"))
  (setq buffer-read-only t)
  (make-variable-buffer-local 'font-lock-defaults)
;  (setq font-lock-defaults '(lcvs-font-lock-keywords))
  (run-hooks 'lvc-hg-mode-hook))

(defun lvc-hg-incoming ()
  "Update a list of incoming changesets in the status buffer"
  (interactive)
  (let* ((basename (file-name-nondirectory lvc-current-directory))
	 (buf (current-buffer))
	 (cmd (list lvc-hg-command "-y" "-q" "incoming"))
	 (procname (format "lvc-incoming-%s" basename))
	 proc insert-pt)
    
    (if (and (get-buffer-process buf)
	     (eq (process-status (get-buffer-process buf)) 'run))
	(error "%s process already running" procname))

    (setq insert-pt (lvc-hg-prepare-buffer "Incoming"))
    (setq proc (apply 'start-process procname buf cmd))
    (set-marker (process-mark proc) insert-pt)
    (set-process-filter proc (function lvc-hg-filter))
    (set-process-sentinel proc (function lvc-hg-sentinel))
    )
  )

(defun lvc-hg-outgoing ()
  "Update a list of outgoing changesets in the status buffer"
  (interactive)
  (let* ((basename (file-name-nondirectory lvc-current-directory))
	 (buf (current-buffer))
	 (cmd (list lvc-hg-command "-y" "-q" "outgoing"))
	 (procname (format "lvc-outgoing-%s" basename))
	 proc insert-pt)
    
    (if (and (get-buffer-process buf)
	     (eq (process-status (get-buffer-process buf)) 'run))
	(error "%s process already running" procname))

    (setq insert-pt (lvc-hg-prepare-buffer "Outgoing"))
    (setq proc (apply 'start-process procname buf cmd))
    (set-marker (process-mark proc) insert-pt)
    (set-process-filter proc (function lvc-hg-filter))
    (set-process-sentinel proc (function lvc-hg-sentinel))
    )
  )

(defun lvc-hg-explain-this-line ()
  "Explain what this line means.
Translates stuff like \"M foo/bar.c\" into something like \"this file has
been locally modified\"."
  (interactive)
  (let* ((state (lvc-hg-current-file-state))
	 (res (mapconcat 'symbol-name state ", ")))
    (if (memq 'out-of-date state)
	(setq res (concat res (format ", BASE is %d" (lvc-hg-current-file-base)))))
    (message res)))

(defun lvc-hg-next-line ()
  "Move cursor to the next file."
  (interactive)
  (if (re-search-forward lvc-hg-linepat nil t)
      (if lvc-hg-explain-each-line
	  (lvc-hg-explain-this-line))
    (error "No more files")))

(defun lvc-hg-prev-line ()
  "Move cursor to the previous file."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (if (re-search-backward lvc-hg-linepat nil t)
	(progn 
	  (goto-char (match-end 0))
	  (if lvc-hg-explain-each-line
	      (lvc-hg-explain-this-line)))
      (goto-char pt)
      (error "No more files"))))

(defun lvc-hg-refresh-buffer (arg)
  "Re-get the status for this dir.
Prefix arg means to show updatable files if the buffer
previously wasn't."
  (interactive "P")
  (lvc-hg-status default-directory 'fresh))

(defun lvc-hg-quit-just-bury ()
  "\"Quit\" lvc-hg-mode by burying the buffer."
  (interactive)
  (bury-buffer))

(defun lvc-hg-kill-process ()
  "Kill the hg process, if there is one.
We assume the current buffer is the one that is supposed to be running
a hg process."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (interrupt-process (get-buffer-process (current-buffer)))
    (error "No hg process running")))

(defun lvc-hg-mark-file ()
  "Mark the file on this line for later processing."
  (interactive)
  (lvc-hg-bitch-if-commit-in-progress)
  (let ((file (lvc-hg-current-file))
	(state (lvc-hg-current-file-state)))
    (if (assoc file lvc-hg-marked-files)
	nil
      (lvc-hg-set-mark-state t)
      (setq lvc-hg-marked-files (cons (cons file state) lvc-hg-marked-files))))
  ;; `lvc-hg-next-line' can error if at the end of files.
  (condition-case nil
      (lvc-hg-next-line)
    (error nil)))

(defun lvc-hg-unmark-file ()
  "Remove the file on this line from the list of to-be-processed files.
See also `lvc-hg-mark-file'."
  (interactive)
  (lvc-hg-bitch-if-commit-in-progress)
  (let ((file (lvc-hg-current-file)))
    (if (not (assoc file lvc-hg-marked-files))
	nil
      (lvc-hg-set-mark-state nil)
      (setq lvc-hg-marked-files (lvc-hg-remassoc file lvc-hg-marked-files))))
  ;; `lvc-hg-next-line' can error if at the end of files.
  (condition-case nil
      (lvc-hg-next-line)
    (error nil)))

(defun lvc-hg-diff-cur (arg)
  "Diff some files against the repository.
Use this when you have locally modified files and want to see what
you have done.  See also `lvc-hg-diff-tip'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Diffing...")
  (lvc-hg-do-command "diff"
		   "No differences with the repository"
		   nil
		   (mapcar 'car (lvc-hg-get-relevant-files arg)))
  (message "Diffing...done"))

(defun lvc-hg-diff-head (arg)
  "Diff some files against the HEAD revision.
Use this when files have been checked in by someone else and you want
to see what has changed before you update your copies.  See also
`lvc-hg-diff-base'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Diffing...")
  (lvc-hg-do-command "diff"
		   "No differences with the HEAD"
		   nil
		   (cons (format "-rBASE:%s"
				 lvc-hg-head-revision-from-last-ustatus)
			 (mapcar 'car (lvc-hg-get-relevant-files arg))))
  (message "Diffing...done"))

;; XXX/lomew work in -v and --stop-on-copy
;; XXX/lomew work in the repositioning stuff?
;;   this would assume we are logging from beyond BASE
(defun lvc-hg-log-base ()
  "Show log for the current file.  Logs from BASE to the earliest revision."
  (interactive)
  (message "Logging...")
  (lvc-hg-do-command "log"
		   "No output"
		   nil
		   (list "-v" (lvc-hg-current-file)))
  (message "Logging...done"))

(defun lvc-hg-log-head ()
  "Shows the log for revisions you would get if you updated this file."
  (interactive)
  (message "Logging...")
  (let ((base (lvc-hg-current-file-base)))
    (if (numberp base)
	;; Don't include BASE itself since we already have that and don't
	;; want to show it (in case this file was changed in BASE).
	(setq base (1+ base)))
    (lvc-hg-do-command "log"
		     "No output"
		     nil
		     (list "-v"
			   (format "-r%s:%s"
				   lvc-hg-head-revision-from-last-ustatus
				   base)
			   (lvc-hg-current-file))))
  (message "Logging...done"))

(defun lvc-hg-sort ()
  "Sort the HG output in this buffer.
This is useful to get files with similar status together."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$")
    (beginning-of-line)
    (forward-char 1)
    (unwind-protect
	(let ((beg (point)))
	  (setq buffer-read-only nil)
	  (sort-lines nil beg (progn (if (re-search-forward "^$" nil t)
					 (point)
				       (point-max)))))
      (setq buffer-read-only t))))

(defun lvc-hg-commit (arg)
  "Commit files.
If given a prefix argument, commit the marked files.  Otherwise commit
the file on this line."
  (interactive "P")
  (lvc-hg-bitch-if-commit-in-progress)
  (let ((files (lvc-hg-get-relevant-files arg))
	(this-buffer (current-buffer))
	cur state)
    ;; Don't let them do something dumb.
    ;; This should match what we display in the "HG:..." lines.
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
    (pop-to-buffer (get-buffer-create "*HG-commit-message*"))
    (lvc-hg-commit-mode this-buffer files)))

(defun lvc-hg-find-file (&optional arg)
  "Find the file on this line, or the marked files if given a prefix arg."
  (interactive "P")
  (mapcar (function (lambda (pair)
		      (if (file-exists-p (car pair))
			  (save-excursion
			    (find-file (car pair))))))
	  (lvc-hg-get-relevant-files arg)))

(defun lvc-hg-find-file-other-window ()
  "Find the file on this line."
  (interactive)
  (let ((file (lvc-hg-current-file)))
    (if (file-exists-p file)
	(find-file-other-window file)
      (error (format "%s not in working copy, probably a new file" file)))))

(defun lvc-hg-add (arg)
  "Schedule unversioned files for addition.
If given a prefix argument, add the marked files.  Otherwise add
the file on this line.
The files won't be actually added to the repository until they are
formally committed."
  (interactive "P")
  (let ((files (lvc-hg-get-relevant-files arg))
	status cur)
    ;; Check thru the files for addable ones.  These are only ?/unversioned ones
    (setq cur files)
    (while cur
      (setq state (cdr (car cur)))
      (setq cur (cdr cur))
      (cond
       ((memq 'unversioned state)
	nil)
       ((memq 'deleted state)
	;; XXX/lomew it would be nice to collect these and then do a hg revert
	;; for them in addition to the add
	(error (substitute-command-keys
		"Deleted files can't be added, use revert, on \\[lvc-hg-revert]")))
       (t
	(error "Can only add unversioned files"))))
    (message "Adding...")
    (setq status (lvc-hg-do-command-quietly "add" nil (mapcar 'car files)))
    (message "Adding...done")
    (if (zerop status)
	;; Update the diplayed state of the files to "A" from "?"
	(let ((cur files)
	      pair)
	  (while cur
	    (setq pair (car cur))
	    (setq cur (cdr cur))
	    (lvc-hg-change-file-state (car pair) 'added)))
      ;; Otherwise an error happened, bitch appropriately
      (pop-to-buffer "*HG-add*")
      (goto-char (point-min))
      (insert "\n"
	      "*** The add was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "\n")
      (error "Add failed, see *HG-add* buffer for details."))))

(defun lvc-hg-update-some-files (arg)
  "Update some files.
If given a prefix arg, update the working copy to HEAD,
otherwise just this file."
  (interactive "P")
  (let ((filename (if arg "." (lvc-hg-current-file)))
	(nconflicts 0)
	update-msg status)
    (setq update-msg (format "Updating %s to %s"
			     (if (string-equal filename ".")
				 "working copy"
			       filename)
			     (if (numberp lvc-hg-head-revision-from-last-ustatus)
				 (format "HEAD (%d)"
					 lvc-hg-head-revision-from-last-ustatus)
			       "HEAD")))
    (message (format "%s..." update-msg))
    (setq status (lvc-hg-do-command-quietly
		  "update"
		  nil
		  (list (format "-r%s" lvc-hg-head-revision-from-last-ustatus)
			filename)))
    (message (format "%s...done" update-msg))

    (pop-to-buffer "*HG-update*")
    (goto-char (point-min))
    (insert "\n"
	    "*** XXX/lomew deal with this buffer\n"
	    "\n")))

;    (if (zerop status)
;	;; Parse the update output and update the displayed state accordingly.
;	(let ((cur (lvc-hg-parse-update-buffer "*HG-update*"))
;	      newstate file)
;	  (while cur
;	    (setq newstate (car (car cur)))
;	    (setq file (cdr (car cur)))
;	    (setq cur (cdr cur))
;	    (if (memq 'updated newstate)
;		(lvc-hg-remove-file-line file)
;	      (lvc-hg-change-file-state file 
;	    (if newpair
;		(let ((newstate (cdr newpair)))
;		  (if (or (equal newstate ?U) (equal newstate ?P))
;		      (lcvs-remove-file-line file)
;		    (lcvs-change-file-state file newstate))
;		  (lcvs-revert-buffers-visiting-file file)
;		  ;; If there were any conflicts, we want them to know.
;		  (if (equal newstate ?C)
;		      (setq nconflicts (1+ nconflicts))))
;	      ;; If there is no match for this file it means it is up to date,
;	      ;; so we delete its status line.
;	      ;; This typically happens with files that have been committed
;	      ;; behind our back, like thru vc-mode.
;	      (lcvs-remove-file-line file)))
;	  ;; If they operated on the marked list, unmark everything.
;	  (if (> (length files) 1)
;	      (lcvs-unmark-all-files))))    

(defun lvc-hg-revert (arg)
  "Revert some files, discarding local changes.
By default reverts the file on this line.
If supplied with a prefix argument, revert the marked files.
By default this command requires confirmation to remove the files.  To
disable the confirmation, you can set `lvc-hg-revert-confirm' to nil."
  (interactive "P")
  (let* ((files (lvc-hg-get-relevant-files arg))
	 (multiple-p (cdr files))
	 status)
    (if (and lvc-hg-revert-confirm
	     (not (yes-or-no-p (format "Revert %s? "
				       (if multiple-p
					   "the marked files"
					 (car (car files)))))))
	(message "Revert cancelled")
      (message "Reverting...")
      (setq status (lvc-hg-do-command-quietly "revert" nil (mapcar 'car files)))
      (message "Reverting...done")
      (if (zerop status)
	  ;; Revert some buffers and remove the file lines from the status buf
	  (let ((cur files)
		pair file)
	    (while cur
	      (setq pair (car cur))
	      (setq cur (cdr cur))
	      (setq file (car pair))
	      (lvc-hg-revert-buffers-visiting-file file)
	      (lvc-hg-remove-file-line file)))
      ;; Otherwise an error happened, bitch appropriately
      (pop-to-buffer "*HG-revert*")
      (goto-char (point-min))
      (insert "\n"
	      "*** The revert was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "\n")
      (error "Revert failed, see *HG-revert* buffer for details.")))))


;; The committing major mode

(defvar lvc-hg-commit-msgs (make-ring 10)
  "Stores last few commit messages.")

(defvar lvc-hg-commit-msgs-index nil)
(make-variable-buffer-local 'lvc-hg-commit-msgs-index)

(defvar lvc-hg-commit-initial-buffer-contents ""
"Contents of the commit buffer when we initially prepare it.
Used for the commit message ring.")
(make-variable-buffer-local 'lvc-hg-commit-initial-buffer-contents)

(defconst lvc-hg-commit-delimiter
  "-- This line, and those below, will be ignored --")

(defvar lvc-hg-commit-parent-buffer nil
  "The examine/update mode buffer.
For commit-mode buffers.")
(make-variable-buffer-local 'lvc-hg-commit-parent-buffer)

(defvar lvc-hg-commit-files nil
  "Alist of the files to be committed.
It has the same form as `lvc-hg-marked-files'.
For commit-mode buffers.")
(make-variable-buffer-local 'lvc-hg-commit-files)

(defvar lvc-hg-commit-mode-map
  (let ((map (make-sparse-keymap 'lvc-hg-commit-mode-map)))
    (define-key map "\C-c\C-c" 'lvc-hg-commit-finish)
    (define-key map "\M-p" 'lvc-hg-commit-insert-prev-commit-msg)
    (define-key map "\M-n" 'lvc-hg-commit-insert-next-commit-msg)
    map)
  "Keymap for `lvc-hg-commit-mode'")

(defun lvc-hg-commit-mode (parent files)
  "Major mode for providing a commit log message and committing files.
This mode is not meant to be user invoked."
  (interactive)

  (setq lvc-hg-commit-parent-buffer parent)
  (setq lvc-hg-commit-files (sort files (lambda (a b)
					(string-lessp (car a) (car b)))))

  (use-local-map lvc-hg-commit-mode-map)
  (setq major-mode 'lvc-hg-commit-mode)
  (setq mode-name "HG-Commit")

  (setq lvc-hg-commit-msgs-index nil)

  (lvc-hg-prepare-commit-buffer files)
  (setq lvc-hg-commit-initial-buffer-contents (buffer-string))
  (goto-char (point-min))
  (if lvc-hg-commit-template
      (insert lvc-hg-commit-template))
  (set-buffer-modified-p nil)

  (message (substitute-command-keys "Type \\[lvc-hg-commit-finish] when done."))
  (run-hooks 'text-mode-hook))

;; Insert stuff to show them what files they're affecting.
;; Imitate logic from hg commit.
;; XXX/lomew I think the affected files output could be more readable.
(defun lvc-hg-prepare-commit-buffer (files)
  (insert "\n\n")
  (insert lvc-hg-commit-delimiter)
  (insert "\n")
  (insert (substitute-command-keys
	   "-- Type \\[lvc-hg-commit-finish] when done --\n"))
  (insert "\n")
  (while files
    (let ((txtmod ?_)
	  (file (car (car files)))
	  (state (cdr (car files))))
      ;; text
      (cond ((memq 'added state)	(setq info "added"))
	    ((memq 'deleted state)	(setq info "removed"))
	    ((memq 'modified state)	(setq info "changed")))
      (insert (format "%s %s\n" info file))
      (setq files (cdr files)))))

(defun lvc-hg-commit-finish ()
  ;; Finish up the commit by grabbing the commit string and calling hg commit.
  ;; If the commit worked, clear out the affected files from the parent buffer.
  ;; Otherwise complain loudly and pop up the commit output.
  ;;
  ;; This is tricky since several buffers are involved, each with their own
  ;; local variables and such.  Watch out.
  (interactive)
  (let ((logbuf (get-buffer "*HG-commit-message*"))
	(commit-bufname "*HG-commit*")
	(files lvc-hg-commit-files)
	(parent lvc-hg-commit-parent-buffer)
	msg justfiles status)
    ;; Make sure they specified some message.
    (if (string-equal (buffer-string) lvc-hg-commit-initial-buffer-contents)
	(error "Please specify a commit message"))
    (setq justfiles (mapcar 'car files))
    ;; Remove any crap from the buffer, extracting the commit message.
    (lvc-hg-commit-tidy-up-buffer)
    (setq msg (buffer-string))
    ;; Check again for deadbeat messages, reinitialize the buffer if needed.
    (if (string-equal msg "")
	(progn
	  (goto-char (point-min))
	  (insert lvc-hg-commit-initial-buffer-contents)
	  (error "Please specify a non-empty commit message")))
    ;; Make sure any buffers visiting those files aren't dirty.
    (lvc-hg-ensure-saved justfiles)

    (unwind-protect
	(progn
	  ;; Do the commit.  We make sure to do it in the parent buffer so
	  ;; CWD, etc is correct.
	  (pop-to-buffer parent)
	  (message "Committing...")
	  (ring-insert lvc-hg-commit-msgs msg)
	  (setq status (lvc-hg-do-command-quietly
			"commit" nil
			(append (list "-m" msg) justfiles)))
	  (message "Committing...done"))
      )
    ;; Remove lines in parent buffer for files we successfully committed.
    ;; Complain loudly if the commit failed.
    (if (zerop status)
	(let ((cur justfiles) file)
	  (while cur
	    (setq file (car cur))
	    (lvc-hg-revert-buffers-visiting-file file)
	    (lvc-hg-remove-file-line file)
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

(defun lvc-hg-commit-tidy-up-buffer ()
  (save-excursion
    ;; Remove leading blank lines.
    (goto-char (point-min))
    (if (and (re-search-forward "\\S-" nil t)
	     (/= (point) (point-min)))
	(progn
	  (forward-char -1)
	  (delete-region (point-min) (point))))
    ;; Remove the instructions and list of files.
    (if (re-search-forward (concat "^" lvc-hg-commit-delimiter "$") nil t)
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

(defun lvc-hg-commit-insert-prev-commit-msg (arg)
  "Cycle backwards thru commit message history."
  (interactive "*p")
  (let ((len (ring-length lvc-hg-commit-msgs)))
    (if (= len 0)
	(error "Empty commit message string")
      (erase-buffer)
      ;; Initialize the index on the first use of this command
      ;; so that the first M-p gets index 0, and the first M-n gets
      ;; index -1.
      (if (null lvc-hg-commit-msgs-index)
	  (setq lvc-hg-commit-msgs-index
		(if (> arg 0) -1
		  (if (< arg 0) 1 0))))
      (setq lvc-hg-commit-msgs-index
	    (mod (+ lvc-hg-commit-msgs-index arg) len))
      (message "Commit Msg %d" (1+ lvc-hg-commit-msgs-index))
      (insert (ring-ref lvc-hg-commit-msgs lvc-hg-commit-msgs-index))
      (insert lvc-hg-commit-initial-buffer-contents))))

(defun lvc-hg-commit-insert-next-commit-msg (arg)
  "Cycle forwards thru commit message history."
  (interactive "*p")
  (lvc-hg-commit-insert-prev-commit-msg (- arg)))


;; Internal functions.

(defun lvc-hg-current-file ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at lvc-hg-linepat)
	(buffer-substring (match-end 0)
			  (progn (end-of-line) (point)))
      (error "No file on this line"))))

(defun lvc-hg-current-file-state ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at lvc-hg-linepat)
	(lvc-hg-parse-state-string (match-string 1))
      (error "No file on this line"))))

(defun lvc-hg-current-file-base ()
  ;; Parse the current line for the base revision info.
  ;; Only makes sense in ustatus mode.  If we can't parse it out
  ;; return "BASE"
  (save-excursion
    (beginning-of-line)
    (if (looking-at lvc-hg-linepat)
	(progn
	  (forward-char 8)
	  (if (looking-at "[ \t]*\\([0-9]+\\)")
	      (string-to-number (match-string 1))
	    "BASE"))
      (error "No file on this line"))))

(defun lvc-hg-set-mark-state (on)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at lvc-hg-linepat))
	(error "No file on this line")
      (setq buffer-read-only nil)
      (replace-match (concat (match-string 1) (if on "*" " ")))
      (setq buffer-read-only t))))

(defun lvc-hg-parse-state-string (str)
  ;; Parse the state portion of status output and return a list of
  ;; symbols
  (let (state)
    ;; 1st column - general
    (if (string-match "^A" str) (setq state (cons 'added state)))
    (if (string-match "^C" str) (setq state (cons 'clean state)))
    (if (string-match "^I" str) (setq state (cons 'ignored state)))
    (if (string-match "^M" str) (setq state (cons 'modified state)))
    (if (string-match "^R" str) (setq state (cons 'deleted state)))
    (if (string-match "^\\?" str) (setq state (cons 'unversioned state)))
    (if (string-match "^!" str) (setq state (cons 'missing state)))
    state))

;; commitable-p
;; updated-p
;; conflicted-p

(defun lvc-hg-redraw-modeline (&optional all)
  ;; Emacs doesn't have this XEmacsism.
  (if (fboundp 'redraw-modeline)
      (redraw-modeline all)
    nil))

(defun lvc-hg-bitch-if-commit-in-progress ()
  ;; If a commit is in progress, go to the commit-message buffer and
  ;; tell them what to do.
  ;; XXX/lomew should tie the commit buffer to the status buffer
  ;; and only enforce one commit per status
  (let ((buf (get-buffer "*HG-commit-message*")))
    (if buf
	(progn
	  (pop-to-buffer buf)
	  (error "Please finish or abort this commit first")))))

;; Too bad Emacs doesn't have this XEmacs feature.
(defun lvc-hg-remassoc (key list)
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

(defun lvc-hg-get-relevant-files (use-marks &optional noerror)
  ;; Return a list of files in the form of `lvc-hg-marked-files'
  ;; If USE-MARKS is non-nil then use the marked file list,
  ;; otherwise use the current file.
  ;; Optionaly NOERROR means return nil instead of throwing an error
  ;; when no files are marked.
  (if use-marks
      (if lvc-hg-marked-files
	  ;; sort modifies
	  (sort (copy-sequence lvc-hg-marked-files)
		(function (lambda (a b)
			    (string-lessp (car a) (car b)))))
	(if noerror
	    nil
	  (error "No marked files")))
    (list (cons (lvc-hg-current-file)
		(lvc-hg-current-file-state)))))

;; XXX/lomew hg doesn't distinguish between args before the command
;; and args after, clean this up later.
(defun lvc-hg-do-command (cmd default-output hgopts &optional cmdopts)
  ;; Do the hg command `cmd' and print the result in buffer *HG-`cmd'*.
  ;; If there is no output, insert some default text.
  ;; Returns the command exit status.
  (let ((args (append hgopts (list cmd) cmdopts))
	(bufname (concat "*HG-" cmd "*"))
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
	       (if (zerop (lvc-hg-buffer-size buf))
		   (insert default-output))
	       (if lvc-hg-debug
		   (progn
		     (goto-char (point-min))
		     (insert (concat "DEBUG: "
				     lvc-hg-command " "
				     (mapconcat 'identity args " ")
				     "\n")))))
	     (let ((win (display-buffer buf)))
	       (if (member cmd lvc-hg-view-mode-commands)
		   (lvc-hg-set-view-mode win buf))))))
      (with-output-to-temp-buffer bufname
	(setq status (apply 'call-process lvc-hg-command
			    nil standard-output
			    nil args))))
    status))

(defun lvc-hg-do-command-quietly (cmd hgopts &optional cmdopts)
  ;; Do the hg command `cmd' and print the result in buffer *HG-`cmd'*.
  ;; Returns the command exit status.
  (let ((args (append hgopts (list cmd) cmdopts))
	(bufname (concat "*HG-" cmd "*"))
	(cwd default-directory)
	status buf)
    (setq buf (get-buffer-create bufname))
    (save-excursion
      (set-buffer buf)
      (setq default-directory cwd)
      (setq buffer-read-only nil)
      (erase-buffer))
    (setq status (apply 'call-process lvc-hg-command
			nil buf nil
			args))
    status))

(defun lvc-hg-set-view-mode (win buf)
  ;; Turn view-mode on for BUF in window WIN, making sure quitting it
  ;; will get us back somewhere sane.
  (if (not lvc-hg-use-view-mode)
      nil
    (let ((prevwin (selected-window))
	  (prevbuf (current-buffer)))
      (save-excursion
	(set-buffer buf)
	(condition-case nil
	    (view-mode prevbuf 'kill-buffer) ;XEmacs
	  (error
	   (view-mode-enter (list win prevwin) 'kill-buffer)
	   (setq buffer-read-only t))))))) ;Emacs

(defun lvc-hg-buffer-size (&optional buffer)
  ;; `buffer-size' in Emacs doesn't take an arg like XEmacs.
  (condition-case nil
      (buffer-size buffer)
    (error (save-excursion
	     (if buffer
		 (set-buffer buffer))
	     (buffer-size)))))

(defun lvc-hg-ensure-saved (files)
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

(defun lvc-hg-revert-buffers-visiting-file (file)
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

(defun lvc-hg-remove-file-line (file)
  ;; Delete lines in the (readonly) status buffer that match a filename.
  ;; Removes from marked files too.
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (unwind-protect
	(progn
	  ;; Remove the line.
	  (delete-matching-lines (concat lvc-hg-linepat (regexp-quote file) "$"))
	  ;; Update marked files.
	  (if (assoc file lvc-hg-marked-files)
	      (setq lvc-hg-marked-files (lvc-hg-remassoc file lvc-hg-marked-files))))
      (setq buffer-read-only t))))

(defun lvc-hg-change-this-file-state (newstate)
  ;; Change the displayed state of the file on this line.
  ;; If the file in in the marked list, update that too.
  ;; Only deals with the file state, first column
  (setq buffer-read-only nil)
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
    (subst-char-in-region (point) (1+ (point)) (char-after)
			  newstatechar 'noundo)

    ;; Update the marked files
    (let ((file (lvc-hg-current-file))
	  (curstate (lvc-hg-current-file-state)))
      (if (assoc file lvc-hg-marked-files)
	  (setq lvc-hg-marked-files
		(cons (cons file curstate)
		      (lvc-hg-remassoc file lvc-hg-marked-files))))))
  (setq buffer-read-only t))

(defun lvc-hg-change-file-state (file newstate)
  ;; Change the displayed state of a file.
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat lvc-hg-linepat (regexp-quote file) "$") nil t)
	(lvc-hg-change-this-file-state newstate))))


;; Process-related stuff.

(defun lvc-hg-filter (proc string)
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
	(lvc-hg-parse-output beg))
      (if moving (goto-char (process-mark proc))))))

(defun lvc-hg-parse-output (beg)
  ;; Scan the output, noting and maybe rewriting some things.
  (goto-char beg)
  (beginning-of-line)			; in case last insert had partial line
  (let ((stuff-to-do t)
	prev-point dont-move)
    (while stuff-to-do
      (setq dont-move nil)

      ;; Move forward.  If point changes we have more stuff to do.
      (if dont-move
	  nil
	(setq prev-point (point))
	(forward-line)
	(setq stuff-to-do (> (point) prev-point))))))

(defun lvc-hg-sentinel (proc msg)
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
	    (lvc-hg-redraw-modeline)

	    (save-excursion
	      (goto-char (process-mark proc))
	      (let ((buffer-read-only nil))
		(insert "---\n"))
	      )

;; 	    ;; Indicate status in buffer too.  Remember that lvc-hg-mode
;; 	    ;; makes the buffer read-only.
;; 	    (save-excursion
;; 	      (goto-char (point-max))
;; 	      (setq buffer-read-only nil)
;; 	      (insert "\n---" msg-long)
;; 	      (forward-char -1)		;back up over the \n to insert the time
;; 	      (insert " at " (substring (current-time-string) 0 19) "\n")
;; 	      (forward-char 1)		;and step over it
;; 	      (setq buffer-read-only t))

	    ;; Go to the first file, if there is one, unless the user
	    ;; has already moved.  lvc-hg-next-line will print stuff
	    ;; unless lvc-hg-explain-each-line is nil.  We make it nil
	    ;; if BUF is not visible.  Also, lvc-hg-next-line will error
	    ;; if no files.
	    (if (= (point) (point-min))
		(let ((lvc-hg-explain-each-line
		       (and lvc-hg-explain-each-line
			    (get-buffer-window buf 'visible))))
		  (condition-case nil
		      (lvc-hg-next-line)
		    (error nil)))))))))

(provide 'lvc-hg)

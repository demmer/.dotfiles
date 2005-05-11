;;; dsvn.el --- demmer's svn mode (heavily stolen from lcvs.el)

;; Copyright (C) 2005- Michael Demmer <demmer@cs.berkeley.edu>
;; Created: April 2005
;; Version: 1.1 ($Revision: 1.10 $)
(defconst dsvn-version "1.1")

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

;; INSTALLATION
;;
;; Put this in some directory, for example, ~/emacs/.  Add this directory
;; to your load-path:
;;	(setq load-path (cons (expand-file-name "~/emacs") load-path))
;;
;; Add an autoload so this file gets loaded when dsvn commands are invoked:
;;	(autoload 'dsvn-examine "dsvn" nil t)
;;	(autoload 'dsvn-update "dsvn" nil t)
;;
;; To get the most out of the diff-mode we use do this
;;	(add-hook 'diff-mode-hook 'turn-on-font-lock)
;;
;; WARNING: If this is byte compiled on XEmacs, it won't work
;; correctly on Emacs It will fail in mysterious ways.  I haven't
;; tried the reverse (this might not still be the case, I haven't
;; tried it recently.)
;;

;; BUGS:
;; - Probably many 


;; User vars.

;; we depend on some lcvs functions
(require 'lcvs)

(defvar dsvn-svn-command "svn"
  "*How to call svn.")

(defvar dsvn-explain-each-line t
  "*If non-nil dsvn-mode will print a message in the echo area
describing the current line.  This info is always available with
the \\[dsvn-explain-this-line] command.")

(defvar dsvn-log-from-repository nil
  "*If non-nil \\[dsvn-show-log] will show the log based on the
current state in the repository, not on the local checked out copy.")

(defvar dsvn-log-restrict-to-changes nil
  "*If non-nil \\[dsvn-show-log] will show output for changes more recent
than the current repository version.")

(defvar dsvn-use-view-mode nil
  "*If non nil, dsvn will use view-mode in log, status, etc, buffers.")

(defvar dsvn-use-diff-mode t
  "*If non nil, dsvn will put diff buffers into diff-mode (if available).")

(defvar dsvn-revert-confirm t
  "*If non-nil, reverting files will require confirmation.")

(defvar dsvn-remove-confirm t
  "*If non-nil, removing files will require confirmation.")

(defvar dsvn-translate-update-output t
  "*If non-nil, dsvn will translate svn update output in examine mode.
This rewrites things like `file is no longer in the repository' as
a `U' line so you can update it in dsvn, not having to go to a shell.")

(defvar dsvn-UP-face
  (let ((face (make-face 'dsvn-UP-face)))
    (condition-case nil
	(make-face-bold face)
      (error (set-face-foreground face "purple")))
    face))

(defvar dsvn-M-face
  (let ((face (make-face 'dsvn-M-face)))
    (condition-case nil
	(make-face-bold face)
      (error nil))
    (set-face-foreground face "royalblue")
    face))

(defvar dsvn-C-face
  (let ((face (make-face 'dsvn-C-face)))
    (condition-case nil
	(make-face-bold face)
      (error nil))
    (set-face-foreground face "red")
    face))

(defvar dsvn-other-face
  (let ((face (make-face 'dsvn-other-face)))
    (condition-case nil
	(make-face-italic face)
      (error (set-face-foreground face "green")))
    face))

(defvar dsvn-font-lock-keywords
  '(("^.      \\*.*" . dsvn-UP-face)
    ("^M[ *].*" . dsvn-M-face)
    ("^C[ *].*" . dsvn-C-face)))
(setq dsvn-font-lock-keywords
  '(("^M      \\*.*" . dsvn-C-face)
    ("^C[ *].*" . dsvn-C-face)
    ("^M[ *].*" . dsvn-M-face)
    ("^.      \\*.*" . dsvn-UP-face)))


;; Internal Vars.

;; Specifies what to search for when looking for the filename
;; in "svn status" output.
;; The parentheses match all the state characters.
(defvar dsvn-status-regexp "^\\([ ACDGIMRX?!~][ CM][ L][ +][ S]  [ *]\\)\\([ ]+[0-9]*[ ]+[ *]\\)")
(setq dsvn-status-regexp "^\\([ ACDGIMRX?!~][ CM][ L][ +][ S]  [ *]\\)\\([ ]+[0-9]*[ ]+[ *]\\)")

;; Describes how a marked line looks.
(defvar dsvn-marked-file-regexp "^[ ACDGIMRX?!~][ CM][ L][ +][ S]  [ *][ ]+[0-9]*[ ]+\\*")

;; Describes lines for files for which a "svn update" would make
;; sense. Basically identical to the status-regexp, except that the *
;; must be set in the 8th column to indicate that the file was changed
;; on the server and we don't include '?' files.
(defvar dsvn-updatable-regexp "^[ ACDGIMRX!~][ CM][ L][ +][ S]  \\*[ ]+[0-9]*[ ]+[ *]")

;; Describes lines for files for which a "svn resolved" would make
;; sense. 
(defvar dsvn-conflicted-regexp "^C[ CM][ L][ +][ S]  \\*[ ]+[0-9]*[ ]+[ *]")

;; List of regexps describing lines that `dsvn-clean' will remove in
;; an update mode buffer.
(defvar dsvn-dirty-regexps '("^\\([UP]\\)[ *]"
			     "^svn update: warning: .* was lost"))

(defun dsvn-local-state (state)
  (aref state 0))

(defun dsvn-property-state (state)
  (aref state 1))

(defun dsvn-update-state (state)
  (aref state 7))

;; Extract the 
(defun dsvn-status-explain (state)
  (let ((local-state    (dsvn-local-state state))
	(property-state (dsvn-property-state state))
	(update-state   (dsvn-update-state state))
	(result nil))
    
    (cond
     ((equal local-state ?M)
      (setq result "File has been locally modified"))
     
     ((equal local-state ?A)
      (setq result "File has been added"))

     ((equal local-state ?R)
      (setq result "File has been removed"))

     ((equal local-state ?C)
      (setq result "File has a conflict"))

     ((equal local-state ??)
      (setq result "File is unknown to SVN"))
     )

    (if (equal property-state ?M)
	(setq result
	      (if (equal result nil)
		  "File properties have been modified"
		(concat result " and properties have been modified"))))

    (if (equal update-state ?*)
	(setq result
	      (if (equal result nil)
		  "File has been updated in the repository"
		(concat result " and has been updated in the repository"))))

    result
    ))
     
;; XXX/demmer todo
(defun dsvn-update-explain (state)
  )

(defvar dsvn-view-mode-commands
  '("annotate" "log" "stat")
  "List of SVN commands that get their output put into view-mode.")

(defvar dsvn-debug nil
  "If non-nil, put dsvn into debug mode.")

(defvar dsvn-mode-map
  (let ((map (make-sparse-keymap 'dsvn-mode-map)))
    (define-key map "?" 'dsvn-explain-this-line)
    (define-key map "n" 'dsvn-next-line)
    (define-key map "p" 'dsvn-prev-line)
    (define-key map "m" 'dsvn-mark-file)
    (define-key map "u" 'dsvn-unmark-file)
    (define-key map "U" 'dsvn-update-some-files)
    (define-key map "-" 'dsvn-remove)
    (define-key map "R" 'dsvn-revert)
    (define-key map "r" 'dsvn-resolve-conflict)
    (define-key map "C" 'dsvn-commit)
    (define-key map "i" 'dsvn-ignore)
    (define-key map "d" 'dsvn-diff-base)
    (define-key map "D" 'dsvn-diff-head)
    (define-key map "e" 'dsvn-ediff)
    (define-key map "l" 'dsvn-show-log)
    (define-key map "L" 'dsvn-show-full-log)
    (define-key map "x" 'dsvn-show-changed-log)
    (define-key map "s" 'dsvn-show-status)
    (define-key map "S" 'dsvn-sort)
    (define-key map "a" 'dsvn-annotate)
    (define-key map "g" 'dsvn-refresh-buffer)
    (define-key map "G" 'dsvn-examine-or-update)
    (define-key map "f" 'dsvn-find-file)
    (define-key map "o" 'dsvn-find-file-other-window)
    (define-key map "q" 'dsvn-quit-just-bury)
    (define-key map "+" 'dsvn-add)
    (define-key map "\C-k" 'dsvn-kill-region-or-line)
    (define-key map "\C-w" 'dsvn-kill-region)
    (define-key map "\C-xu" 'dsvn-undo)
    (define-key map "\C-xc" 'dsvn-clean)
    (condition-case ()
	;; This is for XEmacs, will error in Emacs.
	(define-key map '(control /) 'dsvn-undo)
      (error nil))
    (define-key map "\C-c\C-k" 'dsvn-kill-process)
    map)
  "Keymap for `dsvn-mode'")

(defvar dsvn-marked-files nil
  "Alist of marked files.  It is in the form `\(file . status)'.")
(make-variable-buffer-local 'dsvn-marked-files)

(defvar dsvn-commit-files nil
  "Alist of the files to be committed.
It has the same form as `dsvn-marked-files'.
For commit-mode buffers.")
(make-variable-buffer-local 'dsvn-commit-files)

(defvar dsvn-commit-parent-buffer nil
  "The examine/update mode buffer.
For commit-mode buffers.")
(make-variable-buffer-local 'dsvn-commit-parent-buffer)

(defvar dsvn-explain-func nil
  "Function to explain the line based on the state.")
(make-variable-buffer-local 'dsvn-explain-func)

(defvar dsvn-refresh-command nil
  "The command used to generate the buffer contents.")
(make-variable-buffer-local 'dsvn-refresh-command)

(defvar dsvn-submode nil
  "The submode for `dsvn-mode' for this buffer.")
(make-variable-buffer-local 'dsvn-submode)

(defvar dsvn-sticky-tag nil
  "The tag to update against.  This is remembered for diffs, etc.")
(make-variable-buffer-local 'dsvn-sticky-tag)


;; User functions.

(defun dsvn-examine-update-common (mode dir &optional dont-use-existing)
  ;; Common code for dsvn-examine and dsvn-update.
  (let* ((basename (file-name-nondirectory (directory-file-name dir)))
	 (bufname (format "*SVN-%s-%s*" mode basename))
	 (procname (format "svn-%s-%s" mode basename))
	 (buf (get-buffer bufname))
	 (cmd (if (eq mode 'examine)
		  (list dsvn-svn-command "-u" "status")
		(list dsvn-svn-command "update")))
	 proc)
    ;; Use an existing buffer if it is "visiting" the same dir.
    (if (and (not dont-use-existing)
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
	;; dsvn-mode makes the buffer read-only, so we have to take that
	;; into account here.
	(set-buffer buf)
	(unwind-protect
	    (progn
	      (setq buffer-read-only nil)
	      (buffer-disable-undo (current-buffer))
	      (erase-buffer)
	      (setq default-directory dir)
	      (insert "cd " dir "\n")
	      (insert (mapconcat 'identity cmd " "))
	      (insert "\n\n"))
	  (buffer-enable-undo (current-buffer))
	  (setq buffer-read-only t))

	;; Set up keybindings etc.
	(dsvn-mode mode)

	;; Make the buffer visible and start the process.
	(pop-to-buffer buf)
	(goto-char (point-min))
	(setq proc (apply 'start-process procname buf cmd))
	(set-process-filter proc (function dsvn-filter))
	(set-process-sentinel proc (function dsvn-sentinel))))))

(defun dsvn-examine-update-common-get-args (submode)
  (list (expand-file-name
	 (file-name-as-directory
	  (dsvn-read-directory-name (format "SVN %s directory: " submode)
				    lcvs-last-dir lcvs-last-dir t))
	 current-prefix-arg)))

(defun dsvn-examine (dir &optional dont-use-existing)
  "Call \"svn examine\" in DIR and then call `dsvn-mode' (which see).
Optional arg DONT-USE-EXISTING (interactive prefix arg) means to do the
examine even if there is an examine buffer hanging around for DIR."
  (interactive (dsvn-examine-update-common-get-args 'examine))
  (setq lcvs-last-dir dir)
  (dsvn-examine-update-common 'examine dir dont-use-existing))

(defun dsvn-update (dir &optional dont-use-existing)
  "Call \"svn -q update -dP\" in DIR and then call `dsvn-mode' (which see).
Optional arg DONT-USE-EXISTING (interactive prefix arg) means to do the
update even if there is an update buffer hanging around for DIR."
  (interactive)
  (error "dsvn-update not implemented"))
;;   (interactive (dsvn-examine-update-common-get-args 'update))
;;   (setq lcvs-last-dir dir)
;;   (dsvn-examine-update-common 'update dir dont-use-existing))

(defun dsvn-examine-or-update (dir &optional dont-use-existing)
  "LVCS examine or update based on the current value of `dsvn-submode'.
It doesn't make sense to call this outside of an DSVN buffer."
  (interactive (dsvn-examine-update-common-get-args dsvn-submode))
  (setq lcvs-last-dir dir)
  (dsvn-examine-update-common dsvn-submode dir dont-use-existing))

(defun dsvn-mode (submode)
  "Major mode for interacting with SVN.
The normal entry point is `dsvn-examine' or `dsvn-update'
although you can paste \"svn update\" output into a buffer and
then call this.

The hook `dsvn-mode-hook', if set, is run upon entry.

The following keys have meaning in an `dsvn-mode' buffer:
\\{dsvn-mode-map}
Some of the commands can work on marked files via a \\[universal-argument]
prefix; consult the individual documentation for each command
via `describe-key' on \\[describe-key]"
  ;; XXX/BRR completions would be nice, but are hassle since I have to
  ;; define a keymap
  (interactive "SSubmode (examine or update): ")
  (if (and (not (eq submode 'examine))
	   (not (eq submode 'update)))
      (error "Submode should be examine or update"))
  (kill-all-local-variables)
  (setq dsvn-marked-files nil)
  (cond ((eq submode 'examine)
	 (setq dsvn-explain-func 'dsvn-status-explain)
	 (setq dsvn-refresh-command 'dsvn-examine))
	(t
	 (setq dsvn-explain-func 'dsvn-update-explain)
	 (setq dsvn-refresh-command 'dsvn-update)))
  (setq dsvn-submode submode)
  (use-local-map dsvn-mode-map)
  (setq major-mode 'dsvn-mode
	mode-name "DSVN")
  (setq modeline-process '(":%s"))
  (setq buffer-read-only t)
  (make-variable-buffer-local 'font-lock-defaults)
  (setq font-lock-defaults '(dsvn-font-lock-keywords))
  (run-hooks 'dsvn-mode-hook))

(defun dsvn-clean ()
  "Remove lines for files that have been updated, patched, etc.
For update mode buffers."
  (interactive)
  (if (eq dsvn-submode 'update)
      (save-excursion
	(unwind-protect
	    (let ((regexps dsvn-dirty-regexps))
	      (setq buffer-read-only nil)
	      (while regexps
		(goto-char (point-min))
		(delete-matching-lines (car regexps))
		(setq regexps (cdr regexps))))
	  (setq buffer-read-only t)))))

(defun dsvn-sort ()
  "Sort the SVN output in this buffer.
This is useful to get all the M, U, etc files together and the new directory
messages at the bottom."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$")
    (beginning-of-line)
    (unwind-protect
	(let ((beg (point)))
	  (setq buffer-read-only nil)
	  (sort-lines nil beg (point-max)))
      (setq buffer-read-only t))))

(defun dsvn-explain-this-line ()
  "Explain what this line means.
Translates stuff like \"M foo/bar.c\" into something like \"this file has
been locally modified\"."
  (interactive)
  (let (char)
    (save-excursion
      (beginning-of-line)
      (if (looking-at dsvn-status-regexp)
	  (let ((state (match-string-no-properties 1)))
	    (message (funcall dsvn-explain-func state)))
	(message "I don't know what this line means")))))

(defun dsvn-next-line ()
  "Move cursor to the next file."
  (interactive)
  (if (re-search-forward dsvn-status-regexp nil t)
      (if dsvn-explain-each-line
	  (dsvn-explain-this-line))
    (error "No more files")))

(defun dsvn-prev-line ()
  "Move cursor to the previous file."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (if (re-search-backward dsvn-status-regexp nil t)
	(progn 
	  (goto-char (match-end 0))
	  (if dsvn-explain-each-line
	      (dsvn-explain-this-line)))
      (goto-char pt)
      (error "No more files"))))

(defun dsvn-kill-region-or-line (arg)
  "Kill the selected region or the current line (or ARG lines)."
  (interactive "p")
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(if (dsvn-region-active-p)
	    (kill-region (region-beginning) (region-end))
	  (beginning-of-line)
	  (kill-line arg)))
    (setq buffer-read-only t)))

(defun dsvn-kill-region (beg end)
  "Kill the selected region."
  (interactive "r")
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(kill-region beg end))
    (setq buffer-read-only t)))

(defun dsvn-undo (&optional arg)
  "Undo some changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "P")
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(undo arg))
    (setq buffer-read-only t)))

(defun dsvn-find-file ()
  "Find the file on this line."
  (interactive)
  (let ((file (dsvn-current-file)))
    (if (file-exists-p file)
	(find-file file)
      (error (format "%s not in working tree, probably a new file" file)))))

(defun dsvn-find-file-other-window ()
  "Find the file on this line."
  (interactive)
  (let ((file (dsvn-current-file)))
    (if (file-exists-p file)
	(find-file-other-window file)
      (error (format "%s not in working tree, probably a new file" file)))))

(defun dsvn-mark-file ()
  "Mark the file on this line for later processing."
  (interactive)
  (dsvn-bitch-if-commit-in-progress)
  (let ((file (dsvn-current-file))
	(state (dsvn-current-file-state)))
    (if (assoc file dsvn-marked-files)
	nil
      (save-excursion
	;; Since dsvn-current-file didn't fail, we know we're on a
	;; normal line.
	(beginning-of-line)
	(forward-char 19)
	(setq buffer-read-only nil)
	(subst-char-in-region (point) (1+ (point)) ?\ ?* 'no-undo)
	(setq buffer-read-only t)
	(setq dsvn-marked-files (cons (cons file state) dsvn-marked-files)))))
  ;; `dsvn-next-line' can error if at the end of files.
  (condition-case nil
      (dsvn-next-line)
    (error nil)))

(defun dsvn-unmark-file ()
  "Remove the file on this line from the list of to-be-processed files.
See also `dsvn-mark-file'."
  (interactive)
  (dsvn-bitch-if-commit-in-progress)
  (let ((file (dsvn-current-file)))
    (if (assoc file dsvn-marked-files)
	(save-excursion
	  ;; Since dsvn-current-file didn't fail, we know we're on a
	  ;; normal line.
	  (beginning-of-line)
	  (forward-char 19)
	  (setq buffer-read-only nil)
	  (subst-char-in-region (point) (1+ (point)) ?* ?\ 'no-undo)
	  (setq buffer-read-only t)
	  (setq dsvn-marked-files (dsvn-remassoc file dsvn-marked-files)))))
  ;; `dsvn-next-line' can error if at the end of files.
  (condition-case nil
      (dsvn-next-line)
    (error nil)))

(defun dsvn-update-internal (files)
  ;; Internal guts of update and revert.
  (let ((nconflicts 0)
	status)
    (message "Updating...")
    (setq status (dsvn-do-command-quietly "update" '("-q" "--non-interactive") (mapcar 'car files)))
    (message "Updating...done")
    (if (zerop status)
	;; Parse the update output and update the displayed state accordingly.
	(let ((updated-states (dsvn-parse-update-buffer "*SVN-update*"))
	      (cur files)
	      pair file
	      newpair)
	  (while cur
	    (setq pair (car cur))
	    (setq file (car pair))
	    (setq cur (cdr cur))
	    (setq newpair (assoc file updated-states))
	    (if newpair
		(let ((newstate (cdr newpair)))
		  (if (or (equal newstate ?U) (equal newstate ?P))
		      (dsvn-remove-file-line file)
		    (dsvn-change-file-state file newstate))
		  (dsvn-revert-buffers-visiting-file file)
		  ;; If there were any conflicts, we want them to know.
		  (if (equal newstate ?C)
		      (setq nconflicts (1+ nconflicts))))
	      ;; If there is no match for this file it means it is up to date,
	      ;; so we delete its status line.
	      ;; This typically happens with files that have been committed
	      ;; behind our back, like thru vc-mode.
	      (dsvn-remove-file-line file)))
	  ;; If they operated on the marked list, unmark everything.
	  (if (> (length files) 1)
	      (dsvn-unmark-all-files))))
    (if (not (and (zerop status)
		  (zerop nconflicts)))
	;; An error or conflict happened, bitch appropriately.
	(progn
	  (pop-to-buffer "*SVN-update*")
	  (goto-char (point-min))
	  (insert "\n"
		  "*** The update was not completely successful.\n"
		  (if (zerop nconflicts)
		      "*** Perhaps there were some conflicts?\n"
		    (format "*** There were %d conflicts.\n" nconflicts))
		  "*** Check this buffer closely to determine what is wrong.\n"
		  "*** It contains all the `svn -q update -dP' output.\n"
		  "\n")
	  (error "Update failed, see *SVN-update* buffer for details.")))))

(defun dsvn-update-some-files (arg)
  "Update some files.
By default updates the file on this line.
If supplied with a prefix argument, update the marked files.
If there are no marked files update those that need it according to
`dsvn-updatable-regexp'."
  (interactive "P")
  (if (not (eq dsvn-submode 'examine))
      (error "Updating in an update mode buffer is nonsensical."))
  (let ((files (or (dsvn-get-relevant-files arg 'noerror)
		   (dsvn-get-updatable-files)
		   (error "Nothing to update"))))
    (dsvn-update-internal files)))

(defun dsvn-revert (arg)
  "Revert some files, discarding local changes.
By default reverts the file on this line.
If supplied with a prefix argument, revert the marked files.
The files are removed and then updated.  By default this command requires
confirmation to remove the files.  To disable the confirmation, you can
set `dsvn-revert-confirm' to nil."
  (interactive "P")
  (let* ((files (dsvn-get-relevant-files arg))
	 (multiple (cdr files)))
    (if (and dsvn-revert-confirm
	     (not (yes-or-no-p (format "Discard changes to %s? "
				       (if multiple
					   "the marked files"
					 (car (car files)))))))
	(message "Revert cancelled")
      ;; Otherwise remove the files and do the update.
      (mapcar (function (lambda (e)
			  (delete-file (car e))))
	      files)
      (dsvn-update-internal files))))

(defun dsvn-remove (arg)
  "Remove some files, (obviously) discarding local changes.
By default reverts the file on this line.
If supplied with a prefix argument, revert the marked files.
By default this command requires confirmation to remove the files.
To disable the confirmation, you can set `dsvn-remove-confirm' to nil."
  (interactive "P")
  (let* ((files (dsvn-get-relevant-files arg))
	 (multiple (cdr files)))
    (if (and dsvn-remove-confirm
	     (not (yes-or-no-p (format "Remove %s? "
				       (if multiple
					   "the marked files"
					 (car (car files)))))))
	(message "Remove cancelled")
      ;; Otherwise remove the files
      (mapcar (function (lambda (e)
			  (let ((file (car e)))
			    (shell-command (format "rm -rf %s" file))
			    (dsvn-remove-file-line file)
			    )))
	      files))))

(defun dsvn-commit (arg)
  "Commit files.
If given a prefix argument, commit the marked files.  Otherwise commit
the file on this line."
  (interactive "P")
  (dsvn-bitch-if-commit-in-progress)
  (let ((files (dsvn-get-relevant-files arg))
	(this-buffer (current-buffer))
	cur state)
    ;; Don't let them do something dumb.
    ;; This should match what we display in the "SVN:..." lines.
    (setq cur files)
    (while cur
      (setq state (cdr (car cur)))
      (setq cur (cdr cur))
      (if (or (equal (dsvn-local-state state) ?M)
	      (equal (dsvn-local-state state) ?A)
	      (equal (dsvn-local-state state) ?D)
	      (equal (dsvn-property-state state) ?M))
	  nil
	(error "Can only commit locally modified files")))
    ;; Checks ok, give them the edit buffer.
    (pop-to-buffer (get-buffer-create "*SVN-commit-message*"))
    (dsvn-commit-mode this-buffer files)))

(defun dsvn-ignore (arg)
  "Add the given files to the svn:ignore property for their appropriate directory."
  (interactive "P")
  (dsvn-bitch-if-commit-in-progress)
  (let ((files (dsvn-get-relevant-files arg))
	(this-buffer (current-buffer))
	cur state)
    (setq cur files)
    (while cur
      (let ((dir  (file-name-directory (car (car cur))))
	    (file (file-name-nondirectory (car (car cur)))))
      (setq cur (cdr cur))

      (if (eq dir nil)
	  (setq dir "."))

      ;; XXX/demmer yuck -- too much hassle
      ))))


(defun dsvn-resolve-conflict (arg)
  "Call 'svn resolve' on some files. Use this once you've resolved the
conflict and want to remove the conflicted bit which will let you check it in.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (dsvn-do-command "resolved"
		   "No conflict to resolve"
		   nil
		   (mapcar 'car (dsvn-get-relevant-files arg)))
  (message "Diffing...done"))

(defun dsvn-diff-base (arg)
  "Diff some files against the BASE revision.
Use this when you have locally modified files and want to see what
you have done.  See also `dsvn-diff-head'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Diffing...")
  (dsvn-do-command "diff"
		   "No differences with the BASE"
		   nil
		   (cons "-N" (mapcar 'car (dsvn-get-relevant-files arg))))
  (message "Diffing...done"))

(defun dsvn-diff-head (arg)
  "Diff some files against the HEAD revision.
Use this when files have been checked in by someone else and you want
to see what has changed before you update your copies.  See also
`dsvn-diff-base'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (if (not (eq dsvn-submode 'examine))
      (error "Diffing BASE against HEAD is nonsensical in update mode"))
  (message "Diffing...")
  (dsvn-do-command "diff"
		   "No differences with the HEAD"
		   nil
		   (cons"-N" (cons (format "-rBASE:%s" (dsvn-head-arg))
			(mapcar 'car (dsvn-get-relevant-files arg)))))
  (message "Diffing...done"))

;; There isn't an easy way to do diff-base and diff-head.
;;
;; This doesn't drop you back to the examine/update buffer and is hard
;; to do (would have to set the ediff hook (maybe the cleanup hook) to
;; some function to put us back in the examine buffer then remove the
;; function from the hook.  this would prevent multiple ediffs running
;; from dsvn since the hook is global)
(defun dsvn-ediff (arg)
  "Ediff a file with the HEAD revision.
This will compare the contents of the current-file with the tip of
the current branch."
  (interactive "P")
  (require 'ediff)
  (require 'ediff-vers)
  (find-file (dsvn-current-file))
  (ediff-vc-internal "" "" nil))

(defun dsvn-show-log (arg)
  "Show log info for some files. If given a prefix argument, use the
marked files. Otherwise use the file on this line. Influenced by the
`dsvn-log-from-repository' and `dsvn-log-restrict-to-changes'
variables."
  (interactive "P")
  (let ((files (mapcar 'car (or (dsvn-get-relevant-files arg 'noerror)
				(dsvn-get-updatable-files))))
	args working-revisions)

    ;; If logging just one file or need changes for all, figure out
    ;; working-revision list from SVN/Entries.
    ;; XXX/demmer fixme
;;     (if (or dsvn-log-restrict-to-changes (= (length files) 1))
;; 	(setq working-revisions
;; 	      (mapcar (function
;; 		       (lambda (filename)
;; 			 (let* ((file (file-name-nondirectory filename))
;; 				(dir (file-name-directory filename))
;; 				(entries (concat dir (file-name-as-directory "SVN") "Entries"))
;; 				(working-revision nil))
;; 			   (if (and (file-exists-p entries)
;; 				    (file-readable-p entries))
;; 			       (let ((buf (get-buffer-create "*Entries*")))
;; 				 (save-excursion
;; 				   (set-buffer buf)
;; 				   (erase-buffer)
;; 				   (insert-file-contents entries t nil nil t)
;; 				   (goto-char (point-min))
;; 				   (if (re-search-forward
;; 					;; /foo.c/1.3/blah/blah
;; 					(concat "^/" (regexp-quote file) "/\\([^/]+\\)/") nil t)
;; 				       (setq working-revision (match-string-no-properties 1)))
;; 				   (kill-buffer buf)
;; 				   working-revision))))))
;; 		      files)))

    ;; If logging from repository, transform the file list to prepend
    ;; the repository
    (if dsvn-log-from-repository
	(setq files (mapcar 'dsvn-file-url files))
	)
    (message "log files:")
    (mapcar (function (lambda (f) (message (format "log file %s" f)))) files)
        
    ;; do the actual log command
    (message "Logging...")
    (dsvn-do-command "log" "No output" nil (nconc args files))
    
    ;; If we know the working revision and we're just marking one
    ;; file, go mark it and make it visible. Parts stolen from vc.el.
    (if (and (= (length files) 1) (car working-revisions))
	(let (start end lines win windows (working-revision (car working-revisions)))
	  (save-excursion
	    (set-buffer "*SVN-log*")
	    (goto-char (point-min))
	    (if (not (re-search-forward
		      (concat "----\nrevision " working-revision "\ndate: ")
		      nil t))
		nil
	      ;; Mark this entry so user can determine the working rev.
	      (beginning-of-line)
	      (forward-line -1)
	      (setq start (point))
	      (let ((buffer-read-only nil))
		(insert "\n")
		(end-of-line)
		(insert " ~~~~~~~~~~ YOUR REVISION ~~~~~~~~~~")
		(set-buffer-modified-p nil))
	      ;; Figure out the extent of this log entry.
	      (if (not (re-search-forward "^----*\nrevision" nil t))
		  (setq end (point-max))
		(beginning-of-line)
		(forward-line -1)
		(setq end (point)))
	      (setq lines (count-lines start end))
	      ;; Make the log entry visible in its windows.
	      (setq windows (get-buffer-window-list (current-buffer)))
	      (save-selected-window
		(while windows
		  (setq win (car windows)
			windows (cdr windows))
		  (select-window win)
		  (cond
		   ;; If the global information and this log entry fit
		   ;; into the window, display from the beginning
		   ((< (count-lines (point-min) end) (window-height))
		    (goto-char (point-min))
		    (recenter 0))
		   ;; If the whole entry fits into the window,
		   ;; display it at the bottom of the window
		   ((< (1+ lines) (window-height))
		    (goto-char end)
		    (previous-line (- (window-height) 2))
		    (recenter 0))
		   ;; Otherwise (the entry is too large for the window),
		   ;; display from the start
		   (t
		    (goto-char start)
		    (recenter 0)))))))))

    (if (not dsvn-log-from-repository)
 	;; Do the substitute-command-keys before going to the other buffer.
	(let ((msg (substitute-command-keys
		    (concat
		     "\n"
		     "NOTE: Logging is restricted to the checked out copy.\n"
		     "      To see the full log, use the \\[dsvn-show-full-log]"
		     " command.\n"))))
	  (save-excursion
	    (set-buffer "*SVN-log*")
	    (goto-char (point-max))
	    (insert msg))))
    
    (if dsvn-log-restrict-to-changes
	;; Do the substitute-command-keys before going to the other buffer.
	(save-excursion
	  (set-buffer "*SVN-log*")
	  (goto-char (point-max))
	  
	  (let ((msg (substitute-command-keys
		      (concat
		       "\n"
		       "NOTE: Logging is restricted to changes in files.\n"
		       "      To see the full log, use the \\[dsvn-show-full-log]"
		       " command.\n"))))
	    (insert msg)
	    )))
  (message "Logging...done")))

(defun dsvn-show-full-log (arg)
  "Like \\[dsvn-show-log] but sets `dsvn-log-from-repository'."
  (interactive "P")
  (let ((dsvn-log-from-repository t))
    (dsvn-show-log arg)))

(defun dsvn-show-changed-log (arg)
  "Like \\[dsvn-show-log] but sets `dsvn-log-restrict-to-changes'."
  (interactive "P")
  (let ((dsvn-log-restrict-to-changes t))
    (dsvn-show-log arg)))

(defun dsvn-show-status (arg)
  "Show SVN status info for some files.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Getting status...")
  (dsvn-do-command "info" "No output" nil
		   (mapcar 'car (dsvn-get-relevant-files arg)))
  (message "Getting status...done"))

;;; I don't know why would someone would use marks for this one, but whatever.
(defun dsvn-annotate (arg)
  "Show SVN status info for some files.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Annotating...")
  (dsvn-do-command "annotate" "No output" nil
		   (mapcar 'car (dsvn-get-relevant-files arg)))
  (message "Annotating...done"))

(defun dsvn-add (arg)
  "Add files to the list of member files.
If given a prefix argument, add the marked files.  Otherwise add
the file on this line.
The files won't be actually added to the repository until they are
formally committed."
  (interactive "P")
  (let ((files (dsvn-get-relevant-files arg))
	status)
    (message "Adding...")
    (setq status (dsvn-do-command-quietly "add" nil (mapcar 'car files)))
    (message "Adding...done")
    (if (zerop status)
	;; Update the diplayed state of the files to "A"
	(let ((cur files)
	      pair)
	  (while cur
	    (setq pair (car cur))
	    (setq cur (cdr cur))
	    (dsvn-change-file-state (car pair) ?A)))
      ;; Otherwise an error happened, bitch appropriately
      (pop-to-buffer "*SVN-add*")
      (goto-char (point-min))
      (insert "\n"
	      "*** The add was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "\n")
      (error "Add failed, see *SVN-add* buffer for details."))))

(defun dsvn-refresh-buffer ()
  "Re-examine or update this dir."
  (interactive)
  (funcall dsvn-refresh-command default-directory t))

(defun dsvn-quit-just-bury ()
  "\"Quit\" dsvn-mode by burying the buffer."
  (interactive)
  (bury-buffer))

(defun dsvn-kill-process ()
  "Kill the SVN process, if there is one.
We assume the current buffer is the one that is supposed to be running
a SVN process."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (interrupt-process (get-buffer-process (current-buffer)))
    (error "No SVN process running")))

(defun dsvn-debug ()
  (interactive)
  (setq dsvn-debug (not dsvn-debug))
  (message "dsvn-debug set to %s" dsvn-debug))


;; The committing major mode

(defvar dsvn-commit-msgs (make-ring 10))
(defvar dsvn-commit-msgs-index nil)
(make-variable-buffer-local 'dsvn-commit-msgs-index)

(defvar dsvn-commit-initial-buffer-contents "")
(make-variable-buffer-local 'dsvn-commit-initial-buffer-contents)

(defvar dsvn-commit-mode-map
  (let ((map (make-sparse-keymap 'dsvn-commit-mode-map)))
    (define-key map "\C-c\C-c" 'dsvn-commit-finish)
    (define-key map "\M-p" 'dsvn-commit-insert-prev-commit-msg)
    (define-key map "\M-n" 'dsvn-commit-insert-next-commit-msg)
    map)
  "Keymap for `dsvn-commit-mode'")

(defun dsvn-commit-mode (parent files)
  "Major mode for providing a commit log message and committing files.
This mode is not meant to be user invoked."
  (interactive)

  (setq dsvn-commit-parent-buffer parent)
  (setq dsvn-commit-files (sort files (lambda (a b)
					(string-lessp (car a) (car b)))))

  (use-local-map dsvn-commit-mode-map)
  (setq major-mode 'dsvn-commit-mode)
  (setq mode-name "SVN-Commit")
  (setq dsvn-commit-msgs-index nil)

  ;; Insert "SVN: ..." stuff to show them what files they're affecting.
  (insert "SVN: ")(insert-char ?- 70)(insert "\n")
  (insert "SVN: Enter Log.  Lines beginning with `SVN:' are removed"
	  " automatically\n")
  (insert (substitute-command-keys
	   "SVN: Type \\[dsvn-commit-finish] when done.\n"))
  (insert "SVN:\n")
  (insert "SVN: Committing in " default-directory "\n")
  (insert "SVN:\n")
  ;; These should match what is checked for in the file list before
  ;; calling this.
  (dsvn-commit-insert-matching-files ?M "Modified")
  (dsvn-commit-insert-matching-files ?A "Added")
  (dsvn-commit-insert-matching-files ?R "Removed")
  (insert "SVN: ")(insert-char ?- 70)(insert "\n")
  (dsvn-maybe-insert-template)
  (setq dsvn-commit-initial-buffer-contents (buffer-string))
  (set-buffer-modified-p nil)

  (message (substitute-command-keys "Type \\[dsvn-commit-finish] when done."))
  (run-hooks 'text-mode-hook))

(defun dsvn-maybe-insert-template ()
  ;; Look for a template to insert.
  ;; This is for the SVNROOT/rcsinfo feature of svn.

  ;; First look in SVN/Template.  This will exist if rcsinfo
  ;; specifies a template but the repository is remote.
  (let ((svn-template-filename (concat (file-name-as-directory "SVN")
				       "Template")))
    (if (file-readable-p svn-template-filename)
	(insert-file-contents svn-template-filename)
      ;; Otherwise check if the repository is local and use SVNROOT/rcsinfo
      ;; to find the template.
      (let* ((rcsinfo (dsvn-load-rcsinfo))
	     (templates (dsvn-apply-rcsinfo
			 rcsinfo
			 (dsvn-get-directories (mapcar 'car
						       dsvn-commit-files)))))
	(if templates
	    (progn
	      (if (not (dsvn-all-same-elements templates))
		  (insert "SVN: WARNING: files matched different "
			  "templates in SVNROOT/rcsinfo, using first"))
	      (insert-file-contents (car templates))))))))

(defun dsvn-load-rcsinfo ()
  ;; Load the SVNROOT/rcsinfo file into an alist ((pattern . template) ...)
  ;; Return the alist
  ;; Return nil if we can't find it, can't parse it, etc.
  (let (result local-svnroot)
    ;; Look for a local SVN/Root.  It must start with a slash or :local:
    (let ((svnroot-contents (dsvn-get-special-file-contents-as-one-line
			     "Root")))
      (if svnroot-contents
	  (if (string-match "^:local:\\(.*\\)" svnroot-contents)
	      (setq local-svnroot (match-string-no-properties 1 svnroot-contents))
	    (if (file-name-absolute-p svnroot-contents)
		(setq local-svnroot svnroot-contents)))))
    (if local-svnroot
	;; Now look for the rcsinfo file.
	;; This is normally in $SVNROOT/SVNROOT/rcsinfo
	;; see svn/src/parseinfo.c for the format of these files
	(let ((rcsinfo-filename (concat (file-name-as-directory local-svnroot)
					(file-name-as-directory "SVNROOT")
					"rcsinfo")))
	  (if (file-readable-p rcsinfo-filename)
	      (let ((tempbuf (get-buffer-create " *SVN-rcsinfo*"))
		    done)
		(save-excursion
		  (set-buffer tempbuf)
		  (erase-buffer)
		  (insert-file-contents rcsinfo-filename)
		  (goto-char (point-min))
		  (while (not (eobp))
		    (if (and (not (looking-at "^#"))
			     (looking-at "\\s-*\\(\\S-+\\)\\s-+\\(.*\\)"))
			(let ((exp (dsvn-emacsify-regexp (match-string-no-properties 1)))
			      (value (dsvn-expand local-svnroot (match-string-no-properties 2))))
			  ;; Append since order matters.
			  (setq result (append result (list (cons exp value))))))
		    (forward-line)))
		(kill-buffer tempbuf)))))
    result))

(defun dsvn-expand (svnroot str)
  ;; Expand env vars and ~user things in STR.
  ;; Use param SVNROOT for $SVNROOT
  (let ((current-root (getenv "SVNROOT"))
	result)
    (unwind-protect
	(progn
	  (setenv "SVNROOT" svnroot)
	  (setq result (expand-file-name (substitute-in-file-name str))))
      (setenv "SVNROOT" current-root))
    result))

(defun dsvn-get-directories (files)
  ;; For each file, get the directory it is in relative to $SVNROOT.
  ;; Algorithm:
  ;;   We want to prepend something from the SVN/Repository to each file.
  ;;   If the SVN/Repository is relative, that is our prefix.
  ;;   Else, get the SVN/Root, remove the :local: prefix if present and
  ;;   remove this from the SVN/Repository, that is our prefix.

  (let ((repos (dsvn-get-special-file-contents-as-one-line "Repository"))
	prefix)
    (if repos
	(progn
	  ;; Absolute path, we need to modify it according to SVN/Root
	  (if (file-name-absolute-p repos)
	      (let ((root (dsvn-get-special-file-contents-as-one-line "Root")))
		(if root
		    (progn
		      (if (string-match "^:local:\\(.*\\)" root)
			  (setq root (match-string-no-properties 1 root)))
		      (replace-in-string repos
					 (concat "^" (regexp-quote root))
					 "")))))
	  (setq prefix (file-name-as-directory repos))))
    (if prefix
	(mapcar (lambda (f)
		  (concat prefix f))
		files)
      ;; No prefix probably means no SVN/Repository, they're fucked but
      ;; don't do anything.
      files)))

(defun dsvn-apply-rcsinfo (rcsinfo files)
  ;; Apply the rules in RCSINFO to FILES, returning any matches.
  ;; RCSINFO looks like ((exp . value) (exp . value)...)
  ;; Return the list of VALUEs whose EXPs match a file.
  ;; Two special EXPS:
  ;;	ALL - always matches, used in addition to any matches
  ;;	DEFAULT - used if no matches

  (let (matches)
    ;; First, get the matches.
    (while files
      (let ((file (car files))
	    (info rcsinfo))
	(while info
	  (let* ((elem (car info))
		 (exp (car elem))
		 (value (cdr elem)))
	    (if (string-match exp file)
		(setq matches (cons value matches))))
	  (setq info (cdr info))))
      (setq files (cdr files)))
    ;; If no matches, add the DEFAULT value.
    (if (not matches)
	(let ((default (assoc "DEFAULT" rcsinfo)))
	  (if default
	      (setq matches (list (cdr default))))))
    ;; If there is an ALL rule, add it in.
    (let ((all (assoc "ALL" rcsinfo)))
      (if all
	  (setq matches (cons (cdr all) matches))))
    matches))

(defun dsvn-commit-insert-matching-files (char desc)
  (if (rassoc char dsvn-commit-files)
      (let ((prefix "SVN:    "))
	(insert "SVN: " desc " Files:\n")
	;; This is a lame place to put this, but that is close to what svn does.
	;; We don't look in the SVN/Entries file but assume the SVN/Tag file
	;; is the same.
	(let ((tag (dsvn-sticky-tag (car (car dsvn-commit-files)))))
	  (if tag
	      (insert "SVN:  Tag: " tag "\n")))
	(insert prefix)
	(let ((cur dsvn-commit-files)
	      (curcol (current-column))
	      pair)
	  (while cur
	    (setq pair (car cur))
	    (setq cur (cdr cur))
	    (let ((file (car pair))
		  (state (cdr pair)))
	      (if (equal state char)
		  (progn
		    (if (> (+ curcol (length file) 1)
			   fill-column)
			(progn
			  (insert "\n" prefix)
			  (setq curcol (length prefix))))
		    (insert file " ")
		    (setq curcol (+ curcol (length file) 1))))))
	  (insert "\n")))))

(defun dsvn-commit-finish ()
  ;; Finish up the commit by grabbing the commit string and calling svn commit.
  ;; If the commit worked, clear out the affected files from the parent buffer.
  ;; Otherwise complain loudly and pop up the commit output.
  ;;
  ;; This is tricky since several buffers are involved, each with their own
  ;; local variables and such.  Watch out.
  (interactive)
  (let ((logbuf (get-buffer "*SVN-commit-message*"))
	(commit-bufname "*SVN-commit*")
	status)
    (goto-char (point-min))
    (delete-matching-lines "^SVN:")
    (let ((msg (buffer-string))
	  (files dsvn-commit-files)
	  (parent dsvn-commit-parent-buffer)
	  justfiles)
      (setq justfiles (mapcar 'car files))
      ;; Make sure any buffers visiting those files aren't dirty.
      (dsvn-ensure-saved justfiles)
      ;; Do the commit.  We make sure to do it in the parent buffer so
      ;; CWD, etc is correct.
      ;; We do it this way, all at once, rather than commit for each
      ;; file, mainly so the commitlog remains concise and usable.
      (pop-to-buffer parent)
      (message "Committing...")
      (ring-insert dsvn-commit-msgs msg)
      (setq status (dsvn-do-command-quietly "commit" nil
					    (nconc (list "-m" msg)
						   justfiles)))
      (message "Committing...done")
      ;; Remove lines in parent buffer for files we successfully committed.
      ;; Complain loudly if the commit failed.
      (if (zerop status)
	  (let ((cur files)
		pair)
	    (while cur
	      (setq pair (car cur))
	      (setq cur (cdr cur))
	      (dsvn-revert-buffers-visiting-file (car pair))
	      (dsvn-remove-file-line (car pair)))
	    ;; Only chuck buffer when all is good.
	    (kill-buffer logbuf)
	    ;; If they operated on the marked list, unmark everything.
	    (if (> (length files) 1)
		(dsvn-unmark-all-files)))
	(pop-to-buffer commit-bufname)
	(goto-char (point-min))
	(insert "\n"
		"*** The commit was not completely successful.\n"
		"*** Check this buffer closely to determine what is wrong.\n"
		"*** The commit message is in " (buffer-name logbuf) ".\n"
		"\n")
	(error "Commit failed, see %s buffer for details." commit-bufname)))))

(defun dsvn-commit-insert-prev-commit-msg (arg)
  "Cycle backwards thru commit message history."
  (interactive "*p")
  (let ((len (ring-length dsvn-commit-msgs)))
    (if (= len 0)
	(error "Empty commit message string")
      (erase-buffer)
      (insert dsvn-commit-initial-buffer-contents)
      ;; Initialize the index on the first use of this command
      ;; so that the first M-p gets index 0, and the first M-n gets
      ;; index -1.
      (if (null dsvn-commit-msgs-index)
	  (setq dsvn-commit-msgs-index
		(if (> arg 0) -1
		  (if (< arg 0) 1 0))))
      (setq dsvn-commit-msgs-index
	    (mod (+ dsvn-commit-msgs-index arg) len))
      (message "Commit Msg %d" (1+ dsvn-commit-msgs-index))
      (insert (ring-ref dsvn-commit-msgs dsvn-commit-msgs-index)))))

(defun dsvn-commit-insert-next-commit-msg (arg)
  "Cycle forwards thru commit message history."
  (interactive "*p")
  (dsvn-commit-insert-prev-commit-msg (- arg)))


;; Internal functions.

(defun dsvn-get-special-file-contents (filename)
  ;; Get the contents of .svn/<filename>
  (let ((filename (concat (file-name-as-directory ".svn") filename))
	result)
    (if (file-readable-p filename)
	(let ((tempbuf (get-buffer-create (concat " *" filename "*"))))
	  (save-excursion
	    (set-buffer tempbuf)
	    (erase-buffer)
	    (insert-file-contents filename)
	    (goto-char (point-min))
	    (setq result (buffer-substring (point-min) (point-max))))
	  (kill-buffer tempbuf)))
    result))

(defun dsvn-get-special-file-contents-as-one-line (filename)
  ;; Get the contents of SVN/<filename> as a single line.
  ;; This basically gets you the first line.
  (let ((contents (dsvn-get-special-file-contents filename)))
    (if contents
	(progn
	  (string-match ".*" contents)
	  (match-string-no-properties 0 contents)))))

(defun dsvn-emacsify-regexp (exp)
  ;; Convert a SVN-style regular expression into Emacs-style.
  ;; SVN-style is egrep-style
  ;; We basically just fix the parens and bars to be quoted,
  ;; we don't handle the fancy stuff.
  (setq exp (replace-in-string exp "\\(^\\|[^\\]\\)(" "\\1\\\\("))
  (setq exp (replace-in-string exp "\\([^\\]\\))" "\\1\\\\)"))
  (setq exp (replace-in-string exp "\\([^\\]\\)|" "\\1\\\\|"))
  exp)

(defun dsvn-all-same-elements (list)
  ;; Return non-nil if all elements in LIST are the same
  (let ((first (car list))
	(result t))
    (setq list (cdr list))
    (while list
      (if (not (equal (car list) first))
	  (setq result nil
		list nil)
	(setq list (cdr list))))
    result))

(defun dsvn-head-arg ()
  ;; Return a string referring to the HEAD, this might be a tag.
  (or dsvn-sticky-tag "HEAD"))

(defun dsvn-set-view-mode (win buf)
  ;; Turn view-mode on for BUF in window WIN, making sure quitting it
  ;; will get us back somewhere sane.
  (if (not dsvn-use-view-mode)
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

(defun dsvn-sticky-tag (file)
  ;; Return the sticky tag for FILE, or nil.
  ;; Just looks in the SVN/Tag file, not in Entries.
  (let* ((dir (file-name-directory file))
	 (tagfile (concat dir (file-name-as-directory "SVN") "Tag"))
	 tag)
    (if (and (file-exists-p tagfile)
	     (file-readable-p tagfile))
	(let ((buf (get-buffer-create "*Tag*")))
	  (save-excursion
	    (set-buffer buf)
	    (erase-buffer)
	    (insert-file-contents tagfile t nil nil t)
	    (goto-char (point-min))
	    (if (looking-at "^T\\(.*\\)")
		(setq tag (match-string-no-properties 1)))
	    (kill-buffer buf))))
    tag))

(defun dsvn-file-url (file)
  "Return the svn url for the given file by extracting it from
.svn/entries in the same directory as the given file."
  (let* ((dir (file-name-directory file))
	 (entries (concat dir (file-name-as-directory ".svn") "entries"))
	 tail url)

    (if (and (file-exists-p entries)
	     (file-readable-p entries))
	(setq tail "")
      
      ;; try the current directory, to which we'll tack on the
      ;; file's directory as the tail
      (setq entries (concat (file-name-as-directory ".svn") "entries"))
      
      (if (and (file-exists-p entries)
	       (file-readable-p entries))
	  (setq tail (file-name-as-directory (file-name-directory file)))
	(error "Can't open entries file %s" entries)))

    (let ((buf (get-buffer-create "*SVN-entries*")))
      (save-excursion
	(set-buffer buf)
	(erase-buffer)
	(insert-file-contents entries t nil nil t)
	(goto-char (point-min))
	(re-search-forward "url=\"\\([^\"]+\\)\"")
	(setq url (match-string 1))
	(kill-buffer buf)))
  (concat (file-name-as-directory url) tail (file-name-nondirectory file))
  ))

(defun dsvn-ensure-saved (files)
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

(defun dsvn-revert-buffers-visiting-file (file)
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

(defun dsvn-get-relevant-files (use-marks &optional noerror)
  ;; Return a list of files in the form of `dsvn-marked-files'
  ;; If USE-MARKS is non-nil then use the marked file list,
  ;; otherwise use the current file.
  ;; Optionaly NOERROR means return nil instead of throwing an error
  ;; when no files are marked.
  (if use-marks
      (if dsvn-marked-files
	  ;; sort modifies
	  (sort (copy-sequence dsvn-marked-files)
		(function (lambda (a b)
			    (string-lessp (car a) (car b)))))
	(if noerror
	    nil
	  (error "No marked files")))
    (list (cons (dsvn-current-file)
		(dsvn-current-file-state)))))

(defun dsvn-unmark-all-files ()
  ;; Clear the marked-files list and update the displayed state accordingly.
  (setq dsvn-marked-files nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward dsvn-marked-file-regexp nil t)
      (beginning-of-line)
      (forward-char 1)
      (setq buffer-read-only nil)
      (subst-char-in-region (point) (1+ (point)) ?* ?\ 'no-undo)
      (setq buffer-read-only t))))

(defun dsvn-bitch-if-commit-in-progress ()
  ;; If a commit is in progress, go to the commit-message buffer and
  ;; tell them what to do.
  (let ((buf (get-buffer "*SVN-commit-message*")))
    (if buf
	(progn
	  (pop-to-buffer buf)
	  (error "Please finish or abort this commit first")))))

(defun dsvn-get-updatable-files ()
  ;; Return an alist of the files for which a svn update would make sense.
  ;; The alist is like dsvn-marked-files with (file . state) pairs.
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward dsvn-updatable-regexp nil t)
	(let ((file (dsvn-current-file))
	      (state (dsvn-current-file-state)))
	  (setq result (cons (cons file state) result)))))
    result))

(defun dsvn-parse-update-buffer (buf)
  ;; Return an alist describing the update output in a buffer.
  ;; The buffer is expected to look like "svn examine" output.
  ;; The alist is like dsvn-marked-files with (file . state) pairs.
  (let (result)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward dsvn-status-regexp nil t)
	(let ((file (dsvn-current-file))
	      (state (dsvn-current-file-state)))
	  (setq result (cons (cons file state) result)))))
    result))

;; Too bad Emacs doesn't have this XEmacs feature.
(defun dsvn-region-active-p ()
  (if (fboundp 'region-active-p)
      (region-active-p)
    (and transient-mark-mode
	 (condition-case ()
	     (mark)
	   (error nil)))))

;; Too bad Emacs doesn't have this XEmacs feature.
(defun dsvn-remassoc (key list)
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

(defun dsvn-change-this-file-state (newstate)
  ;; Change the displayed state of the file on this line.
  ;; If the file in in the marked list, update that too.
  (setq buffer-read-only nil)
  (let ((file (dsvn-current-file)))
    (beginning-of-line)
    (if (not (looking-at dsvn-status-regexp))
	(error "No file on this line"))

    (replace-match (format "%c       \\2" newstate))
    
    (if (assoc file dsvn-marked-files)
	(setq dsvn-marked-files
	      (cons (cons file newstate)
		    (dsvn-remassoc file dsvn-marked-files)))))
  (setq buffer-read-only t))

(defun dsvn-change-file-state (file newstate)
  ;; Change the displayed state of a file.
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat dsvn-status-regexp (regexp-quote file) "$") nil t)
	(dsvn-change-this-file-state newstate))))

(defun dsvn-remove-file-line (file)
  ;; Delete lines in the buffer that match a filename, allowing for a
  ;; parenthesized comment on the file line.
  ;; Removes from marked files too.
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (delete-matching-lines (concat dsvn-status-regexp
				   (regexp-quote file)
				   "\\( (.*)\\)?"
				   "$"))
    (if (assoc file dsvn-marked-files)
	(setq dsvn-marked-files (dsvn-remassoc file dsvn-marked-files)))
    (setq buffer-read-only t)))

(defun dsvn-read-directory-name (prompt
				 &optional dir default must-match
				 initial-contents)
  ;; Emacs doesn't have this handy XEmacsism
  (if (fboundp 'read-directory-name)
      (read-directory-name prompt dir default must-match initial-contents)
    (let ((dir (read-file-name prompt dir default must-match initial-contents)))
      (cond ((file-directory-p dir)
	     dir)
	    ((string-equal (expand-file-name dir) buffer-file-name)
	     ;; Undo that lame "default to current buffer" crap.
	     (file-name-directory dir))
	    (t
	     (error "%s is not a directory" dir))))))

(defun dsvn-buffer-size (&optional buffer)
  ;; `buffer-size' in Emacs doesn't take an arg like XEmacs.
  (condition-case nil
      (buffer-size buffer)
    (error (save-excursion
	     (if buffer
		 (set-buffer buffer))
	     (buffer-size)))))

(defun dsvn-do-command (cmd default-output svnopts &optional cmdopts)
  ;; Do the svn command `cmd' and print the result in buffer *SVN-`cmd'*.
  ;; If there is no output, insert some default text.
  ;; Returns the command exit status.
  (message (format "dsvn command: %s" cmd))
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
	       (if (and dsvn-use-diff-mode
			(string-equal cmd "diff")
			(fboundp 'diff-mode))
		   (diff-mode))
	       (setq default-directory cwd)
	       (if (zerop (dsvn-buffer-size buf))
		   (insert default-output))
	       (if dsvn-debug
		   (progn
		     (goto-char (point-min))
		     (insert (concat "DEBUG: "
				     dsvn-svn-command " "
				     (mapconcat 'identity args " ")
				     "\n")))))
	     (let ((win (display-buffer buf)))
	       (if (member cmd dsvn-view-mode-commands)
		   (dsvn-set-view-mode win buf))))))
      (with-output-to-temp-buffer bufname
	(setq status (apply 'call-process dsvn-svn-command
			    nil standard-output
			    nil args))))
    status))

(defun dsvn-do-command-quietly (cmd svnopts &optional cmdopts noerase)
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
      (if (not noerase)
	  (erase-buffer)))
    (setq status (apply 'call-process dsvn-svn-command
			nil buf nil
			args))
    status))

(defun dsvn-print-current-file ()
  (interactive)
  (message (dsvn-current-file)))


(defun dsvn-current-file ()
  ;; Assuming the current line is updatable (by dsvn-status-regexp), return
  ;; the file name, minus any parenthesized comments that dsvn put in.
  ;; e.g. "U elisp (new directory)" => "elisp"
  (save-excursion
    (beginning-of-line)
    (if (looking-at dsvn-status-regexp)
	(let ((beg (match-end 0)) (end (line-end-position))) 	;; take the whole line
	  (buffer-substring-no-properties beg end))
      (error "No file on this line"))))
    
(defun dsvn-current-file-state ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at dsvn-status-regexp)
	(match-string-no-properties 1)
      (error "No file on this line"))))

(defun dsvn-redraw-modeline (&optional all)
  ;; Emacs doesn't have this XEmacsism.
  (if (fboundp 'redraw-modeline)
      (redraw-modeline all)
    nil))

(defun dsvn-filter (proc string)
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
	(if (and (eq dsvn-submode 'examine)
		 dsvn-translate-update-output)
	    (dsvn-fix-lines beg)))
      (if moving (goto-char (process-mark proc))))))

(defvar dsvn-conflict nil)
(make-variable-buffer-local 'dsvn-conflict)

(defun dsvn-kill-entire-line ()
  "Kill the entire line containing point."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun dsvn-fix-lines (beg)
  ;; Fix up stuff that we insert, rewriting some things.
  (goto-char beg)
  (beginning-of-line)			; in case last insert had partial line
  (let ((stuff-to-do t)
	prev-point dont-move)
    (while stuff-to-do
      (setq dont-move nil)

      (cond
       ;; Rewrite removed files as "U file"
       ((looking-at
	 (concat "^svn\\(pserver\\)? \\(update\\|server\\): `?\\([^']*\\)'?"
		 " is no longer \\(pertinent\\|\\(in the repository\\)\\).*\n"))
	(replace-match "U \\3 (removed)\n")
	(setq dont-move t))

       ((looking-at
	 (concat "^svn\\(pserver\\)? \\(update\\|server\\): warning: `?\\([^']*\\)'?"
		 " is not (any longer) pertinent\n"))
	(replace-match "U \\3 (removed)\n")
	(setq dont-move t))

       ;; Rewrite new directories as "U" with a note
       ((looking-at
	 (concat "^svn\\(pserver\\)? \\(update\\|server\\): "
		 "New directory `\\(.*\\)' -- ignored\n"))
	(replace-match "U \\3 (new directory)\n")
	(setq dont-move t))
	

       ;; Ignore RCS noise
       ((looking-at
	 (concat "^"
		 (mapconcat
		  (lambda (s)
		    (concat "\\(" s "\\)"))
		  '("RCS file: .*,v"
		    "retrieving revision .*[0-9]"
		    ;; Don't worry, SVN prints a msg about conflicts
		    ;; as well, we're just removing the RCS msg.
		    "rcsmerge: warning: conflicts during merge")
		  "\\|")
		 "[^\n]*\n"))
	(dsvn-kill-entire-line)
	(setq dont-move t))

       ;; Rewrite M lines from merges as C lines.
       ((looking-at "^Merging differences[^\n]*\n")
	(dsvn-kill-entire-line)
	(setq dont-move t)
	(setq dsvn-conflict t))

       ((and dsvn-conflict (looking-at "^C "))
	(setq dsvn-conflict nil))

       ((and dsvn-conflict (looking-at "^M "))
	(replace-match "C ")
	(setq dsvn-conflict nil)))

      ;; Move forward.  If point changes we have more stuff to do.
      (if dont-move
	  nil
	(setq prev-point (point))
	(forward-line)
	(setq stuff-to-do (> (point) prev-point))))))

(defun dsvn-sentinel (proc msg)
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
	    (dsvn-redraw-modeline)

	    ;; Indicate status in buffer too.  Remember that dsvn-mode
	    ;; makes the buffer read-only.
	    (save-excursion
	      (goto-char (point-max))
	      (setq buffer-read-only nil)
	      (insert "\n" msg-long "\n")
	      (setq buffer-read-only t))

	    ;; Go to the first file, if there is one, unless the user
	    ;; has already moved.  dsvn-next-line will print stuff
	    ;; unless dsvn-explain-each-line is nil.  We make it nil
	    ;; if BUF is not visible.  Also, dsvn-next-line will error
	    ;; if no files.
	    (if (= (point) (point-min))
		(let ((dsvn-explain-each-line (get-buffer-window buf 'visible)))
		  (condition-case nil
		      (dsvn-next-line)
		    (error nil)))))))))

(provide 'dsvn)

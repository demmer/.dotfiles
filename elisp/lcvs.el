;;; lcvs.el --- a little CVS mode, or lomew's CVS mode

;; Copyright (C) 1997-2000 Bart Robinson <lomew@cs.utah.edu>

;; Author: Bart Robinson <lomew@cs.utah.edu>
;; Created: Aug 1997
;; Version: 1.1
(defconst lcvs-version "1.1")
;; Date: Jan 24, 2000
;; Keywords: cvs

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
;;	load-path, autoloads, etc
;;	Warning about byte-compiling:
;;	- If this is byte compiled on XEmacs, it won't work correctly on Emacs
;;	  It will fail in mysterious ways.  I haven't tried the reverse.

;; FEATURES
;; Why not PCL-CVS?
;;	- this is less verbose and simpler
;;	- you can do diffs, etc while the "cvs -nq update" is running
;;	- less fragile, doesn't depend on much cvs output parsing

;; BUGS:
;; - ** If kill a line/region with marked files then the marks stay on
;;   the mark list.  Also undo issues.
;; - chokes when using CVS_RSH=ssh and get prompted for passwd
;; - fix add to cd to dir -- ie handle adding with slash in name,
;;   seems to work fine for me with cvs-1.10

;; TODO:
;;
;; - Show tag name in commit buffer like "cvs commit" when committing on
;;   a branch
;;
;; - Font lock stuff that works on Emacs and XEmacs
;;
;; - Emacs: how to indicate when process is running.  On XEmacs it is in the
;;   mode line.
;;
;; - Change directory prompting to default to cwd and have previous
;;   one in history.  This is a god damn hassle on Emacs since
;;   history isn't god damn integrated with read-file-name (dammit).
;;
;; - Deal with the "New directory `foo' -- ignored" and "no longer
;;   pertinent" messages, especially the latter since they require you
;;   to go to a shell and do the update.  Put the new dir ones at the
;;   bottom and rewrite the "no longer pertinent" ones as "U
;;   filename", since that is how added files are handled.  Could do
;;   this with an input filter (see amsh.el and doc) or by a post-
;;   processing stage.
;;
;; - Hook in with "ediff-revision" for diffs (demmer).  Looks like there isn't
;;   a way to handle the D command, however.
;;
;; - Might be cool to put log/status/diff/etc buffers in view mode
;;
;; - Think more about how dont-use-existing should work.  Could ask "reuse
;;   buffer foo" then say "use g to refresh".  Maybe lcvs-update should just
;;   always do it, but examine would reuse/ask.
;;
;; - Don't allow certain commands if process still running
;;
;; - Provide a way to update a subdir.  This would take the dirname of the
;;   the file under the cursor and update that dir.  Could prompt for dir
;;   with default and M-p/M-n would make less/more specific.
;;
;; - Bind "l" to do local update (not recursive), or have some way to tell
;;   examine to not recurse.  This would take the dirname of the file under the
;;   cursor and update that 
;;
;; - More flexible marking - subdir, all locally modified, region
;;
;; - Save commit message on message ring and allow M-p/n to cycle in
;;   commit-mode.  vc-mode does this.
;;
;; - Handle updating displayed state smarter.  Right now it searches from the
;;   top of the buffer for each one
;;
;; - Have a diff mode that shows the log message at the top of the diff (hard)
;;   This is very useful when working on branches since the log messages
;;   are at the bottom.
;;
;; - For update mode users: have reasonable analog to D command - diff
;;   against last checkin (gordon) also against last last checkin.  This is
;;   is hard since "the previous version" is tricky to compute.
;;
;; - Have a regexp to match/notmatch some files that lcvs-examine would update
;;   automatically (andrew).  This could be done with a postprocessing stage
;;   That collects the files and calls lcvs-update-some-files with them.
;;
;; - Provide a update/commit behavior variable to require a C-u or not
;;   to grab marked files.
;;
;; - Have space and backspace scroll a diff/log/status/etc window if visible


;; User vars.

(defvar lcvs-cvs-command "cvs"
  "*How to call cvs.")

(defvar lcvs-explain-each-line t
  "*If non-nil lcvs-mode will print a message in the echo area
describing the current line.  This info is always available with
the \\[lcvs-explain-this-line] command.")

(defvar lcvs-log-restrict-to-branch nil
  "*If non-nil \\[lcvs-show-log] will show output for the current branch only.
It does this by looking for a CVS/Tag file in the directory of the first
file to be logged and passes the tag within to log.
This can be confusing and thus isn't on by default.")

;; XXX/BRR this works only for XEmacs.
(defvar lcvs-font-lock-keywords
  '(("^[UP][ *].*" . bold)
    ("^M[ *].*" . font-lock-keyword-face)
    ("^C[ *].*" . red)
    ("^cvs update.*" . italic)))


;; Internal Vars.

;; Specifies what to search for when looking for the filename
;; in "cvs update" output.
;; The parens in this are assumed to enclose the state char.
(defvar lcvs-update-regexp "^\\([UPARMC?]\\)[ *]")

;; Describes how a marked line looks.
(defvar lcvs-marked-file-regexp "^[UPARMC?]\\*")

;; Describes lines for files for which a "cvs update" would make sense.
(defvar lcvs-updatable-regexp "^\\([UC]\\)[ *]")

;; List of regexps describing lines that `lcvs-clean' will remove in
;; an update mode buffer.
(defvar lcvs-dirty-regexps '("^\\([UP]\\)[ *]"
			     "^cvs update: warning: .* was lost"))

(defvar lcvs-examine-explanations
  '((?U . "File has been changed in repository and needs to be updated")
    (?A . "File has been added but not committed to the repository")
    (?R . "File has been removed but not committed to the repository")
    (?M . "File has been locally modified")
    (?C . "File has been changed in repository and by you")
    (?? . "File is unknown to CVS"))
  "An alist to map the first column of \"cvs update -n\" output into English.")

(defvar lcvs-update-explanations
  '((?U . "File has been updated")
    (?A . "File has been added but not committed to the repository")
    (?R . "File has been removed but not committed to the repository")
    (?M . "File has been locally modified")
    (?C . "File has undergone a merge and now contains a conflict")
    (?P . "File has been updated with a patch")
    (?? . "File is unknown to CVS"))
  "An alist to map the first column of \"cvs update\" output into English.")

;; The last dir we examined/updated.
(defvar lcvs-last-dir nil)

(defvar lcvs-debug nil
  "If non-nil, put lcvs into debug mode.")

(defvar lcvs-mode-map
  (let ((map (make-sparse-keymap 'lcvs-mode-map)))
    (define-key map "?" 'lcvs-explain-this-line)
    (define-key map "n" 'lcvs-next-line)
    (define-key map "p" 'lcvs-prev-line)
    (define-key map "m" 'lcvs-mark-file)
    (define-key map "u" 'lcvs-unmark-file)
    (define-key map "U" 'lcvs-update-some-files)
    (define-key map "C" 'lcvs-commit)
    (define-key map "d" 'lcvs-diff-base)
    (define-key map "D" 'lcvs-diff-head)
    (define-key map "e" 'lcvs-ediff-base)
    (define-key map "l" 'lcvs-show-log)
    (define-key map "L" 'lcvs-show-log-all-versions)
    (define-key map "s" 'lcvs-show-status)
    (define-key map "S" 'lcvs-sort)
    (define-key map "a" 'lcvs-annotate)
    (define-key map "g" 'lcvs-refresh-buffer)
    (define-key map "G" 'lcvs-examine-or-update)
    (define-key map "f" 'lcvs-find-file)
    (define-key map "o" 'lcvs-find-file-other-window)
    (define-key map "q" 'lcvs-quit-just-bury)
    (define-key map "+" 'lcvs-add)
    (define-key map "\C-k" 'lcvs-kill-region-or-line)
    (define-key map "\C-w" 'lcvs-kill-region)
    (define-key map "\C-xu" 'lcvs-undo)
    (define-key map "\C-xc" 'lcvs-clean)
    (condition-case ()
	;; This is for XEmacs, will error in Emacs.
	(define-key map '(control /) 'lcvs-undo)
      (error nil))
    (define-key map "\C-c\C-k" 'lcvs-kill-process)
    map)
  "Keymap for `lcvs-mode'")

(defvar lcvs-marked-files nil
  "Alist of marked files.  It is in the form `\(file . status)'.")
(make-variable-buffer-local 'lcvs-marked-files)

(defvar lcvs-commit-files nil
  "Alist of the files to be committed.
It has the same form as `lcvs-marked-files'.
For commit-mode buffers.")
(make-variable-buffer-local 'lcvs-commit-files)

(defvar lcvs-commit-parent-buffer nil
  "The examine/update mode buffer.
For commit-mode buffers.")
(make-variable-buffer-local 'lcvs-commit-parent-buffer)

(defvar lcvs-explanations nil
  "An alist explaining lines in this buffer based on the first column.")
(make-variable-buffer-local 'lcvs-explanations)

(defvar lcvs-refresh-command nil
  "The command used to generate the buffer contents.")
(make-variable-buffer-local 'lcvs-refresh-command)

(defvar lcvs-submode nil
  "The submode for `lcvs-mode' for this buffer.")
(make-variable-buffer-local 'lcvs-submode)


;; User functions.

(defun lcvs-examine-update-common (mode dir &optional dont-use-existing)
  ;; Common code for lcvs-examine and lcvs-update.
  (let* ((basename (file-name-nondirectory (directory-file-name dir)))
	 (bufname (format "*CVS-%s-%s*" mode basename))
	 (procname (format "cvs-%s-%s" mode basename))
	 (buf (get-buffer bufname))
	 (cmd (if (eq mode 'examine)
		  (list lcvs-cvs-command "-nq" "update")
		(list lcvs-cvs-command "-q" "update" "-d")))
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
	;; lcvs-mode makes the buffer read-only, so we have to take that
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
	(lcvs-mode mode)

	;; Make the buffer visible and start the process.
	(pop-to-buffer buf)
	(goto-char (point-min))
	(setq proc (apply 'start-process procname buf cmd))
	(set-process-sentinel proc 'lcvs-sentinel)))))

(defun lcvs-examine-update-common-get-args (submode)
  (list (expand-file-name
	 (file-name-as-directory
	  (lcvs-read-directory-name (format "CVS %s directory: " submode)
				    lcvs-last-dir lcvs-last-dir t))
	 current-prefix-arg)))

(defun lcvs-examine (dir &optional dont-use-existing)
  "Call \"cvs -nq update\" in DIR and then call `lcvs-mode' (which see).
Optional arg DONT-USE-EXISTING (interactive prefix arg) means to do the
examine even if there is an examine buffer hanging around for DIR."
  (interactive (lcvs-examine-update-common-get-args "examine"))
  (setq lcvs-last-dir dir)
  (lcvs-examine-update-common 'examine dir dont-use-existing))

(defun lcvs-update (dir &optional dont-use-existing)
  "Call \"cvs -q update -d\" in DIR and then call `lcvs-mode' (which see).
Optional arg DONT-USE-EXISTING (interactive prefix arg) means to do the
update even if there is an update buffer hanging around for DIR."
  (interactive (lcvs-examine-update-common-get-args "update"))
  (setq lcvs-last-dir dir)
  (lcvs-examine-update-common 'update dir dont-use-existing))

(defun lcvs-examine-or-update (dir &optional dont-use-existing)
  "LVCS examine or update based on the current value of `lcvs-submode'.
It doesn't make sense to call this outside of an LCVS buffer."
  (interactive (lcvs-examine-update-common-get-args (symbol-name lcvs-submode)))
  (setq lcvs-last-dir dir)
  (lcvs-examine-update-common lcvs-submode dir dont-use-existing))

(defun lcvs-mode (submode)
  "Major mode for interacting with CVS.
The normal entry point is `lcvs-examine' or `lcvs-update'
although you can paste \"cvs update\" output into a buffer and
then call this.

The hook `lcvs-mode-hook', if set, is run upon entry.

The following keys have meaning in an `lcvs-mode' buffer:
\\{lcvs-mode-map}"
  ;; XXX/BRR completions would be nice, but are hassle since I have to
  ;; define a keymap
  (interactive "SSubmode (examine or update): ")
  (if (and (not (eq submode 'examine))
	   (not (eq submode 'update)))
      (error "Submode should be examine or update"))
  (kill-all-local-variables)
  (setq lcvs-marked-files nil)
  (cond ((eq submode 'examine)
	 (setq lcvs-explanations lcvs-examine-explanations)
	 (setq lcvs-refresh-command 'lcvs-examine))
	(t
	 (setq lcvs-explanations lcvs-update-explanations)
	 (setq lcvs-refresh-command 'lcvs-update)))
  (setq lcvs-submode submode)
  (use-local-map lcvs-mode-map)
  (setq major-mode 'lcvs-mode
	mode-name "LCVS")
  (setq modeline-process '(":%s"))
  (setq buffer-read-only t)
  (run-hooks 'lcvs-mode-hook))

(defun lcvs-clean ()
  "Remove lines for files that have been updated, patched, etc.
For update mode buffers."
  (interactive)
  (if (eq lcvs-submode 'update)
      (save-excursion
	(unwind-protect
	    (let ((regexps lcvs-dirty-regexps))
	      (setq buffer-read-only nil)
	      (while regexps
		(goto-char (point-min))
		(delete-matching-lines (car regexps))
		(setq regexps (cdr regexps))))
	  (setq buffer-read-only t)))))

;; XXX/BRR this can be annoying with cvs versions newer than 1.10 due to
;; the way files that have been changed in your checkout and in the repository.
;; In 1.10 they are reported as C files, but newer versions try to do the merge
;; and report M if there were no conflicts and C if there truly were, but is
;; inconsistent with local and remote repositories.
;; This means if you want to know if a file has been changed in the the
;; repository and in your checkout, you have to look at the lines around the
;; M line for things like "merging differences between blah" -- you can't
;; just look at the line for C or M.  So when you sort the buffer these lines
;; are moved away and it is harder to tell.
;; I've sent a patch to the cvs list which has been apparently ignored.
;; A workaround I haven't implemented is to have a filter on the cvs proc
;; to rewrite these ambiguous M files as C files.
(defun lcvs-sort ()
  "Sort the CVS output in this buffer.
This is useful to get all the M, U, etc files together and the new directory
messages at the bottom."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward lcvs-update-regexp)
    (beginning-of-line)
    (unwind-protect
	(let ((beg (point)))
	  (setq buffer-read-only nil)
	  (sort-lines nil beg (progn (forward-paragraph) (point))))
      (setq buffer-read-only t))))

(defun lcvs-explain-this-line ()
  "Explain what this line means.
Translates stuff like \"M foo/bar.c\" into something like \"this file has
been locally modified\"."
  (interactive)
  (let (char)
    (save-excursion
      (beginning-of-line)
      (if (looking-at lcvs-update-regexp)
	  (progn
	    (setq char (aref (match-string 1) 0))
	    (message (cdr (assoc char lcvs-explanations))))
	(message "I don't know what this line means")))))

(defun lcvs-next-line ()
  "Move cursor to the next file."
  (interactive)
  (if (re-search-forward lcvs-update-regexp nil t)
      (if lcvs-explain-each-line
	  (lcvs-explain-this-line))
    (error "No more files")))

(defun lcvs-prev-line ()
  "Move cursor to the previous file."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (if (re-search-backward lcvs-update-regexp nil t)
	(progn 
	  (goto-char (match-end 0))
	  (if lcvs-explain-each-line
	      (lcvs-explain-this-line)))
      (goto-char pt)
      (error "No more files"))))

(defun lcvs-kill-region-or-line (arg)
  "Kill the selected region or the current line (or ARG lines)."
  (interactive "p")
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(if (lcvs-region-active-p)
	    (kill-region (region-beginning) (region-end))
	  (beginning-of-line)
	  (kill-line arg)))
    (setq buffer-read-only t)))

(defun lcvs-kill-region (beg end)
  "Kill the selected region."
  (interactive "r")
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(kill-region beg end))
    (setq buffer-read-only t)))

(defun lcvs-undo (&optional arg)
  "Undo some changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "P")
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(undo arg))
    (setq buffer-read-only t)))

(defun lcvs-find-file ()
  "Find the file on this line."
  (interactive)
  (let ((file (lcvs-current-file)))
    (if (file-exists-p file)
	(find-file file)
      (error (format "%s not in working tree, probably a new file" file)))))

(defun lcvs-find-file-other-window ()
  "Find the file on this line."
  (interactive)
  (let ((file (lcvs-current-file)))
    (if (file-exists-p file)
	(find-file-other-window file)
      (error (format "%s not in working tree, probably a new file" file)))))

(defun lcvs-mark-file ()
  "Mark the file on this line for later processing."
  (interactive)
  (lcvs-bitch-if-commit-in-progress)
  (let ((file (lcvs-current-file))
	(state (lcvs-current-file-state)))
    (if (assoc file lcvs-marked-files)
	nil
      (save-excursion
	;; Since lcvs-current-file didn't fail, we know we're on a
	;; normal line.
	(beginning-of-line)
	(forward-char 1)
	(setq buffer-read-only nil)
	(subst-char-in-region (point) (1+ (point)) ?\ ?* 'no-undo)
	(setq buffer-read-only t)
	(setq lcvs-marked-files (cons (cons file state) lcvs-marked-files)))))
  ;; `lcvs-next-line' can error if at the end of files.
  (condition-case nil
      (lcvs-next-line)
    (error nil)))

(defun lcvs-unmark-file ()
  "Remove the file on this line from the list of to-be-processed files.
See also `lcvs-mark-file'."
  (interactive)
  (lcvs-bitch-if-commit-in-progress)
  (let ((file (lcvs-current-file)))
    (if (assoc file lcvs-marked-files)
	(save-excursion
	  ;; Since lcvs-current-file didn't fail, we know we're on a
	  ;; normal line.
	  (beginning-of-line)
	  (forward-char 1)
	  (setq buffer-read-only nil)
	  (subst-char-in-region (point) (1+ (point)) ?* ?\ 'no-undo)
	  (setq buffer-read-only t)
	  (setq lcvs-marked-files (lcvs-remassoc file lcvs-marked-files)))))
  ;; `lcvs-next-line' can error if at the end of files.
  (condition-case nil
      (lcvs-next-line)
    (error nil)))

(defun lcvs-update-some-files (arg)
  "Update some files.
By default updates file file on this line.
If supplied with a prefix argument, update the marked files.
If there are no marked files update those that need it according to
`lcvs-updatable-regexp'."
  (interactive "P")
  (if (not (eq lcvs-submode 'examine))
      (error "Updating in an update mode buffer is nonsensical."))
  (let ((files (or (lcvs-get-relevant-files arg 'noerror)
		   (lcvs-get-updatable-files)
		   (error "Nothing to update")))
	(nconflicts 0)
	status)
    (message "Updating...")
    (setq status (lcvs-do-command-quietly "update" '("-q") (mapcar 'car files)))
    (message "Updating...done")
    (if (zerop status)
	;; Parse the update output and update the displayed state accordingly.
	(let ((updated-states (lcvs-parse-update-buffer "*CVS-update*"))
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
		      (lcvs-remove-file-line file)
		    (lcvs-change-file-state file newstate))
		  (lcvs-revert-buffers-visiting-file file)
		  ;; If there were any conflicts, we want them to know.
		  (if (equal newstate ?C)
		      (setq nconflicts (1+ nconflicts))))
	      ;; If there is no match for this file it means it is up to date,
	      ;; so we delete its status line.
	      ;; This typically happens with files that have been committed
	      ;; behind our back, like thru vc-mode.
	      (lcvs-remove-file-line file)))
	  ;; If they operated on the marked list, unmark everything.
	  (if (> (length files) 1)
	      (lcvs-unmark-all-files))))
    (if (not (and (zerop status)
		  (zerop nconflicts)))
	;; An error or conflict happened, bitch appropriately.
	(progn
	  (pop-to-buffer "*CVS-update*")
	  (goto-char (point-min))
	  (insert "\n"
		  "*** The update was not completely successful.\n"
		  (if (zerop nconflicts)
		      "*** Perhaps there were some conflicts?\n"
		    (format "*** There were %d conflicts.\n" nconflicts))
		  "*** Check this buffer closely to determine what is wrong.\n"
		  "*** It contains all the `cvs -q update -d' output.\n"
		  "\n")
	  (error "Update failed, see *CVS-update* buffer for details.")))))

(defun lcvs-commit (arg)
  "Commit files.
If given a prefix argument, commit the marked files.  Otherwise commit
the file on this line."
  (interactive "P")
  (lcvs-bitch-if-commit-in-progress)
  (let ((files (lcvs-get-relevant-files arg))
	(this-buffer (current-buffer))
	cur state)
    ;; Don't let them do something dumb.
    ;; This should match what we display in the "CVS:..." lines.
    (setq cur files)
    (while cur
      (setq state (cdr (car cur)))
      (setq cur (cdr cur))
      (if (or (equal state ?M)
	      (equal state ?A)
	      (equal state ?R))
	  nil
	(error "Can only commit \"M\", \"A\", or \"R\" files")))
    ;; Checks ok, give them the edit buffer.
    (pop-to-buffer (get-buffer-create "*CVS-commit-message*"))
    (lcvs-commit-mode this-buffer files)))

(defun lcvs-diff-base (arg)
  "Diff some files against the BASE revision.
Use this when you have locally modified files and want to see what
you have done.  See also `lcvs-diff-head'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Diffing...")
  (lcvs-do-command "diff"
		   "No differences with the BASE"
		   nil
		   (mapcar 'car (lcvs-get-relevant-files arg)))
  (message "Diffing...done"))

(defun lcvs-diff-head (arg)
  "Diff some files against the HEAD revision.
Use this when files have been checked in by someone else and you want
to see what has changed before you update your copies.  See also
`lcvs-diff-base'.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (if (not (eq lcvs-submode 'examine))
      (error "Diffing BASE against HEAD is nonsensical in update mode"))
  (message "Diffing...")
  (lcvs-do-command "diff"
		   "No differences with the HEAD"
		   nil
		   (append '("-N" "-rBASE" "-rHEAD")
			   (mapcar 'car (lcvs-get-relevant-files arg))))
  (message "Diffing...done"))

(require 'ediff)
(require 'ediff-vers)

(defun lcvs-ediff-base (arg)
  "Ediff some files against the BASE revision.
Use this when you have locally modified files and want to see what
you have done.  See also `lcvs-diff-head'."
  (interactive "P")
;;  (ediff-revision (lcvs-current-file))
  (find-file (lcvs-current-file))
  (ediff-vc-internal "" "" nil)
  )

(defun lcvs-ediff-head (arg)
  "Ediff some files against the HEAD revision.
Use this when you have locally modified files and want to see what
you have done.  See also `lcvs-diff-base'."
  (interactive "P")
;;  (ediff-revision (lcvs-current-file))
  (find-file (lcvs-current-file))
  (ediff-vc-internal "" "" nil)
  )

(defun lcvs-show-log (arg)
  "Show log info for some files.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (let ((files (mapcar 'car (lcvs-get-relevant-files arg))))
    ;; If the CVS/Tag file exists and contains a tag, then we use that
    ;; for logging so we only see messages for this branch.
    (if lcvs-log-restrict-to-branch
	(let* ((dir (file-name-directory (car files)))
	       (tagfile (concat dir (file-name-as-directory "CVS") "Tag"))
	       tag)
	  (if (and (file-exists-p tagfile)
		   (file-readable-p tagfile))
	      (let ((buf (get-buffer-create "*Tag*")))
		(save-excursion
		  (set-buffer buf)
		  (insert-file-contents tagfile t nil nil t)
		  (goto-char (point-min))
		  (if (looking-at "^T\\(.*\\)")
		      (setq files (cons (concat "-r" (match-string 1))
					files)))
		  (kill-buffer buf))))))
    (message "Logging...")
    (lcvs-do-command "log" "No output" nil files)
    (message "Logging...done")))

;; Same as lcvs-show-log, but forces lcvs-log-restrict-to-branch to
;; false
(defun lcvs-show-log-all-versions (arg)
  "Show log info for some files,
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (let ((oldrestrict lcvs-log-restrict-to-branch))
    (setq lcvs-log-restrict-to-branch nil)
    (lcvs-show-log arg)
    (setq lcvs-log-restrict-to-branch oldrestrict)
    ))

(defun lcvs-show-status (arg)
  "Show CVS status info for some files.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Getting status...")
  (lcvs-do-command "status" "No output" nil
		   (mapcar 'car (lcvs-get-relevant-files arg)))
  (message "Getting status...done"))

;;; I don't know why would someone would use marks for this one, but whatever.
(defun lcvs-annotate (arg)
  "Show CVS status info for some files.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line."
  (interactive "P")
  (message "Annotating...")
  (lcvs-do-command "annotate" "No output" nil
		   (mapcar 'car (lcvs-get-relevant-files arg)))
  (message "Annotating...done"))

(defun lcvs-add (arg)
  "Add files to the list of member files.
If given a prefix argument, add the marked files.  Otherwise add
the file on this line.
The files won't be actually added to the repository until they are
formally committed."
  (interactive "P")
  (let ((files (lcvs-get-relevant-files arg))
	status)
    (message "Adding...")
    (setq status (lcvs-do-command-quietly "add" nil (mapcar 'car files)))
    (message "Adding...done")
    (if (zerop status)
	;; Update the diplayed state of the files to "A"
	(let ((cur files)
	      pair)
	  (while cur
	    (setq pair (car cur))
	    (setq cur (cdr cur))
	    (lcvs-change-file-state (car pair) ?A)))
      ;; Otherwise an error happened, bitch appropriately
      (pop-to-buffer "*CVS-add*")
      (goto-char (point-min))
      (insert "\n"
	      "*** The add was not completely successful.\n"
	      "*** Check this buffer closely to determine what is wrong.\n"
	      "\n")
      (error "Add failed, see *CVS-add* buffer for details."))))

(defun lcvs-refresh-buffer ()
  "Re-examine or update this dir."
  (interactive)
  (funcall lcvs-refresh-command default-directory t))

(defun lcvs-quit-just-bury ()
  "\"Quit\" lcvs-mode by burying the buffer."
  (interactive)
  (bury-buffer))

(defun lcvs-kill-process ()
  "Kill the CVS process, if there is one.
We assume the current buffer is the one that is supposed to be running
a CVS process."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (interrupt-process (get-buffer-process (current-buffer)))
    (error "No CVS process running")))

(defun lcvs-debug ()
  (interactive)
  (setq lcvs-debug (not lcvs-debug))
  (message "lcvs-debug set to %s" lcvs-debug))


;; The committing major mode

(defvar lcvs-commit-mode-map
  (let ((map (make-sparse-keymap 'lcvs-commit-mode-map)))
    (define-key map "\C-c\C-c" 'lcvs-commit-finish)
    map)
  "Keymap for `lcvs-commit-mode'")

(defun lcvs-commit-mode (parent files)
  "Major mode for providing a commit log message and committing files.
This mode is not meant to be user invoked."
  (interactive)

  (setq lcvs-commit-parent-buffer parent)
  (setq lcvs-commit-files (sort files (lambda (a b)
					(string-lessp (car a) (car b)))))

  (use-local-map lcvs-commit-mode-map)
  (setq major-mode 'lcvs-commit-mode)
  (setq mode-name "CVS-Commit")

  ;; Insert "CVS: ..." stuff to show them what files they're affecting.
  (insert "CVS: ")(insert-char ?- 70)(insert "\n")
  (insert "CVS: Enter Log.  Lines beginning with `CVS:' are removed"
	  " automatically\n")
  (insert "CVS: Type C-c C-c when done.\n")
  (insert "CVS:\n")
  (insert "CVS: Committing in " default-directory "\n")
  (insert "CVS:\n")
  ;; These should match what is checked for in the file list before
  ;; calling this.
  (lcvs-commit-insert-matching-files ?M "Modified")
  (lcvs-commit-insert-matching-files ?A "Added")
  (lcvs-commit-insert-matching-files ?R "Removed")
  (insert "CVS: ")(insert-char ?- 70)(insert "\n")
  (set-buffer-modified-p nil)

  ;; XXX/BRR shouldn't hardcode binding.
  (message "Type C-c C-c when done.")
  (run-hooks 'text-mode-hook))

(defun lcvs-commit-insert-matching-files (char desc)
  (if (rassoc char lcvs-commit-files)
      (progn
	(insert "CVS: " desc " Files:\n")
	(insert "CVS:    ")
	(let ((cur lcvs-commit-files)
	      pair)
	  (while cur
	    (setq pair (car cur))
	    (setq cur (cdr cur))
	    (let ((file (car pair))
		  (state (cdr pair)))
	      (if (equal state char)
		  (insert file " "))))
	  (insert "\n"))
	(let ((end (point)))
	  (beginning-of-line -1)
	  (let ((fill-prefix "CVS:    "))
	    (fill-region (point) end))))))

(defun lcvs-commit-finish ()
  ;; Finish up the commit by grabbing the commit string and calling cvs commit.
  ;; If the commit worked, clear out the affected files from the parent buffer.
  ;; Otherwise complain loudly and pop up the commit output.
  ;;
  ;; This is tricky since several buffers are involved, each with their own
  ;; local variables and such.  Watch out.
  (interactive)
  (let ((logbuf (get-buffer "*CVS-commit-message*"))
	(commit-bufname "*CVS-commit*")
	status)
    (goto-char (point-min))
    (delete-matching-lines "^CVS:")
    (let ((msg (buffer-string))
	  (files lcvs-commit-files)
	  (parent lcvs-commit-parent-buffer)
	  justfiles)
      (setq justfiles (mapcar 'car files))
      ;; Make sure any buffers visiting those files aren't dirty.
      (lcvs-ensure-saved justfiles)
      ;; Do the commit.  We make sure to do it in the parent buffer so
      ;; CWD, etc is correct.
      ;; We do it this way, all at once, rather than commit for each
      ;; file, mainly so the commitlog remains concise and usable.
      (pop-to-buffer parent)
      (message "Committing...")
      (setq status (lcvs-do-command-quietly "commit" nil
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
	      (lcvs-revert-buffers-visiting-file (car pair))
	      (lcvs-remove-file-line (car pair)))
	    ;; Only chuck buffer when all is good.
	    (kill-buffer logbuf)
	    ;; If they operated on the marked list, unmark everything.
	    (if (> (length files) 1)
		(lcvs-unmark-all-files)))
	(pop-to-buffer commit-bufname)
	(goto-char (point-min))
	(insert "\n"
		"*** The commit was not completely successful.\n"
		"*** Check this buffer closely to determine what is wrong.\n"
		"*** The commit message is in " (buffer-name logbuf) ".\n"
		"\n")
	(error "Commit failed, see %s buffer for details." commit-bufname)))))


;; Internal functions.

(defun lcvs-ensure-saved (files)
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

(defun lcvs-revert-buffers-visiting-file (file)
  ;; Revert any buffers visiting FILE.
  ;; FILE can be relative if the current directory of the caller is correct.
  (let ((buf (get-file-buffer file)))
    (if (and buf
	     (file-exists-p file)
	     (not (verify-visited-file-modtime buf)))
	(save-excursion
	  (set-buffer buf)
	  (revert-buffer nil t t)))))

(defun lcvs-get-relevant-files (use-marks &optional noerror)
  ;; Return a list of files in the form of `lcvs-marked-files'
  ;; If USE-MARKS is non-nil then use the marked file list,
  ;; otherwise use the current file.
  ;; Optionaly NOERROR means return nil instead of throwing an error
  ;; when no files are marked.
  (if use-marks
      (if lcvs-marked-files
	  ;; Copy it in case they change it while editing (commit mode)
	  (copy-sequence lcvs-marked-files)
	(if noerror
	    nil
	  (error "No marked files")))
    (list (cons (lcvs-current-file)
		(lcvs-current-file-state)))))

(defun lcvs-unmark-all-files ()
  ;; Clear the marked-files list and update the displayed state accordingly.
  (setq lcvs-marked-files nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward lcvs-marked-file-regexp nil t)
      (beginning-of-line)
      (forward-char 1)
      (setq buffer-read-only nil)
      (subst-char-in-region (point) (1+ (point)) ?* ?\ 'no-undo)
      (setq buffer-read-only t))))

(defun lcvs-bitch-if-commit-in-progress ()
  ;; If a commit is in progress, go to the commit-message buffer and
  ;; tell them what to do.
  (let ((buf (get-buffer "*CVS-commit-message*")))
    (if buf
	(progn
	  (pop-to-buffer buf)
	  (error "Please finish or abort this commit first")))))

(defun lcvs-get-updatable-files ()
  ;; Return an alist of the files for which a cvs update would make sense.
  ;; The alist is like lcvs-marked-files with (file . state) pairs.
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward lcvs-updatable-regexp nil t)
	(let ((file (lcvs-current-file))
	      (state (lcvs-current-file-state)))
	  (setq result (cons (cons file state) result)))))
    result))

(defun lcvs-parse-update-buffer (buf)
  ;; Return an alist describing the update output in a buffer.
  ;; The buffer is expected to look like "cvs -nq update" output.
  ;; The alist is like lcvs-marked-files with (file . state) pairs.
  (let (result)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward lcvs-update-regexp nil t)
	(let ((file (lcvs-current-file))
	      (state (lcvs-current-file-state)))
	  (setq result (cons (cons file state) result)))))
    result))

;; Too bad Emacs doesn't have this XEmacs feature.
(defun lcvs-region-active-p ()
  (if (fboundp 'region-active-p)
      (region-active-p)
    (and transient-mark-mode
	 (condition-case ()
	     (mark)
	   (error nil)))))

;; Too bad Emacs doesn't have this XEmacs feature.
(defun lcvs-remassoc (key list)
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

(defun lcvs-change-this-file-state (newstate)
  ;; Change the displayed state of the file on this line.
  ;; If the file in in the marked list, update that too.
  (setq buffer-read-only nil)
  (let ((file (lcvs-current-file))
	(curstate (lcvs-current-file-state)))
    (beginning-of-line)
    (subst-char-in-region (point) (1+ (point)) curstate newstate 'noundo)
    (if (assoc file lcvs-marked-files)
	(setq lcvs-marked-files
	      (cons (cons file newstate)
		    (lcvs-remassoc file lcvs-marked-files)))))
  (setq buffer-read-only t))

(defun lcvs-change-file-state (file newstate)
  ;; Change the displayed state of a file.
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat lcvs-update-regexp (regexp-quote file) "$") nil t)
	(lcvs-change-this-file-state newstate))))

(defun lcvs-remove-file-line (file)
  ;; Delete lines in the buffer that match a filename.
  ;; Removes from marked files too.
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (delete-matching-lines (concat lcvs-update-regexp (regexp-quote file) "$"))
    (if (assoc file lcvs-marked-files)
	(setq lcvs-marked-files (lcvs-remassoc file lcvs-marked-files)))
    (setq buffer-read-only t)))

(defun lcvs-read-directory-name (prompt
				 &optional dir default must-match initial-contents
				 history)
  ;; Emacs doesn't have this handy XEmacsism
  (if (fboundp 'read-directory-name)
      (read-directory-name prompt dir default must-match initial-contents
			   history)
    (let ((dir (read-file-name prompt dir default must-match initial-contents)))
      (cond ((file-directory-p dir)
	     dir)
	    ((string-equal dir buffer-file-name)
	     ;; Undo that lame "default to current buffer" crap.
	     (file-name-directory dir))
	    (t
	     (error "%s is not a directory" dir))))))

(defun lcvs-buffer-size (&optional buffer)
  ;; `buffer-size' in Emacs doesn't take an arg like XEmacs.
  (condition-case nil
      (buffer-size buffer)
    (error (save-excursion
	     (if buffer
		 (set-buffer buffer))
	     (buffer-size)))))

(defun lcvs-do-command (cmd default-output cvsopts &optional cmdopts)
  ;; Do the cvs command `cmd' and print the result in buffer *CVS-`cmd'*.
  ;; If there is no output, insert some default text.
  ;; Returns the command exit status.
  (let ((args (append cvsopts (list cmd) cmdopts))
	(bufname (concat "*CVS-" cmd "*"))
	(cwd default-directory)
	status)
    ;; We override `temp-buffer-show-function' so we can insert some
    ;; default text if the command had no output.
    (let ((temp-buffer-show-function
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (setq default-directory cwd)
	       (if (zerop (lcvs-buffer-size buf))
		   (insert default-output))
	       (if lcvs-debug
		   (progn
		     (goto-char (point-min))
		     (insert (concat "DEBUG: "
				     lcvs-cvs-command " "
				     (mapconcat 'identity args " ")
				     "\n")))))
	     (display-buffer buf))))
      (with-output-to-temp-buffer bufname
	(save-excursion
	  (setq status (apply 'call-process lcvs-cvs-command
			      nil standard-output
			      nil args)))))
    status))

(defun lcvs-do-command-quietly (cmd cvsopts &optional cmdopts)
  ;; Do the cvs command `cmd' and print the result in buffer *CVS-`cmd'*.
  ;; Returns the command exit status.
  (let ((args (append cvsopts (list cmd) cmdopts))
	(bufname (concat "*CVS-" cmd "*"))
	(cwd default-directory)
	status buf)
    (setq buf (get-buffer-create bufname))
    (save-excursion
      (set-buffer buf)
      (setq default-directory cwd)
      (setq buffer-read-only nil)
      (erase-buffer))
    (setq status (apply 'call-process lcvs-cvs-command
			nil buf nil
			args))
    status))

(defun lcvs-current-file ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at lcvs-update-regexp)
	(buffer-substring (match-end 0)
			  (progn (end-of-line) (point)))
      (error "No file on this line"))))

(defun lcvs-current-file-state ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at lcvs-update-regexp)
	(aref (match-string 1) 0)
      (error "No file on this line"))))

(defun lcvs-redraw-modeline (&optional all)
  ;; Emacs doesn't have this XEmacsism.
  (if (fboundp 'redraw-modeline)
      (redraw-modeline all)
    nil))

(defun lcvs-sentinel (proc msg)
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
	    (lcvs-redraw-modeline)

	    ;; Indicate status in buffer too.  Remember that lcvs-mode
	    ;; makes the buffer read-only.
	    (save-excursion
	      (goto-char (point-max))
	      (setq buffer-read-only nil)
	      (insert "\n" msg-long "\n")
	      (setq buffer-read-only t))

	    ;; Go to the first file, if there is one, unless the user
	    ;; has already moved.  lcvs-next-line will print stuff
	    ;; unless lcvs-explain-each-line is nil.  We make it nil
	    ;; if BUF is not visible.  Also, lcvs-next-line will error
	    ;; if no files.
	    (if (= (point) (point-min))
		(let ((lcvs-explain-each-line (get-buffer-window buf 'visible)))
		  (condition-case nil
		      (lcvs-next-line)
		    (error nil)))))))))

;;; lcvs.el ends here 

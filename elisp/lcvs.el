;;; lcvs.el --- a little CVS mode, or lomew's CVS mode

;; Copyright (C) 1997-2002 Bart Robinson <lomew@pobox.com>

;; Author: Bart Robinson <lomew@pobox.com>
;; Created: Aug 1997
;; Version: 1.2 ($Revision: 1.38 $)
(defconst lcvs-version "1.2")
;; Date: Jul 10, 2003
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
;;
;; Put this in some directory, for example, ~/emacs/.  Add this directory
;; to your load-path:
;;	(setq load-path (cons (expand-file-name "~/emacs") load-path))
;;
;; Add an autoload so this file gets loaded when lcvs commands are invoked:
;;	(autoload 'lcvs-examine "lcvs" nil t)
;;	(autoload 'lcvs-update "lcvs" nil t)
;;
;; To get the most out of the diff-mode we use do this
;;	(add-hook 'diff-mode-hook 'turn-on-font-lock)
;;
;; WARNING: If this is byte compiled on XEmacs, it won't work
;; correctly on Emacs It will fail in mysterious ways.  I haven't
;; tried the reverse (this might not still be the case, I haven't
;; tried it recently.)
;;

;; FEATURES
;; Why not PCL-CVS?
;;	- this is less verbose and simpler
;;	- you can do diffs, etc while the "cvs -nq update" is running
;;	- less fragile, doesn't depend on much cvs output parsing

;; BUGS:
;; - ** If kill a line/region with marked files then the marks stay on
;;   the mark list.  Also undo issues.
;; - chokes when using CVS_RSH=ssh and get prompted for passwd

;; ASSUMPTIONS:
;; - We assume that all files in a directory have the same branch tag.
;;   That is, CVS/Tag and the tag portion of all entries in CVS/Entries
;;   match.  If this isn't true you are asking for it, really.

;; TODO:
;;
;; - make more asynchronous.  have lcvs-do-command take a callback.
;;
;; - Emacs: how to indicate when process is running.  On XEmacs it is in the
;;   mode line.
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
;; - Have a diff mode that shows the log message at the top of the diff (hard)
;;   This is very useful when working on branches since the log messages
;;   are at the bottom.
;;
;; - For update mode users: have reasonable analog to D command - diff
;;   against last checkin (gordon) also against last last checkin.  This is
;;   is hard since "the previous version" is tricky to compute.
;;
;; - Have space and backspace scroll a diff/log/status/etc window if visible


;; User vars.

(defvar lcvs-cvs-command "cvs"
  "*How to call cvs.")

(defvar lcvs-explain-each-line t
  "*If non-nil lcvs-mode will print a message in the echo area
describing the current line.  This info is always available with
the \\[lcvs-explain-this-line] command.")

(defvar lcvs-log-restrict-to-branch t
  "*If non-nil \\[lcvs-show-log] will show output for the current branch only.
It does this by looking for a CVS/Tag file in the directory of the first
file to be logged and passes the tag within to log.
To see the whole log, use \\[lcvs-show-full-log].")

(defvar lcvs-log-restrict-to-changes nil
  "*If non-nil \\[lcvs-show-log] will show output for changes more recent
than the current repository version.")

(defvar lcvs-correlate-change-log nil
  "*If non-nil \\[lcvs-show-changed-log] will reformat changed log messages
to group similar checkins by author and comment string")

(defvar lcvs-use-view-mode nil
  "*If non nil, lcvs will use view-mode in log, status, etc, buffers.")

(defvar lcvs-use-diff-mode t
  "*If non nil, lcvs will put diff buffers into diff-mode (if available).")

(defvar lcvs-revert-confirm t
  "*If non-nil, reverting files will require confirmation.")

(defvar lcvs-remove-confirm t
  "*If non-nil, removing files will require confirmation.")

(defvar lcvs-translate-update-output t
  "*If non-nil, lcvs will translate cvs update output in examine mode.
This rewrites things like `file is no longer in the repository' as
a `U' line so you can update it in lcvs, not having to go to a shell.")

(defvar lcvs-UP-face
  (let ((face (make-face 'lcvs-UP-face)))
    (condition-case nil
	(make-face-bold face)
      (error (set-face-foreground face "purple")))
    face))

(defvar lcvs-M-face
  (let ((face (make-face 'lcvs-M-face)))
    (condition-case nil
	(make-face-bold face)
      (error nil))
    (set-face-foreground face "royalblue")
    face))

(defvar lcvs-C-face
  (let ((face (make-face 'lcvs-C-face)))
    (condition-case nil
	(make-face-bold face)
      (error nil))
    (set-face-foreground face "red")
    face))

(defvar lcvs-other-face
  (let ((face (make-face 'lcvs-other-face)))
    (condition-case nil
	(make-face-italic face)
      (error (set-face-foreground face "green")))
    face))

(defvar lcvs-font-lock-keywords
  '(("^[UP][ *].*" . lcvs-UP-face)
    ("^M[ *].*" . lcvs-M-face)
    ("^C[ *].*" . lcvs-C-face)
    ("^cvs update.*" . lcvs-other-face)))


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

(defvar lcvs-view-mode-commands
  '("annotate" "log" "stat")
  "List of CVS commands that get their output put into view-mode.")

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
    (define-key map "r" 'lcvs-remove)
    (define-key map "R" 'lcvs-revert)
    (define-key map "C" 'lcvs-commit)
    (define-key map "d" 'lcvs-diff-base)
    (define-key map "D" 'lcvs-diff-head)
    (define-key map "e" 'lcvs-ediff)
    (define-key map "l" 'lcvs-show-log)
    (define-key map "L" 'lcvs-show-full-log)
    (define-key map "x" 'lcvs-show-changed-log)
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

(defvar lcvs-sticky-tag nil
  "The tag to update against.  This is remembered for diffs, etc.")
(make-variable-buffer-local 'lcvs-sticky-tag)


;; User functions.

(defun lcvs-examine-update-common (mode dir &optional dont-use-existing)
  ;; Common code for lcvs-examine and lcvs-update.
  (let* ((basename (file-name-nondirectory (directory-file-name dir)))
	 (bufname (format "*CVS-%s-%s*" mode basename))
	 (procname (format "cvs-%s-%s" mode basename))
	 (buf (get-buffer bufname))
	 (cmd (if (eq mode 'examine)
		  (list lcvs-cvs-command "-nq" "update")
		(list lcvs-cvs-command "-q" "update" "-dP")))
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
	(set-process-filter proc (function lcvs-filter))
	(set-process-sentinel proc (function lcvs-sentinel))))))

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
  (interactive (lcvs-examine-update-common-get-args 'examine))
  (setq lcvs-last-dir dir)
  (lcvs-examine-update-common 'examine dir dont-use-existing))

(defun lcvs-update (dir &optional dont-use-existing)
  "Call \"cvs -q update -dP\" in DIR and then call `lcvs-mode' (which see).
Optional arg DONT-USE-EXISTING (interactive prefix arg) means to do the
update even if there is an update buffer hanging around for DIR."
  (interactive (lcvs-examine-update-common-get-args 'update))
  (setq lcvs-last-dir dir)
  (lcvs-examine-update-common 'update dir dont-use-existing))

(defun lcvs-examine-or-update (dir &optional dont-use-existing)
  "LVCS examine or update based on the current value of `lcvs-submode'.
It doesn't make sense to call this outside of an LCVS buffer."
  (interactive (lcvs-examine-update-common-get-args lcvs-submode))
  (setq lcvs-last-dir dir)
  (lcvs-examine-update-common lcvs-submode dir dont-use-existing))

(defun lcvs-mode (submode)
  "Major mode for interacting with CVS.
The normal entry point is `lcvs-examine' or `lcvs-update'
although you can paste \"cvs update\" output into a buffer and
then call this.

The hook `lcvs-mode-hook', if set, is run upon entry.

The following keys have meaning in an `lcvs-mode' buffer:
\\{lcvs-mode-map}
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
  (make-variable-buffer-local 'font-lock-defaults)
  (setq font-lock-defaults '(lcvs-font-lock-keywords))
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

(defun lcvs-sort ()
  "Sort the CVS output in this buffer.
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

(defun lcvs-update-internal (files)
  ;; Internal guts of update and revert.
  (let ((nconflicts 0)
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
		  "*** It contains all the `cvs -q update -dP' output.\n"
		  "\n")
	  (error "Update failed, see *CVS-update* buffer for details.")))))

(defun lcvs-update-some-files (arg)
  "Update some files.
By default updates the file on this line.
If supplied with a prefix argument, update the marked files.
If there are no marked files update those that need it according to
`lcvs-updatable-regexp'."
  (interactive "P")
  (if (not (eq lcvs-submode 'examine))
      (error "Updating in an update mode buffer is nonsensical."))
  (let ((files (or (lcvs-get-relevant-files arg 'noerror)
		   (lcvs-get-updatable-files)
		   (error "Nothing to update"))))
    (lcvs-update-internal files)))

(defun lcvs-revert (arg)
  "Revert some files, discarding local changes.
By default reverts the file on this line.
If supplied with a prefix argument, revert the marked files.
The files are removed and then updated.  By default this command requires
confirmation to remove the files.  To disable the confirmation, you can
set `lcvs-revert-confirm' to nil."
  (interactive "P")
  (let* ((files (lcvs-get-relevant-files arg))
	 (multiple (cdr files)))
    (if (and lcvs-revert-confirm
	     (not (yes-or-no-p (format "Discard changes to %s? "
				       (if multiple
					   "the marked files"
					 (car (car files)))))))
	(message "Revert cancelled")
      ;; Otherwise remove the files and do the update.
      (mapcar (function (lambda (e)
			  (delete-file (car e))))
	      files)
      (lcvs-update-internal files))))

(defun lcvs-remove (arg)
  "Remove some files, (obviously) discarding local changes.
By default reverts the file on this line.
If supplied with a prefix argument, revert the marked files.
By default this command requires confirmation to remove the files.
To disable the confirmation, you can set `lcvs-remove-confirm' to nil."
  (interactive "P")
  (let* ((files (lcvs-get-relevant-files arg))
	 (multiple (cdr files)))
    (if (and lcvs-remove-confirm
	     (not (yes-or-no-p (format "Remove %s? "
				       (if multiple
					   "the marked files"
					 (car (car files)))))))
	(message "Remove cancelled")
      ;; Otherwise remove the files
      (mapcar (function (lambda (e)
			  (let ((file (car e)))
			    (shell-command (format "rm -rf %s" file))
			    (lcvs-remove-file-line file)
			    )))
	      files))))

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
    (switch-to-buffer (get-buffer-create "*CVS-commit-message*"))
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
		   (cons "-N" (mapcar 'car (lcvs-get-relevant-files arg))))
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
		   (append '("-N" "-rBASE") (lcvs-head-arg)
			   (mapcar 'car (lcvs-get-relevant-files arg))))
  (message "Diffing...done"))

;; There isn't an easy way to do diff-base and diff-head.
;;
;; This doesn't drop you back to the examine/update buffer and is hard
;; to do (would have to set the ediff hook (maybe the cleanup hook) to
;; some function to put us back in the examine buffer then remove the
;; function from the hook.  this would prevent multiple ediffs running
;; from lcvs since the hook is global)
(defun lcvs-ediff (arg)
  "Ediff a file with the HEAD revision.
This will compare the contents of the current-file with the tip of
the current branch."
  (interactive "P")
  (require 'ediff)
  (require 'ediff-vers)
  (find-file (lcvs-current-file))
  (ediff-vc-internal "" "" nil))

(defun lcvs-show-log (arg)
  "Show log info for some files.
If given a prefix argument, use the marked files.  Otherwise use
the file on this line.
Influenced by the `lcvs-log-restrict-to-branch' and
`lcvs-log-restrict-to-changes' variables."
  (interactive "P")
  (let ((files (mapcar 'car (or (lcvs-get-relevant-files arg 'noerror)
				(lcvs-get-updatable-files))))
	args working-revisions)
    ;; If the CVS/Tag file exists and contains a tag, then we use that
    ;; for logging so we only see messages for this branch.
    (if lcvs-log-restrict-to-branch
	(let ((tag (lcvs-sticky-tag (car files))))
	  (if tag
	      (setq args (cons (concat "-r" tag) args)))))

    ;; If logging just one file or need changes for all, figure out
    ;; working-revision list from CVS/Entries.
    (if (or lcvs-log-restrict-to-changes (= (length files) 1))
	(setq working-revisions
	      (mapcar (function
		       (lambda (filename)
			 (let* ((file (file-name-nondirectory filename))
				(dir (file-name-directory filename))
				(entries (concat dir (file-name-as-directory "CVS") "Entries"))
				(working-revision nil))
			   (if (and (file-exists-p entries)
				    (file-readable-p entries))
			       (let ((buf (get-buffer-create "*Entries*")))
				 (save-excursion
				   (set-buffer buf)
				   (erase-buffer)
				   (insert-file-contents entries t nil nil t)
				   (goto-char (point-min))
				   (if (re-search-forward
					;; /foo.c/1.3/blah/blah
					(concat "^/" (regexp-quote file) "/\\([^/]+\\)/") nil t)
				       (setq working-revision (match-string 1)))
				   (kill-buffer buf)
				   working-revision))))))
		      files)))

    ;; do the actual log command
    (message "Logging...")
    (lcvs-do-command "log" "No output" nil (nconc args files))
    
    ;; If we know the working revision and we're just marking one
    ;; file, go mark it and make it visible. Parts stolen from vc.el.
    (if (and (= (length files) 1) (car working-revisions))
	(let (start end lines win windows (working-revision (car working-revisions)))
	  (save-excursion
	    (set-buffer "*CVS-log*")
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

    (if lcvs-log-restrict-to-branch
	;; Do the substitute-command-keys before going to the other buffer.
	(let ((msg (substitute-command-keys
		    (concat
		     "\n"
		     "NOTE: Logging is restricted to the current branch.\n"
		     "      To see the full log, use the \\[lcvs-show-full-log]"
		     " command.\n"))))
	  (save-excursion
	    (set-buffer "*CVS-log*")
	    (goto-char (point-max))
	    (insert msg))))
    
    (if lcvs-log-restrict-to-changes
	;; Do the substitute-command-keys before going to the other buffer.
	(save-excursion
	  (set-buffer "*CVS-log*")
	  (goto-char (point-max))

	  ;; first some cleanup stuff
	  (lcvs-cleanup-logs files working-revisions)
      
	  (if lcvs-correlate-change-log
	      (lcvs-correlate-logs files working-revisions)
	    (let ((msg (substitute-command-keys
			(concat
			 "\n"
			 "NOTE: Logging is restricted to changes in files.\n"
			 "      To see the full log, use the \\[lcvs-show-full-log]"
			 " command.\n"))))
	      (insert msg)
	      ))))
    (message "Logging...done")))

(defvar lcvs-last-update 0
  "Placeholder variable to keep the timestamp of the last progress update")

(defvar lcvs-update-interval 2
  "Number of seconds between progress updates")
  
(defun lcvs-update-progress (what)
  "Simple procedure to update progress in the current buffer"
  (let ((now (nth 1 (current-time))))
    (if (> (- now lcvs-last-update) 2)
	(progn
	  (message "%s... (%d%% complete)" what
		   (/ (* 100 (- (point) (point-min)))
		      (- (point-max) (point))))
	  (setq lcvs-last-update now)))))

(defun lcvs-cleanup-logs (&optional files working-revisions)
  "Strip out all the RCS crap from the log file, leaving only the
interesting bits. Useful for lcvs-correlate-logs."
  (interactive)

  (save-excursion
    (set-buffer "*CVS-log*")
    (buffer-enable-undo)
    (goto-char (point-min))

    (setq buffer-read-only nil)
      
    (make-variable-buffer-local 'kill-whole-line)
    (setq kill-whole-line t)

    ;; strip out irrelevant log entries
    (if files
	(mapcar*
	 (function
	  (lambda (file version)
	    (save-excursion
	      (if (not (re-search-forward (format "Working file: %s" file) nil t)) nil
		(if (not (re-search-forward (format "revision %s" version) nil t)) nil
		  (previous-line 1)
		  (beginning-of-line)
		  (let ((beg (point)))
		    (re-search-forward "^========[=]+\n")
		    (previous-line 1)
		    (beginning-of-line)
		    (kill-region beg (point)))
		  )))))
	 files working-revisions))
	    
    ;; strip out all the crap, leave just the meat
    (while (re-search-forward "^RCS file:" nil t)
      (beginning-of-line)

      (lcvs-update-progress "Stripping RCS junk")
      
      (let ((beg (point)) bound filename)

	(search-forward "Working file: ")
	(setq filename (buffer-substring (point) (line-end-position)))
	(re-search-forward "^----[-]+\n")
	(kill-region beg (point))
	(insert "===================================")
	(insert "===================================\n")

	(save-excursion
	  ;; Find the bound for this file
	  (re-search-forward "^========[=]+\n")
	  (kill-region (match-beginning 0) (match-end 0))
	  (if (looking-at "^\n") (kill-line))
	  (setq bound (point))

	  ;; add the filename to any subsequent revisions
 	  (save-excursion
 	    (while (re-search-backward "^revision " beg t)
 	      (replace-match (format "CVS revision %s:" filename) nil nil)))
	    
	  ;; and the first log entry
	  (while (re-search-backward "^====[=]+\nrevision " beg t)
	    (forward-word 1)
	    (forward-char 1)
	    (insert (format "%s:" filename))
	    (beginning-of-line)
	    (insert "CVS ")
	    (previous-line 1))
	  
	  )))
    
    (goto-char (point-max))
    (insert "===================================")
    (insert "===================================\n") ;; need one at the end
    (goto-char (point-min))
    
    ;; finally change ----- to ======
    (save-excursion
      (while (re-search-forward "^----[-]+" nil t)
	(beginning-of-line)
	(kill-line)
	(insert "===================================")
	(insert "===================================\n")
	))
  ))

(defun lcvs-correlate-logs (&optional files working-revisions)
  "Parse the *CVS-log* buffer, reformatting to group checkins by author
and log message"
  (interactive)

  ;; first some setup stuff
  (save-excursion
    (set-buffer "*CVS-log*")
    (buffer-enable-undo)
    (goto-char (point-min))

    (let ((old-gc-threshold gc-cons-threshold))

      (setq buffer-read-only nil)

      (make-variable-buffer-local 'gc-cons-threshold)
      (setq gc-cons-threshold 100000000)

      (make-variable-buffer-local 'kill-whole-line)
      (setq kill-whole-line t)

      ;; scan through again -- for each file, scan through the
      ;; remainder to try to find matches. 
      (let (regexp file file2 ver ver2 date date2 author author2 log log2 filept)
	(setq regexp (concat "^CVS revision \\([^\:]*\\):\\([^\n]*\\)\n"
		      "date: \\([^;]*\\);  author: \\([^;]*\\).*\n"))

	(while (re-search-forward regexp nil t)
	  (setq file (match-string 1))
	  (setq ver  (match-string 2))
	  (setq date (match-string 3))
	  (setq author (match-string 4))

	  (lcvs-update-progress "Correlating logs")

	  ;; clear all the header stuff once more
	  (kill-region (match-beginning 0) (match-end 0))

	  (insert (format "file: %s:%s\n" file ver))
	  (setq filept (point))
	  (insert (format "date: %s;  author: %s;\n\n" date author))

	  (save-excursion
	    ;; copy all the log lines into the log variable to do the
	    ;; comparison but don't erase them from the buffer
	    (let ((beg (point)) end)
	      (while (not (looking-at "^[-=]+\n"))
		(next-line 1))
	      (setq end (point))
	      (setq log (buffer-substring beg end))
	      )
	    
	    ;; now repeat the scan through the rest of the entries
	    (while (re-search-forward regexp nil t)
	      (setq file2 (match-string 1))
	      (setq ver2  (match-string 2))
	      (setq date2 (match-string 3))
	      (setq author2 (match-string 4))

	      ;; again copy all the log lines into the log variable to do the
	      ;; comparison but don't erase them from the buffer
	      (let ((beg (match-beginning 0)) (logbeg (point)))
		(while (not (looking-at "^[-=]+\n"))
		  (next-line 1))
		(setq log2 (buffer-substring logbeg (point)))

		;; now, if they match, clear out the contents of the
		;; second entry and add the file / version to the list
		(if (and (string= log log2)
			 (string= author author2))
		    (save-excursion
		      (next-line 1)
		      (kill-region beg (point))
		      (goto-char filept)
		      (insert (format "file: %s:%s\n" file2 ver2))))
	      )))
	  ))

      ;;; finally, sort the log entries by date
      (lcvs-update-progress "Sorting records")
      (sort-regexp-fields nil "^[=]+[^=]+" "^date:.*$" (point-min) (point-max))
      
      (garbage-collect)
      (setq gc-cons-threshold old-gc-threshold)
      (setq buffer-read-only t)
    )))

(defun lcvs-show-full-log (arg)
  "Like \\[lcvs-show-log] but ignores `lcvs-log-restrict-to-branch'."
  (interactive "P")
  (let ((lcvs-log-restrict-to-branch nil))
    (lcvs-show-log arg)))

(defun lcvs-show-changed-log (arg)
  "Like \\[lcvs-show-log] but forces `lcvs-log-restrict-to-changes'."
  (interactive "P")
  (let ((lcvs-log-restrict-to-changes t)
	(lcvs-log-restrict-to-branch nil)
	(lcvs-correlate-change-log t))
    (lcvs-show-log arg)))

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

(defvar lcvs-commit-msgs (make-ring 10))
(defvar lcvs-commit-msgs-index nil)
(make-variable-buffer-local 'lcvs-commit-msgs-index)

(defvar lcvs-commit-initial-buffer-contents "")
(make-variable-buffer-local 'lcvs-commit-initial-buffer-contents)

(defvar lcvs-commit-mode-map
  (let ((map (make-sparse-keymap 'lcvs-commit-mode-map)))
    (define-key map "\C-c\C-c" 'lcvs-commit-finish)
    (define-key map "\M-p" 'lcvs-commit-insert-prev-commit-msg)
    (define-key map "\M-n" 'lcvs-commit-insert-next-commit-msg)
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
  (setq lcvs-commit-msgs-index nil)

  ;; Insert "CVS: ..." stuff to show them what files they're affecting.
  (insert "CVS: ")(insert-char ?- 70)(insert "\n")
  (insert "CVS: Enter Log.  Lines beginning with `CVS:' are removed"
	  " automatically\n")
  (insert (substitute-command-keys
	   "CVS: Type \\[lcvs-commit-finish] when done.\n"))
  (insert "CVS:\n")
  (insert "CVS: Committing in " default-directory "\n")
  (insert "CVS:\n")
  ;; These should match what is checked for in the file list before
  ;; calling this.
  (lcvs-commit-insert-matching-files ?M "Modified")
  (lcvs-commit-insert-matching-files ?A "Added")
  (lcvs-commit-insert-matching-files ?R "Removed")
  (insert "CVS: ")(insert-char ?- 70)(insert "\n")
  (lcvs-maybe-insert-template)
  (setq lcvs-commit-initial-buffer-contents (buffer-string))
  (set-buffer-modified-p nil)

  (message (substitute-command-keys "Type \\[lcvs-commit-finish] when done."))
  (run-hooks 'text-mode-hook))

(defun lcvs-maybe-insert-template ()
  ;; Look for a template to insert.
  ;; This is for the CVSROOT/rcsinfo feature of cvs.

  ;; First look in CVS/Template.  This will exist if rcsinfo
  ;; specifies a template but the repository is remote.
  (let ((cvs-template-filename (concat (file-name-as-directory "CVS")
				       "Template")))
    (if (file-readable-p cvs-template-filename)
	(insert-file-contents cvs-template-filename)
      ;; Otherwise check if the repository is local and use CVSROOT/rcsinfo
      ;; to find the template.
      (let* ((rcsinfo (lcvs-load-rcsinfo))
	     (templates (lcvs-apply-rcsinfo
			 rcsinfo
			 (lcvs-get-directories (mapcar 'car
						       lcvs-commit-files)))))
	(if templates
	    (progn
	      (if (not (lcvs-all-same-elements templates))
		  (insert "CVS: WARNING: files matched different "
			  "templates in CVSROOT/rcsinfo, using first"))
	      (insert-file-contents (car templates))))))))

(defun lcvs-load-rcsinfo ()
  ;; Load the CVSROOT/rcsinfo file into an alist ((pattern . template) ...)
  ;; Return the alist
  ;; Return nil if we can't find it, can't parse it, etc.
  (let (result local-cvsroot)
    ;; Look for a local CVS/Root.  It must start with a slash or :local:
    (let ((cvsroot-contents (lcvs-get-special-file-contents-as-one-line
			     "Root")))
      (if cvsroot-contents
	  (if (string-match "^:local:\\(.*\\)" cvsroot-contents)
	      (setq local-cvsroot (match-string 1 cvsroot-contents))
	    (if (file-name-absolute-p cvsroot-contents)
		(setq local-cvsroot cvsroot-contents)))))
    (if local-cvsroot
	;; Now look for the rcsinfo file.
	;; This is normally in $CVSROOT/CVSROOT/rcsinfo
	;; see cvs/src/parseinfo.c for the format of these files
	(let ((rcsinfo-filename (concat (file-name-as-directory local-cvsroot)
					(file-name-as-directory "CVSROOT")
					"rcsinfo")))
	  (if (file-readable-p rcsinfo-filename)
	      (let ((tempbuf (get-buffer-create " *CVS-rcsinfo*"))
		    done)
		(save-excursion
		  (set-buffer tempbuf)
		  (erase-buffer)
		  (insert-file-contents rcsinfo-filename)
		  (goto-char (point-min))
		  (while (not (eobp))
		    (if (and (not (looking-at "^#"))
			     (looking-at "\\s-*\\(\\S-+\\)\\s-+\\(.*\\)"))
			(let ((exp (lcvs-emacsify-regexp (match-string 1)))
			      (value (lcvs-expand local-cvsroot (match-string 2))))
			  ;; Append since order matters.
			  (setq result (append result (list (cons exp value))))))
		    (forward-line)))
		(kill-buffer tempbuf)))))
    result))

(defun lcvs-expand (cvsroot str)
  ;; Expand env vars and ~user things in STR.
  ;; Use param CVSROOT for $CVSROOT
  (let ((current-root (getenv "CVSROOT"))
	result)
    (unwind-protect
	(progn
	  (setenv "CVSROOT" cvsroot)
	  (setq result (expand-file-name (substitute-in-file-name str))))
      (setenv "CVSROOT" current-root))
    result))

(defun lcvs-get-directories (files)
  ;; For each file, get the directory it is in relative to $CVSROOT.
  ;; Algorithm:
  ;;   We want to prepend something from the CVS/Repository to each file.
  ;;   If the CVS/Repository is relative, that is our prefix.
  ;;   Else, get the CVS/Root, remove the :local: prefix if present and
  ;;   remove this from the CVS/Repository, that is our prefix.

  (let ((repos (lcvs-get-special-file-contents-as-one-line "Repository"))
	prefix)
    (if repos
	(progn
	  ;; Absolute path, we need to modify it according to CVS/Root
	  (if (file-name-absolute-p repos)
	      (let ((root (lcvs-get-special-file-contents-as-one-line "Root")))
		(if root
		    (progn
		      (if (string-match "^:local:\\(.*\\)" root)
			  (setq root (match-string 1 root)))
		      (replace-in-string repos
					 (concat "^" (regexp-quote root))
					 "")))))
	  (setq prefix (file-name-as-directory repos))))
    (if prefix
	(mapcar (lambda (f)
		  (concat prefix f))
		files)
      ;; No prefix probably means no CVS/Repository, they're fucked but
      ;; don't do anything.
      files)))

(defun lcvs-apply-rcsinfo (rcsinfo files)
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

(defun lcvs-commit-insert-matching-files (char desc)
  (if (rassoc char lcvs-commit-files)
      (let ((prefix "CVS:    "))
	(insert "CVS: " desc " Files:\n")
	;; This is a lame place to put this, but that is close to what cvs does.
	;; We don't look in the CVS/Entries file but assume the CVS/Tag file
	;; is the same.
	(let ((tag (lcvs-sticky-tag (car (car lcvs-commit-files)))))
	  (if tag
	      (insert "CVS:  Tag: " tag "\n")))
	(insert prefix)
	(let ((cur lcvs-commit-files)
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
      (ring-insert lcvs-commit-msgs msg)
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

(defun lcvs-commit-insert-prev-commit-msg (arg)
  "Cycle backwards thru commit message history."
  (interactive "*p")
  (let ((len (ring-length lcvs-commit-msgs)))
    (if (= len 0)
	(error "Empty commit message string")
      (erase-buffer)
      (insert lcvs-commit-initial-buffer-contents)
      ;; Initialize the index on the first use of this command
      ;; so that the first M-p gets index 0, and the first M-n gets
      ;; index -1.
      (if (null lcvs-commit-msgs-index)
	  (setq lcvs-commit-msgs-index
		(if (> arg 0) -1
		  (if (< arg 0) 1 0))))
      (setq lcvs-commit-msgs-index
	    (mod (+ lcvs-commit-msgs-index arg) len))
      (message "Commit Msg %d" (1+ lcvs-commit-msgs-index))
      (insert (ring-ref lcvs-commit-msgs lcvs-commit-msgs-index)))))

(defun lcvs-commit-insert-next-commit-msg (arg)
  "Cycle forwards thru commit message history."
  (interactive "*p")
  (lcvs-commit-insert-prev-commit-msg (- arg)))


;; Internal functions.

(defun lcvs-get-special-file-contents (filename)
  ;; Get the contents of CVS/<filename>
  (let ((filename (concat (file-name-as-directory "CVS") filename))
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

(defun lcvs-get-special-file-contents-as-one-line (filename)
  ;; Get the contents of CVS/<filename> as a single line.
  ;; This basically gets you the first line.
  (let ((contents (lcvs-get-special-file-contents filename)))
    (if contents
	(progn
	  (string-match ".*" contents)
	  (match-string 0 contents)))))

(defun lcvs-emacsify-regexp (exp)
  ;; Convert a CVS-style regular expression into Emacs-style.
  ;; CVS-style is egrep-style
  ;; We basically just fix the parens and bars to be quoted,
  ;; we don't handle the fancy stuff.
  (setq exp (replace-in-string exp "\\(^\\|[^\\]\\)(" "\\1\\\\("))
  (setq exp (replace-in-string exp "\\([^\\]\\))" "\\1\\\\)"))
  (setq exp (replace-in-string exp "\\([^\\]\\)|" "\\1\\\\|"))
  exp)

(defun lcvs-all-same-elements (list)
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

(defun lcvs-head-arg ()
  ;; Return an arg referring to the HEAD, this might be a tag.
  (or lcvs-sticky-tag
      '("-rHEAD")))

(defun lcvs-set-view-mode (win buf)
  ;; Turn view-mode on for BUF in window WIN, making sure quitting it
  ;; will get us back somewhere sane.
  (if (not lcvs-use-view-mode)
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

(defun lcvs-sticky-tag (file)
  ;; Return the sticky tag for FILE, or nil.
  ;; Just looks in the CVS/Tag file, not in Entries.
  (let* ((dir (file-name-directory file))
	 (tagfile (concat dir (file-name-as-directory "CVS") "Tag"))
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
		(setq tag (match-string 1)))
	    (kill-buffer buf))))
    tag))

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
	     (not (verify-visited-file-modtime buf))
	     (not (buffer-modified-p buf)))
	(save-excursion
	  (set-buffer buf)
	  (revert-buffer nil t)))))

(defun lcvs-get-relevant-files (use-marks &optional noerror)
  ;; Return a list of files in the form of `lcvs-marked-files'
  ;; If USE-MARKS is non-nil then use the marked file list,
  ;; otherwise use the current file.
  ;; Optionaly NOERROR means return nil instead of throwing an error
  ;; when no files are marked.
  (if use-marks
      (if lcvs-marked-files
	  ;; sort modifies
	  (sort (copy-sequence lcvs-marked-files)
		(function (lambda (a b)
			    (string-lessp (car a) (car b)))))
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
  ;; Delete lines in the buffer that match a filename, allowing for a
  ;; parenthesized comment on the file line.
  ;; Removes from marked files too.
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (delete-matching-lines (concat lcvs-update-regexp
				   (regexp-quote file)
				   "\\( (.*)\\)?"
				   "$"))
    (if (assoc file lcvs-marked-files)
	(setq lcvs-marked-files (lcvs-remassoc file lcvs-marked-files)))
    (setq buffer-read-only t)))

(defun lcvs-read-directory-name (prompt
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
	       (if (and lcvs-use-diff-mode
			(string-equal cmd "diff")
			(fboundp 'diff-mode))
		   (diff-mode))
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
	     (let ((win (display-buffer buf)))
	       (if (member cmd lcvs-view-mode-commands)
		   (lcvs-set-view-mode win buf))))))
      (with-output-to-temp-buffer bufname
	(setq status (apply 'call-process lcvs-cvs-command
			    nil standard-output
			    nil args))))
    status))

(defun lcvs-do-command-quietly (cmd cvsopts &optional cmdopts noerase)
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
      (if (not noerase)
	  (erase-buffer)))
    (setq status (apply 'call-process lcvs-cvs-command
			nil buf nil
			args))
    status))

(defun lcvs-current-file ()
  ;; Assuming the current line is updatable (by lcvs-update-regexp), return
  ;; the file name, minus any parenthesized comments that lcvs put in.
  ;; e.g. "U elisp (new directory)" => "elisp"
  (save-excursion
    (beginning-of-line)
    (if (looking-at lcvs-update-regexp)
	(let ((beg (match-end 0))
	      (end (search-forward "(" (line-end-position) t)))
	  (if end
	      (setq end (- end 2)) 		;; back up paren and whitespace
	    (setq end (line-end-position))) 	;; take the whole line
	  (buffer-substring beg end))
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

(defun lcvs-filter (proc string)
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
	(if (and (eq lcvs-submode 'examine)
		 lcvs-translate-update-output)
	    (lcvs-fix-lines beg)))
      (if moving (goto-char (process-mark proc))))))

(defvar lcvs-conflict nil)
(make-variable-buffer-local 'lcvs-conflict)

(defun lcvs-kill-entire-line ()
  "Kill the entire line containing point."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun lcvs-fix-lines (beg)
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
	 (concat "^cvs\\(pserver\\)? \\(update\\|server\\): `?\\([^']*\\)'?"
		 " is no longer \\(pertinent\\|\\(in the repository\\)\\).*\n"))
	(replace-match "U \\3 (removed)\n")
	(setq dont-move t))

       ((looking-at
	 (concat "^cvs\\(pserver\\)? \\(update\\|server\\): warning: `?\\([^']*\\)'?"
		 " is not (any longer) pertinent\n"))
	(replace-match "U \\3 (removed)\n")
	(setq dont-move t))

       ;; Rewrite new directories as "U" with a note
       ((looking-at
	 (concat "^cvs\\(pserver\\)? \\(update\\|server\\): "
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
		    ;; Don't worry, CVS prints a msg about conflicts
		    ;; as well, we're just removing the RCS msg.
		    "rcsmerge: warning: conflicts during merge")
		  "\\|")
		 "[^\n]*\n"))
	(lcvs-kill-entire-line)
	(setq dont-move t))

       ;; Rewrite M lines from merges as C lines.
       ((looking-at "^Merging differences[^\n]*\n")
	(lcvs-kill-entire-line)
	(setq dont-move t)
	(setq lcvs-conflict t))

       ((and lcvs-conflict (looking-at "^C "))
	(setq lcvs-conflict nil))

       ((and lcvs-conflict (looking-at "^M "))
	(replace-match "C ")
	(setq lcvs-conflict nil)))

      ;; Move forward.  If point changes we have more stuff to do.
      (if dont-move
	  nil
	(setq prev-point (point))
	(forward-line)
	(setq stuff-to-do (> (point) prev-point))))))

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

(provide 'lcvs)

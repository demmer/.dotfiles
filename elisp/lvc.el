
;
; Common code for all the lvc modes
;

(require 'lcvs)
(require 'dsvn)
(require 'lvc-hg)

(defvar lvc-use-view-mode nil
  "*If non nil, lsvn will use view-mode in log, status, etc, buffers.")

(defvar lvc-use-diff-mode t
  "*If non nil, lsvn will put diff buffers into diff-mode (if available).")

(defvar lvc-explain-each-line t
  "*If non-nil, the various modes will print a message in the
echo area describing the current line. This info is always
available with the \\[lcvs-explain-this-line] command.")

;; The last dir we examined
(defvar lvc-last-dir nil
  "Last directory where lvc-status was run.")

(defun lvc-read-directory-name (prompt
				&optional dir default must-match
				initial-contents)
  "Prompt for a directory name to use for lvc functions"
  ;; Emacs doesn't have this handy XEmacsism
  (if (fboundp 'read-directory-name)
      (read-directory-name prompt dir default must-match initial-contents)
    (let ((dir (read-file-name prompt dir default must-match
			       initial-contents)))
      (cond ((file-directory-p dir)
	     dir)
	    ((or (string-equal dir buffer-file-name)
		 (string-equal (expand-file-name dir) buffer-file-name))
	     ;; Undo that lame "default to current buffer" crap.
	     (file-name-directory dir))
	    (t
	     (error "%s is not a directory" dir))))))

(defun lvc-status-get-args ()
  "Get the directory name to run the various lvc-status functions"
  (list (expand-file-name
	 (file-name-as-directory
	  (lvc-read-directory-name "VC examine directory: "
				   lvc-last-dir lvc-last-dir nil))
	 current-prefix-arg)))

(defun lvc-status (dir)
  "Run the appropriate lvc function on the given directory."
  (interactive (lvc-status-get-args))
  (cond
   ((file-exists-p (format "%s/CVS" (directory-file-name dir)))
    (lcvs-examine dir))
   ((file-exists-p (format "%s/.svn" (directory-file-name dir)))
    (dsvn-examine dir))
   ((file-exists-p (format "%s/.hg" (directory-file-name dir)))
    (lvc-hg-status dir))
   (t (message (format "Cannot determine VC system in directory %s" dir)))
   )
  )

(provide 'lvc)

(require 'compile)
(require 'grep)
	 
(defvar grope-history nil)
(defvar grope-replace-history nil)

(defun grope-process-setup ()
  "Set up `compilation-exit-message-function' for `grope'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     (cond ((zerop code)
		    '("finished (matches found)\n" . "matched"))
		   ((= code 1)
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code)))))

(define-compilation-mode grope-mode "Grope"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       grep-regexp-alist)
  (set (make-local-variable 'compilation-process-setup-function)
       'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'buffer-read-only) nil))

(defvar grope-mode-font-lock-keywords
  '(;; Command output lines.
     ("^\\([A-Za-z_0-9/\.+-]+\\)[ \t]*:" 1 font-lock-function-name-face)
     (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
      1 grep-error-face)
     ;; remove match from grep-regexp-alist before fontifying
     ("^Grope[/a-zA-z]* started.*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t))
     ("^Grope[/a-zA-z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 compilation-info-face nil t)
      (2 compilation-warning-face nil t))
     ("^Grope[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil message nil help-echo nil mouse-face nil) t)
      (1 grep-error-face)
      (2 grep-error-face nil t))
     ("^.+?-[0-9]+-.*\n" (0 grep-context-face))
     ;; Highlight grep matches and delete markers
     ("\\(\033\\[01;31m\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)"
      ;; Refontification does not work after the markers have been
      ;; deleted.  So we use the font-lock-face property here as Font
      ;; Lock does not clear that.
      (2 (list 'face nil 'font-lock-face grep-match-face))
      ((lambda (bound))
       (progn
	 ;; Delete markers with `replace-match' because it updates
	 ;; the match-data, whereas `delete-region' would render it obsolete.
	 (replace-match "" t t nil 3)
	 (replace-match "" t t nil 1))))
     ("\\(\033\\[[0-9;]*[mK]\\)"
      ;; Delete all remaining escape sequences
      ((lambda (bound))
       (replace-match "" t t nil 1))))
   "Additional things to highlight in grep output.
This gets tacked on the end of the generated expressions.")

(defun grope (sym)
  "Like grep but using grope"
  (interactive
   (list (read-from-minibuffer "Grope for: "
			       (current-word) nil nil 'grope-history)))
  (compilation-start (concat "grope \"" sym "\"")
		     'grope-mode))

(defun grope-replace (from to)
  (interactive
   (let (from to)
     (setq from (read-from-minibuffer "Grope replace: "
				      (current-word) nil nil 'grope-history))
     (setq to   (read-from-minibuffer (format "Grope replace: %s with: " from)
				      nil nil nil 'grope-replace-history))
     (list from to)))

  (let (grope-buf grope-proc elapsed)
    ;; run grope, wait for it to complete
    (setq grope-buf (grope from))
    (setq grope-proc (get-buffer-process grope-buf))

    (message "Waiting for grope process...")
    (while (eq (process-status grope-proc) 'run)
      (sit-for 2))

    (unwind-protect
	(progn
	  (set-buffer grope-buf)
	  (first-error)
	  (sit-for 1)
	  ;(setq from (replace-regexp-in-string "\\\\" "" from))
	  (query-replace from to nil (point) (line-end-position))
	  (next-error))
      (replace-dehighlight))
    ))

(provide 'grope)

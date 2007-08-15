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


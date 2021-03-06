;;;
;;; elisp configuration file
;;; compiled from amd and other sources
;;;

;;; Set up our load path
(push (format "%s/elisp" *HOME*) load-path)

;;; turn off audio bell
(setq visible-bell t)

;;; disable the annoying ctrl-Z mapping
(global-unset-key "\^Z")

;;; wrap too-long lines in half windows
(setq truncate-partial-width-windows nil)

;;; some defaults
(setq text-mode-hook 'turn-on-auto-fill)
(setq line-number-mode t)
(setq column-number-mode t)
(setq delete-auto-save-files t)

(setq require-final-newline 'ask)

;;; show the time
(load "time" t t)
(display-time)

;;; resizes window using mode-line. Must be bound.
(defun mode-line-resize-dynamically ()
  "Resize a window by dragging the mode-line.
This must be bound to a mouse-down event in the mode-line."
  (interactive "@")
  (let* ((mouse (mouse-position))
	 (start-frame (car mouse))
	 (prev-y (cdr (cdr mouse)))
	 (next (next-window)))
    (if (window-at 0 (+ 1 prev-y)) nil
      (split-window (selected-window) (- (window-height) 4))
      (setq next (next-window)))
    (track-mouse
      (while (and (eq (car-safe (read-event)) 'mouse-movement)
		  (eq next (next-window)))
	(let* ((mouse (mouse-position))
	       (frame (car mouse))
	       (new-y (cdr (cdr mouse)))
	       (delta (- new-y prev-y)))
	  (cond ((and (eq frame start-frame)
		      (> (+ delta (window-height (selected-window)))
			 window-min-height))
		 (enlarge-window delta)
		 (setq prev-y new-y))))))))

;;; Set up our tabs in a cool way
;;(setq default-tab-width 5)
;;(setq tab-stop-list '( 5 10 15 20 25 30 35 40 45 50 55 60 65))
;;(setq default-tab-width 8)
;;(setq tab-stop-list '( 8 16 24 32 40 48 56 64 72 80 ))

;;; some look and feel stuff
(cond (window-system
       ;; cool fontification always
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       (blink-cursor-mode -1)
       (global-font-lock-mode t)
       (transient-mark-mode 1)
       (set-face-foreground 'region "ivory")
       (set-face-background 'region "royal blue")))

(defvar my-frame-params '((auto-raise . nil)
			(menu-bar-lines . 1)
			(horizontal-scroll-bars . nil)
			(vertical-scroll-bars . nil)
			))

;;; initial frame appearance (geometry set by X)
(setq initial-frame-alist (append my-frame-params initial-frame-alist))

;;; other frame appearance
(setq default-frame-alist (append my-frame-params initial-frame-alist))

;; timestamp customizations
(require 'time-stamp)
(setq time-stamp-start "[Mm]odified:[     ]+\\\\?[\"<]+")
(setq time-stamp-format "%b %d, %y %I:%M%p by %u")
;; (append-no-dup 'time-stamp write-file-hooks)

;; start gnuserv (not emacsserver)
(load "gnuserv-compat" 'noerror)
(cond ((and window-system
	    (functionp 'gnuserv-start)
	    (or (file-executable-p "/usr/local/bin/gnuserv")
		(file-executable-p "/usr/bin/gnuserv")))
       (message "starting gnuserv")
       (gnuserv-start)
       (start-process "gnuserv-keepalive" nil "gnuserv-keepalive")
       ))

; XXX/demmer fixme
(defvar longlines-mode nil)

;; load in other files
(load "modes")
(load "mjdkeys")
(load "mwheel")
(load "added")

;; lcvs setup
(load "lcvs")
(defun my-lcvs-setup ()
  (set-face-foreground 'lcvs-UP-face "yellow")
  (setq lcvs-log-restrict-to-branch t)
)
(add-hook 'lcvs-mode-hook 'my-lcvs-setup)

;; dsvn setup
(load "dsvn")
(defun my-dsvn-setup ()
  (set-face-foreground 'dsvn-UP-face "yellow")
)
(add-hook 'dsvn-mode-hook 'my-dsvn-setup)

(defun dsvn-lcvs-examine-get-args ()
  (list (expand-file-name
	 (file-name-as-directory
	  (lcvs-read-directory-name (format "CVS/SVN examine directory: ")
				    lcvs-last-dir lcvs-last-dir t))
	 current-prefix-arg)))

(defun lcvs-or-dsvn-examine (dir)
  "Run either lcvs-examine or svn-examine based on whether there's a
CVS or a .svn subdirectory in the named dir"
  (interactive (dsvn-lcvs-examine-get-args))
  (if (file-exists-p (format "%s/CVS" dir))
      (lcvs-examine dir)
    (dsvn-examine dir)))

(require 'lvc)

;; I should probably have a better place for this...
(setq diff-switches (list "-u"))

;; require these features...
(require 'complete)
(partial-completion-mode)
(nconc completion-ignored-extensions '(".T"))

(require 'compile)
(setq compile-command '"make")
(setq compilation-scroll-output t)

;; host specific support
(let ((host-elisp (format "%s/elisp/%s.el" *HOME* (getenv "HOST"))))
  (if (file-readable-p host-elisp)
      (load-file host-elisp)))
    
;;; stuff that, for one reason or another, must come last
(setq gc-cons-threshold 200000)
(setq default-major-mode 'text-mode)


 (defun grope (sym)
   (interactive
    (list (read-from-minibuffer "Grope for: "
				(current-word) nil nil 'grope-history)))
   
   (let* ((compilation-process-setup-function 'grep-process-setup))
     (compile-internal (concat "grope \"" sym "\"")
		       "No more grope hits" "grope"
		       nil grep-regexp-alist)))

;; clear the last message
(message "")

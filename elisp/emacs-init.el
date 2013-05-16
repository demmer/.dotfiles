;;;
;;; elisp configuration file
;;; compiled from amd and other sources
;;;

;;; turn off audio bell
(setq visible-bell t)

;;; disable the annoying ctrl-Z mapping
(global-unset-key "\^Z")

;;; wrap too-long lines in half windows
(setq truncate-partial-width-windows nil)

;;; Don't open new windows when displaying buffers
(setq ns-pop-up-frames nil)

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
			(width . 100)
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

;; start the server
; (server-start)

(setq exec-path
      '(
	"/usr/local/bin"
	"/bin"
	"/usr/bin"
	))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; load in other files
(load "modes")
(load "mjdkeys")
(load "added")
(load "grope")

;; lcvs setup
(load "lcvs")
(defun my-lcvs-setup ()
  (set-face-foreground 'lcvs-UP-face "yellow")
  (setq lcvs-log-restrict-to-branch t)
)
(add-hook 'lcvs-mode-hook 'my-lcvs-setup)

(require 'lvc)
(setq lvc-svn-remember-revision-from-last-ustatus nil)

;; I should probably have a better place for this...
(setq diff-switches (list "-u"))

;; require these features...
(require 'complete)
(partial-completion-mode)

(require 'compile)
(setq compilation-scroll-output t)

(require 'git)
(require 'git-blame)

;; host specific support
(let ((host-elisp (format "%s/elisp/%s.el" *HOME* (getenv "HOST"))))
  (if (file-readable-p host-elisp)
      (load-file host-elisp)))
    
;;; stuff that, for one reason or another, must come last
(setq gc-cons-threshold 200000)
(setq default-major-mode 'text-mode)

;; clear the last message
(message "")

;;;
;;; elisp configuration file
;;; compiled from amd and other sources
;;;

(defun memequal (el list)
  "[Jak] Returns non-nil if ELT is an element of LIST.  Comparison
done with EQUAL.  The value is actually the tail of LIST whose car is ELT."
  (let ((res nil))
    (while list
      (if (equal el (car list))
          (progn
            (setq res (cdr list))
            (setq list nil)))
      (setq list (cdr list)))
    res))

(defmacro append-no-dup (el list)
  "[Jak] Macro: adds EL to the beginning of LIST if it is not already there."
  (`(if (not (memequal (, el) (, list)))
       (setq (, list) (append (, list) (list (, el)))))))

(defmacro prepend-no-dup (el list)
  "[Jak] Macro: adds EL to the beginning of LIST if it is not already there."
  (`(if (not (memequal (, el) (, list)))
       (setq (, list) (cons (, el) (, list))))))

;;; Set up our load path
(prepend-no-dup (format "%s/elisp" *HOME*) load-path)
(prepend-no-dup (format "%s/elisp/tera-os-lisp" *HOME*) load-path)
;(prepend-no-dup "/usr/Tera/lib/gnuemacs/lisp/" load-path)
;(append-no-dup (format "%s/elisp/w3" *HOME*) load-path)

;;; turn off audio bell
(setq visible-bell t)

;;; wrap too-long lines in half windows
(setq truncate-partial-width-windows nil)

;;; some defaults
(setq text-mode-hook 'turn-on-auto-fill)
(setq line-number-mode t)
(setq delete-auto-save-files t)

(setq require-final-newline 'ask)
(setq compile-command '"make")

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
(scroll-bar-mode -1)
(cond (window-system
       ;; cool fontification always
       (global-font-lock-mode t)
       (load "func-menu")
       (transient-mark-mode 1)
       (set-face-foreground 'region "ivory")
       (set-face-background 'region "royal blue")))


;;; initial frame appearance (geometry overridden by X)
(setq initial-frame-alist  (append ;(x-parse-geometry "=80X40+1154+85")
				   '((auto-raise . nil)
				     (menu-bar-lines . 1)
				     (horizontal-scroll-bars . nil)
				     (vertical-scroll-bars . t)
				     )
				   initial-frame-alist))

;;; other frame appearance
(setq default-frame-alist (append ;(x-parse-geometry "80x40+1540+85")
				   '((auto-raise . nil)
				     (menu-bar-lines . 1)
				     (horizontal-scroll-bars . nil)
				     (vertical-scroll-bars . nil)
				     )
				  default-frame-alist))

;; timestamp customizations
(setq time-stamp-start "[Mm]odified:[     ]+\\\\?[\"<]+")
(setq time-stamp-format "%b %d, %y %I:%M%p by %u")
;; (append-no-dup 'time-stamp write-file-hooks)

;; load in other files
(load "modes")
(load "comment")
(load "added")
(load "mjdkeys")
(load "tera-added")
;; require these features...
(require 'complete)
; (require 'crypt++)
; (require 'zwrite)

;;; stuff that, for one reason or another, must come last
(setq gc-cons-threshold 200000)
(setq default-major-mode 'text-mode)

;; clear the last message
(message "")

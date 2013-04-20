;; .emacs
;;
;; Nothing is kept here except bootstrapping since it is faster to
;; load a compiled file.

(message "Loading .emacs...")

(defvar *HOME* (getenv "EMACSHOME"))
(if (null *HOME*)
    (setq *HOME* (getenv "HOME")))

(if (string-equal window-system "ns")
    (progn
      (setq initial-frame-alist '((foreground-color . "white")
				  (background-color . "black")
				  (cursor-color . "yellow")
				  ))
      (set-foreground-color "white")
      (set-background-color "black")
      (set-cursor-color "yellow")
      (set-variable 'ns-command-modifier (quote meta))
      )
  )

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))

(setq which-emacs (cond
		    ((string-match "^19" emacs-version) "emacs-19")
		    ((string-match "^20" emacs-version) "emacs-20")
		    ((string-match "^21" emacs-version) "emacs-21")
		    ((string-match "^22" emacs-version) "emacs-22")
		    (t "emacs")
		    )
      )

(setq load-path (cons (format "%s/elisp" *HOME*) load-path))

;; XXX/demmer no more version-specific initialization
;; (load (format "%s-init" which-emacs))
(load "emacs-init")

(message "Loading .emacs... done.")
(message "")

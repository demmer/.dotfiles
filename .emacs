;; .emacs
;;
;; Nothing is kept here except bootstrapping since it is faster to
;; load a compiled file.

(message "Loading .emacs...")

(setq *HOME* (getenv "HOME"))
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))

(setq which-emacs (cond
;		    ((string-match "XEmacs" emacs-version) "xemacs")
		    ((string-match "^19" emacs-version) "emacs-19")
		    ((string-match "^20" emacs-version) "emacs-20")
		    ((string-match "^21" emacs-version) "emacs-21")
		    (t "emacs")
		    )
      )

(setq load-path (cons (format "%s/elisp" *HOME*) load-path))
(load (format "%s-init" which-emacs))

(message "Loading .emacs... done.")
(message "")
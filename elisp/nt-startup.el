;;
;; nt-startup.el
;; do all the initialization stuff that's normally in my .emacs

(message "Loading nt-startup...")

(setq *HOME* "C:")

(setq default-frame-alist '((foreground-color . "white")
			    (background-color . "black")
			    (cursor-color . "yellow")
			    ))
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "grey")

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message (getenv "USER"))

(setq which-emacs (cond
                    ((string-match "^19" emacs-version) "emacs-19")
                    ((string-match "^20" emacs-version) "emacs-20")
                    ((string-match "^21" emacs-version) "emacs-21")
                    (t "emacs")
                    )
      )

(setq load-path (cons (format "%s/elisp" *HOME*) load-path))
(message (format "load-path: %s" load-path))
(load (format "%s-init" which-emacs))

(message "Loading nt-startup... done.")
(message "")


;(load "/usr/local/lib/xemacs-19.15/lisp/packages/gnuserv.el")
;(setq server-kill-quietly t)
;(server-start)
;(setq gnuserv-frame (selected-frame))	; comment to get new frame per edit

(defun gnuserv-select-frame ()
  "Select current frame for gnu server"
  (interactive)
  (setq gnuserv-frame (selected-frame))
  (message "current frame selected for gnuserv")
  )

(defun server-finish ()
   "Finish server buffer, kill it, and get back to where you were."
   (interactive)
   (let ((oldbuf (buffer-name))
	 (file (buffer-file-name)))
     (server-edit)
     (setq file (concat file "~"))
     (if (file-exists-p file)
	 (delete-file file)))
   )

(load "utilities")

; (load-library "setkeys.el")
(require 'compile)
;(load-library "compile.el")
;(load-library "os-tools.el")
;(load "tags.el")
; (load-library "hilit19.el")
;(load "~susan/emacs/stig-paren.el")	; highlight matching braces
; (load-library "paren.el")
;(load-library "/home/src/local/gnuemacs/emacs-19.25/lib/emacs/19.25/lisp/compile.el")


(autoload 'hide-mode "comments" nil t)
(autoload 'c++-mode "cc-mode" nil t)
(autoload 'hide-mode "comments")
(autoload 'ring-save-text "ring")
(autoload 'web-mode "web" nil t)
(autoload 'web-start-module "os-tools" nil t)
(autoload 'rk-auto-visit-file "visit" nil t)
;(autoload 'get-tags-table "browse" nil t)
(autoload 'webfile-header-insert "os-tools" nil t)
(autoload 'auto-re-search "search" nil t)
(autoload 'auto-query-replace-regexp "search" nil t)
(autoload 'occur "search" nil t)
(autoload 'previous-complex-command "simple" nil t)
(autoload 'next-complex-command "simple" nil t)

(defun webnum () "run webnum on buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "webnum" t))

(defun rm-filelist-comments ()
  (interactive)
  (while (re-search-forward "\#[^\n]*\n" nil t)
    (replace-match "" nil nil))
  )

(defun disassembly-cleanup ()
  (interactive)
  (auto-fill-mode nil)
  (let (pt (point))
    (beginning-of-buffer)
    (while (re-search-forward "JUMP\\|SKIP" nil t)
      (end-of-line)
      (newline)
      )
    (beginning-of-buffer)
    (while (re-search-forward "LABEL" nil t)
      (beginning-of-line)
      (newline)(newline)(newline)
      (end-of-line)
      (newline)(newline)(newline)
    )))


(defun disassembly-cleanup ()
  (interactive)
  (message "fuck")
  (re-search-forward "JUMP\\|SKIP"))

; tdb
(autoload 'tdb "tdb.elc" "grand unified debugging mode with tdb ext"  t)
(autoload 'gdb "gud" "grand unified debugging mode with tdb ext"  t)

  

;
; file: mailonly.el
; author: miked
;
; Quick and dirty elisp file, with just the basics for email

(defmacro prepend-no-dup (el list)
  "Macro: adds EL to the beginning of LIST if it is not already there."
  (`(if (not (memequal (, el) (, list))) 
	(setq (, list) (cons (, el) (, list))))))

; load in keybindings
(setq which-emacs "emacs-19")
(load-file "~/elisp/mjdkeys.elc")

; i don't want exit confirmation, and always save buffers
(global-set-key "\C-x\C-c" (lambda () (interactive) (save-buffers-kill-emacs t)))

; I like this function
(defun line-to-top-of-window nil
  "Move the line the cursor is on to the top of the current window"
  (interactive)
  (recenter 0))

; stuff for better text mode
(defun my-text-setup ()
  (interactive)
;  (define-key indented-text-mode-map "" 'delete-backward-char)
  (define-key text-mode-map "\C-m" 'newline)
;  (define-key indented-text-mode-map "\t" 'tab-to-tab-stop)
  (auto-fill-mode 1)
  )
(add-hook 'text-mode-hook 'my-text-setup)

; quick elm hack to get it to text mode
(setq auto-mode-alist (append
		       '(("snd.[0-9]+\\'" . text-mode))
		       auto-mode-alist))
; and one for mutt
(setq auto-mode-alist (append
		       '(("mutt-[a-z]*-[0-9]+-[0-9]+\\'" . text-mode))
		       auto-mode-alist))
(setq dabbrev-case-fold-search nil
      dabbrev-case-replace nil)

(text-mode)
(setq major-mode 'text-mode)

(recenter) 

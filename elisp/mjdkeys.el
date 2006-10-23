;;;
;;; Emacs Keybindings - mjd taken from amd
;;; 

(global-set-key [(meta left)] 'backward-word)
(global-set-key [(meta right)] 'forward-word)

;;; numeric keypad for movement - havent yet figured out xemacs syntax..

(global-set-key [C-kp-left] 'beginning-of-line)
(global-set-key [C-kp-right] 'end-of-line)
(global-set-key [C-kp-up] 'backward-line)
(global-set-key [C-kp-down] 'forward-line)
(global-set-key [M-kp-left] 'backward-word)
(global-set-key [M-kp-right] 'forward-word)

;;; numeric keypad for home, pgup, etc keys
(global-set-key [(f27)] 'beginning-of-line)
(global-set-key [(f29)] 'scroll-down)
(global-set-key [(f31)] 'recenter)
(global-set-key [(f33)] 'end-of-line)
(global-set-key [(f35)] 'scroll-up)



;;; amd: home=start, end = end
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [begin] 'recenter)

;;; amd: fix backspace
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-\C-h" 'backward-kill-word)

;;; i mistype the help commands a lot
(global-set-key "\C-xh" 'help-command)
(global-set-key "\C-X\C-h" 'help-command)
(global-set-key "\C-X\C-h\C-f" 'describe-function)
(global-set-key "\C-X\C-h\C-k" 'describe-key)
(global-set-key "\C-X\C-h\C-v" 'describe-variable)

;;; amd: my tab shortcuts
(global-set-key "\M-\\" 'indent-region)

; amd: control movement keys
(global-set-key [(control left)] 'beginning-of-line)
(global-set-key [(control right)] 'end-of-line)
(global-set-key [(control up)] 'backward-line)
(global-set-key [(control down)] 'forward-line)

(global-set-key [C-down-mouse-3] 'function-menu)
(global-set-key [C-down-mouse-2] 'function-menu)

; amd: I make find-file mistakes a lot
(global-set-key "\C-xf" 'find-file) 
(global-set-key "\C-x\C-f" 'find-file)

; mjd: i also make write-file mistakes
(global-set-key "\C-xw" 'write-file)

(global-set-key "\C-m" 'newline)
(global-set-key "\C-xk" 'kill-buffer)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-xS" 'run-scheme)

; amd: these are the only ones I actually use
(global-set-key [(f11)] 'compile)
(global-set-key [(f12)] 'repeat-complex-command)
(global-set-key [(f14)] 'advertised-undo)
(global-set-key [(f19)] 'next-error)
(global-set-key [(f20)] 'goto-line)

;;; mjd: switch-CH-buffer I like as M-SPC and FRONT
(global-set-key "\M- " 'switch-CH-buffer)
(global-set-key [(f15)] 'switch-CH-buffer)

;;; mjd: my handy settings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-u" 'advertised-undo)
(global-set-key "\M-\t" 'dabbrev-expand)
(global-set-key "\M-r" 'revert-buffer-at-point)
(global-set-key "\M-1" 'line-to-top-of-window)
(global-set-key "\^[!" 'line-to-top-of-window)
(global-set-key [(control return)] 'gnuserv-select-frame)
(global-set-key "\C-x\C-x" 'pop-global-mark)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\M-a" 'auto-fill-mode)
(global-set-key "\M-p" 'vc-print-log)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)
(global-set-key "\M-\C-l" 'lcvs-or-dsvn-examine)
(global-set-key "\C-l" 'lcvs-or-dsvn-examine)
(global-set-key "\C-xO" 'other-window-only)
(global-set-key "\M-?" 'grope)
(global-set-key "\M-'" 'grope-replace)
(global-set-key "\M-;" 'visit-tags-table)
(global-set-key "\M-," (lambda () (interactive) (find-tag "" t)))
(global-set-key "\C-x\C-c" (lambda () (interactive)
			     (if (> (length (frame-list)) 1)
				 (if (y-or-n-p "Really close frame? ")
				     (delete-frame))
			       (if (y-or-n-p "Really quit? ")
				   (save-buffers-kill-emacs)))))
(global-set-key "\C-x " 'next-error)
(global-set-key "\M-m" 'man)
(global-set-key "\M-\-" 'add-c++-function-separator)

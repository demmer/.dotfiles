;; file: zwrite.el
;; author: Michael Demmer (mjd)
;;
;; This file provides the capability to send zephyr messages from
;; within emacs. Basically the command M-x zwrite will split the
;; current window and replicate the zwrite command from the shell
;; (i.e. Control-D sends, Control-C cancels). Note it does not allow
;; for a dot on a line by itself to send the message.

(message "Loading zwrite...")
(defvar zwrite-target)
(defvar zwrite-proc "zwrite-proc")
(defvar zwrite-map nil "")
(setq zwrite-map (make-sparse-keymap))
(define-key zwrite-map "\C-d" 'zwrite-send)
(define-key zwrite-map "\C-c" 'zwrite-abort)

(defun zwrite ()  
  "[mjd] Sends zwrite messages. First prompts from the minibuffer for
the target to send to, then splits the current window allowing the
message to be typed. Then behavior works as it does from the shell
command line."
  (interactive)
  (setq zwrite-target (read-string "zwrite "))
  (split-window-vertically -10)
  (switch-to-buffer-other-window "*zwrite*")
  (use-local-map zwrite-map)
  (insert "Type your message below. Control-d sends, Control-c aborts.\n")
  (message "C-d to send, C-c to abort")
  )

(defun zwrite-filter (process string)
  (message (substring string 0 (- (length string) 1)))
  )

(defun zwrite-send ()
  "[mjd] Sends the zwrite message. Switches to the buffer called
*zwrite* containing the zwrite message, grabs the string and then runs
the zwrite process."
  (interactive)
  (set-buffer "*zwrite*")
  (beginning-of-buffer)
  (forward-line 1)
  (let ((message (buffer-substring (point) (- (point-max) 1))))
    (setq zwrite-proc "zwrite-proc")
    (let ((zproc (start-process zwrite-proc
						  nil
						  "zwrite"
						  zwrite-target
						  "-m"
						  message )))
	 (set-process-filter zproc 'zwrite-filter) )
    )
  (zwrite-close)
  )

(defun zwrite-abort ()
  "[mjd] Cancels the current zwrite message."
  (interactive)
  (zwrite-close)
  (message "zwrite cancelled")
)

(defun zwrite-close()
  (use-local-map nil)
  (kill-buffer nil)
  (other-window -1)
  (delete-other-windows)
)  

(provide 'zwrite)
(message "Loading zwrite...done")

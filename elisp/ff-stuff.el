;;
;; FastForward / Inktomi specific emacs hooks
;;

;; I don't use zenirc any more, plus I'd need the elisp in site-lisp
;; which I don't have
; (load "zenirc")
; (defun make-zenirc-frame () 
;   (interactive)
;   (let ((ctl-frame 
; 	 (make-frame (list '(width . 112) '(height . 14) '(minibuffer . nil))))
; 	)
;     (modify-frame-parameters ctl-frame (list '(left . 1) 
; 					     '(top . 1015) 
; 					     '(menu-bar-lines . 0)
; 					     '(title . "Zenirc")
; 					     ))
;     (other-frame 1)
;     (zenirc)
;     (switch-to-buffer "*zenirc*")
;     (insert "/join #ff")
;     (zenirc-send-line)
;     (delete-other-windows)
;     (other-frame -1)
;     ))

(let ((checkstcl "~/work/am-1/checkstcl/checkstcl.el"))
  (if (file-exists-p checkstcl)
      (load checkstcl)))
(setq checkstcl-unique-buffer-name t)

(defun am-1-dir ()
  (interactive)
  (if (string-match "\\(.*\\)am-1.*" buffer-file-name)
      (let* ((base(substring buffer-file-name (match-beginning 1) (match-end 1)))
	    (dir1 (format "%sam-1/" base))
	    (dir2 (format "%s%s/am-1/" base (getenv "ARCH"))))

	(cond
	 ((file-readable-p (format "%sbin/amsh" dir1)) dir1)
	 ((file-readable-p (format "%sbin/amsh" dir2)) dir2)
	 (t nil)))
    nil))
  
(defun pump-dir ()
  (interactive)
  (if (string-match "\\(.*\\)pump.*" buffer-file-name)
      (let ((dir
	    (format "%s%s/pump"
		    (substring buffer-file-name 
			       (match-beginning 1) (match-end 1))
		    (getenv "ARCH"))))
	(if (file-directory-p dir) dir nil))
    nil))
  

;; Try to make the compile command do the right thing for amsh
(defun amsh-compile-command-hook ()
  (interactive)
  (let ((amdir	 (am-1-dir))
	(pumpdir (pump-dir))
	(dir)
	)
    (if (null amdir) (setq dir pumpdir) (setq dir amdir))
    (if (null dir) nil
      (make-local-variable 'compile-command)
      (setq compile-command (format "cd %s; make" dir))
      )))

(add-hook 'c-mode-common-hook 'amsh-compile-command-hook)
(add-hook 'makefile-mode-hook 'amsh-compile-command-hook)

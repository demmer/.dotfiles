
(defvar narrow-to-module nil "*This variable determines whether the 
buffer should be narrowed to the current web module after a
goto-web-module command")

(defun c++-web-hook ()
  (interactive)
  (modify-syntax-entry ?' "w" c++-mode-syntax-table)
  
;  (modify-syntax-entry ?@  ". 13" c++-mode-syntax-table)
;  (modify-syntax-entry ?%  ". 2" c++-mode-syntax-table)
;  (modify-syntax-entry ?1  "w 2b" c++-mode-syntax-table)
;  (modify-syntax-entry ?2  "w 2b" c++-mode-syntax-table)
;  (modify-syntax-entry ?3  "w 2b" c++-mode-syntax-table)
;  (modify-syntax-entry ?4  "w 2b" c++-mode-syntax-table)
;  (modify-syntax-entry ?t  "w 2b" c++-mode-syntax-table)

;  (modify-syntax-entry ?c  "w 4 b2" c++-mode-syntax-table)
  
)

(defun web-mode () (interactive)
  (add-hook 'c++-mode-hook 'c++-web-hook)
  (c++-mode)
  (modify-syntax-entry ?: "_" c++-mode-syntax-table) 
  (local-set-key "$" 'self-insert-command)
;  (local-set-key "%" 'self-insert-command)
;  (local-set-key "#" 'self-insert-command)
  (local-set-key "&" 'self-insert-command)
  (local-set-key "<" 'self-insert-command)
  (local-set-key ">" 'self-insert-command)
  (local-set-key "=" 'self-insert-command)
  (local-set-key "_" 'self-insert-command)

  (local-set-key "n" 'next-web-module)
  (local-set-key "p" 'previous-web-module)
;  (local-set-key "g" 'goto-web-module)  
  (local-set-key "s" 'web-bold-word)
  (global-set-key "\^Ct" 'my-latex-mode)
  (global-set-key "\^Cw" 'web-mode)

  (local-set-key "\^C\"" 'TeX-insert-quote)
;  (auto-fill-mode 1)
;  (setq paragraph-separate "^\s-*$\\|^@\\|^$\\|%")
  (setq paragraph-separate "^-*$\\|^@\\|^$\\|%")
  (setq paragraph-start paragraph-separate)
  )


(defun web-renumber (num) (interactive "*p")
  (save-excursion
    (let (modnum)
      (goto-char (point-min))
      (while (re-search-forward "^@\\([0-9]+\\)" nil t)
	(setq modnum (string-to-int
		      (buffer-substring (match-beginning 1) (match-end 1))))
	(if (and (= num 1) (> modnum num))
	    (setq num modnum))
	(if (not (= num modnum))
	    (progn (kill-region (match-beginning 0) (match-end 0))
		   (insert "@" (int-to-string num) " ")))
	(setq num (+ num 1))))))


;; Return a default tag to search for, based on the text at point.
(defun find-tag-default-web ()
  (save-excursion
    (let ((loc (point))
	  (end (progn (end-of-line) (point)))
	  result)
      (beginning-of-line)
      (while (and (re-search-forward "@[^@]+@" end t)
		  (<= loc (match-end 0))))
      (if (and (re-search-backward "@\\([^@]+\\)@" nil t)
	       (<= (match-beginning 0) loc)
	       (<= loc (match-end 0)))
	  (let ((all (buffer-substring (match-beginning 1)
				       (match-end 1))))
	    (if (looking-at "@\\([^@]+\\):\\([0-9]+\\)@")
		(buffer-substring (match-beginning 1) (match-end 1))
	      all))))))

(defun web-convert-tag (web)
  (if web
      (if (not (= 0 (string-to-int web)))
	  (concat "@" web) ;
	(if (not (string-equal "note:" (substring web 0 5)))
	    (concat "@code " web)
	  web))))

(defun find-tag-default ()
  (let ((web  (find-tag-default-web)))
    (if web (web-convert-tag web)
      (find-tag-default-old))))

(defun find-tag-default-old ()
  (save-excursion    
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
	(progn (forward-char 1)
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun recenter-web-module () (interactive)
  (let ((loc (point)))
    (if (or (looking-at "^@[0-9]")
	    (re-search-backward "^@[0-9]" nil t))
	(progn
	  (recenter 0)
	  (goto-char loc)))))

(defun narrow-to-web-module () (interactive)
  (save-excursion
    (if (or (looking-at "^@[0-9]")
	    (re-search-backward "^@[0-9]" nil t))
	(let ((start (point)))
	  (next-line 1)
	  (re-search-forward "^@[0-9]\\|^@section" nil 1)
	  (if (not (eobp)) (beginning-of-line))
	  (narrow-to-region start (point)))
      (error "can't find beginning of module"))))


(defun display-web-module  (start end)
  (goto-char start)
  (recenter 0)
  (if narrow-to-module
      (narrow-to-region start end)))

(defun next-web-module () (interactive)
  (let ((start (point-min))
	(stop (point-max))
	(init (point)))    
    (widen)
    (if (looking-at "^@[0-9]") (next-line 1))
    (if (not (re-search-forward "^@[0-9]" nil t))
	(progn
	  (narrow-to-region start stop)
	  (goto-char init)
	  (error "No next module")))
    (beginning-of-line)
    (setq start (point))
    (next-line 1)
    (re-search-forward "^@[0-9]\\|^@section" nil 1)
    (if (not (eobp)) (beginning-of-line))
    (display-web-module start (point))))

(defun previous-web-module () (interactive)
  (let ((start (point-min))
	(stop (point-max))
	(init (point))
	new-start)    
    (widen)
    (if (not (looking-at "^@[0-9]"))
	(if (not (re-search-backward "^@[0-9]" nil t))
	    (progn
	      (narrow-to-region start stop)
	      (goto-char init)
	      (error "No previous module"))))
    (setq new-end (point))
    (if (not (re-search-backward "^@[0-9]" nil t))
	    (progn
	      (narrow-to-region start stop)
	      (goto-char init)
	      (error "No previous module")))
    (display-web-module (point) new-end)))

(defun goto-web-module () (interactive)
  (let ((tag (web-get-reference)))
    (if tag
	(progn
	  (find-tag tag)
	  (if narrow-to-module
	      (narrow-to-web-module)
	    (recenter-web-module))
	  (search-forward tag)))))


(defun web-get-reference () (interactive)
  (let ((completion-ignore-case t)
	 start end completions str default)
     (save-excursion
       (if (not (looking-at "^@[0-9]"))
	   (re-search-backward "^@[0-9]" nil 1))
       (setq start (point))
       (if (looking-at "^@[0-9]") (next-line 1))	  
       (re-search-forward "^@[0-9]\\||@section" nil 1)
       (setq end (point))
       (goto-char start)
       (while (re-search-forward "@\\([^@\n]+\\)@" end t)
	 (setq str (buffer-substring 
		    (match-beginning 1)
		    (match-end 1)))
	 (if (string-match "\\(.*\\)\\s-:[0-9]+" str)
	     (setq str (substring str
			(match-beginning 1)
			(match-end 1))))
	 (setq completions
	       (cons (cons str nil)
		     completions))))
     (setq default (find-tag-default-web))
     (let ((tag 
	    (completing-read "Goto module: " completions nil nil default)))
       (web-convert-tag tag))))

(defun web-bold-word nil (interactive)
  (forward-char 1)
  (backward-word 1)
  (insert-string "|")
  (forward-word 1)
  (insert-string "|"))

(defun my-web-startup ()
  (require 'tex-mode)
  (require 'tex-stuff)
  (require 'latex)
  (require 'browse)
  )

(setq web-mode-hook 'my-web-startup)

(autoload 'insert-tera-header "os-tools" nil t)


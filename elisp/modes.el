;;;;;;;;;;;;;;;;;;;;;;;;; mode defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; figure out the right script mode based on the magic string
;;
(defun determine-script-mode ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "#!")
	(progn
	  (cond
	   ((looking-at "#![ \n\t]*/[^ \n\t]*perl")  (perl-mode))
	   ((looking-at "#![ \n\t]*/[^ \n\t]*tclsh") (tcl-mode))
	   ((looking-at "#![ \n\t]*/[^ \n\t]*make")  (makefile-mode))
	   ((looking-at "#![ \n\t]*/[^ \n\t]*awk")   (awk-mode))
	   )
	  t)
      nil)
    ))

(add-hook 'find-file-hooks 'determine-script-mode)

;;
;; make sure script files are executable
;;
(defun determine-and-set-executable ()
  (interactive)
  (let ((file (buffer-file-name))
	(script-p (determine-script-mode)))
    (if (and script-p
	     (file-executable-p "/bin/chmod")
	     (not (file-executable-p (buffer-file-name))))
	(progn
	  (shell-command (format "/bin/chmod +x %s" file))
	  (message (format "Wrote %s (and added exec permissions)" file))))
    ))

(add-hook 'after-save-hook 'determine-and-set-executable)

;;; Setup text mode

(defun my-text-setup ()
  (interactive)
  (define-key text-mode-map "\C-m" 'newline)
  (auto-fill-mode 1)
  )
(add-hook 'text-mode-hook 'my-text-setup)

;;; Setup c/c++/java mode
(require 'cc-mode)
(require 'cc-vars)
(require 'dabbrev)

(defun my-c-common-setup ()
  (interactive)
  (setq c-tab-always-indent t)			; indent wherever cursor is
  (setq dabbrev-case-fold-search nil		; don't ignore case for dabbrev
	dabbrev-case-replace nil)
  (setq c-electric-pound-behavior '(alignleft))	; use electric pound
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (define-key c++-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key java-mode-map "\C-m" 'newline-and-indent)
  (define-key java-mode-map "\C-c\C-u" 'uncomment-region)
  )

(add-hook 'c-mode-common-hook 'my-c-common-setup)

(defconst my-c-style
  '((c-basic-offset		. 4)
    (c-hanging-comment-ender-p	. nil)
    (c-offsets-alist
     . ((substatement-open	. 0)		;don't indent braces!
	(inline-open		. 0)		;don't indent braces, please.
	(label 			. -1000)	;flush labels left
	(statement-cont		. c-lineup-math)))))

(defun c-indent-one-tab()
  (interactive)
  (setq c-basic-offset 8)
  (setq indent-tabs-mode t)
  )

(defun c-indent-two-spaces()
  (interactive)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  )

(defvar my-c-style-overrides)
(setq my-c-style-overrides
      '(("~/work/.*DTN" c-indent-one-tab)
	("~/work/nesc" c-indent-two-spaces)
	)
      )

(defun apply-c-style-overrides ()
  (interactive)
  (let ((buffer (expand-file-name (buffer-file-name))))
    (mapcar
     (lambda (pair) "" nil
       (let ((path (expand-file-name (car pair)))
	     (func (cadr pair)))
	 (if (string-match (concat "^" path ".*") buffer)
	     (funcall func)
	   )))
     my-c-style-overrides)
  ))

(defun my-c-setup()
  (interactive)
  (c-add-style "my-c-style" my-c-style t)	; my style above
  (setq indent-tabs-mode nil)	           	; use spaces for tabs
  (apply-c-style-overrides)
  )

(add-hook 'c-mode-hook 'my-c-setup)
(add-hook 'c++-mode-hook 'my-c-setup)

(defun my-java-setup()
  (interactive)
  (my-c-setup)
  (c-indent-two-spaces)
  )

(add-hook 'java-mode-hook 'my-java-setup)

; load visual-basic mode
(require 'visual-basic-mode)

; and nesc mode
(require 'nesc-mode)

; setup my auto modes alist
(setq auto-mode-alist (append '(("\\.pl\\'" . perl-mode)
				("\\.C\\'" . c++-mode)
				("\\.[Hh]\\'" . c++-mode)
				("\\.c[cx]?x?\\'" . c++-mode)
				("\\.nc\\'" . nesc-mode)
				("\\.y\\'" . c++-mode)
			        ("\\.w\\'" . web-mode)
				("\\.zsh\\'" . shell-script-mode)
				("\\.[A-Za-z0-9]*rc" . shell-script-mode)
				("\\GNUmakerules\\'" . makefile-mode)
				("\\.[12345678]\\'" . text-mode)
				("\\.mdn\\'" . sgml-mode)
				("\\.xsl\\'" . sgml-mode)
				("\\.cls\\'" . visual-basic-mode)
				("\\.bas\\'" . visual-basic-mode)
				("\\.frm\\'" . visual-basic-mode)
				)
			      auto-mode-alist))

;;; Setup perl mode
(require 'perl-mode)
(defun my-perl-setup ()
  (interactive)
  (define-key perl-mode-map "\C-m" 'newline-and-indent)
)

(add-hook 'perl-mode-hook 'my-perl-setup)

;; Setup tcl mode
(require 'tcl-mode)
(defun my-tcl-setup ()
  (interactive)
  (define-key tcl-mode-map "\C-m" 'newline-and-indent)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$"))
  )
(add-hook 'tcl-mode-hook 'my-tcl-setup)

;;; Setup lisp mode

(defun my-lisp-setup ()
  (interactive)
  (define-key lisp-mode-map "" 'delete-backward-char)
  (define-key lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key lisp-mode-map "\M-\C-m" 'indent-new-comment-line)
)

(add-hook 'lisp-mode-hook 'my-lisp-setup)

(defun my-elisp-setup ()
  (interactive)
  (define-key emacs-lisp-mode-map "" 'delete-backward-char)
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key emacs-lisp-mode-map "\M-\C-m" 'indent-new-comment-line)
)

(add-hook 'emacs-lisp-mode-hook 'my-elisp-setup)


;; setup sgml mode
(require 'sgml-mode)
(defun my-sgml-setup() (interactive) (auto-fill-mode 0))
(add-hook 'sgml-mode-hook 'my-sgml-setup)
			 
;; suck in sgml-indent
(require 'sgml-indent)

;; setup html mode
(defun html-insert-lt ()(interactive)(insert "&lt;"))
(defun html-insert-gt ()(interactive)(insert "&gt;"))
(defun html-insert-nbsp ()(interactive)(insert "&nbsp;"))
(defun html-insert-red ()(interactive)
  (insert "<font color=\"red\"></font>")
  (forward-char -7)
  )

(defun my-html-setup()
  (interactive)
  (define-key html-mode-map "\C-c<" 'html-insert-lt)
  (define-key html-mode-map "\C-c>" 'html-insert-gt)
  (define-key html-mode-map "\C-c " 'html-insert-nbsp)
  (define-key html-mode-map "\C-cr" 'html-insert-red)
  (setq indent-line-function 'indent-to-left-margin)
  )
(add-hook 'html-mode-hook 'my-html-setup)

;; setup sh-mode (shell-script-mode)
(require 'sh-script)
(defun my-sh-setup () 
  (interactive)
  (define-key sh-mode-map "\t" 'self-insert-command)
  )
(add-hook 'sh-mode-hook 'my-sh-setup)

;;; elisp debugging
;; (autoload 'edebug-defun "edebug" "debugger for elisp" t)
;; (autoload 'edebug-all-defuns "edebug" "debugger for elisp" t)
;; (setq edebug-global-prefix "\C-xX")
;; (define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)
;; (setq debugger 'edebug-debug)
;; (setq max-lisp-eval-depth 10000)

;; Set up the various faces for hilighting

(cond (window-system
       (set-face-foreground 'bold "MediumPurple1")
       (set-face-foreground 'bold-italic "MediumPurple1")

       (set-face-foreground 'italic "medium spring green")

       (set-face-underline-p 'underline nil)
       (set-face-foreground  'underline "SkyBlue1")
       
       (set-face-background 'highlight "steelblue")
       (set-face-foreground 'highlight "white")
       
       (set-face-background 'secondary-selection "red")
       
       (make-face 'fl-comment-face)
       (if (face-differs-from-default-p 'fl-comment-face) nil
	 (set-face-foreground 'fl-comment-face "medium spring green")
	 )       
       (make-face 'fl-doc-string-face)
       (if (face-differs-from-default-p 'fl-doc-string-face) nil
	 (set-face-foreground 'fl-doc-string-face "LavenderBlush1")
	 )
       (make-face 'fl-string-face)
       (if (face-differs-from-default-p 'fl-string-face) nil
	 (set-face-foreground 'fl-string-face "VioletRed2")
	 )
       (make-face 'fl-function-name-face)
       (if (face-differs-from-default-p 'fl-function-name-face) nil
	 (set-face-foreground 'fl-function-name-face "MediumPurple1")
	 )
       (make-face 'fl-keyword-face)
       (if (face-differs-from-default-p 'fl-keyword-face) nil
	 (set-face-foreground 'fl-keyword-face "RoyalBlue1")
	 )

       (make-face 'fl-type-face)
       (if (face-differs-from-default-p 'fl-type-face) nil
	 (set-face-foreground 'fl-type-face "dodger blue")
	 )
       
       (make-face 'fl-misspelled-face)
       (if (face-differs-from-default-p 'fl-misspelled-face) nil
	 (set-face-underline-p 'fl-misspelled-face t)
	 (set-face-foreground 'fl-misspelled-face "grey87")
	 )
       ))
(require 'font-lock)
(defun my-font-lock-init ()
;  (setq font-lock-java-member-face 'fl-java-member-face)
;  (setq font-lock-java-member-func-face 'fl-java-member-func-face)
;  (setq font-lock-java-mod-face 'fl-java-mod-face)
;  (setq font-lock-java-class-face 'fl-java-class-face)
;  (setq font-lock-doc-string-face 'fl-doc-string-face)
  (setq font-lock-comment-face 'fl-comment-face)
  (setq font-lock-string-face 'fl-string-face)
  (setq font-lock-function-name-face 'fl-function-name-face)
  (setq font-lock-keyword-face 'fl-keyword-face)
  (setq font-lock-type-face 'fl-type-face)
  (setq font-lock-reference-face 'fl-type-face)
  (setq font-lock-builtin-face 'fl-keyword-face)
  (setq font-lock-constant-face 'fl-keyword-face)
  (setq search-highlight t)

  (setq font-lock-maximum-decoration				      
	(list
	 (cons 'c++-mode 2)
	 (cons t t)))
  
  (setq c++-font-lock-extra-types nil)
  )

(add-hook 'font-lock-mode-hook 'my-font-lock-init)
;(require 'lazy-lock)
;(setq font-lock-support-mode 'lazy-lock-mode)

;;; Suck in prefix
;; (require 'prefix)

(defvar termtype nil)
(setq termtype (getenv "TERM"))         ; get our terminal type

; (if (and (or (equal termtype "dialup")  ; if we are running one
;              (equal termtype "vt100")   ;  of these terminals
;              (equal termtype "vt200"))
;          (null (getenv "LAYER")))       ; ... but not layers
;       (enable-flow-control))

; (if (equal termtype "vt320")
;     (progn
;       (enable-flow-control)
;       (load (concat term-file-prefix "vt200") nil t)))

; extensions of files I don't want to open
(setq completion-ignored-extensions (append completion-ignored-extensions
					    '(".class"
					      ".T"
					      ".lo")))
; and ignore case
(setq completion-ignore-case t)

; ediff: use the minibar instead of a separate little window
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)

; vc: remap C-x v a to vc-annotate-at-point
;     add C-x v e to ediff-revision
(require 'vc)
(define-key vc-prefix-map "a" 'vc-annotate-goto-line)
(define-key vc-prefix-map "e" 'ediff-revision)
(define-key vc-prefix-map "s" 'vc-print-status)
(define-key vc-prefix-map "?" 'vc-print-status)
(setq vc-follow-symlinks t)

; fixes so shell-mode works with zsh
(require 'comint)
(require 'rlogin)

(defun shell-mode-newline ()
  (interactive)
  (insert "")
  (comint-send-input)
  )

(defun shell-newline-filter (string)
  (let* ((point-marker (point-marker))
         (end (process-mark (get-buffer-process (current-buffer))))
         (beg (or (and (boundp 'comint-last-output-start)
                       comint-last-output-start)
                  (- end (length string)))))
    (goto-char beg)
    (while (search-forward "\\n" end t)
      (delete-char -2))
    (goto-char point-marker)))

(defun my-shell-mode-init ()
  (add-hook 'comint-output-filter-functions 'shell-newline-filter)
  (add-hook 'comint-output-filter-functions 'rlogin-carriage-filter)
  (define-key shell-mode-map "\C-m" 'shell-mode-newline)
)
(setenv "EMACSPARENT" "1")

; Linux has problems with zsh...
(if (string-equal (getenv "ARCH") "Linux")
    (add-hook 'shell-mode-hook 'my-shell-mode-init)
  )

;; encryption
(require 'crypt++)
(setq crypt-encryption-type 'pgp)
(crypt-rebuild-tables)

;; minibuffer
(resize-minibuffer-mode t)

;; More descriptive names than foo<2>
(require 'uniquify)
(if (featurep 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward
	  uniquify-separator ", "))


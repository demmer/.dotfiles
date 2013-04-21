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

; don't ignore case for dabbrev
(require 'dabbrev)
(setq dabbrev-case-fold-search nil		
      dabbrev-case-replace nil)

;;; Setup c/c++/java mode
(require 'cc-mode)
(require 'cc-vars)
(require 'once-only-header)

(setq ooh-file-license
"/*
 * Copyright 2009 Riverbed Technology, Inc.
 * All Rights Reserved. Confidential.
 */

")

(defun ooh-insert-license ()
  "Insert the ooh-file-license into the current buffer"
  (interactive)
  (insert ooh-file-license)
  )

(defun newline-and-indent-non-blank-lines ()
  "If the current line is only whitespace, remove the whitespace
   before adding a newline"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\[ 	\]+$")
	(delete-region (match-beginning 0) (match-end 0))))
  (newline-and-indent)
)

(defun my-c-common-setup ()
  (interactive)
  (setq c-tab-always-indent t)			; indent wherever cursor is
  (setq c-electric-pound-behavior '(alignleft))	; use electric pound
  (define-key c-mode-map "\C-m" 'newline-and-indent-non-blank-lines)
  (define-key c-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key c-mode-map "\C-c\t" 'c-align-space-in-region)
  (define-key c-mode-map "\C-c " 'c-align-space-in-region-by-space)
  (define-key c-mode-map "\C-c=" 'c-align-space-in-region-by-equal)
  (define-key c-mode-map "\M-q" 'my-fill-paragraph)
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (define-key c++-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key c++-mode-map "\C-c\t" 'c-align-space-in-region)
  (define-key c++-mode-map "\C-c " 'c-align-space-in-region-by-space)
  (define-key c++-mode-map "\C-c=" 'c-align-space-in-region-by-equal)
  (define-key c++-mode-map "\M-q" 'my-fill-paragraph)
  (define-key java-mode-map "\C-m" 'newline-and-indent)
  (define-key java-mode-map "\C-c\C-u" 'uncomment-region)
  (setq comment-use-syntax nil)
  (ooh-maybe-insert-cpp-guard)
  )

(add-hook 'c-mode-common-hook 'my-c-common-setup)

(defconst my-c-style
  '((c-basic-offset		. 4)
    (c-hanging-comment-ender-p	. nil)
    (c-offsets-alist
     . ((substatement-open	. 0)		; don't indent braces!
	(inline-open		. 0)		; don't indent braces, please.
	(label 			. -1000)	; flush labels left
	(statement-cont		. c-lineup-math); line up with = signs
	(innamespace		. 0)		; no indent for namespaces
	(inextern-lang		. 0)		; or extern language
	))))

(defun c-indent-one-tab()
  (interactive)
  (setq c-basic-offset 8)
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  )

(defun c-indent-two-spaces()
  (interactive)
  (setq c-basic-offset 2)
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  )

(defun c-indent-four-spaces()
  (interactive)
  (setq c-basic-offset 4)
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  )

(defun c-indent-line-force-8-space-tabs ()
  "Indents one line after temporarily setting c-basic-offset
and tab-width to 8."
  (interactive)
  (let ((tab-width 8)
	(c-basic-offset 8)
	(indent-tabs-mode t))
    (c-indent-line)
    ))

(defun c-indent-region-force-8-space-tabs (START END &optional QUIET)
  "Indents a region after temporarily setting c-basic-offset and
tab-width to 8."
  (interactive)
  (let ((tab-width 8)
	(c-basic-offset 8)
	(indent-tabs-mode t))
    (c-indent-region START END QUIET)
    ))

(defun c-indent-four-spaces-with-8-space-tabs ()
  "Sets indent-line-function and indent-region-function to make the
basic indentaion appear to be four spaces but actually be 8 spaces
with tab characters underneath."
  (interactive)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-line-function   'c-indent-line-force-8-space-tabs)
  (setq indent-region-function 'c-indent-region-force-8-space-tabs)
)

(defvar my-c-style-overrides)
(setq my-c-style-overrides
      '(("~/work/nesc" 		c-indent-two-spaces)
	("~/work/tinyos-1.x" 	c-indent-two-spaces)
	("~/work/.*capriccio"	c-indent-two-spaces)
	)
      )

(defun apply-c-style-overrides ()
  (interactive)
  (if (buffer-file-name)
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
  )

(defun my-c-setup()
  (interactive)
  (c-add-style "my-c-style" my-c-style t)	; my style above
  (setq indent-tabs-mode nil)	           	; use spaces for tabs
  (apply-c-style-overrides)                     ; per-project overrides
  )

(add-hook 'c-mode-hook 'my-c-setup)
(add-hook 'c++-mode-hook 'my-c-setup)

(defun my-java-setup()
  (interactive)
  (my-c-setup)
  (c-indent-four-spaces)
  )

(add-hook 'java-mode-hook 'my-java-setup)

(require 'python)
(defun my-python-setup()
  (interactive)
  (define-key py-mode-map "\C-c\C-c" 'comment-region)
  (define-key py-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key py-mode-map "\C-c\t" 'c-align-space-in-region)
  (set-variable 'py-pseudo-keyword-face 'fl-keyword-face)
  )

(add-hook 'python-mode-hook 'my-python-setup)


; load visual-basic mode
; (require 'visual-basic-mode)

; and nesc mode
; (require 'nesc-mode)

; and sgml mode
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode  "psgml" "Major mode to edit XML files." t)

; setup my auto modes alist
(setq auto-mode-alist (append '(("\\.pl\\'" . perl-mode)
				("\\.[Hh]\\'" . c++-mode)
				("\\.c[cx]?x?\\'" . c++-mode)
				("\\.[Cyxi]\\'" . c++-mode)
				("\\.tcc\\'" . c++-mode)
				("\\.ice\\'" . c-mode)
				("\\.nc\\'" . nesc-mode)
			        ("\\.w\\'" . web-mode)
				("\\.zsh\\'" . shell-script-mode)
				("\\.z[A-Za-z0-9]*\\'" . shell-script-mode)
				("\\.[A-Za-z0-9]*rc" . shell-script-mode)
				("\\(M\\|m\\|GNUm\\)ake\\(file\\|rules\\)\\(\\..*\\)?\\'" .
				 makefile-mode)
				("\\.make\\(\\|.in\\)\\'" . makefile-mode)
				("\\.[12345678]\\'" . text-mode)
				("\\.xsl\\'" . sgml-mode)
				("\\.\\(cls\\|bas\\|frm\\)\\'" . visual-basic-mode)
				("\\.ac\\'" . autoconf-mode)
				)
			      auto-mode-alist))

;; purge ps-mode from the magic-mode-alist
(require 'cl)
(setq magic-mode-alist
      (mapcar
       (lambda (pair) "" nil
	 (if (not (eq (cdr pair) 'ps-mode))
	     pair))
       magic-mode-alist))
(setq magic-mode-alist (remove-if #'null magic-mode-alist))

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
  (setq indent-tabs-mode nil)
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
  (define-key html-mode-map ">" 'self-insert-command)
  (setq indent-line-function 'indent-to-left-margin)
  )
(add-hook 'html-mode-hook 'my-html-setup)

;; little django mode
(require 'django-mode)


;; setup sh-mode (shell-script-mode)
(require 'sh-script)
(defun my-sh-setup () 
  (interactive)
  (define-key sh-mode-map "\C-c\C-c" 'comment-region) 
  (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
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

(require 'font-lock)

(defun my-font-lock-init ()
  (set-face-foreground 'bold "MediumPurple1")
  (set-face-foreground 'bold-italic "MediumPurple1")

  (set-face-foreground 'italic "medium spring green")

  (set-face-underline-p 'underline nil)
  (set-face-foreground  'underline "SkyBlue1")
       
  (set-face-background 'highlight "steelblue")
  (set-face-foreground 'highlight "white")
       
  (set-face-background 'secondary-selection "red")
       
  (set-face-foreground 'font-lock-comment-face "medium spring green")
  (set-face-foreground 'font-lock-string-face "VioletRed2")
  (set-face-foreground 'font-lock-function-name-face "MediumPurple1")
  (set-face-foreground 'font-lock-keyword-face "RoyalBlue1")
  (set-face-foreground 'font-lock-type-face "dodger blue")

  (setq font-lock-builtin-face 'font-lock-keyword-face)
  (setq font-lock-constant-face 'font-lock-keyword-face)

  (setq search-highlight t)

  (setq font-lock-maximum-decoration				      
	(list
	 (cons 'c++-mode 2)
	 (cons t t)))

  (setq font-lock-multiline t)

  (setq c++-font-lock-extra-types (append '("std" "u_char" "u_int")
					  c++-font-lock-extra-types))
  )

(add-hook 'font-lock-mode-hook 'my-font-lock-init)

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
					      ".lo"
					      ".d")))
; and ignore case
(setq completion-ignore-case t)

; ediff: use the minibar instead of a separate little window
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)
(setq ediff-autostore-merges t)

; vc: remap C-x v a to vc-annotate-at-point
;     add C-x v e to ediff-revision
(require 'vc)
(define-key vc-prefix-map "a" 'vc-annotate-goto-line)
(define-key vc-prefix-map "e" 'ediff-revision)
(define-key vc-prefix-map "s" 'vc-print-status)
(define-key vc-prefix-map "?" 'vc-print-status)
(setq vc-follow-symlinks t)

; (require 'vc-svn)
; (setq vc-handled-backends (cons 'SVN vc-handled-backends))

(defun vc-hg-registered (file)
  "Return non-nil if FILE is registered with hg."
  (if (vc-find-root file ".hg")       ; short cut
      (progn
        (load "vc-hg")
        (vc-hg-registered file))))

(setq vc-handled-backends (cons 'hg vc-handled-backends))

; fixes so shell-mode works with zsh
(setenv "EMACSPARENT" "1")

;; ; Darwin has problems with zsh...
(if (string-equal (getenv "ARCH") "Darwin")
    (add-hook 'shell-mode-hook 'my-shell-mode-init)
    (add-hook 'tex-shell-hook 'my-shell-mode-init)
    (setq tex-shell-file-name "/bin/sh")
  )

;; encryption
;(require 'crypt++)
;(setq crypt-encryption-type 'pgp)
;(crypt-rebuild-tables)

;; minibuffer
;; (resize-minibuffer-mode t)

;; More descriptive names than foo<2>
(require 'uniquify)
(if (featurep 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward
	  uniquify-separator ", "
	  uniquify-strip-common-suffix nil))

;; Hooks for the electric buffer menu
(require 'ebuff-menu)
(define-key electric-buffer-menu-mode-map "\C-s" 'isearch-forward)

(require 'grope)

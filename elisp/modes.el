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
(require 'once-only-header)

(setq ooh-file-license
"/*
 * IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING. By
 * downloading, copying, installing or using the software you agree to
 * this license. If you do not agree to this license, do not download,
 * install, copy or use the software.
 * 
 * Intel Open Source License 
 * 
 * Copyright (c) 2005 Intel Corporation. All rights reserved. 
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *   Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 
 *   Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 *   Neither the name of the Intel Corporation nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *  
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE INTEL OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
")

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
  (c-indent-two-spaces)
  )

(add-hook 'java-mode-hook 'my-java-setup)

; load visual-basic mode
(require 'visual-basic-mode)

; and nesc mode
(require 'nesc-mode)

; and sgml mode
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode  "psgml" "Major mode to edit XML files." t)

; setup my auto modes alist
(setq auto-mode-alist (append '(("\\.pl\\'" . perl-mode)
				("\\.[Hh]\\'" . c++-mode)
				("\\.c[cx]?x?\\'" . c++-mode)
				("\\.[Cyxi]\\'" . c++-mode)
				("\\.tcc\\'" . c++-mode)
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
  (define-key html-mode-map ">" 'self-insert-command)
  (setq indent-line-function 'indent-to-left-margin)
  )
(add-hook 'html-mode-hook 'my-html-setup)

;;
;; setup tex / bibtex modes
;;
(require 'tex-mode)
(require 'skeleton)
(require 'tex-site nil t)

(setq font-latex-title-fontity (quote color))

;; (defun latex-insert-section(type)
;;   "Insert a \\section{} style latex declaration."
;;   (interactive)
;;   (let ((section (read-string (format "\\%s: " type))))
;;     (insert "\\" type "{" section "}\n")))

;; (defun latex-insert-face(face)
;;   "Insert a \textbf{} style typeface declaration"
;;   (interactive)
;;   (insert "\\" face "{}")
;;   (backward-char 1))

;; (define-skeleton tex-latex-block-no-options
;;   "Same as tex-latex-block but without any options."
;;   (let ((choice (completing-read (format "LaTeX block name [%s]: "
;; 					 latex-block-default)
;; 				 (mapcar 'list
;; 					 (append standard-latex-block-names
;; 						 latex-block-names))
;; 				 nil nil nil nil latex-block-default)))
;;     (setq latex-block-default choice)
;;     (unless (or (member choice standard-latex-block-names)
;; 		(member choice latex-block-names))
;;       ;; Remember new block names for later completion.
;;       (push choice latex-block-names))
;;     choice)
;;   \n "\\begin{" str ?\}
;;   \n -2 _ \n
;;   "\\end{" str ?\} > \n)

;; (define-key tex-mode-map "\C-cb"
;;   (lambda () (interactive) (latex-insert-face "textbf")))
;; (define-key tex-mode-map "\C-ce"
;;   (lambda () (interactive) (latex-insert-face "emph")))
;; (define-key tex-mode-map "\C-ci"
;;   (lambda () (interactive) (latex-insert-face "item")))
;; (define-key tex-mode-map "\C-ct"
;;   (lambda () (interactive) (latex-insert-face "texttt")))
;; (define-key tex-mode-map "\C-c\C-a"
;;   (lambda () (interactive) (tex-latex-block-no-options "abstract")))
;; (define-key tex-mode-map "\C-c\C-x"
;;   (lambda () (interactive) (tex-latex-block-no-options "center")))
;; (define-key tex-mode-map "\C-c\C-e"
;;   (lambda () (interactive) (tex-latex-block-no-options "enumerate")))
;; (define-key tex-mode-map "\C-c\C-i"
;;   (lambda () (interactive) (tex-latex-block-no-options "itemize")))
;; (define-key tex-mode-map "\C-c\C-s"
;;   (lambda () (interactive) (latex-insert-section "section")))
;; (define-key tex-mode-map "\C-c\C-u"
;;   (lambda () (interactive) (latex-insert-section "subsection")))
;; (define-key tex-mode-map "\C-c\C-v"
;;   (lambda () (interactive) (tex-latex-block-no-options "verbatim")))
;; (define-key tex-mode-map "\C-c\C-c" 'comment-region)

(require 'bibtex)
(defun my-bibtex-setup()
  (interactive)
  (define-key bibtex-mode-map "\M-q" 'bibtex-fill-entry)
  (define-key bibtex-mode-map "\M-\C-l" 'lcvs-examine)
  (setq bibtex-align-at-equal-sign t)
  (setq fill-column 75))

(add-hook 'bibtex-mode-hook 'my-bibtex-setup)

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

  (setq font-lock-multiline t)
  )

(add-hook 'font-lock-mode-hook 'my-font-lock-init)

(setq c++-font-lock-extra-types (append '("std" "u_char" "u_int")
					c++-font-lock-extra-types))

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
					      ".lo"
					      ".d")))
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

;; ; Darwin has problems with zsh...
(if (string-equal (getenv "ARCH") "Darwin")
    (add-hook 'shell-mode-hook 'my-shell-mode-init)
    (add-hook 'tex-shell-hook 'my-shell-mode-init)
    (setq tex-shell-file-name "/bin/sh")
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

;; Hooks for the electric buffer menu
(require 'ebuff-menu)
(define-key electric-buffer-menu-mode-map "\C-s" 'isearch-forward)

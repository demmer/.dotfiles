;;;;;;;;;;;;;;;;;;;;;;;;; mode defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; scheme (copied from cs51)

(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq scheme-mit-dialect nil)
(setq scheme-program-name "stk")

; script mode

(defun determine-script-mode ()
  (goto-char (point-min))
  (cond
    ((looking-at "#![ \n\t]*/[^ \n\t]*perl")
        (perl-mode))
    ((looking-at "#![ \n\t]*/[^ \n\t]*tclsh")
        (tcl-mode))
    ((looking-at "#![ \n\t]*/[^ \n\t]*make")
        (makefile-mode))
    ((looking-at "#![ \n\t]*/[^ \n\t]*awk")
        (awk-mode))
  ))

(add-hook 'find-file-hooks 'determine-script-mode)

;;; load html mode

; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
; (add-hook 'html-helper-load-hook '(lambda () (load "html-font")))

;;;text mode

(defun my-text-setup ()
  (interactive)
;  (define-key indented-text-mode-map "" 'delete-backward-char)
;  (define-key indented-text-mode-map "\C-m" 'newline-and-indent)
;  (define-key indented-text-mode-map "\t" 'tab-to-tab-stop)
  (define-key text-mode-map "\C-m" 'newline)
  (auto-fill-mode 1)
  )
(add-hook 'text-mode-hook 'my-text-setup)

;;; Setup c/c++ mode

(fmakunbound 'c-mode)
(makunbound 'c-mode-map)
(fmakunbound 'c++-mode-map)
(makunbound 'c++-mode-map)
; (makunbound 'c-style-alist)
(autoload 'cc-mode "cc-mode" "cc-mode" t)
(autoload 'c++-mode "cc-mode" "c++-mode" t)
(autoload 'c-mode "cc-mode" "c++c-mode" t)
(autoload 'c-comment "c-fill" "c-comment" t)
(defconst my-c-style
  '("PERSONAL"
    (c-offsets-alist               .  ((access-label . -2)
				       (case-label . 2)
				       (statement-cont . 4)
				       (substatement-open . 0)
				       ))
    )
  "My C Programming Style")


(defun my-c++-setup ()
  (interactive)
  (setq comment-start "// ")
  (setq comment-end "")
  (make-variable-buffer-local 'using-c-comments)
  (setq using-c-comments nil)
  (make-variable-buffer-local 'c-comment-cplusplus)
  (setq c-comment-cplusplus t)

  (define-key c++-mode-map "\M-9" 'box-enterexit)
  (define-key c++-mode-map "\C-c\C-u" 'uncomment-region)
  )

(defun my-c-setup ()
  (interactive)
  (setq comment-start "/* ")
  (setq comment-end " */")
  (make-variable-buffer-local 'using-c-comments)
  (setq using-c-comments t)
  (make-variable-buffer-local 'c-comment-cplusplus)
  (setq c-comment-cplusplus nil)
  )


(defun my-cc-common-setup ()
  (interactive)
  (setq c-tab-always-indent t)
  (setq indent-tabs-mode nil)                             ; Use spaces for tabs
  (let ((my-style "PERSONAL"))
    (or (assoc my-style c-style-alist)
     (setq c-style-alist (append c-style-alist (list my-c-style)))))
  (c-set-style "PERSONAL")
  (setq dabbrev-case-fold-search nil
	   dabbrev-case-replace nil)

  (setq-default c-basic-offset 4)
  (setq c-electric-pound-behavior '(alignleft))
  (setq c-comment-starting-blank nil)
  (setq c-comment-hanging-indent t)
  (setq c-comment-indenting t)
  (setq c-hanging-comment-starter-p nil)
  (setq c-hanging-comment-ender-p nil)
  
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
)

(add-hook 'c++-mode-hook 'my-c++-setup)
(add-hook 'c-mode-hook 'my-c-setup)
(add-hook 'c-mode-common-hook 'my-cc-common-setup)

; setup my auto modes alist
(setq auto-mode-alist (append '(("\\.pl\\'" . perl-mode)
				("\\.C\\'" . c++-mode)
				("\\.[Hh]\\'" . c++-mode)
				("\\.c[cx]?x?\\'" . c++-mode)
				("\\.y\\'" . c++-mode)
				("\\.java\\'" . java-mode)
				("\\.html$" . html-mode)
			        ("\\.w\\'" . web-mode)
				("\\.zsh\\'" . shell-script-mode)
				("\\.[A-Za-z0-9]*rc" . shell-script-mode)
				("\\GNUmakerules\\'" . makefile-mode)
				("\\.[12345678]\\'" . text-mode)
				("\\.mdn\\'" . sgml-mode)
				("\\.xsl\\'" . sgml-mode)
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

; ispell: from gwk
(autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
(autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
(autoload 'ispell-region "ispell" "Check spelling of every word in region" t)
(autoload 'ispell-buffer "ispell" "Check spelling of every word in buffer" t)
(autoload 'ispell-message "ispell" "Check spelling of mail/news message" t)
(setq ispell-program-name "ispell")

; extensions of files I don't want to open
(append-no-dup ".class" completion-ignored-extensions)
(append-no-dup ".T" completion-ignored-extensions)

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
